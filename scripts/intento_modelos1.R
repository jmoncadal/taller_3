library(readxl)
library(sf)


test_final <- read_excel("stores/test_final.xlsx")
View(test_final)

train_final <- read_excel("stores/train_final.xlsx")
View(train_final)


wd_main <- "C:/Users/Rodri/OneDrive/Rodri estudioso/GitHub/taller_3"
wd_data <- "/stores"
wd_code <- "/scripts"
wd_output <- "/views"

# Ponemos la loc ====================================================

train_final <- train_final %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

test_final <- test_final %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

localidades <- st_read(paste0(wd_main, wd_data, "/localidades.geojson"))
localidades <- st_transform(localidades, 4326)
train_sf <- st_join(
  train_sf,
  localidades %>% select(LocCodigo, LocNombre),
  left = TRUE
)

test_sf <- st_join(
  test_sf,
  localidades %>% select(LocCodigo, LocNombre),
  left = TRUE
)

# Volver a data.frame “normal” y sobrescribir los objetos finales
train_final <- train_sf %>% st_drop_geometry()
test_final  <- test_sf %>% st_drop_geometry()

train_final <- train_final %>%
  mutate(
    LocCodigo = LocCodigo.y,
    LocNombre = LocNombre.y
  ) %>%
  select(-LocCodigo.x, -LocNombre.x, -LocCodigo.y, -LocNombre.y)

test_final <- test_final %>%
  mutate(
    LocCodigo = LocCodigo.y,
    LocNombre = LocNombre.y
  ) %>%
  select(-LocCodigo.x, -LocNombre.x, -LocCodigo.y, -LocNombre.y)

vars_text <- c(
  # texto general / categórico
  "property_type",
  
  # todas las *_text y *_txt
  "tipo_inmueble_text",
  "tiene_garaje_text",
  "garaje_cubierto_text",
  "garaje_indep_text",
  "remodelada_text",
  "tiene_vigilancia_text",
  "uso_comercial_text",
  "menciona_cercania_txt",
  "cercania_bus_text",
  "cercania_cafe_txt",
  "menciona_cuadras_txt"
  # "geometry" queda fuera a propósito (objeto sf)
)

# Variables BINARIAS (dummies 0/1, sí/no)
vars_binarias <- c(
  "tiene_sala",
  "tiene_comedor",
  "sala_comedor_conjunto",
  "tiene_deposito_bodega",
  "tiene_terraza",
  "tiene_balcon",
  "terraza_propia",
  "tiene_cocina",
  "cocina_integral",
  "cocina_abierta",
  "tiene_patio_ropas",
  "menciona_centro_comercial",
  "cercania_cc",
  "is_residential"
)

vars_numericas <- c(
  "ESTRATO",
  "CODIGO_UPZ",
  "surface_covered",
  "n_palabras_title",
  "n_cafes_500m",
  "distancia_bus",
  "n_lamparas_200m"
)

x_forward <- unique(c(
  vars_binarias,
  vars_text,
  vars_numericas
))

x_forward

library(MASS)

# Modelo nulo: solo intercepto
modelo_nulo <- lm(price ~ 1, data = df_sel_cc)

# Fórmula "full" con todas las variables candidatas
formula_full <- as.formula(
  paste("price ~", paste(x_forward, collapse = " + "))
)

modelo_full <- lm(formula_full, data = train_final)

# Forward selection (AIC)
modelo_forward <- stepAIC(
  modelo_nulo,
  direction = "forward",
  scope = list(lower = modelo_nulo, upper = modelo_full),
  trace = TRUE
)

summary(modelo_forward)

vars_forward_sel <- attr(terms(modelo_forward), "term.labels")
vars_forward_sel

### Optimización glmnet

library(glmnet)

library(dplyr)
library(glmnet)

# 1) Dataset con outcome, UPZ y las variables seleccionadas
df_enet <- train_final %>%
  dplyr::select(price, CODIGO_UPZ, all_of(vars_forward_sel))

# 2) Quitar filas con NA en cualquiera de estas columnas
df_enet_cc <- na.omit(df_enet)

nrow(df_enet)
nrow(df_enet_cc)   # este es el N que va a usar glmnet

# Fórmula de regresión solo con las variables seleccionadas
form_enet <- as.formula(
  paste(
    "price ~",
    paste(
      c(
        vars_forward_sel,
        "terraza_propia",
        "distancia_bus"
      ),
      collapse = " + "
    )
  )
)

# Matriz X e y (glmnet necesita matrices numéricas)
X_enet <- model.matrix(form_enet, data = df_enet_cc)[, -1]  # quitamos el intercepto
y_enet <- df_enet_cc$price

dim(X_enet)

# foldid según CODIGO_UPZ (misma UPZ = mismo fold)
foldid <- as.integer(as.factor(df_enet_cc$CODIGO_UPZ))

length(unique(foldid))  # número de UPZ usadas como folds
table(foldid)[1:10]     # primeros folds, solo para revisar

set.seed(123)

alpha_grid <- seq(0, 1, by = 0.05)  # puedes afinar luego si quieres

alpha_results <- data.frame(
  alpha      = numeric(),
  lambda_min = numeric(),
  cvm_min    = numeric()
)

cv_fits <- list()

for (a in alpha_grid) {
  cat("Probando alpha =", a, "...\n")
  
  cv_fit <- cv.glmnet(
    x            = X_enet,
    y            = y_enet,
    alpha        = a,
    family       = "gaussian",
    foldid       = foldid,      # <<--- CV geográfica por UPZ
    type.measure = "mae"
  )
  
  cv_fits[[as.character(a)]] <- cv_fit
  
  alpha_results <- rbind(
    alpha_results,
    data.frame(
      alpha      = a,
      lambda_min = cv_fit$lambda.min,
      cvm_min    = min(cv_fit$cvm)  # error CV para lambda.min
    )
  )
}

alpha_results

# Ordenar por menor error CV
alpha_results_best <- alpha_results[order(alpha_results$cvm_min), ]
alpha_results_best

best_row    <- alpha_results_best[1, ]
best_alpha  <- best_row$alpha
best_lambda <- best_row$lambda_min

best_alpha
best_lambda




library(glmnet)
library(dplyr)

### 1. Variables X exactas ----------------------------------------

x_vars <- c(
  "ESTRATO", "tiene_terraza", "tipo_inmueble_text", "property_type",
  "garaje_indep_text", "garaje_cubierto_text", "terraza_propia",
  "sala_comedor_conjunto", "tiene_sala", "cocina_integral",
  "n_lamparas_200m", "menciona_cercania_txt", "tiene_balcon",
  "tiene_patio_ropas", "cocina_abierta", "menciona_cuadras_txt",
  "n_cafes_500m", "tiene_vigilancia_text", "cercania_bus_text",
  "is_residential", "remodelada_text", "distancia_bus",
  "n_palabras_title"
)

# Aseguramos que existen en ambos data frames
x_vars <- x_vars[x_vars %in% names(train_final) & x_vars %in% names(test_final)]

### 2. Combinar train + test solo con esas X -----------------------

n_train <- nrow(train_final)

df_all_X <- bind_rows(
  train_final[, x_vars, drop = FALSE],
  test_final[,  x_vars, drop = FALSE]
)

# Convertir caracteres a factor
chr_cols <- sapply(df_all_X, is.character)
df_all_X[chr_cols] <- lapply(df_all_X[chr_cols], factor)

### 3. Matriz de diseño SIN eliminar filas (na.pass) ---------------

X_all <- model.matrix(
  ~ . - 1,
  data      = df_all_X,
  na.action = na.pass  # <-- clave: no borrar filas con NA
)

# Reemplazar NAs en la matriz numérica por 0
X_all[is.na(X_all)] <- 0

# Separar train / test por índice
X_train <- X_all[1:n_train, , drop = FALSE]
X_test  <- X_all[(n_train + 1):nrow(X_all), , drop = FALSE]

y_train <- train_final$price

# Chequeo rápido
dim(X_train)
dim(X_test)
nrow(train_final)
nrow(test_final)

### 4. Ajustar glmnet con best_alpha y best_lambda -----------------

# Se asume que ya tienes:
# best_alpha
# best_lambda

enet_final <- glmnet(
  x      = X_train,
  y      = y_train,
  alpha  = best_alpha,
  lambda = best_lambda,
  family = "gaussian"
)

### 5. Predicciones en test ----------------------------------------

pred_test <- as.vector(
  predict(enet_final, newx = X_test, s = best_lambda)
)

length(pred_test)
nrow(test_final)  # deben ser iguales

### 6. Exportar submission -----------------------------------------

submission <- data.frame(
  property_id = test_final$property_id,
  price       = pred_test
)

write.csv(
  submission,
  "submission_glmnet_bestalpha_bestlambda.csv",
  row.names = FALSE
)

#Modelo NNN:

library(nnet)
library(dplyr)

### 1. Variables para la red 

library(dplyr)
library(neuralnet)
# NN V01 =========================================================
### 1. VARIABLES ---------------------------------------------
library(keras)
library(dplyr)

## 1. Variables para la NN ----------------------------------

train_final <- train_final %>%
  mutate(
    CODIGO_UPZ = as.factor(CODIGO_UPZ),
    LocCodigo  = as.factor(LocCodigo)
  )

test_final <- test_final %>%
  mutate(
    CODIGO_UPZ = as.factor(CODIGO_UPZ),
    LocCodigo  = as.factor(LocCodigo)
  )


nn_vars <- c(
  "ESTRATO",
  "n_lamparas_200m",
  "distancia_bus",
  "tiene_vigilancia_text",
  "EPE__m2_ha",
  "menciona_cercania_txt",
  "is_residential",
  "ESTRATO_was_na",
  "remodelada_text",
  "property_type",
  "bedrooms_final_set2",
  "bathrooms_final_set2",
  "n_cafes_500m",
  "tiene_cocina",
  "cercania_cc",
  "tiene_garaje_text",
  "LocCodigo"
)

nn_vars <- nn_vars[nn_vars %in% names(train_final)]

train_nn <- train_final
test_nn  <- test_final

# 1.1 NAs -> 0 (para todas estas X)
for (v in nn_vars) {
  train_nn[[v]][is.na(train_nn[[v]])] <- 0
  test_nn[[v]][is.na(test_nn[[v]])]   <- 0
}

# 1.2 Codificar categóricas de forma consistente (train+test juntos)
n_train <- nrow(train_nn)

df_all <- bind_rows(
  train_nn[, nn_vars, drop = FALSE],
  test_nn[,  nn_vars, drop = FALSE]
)

for (v in nn_vars) {
  if (is.character(df_all[[v]]) || is.factor(df_all[[v]])) {
    df_all[[v]] <- as.numeric(factor(df_all[[v]]))
  }
}

X_all   <- as.matrix(df_all)
X_train <- X_all[1:n_train, , drop = FALSE]
X_test  <- X_all[(n_train + 1):nrow(X_all), , drop = FALSE]

y_train <- as.numeric(train_nn$price)

## 2. Validación espacial por UPZ ----------------------------



LocCodigo <- train_final$LocCodigo
LocCodigo[is.na(LocCodigo)] <- 0

set.seed(123)
loc_nonzero <- sort(unique(LocCodigo[LocCodigo != 0]))
n_val_loc   <- max(1, round(0.2 * length(loc_nonzero)))
val_loc     <- sample(loc_nonzero, n_val_loc)

idx_val <- which(LocCodigo %in% val_loc)
idx_tr  <- setdiff(seq_len(nrow(X_train)), idx_val)

X_tr  <- X_train[idx_tr, , drop = FALSE]
y_tr  <- y_train[idx_tr]

X_val <- X_train[idx_val, , drop = FALSE]
y_val <- y_train[idx_val]


## 3. Escalamiento ------------------------------------------

mu <- colMeans(X_tr)
sd <- apply(X_tr, 2, sd)
sd[sd == 0] <- 1

scale_fn <- function(x, mu, sd) {
  sweep(sweep(x, 2, mu, "-"), 2, sd, "/")
}

X_tr_sc     <- scale_fn(X_tr,     mu, sd)
X_val_sc    <- scale_fn(X_val,    mu, sd)
X_train_sc  <- scale_fn(X_train,  mu, sd)
X_test_sc   <- scale_fn(X_test,   mu, sd)

## 4. Grid search sobre número de neuronas -------------------

sizes   <- c(4, 8, 12, 15, 18, 21, 24)  # sencillo y rápido
results <- data.frame(size = sizes, MAE = NA_real_)

for (s in sizes) {
  cat("Training model with", s, "neurons...\n")
  
  model <- keras_model_sequential() %>%
    layer_dense(units = s,
                activation = "relu",
                input_shape = ncol(X_tr_sc)) %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = "adam",
    loss      = "mae"
  )
  
  model %>% fit(
    X_tr_sc, y_tr,
    epochs     = 5,      # corto solo para seleccionar tamaño
    batch_size = 64,
    verbose    = 0
  )
  
  pred_val <- model %>% predict(X_val_sc)
  mae_val  <- mean(abs(y_val - pred_val))
  
  results[results$size == s, "MAE"] <- mae_val
}

print(results)

best_size <- results$size[which.min(results$MAE)]
cat("Best hidden units:", best_size, "\n")

## 5. Modelo final con todo el train -------------------------

model_final <- keras_model_sequential() %>%
  layer_dense(units = best_size,
              activation = "relu",
              input_shape = ncol(X_train_sc)) %>%
  layer_dense(units = 1)

model_final %>% compile(
  optimizer = "adam",
  loss      = "mae"
)

model_final %>% fit(
  X_train_sc, y_train,
  epochs     = 30,
  batch_size = 64,
  verbose    = 1
)

## 6. MAE in-sample ------------------------------------------

pred_train <- model_final %>% predict(X_train_sc)
mae_in_sample <- mean(abs(y_train - pred_train))
cat("MAE in-sample (keras NN):", mae_in_sample, "\n")

## 7. Predicción en test y export ----------------------------

pred_test <- model_final %>% predict(X_test_sc)

submission <- data.frame(
  property_id = test_final$property_id,
  price       = as.vector(pred_test)
)

write.csv(
  submission,
  "submission_keras_NN_vars15_spatialCV.csv",
  row.names = FALSE
)


# CART ======================
library(rpart)
library(dplyr)

## 0. Variables que vamos a usar en los árboles -----------------

vars_cart <- c(
  "price",
  "ESTRATO",
  "surface_total", "surface_covered",
  "n_palabras_title", "n_palabras_desc", "n_palabras_title_nocod",
  "n_cafes_500m", "n_lamparas_200m",
  "distancia_cafe", "distancia_bus",
  "EPE__m2_ha", "EPCC", "EPT",
  "SHAPE_AREA", "SHAPE_Area", "SHAPE_Area_1", "SHAPE_Area_12",
  "CODIGO_UPZ", "CODIGO_ZON", "LocCodigo",
  "tipo_inmueble_text_std", "property_type_final",
  "tiene_sala", "tiene_comedor", "sala_comedor_conjunto",
  "tiene_garaje_text", "garaje_cubierto_text", "garaje_indep_text",
  "tiene_cocina", "cocina_integral", "cocina_abierta",
  "tiene_patio_ropas", "tiene_vigilancia_text",
  "menciona_cercania_txt", "menciona_cuadras_txt",
  "is_residential", "remodelada_text",
  "rooms.y", "bedrooms_final_set2", "bathrooms_final_set2",
  "ESTRATO_was_na", "rooms.y_was_na",
  "bedrooms_final_set2_was_na", "bathrooms_final_set2_was_na"
)

# Nos quedamos solo con las que existen
vars_cart <- intersect(vars_cart, names(train_final))

df_cart <- train_final[, vars_cart, drop = FALSE]

## 1. Tratamiento simple de NA --------------------------------

for (v in setdiff(names(df_cart), "price")) {
  if (is.numeric(df_cart[[v]])) {
    df_cart[[v]][is.na(df_cart[[v]])] <- 0
  } else {
    # caracter/factor -> "missing"
    df_cart[[v]] <- as.character(df_cart[[v]])
    df_cart[[v]][is.na(df_cart[[v]])] <- "missing"
    df_cart[[v]] <- factor(df_cart[[v]])
  }
}

## 2. Partición espacial por LocCodigo ------------------------

LocCodigo <- train_final$LocCodigo
LocCodigo[is.na(LocCodigo)] <- 0

set.seed(123)
loc_nonzero <- sort(unique(LocCodigo[LocCodigo != 0]))
n_val_loc   <- max(1, round(0.2 * length(loc_nonzero)))
val_loc     <- sample(loc_nonzero, n_val_loc)

idx_val <- which(LocCodigo %in% val_loc)
idx_tr  <- setdiff(seq_len(nrow(df_cart)), idx_val)

df_tr  <- df_cart[idx_tr, , drop = FALSE]
df_val <- df_cart[idx_val, , drop = FALSE]

## Función de MAE ---------------------------------------------

mae_fun <- function(y, yhat) mean(abs(y - yhat))

###############################################################
## 3. CART 1: árbol moderado (maxdepth = 7) + tuning de cp  ###
###############################################################

# Ajustamos un árbol base "grueso" en el fold de entrenamiento
set.seed(123)
cart1_base <- rpart(
  price ~ .,
  data   = df_tr,
  method = "anova",
  control = rpart.control(
    minbucket = 50,
    minsplit  = 100,
    maxdepth  = 7,
    cp        = 0.001   # cp pequeño para tener tabla rica
  )
)

# Tabla de complejidad
cp_tab1 <- cart1_base$cptable[, "CP"]

# Para cada cp candidate: podar y medir MAE en validación (validación espacial)
results_cart1 <- data.frame(cp = cp_tab1, MAE_val = NA_real_)

for (i in seq_along(cp_tab1)) {
  cp_i <- cp_tab1[i]
  cart1_pruned_i <- prune(cart1_base, cp = cp_i)
  pred_val_i <- predict(cart1_pruned_i, newdata = df_val)
  results_cart1$MAE_val[i] <- mae_fun(df_val$price, pred_val_i)
}

print(results_cart1)

# cp que minimiza el MAE en validación espacial
best_cp1 <- results_cart1$cp[which.min(results_cart1$MAE_val)]
cat("CART1 - Mejor cp:", best_cp1, "\n")

# Ajustamos árbol en TODO el train (df_cart) y lo podamos con ese cp
set.seed(123)
cart1_full_base <- rpart(
  price ~ .,
  data   = df_cart,
  method = "anova",
  control = rpart.control(
    minbucket = 50,
    minsplit  = 100,
    maxdepth  = 7,
    cp        = 0.001
  )
)

cart1_final <- prune(cart1_full_base, cp = best_cp1)

# MAE in-sample del CART1 final
pred_train_cart1 <- predict(cart1_final, newdata = df_cart)
mae_train_cart1  <- mae_fun(df_cart$price, pred_train_cart1)
cat("CART1 - MAE in-sample:", mae_train_cart1, "\n")

###############################################################
## 4. CART 2: árbol grande (maxdepth = 12) + tuning de cp   ###
###############################################################

set.seed(123)
cart2_base <- rpart(
  price ~ .,
  data   = df_tr,
  method = "anova",
  control = rpart.control(
    minbucket = 20,
    minsplit  = 40,
    maxdepth  = 12,
    cp        = 0.0005   # cp aún más pequeño
  )
)

cp_tab2 <- cart2_base$cptable[, "CP"]

results_cart2 <- data.frame(cp = cp_tab2, MAE_val = NA_real_)

for (i in seq_along(cp_tab2)) {
  cp_i <- cp_tab2[i]
  cart2_pruned_i <- prune(cart2_base, cp = cp_i)
  pred_val_i <- predict(cart2_pruned_i, newdata = df_val)
  results_cart2$MAE_val[i] <- mae_fun(df_val$price, pred_val_i)
}

print(results_cart2)

best_cp2 <- results_cart2$cp[which.min(results_cart2$MAE_val)]
cat("CART2 - Mejor cp:", best_cp2, "\n")

# Ajustamos árbol grande en TODO df_cart y podamos con ese cp
set.seed(123)
cart2_full_base <- rpart(
  price ~ .,
  data   = df_cart,
  method = "anova",
  control = rpart.control(
    minbucket = 20,
    minsplit  = 40,
    maxdepth  = 12,
    cp        = 0.0005
  )
)

cart2_final <- prune(cart2_full_base, cp = best_cp2)

# MAE in-sample del CART2 final
pred_train_cart2 <- predict(cart2_final, newdata = df_cart)
mae_train_cart2  <- mae_fun(df_cart$price, pred_train_cart2)
cat("CART2 - MAE in-sample:", mae_train_cart2, "\n")

## CART Exportamos resultados ====================

# Variables usadas por los árboles (ya las teníamos definidas)
vars_cart <- vars_cart[vars_cart %in% names(test_final)]

# Hacemos una copia limpia del test
df_test_cart <- test_final[, vars_cart, drop = FALSE]

# Tratamiento de NA idéntico al train
for (v in names(df_test_cart)) {
  
  if (v == "price") next  # NO tocar la y (aunque en test no existe)
  
  if (is.numeric(df_test_cart[[v]])) {
    df_test_cart[[v]][is.na(df_test_cart[[v]])] <- 0
  } else {
    df_test_cart[[v]] <- as.character(df_test_cart[[v]])
    df_test_cart[[v]][is.na(df_test_cart[[v]])] <- "missing"
    df_test_cart[[v]] <- factor(df_test_cart[[v]])
  }
}

pred_test_cart2 <- predict(cart2_final, newdata = df_test_cart)

submission_cart2 <- data.frame(
  property_id = test_final$property_id,
  price       = pred_test_cart2
)

write.csv(
  submission_cart2,
  "submission_CART2_spatialCV.csv",
  row.names = FALSE
)

# NN v02 =========================================================
library(keras)
library(dplyr)

set.seed(123)

# 1. VARIABLES FOR THE NN -----------------------------------------

nn_vars <- c(
  "ESTRATO",
  "surface_total", "surface_covered",
  "n_palabras_title",
  "n_cafes_500m", "n_lamparas_200m",
  "distancia_cafe", "distancia_bus",
  "EPE__m2_ha", "EPCC", "EPT",
  "rooms.y", "bedrooms_final_set2", "bathrooms_final_set2",
  "tiene_vigilancia_text",
  "tiene_cocina", "cocina_integral",
  "menciona_cercania_txt", "menciona_cuadras_txt",
  "is_residential",
  "ESTRATO_was_na", "remodelada_text",
  "property_type_final",
  "LocCodigo"
)

nn_vars <- nn_vars[nn_vars %in% names(train_final)]

train_nn <- train_final
test_nn  <- test_final

# 1.1 Simple NA handling: NA -> 0 for these X ---------------------

for (v in nn_vars) {
  train_nn[[v]][is.na(train_nn[[v]])] <- 0
  test_nn[[v]][is.na(test_nn[[v]])]   <- 0
}

# 1.2 Encode categoricals consistently using train+test together ---

n_train <- nrow(train_nn)

df_all <- bind_rows(
  train_nn[, nn_vars, drop = FALSE],
  test_nn[,  nn_vars, drop = FALSE]
)

for (v in nn_vars) {
  if (is.character(df_all[[v]]) || is.factor(df_all[[v]])) {
    df_all[[v]] <- as.numeric(factor(df_all[[v]]))
  }
}

X_all   <- as.matrix(df_all)
X_train <- X_all[1:n_train, , drop = FALSE]
X_test  <- X_all[(n_train + 1):nrow(X_all), , drop = FALSE]

y_train <- as.numeric(train_nn$price)

# 2. SPATIAL VALIDATION BY LocCodigo -------------------------------

LocCodigo_cv <- as.character(train_final$LocCodigo)
LocCodigo_cv[is.na(LocCodigo_cv)] <- "0"

loc_nonzero <- sort(unique(LocCodigo_cv[LocCodigo_cv != "0"]))
n_val_loc   <- max(1, round(0.2 * length(loc_nonzero)))
val_loc     <- sample(loc_nonzero, n_val_loc)

idx_val <- which(LocCodigo_cv %in% val_loc)
idx_tr  <- setdiff(seq_len(nrow(X_train)), idx_val)

X_tr  <- X_train[idx_tr, , drop = FALSE]
y_tr  <- y_train[idx_tr]

X_val <- X_train[idx_val, , drop = FALSE]
y_val <- y_train[idx_val]

# 3. SCALING ------------------------------------------------------

mu <- colMeans(X_tr)
sd <- apply(X_tr, 2, sd)
sd[sd == 0] <- 1

scale_fn <- function(x, mu, sd) {
  sweep(sweep(x, 2, mu, "-"), 2, sd, "/")
}

X_tr_sc     <- scale_fn(X_tr,     mu, sd)
X_val_sc    <- scale_fn(X_val,    mu, sd)
X_train_sc  <- scale_fn(X_train,  mu, sd)
X_test_sc   <- scale_fn(X_test,   mu, sd)

# 4. MODEL BUILDER (layers x units) -------------------------------

build_model <- function(n_layers, n_units, input_dim) {
  model <- keras_model_sequential()
  
  # first hidden layer
  model %>% layer_dense(
    units      = n_units,
    activation = "relu",
    input_shape = input_dim
  )
  
  # additional hidden layers
  if (n_layers > 1) {
    for (i in 2:n_layers) {
      model %>% layer_dense(units = n_units, activation = "relu")
    }
  }
  
  # output layer
  model %>% layer_dense(units = 1)
  
  model %>% compile(
    optimizer = optimizer_adam(learning_rate = 0.001),
    loss      = "mae"
  )
  
  model
}

# 5. GRID SEARCH: LAYERS x UNITS WITH EARLY STOPPING --------------

layers_grid <- c(2, 3, 5, 8)
units_grid  <- c(4, 8, 12, 15, 18,21)

results <- data.frame(
  layers = integer(),
  units  = integer(),
  MAE    = numeric()
)

for (L in layers_grid) {
  for (U in units_grid) {
    cat("Training model with", L, "layers and", U, "units...\n")
    
    model <- build_model(L, U, ncol(X_tr_sc))
    
    cb_es <- callback_early_stopping(
      monitor              = "val_loss",
      patience             = 3,
      restore_best_weights = TRUE
    )
    
    model %>% fit(
      X_tr_sc, y_tr,
      epochs          = 15,     
      batch_size      = 64,
      validation_data = list(X_val_sc, y_val),
      callbacks       = list(cb_es),
      verbose         = 0
    )
    
    pred_val <- model %>% predict(X_val_sc)
    mae_val  <- mean(abs(y_val - pred_val))
    
    results <- rbind(
      results,
      data.frame(layers = L, units = U, MAE = mae_val)
    )
  }
}

print(results)

best_row    <- results[which.min(results$MAE), ]
best_layers <- best_row$layers
best_units  <- best_row$units

cat("Best architecture:", best_layers, "layers x", best_units, "units\n")

# 6. FINAL MODEL ON FULL TRAIN (WITH EARLY STOPPING) ---------------

model_final <- build_model(best_layers, best_units, ncol(X_train_sc))

# slightly smaller LR + more epochs to crush in-sample MAE
model_final %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.0005),
  loss      = "mae"
)

cb_final <- callback_early_stopping(
  monitor              = "val_loss",
  patience             = 3,
  restore_best_weights = TRUE
)

history <- model_final %>% fit(
  X_train_sc, y_train,
  epochs         = 60,
  batch_size     = 64,
  validation_split = 0.2,   # small random validation for early stopping
  callbacks      = list(cb_final),
  verbose        = 1
)

# 7. MAE IN-SAMPLE -----------------------------------------------

pred_train <- model_final %>% predict(X_train_sc)
mae_in_sample <- mean(abs(y_train - pred_train))
cat("MAE in-sample (keras NN v02):", mae_in_sample, "\n")

# 8. PREDICTION ON TEST + EXPORT ---------------------------------

pred_test <- model_final %>% predict(X_test_sc)

submission <- data.frame(
  property_id = test_final$property_id,
  price       = as.vector(pred_test)
)

write.csv(
  submission,
  "submission_keras_NN_v02_layers_units_spatialCV.csv",
  row.names = FALSE
)
