####
# Proyecto : Taller 3 - Modelos de precio de vivienda (Chapinero)
# Archivo  : 01_modelos_price.R
# Autores  : Equipo X
# Fecha    : 2025-11-23
# Descripción:
#   Script maestro que:
#    - Carga y prepara bases train/test
#    - Construye validación espacial por localidad/UPZ
#    - Estima modelos:
#        * Regresión lineal con selección forward (stepAIC)
#        * glmnet (CV espacial por UPZ)
#        * NN v01 (grid de neuronas)
#        * CART (árboles con tuning de cp)
#        * Random Forest usando variables del CART
#        * NN v02 (layers x units + early stopping)
#        * Super Learner (mean, glm, glmnet, CART, RF, NN)
####

####
# ÍNDICE
# 0. Configuración general
# 1. Gestión de paquetes
# 2. Carga de librerías y opciones
# 3. Rutas de trabajo
# 4. Carga y preparación de datos (sf, Localidades)
# 5. Definición de conjuntos de variables
# 6. Modelo lineal con selección forward (stepAIC)
# 7. glmnet con CV espacial por Localidad
# 8. NN v01 (keras, grid tamaño)
# 9. CART + selección de cp con validación espacial
# 10. Random Forest sobre variables del CART
# 11. NN v02 (layers x units + early stopping)
# 12. Super Learner (mean, glm, glmnet, CART, RF, NN)
####

####
# 0. Configuración general
####

set.seed(123)  # Semilla global (ajústala si quieres)
options(
  scipen = 999,                # evitar notación científica fea
  dplyr.summarise.inform = FALSE
)

####
# 1. Gestión de paquetes ====
####

use_packages <- function(pkgs) {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      install.packages(p)
    }
    library(p, character.only = TRUE)
  }
}

####
# 2. Carga de librerías y opciones ====
###

use_packages(c(
  "readxl", "sf", "tidyr", "dplyr",
  "MASS",          # stepAIC (forward selection)
  "glmnet",
  "nnet", "neuralnet", "keras",
  "rpart", "ranger",
  "data.table", "sl3", "future"
))

####
# 3. Rutas y procesamientos ====
####

test_final <- read_excel("stores/test_final.xlsx")
View(test_final)

train_final <- read_excel("stores/train_final.xlsx")
View(train_final)


wd_main <- "C:/Users/Rodri/OneDrive/Rodri estudioso/GitHub/taller_3"
wd_data <- "/stores"
wd_code <- "/scripts"
wd_output <- "/views"


####
# 4. Procesamientos ====
####

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


####
# 5. Definición conjuntos de variables ====
####

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



####
# 6. Modelo lineal con selección de forward ====
####

# Modelo nulo: solo intercepto
modelo_nulo <- lm(price ~ 1, data = df_sel_cc)

# Fórmula "full" con todas las variables candidatas
formula_full <- as.formula(
  paste("price ~", paste(x_forward, collapse = " + "))
)

modelo_full <- lm(formula_full, data = train_final)

# Forward selection 
modelo_forward <- stepAIC(
  modelo_nulo,
  direction = "forward",
  scope = list(lower = modelo_nulo, upper = modelo_full),
  trace = TRUE
)

summary(modelo_forward)

vars_forward_sel <- attr(terms(modelo_forward), "term.labels")
vars_forward_sel

####
# 7. glmnet con CV espacial ====
####



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



enet_final <- glmnet(
  x      = X_train,
  y      = y_train,
  alpha  = 0.5,
  family = "gaussian"
)

### 5. Predicciones en test ----------------------------------------

pred_test <- as.vector(
  predict(enet_final, newx = X_test, s = best_lambda)
)

length(pred_test)
nrow(test_final)  # deben ser iguales

### 6. MAE in-sample (train) ---------------------------------------

# Predicciones sobre el conjunto de entrenamiento
pred_train <- as.vector(
  predict(enet_final, newx = X_train, s = best_lambda)
)

# Calcular MAE in-sample
mae_in_sample <- mean(abs(y_train - pred_train))
cat("MAE in-sample (glmnet):", mae_in_sample, "\n")


### 7. Exportar submission -----------------------------------------

submission <- data.frame(
  property_id = test_final$property_id,
  price       = pred_test
)

write.csv(
  submission,
  "submission_glmnet_bestalpha_bestlambda.csv",
  row.names = FALSE
)


####
# 8. NN V01 ======
####


## 1. Variables para la NN basadas en selection ----------------------

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

## 2. Validación espacial por lOCALIDAD ----------------------------



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


####
# 9. CART + selección cp & validación espacial ======
####

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


## 3. CART 1: árbol moderado (maxdepth = 7) + tuning de cp  ###


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

##
## CART 2: árbol grande (maxdepth = 12) + tuning de cp   ###
###

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

####
# 10.NN V02 (layers unidades y early stopping) ======
####


set.seed(123)

# 1. VARIABLES ---

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

# 1 Manejo de Missings  ---

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

# 2. Validación por LocCodigo ---

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

# 3. Escalamiento ---

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

# 4. Construidor de modelos (layers x units) ---

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

# 5. GRID SEARCH: LAYERS x UNITS WITH EARLY STOPPING ---

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

# 6. Modelo final ---

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

library(ranger)
library(dplyr)


############################################################
# 11. CART + selección cp & validación espacial
############################################################

## 1. Variables del modelo (las del CART) ----------------------------

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

vars_cart <- intersect(vars_cart, names(train_final))

df_cart <- train_final[, vars_cart, drop = FALSE]

## 2. Tratamiento de NA  -------------------

for (v in setdiff(names(df_cart), "price")) {
  if (is.numeric(df_cart[[v]])) {
    df_cart[[v]][is.na(df_cart[[v]])] <- 0
  } else {
    df_cart[[v]] <- as.character(df_cart[[v]])
    df_cart[[v]][is.na(df_cart[[v]])] <- "missing"
    df_cart[[v]] <- factor(df_cart[[v]])
  }
}

## 3. Split espacial por LocCodigo ----------------------------------

LocCodigo_cv <- as.character(train_final$LocCodigo)
LocCodigo_cv[is.na(LocCodigo_cv)] <- "0"

set.seed(123)
loc_nonzero <- sort(unique(LocCodigo_cv[LocCodigo_cv != "0"]))
n_val_loc   <- max(1, round(0.2 * length(loc_nonzero)))
val_loc     <- sample(loc_nonzero, n_val_loc)

idx_val <- which(LocCodigo_cv %in% val_loc)
idx_tr  <- setdiff(seq_len(nrow(df_cart)), idx_val)

df_tr  <- df_cart[idx_tr, , drop = FALSE]
df_val <- df_cart[idx_val, , drop = FALSE]

mae_fun <- function(y, yhat) mean(abs(y - yhat))

## 4. Grilla de hiperparámetros -------------------------------------

p <- ncol(df_cart) - 1  # sin contar price

mtry_grid       <- unique(c(floor(sqrt(p)), floor(p/3), floor(p/2)))
min_node_grid   <- c(10, 20, 40)

grid <- expand.grid(
  mtry          = mtry_grid,
  min.node.size = min_node_grid
)

results_grid <- grid
results_grid$MAE_val <- NA_real_

## 5. Búsqueda en grilla con MAE en validación espacial -------------

for (i in seq_len(nrow(grid))) {
  mtry_i        <- grid$mtry[i]
  min_node_i    <- grid$min.node.size[i]
  
  cat("RF grid search - mtry =", mtry_i,
      "min.node.size =", min_node_i, "...\n")
  
  rf_i <- ranger(
    formula              = price ~ .,
    data                 = df_tr,
    num.trees            = 800,       # fuerte pero razonable
    mtry                 = mtry_i,
    min.node.size        = min_node_i,
    max.depth            = 12,        # fijo como pediste
    sample.fraction      = 0.8,
    respect.unordered.factors = "order",
    seed                 = 123,
    importance           = "impurity",
    write.forest         = TRUE
  )
  
  pred_val_i <- predict(rf_i, data = df_val)$predictions
  mae_val_i  <- mae_fun(df_val$price, pred_val_i)
  
  results_grid$MAE_val[i] <- mae_val_i
}

print(results_grid)

best_row_idx <- which.min(results_grid$MAE_val)
best_mtry    <- results_grid$mtry[best_row_idx]
best_min_node <- results_grid$min.node.size[best_row_idx]

cat("Mejores hiperparámetros RF:",
    "mtry =", best_mtry,
    ", min.node.size =", best_min_node, "\n")

## 6. Entrenar RF final en TODO el train ----------------------------

set.seed(123)
rf_final <- ranger(
  formula              = price ~ .,
  data                 = df_cart,
  num.trees            = 1000,
  mtry                 = best_mtry,
  min.node.size        = best_min_node,
  max.depth            = 12,
  sample.fraction      = 0.8,
  respect.unordered.factors = "order",
  seed                 = 123,
  importance           = "impurity",
  write.forest         = TRUE
)

# MAE in-sample
pred_train_rf <- predict(rf_final, data = df_cart)$predictions
mae_train_rf  <- mae_fun(df_cart$price, pred_train_rf)
cat("RF fuerte - MAE in-sample:", mae_train_rf, "\n")

## 7. Predicción en test y exportación ------------------------------

# Preparar df_test_cart con mismo tratamiento
vars_cart_test <- setdiff(vars_cart, "price")  # en test no hay price
vars_cart_test <- intersect(vars_cart_test, names(test_final))

df_test_cart <- test_final[, vars_cart_test, drop = FALSE]

for (v in names(df_test_cart)) {
  if (is.numeric(df_test_cart[[v]])) {
    df_test_cart[[v]][is.na(df_test_cart[[v]])] <- 0
  } else {
    df_test_cart[[v]] <- as.character(df_test_cart[[v]])
    df_test_cart[[v]][is.na(df_test_cart[[v]])] <- "missing"
    df_test_cart[[v]] <- factor(df_test_cart[[v]])
  }
}

pred_test_rf <- predict(rf_final, data = df_test_cart)$predictions

submission_rf <- data.frame(
  property_id = test_final$property_id,
  price       = pred_test_rf
)

write.csv(
  submission_rf,
  "submission_RF_fuerte_spatialCV_loc.csv",
  row.names = FALSE
)


## ================================================================
## SUPER LEARNER sl3 - PRICE (CONTINUOUS)
## ================================================================

library(data.table)
library(sl3)
library(future)


set.seed(2025)

## 0. Elegir covariables para el SL --------------------------------
## (Basadas en las que has usado en glmnet / RF / NN)

# Elegimos la variable de tipo propiedad según lo que exista
prop_var <- if ("property_type" %in% names(train_final)) {
  "property_type"
} else if ("property_type_final" %in% names(train_final)) {
  "property_type_final"
} else {
  NULL
}

sl_covars <- c(
  "ESTRATO",
  "surface_covered_final",
  "n_palabras_title",
  "n_lamparas_200m",
  "distancia_bus",
  "n_cafes_500m",
  prop_var,
  "is_residential",
  "cocina_integral",                # puede ser NULL; lo filtramos abajo
  "bedrooms_final_set2",
  "bathrooms_final_set2",
  "tiene_vigilancia_text",
  "menciona_cercania_txt",
  "menciona_cuadras_txt",
  "tiene_cocina",
  "cercania_cc",
  "tiene_garaje_text"
)

# Quitamos NAs y nos quedamos solo con las que existen en train_final
sl_covars <- sl_covars[!is.na(sl_covars)]
sl_covars <- sl_covars[sl_covars %in% names(train_final)]

## 1. Copias de trabajo + tratamiento de NAs -----------------------

train_sl <- as.data.table(train_final)
test_sl  <- as.data.table(test_final)

# Aseguramos que LocCodigo exista
if (!"LocCodigo" %in% names(train_sl)) stop("LocCodigo no está en train_final")
if (!"LocCodigo" %in% names(test_sl))  stop("LocCodigo no está en test_final")

# id espacial: sin NA y como character
train_sl[is.na(LocCodigo), LocCodigo := "missing_loc"]
test_sl[ is.na(LocCodigo), LocCodigo := "missing_loc"]

train_sl[, LocCodigo := as.character(LocCodigo)]
test_sl[ , LocCodigo := as.character(LocCodigo)]

# Imputación simple para las X
for (v in sl_covars) {
  if (is.numeric(train_sl[[v]]) || is.integer(train_sl[[v]])) {
    train_sl[is.na(get(v)), (v) := 0]
    test_sl[ is.na(get(v)), (v) := 0]
  } else {
    train_sl[is.na(get(v)), (v) := "missing"]
    test_sl[ is.na(get(v)), (v) := "missing"]
    train_sl[, (v) := as.factor(get(v))]
    test_sl[ , (v) := as.factor(get(v))]
  }
}

# Aseguramos que price no tenga NA
train_sl[is.na(price), price := 0]

## 2. Definir Task con id = LocCodigo (CV clusterizada espacial) ----

task <- sl3_Task$new(
  data         = train_sl,
  covariates   = sl_covars,
  outcome      = "price",
  outcome_type = "continuous",   # regresión
  id           = "LocCodigo"     # <-- CV agrupada por localidad
)

## 3. Definir learners individuales --------------------------------

# 3.0 Mean (PROMEDIO)
lrn_mean <- Lrnr_mean$new()

# 3.1 GLM (regresión lineal)
lrn_glm <- Lrnr_glm$new()  # usa familia gaussiana por outcome continuo

# 3.2 glmnet con alpha óptimo, dejando que el learner
#     genere su propia secuencia de lambdas (lambda path)
if (!exists("best_alpha")) {
  best_alpha <- 0.5  # fallback, por si acaso
}

lrn_glmnet <- Lrnr_glmnet$new(
  alpha   = best_alpha,
  nlambda = 50,           # tamaño de la grilla interna de lambdas
  family  = "gaussian"
)

# 3.3 CART sencillo (rpart)
lrn_cart <- Lrnr_rpart$new(
  cp       = 0.001,  # complejidad baja -> árbol algo detallado
  minsplit = 20,
  minbucket = 7,
  maxdepth = 12
)

# 3.4 Random Forest (ranger)
lrn_ranger <- Lrnr_ranger$new(
  num.trees      = 500,
  mtry           = min(5, length(sl_covars)),
  min.node.size  = 20,
  max.depth      = 12,
  verbose        = FALSE
)

# 3.5 Neural Net sencilla (nnet)
lrn_nnet <- Lrnr_nnet$new(
  size   = 8,
  linout = TRUE,
  maxit  = 500,
  trace  = FALSE
)

# Stack con TODOS los learners pedidos:
learners <- Stack$new(
  lrn_mean,    # promedio
  lrn_glm,     # regresión lineal
  lrn_glmnet,  # elastic net con alpha óptimo
  lrn_cart,    # CART sencillo
  lrn_ranger,  # RF fuerte
  lrn_nnet     # NN simple
)

## 4. Metalearner: NNLS (combinación convexa) ----------------------

metalearner <- Lrnr_nnls$new()

## 5. Definir Super Learner ----------------------------------------

sl <- Lrnr_sl$new(
  learners    = learners,
  metalearner = metalearner
)

## 6. Estimar Super Learner (con CV interna + id -> CV espacial) ----
plan(multisession, workers = 4)
options(future.globals.maxSize = +Inf)
set.seed(2025)
sl_fit <- sl$train(task = task)

## 7. Riesgo CV de cada learner (MSE) -------------------------------

cv_risk <- sl_fit$cv_risk(loss_squared_error)
print(cv_risk)

## 8. Predicciones in-sample + MAE ---------------------------------

y_hat_train <- sl_fit$predict(task = task)
mae_in_sample <- mean(abs(train_sl$price - y_hat_train))
cat("MAE in-sample SuperLearner:", mae_in_sample, "\n")

## 9. Predicción en test y export ----------------------------------

prediction_task <- sl3_Task$new(
  data       = test_sl,
  covariates = sl_covars
)

y_hat_test <- sl_fit$predict(task = prediction_task)

submission_sl <- data.frame(
  property_id = test_final$property_id,
  price       = as.vector(y_hat_test)
)

write.csv(
  submission_sl,
  "submission_sl3_locSpatial_mean_glm_glmnet_cart_ranger_nnet.csv",
  row.names = FALSE
)

cat("Submission saved as: submission_sl3_locSpatial_mean_glm_glmnet_cart_ranger_nnet.csv\n")
