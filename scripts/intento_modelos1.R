library(readxl)
test_final <- read_excel("stores/test_final.xlsx")
View(test_final)
library(readxl)
train_final <- read_excel("stores/train_final.xlsx")
View(train_final)


wd_main <- "C:/Users/Rodri/OneDrive/Rodri estudioso/GitHub/taller_3"
wd_data <- "/stores"
wd_code <- "/scripts"
wd_output <- "/views"

upz <- st_read(paste0(wd_main, wd_data, "/IndUPZ.json"))

## Definición de las variables:

# Variables de TEXTO (strings / categorías no dummy)

df_sel <- train_final %>%
  dplyr::select(price, all_of(x_forward))

# (Opcional) Ver cuántos NA hay por variable
na_por_var <- sapply(df_sel, function(x) sum(is.na(x)))
na_por_var

# 2. Quitar todas las filas con al menos un NA
df_sel_cc <- na.omit(df_sel)

nrow(df_sel)     # total original
nrow(df_sel_cc)  # total sin NAs

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

### 1. Variables para la NN ----------------------------------

nn_vars <- c(
  "ESTRATO",
  "n_palabras_title",
  "n_lamparas_200m",
  "n_cafes_500m",
  "distancia_bus",
  "tiene_sala",
  "tiene_patio_ropas",
  "tiene_vigilancia_text",
  "cocina_integral",
  "cocina_abierta",
  "sala_comedor_conjunto",
  "menciona_cercania_txt",
  "menciona_cuadras_txt",
  "is_residential",
  "remodelada_text",
  "property_type",
  "tipo_inmueble_text",
  "CODIGO_UPZ"   # UPZ como numérica, missing -> 0
)

nn_vars <- nn_vars[nn_vars %in% names(train_final)]

### 2. Copias de trabajo y tratamiento de NAs ----------------

train_nn <- train_final
test_nn  <- test_final

# 2.1 CODIGO_UPZ: missing -> 0
if ("CODIGO_UPZ" %in% nn_vars) {
  train_nn$CODIGO_UPZ[is.na(train_nn$CODIGO_UPZ)] <- 0
  test_nn$CODIGO_UPZ[is.na(test_nn$CODIGO_UPZ)]   <- 0
}

# Guardamos UPZ de train para CV espacial
upz_train <- train_nn$CODIGO_UPZ

# 2.2 NAs numéricos -> 0
for (v in nn_vars) {
  if (is.numeric(train_nn[[v]])) {
    train_nn[[v]][is.na(train_nn[[v]])] <- 0
    test_nn[[v]][is.na(test_nn[[v]])]   <- 0
  }
}

# 2.3 NAs textuales -> "missing"
for (v in nn_vars) {
  if (is.character(train_nn[[v]])) {
    train_nn[[v]][is.na(train_nn[[v]])] <- "missing"
    test_nn[[v]][is.na(test_nn[[v]])]   <- "missing"
  }
}

### 3. Construir matriz X (train+test) -----------------------

df_all <- bind_rows(
  train_nn[, nn_vars, drop = FALSE],
  test_nn[,  nn_vars, drop = FALSE]
)

# Identificar numéricas vs categóricas
num_cols <- sapply(df_all, is.numeric)
df_num   <- df_all[, num_cols, drop = FALSE]
df_cat   <- df_all[, !num_cols, drop = FALSE]

# Categóricas -> factor -> dummies
if (ncol(df_cat) > 0) {
  df_cat[] <- lapply(df_cat, factor)
  X_cat <- model.matrix(~ . - 1, data = df_cat)
} else {
  X_cat <- NULL
}

X_num <- as.matrix(df_num)

# Unimos numéricas + dummies
if (!is.null(X_cat)) {
  X_all <- cbind(X_num, X_cat)
} else {
  X_all <- X_num
}

# NAs residuales -> 0
X_all[is.na(X_all)] <- 0

n_train <- nrow(train_nn)

X_train_all <- X_all[1:n_train, , drop = FALSE]
X_test_all  <- X_all[(n_train + 1):nrow(X_all), , drop = FALSE]
y_train_all <- train_nn$price

### 4. Split espacial train/valid por UPZ --------------------

set.seed(123)

upz_nonzero <- sort(unique(upz_train[upz_train != 0]))
n_val_upz   <- max(1, round(0.2 * length(upz_nonzero)))
val_upz     <- sample(upz_nonzero, size = n_val_upz)

idx_val <- which(upz_train %in% val_upz)
idx_tr  <- setdiff(seq_len(n_train), idx_val)

X_tr  <- X_train_all[idx_tr, , drop = FALSE]
X_val <- X_train_all[idx_val, , drop = FALSE]
y_tr  <- y_train_all[idx_tr]
y_val <- y_train_all[idx_val]

### 5. Escalamiento (muy importante en NN) -------------------

scale_train <- function(X) {
  mu <- colMeans(X)
  sd <- apply(X, 2, sd)
  sd[sd == 0] <- 1
  Xs <- sweep(sweep(X, 2, mu, "-"), 2, sd, "/")
  list(X = Xs, mu = mu, sd = sd)
}

scale_apply <- function(X, mu, sd) {
  sweep(sweep(X, 2, mu, "-"), 2, sd, "/")
}

sc_tr <- scale_train(X_tr)

X_tr_sc     <- sc_tr$X
X_val_sc    <- scale_apply(X_val,       sc_tr$mu, sc_tr$sd)
X_train_sc  <- scale_apply(X_train_all, sc_tr$mu, sc_tr$sd)
X_test_sc   <- scale_apply(X_test_all,  sc_tr$mu, sc_tr$sd)

### 6. Grid search: número de neuronas (hidden) ---------------

sizes   <- c(3, 5, 7, 10)  # puedes ampliar luego si quieres
results <- data.frame(size = sizes, MAE_val = NA_real_)

for (s in sizes) {
  cat("Entrenando neuralnet con", s, "neuronas (validación por UPZ)...\n")
  
  train_tr_df <- data.frame(price = y_tr, X_tr_sc)
  val_df      <- data.frame(X_val_sc)
  
  nn_fit <- neuralnet(
    formula       = price ~ .,
    data          = train_tr_df,
    hidden        = s,
    linear.output = TRUE,
    stepmax       = 1e+06,
    lifesign      = "minimal"
  )
  
  pred_val <- as.vector(compute(nn_fit, val_df)$net.result)
  mae_val  <- mean(abs(y_val - pred_val))
  
  results[results$size == s, "MAE_val"] <- mae_val
}

print(results)

best_size <- results$size[which.min(results$MAE_val)]
cat("Mejor número de neuronas en la capa oculta:", best_size, "\n")

### 7. Modelo final con TODO el train ------------------------

train_full_df <- data.frame(price = y_train_all, X_train_sc)

set.seed(123)
nn_final <- neuralnet(
  formula       = price ~ .,
  data          = train_full_df,
  hidden        = best_size,
  linear.output = TRUE,
  stepmax       = 1e+06,
  lifesign      = "minimal"
)

# MAE in-sample
pred_train_final <- as.vector(compute(nn_final, data.frame(X_train_sc))$net.result)
mae_train_final  <- mean(abs(y_train_all - pred_train_final))
cat("MAE in-sample (modelo NN final):", mae_train_final, "\n")

### 8. Predicción en test y export ---------------------------

pred_test_nn <- as.vector(compute(nn_final, data.frame(X_test_sc))$net.result)

submission_nn <- data.frame(
  property_id = test_final$property_id,
  price       = pred_test_nn
)

write.csv(
  submission_nn,
  "submission_neuralnet_UPZspatial_singlelayer.csv",
  row.names = FALSE
)

library(dplyr)
library(neuralnet)

### 1. Variables para la NN ----------------------------------

nn_vars <- c(

  "n_palabras_title",
  "n_cafes_500m",
  "distancia_bus"   # UPZ como numérica, missing -> 0
)

nn_vars <- nn_vars[nn_vars %in% names(train_final)]

### 2. Copias de trabajo y tratamiento de NAs ----------------

train_nn <- train_final
test_nn  <- test_final

# 2.1 CODIGO_UPZ: missing -> 0
if ("CODIGO_UPZ" %in% nn_vars) {
  train_nn$CODIGO_UPZ[is.na(train_nn$CODIGO_UPZ)] <- 0
  test_nn$CODIGO_UPZ[is.na(test_nn$CODIGO_UPZ)]   <- 0
}

# Guardamos UPZ de train para CV espacial
upz_train <- train_nn$CODIGO_UPZ

# 2.2 NAs numéricos -> 0
for (v in nn_vars) {
  if (is.numeric(train_nn[[v]])) {
    train_nn[[v]][is.na(train_nn[[v]])] <- 0
    test_nn[[v]][is.na(test_nn[[v]])]   <- 0
  }
}

# 2.3 NAs textuales -> "missing"
for (v in nn_vars) {
  if (is.character(train_nn[[v]])) {
    train_nn[[v]][is.na(train_nn[[v]])] <- "missing"
    test_nn[[v]][is.na(test_nn[[v]])]   <- "missing"
  }
}

### 3. Construir matriz X (train+test) -----------------------

df_all <- bind_rows(
  train_nn[, nn_vars, drop = FALSE],
  test_nn[,  nn_vars, drop = FALSE]
)

# Identificar numéricas vs categóricas
num_cols <- sapply(df_all, is.numeric)
df_num   <- df_all[, num_cols, drop = FALSE]
df_cat   <- df_all[, !num_cols, drop = FALSE]

# Categóricas -> factor -> dummies
if (ncol(df_cat) > 0) {
  df_cat[] <- lapply(df_cat, factor)
  X_cat <- model.matrix(~ . - 1, data = df_cat)
} else {
  X_cat <- NULL
}

X_num <- as.matrix(df_num)

# Unimos numéricas + dummies
if (!is.null(X_cat)) {
  X_all <- cbind(X_num, X_cat)
} else {
  X_all <- X_num
}

# NAs residuales -> 0
X_all[is.na(X_all)] <- 0

n_train <- nrow(train_nn)

X_train_all <- X_all[1:n_train, , drop = FALSE]
X_test_all  <- X_all[(n_train + 1):nrow(X_all), , drop = FALSE]
y_train_all <- train_nn$price

### 4. Split espacial train/valid por UPZ --------------------

set.seed(123)

upz_nonzero <- sort(unique(upz_train[upz_train != 0]))
n_val_upz   <- max(1, round(0.2 * length(upz_nonzero)))
val_upz     <- sample(upz_nonzero, size = n_val_upz)

idx_val <- which(upz_train %in% val_upz)
idx_tr  <- setdiff(seq_len(n_train), idx_val)

X_tr  <- X_train_all[idx_tr, , drop = FALSE]
X_val <- X_train_all[idx_val, , drop = FALSE]
y_tr  <- y_train_all[idx_tr]
y_val <- y_train_all[idx_val]

### 5. Escalamiento (muy importante en NN) -------------------

scale_train <- function(X) {
  mu <- colMeans(X)
  sd <- apply(X, 2, sd)
  sd[sd == 0] <- 1
  Xs <- sweep(sweep(X, 2, mu, "-"), 2, sd, "/")
  list(X = Xs, mu = mu, sd = sd)
}

scale_apply <- function(X, mu, sd) {
  sweep(sweep(X, 2, mu, "-"), 2, sd, "/")
}

sc_tr <- scale_train(X_tr)

X_tr_sc     <- sc_tr$X
X_val_sc    <- scale_apply(X_val,       sc_tr$mu, sc_tr$sd)
X_train_sc  <- scale_apply(X_train_all, sc_tr$mu, sc_tr$sd)
X_test_sc   <- scale_apply(X_test_all,  sc_tr$mu, sc_tr$sd)

### 6. Grid search: número de neuronas (hidden) ---------------

sizes   <- c(3, 5, 7, 10)  # puedes ampliar luego si quieres
results <- data.frame(size = sizes, MAE_val = NA_real_)

for (s in sizes) {
  cat("Entrenando neuralnet con", s, "neuronas (validación por UPZ)...\n")
  
  train_tr_df <- data.frame(price = y_tr, X_tr_sc)
  val_df      <- data.frame(X_val_sc)
  
  nn_fit <- neuralnet(
    formula       = price ~ .,
    data          = train_tr_df,
    hidden        = s,
    linear.output = TRUE,
    stepmax       = 1e+06,
    lifesign      = "minimal"
  )
  
  pred_val <- as.vector(compute(nn_fit, val_df)$net.result)
  mae_val  <- mean(abs(y_val - pred_val))
  
  results[results$size == s, "MAE_val"] <- mae_val
}

print(results)

best_size <- results$size[which.min(results$MAE_val)]
cat("Mejor número de neuronas en la capa oculta:", best_size, "\n")

### 7. Modelo final con TODO el train ------------------------

train_full_df <- data.frame(price = y_train_all, X_train_sc)

set.seed(123)
nn_final <- neuralnet(
  formula       = price ~ .,
  data          = train_full_df,
  hidden        = best_size,
  linear.output = TRUE,
  stepmax       = 1e+06,
  lifesign      = "minimal"
)

# MAE in-sample
pred_train_final <- as.vector(compute(nn_final, data.frame(X_train_sc))$net.result)
mae_train_final  <- mean(abs(y_train_all - pred_train_final))
cat("MAE in-sample (modelo NN final):", mae_train_final, "\n")

### 8. Predicción en test y export ---------------------------

pred_test_nn <- as.vector(compute(nn_final, data.frame(X_test_sc))$net.result)

submission_nn <- data.frame(
  property_id = test_final$property_id,
  price       = pred_test_nn
)

write.csv(
  submission_nn,
  "submission_neuralnet_UPZspatial_singlelayer.csv",
  row.names = FALSE
)


