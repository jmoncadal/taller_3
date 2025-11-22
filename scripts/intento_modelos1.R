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


#### Exportnado el modelo ======== 

vars_enet <- all.vars(form_enet)
vars_enet <- vars_enet[vars_enet != "price"]

for (v in vars_enet) {
  if (is.factor(train_final[[v]])) {
    # obligar a test_final a tener los mismos levels que train_final
    test_final[[v]] <- factor(test_final[[v]], levels = levels(train_final[[v]]))
  }
}

# Matriz de diseño TRAIN (sin intercepto)
X_train <- model.matrix(form_enet, data = train_final)[, -1]

# Outcome
y_train <- train_final$price

dim(X_train)

X_test <- model.matrix(form_enet, data = test_final)[, -1]

dim(X_test)

enet_final <- glmnet(
  x      = X_train,
  y      = y_train,
  alpha  = best_alpha,      # = 1
  lambda = best_lambda,     # = 1407382
  family = "gaussian"
)

pred_train <- as.vector(
  predict(enet_final, newx = X_train, s = best_lambda)
)

rmse_train <- sqrt(mean((y_train - pred_train)^2))
mae_train  <- mean(abs(y_train - pred_train))

rmse_train
mae_train

pred_test <- as.vector(
  predict(enet_final, newx = X_test, s = best_lambda)
)

head(pred_test)

submission <- data.frame(
  property_id = test_final$property_id, 
  price       = pred_test
)

write.csv(
  submission,
  "submission_elastic_net_UPZ_MAE_optimal.csv",
  row.names = FALSE
)




