# Taller 3 - Big Data y ML ------------------------------------------------

rm(list = ls())

# Importing libraries -----------------------------------------------------

library(pacman)
p_load(rio, writexl, readxl, tidyverse, caret, keras,
       reticulate, tidymodels, sf, gt, gtsummary, osmdata,
       gridExtra, plotly, 
       skimr, leaflet, lwgeom)

# Establishing paths ------------------------------------------------------

wd_main <- "C:/Users/Usuario/Documents/Andes/taller 3/taller_3"
wd_data <- "/stores"
wd_code <- "/scripts"
wd_output <- "/views"

correr <- 0

# Importing data ----------------------------------------------------------
if (correr == 1){
  test <- read.csv(paste0(wd_main, wd_data, "/test.csv"))
  train <- read.csv(paste0(wd_main, wd_data, "/train.csv"))
  
  upz <- st_read(paste0(wd_main, wd_data, "/IndUPZ.json"))
  estratos <- st_read(paste0(wd_main, wd_data, "/estratos"))
  
  test_enriched <- read.csv(paste0(wd_main, wd_data, "/test_enriched2.csv")) %>% 
    select(-c("city", "month", "year", "property_type", "operation_type", "lat", "lon", "title",
              "description", "description_raw"))
  train_enriched <- read.csv(paste0(wd_main, wd_data, "/train_enriched2.csv"))  %>% 
    select(-c("city", "month", "year", "property_type", "operation_type", "lat", "lon", "title",
              "description", "description_raw"))
  
  catastro <- read_xlsx(paste0(wd_main, wd_data, "/consolidado_localidades.xlsx"))
  
  # Data --------------------------------------------------------------------
  
  # Changing data type
  
  train$property_type <- as.factor(train$property_type)
  test$property_type <- as.factor(test$property_type)
  
  train$year <- as.factor(train$year)
  test$year <- as.factor(test$year)
  
  # Missing values
  
  missings <- skim(train) %>% 
    select(skim_variable, n_missing) %>% 
    dplyr::filter(n_missing!= 0)
  
  # Early exploration
  
  # Solo hay dos valores de vivienda: casa y apartamento. Todos los datos son
  # de Bogotá. Podemos eliminar esas variables desde que son innecesarias.
  unique(train$operation_type)
  unique(train$property_type)
  
  train_nm <- train %>%
    select(-c(operation_type, city,
              title, description)) %>% 
    drop_na(price, surface_total)
  
  # Las casas tienden a tener precios más altos, pero hay muchos valores atípicos
  # para los apartamentos.
  
  ggplot(train) +
    geom_boxplot(aes(x = property_type, y = price, fill = property_type)) +
    xlab("") +
    ylab("Precio") +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Spatial modeling --------------------------------------------------------
  
  train <- train %>% 
    st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  
  test <- test %>% 
    st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  
  # Joining with Estrato
  
  estratos <- st_transform(estratos, st_crs(train))
  estratos <- st_make_valid(estratos)
  
  train <- st_join(train, estratos, join = st_intersects)
  test  <- st_join(test,  estratos, join = st_intersects)
  
  # Joining with UPZ
  upz <- st_transform(upz, 4326)
  
  train <- st_join(train, upz, join = st_intersects)
  test <- st_join(test, upz, join = st_intersects)
  
  # Joining two dataframes
  
  train <- left_join(train, train_enriched, by = "property_id")
  test <- left_join(test, test_enriched, by = "property_id")
  
  # Data cleaning
  
  train <- train %>% 
    mutate(year = as.factor(year),
           property_type = as.factor(property_type),
           CODIGO_UPZ = as.factor(CODIGO_UPZ),
           NOMBRE = as.factor(NOMBRE))
  
  test <- test %>% 
    mutate(year = as.factor(year),
           property_type = as.factor(property_type),
           CODIGO_UPZ = as.factor(CODIGO_UPZ),
           NOMBRE = as.factor(NOMBRE))
  
  # Room imputation
  
  # Moda sencilla (para character o factor)
  get_mode <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA)
    
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  impute_numeric_by_cat <- function(data, numeric_vars, group_vars) {
    data <- as.data.frame(data)
    
    # Validaciones básicas
    missing_g <- setdiff(group_vars, names(data))
    if (length(missing_g) > 0) {
      stop("Estas variables de agrupación no existen en el data: ",
           paste(missing_g, collapse = ", "))
    }
    
    missing_n <- setdiff(numeric_vars, names(data))
    if (length(missing_n) > 0) {
      stop("Estas variables numéricas no existen en el data: ",
           paste(missing_n, collapse = ", "))
    }
    
    # Clave de grupo a partir de las variables categóricas
    key <- interaction(data[group_vars], drop = TRUE, lex.order = TRUE)
    
    for (v in numeric_vars) {
      if (!is.numeric(data[[v]])) {
        warning("La variable ", v, " no es numérica (clase: ",
                paste(class(data[[v]]), collapse = ", "),
                "). Se intenta igual, pero revísalo.")
      }
      
      # Indicador de NA original
      flag_name <- paste0(v, "_was_na")
      data[[flag_name]] <- as.integer(is.na(data[[v]]))
      
      # Promedio global
      global_mean <- mean(data[[v]], na.rm = TRUE)
      
      # Promedio por grupo
      group_means <- tapply(data[[v]], key, function(z) mean(z, na.rm = TRUE))
      gm_vec <- group_means[as.character(key)]  # alineado por fila
      
      x <- data[[v]]
      na_idx <- is.na(x)
      
      # Candidato: primero media de su grupo, si es NA -> media global
      candidate <- gm_vec
      candidate[is.na(candidate)] <- global_mean
      
      x[na_idx] <- candidate[na_idx]
      data[[v]] <- x
    }
    
    return(data)
  }
  
  impute_categorical_by_cat <- function(data, cat_vars, group_vars) {
    data <- as.data.frame(data)
    
    # Validaciones
    missing_g <- setdiff(group_vars, names(data))
    if (length(missing_g) > 0) {
      stop("Estas variables de agrupación no existen en el data: ",
           paste(missing_g, collapse = ", "))
    }
    
    missing_c <- setdiff(cat_vars, names(data))
    if (length(missing_c) > 0) {
      stop("Estas variables categóricas no existen en el data: ",
           paste(missing_c, collapse = ", "))
    }
    
    # Clave de grupo
    key <- interaction(data[group_vars], drop = TRUE, lex.order = TRUE)
    
    for (v in cat_vars) {
      # Indicador de NA original
      flag_name <- paste0(v, "_was_na")
      data[[flag_name]] <- as.integer(is.na(data[[v]]))
      
      x <- data[[v]]
      is_fac <- is.factor(x)
      
      # Trabajamos en character para no enredarnos con los niveles del factor
      x_chr <- as.character(x)
      
      # Moda global
      global_mode <- get_mode(x_chr)
      if (is.na(global_mode)) {
        warning("La variable ", v, " tiene solo NA; no se pudo calcular moda global.")
      }
      
      # Moda por grupo
      group_modes <- tapply(x_chr, key, get_mode)
      gm_vec <- group_modes[as.character(key)]
      
      na_idx <- is.na(x_chr)
      
      candidate <- gm_vec
      candidate[is.na(candidate)] <- global_mode
      
      x_chr[na_idx] <- candidate[na_idx]
      
      # Volvemos a la clase original
      if (is_fac) {
        data[[v]] <- factor(x_chr)
      } else {
        data[[v]] <- x_chr
      }
    }
    
    return(data)
  }
  
  # 2. Imputar categóricas por moda condicional
  train2 <- impute_categorical_by_cat(
    data      = train,
    cat_vars  = c("ESTRATO"),
    group_vars = c("CODIGO_UPZ", "property_type", "month", "year", "tiene_sala","tiene_terraza")
  )
  
  train2 <- impute_categorical_by_cat(
    data      = train2,
    cat_vars  = c("rooms.y"),
    group_vars = c("CODIGO_UPZ", "property_type", "month", "year", "tiene_sala","tiene_terraza")
  )
  
  train2 <- impute_categorical_by_cat(
    data      = train2,
    cat_vars  = c("bedrooms_final_set2"),
    group_vars = c("CODIGO_UPZ", "property_type", "month", "year", "tiene_sala","tiene_terraza")
  )
  
  train2 <- impute_categorical_by_cat(
    data      = train2,
    cat_vars  = c("bathrooms_final_set2"),
    group_vars = c("CODIGO_UPZ", "property_type", "month", "year", "tiene_sala","tiene_terraza")
  )
  
  # Evaluación imputación train
  
  missings_train <- skim(train) %>% 
    select(skim_variable, n_missing) %>% 
    dplyr::filter(n_missing!= 0) %>% 
    arrange(n_missing)
  
  missings_train2 <- skim(train2) %>% 
    select(skim_variable, n_missing) %>% 
    dplyr::filter(n_missing!= 0) %>% 
    arrange(n_missing)
  
  # Evaluación imputación test
  
  # 2. Imputar categóricas por moda condicional
  test2 <- impute_categorical_by_cat(
    data      = test,
    cat_vars  = c("ESTRATO", "rooms.y", "bedrooms_final_set2", "bathrooms_final_set2"),
    group_vars = c("CODIGO_UPZ", "property_type", "month", "year", "tiene_sala","tiene_terraza")
  )
  
  # Evaluación imputación tests
  
  missings_test <- skim(test) %>% 
    select(skim_variable, n_missing) %>% 
    dplyr::filter(n_missing!= 0) %>% 
    arrange(n_missing)
  
  missings_test2 <- skim(test2) %>% 
    select(skim_variable, n_missing) %>% 
    dplyr::filter(n_missing!= 0) %>% 
    arrange(n_missing)
  
  # Definitve databases
  
  train <- train2 %>%
    rename(price = price.y,
           surface_total = surface_total.y,
           surface_covered = surface_covered.y,
           OBJECTID = OBJECTID.x)
  
  test <- test2 %>%
    rename(price = price.y,
           surface_total = surface_total.y,
           surface_covered = surface_covered.y,
           OBJECTID = OBJECTID.x)
  
  # Spatial modeling --------------------------------------------------------
  
  train <- train %>% 
    st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  
  test <- test %>% 
    st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  
  # Variable creation
  
  #  train <- train %>%
  #    drop_na(price, surface_total) %>% 
  #    mutate(p_msq = price/surface_total)
  
  
  # Creating map visualization
  
  train <- train %>% 
    mutate(color = case_when(property_type == "Apartamento" ~ "#00008B",
                             property_type == "Casa" ~ "#8B0000"))
  
  latitud_central <- mean(train$lat)
  longitud_central <- mean(train$lon)
  
  # leaflet() %>% 
  #   addTiles() %>% 
  #   setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>% 
  #   addCircles(lng = train$lon,
  #              lat = train$lat,
  #              col = train$color,
  #              fillOpacity = 1,
  #              opacity = 1,
  #              radius = 1)
  
  # Precio metro cuadrado apartamentos
  
  #  ggplot() +
  #    geom_sf(data = upz, fill = "gray95", color = "black") +
  #    geom_sf(data = train %>% 
  #            dplyr::filter(property_type == "Apartamento"),
  #            aes(color = p_msq),
  #            shape = 16, size = 0.8, alpha = 0.7) +
  #    scale_color_gradient(low = "#00008B", high = "#8B0000", name = "Precio por m²") +
  #    theme_minimal() +
  #    theme(axis.text = element_blank(),
  #          axis.title = element_blank(),
  #          legend.position = "right") +
  #  labs(title = "Precio por metro cuadrado — Apartamento")
  
  # Precio metro cuadrado casas
  
  #  ggplot() +
  #    geom_sf(data = upz, fill = "gray95", color = "black") +
  #    geom_sf(data = train %>% 
  #            dplyr::filter(property_type == "Casa"),
  #            aes(color = p_msq),
  #            shape = 16, size = 0.8, alpha = 0.7) +
  #    scale_color_gradient(low = "#00008B", high = "#8B0000", name = "Precio por m²") +
  #    theme_minimal() +
  #    theme(axis.text = element_blank(),
  #          axis.title = element_blank(),
  #          legend.position = "right") +
  #    labs(title = "Precio por metro cuadrado — Casas")
  
  # Distribución viviendas por UPZ
  
  ggplot() +
    geom_sf(data = upz, fill = "gray95", color = "black") +
    geom_sf(data = train, aes(color = as.factor(CODIGO_UPZ)))
  
  # Spatial data ------------------------------------------------------------
  
  osmdata::available_features()
  amenities <- osmdata::available_tags("amenity")
  leisure <- osmdata::available_tags("leisure")
  public_transport <- osmdata::available_tags("public_transport")
  highway <- osmdata::available_tags("highway")
  landuse <- osmdata::available_tags("landuse")
  
  # Distancia al café más cercano -------------------------------------------
  
  cafes <- opq(bbox = getbb("Bogota Colombia")) |>
    add_osm_feature(key = "amenity", value = "cafe")
  
  # Simple features
  cafes_sf <- osmdata_sf(cafes)
  cafes_geometria <- cafes_sf$osm_points |>
    dplyr::select(osm_id, name)
  cafes_geometria <- st_as_sf(cafes_geometria)
  
  # Coordenadas x,y de cada café
  cafes_geometria <- cafes_geometria |>
    mutate(x = st_coordinates(cafes_geometria)[, "X"]) |>
    mutate(y = st_coordinates(cafes_geometria)[, "Y"])
  
  # Objetos sf en 4326
  cafes_sf <- st_as_sf(cafes_geometria, coords = c("x", "y"), crs = 4326)
  train <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)
  
  # Matriz de distancias
  dist_matrix_cafe <- st_distance(x = train, y = cafes_sf)
  dim(dist_matrix_cafe)   
  
  # Distancia mínima al café más cercano para cada observación
  dist_min_cafe <- apply(dist_matrix_cafe, 1, min)
  
  train$distancia_cafe <- as.numeric(dist_min_cafe)   # distancia en metros
  
  # test
  test <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)
  dist_matrix_cafe_test <- st_distance(x = test, y = cafes_sf)
  dist_min_cafe_test <- apply(dist_matrix_cafe_test, 1, min)
  test$distancia_cafe <- as.numeric(dist_min_cafe_test)
  
  # Scatter con la relación entre el precio del inmueble y distancia a cafes
  ggplot(train, aes(x = distancia_cafe, y = price)) +
    geom_point(color = "darkblue", alpha = 0.4) +
    theme_classic()
  
  # ------- Cantidad de cafes en 500 metros
  # Convertir train y cafés al CRS de metros
  train_m <- st_transform(train, 3116)
  cafes_sf_m <- st_transform(cafes_sf, 3116)
  
  # Buffer
  radio_buffer <- 500
  train_buffer <- st_buffer(train_m, dist = radio_buffer)
  
  # Cafes dentro del buffer para cada obs
  intersections <- st_intersects(train_buffer, cafes_sf_m)
  
  # Número de cafés en el buffer
  n_cafes_500m <- lengths(intersections)
  
  train$n_cafes_500m <- n_cafes_500m
  
  #Scatter
  ggplot(train, aes(x = n_cafes_500m, y = price)) +
    geom_point(color = "darkblue", alpha = 0.4) +
    theme_classic()
  
  # test
  test_sf_m <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326) |>
    st_transform(3116)
  test_buffer_cafe <- st_buffer(test_sf_m, dist = radio_buffer)
  intersections_cafe_test <- st_intersects(test_buffer_cafe, cafes_sf_m)
  n_cafes_500m_test <- lengths(intersections_cafe_test)
  test$n_cafes_500m <- n_cafes_500m_test
  
  # Distancia a estación de transporte --------------------------------------
  
  bus <- opq(bbox = getbb("Bogota Colombia")) |>
    add_osm_feature(key = "amenity", value = "bus_station")
  
  # Simple features
  bus_sf_raw <- osmdata_sf(bus)
  bus_geometria <- bus_sf_raw$osm_points |>
    dplyr::select(osm_id, name)
  bus_geometria <- st_as_sf(bus_geometria)
  
  # Coordenadas
  bus_geometria <- bus_geometria |>
    mutate(x = st_coordinates(bus_geometria)[, "X"],
           y = st_coordinates(bus_geometria)[, "Y"])
  
  bus_sf <- st_as_sf(bus_geometria, coords = c("x", "y"), crs = 4326)
  
  # Matriz
  dist_matrix_bus <- st_distance(x = train, y = bus_sf)
  dim(dist_matrix_bus)
  
  # Distancia mínima a un bus stop para cada observación
  dist_min_bus <- apply(dist_matrix_bus, 1, min)
  
  train$distancia_bus <- as.numeric(dist_min_bus)
  
  #test
  dist_matrix_bus_test <- st_distance(x = test, y = bus_sf)
  dist_min_bus_test <- apply(dist_matrix_bus_test, 1, min)
  test$distancia_bus <- as.numeric(dist_min_bus_test)
  
  # Distancia a parque más cercano ------------------------------------------
  
  #parques <- opq(bbox = getbb("Bogota Colombia")) |>
  #add_osm_feature(key = "leisure", value = "park")
  
  # Simple features
  #parques_sf_raw <- osmdata_sf(parques)
  #parques_geometria <- parques_sf_raw$osm_points |>
  #dplyr::select(osm_id, name)
  #parques_geometria <- st_as_sf(parques_geometria)
  
  # Coordenadas
  #parques_geometria <- parques_geometria |>
  # mutate(x = st_coordinates(parques_geometria)[, "X"],
  #      y = st_coordinates(parques_geometria)[, "Y"])
  #parques_sf <- st_as_sf(parques_geometria, coords = c("x", "y"), crs = 4326)
  
  # Matriz
  #dist_matrix_parques <- st_distance(x = train, y = parques_sf)
  #dim(dist_matrix_parques)
  
  # Distancia mínima a un parque por observación
  #dist_min_parque <- apply(dist_matrix_parques, 1, min)
  
  #train$distancia_parque <- as.numeric(dist_min_parque)
  
  # Iluminación con buffer --------------------------------------------------
  
  lamparas <- opq(bbox = getbb("Bogota Colombia")) |>
    add_osm_feature(key = "highway", value = "street_lamp")
  
  # sf
  lamparas_sf_raw <- osmdata_sf(lamparas)
  
  # Seleccionar puntos
  lamparas_geometria <- lamparas_sf_raw$osm_points |>
    dplyr::select(osm_id)
  
  # sf
  lamparas_geometria <- st_as_sf(lamparas_geometria)
  
  # Coordenadas
  lamparas_geometria <- lamparas_geometria |>
    mutate(x = st_coordinates(lamparas_geometria)[, "X"],
           y = st_coordinates(lamparas_geometria)[, "Y"])
  
  lamparas_sf_m <- st_as_sf(lamparas_geometria, coords = c("x", "y"), crs = 4326) |>
    st_transform(3116)
  
  # Buffer
  radio_buffer <- 200  # metros
  train_buffer_ilum <- st_buffer(train_m, dist = radio_buffer)
  intersections_lamps <- st_intersects(train_buffer_ilum, lamparas_sf_m)
  
  # Cantidad de luces dentro del buffer
  n_lamparas_200m <- lengths(intersections_lamps)
  
  train$n_lamparas_200m <- n_lamparas_200m
  
  #test
  test_sf_m <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326) |>
    st_transform(3116)
  test_buffer_ilum <- st_buffer(test_sf_m, dist = radio_buffer)
  intersections_lamps_test <- st_intersects(test_buffer_ilum, lamparas_sf_m)
  n_lamparas_200m_test <- lengths(intersections_lamps_test)
  test$n_lamparas_200m <- n_lamparas_200m_test
  
  # Zona residencial --------------------------------------------------------
  
  residential <- opq(bbox = getbb("Bogota Colombia")) |>
    add_osm_feature(key = "landuse", value = "residential")
  
  # sf
  residential_sf <- osmdata_sf(residential)
  
  # Polígonos residenciales
  residential_polygons <- residential_sf$osm_polygons
  residential_relation <- st_within(train, residential_polygons)
  
  # 1 si el inumeble está en zona residencial, 0 si no
  train$is_residential <- as.integer(lengths(residential_relation) > 0)
  
  #test
  test_sf <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)
  residential_relation_test <- st_within(test_sf, residential_polygons)
  test$is_residential <- as.integer(lengths(residential_relation_test) > 0)
  
  
  # Exporting models --------------------------------------------------------
  
  write_xlsx(train, paste0(wd_main, wd_data, "/train_final.xlsx"))
  write_xlsx(test, paste0(wd_main, wd_data, "/test_final.xlsx"))
} else {
  # Models ------------------------------------------------------------------
  
  test <- read_xlsx(paste0(wd_main, wd_data, "/test_final.xlsx"))
  train <- read_xlsx(paste0(wd_main, wd_data, "/train_final.xlsx"))
}

# Cross validation folds ---------------------------------------------------

p_load("spatialsample")

p_load("caret")

train <- train %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

test <- test %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

localidades <- st_read(paste0(wd_main, wd_data, "/localidades.geojson"))
localidades <- st_transform(localidades, 4326)

train_sf <- st_join(
  train,
  localidades %>% select(LocCodigo, LocNombre),
  left = TRUE
)

test_sf <- st_join(
  test,
  localidades %>% select(LocCodigo, LocNombre),
  left = TRUE
)


set.seed(123)

location_folds  <- spatial_leave_location_out_cv(
  data  = train_sf,
  group = LocCodigo  # o LocNombre, como prefieras agrupar
  # v = dplyr::n_distinct(train_sf$LocCodigo)  # opcional, número de folds
)

folds<-list()

for(i in 1:nrow(location_folds)){
  folds[[i]]<- location_folds$splits[[i]]$in_id
}

fitControl<-trainControl(method ="cv",
                         index=folds)

# Model ------------------------------------------

# OLS **********************

OLS<-train(log(price) ~ 
             ESTRATO +
             factor(tiene_terraza) +
             property_type +
             factor(garaje_indep_text) +
             factor(garaje_cubierto_text) +
             factor(terraza_propia) +
             factor(sala_comedor_conjunto) +
             factor(tiene_sala) +
             factor(cocina_integral) +
             n_lamparas_200m +
             menciona_cercania_txt +
             tiene_balcon +
             tiene_patio_ropas +
             cocina_abierta +
             factor(menciona_cuadras_txt) +
             n_cafes_500m +
             tiene_vigilancia_text +
             cercania_bus_text +
             is_residential +
             remodelada_text +
             distancia_bus +
             n_palabras_title,
           data=train_sf,
           method = 'lm', 
           trControl = fitControl,
           metric = "MAE"
)

OLS

predictSample <- test %>%
  mutate(
    pred_log_price  = predict(OLS, newdata = test),   # predicción en log
    price      = exp(pred_log_price)             # volver al nivel
  ) %>%
  select(property_id, price)

predictSample <- predictSample %>% 
  st_drop_geometry()

# * Subido * 
write.csv(predictSample,paste0(wd_main, wd_output,"/estimacion_ols_inicial.csv"), row.names = FALSE)

# Model 2 OLS*****************

# -----------------
# Proceso en el medio
catastro <- read_xlsx(paste0(wd_main, wd_data, "/consolidado_localidades.xlsx"))

catastro_long <- catastro |>
  pivot_longer(
    cols = c(Casa, Apartamento),          # columnas de precios
    names_to = "property_type",           # nombre del tipo de vivienda
    values_to = "precio_catastro_prom"    # valor numérico
  )

train_sf <- train_sf |>
  left_join(
    catastro_long,
    by = c("LocNombre", "property_type")
  )

train_sf <- impute_numeric_by_cat(
  data        = train_sf,
  numeric_vars = c("precio_catastro_prom.y"),
  group_vars   = c("CODIGO_UPZ", "property_type", "month", "year")
)

test_sf <- test_sf |>
  left_join(
    catastro_long,
    by = c("LocNombre", "property_type")
  )

test_sf <- impute_numeric_by_cat(
  data        = test_sf,
  numeric_vars = c("precio_catastro_prom"),
  group_vars   = c("CODIGO_UPZ", "property_type", "month", "year")
)

train <- impute_categorical_by_cat(
  data      = train,
  cat_vars  = c("CODIGO_UPZ"),
  group_vars = c("ESTRATO","CODIGO_UPZ", "property_type", "month", "year", "tiene_sala","tiene_terraza")
)

test <- impute_categorical_by_cat(
  data      = test,
  cat_vars  = c("CODIGO_UPZ"),
  group_vars = c("CODIGO_UPZ", "property_type", "month", "year", "tiene_sala","tiene_terraza")
)

# ----------------


OLS2<-train(log(price) ~ 
             CODIGO_UPZ +
             ESTRATO +
             tiene_terraza +
             property_type +
             garaje_indep_text +
             garaje_cubierto_text +
             terraza_propia +
             sala_comedor_conjunto +
             tiene_sala +
             cocina_integral +
             n_lamparas_200m +
             menciona_cercania_txt +
             tiene_balcon +
             tiene_patio_ropas +
             cocina_abierta +
             menciona_cuadras_txt +
             n_cafes_500m +
             tiene_vigilancia_text +
             cercania_bus_text +
             is_residential +
             remodelada_text +
             distancia_bus +
             n_palabras_title,
           data=train,
           method = 'lm', 
           trControl = fitControl,
           metric = "MAE"
)

OLS2

predictSample <- test %>%
  mutate(
    pred_log_price  = predict(OLS, newdata = test),   # predicción en log
    price      = exp(pred_log_price)             # volver al nivel
  ) %>%
  select(property_id, price)
# NO LO SUBIMOS

# Model 3 OLS*****************

OLS3<-train(price ~ 
              precio_catastro_prom.y+
              ESTRATO + ESTRATO:tiene_terraza + tiene_terraza +
              property_type +
              garaje_indep_text +
              garaje_cubierto_text +
              terraza_propia +
              sala_comedor_conjunto +
              tiene_sala +
              cocina_integral +
              n_lamparas_200m +
              menciona_cercania_txt +
              tiene_balcon +
              tiene_patio_ropas +
              cocina_abierta +
              menciona_cuadras_txt +
              poly(n_cafes_500m,2,raw=TRUE) +
              tiene_vigilancia_text +
              cercania_bus_text +
              is_residential +
              remodelada_text +
              distancia_bus +
              n_palabras_title,
            data=train_sf,
            method = 'lm', 
            trControl = fitControl,
            metric = "MAE"
)

OLS3


# RANDOM FOREST **************************

RF_1000 <- train(
  log(price) ~ ESTRATO:property_type + property_type + ESTRATO:tiene_terraza + tiene_terraza +
    property_type +
    garaje_indep_text +
    garaje_cubierto_text +
    terraza_propia +
    sala_comedor_conjunto +
    tiene_sala +
    cocina_integral +
    n_lamparas_200m +
    menciona_cercania_txt +
    tiene_balcon +
    tiene_patio_ropas +
    cocina_abierta +
    menciona_cuadras_txt +
    poly(n_cafes_500m, 2, raw=TRUE) +
    tiene_vigilancia_text +
    cercania_bus_text +
    is_residential +
    remodelada_text +
    distancia_bus +
    n_palabras_title,
  data = train,  # Dataset de entrenamiento
  method = "ranger",  # Usamos el motor ranger para Random Forests
  trControl = fitControl,  # Especificamos los controles de validación cruzada definidos antes
  tuneGrid = expand.grid(   # Definimos la grilla de hiperparámetros a explorar
    mtry = c(3,5,7),  # Número de predictores seleccionados al azar en cada división
    splitrule = "variance",  # Regla de partición basada en la reducción de varianza (regresión)
    min.node.size = c(30, 50)# Tamaño mínimo de nodos terminales
  ),
  metric = "MAE",
  num.trees = 1000
)

