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

# Función de imputación
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
  numeric_vars = c("precio_catastro_prom"),
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

train_sf <- impute_categorical_by_cat(
  data      = train_sf,
  cat_vars  = c("CODIGO_UPZ"),
  group_vars = c("ESTRATO","CODIGO_UPZ", "property_type", "month", "year", "tiene_sala","tiene_terraza")
)

test_sf <- impute_categorical_by_cat(
  data      = test_sf,
  cat_vars  = c("CODIGO_UPZ"),
  group_vars = c("CODIGO_UPZ", "property_type", "month", "year", "tiene_sala","tiene_terraza")
)

train_sf <- impute_categorical_by_cat(
  data      = train_sf,
  cat_vars  = c("EPE__m2_ha"),
  group_vars = c("ESTRATO","CODIGO_UPZ", "property_type", "month", "year", "tiene_sala","tiene_terraza")
)

test_sf <- impute_categorical_by_cat(
  data      = test_sf,
  cat_vars  = c("EPE__m2_ha"),
  group_vars = c("CODIGO_UPZ", "property_type", "month", "year", "tiene_sala","tiene_terraza")
)

train_sf <- impute_categorical_by_cat(
  data      = train_sf,
  cat_vars  = c("ESTRUCTURA"),
  group_vars = c("ESTRATO","CODIGO_UPZ", "property_type", "month", "year", "tiene_sala","tiene_terraza")
)

test_sf <- impute_categorical_by_cat(
  data      = test_sf,
  cat_vars  = c("ESTRUCTURA"),
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
           data=train_sf,
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

# 

p_load(leaps)

model_form <- log(price) ~ month + year+
  property_type  +
  lat + lon + ESTRATO +  + CODIGO_UPZ +
  EPE__m2_ha + ESTRUCTURA +
  n_palabras_desc + n_palabras_title + n_palabras_title_nocod +
  tipo_inmueble_text + tiene_sala + tiene_comedor + sala_comedor_conjunto +
  n_habitaciones_text  + tiene_garaje_text  +
  garaje_cubierto_text + garaje_indep_text + tiene_deposito_bodega +
  tiene_terraza + tiene_balcon + terraza_propia + tiene_cocina +
  cocina_integral + cocina_abierta + tiene_patio_ropas + remodelada_text +
  tiene_vigilancia_text + menciona_centro_comercial + uso_comercial_text +
  menciona_cercania_txt + cercania_bus_text + cercania_cc + cercania_cafe_txt +
  menciona_cuadras_txt + bedrooms_final_set2 +
  bathrooms_final_set2 + tipo_inmueble_text_std + property_type_final +
  ESTRATO_was_na + rooms.y_was_na + bedrooms_final_set2_was_na +
  bathrooms_final_set2_was_na + distancia_cafe + n_cafes_500m +
  distancia_bus + n_lamparas_200m + is_residential  +
  precio_catastro_prom + precio_catastro_prom_was_na + CODIGO_UPZ_was_na
# Función auxiliar para predecir con objetos regsubsets
predict.regsubsets <- function(object, newdata, id, formula_used, ...) {
  # Identificar filas completas (sin NAs) en newdata
  model_vars <- all.vars(formula_used)[-1]  # Excluir variable respuesta
  complete_cases <- complete.cases(newdata[, model_vars, drop = FALSE])
  
  # Si no hay casos completos, devolver vector de NAs
  if (sum(complete_cases) == 0) {
    return(rep(NA, nrow(newdata)))
  }
  
  # Crear matriz solo con casos completos
  newdata_complete <- newdata[complete_cases, , drop = FALSE]
  mat <- model.matrix(formula_used, newdata_complete)
  
  # Obtener coeficientes
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  
  # Verificar que todas las variables estén en la matriz
  if (!all(xvars %in% colnames(mat))) {
    missing <- xvars[!xvars %in% colnames(mat)]
    stop("Variables faltantes en newdata: ", paste(missing, collapse = ", "))
  }
  
  # Hacer predicción para casos completos
  pred_complete <- as.vector(mat[, xvars, drop = FALSE] %*% coefi)
  
  # Crear vector completo con NAs donde corresponde
  pred_full <- rep(NA, nrow(newdata))
  pred_full[complete_cases] <- pred_complete
  
  return(pred_full)
}

# Parámetros de validación cruzada
k <- 5 
n <- nrow(train_sf) 
folds <- sample(rep(1:k, length = n)) 
cv.errors <- matrix(NA, nrow = k, ncol = 100, 
                    dimnames = list(NULL, paste(1:100))) 

# Convertir sf a data.frame
train_df <- st_drop_geometry(train_sf)

train_df <- train_df %>%
  mutate(bedrooms_final_set2 = as.numeric(bedrooms_final_set2))

train_df <- train_df %>%
  mutate(bathrooms_final_set2 = as.numeric(bathrooms_final_set2))

train_df <- train_df %>%
  mutate(CODIGO_UPZ = as.numeric(CODIGO_UPZ))

# Extraer nombre de variable respuesta y predictores
resp <- all.vars(model_form)[1]
preds <- attr(terms(model_form), "term.labels")

# Validación cruzada
for (j in 1:k) {
  cat("Procesando fold", j, "de", k, "\n")
  
  # Dividir datos
  data_train <- train_df[folds != j, ]
  data_test  <- train_df[folds == j, ]
  
  # Eliminar niveles no usados
  data_train <- droplevels(data_train)
  data_test  <- droplevels(data_test)
  
  # Validar predictores en AMBOS conjuntos
  valid_pred <- sapply(preds, function(v) {
    x_train <- data_train[[v]]
    x_test <- data_test[[v]]
    
    # Remover NAs
    x_train_no_na <- x_train[!is.na(x_train)]
    x_test_no_na <- x_test[!is.na(x_test)]
    
    # Verificar que haya datos
    if (length(x_train_no_na) == 0) return(FALSE)
    
    if (is.factor(x_train)) {
      # Para factores: verificar niveles en train y test
      train_levels <- levels(droplevels(x_train_no_na))
      test_levels <- unique(as.character(x_test_no_na))
      
      # Debe tener al menos 2 niveles en train
      # Y los niveles de test deben estar en train
      if (length(train_levels) < 2) return(FALSE)
      if (!all(test_levels %in% train_levels)) return(FALSE)
      
      return(TRUE)
    } else {
      # Para numéricos: al menos 2 valores únicos
      length(unique(x_train_no_na)) > 1
    }
  })
  
  preds_j <- preds[valid_pred]
  
  # Si no hay predictores válidos, saltar este fold
  if (length(preds_j) == 0) {
    cat("  Advertencia: No hay predictores válidos en fold", j, "\n")
    next
  }
  
  # Fórmula para este fold
  form_j <- as.formula(
    paste(resp, "~", paste(preds_j, collapse = " + "))
  )
  
  # Seleccionar columnas necesarias
  data_train <- data_train[, c(resp, preds_j), drop = FALSE]
  data_test  <- data_test[,  c(resp, preds_j), drop = FALSE]
  
  # Asegurar que factores en test tengan los mismos niveles que en train
  for (var in preds_j) {
    if (is.factor(data_train[[var]])) {
      train_levels <- levels(data_train[[var]])
      data_test[[var]] <- factor(data_test[[var]], levels = train_levels)
    }
  }
  
  # Ajustar modelo con selección backward
  best_fit <- regsubsets(
    form_j,
    data   = data_train,
    nvmax  = min(100, length(preds_j)),
    method = "backward"
  )
  
  # Guardar la fórmula para usar en predicción
  formula_j <- form_j
  
  # Calcular errores para cada tamaño de modelo
  num_models <- min(100, length(preds_j))
  
  for (i in 1:num_models) {
    tryCatch({
      pred <- predict.regsubsets(best_fit, newdata = data_test, id = i, 
                                 formula_used = formula_j)
      
      # Obtener valores observados
      obs <- data_test[[resp]]
      
      # Asegurar que pred y obs tengan la misma longitud
      if (length(pred) != length(obs)) {
        # Ajustar pred a la longitud de obs si es necesario
        if (length(pred) < length(obs)) {
          pred <- c(pred, rep(NA, length(obs) - length(pred)))
        } else {
          pred <- pred[1:length(obs)]
        }
      }
      
      # Calcular MSE solo para observaciones válidas (sin NA)
      valid_idx <- !is.na(pred) & !is.na(obs)
      
      if (sum(valid_idx) > 0) {
        cv.errors[j, i] <- mean((obs[valid_idx] - pred[valid_idx])^2)
      } else {
        cv.errors[j, i] <- NA
      }
    }, error = function(e) {
      cat("  Error en modelo", i, "del fold", j, ":", e$message, "\n")
      cv.errors[j, i] <<- NA
    })
  }
}

# Calcular error promedio por número de variables
mean.cv.errors <- colMeans(cv.errors, na.rm = TRUE)

# Mostrar resultados
cat("\nErrores de validación cruzada (MSE promedio):\n")
print(round(mean.cv.errors, 2))

# Encontrar mejor número de variables
best_nvars <- which.min(mean.cv.errors)
cat("\n=============================================\n")
cat("Mejor número de variables:", best_nvars, "\n")
cat("MSE mínimo:", round(mean.cv.errors[best_nvars], 2), "\n")
cat("=============================================\n")

# Ajustar modelo final con todas las observaciones para ver variables seleccionadas
cat("\nAjustando modelo final con todos los datos...\n")
train_df_complete <- train_df[complete.cases(train_df[, c(resp, preds)]), ]
train_df_complete <- droplevels(train_df_complete)

# Validar predictores en el conjunto completo
valid_pred_final <- sapply(preds, function(v) {
  x <- train_df_complete[[v]]
  x_no_na <- x[!is.na(x)]
  
  if (length(x_no_na) == 0) return(FALSE)
  
  if (is.factor(x)) {
    nlevels(droplevels(x_no_na)) > 1
  } else {
    length(unique(x_no_na)) > 1
  }
})

preds_final <- preds[valid_pred_final]
form_final <- as.formula(paste(resp, "~", paste(preds_final, collapse = " + ")))

final_fit <- regsubsets(
  form_final,
  data = train_df_complete,
  nvmax = min(100, length(preds_final)),
  method = "backward"
)

# Mostrar variables seleccionadas para el mejor modelo
cat("\n*** VARIABLES SELECCIONADAS (", best_nvars, " variables) ***\n", sep = "")
selected_vars <- names(coef(final_fit, id = best_nvars))[-1]  # Excluir intercepto
cat(paste("  -", selected_vars, collapse = "\n"), "\n")

# Mostrar resumen de selección para diferentes tamaños de modelo
cat("\n*** RESUMEN DE SELECCIÓN POR TAMAÑO ***\n")
cat("(Las primeras 10 mejores configuraciones)\n\n")

for (i in 1:min(10, length(mean.cv.errors))) {
  vars_i <- names(coef(final_fit, id = i))[-1]
  cat("Modelo con", i, "variable(s) - MSE:", round(mean.cv.errors[i], 2), "\n")
  cat("  Variables:", paste(vars_i, collapse = ", "), "\n\n")
}

# Graficar resultados
plot(mean.cv.errors, type = "b", pch = 19,
     xlab = "Número de variables",
     ylab = "MSE de validación cruzada",
     main = "Error de validación cruzada vs Número de variables",
     col = "steelblue", lwd = 2)
points(best_nvars, mean.cv.errors[best_nvars], 
       col = "red", pch = 19, cex = 2)
text(best_nvars, mean.cv.errors[best_nvars], 
     labels = paste("Óptimo:", best_nvars, "vars"), 
     pos = 3, col = "red", font = 2)
grid()

# Crear objeto con resultados para fácil acceso
results <- list(
  cv_errors = cv.errors,
  mean_cv_errors = mean.cv.errors,
  best_nvars = best_nvars,
  best_mse = mean.cv.errors[best_nvars],
  selected_variables = selected_vars,
  final_model = final_fit
)

cat("\n*** Los resultados están guardados en el objeto 'results' ***\n")
cat("Accede a las variables seleccionadas con: results$selected_variables\n")

return(invisible(results))


OLS3<-train(log(price) ~ 
              # month +
              # year+
              property_type +
              ESTRATO+
              # CODIGO_UPZ +
              EPE__m2_ha + 
              ESTRUCTURA +
              n_palabras_title_nocod +
              tiene_comedor +
              sala_comedor_conjunto +
              tiene_garaje_text +
              tiene_deposito_bodega +
              tiene_terraza +
              tiene_patio_ropas +
              remodelada_text +
              tiene_vigilancia_text +
              menciona_centro_comercial +
              uso_comercial_text +
              menciona_cercania_txt +
              cercania_bus_text +
              cercania_cafe_txt +
              menciona_cuadras_txt +
              bedrooms_final_set2 +
              bathrooms_final_set2 +
              ESTRATO_was_na +
              rooms.y_was_na +
              bathrooms_final_set2_was_na +
              distancia_cafe +
              n_cafes_500m +
              distancia_bus +
              n_lamparas_200m +
              is_residential +
              precio_catastro_prom+
              precio_catastro_prom_was_na +
              cercania_cc  
              # property_type_final
            ,
            data=train_sf,
            method = 'lm', 
            trControl = fitControl,
            metric = "MAE"
)

OLS3


# RANDOM FOREST **************************

RF_1000 <- train(
  log(price) ~ 
    # month +
    # year+
    property_type +
    ESTRATO+
    # CODIGO_UPZ +
    EPE__m2_ha + 
    ESTRUCTURA +
    n_palabras_title_nocod +
    tiene_comedor +
    sala_comedor_conjunto +
    tiene_garaje_text +
    tiene_deposito_bodega +
    tiene_terraza +
    tiene_patio_ropas +
    remodelada_text +
    tiene_vigilancia_text +
    menciona_centro_comercial +
    uso_comercial_text +
    menciona_cercania_txt +
    cercania_bus_text +
    cercania_cafe_txt +
    menciona_cuadras_txt +
    bedrooms_final_set2 +
    bathrooms_final_set2 +
    ESTRATO_was_na +
    rooms.y_was_na +
    bathrooms_final_set2_was_na +
    distancia_cafe +
    n_cafes_500m +
    distancia_bus +
    n_lamparas_200m +
    is_residential +
    precio_catastro_prom+
    precio_catastro_prom_was_na +
    cercania_cc  
  # property_type_final,
  ,
  data = train_sf,  # Dataset de entrenamiento
  method = "ranger",  # Usamos el motor ranger para Random Forests
  trControl = fitControl,  # Especificamos los controles de validación cruzada definidos antes
  tuneGrid = expand.grid(   # Definimos la grilla de hiperparámetros a explorar
    mtry = c(4,6,8),  # Número de predictores seleccionados al azar en cada división
    splitrule = "variance",  # Regla de partición basada en la reducción de varianza (regresión)
    min.node.size = c(30, 50, 70 )# Tamaño mínimo de nodos terminales
  ),
  metric = "MAE",
  num.trees = 1000
)

RF_1000V2 <- train(
  log(price) ~ 
    # efectos principales
    property_type +
    ESTRATO +
    EPE__m2_ha +
    ESTRUCTURA +
    n_palabras_title_nocod +
    tiene_comedor +
    sala_comedor_conjunto +
    tiene_garaje_text +
    tiene_deposito_bodega +
    tiene_terraza +
    tiene_patio_ropas +
    remodelada_text +
    tiene_vigilancia_text +
    menciona_centro_comercial +
    uso_comercial_text +
    menciona_cercania_txt +
    cercania_bus_text +
    cercania_cafe_txt +
    menciona_cuadras_txt +
    bedrooms_final_set2 +
    bathrooms_final_set2 +
    ESTRATO_was_na +
    rooms.y_was_na +
    bathrooms_final_set2_was_na +
    distancia_cafe +
    n_cafes_500m +
    distancia_bus +
    n_lamparas_200m +
    is_residential +
    precio_catastro_prom +
    precio_catastro_prom_was_na +
    cercania_cc +
    
    # NO LINEALIDADES (polinomios y logs)
    I(EPE__m2_ha^2) +
    I(bedrooms_final_set2^2) +
    I(bathrooms_final_set2^2) +
    I(log1p(distancia_cafe)) +
    I(log1p(distancia_bus)) +
    I(log1p(n_cafes_500m)) +
    I(log1p(n_lamparas_200m)) +
    I(precio_catastro_prom^2) +
    
    # INTERACCIONES ESTRUCTURALES
    bedrooms_final_set2:bathrooms_final_set2 +         # tamaño “útil” del hogar
    property_type:bedrooms_final_set2 +               # más cuartos valen distinto en casa vs apto
    property_type:bathrooms_final_set2 +
    property_type:ESTRATO +                           # estrato no pega igual según tipo
    ESTRATO:precio_catastro_prom +                    # catastro refleja mejor precio en altos estratos
    property_type:precio_catastro_prom +
    
    # INTERACCIONES DE LOCALIZACIÓN / AMENITIES
    cercania_cafe_txt:distancia_cafe +
    cercania_bus_text:distancia_bus +
    menciona_centro_comercial:cercania_cc +
    is_residential:uso_comercial_text +               # uso mixto en zonas residenciales
    
    # INTERACCIONES CON REMODELACIÓN / SEGURIDAD
    remodelada_text:ESTRATO +
    remodelada_text:tiene_vigilancia_text +
    
    # INTERACCIÓN CON IMPUTACIONES (slopes distintos cuando el dato fue imputado)
    precio_catastro_prom:precio_catastro_prom_was_na
  ,
  data = train_sf,  # Dataset de entrenamiento
  method = "ranger",  # Usamos el motor ranger para Random Forests
  trControl = fitControl,  # Especificamos los controles de validación cruzada definidos antes
  tuneGrid = expand.grid(   # Definimos la grilla de hiperparámetros a explorar
    mtry = c(4,6,8),  # Número de predictores seleccionados al azar en cada división
    splitrule = "variance",  # Regla de partición basada en la reducción de varianza (regresión)
    min.node.size = c(30, 50, 70 )# Tamaño mínimo de nodos terminales
  ),
  metric = "MAE",
  num.trees = 1000
)
