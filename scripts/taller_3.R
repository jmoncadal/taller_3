# Taller 3 - Big Data y ML ------------------------------------------------

rm(list = ls())

# Importing libraries -----------------------------------------------------

library(pacman)
p_load(rio, writexl, readxl, tidyverse, caret, keras,
       reticulate, tidymodels, sf, gt, gtsummary, osmdata,
       gridExtra, plotly, 
       skimr, leaflet, lwgeom)

# Establishing paths ------------------------------------------------------

wd_main <- "C:/Users/Juan/OneDrive - Universidad de los andes/Escritorio/Universidad/Posgrado/1. Primer Semestre/Big Data y Machine Learning/Trabajos/taller_3"
wd_data <- "/stores"
wd_code <- "/scripts"
wd_output <- "/views"

# Importing data ----------------------------------------------------------

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
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  test <- test %>% 
    st_as_sf(coords = c("lon", "lat"), crs = 4326)

  # Joining with Estrato
  
  estratos <- st_transform(estratos, st_crs(train))
  estratos <- st_make_valid(estratos)
  
  train <- st_join(train, estratos, join = st_intersects, left = FALSE)
  test  <- st_join(test,  estratos, join = st_intersects, left = FALSE)
  
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
  
  train_rooms <- train %>%
    st_drop_geometry() %>%                             
    group_by(month, year, property_type, CODIGO_UPZ) %>% 
    summarise(
      rooms_mean     = round(mean(rooms.y,     na.rm = TRUE)),
      bedrooms_mean  = round(mean(bedrooms.y,  na.rm = TRUE)),
      bathrooms_mean = round(mean(bathrooms.y, na.rm = TRUE)),
      .groups = "drop"
    )
  
  train2 <- train %>%
    left_join(train_rooms,
              by = c("month", "year", "property_type", "CODIGO_UPZ")) %>%
    mutate(
      rooms     = coalesce(rooms.y,     rooms_mean),
      bedrooms  = coalesce(bedrooms.y,  bedrooms_mean),
      bathrooms = coalesce(bathrooms.y, bathrooms_mean)
    ) %>%
    select(-rooms_mean, -bedrooms_mean, -bathrooms_mean)
  
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
  
  test_rooms <- test %>%
    st_drop_geometry() %>%                             
    group_by(month, year, property_type, CODIGO_UPZ) %>% 
    summarise(
      rooms_mean     = round(mean(rooms.y,     na.rm = TRUE)),
      bedrooms_mean  = round(mean(bedrooms.y,  na.rm = TRUE)),
      bathrooms_mean = round(mean(bathrooms.y, na.rm = TRUE)),
      .groups = "drop"
    )
  
  test2 <- test %>%
    left_join(test_rooms,
              by = c("month", "year", "property_type", "CODIGO_UPZ")) %>%
    mutate(
      rooms     = coalesce(rooms.y,     rooms_mean),
      bedrooms  = coalesce(bedrooms.y,  bedrooms_mean),
      bathrooms = coalesce(bathrooms.y, bathrooms_mean)
    ) %>%
    select(-rooms_mean, -bedrooms_mean, -bathrooms_mean)

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
           OBJECTID = OBJECTID.x) %>%
    select(-c(price.x, surface_total.x, surface_covered.x,
              rooms.x, bedrooms.x, bathrooms.x, RESPONSABL,
              OBJECTID.y, operation_type, title, description, CODIGO_CRI,
              NORMATIVA, ACTO_ADMIN, NUMERO_ACT, FECHA_ACTO, ESCALA_CAP,
              FECHA_CAPT, SHAPE_Area_1, SHAPE_Length_1, SHAPE_Length_12,
              SHAPE_Area_12, SHAPE_Length, SHAPE_Area, rooms.y, bedrooms.y,
              bathrooms.y, bedrooms_final_set2, bathrooms_final_set2,
              n_habitaciones_text, n_banos_text,n_garajes_text,
              area_text_m2, piso_text)) %>% 
    drop_na(rooms, bedrooms, bathrooms)
  
  test <- test2 %>%
    rename(price = price.y,
           surface_total = surface_total.y,
           surface_covered = surface_covered.y,
           OBJECTID = OBJECTID.x) %>%
    select(-c(price.x, surface_total.x, surface_covered.x,
              rooms.x, bedrooms.x, bathrooms.x, RESPONSABL,
              OBJECTID.y, operation_type, title, description, CODIGO_CRI,
              NORMATIVA, ACTO_ADMIN, NUMERO_ACT, FECHA_ACTO, ESCALA_CAP,
              FECHA_CAPT, SHAPE_Area_1, SHAPE_Length_1, SHAPE_Length_12,
              SHAPE_Area_12, SHAPE_Length, SHAPE_Area, rooms.y, bedrooms.y,
              bathrooms.y, bedrooms_final_set2, bathrooms_final_set2,
              n_habitaciones_text, n_banos_text, n_garajes_text,
              area_text_m2, piso_text)) %>% 
    drop_na(rooms, bedrooms, bathrooms)
  
  # Variable creation
  
  train <- train %>%
    drop_na(price, surface_total) %>% 
    mutate(p_msq = price/surface_total)
  

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
  
  ggplot() +
    geom_sf(data = upz, fill = "gray95", color = "black") +
    geom_sf(data = train %>% 
            dplyr::filter(property_type == "Apartamento"),
            aes(color = p_msq),
            shape = 16, size = 0.8, alpha = 0.7) +
    scale_color_gradient(low = "#00008B", high = "#8B0000", name = "Precio por m²") +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          legend.position = "right") +
  labs(title = "Precio por metro cuadrado — Apartamento")
  
  # Precio metro cuadrado casas
  
  ggplot() +
    geom_sf(data = upz, fill = "gray95", color = "black") +
    geom_sf(data = train %>% 
            dplyr::filter(property_type == "Casa"),
            aes(color = p_msq),
            shape = 16, size = 0.8, alpha = 0.7) +
    scale_color_gradient(low = "#00008B", high = "#8B0000", name = "Precio por m²") +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          legend.position = "right") +
    labs(title = "Precio por metro cuadrado — Casas")
 
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

# Models ------------------------------------------------------------------


