# Taller 3 - Big Data y ML ------------------------------------------------

rm(list = ls())

# Importing libraries -----------------------------------------------------

library(pacman)
p_load(rio, writexl, readxl, tidyverse, caret, keras,
       reticulate, tidymodels, sf, gt, gtsummary, osmdata,
       gridExtra, plotly, 
       skimr, leaflet)

# Establishing paths ------------------------------------------------------

wd_main <- "taller_3"
wd_data <- "/stores"
wd_code <- "/scripts"
wd_output <- "/views"

# Importing data ----------------------------------------------------------

test <- read.csv(paste0(wd_main, wd_data, "/test.csv"))
train <- read.csv(paste0(wd_main, wd_data, "/train.csv"))
localidades <- st_read(paste0(wd_main, wd_data, "/localidades.geojson"))

# Data --------------------------------------------------------------------

# Changing data type

  train$property_type <- as.factor(train$property_type)
  test$property_type <- as.factor(test$property_type)

# Missing values

  missings <- skim(train) %>% 
    select(skim_variable, n_missing) %>% 
    filter(n_missing!= 0)

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

  # Filtering

  ggplot(train %>%  filter(property_type == "Apartamento")) +
    geom_point(aes(x = surface_total, y = price )) + 
    theme_minimal()
  
  mean_price <- train %>% 
    filter(property_type == "Apartamento") %>% 
    mutate(surface_rounded = round(price/ 10) * 10) %>% 
    group_by(surface_rounded) %>% 
    mutate(mean_price = mean(price, na.rm = TRUE))
  
  ggplot(mean_price) +
    geom_point(aes(x = surface_rounded, y = mean_price )) + 
    theme_minimal()


# Spatial modeling --------------------------------------------------------

  limites <- getbb("Bogota Colombia")

  train <- train %>% 
    filter(between(lon, limites[1, "min"], limites[1, "max"]) & 
           between(lat, limites[2, "min"], limites[2, "max"]))
  
  leaflet() %>%
      addTiles() %>% 
      addCircles(lng = train$lon,
                 lat = train$lat)
  
  # Variable creation
  
  train <- train %>% 
    mutate(p_msq = price/surface_total) %>% 
    drop_na(p_msq)

  # Creating map visualization
  
  train <- train %>% 
    mutate(color = case_when(property_type == "Apartamento" ~ "#00008B",
                                  property_type == "Casa" ~ "#8B0000"))
  
  latitud_central <- mean(train$lat)
  longitud_central <- mean(train$lon)
  
  leaflet() %>% 
    addTiles() %>% 
    setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>% 
    addCircles(lng = train$lon,
               lat = train$lat,
               col = train$color,
               fillOpacity = 1,
               opacity = 1,
               radius = 1)
  
  # Mapa con localidades
  
  localidades_eliminar <- c(89, 94, 95)
  localidades <- st_transform(localidades, 4626) %>% 
    filter(!OBJECTID %in% localidades_eliminar)

  train_sf <- st_as_sf(train, coords = c("lon", "lat"), crs = 4626)
  
  # Distribución precio apartamentos

  ggplot() +
    geom_sf(data = localidades, fill = "gray95", color = "black") +
    geom_sf(
      data = train_sf %>% filter(property_type == "Apartamento"),
      aes(color = p_msq),
      shape = 16, size = 0.8, alpha = 0.7
    ) +
    scale_color_gradient(
      low = "#ffdf00", high = "#0a5c0a",
      name = "Precio m²"
    ) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      legend.position = "right") +
    labs(title = "Precio por metro cuadrado — Apartamentos")
  
  # Distribución precio casas
  
  ggplot() +
    geom_sf(data = localidades, fill = "gray95", color = "black") +
    geom_sf(
      data = train_sf %>% filter(property_type == "Casa"),
      aes(color = p_msq),
      shape = 16, size = 0.8, alpha = 0.7
    ) +
    scale_color_gradient(
      low = "#ffdf00", high = "#0a5c0a",
      name = "Precio m²"
    ) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      legend.position = "right") +
    labs(title = "Precio por metro cuadrado — Casas")

# Spatial data ------------------------------------------------------------

osmdata::available_features()
amenities <- osmdata::available_tags("amenity")
leisure <- osmdata::available_tags("leisure")
public_transport <- osmdata::available_tags("public_transport")
highway <- osmdata::available_tags("highway")
landuse <- osmdata::available_tags("landuse")
wholesale <- osmdata::available_tags("wholesale")


# --------- Distancia al cafe más cercano
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
  train_sf <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)

  # Matriz de distancias
  dist_matrix_cafe <- st_distance(x = train_sf, y = cafes_sf)
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
  train_sf_m <- st_transform(train_sf, 3116)
  cafes_sf_m <- st_transform(cafes_sf, 3116)

  # Buffer
  radio_buffer <- 500
  train_buffer <- st_buffer(train_sf_m, dist = radio_buffer)

  # Cafes dentro del buffer para cada obs
  intersections <- st_intersects(train_buffer, cafes_sf_m)

  # Número de cafés en el buffer
  n_cafes_500m <- lengths(intersections)

  train$n_cafes_500m <- n_cafes_500m

  #Scatter
  ggplot(train, aes(x = n_cafes_500m, y = price)) +
    geom_point(color = "darkblue", alpha = 0.4) +
    theme_classic()


# -------- Distancia a bus station
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
  dist_matrix_bus <- st_distance(x = train_sf, y = bus_sf)
  dim(dist_matrix_bus)

  # Distancia mínima a un bus stop para cada observación
  dist_min_bus <- apply(dist_matrix_bus, 1, min)
  
  train$distancia_bus <- as.numeric(dist_min_bus)


# ---------- Distancia a un parque
parques <- opq(bbox = getbb("Bogota Colombia")) |>
  add_osm_feature(key = "leisure", value = "park")

  # Simple features
  parques_sf_raw <- osmdata_sf(parques)
  parques_geometria <- parques_sf_raw$osm_points |>
   dplyr::select(osm_id, name)
  parques_geometria <- st_as_sf(parques_geometria)

  # Coordenadas
  parques_geometria <- parques_geometria |>
    mutate(x = st_coordinates(parques_geometria)[, "X"],
          y = st_coordinates(parques_geometria)[, "Y"])
  parques_sf <- st_as_sf(parques_geometria, coords = c("x", "y"), crs = 4326)

  # Matriz
  dist_matrix_parques <- st_distance(x = train_sf, y = parques_sf)
  dim(dist_matrix_parques)

  # Distancia mínima a un parque por observación
  dist_min_parque <- apply(dist_matrix_parques, 1, min)

  train$distancia_parque <- as.numeric(dist_min_parque)

# ---------  Iluminación en un buffer
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
  train_buffer_ilum <- st_buffer(train_sf_m, dist = radio_buffer)
  intersections_lamps <- st_intersects(train_buffer_ilum, lamparas_sf_m)

  # Cantidad de luces dentro del buffer
  n_lamparas_200m <- lengths(intersections_lamps)

  train$n_lamparas_200m <- n_lamparas_200m


# -------- Zona residencial
residential <- opq(bbox = getbb("Bogota Colombia")) |>
  add_osm_feature(key = "landuse", value = "residential")

  # sf
  residential_sf <- osmdata_sf(residential)

  # Polígonos residenciales
  residential_polygons <- residential_sf$osm_polygons
  residential_relation <- st_within(train_sf, residential_polygons)

  # 1 si el inumeble está en zona residencial, 0 si no
  train$is_residential <- as.integer(lengths(residential_relation) > 0)


# Models ------------------------------------------------------------------


