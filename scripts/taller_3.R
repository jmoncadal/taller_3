# Taller 3 - Big Data y ML ------------------------------------------------

rm(list = ls())

# Importing libraries -----------------------------------------------------

library(pacman)
p_load(rio, writexl, readxl, tidyverse, caret, keras,
       reticulate, tidymodels, sf, gt, gtsummary, osmdata,
       gridExtra, plotly, 
       skimr, leaflet)

# Establishing paths ------------------------------------------------------

wd_main <- "C:/Users/Juan/OneDrive - Universidad de los andes/Escritorio/Universidad/Posgrado/1. Primer Semestre/Big Data y Machine Learning/Trabajos/taller_3"
wd_data <- "/stores"
wd_code <- "/scripts"
wd_output <- "/views"

# Importing data ----------------------------------------------------------

test <- read.csv(paste0(wd_main, wd_data, "/test.csv"))
train <- read.csv(paste0(wd_main, wd_data, "/train.csv"))
localidades <- st_read(paste0(wd_main, wd_data, "/localidades.geojson"))
upz <- st_read(paste0(wd_main, wd_data, "/IndUPZ.json"))

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

# Spatial modeling --------------------------------------------------------
  
  train_sf <- train_nm %>% 
    st_as_sf(coords = c("lon", "lat"), crs = 4626)
  upz <- st_transform(upz, 4626)
  localidades <- st_transform(localidades, 4626)
  
  train_upz <- st_join(train_sf, upz, join = st_intersects)
  train_full <- st_join(localidades, train_upz, join = st_intersects)

  # Variable creation
  
  train_sf <- train_sf %>% 
    mutate(p_msq = price/surface_total) %>% 
    drop_na(p_msq)

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

  
  # Mapa con UPZ 

  # Precio metro cuadrado apartamentos
  
  ggplot() +
    geom_sf(data = upz, fill = "gray95", color = "black") +
    geom_sf(data = train_sf %>% 
              filter(property_type == "Apartamento"),
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
    geom_sf(data = train_sf %>% 
              filter(property_type == "Casa"),
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
    geom_sf(data = train_upz, aes(color = as.factor(CODIGO_UPZ)))
  
  # Distribución viviendas por localidad
  
  ggplot() +
    geom_sf(data = localidades, fill = "gray95", color = "black") +
    geom_sf(data = train_full, aes(color = as.factor(LocNombre)))
  
    
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
  dist_matrix_bus <- st_distance(x = train_sf, y = bus_sf)
  dim(dist_matrix_bus)

  # Distancia mínima a un bus stop para cada observación
  dist_min_bus <- apply(dist_matrix_bus, 1, min)
  
  train$distancia_bus <- as.numeric(dist_min_bus)

# Distancia a parque más cercano ------------------------------------------

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
  train_buffer_ilum <- st_buffer(train_sf_m, dist = radio_buffer)
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
  residential_relation <- st_within(train_sf, residential_polygons)

  # 1 si el inumeble está en zona residencial, 0 si no
  train$is_residential <- as.integer(lengths(residential_relation) > 0)
  
# Cantidad de restaurantes --------------------------------------------------------
  rest <- opq(bbox = getbb("Bogota Colombia")) |>
    add_osm_feature(key = "amenity", value = "restaurant")
  
  # sf
  rest_sf_raw <- osmdata_sf(rest)
  rest_geometria <- rest_sf_raw$osm_points |>
    dplyr::select(osm_id, name)
  rest_geometria <- st_as_sf(rest_geometria)
  
  # Coordenadas
  rest_geometria <- rest_geometria |>
    mutate(
      x = st_coordinates(rest_geometria)[, "X"],
      y = st_coordinates(rest_geometria)[, "Y"]
    )
  
  # Restaurantes en metros
  rest_sf_m  <- st_as_sf(rest_geometria, coords = c("x", "y"), crs = 4326) |>
    st_transform(3116)
  
  # Buffer
  radio_buffer <- 500
  train_buffer_rest <- st_buffer(train_sf_m, dist = radio_buffer)
  intersections_rest <- st_intersects(train_buffer_rest, rest_sf_m)
  n_restaurants_500m <- lengths(intersections_rest)
  
  train$n_restaurants_500m <- n_restaurants_500m
  
# Distancia a club --------------------------------------------------------
  club_social <- opq(bbox = getbb("Bogota Colombia")) |>
    add_osm_feature(key = "club", value = "social")

  club_social_sf_raw <- osmdata_sf(club_social)
  club_social_geometria <- club_social_sf_raw$osm_points |>
    dplyr::select(osm_id, name)
  
  club_social_geometria <- st_as_sf(club_social_geometria) |>
    mutate(
      x = st_coordinates(geometry)[, "X"],
      y = st_coordinates(geometry)[, "Y"]
    )
  
  club_social_sf <- st_as_sf(club_social_geometria, coords = c("x", "y"), crs = 4326)

  dist_matrix_club <- st_distance(x = train_sf, y = club_social_sf)
  dist_min_club <- apply(dist_matrix_club, 1, min)
  
  train$distancia_club_social <- as.numeric(dist_min_club)
  
# Supermercado más cercano --------------------------------------------------------

  super_query <- opq(bbox = getbb("Bogota Colombia")) |>
    add_osm_feature(key = "shop", value = "supermarket")
  
  super_sf_raw <- osmdata_sf(super_query)
  super_geometria <- super_sf_raw$osm_points |>
    dplyr::select(osm_id, name)

  super_geometria <- st_as_sf(super_geometria) |>
    mutate(
      x = st_coordinates(geometry)[, "X"],
      y = st_coordinates(geometry)[, "Y"]
    )

  super_sf <- st_as_sf(super_geometria, coords = c("x", "y"), crs = 4326)
  
  dist_matrix_super <- st_distance(x = train_sf, y = super_sf)
  dist_min_super <- apply(dist_matrix_super, 1, min)

  train$distancia_supermercado <- as.numeric(dist_min_super)
  
# Centro comercial más cercano --------------------------------------------------------
  mall_query <- opq(bbox = getbb("Bogota Colombia")) |>
    add_osm_feature(key = "shop", value = "mall")
  
  mall_sf_raw <- osmdata_sf(mall_query)
  
  if (!is.null(mall_sf_raw$osm_points) && nrow(mall_sf_raw$osm_points) > 0) {
    
    mall_geometria <- mall_sf_raw$osm_points |>
      dplyr::select(osm_id, name, geometry)
    
  } else {
    
    mall_geometria <- mall_sf_raw$osm_polygons |>
      dplyr::select(osm_id, name, geometry) |>
      st_centroid()
  }
  
  mall_geometria <- st_as_sf(mall_geometria) |>
    mutate(
      x = st_coordinates(geometry)[, "X"],
      y = st_coordinates(geometry)[, "Y"]
    )
  
  mall_sf  <- st_as_sf(mall_geometria, coords = c("x", "y"), crs = 4326)

  dist_matrix_mall <- st_distance(x = train_sf, y = mall_sf)
  dist_min_mall <- apply(dist_matrix_mall, 1, min)
  
  train$distancia_mall <- as.numeric(dist_min_mall)
  
# Models ------------------------------------------------------------------


