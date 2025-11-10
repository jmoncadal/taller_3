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
wholesale <- osmdata::available_tags("wholesale")
  
# Models ------------------------------------------------------------------


