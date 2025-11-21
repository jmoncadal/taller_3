rm(list = ls())

library(pacman) 

p_load(rio,          # Lectura de datos.
       stringi,      # Manipular cadenas de texto.
       tidyverse,    # Manipulación de datos.
       tm,           # Minería de texto.
       tidytext,     # Tokenización.
       stopwords,    # Listas predefinidas de stopwords.
       reticulate,
       spacyr,       # Lematización vía spaCy (de Python).
       conflicted,   # Evitar conflictos entre funciones.
       tidymodels,   # Modelos de Machine Learning.
       sf,           # Manejo de datos espaciales.
       spatialsample # Validación cruzada espacial
) 

conflict_prefer(name = 'import', winner = 'rio')
spacy_install(
  version = "latest",   # or specify a version like "3.7.5"
  force = TRUE          # ensures reinstallation even if already present
)

# 2. Force re-download the Spanish model
spacy_download_langmodel(
  lang_models = "es_core_news_lg",
  force = TRUE          # overwrites existing installation
)

# Descargamos el modelo de lenguaje.
spacy_initialize(model = "es_core_news_lg")

# Establishing paths ------------------------------------------------------

wd_main <- "C:/Users/Juan/OneDrive - Universidad de los andes/Escritorio/Universidad/Posgrado/1. Primer Semestre/Big Data y Machine Learning/Trabajos/taller_3"
wd_data <- "/stores"
wd_code <- "/scripts"
wd_output <- "/views"
# DATA ================================
test <- read_csv(paste0(wd_main, wd_data, "/test.csv"))
train <- read_csv(paste0(wd_main, wd_data, "/train.csv"))

# Limpieza y creación para train y test


test <- test |>
  rename(description_raw = description) |>
  mutate(
    description_clean = description_raw |> 
      str_to_lower() |> 
      stringi::stri_trans_general("Latin-ASCII") |> 
      str_replace_all("\\s+", " ") |>
      str_trim()
  )

train <- train |>
  rename(description_raw = description) |>
  mutate(
    description_clean = description_raw |>
      str_to_lower() |>
      stringi::stri_trans_general("Latin-ASCII") |>
      str_replace_all("[^[:alnum:]]", " ") |>
      str_replace_all("\\s+", " ") |>
      str_trim()
  )

# Corremos spacy para que ecuentre los patrones y podamos lematizar:

train_desc_spacy <- spacy_parse(
  x       = train$description_raw,
  doc_id  = train$property_id,   # cada listing = un documento
  lemma   = TRUE,
  pos     = TRUE,
  tag     = FALSE,
  entity  = TRUE,
  dependency = FALSE
)

test_desc_spacy <- spacy_parse(
  x       = test$description_raw,
  doc_id  = test$property_id,
  lemma   = TRUE,
  pos     = TRUE,
  tag     = FALSE,
  entity = TRUE,
  dependency = FALSE
)
  
