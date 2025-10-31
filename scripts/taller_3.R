# Taller 3 - Big Data y ML ------------------------------------------------

rm(list = ls())

# Importing libraries -----------------------------------------------------

library(pacman)
p_load(writexl, readxl, tidyverse, caret)

# Establishing paths ------------------------------------------------------

wd_main <- "C:/Users/Juan/OneDrive - Universidad de los andes/Escritorio/Universidad/Posgrado/1. Primer Semestre/Big Data y Machine Learning/Trabajos/taller_3"
wd_data <- "/stores"
wd_code <- "/scripts"
wd_output <- "/views"

# Importing data ----------------------------------------------------------

df <- read.csv(paste0(wd_main, wd_data, "/submission_template.csv"))

# Data --------------------------------------------------------------------

# Models ------------------------------------------------------------------


