#00 Importar datos
# ==============================================================
# Taller 2 Big Data
# (Michael Salcedo)
# MAIN / RUN DIRECTORY
# ==============================================================

# ==============================================================
# SECCIÓN 1: Preparando el entorno
# ==============================================================

rm(list = ls())

cat("Working directory:\n")
print(getwd())

# Crear carpetas de output si no existen
for (path in c("02_output/figures",
               "02_output/tables")) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

# ==============================================================
# SECCIÓN 2: Cargue de paquetes
# ==============================================================


# Lista de paquetes requeridos para el análisis completo
required_packages <- c(
  "rvest",      # Para web scraping
  "httr", #Hace solicitudes http a urls
  "tidyverse",
  "magrittr",
  "dplyr",
  "stargazer",
  "tibble", 
  "caret",
  "xtable"
  
)


# Función auxiliar para instalar paquetes si no están disponibles ----
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) {
    cat("Instalando paquetes faltantes:", paste(new_packages, collapse=", "), "\n")
    install.packages(new_packages)
  } else {
    cat("Todos los paquetes ya están instalados.\n")
  }
}
install_if_missing(required_packages)

# Cargar todas las librerías necesarias
lapply(required_packages, function(pkg) {
  cat("Cargando paquete:", pkg, "...\n")
  library(pkg, character.only = TRUE)
})

library(boot) 
library(ggplot2)

require("pacman")
p_load(tidyverse, # Para manipulación de datos.
       glmnet, # Para estimar Elastic NET
       caret # Para facilitar el entrenamiento de los modelos
)

# ==============================================================
# SECCIÓN 3: Ejecución de scripts
# ==============================================================

#source("01_code/00_webscrapping.R")
#source("01_code/01_data_cleaning.R")
#source("01_code/02_seccion1.R")
#source("01_code/03_seccion2.R")
#source("01_code/04_seccion3.R")
