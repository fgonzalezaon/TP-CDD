# =============================================================================
# Descargar + extraer Proyect.zip con archivos + RStudio Project File
# =============================================================================

rm(list = ls())

options(stringsAsFactors = FALSE)
options(scipen = 999)

# Librerías del proyecto
library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(here)

# Definir rutas de manera reproducible
proyecto_dir <- here::here()

dir_data_raw       <- file.path(proyecto_dir, "data", "raw")
dir_data_processed <- file.path(proyecto_dir, "data", "processed")
dir_outputs_fig    <- file.path(proyecto_dir, "outputs", "figures")
dir_outputs_tab    <- file.path(proyecto_dir, "outputs", "tables")

dirs <- c(dir_data_processed, dir_outputs_fig, dir_outputs_tab)
for (d in dirs) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

cat("✅ Setup cargado correctamente.\n")

cat("📁 Directorio del proyecto:", proyecto_dir, "\n")
