#Carga de archivos raw

source("scripts/00_setup.R")

cat("🔄 Cargando archivos RAW...\n")

gini_raw <- read_excel(file.path(dir_data_raw, "gini.xls"))
pobreza_raw <- read_excel(file.path(dir_data_raw, "pobreza.xls"))
ingreso_low_20_raw <- read_excel(file.path(dir_data_raw, "ingreso bottom 20.xls"))
pbi_per_capita_usd_raw <- read_excel(file.path(dir_data_raw, "pbi per capita.xls"))
desempleo_raw <- read_excel(file.path(dir_data_raw, "desempleo.xls"))

cat("✅ Archivos RAW cargados correctamente.\n")

