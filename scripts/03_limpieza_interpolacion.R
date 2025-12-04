#limpieza e interpolacion de datos faltantes

#Se excluyeron paises que no contaban con al menos 4 datos entre los años 2000 - 2024
#Los casos eliminados son heterogéneos y no siguen un patrón común, por lo que su ausencia se interpreta como datos faltantes dispersos

limpiar <- function(df) {
  df %>%
    # seleccionar país, código y todas las columnas numéricas (años)
    select(`Country Name`, `Country Code`, where(is.numeric)) %>%
    
    # filtrar países que tengan al menos 4 valores NO NA
    filter(rowSums(!is.na(select(., where(is.numeric)))) >= 4)
}

gini_no_faltantes           <- limpiar(gini_raw)

#Se imputan los datos faltantes para los años obketivo a traves de una interpolacion lineal

interpolar_gini <- function(df) {
  
  # Años a extraer al final
  anios_objetivo <- c("2015", "2019", "2021")
  
  # Detectar columnas numéricas que sean años
  columnas_anios <- names(df)[sapply(df, is.numeric)]
  anios_num <- as.numeric(columnas_anios)
  
  # Interpolación fila por fila
  interpolado_mat <- t(apply(df[, columnas_anios], 1, function(valores) {
    
    # años que sí tienen datos
    anios_con_datos <- anios_num[!is.na(valores)]
    valores_con_datos <- valores[!is.na(valores)]
    
    if (length(valores_con_datos) == 0) {
      # caso sin datos → todo NA
      return(rep(NA, length(anios_num)))
      
    } else if (length(valores_con_datos) == 1) {
      # caso con 1 solo dato → repetir para todos
      return(rep(valores_con_datos, length(anios_num)))
      
    } else {
      # interpolación lineal completa
      return(
        approx(
          x = anios_con_datos,
          y = valores_con_datos,
          xout = anios_num,
          rule = 2
        )$y
      )
    }
  }))
  
  # reconstruir tabla interpolada
  df_interpolado <- df
  df_interpolado[, columnas_anios] <- interpolado_mat
  
  # devolver solo columnas objetivo
  df_interpolado %>%
    select(`Country Name`, `Country Code`, all_of(anios_objetivo))
}

gini_interpolado <- interpolar_gini(gini_no_faltantes)

#Se busca alinear todas las tablas con los mismos paises para llevae a cabo la regresion

alinear_con_gini <- function(df, gini_df) {
  
  paises_validos <- gini_df$`Country Code`
  
  df %>%
    filter(`Country Code` %in% paises_validos) %>%
    arrange(match(`Country Code`, paises_validos))   # mismo orden que gini
}

pobreza_alineada <- alinear_con_gini(pobreza_raw, gini_interpolado)
ingreso_low_20_alineado <- alinear_con_gini(ingreso_low_20_raw, gini_interpolado)
pbi_alineado             <- alinear_con_gini(pbi_per_capita_usd_raw, gini_interpolado)
desempleo_alineado       <- alinear_con_gini(desempleo_raw, gini_interpolado)

#Interpolar resto de tablas

interpolar_tabla <- interpolar_gini

pobreza_interpolado  <- interpolar_tabla(pobreza_alineada)

ingreso_low_20_interpolado  <- interpolar_tabla(ingreso_low_20_alineado)

pbi_interpolado  <- interpolar_tabla(pbi_alineado)

desempleo_interpolado  <- interpolar_tabla(desempleo_alineado)


#guardar tablas
carpeta_clean <- "C:/Users/Usuario/Downloads/TP Final CDD/data/clean"

# Guardar cada tabla
write.csv(gini_no_faltantes, 
          file = file.path(carpeta_clean, "gini_no_faltantes.csv"),
          row.names = FALSE)

write.csv(pbi_alineado, 
          file = file.path(carpeta_clean, "pbi_alineado.csv"),
          row.names = FALSE)

write.csv(ingreso_low_20_alineado, 
          file = file.path(carpeta_clean, "ingreso_low_20_alineado.csv"),
          row.names = FALSE)

write.csv(pobreza_alineada, 
          file = file.path(carpeta_clean, "pobreza_alineada.csv"),
          row.names = FALSE)

write.csv(desempleo_alineado, 
          file = file.path(carpeta_clean, "desempleo_alineado.csv"),
          row.names = FALSE)

cat("Archivos guardados correctamente en la carpeta CLEAN ✔️\n")


