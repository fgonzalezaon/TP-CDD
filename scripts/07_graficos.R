#Grafico relacion log pbi e indice de gini

library(ggplot2)

grafico_cuadrantes <- function(df, anio) {
  
  mean_gini   <- mean(df$Gini, na.rm = TRUE)
  mean_logpbi <- mean(df$LogPBI, na.rm = TRUE)
  
  ggplot(df, aes(x = Gini, y = LogPBI)) +
    geom_point(size = 2) +   # puntos más chicos
    geom_vline(xintercept = mean_gini, color = "red", linewidth = 1) +
    geom_hline(yintercept = mean_logpbi, color = "red", linewidth = 1) +
    
    # Cuadrantes
    annotate("text",
             x = min(df$Gini), 
             y = max(df$LogPBI),
             label = "I",
             size = 8, fontface = "bold", color = "grey40",
             hjust = -0.3, vjust = 1.2) +
    
    annotate("text",
             x = max(df$Gini),
             y = max(df$LogPBI),
             label = "II",
             size = 8, fontface = "bold", color = "grey40",
             hjust = 1.2, vjust = 1.2) +
    
    annotate("text",
             x = max(df$Gini),
             y = min(df$LogPBI),
             label = "III",
             size = 8, fontface = "bold", color = "grey40",
             hjust = 1.2, vjust = -0.3) +
    
    annotate("text",
             x = min(df$Gini),
             y = min(df$LogPBI),
             label = "IV",
             size = 8, fontface = "bold", color = "grey40",
             hjust = -0.3, vjust = -0.3) +
    
    labs(
      title = paste("Relación entre Desigualdad (Gini) y Log(PBI per cápita) —", anio),
      x = "Coeficiente de Gini",
      y = "Log(PBI per cápita)",
      caption = "Fuente: World Bank Data"
    ) +
    theme_minimal(base_size = 13)
}

grafico_cuadrantes(df_2015, 2015)
grafico_cuadrantes(df_2019, 2019)
grafico_cuadrantes(df_2021, 2021)



#Grafico promedio munidal gini con desempleo, pobreza e ingreso bottom 20

library(tidyverse)

# Función para promedio mundial + variación porcentual
promedio_y_variacion <- function(df_raw, indicador_nombre){
  
  df_raw %>%
    pivot_longer(cols = `2000`:`2024`,
                 names_to = "year",
                 values_to = "value") %>%
    mutate(year = as.numeric(year)) %>%
    group_by(year) %>%
    summarize(promedio = mean(value, na.rm = TRUE)) %>%
    arrange(year) %>%
    mutate(var_pct = (promedio - lag(promedio)) / lag(promedio) * 100,
           indicador = indicador_nombre)
}

gini_m <- promedio_y_variacion(gini_raw, "Gini")
pobreza_m <- promedio_y_variacion(pobreza_raw, "Pobreza")
desempleo_m <- promedio_y_variacion(desempleo_raw, "Desempleo")

df_var <- bind_rows(gini_m, pobreza_m, desempleo_m)

ggplot(df_var, aes(x = year, y = var_pct, color = indicador)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Variación porcentual anual del promedio mundial",
    subtitle = "Gini, Pobreza y Desempleo (2001–2024)",
    x = "Año",
    y = "Variación % anual",
    color = "Indicador"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")




library(tidyverse)

# ---- FUNCION PARA CALCULAR PROMEDIO MUNDIAL E INDEXAR ----
indexar_serie <- function(df_raw, nombre_indicador){
  
  df_raw %>%
    pivot_longer(cols = `2000`:`2024`,
                 names_to = "year",
                 values_to = "value") %>%
    mutate(year = as.numeric(year)) %>%
    group_by(year) %>%
    summarise(promedio = mean(value, na.rm = TRUE)) %>%
    arrange(year) %>%
    mutate(indice_2000 = promedio / promedio[year == 2000] * 100,
           indicador = nombre_indicador)
}

# ---- APLICAR A CADA VARIABLE ----
gini_idx      <- indexar_serie(gini_raw, "Gini")
pobreza_idx   <- indexar_serie(pobreza_raw, "Pobreza")
ingreso20_idx <- indexar_serie(ingreso_low_20_raw, "Ingreso 20% más pobre")
desemp_idx    <- indexar_serie(desempleo_raw, "Desempleo")

# ---- UNIR TODO ----
df_idx <- bind_rows(gini_idx, pobreza_idx, ingreso20_idx, desemp_idx)

# ---- GRAFICO ----
ggplot(df_idx, aes(x = year, y = indice_2000, color = indicador)) +
  geom_line(size = 1.3) +
  geom_point(size = 2) +
  labs(
    title = "Evolución del promedio mundial (índice 2000 = 100)",
    subtitle = "Gini, Pobreza, Ingreso 20% más pobre y Desempleo — Años 2000–2024",
    x = "Año",
    y = "Índice (2000 = 100)",
    color = "Indicador",
    caption = "Fuente: World Bank (WDI)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )

    
