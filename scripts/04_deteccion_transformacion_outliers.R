#Deteccion outliers Regla del IQR

detectar_outliers <- function(df) {
  df %>%
    select(where(is.numeric)) %>%
    gather(variable, valor) %>%
    group_by(variable) %>%
    mutate(
      Q1 = quantile(valor, 0.25, na.rm = TRUE),
      Q3 = quantile(valor, 0.75, na.rm = TRUE),
      IQR = Q3 - Q1,
      limite_inf = Q1 - 1.5 * IQR,
      limite_sup = Q3 + 1.5 * IQR,
      es_outlier = valor < limite_inf | valor > limite_sup
    ) %>%
    filter(es_outlier)
}

outliers_gini <- detectar_outliers(gini_interpolado)
outliers_pobreza <- detectar_outliers(pobreza_interpolado)
outliers_ingreso <- detectar_outliers(ingreso_low_20_interpolado)
outliers_pbi <- detectar_outliers(pbi_interpolado)
outliers_desempleo <- detectar_outliers(desempleo_interpolado)

outliers_gini
outliers_pobreza
outliers_ingreso
outliers_pbi
outliers_desempleo

#Transformacion outliers

#El PIB se logaritmizó y la pobreza se winsorizó porque eran las variables con mayor cantidad de outliers. 
#El log para reducir la asimetría del PIB y la winsorización para limitar valores extremos en pobreza sin eliminar información.

log_transformar_pbi <- function(df) {
  df %>%
    mutate(across(where(is.numeric), ~ log(.), .names = "log_{col}"))
}

winsorizar_pobreza <- function(df, p = 0.05) {
  
  df %>%
    mutate(across(
      where(is.numeric),
      ~ {
        lim_inf <- quantile(., p, na.rm = TRUE)
        lim_sup <- quantile(., 1 - p, na.rm = TRUE)
        pmax(pmin(., lim_sup), lim_inf)
      }
    ))
}

pbi_log <- log_transformar_pbi(pbi_interpolado)

pobreza_winsor <- winsorizar_pobreza(pobreza_interpolado, p = 0.05)
