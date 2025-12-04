#Estadistica descriptiva raw

#Las estadísticas descriptivas reportadas incluyen media, moda, mediana, desvío estándar, 
#valores mínimo y máximo, y los percentiles 25 y 75.

moda <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

estadisticas <- function(df) {
  df %>%
    summarise(across(
      .cols = where(is.numeric),
      .fns = list(
        media = ~mean(.x, na.rm = TRUE),
        mediana = ~median(.x, na.rm = TRUE),
        moda = ~moda(.x[!is.na(.x)]),
        sd = ~sd(.x, na.rm = TRUE),
        iqr = ~IQR(.x, na.rm = TRUE),
        min = ~min(.x, na.rm = TRUE),
        max = ~max(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ))
}

estadisticas(gini_raw)

estadisticas(pobreza_raw)

estadisticas(ingreso_low_20_raw)

estadisticas(pbi_per_capita_usd_raw)

estadisticas(desempleo_raw)
