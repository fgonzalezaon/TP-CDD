#Regresion multiple y test de significancia

#Se arma la tabla final con todas las variables

df_final <- gini_interpolado %>%
  inner_join(pobreza_winsor,               by = c("Country Name", "Country Code")) %>%
  inner_join(ingreso_low_20_interpolado,   by = c("Country Name", "Country Code")) %>%
  inner_join(pbi_log,                      by = c("Country Name", "Country Code")) %>%
  inner_join(desempleo_interpolado,        by = c("Country Name", "Country Code"))


armar_df_anio <- function(df, anio) {
  df %>%
    transmute(
      Country = `Country Name`,
      Code    = `Country Code`,
      Gini    = .[[paste0(anio, ".x")]],
      Pobreza = .[[paste0(anio, ".y")]],
      Ingreso20 = .[[paste0(anio, ".x.x")]],
      Desempleo = .[[paste0(anio, ".y.y")]],
      LogPBI = .[[paste0("log_", anio)]]
    ) %>%
    drop_na()
}

df_2015 <- armar_df_anio(df_final, "2015")
df_2019 <- armar_df_anio(df_final, "2019")
df_2021 <- armar_df_anio(df_final, "2021")

mod_2015 <- lm(Gini ~ Pobreza + Ingreso20 + LogPBI + Desempleo, data = df_2015)
mod_2019 <- lm(Gini ~ Pobreza + Ingreso20 + LogPBI + Desempleo, data = df_2019)
mod_2021 <- lm(Gini ~ Pobreza + Ingreso20 + LogPBI + Desempleo, data = df_2021)

#Resultados regresion

summary(mod_2015)
summary(mod_2019)
summary(mod_2021)

#test de significancia individual nivel pbi sobre indice de gini medido sobre el p value

# p-value directo

summary(mod_2015)$coefficients["LogPBI", "Pr(>|t|)"]
summary(mod_2019)$coefficients["LogPBI", "Pr(>|t|)"]
summary(mod_2021)$coefficients["LogPBI", "Pr(>|t|)"]

