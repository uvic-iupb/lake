
file_estimacion <- "/Users/cristianespinal/Downloads/estimacion_lineal_por_snies.xlsx"
forecast_2027 <- read_excel(file_estimacion)

# Cargar las librerías necesarias
library(tidyverse)

# Convertir valores NA a 0 en las columnas anteriores a 20242
forecast_2027 <- forecast_2027 %>%
  mutate(across(`20151`:`20241`, ~replace_na(., 0)))

# Eliminar las columnas de 20242 a 20272
forecast_2027 <- forecast_2027 %>%
  select(-c(`20242`:`20272`))

# Realizar una proyección lineal para 7 nuevos periodos
forecast_2027 <- forecast_2027 %>%
  rowwise() %>%
  mutate(across(c(`20242`, `20251`, `20252`, `20261`, `20262`, `20271`, `20272`), ~ {
    # Crear un vector de valores existentes (no NA)
    existing_values <- c_across(`20151`:`20241`)
    non_na_values <- existing_values[!is.na(existing_values)]
    
    # Realizar una proyección lineal solo si hay suficientes datos
    if (length(non_na_values) > 1) {
      model <- lm(non_na_values ~ seq_along(non_na_values))
      future_steps <- (length(non_na_values) + 1):(length(non_na_values) + 7)
      predicted_values <- predict(model, newdata = data.frame(seq_along = future_steps))
      return(predicted_values[1])
    } else {
      return(NA)
    }
  }, .names = "proj_{col}"))

# Expandir las proyecciones a las nuevas columnas correspondientes
forecast_2027 <- forecast_2027 %>%
  mutate(
    `20242` = proj_20242,
    `20251` = proj_20251,
    `20252` = proj_20252,
    `20261` = proj_20261,
    `20262` = proj_20262,
    `20271` = proj_20271,
    `20272` = proj_20272
  ) %>%
  select(-starts_with("proj_"))

# Verificar la estructura después de la conversión y proyección
str(forecast_2027)

# Mostrar una vista previa de los datos
print(forecast_2027)

guardar_datos(forecast_2027, path_base, "proyeccion_por_snies")
