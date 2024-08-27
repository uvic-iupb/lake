
# SNIES vs. SICAU

# Agrupación SICAU -----------------

# Directorio de los archivos Excel
directorio <- "../RAW DATA/SNIES/MATRICULADOS/"

# Listar todos los archivos Excel en el directorio
archivos <- list.files(directorio, pattern = "\\.xlsx$", full.names = TRUE)

# Función para leer cada archivo Excel comenzando desde la fila 9
leer_archivo <- function(archivo) {
  read_excel(archivo, col_types = "text") %>%
    clean_names()
}

# Leer y combinar todos los archivos en un solo dataframe
matriculas_snies <- archivos %>%
  map_df(~ leer_archivo(.x))

# Leer y combinar todos los archivos en un solo dataframe
matriculas_snies <- archivos %>%
  map_df(~ leer_archivo(.x))

# Unificar las columnas 'matriculados_2018' y 'matriculados' en una sola columna
matriculas_snies_iupb <- matriculas_snies %>%
  mutate(matriculados = coalesce(matriculados_2018, matriculados)) %>%
  select(-matriculados_2018) 

matriculas_snies_iupb <- matriculas_snies %>%
  filter(codigo_de_la_institucion == "3107")

# Verificar las columnas únicas
colnames(matriculas_snies)

# Ver la estructura del dataframe combinado
str(matriculas_snies)

# Verificar las columnas únicas
colnames(matriculas_snies)

# Agrupación SNIES -----------------------

# Seleccionar columnas relevantes
matricula_reducida <- matricula_totales %>%
  select(codigo_snies, programa, convenio, metodologia, facultad, jornada, 
         tipo_de_aspirante, municipio, comuna, barrio, estrato, etnia, genero, 
         identidad_de_genero, grupo_poblacional, desplazado, tipo_de_desplazamiento, 
         sede, estado, nivel, forma_de_pago, discapacidades, periodo)

# Agrupar por codigo_snies y otras variables de categorización, y contar matriculados
matriculados_sicau_iupb <- matricula_reducida %>%
  group_by(codigo_snies, programa, convenio, metodologia, facultad, jornada, 
           tipo_de_aspirante, municipio, comuna, barrio, estrato, etnia, genero, 
           identidad_de_genero, grupo_poblacional, desplazado, tipo_de_desplazamiento, 
           sede, estado, nivel, forma_de_pago, discapacidades, periodo) %>%
  summarise(matriculados = n()) %>%
  ungroup()

str(matriculados_sicau_iupb)
str(matriculas_snies_iupb)

# Agrupación SNIES y SICAU -----------------------

# Verificar los nombres de las columnas en ambas bases de datos
colnames(matriculados_sicau_iupb)
colnames(matriculas_snies_iupb)

# Renombrar columnas en matriculas_snies_iupb para que coincidan con las de matriculados_sicau_iupb
matriculas_snies_iupb <- matriculas_snies_iupb %>%
  rename(
    codigo_snies_del_programa = codigo_snies_del_programa,
    programa = programa_academico,
    matriculados_snies = matriculados_snies  # Corrige aquí si es matriculados_snies
  ) %>%
  mutate(
    matriculados_snies = as.integer(matriculados_snies)
  )

# Seleccionar y asegurar que ambas bases de datos tengan las mismas columnas relevantes
matriculados_sicau_iupb <- matriculados_sicau_iupb %>%
  select(codigo_snies_del_programa, programa, matriculados_sicau)

matriculas_snies_iupb <- matriculas_snies_iupb %>%
  select(codigo_snies_del_programa, programa, matriculados_snies)

# Agrupar por codigo_snies_del_programa y programa, y sumar matriculados
agrupados_sicau <- matriculados_sicau_iupb %>%
  group_by(codigo_snies_del_programa, programa) %>%
  summarise(matriculados_sicau = sum(matriculados_sicau, na.rm = TRUE)) %>%
  ungroup()

agrupados_snies <- matriculas_snies_iupb %>%
  group_by(codigo_snies_del_programa, programa) %>%
  summarise(matriculados_snies = sum(matriculados_snies, na.rm = TRUE)) %>%
  ungroup()

#####

# Normalizar los nombres de los programas en ambas bases de datos
matriculados_sicau_iupb <- matriculados_sicau_iupb %>%
  mutate(programa = stri_trans_general(programa, "Latin-ASCII"))

matriculas_snies_iupb <- matriculas_snies_iupb %>%
  mutate(programa = stri_trans_general(programa, "Latin-ASCII"))

# Renombrar columnas en matriculas_snies_iupb para que coincidan con matriculados_sicau_iupb
matriculas_snies_iupb <- matriculas_snies_iupb %>%
  rename(
    codigo_snies_del_programa = codigo_snies_del_programa,
    matriculados_snies = matriculados_snies
  ) %>%
  mutate(
    matriculados_snies = as.integer(matriculados_snies)
  )

# Seleccionar y asegurar que ambas bases de datos tengan las mismas columnas relevantes
matriculados_sicau_iupb <- matriculados_sicau_iupb %>%
  select(codigo_snies_del_programa, programa, matriculados_sicau)

matriculas_snies_iupb <- matriculas_snies_iupb %>%
  select(codigo_snies_del_programa, programa, matriculados_snies)

# Agrupar por codigo_snies_del_programa y programa, y sumar matriculados
agrupados_sicau <- matriculados_sicau_iupb %>%
  group_by(codigo_snies_del_programa, programa) %>%
  summarise(matriculados_sicau = sum(matriculados_sicau, na.rm = TRUE)) %>%
  ungroup()

agrupados_snies <- matriculas_snies_iupb %>%
  group_by(codigo_snies_del_programa, programa) %>%
  summarise(matriculados_snies = sum(matriculados_snies, na.rm = TRUE)) %>%
  ungroup()

# Combinar ambas bases de datos agrupadas
combined_data <- full_join(agrupados_sicau, agrupados_snies, by = c("codigo_snies_del_programa", "programa"))

# Reemplazar NA con 0 para poder calcular la diferencia
combined_data <- combined_data %>%
  mutate(
    matriculados_sicau = replace_na(matriculados_sicau, 0),
    matriculados_snies = replace_na(matriculados_snies, 0),
    diferencia = matriculados_sicau - matriculados_snies
  )

# Ver la estructura del dataframe combinado
str(combined_data)

# Corrección por fecha 2018 a 2022

library(dplyr)
library(readxl)
library(stringi)

# Filtrar la base de datos matricula_totales para incluir solo los periodos de 2018 a 2022
matricula_totales_filtered <- matricula_totales %>%
  filter(as.numeric(substr(periodo, 1, 4)) >= 2018 & as.numeric(substr(periodo, 1, 4)) <= 2022)

# Seleccionar y renombrar las columnas relevantes para matriculados_sicau_iupb
matriculados_sicau_iupb <- matricula_totales_filtered %>%
  select(codigo_snies, programa, convenio, metodologia, facultad, jornada, 
         tipo_de_aspirante, municipio, comuna, barrio, estrato, etnia, genero, 
         identidad_de_genero, grupo_poblacional, desplazado, tipo_de_desplazamiento, 
         sede, estado, nivel, forma_de_pago, discapacidades, periodo) %>%
  mutate(matriculados_sicau = 1) %>%
  rename(codigo_snies_del_programa = codigo_snies)

# Normalizar los nombres de los programas en ambas bases de datos
matriculados_sicau_iupb <- matriculados_sicau_iupb %>%
  mutate(programa = stri_trans_general(programa, "Latin-ASCII"))

matriculas_snies_iupb <- matriculas_snies_iupb %>%
  mutate(programa = stri_trans_general(programa_academico, "Latin-ASCII"))

# Renombrar columnas en matriculas_snies_iupb para que coincidan con matriculados_sicau_iupb
matriculas_snies_iupb <- matriculas_snies_iupb %>%
  rename(
    codigo_snies_del_programa = codigo_snies_del_programa,
    matriculados_snies = matriculados_snies
  ) %>%
  mutate(
    matriculados_snies = as.integer(matriculados_snies)
  )

# Seleccionar y asegurar que ambas bases de datos tengan las mismas columnas relevantes
matriculados_sicau_iupb <- matriculados_sicau_iupb %>%
  select(codigo_snies_del_programa, programa, matriculados_sicau)

matriculas_snies_iupb <- matriculas_snies_iupb %>%
  select(codigo_snies_del_programa, programa, matriculados_snies)

# Agrupar por codigo_snies_del_programa y programa, y sumar matriculados
agrupados_sicau <- matriculados_sicau_iupb %>%
  group_by(codigo_snies_del_programa, programa) %>%
  summarise(matriculados_sicau = sum(matriculados_sicau, na.rm = TRUE)) %>%
  ungroup()

agrupados_snies <- matriculas_snies_iupb %>%
  group_by(codigo_snies_del_programa, programa) %>%
  summarise(matriculados_snies = sum(matriculados_snies, na.rm = TRUE)) %>%
  ungroup()

# Combinar ambas bases de datos agrupadas
combined_data <- full_join(agrupados_sicau, agrupados_snies, by = c("codigo_snies_del_programa", "programa"))

# Reemplazar NA con 0 para poder calcular la diferencia
combined_data <- combined_data %>%
  mutate(
    matriculados_sicau = replace_na(matriculados_sicau, 0),
    matriculados_snies = replace_na(matriculados_snies, 0),
    diferencia = matriculados_sicau - matriculados_snies
  )

# Normalizar nombres ------

# Normalizar los nombres de los programas en ambas bases de datos
matriculados_sicau_iupb <- matriculados_sicau_iupb %>%
  mutate(programa = stri_trans_general(programa, "Latin-ASCII"),
         programa = tolower(programa),
         programa = str_trim(programa))

matriculas_snies_iupb <- matriculas_snies_iupb %>%
  mutate(programa = stri_trans_general(programa, "Latin-ASCII"),
         programa = tolower(programa),
         programa = str_trim(programa))

# Renombrar columnas en matriculas_snies_iupb para que coincidan con matriculados_sicau_iupb
matriculas_snies_iupb <- matriculas_snies_iupb %>%
  rename(
    codigo_snies_del_programa = codigo_snies_del_programa,
    matriculados_snies = matriculados_snies
  ) %>%
  mutate(
    matriculados_snies = as.integer(matriculados_snies)
  )

# Seleccionar y asegurar que ambas bases de datos tengan las mismas columnas relevantes
matriculados_sicau_iupb <- matriculados_sicau_iupb %>%
  select(codigo_snies_del_programa, programa, matriculados_sicau)

matriculas_snies_iupb <- matriculas_snies_iupb %>%
  select(codigo_snies_del_programa, programa, matriculados_snies)

# Agrupar por codigo_snies_del_programa y programa, y sumar matriculados
agrupados_sicau <- matriculados_sicau_iupb %>%
  group_by(codigo_snies_del_programa, programa) %>%
  summarise(matriculados_sicau = sum(matriculados_sicau, na.rm = TRUE)) %>%
  ungroup()

agrupados_snies <- matriculas_snies_iupb %>%
  group_by(codigo_snies_del_programa, programa) %>%
  summarise(matriculados_snies = sum(matriculados_snies, na.rm = TRUE)) %>%
  ungroup()

# Combinar ambas bases de datos agrupadas
combined_data <- full_join(agrupados_sicau, agrupados_snies, by = c("codigo_snies_del_programa", "programa"))

# Reemplazar NA con 0 para poder calcular la diferencia
combined_data <- combined_data %>%
  mutate(
    matriculados_sicau = replace_na(matriculados_sicau, 0),
    matriculados_snies = replace_na(matriculados_snies, 0),
    diferencia = matriculados_sicau - matriculados_snies
  )

# Ver la estructura del dataframe combinado
str(combined_data)

# Normalizar nombres ------

# Función para normalizar los nombres de los programas
normalize_program_name <- function(name) {
  name %>%
    stri_trans_general("Latin-ASCII") %>%
    str_to_upper() %>%
    str_squish()
}

# Normalizar los nombres de los programas en ambas bases de datos
matriculados_sicau_iupb <- matriculados_sicau_iupb %>%
  mutate(programa = normalize_program_name(programa))

matriculas_snies_iupb <- matriculas_snies_iupb %>%
  mutate(programa = normalize_program_name(programa))

# Renombrar columnas en matriculas_snies_iupb para que coincidan con matriculados_sicau_iupb
matriculas_snies_iupb <- matriculas_snies_iupb %>%
  rename(
    codigo_snies_del_programa = codigo_snies_del_programa,
    matriculados_snies = matriculados_snies
  ) %>%
  mutate(
    matriculados_snies = as.integer(matriculados_snies)
  )

# Seleccionar y asegurar que ambas bases de datos tengan las mismas columnas relevantes
matriculados_sicau_iupb <- matriculados_sicau_iupb %>%
  select(codigo_snies_del_programa, programa, matriculados_sicau)

matriculas_snies_iupb <- matriculas_snies_iupb %>%
  select(codigo_snies_del_programa, programa, matriculados_snies)

# Agrupar por codigo_snies_del_programa y programa, y sumar matriculados
agrupados_sicau <- matriculados_sicau_iupb %>%
  group_by(codigo_snies_del_programa, programa) %>%
  summarise(matriculados_sicau = sum(matriculados_sicau, na.rm = TRUE)) %>%
  ungroup()

agrupados_snies <- matriculas_snies_iupb %>%
  group_by(codigo_snies_del_programa, programa) %>%
  summarise(matriculados_snies = sum(matriculados_snies, na.rm = TRUE)) %>%
  ungroup()

# Combinar ambas bases de datos agrupadas
combined_data <- full_join(agrupados_sicau, agrupados_snies, by = c("codigo_snies_del_programa", "programa"))

# Reemplazar NA con 0 para poder calcular la diferencia
combined_data <- combined_data %>%
  mutate(
    matriculados_sicau = replace_na(matriculados_sicau, 0),
    matriculados_snies = replace_na(matriculados_snies, 0),
    diferencia = matriculados_sicau - matriculados_snies
  )

# Ver la estructura del dataframe combinado
str(combined_data)

# Unificación de nombres -----------

library(dplyr)
library(stringi)

# Función para normalizar los nombres de los programas
normalize_program_name <- function(name) {
  name %>%
    stri_trans_general("Latin-ASCII") %>%
    str_to_upper() %>%
    str_squish()
}

# Normalizar los nombres de los programas en ambas bases de datos
matriculados_sicau_iupb <- matriculados_sicau_iupb %>%
  mutate(programa = normalize_program_name(programa))

matriculas_snies_iupb <- matriculas_snies_iupb %>%
  mutate(programa = normalize_program_name(programa))

# Crear un diccionario de nombres unificados basados en el código SNIES
unified_names <- matriculados_sicau_iupb %>%
  bind_rows(matriculas_snies_iupb) %>%
  group_by(codigo_snies_del_programa) %>%
  summarise(programa = first(programa))

# Unir los nombres unificados a las bases de datos
matriculados_sicau_iupb <- matriculados_sicau_iupb %>%
  left_join(unified_names, by = "codigo_snies_del_programa") %>%
  select(codigo_snies_del_programa, programa = programa.y, matriculados_sicau)

matriculas_snies_iupb <- matriculas_snies_iupb %>%
  left_join(unified_names, by = "codigo_snies_del_programa") %>%
  select(codigo_snies_del_programa, programa = programa.y, matriculados_snies)

# Agrupar por codigo_snies_del_programa y programa, y sumar matriculados
agrupados_sicau <- matriculados_sicau_iupb %>%
  group_by(codigo_snies_del_programa, programa) %>%
  summarise(matriculados_sicau = sum(matriculados_sicau, na.rm = TRUE)) %>%
  ungroup()

agrupados_snies <- matriculas_snies_iupb %>%
  group_by(codigo_snies_del_programa, programa) %>%
  summarise(matriculados_snies = sum(matriculados_snies, na.rm = TRUE)) %>%
  ungroup()

# Combinar ambas bases de datos agrupadas
combined_data <- full_join(agrupados_sicau, agrupados_snies, by = c("codigo_snies_del_programa", "programa"))

# Reemplazar NA con 0 para poder calcular la diferencia
combined_data <- combined_data %>%
  mutate(
    matriculados_sicau = replace_na(matriculados_sicau, 0),
    matriculados_snies = replace_na(matriculados_snies, 0),
    diferencia = matriculados_sicau - matriculados_snies
  )

# Estandarización código -------

# Función para normalizar los nombres de los programas
normalize_program_name <- function(name) {
  name %>%
    stri_trans_general("Latin-ASCII") %>%
    str_to_upper() %>%
    str_squish()
}

# Crear un mapeo de códigos SNIES que deben ser combinados
snies_mapping <- c("9508" = "9508", "9509" = "9508", 
                   "2078" = "2078", "2079" = "2078",
                   "2076" = "2076", "2077" = "2076")

# Aplicar el mapeo a las bases de datos
matriculados_sicau_iupb <- matriculados_sicau_iupb %>%
  mutate(codigo_snies_del_programa = ifelse(codigo_snies_del_programa %in% names(snies_mapping), snies_mapping[codigo_snies_del_programa], codigo_snies_del_programa))

matriculas_snies_iupb <- matriculas_snies_iupb %>%
  mutate(codigo_snies_del_programa = ifelse(codigo_snies_del_programa %in% names(snies_mapping), snies_mapping[codigo_snies_del_programa], codigo_snies_del_programa))

# Normalizar los nombres de los programas en ambas bases de datos
matriculados_sicau_iupb <- matriculados_sicau_iupb %>%
  mutate(programa = normalize_program_name(programa))

matriculas_snies_iupb <- matriculas_snies_iupb %>%
  mutate(programa = normalize_program_name(programa))

# Crear un diccionario de nombres unificados basados en el código SNIES
unified_names <- matriculados_sicau_iupb %>%
  bind_rows(matriculas_snies_iupb) %>%
  group_by(codigo_snies_del_programa) %>%
  summarise(programa = first(programa))

# Unir los nombres unificados a las bases de datos
matriculados_sicau_iupb <- matriculados_sicau_iupb %>%
  left_join(unified_names, by = "codigo_snies_del_programa") %>%
  select(codigo_snies_del_programa, programa = programa.y, matriculados_sicau)

matriculas_snies_iupb <- matriculas_snies_iupb %>%
  left_join(unified_names, by = "codigo_snies_del_programa") %>%
  select(codigo_snies_del_programa, programa = programa.y, matriculados_snies)

# Agrupar por codigo_snies_del_programa y programa, y sumar matriculados
agrupados_sicau <- matriculados_sicau_iupb %>%
  group_by(codigo_snies_del_programa, programa) %>%
  summarise(matriculados_sicau = sum(matriculados_sicau, na.rm = TRUE)) %>%
  ungroup()

agrupados_snies <- matriculas_snies_iupb %>%
  group_by(codigo_snies_del_programa, programa) %>%
  summarise(matriculados_snies = sum(matriculados_snies, na.rm = TRUE)) %>%
  ungroup()

# Combinar ambas bases de datos agrupadas
combined_data <- full_join(agrupados_sicau, agrupados_snies, by = c("codigo_snies_del_programa", "programa"))

write.csv(combined_data, file = "/Users/cristianespinal/Downloads/combined_data.csv", row.names = FALSE)

# Reemplazar NA con 0 para poder calcular la diferencia
combined_data <- combined_data %>%
  mutate(
    matriculados_sicau = replace_na(matriculados_sicau, 0),
    matriculados_snies = replace_na(matriculados_snies, 0),
    diferencia = matriculados_sicau - matriculados_snies
  )

# Calcular la diferencia porcentual
combined_data <- combined_data %>%
  mutate(diferencia_porcentual = (diferencia / matriculados_snies) * 100)

# Crear el gráfico de coeficientes
coef_plot <- ggplot(combined_data, aes(x = reorder(programa, 5.148936), y = 5.148936)) +
  geom_point() +
  geom_segment(aes(x = programa, xend = programa, y = 0, yend = 5.148936)) +
  coord_flip() +
  labs(title = "Diferencia Porcentual de Matrícula por Programa",
       x = "Programa",
       y = "Diferencia Porcentual (%)") +
  theme_minimal()

# Calcular la diferencia porcentual promedio (excluyendo NA)
diferencia_porcentual_promedio <- combined_data %>%
  summarise(promedio = mean(diferencia_porcentual, na.rm = TRUE)) %>%
  pull(promedio)

# Calcular la diferencia bruta promedio
diferencia_bruta_promedio <- combined_data %>%
  summarise(promedio = mean(diferencia, na.rm = TRUE)) %>%
  pull(promedio)

# Calcular la diferencia relativa ajustada
combined_data <- combined_data %>%
  mutate(diferencia_relativa_ajustada = ifelse(matriculados_snies + matriculados_sicau != 0, 
                                               (2 * (matriculados_sicau - matriculados_snies) / (matriculados_sicau + matriculados_snies)) * 100, 
                                               NA))

# Calcular el cociente de proporciones
combined_data <- combined_data %>%
  mutate(cociente_proporciones = ifelse(matriculados_snies != 0, matriculados_sicau / matriculados_snies, NA))


# Calcular la diferencia media normalizada
combined_data <- combined_data %>%
  mutate(diferencia_media_normalizada = ifelse(matriculados_snies + matriculados_sicau != 0, 
                                               ((matriculados_sicau - matriculados_snies) / ((matriculados_sicau + matriculados_snies) / 2)) * 100, 
                                               NA))


# Crear el gráfico de coeficientes con la Diferencia Relativa Ajustada
coef_plot_dra <- ggplot(combined_data, aes(x = reorder(programa, diferencia_relativa_ajustada), y = diferencia_relativa_ajustada)) +
  geom_point() +
  geom_segment(aes(x = programa, xend = programa, y = 0, yend = diferencia_relativa_ajustada)) +
  coord_flip() +
  labs(title = "Diferencia Relativa Ajustada de Matrícula por Programa",
       x = "Programa",
       y = "Diferencia Relativa Ajustada (%)") +
  theme_minimal()

# Crear el gráfico de coeficientes con la Diferencia Absoluta
coef_plot_diferencia <- ggplot(combined_data, aes(x = reorder(programa, diferencia), y = diferencia)) +
  geom_point() +
  geom_segment(aes(x = programa, xend = programa, y = 0, yend = diferencia)) +
  coord_flip() +
  labs(title = "Diferencia Absoluta de Matrícula por Programa",
       x = "Programa",
       y = "Diferencia Absoluta") +
  theme_minimal()

# Imprimir los resultados
cat("Diferencia porcentual promedio:", diferencia_porcentual_promedio, "%\n")
cat("Diferencia bruta promedio:", diferencia_bruta_promedio, "\n")

# Imprimir los resultados
cat("Diferencia porcentual promedio:", diferencia_porcentual_promedio, "%\n")
cat("Diferencia relativa ajustada promedio:", diferencia_relativa_ajustada_promedio, "%\n")
cat("Diferencia media normalizada promedio:", diferencia_media_normalizada_promedio, "%\n")
