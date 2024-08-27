
# Lematización y agrupación

# Especifica la ruta al archivo Excel
file_path <- "/Users/cristianespinal/Downloads/CLASIFICACIÓN DE PROGRAMAS/programas_iupb.xlsx"

# Lee el archivo Excel
programas_data <- read_excel(file_path)

# Muestra las primeras filas del dataframe
head(programas_data)

# Especifica la ruta al archivo Excel
file_path_clasificacion <- "/Users/cristianespinal/Downloads/CLASIFICACIÓN DE PROGRAMAS/clasificación.xlsx"

# Lee el archivo Excel
clasificacion_data <- read_excel(file_path_clasificacion)

# Muestra las primeras filas del dataframe
head(clasificacion_data)

str(clasificacion_data)
str(programas_data)

names(clasificacion_data)

##########

library(readxl)
library(dplyr)
library(stringr)
library(textstem)

# Función para limpiar texto
limpiar_texto <- function(texto) {
  texto_limpio <- tolower(texto) # Convertir a minúsculas
  texto_limpio <- str_replace_all(texto_limpio, "[^a-z\\s]", "") # Eliminar caracteres especiales
  texto_limpio <- str_replace_all(texto_limpio, "\\s+", " ") # Eliminar espacios extra
  texto_limpio <- str_trim(texto_limpio) # Eliminar espacios al inicio y final
  return(texto_limpio)
}

# Lee los datos del archivo Excel 'programas_iupb.xlsx'
file_path_programas <- "/Users/cristianespinal/Downloads/CLASIFICACIÓN DE PROGRAMAS/programas_iupb.xlsx"
programas_data <- read_excel(file_path_programas) %>%
  mutate(NOMBRE_DEL_PROGRAMA_LIMPIO = limpiar_texto(NOMBRE_DEL_PROGRAMA))

# Lee los datos del archivo Excel 'clasificación.xlsx'
file_path_clasificacion <- "/Users/cristianespinal/Downloads/CLASIFICACIÓN DE PROGRAMAS/clasificación.xlsx"
clasificacion_data <- read_excel(file_path_clasificacion) %>%
  mutate(NOMBRE_LIMPIO = limpiar_texto(NOMBRE))

# Función para lematizar y descomponer en palabras
lematizar_y_descomponer <- function(texto) {
  palabras <- unlist(str_split(texto, "\\s+"))
  palabras_lematizadas <- lemmatize_words(palabras)
  return(palabras_lematizadas)
}

# Función para clasificar programas
clasificar_programa <- function(nombre_programa, clasificacion_data) {
  palabras_programa <- lematizar_y_descomponer(nombre_programa)
  for (i in 1:nrow(clasificacion_data)) {
    palabras_clasificacion <- lematizar_y_descomponer(clasificacion_data$NOMBRE_LIMPIO[i])
    if (any(palabras_programa %in% palabras_clasificacion)) {
      return(clasificacion_data$NOMBRE[i])
    }
  }
  return(NA) # Si no encuentra ninguna palabra clave, retorna NA
}

# Aplica la función de clasificación a cada nombre de programa
programas_data <- programas_data %>%
  mutate(NOMBRE_CLASIFICACION = sapply(NOMBRE_DEL_PROGRAMA_LIMPIO, clasificar_programa, clasificacion_data = clasificacion_data))

agrupacion_programas_lematizados <- programas_data

# Muestra el resultado
print(programas_data)

guardar_datos(agrupacion_programas_lematizados, path_base, "agrupacion_programas_lematizados")
