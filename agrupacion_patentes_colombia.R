# Cargar las librerías necesarias
library(dplyr)
library(readr)
library(purrr)

# Establecer la ruta de la carpeta que contiene los archivos CSV
ruta_carpeta <- "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/Investigación/Patentes"

# Leer los nombres de los archivos CSV en la carpeta
nombres_archivos <- list.files(path = ruta_carpeta, pattern = "\\.csv$", full.names = TRUE)

# Función para leer un archivo CSV y añadir una columna con el nombre del archivo
leer_y_añadir_nombre <- function(archivo) {
  datos <- read_csv(file = archivo, col_types = cols(.default = "c")) # Leer como texto con col_types = cols(.default = "c")
  nombre_sin_ext <- tools::file_path_sans_ext(basename(archivo)) # Extraer el nombre del archivo sin la extensión
  datos <- datos %>% mutate(NombreArchivo = nombre_sin_ext) # Añadir la columna con el nombre del archivo
  return(datos)
}

# Usar map_dfr de purrr para leer todos los archivos, añadir el nombre y combinarlos en un solo data frame
RAW_Patentes <- map_dfr(nombres_archivos, leer_y_añadir_nombre)

# Establecer la ruta del archivo de destino para la exportación
ruta_destino <- "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/Investigación/RAW_Patentes.csv"

# Exportar los datos combinados al archivo CSV
write_csv(RAW_Patentes, ruta_destino)

# Mensaje de confirmación
cat("Los datos han sido exportados exitosamente a", ruta_destino)


# Ver los primeros registros del data frame combinado
head(RAW_Patentes)
