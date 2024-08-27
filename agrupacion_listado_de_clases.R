# Cargar las librerías necesarias
library(readxl)
library(dplyr)
library(purrr)
library(writexl)

# Definir el directorio donde se encuentran los archivos de Excel
directorio_entrada <- "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/Listado de clases/RAW"

# Definir el directorio de salida
directorio_salida <- "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/Listado de clases/SALIDA_CONSOLIDADA"

# Obtener una lista de todos los archivos de Excel en el directorio
archivos <- list.files(path = directorio_entrada, pattern = "\\.xlsx$", full.names = TRUE)

# Función para leer y procesar cada archivo
leer_y_procesar <- function(archivo) {
  datos <- read_excel(archivo, col_types = "text")
  nombre_archivo <- tools::file_path_sans_ext(basename(archivo))
  datos <- datos %>% mutate(Archivo = nombre_archivo)
  return(datos)
}

# Usar map_df de purrr para leer y combinar todos los archivos
datos_consolidados <- map_df(archivos, leer_y_procesar)

# Exportar la base de datos consolidada
write_xlsx(datos_consolidados, file.path(directorio_salida, "consolidada_listado_de_clases.xlsx"))

# Exportar el dataframe consolidado a un nuevo archivo r.data
save(datos_consolidados, file = "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/Listado de clases/SALIDA_CONSOLIDADA/consolidada_listado_clases.rdata")
