# Cargar las librerías necesarias
library(readxl)
library(dplyr)
library(purrr)
library(readr)
library(writexl)

# Establecer el directorio donde se encuentran los archivos Excel
setwd("/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/Reporte de carga académica de profesores/RAW")

# Listar todos los archivos Excel en el directorio
archivos_excel <- list.files(pattern = "\\.xlsx$")

# Función para leer cada archivo y añadir el nombre del archivo como columna
leer_y_anadir_nombre <- function(archivo) {
  datos <- read_excel(archivo, col_types = "text")
  datos$Archivo <- gsub(".xlsx", "", archivo) # Remover la extensión del nombre del archivo
  return(datos)
}

# Leer cada archivo, aplicar la función y combinar todos en un solo dataframe
datos_consolidados <- map_df(archivos_excel, leer_y_anadir_nombre)

# Exportar el dataframe consolidado a un nuevo archivo Excel
write_xlsx(datos_consolidados, path = "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/Reporte de carga académica de profesores/Salida_consolidada/consolidada_carga_academica.xlsx")

# Exportar el dataframe consolidado a un nuevo archivo r.data
save(datos_consolidados, file = "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/Reporte de carga académica de profesores/Salida_consolidada/consolidada_carga_academica.rdata")