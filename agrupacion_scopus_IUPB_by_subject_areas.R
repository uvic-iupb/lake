# Cargar la librería necesaria
library(tidyverse)

# Establecer el directorio donde se encuentran los archivos .csv
setwd("/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/Investigación/Scopus")

# Listar todos los archivos .csv en el directorio
archivos_csv <- list.files(pattern = "\\.csv$")

# Leer cada archivo .csv como texto, agregar la columna con el nombre del archivo, y combinarlos
datos_combinados <- lapply(archivos_csv, function(archivo) {
  datos <- read_csv(archivo, col_types = cols(.default = "c"))  # Leer todas las columnas como texto
  nombre_sin_extension <- tools::file_path_sans_ext(basename(archivo))
  datos <- mutate(datos, `Subject area` = nombre_sin_extension)
})

# Combinar todos los DataFrames en uno solo
RAW_Scopus_IUPB <- bind_rows(datos_combinados)

# Definir la ruta completa del archivo donde quieres guardar el DataFrame
ruta_guardado <- "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/Investigación/RAW_Scopus.csv"

# Usar write_csv para guardar el DataFrame
write_csv(datos_finales, ruta_guardado)

# Mensaje de confirmación
cat("Archivo guardado en:", ruta_guardado)

# Ver los primeros registros del DataFrame combinado para confirmar
head(datos_finales)

read.csv("/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/Investigación/RAW_Scopus.csv")

data_resultado = read_csv("/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/Investigación/RAW_Scopus.csv")