# Agrupación_SaberPro_específicas

# Cargar las librerías necesarias
library(readr)
library(dplyr)

# Establecer la ruta de la carpeta donde se encuentran los archivos .txt
ruta_carpeta_pro <- "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/Resultados Saber Pro/txt_especificas"

# Establecer la ruta de la carpeta donde se van a guardar los archivos
ruta_csv <- "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/DATA CONSOLIDADA/SaberPro_especificas.csv"
ruta_rdata <- "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/DATA CONSOLIDADA/SaberPro_especificas.rdata"

# Función para detectar el delimitador de un archivo
detectar_delimitador <- function(archivo) {
  linea <- read_lines(archivo, n_max = 1)
  if (grepl("¬", linea)) {
    return("¬")
  } else if (grepl("\\|", linea)) {
    return("|")
  } else {
    stop("Delimitador no reconocido en el archivo: ", archivo)
  }
}

# Leer cada archivo .txt considerando diferentes delimitadores y estructuras de columnas
leer_archivo <- function(archivo) {
  delimitador <- detectar_delimitador(archivo)
  nombre_archivo <- tools::file_path_sans_ext(basename(archivo))
  datos <- read_delim(archivo, delim = delimitador, col_names = TRUE, guess_max = 1000)
  datos <- mutate(datos, nombre_archivo = nombre_archivo)
  return(datos)
}

# Leer y combinar todos los archivos .txt en un único dataframe
datos_combinados <- map_dfr(archivos_txt, leer_archivo, .id = "id_archivo")

# Guardar el dataframe combinado en un archivo CSV
write_csv(datos_combinados, ruta_csv)

# Guardar el dataframe combinado en un archivo RData en la ruta especificada
save(datos_combinados, file = ruta_rdata)