# Código para la agrupación de SaberPro y TyT 

# Txt - agrupar - filtrar - exportar a csv y rdata

library(readr)
library(dplyr)
library(janitor)

# Define la ruta donde se encuentran los archivos .txt
ruta_txt <- "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/Resultados Saber TyT/txt_genericas"

# Lista todos los archivos .txt en la ruta especificada
archivos_txt <- list.files(path = ruta_txt, pattern = "\\.txt$", full.names = TRUE)

# Función para leer y procesar cada archivo .txt

lectura_txt <- function(path_a) {
  print(paste("Leyendo:", path_a))  # Imprime la ruta del archivo que se está leyendo
  
  # Lee los datos como texto para evitar problemas de tipos incompatibles
  temp <- read_delim(path_a, delim = "¬", col_types = cols(.default = col_character()), show_col_types = FALSE) %>%
    clean_names()  # Limpia los nombres de las columnas
  
  return(temp)
}

# Lee y combina todos los archivos .txt
base_datos_combinada <- map_df(archivos_txt, lectura_txt)

# FILTRO
base_datos_filtrada <- base_datos_combinada %>%
  filter(inst_nombre_institucion == "INSTITUCION  UNIVERSITARIA PASCUAL BRAVO-MEDELLIN")

# Guarda la base de datos combinada en un archivo .csv
write_csv(base_datos_filtrada, "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/DATA CONSOLIDADA/SaberTyT_seleccion_agrupada_IUPB.csv")
save(base_datos_filtrada, file = "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/DATA CONSOLIDADA/SaberTyT_seleccion_agrupada_IUPB.rdata")

##### Ahora con .txt_seleccion ########

library(readr)
library(dplyr)
library(janitor)

# Define la ruta donde se encuentran los archivos .txt
ruta_txt_seleccion <- "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/Resultados Saber TyT/txt_especificas"

# Lista todos los archivos .txt en la ruta especificada
archivos_txt_seleccion <- list.files(path = ruta_txt_seleccion, pattern = "\\.txt$", full.names = TRUE)

# Función para leer y procesar cada archivo .txt

lectura_txt_seleccion <- function(path_a) {
  print(paste("Leyendo:", path_a))  # Imprime la ruta del archivo que se está leyendo
  
  # Ajusta el delimitador a "|"
  temp <- read_delim(path_a, delim = "¬", col_types = cols(.default = col_character()), show_col_types = FALSE) %>%
    clean_names()  # Limpia los nombres de las columnas
  
  return(temp)
}

# Lee y combina todos los archivos .txt
base_datos_combinada_seleccion <- map_df(archivos_txt_seleccion, lectura_txt_seleccion)

# FILTRO
base_datos_filtrada <- base_datos_combinada_seleccion %>%
  dplyr::filter(inst_nombre_institucion == "INSTITUCION  UNIVERSITARIA PASCUAL BRAVO-MEDELLIN")

# Guarda la base de datos combinada en un archivo .csv
write_csv(base_datos_filtrada, "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/DATA CONSOLIDADA/SaberPro_seleccion_agrupada_IUPB.csv")
save(base_datos_filtrada, file = "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/DATA CONSOLIDADA/SaberPro_seleccion_agrupada_IUPB.rdata")
