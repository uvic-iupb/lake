library(readr)
library(dplyr)
library(arrow)

detect_delimiter <- function(file_path) {
  line <- readLines(file_path, n = 1)
  if (grepl(",", line)) {
    return(",")
  } else if (grepl(";", line)) {
    return(";")
  } else if (grepl("\t", line)) {
    return("\t")
  } else if (grepl("\\|", line)) {
    return("|")
  } else if (grepl("¬", line)) {
    return("¬")
  } else {
    return(" ")
  }
}

path <- "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/Saber Pro & TyT/txt_especificas_proytyt"
files <- list.files(path = path, pattern = "\\.txt$", full.names = TRUE)

all_data <- lapply(files, function(file) {
  delim <- detect_delimiter(file)
  df <- read_delim(file, delim = delim, col_types = cols(.default = col_character()), guess_max = 1000, show_col_types = FALSE)
  df
}) %>% bind_rows()

grouped_data <- all_data %>% group_by(ESTU_CONSECUTIVO)

# Especificar el directorio de destino
output_directory <- "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/DATA CONSOLIDADA"

# Nombre base para los archivos
base_filename <- "agrupación_por_ID_genericas_y_especificas_pro_y_tyt"

# Construir los nombres completos de los archivos con sus rutas
csv_filename <- file.path(output_directory, paste0(base_filename, ".csv"))
rdata_filename <- file.path(output_directory, paste0(base_filename, ".rdata"))

# Exportar a CSV
write.csv(grouped_data, csv_filename, row.names = FALSE)

# Nombre del archivo Parquet
parquet_filename <- file.path(output_directory, "agrupación_por_ID_genericas_y_especificas_pro_y_tyt.parquet")

# Exportar a Parquet
write_parquet(grouped_data, parquet_filename)

# Exportar a RData
save(grouped_data, file = rdata_filename)

library(dplyr)

# Mutate entre COMP_NOMBRE, RESULT_NOMBREPRUEBA
all_data <- all_data %>%
  mutate(NOMBRE_PRUEBA = coalesce(COMP_NOMBRE, RESULT_NOMBREPRUEBA)) %>%
  select(-COMP_NOMBRE, -RESULT_NOMBREPRUEBA) # Remueve las columnas originales si es necesario

all_data <- all_data %>%
  mutate(CONSECUTIVO = coalesce(ESTU_CONSECUTIVO, ESTU_SNP)) %>%
  select(-ESTU_CONSECUTIVO, -ESTU_SNP) # Remueve las columnas originales si es necesario

###################

# Instalar data.table si aún no está instalado
if (!requireNamespace("data.table", quietly = TRUE)) {
  install.packages("data.table")
}

# Cargar la librería
library(data.table)

# Especificar el directorio de destino y el nombre del archivo
output_directory <- "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/DATA CONSOLIDADA"
txt_filename <- file.path(output_directory, "agrupación_por_ID_genericas_y_especificas_pro_y_tyt.txt")

# Convertir el dataframe a data.table
dt <- as.data.table(grouped_data)

# Exportar a TXT (CSV con otro delimitador si es necesario)
fwrite(dt, txt_filename, sep = ",", col.names = TRUE, row.names = FALSE)



