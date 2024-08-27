
# Función para cargar los datos con la segunda fila como nombres de columnas y añadir el nombre del archivo como columna
load_data_with_filename <- function(file_path) {
  temp_data <- read_xlsx(file_path, col_names = FALSE)
  colnames(temp_data) <- as.character(temp_data[2, ])
  temp_data <- temp_data[-(1:2), ]
  temp_data$source_file <- basename(file_path)  # Asegurarse de añadir esta columna
  return(temp_data)
}

# Función para limpiar los nombres de las columnas
clean_column_names <- function(df) {
  colnames(df) <- gsub("ListadoDeReporteDePlanDeTrabajoParaExcelViewModel", "", colnames(df))
  colnames(df) <- gsub("#agg", "", colnames(df))
  colnames(df) <- gsub("[^[:alnum:]_]", "", colnames(df))  # Mejorada para permitir guiones bajos
  return(df)
}

# Función para estandarizar los nombres de columnas y tipos de datos
standardize_df <- function(df) {
  expected_cols <- c("Docente", "EstadoDePlanDeTrabajo", "Facultad", "Identificacion",
                     "PorcentajeDeApoyo", "PorcentajeDeDocenciaDirecta",
                     "PorcentajeDeExtensionYOtra",
                     "PorcentajeDeInvestigacion", "TotalTiempoApoyo",
                     "TotalTiempoDocenciaDirecta",
                     "TotalTiempoExtensionYOtra", "TotalTiempoInvestigacion", "source_file")
  # Asegurar que todas las columnas esperadas existen antes de reordenar
  missing_cols <- setdiff(expected_cols, colnames(df))
  if (length(missing_cols) > 0) {
    df[missing_cols] <- NA  # Agregar columnas faltantes como NA
  }
  df <- df[, expected_cols, drop = FALSE]  # Reordenar y seleccionar solo las columnas esperadas
  return(df)
}

# Define el path de la carpeta
folder_path <- "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/Planes de trabajo"

# Lista todos los archivos Excel en el directorio
excel_files <- list.files(path = folder_path, pattern = "\\.xlsx$", full.names = TRUE)

# Carga todos los archivos, limpia los nombres de las columnas, y estandariza
data_list <- lapply(excel_files, function(file) {
  df <- load_data_with_filename(file)
  df <- clean_column_names(df)
  df <- standardize_df(df)
  return(df)
})

# Función para reemplazar el punto decimal por coma en todo el DataFrame
replace_decimal_separator_all <- function(df) {
  for (col_name in colnames(df)) {
    # Si la columna es numérica, conviértela a carácter
    if (is.numeric(df[[col_name]])) {
      df[[col_name]] <- as.character(df[[col_name]])
    }
    # Reemplaza el punto por una coma
    if (is.character(df[[col_name]])) {
      df[[col_name]] <- gsub("\\.", ",", df[[col_name]])
    }
  }
  return(df)
}

# Función para dividir entre 100 las columnas específicas
scale_columns <- function(df, columns) {
  for (col_name in columns) {
    if (col_name %in% colnames(df)) {
      # Convierte de carácter a numérico, divide entre 100, y vuelve a texto con coma
      df[[col_name]] <- as.numeric(gsub(",", ".", df[[col_name]])) / 100
      df[[col_name]] <- gsub("\\.", ",", as.character(df[[col_name]]))
    }
  }
  return(df)
}

# Nombres de las columnas que necesitan dividirse entre 100
target_columns <- c("PorcentajeDeApoyo", "PorcentajeDeDocenciaDirecta", 
                    "PorcentajeDeExtensionYOtra", "PorcentajeDeInvestigacion")

# Reemplazar punto por coma en todas las columnas y luego dividir las específicas entre 100
planes_de_trabajo <- replace_decimal_separator_all(planes_de_trabajo)
planes_de_trabajo <- scale_columns(planes_de_trabajo, target_columns)

# Función para procesar la columna source_file y extraer año y semestre
process_source_file <- function(df) {
  # Extrae el año y semestre del nombre del archivo
  df$year_semester <- gsub("PlanesDeTrabajo_(\\d{5}),xlsx", "\\1", df$source_file)
  
  # Extrae el año (los primeros 4 caracteres)
  df$year <- substr(df$year_semester, 1, 4)
  
  # Extrae el semestre (el último caracter)
  df$semestre <- substr(df$year_semester, 5, 5)
  
  # Convierte la columna año a Date con el formato dd-mm-yyyy
  # Asumiendo el inicio de cada año para el primer semestre y mediados para el segundo
  df$fecha <- as.Date(paste0("01-0", ifelse(df$semestre == "1", "1", "7"), "-", df$year), format = "%d-%m-%Y")
  
  # Limpia las columnas que no necesitamos más
  df <- df %>% select(-year_semester, -year)
  
  return(df)
}

# Aplica la función al DataFrame
planes_de_trabajo <- process_source_file(planes_de_trabajo)

# Definir la función guardar_datos con el uso correcto del paquete openxlsx para escribir archivos .xlsx
guardar_datos <- function(data, path_base, nombre_base) {
  # Asegurar que la ruta base termina con '/'
  if (!grepl("/$", path_base)) {
    path_base <- paste0(path_base, "/")
  }
  
  # Guardar los datos en diferentes formatos con codificación UTF-8
  save(data, file = paste0(path_base, nombre_base, ".rdata"))
  write.xlsx(data, file = paste0(path_base, nombre_base, ".xlsx"))
  write.csv2(data, file = paste0(path_base, nombre_base, ".csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.table(data, file = paste0(path_base, nombre_base, ".txt"), sep = "\t", row.names = FALSE, fileEncoding = "UTF-8")
}

# Ruta base donde se guardarán los archivos
path_base <- "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/DATA CONSOLIDADA/"   

# Nombre base para los archivos
nombre_base <- "Planes_de_Trabajo"

# Aplicar la función guardar_datos al dataframe planes_de_trabajo
guardar_datos(planes_de_trabajo, path_base, nombre_base)

############################################################################################################################################################

## Agrupación de Planes de Trabajo

# Instala el paquete readxl si no lo tienes instalado
if (!require(readxl)) install.packages("readxl", dependencies = TRUE)
if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE)
library(openxlsx)

# Carga las bibliotecas necesarias
library(readxl)
library(dplyr)

# Función para cargar los datos con la segunda fila como nombres de columnas y añadir el nombre del archivo como columna
load_data_with_filename <- function(file_path) {
  # Cargar el archivo sin nombres de columnas
  temp_data <- read_xlsx(file_path, col_names = FALSE)
  # Usar la segunda fila como nombres de columnas
  colnames(temp_data) <- as.character(temp_data[2, ])
  # Eliminar las dos primeras filas para comenzar desde la tercera fila
  temp_data <- temp_data[-(1:2), ]
  # Añadir el nombre del archivo como una nueva columna
  temp_data$source_file <- basename(file_path)
  return(temp_data)
}

# Define el path de la carpeta
folder_path <- "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/Planes de trabajo"

# Lista todos los archivos Excel en el directorio
excel_files <- list.files(path = folder_path, pattern = "\\.xlsx$", full.names = TRUE)

# Carga todos los archivos aplicando la función
data_list <- lapply(excel_files, load_data_with_filename)

# Unir todos los data.frames en uno solo, si es necesario
combined_data <- bind_rows(data_list)

# Función para limpiar los nombres de las columnas
clean_column_names <- function(df) {
  # Elimina cualquier prefijo innecesario en los nombres de columnas
  colnames(df) <- gsub("/ListadoDeReporteDePlanDeTrabajoParaExcelViewModel/", "", colnames(df))
  
  # Elimina caracteres especiales adicionales, como los agregados por la función `read_xlsx`
  colnames(df) <- gsub("#agg", "", colnames(df))
  colnames(df) <- gsub("[^[:alnum:] ]", "", colnames(df))
  
  return(df)
}

# Aplica la función a cada DataFrame cargado
PD_20241 <- clean_column_names(PD_20241)
PD_20232 <- clean_column_names(PD_20232)
PD_20231 <- clean_column_names(PD_20231)
PD_20222 <- clean_column_names(PD_20222)
PD_20221 <- clean_column_names(PD_20221)

# Estandarizar nombres <- agrupar 

# Verificar los nombres y tipos de las columnas de cada DataFrame
sapply(data_list, colnames)
sapply(data_list, function(df) sapply(df, class))

# Función para estandarizar los nombres de columnas y tipos de datos
standardize_df <- function(df) {
  # Define los nombres de las columnas esperados y sus tipos
  # (ajusta esta parte según tus necesidades específicas)
  expected_cols <- c("Docente", "EstadoDePlanDeTrabajo", "Facultad", "Identificacion",
                    "PorcentajeDeApoyo", "PorcentajeDeDocenciaDirecta",
                    "PorcentajeDeExtensionYOtra",
                    "PorcentajeDeInvestigacion", "TotalTiempoApoyo",
                    "TotalTiempoDocenciaDirecta",
                    "TotalTiempoExtensionYOtra", "TotalTiempoInvestigacion")
  # Reordenar las columnas según los nombres esperados y convertir tipos si es necesario
  df <- df[, expected_cols, drop = FALSE]
  # Aquí podrías añadir conversiones de tipo si fuera necesario
  return(df)
}

# Aplicar la estandarización a cada DataFrame
data_list <- lapply(data_list, standardize_df)

# Combinar los DataFrames limpios
planes_de_trabajo <- bind_rows(data_list, .id = "file_name") %>%
  mutate(fecha = extract_fecha(file_name)) %>%
  select(-file_name)  # Elimina la columna file_name si no es necesaria

# Función para reemplazar el punto decimal por una coma
replace_decimal_separator <- function(df) {
  # Recorre cada columna del DataFrame
  for (col_name in colnames(df)) {
    # Si la columna es numérica, conviértela primero a carácter
    if (is.numeric(df[[col_name]])) {
      df[[col_name]] <- as.character(df[[col_name]])
    }
    # Reemplaza el punto por una coma si es una columna de tipo carácter
    if (is.character(df[[col_name]])) {
      df[[col_name]] <- gsub("\\.", ",", df[[col_name]])
    }
  }
  return(df)
}

# Aplica la función a `planes_de_trabajo`
planes_de_trabajo <- replace_decimal_separator(planes_de_trabajo)

# Definir la función guardar_datos con el uso correcto del paquete openxlsx para escribir archivos .xlsx
guardar_datos <- function(data, path_base, nombre_base) {
  # Asegurar que la ruta base termina con '/'
  if (!grepl("/$", path_base)) {
    path_base <- paste0(path_base, "/")
  }
  
  # Guardar los datos en diferentes formatos con codificación UTF-8
  save(data, file = paste0(path_base, nombre_base, ".rdata"))
  write.xlsx(data, file = paste0(path_base, nombre_base, ".xlsx"))
  write.csv2(data, file = paste0(path_base, nombre_base, ".csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.table(data, file = paste0(path_base, nombre_base, ".txt"), sep = "\t", row.names = FALSE, fileEncoding = "UTF-8")
}

# Ruta base donde se guardarán los archivos
path_base <- "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/DATA CONSOLIDADA/"

# Nombre base para los archivos
nombre_base <- "Planes_de_Trabajo"

# Aplicar la función guardar_datos al dataframe planes_de_trabajo
guardar_datos(planes_de_trabajo, path_base, nombre_base)

