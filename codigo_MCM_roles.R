# Cargar las librerías necesarias
library(pdftools)
library(stringr)
library(openxlsx)
library(pdftools)

# Función para limpiar y preparar el texto extraído del PDF
limpiar_texto <- function(texto) {
  texto <- gsub("\r", "\n", texto) # Reemplazar retornos de carro con saltos de línea
  texto <- gsub("\n{2,}", "\n", texto) # Reemplazar múltiples saltos de línea con uno solo
  texto <- gsub("", "", texto) # Eliminar símbolos de viñetas si es necesario
  texto <- trimws(texto) # Eliminar espacios en blanco al principio y al final
  return(texto)
}

# Función para extraer ocupaciones relacionadas
extraer_ocupaciones <- function(texto) {
  texto <- limpiar_texto(texto)
  patron_inicio <- "Ocupaciones relacionadas:"
  patron_final <- "Otras denominaciones:"
  
  inicio <- str_locate(texto, fixed(patron_inicio))[1, 2] + 1
  final <- str_locate(texto, fixed(patron_final))[1, 1] - 1
  
  if (!is.na(inicio) && !is.na(final) && inicio < final) {
    texto_ocupaciones <- substring(texto, inicio, final)
    ocupaciones <- unlist(str_split(texto_ocupaciones, "\n"))
    ocupaciones <- ocupaciones[nchar(ocupaciones) > 0] # Eliminar líneas vacías
    return(ocupaciones)
  } else {
    return(NA) # Retornar NA si no se encuentran las secciones
  }
}

# Lista de archivos PDF
# Lista todos los archivos PDF en el directorio y subdirectorios
archivos_pdf <- list.files(path = "/Users/cristianespinal/Downloads/CUALIFICACIONES MCM", pattern = "\\.pdf$", full.names = TRUE, recursive = TRUE)

# Filtrar solo archivos, excluyendo directorios
archivos_pdf <- archivos_pdf[sapply(archivos_pdf, function(x) file.info(x)$isdir == FALSE)]

# Inicializar un dataframe para guardar las ocupaciones de cada archivo
df_ocupaciones <- data.frame(directorio_principal = character(), 
                             subdirectorio1 = character(), 
                             subdirectorio2 = character(), 
                             archivo = character(), 
                             ocupacion = character(), 
                             stringsAsFactors = FALSE)

# Bucle para procesar cada archivo
for (archivo in archivos_pdf) {
  texto_pdf <- pdf_text(archivo)
  for (pagina in texto_pdf) {
    ocupaciones_relacionadas <- extraer_ocupaciones(pagina)
    if (length(ocupaciones_relacionadas) > 0) { # Verifica si hay ocupaciones relacionadas
      # Dividir la ruta del archivo en componentes
      partes <- unlist(strsplit(archivo, "/"))
      n <- length(partes)
      
      # Asignar componentes a las columnas correspondientes
      directorio_principal <- ifelse(n >= 2, partes[n-2], NA)
      subdirectorio1 <- ifelse(n >= 3, partes[n-3], NA)
      subdirectorio2 <- ifelse(n >= 4, partes[n-4], NA)
      
      df_temp <- data.frame(directorio_principal = directorio_principal, 
                            subdirectorio1 = subdirectorio1, 
                            subdirectorio2 = subdirectorio2, 
                            archivo = basename(archivo), 
                            ocupacion = ocupaciones_relacionadas, 
                            stringsAsFactors = FALSE)
      df_ocupaciones <- rbind(df_ocupaciones, df_temp)
    }
  }
}

# Función para limpiar las ocupaciones
limpiar_ocupaciones <- function(ocupacion) {
  ocupacion_limpia <- ocupacion %>%
    str_trim() %>%  # Eliminar espacios en blanco al principio y al final
    str_replace_all("^[0-9]+\\.?[0-9]*\\s*", "") %>%  # Eliminar números al inicio, opcionalmente seguidos por un punto y más números
    str_replace_all("^[\\-\\*:•]+\\s*", "") %>%  # Eliminar caracteres especiales al inicio
    str_replace_all("\\s+", " ")  # Reemplazar múltiples espacios por uno solo
  
  return(ocupacion_limpia)
}

# Aplicar la función de limpieza a la columna 'ocupacion'
df_ocupaciones$ocupacion <- sapply(df_ocupaciones$ocupacion, limpiar_ocupaciones)

# Eliminar filas con ocupaciones vacías o que solo contienen caracteres especiales después de la limpieza
df_ocupaciones <- df_ocupaciones %>%
  dplyr::filter(ocupacion != "" & !stringr::str_detect(ocupacion, "^[\\-\\*:•\\s]*$"))

# Exportar a Excel
ruta_directorio <- "/Users/cristianespinal/Downloads/CUALIFICACIONES MCM"
nombre_archivo_excel <- "Ocupaciones_Relacionadas_version4.xlsx"
ruta_completa <- file.path(ruta_directorio, nombre_archivo_excel)

write.xlsx(df_ocupaciones, file = ruta_completa)

# Mensaje de éxito
cat("El archivo 'Ocupaciones_Relacionadas.xlsx' ha sido creado con éxito.")
