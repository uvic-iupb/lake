library(readxl)
library(dplyr)
library(janitor)
library(openxlsx)
library(glue)

# Matriculados SNIES --------------------

# Definir la función para leer y limpiar la base de datos
lectura_base <- function(path_a) { 
  print(glue("Leyendo el archivo: {path_a}"))
  tryCatch({
    temp <- read_excel(path = path_a, col_types = "text") %>% 
      clean_names()
    
    # Renombrar columnas duplicadas para estandarizar nombres
    if ("matriculados_2018" %in% colnames(temp)) {
      temp <- temp %>% rename(matriculados = matriculados_2018)
    }
    if ("codigo_del_municipio_ies" %in% colnames(temp)) {
      temp <- temp %>% rename(codigo_del_municipio = codigo_del_municipio_ies)
    }
    if ("id_area_de_conocimiento" %in% colnames(temp)) {
      temp <- temp %>% rename(id_area = id_area_de_conocimiento)
    }
    
    return(temp)
  }, error = function(e) {
    cat(glue("Error leyendo el archivo: {path_a}\nError: {e$message}\n"))
    return(NULL)
  })
}

# Definir la función para guardar los datos en varios formatos
guardar_datos <- function(data, path_base, nombre_base) {
  # Asegurar que la ruta base termina con '/'
  if (!grepl("/$", path_base)) {
    path_base <- paste0(path_base, "/")
  }
  
  # Guardar los datos en diferentes formatos con codificación UTF-8
  save(data, file = glue("{path_base}{nombre_base}.rdata"))
  write.xlsx(data, file = glue("{path_base}{nombre_base}.xlsx"), encoding = "UTF-8")
  write.csv2(data, file = glue("{path_base}{nombre_base}.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.table(data, file = glue("{path_base}{nombre_base}.txt"), sep = "\t", row.names = FALSE, fileEncoding = "UTF-8")
}

# Ruta base donde se guardarán los archivos
path_base <- "../DATA CONSOLIDADA/"

# Directorio específico de MATRICULADOS
directorio <- "MATRICULADOS"

# Obtener la lista de archivos en el directorio de MATRICULADOS
archivos <- list.files(path = glue("../RAW DATA/SNIES_IUPB/{directorio}"), full.names = TRUE)

# Leer y combinar las bases de datos de MATRICULADOS
matriculados_snies <- lapply(archivos, function(archivo) {
  lectura_base(archivo)
}) %>% 
  bind_rows()

# Filtrar filas con información válida (si hay columnas)
if (ncol(matriculados_snies) > 0) {
  matriculados_snies <- matriculados_snies %>%
    filter_all(any_vars(!is.na(.)))
}

# Validar que la consolidación no esté vacía
if(nrow(matriculados_snies) > 0) {
  # Guardar la base de datos consolidada
  guardar_datos(matriculados_snies, path_base, "matriculados_snies")
} else {
  cat(glue("No se encontraron datos válidos en el directorio: {directorio}\n"))
}

# Admitidos SNIES --------------------

# Definir la función para leer y limpiar la base de datos
lectura_base <- function(path_a) { 
  print(glue("Leyendo el archivo: {path_a}"))
  tryCatch({
    temp <- read_excel(path = path_a, col_types = "text") %>% 
      clean_names()
    
    # Renombrar columnas duplicadas para estandarizar nombres
    if ("admisiones_2017" %in% colnames(temp)) {
      temp <- temp %>% rename(admitidos = admisiones_2017)
    }
    if ("admisiones_2018" %in% colnames(temp)) {
      temp <- temp %>% rename(admitidos = admisiones_2018)
    }
    if ("id_caracter_ies" %in% colnames(temp)) {
      temp <- temp %>% rename(id_caracter = id_caracter_ies)
    }
    if ("codigo_del_municipio_ies" %in% colnames(temp)) {
      temp <- temp %>% rename(codigo_del_municipio = codigo_del_municipio_ies)
    }
    
    return(temp)
  }, error = function(e) {
    cat(glue("Error leyendo el archivo: {path_a}\nError: {e$message}\n"))
    return(NULL)
  })
}

# Definir la función para guardar los datos en varios formatos
guardar_datos <- function(data, path_base, nombre_base) {
  # Asegurar que la ruta base termina con '/'
  if (!grepl("/$", path_base)) {
    path_base <- paste0(path_base, "/")
  }
  
  # Guardar los datos en diferentes formatos con codificación UTF-8
  save(data, file = glue("{path_base}{nombre_base}.rdata"))
  write.xlsx(data, file = glue("{path_base}{nombre_base}.xlsx"), encoding = "UTF-8")
  write.csv2(data, file = glue("{path_base}{nombre_base}.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.table(data, file = glue("{path_base}{nombre_base}.txt"), sep = "\t", row.names = FALSE, fileEncoding = "UTF-8")
}

# Ruta base donde se guardarán los archivos
path_base <- "../DATA CONSOLIDADA/"

# Directorio específico de ADMITIDOS
directorio <- "ADMITIDOS"

# Obtener la lista de archivos en el directorio de ADMITIDOS
archivos <- list.files(path = glue("../RAW DATA/SNIES_IUPB/{directorio}"), full.names = TRUE)

# Leer y combinar las bases de datos de ADMITIDOS
admitidos_snies <- lapply(archivos, function(archivo) {
  lectura_base(archivo)
}) %>% 
  bind_rows()

# Filtrar filas con información válida (si hay columnas)
if (ncol(admitidos_snies) > 0) {
  admitidos_snies <- admitidos_snies %>%
    filter_all(any_vars(!is.na(.)))
}

# Validar que la consolidación no esté vacía
if(nrow(admitidos_snies) > 0) {
  # Guardar la base de datos consolidada
  guardar_datos(admitidos_snies, path_base, "admitidos_snies")
} else {
  cat(glue("No se encontraron datos válidos en el directorio: {directorio}\n"))
}

# Graduados SNIES --------------------

# Definir la función para leer y limpiar la base de datos
lectura_base <- function(path_a) { 
  print(glue("Leyendo el archivo: {path_a}"))
  tryCatch({
    temp <- read_excel(path = path_a, col_types = "text") %>% 
      clean_names()
    return(temp)
  }, error = function(e) {
    cat(glue("Error leyendo el archivo: {path_a}\nError: {e$message}\n"))
    return(NULL)
  })
}

# Definir la función para guardar los datos en varios formatos
guardar_datos <- function(data, path_base, nombre_base) {
  # Asegurar que la ruta base termina con '/'
  if (!grepl("/$", path_base)) {
    path_base <- paste0(path_base, "/")
  }
  
  # Guardar los datos en diferentes formatos con codificación UTF-8
  save(data, file = glue("{path_base}{nombre_base}.rdata"))
  write.xlsx(data, file = glue("{path_base}{nombre_base}.xlsx"), encoding = "UTF-8")
  write.csv2(data, file = glue("{path_base}{nombre_base}.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.table(data, file = glue("{path_base}{nombre_base}.txt"), sep = "\t", row.names = FALSE, fileEncoding = "UTF-8")
}

# Ruta base donde se guardarán los archivos
path_base <- "../DATA CONSOLIDADA/"

# Directorio específico de GRADUADOS
directorio <- "GRADUADOS"

# Obtener la lista de archivos en el directorio de GRADUADOS
archivos <- list.files(path = glue("../RAW DATA/SNIES_IUPB/{directorio}"), full.names = TRUE)

# Leer y combinar las bases de datos de GRADUADOS
graduados_snies <- lapply(archivos, function(archivo) {
  lectura_base(archivo)
}) %>% 
  bind_rows()

# Filtrar filas con información válida (si hay columnas)
if (ncol(graduados_snies) > 0) {
  graduados_snies <- graduados_snies %>%
    filter_all(any_vars(!is.na(.)))
}

# Validar que la consolidación no esté vacía
if(nrow(graduados_snies) > 0) {
  # Guardar la base de datos consolidada
  guardar_datos(graduados_snies, path_base, "graduados_snies")
} else {
  cat(glue("No se encontraron datos válidos en el directorio: {directorio}\n"))
}

# Graduados SNIES --------------------

library(readxl)
library(dplyr)
library(janitor)
library(openxlsx)
library(glue)

# Definir la función para leer y limpiar la base de datos
lectura_base <- function(path_a) { 
  print(glue("Leyendo el archivo: {path_a}"))
  tryCatch({
    temp <- read_excel(path = path_a, col_types = "text") %>% 
      clean_names()
    
    # Renombrar columnas duplicadas para estandarizar nombres
    if ("codigo_del_municipio_ies" %in% colnames(temp)) {
      temp <- temp %>% rename(codigo_del_municipio = codigo_del_municipio_ies)
    }
    if ("cdigo_del_municipio_programa" %in% colnames(temp)) {
      temp <- temp %>% rename(codigo_del_municipio = cdigo_del_municipio_programa)
    }
    if ("id_area_de_conocimiento" %in% colnames(temp)) {
      temp <- temp %>% rename(id_area = id_area_de_conocimiento)
    }
    
    return(temp)
  }, error = function(e) {
    cat(glue("Error leyendo el archivo: {path_a}\nError: {e$message}\n"))
    return(NULL)
  })
}

# Definir la función para guardar los datos en varios formatos
guardar_datos <- function(data, path_base, nombre_base) {
  # Asegurar que la ruta base termina con '/'
  if (!grepl("/$", path_base)) {
    path_base <- paste0(path_base, "/")
  }
  
  # Guardar los datos en diferentes formatos con codificación UTF-8
  save(data, file = glue("{path_base}{nombre_base}.rdata"))
  write.xlsx(data, file = glue("{path_base}{nombre_base}.xlsx"), encoding = "UTF-8")
  write.csv2(data, file = glue("{path_base}{nombre_base}.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.table(data, file = glue("{path_base}{nombre_base}.txt"), sep = "\t", row.names = FALSE, fileEncoding = "UTF-8")
}

# Ruta base donde se guardarán los archivos
path_base <- "../DATA CONSOLIDADA/"

# Directorio específico de GRADUADOS
directorio <- "GRADUADOS"

# Obtener la lista de archivos en el directorio de GRADUADOS
archivos <- list.files(path = glue("../RAW DATA/SNIES_IUPB/{directorio}"), full.names = TRUE)

# Leer y combinar las bases de datos de GRADUADOS
graduados_snies <- lapply(archivos, function(archivo) {
  lectura_base(archivo)
}) %>% 
  bind_rows()

# Filtrar filas con información válida (si hay columnas)
if (ncol(graduados_snies) > 0) {
  graduados_snies <- graduados_snies %>%
    filter_all(any_vars(!is.na(.)))
}

# Validar que la consolidación no esté vacía
if(nrow(graduados_snies) > 0) {
  # Guardar la base de datos consolidada
  guardar_datos(graduados_snies, path_base, "graduados_snies")
} else {
  cat(glue("No se encontraron datos válidos en el directorio: {directorio}\n"))
}
