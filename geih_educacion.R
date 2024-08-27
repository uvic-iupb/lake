
# GEIH

# 1. IDENTIFICAR COLUMNAS ----------------------

# Establece la ruta donde se encuentran los archivos .csv
ruta <- "../RAW DATA/GRAN ENCUESTA GEIH/RAW_GEIH_CARAC"

# Lista todos los archivos .csv en el directorio
archivos_csv <- list.files(path = ruta, pattern = "\\.CSV$", full.names = TRUE)

# Función para leer un archivo y añadir una columna con el nombre del archivo
leer_y_anadir_nombre <- function(archivo) {
  df <- read.csv(archivo, sep = ";")
  df$nombre_archivo <- basename(archivo)
  return(df)
}

# Lee todos los archivos y verifica que tengan las mismas columnas
lista_dataframes <- lapply(archivos_csv, leer_y_anadir_nombre)
nombres_columnas <- lapply(lista_dataframes, colnames)

# Encuentra las columnas comunes y las columnas diferentes
columnas_comunes <- Reduce(intersect, nombres_columnas)
columnas_diferentes <- unique(unlist(nombres_columnas))
columnas_diferentes <- setdiff(columnas_diferentes, columnas_comunes)

# Imprime las columnas comunes y diferentes
cat("Columnas comunes:\n")
print(columnas_comunes)

cat("\nColumnas diferentes:\n")
print(columnas_diferentes)

# Verifica si todos los archivos tienen las mismas columnas
if (length(unique(nombres_columnas)) == 1) {
  # Combina todos los dataframes en uno solo
  geih_ocupados <- bind_rows(lista_dataframes)
  
  # Exporta el dataframe combinado a un nuevo archivo .csv en la misma ruta con el separador ";"
  write.csv(geih_ocupados, file = paste0(ruta, "/geih_ocupados.csv"), row.names = FALSE)
  print("Todos los archivos tienen las mismas columnas. El archivo combinado ha sido guardado.")
} else {
  print("Los archivos tienen diferentes columnas. No se ha realizado la combinación.")
}

# 2. AGRUPAR COLUMNAS ----------------------

# Función para leer un archivo y añadir una columna con el nombre del archivo
leer_y_anadir_nombre <- function(archivo) {
  df <- read.csv(archivo, sep = ";")
  df$nombre_archivo <- basename(archivo)
  return(df)
}

# Lee todos los archivos y añade la columna con el nombre del archivo
lista_dataframes <- lapply(archivos_csv, leer_y_anadir_nombre)

# Combina todos los dataframes en uno solo, ignorando las diferencias de columnas
geih_edu <- bind_rows(lista_dataframes)

# Exporta el dataframe combinado a un nuevo archivo .csv en la misma ruta con el separador ";"
write.csv(geih_ocupados, file = paste0(ruta, "/geih_ocupados.csv"), row.names = FALSE)

# 3. DICCIONARIO GEIH ----------------------

# Establece la ruta del archivo DTA
ruta_dta <- "/Users/cristianespinal/Downloads/GEIH_Diciembre_2022_Marco_2018-2/DTA/Caracter°sticas generales, seguridad social en salud y educaci¢n.DTA"

# Lee el archivo DTA y guarda los datos en un dataframe
diccionario_geih <- read_dta(ruta_dta)

# Filtra datos de Antioquia y selecciona columnas específicas
antioquia <- geih_edu %>% 
  filter(DPTO == 5) %>%  # Ajuste del código: El código departamental de Antioquia es 5
  distinct() %>% 
  group_by(DIRECTORIO) %>% 
  filter(row_number() == 1)

# Filtra y selecciona datos de educación para Antioquia
edu_anti <- geih_edu %>% 
  filter(DIRECTORIO %in% antioquia$DIRECTORIO) %>% 
  select(DIRECTORIO, P6210, P6040, P6170, P3041, P3042, P3042S1, P3042S2, P3043S1) %>% 
  mutate(rango_edad = case_when(
    P6040 <= 16 ~ "0-16",
    P6040 <= 24 ~ "17-24",
    P6040 <= 50 ~ "25-50",
    TRUE ~ "Mayores de 50"
  ))

# 3. PALABRAS DE CINE F (P3042S2) GEIH ----------------------

# Carga el archivo Excel con la descripción de los campos detallados
ruta_excel <- "../RAW DATA/GRAN ENCUESTA GEIH/RAW_GEIH_CARAC/campos_detallados_descripcion.xlsx"
campos_detallados <- read_excel(ruta_excel)

# Muestra las primeras filas del dataframe para verificar la correcta lectura
print(head(campos_detallados))

# Renombra la columna de cruce si es necesario
colnames(campos_detallados) <- make.names(colnames(campos_detallados))
campos_detallados <- campos_detallados %>%
  rename(CAMPO_DETALLADO = `CAMPO_DETALLADO`)  # Ajusta el nombre de la columna según sea necesario

# Convierte CAMPO_DETALLADO a número, reemplazando NA por 0
campos_detallados <- campos_detallados %>%
  mutate(CAMPO_DETALLADO = as.numeric(CAMPO_DETALLADO))

# Verifica la conversión
print(head(campos_detallados))

# Convierte P3042S2 a número en edu_anti, reemplazando NA por 0
edu_anti <- edu_anti %>%
  mutate(P3042S2 = as.numeric(P3042S2),
         P3042S2 = ifelse(is.na(P3042S2), 0, P3042S2))

# Verifica la conversión
print(head(edu_anti))

# Realiza la unión de los dataframes
edu_anti_merged <- edu_anti %>%
  left_join(campos_detallados, by = c("P3042S2" = "CAMPO_DETALLADO"))

# Muestra las primeras filas del dataframe resultante para verificar la correcta unión
print(head(edu_anti_merged))

# Guarda el dataframe resultante en un archivo CSV (ajusta la ruta según tus necesidades)
write.csv(edu_anti_merged, file = "/Users/cristianespinal/Downloads/edu_anti_merged.csv", row.names = FALSE)

print("La variable ha sido insertada y los datos han sido guardados con éxito.")

# BUSCAR LA P3042, GRADO. 11.Especialización // 12.Maestría

# Filtrar la base de datos por los valores 11 y 12 en la variable P3042
edu_anti_merged_posgrado <- edu_anti_merged %>% 
  filter(P3042 %in% c(11, 12))

# Hacer un conteo de registros por la variable DESCRIPCIÓN
count_by_description <- edu_anti_merged_posgrado %>% 
  group_by(DESCRIPCIÓN) %>% 
  summarise(count = n(), cine_campo = first(P3042S2))

# Mostrar el resultado
print(count_by_description)

# SNIES -- CAMPO AMPLIO

# Leer el archivo Excel
file_path <- "/Users/cristianespinal/Downloads/matriculados_snies_2022.xlsx"
matriculados_snies_2022 <- read_excel(file_path)

# Leer el archivo Excel
file_path <- "/Users/cristianespinal/Downloads/matriculados_snies_2022.xlsx"
matriculados_snies_2022 <- read_excel(file_path)

# Función para limpiar los nombres de las columnas
clean_column_names <- function(df) {
  colnames(df) <- tolower(colnames(df))
  colnames(df) <- gsub(" ", "_", colnames(df))
  colnames(df) <- gsub("'", "", colnames(df))
  colnames(df) <- stringi::stri_trans_general(colnames(df), "Latin-ASCII")
  return(df)
}

matriculados_snies_2022 <- clean_column_names(matriculados_snies_2022)

library(dplyr)

# Asegúrate de que las variables tengan el mismo tipo de dato
count_by_description$cine_campo <- as.numeric(count_by_description$cine_campo)
matriculados_snies_2022$id_cine_campo_especifico <- as.numeric(matriculados_snies_2022$id_cine_campo_especifico)

# Unir las bases de datos por cine_campo y id_cine_campo_especifico
merged_data <- left_join(matriculados_snies_2022, count_by_description, 
                         by = c("id_cine_campo_especifico" = "cine_campo"))

# Agrupar por id_cine_campo_especifico y sumar las variables count
grouped_data <- merged_data %>% 
  group_by(id_cine_campo_especifico) %>% 
  summarise(total_matriculados = sum(matriculados, na.rm = TRUE),
            count = first(count))

# Verifica el resultado
print(grouped_data)

# Agrupación de la base de datos -------------------------------------------------------------

# Verificar valores únicos
unique_id_cine_campo_especifico <- unique(matriculados_snies_2022$id_cine_codigo_detallado)
unique_cine_campo <- unique(count_by_description$cine_campo)

# Ver los valores únicos
print(unique_id_cine_campo_especifico)
print(unique_cine_campo)

# Asegúrate de que ambas columnas sean numéricas
count_by_description$cine_campo <- as.numeric(count_by_description$cine_campo)
matriculados_snies_2022$id_cine_codigo_detallado <- as.numeric(matriculados_snies_2022$id_cine_codigo_detallado)

# Verificar los valores mínimos y máximos
summary(matriculados_snies_2022$id_cine_codigo_detallado)
summary(count_by_description$cine_campo)

# Verificar si hay coincidencias
matches <- unique_id_cine_campo_especifico %in% unique_cine_campo
print(matches)

# Filtrar los valores que coinciden
matching_cine_campo <- unique_id_cine_campo_especifico[matches]

# Filtrar count_by_description para incluir solo los valores coincidentes
filtered_count_by_description <- count_by_description %>%
  filter(cine_campo %in% matching_cine_campo)

# Filtrar matriculados_snies_2022 para incluir solo los valores coincidentes
filtered_matriculados_snies_2022 <- matriculados_snies_2022 %>%
  filter(id_cine_codigo_detallado %in% matching_cine_campo)

# Unir las bases de datos filtradas
merged_data <- left_join(filtered_matriculados_snies_2022, filtered_count_by_description, 
                         by = c("id_cine_codigo_detallado" = "cine_campo"))

# Verificar el resultado
print(head(merged_data))

# Palabras ------------------------------------------------------------------------------

# Definir la función de limpieza de nombres de columnas
limpieza_colnames <- function(colname){
  temp <- colname %>% 
    gsub("_|-", " ", .) %>%              # Reemplazar guiones bajos por espacios
    gsub("[[:cntrl:]]", "", .) %>%       # Eliminar caracteres de control
    stringi::stri_trans_general(id = "Latin-ASCII") %>% 
    gsub("\\s+", " ", .) %>%             # Eliminar espacios adicionales
    gsub("[[:punct:]]", "", .) %>%       # Eliminar puntuación
    tolower(.) %>%                       # Convertir a minúsculas
    str_trim(.) %>%                      # Eliminar espacios en blanco al inicio y al final
    gsub("^$|^_$", "unnamed_column", .)  # Reemplazar nombres vacíos o "_" con "unnamed_column"
  
  return(temp)
}

# Obtener las palabras de parada en español
stopwords_es <- data.frame(word = stopwords(language = "es", source = "nltk"))

# Procesar los nombres de programas en merged_data
palabras <- merged_data %>% 
  select(codigo_snies_del_programa, programa_academico) %>% 
  distinct() %>% 
  mutate(pro = map(.x = programa_academico, ~limpieza_colnames(.x))) %>% 
  unnest_tokens(output = word, input = pro) %>% 
  anti_join(stopwords_es, by = "word")

palabras2 <- palabras %>% 
  mutate(oe = wordStem(word, language = "en"))

la_primera <- palabras2 %>% 
  group_by(oe, word) %>%
  mutate(nv = n()) %>% 
  group_by(oe) %>% 
  mutate(nveces_stem = n()) %>% 
  filter(nv == max(nv)) %>% 
  filter(row_number() == 1)

# Filtrar merged_data para los programas activos en "Arte y Humanidades" en el nivel académico "Posgrado"
programas_campo_mapli0 <- merged_data %>% 
  filter(id_cine_campo_amplio == 2, nivel_academico == "Posgrado", !is.na(estado_programa), estado_programa == "Activo")

# Filtrar merged_data y aplicar el mismo procesamiento
matricula <- merged_data %>% 
  group_by(codigo_snies_del_programa) %>% 
  inner_join(palabras2, by = "codigo_snies_del_programa")

palabras_dentro_artes <- palabras2 %>% 
  filter(codigo_snies_del_programa %in% programas_campo_mapli0$codigo_snies_del_programa) %>% 
  group_by(oe, word) %>%
  mutate(nv = n()) %>% 
  group_by(oe) %>% 
  mutate(nveces_stem = n()) %>% 
  filter(nv == max(nv)) %>% 
  filter(row_number() == 1)

# Aplicar el análisis final
matiruulas2 <- matricula %>% 
  group_by(oe) %>% 
  summarise(cantidad = sum(cantidad, na.rm = TRUE)) %>% 
  arrange(desc(cantidad)) %>% 
  left_join(palabras_dentro_artes, by = "oe") %>% 
  rename(raiz_palabra = oe, Nro_matricula = cantidad) %>% 
  mutate(ratio = Nro_matricula / nveces_stem) %>% 
  arrange(desc(ratio))

# Ver el resultado
print(matiruulas2)