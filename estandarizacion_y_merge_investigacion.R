# Leer archivo 

GrupLAC <- read.csv(file = "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/DATA CONSOLIDADA/GrupLac_IUPB.csv")
Inv_produccion_real <- read.csv(file = "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/Investigación/Grupos_investigacion/Inv_Produccion_real.csv") 

# Suponemos que tu dataframe GrupLAC ya está cargado y tiene las columnas originales que mencionaste

# Cambiamos los nombres de las columnas específicas
colnames(GrupLAC)[colnames(GrupLAC) == "nombre_grupo"] <- "nme_grupo_gr"
colnames(GrupLAC)[colnames(GrupLAC) == "Categoria"] <- "nme_clase_pd"
colnames(GrupLAC)[colnames(GrupLAC) == "Tipologia"] <- "nme_tipologia_pd"
colnames(GrupLAC)[colnames(GrupLAC) == "Titulo"] <- "nombre_estandar"

# Asegúrate de que el dataframe y la columna existen
if ("nme_clase_pd" %in% colnames(GrupLAC)) {
  # Convertir toda la cadena a minúsculas y luego capitalizar la primera letra
  GrupLAC$nme_clase_pd <- sapply(GrupLAC$nme_clase_pd, function(x) {
    paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))
  })
}

# Asegúrate de que el dataframe y la columna "Año" existen
if ("Año" %in% colnames(GrupLAC)) {
  # Crear la nueva columna de fecha
  GrupLAC$fcreacion_pd <- as.Date(paste(GrupLAC$Año, "01", "01", sep="-"), "%Y-%m-%d")
  
  # Formatear la fecha en el formato deseado "día-mes-año"
  GrupLAC$fcreacion_pd <- format(GrupLAC$fcreacion_pd, "%d-%m-%Y")
}
# Asegurarnos que 'fcreacion_pd' esté en formato de fecha para poder hacer la comparación
GrupLAC$fcreacion_pd <- as.Date(GrupLAC$fcreacion_pd, format = "%d-%m-%Y")

# Eliminar filas con fechas anteriores al 31 de diciembre de 2020
GrupLAC <- GrupLAC[GrupLAC$fcreacion_pd > as.Date("2020-12-31"), ]

# Eliminar la columna 'nme_grupo_gr'
GrupLAC$nme_grupo_gr <- NULL
GrupLAC$Año <- NULL

library(lubridate)
library(dplyr)

# Asegurar que Inv_produccion_real es un data frame y fcreacion_pd está disponible
if ("fcreacion_pd" %in% names(Inv_produccion_real)) {
  # Convertir las fechas al formato correcto y manejar posibles errores
  Inv_produccion_real <- Inv_produccion_real %>%
    mutate(fcreacion_pd = dmy(fcreacion_pd), # Convertir a formato fecha
           fcreacion_pd = if_else(year(fcreacion_pd) < 100, # Corregir fechas incorrectas
                                  update(fcreacion_pd, year = 1900 + year(fcreacion_pd)),
                                  fcreacion_pd))
  
  # Formatear la fecha como 'dd-mm-yyyy'
  Inv_produccion_real <- Inv_produccion_real %>%
    mutate(fcreacion_pd = format(fcreacion_pd, "%d-%m-%Y"))
} else {
  stop("La columna fcreacion_pd no existe en el dataframe Inv_produccion_real.")
}

# Verificar resultados
head(Inv_produccion_real)

# Supongamos que df es tu dataframe y fecha_original la columna con fechas
GrupLAC <- GrupLAC %>%
  mutate(fecha_formateada = format(ymd(fcreacion_pd), "%d-%m-%Y"))

GrupLAC$fcreacion_pd <- NULL
fcreacion_pd <- GrupLAC$fecha_formateada

colnames(GrupLAC)[colnames(GrupLAC) == "fecha_formateada"] <- "fcreacion_pd"

write.csv(GrupLAC, file = "../RAW DATA/Investigación/Mutate/GrupLAC_tratada.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(Inv_produccion_real, file = "../RAW DATA/Investigación/Mutate/Inv_produccion_real_tratada.csv", row.names = FALSE, fileEncoding = "UTF-8")

# Cargar las bibliotecas
library(dplyr)
library(readr)

# Leer los archivos
GrupLAC_tratada <- read_csv("../RAW DATA/Investigación/Mutate/GrupLAC_tratada.csv")
Inv_produccion_real_tratada <- read_csv("../RAW DATA/Investigación/Mutate/Inv_produccion_real_tratada.csv")

# Combinar los archivos
Inv_existencia_y_calidad <- bind_rows(GrupLAC_tratada, Inv_produccion_real_tratada)

# Guardar el archivo combinado en un nuevo CSV
write_csv(Inv_existencia_y_calidad, "../RAW DATA/Investigación/Mutate/Inv_existencia_y_calidad.csv")

# Existencia // existencia y calidad

Inv_existencia_y_calidad <- Inv_existencia_y_calidad %>%
  mutate(Etiqueta = if_else(dmy(fcreacion_pd) < dmy("31-12-2020"), 
                            "Existencia y calidad", 
                            "Existencia"))


save(Inv_existencia_y_calidad, file = "../DATA CONSOLIDADA/Inv_existencia_y_calidad.rdata")
write_csv(Inv_existencia_y_calidad, "../DATA CONSOLIDADA/Inv_existencia_y_calidad.csv")

# Cambiarle el delimitador por //

Inv_existencia_y_calidad <- read.csv("../DATA CONSOLIDADA/Inv_existencia_y_calidad.csv", sep = ",", header = TRUE)

# Leer el archivo como líneas de texto
lines <- readLines("../DATA CONSOLIDADA/Inv_existencia_y_calidad.csv")

# Unir todas las líneas en una sola cadena de texto
all_text <- paste(lines, collapse = "")

# Encontrar caracteres únicos
unique_chars <- unique(strsplit(all_text, "")[[1]])

# Lista de posibles delimitadores
possible_delimiters <- c(";", "|", "\t", ":", "//", "@", "#", "^", "&", "*", "`")

# Filtrar aquellos que no están en los datos
unused_delimiters <- setdiff(possible_delimiters, unique_chars)

# Mostrar los delimitadores no usados
print(unused_delimiters)

# Guardar el archivo con el delimitador '`'
write.table(Inv_existencia_y_calidad, "../DATA CONSOLIDADA/Inv_existencia_y_calidad_delimitador.csv", sep = "`", row.names = FALSE, col.names = TRUE, quote = FALSE)

# Recuento para GrupLAC por año

# Asumiendo que GrupLAC_tratada es tu data frame y fcreacion_pd la columna de fechas
resultado <- GrupLAC_tratada %>%
  mutate(Ano = year(dmy(fcreacion_pd))) %>%  # Convertir la fecha y extraer el año
  group_by(Ano) %>%                          # Agrupar por año
  summarise(Recuento = n())                  # Hacer el recuento de filas por año

# Mostrar el resultado
print(resultado)

# Cargar la librería dplyr
library(dplyr)

# Asegurar que la columna fcreacion_pd sea interpretada correctamente como fechas
GrupLAC_tratada$fcreacion_pd <- as.Date(GrupLAC_tratada$fcreacion_pd, format = "%d-%m-%Y")

# Extraer el año de la fecha de creación
GrupLAC_tratada$Year <- format(GrupLAC_tratada$fcreacion_pd, "%Y")

# Agrupar los datos por nombre del grupo y por año, y luego contar las ocurrencias
conteo_anual <- GrupLAC_tratada %>%
  group_by(Nombredelgrupo, Year) %>%
  summarise(Count = n(), .groups = 'drop')  # Contar las ocurrencias y eliminar el agrupamiento

# Ver el resultado
print(conteo_anual)

write_csv(conteo_anual, "../RAW DATA/Investigación/Mutate/conteo_por_grupo_24.csv")

Integrantes_grupos <- read_excel(path = "../RAW DATA/Investigación/Grupos_investigacion/Integrantes_grupos_IUPB.xlsx")

library(dplyr)
library(tidyr)
library(lubridate)

# Asegurar que las columnas de fecha son del tipo Date
Integrantes_grupos$Inicio_tratada <- as.Date(Integrantes_grupos$Inicio_tratada)
Integrantes_grupos$Fin_tratada <- as.Date(Integrantes_grupos$Fin_tratada)

# Crear una secuencia de años para cada investigador
Integrantes_grupos <- Integrantes_grupos %>%
  rowwise() %>%
  mutate(Year = list(seq(from = year(Inicio_tratada), 
                         to = if_else(is.na(Fin_tratada), year(Sys.Date()), year(Fin_tratada)), 
                         by = 1))) %>%
  unnest(Year)

# Agrupar por nombre del grupo y por año, contar los investigadores únicos
conteo_investigadores <- Integrantes_grupos %>%
  group_by(nombre_grupo, Year) %>%
  summarise(N_investigadores = n_distinct(Nombre), .groups = 'drop')

# Ver el resultado
print(conteo_investigadores)

write_csv(conteo_investigadores, "/Users/cristianespinal/Downloads/conteo_investigadores.csv")
