
# 1. ENCUESTA PROCESAMIENTO -----------

library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)
library(readxl)
library(janitor)
library(dplyr)
library(purrr)
library(tidyr)
library(openxlsx)
library(stringi)
library(lubridate)

file_path <- "../DATA CONSOLIDADA/encuesta_investigacion_uvic_tabulada.xlsx"
encuesta_talento <- read_excel(file_path)
head(encuesta_talento)
names(encuesta_talento)

# Agrupa las líneas de investigación en una sola columna
encuesta_talento <- encuesta_talento %>%
  unite("Lineas_de_Investigacion", 
        c("Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Agroambiental]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Desarrollo Sostenible]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Energías Renovables y Sistemas Eléctricos]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Materiales Sostenibles]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Movilidad Sostenible]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Sistemas Inteligentes Sostenibles]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Gestión de proyectos con orientación a los ODS]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Gestión del conocimiento y de la Innovación]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Logística y Cadena De Suministro]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Productividad y Calidad]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Educación y TIC]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Instrumentación Científica e Industrial]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Modelado Computacional]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Nanotecnología]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Telecomunicaciones]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Gestión Energética]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Gestión del Mantenimiento]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Materiales y Procesos en Ingeniería]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Sistemas de Potencia]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Creatividad, interacción y mediación]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Diseño Sostenible]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Pensamiento Social]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [No aplica]"), 
        sep = "; ", na.rm = TRUE)

# Agrupa las disciplinas en una sola columna
encuesta_talento <- encuesta_talento %>%
  unite("Disciplinas", 
        c("Elija la disciplina en la que se enfoca su actividad investigativa [Ciencias Naturales]",
          "Elija la disciplina en la que se enfoca su actividad investigativa [Ingeniería y Tecnología]",
          "Elija la disciplina en la que se enfoca su actividad investigativa [Ciencias Médica y De La Salud]",
          "Elija la disciplina en la que se enfoca su actividad investigativa [Ciencias Agrícolas]",
          "Elija la disciplina en la que se enfoca su actividad investigativa [Ciencias Sociales]",
          "Elija la disciplina en la que se enfoca su actividad investigativa [Humanidades]",
          "Elija la disciplina en la que se enfoca su actividad investigativa [No aplica]"), 
        sep = "; ", na.rm = TRUE)

# Renombrar las variables según lo especificado
encuesta_talento <- encuesta_talento %>%
  rename(
    Cédula = `Cédula  (no incluir puntos ni comas, solo números ej: 1281052350)`,
    Departamento = `Elija el departamento al cual pertenece`,
    Programa_Academico = `Elija el programa académico institucional al cual aporta`,
    Grupo_Investigacion = `Elija el grupo de investigación al cual pertenece. De lo contrario, marque la opción no aplica.`,
    Enfoque_Investigativo = `Escriba el nombre específico de su enfoque investigativo (por ejemplo: Síntesis de materiales, modelamiento de procesos, modelado computacional, sistemas de potencia, etc)`,
    Nucleo_Basico_Conocimiento = `Elija el núcleo básico de conocimiento en el cuál se enfoca su quehacer investigativo`
  )

# Separa las filas
encuesta_talento <- encuesta_talento %>%
  separate_rows(Lineas_de_Investigacion, sep = "; ")
encuesta_talento <- encuesta_talento %>%
  separate_rows(Disciplinas, sep = "; ")

# Elimina duplicados basados en Identificación, Lineas_de_Investigacion y Disciplinas
encuesta_talento <- encuesta_talento %>%
  distinct(Identificación, Lineas_de_Investigacion, Disciplinas, .keep_all = TRUE)

# Contar las líneas de investigación
lineas_conteo <- encuesta_talento %>%
  group_by(Lineas_de_Investigacion) %>%
  summarise(conteo = n()) %>%
  mutate(instrumento_lineas = (conteo - min(conteo)) / (max(conteo) - min(conteo)))

# Contar las disciplinas
disciplinas_conteo <- encuesta_talento %>%
  group_by(Disciplinas) %>%
  summarise(conteo = n()) %>%
  mutate(instrumento_disciplinas = (conteo - min(conteo)) / (max(conteo) - min(conteo)))

# Contar los enfoques investigativos y normalizar los valores
enfoque_conteo <- encuesta_talento %>%
  group_by(Enfoque_Investigativo) %>%
  summarise(conteo = n()) %>%
  mutate(instrumento_enfoque = (conteo - min(conteo)) / (max(conteo) - min(conteo)))

# Contar los núcleos básicos de conocimiento y normalizar los valores
nucleo_conteo <- encuesta_talento %>%
  group_by(Nucleo_Basico_Conocimiento) %>%
  summarise(conteo = n()) %>%
  mutate(instrumento_nucleo = (conteo - min(conteo)) / (max(conteo) - min(conteo)))

# Muestra las primeras filas de los datos de conteo
head(lineas_conteo)
head(disciplinas_conteo)
head(enfoque_conteo)
head(nucleo_conteo)

# 2. WEB-SCRAPPING PROCESAMIENTO -----------

file_scrapping <- "../DATA CONSOLIDADA/Minciencias_Por_Autor_Pascual.xlsx"
web_scrapping_investigacion <- read_excel(file_scrapping)
names(produccion_productividad)

# 3. AGRUPACIÓN INDEX

# Convertir los espacios en blanco a NA y luego a 0 en Identificación
encuesta_talento$Identificación[encuesta_talento$Identificación == ""] <- NA
produccion_productividad$Identificación[produccion_productividad$Identificación == ""] <- NA

# Convertir Identificación a tipo numérico
encuesta_talento$Identificación <- as.numeric(encuesta_talento$Identificación)
produccion_productividad$Identificación <- as.numeric(produccion_productividad$Identificación)

# Unir ambas bases de datos por la variable Identificación
index_talento_produccion <- left_join(encuesta_talento, produccion_productividad, by = "Identificación")

### DISTINC

# Contar el número de filas antes de eliminar duplicados
num_filas_antes <- nrow(index_talento_produccion)

# Eliminar duplicados
index_talento_produccion_distinct <- index_talento_produccion %>%
  distinct(Identificación, Grupo_Investigacion, Categoria, Tipologia, Titulo, .keep_all = TRUE)

# Contar el número de filas después de eliminar duplicados
num_filas_despues <- nrow(index_talento_produccion_distinct)

# Calcular el número de filas eliminadas
filas_eliminadas <- num_filas_antes - num_filas_despues

# Mostrar el número de filas eliminadas
cat("Número de filas eliminadas: ", filas_eliminadas, "\n")

# 2.1. SUBSET POR LINEAS -----------

# Crear un subset que se llame index_lineas
index_lineas <- index_talento_produccion %>%
  select(Identificación, Lineas_de_Investigacion, Titulo) %>%
  distinct()

# Contar por línea de investigación cuántos investigadores tenemos (Talento)
index_lineas_talento <- index_lineas %>%
  group_by(Lineas_de_Investigacion) %>%
  summarise(Talento = n_distinct(Identificación))

# Contar el número de productos por línea (Producción)
index_lineas_produccion <- index_lineas %>%
  group_by(Lineas_de_Investigacion) %>%
  summarise(Producción = n_distinct(Titulo))

# Unir los conteos de Talento y Producción
index_lineas <- left_join(index_lineas_talento, index_lineas_produccion, by = "Lineas_de_Investigacion")

# Crear una variable que sea un índice de productividad (Producción / Talento)
index_lineas <- index_lineas %>%
  mutate(Indice_Productividad = Producción / Talento)

# Asegurar que el índice de productividad esté entre 0 y 1
index_lineas <- index_lineas %>%
  mutate(Indice_Productividad = (Indice_Productividad - min(Indice_Productividad)) / (max(Indice_Productividad) - min(Indice_Productividad)))

# Crear variable instrumento_talento normalizando Talento de 0 a 1
index_lineas <- index_lineas %>%
  mutate(instrumento_talento = (Talento - min(Talento)) / (max(Talento) - min(Talento)))

# Crear variable instrumento_producción normalizando Producción de 0 a 1
index_lineas <- index_lineas %>%
  mutate(instrumento_producción = (Producción - min(Producción)) / (max(Producción) - min(Producción)))

# Muestra las primeras filas del índice de líneas de investigación
head(index_lineas)

# 2.2. SUBSET PARA LOS DEMÁS ----------

# Crear funciones para el procesamiento y normalización
procesar_conteo <- function(data, grupo_var) {
  # Contar por grupo_var cuántos investigadores tenemos (Talento)
  talento_conteo <- data %>%
    group_by({{ grupo_var }}) %>%
    summarise(Talento = n_distinct(Identificación))
  
  # Contar el número de productos por grupo_var (Producción)
  produccion_conteo <- data %>%
    group_by({{ grupo_var }}) %>%
    summarise(Producción = n_distinct(Titulo))
  
  # Unir los conteos de Talento y Producción
  conteo <- left_join(talento_conteo, produccion_conteo, by = as_label(enquo(grupo_var)))
  
  # Crear una variable que sea un índice de productividad (Producción / Talento)
  conteo <- conteo %>%
    mutate(Indice_Productividad = Producción / Talento)
  
  # Asegurar que el índice de productividad esté entre 0 y 1
  conteo <- conteo %>%
    mutate(Indice_Productividad = (Indice_Productividad - min(Indice_Productividad, na.rm = TRUE)) / 
             (max(Indice_Productividad, na.rm = TRUE) - min(Indice_Productividad, na.rm = TRUE)))
  
  # Crear variable instrumento_talento normalizando Talento de 0 a 1
  conteo <- conteo %>%
    mutate(instrumento_talento = (Talento - min(Talento, na.rm = TRUE)) / 
             (max(Talento, na.rm = TRUE) - min(Talento, na.rm = TRUE)))
  
  # Crear variable instrumento_producción normalizando Producción de 0 a 1
  conteo <- conteo %>%
    mutate(instrumento_producción = (Producción - min(Producción, na.rm = TRUE)) / 
             (max(Producción, na.rm = TRUE) - min(Producción, na.rm = TRUE)))
  
  return(conteo)
}

# Crear subsets específicos
index_talento <- index_talento_produccion %>%
  select(Identificación, Lineas_de_Investigacion, Titulo, Enfoque_Investigativo, Nucleo_Basico_Conocimiento, Disciplinas, 
         Grupo_Investigacion, Categoria, Tipologia) %>%
  distinct()

# Procesar conteo para cada grupo

index_grupo <- procesar_conteo(web_scrapping_investigacion, `Nombre del grupo`)
library(dplyr)

index_grupo <- index_grupo %>%
  mutate(
    `Nombre del grupo` = case_when(
      `Nombre del grupo` == "GRUPO DE INVESTIGACION E INNOVACION EN ENERGIA - GIIEN" ~ "GIIEN",
      `Nombre del grupo` == "GRUPO DE INVESTIGACION EN CIENCIAS ELECTRONICAS E INFORMATICAS - GICEI" ~ "GICEI",
      `Nombre del grupo` == "INVESTIGACION E INNOVACION AMBIENTAL. GIIAM" ~ "GIIAM",
      `Nombre del grupo` == "QUALIPRO- GRUPO DE INVESTIGACION EN CALIDAD Y PRODUCTIVIDAD" ~ "QUALIPRO",
      TRUE ~ `Nombre del grupo`
    )
  )

web_scrapping_investigacion <- web_scrapping_investigacion %>%
  mutate(
    `Nombre del grupo` = case_when(
      `Nombre del grupo` == "GRUPO DE INVESTIGACION E INNOVACION EN ENERGIA - GIIEN" ~ "GIIEN",
      `Nombre del grupo` == "GRUPO DE INVESTIGACION EN CIENCIAS ELECTRONICAS E INFORMATICAS - GICEI" ~ "GICEI",
      `Nombre del grupo` == "INVESTIGACION E INNOVACION AMBIENTAL. GIIAM" ~ "GIIAM",
      `Nombre del grupo` == "QUALIPRO- GRUPO DE INVESTIGACION EN CALIDAD Y PRODUCTIVIDAD" ~ "QUALIPRO",
      TRUE ~ `Nombre del grupo`
    )
  )

# Producción por grupo de investigación



index_lineas <- procesar_conteo(index_talento, Lineas_de_Investigacion)
index_enfoques <- procesar_conteo(index_talento, Enfoque_Investigativo)
index_nucleos <- procesar_conteo(index_talento, Nucleo_Basico_Conocimiento)
index_disciplinas <- procesar_conteo(index_talento, Disciplinas)
index_categoria <- procesar_conteo(index_talento, Categoria)
index_tipologia <- procesar_conteo(index_talento, Tipologia)

# Muestra las primeras filas de cada índice
head(index_lineas)
head(index_enfoques)
head(index_nucleos)
head(index_disciplinas)

# 2.3. TIPOLOGIAS POR CATEGORÍAS --------

# Obtener las categorías únicas de index_talento_produccion
categorias <- unique(index_talento_produccion$Categoria)

# Función para procesar datos por Tipología dentro de cada Categoría
procesar_tipologia <- function(data) {
  data %>%
    group_by(Tipologia) %>%
    summarise(
      Talento = n_distinct(Identificación),
      Producción = n_distinct(Titulo)
    ) %>%
    mutate(
      Indice_Productividad = Producción / Talento
    )
}

# Crear una lista para almacenar los resultados
resultados_tipologia <- list()

# Definir la ruta base para guardar los archivos
base_path <- "../DATA CONSOLIDADA"

# Procesar datos por cada categoría y tipología
for (categoria in categorias) {
  # Filtrar por categoría
  data_categoria <- filter(index_talento_produccion, Categoria == categoria)
  
  # Procesar datos por Tipología
  resultado_tipologia <- procesar_tipologia(data_categoria)
  
  # Recalcular los valores de los instrumentos dentro de cada subset
  resultado_tipologia <- resultado_tipologia %>%
    mutate(
      instrumento_talento = (Talento - min(Talento)) / (max(Talento) - min(Talento)),
      instrumento_producción = (Producción - min(Producción)) / (max(Producción) - min(Producción)),
      Indice_Productividad = ifelse(is.nan(Indice_Productividad), 0, Indice_Productividad)
    )
  
  # Guardar el resultado en la lista
  resultados_tipologia[[categoria]] <- resultado_tipologia
  
  # Guardar el resultado en archivos CSV y XLSX
  write.csv(resultado_tipologia, 
            file = paste0(base_path, "/index_tipologia_", gsub(" ", "_", categoria), ".csv"), 
            row.names = FALSE)
  write.xlsx(resultado_tipologia, 
             file = paste0(base_path, "/index_tipologia_", gsub(" ", "_", categoria), ".xlsx"), 
             rowNames = FALSE)
}

# Guardar los resultados en un archivo .RData
save(resultados_tipologia, file = paste0(base_path, "/resultados_tipologia.RData"))

# NUEVA VERSIÓN. AJUSTES: ampliación de la base de datos de talento humano y nuevas gráficas. ----

# 3.1 AGRUPACIÓN INTEGRANTES POR GRUPOS ----

ruta <- "../RAW DATA/integrantes_grupos/"
archivos <- list.files(path = ruta, pattern = "^[^~].*\\.xlsx$", full.names = TRUE)

# Función para leer un archivo Excel y agregar la columna Grupo
leer_y_agregar_grupo <- function(archivo) {
  nombre_archivo <- str_remove(basename(archivo), "\\.xlsx$")
  datos <- read_excel(archivo)
  datos <- datos %>%
    mutate(Grupo = nombre_archivo)
  return(datos)
}

# Leer y combinar todos los archivos en un solo dataframe
investigadores_por_grupos <- archivos %>%
  lapply(leer_y_agregar_grupo) %>%
  bind_rows()

# Ver los valores únicos de la variable Grupo
unique(investigadores_por_grupos$Grupo)

# Convertir nombres a formato Nombre Propio
investigadores_por_grupos <- investigadores_por_grupos %>%
  mutate(Nombre = str_to_title(Nombre))

# Función para convertir "YYYY/MM" a "YYYY-MM-DD"
convert_date <- function(date_str) {
  if (str_detect(date_str, "/")) {
    parts <- str_split(date_str, "/", simplify = TRUE)
    paste0(parts[1], "-", str_pad(parts[2], 2, pad = "0"), "-01")
  } else {
    date_str
  }
}

# Separar las fechas en dos columnas y calcular días de vinculación
hoy <- Sys.Date()

investigadores_por_grupos <- investigadores_por_grupos %>%
  mutate(
    `Inicio Vinculación` = str_extract(`Inicio - Fin Vinculación`, "^[^ ]+"),
    `Fin Vinculación` = str_extract(`Inicio - Fin Vinculación`, "[^ ]+$"),
    `Fin Vinculación` = ifelse(`Fin Vinculación` == "Actual", as.character(hoy), `Fin Vinculación`),
    `Inicio Vinculación` = ymd(sapply(`Inicio Vinculación`, convert_date)),
    `Fin Vinculación` = ymd(sapply(`Fin Vinculación`, convert_date))
  ) %>%
  mutate(
    `Días de Vinculación` = as.integer(difftime(`Fin Vinculación`, `Inicio Vinculación`, units = "days")),
    `Vigente` = ifelse(`Fin Vinculación` == hoy, "Sí", "No")
  )

# Agrupar por Nombre y combinar los grupos en los que participan si están vigentes
agrupacion_investigadores_por_grupos <- investigadores_por_grupos %>%
  group_by(Nombre) %>%
  summarise(
    Vinculación = first(Vinculación),
    `Horas dedicación` = first(`Horas dedicación`),
    `Inicio Vinculación` = first(`Inicio Vinculación`),
    `Fin Vinculación` = first(`Fin Vinculación`),
    `Días de Vinculación` = first(`Días de Vinculación`),
    Tipologia = first(Tipologia),
    Grupo_Vigente = paste(unique(Grupo[Vigente == "Sí"]), collapse = ", "),
    Grupo_No_Vigente = paste(unique(Grupo[Vigente == "No"]), collapse = ", "),
    Vigente = ifelse(any(Vigente == "Sí"), "Sí", "No")
  ) %>%
  mutate(
    Grupo = ifelse(Grupo_Vigente != "" & Grupo_No_Vigente != "", 
                   paste(Grupo_Vigente, Grupo_No_Vigente, sep = ", "), 
                   ifelse(Grupo_Vigente != "", Grupo_Vigente, Grupo_No_Vigente))
  ) %>%
  select(-Grupo_Vigente, -Grupo_No_Vigente) %>%
  ungroup()

# 3.2 AGRUPACIÓN INTEGRANTES POR GRUPOS ----

ruta_docentes <- "../RAW DATA/Docentes/profesores_TC_20241.xlsx"
archivos <- list.files(path = ruta_docentes, pattern = "^[^~].*\\.xlsx$", full.names = TRUE)
docentes_20241 <- read_excel(ruta_docentes)

# Función para reorganizar nombres
reorganizar_nombres <- function(nombre_completo) {
  partes <- unlist(strsplit(nombre_completo, " "))
  num_palabras <- length(partes)
  
  if (num_palabras == 2) {
    nuevo_nombre <- paste(partes[2], partes[1])
  } else if (num_palabras == 3) {
    nuevo_nombre <- paste(partes[1], partes[2], partes[3])
  } else if (num_palabras == 4) {
    nuevo_nombre <- paste(partes[3], partes[4], partes[1], partes[2])
  } else {
    nuevo_nombre <- nombre_completo
  }
  
  return(nuevo_nombre)
}

# Aplicar la función a la columna 'Nombres Completo'
docentes_20241$Nombres_Reorganizados <- sapply(docentes_20241$`Nombres Completo`, reorganizar_nombres)

# 3.3 MERGE BASES DE DATOS: TALENTO HUMANO con INVESTIGADORES CVLAC ----

docentes_20241 <- docentes_20241 %>%
  mutate(Nombres_Reorganizados = tolower(Nombres_Reorganizados))

investigadores_por_grupos <- investigadores_por_grupos %>%
  mutate(Nombre = tolower(Nombre))

docentes_agrupados <- inner_join(docentes_20241, investigadores_por_grupos, by = c("Nombres_Reorganizados" = "Nombre"))

nombres_coincidentes <- nrow(docentes_agrupados)

print(paste("Se encontraron", nombres_coincidentes, "nombres coincidentes."))

print(docentes_agrupados)

# 3.4 MERGE BASES DE DATOS: RESULTADO ANTERIOR CON ENCUESTA_TALENTO ----

# Renombrar la columna 'Identificación' en 'encuesta_talento' a 'Número de documento'
encuesta_talento <- encuesta_talento %>%
  rename(`Número de documento` = Identificación)

# Convertir a character para asegurarse de que los tipos de datos sean consistentes
encuesta_talento <- encuesta_talento %>%
  mutate(`Número de documento` = as.character(`Número de documento`))

docentes_agrupados <- docentes_agrupados %>%
  mutate(`Número de documento` = as.character(`Número de documento`))

# Unir las bases de datos por 'Número de documento'
encuesta_talento_2 <- left_join(encuesta_talento, docentes_agrupados, by = "Número de documento")

# Ver cuántos registros se adicionaron
registros_adicionados <- nrow(encuesta_talento_2) - nrow(encuesta_talento)

# Mostrar el resultado
print(paste("Se adicionaron", registros_adicionados, "registros a la nueva base de datos."))

# Llenar los valores de 'Grupo_Investigacion' con 'Grupo' cuando 'Grupo_Investigacion' está vacía
encuesta_talento_2 <- encuesta_talento_2 %>%
  mutate(Grupo_Investigacion = ifelse(is.na(Grupo_Investigacion) | Grupo_Investigacion == "", Grupo, Grupo_Investigacion))

encuesta_talento_2 <- encuesta_talento_2 %>%
  select(-`Nombres Completo`, -No., -`...1`, -Nombre, -Cédula)

# Reorganizar las columnas
encuesta_talento_2 <- encuesta_talento_2 %>%
  select(
    `Número de documento`,
    Nombres_Reorganizados,
    Grupo_Investigacion,
    Lineas_de_Investigacion,
    Disciplinas,
    Enfoque_Investigativo,
    Nucleo_Basico_Conocimiento,
    `Inicio Vinculación`,
    `Fin Vinculación`,
    `Días de Vinculación`,
    Programa,
    DepartamentoAcademico,
    Nivel,
    Departamento,
    Programa_Academico,
    Genero,
    Dedicación,
    `Tipo de contratación`,
    `Duración del contrato en meses`,
    `Máximo nivel de formación obtenido`,
    `Institución en la que obtuvo el grado en el máximo nivel de formación`,
    `Modalidad del programa en el que obtuvo el máximo nivel máximo de formación (distancia o presencial)`,
    Vinculación,
    `Horas dedicación`,
    `Inicio - Fin Vinculación`,
    Tipologia,
    Vigente,
    Discapacidades,
    `Numero de hijos`,
    Estrato,
    `Estado civil`,
    Colegio,
    Barrio,
    Comuna,
    Municipio,
    `Puntaje Icfes`,
    Transporte,
    `Trabaja actualmente`,
    `Fecha de diligenciamiento`
  )

# CARGAR .RDATA

setwd("../DATA CONSOLIDADA/")
load("proyeccion_por_snies")
ls()

# 12 DE AGOSTO DE 2023 ------------------

# Priorización de tipologias por Cátegorias: Nuevo Conocimiento y Producción TyT.

# Filtrar las categorías especificadas
index_seleccion_categorias <- index_talento_produccion %>%
  filter(Tipologia %in% c("Articulos publicados", 
                          "Capitulos de libro publicados", 
                          "Prototipos", 
                          "Innovaciones generadas en la Gestión Empresarial", 
                          "Softwares", 
                          "Signos distintivos", 
                          "Innovaciones en procesos y procedimientos",
                          "Eventos Científicos",
                          "Informes de investigacion",
                          "Desarrollo Web",
                          "Consultorías científico-tecnológicas",
                          "Procesos de apropiación social del Conocimiento para el fortalecimiento o solución de asuntos de interés social",
                          "Trabajos dirigidos/tutorias",
                          "Curso de Corta Duración Dictados"))

# Mostrar la nueva base de datos
index_seleccion_categorias

conteo_lineas_tipologia <- index_seleccion_categorias %>%
  group_by(Lineas_de_Investigacion, Tipologia, Categoria) %>%
  summarise(Conteo = n(),
            Talento = n_distinct(Nombre)) %>%
  ungroup() %>%
  arrange(desc(Conteo))

conteo_lineas_tipologia <- conteo_lineas_tipologia %>%
  mutate(Productividad = Talento / Conteo)

# Etiquetas CONSOLIDADA, POR CONSOLIDAR, NINGUNA

# Calcular los promedios de Conteo y Productividad
promedio_conteo <- mean(conteo_lineas_tipologia$Conteo)
promedio_productividad <- mean(conteo_lineas_tipologia$Productividad)

# Crear la nueva variable de etiqueta
conteo_lineas_tipologia <- conteo_lineas_tipologia %>%
  mutate(Etiqueta = case_when(
    Conteo > promedio_conteo & Productividad > promedio_productividad ~ "Consolidada",
    Conteo < promedio_conteo & Productividad > promedio_productividad ~ "Por consolidar",
    TRUE ~ "No consolidada"
  ))

# Por percentiles

# Calcular los percentiles
percentil_conteo <- quantile(conteo_lineas_tipologia$Conteo, 0.40) # Usamos el percentil 60 para un criterio más flexible
percentil_productividad <- quantile(conteo_lineas_tipologia$Productividad, 0.40) # Usamos el percentil 60

# Crear la nueva variable de etiqueta con criterios más flexibles
conteo_lineas_tipologia <- conteo_lineas_tipologia %>%
  mutate(Etiqueta = case_when(
    Conteo > percentil_conteo & Productividad > percentil_productividad ~ "Consolidada",
    Conteo > percentil_conteo & Productividad <= percentil_productividad ~ "Potencial a consolidar",
    TRUE ~ "No consolidada"
  ))

# POR ENFOQUE_INVESTIGATIVO

# Calcular Conteo, Talento y Productividad por Lineas_de_Investigacion y Enfoque_Investigativo
conteo_lineas_enfoque <- index_seleccion_categorias %>%
  group_by(Lineas_de_Investigacion, Enfoque_Resumido, Categoria) %>%
  summarise(
    Conteo = n(),
    Talento = n_distinct(Nombre),
    Productividad = Talento / Conteo
  ) %>%
  ungroup()

# Calcular Conteo, Talento y Productividad por Lineas_de_Investigacion y Enfoque_Investigativo
conteo_enfoque_categoria <- index_seleccion_categorias %>%
  group_by(Nombre, Categoria, Enfoque_Resumido) %>%
  summarise(
    Conteo = n(),
    Talento = n_distinct(Nombre),
    Productividad = Talento / Conteo
  ) %>%
  ungroup()

# Calcular los percentiles para un criterio más flexible
percentil_conteo <- quantile(conteo_enfoque_categoria$Conteo, 0.60)
percentil_productividad <- quantile(conteo_enfoque_categoria$Productividad, 0.60)

# Crear la nueva variable de etiqueta con criterios más flexibles
conteo_enfoque_categoria <- conteo_enfoque_categoria %>%
  mutate(Etiqueta = case_when(
    Conteo > percentil_conteo & Productividad > percentil_productividad ~ "Consolidada",
    Conteo > percentil_conteo & Productividad <= percentil_productividad ~ "Potencial a consolidar",
    TRUE ~ "No consolidada"
  ))

# POR DISCIPLINAS

# Calcular Conteo, Talento y Productividad por Lineas_de_Investigacion y Disciplinas
conteo_lineas_disciplinas <- index_seleccion_categorias %>%
  group_by(Lineas_de_Investigacion, Disciplinas, Categoria) %>%
  summarise(
    Conteo = n(),
    Talento = n_distinct(Nombre),
    Productividad = Talento / Conteo
  ) %>%
  ungroup()

# Calcular los percentiles para un criterio más flexible
percentil_conteo <- quantile(conteo_lineas_disciplinas$Conteo, 0.60)
percentil_productividad <- quantile(conteo_lineas_disciplinas$Productividad, 0.60)

# Crear la nueva variable de etiqueta con criterios más flexibles
conteo_lineas_disciplinas <- conteo_lineas_disciplinas %>%
  mutate(Etiqueta = case_when(
    Conteo > percentil_conteo & Productividad > percentil_productividad ~ "Consolidada",
    Conteo <= percentil_conteo & Productividad > percentil_productividad ~ "Por consolidar",
    Conteo > percentil_conteo & Productividad <= percentil_productividad ~ "Potencial a consolidar",
    TRUE ~ "No consolidada"
  ))

# ENFOQUES RESUMIDOS

enfoque_resumido <- read_excel("/Users/cristianespinal/Downloads/enfoque_resumido.xlsx")
str(enfoque_resumido)

# Asegúrate de que los nombres de las columnas coincidan
enfoque_resumido <- enfoque_resumido %>%
  rename(Enfoque_Investigativo = Enfoque_Investigativo)

# Unir las bases de datos
index_seleccion_categorias <- index_seleccion_categorias %>%
  left_join(enfoque_resumido, by = "Enfoque_Investigativo")

# LISTADO DE ENFOQUES RESUMIDOS

# Agrupar por Categoría y Enfoque Resumido
listado_enfoques <- index_seleccion_categorias %>%
  group_by(Categoria, Enfoque_Resumido) %>%
  summarise(Conteo = n(), .groups = 'drop') %>%
  arrange(Categoria, desc(Conteo))

# PRODUCTOS POR AÑO

productos_por_año <- index_seleccion_categorias %>%
  group_by(Año) %>%
  summarise(Conteo = n(), .groups = 'drop') %>%
  arrange(Año, desc(Conteo))

# Convertir la columna Año a character antes de sumar el total
productos_por_año <- index_talento_produccion %>%
  group_by(Año, Grupo_Investigacion) %>%
  summarise(Conteo = n(), .groups = 'drop') %>%
  arrange(Año, desc(Conteo)) %>%
  mutate(Año = as.character(Año))

# Calcular el total de la columna Conteo
total <- productos_por_año %>%
  summarise(Año = "TOTAL", Conteo = sum(Conteo))

# Combinar el total con el dataframe original
productos_por_año <- bind_rows(productos_por_año, total)

# Convertir la columna Año a character antes de sumar el total
productos_por_año <- index_talento_produccion %>%
  group_by(Año, Grupo_Investigacion) %>%
  summarise(Conteo = n(), .groups = 'drop') %>%
  arrange(Año, desc(Conteo)) %>%
  mutate(Año = as.character(Año))

# Calcular el total de la columna Conteo
total <- productos_por_año %>%
  summarise(Año = "TOTAL", Conteo = sum(Conteo))

# Combinar el total con el dataframe original
productos_por_año <- bind_rows(productos_por_año, total)

# Ver el resultado
print(productos_por_año)

productos_por_año <- produccion_productividad %>%
  group_by(Año, `Nombre del grupo`) %>%
  summarise(Conteo = n(), .groups = 'drop') %>%
  arrange(Año, desc(Conteo)) %>%
  mutate(Año = as.character(Año))

productos_por_año <- web_scrapping_investigacion %>%
  group_by(Año) %>%
  summarise(Conteo = n(), .groups = 'drop') %>%
  arrange(Año, desc(Conteo)) %>%
  mutate(Año = as.character(Año))

productos_por_grupo <- web_scrapping_investigacion %>%
  group_by(`Nombre del grupo`) %>%
  summarise(Conteo = n(), .groups = 'drop') %>%
  arrange(`Nombre del grupo`, desc(Conteo)) %>%
  mutate(`Nombre del grupo` = as.character(`Nombre del grupo`))

produccion_productividad

# GUARDAR -----------

guardar_datos(conteo_enfoque_categoria, path_base, "conteo_enfoque_categoria")
guardar_datos(index_grupo, path_base, "index_grupo")
guardar_datos(index_seleccion_categorias, path_base, "index_seleccion_categorias")
guardar_datos(web_scrapping_investigacion, path_base, "web_scrapping_investigacion")
guardar_datos(productos_por_año, path_base, "productos_por_año")
guardar_datos(listado_enfoques, path_base, "listado_enfoques")
guardar_datos(conteo_lineas_disciplinas, path_base, "conteo_lineas_disciplinas")
guardar_datos(conteo_lineas_enfoque, path_base, "conteo_lineas_enfoque")
guardar_datos(conteo_lineas_tipologia, path_base, "conteo_lineas_tipologia")
guardar_datos(index_lineas, path_base, "index_lineas")
guardar_datos(index_seleccion_categorias, path_base, "index_seleccion_categorias")
guardar_datos(index_nucleos, path_base, "index_nucleos")
guardar_datos(index_disciplinas, path_base, "index_disciplinas")
guardar_datos(index_enfoques, path_base, "index_enfoques")
guardar_datos(index_grupo, path_base, "index_grupo")
guardar_datos(index_categoria, path_base, "index_categoria")
guardar_datos(index_tipologia, path_base, "index_tipologia")
guardar_datos(investigadores_por_grupos, path_base, "investigadores_por_grupos")
guardar_datos(agrupacion_investigadores_por_grupos, path_base, "agrupacion_investigadores_por_grupos")
