
# Analisis de oferta - antiguedad y crecimiento

library(readxl)
library(dplyr)
library(janitor)
library(readxl)
library(dplyr)
library(lubridate)
library(janitor)
library(tidytext)

# Cargar BD programas SNIES 

file_path_1 <- "../RAW DATA/SNIES/Programas/Programas.xlsx"
programas_data <- read_excel(file_path_1)
head(programas_data)
programas_data <- clean_names(programas_data)
head(programas_data)
str(programas_data)

# Creación variable: antiguedad del programa --------------------------------------------------

programas_data <- programas_data %>%
  mutate(antiguedad_snies = as.numeric(difftime(Sys.Date(), fecha_de_registro_en_snies, units = "days")) / 365.25)

# Creación variable: tasa de crecimiento --------------------------------------------------

programas_data <- programas_data %>%
  mutate(tasa_crecimiento = ifelse(antiguedad_snies > 0, 1 / antiguedad_snies, 0))

# Agrupar los datos y calcular las métricas agregadas
resumen_programas <- programas_data %>%
  group_by(cine_f_2013_ac_campo_amplio, cine_f_2013_ac_campo_especific, cine_f_2013_ac_campo_detallado, area_de_conocimiento, nucleo_basico_del_conocimiento) %>%
  summarise(total_programas = n(),
            crecimiento_promedio = mean(tasa_crecimiento, na.rm = TRUE)) %>%
  arrange(desc(crecimiento_promedio))

# Detalle para un área de conocimiento específica
area_especifica <- "Ingeniería, arquitectura, urbanismo y afines"

detalle_area <- programas_data %>%
  filter(area_de_conocimiento == area_especifica) %>%
  group_by(nombre_del_programa, cine_f_2013_ac_campo_especific) %>%
  summarise(total_programas = n(),
            crecimiento_promedio = mean(tasa_crecimiento, na.rm = TRUE)) %>%
  arrange(desc(crecimiento_promedio))

# Metricas ----------------------------------------

# Extraer el año de la fecha de registro
programas_data <- programas_data %>%
  mutate(año_registro = year(fecha_de_registro_en_snies))

# Calcular el número de nuevos programas por año
nuevos_programas_por_año <- programas_data %>%
  group_by(año_registro) %>%
  summarise(total_nuevos_programas = n()) %>%
  arrange(año_registro)

# Agrupar y contar programas por departamento y municipio
distribucion_geografica <- programas_data %>%
  group_by(departamento_oferta_programa, municipio_oferta_programa) %>%
  summarise(total_programas = n()) %>%
  arrange(desc(total_programas))

# Agrupar por modalidad y año de registro
popularidad_modalidad <- programas_data %>%
  group_by(modalidad, año_registro) %>%
  summarise(total_programas = n()) %>%
  arrange(año_registro)

# Calcular la tasa de acreditación de calidad
tasa_acreditacion <- programas_data %>%
  group_by(area_de_conocimiento) %>%
  summarise(total_programas = n(),
            programas_acreditados = sum(reconocimiento_del_ministerio == "Acreditación de alta calidad", na.rm = TRUE),
            tasa_acreditacion = programas_acreditados / total_programas) %>%
  arrange(desc(tasa_acreditacion))

# Calcular la duración promedio de los programas
duracion_promedio <- programas_data %>%
  group_by(area_de_conocimiento, nivel_de_formacion) %>%
  summarise(duracion_promedio = mean(numero_periodos_de_duracion, na.rm = TRUE)) %>%
  arrange(desc(duracion_promedio))

# Calcular el costo promedio de matrícula
costo_promedio_matricula <- programas_data %>%
  group_by(area_de_conocimiento, nivel_de_formacion) %>%
  summarise(costo_promedio = mean(costo_matricula_estud_nuevos, na.rm = TRUE)) %>%
  arrange(desc(costo_promedio))

# Analisis por palabras clave ------------------------------------

# Diseño - Arte y Humanidades

# Crear una lista de palabras clave
palabras_clave <- c("creatividad", "color", "periodismo", "negocio", "humanistico", "lingistica", "solidaria", 
                    "3d", "entorno", "transmedia", "empresa", "gerencia", "cultura", "tecnica", "administracion", 
                    "sistema", "arquitectura", "bioclimatica", "etnoliteratura", "audiovisu", "literario", 
                    "bioetica", "educacion", "brand", "construccion", "industri", "moral", "problema", "sosten", 
                    "especializacion", "genero", "interculturalidad", "literaria", "digital", "gestion", 
                    "ingenieria", "hermeneutica", "humanismo", "persona", "sinfonica", "teologica", "digit", 
                    "publicitario", "animacion", "traduccion", "escritura", "interpretacion", "publicidad", 
                    "electronica", "instrument", "tiempo", "humanidad", "art", "filosofia", "musica", 
                    "comunicacion", "ciencia", "lectura", "creativa", "social", "proyecto", "ensenanza", 
                    "espanol", "literatura", "linguistica", "ingl", "contemporaneo", "clasica", "clinico", 
                    "fotografia", "laboratorio", "musicologia", "paisaj", "responsabilidad", "maestria", 
                    "estudio", "contemporanea", "extranjera", "market", "doctorado", "pedagogia", "colombiana", 
                    "nuevo", "teologia", "etica", "plastica", "aprendizaj", "autodirigido", "biblia", "labor", 
                    "muebl", "historia", "lengua", "aplicada", "estetica", "antropologia", "cultural", 
                    "produccion", "lenguaj", "diseno", "contenido", "desarrollo", "creacion", "cultur", 
                    "direccion", "medio", "patrimonio", "visual", "archivistica", "bursatil", "clasico", 
                    "estrategico", "financiero", "interactivo", "interdisciplinar", "teatro", "viva", 
                    "tecnologica", "producto", "industria", "didactica", "dramaturgia", "innovacion", "ilustracion")

# Crear una lista de niveles de formación
niveles_formacion <- c("Maestría", "Maestria", "Especialización universitaria", "Especialización", "Doctorado")

# Filtrar los programas que contienen alguna de las palabras clave en su nombre, pertenecen a Arte y Humanidades y tienen los niveles de formación especificados
programas_filtrados <- programas_data %>%
  filter((grepl(paste(palabras_clave, collapse = "|"), nombre_del_programa, ignore.case = TRUE) |
            cine_f_2013_ac_campo_amplio == "Arte y Humanidades") &
           nivel_de_formacion %in% niveles_formacion)

# Filtrar los programas que contienen alguna de las palabras clave en su nombre, pertenecen a Arte y Humanidades y tienen los niveles de formación especificados
programas_filtrados <- programas_data %>%
  filter((grepl(paste(palabras_clave, collapse = "|"), nombre_del_programa, ignore.case = TRUE) |
            cine_f_2013_ac_campo_amplio == "Arte y Humanidades") &
           nivel_de_formacion %in% niveles_formacion)

# Calcular fecha límite para considerar programas recientes (últimos 5 años)
años_recientes <- 5
fecha_limite <- Sys.Date() - years(años_recientes)

# Filtrar programas creados en los últimos 5 años
programas_recientes <- programas_filtrados %>%
  filter(fecha_de_registro_en_snies >= fecha_limite)

# Calcular el número de nuevos programas por año
nuevos_programas_por_año <- programas_recientes %>%
  mutate(año_registro = year(fecha_de_registro_en_snies)) %>%
  group_by(año_registro) %>%
  summarise(total_nuevos_programas = n()) %>%
  arrange(año_registro)

# Calcular la tasa de crecimiento promedio
crecimiento_promedio <- programas_recientes %>%
  summarise(crecimiento_promedio = mean(tasa_crecimiento, na.rm = TRUE))

# Agrupar y contar programas por departamento y municipio
distribucion_geografica <- programas_recientes %>%
  group_by(departamento_oferta_programa, municipio_oferta_programa) %>%
  summarise(total_programas = n()) %>%
  arrange(desc(total_programas))

# Calcular la frecuencia de aparición de palabras clave en los nombres de los programas recientes
frecuencia_palabras <- programas_recientes %>%
  unnest_tokens(word, nombre_del_programa) %>%
  filter(word %in% palabras_clave) %>%
  count(word, sort = TRUE)

# Identificar palabras clave que tienen pocas ocurrencias pero muestran potencial
nicho_palabras <- frecuencia_palabras %>%
  filter(n <= 5)  # Puedes ajustar el umbral según sea necesario

# Convertir las palabras clave y los nombres de los programas a minúsculas para la comparación
programas_recientes <- programas_recientes %>%
  mutate(nombre_del_programa_lower = str_to_lower(nombre_del_programa))

nicho_palabras_lower <- nicho_palabras %>%
  mutate(word_lower = str_to_lower(word))

# Crear una lista de programas sugeridos basados en popularidad reciente y palabras clave emergentes
sugerencias_programas <- programas_recientes %>%
  filter(str_detect(nombre_del_programa_lower, paste(nicho_palabras_lower$word_lower, collapse = "|"))) %>%
  arrange(desc(crecimiento_promedio))

# Resultado 1. POPULARIDAD BASADA EN TASA DE CRECIMIENTO ------------------------------------------------------------------------------------

# Calcular la popularidad basada en la tasa de crecimiento promedio
popularidad_recientes <- programas_recientes %>%
  group_by(nombre_del_programa) %>%
  summarise(total_programas = n(),
            crecimiento_promedio = mean(tasa_crecimiento, na.rm = TRUE)) %>%
  arrange(desc(crecimiento_promedio))

# Match con Matriculados

# Importar la nueva base de datos
file_path_2 <- "../RAW DATA/SNIES/MATRICULADOS/MATRICULADOS_2022.xlsx"
matriculados_data <- read_excel(file_path_2)

# Renombrar la columna para que coincida
matriculados_data <- matriculados_data %>%
  rename(codigo_snies_del_programa = `CÓDIGO SNIES DEL PROGRAMA`)

# Combinar las dos bases de datos
programas_combinados <- programas_filtrados %>%
  left_join(matriculados_data %>% select(codigo_snies_del_programa, MATRICULADOS), by = "codigo_snies_del_programa")

# Verificar las primeras filas de la base de datos combinada
head(programas_combinados)

# Calcular el ratio de matriculados por antigüedad del programa
programas_combinados <- programas_combinados %>%
  mutate(ratio_matriculados_antiguedad = MATRICULADOS / antiguedad_snies)

# Crear una métrica compuesta que combine tasa de crecimiento y número de matriculados
programas_combinados <- programas_combinados %>%
  mutate(tasa_matriculados = tasa_crecimiento * MATRICULADOS)

# Resultado 1. POPULARIDAD BASADA EN TASA DE CRECIMIENTO ------------------------------------------------------------------------------------

