
# IAMDBN - Evaluación de Impacto

# Inscritos
# Admitidos
# Matriculados
# Desetores
# Bienestar
# Notas

# 1. IAMD ------------
# Con desertores

# Función para leer un archivo Excel y convertir todas las columnas a texto
convertir_a_texto <- function(df) {
  df %>% 
    mutate(across(everything(), as.character))  # Convierte todas las columnas a texto
}

# Modifica la función de lectura para que use convertir_a_texto
lectura_base <- function(path_a) {
  temp <- read_excel(path = path_a, col_types = "text") %>%  # Leer todas las columnas como texto
    clean_names() %>%
    convertir_a_texto()  # Asegura que todas las columnas sean de tipo texto
  return(temp)
}

matricula_totales <- convertir_a_texto(matricula_totales)
matricula_nuevos <- convertir_a_texto(matricula_nuevos)
admitidos <- convertir_a_texto(admitidos)
aspirantes <- convertir_a_texto(aspirantes)
desertores <- convertir_a_texto(desertores)

# Rutas de los archivos
path_base <- "../DATA CONSOLIDADA/"

# Asignar Facultad

# Función para asignar la facultad basada en el programa
asignar_facultad <- function(programa) {
  case_when(
    programa %in% c("ESPECIALIZACIÓN EN BIG DATA", "INGENIERÍA DE MATERIALES", "INGENIERÍA DE SOFTWARE", 
                    "INGENIERÍA ELÉCTRICA", "INGENIERÍA MECÁNICA", "MAESTRÍA EN CIENCIAS COMPUTACIONALES - INVESTIGACION",
                    "MAESTRÍA EN CIENCIAS COMPUTACIONALES - PROFUNDIZACION", "MAESTRÍA EN ENERGÍA",
                    "TÉCNICA PROFESIONAL EN REDES ELÉCTRICAS DE DISTRIBUCIÓN DE ENERGÍA", "TECNOLOGÍA ELÉCTRICA",
                    "TECNOLOGÍA ELECTRÓNICA", "TECNOLOGÍA EN DESARROLLO DE SOFTWARE", "TECNOLOGÍA EN ELECTROMECÁNICA",
                    "TECNOLOGÍA EN GESTIÓN DE MANTENIMIENTO ELECTROMECÁNICO", "TECNOLOGÍA EN GESTIÓN DEL MANTENIMIENTO AERONÁUTICO",
                    "TECNOLOGÍA EN INFORMÁTICA", "TECNOLOGÍA EN MANTENIMIENTO DE AERONAVES", "TECNOLOGÍA EN MECÁNICA",
                    "TECNOLOGÍA EN MECÁNICA AUTOMOTRIZ", "TECNOLOGÍA EN MECATRÓNICA", "TECNOLOGÍA EN SISTEMAS ELECTROMECÁNICOS",
                    "TECNOLOGÍA EN SISTEMAS MECATRÓNICOS", "TECNOLOGÍA EN SUPERVISIÓN DE SISTEMAS DE GENERACIÓN Y DISTRIBUCIÓN DE ENERGÍA ELÉCTRICA",
                    "TECNOLOGÍA EN SUPERVISIÓN DE SISTEMAS ELÉCTRICOS DE POTENCIA", "TECNOLOGÍA MECÁNICA INDUSTRIAL") ~ "Facultad de Ingeniería",
    programa %in% c("ESPECIALIZACIÓN EN GESTIÓN DE PROYECTOS", "INGENIERÍA ADMINISTRATIVA", "INGENIERÍA EN LOGÍSTICA",
                    "INGENIERÍA INDUSTRIAL", "MAESTRÍA EN DISEÑO Y EVALUACIÓN DE PROYECTOS  REGIONALES", "MAESTRÍA EN GERENCIA DE LA TRANSFORMACIÓN DIGITAL PROFUNDIZACIÓN",
                    "PROFESIONAL EN DISEÑO DE VESTUARIO", "PROFESIONAL EN DISEÑO GRÁFICO", "PROFESIONAL EN GESTIÓN DEL DISEÑO",
                    "TECNOLOGÍA EN ANIMACIÓN DIGITAL", "TECNOLOGÍA EN DISEÑO GRÁFICO", "TECNOLOGÍA EN DISEÑO TEXTIL Y DE MODAS",
                    "TECNOLOGÍA EN DISEÑO TEXTIL Y PRODUCCIÓN DE MODAS", "TECNOLOGÍA EN DISEÑO Y GESTIÓN DE LA IMÁGEN",
                    "TECNOLOGÍA EN ELECTRÓNICA INDUSTRIAL", "TECNOLOGÍA EN GESTIÓN DEL DISEÑO GRÁFICO", "TECNOLOGÍA EN GESTIÓN DEL DISEÑO TEXTIL Y DE MODAS",
                    "TECNOLOGÍA EN GESTIÓN LOGÍSTICA", "TECNOLOGÍA EN OPERACIÓN INTEGRAL DEL TRANSPORTE",
                    "TECNOLOGÍA EN PRODUCCIÓN INDUSTRIAL") ~ "Facultad de Producción y Diseño",
    TRUE ~ "Otra Facultad"  # Para programas no listados
  )
}

asignar_grupo_pago <- function(forma_de_pago) {
  case_when(
    grepl("COOPERATIVAS|FUNDACIONES|EMPRESAS", forma_de_pago) ~ "COOPERATIVAS, FUNDACIONES Y EMPRESAS",
    grepl("FRACCIONAMIENTO DE MATRÍCULA", forma_de_pago) ~ "FRACCIONAMIENTO DE MATRÍCULA",
    grepl("AUXILIOS PÚBLICOS|ICETEX|GENERACION E|SAPIENCIA|PRESUPUESTO PARTICIPATIVO", forma_de_pago) ~ "AUXILIOS PÚBLICOS",
    grepl("RECURSOS PROPIOS", forma_de_pago) ~ "RECURSOS PROPIOS",
    TRUE ~ "Otros"
  )
}

# Función para cargar datos desde una ruta específica

cargar_datos <- function(path) {
  files <- list.files(path = path, recursive = TRUE, full.names = TRUE)
  data <- map(files, ~ lectura_base(.x, encoding = "UTF-8"))  # Agrega encoding = "UTF-8"
  data_binded <- bind_rows(data, .id = "id") %>%
    mutate(file = basename(files))  # Añade el nombre del archivo base a los datos
  return(data_binded)
}

# Combinar todos los datos en un solo dataframe IAM

IAMD <- bind_rows(matricula_totales, matricula_nuevos, admitidos, aspirantes, desertores) %>%
  mutate(
    fecha_iam = calcular_fecha(as.character(periodo)),
    semestre = as.numeric(substr(periodo, 5, 5)),
    file_grupos = gsub("_", " ", tools::file_path_sans_ext(file)),
    file_grupos = substr(file_grupos, 1, nchar(file_grupos) - 5),
    Facultad = asignar_facultad(programa),
    forma_de_pago_agrupada = asignar_grupo_pago(forma_de_pago),
    EdadConDefault = if_else(is.na(edad) | edad == "", 0, as.numeric(edad))
  ) %>%
  select(-discapacidades)

# Convertir las cadenas de texto a UTF-8 con manejo de bytes desconocidos y validación
IAMD[] <- lapply(IAMD, function(x) {
  if (is.character(x)) {
    stri_enc_toutf8(x, is_unknown_8bit = TRUE, validate = TRUE)
  } else {
    x
  }
})

# Crear la variable dummy 'dummy_desercion'
IAMD <- IAMD %>%
  mutate(dummy_desercion = ifelse(identificacion %in% desertores$identificacion, 1, 0))

# Generar un resumen estadístico por 'file' y 'dummy_desercion'
resumen <- IAMD %>%
  group_by(file, dummy_desercion) %>%
  summarise(count = n(),
            mean_age = mean(as.numeric(edad), na.rm = TRUE),
            median_age = median(as.numeric(edad), na.rm = TRUE),
            sd_age = sd(as.numeric(edad), na.rm = TRUE)) %>%
  ungroup()

IAMD <- IAMD %>%
  select(file, ruta, dummy_desercion, everything())

verificacion <- IAMD %>%
  filter(identificacion %in% desertores$identificacion & dummy_desercion != 1)

# Verificar si hay algún registro que no cumple con la condición
if(nrow(verificacion) == 0) {
  cat("Todos los desertores tienen dummy_desercion igual a 1.\n")
} else {
  cat("Hay registros de desertores donde dummy_desercion no es igual a 1. Verificar los datos.\n")
  print(verificacion)
}

# Mostrar el resumen
print(resumen)

guardar_datos(IAMD, path_base, "IAMD")

# Carga de datos

# Cargar datos para cada categoría
matriculados_totales <- cargar_datos("../RAW DATA/Matriculados_totales")
matriculados_nuevos <- cargar_datos("../RAW DATA/Matriculados_nuevos")
admitidos <- cargar_datos("../RAW DATA/Admitidos")
aspirantes <- cargar_datos("../RAW DATA/Aspirantes")

# Leer los datos
matricula_totales <- read_excel(paste0(path_base, "matricula_totales.xlsx"))
matricula_nuevos <- read_excel(paste0(path_base, "matricula_nuevos.xlsx"))
admitidos <- read_excel(paste0(path_base, "admitidos.xlsx"))
aspirantes <- read_excel(paste0(path_base, "Aspirantes.xlsx"))

# 2. IAMDB ------------
# Con bienestar

# Cargar las bibliotecas necesarias
library(dplyr)
library(readxl)
library(purrr)
library(stringr)

# Definir la ruta de las bases de datos
ruta <- "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/Bienestar"
ruta_iamd <- "ruta_a_tu_archivo/IAMD.xlsx"
ruta_desertores <- "ruta_a_tu_archivo/desertores.xlsx"

# Listar todos los archivos en la ruta especificada
archivos <- list.files(path = ruta, pattern = "*.xlsx", full.names = TRUE)

# Función para leer cada archivo, agregar columnas con el nombre y la ruta del archivo,
# arreglar los nombres de las columnas y modificar la columna 'Periodo'
leer_datos <- function(archivo) {
  datos <- read_excel(archivo)
  datos <- datos %>%
    mutate(file = basename(archivo), ruta = archivo) %>%
    rename_all(~ tolower(make.names(.))) %>%  # Arreglar los nombres de las columnas y convertir a minúscula
    mutate(periodo = str_replace_all(periodo, "_", ""))  # Modificar la columna 'Periodo'
  return(datos)
}

# Leer todos los archivos y combinarlos en un solo dataframe
data_combined <- archivos %>%
  map_dfr(leer_datos)

# Ver la estructura del dataframe combinado
glimpse(data_combined)

bienestar <- data_combined

# Leer las bases de datos IAMD y desertores
IAMD <- read_excel(ruta_iamd)
desertores <- read_excel(ruta_desertores)

IAMD <- IAMD %>% 
  rename(periodo = periodo.x)

# Crear la dummy 'dummy_desercion'
IAMD <- IAMD %>%
  mutate(dummy_desercion = ifelse(identificacion %in% desertores$identificacion, 1, 0))

# Crear la dummy 'dummy_bienestar' inicializada a 0
IAMD <- IAMD %>%
  mutate(dummy_bienestar = 0)

# Crear las dummies basadas en la variable 'area' de la base de datos 'bienestar'
areas_bienestar <- unique(bienestar$area)

# Inicializar las nuevas dummies en la base de datos 'IAMD' con 0
for(area in areas_bienestar) {
  dummy_name <- paste0("dummy_bienestar_", str_replace_all(tolower(area), " ", "_"))
  IAMD[[dummy_name]] <- 0
}

# Convertir la columna periodo de IAMD a numérica para la comparación
IAMD <- IAMD %>%
  mutate(periodo = as.numeric(str_replace_all(periodo, "_", "")))

# Actualizar las dummies a 1 para los estudiantes que cursaron algún programa de bienestar
for(area in areas_bienestar) {
  dummy_name <- paste0("dummy_bienestar_", str_replace_all(tolower(area), " ", "_"))
  estudiantes_area <- bienestar %>%
    filter(area == !!area) %>%
    select(identificacion, periodo) %>%
    distinct()
  
  estudiantes_area <- estudiantes_area %>%
    mutate(periodo = as.numeric(periodo))
  
  IAMD <- IAMD %>%
    left_join(estudiantes_area, by = "identificacion", suffix = c("", ".y"), relationship = "many-to-many") %>%
    mutate(
      dummy_bienestar = ifelse(!is.na(periodo.y) & periodo <= periodo.y, 1, dummy_bienestar),
      !!dummy_name := ifelse(!is.na(periodo.y) & periodo <= periodo.y, 1, get(dummy_name))
    ) %>%
    select(-periodo.y)
}

IAMD <- IAMD %>%
  select(file, ruta, dummy_desercion, dummy_bienestar, dummy_bienestar_cultura, dummy_bienestar_desarrollo_humano,
         dummy_bienestar_deportes_y_recreación, dummy_bienestar_socioeconómica, everything()) %>%
  select(-dummy_desersion)

# Ver la estructura de la nueva base de datos 'IAMDB'
glimpse(IAMD)

IAMDB <- IAMD

# Participación en Bienestar

IAMD <- IAMD %>%
  left_join(bienestar %>%
              group_by(identificacion) %>%
              summarise(conteo_general = n()), by = "identificacion")

# Crear columnas de conteo por área
for(area in areas_bienestar) {
  conteo_name <- paste0("conteo_", str_replace_all(tolower(area), " ", "_"))
  conteo_area <- bienestar %>%
    filter(area == !!area) %>%
    group_by(identificacion) %>%
    summarise(!!conteo_name := n())
  
  IAMD <- IAMD %>%
    left_join(conteo_area, by = "identificacion") %>%
    mutate(!!conteo_name := coalesce(!!sym(conteo_name), 0))
}

# Asegurar que la columna de conteo general tenga valores cero en lugar de NA
IAMD <- IAMD %>%
  mutate(conteo_general = coalesce(conteo_general, 0))

IAMD <- IAMD %>%
  select(file, ruta, dummy_desercion, dummy_bienestar, dummy_bienestar_cultura, dummy_bienestar_desarrollo_humano,
         dummy_bienestar_deportes_y_recreación, dummy_bienestar_socioeconómica, 
         conteo_general, conteo_cultura, conteo_desarrollo_humano, conteo_deportes_y_recreación, 
         conteo_socioeconómica, everything())

# Ver la estructura de la nueva base de datos 'IAMDB' con los conteos agregados
glimpse(IAMD)

# 3. IAMDBN ------------
# Con notas

# Convertir la columna nota_parcial_acumulada a numérico en listado_notas
listado_notas <- listado_notas %>%
  mutate(nota_parcial_acumulada = as.numeric(nota_parcial_acumulada))

# Calcular el promedio de nota_parcial_acumulada por identificación
promedio_notas <- listado_notas %>%
  group_by(identificacion) %>%
  summarise(promedio_nota = mean(nota_parcial_acumulada, na.rm = TRUE))

# Realizar un left_join con IAMD para agregar la variable promedio_nota
IAMD <- IAMD %>%
  left_join(promedio_notas, by = "identificacion")

# Ver la estructura de la base de datos IAMD con el promedio de notas agregado
glimpse(IAMD)

IAMD <- IAMD %>%
  select(file, ruta, dummy_desercion, dummy_bienestar, dummy_bienestar_cultura, dummy_bienestar_desarrollo_humano,
         dummy_bienestar_deportes_y_recreación, dummy_bienestar_socioeconómica, 
         conteo_general, conteo_cultura, conteo_desarrollo_humano, conteo_deportes_y_recreación, 
         conteo_socioeconómica, promedio_nota, everything())

IAMDBN <- IAMD

guardar_datos(IAMDBN, path_base, "IAMDBN")

# Temporalidad --> dummy_desercion

# Convertir las columnas necesarias a formato numérico en ambas bases de datos
IAMDBN <- IAMDBN %>%
  mutate(across(c(edad, promedio_nota, conteo_general, conteo_cultura, conteo_desarrollo_humano, conteo_deportes_y_recreación, conteo_socioeconómica), as.numeric))

desertores <- desertores %>%
  mutate(across(c(edad), as.numeric))

# Crear columnas de periodos de deserción y matriculación
desertores <- desertores %>%
  mutate(periodo_desercion = as.numeric(periodo))

IAMDBN <- IAMDBN %>%
  mutate(periodo_matricula = as.numeric(periodo))

# Crear una lista de identificaciones únicas de estudiantes
identificaciones <- unique(IAMDBN$identificacion)

# Inicializar dummy_desercion a 0
IAMDBN <- IAMDBN %>%
  mutate(dummy_desercion = 0)

# Actualizar la variable dummy_desercion considerando los periodos de deserción y matriculación
for (id in identificaciones) {
  # Obtener los periodos de deserción para el estudiante
  periodos_desercion <- desertores %>%
    filter(identificacion == id) %>%
    pull(periodo_desercion)
  
  # Obtener los periodos de matriculación para el estudiante
  periodos_matricula <- IAMDBN %>%
    filter(identificacion == id) %>%
    pull(periodo_matricula)
  
  # Actualizar la variable dummy_desercion para cada registro
  IAMDBN <- IAMDBN %>%
    mutate(dummy_desercion = ifelse(
      identificacion == id & periodo_matricula %in% periodos_desercion & 
        (is.na(lead(periodo_matricula)) | periodo_matricula < lead(periodo_matricula)), 1, dummy_desercion))
}

# Mostrar la estructura actualizada de IAMDBN
glimpse(IAMDBN)

guardar_datos(IAMDBN, path_base, "IAMDBN")
