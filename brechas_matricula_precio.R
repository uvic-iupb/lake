
# Costos // iupb

library(readxl)

# Define la ruta del archivo
file_path <- "/Users/cristianespinal/Downloads/COSTOS POR PROGRAMA IUPB/costos_iupb.xlsx"

# Lee el archivo de Excel
costos_data <- read_excel(file_path)

# Muestra las primeras filas del dataframe
head(costos_data)

# Cargar la librería readxl
library(readxl)

# Leer el archivo Excel
file_path <- "/Users/cristianespinal/Downloads/COSTOS POR PROGRAMA IUPB/costos_iupb.xlsx"
costos_data <- read_excel(file_path)

# Crear el subset con las columnas de costos
costos_subset <- costos_data %>%
  select(
    `NOMBRE OBJETO`,
    `COSTO 2019-I`,
    `COSTO 2019-II`,
    `COSTO 2020-I`,
    `COSTO 2020-II`,
    `COSTO 2021-I`,
    `COSTO 2021-II`,
    `COSTO 2022-I`,
    `COSTO 2022-II`,
    `COSTO 2023-I`,
    `COSTO 2023-II`
  )

# Ver las primeras filas del subset
head(costos_subset)

# SUM por año

# Crear el subset con las columnas de costos
costos_subset <- costos_data %>%
  select(
    `NOMBRE OBJETO`,
    `COSTO 2019-I`,
    `COSTO 2019-II`,
    `COSTO 2020-I`,
    `COSTO 2020-II`,
    `COSTO 2021-I`,
    `COSTO 2021-II`,
    `COSTO 2022-I`,
    `COSTO 2022-II`,
    `COSTO 2023-I`,
    `COSTO 2023-II`
  )

# Sumar los costos por años
costos_subset <- costos_subset %>%
  mutate(
    `COSTO 2019` = `COSTO 2019-I` + `COSTO 2019-II`,
    `COSTO 2020` = `COSTO 2020-I` + `COSTO 2020-II`,
    `COSTO 2021` = `COSTO 2021-I` + `COSTO 2021-II`,
    `COSTO 2022` = `COSTO 2022-I` + `COSTO 2022-II`,
    `COSTO 2023` = `COSTO 2023-I` + `COSTO 2023-II`
  )

# Ver las primeras filas del dataframe con las nuevas columnas
head(costos_subset)

# Cargar las librerías necesarias
library(readxl)
library(dplyr)

# Leer el archivo Excel
file_path <- "/Users/cristianespinal/Downloads/COSTOS POR PROGRAMA IUPB/costos_iupb.xlsx"
costos_data <- read_excel(file_path)

# Crear el subset con las columnas de costos
costos_subset <- costos_data %>%
  select(
    `NOMBRE OBJETO`,
    `COSTO 2019-I`,
    `COSTO 2019-II`,
    `COSTO 2020-I`,
    `COSTO 2020-II`,
    `COSTO 2021-I`,
    `COSTO 2021-II`,
    `COSTO 2022-I`,
    `COSTO 2022-II`,
    `COSTO 2023-I`,
    `COSTO 2023-II`
  )

# Sumar los costos por años
costos_subset <- costos_subset %>%
  mutate(
    `COSTO 2019` = `COSTO 2019-I` + `COSTO 2019-II`,
    `COSTO 2020` = `COSTO 2020-I` + `COSTO 2020-II`,
    `COSTO 2021` = `COSTO 2021-I` + `COSTO 2021-II`,
    `COSTO 2022` = `COSTO 2022-I` + `COSTO 2022-II`,
    `COSTO 2023` = `COSTO 2023-I` + `COSTO 2023-II`
  ) %>%
  select(
    `NOMBRE OBJETO`,
    `COSTO 2019`,
    `COSTO 2020`,
    `COSTO 2021`,
    `COSTO 2022`,
    `COSTO 2023`
  )

# Ver las primeras filas del dataframe con las nuevas columnas
head(costos_subset)

# INGRESOS

# Define la ruta del archivo
file_path_ingresos <- "/Users/cristianespinal/Downloads/COSTOS POR PROGRAMA IUPB/ingresos_iupb.xlsx"

# Lee el archivo de Excel
ingresos_data <- read_excel(file_path_ingresos)

str(ingresos_data)

# Cargar la librería dplyr
library(dplyr)

# Asegúrate de que el dataframe ingresos_data ya esté cargado con tus datos

# Extraer el año de la columna Periodo
ingresos_data <- ingresos_data %>%
  mutate(Año = as.numeric(substr(Periodo, 1, 4)))

# Agrupar por año y programa y sumar las columnas Matrícula, Dctos y Neto
ingresos_resumen <- ingresos_data %>%
  group_by(Año, Programa) %>%
  summarise(
    Total_Matrícula = sum(Matrícula, na.rm = TRUE),
    Total_Dctos = sum(Dctos, na.rm = TRUE),
    Total_Neto = sum(Neto, na.rm = TRUE)
  )

# Ver el resumen de ingresos por año y programa
print(ingresos_resumen)

ingresos_subset <- ingresos_resumen

# MERGE

# Renombrar la columna en costos_subset para que coincida con ingresos_subset
costos_subset <- costos_subset %>%
  rename(Programa = `NOMBRE OBJETO`)

# Convertir costos_subset en formato largo
costos_long <- costos_subset %>%
  pivot_longer(cols = starts_with("COSTO"), 
               names_to = "Año", 
               names_prefix = "COSTO ",
               values_to = "Costo")

# Convertir la columna Año a numérica
costos_long <- costos_long %>%
  mutate(Año = as.numeric(Año))

# Agrupar y sumar los costos por Programa y Año
costos_resumen <- costos_long %>%
  group_by(Programa, Año) %>%
  summarise(Total_Costo = sum(Costo, na.rm = TRUE), .groups = 'drop')

# Convertir ingresos_subset en formato largo
ingresos_long <- ingresos_subset %>%
  pivot_longer(cols = starts_with("Total_"), 
               names_to = "Tipo_Ingreso", 
               values_to = "Valor_Ingreso")

# Agrupar y sumar los ingresos por Programa, Año y Tipo_Ingreso
ingresos_resumen <- ingresos_long %>%
  group_by(Programa, Año, Tipo_Ingreso) %>%
  summarise(Total_Ingreso = sum(Valor_Ingreso, na.rm = TRUE), .groups = 'drop')

# Combinar ambas bases de datos
combined_data <- left_join(ingresos_resumen, costos_resumen, by = c("Programa", "Año"))

# Ver las primeras filas del dataframe combinado
head(combined_data)

# Validación 1: nombres

# Instalar y cargar el paquete stringdist
install.packages("stringdist")
library(stringdist)

# Extraer los nombres de los programas de ambos dataframes
nombres_costos <- unique(costos_subset$Programa)
nombres_ingresos <- unique(ingresos_subset$Programa)

# Crear una matriz de distancias de Levenshtein
dist_matrix <- stringdistmatrix(nombres_costos, nombres_ingresos, method = "lv")

# Convertir la matriz en un dataframe para facilitar el análisis
dist_df <- as.data.frame(as.table(dist_matrix))
names(dist_df) <- c("Programa_Costos", "Programa_Ingresos", "Distancia")

# Filtrar los pares con una distancia de Levenshtein baja (ajusta el umbral según sea necesario)
umbral <- 5
pares_similares <- dist_df %>% filter(Distancia <= umbral)

# Ver los pares de nombres similares
print(pares_similares)

# Cargar las librerías necesarias
library(readxl)
library(dplyr)

# Leer la hoja que contiene los datos combinados
combined_data <- read_excel(file_path, sheet = "combined_data")

# Obtener el listado distintivo de "Programas"
distinct_programas <- combined_data %>%
  select(Programa) %>%
  distinct()

# Ver el listado distintivo de "Programas"
print(distinct_programas)

# ESTUDIANTES

# Define la ruta del archivo
file_path_estudiantes <- "/Users/cristianespinal/Downloads/COSTOS POR PROGRAMA IUPB/estudiantes_iupb.xlsx"

# Lee el archivo de Excel
estudiantes_data <- read_excel(file_path_estudiantes)

#############

# Unir ingresos y estudiantes por año y programa
ingresos_estudiantes <- ingresos_subset %>%
  left_join(estudiantes_resumen, by = c("Año", "Programa"))

# Transformar costos a formato largo
costos_largos <- costos_subset %>%
  pivot_longer(cols = starts_with("COSTO"), names_to = "Año", values_to = "Costo") %>%
  mutate(Año = as.numeric(gsub("COSTO ", "", Año)))

# Unir todos los datos resultantes en un solo DataFrame
datos_combinados <- ingresos_estudiantes %>%
  left_join(costos_largos, by = c("Año", "Programa"))

# 

# Añadir columnas de Precio por Estudiante y Costo por Estudiante
datos_combinados <- datos_combinados %>%
  mutate(
    Precio_por_Estudiante = ifelse(Total_Estudiantes != 0, Total_Neto / Total_Estudiantes, NA),
    Costo_por_Estudiante = ifelse(Total_Estudiantes != 0, Costo / Total_Estudiantes, NA)
  )

# Mostrar el resultado
print(datos_combinados)

datos_combinados <- datos_combinados %>%
  mutate(
    Brecha_por_Estudiante = Precio_por_Estudiante - Costo_por_Estudiante
  )

brecha_precio_costo_por_estudiante <- datos_combinados

guardar_datos(brecha_precio_costo_por_estudiante, path_base, "brecha_precio_costo_por_estudiante")

################################################################################################

# Librerías necesarias
library(dplyr)
library(tidyr)

# Reemplazar nombres similares en la columna Programa en cada dataset
estudiantes_resumen <- estudiantes_resumen %>%
  mutate(Programa = ifelse(Programa %in% c("TECNOLOGÍA MECÁNICA INDUSTRIAL", "TECNOLOGÍA EN MECÁNICA INDUSTRIAL"), 
                           "TECNOLOGÍA MECÁNICA INDUSTRIAL", Programa))

ingresos_data <- ingresos_data %>%
  mutate(Programa = ifelse(Programa %in% c("TECNOLOGÍA MECÁNICA INDUSTRIAL", "TECNOLOGÍA EN MECÁNICA INDUSTRIAL"), 
                           "TECNOLOGÍA MECÁNICA INDUSTRIAL", Programa))

costos_long <- costos_long %>%
  mutate(Programa = ifelse(Programa %in% c("TECNOLOGÍA MECÁNICA INDUSTRIAL", "TECNOLOGÍA EN MECÁNICA INDUSTRIAL"), 
                           "TECNOLOGÍA MECÁNICA INDUSTRIAL", Programa))

# Agrupar los datos por Programa y Año, y sumar Total_Estudiantes en estudiantes_resumen
estudiantes_resumen <- estudiantes_resumen %>%
  group_by(Año, Programa) %>%
  summarise(Total_Estudiantes = sum(Total_Estudiantes, na.rm = TRUE)) %>%
  ungroup()

# Unir ingresos y estudiantes por año y programa
ingresos_estudiantes <- ingresos_data %>%
  left_join(estudiantes_resumen, by = c("Año", "Programa"))

# Unir todos los datos resultantes en un solo DataFrame
datos_combinados <- ingresos_estudiantes %>%
  left_join(costos_long, by = c("Año", "Programa"))

# Añadir columnas de Precio por Estudiante, Costo por Estudiante y Brecha por Estudiante
datos_combinados <- datos_combinados %>%
  mutate(
    Precio_por_Estudiante = ifelse(Total_Estudiantes != 0, Neto / Total_Estudiantes, NA),
    Costo_por_Estudiante = ifelse(Total_Estudiantes != 0, Costo / Total_Estudiantes, NA),
    Brecha_por_Estudiante = Precio_por_Estudiante - Costo_por_Estudiante
  )

# Mostrar el resultado
print(datos_combinados)

# AGRUPACIÓN-SOLO COSTO/INGRESOS

# Filtrar las columnas que empiezan con 'COSTO PROMEDIO POR ESTUDIANTE' junto con 'NOMBRE OBJETO'
filtered_columns <- grep("^COSTO PROMEDIO POR ESTUDIANTE|NOMBRE OBJETO", colnames(costos_data), value = TRUE)
filtered_data <- costos_data[, filtered_columns]

# Cargar la librería tidyr
library(tidyr)

# Supongamos que tu dataframe filtrado se llama filtered_data

# Convertir el dataframe a formato largo
long_data <- pivot_longer(filtered_data, 
                          cols = starts_with("COSTO PROMEDIO POR ESTUDIANTE"), 
                          names_to = "PERIODO", 
                          values_to = "COSTO PROMEDIO POR ESTUDIANTE")

# Extraer el periodo del nombre de la columna y limpiar el nombre de la columna PERIODO
long_data$PERIODO <- sub("COSTO PROMEDIO POR ESTUDIANTE ", "", long_data$PERIODO)

# Reordenar las columnas
final_data <- long_data[, c("NOMBRE OBJETO", "COSTO PROMEDIO POR ESTUDIANTE", "PERIODO")]

# Mostrar el dataframe final
print(final_data)

#######

# Cargar la librería dplyr para manipulación de datos
library(dplyr)

# Crear la columna "año" extrayendo el año del "PERIODO"
long_data <- long_data %>%
  mutate(año = sub("-.*", "", PERIODO))

# Calcular el costo promedio anual por estudiante para cada "NOMBRE OBJETO" y "año"
average_annual_data <- long_data %>%
  group_by(`NOMBRE OBJETO`, año) %>%
  summarise(`COSTO PROMEDIO POR ESTUDIANTE (ANUAL)` = mean(`COSTO PROMEDIO POR ESTUDIANTE`, na.rm = TRUE)) %>%
  ungroup()

# Unir los datos anuales con los datos originales
final_data <- long_data %>%
  left_join(average_annual_data, by = c("NOMBRE OBJETO", "año"))

# Mostrar el dataframe final
print(final_data)

# Cargar la librería tidyr
library(tidyr)

# Convertir el dataframe a formato largo
long_estudiantes_data <- pivot_longer(estudiantes_data, 
                                      cols = starts_with("No. ESTUDIANTES"), 
                                      names_to = "Periodo", 
                                      values_to = "No. de Estudiantes")

# Limpiar la columna "Periodo" para extraer el periodo correcto
long_estudiantes_data$Periodo <- sub("No. ESTUDIANTES ", "", long_estudiantes_data$Periodo)

# Renombrar las columnas para tener consistencia
colnames(long_estudiantes_data)[1] <- "Programa"

# Mostrar el dataframe final
print(long_estudiantes_data)

# Cargar la librería dplyr para manipulación de datos
library(dplyr)

# Crear la columna "año" extrayendo el año del "Periodo"
long_estudiantes_data <- long_estudiantes_data %>%
  mutate(año = sub("-.*", "", Periodo))

# Calcular el número total de estudiantes por programa y año
total_estudiantes_anual <- long_estudiantes_data %>%
  group_by(Programa, año) %>%
  summarise(`Total No. de Estudiantes (Anual)` = sum(`No. de Estudiantes`, na.rm = TRUE)) %>%
  ungroup()

# Unir los datos anuales con los datos originales
final_estudiantes_data <- long_estudiantes_data %>%
  left_join(total_estudiantes_anual, by = c("Programa", "año"))

# Mostrar el dataframe final
print(final_estudiantes_data)

# Cargar la librería dplyr para manipulación de datos
library(dplyr)

# Transformar la variable Periodo
ingresos_data <- ingresos_data %>%
  mutate(Periodo = ifelse(substr(Periodo, 5, 5) == "1", 
                          paste0(substr(Periodo, 1, 4), "-I"), 
                          paste0(substr(Periodo, 1, 4), "-II")))

# Mostrar el dataframe transformado
print(ingresos_data)

# Cargar la librería dplyr para manipulación de datos
library(dplyr)

# Calcular las sumas anuales
annual_sums <- ingresos_data %>%
  group_by(Programa, Año) %>%
  summarise(`Matrícula (año)` = sum(Matrícula, na.rm = TRUE),
            `Dctos (año)` = sum(Dctos, na.rm = TRUE),
            `Neto (año)` = sum(Neto, na.rm = TRUE)) %>%
  ungroup()

# Unir los datos anuales con los datos originales
final_ingresos_data <- ingresos_data %>%
  left_join(annual_sums, by = c("Programa", "Año"))

# Mostrar el dataframe final
print(final_ingresos_data)

#########

# Cargar la librería dplyr para manipulación de datos
library(dplyr)

# Calcular las sumas anuales (ya hecho previamente)
annual_sums <- ingresos_data %>%
  group_by(Programa, Año) %>%
  summarise(`Matrícula (año)` = sum(Matrícula, na.rm = TRUE),
            `Dctos (año)` = sum(Dctos, na.rm = TRUE),
            `Neto (año)` = sum(Neto, na.rm = TRUE)) %>%
  ungroup()

# Unir los datos anuales con los datos originales
final_ingresos_data <- ingresos_data %>%
  left_join(annual_sums, by = c("Programa", "Año"))

# Crear un subset con las columnas anuales, año y programa
annual_subset <- final_ingresos_data %>%
  select(Programa, Año, `Matrícula (año)`, `Dctos (año)`, `Neto (año)`) %>%
  distinct()

# Eliminar las columnas anuales del dataframe original
final_ingresos_data <- final_ingresos_data %>%
  select(-`Matrícula (año)`, -`Dctos (año)`, -`Neto (año)`)

# Mostrar el subset de datos anual
print(annual_subset)

# Mostrar el dataframe original sin las columnas anuales
print(final_ingresos_data)

# Agrupación:

# Cargar las librerías necesarias
library(dplyr)

# Renombrar las columnas para hacer la unión más fácil
final_ingresos_data <- final_ingresos_data %>%
  rename(NOMBRE_OBJETO = Programa, PERIODO = Periodo)

final_estudiantes_data <- final_estudiantes_data %>%
  rename(NOMBRE_OBJETO = Programa, PERIODO = Periodo)

# Unir las bases de datos
brechas_precio_costo <- final_ingresos_data %>%
  left_join(final_estudiantes_data, by = c("NOMBRE_OBJETO" = "NOMBRE_OBJETO", "PERIODO" = "PERIODO")) %>%
  left_join(final_data, by = c("NOMBRE_OBJETO" = "NOMBRE OBJETO", "PERIODO" = "PERIODO"))

# Seleccionar y reordenar las columnas deseadas
brechas_precio_costo <- brechas_precio_costo %>%
  select(NOMBRE_OBJETO, PERIODO, Matrícula, Dctos, Neto, `No. de Estudiantes`, `COSTO PROMEDIO POR ESTUDIANTE`)

# Mostrar el dataframe final
print(brechas_precio_costo)

# Cargar las librerías necesarias
library(dplyr)

# Renombrar las columnas para hacer la unión más fácil
final_ingresos_data <- final_ingresos_data %>%
  rename(NOMBRE_OBJETO = Programa, PERIODO = Periodo)

final_estudiantes_data <- final_estudiantes_data %>%
  rename(NOMBRE_OBJETO = Programa, PERIODO = Periodo)

# Unir las bases de datos
brechas_precio_costo <- final_ingresos_data %>%
  left_join(final_estudiantes_data, by = c("NOMBRE_OBJETO" = "NOMBRE_OBJETO", "PERIODO" = "PERIODO", "Año" = "año")) %>%
  left_join(final_data, by = c("NOMBRE_OBJETO" = "NOMBRE OBJETO", "PERIODO" = "PERIODO", "Año" = "año"))

# Seleccionar y reordenar las columnas deseadas
brechas_precio_costo <- brechas_precio_costo %>%
  select(NOMBRE_OBJETO, PERIODO, Matrícula, Dctos, Neto, `No. de Estudiantes`, final_estudiantes_data$`Total No. de Estudiantes (Anual)`, `COSTO PROMEDIO POR ESTUDIANTE`)

# Mostrar el dataframe final
print(brechas_precio_costo)

# Resta

# Verificar valores faltantes en las columnas relevantes y eliminarlos si es necesario
brechas_precio_costo <- brechas_precio_costo %>%
  filter(!is.na(`No. de Estudiantes`) & !is.na(`COSTO PROMEDIO POR ESTUDIANTE`) & !is.na(Neto))

# Calcular la variable precio_por_estudiante
brechas_precio_costo <- brechas_precio_costo %>%
  mutate(precio_por_estudiante = Neto / `No. de Estudiantes`)

# Calcular la variable brecha
brechas_precio_costo <- brechas_precio_costo %>%
  mutate(brecha = precio_por_estudiante - `COSTO PROMEDIO POR ESTUDIANTE`)

# Mostrar la base de datos final con las nuevas variables
print(brechas_precio_costo)

# Calcular las variables precio_por_estudiante, brecha, dctos_por_estudiante y razon_dctos_por_estudiante
brechas_precio_costo <- brechas_precio_costo %>%
  mutate(precio_por_estudiante = Neto / `No. de Estudiantes`,
         brecha = precio_por_estudiante - `COSTO PROMEDIO POR ESTUDIANTE`,
         dctos_por_estudiante = Dctos / `No. de Estudiantes`,
         razon_dctos_por_estudiante = dctos_por_estudiante / precio_por_estudiante)

# Mostrar la base de datos final con las nuevas variables
print(brechas_precio_costo)

# Niveles

library(readxl)

# Define la ruta del archivo
niveles_path <- "/Users/cristianespinal/Downloads/nivel_programas.xlsx"

# Lee el archivo de Excel
niveles_data <- read_excel(niveles_path)

# Renombrar la columna para hacer la unión más fácil
niveles_data <- niveles_data %>%
  rename(NOMBRE_OBJETO = NOMBRE_OBJETO)

guardar_datos(brechas_precio_costo, path_base, "brechas_precio_costo")

# Unir la base de datos brechas_precio_costo con niveles_data para obtener los niveles correctos
brechas_precio_costo <- brechas_precio_costo %>%
  left_join(niveles_data, by = "NOMBRE_OBJETO")

# Mostrar la base de datos final con la nueva variable nivel
print(brechas_precio_costo)

# brechas_subset_general

# Cargar las librerías necesarias
library(dplyr)

# Crear el subset brecha_subset_general
brecha_subset_general <- brechas_precio_costo %>%
  group_by(PERIODO) %>%
  summarise(across(c(Matrícula, Dctos, Neto, `No. de Estudiantes`, 
                     `COSTO PROMEDIO POR ESTUDIANTE`, precio_por_estudiante, brecha), 
                   mean, na.rm = TRUE))

# Mostrar el subset general con los promedios por periodo
print(brecha_subset_general)

guardar_datos(brecha_subset_general, path_base, "brecha_subset_general")

# Crear el subset brecha_subset_niveles con los promedios por periodo y nivel
brecha_subset_niveles <- brechas_precio_costo %>%
  group_by(PERIODO, nivel) %>%
  summarise(across(c(Matrícula, Dctos, Neto, `No. de Estudiantes`, 
                     `COSTO PROMEDIO POR ESTUDIANTE`, precio_por_estudiante, brecha, dctos_por_estudiante, razon_dctos_por_estudiante), 
                   ~mean(.)))

# Mostrar el subset por niveles con los promedios por periodo y nivel
print(brecha_subset_niveles)

guardar_datos(brecha_subset_niveles, path_base, "brecha_subset_niveles")

# Tasa de crecimiento

library(dplyr)

# Crear el subset brechas_tasas
brechas_tasas <- brechas_precio_costo %>%
  group_by(NOMBRE_OBJETO) %>%
  arrange(PERIODO) %>%
  mutate(
    tasa_crecimiento_brecha = (brecha - lag(brecha)) / lag(brecha) * 100
  ) %>%
  filter(!is.na(tasa_crecimiento_brecha))

library(dplyr)
library(zoo)

brechas_tasas <- brechas_precio_costo %>%
  group_by(NOMBRE_OBJETO) %>%
  arrange(PERIODO) %>%
  mutate(
    # Calcular el promedio móvil de las brechas de los últimos dos periodos
    promedio_2 = rollapply(brecha, width = 2, FUN = mean, align = "right", fill = NA, na.rm = TRUE),
    # Calcular el promedio móvil de las brechas de los últimos cuatro periodos
    promedio_4 = rollapply(brecha, width = 4, FUN = mean, align = "right", fill = NA, na.rm = TRUE),
    # Calcular la tasa de crecimiento normalizada
    tasa_normalizada = (promedio_2 - promedio_4) / (promedio_2 + promedio_4)
  ) %>%
  filter(!is.na(tasa_normalizada))

# Mostrar el resultado
print(brechas_tasas)

guardar_datos(brechas_tasas, path_base, "brechas_tasas")



