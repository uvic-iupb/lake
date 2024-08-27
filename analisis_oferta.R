
# Analisis de oferta -- factibilidad

library(readxl)
library(dplyr)
library(janitor)

# Cargar BD programas SNIES
file_path_1 <- "/Users/cristianespinal/Downloads/Programas-2.xlsx"
programas_data <- read_excel(file_path_1)
head(programas_data)
programas_data <- clean_names(programas_data)
head(programas_data)

# Cargar BD programas Matriculados Primer Curso
file_path_2 <- "/Users/cristianespinal/Downloads/PRIMER_CURSO_2022.xlsx"
matriculas_data <- read_excel(file_path_2)
head(matriculas_data)
matriculas_data <- clean_names(matriculas_data)
head(matriculas_data)

# Agrupar por código SNIES
programas_y_matriculas_SNIES <- programas_data_diseño %>%
  left_join(matriculas_data_diseño, by = "codigo_snies_del_programa")

names(programas_y_matriculas_SNIES)

# Filtrar por nivel_academico y departamento_de_domicilio_de_la_ies
matriculas_data_diseño <- matriculas_data %>%
  filter(nivel_academico == "POSGRADO",
         departamento_de_domicilio_de_la_ies == "Antioquia",
         grepl("Diseño", programa_academico, ignore.case = TRUE))

programas_data_diseño <- programas_data %>%
  filter(nivel_academico == "Posgrado",
         departamento_oferta_programa == "Antioquia",
         grepl("Diseño", nombre_del_programa, ignore.case = TRUE))

# N-distinct

# Cargar el paquete dplyr
library(dplyr)

distinct_counts_grouped <- programas_data_diseño %>%
  group_by(cine_f_2013_ac_campo_amplio, area_de_conocimiento, nucleo_basico_del_conocimiento) %>%
  reframe(sum= n())
  reframe(
    distinct_campo_especifico = n_distinct(cine_f_2013_ac_campo_especific),
    distinct_campo_detallado = n_distinct(cine_f_2013_ac_campo_detallado),
    distinct_area_conocimiento = n_distinct(area_de_conocimiento),
    distinct_nucleo_basico_conocimiento = n_distinct(nucleo_basico_del_conocimiento)
  )

# Mostrar el resultado
print(distinct_counts_grouped)

# Mostrar el resultado
print(distinct_counts)


# Mostrar el resultado
print(distinct_counts)


programas_y_matriculas_seleccion <- programas_y_matriculas_SNIES %>%
  select(nombre_institucion, sector, codigo_snies_del_programa, nombre_del_programa, 
         cine_f_2013_ac_campo_amplio, cine_f_2013_ac_campo_especific, 
         cine_f_2013_ac_campo_detallado, area_de_conocimiento.x, 
         nucleo_basico_del_conocimiento, nivel_de_formacion.x, 
         costo_matricula_estud_nuevos, primer_curso, ies_acreditada, reconocimiento_del_ministerio)

# Paso 1: Calcular la participación de primer_curso por nombre_institucion
programas_y_matriculas_seleccion <- programas_y_matriculas_seleccion %>%
  group_by(nombre_institucion) %>%
  mutate(primer_curso_promedio = ifelse(is.na(primer_curso), mean(primer_curso, na.rm = TRUE), primer_curso),
         total_primer_curso = sum(primer_curso_promedio, na.rm = TRUE),
         participacion_primer_curso = primer_curso_promedio / total_primer_curso) %>%
  ungroup()

# Paso 2: Calcular el promedio general de primer_curso_promedio
promedio_general <- mean(programas_y_matriculas_seleccion$primer_curso_promedio, na.rm = TRUE)

# Paso 3: Completar los NaN en primer_curso_promedio con el promedio general
programas_y_matriculas_seleccion <- programas_y_matriculas_seleccion %>%
  mutate(primer_curso_promedio = ifelse(is.na(primer_curso_promedio), promedio_general, primer_curso_promedio))

# Paso 4: Calcular las participaciones de las variables cine_f_2013_ac_campo_amplio, cine_f_2013_ac_campo_especific y cine_f_2013_ac_campo_detallado a partir de primer_curso_promedio
programas_y_matriculas_seleccion <- programas_y_matriculas_seleccion %>%
  group_by(cine_f_2013_ac_campo_amplio) %>%
  mutate(total_campo_amplio = sum(primer_curso_promedio, na.rm = TRUE),
         participacion_campo_amplio = primer_curso_promedio / total_campo_amplio) %>%
  ungroup() %>%
  group_by(cine_f_2013_ac_campo_especific) %>%
  mutate(total_campo_especifico = sum(primer_curso_promedio, na.rm = TRUE),
         participacion_campo_especifico = primer_curso_promedio / total_campo_especifico) %>%
  ungroup() %>%
  group_by(cine_f_2013_ac_campo_detallado) %>%
  mutate(total_campo_detallado = sum(primer_curso_promedio, na.rm = TRUE),
         participacion_campo_detallado = primer_curso_promedio / total_campo_detallado) %>%
  ungroup()

write.csv(programas_y_matriculas_seleccion, "/Users/cristianespinal/Downloads/programas_y_matriculas_seleccion.csv", row.names = FALSE)
write.xlsx(programas_y_matriculas_seleccion, "/Users/cristianespinal/Downloads/programas_y_matriculas_seleccion.xlsx")

# Ver los primeros registros para verificar los cambios
head(programas_y_matriculas_seleccion)

# Regresión 1

# Paso 1: Crear la variable numero_programas
programas_y_matriculas_seleccion <- programas_y_matriculas_seleccion %>%
  group_by(nombre_institucion) %>%
  mutate(numero_programas = n_distinct(nombre_del_programa))

# Paso 2: Crear la variable numero_instituciones
numero_instituciones <- programas_y_matriculas_seleccion %>%
  summarise(numero_instituciones = n_distinct(nombre_institucion))

# Agregar la variable numero_instituciones a la base de datos
programas_y_matriculas_seleccion <- programas_y_matriculas_seleccion %>%
  mutate(numero_instituciones = numero_instituciones$numero_instituciones[1])

# Paso 3: Crear la variable dummy para instituciones públicas
programas_y_matriculas_seleccion <- programas_y_matriculas_seleccion %>%
  mutate(institucion_publica = ifelse(sector == "Oficial", 1, 0))

# Paso 4: Crear una variable dummy para area_de_conocimiento.x igual a Bellas artes
programas_y_matriculas_seleccion <- programas_y_matriculas_seleccion %>%
  mutate(dummy_bellas_artes = ifelse(area_de_conocimiento.x == "Bellas artes", 1, 0))

# Paso 5: Crear una variable dummy para nombre_del_programa que incluya MAESTRIA, Maestría o MAESTRÍA
programas_y_matriculas_seleccion <- programas_y_matriculas_seleccion %>%
  mutate(dummy_maestria = ifelse(grepl("MAESTRIA|Maestría|MAESTRÍA", nombre_del_programa, ignore.case = TRUE), 1, 0))

# Paso 4: Seleccionar las variables necesarias para la regresión
datos_regresion <- programas_y_matriculas_seleccion %>%
  select(numero_programas, numero_instituciones, institucion_publica, 
         primer_curso_promedio, participacion_campo_amplio, 
         participacion_campo_especifico, participacion_campo_detallado)

# Eliminar duplicados para que no haya filas repetidas por institución
datos_regresion <- datos_regresion %>%
  distinct()

# Paso 5: Correr la regresión lineal
modelo <- lm(numero_programas ~ numero_instituciones + institucion_publica + 
               primer_curso_promedio + participacion_campo_amplio + 
               participacion_campo_especifico + participacion_campo_detallado, 
             data = datos_regresion)

# Corrección 1 del modelo

# Crear las variables dummy
programas_y_matriculas_seleccion <- programas_y_matriculas_seleccion %>%
  mutate(dummy_bellas_artes = ifelse(area_de_conocimiento.x == "Bellas artes", 1, 0),
         dummy_maestria = ifelse(grepl("MAESTRIA|Maestría|MAESTRÍA", nombre_del_programa, ignore.case = TRUE), 1, 0))

# Crear la variable numero_programas
programas_y_matriculas_seleccion <- programas_y_matriculas_seleccion %>%
  group_by(nombre_institucion) %>%
  mutate(numero_programas = n_distinct(nombre_del_programa))

# Eliminar la variable numero_instituciones para evitar colinealidad
# Crear la variable institucion_publica
programas_y_matriculas_seleccion <- programas_y_matriculas_seleccion %>%
  mutate(institucion_publica = ifelse(sector == "Oficial", 1, 0))

# Seleccionar las variables necesarias para la regresión
datos_regresion <- programas_y_matriculas_seleccion %>%
  select(numero_programas, institucion_publica, primer_curso_promedio, 
         participacion_campo_amplio, participacion_campo_especifico, 
         participacion_campo_detallado, dummy_bellas_artes, dummy_maestria) %>%
  distinct()

# Correr la regresión lineal
modelo <- lm(numero_programas ~ institucion_publica + primer_curso_promedio + 
               participacion_campo_amplio + participacion_campo_especifico + 
               participacion_campo_detallado + dummy_bellas_artes + dummy_maestria, 
             data = datos_regresion)

# Resumen del modelo
summary(modelo)

# Corrección 2 del modelo

# Crear la variable dummy para ies_acreditada
programas_y_matriculas_seleccion <- programas_y_matriculas_seleccion %>%
  mutate(dummy_ies_acreditada = ifelse(ies_acreditada == "SI", 1, 0))

programas_y_matriculas_seleccion <- programas_y_matriculas_seleccion %>%
  mutate(dummy_ies_acreditada = ifelse(is.na(dummy_ies_acreditada), 0, dummy_ies_acreditada))

# Completar los valores faltantes en costo_matricula_estud_nuevos con el promedio
promedio_costo_matricula <- mean(programas_y_matriculas_seleccion$costo_matricula_estud_nuevos, na.rm = TRUE)
programas_y_matriculas_seleccion <- programas_y_matriculas_seleccion %>%
  mutate(costo_matricula_completo = ifelse(is.na(costo_matricula_estud_nuevos), promedio_costo_matricula, costo_matricula_estud_nuevos))

# Crear las variables dummy adicionales
programas_y_matriculas_seleccion <- programas_y_matriculas_seleccion %>%
  mutate(dummy_bellas_artes = ifelse(area_de_conocimiento.x == "Bellas artes", 1, 0),
         dummy_maestria = ifelse(grepl("MAESTRIA|Maestría|MAESTRÍA", nombre_del_programa, ignore.case = TRUE), 1, 0))

# Crear la variable numero_programas
programas_y_matriculas_seleccion <- programas_y_matriculas_seleccion %>%
  group_by(nombre_institucion) %>%
  mutate(numero_programas = n_distinct(nombre_del_programa))

# Crear la variable institucion_publica
programas_y_matriculas_seleccion <- programas_y_matriculas_seleccion %>%
  mutate(institucion_publica = ifelse(sector == "Oficial", 1, 0))

# Seleccionar las variables necesarias para la regresión
datos_regresion <- programas_y_matriculas_seleccion %>%
  select(numero_programas, institucion_publica, primer_curso_promedio, 
         participacion_campo_amplio, participacion_campo_especifico, 
         participacion_campo_detallado, dummy_bellas_artes, dummy_maestria,
         dummy_ies_acreditada, costo_matricula_completo) %>%
  distinct()

# Correr la regresión lineal
modelo <- glm(numero_programas ~ institucion_publica + primer_curso_promedio + 
               participacion_campo_amplio + participacion_campo_especifico + 
               participacion_campo_detallado + dummy_bellas_artes + dummy_maestria +
               dummy_ies_acreditada + costo_matricula_completo, 
             data = datos_regresion, family = "poisson")

# Resumen del modelo
summary(modelo)

# Nueva base de datos -- versión 2

# Unir las dos bases de datos a partir de 'codigo_snies_del_programa'
matriculas_data_v2 <- matriculas_data %>%
  left_join(programas_data %>% select(codigo_snies_del_programa, costo_matricula_estud_nuevos), by = c("codigo_snies_del_programa" = "codigo_snies_del_programa"))

# Completar 'costo_matricula_estud_nuevos' con el promedio de la columna por 'desc_cine_campo_especifico'
matriculas_data_v2 <- matriculas_data_v2 %>%
  group_by(desc_cine_campo_especifico) %>%
  mutate(costo_matricula_estud_nuevos = ifelse(is.na(costo_matricula_estud_nuevos), mean(costo_matricula_estud_nuevos, na.rm = TRUE), costo_matricula_estud_nuevos)) %>%
  ungroup()

# Función para calcular la participación
calcular_participacion <- function(df, grupo, columna) {
  df %>%
    group_by(!!sym(grupo)) %>%
    mutate(!!sym(columna) := n() / nrow(df)) %>%
    ungroup()
}

# Crear variables de participación
matriculas_data_v2 <- matriculas_data_v2 %>%
  calcular_participacion("desc_cine_campo_especifico", "participacion_nombre_institucion_desc_cine_campo_especifico") %>%
  calcular_participacion("desc_cine_campo_especifico", "participacion_nombre_institucion_desc_cine_campo_especifico") %>%
  calcular_participacion("desc_cine_campo_especifico", "participacion_nombre_institucion_desc_cine_campo_especifico") %>%
  calcular_participacion("desc_cine_campo_especifico", "participacion_nombre_institucion_desc_cine_campo_especifico") %>%
  calcular_participacion("area_de_conocimiento", "participacion_nombre_institucion_area_de_conocimiento")

# Completar 'primer_curso' con el promedio de la columna por 'desc_cine_campo_especifico'
matriculas_data_v2 <- matriculas_data_v2 %>%
  group_by(desc_cine_campo_especifico) %>%
  mutate(primer_curso = ifelse(is.na(primer_curso), mean(primer_curso, na.rm = TRUE), primer_curso)) %>%
  ungroup()

# Crear una dummy si el programa es acreditado
matriculas_data_v2 <- matriculas_data_v2 %>%
  mutate(dummy_programa_acreditado = ifelse(programa_acreditado == "SI", 1, 0))

# Crear una dummy si el programa es de pregrado
matriculas_data_v2 <- matriculas_data_v2 %>%
  mutate(dummy_pregrado = ifelse(nivel_academico == "PREGRADO", 1, 0))

# Crear una dummy si el programa es de posgrado
matriculas_data_v2 <- matriculas_data_v2 %>%
  mutate(dummy_posgrado = ifelse(nivel_academico == "POSGRADO", 1, 0))

# Crear una dummy si el programa tiene un nivel de formación de Maestría
matriculas_data_v2 <- matriculas_data_v2 %>%
  mutate(dummy_maestria = ifelse(nivel_de_formacion == "Maestría", 1, 0))

# Crear una dummy si el programa tiene un nivel de formación de Especialización
matriculas_data_v2 <- matriculas_data_v2 %>%
  mutate(dummy_especializacion = ifelse(nivel_de_formacion == "Especialización", 1, 0))

# Crear una variable que represente el número de programas que tiene cada IES
matriculas_data_v2 <- matriculas_data_v2 %>%
  group_by(institucion_de_educacion_superior_ies) %>%
  mutate(numero_programas_por_ies = n_distinct(programa_academico)) %>%
  ungroup()

# Crear una variable que represente el número de programas por campo amplio según la IES
matriculas_data_v2 <- matriculas_data_v2 %>%
  group_by(institucion_de_educacion_superior_ies, desc_cine_campo_amplio) %>%
  mutate(numero_programas_por_campo_amplio = n_distinct(programa_academico)) %>%
  ungroup()

# Crear una variable que represente el número de programas por campo específico según la IES
matriculas_data_v2 <- matriculas_data_v2 %>%
  group_by(institucion_de_educacion_superior_ies, desc_cine_campo_especifico) %>%
  mutate(numero_programas_por_campo_especifico = n_distinct(programa_academico)) %>%
  ungroup()

# Regresión general

# Asegúrate de que tienes las variables adecuadas en el dataset
required_columns <- c("numero_programas_por_campo_amplio", "numero_programas_por_campo_especifico",
                      "participacion_nombre_institucion_area_de_conocimiento", "costo_matricula_estud_nuevos",
                      "dummy_programa_acreditado")

# Verificar si las columnas requeridas están presentes
missing_columns <- setdiff(required_columns, names(matriculas_data_v2))
if(length(missing_columns) > 0) {
  stop(paste("Las siguientes columnas faltan en los datos:", paste(missing_columns, collapse = ", ")))
}

# Ajustar el modelo de regresión lineal
modelo_oferta <- lm(numero_programas_por_ies ~ numero_programas_por_campo_amplio + numero_programas_por_campo_especifico +
                      participacion_nombre_institucion_area_de_conocimiento + costo_matricula_estud_nuevos +
                      dummy_programa_acreditado, data = matriculas_data_v2)

# Resumen del modelo para ver los coeficientes
summary(modelo_oferta)

# Obtener los coeficientes del modelo
coeficientes <- coef(modelo_oferta)[-1]  # Excluir el intercepto

# Normalizar los coeficientes para que sumen 0.25
ponderaciones <- coeficientes / sum(coeficientes) * 0.25

# Crear un data frame con las ponderaciones
ponderaciones_df <- data.frame(Variable = names(ponderaciones), Ponderacion = ponderaciones)

# Convertir las ponderaciones a valores absolutos para evitar negativas en el contexto de ponderación
ponderaciones_abs <- abs(ponderaciones)

# Normalizar las ponderaciones para que sumen 0.25
ponderaciones_normalizadas <- ponderaciones_abs / sum(ponderaciones_abs) * 0.25

# Crear un data frame con las ponderaciones normalizadas
ponderaciones_normalizadas_df <- data.frame(Variable = names(ponderaciones_normalizadas), Ponderacion = ponderaciones_normalizadas)

# Mostrar las ponderaciones normalizadas
print(ponderaciones_normalizadas_df)

ponderaciones <- c(2.955561e-03, 4.923421e-04, 2.238351e-01, 1.445994e-10, 2.271700e-02)
names(ponderaciones) <- c("numero_programas_por_campo_amplio", "numero_programas_por_campo_especifico", 
                          "participacion_nombre_institucion_area_de_conocimiento", "costo_matricula_estud_nuevos", 
                          "dummy_programa_acreditado")

# Normalizar las ponderaciones para que sumen 0.25
ponderaciones_normalizadas <- ponderaciones / sum(ponderaciones) * 0.25

# Crear un data frame con las ponderaciones normalizadas
ponderaciones_normalizadas_df <- data.frame(Variable = names(ponderaciones_normalizadas), 
                                            Ponderacion = ponderaciones_normalizadas)

# Formatear las ponderaciones para que no se muestren en notación científica
ponderaciones_normalizadas_df$Ponderacion <- format(ponderaciones_normalizadas_df$Ponderacion, scientific = FALSE, digits = 10) * 100

ponderaciones_normalizadas_df$Ponderacion <- ponderaciones_normalizadas_df$Ponderacion*100

# Mostrar las ponderaciones normalizadas
print(ponderaciones_normalizadas_df)

# Mostrar las ponderaciones
print(ponderaciones_df)

# Base de datos con diseño

# Supongamos que `nombres_unicos` es tu vector de nombres de programas
nombres_unicos <- c("INGENIERIA AGRONOMICA", "MEDICINA VETERINARIA", "ZOOTECNIA", "DISEÑO GRAFICO", "DISEÑO INDUSTRIAL", "CINE Y TELEVISION", "ENFERMERIA")

# Define las palabras clave relacionadas con Diseño
palabras_clave <- c("DISEÑO", "DESIGN", "ARTE", "ARTES", "CREATIVO", "VISUAL", "GRAFICO", "INDUSTRIAL", "URBANO", "PRODUCTO")

# Filtrar los nombres de los programas que contienen alguna palabra clave
programas_diseno <- nombres_unicos[grepl(paste(palabras_clave, collapse = "|"), nombres_unicos, ignore.case = TRUE)]

# Imprimir los programas relacionados con Diseño
print(programas_diseno)

# Define las palabras clave relacionadas con Diseño
palabras_clave <- c("DISEÑO", "DESIGN", "ARTE", "ARTES", "CREATIVO", "VISUAL", "GRAFICO", "URBANO", "PRODUCTO", "3D", "3d", "MODA")

# Filtrar los programas que contienen alguna palabra clave en 'programa_academico'
matriculas_data_v2_filtrada <- matriculas_data_v2_filtrada %>%
  filter(grepl(paste(palabras_clave, collapse = "|"), programa_academico, ignore.case = TRUE))

# Crear una dummy si el desc_cine_campo_amplio es igual a "Arte y Humanidades"
matriculas_data_v2_filtrada <- matriculas_data_v2_filtrada %>%
  mutate(dummy_arte_humanidades = ifelse(desc_cine_campo_amplio == "Arte y Humanidades", 1, 0))

# Crear una dummy si el sector_ies es "OFICIAL"
matriculas_data_v2_filtrada <- matriculas_data_v2_filtrada %>%
  mutate(dummy_sector_oficial = ifelse(sector_ies == "OFICIAL", 1, 0))

# Regresión 2. BD filtrada: matriculas_data_v2_filtrada

modelo_oferta <- lm(numero_programas_por_ies ~ numero_programas_por_campo_amplio + 
                      numero_programas_por_campo_especifico + 
                      costo_matricula_estud_nuevos + 
                      dummy_programa_acreditado + 
                      dummy_sector_oficial, 
                    data = matriculas_data_v2_filtrada)

# BD 3 -- artes y humanidades

# Filtrar la base de datos por desc_cine_campo_amplio = "Arte y Humanidades"
matriculas_data_v2_artesyhumanidades <- matriculas_data_v2 %>%
  filter(desc_cine_campo_amplio == "Arte y Humanidades")

# Ajustar el modelo de regresión lineal con la nueva base de datos filtrada
modelo_oferta_artesyhumanidades <- lm(numero_programas_por_ies ~ numero_programas_por_campo_amplio + 
                                        numero_programas_por_campo_especifico + 
                                        participacion_nombre_institucion_area_de_conocimiento + 
                                        costo_matricula_estud_nuevos + 
                                        dummy_programa_acreditado + 
                                        dummy_sector_oficial, 
                                      data = matriculas_data_v2_artesyhumanidades)

# Resumen del modelo para ver los coeficientes
summary(modelo_oferta_artesyhumanidades)
coeficientes_artesyhumanidades <- coef(modelo_oferta_artesyhumanidades)[-1]  # Excluir el intercepto
ponderaciones_artesyhumanidades <- coeficientes_artesyhumanidades / sum(abs(coeficientes_artesyhumanidades)) * 0.25
ponderaciones_normalizadas_artesyhumanidades_df <- data.frame(Variable = names(ponderaciones_artesyhumanidades), 
                                                              Ponderacion = format(ponderaciones_artesyhumanidades, scientific = FALSE, digits = 10))
print(ponderaciones_normalizadas_artesyhumanidades_df)

# Instalar y cargar el paquete car si no está instalado
if (!require(car)) install.packages("car")
library(car)

# Calcular VIF (Variance Inflation Factor)
vif(modelo_oferta_artesyhumanidades)

# Instalar y cargar el paquete lmtest si no está instalado
if (!require(lmtest)) install.packages("lmtest")
library(lmtest)

# Test de Breusch-Pagan
bptest(modelo_oferta_artesyhumanidades)

# Graficar residuos vs valores ajustados
plot(modelo_oferta_artesyhumanidades$fitted.values, modelo_oferta_artesyhumanidades$residuals)
abline(h = 0, col = "red")

# Ajustar el modelo con errores estándar robustos
modelo_oferta_artesyhumanidades_robusto <- coeftest(modelo_oferta_artesyhumanidades, vcov = vcovHC(modelo_oferta_artesyhumanidades, type = "HC1"))

library(sandwich)

# Regresión 3

# Ajustar el modelo con errores estándar robustos
modelo_oferta_artesyhumanidades_robusto <- coeftest(modelo_oferta_artesyhumanidades, vcov = vcovHC(modelo_oferta_artesyhumanidades, type = "HC1"))

# Obtener los coeficientes del modelo robusto (excluyendo el intercepto)
coeficientes_robustos <- modelo_oferta_artesyhumanidades_robusto[-1, 1]  # Excluir el intercepto

# Calcular las ponderaciones normalizadas para que sumen 0.25
ponderaciones_robustas <- abs(coeficientes_robustos) / sum(abs(coeficientes_robustos)) * 0.25

# Crear un data frame con las ponderaciones normalizadas
ponderaciones_normalizadas_robustas_df <- data.frame(
  Variable = names(ponderaciones_robustas), 
  Ponderacion = format(ponderaciones_robustas, scientific = FALSE, digits = 10)
)

# Mostrar las ponderaciones normalizadas
print(ponderaciones_normalizadas_robustas_df)

# Mostrar los resultados del modelo con errores estándar robustos
print(modelo_oferta_artesyhumanidades_robusto)

# Gráfico 

datos_agrupados <- matriculas_data_v2_artesyhumanidades %>%
  group_by(institucion_de_educacion_superior_ies, desc_cine_campo_amplio) %>%
  summarise(numero_programas = n_distinct(programa_academico),
            total_primer_curso = sum(primer_curso, na.rm = TRUE)) %>%
  ungroup()

# Verificar los datos agrupados
print(head(datos_agrupados))

datos_agrupados <- matriculas_data_v2_artesyhumanidades %>%
  group_by(institucion_de_educacion_superior_ies) %>%
  summarise(numero_programas = n_distinct(programa_academico),
            total_primer_curso = sum(primer_curso, na.rm = TRUE)) %>%
  ungroup()

# Verificar los datos agrupados
print(head(datos_agrupados))

# Crear el gráfico de dispersión
grafico <- ggplxot(datos_agrupados, aes(x = total_primer_curso, y = numero_programas)) +
  geom_point(aes(color = institucion_de_educacion_superior_ies), size = 3) +
  theme_minimal() +
  labs(title = "Relación entre el Número de Programas y el Número de Matriculados",
       x = "Número de Matriculados (primer_curso)",
       y = "Número de Programas",
       color = "Institución") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Paso 1: Filtrar los datos por la categoría de artes y humanidades
artes_humanidades_data <- matriculas_data_v2_artesyhumanidades %>%
  filter(desc_cine_campo_amplio == "Arte y Humanidades")

# Paso 2: Contar el número de programas por IES
conteo_programas_por_ies <- artes_humanidades_data %>%
  group_by(institucion_de_educacion_superior_ies) %>%
  summarise(numero_programas_artes_humanidades = n_distinct(codigo_snies_del_programa))

# Paso 3: Unir este conteo con la base de datos original
matriculas_data_v2_artesyhumanidades <- matriculas_data_v2_artesyhumanidades %>%
  left_join(conteo_programas_por_ies, by = "institucion_de_educacion_superior_ies")

# Paso 1: Agrupar los datos por IES para obtener el número total de estudiantes en el primer curso
datos_agrupados_por_ies <- matriculas_data_v2_artesyhumanidades %>%
  group_by(institucion_de_educacion_superior_ies) %>%
  summarise(numero_programas_artes_humanidades = first(numero_programas_artes_humanidades),
            total_primer_curso = sum(primer_curso, na.rm = TRUE))

# Paso 2: Crear la gráfica de dispersión
ggplot(datos_agrupados_por_ies, aes(x = numero_programas_artes_humanidades, y = total_primer_curso)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  geom_point(data = filter(datos_agrupados_por_ies, institucion_de_educacion_superior_ies == "INSTITUCIÓN UNIVERSITARIA PASCUAL BRAVO"),
             aes(x = numero_programas_artes_humanidades, y = total_primer_curso), 
             color = "red", size = 5, shape = 17) +
  labs(title = "Número de Programas en Artes y Humanidades vs. Estudiantes en Primer Curso por IES",
       x = "Número de Programas en Artes y Humanidades",
       y = "Número de Estudiantes en Primer Curso") +
  theme_minimal()

# Sin datos aticos

# Paso 1: Agrupar los datos por IES para obtener el número total de estudiantes en el primer curso
datos_agrupados_por_ies <- matriculas_data_v2_artesyhumanidades %>%
  group_by(institucion_de_educacion_superior_ies) %>%
  summarise(numero_programas_artes_humanidades = first(numero_programas_artes_humanidades),
            total_primer_curso = sum(primer_curso, na.rm = TRUE))

# Paso 2: Eliminar los datos atípicos utilizando el método de los cuantiles
quantiles <- quantile(datos_agrupados_por_ies$total_primer_curso, probs = c(0.05, 0.95), na.rm = TRUE)
datos_filtrados <- datos_agrupados_por_ies %>%
  filter(total_primer_curso >= quantiles[1] & total_primer_curso <= quantiles[2])

# Paso 3: Crear la gráfica de dispersión sin datos atípicos
ggplot(datos_filtrados, aes(x = numero_programas_artes_humanidades, y = total_primer_curso)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  geom_point(data = filter(datos_filtrados, institucion_de_educacion_superior_ies == "INSTITUCIÓN UNIVERSITARIA PASCUAL BRAVO"),
             aes(x = numero_programas_artes_humanidades, y = total_primer_curso), 
             color = "red", size = 5, shape = 17) +
  labs(title = "Número de Programas en Artes y Humanidades vs. Estudiantes en Primer Curso por IES (Sin Outliers)",
       x = "Número de Programas en Artes y Humanidades",
       y = "Número de Estudiantes en Primer Curso") +
  theme_minimal()

# Filtrar los programas de posgrado
posgrados <- matriculas_data_v2_artesyhumanidades %>%
  filter(dummy_posgrado == 1)

# Obtener los nombres únicos de programas académicos junto con el campo amplio asociado
programas_unicos <- posgrados %>%
  distinct(programa_academico, desc_cine_campo_amplio)

# Especificar la ruta de guardado
output_path <- '/Users/cristianespinal/Downloads/programas_posgrado_unicos.xlsx'

# Exportar a un archivo Excel
write.xlsx(programas_unicos, output_path, sheetName = "Posgrados", rowNames = FALSE)

# Ver el resultado
print(programas_unicos)

# Modelo 3: Modelo GLM con selección de variables -- Poisson o Quasi-Poisson con regularización Lasso (Least Absolute Shrinkage and Selection Operator) o Ridge (Tikhonov regularization) ----------------------

# Convertir todas las variables categóricas en factores
matriculas_data_v2_artesyhumanidades <- matriculas_data_v2_artesyhumanidades %>%
  mutate(
    sector_ies = factor(sector_ies),
    ies_acreditada = factor(ies_acreditada),
    departamento_de_domicilio_de_la_ies = factor(departamento_de_domicilio_de_la_ies),
    municipio_de_domicilio_de_la_ies = factor(municipio_de_domicilio_de_la_ies),
    programa_academico = factor(programa_academico),
    programa_acreditado = factor(programa_acreditado),
    nivel_academico = factor(nivel_academico),
    dummy_sector_oficial = factor(dummy_sector_oficial),
  )

# Verificar y eliminar variables categóricas con solo un nivel
factor_cols <- sapply(matriculas_data_v2_artesyhumanidades, is.factor)
levels_check <- sapply(matriculas_data_v2_artesyhumanidades[, factor_cols], nlevels)
single_level_vars <- names(levels_check[levels_check < 2])

if (length(single_level_vars) > 0) {
  cat("Variables con un solo nivel y que serán eliminadas:\n")
  print(single_level_vars)
  matriculas_data_v2_artesyhumanidades <- matriculas_data_v2_artesyhumanidades %>%
    select(-one_of(single_level_vars))
}

# Convertir nuevamente todas las variables de carácter restantes en factores
matriculas_data_v2_artesyhumanidades <- matriculas_data_v2_artesyhumanidades %>%
  mutate_if(is.character, as.factor)

# Convertir las variables restantes en una matriz de diseño
data <- model.matrix(numero_programas_por_ies ~ . - 1, data = matriculas_data_v2_artesyhumanidades)

# Variable respuesta
y <- matriculas_data_v2_artesyhumanidades$numero_programas_por_ies

# Ajustar el modelo Poisson con Lasso
fit_lasso <- glmnet(data, y, family = "poisson", alpha = 1)

# Ajustar el modelo Poisson con Ridge
fit_ridge <- glmnet(data, y, family = "poisson", alpha = 0)

# Ajustar el modelo Poisson con Elastic Net
fit_elastic_net <- glmnet(data, y, family = "poisson", alpha = 0.5)

# Validación cruzada para seleccionar el mejor lambda
cv_lasso <- cv.glmnet(data, y, family = "poisson", alpha = 1)
cv_ridge <- cv.glmnet(data, y, family = "poisson", alpha = 0)
cv_elastic_net <- cv.glmnet(data, y, family = "poisson", alpha = 0.5)

# Obtener los mejores lambdas
best_lambda_lasso <- cv_lasso$lambda.min
best_lambda_ridge <- cv_ridge$lambda.min
best_lambda_elastic_net <- cv_elastic_net$lambda.min

# Obtener los coeficientes de los mejores modelos
coef_lasso <- coef(cv_lasso, s = "lambda.min")
coef_ridge <- coef(cv_ridge, s = "lambda.min")
coef_elastic_net <- coef(cv_elastic_net, s = "lambda.min")

# Mostrar los coeficientes de los modelos
print("Coeficientes del modelo Lasso:")
print(coef_lasso)
print("Coeficientes del modelo Ridge:")
print(coef_ridge)
print("Coeficientes del modelo Elastic Net:")
print(coef_elastic_net)

# Extraer los coeficientes y convertirlos en dataframes
extract_coef <- function(model, lambda, model_name) {
  coef_matrix <- as.matrix(coef(model, s = lambda))
  coef_df <- as.data.frame(coef_matrix)
  coef_df <- coef_df[coef_df != 0, , drop = FALSE] # Filtrar coeficientes diferentes de cero
  coef_df <- data.frame(term = rownames(coef_df), estimate = coef_df[, 1])
  coef_df$model <- model_name
  return(coef_df)
}

# Extraer y resumir los coeficientes del modelo Lasso
coef_lasso <- extract_coef(cv_lasso, cv_lasso$lambda.min, "Lasso")

# Extraer y resumir los coeficientes del modelo Ridge
coef_ridge <- extract_coef(cv_ridge, cv_ridge$lambda.min, "Ridge")

# Extraer y resumir los coeficientes del modelo Elastic Net
coef_elastic_net <- extract_coef(cv_elastic_net, cv_elastic_net$lambda.min, "Elastic Net")

# Combinar los resultados en un solo dataframe
coef_summary <- bind_rows(coef_lasso, coef_ridge, coef_elastic_net)

# Mostrar el resumen de los coeficientes
print(coef_summary)

# Base de datos y regresión 3 -- solo LM/GLM ------------

# Cargar BD programas SNIES
file_path_1 <- "/Users/cristianespinal/Downloads/Programas-2.xlsx"
programas_data <- read_excel(file_path_1)
head(programas_data)
programas_data <- clean_names(programas_data)
head(programas_data)

# Cargar BD programas Matriculados Primer Curso
file_path_2 <- "/Users/cristianespinal/Downloads/PRIMER_CURSO_2022.xlsx"
matriculas_data <- read_excel(file_path_2)
head(matriculas_data)
matriculas_data <- clean_names(matriculas_data)
head(matriculas_data)

# Agrupar por código SNIES
programas_y_matriculas_SNIES <- programas_data_diseño %>%
  left_join(matriculas_data_diseño, by = "codigo_snies_del_programa")

names(programas_y_matriculas_SNIES)

# Filtrar por nivel_academico y departamento_de_domicilio_de_la_ies
matriculas_data_diseño <- matriculas_data %>%
  filter(nivel_academico == "POSGRADO",
         departamento_de_domicilio_de_la_ies == "Antioquia",
         grepl("Diseño", programa_academico, ignore.case = TRUE))

programas_data_diseño <- programas_data %>%
  filter(nivel_academico == "Posgrado",
         departamento_oferta_programa == "Antioquia",
         grepl("Diseño", nombre_del_programa, ignore.case = TRUE))

programas_y_matriculas_seleccion <- programas_y_matriculas_SNIES %>%
  select(nombre_institucion, sector, codigo_snies_del_programa, nombre_del_programa, 
         cine_f_2013_ac_campo_amplio, cine_f_2013_ac_campo_especific, 
         cine_f_2013_ac_campo_detallado, area_de_conocimiento.x, 
         nucleo_basico_del_conocimiento, nivel_de_formacion.x, 
         costo_matricula_estud_nuevos, primer_curso, ies_acreditada, reconocimiento_del_ministerio)

# Paso 1: Calcular la participación de primer_curso por nombre_institucion
programas_y_matriculas_seleccion <- programas_y_matriculas_seleccion %>%
  group_by(nombre_institucion) %>%
  mutate(primer_curso_promedio = ifelse(is.na(primer_curso), mean(primer_curso, na.rm = TRUE), primer_curso),
         total_primer_curso = sum(primer_curso_promedio, na.rm = TRUE),
         participacion_primer_curso = primer_curso_promedio / total_primer_curso) %>%
  ungroup()

# Paso 2: Calcular el promedio general de primer_curso_promedio
promedio_general <- mean(programas_y_matriculas_seleccion$primer_curso_promedio, na.rm = TRUE)

# Paso 3: Completar los NaN en primer_curso_promedio con el promedio general
programas_y_matriculas_seleccion <- programas_y_matriculas_seleccion %>%
  mutate(primer_curso_promedio = ifelse(is.na(primer_curso_promedio), promedio_general, primer_curso_promedio))

# Paso 4: Calcular las participaciones de las variables cine_f_2013_ac_campo_amplio, cine_f_2013_ac_campo_especific y cine_f_2013_ac_campo_detallado a partir de primer_curso_promedio
programas_y_matriculas_seleccion <- programas_y_matriculas_seleccion %>%
  group_by(cine_f_2013_ac_campo_amplio) %>%
  mutate(total_campo_amplio = sum(primer_curso_promedio, na.rm = TRUE),
         participacion_campo_amplio = primer_curso_promedio / total_campo_amplio) %>%
  ungroup() %>%
  group_by(cine_f_2013_ac_campo_especific) %>%
  mutate(total_campo_especifico = sum(primer_curso_promedio, na.rm = TRUE),
         participacion_campo_especifico = primer_curso_promedio / total_campo_especifico) %>%
  ungroup() %>%
  group_by(cine_f_2013_ac_campo_detallado) %>%
  mutate(total_campo_detallado = sum(primer_curso_promedio, na.rm = TRUE),
         participacion_campo_detallado = primer_curso_promedio / total_campo_detallado) %>%
  ungroup()

# Define las palabras clave relacionadas con Diseño
palabras_clave <- c("DISEÑO", "DESIGN", "ARTE", "ARTES", "CREATIVO", "VISUAL", "GRAFICO", "URBANO", "PRODUCTO", "3D", "3d", "MODA")

# Filtrar los programas que contienen alguna palabra clave en 'programa_academico'
matriculas_data_v2_filtrada <- matriculas_data_v2_filtrada %>%
  filter(grepl(paste(palabras_clave, collapse = "|"), programa_academico, ignore.case = TRUE))

# Crear una dummy si el desc_cine_campo_amplio es igual a "Arte y Humanidades"
matriculas_data_v2_filtrada <- matriculas_data_v2_filtrada %>%
  mutate(dummy_arte_humanidades = ifelse(desc_cine_campo_amplio == "Arte y Humanidades", 1, 0))

# Crear una dummy si el sector_ies es "OFICIAL"
matriculas_data_v2_filtrada <- matriculas_data_v2_filtrada %>%
  mutate(dummy_sector_oficial = ifelse(sector_ies == "OFICIAL", 1, 0))

# Convertir todas las variables categóricas en factores
matriculas_data_v2_artesyhumanidades <- matriculas_data_v2_artesyhumanidades %>%
  mutate(
    sector_ies = factor(sector_ies),
    ies_acreditada = factor(ies_acreditada),
    departamento_de_domicilio_de_la_ies = factor(departamento_de_domicilio_de_la_ies),
    municipio_de_domicilio_de_la_ies = factor(municipio_de_domicilio_de_la_ies),
    programa_academico = factor(programa_academico),
    programa_acreditado = factor(programa_acreditado),
    nivel_academico = factor(nivel_academico),
    dummy_sector_oficial = factor(dummy_sector_oficial),
  )

# Verificar y eliminar variables categóricas con solo un nivel
factor_cols <- sapply(matriculas_data_v2_artesyhumanidades, is.factor)
levels_check <- sapply(matriculas_data_v2_artesyhumanidades[, factor_cols], nlevels)
single_level_vars <- names(levels_check[levels_check < 2])

if (length(single_level_vars) > 0) {
  cat("Variables con un solo nivel y que serán eliminadas:\n")
  print(single_level_vars)
  matriculas_data_v2_artesyhumanidades <- matriculas_data_v2_artesyhumanidades %>%
    select(-one_of(single_level_vars))
}

# Regresiones

# Filtrar las variables numéricas
variables_numericas <- matriculas_data_v2_artesyhumanidades %>%
  select(where(is.numeric))

# Ajustar el modelo de regresión de Poisson
modelo_poisson_numerico <- glm(numero_programas_artes_humanidades ~ ., family = poisson, data = variables_numericas)
summary(modelo_poisson_numerico)

# Ajuste del modelo 1: Solucionar problema de multicolinealidad

# Crear una función para calcular VIF solo para las variables no aliased
calculate_vif <- function(model) {
  # Obtener los coeficientes no aliased
  aliased_coeffs <- is.na(coef(model))
  variables <- names(aliased_coeffs)[!aliased_coeffs]
  
  # Remover el Intercepto de la fórmula
  variables <- variables[variables != "(Intercept)"]
  
  # Crear una fórmula de regresión solo con las variables no aliased
  formula <- as.formula(paste("numero_programas_artes_humanidades ~", paste(variables, collapse = " + ")))
  
  # Ajustar el modelo solo con las variables no aliased
  model_no_alias <- glm(formula, family = poisson, data = variables_numericas)
  
  # Calcular VIF
  vif_values <- vif(model_no_alias)
  return(vif_values)
}

# Calcular VIF para el modelo de Poisson
vif_values <- calculate_vif(modelo_poisson_numerico)
vif_values

# Eliminar variables que general multicolinealidad

# Variables con VIF mayores a 10
variables_alto_vif <- names(vif_values[vif_values > 10])
variables_alto_vif

# Remover variables con VIF altos
variables_numericas_dep <- variables_numericas %>%
  select(-one_of(variables_alto_vif))

# Correr de nuevo

# Ajustar el modelo de regresión de Poisson depurado
modelo_poisson_numerico_dep <- glm(numero_programas_artes_humanidades ~ ., family = poisson, data = variables_numericas_dep)
summary(modelo_poisson_numerico_dep)

# Calcular VIF para el modelo depurado
vif_values_dep <- calculate_vif(modelo_poisson_numerico_dep)
vif_values_dep

# Calcular VIF para el modelo depurado
vif_values_dep <- calculate_vif(modelo_poisson_numerico_dep)
vif_values_dep

# Identificar variables no significativas
summary_modelo_dep <- summary(modelo_poisson_numerico_dep)
variables_no_significativas_dep <- rownames(summary_modelo_dep$coefficients)[summary_modelo_dep$coefficients[, 4] > 0.05]
variables_no_significativas_dep <- variables_no_significativas_dep[variables_no_significativas_dep != "(Intercept)"]
variables_no_significativas_dep

# Remover variables no significativas
variables_numericas_final <- variables_numericas_dep %>%
  select(-one_of(variables_no_significativas_dep))

# Ajustar el modelo de regresión de Poisson final
modelo_poisson_numerico_final <- glm(numero_programas_artes_humanidades ~ ., family = poisson, data = variables_numericas_final)
summary(modelo_poisson_numerico_final)

# Ajustar el modelo de regresión de Poisson con las variables numéricas filtradas previamente
modelo_poisson_numerico_dep <- glm(numero_programas_artes_humanidades ~ ., family = poisson, data = variables_numericas_dep)
summary(modelo_poisson_numerico_dep)

# Ajustar el modelo de regresión de Poisson con las variables numéricas filtradas previamente
modelo_poisson_numerico_dep <- glm(numero_programas_artes_humanidades ~ ., family = poisson, data = variables_numericas_dep)
summary(modelo_poisson_numerico_dep)

# Identificar variables con coeficientes NA
variables_na <- rownames(summary(modelo_poisson_numerico_dep)$coefficients)[is.na(coef(modelo_poisson_numerico_dep))]
variables_na

# Remover variables con coeficientes NA
variables_numericas_dep <- variables_numericas_dep %>%
  select(-one_of(variables_na))

# Ajustar el modelo de regresión de Poisson nuevamente sin las variables NA
modelo_poisson_numerico_dep <- glm(numero_programas_artes_humanidades ~ ., family = poisson, data = variables_numericas_dep)
summary(modelo_poisson_numerico_dep)

# Identificar variables no significativas
summary_modelo_dep <- summary(modelo_poisson_numerico_dep)
variables_no_significativas_dep <- rownames(summary_modelo_dep$coefficients)[summary_modelo_dep$coefficients[, 4] > 0.05]
variables_no_significativas_dep <- variables_no_significativas_dep[variables_no_significativas_dep != "(Intercept)"]
variables_no_significativas_dep

# Remover variables no significativas
variables_numericas_final <- variables_numericas_dep %>%
  select(-one_of(variables_no_significativas_dep))

# Ajustar el modelo de regresión de Poisson final
modelo_poisson_numerico_final <- glm(numero_programas_artes_humanidades ~ ., family = poisson, data = variables_numericas_final)
summary(modelo_poisson_numerico_final)

# Calcular VIF para el modelo final
vif_values_final <- calculate_vif(modelo_poisson_numerico_final)
vif_values_final

# Diagnóstico de residuos
residuos_final <- residuals(modelo_poisson_numerico_final, type = "deviance")

# Graficar residuos
ggplot(data.frame(residuos = residuos_final), aes(x = residuos)) +
  geom_histogram(binwidth = 0.5, fill = "blue", alpha = 0.7) +
  labs(title = "Histograma de los Residuos del Modelo Final",
       x = "Residuos",
       y = "Frecuencia")

# Gráfico Q-Q de los residuos
qqnorm(residuos_final)
qqline(residuos_final, col = "red")

# Extraer los coeficientes del modelo final
coeficientes_final <- broom::tidy(modelo_poisson_numerico_final)

# Graficar los coeficientes
ggplot(coeficientes_final, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0.2) +
  coord_flip() +
  labs(title = "Coeficientes del Modelo de Regresión de Poisson Final (Variables Numéricas)",
       x = "Variables",
       y = "Estimación del Coeficiente") +
  theme_minimal()

# Normalizar los coeficientes para que sumen 25%
coeficientes_final$ponderado <- (coeficientes_final$estimate / sum(abs(coeficientes_final$estimate))) * 25

# Visualizar los coeficientes ponderados
ggplot(coeficientes_final, aes(x = reorder(term, ponderado), y = ponderado)) +
  geom_point() +
  coord_flip() +
  labs(title = "Coeficientes Ponderados del Modelo de Regresión de Poisson Final (Variables Numéricas)",
       x = "Variables",
       y = "Coeficiente Ponderado (sobre 25%)") +
  theme_minimal()

# Crear un dataframe con los coeficientes significativos
coeficientes_significativos <- coeficientes_final %>%
  filter(!is.na(estimate) & p.value < 0.05) %>%
  select(term, estimate)

# Remover el intercepto
coeficientes_significativos <- coeficientes_significativos %>%
  filter(term != "(Intercept)")

# Calcular la suma de los valores absolutos de los coeficientes
suma_coeficientes <- sum(abs(coeficientes_significativos$estimate))

# Normalizar los coeficientes para que sumen 25%
coeficientes_significativos <- coeficientes_significativos %>%
  mutate(ponderado = (abs(estimate) / suma_coeficientes) * 25)

# Ver los coeficientes normalizados
print(coeficientes_significativos)

# Calcular la suma de los valores absolutos de los coeficientes
suma_coeficientes <- sum(abs(coeficientes_significativos$estimate))

# Normalizar los coeficientes para que sumen 25%
coeficientes_significativos <- coeficientes_significativos %>%
  mutate(ponderado = (abs(estimate) / suma_coeficientes) * 25)

# Ver los coeficientes normalizados
print(coeficientes_significativos)

# Graficar los coeficientes ponderados
ggplot(coeficientes_significativos, aes(x = reorder(term, ponderado), y = ponderado)) +
  geom_point() +
  coord_flip() +
  labs(title = "Coeficientes Ponderados del Modelo de Regresión de Poisson",
       x = "Variables",
       y = "Coeficiente Ponderado (sobre 25%)") +
  theme_minimal()

# ---
  
# Crear un dataframe con los coeficientes significativos y sin `NA`
  coeficientes_significativos <- coeficientes_final %>%
  filter(!is.na(estimate) & p.value < 0.05) %>%
  select(term, estimate)

# Remover el intercepto
coeficientes_significativos <- coeficientes_significativos %>%
  filter(term != "(Intercept)")

# Calcular la suma de los valores absolutos de los coeficientes
suma_coeficientes <- sum(abs(coeficientes_significativos$estimate))

# Normalizar los coeficientes para que sumen 25%
coeficientes_significativos <- coeficientes_significativos %>%
  mutate(ponderado = (abs(estimate) / suma_coeficientes) * 25)

# Ver los coeficientes normalizados
print(coeficientes_significativos)

ggplot(coeficientes_significativos, aes(x = reorder(term, ponderado), y = ponderado)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = round(ponderado, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, 
            size = 3, color = "black") +
  coord_flip() +
  labs(title = "Determinantes de la Oferta de Programas en Artes y Humanidades",
       x = "Factores",
       y = "Impacto Ponderado (sobre 25%)") +
  theme_minimal()

ggplot(coeficientes_significativos, aes(x = reorder(term, ponderado), y = ponderado)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = round(ponderado, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, 
            size = 3, color = "black") +
  coord_flip() +
  labs(title = "Determinantes de la Oferta de Programas en Artes y Humanidades",
       x = "Factores",
       y = "Impacto Ponderado (sobre 25%)") +
  theme_minimal()

