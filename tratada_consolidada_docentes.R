# Manipulación de consolidada_docentes

# PERIODO -------------

# Cargar la librería dplyr si no está cargada
library(dplyr)

# Transformar la columna 'Archivo.x' en una nueva columna 'Periodo'
consolidada_docentes <- consolidada_docentes %>%
  mutate(Periodo = sub("^[A-Z]+_", "", Archivo.x))

# Opcionalmente, puedes querer asegurarte de que 'Periodo' sea un número
consolidada_docentes$Periodo <- as.numeric(consolidada_docentes$Periodo)

# Ver los cambios realizados
head(consolidada_docentes)

# FECHA -------------

# Cargar las librerías necesarias
library(dplyr)
library(lubridate)

# Crear la columna 'fecha' en 'consolidada_docentes'
consolidada_docentes <- consolidada_docentes %>%
  mutate(
    Año = as.numeric(substr(Periodo, 1, 4)),  # Extraer el año
    Semestre = as.numeric(substr(Periodo, 5, 5)),  # Extraer el semestre
    fecha = case_when(
      Semestre == 1 ~ make_date(Año, 1, 1),  # Primer semestre: 1 de enero
      Semestre == 2 ~ make_date(Año, 7, 1),  # Segundo semestre: 1 de julio
      TRUE ~ NA_Date_  # Utilizar NA_Date_ para valores de fecha faltantes
    )
  ) %>%
  select(-Año, -Semestre)  # Opcional: eliminar las columnas temporales 'Año' y 'Semestre'

# Ver los cambios realizados
head(consolidada_docentes)

# CONTEO -------------

# Cargar la librería dplyr si no está cargada
library(dplyr)

# Contar el número de docentes únicos por asignatura, plan de estudio y periodo
docentes_por_asignatura <- consolidada_docentes %>%
  group_by(CodigoInterno, Periodo, PlanDeEstudio) %>%
  summarise(NumeroDocentes = n_distinct(IdentificacionDocente), .groups = 'drop')

# Ver los resultados
print(docentes_por_asignatura)

# PRUEBA ----------------------

docentes_por_periodo_ty  <- consolidada_docentes %>%
  filter(PlanDeEstudio %in% c("TECNOLOGÍA EN DESARROLLO DE SOFTWARE (Virtual) SNIES 103168  - 20142", 
                              "TECNOLOGÍA EN DESARROLLO DE SOFTWARE (Presencial) SNIES 102570  - 20141")) %>%
  group_by(Periodo) %>%
  summarise(TotalDocentes = n_distinct(IdentificacionDocente), .groups = 'drop')
  
# EJERCICIO DE MAYORIA ------------------

# 1- Agrupar por 'CodigoInterno', 'Periodo', y 'PlanDeEstudio', y contar los estudiantes en cada grupo.
# 2- Determinar qué plan de estudio tiene la mayoría de estudiantes para cada 'CodigoInterno' y 'Periodo'.
# 3- Filtrar los docentes según el plan de estudio mayoritario por asignatura y periodo.
# 4- Contar los docentes únicos que enseñan en cada plan de estudio mayoritario por periodo.

# Cargar la librería dplyr si no está cargada
library(dplyr)

library(dplyr)

# Paso 1: Contar estudiantes por CodigoInterno, Periodo, PlanDeEstudio manteniendo las columnas adicionales
estudiantes_por_grupo <- consolidada_docentes %>%
  group_by(CodigoInterno, Periodo, PlanDeEstudio) %>%
  summarise(NumeroEstudiantes = n(),
            Asignatura = first(Asignatura),
            Grupo = first(Grupo),
            Jornada.x = first(Jornada.x),
            Departamento = first(Departamento),
            fecha = first(fecha),
            .groups = 'drop')

# Asegurarse de que las columnas adicionales están presentes
print(colnames(estudiantes_por_grupo))

# Paso 2: Determinar el PlanDeEstudio mayoritario por CodigoInterno y Periodo
mayoria_plan <- estudiantes_por_grupo %>%
  group_by(CodigoInterno, Periodo) %>%
  slice_max(NumeroEstudiantes, n = 1) %>%
  ungroup()

# Asegurarse de que las columnas adicionales están presentes después de slice_max
print(colnames(mayoria_plan))

# Paso 3: Filtrar los docentes según el plan mayoritario, manteniendo las columnas adicionales
docentes_mayoria_plan <- consolidada_docentes %>%
  inner_join(mayoria_plan, by = c("CodigoInterno", "Periodo", "PlanDeEstudio"))

# Verificar que todas las columnas necesarias estén después del join
print(colnames(docentes_mayoria_plan))

# Paso 4: Contar los docentes únicos que enseñan en cada plan de estudio mayoritario por periodo y añadir las columnas
docentes_por_periodo_por_mayoria <- docentes_mayoria_plan %>%
  group_by(Periodo, PlanDeEstudio) %>%
  summarise(TotalDocentes = n_distinct(IdentificacionDocente),
            Asignatura = first(Asignatura.x), 
            CodigoInterno = first(CodigoInterno), 
            Grupo = first(Grupo.x), 
            Jornada.x = first(Jornada.x.x), 
            Departamento = first(Departamento.x), 
            fecha = first(fecha.x),
            .groups = 'drop')

# Verificar que las columnas adicionales están presentes en el resultado final
print(colnames(docentes_por_periodo_por_mayoria))

# Ver los resultados
print(docentes_por_periodo_por_mayoria)
