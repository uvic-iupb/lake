# Conteo único de docentes por Plan de Estudio

# Cargar
load("/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/DATA CONSOLIDADA/consolidada_docentes_por_programa.rdata")

library(dplyr)
library(writexl)
library(lubridate)
library(stringr)

resultado <- consolidada_docentes %>%
  distinct(PlanDeEstudio, IdentificacionDocente, .keep_all = TRUE) %>%
  group_by(PlanDeEstudio) %>%
  summarise(NumeroUnicoDocentes = n_distinct(IdentificacionDocente)) %>%
  left_join(consolidada_docentes %>%
              select(PlanDeEstudio, Departamento, Asignatura, CodigoInterno, NombreDocente, ApellidoDocente, IdentificacionDocente, Archivo.x),
            by = "PlanDeEstudio")

resultado_distinct <- distinct(resultado_distinct)

# Estandarizar plan de estudio

resultado_distinct <- resultado_distinct %>%
  mutate(
    PlanDeEstudio = sub(" - .*", "", PlanDeEstudio)
  )

# Crear fecha a partir de Archivo.x

resultado_distinct <- resultado_distinct %>%
  mutate(
    Año = as.integer(substr(Archivo.x, 4, 7)),  # Extraer el año
    Semestre = as.integer(substr(Archivo.x, 8, 8)),  # Extraer el número de semestre
    Fecha = case_when(
      Semestre == 1 ~ ymd(paste0(Año, "-01-01")),  # Asumir que el semestre 1 comienza el 1 de enero
      Semestre == 2 ~ ymd(paste0(Año, "-07-01")),  # Asumir que el semestre 2 comienza el 1 de julio
      TRUE ~ NA_Date_  # En caso de cualquier otro número, asignar NA
    )
  )

# Extraer código SNIES

resultado_distinct <- resultado_distinct %>%
  mutate(
    CodigoSNIES = str_extract(PlanDeEstudio, "(?<=SNIES )\\d+")
  )

# Ruta para guardar el archivo Excel
ruta_excel <- "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/DATA CONSOLIDADA/conteo_unico_docentes.xlsx"
write.xlsx(resultado_distinct, file = ruta_excel)

# Ruta para guardar el archivo .Rdata
ruta_rdata <- "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/DATA CONSOLIDADA/conteo_unico_docentes.rdata"
save(resultado, file = ruta_rdata)
