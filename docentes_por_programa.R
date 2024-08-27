# Instalar y cargar librerías necesarias
install.packages("tidyverse")
install.packages("readxl")
library(tidyverse)
library(readxl)

# Rutas de los archivos Excel
ruta_base <- "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/Docentes por programa/"
archivo_listado_clases <- paste0(ruta_base, "consolidada_listado_de_clases.xlsx")
archivo_carga_academica <- paste0(ruta_base, "consolidada_carga_academica.xlsx")

# Leer las bases de datos desde los archivos Excel
listado_clases <- read_excel(archivo_listado_clases, col_types = "text")
carga_academica <- read_excel(archivo_carga_academica, col_types = "text")

# Realizar la unión de las bases de datos
consolidada_docentes <- left_join(listado_clases, carga_academica,
                                  by = c("CodigoInterno" = "NumeroDeGrupo"))

# Escribir la base de datos consolidada en un nuevo archivo Excel
ruta_salida <- paste0(ruta_base, "SALIDA_CONSOLIDADA/consolidada_docentes_por_programa.xlsx")
write_excel_csv(consolidada_docentes, file = ruta_salida, col_names = TRUE)
save(consolidada_docentes, file = "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/Docentes por programa//SALIDA_CONSOLIDADA/consolidada_docentes_por_programa.rdata")

# Confirmar la finalización del proceso
print("Proceso completado. La base de datos consolidada se ha exportado correctamente.")
