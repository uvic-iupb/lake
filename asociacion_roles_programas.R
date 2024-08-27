# Cargar las librerías necesarias
library(dplyr)
library(stringr)
library(readxl)
library(tidyr)

# Cargar las bases de datos desde archivos Excel
db2 <- read_excel("/Users/cristianespinal/Desktop/Marco Nacional de Cualificaciones/programas_snies.xlsx")
db1 <- read_excel("/Users/cristianespinal/Desktop/Marco Nacional de Cualificaciones/MNC.xlsx")

# Normalizar los datos (convertir a minúsculas y quitar espacios innecesarios)
db1 <- db1 %>% mutate(bloque = tolower(str_trim(bloque)))
db2 <- db2 %>% mutate(programa = tolower(str_trim(programa)))

# Definir una función para encontrar afinidades
# Esta es una función básica que busca coincidencias directas de palabras clave. Deberás ajustarla a tus necesidades.
encontrar_afinidad <- function(bloque, programa) {
  if(str_detect(programa, bloque)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Aplicar la función para encontrar programas relacionados para cada bloque
# Esto creará una lista de programas relacionados para cada entrada en db1
db1$programas_relacionados <- sapply(db1$bloque, function(bloque) {
  programas <- db2$programa[sapply(db2$programa, encontrar_afinidad, bloque = bloque)]
  if(length(programas) == 0) return(NA)
  paste(programas, collapse = ", ")
})

# Especifica la ruta y el nombre del archivo de salida
ruta_archivo_salida <- "/Users/cristianespinal/Desktop/Marco Nacional de Cualificaciones/programas_por_roles.xlsx"

# Escribir el DataFrame a un archivo Excel
write.xlsx(db1, ruta_archivo_salida)

# Ver los resultados
print(db1)

