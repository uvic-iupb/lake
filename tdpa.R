install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")

library(dplyr)
library(lubridate)
library(ggplot2)

load("/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/DATA CONSOLIDADA/desertores.rdata")
load("/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/DATA CONSOLIDADA/matriculados_nuevos.rdata")

# Asumiendo que 'periodo' es la columna que indica la cohorte
# Calcular el total de desertores por cohorte
total_desertores <- desertores %>%
  count(periodo) %>%
  rename(total_desertores = n)

# Calcular el total de nuevos matriculados (primíparos) por cohorte
total_primiparos <- matricula_nuevos %>%
  count(periodo) %>%
  rename(total_primiparos = n)

# Combinar los totales por cohorte
tdpa <- total_desertores %>%
  left_join(total_primiparos, by = "periodo")

# Calcular la TDPA
tdpa <- tdpa %>%
  mutate(TDPA = (total_desertores / total_primiparos) * 100)

# Revisar los resultados
print(tdpa)

library(writexl)
write_xlsx(tdpa, path = "/Users/cristianespinal/Downloads/TDPA_results.xlsx")

