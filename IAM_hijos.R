
# IAM_hijos > 0

# Filtrar la base de datos
IAM_hijos <- IAM %>%
  filter(!is.na(numero_de_hijos) & numero_de_hijos > 0)

# Verificar la nueva base de datos filtrada
guardar_datos(IAM_hijos, path_base, "IAM_hijos")

IAM_hijos <- IAM_hijos %>%
  select(file, ruta, numero_de_hijos, everything())
