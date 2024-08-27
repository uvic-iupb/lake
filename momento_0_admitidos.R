
# Subset_admitidos

str(ListadoDeAdmitidos_20242)

# Cargar dplyr para usar select
library(dplyr)

# Crear el subset de datos con las variables especificadas
subset_carac <- ListadoDeAdmitidos_20242 %>% select(
  Apellido, SegundoApellido, Nombre, SegundoNombre, TipoDeIdentificacion, Identificacion, 
  FechaDeExpedicion, CiudadDeExpedicion, Telefono, Celular, CiudadDeResidencia, 
  Direccion, FechaNacimiento, Edad, CiudadDeNacimiento, Email, Programa, 
  IcfesMatematicas, IcfesFisica, IcfesLenguaje, IcfesLecturaCritica, Estrato, Etnia, 
  TieneAccesoAInternetDesdeResidencia, EsVictimaConflictoArmado, 
  EsVictimaDesplazamientoForzado, SabeOfimatica, TieneHabitosDeLectura, 
  TipoDeVinculoLaboral, TipoViolenciasDeGenero, TiempoSinEstudiar, EsMigrante, 
  ConsumeDrogas, EsEstadoDeGestacion, TrabajaPara,
)

# Mostrar las primeras filas del subset
head(subset_carac)

# Función para imprimir valores únicos de cada columna
print_unique_values <- function(df) {
  for (col in names(df)) {
    cat("Valores únicos de", col, ":\n")
    print(unique(df[[col]]))
    cat("\n")
  }
}

# Aplicar la función al subset_carac
print_unique_values(subset_carac)

# Transformar las variables según la condición especificada y reemplazar NA por 0
subset_carac <- subset_carac %>%
  mutate(
    IcfesMatematicas = ifelse(is.na(IcfesMatematicas) | IcfesMatematicas == "" | IcfesMatematicas == " ", 0, ifelse(IcfesMatematicas <= 40, 1, 0)),
    IcfesFisica = ifelse(is.na(IcfesFisica) | IcfesFisica == "" | IcfesFisica == " ", 0, ifelse(IcfesFisica <= 40, 1, 0)),
    IcfesLenguaje = ifelse(is.na(IcfesLenguaje) | IcfesLenguaje == "" | IcfesLenguaje == " ", 0, ifelse(IcfesLenguaje <= 40, 1, 0)),
    IcfesLecturaCritica = ifelse(is.na(IcfesLecturaCritica) | IcfesLecturaCritica == "" | IcfesLecturaCritica == " ", 0, ifelse(IcfesLecturaCritica <= 40, 1, 0)),
    Estrato = ifelse(is.na(Estrato) | Estrato == "" | Estrato == " ", 0, ifelse(Estrato == 1 | Estrato == 2, 1, 0)),
    Etnia = ifelse(is.na(Etnia) | Etnia == "" | Etnia == " ", 0, ifelse(Etnia %in% c("Afrodescendiente", "Indígena", "Raizal"), 1, 0)),
    EsVictimaDesplazamientoForzado = ifelse(is.na(EsVictimaDesplazamientoForzado) | EsVictimaDesplazamientoForzado == "" | EsVictimaDesplazamientoForzado == " ", 0, ifelse(EsVictimaDesplazamientoForzado == "Sí", 1, 0)),
    SabeOfimatica = ifelse(is.na(SabeOfimatica) | SabeOfimatica == "" | SabeOfimatica == " ", 0, ifelse(SabeOfimatica == "No", 1, 0)),
    TieneHabitosDeLectura = ifelse(is.na(TieneHabitosDeLectura) | TieneHabitosDeLectura == "" | TieneHabitosDeLectura == " ", 0, ifelse(TieneHabitosDeLectura == "No", 1, 0)),
    TipoDeVinculoLaboral = ifelse(is.na(TipoDeVinculoLaboral) | TipoDeVinculoLaboral == "" | TipoDeVinculoLaboral == " ", 0, ifelse(TipoDeVinculoLaboral %in% c("Informal ( Sin contrato, sin cumplimiento de horarios, por horas, trabajo familiar, sin prestaciones)", "Formal ( Contrato laboral definido, cumplimiento horario, con prestaciones )", "Independiente ( negocio propio, manejo del tiempo, emprendimiento )"), 1, 0)),
    TipoViolenciasDeGenero = ifelse(is.na(TipoViolenciasDeGenero) | TipoViolenciasDeGenero == "" | TipoViolenciasDeGenero == " ", 0, ifelse(TipoViolenciasDeGenero %in% c("Violencia Psicológica", "Violencia Laboral", "Violencia Sexual", "Violencia Económica", "Violencia Física", "Otra"), 1, 0)),
    TiempoSinEstudiar = ifelse(is.na(TiempoSinEstudiar) | TiempoSinEstudiar == "" | TiempoSinEstudiar == " ", 0, ifelse(TiempoSinEstudiar %in% c("Entre 2 y 4 años", "Más de 4 años"), 1, 0)),
    EsMigrante = ifelse(is.na(EsMigrante) | EsMigrante == "" | EsMigrante == " ", 0, ifelse(EsMigrante == "Sí", 1, 0)),
    ConsumeDrogas = ifelse(is.na(ConsumeDrogas) | ConsumeDrogas == "" | ConsumeDrogas == " ", 0, ifelse(ConsumeDrogas %in% c("A veces", "SI"), 1, 0)),
    EsEstadoDeGestacion = ifelse(is.na(EsEstadoDeGestacion) | EsEstadoDeGestacion == "" | EsEstadoDeGestacion == " ", 0, ifelse(EsEstadoDeGestacion == "Sí", 1, 0)),
    TrabajaPara = ifelse(is.na(TrabajaPara) | TrabajaPara == "" | TrabajaPara == " ", 0, ifelse(TrabajaPara %in% c("Para ayudar en la casa", "Para pagar la matrícula", "Para pagar deudas"), 1, 0)),
    TieneAccesoAInternetDesdeResidencia = ifelse(is.na(TieneAccesoAInternetDesdeResidencia) | TieneAccesoAInternetDesdeResidencia == "" | TieneAccesoAInternetDesdeResidencia == " " | TieneAccesoAInternetDesdeResidencia == "Si", 0, 1),
    EsVictimaConflictoArmado = ifelse(is.na(EsVictimaConflictoArmado) | EsVictimaConflictoArmado == "" | EsVictimaConflictoArmado == " " | EsVictimaConflictoArmado == "No", 0, 1)
  )

# Cargar dplyr para usar mutate y count
library(dplyr)

# Crear una columna con el número de variables críticas
subset_carac <- subset_carac %>%
  mutate(
    num_variables_criticas = IcfesMatematicas + IcfesFisica + IcfesLenguaje + IcfesLecturaCritica + Estrato + Etnia + 
      EsVictimaDesplazamientoForzado + SabeOfimatica + TieneHabitosDeLectura + TipoDeVinculoLaboral + 
      TipoViolenciasDeGenero + TiempoSinEstudiar + EsMigrante + ConsumeDrogas + EsEstadoDeGestacion
  )

# Crear la tabla que cuenta los estudiantes según el número de variables críticas
tabla_variables_criticas <- subset_carac %>%
  group_by(num_variables_criticas) %>%
  summarise(
    NumeroDeEstudiantes = n(),
    IcfesMatematicas = sum(IcfesMatematicas, na.rm = TRUE),
    IcfesFisica = sum(IcfesFisica, na.rm = TRUE),
    IcfesLenguaje = sum(IcfesLenguaje, na.rm = TRUE),
    IcfesLecturaCritica = sum(IcfesLecturaCritica, na.rm = TRUE),
    Estrato = sum(Estrato, na.rm = TRUE),
    Etnia = sum(Etnia, na.rm = TRUE),
    EsVictimaDesplazamientoForzado = sum(EsVictimaDesplazamientoForzado, na.rm = TRUE),
    SabeOfimatica = sum(SabeOfimatica, na.rm = TRUE),
    TieneHabitosDeLectura = sum(TieneHabitosDeLectura, na.rm = TRUE),
    TipoDeVinculoLaboral = sum(TipoDeVinculoLaboral, na.rm = TRUE),
    TipoViolenciasDeGenero = sum(TipoViolenciasDeGenero, na.rm = TRUE),
    TiempoSinEstudiar = sum(TiempoSinEstudiar, na.rm = TRUE),
    EsMigrante = sum(EsMigrante, na.rm = TRUE),
    ConsumeDrogas = sum(ConsumeDrogas, na.rm = TRUE),
    EsEstadoDeGestacion = sum(EsEstadoDeGestacion, na.rm = TRUE)
  )

# Mostrar la tabla
tabla_variables_criticas

guardar_datos(tabla_variables_criticas, path_base, "tabla_variables_criticas")
guardar_datos(subset_carac, path_base, "subset_carac")
