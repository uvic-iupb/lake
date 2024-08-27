

load(file = "../DATA CONSOLIDADA/listado_notas.rdata")
load(file = "../DATA CONSOLIDADA/matriculados_totales.rdata")

library(dlookr)
library(tidyverse)

ETL_DIR <- function(direccion){
  require(stringi)
  direccion <- toupper(direccion)
  direccion <- gsub(pattern = "[^[:graph:]]", replacement = " ", direccion) # remove non-printable characters
  direccion <- gsub(pattern = "[^[:print:]]", replacement = " ", direccion) # remove non-printable characters
  direccion <- gsub(pattern = "[^[:alnum:] ]", replacement = " ", direccion)
  direccion <- stri_trans_general(direccion, "Latin-ASCII")
  direccion <- iconv(direccion, to='UTF-8')
  #direccion <- gsub(pattern = "[[:punct:]]", replacement = " ", direccion) # quito puntuacin
  direccion <- str_trim(direccion, side = "both")
  direccion <- str_squish(direccion)
  
  
  
  return(direccion)
  
}


colegio_matricula <-
  matriculados_totales %>% 
  select(identificacion, colegio) %>% 
  filter(!is.na(colegio)) %>% 
  distinct() %>% 
  group_by(identificacion) %>% 
  filter(row_number()==1) %>% 
  ungroup()


notas_fundamentacion <-
  listado_notas %>% 
  filter(departamento_academico=="FUNDAMENTACIÓN") %>% 
  filter(porcentaje_evaluado ==100) %>% 
  mutate(nombre_asignatura= ETL_DIR(nombre_asignatura))


union_base <-
  notas_fundamentacion %>% 
  inner_join(colegio_matricula) %>% 
  group_by(colegio, nombre_asignatura) %>% 
  reframe(nota_promedio=mean(nota_parcial_acumulada, na.rm = T),
            mediana=median(nota_parcial_acumulada, na.rm=T) ,
            desdest=sd(nota_parcial_acumulada, na.rm=T), casos=n()) %>% 
  mutate(
    type = case_when(
      str_detect(nombre_asignatura, "INGLES 1|INGLES 2|INGLES 3")~ "Segundo_idioma",
      str_detect(nombre_asignatura, "DIFERENCI|INTEGRAL|MATEMATI|LINEAL")~ "Matematicas",
      str_detect(nombre_asignatura, "MATERNA|COMUNICATI")~ "Lengua",
      .default = "other"
    )
  ) %>% 
  filter(type != "other") %>% 
  group_by(colegio, type) %>% 
  reframe(indicador=round(mean(nota_promedio),1)) %>% 
  pivot_wider(names_from = type, values_from = indicador) %>% 
  drop_na() %>% 
  mutate(bin_segundoidioma=binning(Segundo_idioma,5,type = "kmeans", ordered = T)) %>% 
  mutate(bin_Matematicas=binning(Matematicas,5,type = "kmeans", ordered = T)) %>% 
  mutate(bin_lengua=binning(Lengua,5,type = "kmeans", ordered = T)) %>% 
  mutate(lbin_segundoidioma=binning(Segundo_idioma,5,type = "kmeans", ordered = T, labels=c("Bajo", "Medio bajo", "Medio", "Medio alto", "Alto"))) %>% 
  mutate(bin_Matematicas=binning(Matematicas,5,type = "kmeans", ordered = T,  labels=c("Bajo", "Medio bajo", "Medio", "Medio alto", "Alto"))) %>% 
  mutate(bin_lengua=binning(Lengua,5,type = "kmeans", ordered = T, labels=c("Bajo", "Medio bajo", "Medio", "Medio alto", "Alto")) )


openxlsx::write.xlsx(union_base, file = "../Salidas_resultados/cluster_colegio.xlsx")



load(file = "../DATA CONSOLIDADA/Aspirantes.rdata")





aspirantes_20241 <-
  aspirantes %>% 
  filter(periodo=="20241")
