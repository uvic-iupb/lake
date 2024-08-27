library(tidyverse)
library(tidytext)
library(readxl)
library(janitor)
library(SnowballC)

load(file = "../RAW DATA/snies_compilados/pc_matricula.rdata")

limpieza_colnames <- function(colname){
  require(stringi)
  temp <- colname %>% 
    gsub("_|-", " ", .) %>%              # Reemplazar guiones bajos por espacios
    gsub("[[:cntrl:]]", "", .) %>%     # Eliminar caracteres de control
    stringi::stri_trans_general (id = "Latin-ASCII") %>% 
    # base::iconv (to="ASCII//TRANSLIT") %>% 
    # iconv(., "UTF-8", "ASCII//TRANSLIT") %>%  # Eliminar acentos
    gsub("\\s+", " ", .) %>%           # Eliminar espacios adicionales
    gsub("[[:punct:]]", "", .) %>%     # Eliminar puntuación
    tolower(.) %>%                     # Convertir a minúsculas
    str_trim(.) %>%                    # Eliminar espacios en blanco al inicio y al final
    gsub("^$|^_$", "unnamed_column", .) # Reemplazar nombres vacíos o "_" con "unnamed_column"
  
  return(temp)
}

programas <-
  read_excel("../../../../../6. Lago de datos/RAW DATA/snies_programas/Programas.xlsx") %>% 
  clean_names() 

install.packages("stopwords")
library(stopwords)

# Obtener las palabras de parada en español
stopwords_es = data.frame(word=stopwords(language = "es", source = "nltk") )
  
palabras <-
  programas %>% 
  select(codigo_snies_del_programa, nombre_del_programa) %>% 
  distinct() %>% 
  mutate(pro=map(.x=nombre_del_programa, ~limpieza_colnames(.x))) %>% 
  unnest_tokens(output = word, input = pro) %>% 
  anti_join(stopwords_es)

palabras2 <- palabras %>% 
  mutate(oe = wordStem(word, language = "en"))

la_primera <-
  palabras2 %>% 
  group_by(oe, word) %>%
  mutate(nv=n()) %>% 
  group_by(oe) %>% 
  mutate(nveces_stem=n()) %>% 
  filter(nv==max(nv)) %>% 
  filter(row_number()==1) 

programas_campo_mapli0 <-
  programas %>% 
  filter(cine_f_2013_ac_campo_amplio=="Arte y Humanidades") %>% 
  filter(nivel_academico=="Posgrado") %>% 
  filter(estado_programa=="Activo")

matricula <-
  pc_matricula %>% 
  filter(ano==2022, semestre==1) %>% 
  filter(codigo_snies_del_programa %in% programas_campo_mapli0$codigo_snies_del_programa) %>% 
  group_by(codigo_snies_del_programa) %>% 
  reframe(cantidad=sum(cantidad)) %>% 
  inner_join(palabras2) 

palabras_dentro_artes <-
  palabras2 %>% 
  filter(codigo_snies_del_programa %in% programas_campo_mapli0$codigo_snies_del_programa) %>% 
  group_by(oe, word) %>%
  mutate(nv=n()) %>% 
  group_by(oe) %>% 
  mutate(nveces_stem=n()) %>% 
  filter(nv==max(nv)) %>% 
  filter(row_number()==1) 

quantile(matiruulas2$nveces_stem, c(0.25, .5, .75,.9 ))

matiruulas2 <-
  matricula %>% 
  group_by(oe) %>% 
  reframe(cantidad=sum(cantidad)) %>% 
  arrange(desc(cantidad)) %>% 
  left_join(palabras_dentro_artes) %>% 
  rename(raiz_palabra=oe, Nro.maricula=cantidad) %>% 
  mutate(ratio=Nro.maricula/nveces_stem) %>% 
  arrange(desc(ratio))

openxlsx::write.xlsx(matiruulas2, "Palbras_posgrado_artesyhumanidadesv2.xlsx")
  