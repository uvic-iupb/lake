
Programas <- read_excel("../RAW DATA/snies_programas/Programas.xlsx")

palabras_diseno <-
  read_excel("../RAW DATA/snies_programas/Programas.xlsx") %>% 
  clean_names() %>% 
  ungroup() %>% 
  relocate(nombre_del_programa, area_de_conocimiento,contains("cine"), contains("snies")) %>% 
  filter(nivel_academico=="Posgrado") %>% 
  filter(estado_programa=="Activo") %>% 
  filter(nivel_de_formacion %in% c( "Maestría", "Especialización universitaria")) %>% 
  filter(cine_f_2013_ac_campo_amplio == "Arte y Humanidades" &
           str_detect(cine_f_2013_ac_campo_detallado, "no clasificadas|rte|audiovisuales|interdisciplinarios")|str_detect(cine_f_2013_ac_campo_detallado, "Mercadotecnia y publicidad") &area_de_conocimiento =="Bellas artes"
         
palabras_clave <- c("DISEÑO", "DESIGN", "ARTE", "ARTES", "CREATIVO", "VISUAL", "GRAFICO", "INDUSTRIAL", "URBANO", "PRODUCTO")

Index <- read_excel("/Users/cristianespinal/Downloads/base_INDEX20240614_v2.xlsx")

# Supongamos que tu dataframe se llama 'Index'
# Normalizar los valores entre 0 y 1
Index <- Index %>%
  mutate(def_IndexOferta_norm = (def_IndexOferta - min(def_IndexOferta)) / (max(def_IndexOferta) - min(def_IndexOferta)),
         def_IndexDemanda_norm = (def_IndexDemanda - min(def_IndexDemanda)) / (max(def_IndexDemanda) - min(def_IndexDemanda)))

# Lista de palabras a incluir en el gráfico
palabras <- c("diseno", "diseño", "ilustracion", "animacion", "audiovisual", 
              "producto", "estetico", "creatividad", "creativo", "comunicacion", 
              "comunicación", "comunicador", "comunicativo", "multimedia", 
              "multimedial", "visual", "arte", "artistico", "ilustracion", 
              "creativo", "creativa", "3d")

# Filtrar el dataframe para incluir solo las palabras especificadas
Index_filtrado <- Index %>% filter(word %in% palabras)

# Crear el diagrama de dispersión de cuadrantes
ggplot(Index_filtrado, aes(x = def_IndexOferta_norm, y = def_IndexDemanda_norm)) +
  geom_point(color = "blue", size = 3) +
  geom_text(aes(label = word), vjust = -1, hjust = 1, size = 3) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  labs(title = "Diagrama de Dispersión de Cuadrantes",
       x = "Índice de Oferta Normalizado",
       y = "Índice de Demanda Normalizado") +
  theme_minimal()

# Cargar librerías necesarias
library(dplyr)
library(ggplot2)

# Supongamos que tu dataframe se llama 'Index'
# Normalizar los valores entre 0 y 1
Index <- Index %>%
  mutate(def_IndexOferta_norm = (def_IndexOferta - min(def_IndexOferta)) / (max(def_IndexOferta) - min(def_IndexOferta)),
         def_IndexDemanda_norm = (def_IndexDemanda - min(def_IndexDemanda)) / (max(def_IndexDemanda) - min(def_IndexDemanda)))

# Lista de palabras relacionadas con diseño
palabras_diseno <- c("diseno", "diseño", "ilustracion", "animacion", "audiovisual", 
                     "producto", "estetico", "creatividad", "creativo", "comunicacion", 
                     "comunicación", "comunicador", "comunicativo", "multimedia", 
                     "multimedial", "visual", "arte", "artistico", "ilustracion", 
                     "creativo", "creativa", "3d")

# Crear una columna para identificar las palabras relacionadas con diseño
Index <- Index %>%
  mutate(categoria = ifelse(word %in% palabras_diseno, "Diseño", "Otras"))

# Crear el diagrama de dispersión de cuadrantes
ggplot(Index, aes(x = def_IndexOferta_norm, y = def_IndexDemanda_norm)) +
  geom_point(aes(color = categoria), size = 3) +
  geom_text_repel(data = subset(Index, categoria == "Diseño"), aes(label = word, color = categoria), 
                  size = 3, box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50', 
                  bg.color = "lightgrey", bg.r = 0.15) +
  geom_text(data = subset(Index, categoria == "Otras"), aes(label = word), color = "grey60", 
            vjust = -1, hjust = 1, size = 2, alpha = 0.2) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  labs(title = "Diagrama de Dispersión de Cuadrantes",
       x = "Índice de Oferta Normalizado",
       y = "Índice de Demanda Normalizado") +
  scale_color_manual(values = c("Diseño" = "blue", "Otras" = "grey60")) +
  theme_minimal()

# Ajuste de la visualización

# Cargar librerías necesarias
library(dplyr)
library(ggplot2)
library(ggrepel)

# Supongamos que tu dataframe se llama 'Index'
# Normalizar los valores entre 0 y 1
Index <- Index %>%
  mutate(def_IndexOferta_norm = (def_IndexOferta - min(def_IndexOferta)) / (max(def_IndexOferta) - min(def_IndexOferta)),
         def_IndexDemanda_norm = (def_IndexDemanda - min(def_IndexDemanda)) / (max(def_IndexDemanda) - min(def_IndexDemanda)))

# Lista de palabras relacionadas con diseño
palabras_diseno <- c("diseno", "diseño", "ilustracion", "animacion", "audiovisual", 
                     "producto", "estetico", "creatividad", "creativo", "comunicacion", 
                     "comunicación", "comunicador", "comunicativo", "multimedia", 
                     "multimedial", "visual", "arte", "artistico", "ilustracion", 
                     "creativo", "creativa", "3d")

# Crear una columna para identificar las palabras relacionadas con diseño
Index <- Index %>%
  mutate(categoria = ifelse(word %in% palabras_diseno, "Diseño", "Otras"))

# Crear el diagrama de dispersión de cuadrantes con transformación logarítmica y jitter
ggplot(Index, aes(x = def_IndexOferta_norm, y = def_IndexDemanda_norm)) +
  geom_jitter(aes(color = categoria), size = 3, width = 0.02, height = 0.02) +
  geom_text_repel(data = subset(Index, categoria == "Diseño"), aes(label = word, color = categoria), 
                  size = 4, box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50', 
                  bg.color = "lightgrey", bg.r = 0.15) +
  geom_text(data = subset(Index, categoria == "Otras"), aes(label = word), color = "grey60", 
            vjust = -1, hjust = 1, size = 3, alpha = 0.5) +
  geom_vline(xintercept = log10(0.5), linetype = "dashed", color = "red") +
  geom_hline(yintercept = log10(0.5), linetype = "dashed", color = "red") +
  scale_x_log10(labels = scales::label_number(scale_cut = scales::cut_si(""))) +
  scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_si(""))) +
  labs(title = "Diagrama de Dispersión de Cuadrantes (Transformación Logarítmica)",
       x = "Índice de Oferta Normalizado (Log)",
       y = "Índice de Demanda Normalizado (Log)") +
  scale_color_manual(values = c("Diseño" = "blue", "Otras" = "grey60")) +
  theme_minimal()

# Visualización con los datos ajustados:

Index <- read_excel("/Users/cristianespinal/Downloads/base_INDEX20240614_v2 (1).xlsx")

# Selección de columnas necesarias
Index_selected <- Index %>% 
  select(stemmed, final, Def_IndexOferta, Def_IndexOferta2, Def_IndexDemanda, Prop.Diseno)

# Normalización de las variables
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

Index_selected <- Index_selected %>%
  mutate(
    `Índice de Oferta` = normalize(Def_IndexOferta),
    `Índice de Demanda` = normalize(Def_IndexDemanda),
    `Índice de Oferta 2` = normalize(Def_IndexOferta2),
    Categoría = ifelse(Prop.Diseno == 1, "Diseño", "Otras"),
    word = ifelse(Prop.Diseno == 1, stemmed, NA)
  )

# Asegurarse de que todos los NA en la columna `Categoría` sean etiquetados como "Otras"
Index_selected$Categoría[is.na(Index_selected$Categoría)] <- "Otras"

# Crear el diagrama de dispersión de cuadrantes para Índice de Demanda vs Índice de Oferta 2
ggplot(Index_selected, aes(x = `Índice de Oferta 2`, y = `Índice de Demanda`)) +
  geom_point(aes(color = Categoría), size = 3) +
  geom_text_repel(data = subset(Index_selected, Categoría == "Diseño"), aes(label = word, color = Categoría), 
                  size = 3, box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50', 
                  bg.color = "lightgrey", bg.r = 0.15) +
  geom_text(data = subset(Index_selected, Categoría == "Otras"), aes(label = word), color = "grey60", 
            vjust = -1, hjust = 1, size = 2, alpha = 0.2) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  labs(title = "Diagrama de Dispersión de Cuadrantes",
       subtitle = "Índice de Demanda vs Índice de Oferta 2",
       x = "Índice de Oferta 2",
       y = "Índice de Demanda") +
  scale_color_manual(values = c("Diseño" = "blue", "Otras" = "grey60")) +
  theme_minimal()

# Crear el diagrama de dispersión de cuadrantes para Índice de Demanda vs Índice de Oferta
ggplot(Index_selected, aes(x = `Índice de Oferta`, y = `Índice de Demanda`)) +
  geom_point(aes(color = Categoría), size = 3) +
  geom_text_repel(data = subset(Index_selected, Categoría == "Diseño"), aes(label = word, color = Categoría), 
                  size = 3, box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50', 
                  bg.color = "lightgrey", bg.r = 0.15) +
  geom_text(data = subset(Index_selected, Categoría == "Otras"), aes(label = word), color = "grey60", 
            vjust = -1, hjust = 1, size = 2, alpha = 0.2) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  labs(title = "Diagrama de Dispersión de Cuadrantes",
       subtitle = "Índice de Demanda vs Índice de Oferta",
       x = "Índice de Oferta",
       y = "Índice de Demanda") +
  scale_color_manual(values = c("Diseño" = "blue", "Otras" = "grey60")) +
  theme_minimal()

########################################################################################################################

# Gráfico de red #########################

library(igraph)
library(ggraph)
library(tidyverse)

Of_nueva <- read_excel("/Users/cristianespinal/Downloads/oferta_sin_escalar.xlsx")
str(Of_nueva)

duplicated_vertices <- any(duplicated(Of_nueva$stemmed))
print(paste("Hay duplicados en los vértices:", duplicated_vertices))

# Remover duplicados si existen
Of_nueva <- Of_nueva %>% distinct(stemmed, .keep_all = TRUE)

# Verificar nuevamente
duplicated_vertices <- any(duplicated(Of_nueva$stemmed))
print(paste("Hay duplicados en los vértices:", duplicated_vertices))

# Verificar si hay espacios en blanco u otros caracteres ocultos
Of_nueva$stemmed <- trimws(Of_nueva$stemmed)

# Generar aristas de manera que todos los nombres coincidan
set.seed(123) # Para reproducibilidad
n <- nrow(Of_nueva)
edges <- tibble(
  from = sample(Of_nueva$stemmed, n, replace = TRUE),
  to = sample(Of_nueva$stemmed, n, replace = TRUE)
)

# Filtrar para evitar auto-bucles (opcional)
edges <- edges %>% filter(from != to)

# Verificar que todos los nombres de las aristas estén en los vértices
all(edges$from %in% Of_nueva$stemmed)
all(edges$to %in% Of_nueva$stemmed)

# Creación del grafo con aristas generadas
graph <- graph_from_data_frame(edges, directed = FALSE, vertices = Of_nueva)

# Añadir atributos a los nodos
V(graph)$name <- V(graph)$name # Los nombres ya están asignados al crear el grafo
V(graph)$size <- Of_nueva$Of_N.programa[match(V(graph)$name, Of_nueva$stemmed)]

# Verificar asignación de atributos
print(V(graph)$name)
print(V(graph)$size)

# Plotear la red utilizando ggraph
ggraph(graph, layout = "fr") + 
  geom_edge_link(alpha = 0.2, color = "gray") + 
  geom_node_point(aes(size = size, color = size)) + 
  geom_node_text(aes(label = name), repel = TRUE, max.overlaps = Inf) + 
  scale_size(range = c(3, 20)) + 
  theme_void()

# Gráfica de burbujas #########################

# Crear gráfico de burbujas
ggplot(Of_nueva, aes(x = stemmed, y = Of_N.programa, size = Of_N.programa)) +
  geom_point(alpha = 0.7) +
  scale_size(range = c(3, 20)) +
  theme_minimal() +
  labs(title = "Gráfico de Burbujas de Nombres y Tamaños de Programas",
       x = "Nombre",
       y = "Número de Programas",
       size = "Tamaño del Programa") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

install.packages("ggplot2")
install.packages("dplyr")
install.packages("wordcloud")
install.packages("tm")

library(ggplot2)
library(dplyr)
library(wordcloud)
library(tm)
library(readxl)

# word_freq_filtered #####

# Leer los datos desde el archivo Excel
Of_nueva <- read_excel("/Users/cristianespinal/Downloads/oferta_sin_escalar.xlsx")

# Filtrar los datos por Prop.Diseno = 1
data_filtered <- Of_nueva %>% filter(Prop.Diseno.Nuevo == 1)

# Crear una tabla de frecuencia de las palabras stemmed con Prop.Diseno = 1
word_freq_filtered <- data_filtered %>%
  group_by(stemmed) %>%
  summarise(freq = sum(Of_N.programa))

# Crear una tabla de frecuencia de las palabras stemmed sin ningún filtro
word_freq_all <- Of_nueva %>%
  group_by(stemmed) %>%
  summarise(freq = sum(Of_N.programa))

# Crear el gráfico de nube de palabras con Prop.Diseno = 1
set.seed(1234) # Para reproducibilidad
wordcloud(words = word_freq_filtered$stemmed, freq = word_freq_filtered$freq, min.freq = 1,
          max.words = 200, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

# Crear el gráfico de nube de palabras sin ningún filtro
set.seed(1234) # Para reproducibilidad
wordcloud(words = word_freq_all$stemmed, freq = word_freq_all$freq, min.freq = 1,
          max.words = 200, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

# Cowplot #########################

# Leer los datos desde el archivo Excel
Of_nueva <- read_excel("/Users/cristianespinal/Downloads/oferta_sin_escalar.xlsx")

# Filtrar los datos por Prop.Diseno = 1
data_filtered <- Of_nueva %>% filter(Prop.Diseno == 1)

# Crear una tabla de frecuencia de las palabras stemmed con filtro
word_freq_filtered <- data_filtered %>%
  group_by(stemmed) %>%
  summarise(freq = sum(Of_N.programa))

# Crear una tabla de frecuencia de las palabras stemmed sin filtro
word_freq_all <- Of_nueva %>%
  group_by(stemmed) %>%
  summarise(freq = sum(Of_N.programa))

# Crear el gráfico de nube de palabras con filtro
set.seed(1234) # Para reproducibilidad
wordcloud_filtered <- wordcloud(words = word_freq_filtered$stemmed, freq = word_freq_filtered$freq, min.freq = 1,
                                max.words = 200, random.order = FALSE, rot.per = 0.35,
                                colors = brewer.pal(8, "Dark2"))

# Crear el gráfico de nube de palabras sin filtro
set.seed(1234) # Para reproducibilidad
wordcloud_all <- wordcloud(words = word_freq_all$stemmed, freq = word_freq_all$freq, min.freq = 1,
                           max.words = 200, random.order = FALSE, rot.per = 0.35,
                           colors = brewer.pal(8, "Set3"))

# Guardar los gráficos de nube de palabras en archivos temporales
png("wordcloud_filtered.png")
wordcloud(words = word_freq_filtered$stemmed, freq = word_freq_filtered$freq, min.freq = 1,
          max.words = 200, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))
dev.off()

png("wordcloud_all.png")
wordcloud(words = word_freq_all$stemmed, freq = word_freq_all$freq, min.freq = 1,
          max.words = 200, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Set3"))
dev.off()

# Cargar las imágenes de nube de palabras
img_filtered <- ggdraw() + draw_image("wordcloud_filtered.png")
img_all <- ggdraw() + draw_image("wordcloud_all.png")

# Combinar los gráficos en una sola imagen
combined_plot <- plot_grid(img_filtered, img_all, labels = c("Diseño", "General"), ncol = 2)

# Mostrar la imagen combinada
print(combined_plot)
print(img_filtered)
print(img_all)

# Gráfico de barras #########################

# Crear el gráfico de barras con filtro
barplot_filtered <- ggplot(data_filtered, aes(x = reorder(stemmed, Of_N.programa), y = Of_N.programa)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Frecuencia de Palabras Stemmed por Tamaño de Programa (Filtro Prop.Diseno = 1)",
       x = "Palabra Stemmed",
       y = "Tamaño del Programa") +
  theme_minimal()

# Crear el gráfico de barras sin filtro
barplot_all <- ggplot(Of_nueva, aes(x = reorder(stemmed, Of_N.programa), y = Of_N.programa)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Frecuencia de Palabras Stemmed por Tamaño de Programa (Sin Filtro)",
       x = "Palabra Stemmed",
       y = "Tamaño del Programa") +
  theme_minimal()

# Mostrar los gráficos
print(barplot_filtered)
print(barplot_all)

# Treemap #########################

# Crear el treemap con filtro
treemap(data_filtered,
        index = "stemmed",
        vSize = "Of_N.programa",
        title = "Treemap de Palabras Stemmed por Tamaño de Programa (Filtro Prop.Diseno = 1)")

# Crear el treemap sin filtro
treemap(Of_nueva,
        index = "stemmed",
        vSize = "Of_N.programa",
        title = "Treemap de Palabras Stemmed por Tamaño de Programa (Sin Filtro)")

# Dispersión ######################### 

# Leer los datos desde el archivo Excel
Of_nueva <- read_excel("/Users/cristianespinal/Downloads/oferta_sin_escalar.xlsx")

# Filtrar los datos por Prop.Diseno = 1
data_filtered <- Of_nueva %>% filter(Prop.Diseno == 1)

# Ordenar los datos por tamaño del programa de mayor a menor
data_filtered <- data_filtered %>% arrange(desc(Of_N.programa))
Of_nueva <- Of_nueva %>% arrange(desc(Of_N.programa))

# Crear el gráfico de dispersión con filtro y ordenado
scatterplot_filtered <- ggplot(data_filtered, aes(x = reorder(stemmed, -Of_N.programa), y = Of_N.programa)) +
  geom_point(color = "steelblue", size = 3) +
  labs(title = "Dispersión de Palabras Stemmed por Tamaño de Programa (Filtro Prop.Diseno = 1)",
       x = "Palabra Stemmed",
       y = "Tamaño del Programa") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()

# Crear el gráfico de dispersión sin filtro y ordenado
scatterplot_all <- ggplot(Of_nueva, aes(x = reorder(stemmed, -Of_N.programa), y = Of_N.programa)) +
  geom_point(color = "darkorange", size = 3) +
  labs(title = "Dispersión de Palabras Stemmed por Tamaño de Programa (Sin Filtro)",
       x = "Palabra Stemmed",
       y = "Tamaño del Programa") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()

# Combinar los gráficos en una sola imagen
combined_plot <- plot_grid(scatterplot_filtered, scatterplot_all, labels = c("A", "B"), ncol = 2)

# Mostrar la imagen combinada
print(combined_plot)
print(scatterplot_filtered)

# Gráfico de Burbuja #########

# Leer los datos desde el archivo Excel
Of_nueva <- read_excel("/Users/cristianespinal/Downloads/oferta_sin_escalar.xlsx")

# Ordenar los datos por tamaño del programa de mayor a menor
Of_nueva <- Of_nueva %>% arrange(desc(Of_N.programa))

# Calcular el punto de corte en el 50% para el tamaño del programa
cutoff <- median(Of_nueva$Of_N.programa)

# Crear el gráfico de burbuja con la línea roja punteada en el 50%
bubbleplot_all <- ggplot(Of_nueva, aes(x = reorder(stemmed, -Of_N.programa), y = Of_N.programa, size = Of_N.programa, color = Of_N.programa)) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = cutoff, linetype = "dashed", color = "red") +
  labs(title = "Gráfico de Burbuja de Palabras Stemmed por Tamaño de Programa",
       x = "Palabra Stemmed",
       y = "Tamaño del Programa") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal() +
  scale_color_gradient(low = "lightgreen", high = "darkgreen")

# Dividir los datos en dos subconjuntos: 50% con más oferta y 50% con menos oferta
top_50 <- Of_nueva %>% filter(Of_N.programa > cutoff)
bottom_50 <- Of_nueva %>% filter(Of_N.programa <= cutoff)

# Crear el gráfico de burbuja para el 50% con más oferta
bubbleplot_top_50 <- ggplot(top_50, aes(x = reorder(stemmed, -Of_N.programa), y = Of_N.programa, size = Of_N.programa, color = Of_N.programa)) +
  geom_point(alpha = 0.7) +
  labs(title = "Gráfico de Burbuja de Palabras Stemmed (Top 50%)",
       x = "Palabra Stemmed",
       y = "Tamaño del Programa") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal() +
  scale_color_gradient(low = "lightblue", high = "blue")

# Crear el gráfico de burbuja para el 50% con menos oferta
bubbleplot_bottom_50 <- ggplot(bottom_50, aes(x = reorder(stemmed, -Of_N.programa), y = Of_N.programa, size = Of_N.programa, color = Of_N.programa)) +
  geom_point(alpha = 0.7) +
  labs(title = "Gráfico de Burbuja de Palabras Stemmed (Bottom 50%)",
       x = "Palabra Stemmed",
       y = "Tamaño del Programa") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal() +
  scale_color_gradient(low = "lightcoral", high = "darkred")

# Combinar los gráficos en una sola imagen
combined_plot <- plot_grid(bubbleplot_top_50, bubbleplot_bottom_50, labels = c("A", "B"), ncol = 2)

# Mostrar la imagen combinada
print(bubbleplot_all)
print(combined_plot)

# Gráfico de red

library(igraph)
library(ggraph)
library(tidyverse)
library(readxl)
library(widyr)

# Crear las combinaciones de palabras
edges <- Of_nueva %>%
  mutate(id = row_number()) %>%
  group_by(id) %>%
  summarise(words = list(stemmed)) %>%
  unnest(words) %>%
  expand_grid(item1 = words, item2 = words) %>%
  filter(item1 != item2) %>%
  distinct(item1, item2)

# Crear el objeto grafo
graph <- graph_from_data_frame(d = edges, directed = FALSE)

# Graficar la red
set.seed(1234)  # Para reproducibilidad
ggraph(graph, layout = 'fr') +
  geom_edge_link(aes(edge_alpha = 0.5), show.legend = FALSE) +
  geom_node_point(color = 'skyblue', size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_void() +
  labs(title = "Gráfico de Red de Palabras Stemmed")

# 

library(ggplot2)
library(dplyr)
library(readxl)
library(cowplot)

# Leer los datos desde el archivo Excel
Of_nueva <- read_excel("/Users/cristianespinal/Downloads/oferta_sin_escalar.xlsx")

# Filtrar los datos donde Prop.Diseno = 1
Of_nueva <- Of_nueva %>% filter(Prop.Diseno == 1)

# Ordenar los datos por tamaño del programa de mayor a menor
Of_nueva <- Of_nueva %>% arrange(desc(Of_N.programa))

# Calcular el punto de corte en el 50% para el tamaño del programa
cutoff <- median(Of_nueva$Of_N.programa)

# Crear el gráfico de burbuja con la línea roja punteada en el 50%
bubbleplot_all <- ggplot(Of_nueva, aes(y = reorder(stemmed, -Of_N.programa), x = Of_N.programa, size = Of_N.programa, color = Of_N.programa)) +
  geom_point(alpha = 0.7) +
  geom_vline(xintercept = cutoff, linetype = "dashed", color = "red") +
  labs(title = "Gráfico de Burbuja de Palabras Stemmed por Tamaño de Programa",
       x = "Tamaño del Programa",
       y = "Palabra Stemmed") +
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
  theme_minimal() +
  scale_color_gradient(low = "lightgreen", high = "darkgreen")

# Dividir los datos en dos subconjuntos: 50% con más oferta y 50% con menos oferta
top_50 <- Of_nueva %>% filter(Of_N.programa > cutoff)
bottom_50 <- Of_nueva %>% filter(Of_N.programa <= cutoff)

# Calcular la media para cada subconjunto
mean_top_50 <- mean(top_50$Of_N.programa)
mean_bottom_50 <- mean(bottom_50$Of_N.programa)

# Crear el gráfico de burbuja para el 50% con más oferta
bubbleplot_top_50 <- ggplot(top_50, aes(y = reorder(stemmed, -Of_N.programa), x = Of_N.programa, size = Of_N.programa, color = Of_N.programa)) +
  geom_point(alpha = 0.7) +
  geom_vline(xintercept = mean_top_50, linetype = "dashed", color = "blue") +
  annotate("text", x = mean_top_50, y = max(top_50$Of_N.programa), label = paste("Media:", round(mean_top_50, 2)), color = "blue") +
  labs(title = "Gráfico de Burbuja (>50%)",
       subtitle = "50% Superior",
       x = "No. de Programas",
       y = "Palabras") +
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
  theme_minimal() +
  scale_color_gradient(low = "lightblue", high = "blue")

# Crear el gráfico de burbuja para el 50% con menos oferta
bubbleplot_bottom_50 <- ggplot(bottom_50, aes(y = reorder(stemmed, -Of_N.programa), x = Of_N.programa, size = Of_N.programa, color = Of_N.programa)) +
  geom_point(alpha = 0.7) +
  geom_vline(xintercept = mean_bottom_50, linetype = "dashed", color = "red") +
  annotate("text", x = mean_bottom_50, y = max(bottom_50$Of_N.programa), label = paste("Media:", round(mean_bottom_50, 2)), color = "red") +
  labs(title = "Gráfico de Burbuja de Palabras (<50%)",
       subtitle = "50% Inferior",
       x = "No. de Programas",
       y = "Palabras") +
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
  theme_minimal() +
  scale_color_gradient(low = "lightcoral", high = "darkred")

# Combinar los gráficos en una sola imagen
combined_plot <- plot_grid(bubbleplot_top_50, bubbleplot_bottom_50, labels = c("A", "B"), ncol = 2)

# Mostrar la imagen combinada
print(bubbleplot_all)
print(combined_plot)

# Nube de palabras #########

library(packcircles)
library(ggplot2)
library(dplyr)
library(readxl)
library(viridis)

# Leer los datos desde el archivo Excel
Of_nueva <- read_excel("/Users/cristianespinal/Downloads/oferta_sin_escalar.xlsx")

# Filtrar los datos por Prop.Diseno = 1
data_filtered <- Of_nueva %>% filter(Prop.Diseno == 1)

# Crear una tabla de frecuencia de las palabras stemmed con Prop.Diseno = 1
word_freq_filtered <- data_filtered %>%
  group_by(stemmed) %>%
  summarise(freq = sum(Of_N.programa))

# Algoritmo de empaquetamiento circular
packing <- circleProgressiveLayout(word_freq_filtered$freq, sizetype = "area")

# Agregar la información de las posiciones al dataframe
word_freq_filtered <- cbind(word_freq_filtered, packing)

# Generar el diagrama
ggplot() + 
  geom_polygon(data = circleLayoutVertices(packing), aes(x, y, group = id, fill = as.factor(id)), colour = "black", alpha = 0.6) +
  geom_text(data = word_freq_filtered, aes(x, y, label = stemmed), size = 3, color = "black") +
  scale_fill_viridis_d() +
  theme_void() +
  theme(legend.position = "none")


# Leer los datos desde el archivo Excel
Of_nueva <- read_excel("/Users/cristianespinal/Downloads/oferta_sin_escalar.xlsx")

# Filtrar los datos por Prop.Diseno = 1
data_filtered <- Of_nueva %>% filter(Prop.Diseno == 1)

# Crear una tabla de frecuencia de las palabras stemmed con Prop.Diseno = 1
word_freq_filtered <- data_filtered %>%
  group_by(stemmed) %>%
  summarise(freq = sum(Of_N.programa), N_programa = sum(Of_N.programa))

# Algoritmo de empaquetamiento circular
packing <- circleProgressiveLayout(word_freq_filtered$freq, sizetype = "area")

# Agregar la información de las posiciones al dataframe
word_freq_filtered <- cbind(word_freq_filtered, packing)

# Generar el diagrama
ggplot() + 
  geom_polygon(data = circleLayoutVertices(packing), aes(x, y, group = id, fill = as.factor(id)), colour = "black", alpha = 0.6) +
  geom_text(data = word_freq_filtered, aes(x, y, label = stemmed), size = 3, color = "black", vjust = -1) +
  geom_text(data = word_freq_filtered, aes(x, y, label = paste("N =", N_programa)), size = 3, color = "black", vjust = 1.5) +
  scale_fill_viridis_d() +
  theme_void() +
  theme(legend.position = "none")
