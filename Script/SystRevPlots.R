### SYSTEMATIC ANALYSIS - AI with invasive species
# Erola Fenollosa

# Cargar las librerías
library(ggplot2)
library(dplyr)

# Importar datos
library(readxl)
inv.ai <- read_excel("C:/Users/zool2620/Dropbox/2024 MSCA/05_WP4_Outreach/lyrCIS-Ecosistemas/Ecosistemas/systRev/savedrecs_ed_1.xlsx", 
                             sheet = "Selected")

# Ordenar grupos
inv.ai$Aplication2 <- factor(inv.ai$Aplication1, levels=c("DETECT", "DETECT AND MODEL", 
             "MODEL" , "PREDICT AND MODEL", "PREDICT", "OTHER"))


# Gráfico 1: Número de artículos por año
ggplot(inv.ai, aes(x = `Publication Year`)) +
  geom_bar() +
  labs(title = "Número de artículos por año",
       x = "Año de publicación",
       y = "Número de artículos") +
  theme_minimal()



# Gráfico 2: Crear el gráfico de barras apiladas
# Agrupar y calcular proporciones
df_prop <- inv.ai %>%
  count(`Publication Year`, Aplication2) %>%
  group_by(`Publication Year`) %>%
  mutate(Prop = n / sum(n))

# Definir la paleta de colores personalizada
colores <- c("DETECT" = "#027381", "DETECT AND MODEL" = "#0EB9CB", 
             "MODEL" = "#ffc23d", "PREDICT" = "#FF7751", "PREDICT AND MODEL" = "#F33829", "OTHER"="#4a0875")



ggplot(df_prop, aes(x = `Publication Year`, y = Prop, fill = Aplication2)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = colores) +
  labs(title = "Proporción de tipos de modelo por año",
       x = "Año de publicación",
       y = "Proporción",
       fill = "Tipo de modelo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Grafico 3: Crear el gráfico de barras apiladas en números absolutos
# Agrupar y contar el número de artículos por año y modelo
df_count_abs <- inv.ai %>%
  count(`Publication Year`, Aplication2) %>%
  rename(Conteo = n)

ggplot(df_count_abs, aes(x = `Publication Year`, y = Conteo, fill = Aplication2)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = colores) +
  labs(title = "Número de artículos por año con proporción de tipos de modelo",
       x = "Año de publicación",
       y = "Número de artículos",
       fill = "Tipo de modelo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Grafico 4: Taxones Treemap
# Instalar y cargar las librerías necesarias
if (!require(treemap)) install.packages("treemap")
library(treemap)
library(RColorBrewer)

# Crear un dataframe resumen que cuente el número de estudios por taxón
summary_taxa <- as.data.frame(table(inv.ai$Taxa2))
colnames(summary_taxa) <- c("Taxon", "Count")

# Crear el treemap
treemap(summary_taxa,
        index = "Taxon",       # Variable categórica
        vSize = "Count",       # Variable de tamaño (número de estudios)
        type = "index",        # Tipo de color (index usa colores distintos por categoría)
        title = "Treemap de Estudios por Taxón",
        fontsize.labels = 12,  # Tamaño de las etiquetas
        fontfamily.labels = "Arial",
        border.col = "white",   # Color del borde
        palette = "Set1"
)

# Grafico 5: Taxones Treemap con colores personalizados por categoria
# Crear un dataframe con las categorías y colores específicos
# Crear un dataframe con las categorías y colores específicos
categories <- c("Amphibians", "Nematoda", "Cnidaria", "Fungus", "Bird", 
                "Mammals", "Virtual", "Aquatic plant", "Mollusca", 
                "Protists/Algae", "Multiple", "Fish", "Arthropoda", "Plant")
colors <- c(
  "#6A9955", # Amphibians: Verde musgo
  "#F4D03F", # Nematoda: Amarillo pastel (más claro)
  "#AED6F1", # Cnidaria: Azul claro
  "#6f3d19", # Fungus: Marrón oscuro natural
  "#d0b13e", # Bird: Amarillo pastel
  "#A9A9A9", # Mammals: Gris claro neutro
  "#5D6D7E", # Virtual: Gris azulado
  "#196f68", # Aquatic plant: Verde oscuro
  "#D7BDE2", # Mollusca: Rosa suave
  "#82E0AA", # Protists/Algae: Verde amarillento
  "#6C3483", # Multiple: Púrpura oscuro
  "#0086BF", # Fish: Azul profundo
  "#C0392B", # Arthropoda: Rojo intenso
  "#196F3D"  # Plant: Verde bosque
)


# Crear un dataframe resumen que cuente el número de estudios por taxón
summary_taxa <- as.data.frame(table(inv.ai$Taxa2))
colnames(summary_taxa) <- c("Taxon", "Count")

# Asegurar que los colores estén en el orden correcto para las categorías presentes
summary_taxa$Color <- colors[match(summary_taxa$Taxon, categories)]

# Crear el treemap con colores personalizados
treemap(summary_taxa,
        index = "Taxon",       # Variable categórica
        vSize = "Count",       # Variable de tamaño (número de estudios)
        vColor = "Color",      # Variable de color personalizada
        type = "color",        # Especificar que se usará un color personalizado
        title = "Treemap de Estudios por Taxón",
        fontsize.labels = 12,  # Tamaño de las etiquetas
        fontfamily.labels = "Arial",
        fontcolor.labels = "white",   # Color de las letras negro
        border.col = "white"   # Color del borde
)

# AHORA CON ESCALA CUANTITATIVA
color_scale <- colorRampPalette(brewer.pal(9, "Blues"))(length(summary_taxa$Count))
color_scale <- colorRampPalette(brewer.pal(9, "Greens"))(length(summary_taxa$Count))
color_scale <- colorRampPalette(brewer.pal(9, "Oranges"))(length(summary_taxa$Count))
color_scale <- colorRampPalette(brewer.pal(9, "Purples"))(length(summary_taxa$Count))

# Asociar colores a cada categoría según su cantidad
summary_taxa <- summary_taxa[order(summary_taxa$Count, decreasing = TRUE), ]  # Ordenar por cantidad
summary_taxa$Color <- color_scale


# Crear el treemap con la nueva paleta basada en cantidad
treemap(summary_taxa,
        index = "Taxon",        # Variable categórica
        vSize = "Count",        # Variable de tamaño (número de estudios)
        vColor = "Color",       # Variable de color personalizada
        type = "color",         # Especificar que se usará un color personalizado
        title = "Treemap de Estudios por Taxón (basado en cantidad)",
        fontsize.labels = 12,   # Tamaño de las etiquetas
        fontfamily.labels = "Arial",  # Fuente de las etiquetas
        fontcolor.labels = "black",   # Color de las letras negro
        border.col = "black"    # Color del borde de las categorías
)



# Definir la paleta específica
custom_palette <- c("#027381", "#0EB9CB", "#ffc23d", "#FF7751", "#F33829", "#4a0875")
custom_palette <- c("#4a0875", "#ffc23d","#FF7751","white")

# Crear un gradiente personalizado basado en la paleta invertida
color_scale <- colorRampPalette(custom_palette)(nrow(summary_taxa))

# Asociar los colores generados a cada categoría
summary_taxa <- summary_taxa[order(summary_taxa$Count, decreasing = TRUE), ]  # Ordenar por cantidad
summary_taxa$Color <- color_scale

# Crear el Treemap con la paleta invertida
treemap(summary_taxa,
        index = "Taxon",        # Variable categórica
        vSize = "Count",        # Variable de tamaño (número de estudios)
        vColor = "Color",       # Variable de color personalizada
        type = "color",         # Especificar que se usará un color personalizado
        title = "Treemap de Estudios por Taxón (basado en cantidad)",
        fontsize.labels = 12,   # Tamaño de las etiquetas
        fontfamily.labels = "Arial",  # Fuente de las etiquetas
        fontcolor.labels = "black",   # Color de las letras negro
        border.col = "black"    # Color del borde de las categorías
)

# Grafico 5: Taxones y applicaciones
# Crear un gráfico de barras apiladas
# Filtrar los datos para incluir solo las aplicaciones seleccionadas
# Filtrar los datos y asegurarse de que las variables sean factores ordenados
filtered_data <- inv.ai %>%
  filter(Aplication2 %in% c("DETECT", "MODEL", "PREDICT")) %>% # Filtrar aplicaciones
  mutate(
    Aplication2 = factor(Aplication2, levels = c("DETECT", "MODEL", "PREDICT")), # Ordenar aplicaciones
    Taxa2 = factor(Taxa2, levels = names(sort(table(Taxa2), decreasing = TRUE))) # Ordenar taxones por frecuencia
  )

# Ver los primeros datos para asegurarse del filtro
head(filtered_data)

ggplot(data = filtered_data, aes(x = Taxa2, fill = Aplication2)) +
  geom_bar(position = "stack") +
  labs(
    title = "Aplicaciones (DETECT, MODEL, PREDICT) por Taxón",
    x = "Taxón",
    y = "Número de Estudios",
    fill = "Aplicación"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(size = 12)
  )

# Calcular el número total de artículos por taxón
filtered_data_summary <- filtered_data %>%
  group_by(Taxa2) %>%
  summarise(total_articles = n()) %>%
  filter(total_articles > 5)

# Filtrar el conjunto de datos original para incluir solo los taxones seleccionados
filtered_data_filtered <- filtered_data %>%
  filter(Taxa2 %in% filtered_data_summary$Taxa2)

# Crear el gráfico
ggplot(data = filtered_data_filtered, aes(x = Taxa2, fill = Aplication2)) +
  geom_bar(position = "stack") +
  scale_fill_manual(values = c("DETECT" = "#4e79a7", "MODEL" = "#f28e2b", "PREDICT" = "#e15759")) +
  labs(
    title = "Aplicaciones (DETECT, MODEL, PREDICT) por Taxón",
    x = "Taxón",
    y = "Número de Estudios",
    fill = "Aplicación"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(size = 12)
  )

# Crear el gráfico con etiquetas encima de las barras
ggplot(data = filtered_data_filtered, aes(x = Taxa2, fill = Aplication2)) +
  geom_bar(position = "stack") +
  # Añadir etiquetas con el total de artículos
  geom_text(data = filtered_data_summary, aes(x = Taxa2, y = total_articles, label = total_articles),
            vjust = -0.5, inherit.aes = FALSE) +
  scale_fill_manual(values = c("DETECT" = "#4e79a7", "MODEL" = "#f28e2b", "PREDICT" = "#e15759")) +
  labs(
    title = "Aplicaciones (DETECT, MODEL, PREDICT) por Taxón",
    x = "Taxón",
    y = "Número de Estudios",
    fill = "Aplicación"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(size = 12)
  )


#  HEATMAP?
# Calcular el número de artículos por combinación de Taxa2 y Aplication2
heatmap_data <- filtered_data %>%
  filter(Taxa2 %in% filtered_data_summary$Taxa2) %>%
  group_by(Taxa2, Aplication2) %>%
  summarise(count = n(), .groups = "drop")

# Crear el heatmap
ggplot(data = heatmap_data, aes(x = Aplication2, y = Taxa2, fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "#e15759", name = "Número de Estudios") +
  labs(
    title = "Número de Estudios por Taxón y Aplicación",
    x = "Aplicación",
    y = "Taxón"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    text = element_text(size = 12)
  )

# Crear el heatmap con escala logarítmica en el color
ggplot(data = heatmap_data, aes(x = Aplication2, y = Taxa2, fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    low = "#FEFEE3",
    high = "#d62728",
    name = "Número de Estudios\n(escala logarítmica)",
    trans = "log",
    breaks = c(1, 10, 50),
    labels = c("1", "10", "50")
  ) +
  labs(
    title = "Número de Estudios por Taxón y Aplicación (Escala Logarítmica)",
    x = "Aplicación",
    y = "Taxón"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    text = element_text(size = 12)
  )

# Crear el gráfico de puntos con relleno
ggplot(data = heatmap_data, aes(x = Aplication2, y = Taxa2)) +
  geom_point(aes(size = count, fill = count), shape = 21, color = "black", stroke = 0.3) +
  scale_fill_gradient(
    low = "#FEFEE3",  # Color mínimo
    high = "#d62728",  # Color máximo
    name = "Número de Estudios\n(escala logarítmica)",
    trans = "log",
    breaks = c(1, 10, 50, 100),
    labels = c("1", "10", "50", "100")
  ) +
  scale_size_continuous(
    range = c(3, 20),  # Tamaño de puntos
    name = "Número de Estudios",
    trans = "log",
    breaks = c(1, 10, 50, 100),
    labels = c("1", "10", "50", "100")
  ) +
  labs(
    title = "Número de Estudios por Taxón y Aplicación (Escala Logarítmica)",
    x = "Aplicación",
    y = "Taxón"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    text = element_text(size = 12)
  )



# Normalizar los valores dentro de cada taxón
heatmap_data_normalized <- heatmap_data %>%
  group_by(Taxa2) %>%
  mutate(relative_count = count / max(count)) %>%  # Escalar relativo al máximo dentro del taxón
  ungroup()

# Crear el gráfico de puntos con escala de color relativa a cada taxón
ggplot(data = heatmap_data_normalized, aes(x = Aplication2, y = Taxa2)) +
  geom_point(aes(size = count, fill = relative_count), shape = 21, color = "black", stroke = 0.3) +
  scale_fill_gradient(
    low = "#fff2d8",  # Color mínimo
    high = "#027381",  # Color máximo
    name = "Relativo al máximo\npor Taxón"
  ) +
  scale_size_continuous(
    range = c(3, 20),  # Tamaño de puntos
    name = "Número de Estudios",
    trans = "log",
    breaks = c(1, 10, 50),
    labels = c("1", "10", "50")
  ) +
  labs(
    title = "Número de Estudios por Taxón y Aplicación (Escala Relativa)",
    x = "Aplicación",
    y = "Taxón"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    text = element_text(size = 12)
  )

ggplot(data = heatmap_data_normalized, aes(x = Taxa2, y = Aplication2)) +  # Intercambiar x e y
  geom_point(aes(size = count, fill = relative_count), shape = 21, color = "black", stroke = 0.3) +
  scale_fill_gradient(
    low = "#fff2d8",  # Color mínimo
    high = "#027381",  # Color máximo
    name = "Relativo al máximo\npor Taxón"
  ) +
  scale_size_continuous(
    range = c(3, 20),  # Tamaño de puntos
    name = "Número de Estudios",
    trans = "log",
    breaks = c(1, 10, 50),
    labels = c("1", "10", "50")
  ) +
  labs(
    title = "Número de Estudios por Taxón y Aplicación (Escala Relativa)",
    x = "Taxón",       # Actualizar etiquetas de los ejes
    y = "Aplicación"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    text = element_text(size = 12)
  )
