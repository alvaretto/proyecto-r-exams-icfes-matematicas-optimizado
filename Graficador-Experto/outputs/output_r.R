#!/usr/bin/env Rscript
# Gráfica de poblaciones de 5 países (1960-2013)
# Generado por Graficador-Experto

library(ggplot2)
library(scales)

# Datos de los países
anos <- c(1960, 1965, 1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2013)

# Crear dataframe con todos los países
datos <- data.frame(
  Ano = rep(anos, 5),
  Poblacion = c(
    # País 1 - Crecimiento desde ~19M a ~41M
    19e6, 21e6, 23.5e6, 26e6, 28.5e6, 31e6, 33.5e6, 36e6, 38e6, 39.5e6, 40.5e6, 41e6,
    # País 2 - Crecimiento desde ~22M a ~38M
    22e6, 24e6, 26.5e6, 29e6, 30e6, 31.5e6, 33e6, 35e6, 36.5e6, 37e6, 37.5e6, 38e6,
    # País 3 - Crecimiento desde ~20M a ~38M
    20e6, 22e6, 24e6, 26e6, 28e6, 30e6, 32e6, 34e6, 35.5e6, 36.5e6, 37.5e6, 38e6,
    # País 4 - Mayor crecimiento desde ~31M a ~47M
    31e6, 33e6, 35e6, 37e6, 38.5e6, 40e6, 41.5e6, 43e6, 44.5e6, 45.5e6, 46.5e6, 47e6,
    # País 5 - Relativamente estable desde ~30M a ~38M
    30e6, 30.5e6, 31e6, 31.5e6, 32e6, 33e6, 34.5e6, 36e6, 37e6, 37.5e6, 38e6, 38e6
  ),
  Pais = factor(rep(c("País 1", "País 2", "País 3", "País 4", "País 5"), each = 12),
                levels = c("País 1", "País 2", "País 3", "País 4", "País 5"))
)

# Colores según paleta identificada
colores <- c("País 1" = "#00CED1", "País 2" = "#000000", "País 3" = "#8B4513",
             "País 4" = "#00CED1", "País 5" = "#FF8C00")

# Tipos de línea
tipos_linea <- c("País 1" = "dotted", "País 2" = "dashed", "País 3" = "solid",
                 "País 4" = "solid", "País 5" = "solid")

# Formas de marcadores (16=círculo, 15=cuadrado, 17=triángulo, NA=ninguno)
formas <- c("País 1" = NA, "País 2" = 15, "País 3" = NA, "País 4" = 17, "País 5" = 16)

# Crear subsets para cada país (para controlar marcadores individualmente)
pais1_data <- subset(datos, Pais == "País 1")
pais2_data <- subset(datos, Pais == "País 2")
pais3_data <- subset(datos, Pais == "País 3")
pais4_data <- subset(datos, Pais == "País 4")
pais5_data <- subset(datos, Pais == "País 5")

# Función para formatear números en eje Y
formato_poblacion <- function(x) {
  paste0(format(x / 1e6, nsmall = 0, big.mark = "."), ".000.000")
}

# Crear gráfica
p <- ggplot() +
  # País 1 - Línea punteada cyan (sin marcadores)
  geom_line(data = pais1_data, aes(x = Ano, y = Poblacion, color = "País 1"),
            linetype = "dotted", linewidth = 1) +

  # País 2 - Línea discontinua negra con marcadores cuadrados
  geom_line(data = pais2_data, aes(x = Ano, y = Poblacion, color = "País 2"),
            linetype = "dashed", linewidth = 1) +
  geom_point(data = pais2_data[seq(1, nrow(pais2_data), 2), ],
             aes(x = Ano, y = Poblacion, color = "País 2"), shape = 15, size = 3) +

  # País 3 - Línea sólida marrón (sin marcadores)
  geom_line(data = pais3_data, aes(x = Ano, y = Poblacion, color = "País 3"),
            linetype = "solid", linewidth = 1) +

  # País 4 - Línea sólida cyan con marcadores triangulares
  geom_line(data = pais4_data, aes(x = Ano, y = Poblacion, color = "País 4"),
            linetype = "solid", linewidth = 1) +
  geom_point(data = pais4_data[seq(1, nrow(pais4_data), 2), ],
             aes(x = Ano, y = Poblacion, color = "País 4"), shape = 17, size = 3) +

  # País 5 - Línea sólida naranja con marcadores circulares
  geom_line(data = pais5_data, aes(x = Ano, y = Poblacion, color = "País 5"),
            linetype = "solid", linewidth = 1) +
  geom_point(data = pais5_data[seq(1, nrow(pais5_data), 2), ],
             aes(x = Ano, y = Poblacion, color = "País 5"), shape = 16, size = 3) +

  # Escala de colores manual
  scale_color_manual(values = colores, name = NULL) +

  # Configuración de ejes
  scale_x_continuous(breaks = anos, limits = c(1960, 2013)) +
  scale_y_continuous(breaks = seq(15e6, 45e6, 5e6),
                     labels = formato_poblacion,
                     limits = c(15e6, 48e6)) +

  # Etiquetas
  labs(x = "Año", y = "Población") +

  # Tema
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "#CCCCCC", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 11),
    legend.position = "right",
    legend.background = element_blank(),
    legend.key = element_blank()
  )

# Guardar gráfica
ggsave("output_r.png", plot = p, width = 10, height = 6, dpi = 150, bg = "white")
ggsave("output_r.pdf", plot = p, width = 10, height = 6, bg = "white")

cat("Gráfica guardada en output_r.png y output_r.pdf\n")
