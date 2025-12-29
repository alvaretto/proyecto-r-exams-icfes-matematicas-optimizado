#!/usr/bin/env Rscript
# Gráfico de Población por País (1960-2013)
# Generado automáticamente para reproducir imagen ICFES

library(ggplot2)
library(scales)

# Datos de los países
# País 1 - Cyan punteada, crecimiento lineal de 20M a 43M
pais1 <- data.frame(
  año = c(1960, 1965, 1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2013),
  poblacion = c(20e6, 24e6, 27.5e6, 30e6, 32e6, 35e6, 37.5e6, 39e6, 40.5e6, 41.5e6, 42.5e6, 43e6),
  pais = "País 1"
)

# País 2 - Negra discontinua
pais2 <- data.frame(
  año = c(1960, 1965, 1970, 1975, 1980, 1985, 1990, 1995, 1998, 2000, 2005, 2010, 2013),
  poblacion = c(22e6, 23.5e6, 25e6, 27e6, 29e6, 31.5e6, 34e6, 36e6, 37e6, 37.5e6, 38.5e6, 39.5e6, 40e6),
  pais = "País 2"
)

# País 3 - Naranja/marrón sólida, crecimiento continuo de 15M a 38M
pais3 <- data.frame(
  año = c(1960, 1965, 1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2013),
  poblacion = c(15e6, 17e6, 19e6, 22e6, 25e6, 27e6, 29e6, 31e6, 33e6, 35e6, 37e6, 38e6),
  pais = "País 3"
)

# País 4 - Azul sólida con triángulos
pais4 <- data.frame(
  año = c(1960, 1965, 1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2013),
  poblacion = c(30e6, 31.5e6, 33e6, 35e6, 37e6, 39e6, 41e6, 42.5e6, 44e6, 45e6, 46e6, 47e6),
  pais = "País 4"
)

# País 5 - Naranja sólida con círculos
pais5 <- data.frame(
  año = c(1960, 1965, 1970, 1975, 1980, 1985, 1990, 1995, 1998, 2000, 2005, 2010, 2013),
  poblacion = c(30e6, 30.5e6, 31e6, 32e6, 33e6, 34e6, 35e6, 36e6, 37e6, 37.5e6, 38e6, 38e6, 38.5e6),
  pais = "País 5"
)

# Combinar todos los datos
datos <- rbind(pais1, pais2, pais3, pais4, pais5)

# Colores según paleta identificada
colores <- c(
  "País 1" = "#00BFFF",  # Cyan
  "País 2" = "#000000",  # Negro
  "País 3" = "#CC6600",  # Naranja/marrón
  "País 4" = "#0066CC",  # Azul
  "País 5" = "#FF9900"   # Naranja
)

# Estilos de línea
linetypes <- c(
  "País 1" = "dotted",
  "País 2" = "dashed",
  "País 3" = "solid",
  "País 4" = "solid",
  "País 5" = "solid"
)

# Crear gráfico base
p <- ggplot() +
  # País 1 - Línea punteada cyan (sin marcadores)
  geom_line(data = pais1, aes(x = año, y = poblacion, color = pais, linetype = pais),
            linewidth = 1) +
  # País 2 - Línea discontinua negra (sin marcadores)
  geom_line(data = pais2, aes(x = año, y = poblacion, color = pais, linetype = pais),
            linewidth = 1) +
  # País 3 - Línea sólida naranja/marrón (sin marcadores)
  geom_line(data = pais3, aes(x = año, y = poblacion, color = pais, linetype = pais),
            linewidth = 1) +
  # País 4 - Línea sólida azul CON triángulos
  geom_line(data = pais4, aes(x = año, y = poblacion, color = pais, linetype = pais),
            linewidth = 1) +
  geom_point(data = pais4, aes(x = año, y = poblacion, color = pais),
             shape = 17, size = 3) +  # Triángulo relleno
  # País 5 - Línea sólida naranja CON círculos
  geom_line(data = pais5, aes(x = año, y = poblacion, color = pais, linetype = pais),
            linewidth = 1) +
  geom_point(data = pais5, aes(x = año, y = poblacion, color = pais),
             shape = 19, size = 2.5) +  # Círculo relleno

  # Escalas
  scale_color_manual(values = colores, name = NULL) +
  scale_linetype_manual(values = linetypes, name = NULL) +
  scale_x_continuous(
    breaks = c(1960, 1965, 1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2013),
    limits = c(1960, 2013)
  ) +
  scale_y_continuous(
    breaks = c(15e6, 20e6, 25e6, 30e6, 35e6, 40e6, 45e6),
    labels = function(x) format(x, big.mark = ".", scientific = FALSE),
    limits = c(15e6, 47e6)
  ) +

  # Etiquetas
  labs(x = "Año", y = "Población") +

  # Tema
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "#CCCCCC", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.line = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 10),
    legend.position = "right",
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.key.width = unit(1.5, "cm"),
    legend.text = element_text(size = 9),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  guides(
    color = guide_legend(order = 1, override.aes = list(linewidth = 1)),
    linetype = guide_legend(order = 1)
  )

# Guardar como PDF y PNG
ggsave("r_final.pdf", plot = p, width = 9, height = 6, dpi = 150)
ggsave("r_final.png", plot = p, width = 9, height = 6, dpi = 150)

cat("Gráfico R generado exitosamente:\n")
cat("  - r_final.pdf\n")
cat("  - r_final.png\n")
