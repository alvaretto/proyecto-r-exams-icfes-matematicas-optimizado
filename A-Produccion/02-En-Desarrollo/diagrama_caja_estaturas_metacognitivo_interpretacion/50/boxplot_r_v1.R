#!/usr/bin/env Rscript
# Boxplot estilo ICFES - Versión 1 (R/ggplot2)
# Escala izquierda, etiquetas derecha con líneas de conexión

library(ggplot2)

# Parámetros del boxplot (Opción B del original)
minval <- 155
q1 <- 158
median_val <- 165
q3 <- 171
maxval <- 172

# Crear dataframe para el boxplot manual
df <- data.frame(
  x = 1,
  ymin = minval,
  lower = q1,
  middle = median_val,
  upper = q3,
  ymax = maxval
)

# Crear el gráfico
p <- ggplot(df, aes(x = x)) +
  # Boxplot manual
  geom_boxplot(
    aes(ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax),
    stat = "identity",
    width = 0.5,
    fill = "white",
    color = "black",
    linewidth = 0.6
  ) +
  # Líneas de conexión hacia etiquetas
  annotate("segment", x = 1.25, xend = 1.8, y = minval, yend = minval, linewidth = 0.3) +
  annotate("segment", x = 1.25, xend = 1.8, y = q1, yend = q1, linewidth = 0.3) +
  annotate("segment", x = 1.25, xend = 1.8, y = median_val, yend = median_val, linewidth = 0.3) +
  annotate("segment", x = 1.25, xend = 1.8, y = q3, yend = q3, linewidth = 0.3) +
  annotate("segment", x = 1.25, xend = 1.8, y = maxval, yend = maxval, linewidth = 0.3) +
  # Etiquetas a la derecha
  annotate("text", x = 1.85, y = minval, label = "Mín.", hjust = 0, size = 3) +
  annotate("text", x = 1.85, y = q1, label = "Q1", hjust = 0, size = 3) +
  annotate("text", x = 1.85, y = median_val, label = "Q2", hjust = 0, size = 3) +
  annotate("text", x = 1.85, y = q3, label = "Q3", hjust = 0, size = 3) +
  annotate("text", x = 1.85, y = maxval, label = "Máx.", hjust = 0, size = 3) +
  # Escala Y completa
  scale_y_continuous(
    limits = c(154.5, 172.5),
    breaks = 155:172,
    expand = c(0, 0)
  ) +
  scale_x_continuous(limits = c(0.3, 2.1)) +
  # Tema
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_text(size = 8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.margin = margin(5, 5, 5, 5)
  )

# Guardar
ggsave("boxplot_r_v1.png", plot = p, width = 3, height = 5, dpi = 150, bg = "white")
cat("R v1 generado\n")
