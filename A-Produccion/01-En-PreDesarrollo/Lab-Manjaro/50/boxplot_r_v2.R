#!/usr/bin/env Rscript
# Boxplot estilo ICFES - Versión 2 (R/ggplot2)
# Ajustes finos para mejor fidelidad

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
    width = 0.45,
    fill = "white",
    color = "black",
    linewidth = 0.5
  ) +
  # Líneas de conexión hacia etiquetas (más finas)
  annotate("segment", x = 1.225, xend = 1.7, y = minval, yend = minval, linewidth = 0.25) +
  annotate("segment", x = 1.225, xend = 1.7, y = q1, yend = q1, linewidth = 0.25) +
  annotate("segment", x = 1.225, xend = 1.7, y = median_val, yend = median_val, linewidth = 0.25) +
  annotate("segment", x = 1.225, xend = 1.7, y = q3, yend = q3, linewidth = 0.25) +
  annotate("segment", x = 1.225, xend = 1.7, y = maxval, yend = maxval, linewidth = 0.25) +
  # Etiquetas a la derecha
  annotate("text", x = 1.72, y = minval, label = "Mín.", hjust = 0, size = 2.8) +
  annotate("text", x = 1.72, y = q1, label = "Q1", hjust = 0, size = 2.8) +
  annotate("text", x = 1.72, y = median_val, label = "Q2", hjust = 0, size = 2.8) +
  annotate("text", x = 1.72, y = q3, label = "Q3", hjust = 0, size = 2.8) +
  annotate("text", x = 1.72, y = maxval, label = "Máx.", hjust = 0, size = 2.8) +
  # Escala Y completa
  scale_y_continuous(
    limits = c(154.5, 172.5),
    breaks = 155:172,
    expand = c(0, 0)
  ) +
  scale_x_continuous(limits = c(0.4, 2)) +
  # Tema
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_text(size = 7, margin = margin(r = 2)),
    axis.ticks.y = element_line(linewidth = 0.3),
    axis.ticks.length.y = unit(2, "pt"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    plot.margin = margin(8, 8, 8, 8)
  )

# Guardar
ggsave("boxplot_r_v2.png", plot = p, width = 2.8, height = 4.5, dpi = 150, bg = "white")
cat("R v2 generado\n")
