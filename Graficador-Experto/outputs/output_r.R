#!/usr/bin/env Rscript
# Gráfico de Poblaciones de 5 Países (1960-2013)
# Generado por Graficador-Experto - Listo para chunk R-exams

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# Datos
años <- c(1960, 1965, 1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2013)

datos <- data.frame(
  año = años,
  `País 1` = c(30, 31.5, 33, 34.5, 36, 37.5, 38.5, 39.5, 40.5, 41.5, 42, 42.5) * 1e6,
  `País 2` = c(22, 23.5, 25, 27, 29, 30.5, 32, 34, 35.5, 36.5, 37.5, 38) * 1e6,
  `País 3` = c(30, 30.5, 31, 31.5, 32, 33, 34, 35, 36, 37, 37.5, 38) * 1e6,
  `País 4` = c(30.5, 32, 33.5, 35, 37, 38.5, 40, 42, 44, 45.5, 46.5, 47) * 1e6,
  `País 5` = c(30, 29.5, 29, 28.5, 29, 30, 31.5, 33, 35, 36.5, 37.5, 38) * 1e6,
  check.names = FALSE
)

# Convertir a formato largo
datos_largo <- datos %>%
  pivot_longer(cols = -año, names_to = "país", values_to = "población") %>%
  mutate(país = factor(país, levels = c("País 1", "País 2", "País 3", "País 4", "País 5")))

# Colores y estilos según original
colores <- c(
  "País 1" = "#00CED1",
  "País 2" = "#000000",
  "País 3" = "#CD853F",
  "País 4" = "#00CED1",
  "País 5" = "#FF8C00"
)

linetypes <- c(
  "País 1" = "dotted",
  "País 2" = "dashed",
  "País 3" = "solid",
  "País 4" = "solid",
  "País 5" = "solid"
)

# Crear gráfico
p <- ggplot(datos_largo, aes(x = año, y = población, color = país, linetype = país)) +
  geom_line(size = 1.2) +
  
  # Marcadores para País 4 (triángulos) y País 5 (círculos)
  geom_point(
    data = filter(datos_largo, país == "País 4"),
    aes(x = año, y = población),
    shape = 17, size = 3, show.legend = FALSE
  ) +
  geom_point(
    data = filter(datos_largo, país == "País 5"),
    aes(x = año, y = población),
    shape = 16, size = 2.5, show.legend = FALSE
  ) +
  
  # Escalas
  scale_color_manual(values = colores) +
  scale_linetype_manual(values = linetypes) +
  scale_x_continuous(
    breaks = años,
    limits = c(1960, 2013)
  ) +
  scale_y_continuous(
    breaks = seq(15e6, 45e6, by = 5e6),
    labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
    limits = c(15e6, 48e6)
  ) +
  
  # Etiquetas
  labs(x = "Año", y = "Población", color = NULL, linetype = NULL) +
  
  # Tema
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray80", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.5, "cm")
  ) +
  guides(color = guide_legend(override.aes = list(size = 1.5)))

# Guardar
ggsave(
  "renders/render_r.png",
  plot = p,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "white"
)

cat("Gráfico R guardado en: renders/render_r.png\n")
