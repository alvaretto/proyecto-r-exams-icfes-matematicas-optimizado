#!/usr/bin/env Rscript
# Gráfica de Poblaciones de 5 Países (1960-2013)
# Ejercicio ICFES - Generado con ggplot2

library(ggplot2)
library(scales)

# Datos de población por país
años <- c(1960, 1965, 1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2013)

# Crear dataframe con todos los datos
datos <- data.frame(
  año = rep(años, 5),
  poblacion = c(
    # País 1 - Crecimiento muy pronunciado
    20e6, 23e6, 26e6, 29.5e6, 33e6, 36e6, 38.5e6, 40.5e6, 42e6, 43.5e6, 45e6, 47e6,
    # País 2 - Crecimiento moderado (cruza País 5 en ~1986)
    20e6, 22e6, 24e6, 26e6, 28.5e6, 30e6, 32.5e6, 34.5e6, 36e6, 37e6, 38e6, 38.5e6,
    # País 3 - Crecimiento desde abajo
    15e6, 17e6, 19.5e6, 22.5e6, 26e6, 29e6, 32e6, 34.5e6, 36.5e6, 37.5e6, 38e6, 38.5e6,
    # País 4 - Empieza alto, crecimiento moderado
    31e6, 31.5e6, 32.5e6, 34e6, 36e6, 37.5e6, 39e6, 40e6, 40.5e6, 41.5e6, 42e6, 42.5e6,
    # País 5 - Horizontal hasta 1980, luego crece (cruza País 2 en ~1986)
    30e6, 30e6, 30e6, 30e6, 30e6, 30.5e6, 33e6, 35e6, 36.5e6, 37.5e6, 38e6, 38.5e6
  ),
  pais = factor(rep(c("País 1", "País 2", "País 3", "País 4", "País 5"), each = 12),
                levels = c("País 1", "País 2", "País 3", "País 4", "País 5"))
)

# Crear gráfica
p <- ggplot(datos, aes(x = año, y = poblacion, color = pais, linetype = pais, shape = pais)) +
  # Líneas
  geom_line(linewidth = 1) +
  # Puntos solo para País 4 y País 5
  geom_point(data = subset(datos, pais %in% c("País 4", "País 5")), size = 2.5) +

  # Colores personalizados
  scale_color_manual(values = c(
    "País 1" = "cyan3",
    "País 2" = "black",
    "País 3" = "brown",
    "País 4" = "cyan3",
    "País 5" = "orange"
  )) +

  # Tipos de línea
  scale_linetype_manual(values = c(
    "País 1" = "dotted",
    "País 2" = "dashed",
    "País 3" = "solid",
    "País 4" = "solid",
    "País 5" = "solid"
  )) +

  # Formas de marcadores
  scale_shape_manual(values = c(
    "País 1" = NA,
    "País 2" = NA,
    "País 3" = NA,
    "País 4" = 17,  # triángulo
    "País 5" = 16   # círculo
  )) +

  # Ejes
  scale_x_continuous(
    breaks = años,
    limits = c(1958, 2016)
  ) +
  scale_y_continuous(
    breaks = seq(15e6, 45e6, by = 5e6),
    limits = c(14e6, 48e6),
    labels = function(x) paste0(format(x/1e6, big.mark = ".", decimal.mark = ","), ".000.000")
  ) +

  # Etiquetas
  labs(
    x = "Año",
    y = "Población",
    color = NULL,
    linetype = NULL,
    shape = NULL
  ) +

  # Tema
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray80", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 12),
    legend.position = "right",
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.key.width = unit(1.5, "cm")
  ) +

  # Guías de leyenda
  guides(
    color = guide_legend(override.aes = list(
      linetype = c("dotted", "dashed", "solid", "solid", "solid"),
      shape = c(NA, NA, NA, 17, 16)
    ))
  )

# Guardar
ggsave("output_r.png", plot = p, width = 12, height = 8, dpi = 300, bg = "white")
ggsave("output_r.pdf", plot = p, width = 12, height = 8, bg = "white")

cat("Gráfica guardada en: output_r.png y output_r.pdf\n")
