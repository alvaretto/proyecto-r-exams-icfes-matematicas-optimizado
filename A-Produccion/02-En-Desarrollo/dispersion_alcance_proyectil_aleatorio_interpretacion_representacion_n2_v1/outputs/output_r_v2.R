#!/usr/bin/env Rscript
# Graficador Experto - Dispersión Alcance Proyectil - v2
# Iteración para alcanzar >=95% similitud con imagen ICFES original

library(ggplot2)

# Configurar semilla para reproducibilidad
set.seed(42)

# Parámetros físicos ajustados
g <- 9.8    # gravedad (m/s^2)
v0 <- 11.3  # velocidad inicial ajustada para alcance max ~13m
n_puntos <- 99  # número exacto de lanzamientos

# Generar ángulos uniformemente distribuidos
angulos <- runif(n_puntos, 0.05, 1.55)

# Calcular alcance teórico
alcance_teorico <- (v0^2 * sin(2 * angulos)) / g

# Ruido proporcional al alcance
ruido_base <- 0.42
ruido <- rnorm(n_puntos, 0, ruido_base * sqrt(pmax(0.5, alcance_teorico)))
alcance_observado <- alcance_teorico + ruido
alcance_observado <- pmax(0.3, pmin(13.8, alcance_observado))

# Crear data frame
datos <- data.frame(
  angulo = angulos,
  alcance = alcance_observado
)

# Crear gráfico replicando imagen original exactamente
p <- ggplot(datos, aes(x = angulo, y = alcance)) +
  geom_point(
    color = "#00CED1",  # cyan turquesa exacto
    shape = 18,         # diamante
    size = 3.2,         # tamaño ajustado
    alpha = 0.9
  ) +
  scale_x_continuous(
    name = "Ángulo (en radianes)",
    breaks = seq(0, 1.6, 0.2),
    limits = c(0, 1.7),
    expand = c(0, 0.02)
  ) +
  scale_y_continuous(
    name = "Alcance horizontal (m)",
    breaks = seq(0, 14, 2),
    limits = c(0, 15),
    expand = c(0, 0.02)
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_line(color = "gray80", linewidth = 0.5),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    axis.line = element_line(color = "black", linewidth = 0.5)
  )

# Guardar figura
ggsave("r_output_v2.png", plot = p, width = 9, height = 6, dpi = 150)

cat("Gráfico v2 generado: r_output_v2.png\n")
