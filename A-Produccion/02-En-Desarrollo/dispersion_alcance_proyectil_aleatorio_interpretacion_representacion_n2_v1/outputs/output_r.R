#!/usr/bin/env Rscript
# Graficador Experto - Dispersión Alcance Proyectil
# Genera scatter plot replicando imagen ICFES original

library(ggplot2)

# Configurar semilla para reproducibilidad
set.seed(42)

# Parámetros físicos
g <- 9.8    # gravedad (m/s^2)
v0 <- 11.5  # velocidad inicial (m/s)
n_puntos <- 99  # número de lanzamientos

# Generar ángulos uniformemente distribuidos
angulos <- runif(n_puntos, 0.05, 1.55)

# Calcular alcance teórico: R = v0^2 * sin(2*theta) / g
alcance_teorico <- (v0^2 * sin(2 * angulos)) / g

# Agregar ruido proporcional al alcance (mayor dispersión en alcances mayores)
ruido_base <- 0.45
ruido <- rnorm(n_puntos, 0, ruido_base * sqrt(alcance_teorico))
alcance_observado <- alcance_teorico + ruido
alcance_observado <- pmax(0.2, alcance_observado)  # Evitar valores negativos

# Crear data frame
datos <- data.frame(
  angulo = angulos,
  alcance = alcance_observado
)

# Crear gráfico con ggplot2
p <- ggplot(datos, aes(x = angulo, y = alcance)) +
  geom_point(
    color = "#00CED1",  # cyan/turquesa
    shape = 18,         # diamante
    size = 3,
    alpha = 0.85
  ) +
  scale_x_continuous(
    name = "Ángulo (en radianes)",
    breaks = seq(0, 1.6, 0.2),
    limits = c(0, 1.7)
  ) +
  scale_y_continuous(
    name = "Alcance horizontal (m)",
    breaks = seq(0, 14, 2),
    limits = c(0, 15)
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray85", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    axis.line = element_line(color = "black", linewidth = 0.5)
  )

# Guardar figura
ggsave("r_output.png", plot = p, width = 10, height = 6.5, dpi = 150)

cat("Gráfico generado: r_output.png\n")
