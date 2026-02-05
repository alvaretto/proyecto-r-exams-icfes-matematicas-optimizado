# Cargar librerías
library(ggplot2)
library(ggpubr)

# Datos de la tabla
datos <- data.frame(
  Tiempo = c(0, 2, 4, 6, 8),
  Combustible = c(55, 45, 35, 25, 15),
  Distancia = c(0, 20, 40, 60, 80)
)

# Crear la gráfica de doble escala
grafica <- ggplot(datos, aes(x = Tiempo)) +
  geom_line(aes(y = Combustible, color = "Combustible (litros)"), size = 1.2) +
  geom_point(aes(y = Combustible, color = "Combustible (litros)"), size = 3) +
  geom_line(aes(y = Distancia, color = "Distancia (km)"), size = 1.2) +
  geom_point(aes(y = Distancia, color = "Distancia (km)"), size = 3) +
  scale_y_continuous(
    name = "Combustible (litros)",
    sec.axis = sec_axis(~ ., name = "Distancia (km)")
  ) +
  labs(
    x = "Tiempo (horas)",
    color = "Leyenda",
    title = "Gráfica combinada: Combustible y Distancia vs Tiempo"
  ) +
  theme_minimal() +
  theme(
    axis.title.y.left = element_text(color = "blue", size = 12),
    axis.title.y.right = element_text(color = "red", size = 12),
    legend.position = "top"
  )

# Mostrar la gráfica
print(grafica)
