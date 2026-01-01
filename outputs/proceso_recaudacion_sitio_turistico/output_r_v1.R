# ============================================================
# Diagrama de Flujo - Proceso de Recaudación Sitio Turístico
# Versión R/ggplot2 v1 - Compatible con R-exams
# ============================================================

library(ggplot2)
library(grid)
library(gridExtra)

generar_diagrama_flujo <- function(precio_reserva = 22.5,
                                    precio_sin_reserva = 17,
                                    output_file = "r_output_v1.png") {

  # Formatear precios con coma decimal (español)
  # Solo mostrar decimal si es necesario
  if (precio_reserva %% 1 == 0) {
    precio_reserva_fmt <- as.character(as.integer(precio_reserva))
  } else {
    precio_reserva_fmt <- gsub("\\.", ",", as.character(precio_reserva))
  }
  if (precio_sin_reserva %% 1 == 0) {
    precio_sin_reserva_fmt <- as.character(as.integer(precio_sin_reserva))
  } else {
    precio_sin_reserva_fmt <- gsub("\\.", ",", as.character(precio_sin_reserva))
  }

  # Colores
  color_caja <- "#EDF7FA"
  color_borde <- "#4A90A4"
  color_circulo <- "#E67E22"
  color_flecha <- "#4A90A4"

  # Crear plot base
  p <- ggplot() +
    xlim(0, 11) +
    ylim(0, 3.5) +
    coord_fixed(ratio = 1) +
    theme_void() +
    theme(plot.margin = margin(10, 10, 10, 10))

  # Posiciones Y
  y_sup <- 2.6
  y_inf <- 1.1
  y_medio <- (y_sup + y_inf) / 2 + 0.15

  # Dimensiones
  ancho_caja1 <- 2.0
  alto_caja <- 0.7
  ancho_caja2 <- 2.3
  ancho_caja3 <- 1.2

  # === CAJAS ===
  # Caja 1 superior
  x1 <- 0.8
  p <- p + annotate("rect", xmin = x1, xmax = x1 + ancho_caja1,
                    ymin = y_sup - alto_caja/2, ymax = y_sup + alto_caja/2,
                    fill = color_caja, color = color_borde, linewidth = 0.8)
  p <- p + annotate("text", x = x1 + ancho_caja1/2, y = y_sup,
                    label = "Sumar la cantidad de\npersonas que entraron con\nreserva durante la semana.",
                    size = 2.3, lineheight = 0.9)

  # Caja 1 inferior
  p <- p + annotate("rect", xmin = x1, xmax = x1 + ancho_caja1,
                    ymin = y_inf - alto_caja/2, ymax = y_inf + alto_caja/2,
                    fill = color_caja, color = color_borde, linewidth = 0.8)
  p <- p + annotate("text", x = x1 + ancho_caja1/2, y = y_inf,
                    label = "Sumar la cantidad de\npersonas que entraron sin\nreserva durante la semana.",
                    size = 2.3, lineheight = 0.9)

  # Caja 2 superior
  x2 <- 3.5
  p <- p + annotate("rect", xmin = x2, xmax = x2 + ancho_caja2,
                    ymin = y_sup - alto_caja/2, ymax = y_sup + alto_caja/2,
                    fill = color_caja, color = color_borde, linewidth = 0.8)
  p <- p + annotate("text", x = x2 + ancho_caja2/2, y = y_sup,
                    label = paste0("Multiplicar la cantidad\nobtenida en el paso    por\n", precio_reserva_fmt, "."),
                    size = 2.3, lineheight = 0.9)

  # Caja 2 inferior
  p <- p + annotate("rect", xmin = x2, xmax = x2 + ancho_caja2,
                    ymin = y_inf - alto_caja/2, ymax = y_inf + alto_caja/2,
                    fill = color_caja, color = color_borde, linewidth = 0.8)
  p <- p + annotate("text", x = x2 + ancho_caja2/2, y = y_inf,
                    label = paste0("Multiplicar la cantidad\nobtenida en el paso    por\n", precio_sin_reserva_fmt, "."),
                    size = 2.3, lineheight = 0.9)

  # Caja 3 (resultado)
  x3 <- 7.8
  p <- p + annotate("rect", xmin = x3, xmax = x3 + ancho_caja3,
                    ymin = y_medio - alto_caja/2, ymax = y_medio + alto_caja/2,
                    fill = color_caja, color = color_borde, linewidth = 0.8)
  p <- p + annotate("text", x = x3 + ancho_caja3/2, y = y_medio,
                    label = "Comparar los\nresultados",
                    size = 2.3, lineheight = 0.9)

  # === CÍRCULOS GRANDES (pasos) ===
  radio <- 0.18

  # Círculo 1 superior
  p <- p + annotate("point", x = 0.45, y = y_sup, size = 8, color = color_circulo)
  p <- p + annotate("text", x = 0.45, y = y_sup, label = "1", color = "white", fontface = "bold", size = 3.5)

  # Círculo 1 inferior
  p <- p + annotate("point", x = 0.45, y = y_inf, size = 8, color = color_circulo)
  p <- p + annotate("text", x = 0.45, y = y_inf, label = "1", color = "white", fontface = "bold", size = 3.5)

  # Círculo 2 superior
  p <- p + annotate("point", x = 3.15, y = y_sup, size = 8, color = color_circulo)
  p <- p + annotate("text", x = 3.15, y = y_sup, label = "2", color = "white", fontface = "bold", size = 3.5)

  # Círculo 2 inferior
  p <- p + annotate("point", x = 3.15, y = y_inf, size = 8, color = color_circulo)
  p <- p + annotate("text", x = 3.15, y = y_inf, label = "2", color = "white", fontface = "bold", size = 3.5)

  # Círculo 3 (posicionado como en TikZ v6)
  p <- p + annotate("point", x = 7.45, y = y_medio - 0.2, size = 8, color = color_circulo)
  p <- p + annotate("text", x = 7.45, y = y_medio - 0.2, label = "3", color = "white", fontface = "bold", size = 3.5)

  # === CÍRCULOS PEQUEÑOS (dentro de cajas 2) ===
  # Círculo pequeño 1 en caja 2 superior - en la segunda línea
  p <- p + annotate("point", x = x2 + ancho_caja2/2 + 0.38, y = y_sup - 0.02, size = 4, color = color_circulo)
  p <- p + annotate("text", x = x2 + ancho_caja2/2 + 0.38, y = y_sup - 0.02, label = "1", color = "white", fontface = "bold", size = 1.8)

  # Círculo pequeño 1 en caja 2 inferior - en la segunda línea
  p <- p + annotate("point", x = x2 + ancho_caja2/2 + 0.38, y = y_inf - 0.02, size = 4, color = color_circulo)
  p <- p + annotate("text", x = x2 + ancho_caja2/2 + 0.38, y = y_inf - 0.02, label = "1", color = "white", fontface = "bold", size = 1.8)

  # === FLECHAS ===
  # Flecha caja1 -> círculo2 (superior)
  p <- p + annotate("segment", x = x1 + ancho_caja1, xend = 2.97, y = y_sup, yend = y_sup,
                    arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
                    color = color_flecha, linewidth = 0.6)

  # Flecha caja1 -> círculo2 (inferior)
  p <- p + annotate("segment", x = x1 + ancho_caja1, xend = 2.97, y = y_inf, yend = y_inf,
                    arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
                    color = color_flecha, linewidth = 0.6)

  # Líneas convergentes desde caja2 hacia caja3
  x_conv <- 6.8

  # Desde caja2 superior
  p <- p + annotate("segment", x = x2 + ancho_caja2, xend = x_conv, y = y_sup, yend = y_sup,
                    color = color_flecha, linewidth = 0.6)
  p <- p + annotate("segment", x = x_conv, xend = x_conv, y = y_sup, yend = y_medio,
                    color = color_flecha, linewidth = 0.6)

  # Desde caja2 inferior
  p <- p + annotate("segment", x = x2 + ancho_caja2, xend = x_conv, y = y_inf, yend = y_inf,
                    color = color_flecha, linewidth = 0.6)
  p <- p + annotate("segment", x = x_conv, xend = x_conv, y = y_inf, yend = y_medio,
                    color = color_flecha, linewidth = 0.6)

  # Flecha final a caja3
  p <- p + annotate("segment", x = x_conv, xend = x3, y = y_medio, yend = y_medio,
                    arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
                    color = color_flecha, linewidth = 0.6)

  # Guardar
  ggsave(output_file, plot = p, width = 11, height = 3.5, dpi = 150, bg = "white")

  return(output_file)
}

# Ejecutar
output <- generar_diagrama_flujo(22.5, 17, "r_output_v1.png")
cat("Diagrama generado:", output, "\n")
