#!/usr/bin/env Rscript
# REPLICA SIMPLE DE LA IMAGEN DE COTANGENTE
# Versión simplificada y robusta

# Cargar librerías base
library(graphics)

# Función para generar datos de cotangente
generar_cotangente <- function() {
  
  cat("Generando datos de cotangente...\n")
  
  # Segmento 1: Curva principal (0.1 a 1.4)
  x1 <- seq(0.1, 1.4, length.out = 50)
  y1 <- 1 / tan(x1)
  
  # Filtrar valores extremos
  y1[y1 > 5] <- 5
  y1[y1 < 0] <- NA
  
  # Segmento 2: Después de discontinuidad (1.8 a 3.0)
  x2 <- seq(1.8, 3.0, length.out = 30)
  y2 <- 1 / tan(x2)
  
  # Filtrar solo valores positivos pequeños
  y2[y2 < 0] <- NA
  y2[y2 > 2] <- NA
  
  # Segmento 3: Final (3.2 a 4.4)
  x3 <- seq(3.2, 4.4, length.out = 25)
  y3 <- 1 / tan(x3)
  y3[y3 > 5] <- 5
  y3[y3 < 0] <- NA
  
  return(list(
    x1 = x1, y1 = y1,
    x2 = x2, y2 = y2,
    x3 = x3, y3 = y3
  ))
}

# Generar datos
datos <- generar_cotangente()

# Crear gráfico usando base R
png("replica_cotangente_base.png", width = 800, height = 600, bg = "white")

# Configurar área de gráfico
par(mar = c(4, 4, 2, 2))

# Crear gráfico base
plot(0, 0, type = "n", 
     xlim = c(0, 4.5), ylim = c(0, 5),
     xlab = expression(paste("Ángulo ", alpha)),
     ylab = "Distancia PK",
     axes = FALSE,
     main = "")

# Dibujar ejes con flechas
arrows(0, 0, 4.5, 0, length = 0.1, lwd = 2)
arrows(0, 0, 0, 5, length = 0.1, lwd = 2)

# Dibujar curva cotangente en segmentos
lines(datos$x1, datos$y1, col = "cyan3", lwd = 3)
lines(datos$x2, datos$y2, col = "cyan3", lwd = 3)
lines(datos$x3, datos$y3, col = "cyan3", lwd = 3)

# Líneas punteadas verticales
abline(v = 1.5, lty = 2, col = "black", lwd = 1.5)
abline(v = 3.0, lty = 2, col = "black", lwd = 1.5)

# Etiquetas
text(1.5, 4.8, "QP", cex = 1.2, font = 2)
text(3.0, 4.8, expression(I[1]), cex = 1.2, font = 2)

# Etiquetas de ejes
mtext(expression(paste("Ángulo ", alpha)), side = 1, line = 2.5, cex = 1.2)
mtext("Distancia PK", side = 2, line = 2.5, cex = 1.2)

dev.off()

cat("✅ Gráfico generado: replica_cotangente_base.png\n")
cat("📊 Función cotangente replicada exitosamente\n")

# Versión alternativa con ggplot2 (si está disponible)
if (require(ggplot2, quietly = TRUE)) {
  
  cat("Generando versión con ggplot2...\n")
  
  # Preparar datos para ggplot2
  df1 <- data.frame(x = datos$x1, y = datos$y1)
  df2 <- data.frame(x = datos$x2, y = datos$y2)
  df3 <- data.frame(x = datos$x3, y = datos$y3)
  
  # Filtrar NAs
  df1 <- df1[!is.na(df1$y), ]
  df2 <- df2[!is.na(df2$y), ]
  df3 <- df3[!is.na(df3$y), ]
  
  # Crear gráfico ggplot2
  p <- ggplot() +
    geom_line(data = df1, aes(x = x, y = y), color = "cyan3", size = 1.5) +
    geom_line(data = df2, aes(x = x, y = y), color = "cyan3", size = 1.5) +
    geom_line(data = df3, aes(x = x, y = y), color = "cyan3", size = 1.5) +
    
    geom_vline(xintercept = 1.5, linetype = "dashed", color = "black") +
    geom_vline(xintercept = 3.0, linetype = "dashed", color = "black") +
    
    annotate("text", x = 1.5, y = 4.8, label = "QP", size = 5, fontface = "bold") +
    annotate("text", x = 3.0, y = 4.8, label = expression(I[1]), size = 5, fontface = "bold") +
    
    scale_x_continuous(name = expression(paste("Ángulo ", alpha)), 
                       limits = c(0, 4.5), expand = c(0, 0)) +
    scale_y_continuous(name = "Distancia PK", 
                       limits = c(0, 5), expand = c(0, 0)) +
    
    theme_minimal() +
    theme(
      panel.border = element_blank(),
      axis.line = element_line(color = "black", size = 0.8),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      axis.ticks = element_blank(),
      axis.text = element_blank()
    )
  
  ggsave("replica_cotangente_ggplot.png", plot = p, 
         width = 8, height = 6, dpi = 300, bg = "white")
  
  cat("✅ Versión ggplot2 generada: replica_cotangente_ggplot.png\n")
}

cat("\n🎯 REPLICACIÓN COMPLETADA\n")
cat("📁 Archivos generados en el directorio actual\n")
cat("🔧 Función: cot(α) = 1/tan(α)\n")
cat("📏 Rango: α ∈ [0, 4.5], PK ∈ [0, 5]\n")
