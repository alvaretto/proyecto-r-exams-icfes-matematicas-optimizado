#!/usr/bin/env Rscript
# REPLICACIÓN FIEL DE LA IMAGEN DE COTANGENTE
# Genera gráfico R que replica exactamente la imagen compartida
# Función: Distancia PK vs Ángulo α

# Verificar e instalar paquetes si es necesario
if (!require(ggplot2, quietly = TRUE)) {
  install.packages("ggplot2", repos = "https://cran.r-project.org")
  library(ggplot2)
}

if (!require(dplyr, quietly = TRUE)) {
  install.packages("dplyr", repos = "https://cran.r-project.org")
  library(dplyr)
}

# Función para generar datos de cotangente con discontinuidades
generar_datos_cotangente <- function() {
  
  cat("🎯 GENERANDO DATOS PARA REPLICAR IMAGEN\n")
  cat("=" * 40, "\n")
  
  # Parámetros basados en análisis visual de la imagen
  x_min <- 0
  x_max <- 4.5
  y_min <- 0
  y_max <- 5
  
  # Discontinuidades aproximadas (observadas en la imagen)
  disc1 <- pi/2  # ≈ 1.57 (primera discontinuidad)
  disc2 <- pi    # ≈ 3.14 (segunda discontinuidad)
  
  # SEGMENTO 1: Curva principal que baja dramáticamente
  # Desde x=0.1 hasta antes de la primera discontinuidad
  x1 <- seq(0.1, disc1 - 0.1, length.out = 50)
  y1 <- sapply(x1, function(x) {
    cot_val <- 1/tan(x)
    # Limitar valores extremos
    if (cot_val > y_max) return(y_max)
    if (cot_val < y_min) return(y_min)
    return(cot_val)
  })
  
  segmento1 <- data.frame(
    x = x1,
    y = y1,
    segmento = "1"
  )
  
  # SEGMENTO 2: Curva después de la discontinuidad
  # Desde después de π/2 hasta antes de π
  x2 <- seq(disc1 + 0.1, disc2 - 0.1, length.out = 40)
  y2 <- sapply(x2, function(x) {
    cot_val <- 1/tan(x)
    # Filtrar solo valores positivos y dentro del rango
    if (cot_val > 0 && cot_val <= y_max) {
      return(cot_val)
    } else {
      return(NA)
    }
  })
  
  # Filtrar NAs
  validos2 <- !is.na(y2)
  segmento2 <- data.frame(
    x = x2[validos2],
    y = y2[validos2],
    segmento = "2"
  )
  
  # SEGMENTO 3: Curva final (después de π)
  x3 <- seq(disc2 + 0.1, x_max, length.out = 30)
  y3 <- sapply(x3, function(x) {
    cot_val <- 1/tan(x)
    # Limitar valores
    if (cot_val > y_max) return(y_max)
    if (cot_val < y_min) return(y_min)
    return(cot_val)
  })
  
  segmento3 <- data.frame(
    x = x3,
    y = y3,
    segmento = "3"
  )
  
  # Combinar todos los segmentos
  datos_completos <- rbind(segmento1, segmento2, segmento3)
  
  cat("✅ Segmento 1:", nrow(segmento1), "puntos\n")
  cat("✅ Segmento 2:", nrow(segmento2), "puntos\n") 
  cat("✅ Segmento 3:", nrow(segmento3), "puntos\n")
  cat("📊 Total:", nrow(datos_completos), "puntos\n")
  
  return(datos_completos)
}

# Generar los datos
datos_cotangente <- generar_datos_cotangente()

# Crear el gráfico que replica la imagen
grafico_replica <- ggplot(datos_cotangente, aes(x = x, y = y)) +
  
  # Curva principal en cyan/azul claro (como en la imagen)
  geom_line(color = "#00BFFF", size = 2, alpha = 0.9) +
  
  # Líneas punteadas verticales (QP e I₁)
  geom_vline(xintercept = 1.5, linetype = "dashed", color = "black", size = 0.8) +
  geom_vline(xintercept = 3.0, linetype = "dashed", color = "black", size = 0.8) +
  
  # Etiquetas de las líneas punteadas
  annotate("text", x = 1.5, y = 4.8, label = "QP", size = 4, fontface = "bold") +
  annotate("text", x = 3.0, y = 4.8, label = expression(I[1]), size = 4, fontface = "bold") +
  
  # Configuración de ejes (replicando la imagen)
  scale_x_continuous(
    name = expression(paste("Ángulo ", alpha)),
    limits = c(0, 4.5),
    expand = c(0, 0)
  ) +
  
  scale_y_continuous(
    name = "Distancia PK",
    limits = c(0, 5),
    expand = c(0, 0)
  ) +
  
  # Tema que replica el estilo de la imagen
  theme_minimal() +
  theme(
    # Ejes solo en left y bottom (como en la imagen)
    panel.border = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.8, arrow = arrow(length = unit(0.3, "cm"))),
    axis.line.y = element_line(color = "black", size = 0.8, arrow = arrow(length = unit(0.3, "cm"))),
    
    # Texto de ejes
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10), angle = 90),
    
    # Sin grid (la imagen no tiene grid visible)
    panel.grid = element_blank(),
    
    # Fondo blanco
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    
    # Sin ticks en los ejes (como en la imagen)
    axis.ticks = element_blank(),
    axis.text = element_blank()
  )

# Mostrar el gráfico
print(grafico_replica)

# Guardar el gráfico
ggsave(
  filename = "replica_cotangente_imagen.png",
  plot = grafico_replica,
  width = 8,
  height = 6,
  dpi = 300,
  bg = "white"
)

cat("\n🎨 GRÁFICO GENERADO EXITOSAMENTE\n")
cat("📁 Archivo guardado: replica_cotangente_imagen.png\n")
cat("📏 Dimensiones: 8x6 pulgadas, 300 DPI\n")
cat("🎯 Replica fiel de la imagen original\n")

# Información adicional sobre la función
cat("\n📊 INFORMACIÓN DE LA FUNCIÓN:\n")
cat("• Función: Cotangente cot(α) = 1/tan(α)\n")
cat("• Discontinuidades en: π/2 ≈ 1.57, π ≈ 3.14\n")
cat("• Rango X: [0, 4.5]\n")
cat("• Rango Y: [0, 5]\n")
cat("• Color curva: Cyan (#00BFFF)\n")
cat("• Líneas punteadas en x = 1.5 (QP) y x = 3.0 (I₁)\n")
