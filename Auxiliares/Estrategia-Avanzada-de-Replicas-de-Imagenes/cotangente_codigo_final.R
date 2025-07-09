# CÓDIGO R FINAL PARA REPLICAR LA IMAGEN DE COTANGENTE
# Genera datos y código para replicar fielmente la imagen compartida

cat("🎯 GENERADOR DE CÓDIGO COTANGENTE\n")
cat("================================\n")

# Función para generar datos de cotangente
generar_datos_cotangente <- function() {
  
  # Parámetros de la imagen
  x_min <- 0
  x_max <- 4.5
  y_min <- 0
  y_max <- 5
  
  # Discontinuidades observadas en la imagen
  disc1 <- pi/2  # ≈ 1.57
  disc2 <- pi    # ≈ 3.14
  
  cat("📊 Generando segmentos de cotangente...\n")
  
  # SEGMENTO 1: Curva principal descendente (0.1 a 1.4)
  x1 <- seq(0.1, 1.4, length.out = 50)
  y1 <- sapply(x1, function(x) {
    val <- 1/tan(x)  # cotangente
    if (val > y_max) return(y_max)
    if (val < y_min) return(y_min)
    return(val)
  })
  
  # SEGMENTO 2: Después de discontinuidad (1.8 a 3.0)
  x2 <- seq(1.8, 3.0, length.out = 30)
  y2 <- sapply(x2, function(x) {
    val <- 1/tan(x)
    # Solo valores positivos pequeños
    if (val > 0 && val <= 2) return(val)
    return(NA)
  })
  
  # Filtrar NAs
  validos <- !is.na(y2)
  x2 <- x2[validos]
  y2 <- y2[validos]
  
  # SEGMENTO 3: Final (3.2 a 4.4)
  x3 <- seq(3.2, 4.4, length.out = 25)
  y3 <- sapply(x3, function(x) {
    val <- 1/tan(x)
    if (val > y_max) return(y_max)
    if (val < y_min) return(y_min)
    return(val)
  })
  
  cat("✅ Segmento 1:", length(x1), "puntos\n")
  cat("✅ Segmento 2:", length(x2), "puntos\n")
  cat("✅ Segmento 3:", length(x3), "puntos\n")
  
  return(list(
    x1 = x1, y1 = y1,
    x2 = x2, y2 = y2,
    x3 = x3, y3 = y3
  ))
}

# Generar los datos
datos <- generar_datos_cotangente()

# Función para crear el gráfico
crear_grafico_cotangente <- function(datos, archivo = "cotangente_replica.png") {
  
  cat("🎨 Creando gráfico de cotangente...\n")
  
  # Intentar crear archivo PNG
  tryCatch({
    png(archivo, width = 800, height = 600, bg = "white")
    
    # Configurar gráfico
    par(mar = c(4, 4, 1, 1))
    
    # Gráfico base
    plot(0, 0, type = "n", 
         xlim = c(0, 4.5), ylim = c(0, 5),
         xlab = "", ylab = "", axes = FALSE)
    
    # Ejes con flechas
    arrows(0, 0, 4.5, 0, length = 0.1, lwd = 2)
    arrows(0, 0, 0, 5, length = 0.1, lwd = 2)
    
    # Curvas cotangente en cyan
    lines(datos$x1, datos$y1, col = "cyan3", lwd = 3)
    lines(datos$x2, datos$y2, col = "cyan3", lwd = 3)
    lines(datos$x3, datos$y3, col = "cyan3", lwd = 3)
    
    # Líneas punteadas verticales
    segments(1.5, 0, 1.5, 5, lty = 2, col = "black", lwd = 1.5)
    segments(3.0, 0, 3.0, 5, lty = 2, col = "black", lwd = 1.5)
    
    # Etiquetas
    text(1.5, 4.7, "QP", cex = 1.2, font = 2)
    text(3.0, 4.7, expression(I[1]), cex = 1.2, font = 2)
    
    # Etiquetas de ejes
    mtext(expression(paste("Ángulo ", alpha)), side = 1, line = 2.5, cex = 1.2)
    mtext("Distancia PK", side = 2, line = 2.5, cex = 1.2)
    
    dev.off()
    
    cat("✅ Gráfico guardado:", archivo, "\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Error creando PNG:", e$message, "\n")
    cat("💡 Mostrando gráfico en pantalla...\n")
    
    # Mostrar en pantalla si PNG falla
    plot(0, 0, type = "n", 
         xlim = c(0, 4.5), ylim = c(0, 5),
         xlab = expression(paste("Ángulo ", alpha)),
         ylab = "Distancia PK",
         main = "Función Cotangente - Replica de Imagen")
    
    lines(datos$x1, datos$y1, col = "cyan3", lwd = 3)
    lines(datos$x2, datos$y2, col = "cyan3", lwd = 3)
    lines(datos$x3, datos$y3, col = "cyan3", lwd = 3)
    
    abline(v = 1.5, lty = 2, col = "black")
    abline(v = 3.0, lty = 2, col = "black")
    
    text(1.5, 4.7, "QP", cex = 1.2, font = 2)
    text(3.0, 4.7, expression(I[1]), cex = 1.2, font = 2)
    
    return(FALSE)
  })
}

# Crear el gráfico
resultado <- crear_grafico_cotangente(datos)

# Mostrar código para replicar manualmente
cat("\n📋 CÓDIGO R PARA REPLICAR LA IMAGEN:\n")
cat("=====================================\n")

codigo_replica <- '
# Datos de cotangente
x1 <- seq(0.1, 1.4, length.out = 50)
y1 <- pmax(0, pmin(5, 1/tan(x1)))

x2 <- seq(1.8, 3.0, length.out = 30)
y2 <- 1/tan(x2)
y2 <- y2[y2 > 0 & y2 <= 2]
x2 <- x2[1:length(y2)]

x3 <- seq(3.2, 4.4, length.out = 25)
y3 <- pmax(0, pmin(5, 1/tan(x3)))

# Gráfico
plot(0, 0, type = "n", xlim = c(0, 4.5), ylim = c(0, 5),
     xlab = expression(paste("Ángulo ", alpha)), 
     ylab = "Distancia PK")

lines(x1, y1, col = "cyan3", lwd = 3)
lines(x2, y2, col = "cyan3", lwd = 3)
lines(x3, y3, col = "cyan3", lwd = 3)

abline(v = 1.5, lty = 2)
abline(v = 3.0, lty = 2)

text(1.5, 4.7, "QP", font = 2)
text(3.0, 4.7, expression(I[1]), font = 2)
'

cat(codigo_replica)

cat("\n🎯 REPLICACIÓN COMPLETADA\n")
cat("📊 Función: cot(α) = 1/tan(α)\n")
cat("📏 Rango: α ∈ [0, 4.5], PK ∈ [0, 5]\n")
cat("🎨 Color: Cyan (como en la imagen original)\n")
cat("📍 Líneas verticales: QP (x=1.5), I₁ (x=3.0)\n")
