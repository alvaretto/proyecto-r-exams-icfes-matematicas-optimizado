# REPLICA BÁSICA DE COTANGENTE - Solo R base
# Genera la imagen de cotangente usando únicamente funciones base

cat("🎯 INICIANDO REPLICACIÓN DE COTANGENTE\n")

# Generar datos de cotangente en segmentos
cat("📊 Generando datos...\n")

# Segmento 1: Curva principal descendente
x1 <- seq(0.1, 1.4, by = 0.05)
y1 <- 1 / tan(x1)
y1[y1 > 5] <- 5
y1[y1 < 0] <- NA

# Segmento 2: Después de primera discontinuidad  
x2 <- seq(1.8, 3.0, by = 0.05)
y2 <- 1 / tan(x2)
y2[y2 < 0] <- NA
y2[y2 > 2] <- NA

# Segmento 3: Segmento final
x3 <- seq(3.2, 4.4, by = 0.05)
y3 <- 1 / tan(x3)
y3[y3 > 5] <- 5
y3[y3 < 0] <- NA

cat("✅ Datos generados exitosamente\n")
cat("   Segmento 1:", length(x1), "puntos\n")
cat("   Segmento 2:", length(x2), "puntos\n") 
cat("   Segmento 3:", length(x3), "puntos\n")

# Crear gráfico
cat("🎨 Creando gráfico...\n")

png("cotangente_replica.png", width = 800, height = 600, bg = "white")

# Configurar márgenes
par(mar = c(4, 4, 1, 1))

# Gráfico base vacío
plot(0, 0, type = "n", 
     xlim = c(0, 4.5), ylim = c(0, 5),
     xlab = "", ylab = "",
     axes = FALSE)

# Dibujar ejes con flechas
arrows(0, 0, 4.5, 0, length = 0.1, lwd = 2, col = "black")
arrows(0, 0, 0, 5, length = 0.1, lwd = 2, col = "black")

# Dibujar curvas cotangente
lines(x1, y1, col = "cyan3", lwd = 3)
lines(x2, y2, col = "cyan3", lwd = 3)  
lines(x3, y3, col = "cyan3", lwd = 3)

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

cat("✅ Gráfico guardado: cotangente_replica.png\n")
cat("📏 Dimensiones: 800x600 píxeles\n")
cat("🎯 Replicación completada exitosamente\n")

# Mostrar información de la función
cat("\n📋 INFORMACIÓN DE LA FUNCIÓN:\n")
cat("• Función: cot(α) = 1/tan(α)\n")
cat("• Discontinuidades: π/2 ≈ 1.57, π ≈ 3.14\n")
cat("• Dominio: [0, 4.5]\n")
cat("• Rango: [0, 5]\n")
cat("• Color: Cyan\n")
cat("• Líneas verticales: QP (x=1.5), I₁ (x=3.0)\n")

cat("\n🔍 ARCHIVOS GENERADOS:\n")
cat("📁 cotangente_replica.png - Imagen principal\n")
