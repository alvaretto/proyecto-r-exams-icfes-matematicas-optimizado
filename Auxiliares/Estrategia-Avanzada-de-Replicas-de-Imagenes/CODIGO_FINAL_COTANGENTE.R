# ============================================================================
# CÓDIGO R FINAL PARA REPLICAR LA IMAGEN DE COTANGENTE
# Replica fielmente la imagen compartida: Distancia PK vs Ángulo α
# ============================================================================

# PASO 1: Generar datos de la función cotangente
cat("Generando datos de cotangente...\n")

# Segmento 1: Curva principal descendente (antes de π/2)
x1 <- seq(0.1, 1.4, length.out = 50)
y1 <- 1/tan(x1)
y1[y1 > 5] <- 5    # Limitar valores extremos
y1[y1 < 0] <- 0    # Solo valores positivos

# Segmento 2: Después de primera discontinuidad (entre π/2 y π)
x2 <- seq(1.8, 3.0, length.out = 30)
y2 <- 1/tan(x2)
# Filtrar solo valores positivos pequeños (como se ve en la imagen)
validos <- y2 > 0 & y2 <= 2
x2 <- x2[validos]
y2 <- y2[validos]

# Segmento 3: Después de segunda discontinuidad (después de π)
x3 <- seq(3.2, 4.4, length.out = 25)
y3 <- 1/tan(x3)
y3[y3 > 5] <- 5
y3[y3 < 0] <- 0

cat("Datos generados exitosamente\n")

# PASO 2: Crear el gráfico que replica la imagen
cat("Creando gráfico...\n")

# Configurar área de gráfico
par(mar = c(4, 4, 1, 1))

# Crear gráfico base con ejes y límites exactos de la imagen
plot(0, 0, type = "n", 
     xlim = c(0, 4.5), ylim = c(0, 5),
     xlab = expression(paste("Ángulo ", alpha)),
     ylab = "Distancia PK",
     axes = FALSE)

# Dibujar ejes con flechas (como en la imagen original)
arrows(0, 0, 4.5, 0, length = 0.1, lwd = 2, col = "black")
arrows(0, 0, 0, 5, length = 0.1, lwd = 2, col = "black")

# Dibujar la función cotangente en color cyan (como en la imagen)
lines(x1, y1, col = "cyan3", lwd = 3)
lines(x2, y2, col = "cyan3", lwd = 3)
lines(x3, y3, col = "cyan3", lwd = 3)

# Líneas punteadas verticales (QP e I₁)
segments(1.5, 0, 1.5, 5, lty = 2, col = "black", lwd = 1.5)
segments(3.0, 0, 3.0, 5, lty = 2, col = "black", lwd = 1.5)

# Etiquetas de las líneas verticales
text(1.5, 4.7, "QP", cex = 1.2, font = 2)
text(3.0, 4.7, expression(I[1]), cex = 1.2, font = 2)

cat("Gráfico completado\n")

# PASO 3: Guardar como imagen (opcional)
# Descomenta las siguientes líneas para guardar como PNG:
#
# png("cotangente_replica.png", width = 800, height = 600, bg = "white")
# [repetir todo el código del gráfico aquí]
# dev.off()

# ============================================================================
# INFORMACIÓN TÉCNICA DE LA REPLICACIÓN
# ============================================================================

cat("\n=== INFORMACIÓN DE LA REPLICACIÓN ===\n")
cat("Función matemática: cot(α) = 1/tan(α)\n")
cat("Dominio: α ∈ [0, 4.5] radianes\n")
cat("Rango: PK ∈ [0, 5] unidades\n")
cat("Discontinuidades: π/2 ≈ 1.57, π ≈ 3.14\n")
cat("Color de curva: Cyan\n")
cat("Líneas verticales: QP (x=1.5), I₁ (x=3.0)\n")
cat("Estilo: Líneas punteadas para referencias\n")

# ============================================================================
# CÓDIGO COMPACTO PARA COPIAR Y PEGAR
# ============================================================================

cat("\n=== CÓDIGO COMPACTO ===\n")
cat("# Copiar y pegar en R para replicar la imagen:\n\n")

codigo_compacto <- '
# Datos cotangente
x1 <- seq(0.1, 1.4, length.out = 50); y1 <- pmax(0, pmin(5, 1/tan(x1)))
x2 <- seq(1.8, 3.0, length.out = 30); y2 <- 1/tan(x2); v <- y2>0 & y2<=2; x2 <- x2[v]; y2 <- y2[v]
x3 <- seq(3.2, 4.4, length.out = 25); y3 <- pmax(0, pmin(5, 1/tan(x3)))

# Gráfico
plot(0, 0, type="n", xlim=c(0,4.5), ylim=c(0,5), xlab=expression(paste("Ángulo ",alpha)), ylab="Distancia PK", axes=F)
arrows(0,0,4.5,0,length=0.1,lwd=2); arrows(0,0,0,5,length=0.1,lwd=2)
lines(x1,y1,col="cyan3",lwd=3); lines(x2,y2,col="cyan3",lwd=3); lines(x3,y3,col="cyan3",lwd=3)
segments(1.5,0,1.5,5,lty=2,lwd=1.5); segments(3.0,0,3.0,5,lty=2,lwd=1.5)
text(1.5,4.7,"QP",font=2); text(3.0,4.7,expression(I[1]),font=2)
'

cat(codigo_compacto)
cat("\n\n✅ REPLICACIÓN COMPLETADA\n")
cat("🎯 El código anterior replica fielmente la imagen compartida\n")
