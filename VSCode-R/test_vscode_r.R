# Script de prueba para R en VSCode
# Este script verifica que R funciona correctamente en VSCode

# Cargar paquetes comunes
load_common_packages()

# Crear datos de ejemplo
x <- 1:10
y <- x^2

# Crear un gráfico simple
plot(x, y, 
     type = "b", 
     main = "Gráfico de prueba en VSCode",
     xlab = "X", 
     ylab = "Y = X²",
     col = "blue",
     pch = 16)

# Agregar línea de tendencia
lines(x, y, col = "red", lwd = 2)

# Mostrar información
cat("Datos creados:\n")
print(data.frame(x = x, y = y))

cat("\n¡R funcionando correctamente en VSCode!\n")
