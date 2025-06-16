# Demo: Diversidad de nombres masculinos y femeninos
# Muestra ejemplos de las combinaciones de nombres generadas

cat("=== DEMO: NOMBRES MIXTOS EN EL EJERCICIO ===\n\n")

# Simular la lógica de aleatorización de nombres del ejercicio
nombres_masculinos <- c("Pedro", "Carlos", "Miguel", "Antonio", "Diego", "Sebastián", "Andrés", "Luis", "José", "Francisco")
nombres_femeninos <- c("María", "Ana", "Carmen", "Laura", "Sofía", "Valentina", "Isabella", "Camila", "Daniela", "Alejandra")

cat("Nombres masculinos disponibles:", length(nombres_masculinos), "\n")
cat(paste(nombres_masculinos, collapse=", "), "\n\n")

cat("Nombres femeninos disponibles:", length(nombres_femeninos), "\n")
cat(paste(nombres_femeninos, collapse=", "), "\n\n")

cat("Ejemplos de combinaciones generadas:\n")
cat("=====================================\n")

# Generar 10 ejemplos aleatorios
set.seed(123)  # Para reproducibilidad en la demo
for(i in 1:10) {
  # Seleccionar aleatoriamente un nombre masculino y uno femenino
  nombre_masculino <- sample(nombres_masculinos, 1)
  nombre_femenino <- sample(nombres_femeninos, 1)
  
  # Aleatorizar el orden (quién se menciona primero)
  if(sample(c(TRUE, FALSE), 1)) {
    nombre1 <- nombre_masculino
    nombre2 <- nombre_femenino
    orden <- "M-F"
  } else {
    nombre1 <- nombre_femenino
    nombre2 <- nombre_masculino
    orden <- "F-M"
  }
  
  cat(sprintf("%2d. %s y %s (%s)\n", i, nombre1, nombre2, orden))
}

cat("\nEstadísticas:\n")
cat("=============\n")
cat("Total de nombres masculinos:", length(nombres_masculinos), "\n")
cat("Total de nombres femeninos:", length(nombres_femeninos), "\n")
cat("Combinaciones únicas posibles:", length(nombres_masculinos) * length(nombres_femeninos), "\n")
cat("Con orden aleatorio (2 órdenes):", length(nombres_masculinos) * length(nombres_femeninos) * 2, "combinaciones\n")

cat("\nBeneficios de esta implementación:\n")
cat("==================================\n")
cat("✓ Inclusión de género en los nombres\n")
cat("✓ Mayor diversidad (200 vs 60 combinaciones anteriores)\n")
cat("✓ Representación equilibrada masculino/femenino\n")
cat("✓ Orden aleatorio para evitar sesgos\n")
cat("✓ Nombres comunes y reconocibles en español\n")

cat("\n=== FIN DE LA DEMO ===\n")
