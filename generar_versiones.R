# Script para generar múltiples versiones del ejercicio
# y verificar que se generan al menos 300 versiones diferentes

library(exams)
library(digest)

# Función para generar una versión del ejercicio y obtener su huella digital
generar_version <- function(seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Generar el ejercicio
  resultado <- exams::xexams(
    file = "interpretacion_grafica_viaje_optimizado.Rmd",
    driver = list(sweave = NULL),
    quiet = TRUE
  )
  
  # Extraer los datos relevantes para la huella digital
  datos <- resultado[[1]][[1]]
  
  # Crear una huella digital basada en los metadatos y la solución
  huella <- digest::digest(list(
    metadatos = datos$metainfo,
    pregunta = datos$question,
    solucion = datos$solution
  ))
  
  return(huella)
}

# Generar múltiples versiones y contar cuántas son únicas
n_simulaciones <- 350
cat("Generando", n_simulaciones, "versiones del ejercicio...\n")
cat("Este proceso puede tardar varios minutos.\n")

# Crear un vector de semillas aleatorias para maximizar la diversidad
set.seed(123) # Semilla maestra para reproducibilidad
semillas <- sample(1:10000, n_simulaciones)

# Generar las versiones
huellas_digitales <- character(n_simulaciones)
for (i in 1:n_simulaciones) {
  if (i %% 50 == 0) cat("Progreso:", i, "de", n_simulaciones, "versiones\n")
  
  # Usar una semilla diferente para cada versión
  huellas_digitales[i] <- generar_version(semillas[i])
}

# Contar cuántas versiones únicas se generaron
n_unicos <- length(unique(huellas_digitales))
cat(sprintf("\nNúmero de versiones únicas generadas: %d de %d (%.1f%%)\n",
            n_unicos, n_simulaciones, 100 * n_unicos / n_simulaciones))

# Verificar si se alcanzó el umbral mínimo de 300 versiones
if (n_unicos >= 300) {
  cat("\n✅ ÉXITO: Se generaron", n_unicos, "versiones diferentes del ejercicio.\n")
  cat("\n✅ Se garantiza un mínimo de 300 versiones diferentes.\n")
} else {
  cat("\n❌ ERROR: Solo se generaron", n_unicos, "versiones diferentes.\n")
  cat("\n❌ Se requieren al menos 300 versiones diferentes.\n")
}
