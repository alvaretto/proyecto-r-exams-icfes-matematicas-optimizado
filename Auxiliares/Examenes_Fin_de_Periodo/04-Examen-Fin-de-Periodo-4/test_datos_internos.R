# Script para verificar si los DATOS INTERNOS de cada pregunta cambian con diferentes semillas
# Este script simula lo que hace exams2pdf

library(exams)

cat("=== PRUEBA DE DATOS INTERNOS DE PREGUNTAS ===\n\n")

# Archivo de pregunta a probar
archivo_prueba <- "cateto_teorema_pitagoras_geometrico_metrico_formulacion_ejecucion_n2_v1_1.Rmd"

cat("Probando archivo:", archivo_prueba, "\n\n")

# Función para extraer datos de una pregunta con una semilla específica
extraer_datos <- function(semilla_test) {
  set.seed(semilla_test)
  
  # Generar el ejercicio (esto ejecuta el .Rmd)
  ejercicio <- exams2html(archivo_prueba, 
                          n = 1, 
                          name = paste0("test_", semilla_test),
                          dir = "salida/test_temp",
                          edir = ".",
                          verbose = FALSE,
                          quiet = TRUE)
  
  # Extraer información del ejercicio generado
  return(list(
    semilla = semilla_test,
    pregunta = substr(ejercicio[[1]][[1]]$question, 1, 200),  # Primeros 200 caracteres
    solucion = ejercicio[[1]][[1]]$solution,
    metainfo = ejercicio[[1]][[1]]$metainfo
  ))
}

# Probar con 3 semillas diferentes
cat("Generando ejercicio con semilla 1001...\n")
datos_1001 <- extraer_datos(1001)

cat("Generando ejercicio con semilla 1002...\n")
datos_1002 <- extraer_datos(1002)

cat("Generando ejercicio con semilla 1003...\n")
datos_1003 <- extraer_datos(1003)

cat("\n=== COMPARACIÓN DE RESULTADOS ===\n\n")

cat("Pregunta con semilla 1001:\n")
cat(datos_1001$pregunta, "...\n\n")

cat("Pregunta con semilla 1002:\n")
cat(datos_1002$pregunta, "...\n\n")

cat("Pregunta con semilla 1003:\n")
cat(datos_1003$pregunta, "...\n\n")

# Verificar si son diferentes
if (identical(datos_1001$pregunta, datos_1002$pregunta)) {
  cat("❌ ERROR CRÍTICO: Semillas 1001 y 1002 generan la MISMA pregunta\n")
  cat("   Esto significa que los datos internos NO están cambiando\n\n")
} else {
  cat("✓ OK: Semillas 1001 y 1002 generan preguntas DIFERENTES\n\n")
}

if (identical(datos_1002$pregunta, datos_1003$pregunta)) {
  cat("❌ ERROR CRÍTICO: Semillas 1002 y 1003 generan la MISMA pregunta\n")
  cat("   Esto significa que los datos internos NO están cambiando\n\n")
} else {
  cat("✓ OK: Semillas 1002 y 1003 generan preguntas DIFERENTES\n\n")
}

# Verificar soluciones
cat("=== VERIFICACIÓN DE SOLUCIONES ===\n\n")
cat("Solución correcta con semilla 1001:", datos_1001$metainfo$solution, "\n")
cat("Solución correcta con semilla 1002:", datos_1002$metainfo$solution, "\n")
cat("Solución correcta con semilla 1003:", datos_1003$metainfo$solution, "\n\n")

if (datos_1001$metainfo$solution == datos_1002$metainfo$solution &&
    datos_1002$metainfo$solution == datos_1003$metainfo$solution) {
  cat("❌ ERROR CRÍTICO: Todas las semillas generan la MISMA solución\n")
  cat("   Esto confirma que los datos NO están cambiando\n\n")
} else {
  cat("✓ OK: Diferentes semillas generan diferentes soluciones\n\n")
}

# Limpiar archivos temporales
unlink("salida/test_temp", recursive = TRUE)

cat("=== CONCLUSIÓN ===\n")
cat("Si ves errores ❌, el problema está en cómo los archivos .Rmd generan datos.\n")
cat("Si ves ✓ OK, el problema está en otro lugar (caché, PDFs antiguos, etc.)\n")

