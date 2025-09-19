# ===============================================================================
# SCRIPT DE PRUEBA - HTML CONSOLIDADO
# ===============================================================================
# Propósito: Demostrar la diferencia entre generar archivos HTML individuales
#           vs un archivo HTML consolidado con múltiples preguntas
# ===============================================================================

library(exams)

# Configuración de prueba
archivo_examen <- "consumo_gas_natural_porcentaje_maximo_aleatorio_interpretacion_representacion_n2_cloze_v1.Rmd"
num_preguntas <- 5  # Número reducido para prueba
semilla <- 12345
set.seed(semilla)

# Verificar que el archivo existe
if (!file.exists(archivo_examen)) {
  stop("❌ Error: No se encuentra el archivo ", archivo_examen)
}

cat("🧪 PRUEBA DE GENERACIÓN HTML CONSOLIDADO\n")
cat("========================================\n")
cat("📄 Archivo:", archivo_examen, "\n")
cat("🔢 Número de preguntas:", num_preguntas, "\n")
cat("🎲 Semilla:", semilla, "\n\n")

# Crear directorios de prueba
dir.create("prueba_html_individual", showWarnings = FALSE)
dir.create("prueba_html_consolidado", showWarnings = FALSE)

cat("📋 MÉTODO ANTERIOR (archivos individuales):\n")
cat("-------------------------------------------\n")
cat("Comando: exams2html(archivo, n = num_preguntas)\n")
cat("Resultado: Genera", num_preguntas, "archivos HTML separados\n\n")

# Método anterior: genera archivos individuales
set.seed(semilla)
resultado_individual <- exams2html(archivo_examen,
                                  n = num_preguntas,
                                  name = "individual",
                                  dir = "prueba_html_individual",
                                  template = "plain",
                                  mathjax = TRUE,
                                  verbose = FALSE)

archivos_individuales <- list.files("prueba_html_individual", pattern = "\\.html$")
cat("✅ Archivos individuales generados:", length(archivos_individuales), "\n")
for(i in seq_along(archivos_individuales)) {
  tamaño <- round(file.size(file.path("prueba_html_individual", archivos_individuales[i])) / 1024, 2)
  cat("  ", i, ":", archivos_individuales[i], "(", tamaño, "KB )\n")
}

cat("\n📋 MÉTODO NUEVO (archivo consolidado):\n")
cat("--------------------------------------\n")
cat("Comando: exams2html(rep(archivo, num_preguntas))\n")
cat("Resultado: Genera 1 archivo HTML con todas las preguntas\n\n")

# Método nuevo: genera archivo consolidado
set.seed(semilla)
resultado_consolidado <- exams2html(rep(archivo_examen, num_preguntas),
                                   name = "consolidado",
                                   dir = "prueba_html_consolidado",
                                   template = "plain",
                                   mathjax = TRUE,
                                   verbose = FALSE)

archivos_consolidados <- list.files("prueba_html_consolidado", pattern = "\\.html$")
cat("✅ Archivos consolidados generados:", length(archivos_consolidados), "\n")
for(i in seq_along(archivos_consolidados)) {
  tamaño <- round(file.size(file.path("prueba_html_consolidado", archivos_consolidados[i])) / 1024, 2)
  cat("  ", i, ":", archivos_consolidados[i], "(", tamaño, "KB )\n")
}

cat("\n📊 COMPARACIÓN DE RESULTADOS:\n")
cat("=============================\n")
cat("Método anterior:", length(archivos_individuales), "archivos HTML individuales\n")
cat("Método nuevo:   ", length(archivos_consolidados), "archivo HTML consolidado\n")

# Calcular tamaño total
tamaño_total_individual <- sum(sapply(archivos_individuales, function(x) {
  file.size(file.path("prueba_html_individual", x))
})) / 1024

tamaño_total_consolidado <- sum(sapply(archivos_consolidados, function(x) {
  file.size(file.path("prueba_html_consolidado", x))
})) / 1024

cat("Tamaño total individual:", round(tamaño_total_individual, 2), "KB\n")
cat("Tamaño total consolidado:", round(tamaño_total_consolidado, 2), "KB\n")

cat("\n✅ PRUEBA COMPLETADA\n")
cat("Los archivos están en:\n")
cat("- prueba_html_individual/ (método anterior)\n")
cat("- prueba_html_consolidado/ (método nuevo)\n")

cat("\n💡 EXPLICACIÓN DE LA MODIFICACIÓN:\n")
cat("==================================\n")
cat("La clave está en cambiar:\n")
cat("  DE: exams2html(archivo, n = N)\n")
cat("  A:  exams2html(rep(archivo, N))\n\n")
cat("- rep(archivo, N) crea un vector con N repeticiones del archivo\n")
cat("- Esto hace que exams2html() trate las N preguntas como un solo examen\n")
cat("- Resultado: 1 archivo HTML con N preguntas en lugar de N archivos\n")
