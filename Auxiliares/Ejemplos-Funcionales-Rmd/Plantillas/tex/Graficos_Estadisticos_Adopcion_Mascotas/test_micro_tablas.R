# Script de prueba específico para la plantilla micro (texto muy pequeño en tablas)
library(exams)

# Configurar modo generación de exámenes
.exams_generation_mode <- TRUE

# Configuración de prueba
archivo_examen <- "I_1796473-Opc-A2.Rmd"
copias <- 1
numpreg <- 2
semilla <- 12345
set.seed(semilla)
dir_salida <- "test_micro_salida"
dir_ejercicios <- "."

# Crear directorio de salida si no existe
if (!dir.exists(dir_salida)) {
  dir.create(dir_salida)
}

nombre_sin_extension <- sub("\\.Rmd$", "", archivo_examen)

cat("=== PRUEBA DE PLANTILLA MICRO PARA TABLAS GRANDES ===\n\n")

################################################################################
# PRUEBA: Comparación entre plantillas para tablas

cat("PRUEBA 1: Generando PDF con plantilla ESTÁNDAR...\n")
tryCatch({
  exams2pdf(rep(archivo_examen, numpreg),
            n = copias,
            name = paste0(nombre_sin_extension, "_estandar_"),
            encoding = "UTF-8",
            template = "oficio_solpcielo",
            dir = dir_salida,
            edir = dir_ejercicios,
            verbose = TRUE)
  cat("✅ PRUEBA 1 COMPLETADA: PDF estándar generado\n\n")
}, error = function(e) {
  cat("❌ PRUEBA 1 FALLÓ:", e$message, "\n\n")
})

cat("PRUEBA 2: Generando PDF con plantilla MICRO (texto muy pequeño)...\n")
tryCatch({
  exams2pdf(rep(archivo_examen, numpreg),
            n = copias,
            name = paste0(nombre_sin_extension, "_micro_"),
            encoding = "UTF-8",
            template = "oficio_solpcielo_micro",
            dir = dir_salida,
            edir = dir_ejercicios,
            verbose = TRUE)
  cat("✅ PRUEBA 2 COMPLETADA: PDF micro generado\n\n")
}, error = function(e) {
  cat("❌ PRUEBA 2 FALLÓ:", e$message, "\n\n")
})

cat("PRUEBA 3: Generando PDF con plantilla TABLAS (ultra-compacta)...\n")
tryCatch({
  exams2pdf(rep(archivo_examen, numpreg),
            n = copias,
            name = paste0(nombre_sin_extension, "_tablas_"),
            encoding = "UTF-8",
            template = "oficio_solpcielo_tablas",
            dir = dir_salida,
            edir = dir_ejercicios,
            verbose = TRUE)
  cat("✅ PRUEBA 3 COMPLETADA: PDF tablas generado\n\n")
}, error = function(e) {
  cat("❌ PRUEBA 3 FALLÓ:", e$message, "\n\n")
})

################################################################################
# Resumen de archivos generados
cat("=== ARCHIVOS GENERADOS PARA COMPARACIÓN ===\n")
archivos_generados <- list.files(dir_salida, pattern = "\\.pdf$", full.names = FALSE)
if (length(archivos_generados) > 0) {
  cat("Archivos PDF en", dir_salida, ":\n")
  for (archivo in archivos_generados) {
    cat("  -", archivo, "\n")
  }
} else {
  cat("No se generaron archivos PDF.\n")
}

cat("\n=== INSTRUCCIONES DE COMPARACIÓN ===\n")
cat("Compara los 3 PDFs generados:\n\n")

cat("1. **_estandar_**: Plantilla normal\n")
cat("   - Texto de tablas: tamaño normal\n")
cat("   - Problema: Tablas pueden desbordarse\n\n")

cat("2. **_micro_**: Plantilla con texto muy pequeño\n")
cat("   - Texto de tablas: fontsize{5}{6} (muy pequeño)\n")
cat("   - Solución: Tablas deberían caber en columnas\n\n")

cat("3. **_tablas_**: Plantilla ultra-compacta\n")
cat("   - Texto de tablas: tiny + espaciado mínimo\n")
cat("   - Alternativa: Compacidad máxima\n\n")

cat("=== CRITERIOS DE EVALUACIÓN ===\n")
cat("✅ ÉXITO si:\n")
cat("   - Las tablas se ven completas dentro de las columnas\n")
cat("   - El texto es legible (aunque pequeño)\n")
cat("   - No hay desbordamiento horizontal\n")
cat("   - El formato de dos columnas se mantiene\n\n")

cat("❌ FALLA si:\n")
cat("   - Las tablas se cortan o desbordan\n")
cat("   - El texto es ilegible\n")
cat("   - Las columnas se rompen\n\n")

cat("=== RECOMENDACIÓN DE USO ===\n")
cat("Según los resultados, usar:\n")
cat("- Si _micro_ funciona bien: template = 'oficio_solpcielo_micro'\n")
cat("- Si _tablas_ es mejor: template = 'oficio_solpcielo_tablas'\n")
cat("- Si ambos fallan: considerar dividir tablas grandes\n\n")

cat("=== PRUEBA COMPLETADA ===\n")
cat("Revisa los PDFs generados y elige la mejor opción.\n")
