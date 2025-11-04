################################################################################
# SCRIPT: test_formatos.R
# DESCRIPCIÓN: Prueba rápida de cada formato de salida con un solo ejercicio
# PROPÓSITO: Diagnosticar qué formatos funcionan y cuáles fallan
################################################################################

library(exams)

cat("\n")
cat("================================================================================\n")
cat("  PRUEBA RÁPIDA DE FORMATOS DE SALIDA\n")
cat("================================================================================\n")
cat("\n")

# Directorio de salida
dir_salida <- "salida"
dir.create(dir_salida, showWarnings = FALSE, recursive = TRUE)

# Seleccionar primer ejercicio disponible
ejercicios_disponibles <- list.files(pattern = "^[0-9]{3}-.*\\.Rmd$")

if (length(ejercicios_disponibles) == 0) {
  stop("ERROR: No se encontraron ejercicios .Rmd con prefijo numérico")
}

ejercicio_test <- ejercicios_disponibles[1]

cat(sprintf("Ejercicio de prueba: %s\n", ejercicio_test))
cat("\n")

# Contador de éxitos y fallos
exitos <- 0
fallos <- 0
resultados <- list()

################################################################################
# PRUEBA 1: DOCX CON SOLUCIONES
################################################################################

cat("================================================================================\n")
cat("PRUEBA 1: DOCX con soluciones (exams2pandoc)\n")
cat("================================================================================\n")

tryCatch({
  exams2pandoc(ejercicio_test,
               n = 1,
               name = "test_docx_sol",
               encoding = "UTF-8",
               template = "pcielo.tex",
               dir = dir_salida,
               verbose = FALSE,
               type = "docx")
  
  cat("✓ RESULTADO: ÉXITO\n")
  cat(sprintf("  Archivo generado: %s/test_docx_sol1.docx\n", dir_salida))
  exitos <- exitos + 1
  resultados$docx_sol <- "ÉXITO"
  
}, error = function(e) {
  cat("✗ RESULTADO: ERROR\n")
  cat(sprintf("  Mensaje de error: %s\n", e$message))
  fallos <- fallos + 1
  resultados$docx_sol <- paste("ERROR:", e$message)
})

cat("\n")

################################################################################
# PRUEBA 2: DOCX SIN SOLUCIONES
################################################################################

cat("================================================================================\n")
cat("PRUEBA 2: DOCX sin soluciones (exams2pandoc)\n")
cat("================================================================================\n")

tryCatch({
  exams2pandoc(ejercicio_test,
               n = 1,
               name = "test_docx_sin_sol",
               encoding = "UTF-8",
               template = "pcielo_nosol.tex",
               solution = FALSE,
               dir = dir_salida,
               verbose = FALSE,
               type = "docx")
  
  cat("✓ RESULTADO: ÉXITO\n")
  cat(sprintf("  Archivo generado: %s/test_docx_sin_sol1.docx\n", dir_salida))
  exitos <- exitos + 1
  resultados$docx_sin_sol <- "ÉXITO"
  
}, error = function(e) {
  cat("✗ RESULTADO: ERROR\n")
  cat(sprintf("  Mensaje de error: %s\n", e$message))
  fallos <- fallos + 1
  resultados$docx_sin_sol <- paste("ERROR:", e$message)
})

cat("\n")

################################################################################
# PRUEBA 3: PDF CON SOLUCIONES
################################################################################

cat("================================================================================\n")
cat("PRUEBA 3: PDF con soluciones (exams2pdf)\n")
cat("================================================================================\n")

tryCatch({
  exams2pdf(ejercicio_test,
            n = 1,
            name = "test_pdf_sol",
            encoding = "UTF-8",
            template = "solpcielo",
            dir = dir_salida,
            verbose = FALSE)
  
  cat("✓ RESULTADO: ÉXITO\n")
  cat(sprintf("  Archivo generado: %s/test_pdf_sol1.pdf\n", dir_salida))
  exitos <- exitos + 1
  resultados$pdf_sol <- "ÉXITO"
  
}, error = function(e) {
  cat("✗ RESULTADO: ERROR\n")
  cat(sprintf("  Mensaje de error: %s\n", e$message))
  fallos <- fallos + 1
  resultados$pdf_sol <- paste("ERROR:", e$message)
})

cat("\n")

################################################################################
# PRUEBA 4: PDF SIN SOLUCIONES
################################################################################

cat("================================================================================\n")
cat("PRUEBA 4: PDF sin soluciones (exams2pdf)\n")
cat("================================================================================\n")

tryCatch({
  exams2pdf(ejercicio_test,
            n = 1,
            name = "test_pdf",
            encoding = "UTF-8",
            template = "exam",
            dir = dir_salida,
            verbose = FALSE)
  
  cat("✓ RESULTADO: ÉXITO\n")
  cat(sprintf("  Archivo generado: %s/test_pdf1.pdf\n", dir_salida))
  exitos <- exitos + 1
  resultados$pdf <- "ÉXITO"
  
}, error = function(e) {
  cat("✗ RESULTADO: ERROR\n")
  cat(sprintf("  Mensaje de error: %s\n", e$message))
  fallos <- fallos + 1
  resultados$pdf <- paste("ERROR:", e$message)
})

cat("\n")

################################################################################
# PRUEBA 5: NOPS CON SOLUCIONES
################################################################################

cat("================================================================================\n")
cat("PRUEBA 5: NOPS con soluciones (exams2nops)\n")
cat("================================================================================\n")

tryCatch({
  exams2nops(ejercicio_test,
             n = 1,
             name = "test_nops_sol",
             encoding = "UTF-8",
             dir = dir_salida,
             language = "es",
             verbose = FALSE,
             solution = TRUE)
  
  cat("✓ RESULTADO: ÉXITO\n")
  cat(sprintf("  Archivo generado: %s/test_nops_sol1.pdf\n", dir_salida))
  exitos <- exitos + 1
  resultados$nops_sol <- "ÉXITO"
  
}, error = function(e) {
  cat("✗ RESULTADO: ERROR\n")
  cat(sprintf("  Mensaje de error: %s\n", e$message))
  fallos <- fallos + 1
  resultados$nops_sol <- paste("ERROR:", e$message)
})

cat("\n")

################################################################################
# PRUEBA 6: NOPS SIN SOLUCIONES
################################################################################

cat("================================================================================\n")
cat("PRUEBA 6: NOPS sin soluciones (exams2nops)\n")
cat("================================================================================\n")

tryCatch({
  exams2nops(ejercicio_test,
             n = 1,
             name = "test_nops",
             encoding = "UTF-8",
             dir = dir_salida,
             language = "es",
             verbose = FALSE,
             solution = FALSE)
  
  cat("✓ RESULTADO: ÉXITO\n")
  cat(sprintf("  Archivo generado: %s/test_nops1.pdf\n", dir_salida))
  exitos <- exitos + 1
  resultados$nops <- "ÉXITO"
  
}, error = function(e) {
  cat("✗ RESULTADO: ERROR\n")
  cat(sprintf("  Mensaje de error: %s\n", e$message))
  fallos <- fallos + 1
  resultados$nops <- paste("ERROR:", e$message)
})

cat("\n")

################################################################################
# RESUMEN DE RESULTADOS
################################################################################

cat("\n")
cat("================================================================================\n")
cat("  RESUMEN DE PRUEBAS\n")
cat("================================================================================\n")
cat("\n")

cat(sprintf("Total de pruebas: %d\n", exitos + fallos))
cat(sprintf("Éxitos: %d\n", exitos))
cat(sprintf("Fallos: %d\n", fallos))
cat("\n")

cat("Resultados detallados:\n")
cat("----------------------\n")

for (formato in names(resultados)) {
  resultado <- resultados[[formato]]
  simbolo <- if (grepl("^ÉXITO", resultado)) "✓" else "✗"
  cat(sprintf("%s %-20s: %s\n", simbolo, formato, resultado))
}

cat("\n")

if (fallos > 0) {
  cat("⚠️  ATENCIÓN: Algunos formatos fallaron\n")
  cat("\n")
  cat("Recomendaciones:\n")
  cat("----------------\n")
  
  if (!grepl("^ÉXITO", resultados$pdf_sol) || !grepl("^ÉXITO", resultados$pdf)) {
    cat("• Verificar instalación de LaTeX: pdflatex --version\n")
    cat("• Verificar templates LaTeX disponibles\n")
  }
  
  if (!grepl("^ÉXITO", resultados$nops_sol) || !grepl("^ÉXITO", resultados$nops)) {
    cat("• Verificar que el ejercicio sea tipo 'schoice' (selección múltiple)\n")
    cat("• exams2nops solo funciona con preguntas de selección múltiple\n")
  }
  
  cat("\n")
  cat("Consulte el archivo 04-DIAGNOSTICO_Y_SOLUCIONES.md para más información\n")
  
} else {
  cat("✓ ¡TODOS LOS FORMATOS FUNCIONAN CORRECTAMENTE!\n")
  cat("\n")
  cat("Puede ejecutar el script completo con confianza:\n")
  cat("  source('SemilleroFinDePeriodo_v4.R')\n")
}

cat("\n")
cat("================================================================================\n")
cat("\n")

# Guardar resultados en archivo
resultados_file <- "resultados_prueba_formatos.txt"
sink(resultados_file)
cat("RESULTADOS DE PRUEBA DE FORMATOS\n")
cat("================================\n\n")
cat(sprintf("Fecha: %s\n", Sys.time()))
cat(sprintf("Ejercicio probado: %s\n\n", ejercicio_test))
cat(sprintf("Éxitos: %d\n", exitos))
cat(sprintf("Fallos: %d\n\n", fallos))
cat("Detalles:\n")
for (formato in names(resultados)) {
  cat(sprintf("  %-20s: %s\n", formato, resultados[[formato]]))
}
sink()

cat(sprintf("Resultados guardados en: %s\n", resultados_file))
cat("\n")

