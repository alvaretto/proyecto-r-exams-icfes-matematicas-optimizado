library(exams)

# Test específico para verificar que NUNCA se muestren soluciones
cat("=== TEST: VERIFICAR QUE NO SE MUESTREN SOLUCIONES ===\n")

# Test con múltiples archivos y múltiples ejecuciones
archivos_test <- c("11_C01_G09_2020_Tipo1.Rmd", "vaso-cilindrico-v6.Rmd")

for(i in 1:3) {
  cat(sprintf("\n--- Ejecución %d ---\n", i))
  
  try({
    result <- exams2pandoc(archivos_test,
                           n = 1,
                           name = paste0("test_nosol_", i),
                           encoding = "UTF-8",
                           template = "pcielo_nosol.tex",
                           solution = FALSE,  # Explícitamente desactivar soluciones
                           quiet = FALSE,
                           verbose = TRUE,
                           dir = "test_output",
                           edir = ".",
                           type = "docx")
    cat(sprintf("✓ Ejecución %d: ÉXITO\n", i))
  }, silent = FALSE)
}

cat("\n=== VERIFICACIÓN MANUAL REQUERIDA ===\n")
cat("Por favor, revisa los archivos generados en test_output/\n")
cat("para confirmar que NO contienen secciones de solución.\n")
cat("Los archivos deberían mostrar solo preguntas y opciones.\n")
