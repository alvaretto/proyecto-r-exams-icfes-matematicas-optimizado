library(exams)
set.seed(123)

archivo <- "consumo_telefonico_adicional_n2_v1.Rmd"
resultados <- list()

cat("\n=== INICIANDO FASE 1: RENDERIZADO INICIAL ===\n\n")

# HTML
cat("🔄 Renderizando HTML...\n")
resultados$html <- tryCatch({
  exams2html(archivo, n=1, dir="test_output/html", encoding="UTF-8")
  "✅ EXITOSO"
}, error = function(e) {
  paste("❌ ERROR:", e$message)
}, warning = function(w) {
  paste("⚠️ ADVERTENCIA:", w$message)
})

# PDF
cat("🔄 Renderizando PDF...\n")
resultados$pdf <- tryCatch({
  exams2pdf(archivo, n=1, dir="test_output/pdf", template="plain", encoding="UTF-8")
  "✅ EXITOSO"
}, error = function(e) {
  paste("❌ ERROR:", e$message)
}, warning = function(w) {
  paste("⚠️ ADVERTENCIA:", w$message)
})

# DOCX
cat("🔄 Renderizando DOCX...\n")
resultados$docx <- tryCatch({
  exams2pandoc(archivo, n=1, dir="test_output/docx", type="docx", encoding="UTF-8")
  "✅ EXITOSO"
}, error = function(e) {
  paste("❌ ERROR:", e$message)
}, warning = function(w) {
  paste("⚠️ ADVERTENCIA:", w$message)
})

# NOPS
cat("🔄 Renderizando NOPS...\n")
resultados$nops <- tryCatch({
  exams2nops(archivo, n=1, dir="test_output/nops", encoding="UTF-8")
  "✅ EXITOSO"
}, error = function(e) {
  paste("❌ ERROR:", e$message)
}, warning = function(w) {
  paste("⚠️ ADVERTENCIA:", w$message)
})

# Reporte final
cat("\n")
cat("╔════════════════════════════════════════════════╗\n")
cat("║   REPORTE DE RENDERIZADO - FASE 1 COMPLETA   ║\n")
cat("╚════════════════════════════════════════════════╝\n\n")

cat("📄 HTML:  ", resultados$html, "\n")
cat("📄 PDF:   ", resultados$pdf, "\n")
cat("📄 DOCX:  ", resultados$docx, "\n")
cat("📄 NOPS:  ", resultados$nops, "\n\n")

# Contar éxitos
exitos <- sum(grepl("✅", c(resultados$html, resultados$pdf, resultados$docx, resultados$nops)))
cat("📊 RESULTADO: ", exitos, "/4 formatos exitosos\n\n")

# Siguiente paso
if (exitos == 4) {
  cat("✅ FASE 1 COMPLETADA CON ÉXITO\n")
  cat("➡️  SIGUIENTE PASO: Ejecutar FASE 2 (/validar-coherencia)\n")
} else if (exitos >= 2) {
  cat("⚠️  FASE 1 COMPLETADA CON ERRORES PARCIALES\n")
  cat("➡️  SIGUIENTE PASO: Ejecutar FASE 2 y luego FASE 3 (/diagnosticar-errores)\n")
} else {
  cat("❌ FASE 1 FALLÓ EN MÚLTIPLES FORMATOS\n")
  cat("➡️  SIGUIENTE PASO: Ejecutar FASE 3 (/diagnosticar-errores)\n")
}

# Guardar log
sink("test_output/fase1_log.txt")
cat("=== REPORTE DE RENDERIZADO FASE 1 ===\n")
cat("Archivo:", archivo, "\n")
cat("Fecha:", as.character(Sys.time()), "\n\n")
cat("HTML:", resultados$html, "\n")
cat("PDF:", resultados$pdf, "\n")
cat("DOCX:", resultados$docx, "\n")
cat("NOPS:", resultados$nops, "\n")
cat("\nÉxitos:", exitos, "/4\n")
sink()

cat("\n📁 Log guardado en: test_output/fase1_log.txt\n")
