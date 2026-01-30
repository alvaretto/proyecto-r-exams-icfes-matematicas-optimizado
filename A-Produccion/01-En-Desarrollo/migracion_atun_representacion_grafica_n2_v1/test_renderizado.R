# Script de Validación Completa - FASE 1: Renderizado Inicial
# Ciclo de Validación Automática

library(exams)
set.seed(123)

archivo <- "migracion_atun_representacion_grafica_aleatorio_interpretacion_n2_v1.Rmd"
resultados <- list()

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║  🔄 FASE 1: RENDERIZADO INICIAL - Ciclo de Validación    ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# Crear directorios de prueba
dir.create("test", showWarnings = FALSE)
dir.create("test/html", showWarnings = FALSE, recursive = TRUE)
dir.create("test/pdf", showWarnings = FALSE, recursive = TRUE)
dir.create("test/docx", showWarnings = FALSE, recursive = TRUE)
dir.create("test/nops", showWarnings = FALSE, recursive = TRUE)

# HTML
cat("📄 Renderizando HTML...\n")
resultados$html <- tryCatch({
  exams2html(archivo, n=1, dir="test/html", encoding = "UTF-8")
  "✅ EXITOSO"
}, error = function(e) {
  paste("❌", e$message)
})

# PDF
cat("📄 Renderizando PDF...\n")
resultados$pdf <- tryCatch({
  exams2pdf(archivo, n=1, dir="test/pdf", template = "plain", encoding = "UTF-8")
  "✅ EXITOSO"
}, error = function(e) {
  paste("❌", e$message)
})

# DOCX
cat("📄 Renderizando DOCX...\n")
resultados$docx <- tryCatch({
  exams2pandoc(archivo, n=1, dir="test/docx", type="docx", encoding = "UTF-8")
  "✅ EXITOSO"
}, error = function(e) {
  paste("❌", e$message)
})

# NOPS
cat("📄 Renderizando NOPS...\n")
resultados$nops <- tryCatch({
  exams2nops(archivo, n=1, dir="test/nops", encoding = "UTF-8")
  "✅ EXITOSO"
}, error = function(e) {
  paste("❌", e$message)
})

# Reporte Final
cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║           📊 REPORTE DE RENDERIZADO - FASE 1              ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

cat("📄 HTML:  ", resultados$html, "\n")
cat("📄 PDF:   ", resultados$pdf, "\n")
cat("📄 DOCX:  ", resultados$docx, "\n")
cat("📄 NOPS:  ", resultados$nops, "\n\n")

# Contar éxitos
exitos <- sum(grepl("✅", unlist(resultados)))
total <- length(resultados)

cat("═══════════════════════════════════════════════════════\n")
cat("  RESULTADO: ", exitos, "/", total, " formatos exitosos\n")
cat("═══════════════════════════════════════════════════════\n\n")

# Recomendación siguiente paso
if (exitos == 4) {
  cat("✅ FASE 1 COMPLETADA - Continuar a FASE 2: /validar-coherencia\n\n")
} else if (exitos >= 3) {
  cat("⚠️  FASE 1 CON ADVERTENCIAS - Diagnosticar formato fallido\n")
  cat("    Luego continuar a FASE 2: /validar-coherencia\n\n")
} else {
  cat("❌ FASE 1 CON ERRORES CRÍTICOS - Ejecutar /diagnosticar-errores\n\n")
}

# Listar archivos generados
cat("📂 Archivos generados:\n")
if (file.exists("test/html/plain1.html")) {
  cat("   ✓ test/html/plain1.html\n")
}
if (file.exists("test/pdf/plain1.pdf")) {
  cat("   ✓ test/pdf/plain1.pdf\n")
}
if (length(list.files("test/docx", pattern = "*.docx")) > 0) {
  cat("   ✓ test/docx/*.docx\n")
}
if (length(list.files("test/nops", pattern = "*.pdf")) > 0) {
  cat("   ✓ test/nops/*.pdf\n")
}

cat("\n")
