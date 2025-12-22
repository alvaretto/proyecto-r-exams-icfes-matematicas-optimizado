#!/usr/bin/env Rscript
# Script de Validación de Renderizado
# Archivo: proyeccion_usuarios_parabola_geometrico_interpretacion_n2_v1.Rmd

library(exams)

# Configuración
archivo <- "proyeccion_usuarios_parabola_geometrico_interpretacion_n2_v1.Rmd"
set.seed(123)  # Semilla fija para reproducibilidad

# Crear directorios de salida
dir.create("test_renderizado", showWarnings = FALSE)
dir.create("test_renderizado/html", showWarnings = FALSE, recursive = TRUE)
dir.create("test_renderizado/pdf", showWarnings = FALSE, recursive = TRUE)
dir.create("test_renderizado/docx", showWarnings = FALSE, recursive = TRUE)

# Almacenar resultados
resultados <- list()
tiempos <- list()

cat("\n")
cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║         VALIDACIÓN DE RENDERIZADO - EXAMS2*                  ║\n")
cat("╠══════════════════════════════════════════════════════════════╣\n")
cat("║  Archivo:", archivo, "           ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

# ============================================================
# TEST 1: HTML
# ============================================================
cat("📄 [1/4] Renderizando HTML...\n")
tiempo_inicio <- Sys.time()
resultados$html <- tryCatch({
  exams2html(archivo,
             n = 1,
             dir = "test_renderizado/html",
             encoding = "UTF-8",
             template = "plain")

  tiempo_fin <- Sys.time()
  tiempos$html <- as.numeric(difftime(tiempo_fin, tiempo_inicio, units = "secs"))

  # Verificar archivo generado
  if (file.exists("test_renderizado/html/plain1.html")) {
    list(status = "✅ EXITOSO",
         tiempo = round(tiempos$html, 2),
         archivo = "test_renderizado/html/plain1.html")
  } else {
    list(status = "❌ FALLIDO",
         error = "Archivo HTML no generado",
         tiempo = round(tiempos$html, 2))
  }
}, error = function(e) {
  tiempo_fin <- Sys.time()
  tiempos$html <- as.numeric(difftime(tiempo_fin, tiempo_inicio, units = "secs"))
  list(status = "❌ ERROR",
       error = e$message,
       tiempo = round(tiempos$html, 2))
})

cat("   Resultado:", resultados$html$status, "\n")
if (!is.null(resultados$html$error)) {
  cat("   Error:", resultados$html$error, "\n")
}
cat("   Tiempo:", resultados$html$tiempo, "segundos\n\n")

# ============================================================
# TEST 2: PDF
# ============================================================
cat("📄 [2/4] Renderizando PDF...\n")
tiempo_inicio <- Sys.time()
resultados$pdf <- tryCatch({
  exams2pdf(archivo,
            n = 1,
            dir = "test_renderizado/pdf",
            encoding = "UTF-8",
            template = "plain")

  tiempo_fin <- Sys.time()
  tiempos$pdf <- as.numeric(difftime(tiempo_fin, tiempo_inicio, units = "secs"))

  # Verificar archivo generado
  if (file.exists("test_renderizado/pdf/plain1.pdf")) {
    list(status = "✅ EXITOSO",
         tiempo = round(tiempos$pdf, 2),
         archivo = "test_renderizado/pdf/plain1.pdf")
  } else {
    list(status = "❌ FALLIDO",
         error = "Archivo PDF no generado",
         tiempo = round(tiempos$pdf, 2))
  }
}, error = function(e) {
  tiempo_fin <- Sys.time()
  tiempos$pdf <- as.numeric(difftime(tiempo_fin, tiempo_inicio, units = "secs"))
  list(status = "❌ ERROR",
       error = e$message,
       tiempo = round(tiempos$pdf, 2))
})

cat("   Resultado:", resultados$pdf$status, "\n")
if (!is.null(resultados$pdf$error)) {
  cat("   Error:", resultados$pdf$error, "\n")
}
cat("   Tiempo:", resultados$pdf$tiempo, "segundos\n\n")

# ============================================================
# TEST 3: DOCX (Pandoc)
# ============================================================
cat("📄 [3/4] Renderizando DOCX...\n")
tiempo_inicio <- Sys.time()
resultados$docx <- tryCatch({
  exams2pandoc(archivo,
               n = 1,
               dir = "test_renderizado/docx",
               type = "docx",
               encoding = "UTF-8")

  tiempo_fin <- Sys.time()
  tiempos$docx <- as.numeric(difftime(tiempo_fin, tiempo_inicio, units = "secs"))

  # Buscar archivo generado (puede tener nombre diferente)
  archivos_docx <- list.files("test_renderizado/docx", pattern = "\\.docx$", full.names = TRUE)

  if (length(archivos_docx) > 0) {
    list(status = "✅ EXITOSO",
         tiempo = round(tiempos$docx, 2),
         archivo = archivos_docx[1])
  } else {
    list(status = "❌ FALLIDO",
         error = "Archivo DOCX no generado",
         tiempo = round(tiempos$docx, 2))
  }
}, error = function(e) {
  tiempo_fin <- Sys.time()
  tiempos$docx <- as.numeric(difftime(tiempo_fin, tiempo_inicio, units = "secs"))
  list(status = "❌ ERROR",
       error = e$message,
       tiempo = round(tiempos$docx, 2))
})

cat("   Resultado:", resultados$docx$status, "\n")
if (!is.null(resultados$docx$error)) {
  cat("   Error:", resultados$docx$error, "\n")
}
cat("   Tiempo:", resultados$docx$tiempo, "segundos\n\n")

# ============================================================
# TEST 4: NOPS
# ============================================================
cat("📄 [4/4] Renderizando NOPS...\n")
tiempo_inicio <- Sys.time()
resultados$nops <- tryCatch({
  # Crear directorio temporal para NOPS
  dir.create("test_renderizado/nops", showWarnings = FALSE, recursive = TRUE)

  exams2nops(archivo,
             n = 1,
             dir = "test_renderizado/nops",
             encoding = "UTF-8")

  tiempo_fin <- Sys.time()
  tiempos$nops <- as.numeric(difftime(tiempo_fin, tiempo_inicio, units = "secs"))

  # Buscar archivo generado
  archivos_nops <- list.files("test_renderizado/nops", pattern = "\\.pdf$", full.names = TRUE)

  if (length(archivos_nops) > 0) {
    list(status = "✅ EXITOSO",
         tiempo = round(tiempos$nops, 2),
         archivo = archivos_nops[1])
  } else {
    list(status = "❌ FALLIDO",
         error = "Archivo NOPS no generado",
         tiempo = round(tiempos$nops, 2))
  }
}, error = function(e) {
  tiempo_fin <- Sys.time()
  tiempos$nops <- as.numeric(difftime(tiempo_fin, tiempo_inicio, units = "secs"))
  list(status = "❌ ERROR",
       error = e$message,
       tiempo = round(tiempos$nops, 2))
})

cat("   Resultado:", resultados$nops$status, "\n")
if (!is.null(resultados$nops$error)) {
  cat("   Error:", resultados$nops$error, "\n")
}
cat("   Tiempo:", resultados$nops$tiempo, "segundos\n\n")

# ============================================================
# RESUMEN FINAL
# ============================================================
cat("\n")
cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║                  RESUMEN DE RENDERIZADO                      ║\n")
cat("╠══════════════════════════════════════════════════════════════╣\n")

# Contar exitosos
exitosos <- sum(sapply(resultados, function(r) grepl("✅", r$status)))
total <- length(resultados)

cat(sprintf("║  HTML:     %-50s║\n", resultados$html$status))
cat(sprintf("║  PDF:      %-50s║\n", resultados$pdf$status))
cat(sprintf("║  DOCX:     %-50s║\n", resultados$docx$status))
cat(sprintf("║  NOPS:     %-50s║\n", resultados$nops$status))
cat("╠══════════════════════════════════════════════════════════════╣\n")
cat(sprintf("║  TOTAL:    %d/%d formatos exitosos                           ║\n", exitosos, total))
cat("╠══════════════════════════════════════════════════════════════╣\n")

# Evaluación general
if (exitosos == 4) {
  cat("║  ESTADO:   ✅ APROBADO - Todos los formatos OK               ║\n")
  cat("║  ACCIÓN:   Continuar a inspección visual                    ║\n")
} else if (exitosos >= 3) {
  cat("║  ESTADO:   ⚠️  PARCIAL - Algunos formatos fallaron           ║\n")
  cat("║  ACCIÓN:   Diagnosticar formatos fallidos                   ║\n")
} else {
  cat("║  ESTADO:   ❌ FALLIDO - Múltiples errores                    ║\n")
  cat("║  ACCIÓN:   Ejecutar /diagnosticar-errores                   ║\n")
}

cat("╚══════════════════════════════════════════════════════════════╝\n\n")

# Archivos generados
if (exitosos > 0) {
  cat("📁 ARCHIVOS GENERADOS:\n")
  cat("───────────────────────────────────────────────────────────────\n")

  if (!is.null(resultados$html$archivo)) {
    cat("  •", resultados$html$archivo, "\n")
  }
  if (!is.null(resultados$pdf$archivo)) {
    cat("  •", resultados$pdf$archivo, "\n")
  }
  if (!is.null(resultados$docx$archivo)) {
    cat("  •", resultados$docx$archivo, "\n")
  }
  if (!is.null(resultados$nops$archivo)) {
    cat("  •", resultados$nops$archivo, "\n")
  }

  cat("\n")
}

# Guardar reporte en archivo
reporte <- paste0(
  "# Reporte de Validación de Renderizado\n",
  "Archivo: ", archivo, "\n",
  "Fecha: ", Sys.time(), "\n\n",
  "## Resultados\n",
  "- HTML: ", resultados$html$status, " (", resultados$html$tiempo, "s)\n",
  "- PDF: ", resultados$pdf$status, " (", resultados$pdf$tiempo, "s)\n",
  "- DOCX: ", resultados$docx$status, " (", resultados$docx$tiempo, "s)\n",
  "- NOPS: ", resultados$nops$status, " (", resultados$nops$tiempo, "s)\n\n",
  "## Total: ", exitosos, "/", total, " formatos exitosos\n"
)

writeLines(reporte, "test_renderizado/REPORTE.md")
cat("📄 Reporte guardado en: test_renderizado/REPORTE.md\n\n")

# Código de salida
if (exitosos == total) {
  quit(status = 0)
} else {
  quit(status = 1)
}
