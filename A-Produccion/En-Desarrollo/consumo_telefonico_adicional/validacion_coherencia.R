# Script de validación de coherencia FASE 2 (sin dependencias externas)
archivo <- "consumo_telefonico_adicional_n2_v1.Rmd"
contenido <- readLines(archivo, encoding = "UTF-8")

cat("\n╔════════════════════════════════════════════════╗\n")
cat("║   FASE 2: VALIDACIÓN DE COHERENCIA             ║\n")
cat("╚════════════════════════════════════════════════╝\n\n")

errores <- 0
advertencias <- 0
detalles_errores <- c()

# ============================================================
# 1. COHERENCIA MATEMÁTICA (ERR_C1)
# ============================================================
cat("🔍 1. COHERENCIA MATEMÁTICA\n")
cat("   └─ Verificando cálculos y fórmulas...\n")

# Buscar operaciones matemáticas sobre variables formateadas
lineas_problematicas <- grep("(abs|round|floor|ceiling|sqrt)\\(.*format", contenido, ignore.case = TRUE)
if (length(lineas_problematicas) > 0) {
  errores <- errores + 1
  detalles_errores <- c(detalles_errores, paste("ERR_C1: Operaciones matemáticas sobre strings en líneas:", paste(lineas_problematicas, collapse=", ")))
  cat("   ❌ ERROR: Operaciones matemáticas sobre strings en líneas:", paste(lineas_problematicas, collapse=", "), "\n")
} else {
  cat("   ✅ No hay operaciones matemáticas sobre variables formateadas\n")
}

# Verificar que existe test_that para validaciones
tests_matematicos <- grep("test_that", contenido, ignore.case = TRUE)
if (length(tests_matematicos) > 0) {
  cat("   ✅ Tests matemáticos encontrados:", length(tests_matematicos), "\n")
} else {
  advertencias <- advertencias + 1
  cat("   ⚠️  No se encontraron tests de validación\n")
}

# ============================================================
# 2. COHERENCIA IMAGEN-TEXTO (ERR_C2)
# ============================================================
cat("\n🔍 2. COHERENCIA IMAGEN-TEXTO\n")
cat("   └─ Verificando sincronización de valores...\n")

# Verificar que hay generación de gráficos
generacion_graficos <- grep("py_run_string|plt\\.savefig|include_tikz", contenido, ignore.case = TRUE)
if (length(generacion_graficos) > 0) {
  cat("   ✅ Generación de gráficos encontrada\n")
} else {
  cat("   ℹ️  No se detectó generación de gráficos\n")
}

# Verificar interpolación de variables en texto
interpolaciones <- grep("`r [a-zA-Z_]", contenido)
if (length(interpolaciones) > 0) {
  cat("   ✅ Variables interpoladas en texto detectadas\n")
}

# ============================================================
# 3. COHERENCIA DE CÓDIGO (ERR_C3)
# ============================================================
cat("\n🔍 3. COHERENCIA DE CÓDIGO\n")
cat("   └─ Verificando sincronización R↔Python↔TikZ...\n")

# Verificar código TikZ
codigo_tikz <- grep("begin\\{tikz", contenido, ignore.case = TRUE)
if (length(codigo_tikz) > 0) {
  cat("   ✅ Código TikZ encontrado\n")
}

# Verificar código Python
codigo_python <- grep("py_run_string|import matplotlib", contenido, ignore.case = TRUE)
if (length(codigo_python) > 0) {
  cat("   ✅ Código Python encontrado\n")
}

# Verificar formato de números
formatos <- grep("format\\(|sprintf|formatC", contenido)
if (length(formatos) > 0) {
  cat("   ✅ Formateo de números encontrado\n")
}

# ============================================================
# 4. ERRORES DE TEXTO (ERR_T2)
# ============================================================
cat("\n🔍 4. ERRORES DE TEXTO/TIPOGRÁFICOS\n")
cat("   └─ Buscando errores comunes...\n")

# Buscar "consummos" (error tipográfico)
error_consummos <- grep("consummos", contenido, ignore.case = TRUE)
if (length(error_consummos) > 0) {
  errores <- errores + 1
  detalles_errores <- c(detalles_errores, paste0("ERR_T2: Error tipográfico 'consummos' → 'consumos' en línea(s): ", paste(error_consummos, collapse=", ")))
  cat("   ❌ ERROR TIPOGRÁFICO en línea(s):", paste(error_consummos, collapse=", "), "\n")
  cat("      → 'consummos' debería ser 'consumos'\n")
  cat("      Fragmento:", substr(contenido[error_consummos[1]], 1, 80), "...\n")
}

# ============================================================
# 5. METADATOS (Validación)
# ============================================================
cat("\n🔍 5. METADATOS\n")
cat("   └─ Verificando metadatos exams...\n")

metadatos <- c("exname:", "extype:", "exsolution:", "exshuffle:")
meta_ok <- 0
for (meta in metadatos) {
  if (any(grepl(paste0("^", meta), contenido))) {
    cat("   ✅", meta, "\n")
    meta_ok <- meta_ok + 1
  } else {
    errores <- errores + 1
    detalles_errores <- c(detalles_errores, paste("ERR_META: Falta metadato", meta))
    cat("   ❌ Falta:", meta, "\n")
  }
}

# ============================================================
# REPORTE FINAL
# ============================================================
cat("\n╔════════════════════════════════════════════════╗\n")
cat("║           RESUMEN DE VALIDACIÓN                ║\n")
cat("╚════════════════════════════════════════════════╝\n\n")

cat("📊 ESTADÍSTICAS:\n")
cat("   • Errores encontrados:", errores, "\n")
cat("   • Advertencias:", advertencias, "\n\n")

if (errores == 0 && advertencias == 0) {
  cat("✅ VALIDACIÓN EXITOSA - Sin errores ni advertencias\n\n")
  cat("➡️  SIGUIENTE PASO: Aprobar para producción (/promover-ejercicio)\n")
  resultado <- "APROBADO"
} else if (errores > 0) {
  cat("❌ VALIDACIÓN CON ERRORES\n\n")
  cat("DETALLES:\n")
  for (det in detalles_errores) {
    cat("  •", det, "\n")
  }
  cat("\n➡️  SIGUIENTE PASO: Ejecutar FASE 3 (/diagnosticar-errores)\n")
  resultado <- "CON_ERRORES"
} else {
  cat("⚠️  VALIDACIÓN CON ADVERTENCIAS\n\n")
  cat("➡️  RECOMENDACIÓN: Revisar advertencias antes de continuar\n")
  resultado <- "CON_ADVERTENCIAS"
}

# Guardar reporte
sink("test_output/fase2_coherencia.txt")
cat("=== REPORTE DE COHERENCIA FASE 2 ===\n")
cat("Archivo:", archivo, "\n")
cat("Fecha:", as.character(Sys.time()), "\n\n")
cat("Errores:", errores, "\n")
cat("Advertencias:", advertencias, "\n\n")
if (length(detalles_errores) > 0) {
  cat("DETALLES:\n")
  for (det in detalles_errores) {
    cat(det, "\n")
  }
}
cat("\nResultado:", resultado, "\n")
sink()

cat("\n📁 Reporte guardado en: test_output/fase2_coherencia.txt\n\n")
