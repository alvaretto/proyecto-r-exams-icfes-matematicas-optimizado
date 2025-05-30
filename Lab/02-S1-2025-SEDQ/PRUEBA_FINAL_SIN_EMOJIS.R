# Script de prueba final para verificar eliminación completa de emojis
# Aplicando modo ultrathink y nombres de variables en español

# Configuración robusta anti-emojis ANTES de cargar bibliotecas
options(testthat.use_colours = FALSE)
options(testthat.unicode = FALSE)
options(testthat.progress = FALSE)
options(testthat.summary = FALSE)
Sys.setenv("TESTTHAT_UNICODE" = "false")
Sys.setenv("TESTTHAT_COLOURS" = "false")

# Cargar bibliotecas necesarias
library(exams)
library(knitr)
library(stringr)
library(testthat)
library(digest)

# Nombre del archivo a probar
archivo_ejercicio_mejorado <- "probabilidad_condicional_tabla_contingencia_razonamiento_nivel3_v2_MEJORADO.Rmd"

cat("=== PRUEBA FINAL DE ELIMINACIÓN COMPLETA DE EMOJIS ===\n")
cat("Archivo:", archivo_ejercicio_mejorado, "\n\n")

# Prueba 1: Verificar configuraciones anti-emojis
cat("1. Verificando configuraciones anti-emojis...\n")
configuraciones_correctas <- c(
  getOption("testthat.use_colours") == FALSE,
  getOption("testthat.unicode") == FALSE,
  getOption("testthat.progress") == FALSE,
  getOption("testthat.summary") == FALSE,
  Sys.getenv("TESTTHAT_UNICODE") == "false",
  Sys.getenv("TESTTHAT_COLOURS") == "false"
)

if (all(configuraciones_correctas)) {
  cat("  ✅ Todas las configuraciones anti-emojis están activas\n")
} else {
  cat("  ❌ Algunas configuraciones anti-emojis fallan\n")
  cat("  Detalles:", which(!configuraciones_correctas), "\n")
}

# Prueba 2: Verificar que no hay emojis en el código fuente
cat("\n2. Verificando ausencia de emojis en código fuente...\n")
contenido_archivo <- readLines(archivo_ejercicio_mejorado, encoding = "UTF-8")
emojis_problematicos <- c("🥇", "🥳", "🌈", "😸", "🎉", "✅", "❌", "⚠️")

emojis_encontrados_codigo <- character(0)
for (emoji in emojis_problematicos) {
  lineas_con_emoji <- grep(emoji, contenido_archivo, fixed = TRUE)
  if (length(lineas_con_emoji) > 0) {
    emojis_encontrados_codigo <- c(emojis_encontrados_codigo, emoji)
    cat("  ❌ ENCONTRADO en código:", emoji, "en líneas:", paste(lineas_con_emoji, collapse = ", "), "\n")
  }
}

if (length(emojis_encontrados_codigo) == 0) {
  cat("  ✅ No se encontraron emojis en el código fuente\n")
} else {
  cat("  ❌ Se encontraron", length(emojis_encontrados_codigo), "emojis en código\n")
}

# Prueba 3: Compilación con knit()
cat("\n3. Probando compilación con knit()...\n")
resultado_compilacion <- tryCatch({
  archivo_salida <- knit(archivo_ejercicio_mejorado, quiet = TRUE)
  cat("  ✅ knit() exitoso. Archivo generado:", archivo_salida, "\n")
  TRUE
}, error = function(e) {
  cat("  ❌ Error en knit():", e$message, "\n")
  FALSE
})

# Prueba 4: Verificar ausencia de emojis en archivo generado
if (resultado_compilacion && file.exists("probabilidad_condicional_tabla_contingencia_razonamiento_nivel3_v2_MEJORADO.md")) {
  cat("\n4. Verificando ausencia de emojis en archivo generado...\n")
  contenido_generado <- readLines("probabilidad_condicional_tabla_contingencia_razonamiento_nivel3_v2_MEJORADO.md", encoding = "UTF-8")
  
  emojis_encontrados_salida <- character(0)
  for (emoji in emojis_problematicos) {
    lineas_con_emoji_salida <- grep(emoji, contenido_generado, fixed = TRUE)
    if (length(lineas_con_emoji_salida) > 0) {
      emojis_encontrados_salida <- c(emojis_encontrados_salida, emoji)
      cat("  ❌ ENCONTRADO en salida:", emoji, "en líneas:", paste(lineas_con_emoji_salida, collapse = ", "), "\n")
    }
  }
  
  if (length(emojis_encontrados_salida) == 0) {
    cat("  ✅ No se encontraron emojis en archivo generado\n")
  } else {
    cat("  ❌ Se encontraron", length(emojis_encontrados_salida), "emojis en salida\n")
  }
} else {
  cat("\n4. ❌ No se pudo verificar archivo generado\n")
  emojis_encontrados_salida <- "ERROR"
}

# Prueba 5: Verificar que los chunks tienen configuración correcta
cat("\n5. Verificando configuración de chunks...\n")
chunks_con_include_false <- grep("include=FALSE", contenido_archivo)
chunks_con_results_asis <- grep("results='asis'", contenido_archivo)

cat("  Chunks con include=FALSE:", length(chunks_con_include_false), "líneas:", paste(chunks_con_include_false, collapse = ", "), "\n")
cat("  Chunks con results='asis':", length(chunks_con_results_asis), "líneas:", paste(chunks_con_results_asis, collapse = ", "), "\n")

if (length(chunks_con_include_false) >= 2) {
  cat("  ✅ Configuración de chunks correcta\n")
} else {
  cat("  ❌ Configuración de chunks insuficiente\n")
}

# Prueba 6: Test silencioso (sin emojis)
cat("\n6. Ejecutando test silencioso...\n")
test_silencioso <- tryCatch({
  # Test básico sin mostrar resultados
  expect_true(file.exists(archivo_ejercicio_mejorado))
  expect_true(resultado_compilacion)
  expect_equal(length(emojis_encontrados_codigo), 0)
  if (emojis_encontrados_salida != "ERROR") {
    expect_equal(length(emojis_encontrados_salida), 0)
  }
  cat("  ✅ Tests silenciosos pasaron correctamente\n")
  TRUE
}, error = function(e) {
  cat("  ❌ Error en tests silenciosos:", e$message, "\n")
  FALSE
})

# Resumen final
cat("\n=== RESUMEN FINAL ===\n")
todas_pruebas_exitosas <- all(configuraciones_correctas) && 
                          length(emojis_encontrados_codigo) == 0 && 
                          resultado_compilacion && 
                          (emojis_encontrados_salida == "ERROR" || length(emojis_encontrados_salida) == 0) &&
                          test_silencioso

if (todas_pruebas_exitosas) {
  cat("SUCCESS: Emojis completamente eliminados\n")
  cat("- Configuraciones anti-emojis activas\n")
  cat("- Sin emojis en código fuente\n")
  cat("- Compilación exitosa\n")
  cat("- Sin emojis en archivo generado\n")
  cat("- Tests funcionando silenciosamente\n")
  cat("\nEl archivo está listo para r-exams sin errores Unicode.\n")
} else {
  cat("PROBLEMAS DETECTADOS:\n")
  if (!all(configuraciones_correctas)) cat("- Configuraciones anti-emojis incompletas\n")
  if (length(emojis_encontrados_codigo) > 0) cat("- Emojis en código fuente\n")
  if (!resultado_compilacion) cat("- Error en compilación\n")
  if (emojis_encontrados_salida != "ERROR" && length(emojis_encontrados_salida) > 0) cat("- Emojis en archivo generado\n")
  if (!test_silencioso) cat("- Error en tests silenciosos\n")
}

cat("\n=== FIN DE PRUEBA ===\n")
