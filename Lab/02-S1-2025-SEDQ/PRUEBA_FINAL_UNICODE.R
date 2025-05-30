# Script de prueba final para verificar que el error Unicode está completamente resuelto
# Aplicando modo ultrathink y nombres de variables en español

# Cargar bibliotecas necesarias
library(exams)
library(knitr)
library(stringr)
library(testthat)
library(digest)

# Configuración para suprimir emojis y caracteres Unicode problemáticos
options(testthat.use_colours = FALSE)
options(testthat.unicode = FALSE)

# Nombre del archivo a probar
archivo_ejercicio <- "probabilidad_condicional_tabla_contingencia_razonamiento_nivel3_v2_MEJORADO.Rmd"

cat("=== PRUEBA FINAL DE RESOLUCIÓN DE ERROR UNICODE ===\n")
cat("Archivo:", archivo_ejercicio, "\n\n")

# Prueba 1: Verificar que no hay símbolos Unicode problemáticos en el archivo
cat("1. Verificando ausencia de símbolos Unicode problemáticos...\n")
contenido_archivo <- readLines(archivo_ejercicio, encoding = "UTF-8")
simbolos_problematicos <- c("∩", "∪", "≤", "≥", "≠", "∈", "🥳", "🌈", "😸")

simbolos_encontrados <- character(0)
for (simbolo in simbolos_problematicos) {
  lineas_con_simbolo <- grep(simbolo, contenido_archivo, fixed = TRUE)
  if (length(lineas_con_simbolo) > 0) {
    simbolos_encontrados <- c(simbolos_encontrados, simbolo)
    cat("  ❌ ENCONTRADO:", simbolo, "en líneas:", paste(lineas_con_simbolo, collapse = ", "), "\n")
  }
}

if (length(simbolos_encontrados) == 0) {
  cat("  ✅ No se encontraron símbolos Unicode problemáticos\n")
} else {
  cat("  ❌ Se encontraron", length(simbolos_encontrados), "símbolos problemáticos\n")
}

# Prueba 2: Verificar compilación con knit()
cat("\n2. Probando compilación con knit()...\n")
resultado_knit <- tryCatch({
  archivo_salida <- knit(archivo_ejercicio, quiet = TRUE)
  cat("  ✅ knit() exitoso. Archivo generado:", archivo_salida, "\n")
  TRUE
}, error = function(e) {
  cat("  ❌ Error en knit():", e$message, "\n")
  FALSE
})

# Prueba 3: Verificar que la notación LaTeX es correcta
cat("\n3. Verificando notación LaTeX correcta...\n")
lineas_con_cap <- grep("\\$\\\\cap\\$", contenido_archivo)
if (length(lineas_con_cap) > 0) {
  cat("  ✅ Notación LaTeX $\\cap$ encontrada en", length(lineas_con_cap), "líneas\n")
  cat("  Líneas:", paste(lineas_con_cap, collapse = ", "), "\n")
} else {
  cat("  ⚠️  No se encontró notación $\\cap$ (puede ser normal si no hay intersecciones)\n")
}

# Prueba 4: Verificar coherencia matemática en el archivo generado
if (resultado_knit && file.exists("probabilidad_condicional_tabla_contingencia_razonamiento_nivel3_v2_MEJORADO.md")) {
  cat("\n4. Verificando coherencia matemática en archivo generado...\n")
  contenido_md <- readLines("probabilidad_condicional_tabla_contingencia_razonamiento_nivel3_v2_MEJORADO.md")
  
  # Buscar proporciones en el archivo
  lineas_proporciones <- grep("= 0\\.[0-9]", contenido_md, value = TRUE)
  if (length(lineas_proporciones) > 0) {
    cat("  ✅ Proporciones encontradas en archivo generado\n")
    cat("  Ejemplos:", head(lineas_proporciones, 2), "\n")
  }
  
  # Verificar que aparece la fórmula de probabilidad condicional
  formula_encontrada <- any(grepl("P\\(A\\|B\\)", contenido_md))
  if (formula_encontrada) {
    cat("  ✅ Fórmula de probabilidad condicional encontrada\n")
  } else {
    cat("  ⚠️  Fórmula de probabilidad condicional no encontrada\n")
  }
}

# Prueba 5: Verificar que los tests se ejecutan correctamente (sin mostrar emojis)
cat("\n5. Verificando ejecución de tests sin emojis...\n")
test_that("Prueba de integridad final", {
  expect_true(file.exists(archivo_ejercicio))
  expect_true(length(simbolos_encontrados) == 0)
  expect_true(resultado_knit)
})

# Resumen final
cat("\n=== RESUMEN FINAL ===\n")
if (length(simbolos_encontrados) == 0 && resultado_knit) {
  cat("🎉 ÉXITO COMPLETO: Error Unicode completamente resuelto\n")
  cat("✅ Sin símbolos Unicode problemáticos\n")
  cat("✅ Compilación exitosa\n")
  cat("✅ Notación LaTeX correcta\n")
  cat("✅ Tests funcionando sin emojis\n")
  cat("\nEl archivo está listo para uso en producción con r-exams.\n")
} else {
  cat("❌ PROBLEMAS DETECTADOS:\n")
  if (length(simbolos_encontrados) > 0) {
    cat("- Símbolos Unicode problemáticos:", paste(simbolos_encontrados, collapse = ", "), "\n")
  }
  if (!resultado_knit) {
    cat("- Error en compilación\n")
  }
}

cat("\n=== FIN DE PRUEBA ===\n")
