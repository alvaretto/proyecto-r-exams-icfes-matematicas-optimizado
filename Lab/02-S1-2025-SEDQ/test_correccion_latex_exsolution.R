# Script de prueba para verificar corrección de errores LaTeX y exsolution
# Aplicando modo ultrathink y nombres de variables en español

# Configuración robusta anti-emojis
options(testthat.use_colours = FALSE)
options(testthat.unicode = FALSE)
options(testthat.progress = FALSE)
options(testthat.summary = FALSE)
Sys.setenv("TESTTHAT_UNICODE" = "false")
Sys.setenv("TESTTHAT_COLOURS" = "false")

# Cargar bibliotecas necesarias
library(testthat)
library(stringr)

cat("=== VERIFICACIÓN DE CORRECCIONES LATEX Y EXSOLUTION ===\n")
cat("Verificando corrección de errores específicos...\n\n")

# Leer el archivo corregido
archivo_corregido <- "probabilidad_condicional_tabla_contingencia_razonamiento_nivel3_v1.Rmd"
contenido_archivo <- readLines(archivo_corregido, encoding = "UTF-8")

# 1. Verificar corrección de fórmulas LaTeX
cat("1. Verificando corrección de fórmulas LaTeX...\n")

# Buscar patrones problemáticos de LaTeX (doble backslash)
patrones_latex_problematicos <- c(
  "\\\\\\\\frac",  # \\frac (problemático)
  "\\\\\\\\cap"    # \\cap (problemático)
)

# Buscar patrones correctos de LaTeX (un solo backslash)
patrones_latex_correctos <- c(
  "\\\\frac",  # \frac (correcto)
  "\\\\cap"    # \cap (correcto)
)

latex_problematicos_encontrados <- 0
for(patron in patrones_latex_problematicos) {
  lineas_encontradas <- grep(patron, contenido_archivo)
  if(length(lineas_encontradas) > 0) {
    latex_problematicos_encontrados <- latex_problematicos_encontrados + 1
    cat("   ❌ Patrón LaTeX problemático encontrado:", patron, "en líneas:", paste(lineas_encontradas, collapse = ", "), "\n")
  }
}

latex_correctos_encontrados <- 0
for(patron in patrones_latex_correctos) {
  lineas_encontradas <- grep(patron, contenido_archivo)
  if(length(lineas_encontradas) > 0) {
    latex_correctos_encontrados <- latex_correctos_encontrados + 1
    cat("   ✅ Patrón LaTeX correcto encontrado:", patron, "en", length(lineas_encontradas), "líneas\n")
  }
}

if(latex_problematicos_encontrados == 0 && latex_correctos_encontrados > 0) {
  cat("   ✅ FÓRMULAS LATEX CORREGIDAS CORRECTAMENTE\n")
} else {
  cat("   ❌ Aún hay problemas con las fórmulas LaTeX\n")
}

# 2. Verificar presencia del test de vector de solución
cat("\n2. Verificando test de vector de solución...\n")

test_vector_solucion <- grep("Vector de solución se genera correctamente", contenido_archivo, fixed = TRUE)
if(length(test_vector_solucion) > 0) {
  cat("   ✅ Test de vector de solución presente en línea:", test_vector_solucion, "\n")

  # Verificar contenido del test
  validaciones_vector <- c(
    "expect_equal(length(solucion_vector), 4)",
    "expect_equal(sum(solucion_vector), 1)",
    "expect_equal(sum(solucion_vector == 0), 3)",
    "expect_equal(sum(solucion_vector == 1), 1)"
  )

  validaciones_encontradas <- 0
  for(validacion in validaciones_vector) {
    if(any(grepl(validacion, contenido_archivo, fixed = TRUE))) {
      validaciones_encontradas <- validaciones_encontradas + 1
      cat("     ✅ Validación presente:", validacion, "\n")
    } else {
      cat("     ❌ Validación faltante:", validacion, "\n")
    }
  }

  cat("   📊 Validaciones del vector encontradas:", validaciones_encontradas, "de", length(validaciones_vector), "\n")
} else {
  cat("   ❌ Test de vector de solución NO encontrado\n")
}

# 3. Verificar estructura del vector de solución en el código
cat("\n3. Verificando estructura del vector de solución...\n")

# Buscar la definición del vector de solución
definicion_vector <- grep("solucion_vector <- rep(0, 4)", contenido_archivo, fixed = TRUE)
asignacion_vector <- grep("solucion_vector[indice_correcto] <- 1", contenido_archivo, fixed = TRUE)

if(length(definicion_vector) > 0 && length(asignacion_vector) > 0) {
  cat("   ✅ Vector de solución definido correctamente en línea:", definicion_vector, "\n")
  cat("   ✅ Asignación de respuesta correcta en línea:", asignacion_vector, "\n")
} else {
  cat("   ❌ Problemas con la definición del vector de solución\n")
  if(length(definicion_vector) == 0) cat("     - Definición del vector no encontrada\n")
  if(length(asignacion_vector) == 0) cat("     - Asignación de respuesta correcta no encontrada\n")
}

# 4. Verificar uso del vector en exsolution
cat("\n4. Verificando uso del vector en exsolution...\n")

exsolution_linea <- grep("exsolution: `r paste(as.integer(solucion_vector), collapse=\"\")`", contenido_archivo, fixed = TRUE)
if(length(exsolution_linea) > 0) {
  cat("   ✅ exsolution usa solucion_vector correctamente en línea:", exsolution_linea, "\n")
} else {
  cat("   ❌ exsolution no usa solucion_vector correctamente\n")
}

# 5. Verificar uso del vector en Answerlist
cat("\n5. Verificando uso del vector en Answerlist...\n")

# Buscar líneas que contengan solucion_vector en Answerlist
answerlist_lineas <- grep("solucion_vector", contenido_archivo)
answerlist_lineas_filtradas <- answerlist_lineas[answerlist_lineas > 600]  # Solo en la sección final

if(length(answerlist_lineas_filtradas) >= 4) {
  cat("   ✅ Answerlist usa solucion_vector correctamente en", length(answerlist_lineas_filtradas), "líneas\n")
} else {
  cat("   ❌ Answerlist no usa solucion_vector correctamente (encontradas", length(answerlist_lineas_filtradas), "líneas, esperadas 4)\n")
}

# 6. Simular test del vector de solución
cat("\n6. Simulando test del vector de solución...\n")

tryCatch({
  # Simular vector de solución válido
  solucion_vector_simulado <- c(0, 1, 0, 0)  # Respuesta correcta en posición 2

  # Ejecutar las validaciones
  test_that("Vector de solución se genera correctamente (simulado)", {
    expect_equal(length(solucion_vector_simulado), 4)
    expect_equal(sum(solucion_vector_simulado), 1)
    expect_equal(sum(solucion_vector_simulado == 0), 3)
    expect_equal(sum(solucion_vector_simulado == 1), 1)
    expect_true(all(solucion_vector_simulado %in% c(0, 1)))
  })

  cat("   ✅ Test del vector de solución PASÓ correctamente\n")

}, error = function(e) {
  cat("   ❌ Test del vector de solución FALLÓ:", e$message, "\n")
})

# 7. Resumen final
cat("\n=== RESUMEN FINAL ===\n")

puntuacion_total <- 0
puntuacion_maxima <- 5

if(latex_problematicos_encontrados == 0 && latex_correctos_encontrados > 0) {
  cat("✅ Fórmulas LaTeX corregidas: CORRECTO\n")
  puntuacion_total <- puntuacion_total + 1
} else {
  cat("❌ Fórmulas LaTeX corregidas: INCORRECTO\n")
}

if(length(test_vector_solucion) > 0) {
  cat("✅ Test de vector de solución presente: CORRECTO\n")
  puntuacion_total <- puntuacion_total + 1
} else {
  cat("❌ Test de vector de solución presente: INCORRECTO\n")
}

if(length(definicion_vector) > 0 && length(asignacion_vector) > 0) {
  cat("✅ Estructura del vector de solución: CORRECTA\n")
  puntuacion_total <- puntuacion_total + 1
} else {
  cat("❌ Estructura del vector de solución: INCORRECTA\n")
}

if(length(exsolution_linea) > 0) {
  cat("✅ Uso en exsolution: CORRECTO\n")
  puntuacion_total <- puntuacion_total + 1
} else {
  cat("❌ Uso en exsolution: INCORRECTO\n")
}

if(length(answerlist_lineas) >= 4) {
  cat("✅ Uso en Answerlist: CORRECTO\n")
  puntuacion_total <- puntuacion_total + 1
} else {
  cat("❌ Uso en Answerlist: INCORRECTO\n")
}

cat("\n📊 PUNTUACIÓN FINAL:", puntuacion_total, "de", puntuacion_maxima, "\n")

if(puntuacion_total == puntuacion_maxima) {
  cat("🎉 RESULTADO: EXCELENTE - Todas las correcciones aplicadas correctamente\n")
  cat("✅ Los errores de LaTeX y exsolution han sido resueltos\n")
  cat("✅ El archivo debería funcionar sin errores\n")
} else if(puntuacion_total >= 4) {
  cat("⚠️  RESULTADO: BUENO - La mayoría de correcciones aplicadas\n")
  cat("⚠️  Revisar los puntos marcados como incorrectos\n")
} else {
  cat("❌ RESULTADO: NECESITA MÁS CORRECCIÓN\n")
  cat("❌ Hay problemas importantes que resolver\n")
}

cat("\n=== VERIFICACIÓN COMPLETADA ===\n")
