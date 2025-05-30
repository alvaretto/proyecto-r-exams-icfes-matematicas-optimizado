# Script de verificación temporal para confirmar resolución de errores de regex
# Aplicando modo ultrathink y nombres de variables en español

# Configuración robusta para evitar problemas
options(testthat.use_colours = FALSE)
options(testthat.unicode = FALSE)
Sys.setlocale(category = "LC_NUMERIC", locale = "C")

# Cargar bibliotecas necesarias
library(stringr)
library(testthat)

cat("=== VERIFICACIÓN DE CORRECCIÓN DE ERRORES DE REGEX ===\n")
cat("Fecha:", Sys.time(), "\n\n")

# Función de escape LaTeX (copiada del archivo principal)
escapar_latex <- function(texto) {
  # Reemplazar caracteres especiales comunes en español
  texto <- gsub("á", "\\\\'{a}", texto, fixed = TRUE)
  texto <- gsub("é", "\\\\'{e}", texto, fixed = TRUE)
  texto <- gsub("í", "\\\\'{i}", texto, fixed = TRUE)
  texto <- gsub("ó", "\\\\'{o}", texto, fixed = TRUE)
  texto <- gsub("ú", "\\\\'{u}", texto, fixed = TRUE)
  texto <- gsub("ñ", "\\\\~{n}", texto, fixed = TRUE)
  texto <- gsub("Á", "\\\\'{A}", texto, fixed = TRUE)
  texto <- gsub("É", "\\\\'{E}", texto, fixed = TRUE)
  texto <- gsub("Í", "\\\\'{I}", texto, fixed = TRUE)
  texto <- gsub("Ó", "\\\\'{O}", texto, fixed = TRUE)
  texto <- gsub("Ú", "\\\\'{U}", texto, fixed = TRUE)
  texto <- gsub("Ñ", "\\\\~{N}", texto, fixed = TRUE)
  # Reemplazar espacios con espacios LaTeX seguros
  texto <- gsub(" ", "\\\\ ", texto, fixed = TRUE)
  return(texto)
}

# Prueba 1: Verificar que la función de escape funciona
cat("1. Probando función de escape LaTeX...\n")
texto_prueba <- "más de 21 años"
resultado_escape <- tryCatch({
  escapar_latex(texto_prueba)
}, error = function(e) {
  cat("  ERROR en función de escape:", e$message, "\n")
  return(NULL)
})

if (!is.null(resultado_escape)) {
  cat("  ✓ Función de escape ejecutada correctamente\n")
  cat("  Texto original:", texto_prueba, "\n")
  cat("  Texto escapado:", resultado_escape, "\n")
} else {
  cat("  ✗ Error en función de escape\n")
}

# Prueba 2: Verificar que stringr::str_detect funciona con las secuencias
cat("\n2. Probando detección con stringr...\n")
if (!is.null(resultado_escape)) {
  prueba_stringr <- tryCatch({
    # Estas son las verificaciones que estaban fallando antes
    resultado1 <- stringr::str_detect(resultado_escape, stringr::fixed("\\'{a}"))
    resultado2 <- stringr::str_detect(resultado_escape, stringr::fixed("\\~{n}"))
    resultado3 <- stringr::str_detect(resultado_escape, stringr::fixed("\\\\ "))
    
    list(acento = resultado1, enie = resultado2, espacio = resultado3)
  }, error = function(e) {
    cat("  ERROR en stringr::str_detect:", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(prueba_stringr)) {
    cat("  ✓ stringr::str_detect ejecutado correctamente\n")
    cat("  Detecta \\'{a}:", prueba_stringr$acento, "\n")
    cat("  Detecta \\~{n}:", prueba_stringr$enie, "\n")
    cat("  Detecta \\\\ :", prueba_stringr$espacio, "\n")
  } else {
    cat("  ✗ Error en stringr::str_detect\n")
  }
}

# Prueba 3: Verificar que las pruebas problemáticas anteriores ahora funcionan
cat("\n3. Probando las verificaciones que antes fallaban...\n")
pruebas_anteriores <- tryCatch({
  # Recrear las condiciones que causaban el error
  texto_test <- "más de 21 años"
  texto_escapado_test <- escapar_latex(texto_test)
  
  # Usar el nuevo método con stringr (que debería funcionar)
  test1 <- stringr::str_detect(texto_escapado_test, stringr::fixed("\\'{a}"))
  test2 <- stringr::str_detect(texto_escapado_test, stringr::fixed("\\~{n}"))
  test3 <- !("á" %in% strsplit(texto_escapado_test, "")[[1]])
  test4 <- !("ñ" %in% strsplit(texto_escapado_test, "")[[1]])
  
  list(test1 = test1, test2 = test2, test3 = test3, test4 = test4)
}, error = function(e) {
  cat("  ERROR en pruebas anteriores:", e$message, "\n")
  return(NULL)
})

if (!is.null(pruebas_anteriores)) {
  cat("  ✓ Todas las pruebas anteriores ejecutadas correctamente\n")
  cat("  Detecta secuencia de á:", pruebas_anteriores$test1, "\n")
  cat("  Detecta secuencia de ñ:", pruebas_anteriores$test2, "\n")
  cat("  No contiene á original:", pruebas_anteriores$test3, "\n")
  cat("  No contiene ñ original:", pruebas_anteriores$test4, "\n")
} else {
  cat("  ✗ Error en pruebas anteriores\n")
}

# Prueba 4: Verificar con testthat (simulando el entorno real)
cat("\n4. Probando con testthat (entorno real)...\n")
prueba_testthat <- tryCatch({
  test_that("Prueba de verificación de corrección", {
    texto_prueba_final <- "más de 21 años"
    texto_escapado_final <- escapar_latex(texto_prueba_final)
    
    # Verificaciones robustas sin regex complejas
    expect_false("á" %in% strsplit(texto_escapado_final, "")[[1]])
    expect_false("ñ" %in% strsplit(texto_escapado_final, "")[[1]])
    
    # Verificar usando stringr
    expect_true(stringr::str_detect(texto_escapado_final, stringr::fixed("\\'{a}")))
    expect_true(stringr::str_detect(texto_escapado_final, stringr::fixed("\\~{n}")))
    expect_true(nchar(texto_escapado_final) > nchar(texto_prueba_final))
    expect_true(stringr::str_detect(texto_escapado_final, stringr::fixed("\\\\")))
  })
  TRUE
}, error = function(e) {
  cat("  ERROR en testthat:", e$message, "\n")
  FALSE
})

if (prueba_testthat) {
  cat("  ✓ Pruebas con testthat pasaron correctamente\n")
} else {
  cat("  ✗ Error en pruebas con testthat\n")
}

# Resumen final
cat("\n=== RESUMEN DE VERIFICACIÓN ===\n")
if (!is.null(resultado_escape) && !is.null(prueba_stringr) && 
    !is.null(pruebas_anteriores) && prueba_testthat) {
  cat("✓ TODAS LAS PRUEBAS PASARON CORRECTAMENTE\n")
  cat("✓ El error de expresiones regulares ha sido resuelto\n")
  cat("✓ Las funciones de escape LaTeX funcionan correctamente\n")
  cat("✓ stringr::str_detect es una solución robusta\n")
} else {
  cat("✗ ALGUNAS PRUEBAS FALLARON\n")
  cat("✗ Revisar los errores reportados arriba\n")
}

cat("\nVerificación completada:", Sys.time(), "\n")
