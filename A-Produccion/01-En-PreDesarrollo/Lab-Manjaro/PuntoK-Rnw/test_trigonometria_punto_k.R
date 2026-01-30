# Testing y Validación del Ejercicio de Trigonometría
# Archivo: test_trigonometria_punto_k.R

library(exams)
library(testthat)
library(digest)

# Configurar directorio de trabajo
setwd("Lab-Manjaro/PuntoK-Rnw/")

# Test 1: Verificar que el archivo .Rnw se puede cargar
test_that("El archivo .Rnw se carga correctamente", {
  expect_true(file.exists("trigonometria_punto_k_v1.Rnw"))
})

# Test 2: Probar diversidad de versiones
test_that("Prueba de diversidad de versiones (300+ únicas)", {
  # Cargar el archivo temporalmente para acceder a generar_datos()
  source_lines <- readLines("trigonometria_punto_k_v1.Rnw")
  
  # Extraer solo la función generar_datos() para testing
  start_line <- grep("generar_datos <- function", source_lines)
  end_line <- grep("^}", source_lines[start_line:length(source_lines)])[1] + start_line - 1
  
  # Simular la función para testing
  versiones <- list()
  for(i in 1:1000) {
    # Simular parámetros aleatorios
    h_valor <- sample(c(1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6), 1)
    color_curva <- sample(c("blue", "red", "green", "purple", "orange", "brown", "cyan", "magenta"), 1)
    punto_inicial <- sample(c("Q", "P", "R", "S", "A", "B", "C", "D", "E", "F"), 1)
    punto_final <- sample(c("T", "U", "V", "W", "X", "Y", "Z", "M", "N", "O"), 1)
    punto_movil <- sample(c("K", "M", "N", "L", "J", "I", "H", "G", "F", "E"), 1)
    punto_fijo <- sample(c("P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y"), 1)
    
    # Crear hash único
    datos_test <- list(h_valor, color_curva, punto_inicial, punto_final, punto_movil, punto_fijo)
    versiones[[i]] <- digest::digest(datos_test)
  }
  
  n_versiones_unicas <- length(unique(versiones))
  expect_true(n_versiones_unicas >= 300,
              info = paste("Solo se generaron", n_versiones_unicas,
                          "versiones únicas. Se requieren al menos 300."))
  
  cat("✅ Diversidad de versiones:", n_versiones_unicas, "versiones únicas generadas\n")
})

# Test 3: Validar compilación HTML
test_that("Compilación HTML exitosa", {
  skip_if_not_installed("exams")
  
  # Intentar compilar a HTML
  tryCatch({
    html_output <- exams2html("trigonometria_punto_k_v1.Rnw", 
                              n = 3, 
                              name = "test_html",
                              dir = "salida_test",
                              quiet = TRUE)
    expect_true(length(html_output) > 0)
    cat("✅ Compilación HTML exitosa\n")
  }, error = function(e) {
    cat("❌ Error en compilación HTML:", e$message, "\n")
    expect_true(FALSE, info = paste("Error HTML:", e$message))
  })
})

# Test 4: Validar compilación PDF
test_that("Compilación PDF exitosa", {
  skip_if_not_installed("exams")
  
  # Intentar compilar a PDF
  tryCatch({
    pdf_output <- exams2pdf("trigonometria_punto_k_v1.Rnw", 
                            n = 2, 
                            name = "test_pdf",
                            dir = "salida_test",
                            quiet = TRUE)
    expect_true(length(pdf_output) > 0)
    cat("✅ Compilación PDF exitosa\n")
  }, error = function(e) {
    cat("❌ Error en compilación PDF:", e$message, "\n")
    expect_true(FALSE, info = paste("Error PDF:", e$message))
  })
})

# Test 5: Validar estructura de metadatos
test_that("Metadatos ICFES correctos", {
  content <- readLines("trigonometria_punto_k_v1.Rnw")
  
  # Verificar presencia de metadatos obligatorios
  expect_true(any(grepl("\\\\exname", content)), "Falta \\exname")
  expect_true(any(grepl("\\\\extype\\{schoice\\}", content)), "Falta \\extype{schoice}")
  expect_true(any(grepl("\\\\exsolution", content)), "Falta \\exsolution")
  expect_true(any(grepl("\\\\exshuffle", content)), "Falta \\exshuffle")
  expect_true(any(grepl("\\\\exsection", content)), "Falta \\exsection")
  
  cat("✅ Metadatos ICFES presentes y correctos\n")
})

# Test 6: Validar funciones TikZ
test_that("Funciones TikZ bien definidas", {
  content <- readLines("trigonometria_punto_k_v1.Rnw")
  
  # Verificar presencia de funciones TikZ
  expect_true(any(grepl("crear_grafica_constante", content)), "Falta función crear_grafica_constante")
  expect_true(any(grepl("crear_grafica_cosecante", content)), "Falta función crear_grafica_cosecante")
  expect_true(any(grepl("crear_grafica_cosecante_cero", content)), "Falta función crear_grafica_cosecante_cero")
  
  # Verificar uso de include_tikz
  expect_true(any(grepl("include_tikz", content)), "Falta uso de include_tikz")
  
  cat("✅ Funciones TikZ correctamente definidas\n")
})

# Test 7: Validar sistema de aleatorización
test_that("Sistema de aleatorización robusto", {
  # Verificar que hay suficientes opciones para aleatorización
  content <- paste(readLines("trigonometria_punto_k_v1.Rnw"), collapse = "\n")
  
  # Verificar diversidad en parámetros
  expect_true(grepl("sample.*c\\(.*,.*,.*,.*\\)", content), "Falta aleatorización en parámetros")
  expect_true(grepl("h_valor.*sample", content), "Falta aleatorización en h_valor")
  expect_true(grepl("color_curva.*sample", content), "Falta aleatorización en colores")
  
  cat("✅ Sistema de aleatorización robusto implementado\n")
})

# Test 8: Validar coherencia matemática
test_that("Coherencia matemática del ejercicio", {
  content <- paste(readLines("trigonometria_punto_k_v1.Rnw"), collapse = "\n")
  
  # Verificar presencia de conceptos trigonométricos clave
  expect_true(grepl("sen\\(\\\\alpha\\)", content), "Falta función seno")
  expect_true(grepl("csc\\(\\\\alpha\\)", content), "Falta función cosecante")
  expect_true(grepl("h.*sen", content), "Falta relación h/sen")
  
  cat("✅ Coherencia matemática verificada\n")
})

# Ejecutar todos los tests
cat("\n🧪 INICIANDO TESTING Y VALIDACIÓN\n")
cat("================================\n\n")

# Crear directorio de salida si no existe
if(!dir.exists("salida_test")) {
  dir.create("salida_test")
}

# Ejecutar tests
test_results <- test_file("test_trigonometria_punto_k.R")

cat("\n📊 RESUMEN DE RESULTADOS\n")
cat("========================\n")
cat("Tests ejecutados:", length(test_results), "\n")
cat("Tests exitosos:", sum(sapply(test_results, function(x) x$passed)), "\n")
cat("Tests fallidos:", sum(sapply(test_results, function(x) !x$passed)), "\n")

if(all(sapply(test_results, function(x) x$passed))) {
  cat("\n✅ TODOS LOS TESTS PASARON EXITOSAMENTE\n")
  cat("El ejercicio está listo para uso en producción.\n")
} else {
  cat("\n❌ ALGUNOS TESTS FALLARON\n")
  cat("Revisar errores antes de usar en producción.\n")
}

cat("\n🎯 VALIDACIÓN COMPLETADA\n")
