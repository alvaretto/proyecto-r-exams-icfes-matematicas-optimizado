# ===============================================================================
# SCRIPT DE TESTING COMPLETO PARA tikz-cloze.Rmd
# ===============================================================================
# Archivo: test_tikz_cloze.R
# Propósito: Validación integral del ejercicio ICFES optimizado
# Autor: Sistema de IA Augment Agent
# Fecha: Septiembre 2025
# ===============================================================================

# Configuración inicial
library(exams)
library(knitr)

# Configuración de testing
config_test <- list(
  archivo = "tikz-cloze.Rmd",
  versiones_test = 10,
  dir_output = "test_output_tikz",
  semilla = 12345
)

cat("🚀 INICIANDO TESTING COMPLETO DE tikz-cloze.Rmd\n")
cat(paste(rep("=", 60), collapse=""), "\n")

# ===============================================================================
# TEST 1: VALIDACIÓN DE COMPILACIÓN
# ===============================================================================

cat("\n📋 TEST 1: Validación de Compilación R Markdown\n")
cat(paste(rep("-", 50), collapse = ""), "\n")

test_compilacion <- function() {
  tryCatch({
    set.seed(config_test$semilla)
    resultado <- knit(config_test$archivo, quiet = TRUE)
    cat("✅ Compilación R Markdown: EXITOSA\n")
    return(TRUE)
  }, error = function(e) {
    cat("❌ Error en compilación:", e$message, "\n")
    return(FALSE)
  })
}

compilacion_ok <- test_compilacion()

# ===============================================================================
# TEST 2: VALIDACIÓN DE DATOS GENERADOS
# ===============================================================================

cat("\n📊 TEST 2: Validación de Datos Estadísticos\n")
cat(paste(rep("-", 50), collapse = ""), "\n")

test_datos_estadisticos <- function(n_tests = 20) {
  errores <- 0
  
  for (i in 1:n_tests) {
    tryCatch({
      set.seed(i * 100)
      source(config_test$archivo, local = TRUE)
      
      # Validaciones matemáticas
      suma_prob <- respuesta_1 + respuesta_2 + respuesta_3
      if (abs(suma_prob - 1.0) > 0.01) {
        cat("❌ Test", i, ": Suma probabilidades =", suma_prob, "\n")
        errores <- errores + 1
      }
      
      # Validar orden de percentiles
      if (!(respuesta_4 < respuesta_6 && respuesta_6 < respuesta_5)) {
        cat("❌ Test", i, ": Orden percentiles incorrecto\n")
        errores <- errores + 1
      }
      
      # Validar longitud de soluciones
      if (length(solucion_completa) != 10) {
        cat("❌ Test", i, ": Longitud solución =", length(solucion_completa), "\n")
        errores <- errores + 1
      }
      
    }, error = function(e) {
      cat("❌ Test", i, ": Error en generación -", e$message, "\n")
      errores <- errores + 1
    })
  }
  
  if (errores == 0) {
    cat("✅ Validación de datos: TODOS LOS TESTS PASARON (", n_tests, "/", n_tests, ")\n")
  } else {
    cat("⚠️  Validación de datos: ", errores, " errores de ", n_tests, " tests\n")
  }
  
  return(errores == 0)
}

datos_ok <- test_datos_estadisticos()

# ===============================================================================
# TEST 3: GENERACIÓN DE FORMATOS MÚLTIPLES
# ===============================================================================

cat("\n🎯 TEST 3: Generación de Formatos Múltiples\n")
cat(paste(rep("-", 50), collapse = ""), "\n")

test_formatos <- function() {
  resultados <- list()
  
  # Crear directorio de salida
  if (!dir.exists(config_test$dir_output)) {
    dir.create(config_test$dir_output, recursive = TRUE)
  }
  
  # Test HTML
  cat("🔍 Testing formato HTML...\n")
  tryCatch({
    set.seed(config_test$semilla)
    exams2html(config_test$archivo, n = 2, 
               name = "test_html", 
               dir = config_test$dir_output)
    cat("✅ HTML: EXITOSO\n")
    resultados$html <- TRUE
  }, error = function(e) {
    cat("❌ HTML: Error -", e$message, "\n")
    resultados$html <- FALSE
  })
  
  # Test PDF (si está disponible)
  cat("🔍 Testing formato PDF...\n")
  tryCatch({
    set.seed(config_test$semilla)
    exams2pdf(config_test$archivo, n = 1, 
              name = "test_pdf", 
              dir = config_test$dir_output)
    cat("✅ PDF: EXITOSO\n")
    resultados$pdf <- TRUE
  }, error = function(e) {
    cat("⚠️  PDF: No disponible -", e$message, "\n")
    resultados$pdf <- FALSE
  })
  
  # Test Moodle XML
  cat("🔍 Testing formato Moodle...\n")
  tryCatch({
    set.seed(config_test$semilla)
    exams2moodle(config_test$archivo, n = 1, 
                 name = "test_moodle", 
                 dir = config_test$dir_output)
    cat("✅ Moodle: EXITOSO\n")
    resultados$moodle <- TRUE
  }, error = function(e) {
    cat("❌ Moodle: Error -", e$message, "\n")
    resultados$moodle <- FALSE
  })
  
  return(resultados)
}

formatos_ok <- test_formatos()

# ===============================================================================
# TEST 4: ANÁLISIS DE VARIABILIDAD
# ===============================================================================

cat("\n🔄 TEST 4: Análisis de Variabilidad de Versiones\n")
cat(paste(rep("-", 50), collapse = ""), "\n")

test_variabilidad <- function(n_versions = 50) {
  parametros_unicos <- list()
  contextos_unicos <- c()
  
  for (i in 1:n_versions) {
    tryCatch({
      set.seed(i * 123)
      source(config_test$archivo, local = TRUE)
      
      # Recopilar parámetros únicos
      param_key <- paste(datos$alfa, datos$beta, datos$tipo_evaluacion, sep = "_")
      parametros_unicos[[param_key]] <- TRUE
      
      contextos_unicos <- c(contextos_unicos, datos$tipo_evaluacion)
      
    }, error = function(e) {
      cat("⚠️  Error en versión", i, ":", e$message, "\n")
    })
  }
  
  n_parametros_unicos <- length(parametros_unicos)
  n_contextos_unicos <- length(unique(contextos_unicos))
  
  cat("📊 Parámetros únicos generados:", n_parametros_unicos, "de", n_versions, "\n")
  cat("📊 Contextos únicos:", n_contextos_unicos, "\n")
  cat("📊 Variabilidad:", round(n_parametros_unicos/n_versions * 100, 1), "%\n")
  
  if (n_parametros_unicos >= n_versions * 0.8) {
    cat("✅ Variabilidad: EXCELENTE (>80%)\n")
    return(TRUE)
  } else if (n_parametros_unicos >= n_versions * 0.6) {
    cat("⚠️  Variabilidad: BUENA (60-80%)\n")
    return(TRUE)
  } else {
    cat("❌ Variabilidad: INSUFICIENTE (<60%)\n")
    return(FALSE)
  }
}

variabilidad_ok <- test_variabilidad()

# ===============================================================================
# TEST 5: VALIDACIÓN DE TOLERANCIAS CLOZE
# ===============================================================================

cat("\n🎯 TEST 5: Validación de Sistema Cloze\n")
cat(paste(rep("-", 50), collapse = ""), "\n")

test_cloze <- function() {
  tryCatch({
    set.seed(config_test$semilla)
    source(config_test$archivo, local = TRUE)
    
    # Verificar tipos de respuesta
    tipos_esperados <- c("num", "num", "num", "num", "num", "num", "num", "num", "num", "schoice")
    if (!identical(tipos_respuesta, tipos_esperados)) {
      cat("❌ Tipos de respuesta incorrectos\n")
      return(FALSE)
    }
    
    # Verificar tolerancias
    tolerancias_esperadas <- c(0.003, 0.003, 0.003, 0.5, 0.5, 0.5, 0.003, 0.5, 0, 0)
    if (!identical(tolerancias, tolerancias_esperadas)) {
      cat("❌ Tolerancias incorrectas\n")
      return(FALSE)
    }
    
    # Verificar formato de solución schoice
    schoice_pattern <- mchoice2string(solucion_schoice)
    if (!grepl("^[01]{4}$", schoice_pattern)) {
      cat("❌ Formato schoice incorrecto:", schoice_pattern, "\n")
      return(FALSE)
    }
    
    cat("✅ Sistema cloze: CONFIGURACIÓN CORRECTA\n")
    cat("  - 10 elementos (9 num + 1 schoice)\n")
    cat("  - Tolerancias apropiadas\n")
    cat("  - Formato schoice válido\n")
    
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Error en validación cloze:", e$message, "\n")
    return(FALSE)
  })
}

cloze_ok <- test_cloze()

# ===============================================================================
# RESUMEN FINAL
# ===============================================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("📋 RESUMEN FINAL DE TESTING\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

tests_pasados <- sum(c(compilacion_ok, datos_ok, 
                      formatos_ok$html, formatos_ok$moodle,
                      variabilidad_ok, cloze_ok))
total_tests <- 6

cat("✅ Tests pasados:", tests_pasados, "de", total_tests, "\n")
cat("📊 Tasa de éxito:", round(tests_pasados/total_tests * 100, 1), "%\n")

if (tests_pasados == total_tests) {
  cat("🎉 RESULTADO: tikz-cloze.Rmd COMPLETAMENTE FUNCIONAL\n")
  cat("🚀 Listo para uso en producción\n")
} else if (tests_pasados >= total_tests * 0.8) {
  cat("⚠️  RESULTADO: tikz-cloze.Rmd MAYORMENTE FUNCIONAL\n")
  cat("🔧 Requiere ajustes menores\n")
} else {
  cat("❌ RESULTADO: tikz-cloze.Rmd REQUIERE CORRECCIONES\n")
  cat("🛠️  Revisar errores antes de usar\n")
}

cat("\n📁 Archivos de prueba generados en:", config_test$dir_output, "\n")
cat("🔍 Revisar archivos HTML para validación visual\n")

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("🎯 TESTING COMPLETADO\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
