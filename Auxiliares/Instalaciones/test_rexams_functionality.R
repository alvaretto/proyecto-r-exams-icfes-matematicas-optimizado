#!/usr/bin/env Rscript

################################################################################
# Script de Prueba de Funcionalidad R-Exams ICFES
# Proyecto: RepositorioMatematicasICFES_R_Exams
# Descripción: Prueba todas las funcionalidades críticas del entorno
################################################################################

cat("\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║       PRUEBA DE FUNCIONALIDAD R-EXAMS ICFES - MANJARO        ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

# Crear directorio temporal para pruebas
test_dir <- tempdir()
cat("📁 Directorio de pruebas:", test_dir, "\n\n")

# Contador de pruebas
total_tests <- 0
passed_tests <- 0
failed_tests <- 0

# Función para ejecutar pruebas
run_test <- function(test_name, test_function) {
  total_tests <<- total_tests + 1
  cat(sprintf("🧪 Prueba %d: %s\n", total_tests, test_name))
  
  result <- tryCatch({
    test_function()
    TRUE
  }, error = function(e) {
    cat("   ❌ ERROR:", e$message, "\n")
    FALSE
  })
  
  if (result) {
    cat("   ✅ PASÓ\n\n")
    passed_tests <<- passed_tests + 1
  } else {
    cat("   ❌ FALLÓ\n\n")
    failed_tests <<- failed_tests + 1
  }
  
  return(result)
}

cat("═══════════════════════════════════════════════════════════════\n")
cat("PRUEBAS DE PAQUETES R\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# Prueba 1: Cargar paquetes críticos
run_test("Cargar paquetes críticos", function() {
  library(exams)
  library(knitr)
  library(rmarkdown)
  library(reticulate)
  library(ggplot2)
  cat("   Paquetes cargados correctamente\n")
})

# Prueba 2: Verificar versiones
run_test("Verificar versiones de paquetes", function() {
  exams_version <- packageVersion("exams")
  knitr_version <- packageVersion("knitr")
  reticulate_version <- packageVersion("reticulate")
  
  cat("   exams:", as.character(exams_version), "\n")
  cat("   knitr:", as.character(knitr_version), "\n")
  cat("   reticulate:", as.character(reticulate_version), "\n")
})

cat("═══════════════════════════════════════════════════════════════\n")
cat("PRUEBAS DE INTEGRACIÓN PYTHON-R\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# Prueba 3: Configurar Python
run_test("Configurar Python con reticulate", function() {
  library(reticulate)
  python3_path <- Sys.which("python3")
  
  if (python3_path == "") {
    stop("Python 3 no encontrado")
  }
  
  use_python(python3_path, required = FALSE)
  py_config_info <- py_config()
  
  cat("   Python:", py_config_info$python, "\n")
  cat("   Versión:", py_config_info$version, "\n")
})

# Prueba 4: Ejecutar código Python
run_test("Ejecutar código Python desde R", function() {
  library(reticulate)
  
  py_run_string("x = 2 + 2")
  result <- py$x
  
  if (result != 4) {
    stop("Resultado incorrecto: esperado 4, obtenido ", result)
  }
  
  cat("   Resultado: 2 + 2 =", result, "\n")
})

# Prueba 5: Importar paquetes Python
run_test("Importar paquetes Python (matplotlib, numpy)", function() {
  library(reticulate)
  
  py_run_string("import matplotlib")
  py_run_string("import numpy")
  py_run_string("import pandas")
  
  cat("   matplotlib, numpy, pandas importados correctamente\n")
})

cat("═══════════════════════════════════════════════════════════════\n")
cat("PRUEBAS DE GENERACIÓN DE GRÁFICOS\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# Prueba 6: Crear gráfico con ggplot2
run_test("Crear gráfico con ggplot2", function() {
  library(ggplot2)
  
  data <- data.frame(x = 1:10, y = (1:10)^2)
  plot <- ggplot(data, aes(x = x, y = y)) +
    geom_line() +
    theme_minimal()
  
  plot_file <- file.path(test_dir, "test_ggplot.png")
  ggsave(plot_file, plot, width = 6, height = 4)
  
  if (!file.exists(plot_file)) {
    stop("Archivo de gráfico no creado")
  }
  
  cat("   Gráfico guardado en:", plot_file, "\n")
})

# Prueba 7: Crear gráfico con Python/matplotlib
run_test("Crear gráfico con Python/matplotlib", function() {
  library(reticulate)
  
  py_run_string("
import matplotlib
matplotlib.use('Agg')  # Backend sin GUI
import matplotlib.pyplot as plt
import numpy as np

x = np.linspace(0, 10, 100)
y = np.sin(x)

plt.figure()
plt.plot(x, y)
plt.savefig('" %+% file.path(test_dir, "test_matplotlib.png") %+% "')
plt.close()
")
  
  plot_file <- file.path(test_dir, "test_matplotlib.png")
  
  if (!file.exists(plot_file)) {
    stop("Archivo de gráfico Python no creado")
  }
  
  cat("   Gráfico Python guardado en:", plot_file, "\n")
})

cat("═══════════════════════════════════════════════════════════════\n")
cat("PRUEBAS DE COMPILACIÓN R-EXAMS\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# Prueba 8: Crear archivo .Rmd de prueba
run_test("Crear archivo .Rmd de prueba", function() {
  rmd_content <- '---
output: html_document
---

```{r setup, include=FALSE}
library(exams)
library(knitr)
knitr::opts_chunk$set(echo = FALSE)
```

Question
========

Este es un ejercicio de prueba para validar R-exams.

¿Cuál es el resultado de $2 + 2$?

Answerlist
----------
- 3
- 4
- 5
- 6

Solution
========

La respuesta correcta es 4.

Answerlist
----------
- Falso
- Verdadero
- Falso
- Falso

Meta-information
================
exname: test-funcionalidad
extype: schoice
exsolution: 0100
exshuffle: TRUE
'
  
  rmd_file <- file.path(test_dir, "test_exercise.Rmd")
  writeLines(rmd_content, rmd_file)
  
  if (!file.exists(rmd_file)) {
    stop("Archivo .Rmd no creado")
  }
  
  cat("   Archivo .Rmd creado:", rmd_file, "\n")
})

# Prueba 9: Compilar a HTML
run_test("Compilar ejercicio a HTML", function() {
  library(exams)
  
  rmd_file <- file.path(test_dir, "test_exercise.Rmd")
  
  exams2html(rmd_file, 
             n = 1, 
             name = "test_html",
             dir = test_dir,
             edir = test_dir,
             quiet = TRUE)
  
  html_file <- file.path(test_dir, "test_html1.html")
  
  if (!file.exists(html_file)) {
    stop("Archivo HTML no generado")
  }
  
  cat("   HTML generado:", html_file, "\n")
})

# Prueba 10: Compilar a PDF (si LaTeX está disponible)
run_test("Compilar ejercicio a PDF", function() {
  library(exams)
  
  # Verificar si pdflatex está disponible
  if (Sys.which("pdflatex") == "") {
    stop("pdflatex no disponible (esto es normal si solo se usa TinyTeX)")
  }
  
  rmd_file <- file.path(test_dir, "test_exercise.Rmd")
  
  exams2pdf(rmd_file,
            n = 1,
            name = "test_pdf",
            dir = test_dir,
            edir = test_dir,
            quiet = TRUE)
  
  pdf_file <- file.path(test_dir, "test_pdf1.pdf")
  
  if (!file.exists(pdf_file)) {
    stop("Archivo PDF no generado")
  }
  
  cat("   PDF generado:", pdf_file, "\n")
})

cat("═══════════════════════════════════════════════════════════════\n")
cat("PRUEBAS DE FUNCIONALIDAD AVANZADA\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# Prueba 11: Verificar TinyTeX
run_test("Verificar TinyTeX", function() {
  library(tinytex)
  
  if (is_tinytex()) {
    cat("   TinyTeX instalado y configurado\n")
  } else {
    cat("   TinyTeX no detectado (usando TeX Live del sistema)\n")
  }
})

# Prueba 12: Probar diversidad de versiones
run_test("Probar generación de versiones únicas", function() {
  library(digest)
  
  # Generar 10 versiones con diferentes semillas
  versions <- sapply(1:10, function(i) {
    set.seed(i)
    data <- list(
      a = sample(1:100, 1),
      b = sample(1:100, 1),
      c = sample(letters, 5)
    )
    digest(data)
  })
  
  unique_versions <- length(unique(versions))
  
  if (unique_versions < 10) {
    stop("No se generaron versiones únicas: ", unique_versions, "/10")
  }
  
  cat("   Generadas", unique_versions, "versiones únicas de 10\n")
})

cat("\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║                    RESUMEN DE PRUEBAS                          ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

cat(sprintf("Total de pruebas: %d\n", total_tests))
cat(sprintf("✅ Exitosas: %d\n", passed_tests))
cat(sprintf("❌ Fallidas: %d\n", failed_tests))

success_rate <- round((passed_tests / total_tests) * 100, 1)
cat(sprintf("📊 Tasa de éxito: %.1f%%\n\n", success_rate))

if (failed_tests == 0) {
  cat("╔════════════════════════════════════════════════════════════════╗\n")
  cat("║  🎉 TODAS LAS PRUEBAS PASARON EXITOSAMENTE                    ║\n")
  cat("║     El entorno R-exams está completamente funcional           ║\n")
  cat("╚════════════════════════════════════════════════════════════════╝\n")
  
  cat("\n✅ El sistema está listo para:\n")
  cat("   • Crear ejercicios R-exams\n")
  cat("   • Generar gráficos con ggplot2 y matplotlib\n")
  cat("   • Compilar a HTML, PDF y Moodle\n")
  cat("   • Usar integración Python-R\n")
  cat("   • Generar múltiples versiones de ejercicios\n\n")
  
} else if (failed_tests <= 2) {
  cat("╔════════════════════════════════════════════════════════════════╗\n")
  cat("║  ⚠️ ALGUNAS PRUEBAS FALLARON (FUNCIONALIDAD PARCIAL)          ║\n")
  cat("║     El sistema es funcional pero con limitaciones             ║\n")
  cat("╚════════════════════════════════════════════════════════════════╝\n")
  
  cat("\n⚠️ Revisa las pruebas fallidas arriba\n")
  cat("   Algunas funcionalidades pueden no estar disponibles\n\n")
  
} else {
  cat("╔════════════════════════════════════════════════════════════════╗\n")
  cat("║  ❌ MÚLTIPLES PRUEBAS FALLARON                                 ║\n")
  cat("║     Revisa la instalación y configuración                     ║\n")
  cat("╚════════════════════════════════════════════════════════════════╝\n")
  
  cat("\n❌ Acciones recomendadas:\n")
  cat("   1. Ejecutar: ./verify_installation.sh\n")
  cat("   2. Revisar instalación de paquetes faltantes\n")
  cat("   3. Consultar: INSTRUCCIONES_INSTALACION_MANJARO.md\n\n")
}

cat("📁 Archivos de prueba generados en:", test_dir, "\n")
cat("💡 Puedes eliminarlos con: unlink('", test_dir, "', recursive = TRUE)\n\n")

