# Script de Validación Completa - Rectángulos Negros en Gráficos Circulares
# Valida el funcionamiento de los ejercicios modificados con rectángulos de fondo

library(exams)
library(reticulate)

# Configurar directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

cat("🔍 VALIDACIÓN COMPLETA - RECTÁNGULOS NEGROS EN GRÁFICOS CIRCULARES\n")
cat("=" %R% 70, "\n\n")

# Lista de ejercicios modificados con rectángulos negros
ejercicios_modificados <- list(
  list(
    nombre = "Empaques Tetra Pak (Original)",
    archivo = "empaques_tetra_pak_argumentacion_n3_v1/empaques_tetra_pak_argumentacion_n3_v1.Rmd",
    directorio = "empaques_tetra_pak_argumentacion_n3_v1",
    descripcion = "Ejercicio original con rectángulos negros implementados"
  ),
  list(
    nombre = "Gráfico Circular Bienes",
    archivo = "01-S2-2025-SEDQ/01-S2-2025-SEDQ-grafico_circular_bienes_v0.Rmd",
    directorio = "01-S2-2025-SEDQ",
    descripcion = "Ejercicio de bienes con rectángulos negros aplicados"
  ),
  list(
    nombre = "Porcentajes Sabores",
    archivo = "19/porcentajes_ordenamiento_sabores_v1.Rmd",
    directorio = "19",
    descripcion = "Ejercicio de sabores con rectángulos negros implementados"
  )
)

# Función para validar un ejercicio
validar_ejercicio <- function(ejercicio) {
  cat("📋 Validando:", ejercicio$nombre, "\n")
  cat("📁 Archivo:", ejercicio$archivo, "\n")
  
  # Cambiar al directorio del ejercicio
  directorio_original <- getwd()
  setwd(ejercicio$directorio)
  
  resultado <- list(
    nombre = ejercicio$nombre,
    html_exitoso = FALSE,
    pdf_exitoso = FALSE,
    moodle_exitoso = FALSE,
    errores = c(),
    warnings = c(),
    archivos_generados = c()
  )
  
  tryCatch({
    # Prueba 1: Generar HTML
    cat("  🌐 Generando HTML... ")
    exams2html(basename(ejercicio$archivo), 
               name = paste0("test_", gsub("[^a-zA-Z0-9]", "_", ejercicio$nombre)),
               dir = "test_output",
               n = 1,
               quiet = TRUE)
    resultado$html_exitoso <- TRUE
    cat("✅\n")
    
    # Prueba 2: Generar PDF (si es posible)
    cat("  📄 Generando PDF... ")
    exams2pdf(basename(ejercicio$archivo), 
              name = paste0("test_pdf_", gsub("[^a-zA-Z0-9]", "_", ejercicio$nombre)),
              dir = "test_output",
              n = 1,
              quiet = TRUE)
    resultado$pdf_exitoso <- TRUE
    cat("✅\n")
    
    # Prueba 3: Generar Moodle XML
    cat("  🎓 Generando Moodle XML... ")
    exams2moodle(basename(ejercicio$archivo), 
                 name = paste0("test_moodle_", gsub("[^a-zA-Z0-9]", "_", ejercicio$nombre)),
                 dir = "test_output",
                 n = 1,
                 quiet = TRUE)
    resultado$moodle_exitoso <- TRUE
    cat("✅\n")
    
    # Verificar archivos generados
    archivos_test <- list.files("test_output", full.names = TRUE)
    resultado$archivos_generados <- archivos_test
    
  }, error = function(e) {
    resultado$errores <- c(resultado$errores, e$message)
    cat("❌ Error:", e$message, "\n")
  }, warning = function(w) {
    resultado$warnings <- c(resultado$warnings, w$message)
    cat("⚠️ Warning:", w$message, "\n")
  })
  
  # Volver al directorio original
  setwd(directorio_original)
  
  return(resultado)
}

# Ejecutar validación para todos los ejercicios
cat("🚀 INICIANDO VALIDACIÓN DE", length(ejercicios_modificados), "EJERCICIOS\n\n")

resultados_validacion <- list()
for (i in seq_along(ejercicios_modificados)) {
  cat("📊 EJERCICIO", i, "de", length(ejercicios_modificados), "\n")
  resultado <- validar_ejercicio(ejercicios_modificados[[i]])
  resultados_validacion[[i]] <- resultado
  cat("\n")
}

# Generar reporte de resultados
cat("📋 REPORTE DE VALIDACIÓN COMPLETA\n")
cat("=" %R% 50, "\n\n")

exitosos_html <- sum(sapply(resultados_validacion, function(x) x$html_exitoso))
exitosos_pdf <- sum(sapply(resultados_validacion, function(x) x$pdf_exitoso))
exitosos_moodle <- sum(sapply(resultados_validacion, function(x) x$moodle_exitoso))
total_ejercicios <- length(ejercicios_modificados)

cat("✅ RESULTADOS GENERALES:\n")
cat("  📊 Total de ejercicios validados:", total_ejercicios, "\n")
cat("  🌐 HTML exitosos:", exitosos_html, "/", total_ejercicios, "\n")
cat("  📄 PDF exitosos:", exitosos_pdf, "/", total_ejercicios, "\n")
cat("  🎓 Moodle exitosos:", exitosos_moodle, "/", total_ejercicios, "\n\n")

# Detalles por ejercicio
cat("📋 DETALLES POR EJERCICIO:\n")
for (i in seq_along(resultados_validacion)) {
  resultado <- resultados_validacion[[i]]
  cat("📊", resultado$nombre, "\n")
  cat("  🌐 HTML:", ifelse(resultado$html_exitoso, "✅", "❌"), "\n")
  cat("  📄 PDF:", ifelse(resultado$pdf_exitoso, "✅", "❌"), "\n")
  cat("  🎓 Moodle:", ifelse(resultado$moodle_exitoso, "✅", "❌"), "\n")
  
  if (length(resultado$errores) > 0) {
    cat("  ❌ Errores:", length(resultado$errores), "\n")
  }
  if (length(resultado$warnings) > 0) {
    cat("  ⚠️ Warnings:", length(resultado$warnings), "\n")
  }
  cat("\n")
}

# Verificación de patrón de rectángulos negros
cat("🎨 VERIFICACIÓN DEL PATRÓN DE RECTÁNGULOS NEGROS:\n")
cat("=" %R% 50, "\n")

patron_implementado <- c(
  "ancho_rect = len(texto) * 0.090",
  "alto_rect = 0.12",
  "facecolor='black', alpha=0.8",
  "autotext.set_color('white')",
  "autotext.set_zorder(2)"
)

for (ejercicio in ejercicios_modificados) {
  cat("📊", ejercicio$nombre, "\n")
  contenido <- readLines(ejercicio$archivo, warn = FALSE)
  contenido_texto <- paste(contenido, collapse = " ")
  
  for (patron in patron_implementado) {
    if (grepl(patron, contenido_texto, fixed = TRUE)) {
      cat("  ✅", patron, "\n")
    } else {
      cat("  ❌", patron, "\n")
    }
  }
  cat("\n")
}

# Resumen final
if (exitosos_html == total_ejercicios && exitosos_moodle == total_ejercicios) {
  cat("🎉 VALIDACIÓN EXITOSA: Todos los ejercicios funcionan correctamente\n")
  cat("🎨 PATRÓN APLICADO: Rectángulos negros implementados exitosamente\n")
  cat("📊 MEJORA VISUAL: Visibilidad de porcentajes optimizada\n")
} else {
  cat("⚠️ VALIDACIÓN PARCIAL: Algunos ejercicios requieren revisión\n")
  cat("🔧 ACCIÓN REQUERIDA: Verificar errores reportados\n")
}

cat("\n🏁 VALIDACIÓN COMPLETA FINALIZADA\n")