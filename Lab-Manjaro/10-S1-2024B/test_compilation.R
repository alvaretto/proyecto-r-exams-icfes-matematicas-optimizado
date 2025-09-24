# Script para probar la compilación y analizar opciones de respuesta
# del archivo area_cuadrado_rotado_geometrico_metrico_formulacion_ejecucion_n2_v1.Rmd

library(exams)
library(digest)

# El directorio de trabajo ya está configurado correctamente

# Función para analizar opciones de respuesta en múltiples versiones
analizar_opciones <- function(num_versiones = 10) {
  cat("=== ANÁLISIS DE OPCIONES DE RESPUESTA ===\n")
  
  opciones_todas <- list()
  problemas_detectados <- list()
  
  for (i in 1:num_versiones) {
    cat(paste("Analizando versión", i, "...\n"))
    
    # Generar una versión del ejercicio
    tryCatch({
      resultado <- exams2html(
        "area_cuadrado_rotado_geometrico_metrico_formulacion_ejecucion_n2_v1.Rmd",
        n = 1,
        name = paste0("test_version_", i),
        dir = "test_analysis",
        template = "plain.html",
        quiet = TRUE
      )
      
      # Leer el archivo HTML generado para extraer opciones
      archivo_html <- file.path("test_analysis", paste0("test_version_", i, "1.html"))
      if (file.exists(archivo_html)) {
        contenido <- readLines(archivo_html, warn = FALSE)
        
        # Buscar líneas que contengan las opciones de respuesta
        lineas_opciones <- grep("\\$x = .*\\$", contenido, value = TRUE)
        
        if (length(lineas_opciones) > 0) {
          opciones_todas[[i]] <- lineas_opciones
          
          # Verificar duplicados en esta versión
          if (length(unique(lineas_opciones)) < length(lineas_opciones)) {
            problemas_detectados[[length(problemas_detectados) + 1]] <- list(
              version = i,
              problema = "opciones_duplicadas",
              opciones = lineas_opciones
            )
          }
        }
      }
      
    }, error = function(e) {
      problemas_detectados[[length(problemas_detectados) + 1]] <- list(
        version = i,
        problema = "error_compilacion",
        mensaje = e$message
      )
    })
  }
  
  # Reportar resultados
  cat("\n=== RESULTADOS DEL ANÁLISIS ===\n")
  cat(paste("Versiones analizadas:", num_versiones, "\n"))
  cat(paste("Problemas detectados:", length(problemas_detectados), "\n\n"))
  
  if (length(problemas_detectados) > 0) {
    cat("PROBLEMAS ENCONTRADOS:\n")
    for (i in seq_along(problemas_detectados)) {
      problema <- problemas_detectados[[i]]
      cat(paste("- Versión", problema$version, ":", problema$problema, "\n"))
      if (problema$problema == "opciones_duplicadas") {
        cat("  Opciones:", paste(problema$opciones, collapse = " | "), "\n")
      } else if (problema$problema == "error_compilacion") {
        cat("  Error:", problema$mensaje, "\n")
      }
    }
  } else {
    cat("✅ No se detectaron problemas en las versiones analizadas.\n")
  }
  
  return(list(
    opciones_todas = opciones_todas,
    problemas = problemas_detectados
  ))
}

# Función para probar diversidad de versiones
probar_diversidad <- function(num_pruebas = 100) {
  cat("\n=== PRUEBA DE DIVERSIDAD DE VERSIONES ===\n")
  
  # Cargar el archivo Rmd para acceder a la función generar_datos
  source_rmd <- function(file) {
    temp_r <- tempfile(fileext = ".R")
    knitr::purl(file, output = temp_r, quiet = TRUE)
    source(temp_r)
    unlink(temp_r)
  }
  
  tryCatch({
    source_rmd("area_cuadrado_rotado_geometrico_metrico_formulacion_ejecucion_n2_v1.Rmd")
    
    versiones <- list()
    for(i in 1:num_pruebas) {
      datos_test <- generar_datos()
      versiones[[i]] <- digest::digest(datos_test)
    }
    
    n_versiones_unicas <- length(unique(versiones))
    cat(paste("Versiones únicas generadas:", n_versiones_unicas, "de", num_pruebas, "\n"))
    cat(paste("Porcentaje de diversidad:", round(n_versiones_unicas/num_pruebas * 100, 2), "%\n"))
    
    if (n_versiones_unicas >= 300 * (num_pruebas/1000)) {
      cat("✅ La diversidad cumple con el estándar requerido.\n")
    } else {
      cat("❌ La diversidad NO cumple con el estándar requerido.\n")
    }
    
  }, error = function(e) {
    cat("❌ Error al probar diversidad:", e$message, "\n")
  })
}

# Ejecutar análisis
cat("Iniciando análisis del archivo R-exams...\n\n")

# Probar compilación básica
cat("=== PRUEBA DE COMPILACIÓN BÁSICA ===\n")
tryCatch({
  resultado_basico <- exams2html(
    "area_cuadrado_rotado_geometrico_metrico_formulacion_ejecucion_n2_v1.Rmd",
    n = 1,
    name = "test_basico",
    dir = "test_basic",
    template = "plain.html",
    quiet = FALSE
  )
  cat("✅ Compilación básica exitosa.\n")
}, error = function(e) {
  cat("❌ Error en compilación básica:", e$message, "\n")
})

# Analizar opciones de respuesta
resultado_analisis <- analizar_opciones(10)

# Probar diversidad
probar_diversidad(100)

cat("\n=== ANÁLISIS COMPLETADO ===\n")
