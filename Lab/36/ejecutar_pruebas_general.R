# ejecutar_pruebas_general.R
# Script para realizar pruebas unitarias exhaustivas a ejercicios R-exams

library(testthat)
library(rmarkdown)
library(exams)

# Función para analizar la coherencia visual de gráficas
analizar_coherencia_visual <- function(archivo_rmd, patron_imagenes = "*.png", n_simulaciones = 5) {
  # Cargar bibliotecas necesarias para análisis de imágenes
  if (!requireNamespace("png", quietly = TRUE)) {
    install.packages("png")
    library(png)
  }

  # Crear directorio para guardar imágenes de prueba si no existe
  dir_pruebas <- "pruebas_imagenes"
  if (!dir.exists(dir_pruebas)) {
    dir.create(dir_pruebas)
  }

  # Vector para almacenar resultados de análisis visual
  resultados_analisis <- list()

  # Identificar chunks que generan gráficas
  lineas <- readLines(archivo_rmd)
  chunks_python <- grep("```\\{r.*python", lineas)
  chunks_plot <- grep("```\\{r.*plot", lineas)
  chunks_grafica <- c(chunks_python, chunks_plot)

  if (length(chunks_grafica) == 0) {
    warning("No se encontraron chunks que generen gráficas en el archivo ", archivo_rmd)
    return(NULL)
  }

  # Identificar chunk de definición de variables
  chunk_variables <- grep("```\\{r.*Definicion|```\\{r.*definicion|```\\{r.*Variables|```\\{r.*variables", lineas)

  if (length(chunk_variables) == 0) {
    warning("No se encontró un chunk de definición de variables en el archivo ", archivo_rmd)
    return(NULL)
  }

  # Ejecutar simulaciones
  for (i in 1:n_simulaciones) {
    cat("\nAnalizando simulación", i, "de", n_simulaciones, "...\n")

    # Establecer semilla aleatoria
    set.seed(i)

    # Crear entorno para ejecutar el código
    source_env <- new.env()

    # Ejecutar el chunk de definición de variables
    inicio_chunk_vars <- chunk_variables[1]
    fin_chunk_vars <- min(grep("```", lineas)[(grep("```", lineas) > inicio_chunk_vars)])

    cat("  Ejecutando chunk de definición de variables (líneas", inicio_chunk_vars+1, "a", fin_chunk_vars-1, ")...\n")
    tryCatch({
      eval(parse(text = lineas[(inicio_chunk_vars+1):(fin_chunk_vars-1)]), envir = source_env)
    }, error = function(e) {
      warning("Error al ejecutar el chunk de definición de variables: ", e$message)
    })

    # Ejecutar cada chunk que genera gráficas
    for (j in seq_along(chunks_grafica)) {
      inicio_chunk <- chunks_grafica[j]
      fin_chunk <- min(grep("```", lineas)[(grep("```", lineas) > inicio_chunk)])

      cat("  Ejecutando chunk de gráficas", j, "(líneas", inicio_chunk+1, "a", fin_chunk-1, ")...\n")
      tryCatch({
        eval(parse(text = lineas[(inicio_chunk+1):(fin_chunk-1)]), envir = source_env)
      }, error = function(e) {
        warning("Error al ejecutar el chunk de gráficas ", j, ": ", e$message)
      })
    }

    # Buscar imágenes generadas
    imagenes <- list.files(pattern = patron_imagenes)

    if (length(imagenes) == 0) {
      warning("No se encontraron imágenes generadas con el patrón ", patron_imagenes)
      next
    }

    cat("  Se encontraron", length(imagenes), "imágenes:", paste(imagenes, collapse = ", "), "\n")

    # Analizar cada imagen
    for (img_file in imagenes) {
      cat("  Analizando imagen:", img_file, "\n")

      # Guardar una copia de la imagen para análisis
      file.copy(img_file, file.path(dir_pruebas, paste0(i, "_", img_file)), overwrite = TRUE)

      # Analizar la imagen
      tryCatch({
        # Determinar el tipo de imagen
        extension <- tolower(tools::file_ext(img_file))

        if (extension == "png") {
          # Cargar la imagen PNG
          img <- png::readPNG(img_file)

          # Verificar dimensiones de la imagen
          test_that(paste("La imagen", img_file, "tiene dimensiones adecuadas"), {
            expect_true(dim(img)[1] > 100 && dim(img)[2] > 100,
                        "La imagen es demasiado pequeña")
          })

          # Verificar que la imagen tenga suficiente contenido (no sea mayormente en blanco)
          # Calcular el porcentaje de píxeles no blancos
          if (dim(img)[3] == 4) { # Imagen RGBA
            pixeles_no_blancos <- mean(img[,,4] > 0.1) # Usar canal alfa
          } else { # Imagen RGB
            pixeles_no_blancos <- mean(img[,,1] < 0.95 | img[,,2] < 0.95 | img[,,3] < 0.95)
          }

          test_that(paste("La imagen", img_file, "tiene contenido visible"), {
            expect_true(pixeles_no_blancos > 0.05,
                        "La imagen parece estar mayormente en blanco")
          })

          # Verificar que la imagen no tenga demasiado contenido (no sea mayormente negra)
          test_that(paste("La imagen", img_file, "no está saturada de contenido"), {
            expect_true(pixeles_no_blancos < 0.95,
                        "La imagen parece estar mayormente negra o saturada")
          })

          # Guardar resultados del análisis
          resultados_analisis[[paste0(i, "_", img_file)]] <- list(
            semilla = i,
            archivo = img_file,
            dimensiones = dim(img),
            porcentaje_contenido = pixeles_no_blancos
          )

        } else if (extension %in% c("jpg", "jpeg")) {
          # Para imágenes JPEG, solo verificamos que existan y no estén vacías
          test_that(paste("La imagen", img_file, "existe y no está vacía"), {
            expect_true(file.exists(img_file), paste("La imagen", img_file, "no existe"))
            expect_true(file.info(img_file)$size > 0, paste("La imagen", img_file, "está vacía"))
          })

          resultados_analisis[[paste0(i, "_", img_file)]] <- list(
            semilla = i,
            archivo = img_file,
            dimensiones = "No disponible para JPEG",
            porcentaje_contenido = "No disponible para JPEG"
          )
        } else {
          # Para otros formatos, solo verificamos que existan y no estén vacías
          test_that(paste("La imagen", img_file, "existe y no está vacía"), {
            expect_true(file.exists(img_file), paste("La imagen", img_file, "no existe"))
            expect_true(file.info(img_file)$size > 0, paste("La imagen", img_file, "está vacía"))
          })

          resultados_analisis[[paste0(i, "_", img_file)]] <- list(
            semilla = i,
            archivo = img_file,
            dimensiones = "No disponible para este formato",
            porcentaje_contenido = "No disponible para este formato"
          )
        }

      }, error = function(e) {
        warning("Error al analizar la imagen ", img_file, ": ", e$message)
      })
    }
  }

  # Análisis global de coherencia visual
  if (length(resultados_analisis) > 0) {
    # Filtrar solo resultados de PNG para análisis estadístico
    resultados_png <- resultados_analisis[sapply(resultados_analisis, function(x) !is.character(x$dimensiones))]

    if (length(resultados_png) > 0) {
      # Extraer dimensiones y porcentajes de contenido
      dimensiones <- sapply(resultados_png, function(x) paste(x$dimensiones[1:2], collapse="x"))
      porcentajes <- sapply(resultados_png, function(x) x$porcentaje_contenido)

      # Verificar coherencia de dimensiones
      test_that("Las imágenes generadas tienen dimensiones coherentes", {
        # Permitir hasta 3 tamaños diferentes (por ejemplo, para diferentes tipos de gráficas)
        expect_true(length(unique(dimensiones)) <= 3,
                    "Las imágenes generadas tienen más de 3 dimensiones diferentes")
      })

      # Verificar coherencia de contenido
      test_that("Las imágenes generadas tienen contenido coherente", {
        expect_true(all(porcentajes > 0.05),
                    "Algunas imágenes tienen muy poco contenido visible")
        expect_true(all(porcentajes < 0.95),
                    "Algunas imágenes están saturadas de contenido")
      })

      # Imprimir resumen de análisis visual
      cat("\nResumen de análisis visual de", length(resultados_png), "imágenes PNG:\n")
      cat("- Dimensiones encontradas:", paste(unique(dimensiones), collapse=", "), "\n")
      cat("- Rango de porcentaje de contenido:",
          round(min(porcentajes)*100, 1), "% -", round(max(porcentajes)*100, 1), "%\n")
    }
  }

  cat("\nImágenes guardadas en directorio:", dir_pruebas, "\n")

  return(resultados_analisis)
}

# Función para verificar diversidad de ejercicios generados
verificar_diversidad <- function(archivo_rmd, n_simulaciones = 300) {
  cat("\nVerificando diversidad de ejercicios (", n_simulaciones, "simulaciones)...\n")

  # Vectores para almacenar "huellas digitales" de cada generación
  huellas_digitales <- character(n_simulaciones)

  # Identificar chunk de definición de variables
  lineas <- readLines(archivo_rmd)
  chunk_variables <- grep("```\\{r.*Definicion|```\\{r.*definicion|```\\{r.*Variables|```\\{r.*variables", lineas)

  if (length(chunk_variables) == 0) {
    warning("No se encontró un chunk de definición de variables en el archivo ", archivo_rmd)
    return(0)
  }

  # Determinar inicio y fin del chunk de variables
  inicio_chunk_vars <- chunk_variables[1]
  fin_chunk_vars <- min(grep("```", lineas)[(grep("```", lineas) > inicio_chunk_vars)])

  # Ejecutar simulaciones
  for (i in 1:n_simulaciones) {
    if (i %% 50 == 0) cat("  Procesando simulación", i, "de", n_simulaciones, "\n")

    # Establecer semilla aleatoria
    set.seed(i)

    # Ejecutar el chunk de definición de variables
    source_env <- new.env()

    tryCatch({
      eval(parse(text = lineas[(inicio_chunk_vars+1):(fin_chunk_vars-1)]), envir = source_env)

      # Crear una "huella digital" de esta generación
      # Obtener todas las variables del entorno
      variables <- ls(envir = source_env)

      # Filtrar solo variables numéricas y de texto para la huella
      valores <- lapply(variables, function(var) {
        valor <- get(var, envir = source_env)
        if (is.numeric(valor) || is.character(valor) || is.logical(valor)) {
          if (length(valor) <= 10) {  # Limitar tamaño para evitar huellas enormes
            return(paste(var, "=", paste(valor, collapse = ",")))
          }
        }
        return(NULL)
      })

      # Eliminar valores NULL
      valores <- valores[!sapply(valores, is.null)]

      # Crear huella digital
      huella <- paste(valores, collapse = "|")
      huellas_digitales[i] <- huella

    }, error = function(e) {
      warning("Error en simulación ", i, ": ", e$message)
      huellas_digitales[i] <- paste("ERROR:", e$message)
    })
  }

  # Contar cuántas huellas únicas hay
  n_unicas <- length(unique(huellas_digitales))

  # Calcular porcentaje de diversidad
  porcentaje_diversidad <- (n_unicas / n_simulaciones) * 100

  # Prueba de diversidad
  test_that("Se generan suficientes versiones diferentes", {
    expect_true(n_unicas >= 290,
                paste("Solo se generaron", n_unicas, "versiones únicas de", n_simulaciones,
                      "(", round(porcentaje_diversidad, 1), "%). Se requieren al menos 290."))
  })

  # Imprimir resultados
  cat("\nResultados de diversidad:\n")
  cat("- Versiones únicas:", n_unicas, "de", n_simulaciones,
      "(", round(porcentaje_diversidad, 1), "%)\n")

  return(n_unicas)
}

# Función para verificar que no hay duplicidad en las opciones de respuesta
verificar_opciones_respuesta <- function(archivo_rmd, n_simulaciones = 100) {
  cat("\nVerificando opciones de respuesta (", n_simulaciones, "simulaciones)...\n")

  # Contador de ejercicios con opciones duplicadas
  ejercicios_con_duplicados <- 0

  for (i in 1:n_simulaciones) {
    if (i %% 20 == 0) cat("  Procesando simulación", i, "de", n_simulaciones, "\n")

    # Establecer semilla aleatoria
    set.seed(i)

    # Generar el ejercicio
    tryCatch({
      ejercicio <- exams::xexams(archivo_rmd, driver = list(sweave = NULL, read = NULL), quiet = TRUE)

      # Extraer las opciones de respuesta
      opciones <- ejercicio[[1]][[1]]$metainfo$solution

      # Verificar si hay duplicados (todas las opciones deben ser diferentes)
      if (length(unique(opciones)) < length(opciones)) {
        ejercicios_con_duplicados <- ejercicios_con_duplicados + 1

        if (ejercicios_con_duplicados <= 3) {  # Mostrar detalles solo para los primeros 3 casos
          cat("  Ejercicio", i, "tiene opciones duplicadas:", paste(opciones, collapse = ", "), "\n")
        }
      }

      # Verificar que hay exactamente una respuesta correcta
      if (sum(opciones) != 1) {
        warning("Ejercicio ", i, " no tiene exactamente una respuesta correcta. Suma de opciones: ", sum(opciones))
      }

    }, error = function(e) {
      warning("Error al generar ejercicio ", i, ": ", e$message)
    })
  }

  # Prueba de no duplicidad
  test_that("No hay duplicidad en las opciones de respuesta", {
    # Permitir hasta un 5% de ejercicios con opciones duplicadas
    max_duplicados_permitidos <- ceiling(n_simulaciones * 0.05)
    expect_true(ejercicios_con_duplicados <= max_duplicados_permitidos,
                paste("Se encontraron", ejercicios_con_duplicados, "ejercicios con opciones duplicadas de",
                      n_simulaciones, "(máximo permitido:", max_duplicados_permitidos, ")"))
  })

  # Imprimir resultados
  cat("\nResultados de verificación de opciones:\n")
  cat("- Ejercicios con opciones duplicadas:", ejercicios_con_duplicados, "de", n_simulaciones,
      "(", round((ejercicios_con_duplicados/n_simulaciones)*100, 1), "%)\n")

  return(ejercicios_con_duplicados)
}

# Función para verificar la compatibilidad del ejercicio con r-exams
verificar_compatibilidad_exams <- function(archivo_rmd) {
  cat("\nVerificando compatibilidad con r-exams...\n")

  # Crear directorio para archivos de salida si no existe
  dir_salida <- "salida_exams"
  if (!dir.exists(dir_salida)) {
    dir.create(dir_salida)
  }

  # Verificar compatibilidad con diferentes formatos
  formatos_ok <- 0

  # 1. Verificar compatibilidad con PDF
  tryCatch({
    cat("  Generando PDF...\n")
    resultado_pdf <- exams2pdf(archivo_rmd, dir = dir_salida, template = "plain")
    formatos_ok <- formatos_ok + 1
    cat("  ✓ PDF generado correctamente\n")
  }, error = function(e) {
    cat("  ✗ Error al generar PDF:", e$message, "\n")
  })

  # 2. Verificar compatibilidad con HTML
  tryCatch({
    cat("  Generando HTML...\n")
    resultado_html <- exams2html(archivo_rmd, dir = dir_salida)
    formatos_ok <- formatos_ok + 1
    cat("  ✓ HTML generado correctamente\n")
  }, error = function(e) {
    cat("  ✗ Error al generar HTML:", e$message, "\n")
  })

  # 3. Verificar compatibilidad con Moodle
  tryCatch({
    cat("  Generando XML para Moodle...\n")
    resultado_moodle <- exams2moodle(archivo_rmd, dir = dir_salida)
    formatos_ok <- formatos_ok + 1
    cat("  ✓ XML para Moodle generado correctamente\n")
  }, error = function(e) {
    cat("  ✗ Error al generar XML para Moodle:", e$message, "\n")
  })

  # 4. Verificar compatibilidad con NOPS
  tryCatch({
    cat("  Generando NOPS...\n")
    resultado_nops <- exams2nops(archivo_rmd, dir = dir_salida, verbose = FALSE)
    formatos_ok <- formatos_ok + 1
    cat("  ✓ NOPS generado correctamente\n")
  }, error = function(e) {
    cat("  ✗ Error al generar NOPS:", e$message, "\n")
  })

  # Prueba de compatibilidad
  test_that("El ejercicio es compatible con múltiples formatos de r-exams", {
    expect_true(formatos_ok >= 2,
                paste("El ejercicio solo es compatible con", formatos_ok, "de 4 formatos probados"))
  })

  # Imprimir resultados
  cat("\nResultados de compatibilidad:\n")
  cat("- Formatos compatibles:", formatos_ok, "de 4\n")
  cat("- Archivos generados en:", dir_salida, "\n")

  return(formatos_ok)
}

# Función principal para ejecutar todas las pruebas
ejecutar_pruebas_completas <- function(archivo_rmd, n_simulaciones_diversidad = 300,
                                      n_simulaciones_opciones = 100, n_simulaciones_visual = 5) {
  cat("\n=== INICIANDO PRUEBAS PARA", archivo_rmd, "===\n")

  # 1. Verificar diversidad de ejercicios
  cat("\n1. VERIFICANDO DIVERSIDAD DE EJERCICIOS\n")
  cat("======================================\n")
  num_versiones <- verificar_diversidad(archivo_rmd, n_simulaciones_diversidad)

  # 2. Verificar opciones de respuesta
  cat("\n2. VERIFICANDO OPCIONES DE RESPUESTA\n")
  cat("===================================\n")
  duplicados <- verificar_opciones_respuesta(archivo_rmd, n_simulaciones_opciones)

  # 3. Analizar coherencia visual de gráficas
  cat("\n3. ANALIZANDO COHERENCIA VISUAL DE GRÁFICAS\n")
  cat("=========================================\n")
  resultados_visual <- analizar_coherencia_visual(archivo_rmd, n_simulaciones = n_simulaciones_visual)

  # 4. Verificar compatibilidad con r-exams
  cat("\n4. VERIFICANDO COMPATIBILIDAD CON R-EXAMS\n")
  cat("=======================================\n")
  formatos_ok <- verificar_compatibilidad_exams(archivo_rmd)

  # Resumen final
  cat("\n=== RESUMEN DE PRUEBAS ===\n")
  cat("- Archivo analizado:", archivo_rmd, "\n")
  cat("- Diversidad:", num_versiones, "versiones únicas de", n_simulaciones_diversidad,
      "(", round((num_versiones/n_simulaciones_diversidad)*100, 1), "%)\n")
  cat("- Opciones de respuesta:", n_simulaciones_opciones - duplicados, "ejercicios sin duplicados de",
      n_simulaciones_opciones, "(", round(((n_simulaciones_opciones-duplicados)/n_simulaciones_opciones)*100, 1), "%)\n")
  cat("- Coherencia visual:", if(!is.null(resultados_visual)) paste(length(resultados_visual), "imágenes analizadas\n") else "No se analizaron imágenes\n")
  cat("- Compatibilidad r-exams:", formatos_ok, "de 4 formatos compatibles\n")

  cat("\n=== PRUEBAS COMPLETADAS ===\n")
}

# Si se ejecuta el script directamente, buscar archivos .Rmd en el directorio actual
if (interactive()) {
  cat("Ejecute este script con un archivo .Rmd específico:\n")
  cat("ejecutar_pruebas_completas('nombre_del_archivo.Rmd')\n")
} else {
  # Buscar archivos .Rmd en el directorio actual
  archivos_rmd <- list.files(pattern = "\\.Rmd$")

  if (length(archivos_rmd) > 0) {
    cat("Archivos .Rmd encontrados:", paste(archivos_rmd, collapse = ", "), "\n")
    cat("Ejecutando pruebas para el primer archivo:", archivos_rmd[1], "\n")
    ejecutar_pruebas_completas(archivos_rmd[1])
  } else {
    cat("No se encontraron archivos .Rmd en el directorio actual.\n")
  }
}
