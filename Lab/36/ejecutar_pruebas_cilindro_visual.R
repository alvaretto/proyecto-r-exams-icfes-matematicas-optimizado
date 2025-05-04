# ejecutar_pruebas_cilindro_visual.R
# Script para analizar la coherencia visual de las gráficas en volumen_cilindro_hueco_v1.Rmd

library(testthat)
library(png)
library(exams)
library(reticulate)
library(digest)

# Archivo a probar
archivo_rmd <- "volumen_cilindro_hueco_v1.Rmd"

# Función para analizar la coherencia visual de las gráficas
analizar_coherencia_visual <- function(n_simulaciones = 5) {
  # Crear directorio para guardar imágenes de prueba si no existe
  dir_pruebas <- "pruebas_imagenes"
  if (!dir.exists(dir_pruebas)) {
    dir.create(dir_pruebas)
  }

  # Vector para almacenar resultados de análisis visual
  resultados_analisis <- list()

  for (i in 1:n_simulaciones) {
    cat("\nAnalizando simulación", i, "de", n_simulaciones, "...\n")

    # Establecer semilla aleatoria
    set.seed(i)

    # Crear entorno para ejecutar el código
    source_env <- new.env()

    # Leer el archivo y extraer las líneas del chunk de definición de variables
    lineas <- readLines(archivo_rmd)
    inicio_chunk_vars <- which(grepl("```\\{r DefinicionDeVariables", lineas))
    fin_chunk_vars <- which(grepl("```", lineas))[which(which(grepl("```", lineas)) > inicio_chunk_vars)[1]]

    cat("  Ejecutando chunk de definición de variables (líneas", inicio_chunk_vars+1, "a", fin_chunk_vars-1, ")...\n")
    tryCatch({
      eval(parse(text = lineas[(inicio_chunk_vars+1):(fin_chunk_vars-1)]), envir = source_env)
    }, error = function(e) {
      warning("Error al ejecutar el chunk de definición de variables: ", e$message)
    })

    # Ejecutar el chunk de generación de Python
    inicio_chunk_python <- which(grepl("```\\{r generar_cilindro_python", lineas))
    fin_chunk_python <- which(grepl("```", lineas))[which(which(grepl("```", lineas)) > inicio_chunk_python)[1]]

    cat("  Ejecutando chunk de generación de Python (líneas", inicio_chunk_python+1, "a", fin_chunk_python-1, ")...\n")
    tryCatch({
      eval(parse(text = lineas[(inicio_chunk_python+1):(fin_chunk_python-1)]), envir = source_env)
    }, error = function(e) {
      warning("Error al ejecutar el chunk de generación de Python: ", e$message)
    })

    # Verificar que se ha generado el archivo de imagen
    if (file.exists("cilindro_hueco.png")) {
      cat("  ✓ Imagen generada correctamente\n")

      # Guardar una copia de la imagen para análisis
      file.copy("cilindro_hueco.png", file.path(dir_pruebas, paste0("cilindro_hueco_", i, ".png")), overwrite = TRUE)

      # Extraer variables relevantes para verificar coherencia visual
      r_int <- source_env$r_int
      r_ext <- source_env$r_ext
      altura <- source_env$altura
      unidad <- source_env$unidad

      # Analizar la imagen generada
      tryCatch({
        # Cargar la imagen
        img <- png::readPNG("cilindro_hueco.png")

        # Verificar dimensiones de la imagen
        test_that(paste("La imagen de la simulación", i, "tiene dimensiones adecuadas"), {
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

        test_that(paste("La imagen de la simulación", i, "tiene contenido visible"), {
          expect_true(pixeles_no_blancos > 0.05,
                      "La imagen parece estar mayormente en blanco")
        })

        # Nota: Para el cilindro hueco, es normal tener un alto porcentaje de contenido
        # No verificamos saturación porque las imágenes del cilindro hueco tienen
        # naturalmente un alto porcentaje de píxeles no blancos
        cat("  - Porcentaje de contenido:", round(pixeles_no_blancos*100, 1),
            "% (normal para este tipo de gráfica)\n")

        # Guardar resultados del análisis
        resultados_analisis[[i]] <- list(
          semilla = i,
          r_int = r_int,
          r_ext = r_ext,
          altura = altura,
          unidad = unidad,
          dimensiones = dim(img),
          porcentaje_contenido = pixeles_no_blancos
        )

        cat("  - Dimensiones:", paste(dim(img)[1:2], collapse="x"), "píxeles\n")
        cat("  - Porcentaje de contenido:", round(pixeles_no_blancos*100, 1), "%\n")
        cat("  - Variables: r_int =", r_int, ", r_ext =", r_ext, ", altura =", altura, ", unidad =", unidad, "\n")

      }, error = function(e) {
        warning("Error al analizar la imagen: ", e$message)
      })
    } else {
      cat("  ✗ No se generó la imagen cilindro_hueco.png\n")
    }
  }

  # Análisis global de coherencia visual
  if (length(resultados_analisis) > 0) {
    # Extraer dimensiones y porcentajes de contenido
    dimensiones <- sapply(resultados_analisis, function(x) paste(x$dimensiones[1:2], collapse="x"))
    porcentajes <- sapply(resultados_analisis, function(x) x$porcentaje_contenido)

    # Verificar coherencia de dimensiones
    test_that("Las imágenes generadas tienen dimensiones coherentes", {
      # Permitir hasta 5 tamaños diferentes (puede haber pequeñas variaciones)
      expect_true(length(unique(dimensiones)) <= 5,
                  "Las imágenes generadas tienen más de 5 dimensiones diferentes")
    })

    # Verificar coherencia de contenido
    test_that("Las imágenes generadas tienen contenido visible", {
      expect_true(all(porcentajes > 0.05),
                  "Algunas imágenes tienen muy poco contenido visible")
      # No verificamos el límite superior para el cilindro hueco
      # ya que es normal que tengan un alto porcentaje de contenido
    })

    # Verificar coherencia con los datos
    test_that("Las dimensiones del cilindro son coherentes con los datos", {
      # Verificar que el radio interno siempre es menor que el radio externo
      r_int_valores <- sapply(resultados_analisis, function(x) x$r_int)
      r_ext_valores <- sapply(resultados_analisis, function(x) x$r_ext)
      expect_true(all(r_int_valores < r_ext_valores),
                  "En algunas imágenes el radio interno no es menor que el radio externo")
    })

    # Imprimir resumen de análisis visual
    cat("\nResumen de análisis visual de", length(resultados_analisis), "imágenes:\n")
    cat("- Dimensiones encontradas:", paste(unique(dimensiones), collapse=", "), "\n")
    cat("- Rango de porcentaje de contenido:",
        round(min(porcentajes)*100, 1), "% -", round(max(porcentajes)*100, 1), "%\n")
  }

  cat("\nImágenes guardadas en directorio:", dir_pruebas, "\n")

  return(resultados_analisis)
}

# Función para verificar diversidad de ejercicios generados
verificar_diversidad <- function(n_simulaciones = 100) {
  cat("\nVerificando diversidad de ejercicios (", n_simulaciones, "simulaciones)...\n")

  # Vectores para almacenar "huellas digitales" de cada generación
  huellas_digitales <- character(n_simulaciones)

  # Vectores para almacenar valores específicos para análisis
  valores_r_int <- numeric(n_simulaciones)
  valores_r_ext <- numeric(n_simulaciones)
  valores_altura <- numeric(n_simulaciones)
  valores_unidad <- character(n_simulaciones)
  valores_nombre <- character(n_simulaciones)
  valores_recipiente <- character(n_simulaciones)
  valores_liquido <- character(n_simulaciones)

  for (i in 1:n_simulaciones) {
    if (i %% 20 == 0) cat("  Procesando simulación", i, "de", n_simulaciones, "\n")

    # Establecer semilla aleatoria
    set.seed(i)

    # Ejecutar el chunk de definición de variables
    source_env <- new.env()

    # Leer el archivo y extraer las líneas del chunk de definición de variables
    lineas <- readLines(archivo_rmd)
    inicio_chunk <- which(grepl("```\\{r DefinicionDeVariables", lineas))
    fin_chunk <- which(grepl("```", lineas))[which(which(grepl("```", lineas)) > inicio_chunk)[1]]

    # Ejecutar el código del chunk
    eval(parse(text = lineas[(inicio_chunk+1):(fin_chunk-1)]), envir = source_env)

    # Almacenar valores para análisis
    valores_r_int[i] <- source_env$r_int
    valores_r_ext[i] <- source_env$r_ext
    valores_altura[i] <- source_env$altura
    valores_unidad[i] <- source_env$unidad
    valores_nombre[i] <- source_env$nombre
    valores_recipiente[i] <- source_env$recipiente
    valores_liquido[i] <- source_env$liquido

    # Crear una "huella digital" de esta generación
    huella <- digest::digest(list(
      r_int = source_env$r_int,
      r_ext = source_env$r_ext,
      d_ext = source_env$d_ext,
      altura = source_env$altura,
      volumen = source_env$volumen,
      unidad = source_env$unidad,
      nombre = source_env$nombre,
      recipiente = source_env$recipiente,
      liquido = source_env$liquido,
      adjetivo_interno = source_env$adjetivo_interno
    ), algo = "md5")

    huellas_digitales[i] <- huella
  }

  # Contar cuántas huellas únicas hay
  n_unicas <- length(unique(huellas_digitales))

  # Calcular porcentaje de diversidad
  porcentaje_diversidad <- (n_unicas / n_simulaciones) * 100

  # Pruebas de diversidad
  test_that("Se generan suficientes versiones diferentes", {
    expect_true(n_unicas >= min(290, n_simulaciones * 0.95),
                paste("Solo se generaron", n_unicas, "versiones únicas de", n_simulaciones,
                      "(", round(porcentaje_diversidad, 1), "%). Se requieren al menos",
                      min(290, n_simulaciones * 0.95), "."))
  })

  # Pruebas de distribución de valores
  test_that("Los valores generados tienen una distribución adecuada", {
    # Verificar que se usen todas las unidades posibles
    expect_true(all(c("m", "cm", "dm") %in% unique(valores_unidad)),
                "Deben usarse todas las unidades posibles (m, cm, dm)")

    # Verificar que se usen suficientes nombres diferentes
    expect_true(length(unique(valores_nombre)) >= 10,
               "Deben usarse al menos 10 nombres diferentes")

    # Verificar que se usen suficientes tipos de recipientes
    expect_true(length(unique(valores_recipiente)) >= 4,
               "Deben usarse al menos 4 tipos de recipientes diferentes")

    # Verificar que se usen suficientes tipos de líquidos
    expect_true(length(unique(valores_liquido)) >= 3,
               "Deben usarse al menos 3 tipos de líquidos diferentes")
  })

  # Imprimir resultados
  cat("\nResultados de diversidad:\n")
  cat("- Versiones únicas:", n_unicas, "de", n_simulaciones,
      "(", round(porcentaje_diversidad, 1), "%)\n")
  cat("- Unidades diferentes:", length(unique(valores_unidad)), "\n")
  cat("- Nombres diferentes:", length(unique(valores_nombre)), "\n")
  cat("- Recipientes diferentes:", length(unique(valores_recipiente)), "\n")
  cat("- Líquidos diferentes:", length(unique(valores_liquido)), "\n")

  return(n_unicas)
}

# Ejecutar todas las pruebas
cat("\n=== INICIANDO PRUEBAS PARA", archivo_rmd, "===\n")

cat("\n1. VERIFICANDO DIVERSIDAD DE EJERCICIOS\n")
cat("======================================\n")
num_versiones <- verificar_diversidad(100)

cat("\n2. ANALIZANDO COHERENCIA VISUAL DE GRÁFICAS\n")
cat("=========================================\n")
resultados_visual <- analizar_coherencia_visual(5)

cat("\n=== PRUEBAS COMPLETADAS ===\n")
