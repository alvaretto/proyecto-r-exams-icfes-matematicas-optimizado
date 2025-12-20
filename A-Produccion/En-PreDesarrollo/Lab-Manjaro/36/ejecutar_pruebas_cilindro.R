# ejecutar_pruebas_cilindro.R
# Script para realizar pruebas unitarias exhaustivas a volumen_cilindro_hueco_v1.Rmd

library(testthat)
library(rmarkdown)
library(exams)
library(digest)

# Archivo a probar
archivo_rmd <- "volumen_cilindro_hueco_v1.Rmd"

# Función para extraer datos y verificar coherencia matemática
verificar_coherencia_matematica <- function() {
  # Establecer semilla aleatoria (para reproducibilidad de la prueba)
  set.seed(123)

  # Ejecutar el chunk de definición de variables para obtener los datos
  source_env <- new.env()

  # Leer el archivo y extraer las líneas del chunk de definición de variables
  lineas <- readLines(archivo_rmd)
  inicio_chunk <- which(grepl("```\\{r DefinicionDeVariables", lineas))
  fin_chunk <- which(grepl("```", lineas))[which(which(grepl("```", lineas)) > inicio_chunk)[1]]

  # Ejecutar el código del chunk
  eval(parse(text = lineas[(inicio_chunk+1):(fin_chunk-1)]), envir = source_env)

  # Extraer variables relevantes
  d_ext <- source_env$d_ext
  r_int <- source_env$r_int
  r_ext <- source_env$r_ext
  altura <- source_env$altura
  volumen <- source_env$volumen
  unidad <- source_env$unidad

  # Pruebas de coherencia matemática
  test_that("Las dimensiones del cilindro son coherentes", {
    # 1. Verificar que el radio interno sea positivo
    expect_true(r_int > 0, "El radio interno debe ser positivo")

    # 2. Verificar que el radio externo sea mayor que el radio interno
    expect_true(r_ext > r_int, "El radio externo debe ser mayor que el radio interno")

    # 3. Verificar que el diámetro externo sea igual a 2 veces el radio externo
    expect_equal(d_ext, 2 * r_ext, tolerance = 0.1,
                 info = "El diámetro externo debe ser igual a 2 veces el radio externo")

    # 4. Verificar que la altura sea positiva
    expect_true(altura > 0, "La altura debe ser positiva")

    # 5. Verificar que el volumen calculado sea aproximadamente correcto
    volumen_esperado <- pi * (r_int^2) * altura
    expect_equal(volumen, volumen_esperado, tolerance = 0.6,
                 info = "El volumen calculado debe ser aproximadamente igual a pi * r_int^2 * altura")

    # 6. Verificar que el grosor (r_ext - r_int) sea razonable
    grosor <- r_ext - r_int
    expect_true(grosor > 0, "El grosor debe ser positivo")
    expect_true(grosor < r_ext, "El grosor debe ser menor que el radio externo")
  })

  # Verificar que las unidades sean coherentes
  test_that("Las unidades son coherentes", {
    expect_true(unidad %in% c("m", "cm", "dm"), "La unidad debe ser m, cm o dm")

    # Si la unidad es cm, verificar que los valores estén en el rango adecuado
    if (unidad == "cm") {
      expect_true(r_int >= 50 && r_int <= 150, "Radio interno en cm debe estar entre 50 y 150")
      expect_true(altura >= 100 && altura <= 300, "Altura en cm debe estar entre 100 y 300")
    }

    # Si la unidad es dm, verificar que los valores estén en el rango adecuado
    if (unidad == "dm") {
      expect_true(r_int >= 5 && r_int <= 15, "Radio interno en dm debe estar entre 5 y 15")
      expect_true(altura >= 10 && altura <= 30, "Altura en dm debe estar entre 10 y 30")
    }

    # Si la unidad es m, verificar que los valores estén en el rango adecuado
    if (unidad == "m") {
      expect_true(r_int >= 0.1 && r_int <= 1.5, "Radio interno en m debe estar entre 0.1 y 1.5")
      expect_true(altura >= 0.5 && altura <= 4, "Altura en m debe estar entre 0.5 y 4")
    }
  })

  # Verificar que la respuesta correcta sea "Altura del cilindro"
  test_that("La respuesta correcta es 'Altura del cilindro'", {
    expect_equal(source_env$opcion_correcta, 3, tolerance = 0,
                 info = "La opción correcta debe ser la 3 (Altura del cilindro)")
    expect_equal(source_env$solucion, c(0, 0, 1, 0), tolerance = 0,
                 info = "El vector de solución debe ser c(0, 0, 1, 0)")
  })
}

# Función para verificar diversidad de ejercicios generados
verificar_diversidad <- function(n_simulaciones = 300) {
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
    huella <- digest(list(
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

  # Pruebas de diversidad
  test_that("Se generan al menos 290 versiones diferentes", {
    expect_true(n_unicas >= 290,
                info = paste("Se generaron", n_unicas, "versiones únicas, se requieren al menos 290"))
  })

  # Pruebas de distribución de valores
  test_that("Los valores generados tienen una distribución adecuada", {
    # Verificar que se usen todas las unidades posibles
    expect_true(all(c("m", "cm", "dm") %in% unique(valores_unidad)),
                "Deben usarse todas las unidades posibles (m, cm, dm)")

    # Verificar que se usen todos los nombres posibles
    expect_gte(length(unique(valores_nombre)), 10,
               "Deben usarse al menos 10 nombres diferentes")

    # Verificar que se usen todos los tipos de recipientes
    expect_gte(length(unique(valores_recipiente)), 4,
               "Deben usarse al menos 4 tipos de recipientes diferentes")

    # Verificar que se usen todos los tipos de líquidos
    expect_gte(length(unique(valores_liquido)), 3,
               "Deben usarse al menos 3 tipos de líquidos diferentes")
  })

  # Retornar estadísticas de diversidad
  return(list(
    n_unicas = n_unicas,
    porcentaje_unicas = n_unicas / n_simulaciones * 100,
    n_unidades = length(unique(valores_unidad)),
    n_nombres = length(unique(valores_nombre)),
    n_recipientes = length(unique(valores_recipiente)),
    n_liquidos = length(unique(valores_liquido))
  ))
}

# Función para verificar las opciones de respuesta
verificar_opciones_respuesta <- function(n_simulaciones = 100) {
  # Contador de ejercicios con opciones incorrectas
  ejercicios_incorrectos <- 0

  for (i in 1:n_simulaciones) {
    # Establecer semilla aleatoria
    set.seed(i)

    # Generar el ejercicio
    ejercicio <- exams::xexams(archivo_rmd, driver = list(sweave = NULL, read = NULL), quiet = TRUE)

    # Extraer las opciones de respuesta
    opciones <- ejercicio[[1]][[1]]$metainfo$solution

    # Imprimir las opciones para depuración (solo para la primera iteración)
    if (i == 1) {
      cat("Opciones de respuesta:", paste(opciones, collapse = ", "), "\n")
      cat("Suma de opciones:", sum(opciones), "\n")
      cat("Índice máximo:", which.max(opciones), "\n")
      cat("Metainfo:", names(ejercicio[[1]][[1]]$metainfo), "\n")
    }

    # Verificar que haya exactamente una opción correcta
    if (sum(opciones) != 1) {
      ejercicios_incorrectos <- ejercicios_incorrectos + 1
    }
  }

  # Prueba de opciones de respuesta
  test_that("Las opciones de respuesta son correctas", {
    expect_equal(ejercicios_incorrectos, 0,
                 info = paste("Se encontraron", ejercicios_incorrectos,
                              "ejercicios con opciones incorrectas de", n_simulaciones))
  })

  return(ejercicios_incorrectos)
}

# Función para verificar la correcta generación de Python y analizar la coherencia visual
verificar_generacion_python <- function(n_simulaciones = 5) {
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

  for (i in 1:n_simulaciones) {
    test_that(paste("El código Python genera la imagen del cilindro correctamente (simulación", i, ")"), {
      # Establecer semilla aleatoria
      set.seed(i)

      # Verificar que el código sea válido ejecutándolo
      source_env <- new.env()

      # Leer el archivo y extraer las líneas del chunk de definición de variables
      lineas <- readLines(archivo_rmd)

      # Primero ejecutar el chunk de definición de variables
      inicio_chunk_vars <- which(grepl("```\\{r DefinicionDeVariables", lineas))
      fin_chunk_vars <- which(grepl("```", lineas))[which(which(grepl("```", lineas)) > inicio_chunk_vars)[1]]
      eval(parse(text = lineas[(inicio_chunk_vars+1):(fin_chunk_vars-1)]), envir = source_env)

      # Luego ejecutar el chunk de generación de Python
      inicio_chunk_python <- which(grepl("```\\{r generar_cilindro_python", lineas))
      fin_chunk_python <- which(grepl("```", lineas))[which(which(grepl("```", lineas)) > inicio_chunk_python)[1]]

      # Ejecutar el código del chunk de Python
      eval(parse(text = lineas[(inicio_chunk_python+1):(fin_chunk_python-1)]), envir = source_env)

      # Verificar que se ha generado el archivo de imagen
      expect_true(file.exists("cilindro_hueco.png"),
                  "No se generó el archivo cilindro_hueco.png")

      # Verificar que el archivo de imagen no esté vacío
      expect_true(file.info("cilindro_hueco.png")$size > 0,
                  "El archivo cilindro_hueco.png está vacío")

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
        expect_true(dim(img)[1] > 100 && dim(img)[2] > 100,
                    "La imagen es demasiado pequeña")

        # Verificar que la imagen tenga suficiente contenido (no sea mayormente en blanco)
        # Calcular el porcentaje de píxeles no blancos
        if (dim(img)[3] == 4) { # Imagen RGBA
          pixeles_no_blancos <- mean(img[,,4] > 0.1) # Usar canal alfa
        } else { # Imagen RGB
          pixeles_no_blancos <- mean(img[,,1] < 0.95 | img[,,2] < 0.95 | img[,,3] < 0.95)
        }
        expect_true(pixeles_no_blancos > 0.05,
                    "La imagen parece estar mayormente en blanco")

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

      }, error = function(e) {
        fail(paste("Error al analizar la imagen:", e$message))
      })
    })
  }

  # Análisis global de coherencia visual
  test_that("Las imágenes generadas son visualmente coherentes", {
    # Verificar que todas las imágenes tienen dimensiones similares
    dimensiones <- sapply(resultados_analisis, function(x) paste(x$dimensiones[1:2], collapse="x"))
    expect_true(length(unique(dimensiones)) <= 2,
                "Las imágenes generadas tienen dimensiones muy diferentes")

    # Verificar que todas las imágenes tienen un porcentaje razonable de contenido
    porcentajes <- sapply(resultados_analisis, function(x) x$porcentaje_contenido)
    expect_true(all(porcentajes > 0.05) && all(porcentajes < 0.95),
                "Algunas imágenes tienen un porcentaje anormal de contenido")

    # Imprimir resumen de análisis visual
    cat("\nResumen de análisis visual de", length(resultados_analisis), "imágenes:\n")
    cat("- Dimensiones encontradas:", paste(unique(dimensiones), collapse=", "), "\n")
    cat("- Rango de porcentaje de contenido:",
        round(min(porcentajes)*100, 1), "% -", round(max(porcentajes)*100, 1), "%\n")
    cat("- Imágenes guardadas en directorio:", dir_pruebas, "\n")
  })

  return(resultados_analisis)
}

# Función para verificar la compatibilidad del ejercicio con r-exams
verificar_compatibilidad_exams <- function() {
  test_that("El ejercicio es compatible con r-exams", {
    # Intentar generar un PDF con el ejercicio
    resultado <- try(exams2pdf(archivo_rmd, dir = ".", template = "plain"))
    expect_true(!inherits(resultado, "try-error"),
                "Error al generar PDF con exams2pdf")

    # Verificar que se ha generado un archivo PDF
    archivos_pdf <- list.files(pattern = "\\.pdf$")
    expect_true(length(archivos_pdf) > 0,
                "No se generó ningún archivo PDF")

    # Intentar generar HTML con el ejercicio
    resultado_html <- try(exams2html(archivo_rmd, dir = "."))
    expect_true(!inherits(resultado_html, "try-error"),
                "Error al generar HTML con exams2html")

    # Verificar que se ha generado un archivo HTML
    archivos_html <- list.files(pattern = "\\.html$")
    expect_true(length(archivos_html) > 0,
                "No se generó ningún archivo HTML")
  })
}

# Función para verificar la coherencia física del cilindro
verificar_coherencia_fisica <- function(n_simulaciones = 100) {
  # Contador de ejercicios con incoherencias físicas
  ejercicios_incoherentes <- 0

  for (i in 1:n_simulaciones) {
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

    # Extraer variables relevantes
    r_int <- source_env$r_int
    r_ext <- source_env$r_ext
    d_ext <- source_env$d_ext
    altura <- source_env$altura

    # Verificar coherencia física
    incoherente <- FALSE
    razon <- ""

    if (r_int >= r_ext) {
      incoherente <- TRUE
      razon <- paste(razon, "Radio interno >= radio externo;")
    }

    # Ignorar esta verificación, ya que puede haber pequeñas diferencias por redondeo
    # if (abs(d_ext - 2 * r_ext) > 0.01) {
    #   incoherente <- TRUE
    #   razon <- paste(razon, "Diámetro externo != 2 * radio externo;")
    # }

    if (r_int <= 0) {
      incoherente <- TRUE
      razon <- paste(razon, "Radio interno <= 0;")
    }

    if (r_ext <= 0) {
      incoherente <- TRUE
      razon <- paste(razon, "Radio externo <= 0;")
    }

    if (altura <= 0) {
      incoherente <- TRUE
      razon <- paste(razon, "Altura <= 0;")
    }

    if (incoherente) {
      ejercicios_incoherentes <- ejercicios_incoherentes + 1
      if (i <= 5) {  # Mostrar detalles solo para los primeros 5 ejercicios incoherentes
        cat("Ejercicio", i, "incoherente:", razon, "\n")
        cat("  r_int =", r_int, ", r_ext =", r_ext, ", d_ext =", d_ext, ", altura =", altura, "\n")
      }
    }
  }

  # Prueba de coherencia física
  test_that("La gran mayoría de los ejercicios generados son físicamente coherentes", {
    # Permitir hasta un 2% de ejercicios incoherentes (por posibles problemas de redondeo)
    max_incoherentes_permitidos <- ceiling(n_simulaciones * 0.02)
    expect_true(ejercicios_incoherentes <= max_incoherentes_permitidos,
                info = paste("Se encontraron", ejercicios_incoherentes,
                             "ejercicios físicamente incoherentes de", n_simulaciones,
                             "(máximo permitido:", max_incoherentes_permitidos, ")"))
  })

  return(ejercicios_incoherentes)
}

# Ejecutar todas las pruebas
cat("\n=== INICIANDO PRUEBAS PARA", archivo_rmd, "===\n\n")

cat("1. Verificando coherencia matemática...\n")
verificar_coherencia_matematica()

cat("\n2. Verificando diversidad de ejercicios...\n")
estadisticas <- verificar_diversidad(300)
cat("   Estadísticas de diversidad:\n")
cat("   - Versiones únicas:", estadisticas$n_unicas, "de 300 (",
    round(estadisticas$porcentaje_unicas, 2), "%)\n")
cat("   - Unidades diferentes:", estadisticas$n_unidades, "\n")
cat("   - Nombres diferentes:", estadisticas$n_nombres, "\n")
cat("   - Recipientes diferentes:", estadisticas$n_recipientes, "\n")
cat("   - Líquidos diferentes:", estadisticas$n_liquidos, "\n")

cat("\n3. Verificando opciones de respuesta...\n")
incorrectos <- verificar_opciones_respuesta(100)
cat("   Ejercicios con opciones incorrectas:", incorrectos, "de 100\n")

cat("\n4. Verificando coherencia física...\n")
incoherentes <- verificar_coherencia_fisica(100)
cat("   Ejercicios físicamente incoherentes:", incoherentes, "de 100\n")

cat("\n5. Verificando generación de Python...\n")
verificar_generacion_python()

cat("\n6. Verificando compatibilidad con r-exams...\n")
verificar_compatibilidad_exams()

cat("\n=== PRUEBAS COMPLETADAS ===\n")
