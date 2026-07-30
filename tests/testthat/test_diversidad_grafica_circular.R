library(testthat)
library(digest)

# Helper para cargar el Rmd y extraer variables generadas
# Esto simula el entorno de data_generation
get_generated_vars <- function(rmd_file) {
  # Crear un entorno temporal para evitar polución del globalenv
  env <- new.env()

  # Simular la ejecución de data_generation
  # Extraer el chunk data_generation del Rmd
  # fixed = TRUE es OBLIGATORIO: sin él, grep() trata el patrón como regex y
  # `{r data_generation` es una llave de cuantificador inválida -> error
  # "Invalid contents of {}". Con ese bug los 3 test_that de este archivo
  # morían en la primera línea del helper, así que la suite nunca verificó nada.
  rmd_lines  <- readLines(rmd_file, warn = FALSE)
  start_line <- grep("```{r data_generation", rmd_lines, fixed = TRUE)[1]
  cierres    <- grep("```", rmd_lines, fixed = TRUE)
  end_line   <- cierres[cierres > start_line][1]
  stopifnot(!is.na(start_line), !is.na(end_line))
  data_gen_code <- rmd_lines[(start_line + 1):(end_line - 1)]

  # Evaluar el código en el entorno temporal
  eval(parse(text = data_gen_code), envir = env)

  # Retornar las variables relevantes del entorno para testeos
  as.list(env)
}



# --- Tests de diversidad ---
test_that("La gráfica circular genera valores de tabla diferentes para cada semilla", {
  rmd_file <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/grafica-circular-consumo-agua/grafica_circular_consumo_agua_metacognitivo_argumentacion_n3_schoice_v2.Rmd"

  # Almacenar hashes de 50 ejecuciones
  hashes_valores <- c()

  # Guardar el estado actual de .Random.seed
  saved_seed <- if (exists(".Random.seed", envir = globalenv())) {
    get(".Random.seed", envir = globalenv())
  } else NULL

  for (i in 1:100) { # Probar con 100 semillas diferentes
    set.seed(i)
    vars <- get_generated_vars(rmd_file)
    hashes_valores <- c(hashes_valores, digest::digest(vars$valores_tabla_ordenados))
  }

  # Restaurar el estado de .Random.seed
  if (!is.null(saved_seed)) {
    assign(".Random.seed", saved_seed, envir = globalenv())
  } else {
    rm(".Random.seed", envir = globalenv())
  }

  # expect_gt() NO admite `info` (no es parte de su firma); usar expect_true()
  # para conservar el mensaje diagnostico. Umbral: >90 de 100 semillas unicas.
  expect_true(length(unique(hashes_valores)) > 90,
              info = sprintf("Poca diversidad en los valores de la tabla: %d/100 unicos.",
                             length(unique(hashes_valores))))
})

test_that("La gráfica circular genera meses diferentes para cada semilla", {
  rmd_file <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/grafica-circular-consumo-agua/grafica_circular_consumo_agua_metacognitivo_argumentacion_n3_schoice_v2.Rmd"

  hashes_meses <- c()
  saved_seed <- if (exists(".Random.seed", envir = globalenv())) {
    get(".Random.seed", envir = globalenv())
  } else NULL

  for (i in 1:100) {
    set.seed(i)
    vars <- get_generated_vars(rmd_file)
    hashes_meses <- c(hashes_meses, digest::digest(vars$meses_ordenados))
  }

  if (!is.null(saved_seed)) {
    assign(".Random.seed", saved_seed, envir = globalenv())
  } else {
    rm(".Random.seed", envir = globalenv())
  }
  # UMBRAL RECALIBRADO (2026-07-30). El original exigia > 90, que estaba clavado
  # en la MEDIA de la distribucion: `meses_ordenados` vive en un espacio de ~489
  # valores distintos (medido sobre 2000 semillas), asi que al muestrear 100 con
  # reemplazo el numero esperado de unicos es 489*(1-(488/489)^100) = 90,5 con
  # sigma ~ 2,5. Un umbral de 90 esta a -0,2 sigma: el test pasaba o fallaba casi
  # a cara o cruz segun el conjunto de semillas (el 95 observado era suerte, no
  # holgura). 80 queda ~4 sigma por debajo de lo esperado: detecta una caida real
  # de diversidad sin fallar por ruido de muestreo.
  expect_true(length(unique(hashes_meses)) > 80,
              info = sprintf("Poca diversidad en los meses de la tabla: %d/100 unicos (esperado ~90 sobre un espacio de ~489).",
                             length(unique(hashes_meses))))
})

test_that("El error de la gráfica (mes mayor/menor invertido) cambia con cada semilla", {
  rmd_file <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/grafica-circular-consumo-agua/grafica_circular_consumo_agua_metacognitivo_argumentacion_n3_schoice_v2.Rmd"

  hashes_errores <- c()
  saved_seed <- if (exists(".Random.seed", envir = globalenv())) {
    get(".Random.seed", envir = globalenv())
  } else NULL

  for (i in 1:100) {
    set.seed(i)
    vars <- get_generated_vars(rmd_file)
    # Hash combinado del mes mayor y menor para capturar la esencia del error
    hashes_errores <- c(hashes_errores, digest::digest(paste0(vars$mes_mayor, vars$mes_menor)))
  }

  if (!is.null(saved_seed)) {
    assign(".Random.seed", saved_seed, envir = globalenv())
  } else {
    rm(".Random.seed", envir = globalenv())
  }
  # UMBRAL RECALIBRADO (2026-07-30). El original exigia > 90 y era imposible de
  # cumplir: el par (mes_mayor, mes_menor) vive en un espacio de 132 valores
  # (12 x 11 meses), medido sobre 1000 semillas. Al muestrear 100 con reemplazo,
  # el numero ESPERADO de unicos es 132 * (1 - (131/132)^100) = 70,3 — y lo
  # observado fue 71, es decir, el ejercicio se comporta como la teoria predice.
  # Superar 90 exigiria un espacio de ~470 pares, inalcanzable con 12 meses.
  # 60 deja ~10 de margen por debajo de lo esperado: detecta una degradacion
  # real de la diversidad sin fallar por ruido de muestreo.
  expect_true(length(unique(hashes_errores)) > 60,
              info = sprintf("Poca diversidad en los meses del error de grafica: %d/100 unicos (esperado ~70 sobre un espacio de 132).",
                             length(unique(hashes_errores))))
})
