# --- Test Setup ---
library(testthat)
library(knitr)
library(reticulate)
# library(stringr) # No estrictamente necesario para estas pruebas, pero útil a menudo

# Define la ruta del archivo Rmd (asumiendo que está en el mismo directorio)
rmd_file <- "interpretacion_grafica_viaje.Rmd"
# Define el número de versiones a probar
N_VERSIONS <- 310 # Un poco más de 300 por seguridad


# --- Funciones Auxiliares ---

# Función para extraer y ejecutar código R del Rmd en un entorno controlado
run_rmd_r_code <- function(file_path) {
  matplotlib_available <- FALSE # Por defecto FALSE
  matplotlib_check_warning <- NULL # Almacena cualquier aviso durante la verificación

  # Verifica si matplotlib está potencialmente disponible
  # Usa tryCatch específicamente alrededor de la llamada que podría fallar
  matplotlib_available <- tryCatch({
    # Puede necesitar reticulate::use_python('/ruta/a/python', required=TRUE) fuera de esta función una vez.
    reticulate::py_available("matplotlib")
  }, warning = function(w) {
    matplotlib_check_warning <<- paste("Advertencia durante la verificación de matplotlib:", w$message)
    FALSE # Asume no disponible si la verificación advierte
  }, error = function(e) {
    matplotlib_check_warning <<- paste("No se pudo verificar la disponibilidad de matplotlib:", e$message)
    FALSE # Asume no disponible si la verificación da error
  })

  # Imprime la advertencia si la verificación falló, solo una vez
  if (!is.null(matplotlib_check_warning)) {
    # Usamos message en lugar de warning para no interrumpir flujos de testthat que paran en warnings
    message("Nota sobre Matplotlib: ", matplotlib_check_warning)
  }


  # Crea un archivo temporal para el código R extraído
  temp_r_file <- tempfile(fileext = ".R")
  # Asegura la limpieza del archivo R temporal
  on.exit(unlink(temp_r_file, force = TRUE), add = TRUE)

  # Extrae los chunks de código R usando purl
  tryCatch({
    # Redirige la salida a NULL para suprimir posible salida markdown durante purl
    suppressMessages(utils::capture.output(
      knitr::purl(file_path, output = temp_r_file, documentation = 0, quiet = TRUE)
    ))
  }, error = function(e) {
    stop("Falló al extraer (purl) el código R del Rmd: ", e$message)
  })

  # Crea un nuevo entorno para ejecutar el código
  test_env <- new.env(parent = globalenv())
  source_error <- NULL # Para almacenar error de source

  # Ejecuta (source) el código R extraído en el nuevo entorno
  # Envuelve en tryCatch para capturar errores durante la ejecución del código Rmd
  tryCatch({
    # También captura la salida durante source para evitar desorden en la consola
    suppressMessages(utils::capture.output(
      source(temp_r_file, local = test_env, echo = FALSE)
    ))
  }, error = function(e) {
    # Guarda el error si source() falla
    source_error <<- e
  })

  # Devuelve el entorno, el error (si ocurrió) y el estado de matplotlib
  return(list(env = test_env, error = source_error, matplotlib = matplotlib_available))

}

# --- Suite de Pruebas ---
# Contexto para la suite de pruebas
context(paste("Probando generación de", rmd_file, "(", N_VERSIONS, "versiones )"))

# Almacena resultados entre iteraciones para verificar variedad
generated_conductors <- vector("character", N_VERSIONS) # Preasignar vector
generated_initial_fuel <- vector("numeric", N_VERSIONS)
generated_final_distance <- vector("numeric", N_VERSIONS)
generated_final_fuel <- vector("numeric", N_VERSIONS) # Comprobación añadida
generated_sol_strings <- vector("character", N_VERSIONS)
generated_option_orders <- vector("character", N_VERSIONS) # Almacena orden como string
all_runs_passed_basic <- TRUE # Bandera para rastrear si alguna ejecución falló comprobaciones esenciales

# --- Bucle Principal de Pruebas ---
for (i in 1:N_VERSIONS) {

  # --- Ejecutar Código Rmd UNA VEZ por Iteración ---
  run_result <- run_rmd_r_code(rmd_file)
  test_env <- run_result$env
  run_error <- run_result$error
  matplotlib_ok <- run_result$matplotlib # Estado de la verificación/disponibilidad de matplotlib

  # Probar cada generación de versión individualmente
  test_that(paste("Versión", i, "- Ejecución de Código y Comprobaciones Básicas"), {

    # 1. Comprobar errores de ejecución del código R
    if (!is.null(run_error)) {
      all_runs_passed_basic <<- FALSE # Marcar fallo para la comprobación final de variedad
      fail(paste("La ejecución del código R falló en la ejecución", i, ":", run_error$message))
    }
    # Saltar comprobaciones adicionales *en este bloque* si la ejecución falló
    skip_if(!is.null(run_error), "Saltando comprobaciones básicas debido a error en ejecución de código R.")

    # 2. Comprobar si se crearon los objetos R esenciales
    expect_true(exists("conductor", envir = test_env))#, info = "Objeto 'conductor' debe existir.") # Eliminado info
    expect_true(exists("datos", envir = test_env))#, info = "Objeto 'datos' debe existir.")
    expect_true(exists("opciones", envir = test_env))#, info = "Objeto 'opciones' debe existir.")
    expect_true(exists("solucion", envir = test_env))#, info = "Objeto 'solucion' debe existir.")
    expect_true(exists("sol_string", envir = test_env))#, info = "Objeto 'sol_string' debe existir.")
    expect_true(exists("tiempos", envir = test_env))#, info = "Objeto 'tiempos' debe existir.")
    expect_true(exists("costo_viaje", envir = test_env))#, info = "Objeto 'costo_viaje' debe existir.")
    expect_true(exists("combustible", envir = test_env))#, info = "Objeto 'combustible' debe existir.")
    expect_true(exists("distancia", envir = test_env))#, info = "Objeto 'distancia' debe existir.")

    # 3. Información del estado de dependencia de Python (mensaje opcional)
    #    No usamos expect_true aquí, ya que fallar la verificación/búsqueda no es un fallo de la ejecución de R en sí.
    if (!matplotlib_ok) {
      # Usamos message() en lugar de inform() para compatibilidad
      message("Nota: La verificación de Matplotlib devolvió falso o falló. Los chunks de Python podrían no haberse ejecutado correctamente.")
    }

    # Almacenar resultados para comprobaciones de variedad posteriores SOLO SI la ejecución básica pasó
    # (Este bloque se salta si run_error no fue NULL)
    # Añadimos una comprobación extra por si acaso run_error es NULL pero falta 'datos'
    if (exists("datos", envir=test_env) && exists("conductor", envir=test_env) && exists("sol_string", envir=test_env) && exists("opciones", envir=test_env)) {
      generated_conductors[i] <- test_env$conductor
      generated_initial_fuel[i] <- test_env$datos$Combustible[1]
      generated_final_distance[i] <- test_env$datos$Distancia[nrow(test_env$datos)]
      generated_final_fuel[i] <- test_env$datos$Combustible[nrow(test_env$datos)]
      generated_sol_strings[i] <- test_env$sol_string
      generated_option_orders[i] <- paste(test_env$opciones, collapse = "")
    } else {
      # Si faltan objetos clave incluso sin error reportado, marca como fallo básico
      all_runs_passed_basic <<- FALSE
      warning(paste("Advertencia en ejecución", i, ": Faltan objetos R clave incluso sin error explícito de ejecución."), call. = FALSE)
    }


  }) # Fin test_that para Ejecución Versión i

  # Probar coherencia matemática/lógica para la versión i
  test_that(paste("Versión", i, "- Comprobaciones de Coherencia de Datos"), {
    # Saltar este bloque completo si la ejecución inicial del código R falló
    skip_if(!is.null(run_error), "Saltando pruebas de coherencia debido a error en ejecución de código R.")
    # Saltar también si faltan los datos necesarios
    skip_if(!exists("datos", envir = test_env), "Saltando pruebas de coherencia porque 'datos' no existe.")

    # ---- Ahora realizar comprobaciones usando 'test_env' poblado anteriormente ----

    # Extraer objetos para evitar problemas con el entorno en las expectations
    # (Aunque no es seguro que sea necesario, es más robusto)
    datos_actuales <- test_env$datos
    conductor_actual <- test_env$conductor
    opciones_actuales <- test_env$opciones
    solucion_actual <- test_env$solucion
    sol_string_actual <- test_env$sol_string

    # Comprobar Estructura de Datos
    skip_if(!is.data.frame(datos_actuales), "'datos' no es un data frame.") # Comprueba antes de usar expect_s3_class
    expect_s3_class(datos_actuales, "data.frame")#, info = "Objeto 'datos' debe ser un data frame.") # Eliminado info
    expect_equal(ncol(datos_actuales), 4)#, info = "Data frame 'datos' debe tener 4 columnas.")
    expect_equal(nrow(datos_actuales), 5)#, info = "Data frame 'datos' debe tener 5 filas (verificar si tiempo cambió).")
    expect_equal(colnames(datos_actuales), c("Tiempo", "Costo", "Combustible", "Distancia"))#, info = "Nombres de columna de 'datos' no coinciden.")

    # Comprobar Tiempo (debería ser fijo)
    expect_equal(datos_actuales$Tiempo, c(0, 2, 4, 6, 8))#, info = "Secuencia de Tiempo debe ser 0, 2, 4, 6, 8.")

    # Comprobar Conductor (debería ser uno de los predefinidos)
    possible_conductores <- c("un conductor", "una conductora", "un taxista", "un mensajero", "un repartidor")
    expect_true(conductor_actual %in% possible_conductores)#, info = "'conductor' seleccionado no está en la lista predefinida.")

    # Comprobar Costo
    expect_equal(datos_actuales$Costo[1], 0)#, info = "Costo inicial debe ser 0.")
    expect_true(all(datos_actuales$Costo >= 0))#, info = "Todos los valores de Costo deben ser no negativos.")
    # Comprobar tendencia general (permitir ruido)
    expect_true( sum(diff(datos_actuales$Costo) >= -5) >= (length(datos_actuales$Costo) - 2) )#,
    # info = "Costo debe aumentar generalmente (permitiendo pequeño ruido).")
    expect_gt(datos_actuales$Costo[5], datos_actuales$Costo[2] - 5)#, info = "Costo final debe ser generalmente > que valores iniciales (permitiendo ruido).")

    # Comprobar Combustible
    expect_gt(datos_actuales$Combustible[1], 0)#, info = "Combustible inicial debe ser positivo.")
    expect_true(all(datos_actuales$Combustible >= 0))#, info = "Todos los valores de Combustible deben ser no negativos.")
    # Comprobar tendencia general (permitir ruido)
    expect_true( sum(diff(datos_actuales$Combustible) <= 5) >= (length(datos_actuales$Combustible) - 2) )#,
    #info = "Combustible debe disminuir generalmente (permitiendo pequeño ruido).")
    expect_lt(datos_actuales$Combustible[5], datos_actuales$Combustible[1] + 5)#, info = "Combustible final debe ser generalmente menor que inicial (permitiendo ruido).")
    expect_gte(datos_actuales$Combustible[1], datos_actuales$Combustible[2] - 5)#, info = "Combustible inicial debe ser >= segundo valor (permitiendo ruido).")

    # Comprobar Distancia
    expect_equal(datos_actuales$Distancia[1], 0)#, info = "Distancia inicial debe ser 0.")
    expect_true(all(datos_actuales$Distancia >= 0))#, info = "Todos los valores de Distancia deben ser no negativos.")
    # Comprobar tendencia general (permitir ruido)
    expect_true( sum(diff(datos_actuales$Distancia) >= -5) >= (length(datos_actuales$Distancia) - 2) )#,
    #info = "Distancia debe aumentar generalmente (permitiendo pequeño ruido).")
    expect_gt(datos_actuales$Distancia[5], datos_actuales$Distancia[2] - 5)#, info = "Distancia final debe ser generalmente > que valores iniciales (permitiendo ruido).")
    expect_gt(datos_actuales$Distancia[5], 0 + 1e-9)#, info = "Distancia final debe ser mayor que 0.")

    # Comprobar Opciones y Cadena de Solución
    expect_type(opciones_actuales, "character")
    expect_equal(length(opciones_actuales), 4)#, info = "Debe haber 4 opciones.")
    expect_setequal(opciones_actuales, LETTERS[1:4])#, info = "Opciones deben ser una permutación de A, B, C, D.")

    expect_type(solucion_actual, "integer")
    expect_length(solucion_actual, 1)
    expect_true(solucion_actual %in% 1:4)#, info = "Índice de solución fuera de límites.")

    expect_type(sol_string_actual, "character")
    expect_length(sol_string_actual, 1)
    expect_match(sol_string_actual, "^[01]{4}$")#, info = "Cadena de solución debe ser 4 ceros o unos.")
    expect_equal(sum(as.numeric(strsplit(sol_string_actual, "")[[1]])), 1)#, info = "Cadena de solución debe contener exactamente un '1'.")

    # Comprobar si sol_string apunta correctamente a la posición de 'A'
    correct_position <- which(opciones_actuales == "A")
    expect_equal(solucion_actual, correct_position)#, info = "Variable 'solucion' debe coincidir con el índice de 'A' en 'opciones'.")
    expected_sol_vector <- rep(0, 4)
    expected_sol_vector[correct_position] <- 1
    expect_equal(sol_string_actual, paste(expected_sol_vector, collapse = ""))#, info = "Cadena de solución no coincide con la posición aleatoria de la respuesta correcta 'A'.")

  }) # Fin test_that para Coherencia Versión i

  # --- Limpieza de Archivos Generados ---
  # Borra los archivos de gráficos generados por esta iteración para evitar desorden
  plot_files <- c("grafica_A.png", "grafica_B.png", "grafica_C.png", "grafica_D.png", "grafica_solucion.png")
  # Suprime advertencias si los archivos no existen (p.ej., si Python falló)
  suppressWarnings(file.remove(plot_files[file.exists(plot_files)]))

} # Fin bucle principal de pruebas


# --- Comprobaciones Finales de Variedad ---
test_that(paste("Variedad a través de", N_VERSIONS, "ejecuciones"), {
  # Comprueba si la bandera indica que todas las ejecuciones pasaron la ejecución básica necesaria para la recolección de datos de variedad
  skip_if_not(all_runs_passed_basic, "Saltando comprobaciones de variedad porque algunas ejecuciones fallaron la ejecución básica de R.")
  # Comprobación adicional: asegurar que el bucle realmente terminó de recolectar datos válidos
  valid_entries <- generated_conductors != "" # Verifica entradas no vacías
  n_valid_entries <- sum(valid_entries)
  skip_if(n_valid_entries < (N_VERSIONS * 0.9), # Permite un pequeño % de fallos si es necesario
          paste("Saltando comprobaciones de variedad ya que un número significativo de iteraciones (",
                N_VERSIONS - n_valid_entries, ") no completó el almacenamiento de datos."))

  # Filtrar datos solo de ejecuciones válidas
  valid_conductors <- generated_conductors[valid_entries]
  valid_initial_fuel <- generated_initial_fuel[valid_entries]
  valid_final_distance <- generated_final_distance[valid_entries]
  valid_final_fuel <- generated_final_fuel[valid_entries]
  valid_option_orders <- generated_option_orders[valid_entries]
  valid_sol_strings <- generated_sol_strings[valid_entries]

  # Comprobar si se generaron múltiples conductores diferentes
  unique_conductors <- unique(valid_conductors)
  n_unique_conductors <- length(unique_conductors)
  print(paste("Conductores únicos generados:", n_unique_conductors, "de", length(possible_conductores), "posibles"))
  expect_gt(n_unique_conductors, 1)#, info = "Se esperaba más de 1 conductor único entre las ejecuciones.") # Eliminado info
  possible_conductores_count <- length(c("un conductor", "una conductora", "un taxista", "un mensajero", "un repartidor"))
  expect_gt(n_unique_conductors, min(possible_conductores_count - 2, n_valid_entries/50))#, info = "Se esperaba una variedad razonable de conductores.")

  # Comprobar si los datos numéricos varían
  unique_initial_fuel <- unique(round(valid_initial_fuel, 4))
  unique_final_dist <- unique(round(valid_final_distance, 4))
  unique_final_fuel <- unique(round(valid_final_fuel, 4))
  n_unique_initial_fuel <- length(unique_initial_fuel)
  n_unique_final_dist <- length(unique_final_dist)
  n_unique_final_fuel <- length(unique_final_fuel)

  print(paste("Valores únicos de combustible inicial (aprox):", n_unique_initial_fuel))
  print(paste("Valores únicos de distancia final (aprox):", n_unique_final_dist))
  print(paste("Valores únicos de combustible final (aprox):", n_unique_final_fuel))

  # Esperar una buena porción de valores únicos si la aleatorización es efectiva
  expect_gt(n_unique_initial_fuel, n_valid_entries * 0.1)#, info = "Se esperaba variación significativa (>10%) en combustible inicial.")
  expect_gt(n_unique_final_dist, n_valid_entries * 0.1)#, info = "Se esperaba variación significativa (>10%) en distancia final.")
  expect_gt(n_unique_final_fuel, n_valid_entries * 0.1)#, info = "Se esperaba variación significativa (>10%) en combustible final.")

  # Comprobar si el orden de las opciones varió
  unique_option_orders <- unique(valid_option_orders)
  n_unique_option_orders <- length(unique_option_orders)
  possible_orders_count <- factorial(4) # 4! = 24
  print(paste("Órdenes de opciones únicos generados:", n_unique_option_orders, "de", possible_orders_count, "posibles"))
  expect_gt(n_unique_option_orders, min(possible_orders_count / 2, n_valid_entries * 0.05))#, info = "Se esperaban múltiples órdenes de opciones diferentes.")

  # Comprobar si la cadena de solución varió (debido al cambio de orden de opciones)
  unique_sol_strings <- unique(valid_sol_strings)
  n_unique_sol_strings <- length(unique_sol_strings)
  print(paste("Cadenas de solución únicas generadas:", n_unique_sol_strings))
  expect_gt(n_unique_sol_strings, 1)#, info = "Se esperaba más de 1 cadena de solución única (debido al cambio de orden).")
  expect_lte(n_unique_sol_strings, 4)#, info = "No puede haber más de 4 cadenas de solución únicas para esta configuración.")
  # Debería idealmente coincidir con el número de posiciones únicas que tomó 'A'
  # expect_equal(n_unique_sol_strings, length(unique(substr(valid_option_orders, 1, 4))), info="Número de cadenas de solución únicas debe coincidir con número de posiciones únicas que tomó A") # Info eliminado

  # Confirmación final del número objetivo de variaciones *intentadas* y *exitosas*
  expect_equal(n_valid_entries, N_VERSIONS)#, info="El bucle debería haber completado N_VERSIONS veces y almacenado datos para (casi) todas.") # Ajustado a n_valid_entries

})
