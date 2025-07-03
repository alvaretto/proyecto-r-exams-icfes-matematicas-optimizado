# Script para Pruebas Unitarias de crecimiento_exponencial_valor_inicial_v1.Rmd

# --- Configuración ---
archivo_rmd <- "crecimiento_exponencial_valor_inicial_v1.Rmd"
numero_de_iteraciones <- 400 # Un poco más de 300 para tener margen
set.seed(Sys.time()) # Usar una semilla diferente cada vez que se ejecutan las pruebas

cat("Iniciando pruebas para:", archivo_rmd, "\n")
cat("Número de iteraciones:", numero_de_iteraciones, "\n\n")

# --- Funciones Auxiliares ---

# Función para extraer el contenido de un chunk específico de un archivo .Rmd
extraer_codigo_chunk <- function(ruta_archivo_rmd, nombre_chunk) {
  tryCatch({
    contenido_rmd <- readLines(ruta_archivo_rmd, warn = FALSE)
    
    # Patrón para encontrar el inicio y fin del chunk
    patron_inicio_chunk <- paste0("```{r ", nombre_chunk)
    patron_fin_chunk <- "```"
    
    lineas_inicio <- grep(patron_inicio_chunk, contenido_rmd, fixed = TRUE)
    if (length(lineas_inicio) == 0) {
      stop("No se encontró el chunk '", nombre_chunk, "' en el archivo.")
    }
    # Tomar la primera ocurrencia si hay múltiples chunks con nombres similares (no debería)
    linea_inicio <- lineas_inicio[1] 
    
    # Encontrar el fin del chunk a partir de la línea de inicio
    lineas_fin_potenciales <- grep(patron_fin_chunk, contenido_rmd, fixed = TRUE)
    linea_fin <- min(lineas_fin_potenciales[lineas_fin_potenciales > linea_inicio])
    
    if (is.infinite(linea_fin)) {
      stop("No se encontró el final del chunk '", nombre_chunk, "'.")
    }
    
    # Extraer las líneas de código del chunk (excluyendo la línea de inicio y fin del chunk)
    codigo_chunk <- contenido_rmd[(linea_inicio + 1):(linea_fin - 1)]
    return(paste(codigo_chunk, collapse = "\n"))
  }, error = function(e) {
    message("Error extrayendo el chunk '", nombre_chunk, "': ", e$message)
    return(NULL)
  })
}

# --- Inicialización de Contadores y Almacenamiento ---
firmas_preguntas <- character() # Para almacenar firmas únicas de las preguntas
errores_duplicidad_opciones <- 0
errores_coherencia_matematica_num_correctas <- 0
errores_coherencia_matematica_valor_correcto <- 0
errores_opciones_na <- 0
total_ejecuciones_exitosas_chunk <- 0

# --- Extracción del Código del Chunk ---
cat("Extrayendo código del chunk 'DefinicionDeVariables'...\n")
codigo_definicion_variables <- extraer_codigo_chunk(archivo_rmd, "DefinicionDeVariables")

if (is.null(codigo_definicion_variables)) {
  stop("No se pudo extraer el código del chunk. Abortando pruebas.")
}
cat("Código del chunk extraído exitosamente.\n\n")

# Cargar paquetes necesarios dentro del entorno de prueba si es necesario
# (glue ya está en el Rmd, pero por si acaso para el eval)
if (!requireNamespace("glue", quietly = TRUE)) {
  install.packages("glue")
}
library(glue)


# --- Bucle Principal de Pruebas ---
cat("Ejecutando iteraciones de prueba...\n")
for (i in 1:numero_de_iteraciones) {
  if (i %% 50 == 0) {
    cat("Iteración:", i, "/", numero_de_iteraciones, "\n")
  }
  
  entorno_prueba <- new.env()
  
  # Ejecutar el código del chunk en el nuevo entorno
  # Se incluye Sys.setlocale y options(OutDec) por si afectan la generación de números
  # y no están en el chunk 'DefinicionDeVariables' pero sí en 'setup'.
  # El set.seed ya está en el chunk 'DefinicionDeVariables'.
  tryCatch({
    eval(parse(text = "Sys.setlocale(category = 'LC_NUMERIC', locale = 'C'); options(OutDec = '.')"), envir = entorno_prueba)
    eval(parse(text = codigo_definicion_variables), envir = entorno_prueba)
    total_ejecuciones_exitosas_chunk <- total_ejecuciones_exitosas_chunk + 1
  }, error = function(e) {
    cat("Error al ejecutar el chunk en la iteración", i, ":", e$message, "\n")
    # Saltar esta iteración si el chunk falla
    return() 
  })
  
  # Verificar que las variables esperadas existan en el entorno
  variables_esperadas <- c("coef_inicial", "base_exponencial", "factor_tiempo", 
                           "organismo_plural", "unidad_tiempo_plural", "funcion_texto",
                           "opciones_ordenadas", "solucion_logica", "respuesta_correcta")
  
  existen_todas <- all(sapply(variables_esperadas, exists, envir = entorno_prueba))
  
  if (!existen_todas) {
    cat("Advertencia: No todas las variables esperadas fueron generadas en la iteración", i, ". Saltando validaciones para esta iteración.\n")
    variables_faltantes <- variables_esperadas[!sapply(variables_esperadas, exists, envir = entorno_prueba)]
    cat("Variables faltantes:", paste(variables_faltantes, collapse=", "), "\n")
    next # Saltar al siguiente ciclo de la iteración
  }

  # Extraer variables del entorno de prueba
  opciones <- entorno_prueba$opciones_ordenadas
  solucion <- entorno_prueba$solucion_logica
  resp_correcta_calculada <- entorno_prueba$respuesta_correcta
  
  # 1. Prueba de NAs en opciones
  if (any(is.na(opciones))) {
    errores_opciones_na <- errores_opciones_na + 1
  }

  # 2. Prueba de no duplicidad de opciones
  # Solo si no hay NAs, porque unique(c(1,2,NA,NA)) tiene longitud 3
  if (!any(is.na(opciones))) {
    if (length(unique(opciones)) != 4) {
      errores_duplicidad_opciones <- errores_duplicidad_opciones + 1
    }
  } else {
    # Si hay NAs, la prueba de duplicidad no es directamente aplicable de esta forma,
    # pero el error de NA ya fue contado.
  }

  # 3. Prueba de coherencia matemática
  #   a. Debe haber exactamente una solución correcta marcada como TRUE
  if (sum(solucion, na.rm = TRUE) != 1) { # na.rm por si 'solucion' tuviera NAs (no debería si opciones no los tiene)
    errores_coherencia_matematica_num_correctas <- errores_coherencia_matematica_num_correctas + 1
  }
  
  #   b. El valor de la opción marcada como correcta debe ser igual a 'respuesta_correcta'
  #      Solo si hay exactamente una solución TRUE y no hay NAs en 'opciones' donde 'solucion' es TRUE
  if (sum(solucion, na.rm = TRUE) == 1) {
    indice_correcto_marcado <- which(solucion)
    if (length(indice_correcto_marcado) == 1 && !is.na(opciones[indice_correcto_marcado])) {
      if (opciones[indice_correcto_marcado] != resp_correcta_calculada) {
        errores_coherencia_matematica_valor_correcto <- errores_coherencia_matematica_valor_correcto + 1
      }
    } else if (length(indice_correcto_marcado) == 1 && is.na(opciones[indice_correcto_marcado])) {
      # Si la opción marcada como correcta es NA, es un error de valor
       errores_coherencia_matematica_valor_correcto <- errores_coherencia_matematica_valor_correcto + 1
    }
  }
  
  # 4. Generar firma para la pregunta y almacenarla (para prueba de diversidad)
  #    La firma debe incluir todos los elementos que hacen una pregunta "única" visualmente y conceptualmente.
  firma_actual <- digest::digest(
    list(
      entorno_prueba$coef_inicial,
      entorno_prueba$base_exponencial,
      entorno_prueba$factor_tiempo,
      entorno_prueba$organismo_plural,
      entorno_prueba$unidad_tiempo_plural,
      entorno_prueba$funcion_texto, # La función matemática tal como se muestra
      # Incluir las opciones también, ya que su variación contribuye a la "versión"
      # Ordenarlas para que el orden de presentación no afecte la firma de unicidad de la pregunta base
      sort(opciones) 
    )
  )
  firmas_preguntas <- c(firmas_preguntas, firma_actual)
}
cat("Proceso de iteraciones completado.\n\n")

# --- Análisis de Resultados ---
cat("--- Resumen de Pruebas ---\n")

# Prueba de diversidad de preguntas
numero_versiones_unicas <- length(unique(firmas_preguntas))
cat("1. Número de versiones únicas de preguntas generadas:", numero_versiones_unicas, "\n")
if (numero_versiones_unicas >= 300) {
  cat("   Resultado: ÉXITO (>= 300 versiones únicas)\n")
} else {
  cat("   Resultado: FALLO (< 300 versiones únicas)\n")
}

# Prueba de NAs en opciones
cat("2. Número de iteraciones con NAs en las opciones de respuesta:", errores_opciones_na, "\n")
if (errores_opciones_na == 0) {
  cat("   Resultado: ÉXITO (Sin NAs en opciones)\n")
} else {
  cat("   Resultado: FALLO\n")
}

# Prueba de duplicidad de opciones
cat("3. Número de iteraciones con opciones de respuesta duplicadas:", errores_duplicidad_opciones, "\n")
if (errores_duplicidad_opciones == 0) {
  cat("   Resultado: ÉXITO (Sin opciones duplicadas)\n")
} else {
  cat("   Resultado: FALLO\n")
}

# Pruebas de coherencia matemática
cat("4. Coherencia matemática:\n")
cat("   a. Iteraciones con un número incorrecto de respuestas marcadas como TRUE:", 
    errores_coherencia_matematica_num_correctas, "\n")
if (errores_coherencia_matematica_num_correctas == 0) {
  cat("      Resultado: ÉXITO\n")
} else {
  cat("      Resultado: FALLO\n")
}

cat("   b. Iteraciones donde la opción marcada como TRUE no coincide con la respuesta correcta calculada:", 
    errores_coherencia_matematica_valor_correcto, "\n")
if (errores_coherencia_matematica_valor_correcto == 0) {
  cat("      Resultado: ÉXITO\n")
} else {
  cat("      Resultado: FALLO\n")
}

cat("\nTotal de ejecuciones del chunk 'DefinicionDeVariables' (sin errores de ejecución del chunk):", total_ejecuciones_exitosas_chunk, "de", numero_de_iteraciones, "\n")

if (total_ejecuciones_exitosas_chunk < numero_de_iteraciones) {
    cat("ADVERTENCIA: Algunas ejecuciones del chunk fallaron y no se pudieron probar.\n")
}

cat("\n--- Fin de las Pruebas ---\n")

# Sugerencia: Si hay fallos, se podrían guardar los entornos de las iteraciones problemáticas
# para un análisis más detallado.
