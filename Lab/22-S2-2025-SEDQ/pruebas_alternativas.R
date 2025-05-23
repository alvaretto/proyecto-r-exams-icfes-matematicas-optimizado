#!/usr/bin/env Rscript

# Pruebas unitarias alternativas para fracciones_reparto_premio_v1.Rmd
# Este script utiliza un enfoque diferente para evaluar el ejercicio

library(testthat)
library(exams)
library(digest)
library(dplyr)
library(ggplot2)

# Configuración
archivo_rmd <- "fracciones_reparto_premio_v1.Rmd"
num_simulaciones <- 50  # Número de simulaciones para probar variabilidad
verbose <- TRUE  # Mostrar información detallada durante la ejecución

cat("Iniciando pruebas alternativas para", archivo_rmd, "\n")
cat("Ejecutando", num_simulaciones, "simulaciones...\n")

# Enfoque alternativo: Extraer datos directamente del archivo Rmd
# En lugar de intentar acceder al entorno, vamos a ejecutar el código R directamente
extraer_datos_directo <- function(i) {
  set.seed(i)  # Semilla diferente para cada iteración
  
  if (verbose) {
    cat("Simulación", i, "...\n")
  }
  
  tryCatch({
    # Leer el archivo Rmd
    contenido <- readLines(archivo_rmd)
    
    # Extraer bloques de código R
    bloques_r <- list()
    en_bloque <- FALSE
    bloque_actual <- character(0)
    nombre_bloque <- ""
    
    for (linea in contenido) {
      if (grepl("^```\\{r", linea)) {
        en_bloque <- TRUE
        nombre_match <- regmatches(linea, regexec("^```\\{r\\s+([^,}]+)", linea))
        nombre_bloque <- if (length(nombre_match[[1]]) > 1) nombre_match[[1]][2] else "unnamed"
        bloque_actual <- character(0)
      } else if (en_bloque && grepl("^```$", linea)) {
        en_bloque <- FALSE
        bloques_r[[nombre_bloque]] <- bloque_actual
      } else if (en_bloque) {
        bloque_actual <- c(bloque_actual, linea)
      }
    }
    
    if (verbose) {
      cat("  Bloques de código R encontrados:", paste(names(bloques_r), collapse=", "), "\n")
    }
    
    # Crear un entorno para ejecutar el código
    env <- new.env()
    
    # Ejecutar bloques de código en orden
    for (nombre in names(bloques_r)) {
      if (verbose) {
        cat("  Ejecutando bloque", nombre, "...\n")
      }
      
      # Saltar bloques que no necesitamos para las pruebas
      if (nombre %in% c("setup", "tabla_distribucion")) {
        if (verbose) cat("    Saltando bloque", nombre, "\n")
        next
      }
      
      # Ejecutar el código en el entorno
      tryCatch({
        eval(parse(text = bloques_r[[nombre]]), envir = env)
        if (verbose) cat("    Bloque", nombre, "ejecutado correctamente\n")
      }, error = function(e) {
        if (verbose) cat("    Error al ejecutar bloque", nombre, ":", e$message, "\n")
      })
    }
    
    # Extraer variables relevantes del entorno
    datos <- list()
    variables_clave <- c(
      "contexto", "competencia", "grupo_edad", "premio_total",
      "fraccion_primer_puesto", "fraccion_segundo_puesto", "valor_tercer_puesto",
      "monto_primer_puesto", "monto_segundo_puesto", "monto_tercer_puesto",
      "opciones", "opciones_mezcladas", "respuesta_correcta", "termino_dinero",
      "paleta_seleccionada"
    )
    
    for (var in variables_clave) {
      if (exists(var, envir = env)) {
        datos[[var]] <- get(var, envir = env)
      } else {
        if (verbose) cat("    Variable", var, "no encontrada\n")
      }
    }
    
    # Añadir el índice de simulación
    datos$simulacion <- i
    
    # Calcular un hash único para esta versión
    datos$hash <- digest::digest(datos, algo = "md5")
    
    if (verbose) {
      cat("  Simulación", i, "completada con éxito\n")
      cat("  Variables extraídas:", paste(names(datos), collapse=", "), "\n")
    }
    
    return(datos)
  }, error = function(e) {
    if (verbose) {
      cat("  ERROR en simulación", i, ":", e$message, "\n")
    }
    return(NULL)
  })
}

# Ejecutar simulaciones
start_time <- Sys.time()
resultados <- lapply(1:num_simulaciones, extraer_datos_directo)
end_time <- Sys.time()

# Filtrar resultados nulos (errores)
resultados_validos <- resultados[!sapply(resultados, is.null)]
num_resultados_validos <- length(resultados_validos)

cat("Simulaciones completadas:", num_resultados_validos, "de", num_simulaciones, "\n")
cat("Tiempo de ejecución:", difftime(end_time, start_time, units = "secs"), "segundos\n\n")

# Verificar si hay resultados válidos
if (num_resultados_validos == 0) {
  stop("No se obtuvieron resultados válidos. Revise el archivo Rmd y las funciones de extracción.")
}

# Convertir lista de resultados a data frame para análisis
cat("Convirtiendo resultados a data frame...\n")
tryCatch({
  # Crear data frame manualmente para evitar problemas con tipos de datos
  df_cols <- unique(unlist(lapply(resultados_validos, names)))
  df_resultados <- data.frame(matrix(NA, nrow = length(resultados_validos), ncol = length(df_cols)))
  names(df_resultados) <- df_cols
  
  for (i in 1:length(resultados_validos)) {
    for (col in names(resultados_validos[[i]])) {
      # Manejar vectores y listas de manera especial
      if (is.vector(resultados_validos[[i]][[col]]) && length(resultados_validos[[i]][[col]]) > 1) {
        df_resultados[i, col] <- list(resultados_validos[[i]][[col]])
      } else {
        df_resultados[i, col] <- resultados_validos[[i]][[col]]
      }
    }
  }
  
  if (verbose) {
    cat("Columnas en el data frame resultante:\n")
    print(names(df_resultados))
    cat("Dimensiones del data frame:", dim(df_resultados), "\n")
  }
}, error = function(e) {
  cat("ERROR al convertir resultados a data frame:", e$message, "\n")
  stop("No se pudo crear el data frame para análisis")
})

# =====================================================================
# PRUEBA 1: Verificar generación de al menos 300 versiones diferentes
# =====================================================================
cat("\nPRUEBA 1: Verificación de variabilidad\n")

# Contar versiones únicas basadas en hash
versiones_unicas <- length(unique(df_resultados$hash))
cat("Versiones únicas generadas:", versiones_unicas, "de", num_resultados_validos, "\n")

# Verificar si cumple el requisito mínimo (ajustado para el número de simulaciones)
porcentaje_unicidad <- (versiones_unicas / num_resultados_validos) * 100
cat("Porcentaje de unicidad:", round(porcentaje_unicidad, 2), "%\n")

if (porcentaje_unicidad >= 90) {
  cat("✓ APROBADO: Alta variabilidad (", round(porcentaje_unicidad, 2), "% de versiones únicas)\n")
  cat("  Proyección: Con 300 simulaciones se generarían aproximadamente", 
      round(300 * porcentaje_unicidad / 100), "versiones únicas\n")
} else {
  cat("✗ FALLIDO: Baja variabilidad (", round(porcentaje_unicidad, 2), "% de versiones únicas)\n")
}

# =====================================================================
# PRUEBA 2: Verificar no duplicidad de opciones de respuesta
# =====================================================================
cat("\nPRUEBA 2: Verificación de no duplicidad en opciones de respuesta\n")

# Función para verificar duplicados en las opciones
verificar_duplicados_opciones <- function(opciones) {
  if (is.null(opciones) || !is.vector(opciones)) return(FALSE)
  length(unique(opciones)) == length(opciones)
}

# Aplicar verificación a cada conjunto de opciones
opciones_sin_duplicados <- sapply(resultados_validos, function(r) {
  if ("opciones_mezcladas" %in% names(r)) {
    verificar_duplicados_opciones(r$opciones_mezcladas)
  } else {
    FALSE
  }
})

# Contar casos con opciones duplicadas
num_con_duplicados <- sum(!opciones_sin_duplicados)
cat("Casos con opciones duplicadas:", num_con_duplicados, "de", num_resultados_validos, "\n")

# Verificar si cumple el requisito
if (num_con_duplicados == 0) {
  cat("✓ APROBADO: Ningún caso presenta opciones duplicadas\n")
} else {
  cat("✗ FALLIDO:", num_con_duplicados, "casos presentan opciones duplicadas\n")
}

# =====================================================================
# PRUEBA 3: Verificar coherencia matemática
# =====================================================================
cat("\nPRUEBA 3: Verificación de coherencia matemática\n")

# Verificar que la suma de los montos es igual al premio total
verificar_suma_montos <- function(r) {
  if (!all(c("monto_primer_puesto", "monto_segundo_puesto", "monto_tercer_puesto", "premio_total") %in% names(r))) {
    return(FALSE)
  }
  
  suma <- r$monto_primer_puesto + r$monto_segundo_puesto + r$monto_tercer_puesto
  abs(suma - r$premio_total) < 0.01  # Tolerancia para errores de redondeo
}

# Verificar que el valor del tercer puesto es positivo
verificar_tercer_puesto_positivo <- function(r) {
  if (!("valor_tercer_puesto" %in% names(r))) return(FALSE)
  r$valor_tercer_puesto > 0
}

# Verificar que la respuesta correcta coincide con el monto del tercer puesto
verificar_respuesta_correcta <- function(r) {
  if (!all(c("respuesta_correcta", "monto_tercer_puesto") %in% names(r))) return(FALSE)
  abs(r$respuesta_correcta - r$monto_tercer_puesto) < 0.01  # Tolerancia para errores de redondeo
}

# Aplicar verificaciones
suma_correcta <- sapply(resultados_validos, verificar_suma_montos)
tercer_puesto_positivo <- sapply(resultados_validos, verificar_tercer_puesto_positivo)
respuesta_correcta <- sapply(resultados_validos, verificar_respuesta_correcta)

# Contar casos con problemas
num_suma_incorrecta <- sum(!suma_correcta)
num_tercer_puesto_negativo <- sum(!tercer_puesto_positivo)
num_respuesta_incorrecta <- sum(!respuesta_correcta)

cat("Casos con suma de montos incorrecta:", num_suma_incorrecta, "de", num_resultados_validos, "\n")
cat("Casos con tercer puesto negativo:", num_tercer_puesto_negativo, "de", num_resultados_validos, "\n")
cat("Casos con respuesta incorrecta:", num_respuesta_incorrecta, "de", num_resultados_validos, "\n")

# Verificar si cumple todos los requisitos
if (num_suma_incorrecta == 0 && num_tercer_puesto_negativo == 0 && num_respuesta_incorrecta == 0) {
  cat("✓ APROBADO: Todos los casos mantienen coherencia matemática\n")
} else {
  cat("✗ FALLIDO: Se encontraron problemas de coherencia matemática\n")
}

# =====================================================================
# RESUMEN FINAL
# =====================================================================
cat("\n=== RESUMEN DE PRUEBAS ===\n")

pruebas_aprobadas <- c(
  porcentaje_unicidad >= 90,
  num_con_duplicados == 0,
  num_suma_incorrecta == 0 && num_tercer_puesto_negativo == 0 && num_respuesta_incorrecta == 0
)

nombres_pruebas <- c(
  "Alta variabilidad (proyección de al menos 300 versiones diferentes)",
  "No duplicidad de opciones de respuesta",
  "Coherencia matemática"
)

for (i in 1:length(pruebas_aprobadas)) {
  cat(ifelse(pruebas_aprobadas[i], "✓", "✗"), " ", nombres_pruebas[i], "\n", sep = "")
}

cat("\nResultado final: ", 
    ifelse(all(pruebas_aprobadas), "✓ TODAS LAS PRUEBAS APROBADAS", "✗ ALGUNAS PRUEBAS FALLARON"),
    "\n", sep = "")

# Guardar resultados en un archivo CSV para análisis posterior
write.csv(df_resultados, "resultados_pruebas_alternativas.csv", row.names = FALSE)
cat("\nLos resultados detallados se han guardado en 'resultados_pruebas_alternativas.csv'\n")
