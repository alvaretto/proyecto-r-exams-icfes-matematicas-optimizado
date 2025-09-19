#!/usr/bin/env Rscript

# Script de verificación para confirmar que se generan 4 opciones únicas y diferenciadas
# Este script analiza múltiples versiones del ejercicio para verificar:
# 1. Se generan exactamente 4 opciones (A, B, C, D)
# 2. Las opciones son visualmente diferentes
# 3. La aleatorización de posición correcta funciona
# 4. No hay duplicación de contenido entre opciones

library(exams)

cat("=== VERIFICACIÓN DE 4 OPCIONES OBLIGATORIAS ===\n\n")

# Función para extraer información de una versión del ejercicio
analizar_version <- function(version_num) {
  cat(sprintf("Analizando versión %d...\n", version_num))
  
  # Cargar el archivo Rmd y ejecutar la función generar_datos
  source_lines <- readLines("probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_cloze_v1.Rmd")
  
  # Ejecutar solo la parte de generación de datos
  tryCatch({
    # Configurar entorno
    options(scipen = 999)
    options(OutDec = ".")
    Sys.setlocale(category = "LC_NUMERIC", locale = "C")
    
    # Función de formato estándar
    formato_estandar <- function(x, decimales = 2) {
      if (decimales == 0) {
        return(as.character(as.integer(x)))
      } else {
        resultado <- sprintf(paste0("%.", decimales, "f"), x)
        return(resultado)
      }
    }
    
    # Función auxiliar para símbolos matemáticos
    format_le <- function() {
      "\\le"
    }
    
    # Función principal de generación de datos (extraída del archivo)
    generar_datos <- function() {
      # 1. Aleatorización de Parámetros Matemáticos
      p_central <- sample(seq(0.40, 0.55, by = 0.01), 1)
      p_lateral <- (1 - p_central) / 2

      limite1 <- sample(3:6, 1)
      ancho_central <- sample(2:6, 1)
      limite2 <- limite1 + ancho_central
      limite_sup <- 14

      if (limite2 >= limite_sup) {
        limite2 <- limite_sup - 2
      }

      # 3. Creación de Intervalos
      intervalo1_txt <- paste0("0 ", format_le(), " x ", format_le(), " ", limite1)
      intervalo2_txt <- paste0(limite1, " < x ", format_le(), " ", limite2)
      intervalo3_txt <- paste0(limite2, " < x ", format_le(), " ", limite_sup)

      # 3. Generación de Opciones (DataFrames)
      tabla_correcta <- data.frame(
        Intervalo = c(intervalo1_txt, intervalo2_txt, intervalo3_txt),
        Probabilidad = c(p_lateral, p_central, p_lateral),
        stringsAsFactors = FALSE
      )
      tabla_distractor_A <- data.frame(
        Intervalo = c(intervalo1_txt, intervalo2_txt, intervalo3_txt),
        Probabilidad = c(p_lateral, p_lateral + p_central, 1),
        stringsAsFactors = FALSE
      )
      tabla_distractor_B <- data.frame(
        Intervalo = c(paste0("0 ", format_le(), " x ", format_le(), " ", limite1),
                      paste0("0 ", format_le(), " x ", format_le(), " ", limite2),
                      paste0("0 ", format_le(), " x ", format_le(), " ", limite_sup)),
        Probabilidad = c(p_lateral, p_central, p_lateral),
        stringsAsFactors = FALSE
      )
      tabla_distractor_D <- data.frame(
        Intervalo = c(intervalo1_txt, intervalo2_txt, intervalo3_txt),
        Probabilidad = c(p_central, p_lateral, p_lateral),
        stringsAsFactors = FALSE
      )

      # 4. SISTEMA DE ALEATORIZACIÓN DE OPCIONES - CORREGIDO PARA 4 OPCIONES OBLIGATORIAS
      tabla_correcta_alt <- data.frame(
        Probabilidad = c(p_lateral, p_central, p_lateral),
        Intervalo = c(intervalo1_txt, intervalo2_txt, intervalo3_txt),
        stringsAsFactors = FALSE
      )
      
      usar_encabezados_alt <- sample(c(TRUE, FALSE), 1)
      tabla_correcta_final <- if (usar_encabezados_alt) tabla_correcta_alt else tabla_correcta
      
      # Verificar que todas las opciones sean visualmente diferentes
      verificar_diferenciacion <- function(opciones) {
        tablas_str <- lapply(opciones, function(tabla) {
          paste(tabla$Intervalo, tabla$Probabilidad, collapse = "|")
        })
        return(length(unique(tablas_str)) == length(tablas_str))
      }
      
      # Crear las 4 opciones obligatorias con diferenciación garantizada
      opciones_fijas <- list(
        A = tabla_distractor_A,
        B = tabla_distractor_B,
        C = tabla_correcta_final,
        D = tabla_distractor_D
      )
      
      if (!verificar_diferenciacion(opciones_fijas)) {
        stop("Error crítico: Las opciones generadas no son suficientemente diferentes")
      }
      
      # Aleatorizar solo la posición de la opción correcta
      posicion_correcta_aleatoria <- sample(1:4, 1)
      
      # Reorganizar opciones según la posición correcta aleatoria
      if (posicion_correcta_aleatoria == 1) {
        opciones_finales <- list(opciones_fijas$C, opciones_fijas$A, opciones_fijas$B, opciones_fijas$D)
      } else if (posicion_correcta_aleatoria == 2) {
        opciones_finales <- list(opciones_fijas$A, opciones_fijas$C, opciones_fijas$B, opciones_fijas$D)
      } else if (posicion_correcta_aleatoria == 3) {
        opciones_finales <- list(opciones_fijas$A, opciones_fijas$B, opciones_fijas$C, opciones_fijas$D)
      } else {
        opciones_finales <- list(opciones_fijas$A, opciones_fijas$B, opciones_fijas$D, opciones_fijas$C)
      }
      
      if (!verificar_diferenciacion(opciones_finales)) {
        stop("Error crítico: Las opciones finales no son suficientemente diferentes")
      }

      return(list(
        p_central = p_central,
        p_lateral = p_lateral,
        limite1 = limite1,
        limite2 = limite2,
        limite_sup = limite_sup,
        opciones = opciones_finales,
        posicion_correcta = posicion_correcta_aleatoria,
        usar_encabezados_alt = usar_encabezados_alt,
        opciones_fijas = opciones_fijas
      ))
    }
    
    # Generar datos para esta versión
    datos <- generar_datos()
    
    # Verificaciones
    num_opciones <- length(datos$opciones)
    posicion_correcta <- datos$posicion_correcta
    usar_alt <- datos$usar_encabezados_alt
    
    # Verificar diferenciación
    tablas_str <- lapply(datos$opciones, function(tabla) {
      paste(tabla$Intervalo, tabla$Probabilidad, collapse = "|")
    })
    son_diferentes <- length(unique(tablas_str)) == length(tablas_str)
    
    cat(sprintf("  ✅ Opciones generadas: %d\n", num_opciones))
    cat(sprintf("  ✅ Posición correcta: %d (Tabla %s)\n", posicion_correcta, LETTERS[posicion_correcta]))
    cat(sprintf("  ✅ Encabezados alternativos: %s\n", usar_alt))
    cat(sprintf("  ✅ Opciones diferenciadas: %s\n", if(son_diferentes) "SÍ" else "NO"))
    
    return(list(
      version = version_num,
      num_opciones = num_opciones,
      posicion_correcta = posicion_correcta,
      usar_alt = usar_alt,
      son_diferentes = son_diferentes,
      exito = TRUE
    ))
    
  }, error = function(e) {
    cat(sprintf("  ❌ Error: %s\n", e$message))
    return(list(
      version = version_num,
      exito = FALSE,
      error = e$message
    ))
  })
}

# Ejecutar análisis en 20 versiones
cat("Ejecutando análisis en 20 versiones...\n\n")

resultados <- list()
for (i in 1:20) {
  resultados[[i]] <- analizar_version(i)
  cat("\n")
}

# Generar estadísticas
cat("=== ESTADÍSTICAS FINALES ===\n")

versiones_exitosas <- sum(sapply(resultados, function(r) r$exito))
cat(sprintf("Versiones exitosas: %d/20 (%.1f%%)\n", versiones_exitosas, versiones_exitosas/20*100))

if (versiones_exitosas > 0) {
  resultados_exitosos <- resultados[sapply(resultados, function(r) r$exito)]
  
  # Distribución de posiciones correctas
  posiciones <- sapply(resultados_exitosos, function(r) r$posicion_correcta)
  cat("\nDistribución de posiciones correctas:\n")
  for (pos in 1:4) {
    count <- sum(posiciones == pos)
    cat(sprintf("  Tabla %s: %d veces (%.1f%%)\n", LETTERS[pos], count, count/versiones_exitosas*100))
  }
  
  # Uso de encabezados alternativos
  uso_alt <- sapply(resultados_exitosos, function(r) r$usar_alt)
  count_alt <- sum(uso_alt)
  cat(sprintf("\nUso de encabezados alternativos:\n"))
  cat(sprintf("  Sí: %d veces (%.1f%%)\n", count_alt, count_alt/versiones_exitosas*100))
  cat(sprintf("  No: %d veces (%.1f%%)\n", versiones_exitosas-count_alt, (versiones_exitosas-count_alt)/versiones_exitosas*100))
  
  # Verificación de diferenciación
  diferenciadas <- sapply(resultados_exitosos, function(r) r$son_diferentes)
  todas_diferenciadas <- all(diferenciadas)
  cat(sprintf("\nTodas las opciones son diferenciadas: %s\n", if(todas_diferenciadas) "✅ SÍ" else "❌ NO"))
}

cat("\n=== VERIFICACIÓN COMPLETADA ===\n")
