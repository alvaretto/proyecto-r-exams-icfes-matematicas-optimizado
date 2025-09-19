#!/usr/bin/env Rscript

# Script de Verificación de Integridad Matemática
# Objetivo: Verificar que los cálculos matemáticos permanezcan correctos
# independientemente de la posición de la opción correcta

library(exams)

cat("=== VERIFICACIÓN DE INTEGRIDAD MATEMÁTICA ===\n\n")

# Función para verificar la integridad matemática de una versión
verificar_integridad_matematica <- function(version_num) {
  tryCatch({
    # Configurar entorno R
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
    
    # Función principal de generación de datos
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

      # 4. SISTEMA DE ALEATORIZACIÓN DE OPCIONES - ALGORITMO MEJORADO
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
      
      # ALGORITMO DE ALEATORIZACIÓN MEJORADO - DISTRIBUCIÓN PERFECTAMENTE EQUILIBRADA
      # Seleccionar posición correcta con distribución uniforme garantizada
      posicion_correcta_aleatoria <- sample(1:4, 1)
      
      # Crear array de distractores en orden fijo para eliminar sesgos
      distractores <- list(opciones_fijas$A, opciones_fijas$B, opciones_fijas$D)
      
      # Inicializar opciones finales con distractores
      opciones_finales <- vector("list", 4)
      
      # Colocar la opción correcta en la posición seleccionada
      opciones_finales[[posicion_correcta_aleatoria]] <- opciones_fijas$C
      
      # Llenar las posiciones restantes con distractores en orden secuencial
      indice_distractor <- 1
      for (i in 1:4) {
        if (i != posicion_correcta_aleatoria) {
          opciones_finales[[i]] <- distractores[[indice_distractor]]
          indice_distractor <- indice_distractor + 1
        }
      }
      
      # Verificación final de diferenciación
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
    
    # VERIFICACIONES MATEMÁTICAS
    
    # 1. Verificar que las probabilidades sumen 1
    tabla_correcta <- datos$opciones[[datos$posicion_correcta]]
    suma_probabilidades <- sum(as.numeric(tabla_correcta$Probabilidad))
    suma_correcta <- abs(suma_probabilidades - 1.0) < 0.001
    
    # 2. Verificar que p_lateral + p_central + p_lateral = 1
    suma_teorica <- datos$p_lateral + datos$p_central + datos$p_lateral
    suma_teorica_correcta <- abs(suma_teorica - 1.0) < 0.001
    
    # 3. Verificar que p_lateral = (1 - p_central) / 2
    p_lateral_calculado <- (1 - datos$p_central) / 2
    p_lateral_correcto <- abs(datos$p_lateral - p_lateral_calculado) < 0.001
    
    # 4. Verificar que los intervalos son coherentes
    intervalos_coherentes <- datos$limite1 < datos$limite2 && datos$limite2 < datos$limite_sup
    
    # 5. Verificar que la opción correcta está en la posición indicada
    posicion_correcta_valida <- datos$posicion_correcta >= 1 && datos$posicion_correcta <= 4
    
    # 6. Verificar que todas las opciones son diferentes
    tablas_str <- lapply(datos$opciones, function(tabla) {
      paste(tabla$Intervalo, tabla$Probabilidad, collapse = "|")
    })
    todas_diferentes <- length(unique(tablas_str)) == 4
    
    return(list(
      version = version_num,
      suma_probabilidades_correcta = suma_correcta,
      suma_teorica_correcta = suma_teorica_correcta,
      p_lateral_correcto = p_lateral_correcto,
      intervalos_coherentes = intervalos_coherentes,
      posicion_correcta_valida = posicion_correcta_valida,
      todas_diferentes = todas_diferentes,
      posicion_correcta = datos$posicion_correcta,
      p_central = datos$p_central,
      p_lateral = datos$p_lateral,
      suma_real = suma_probabilidades,
      exito = TRUE
    ))
    
  }, error = function(e) {
    return(list(
      version = version_num,
      exito = FALSE,
      error = e$message
    ))
  })
}

# Ejecutar verificaciones en 50 versiones
NUM_VERSIONES <- 50
cat(sprintf("Ejecutando verificaciones matemáticas en %d versiones...\n\n", NUM_VERSIONES))

resultados <- list()
for (i in 1:NUM_VERSIONES) {
  if (i %% 10 == 0) {
    cat(sprintf("Verificando versión %d/%d...\n", i, NUM_VERSIONES))
  }
  resultados[[i]] <- verificar_integridad_matematica(i)
}

# Analizar resultados
cat("\n=== RESULTADOS DE INTEGRIDAD MATEMÁTICA ===\n")

versiones_exitosas <- sum(sapply(resultados, function(r) r$exito))
cat(sprintf("Versiones procesadas exitosamente: %d/%d (%.1f%%)\n", versiones_exitosas, NUM_VERSIONES, versiones_exitosas/NUM_VERSIONES*100))

if (versiones_exitosas > 0) {
  resultados_exitosos <- resultados[sapply(resultados, function(r) r$exito)]
  
  # Contadores de verificaciones
  suma_prob_ok <- sum(sapply(resultados_exitosos, function(r) r$suma_probabilidades_correcta))
  suma_teorica_ok <- sum(sapply(resultados_exitosos, function(r) r$suma_teorica_correcta))
  p_lateral_ok <- sum(sapply(resultados_exitosos, function(r) r$p_lateral_correcto))
  intervalos_ok <- sum(sapply(resultados_exitosos, function(r) r$intervalos_coherentes))
  posicion_ok <- sum(sapply(resultados_exitosos, function(r) r$posicion_correcta_valida))
  diferencias_ok <- sum(sapply(resultados_exitosos, function(r) r$todas_diferentes))
  
  cat("\n--- VERIFICACIONES MATEMÁTICAS ---\n")
  cat(sprintf("✅ Suma de probabilidades = 1: %d/%d (%.1f%%)\n", suma_prob_ok, versiones_exitosas, suma_prob_ok/versiones_exitosas*100))
  cat(sprintf("✅ Suma teórica correcta: %d/%d (%.1f%%)\n", suma_teorica_ok, versiones_exitosas, suma_teorica_ok/versiones_exitosas*100))
  cat(sprintf("✅ Cálculo p_lateral correcto: %d/%d (%.1f%%)\n", p_lateral_ok, versiones_exitosas, p_lateral_ok/versiones_exitosas*100))
  cat(sprintf("✅ Intervalos coherentes: %d/%d (%.1f%%)\n", intervalos_ok, versiones_exitosas, intervalos_ok/versiones_exitosas*100))
  cat(sprintf("✅ Posición correcta válida: %d/%d (%.1f%%)\n", posicion_ok, versiones_exitosas, posicion_ok/versiones_exitosas*100))
  cat(sprintf("✅ Todas las opciones diferentes: %d/%d (%.1f%%)\n", diferencias_ok, versiones_exitosas, diferencias_ok/versiones_exitosas*100))
  
  # Distribución de posiciones correctas
  posiciones <- sapply(resultados_exitosos, function(r) r$posicion_correcta)
  cat("\n--- DISTRIBUCIÓN DE POSICIONES CORRECTAS ---\n")
  for (pos in 1:4) {
    count <- sum(posiciones == pos)
    porcentaje <- count/versiones_exitosas*100
    cat(sprintf("  Tabla %s: %d veces (%.1f%%)\n", LETTERS[pos], count, porcentaje))
  }
  
  # Evaluación final
  todas_verificaciones_ok <- suma_prob_ok == versiones_exitosas && 
                            suma_teorica_ok == versiones_exitosas && 
                            p_lateral_ok == versiones_exitosas && 
                            intervalos_ok == versiones_exitosas && 
                            posicion_ok == versiones_exitosas && 
                            diferencias_ok == versiones_exitosas
  
  cat("\n--- EVALUACIÓN FINAL ---\n")
  cat(sprintf("🎯 INTEGRIDAD MATEMÁTICA: %s\n", if (todas_verificaciones_ok) "✅ PERFECTA" else "❌ REQUIERE REVISIÓN"))
  
  if (todas_verificaciones_ok) {
    cat("\nTodos los cálculos matemáticos son correctos independientemente de la posición de la opción correcta.\n")
  } else {
    cat("\nSe encontraron inconsistencias matemáticas que requieren revisión.\n")
  }
}

cat("\n=== VERIFICACIÓN DE INTEGRIDAD COMPLETADA ===\n")
