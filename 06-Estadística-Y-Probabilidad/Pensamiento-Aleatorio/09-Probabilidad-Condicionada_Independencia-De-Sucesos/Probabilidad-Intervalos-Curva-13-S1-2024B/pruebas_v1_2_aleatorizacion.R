#!/usr/bin/env Rscript

# Script de Pruebas Estadísticas para el archivo _v1_2.Rmd
# Objetivo: Verificar que el algoritmo de aleatorización equilibrada funciona correctamente
# en el archivo hermano probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_cloze_v1_2.Rmd

library(exams)

cat("=== PRUEBAS ESTADÍSTICAS ARCHIVO _v1_2.Rmd ===\n\n")

# Configuración de pruebas
NUM_VERSIONES <- 50  # Muestra para análisis estadístico
TOLERANCIA_PORCENTAJE <- 5  # ±5% de tolerancia respecto al 25% ideal

# Función para extraer información de aleatorización del archivo _v1_2
analizar_aleatorizacion_v1_2 <- function(version_num) {
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
    
    # Función principal de generación de datos (adaptada del archivo _v1_2)
    generar_datos <- function() {
      # 1. Aleatorización de Parámetros Matemáticos (Incremento de dificultad)
      p_central <- sample(seq(0.35, 0.65, by = 0.01), 1)
      p_lateral <- (1 - p_central) / 2

      limite1 <- sample(2:8, 1)
      ancho_central <- sample(3:8, 1)
      limite2 <- limite1 + ancho_central
      limite_sup <- sample(15:18, 1)

      if (limite2 >= limite_sup) {
        limite2 <- limite_sup - 2
      }

      # 3. Creación de Intervalos (Formato adaptativo)
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

      # 4. SISTEMA DE ALEATORIZACIÓN DE OPCIONES - ALGORITMO MEJORADO EQUILIBRADO
      # Crear tabla alternativa con encabezados intercambiados para mayor diversidad
      tabla_correcta_alt <- data.frame(
        Probabilidad = c(p_lateral, p_central, p_lateral),
        Intervalo = c(intervalo1_txt, intervalo2_txt, intervalo3_txt),
        stringsAsFactors = FALSE
      )

      # Aleatorizar qué tabla correcta usar (normal o con encabezados intercambiados)
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
        A = tabla_distractor_A,      # Probabilidades acumuladas incorrectas
        B = tabla_distractor_B,      # Intervalos acumulativos incorrectos
        C = tabla_correcta_final,    # Tabla correcta (puede tener encabezados intercambiados)
        D = tabla_distractor_D       # Probabilidades intercambiadas
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
    
    return(list(
      version = version_num,
      posicion_correcta = datos$posicion_correcta,
      usar_alt = datos$usar_encabezados_alt,
      num_opciones = length(datos$opciones),
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

# Ejecutar análisis estadístico
cat(sprintf("Ejecutando análisis estadístico en %d versiones del archivo _v1_2.Rmd...\n\n", NUM_VERSIONES))

# Inicializar contadores
contadores <- c(A = 0, B = 0, C = 0, D = 0)
versiones_exitosas <- 0
errores <- c()

# Generar versiones y recopilar estadísticas
for (i in 1:NUM_VERSIONES) {
  if (i %% 10 == 0) {
    cat(sprintf("Procesando versión %d/%d...\n", i, NUM_VERSIONES))
  }
  
  resultado <- analizar_aleatorizacion_v1_2(i)
  
  if (resultado$exito) {
    versiones_exitosas <- versiones_exitosas + 1
    posicion <- resultado$posicion_correcta
    letra <- LETTERS[posicion]
    contadores[letra] <- contadores[letra] + 1
  } else {
    errores <- c(errores, resultado$error)
  }
}

cat("\n=== RESULTADOS ESTADÍSTICOS ARCHIVO _v1_2.Rmd ===\n")

# Estadísticas básicas
cat(sprintf("Versiones exitosas: %d/%d (%.1f%%)\n", versiones_exitosas, NUM_VERSIONES, versiones_exitosas/NUM_VERSIONES*100))

if (length(errores) > 0) {
  cat(sprintf("Errores encontrados: %d\n", length(errores)))
  cat("Tipos de errores:\n")
  for (error in unique(errores)) {
    cat(sprintf("  - %s\n", error))
  }
}

if (versiones_exitosas > 0) {
  cat("\n--- DISTRIBUCIÓN DE POSICIONES CORRECTAS ---\n")
  
  # Calcular porcentajes
  porcentajes <- (contadores / versiones_exitosas) * 100
  
  # Mostrar distribución
  for (i in 1:4) {
    letra <- LETTERS[i]
    count <- contadores[letra]
    porcentaje <- porcentajes[letra]
    
    # Determinar si está dentro de la tolerancia
    dentro_tolerancia <- abs(porcentaje - 25) <= TOLERANCIA_PORCENTAJE
    estado <- if (dentro_tolerancia) "✅" else "❌"
    
    cat(sprintf("  Tabla %s: %d veces (%.1f%%) %s\n", letra, count, porcentaje, estado))
  }
  
  # Análisis estadístico avanzado
  cat("\n--- ANÁLISIS ESTADÍSTICO AVANZADO ---\n")
  
  # Prueba Chi-cuadrado para uniformidad
  esperado <- rep(versiones_exitosas/4, 4)
  observado <- as.numeric(contadores)
  
  chi_cuadrado <- sum((observado - esperado)^2 / esperado)
  grados_libertad <- 3
  p_valor <- 1 - pchisq(chi_cuadrado, grados_libertad)
  
  cat(sprintf("Prueba Chi-cuadrado:\n"))
  cat(sprintf("  Estadístico χ²: %.4f\n", chi_cuadrado))
  cat(sprintf("  Grados de libertad: %d\n", grados_libertad))
  cat(sprintf("  Valor p: %.4f\n", p_valor))
  cat(sprintf("  Interpretación: %s\n", if (p_valor > 0.05) "✅ Distribución uniforme (p > 0.05)" else "❌ Distribución no uniforme (p ≤ 0.05)"))
  
  # Evaluación final
  cat("\n--- EVALUACIÓN FINAL ---\n")
  
  todas_dentro_tolerancia <- all(abs(porcentajes - 25) <= TOLERANCIA_PORCENTAJE)
  distribucion_uniforme <- p_valor > 0.05
  
  cat(sprintf("Criterio de tolerancia (25%% ± %d%%): %s\n", TOLERANCIA_PORCENTAJE, if (todas_dentro_tolerancia) "✅ CUMPLIDO" else "❌ NO CUMPLIDO"))
  cat(sprintf("Prueba de uniformidad estadística: %s\n", if (distribucion_uniforme) "✅ CUMPLIDA" else "❌ NO CUMPLIDA"))
  
  resultado_final <- todas_dentro_tolerancia && distribucion_uniforme
  cat(sprintf("\n🎯 RESULTADO FINAL ARCHIVO _v1_2.Rmd: %s\n", if (resultado_final) "✅ ALEATORIZACIÓN EQUILIBRADA EXITOSA" else "❌ ALEATORIZACIÓN REQUIERE AJUSTES"))
  
  if (resultado_final) {
    cat("\nEl archivo _v1_2.Rmd cumple todos los criterios estadísticos para distribución equilibrada.\n")
  } else {
    cat("\nEl archivo _v1_2.Rmd requiere ajustes para lograr distribución perfectamente equilibrada.\n")
  }
}

cat("\n=== ANÁLISIS ARCHIVO _v1_2.Rmd COMPLETADO ===\n")
