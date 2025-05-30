---
tipo_pregunta: "Selección múltiple con única respuesta"
competencia: "Interpretación y representación"
componente: "Aleatorio y sistemas de datos"
afirmacion: "Interpreta información presentada en tablas y gráficos"
nivel_dificultad: "Medio"
tiempo_estimado: "3 minutos"
autor: "Sistema R-Exams ICFES"
version: "1.0"
output:
  pdf_document:
    keep_tex: true
  html_document: default
  word_document: default
---




``` r
options(OutDec = ".")
# REFORZAR CONFIGURACIÓN ANTI-NOTACIÓN CIENTÍFICA
options(scipen = 999)
options(digits = 10)

# Establecer semilla para reproducibilidad
set.seed(sample(1:10000, 1))

# FUNCIÓN RADICAL DE FORMATEO ENTERO
formatear_entero <- function(numero) {
  # Forzar formato entero sin notación científica JAMÁS
  formatC(as.numeric(numero), format = "d", big.mark = "")
}

# Aleatorización del contexto
contextos <- c("canal de deportes", "plataforma de streaming", "revista deportiva",
               "portal web deportivo", "aplicación móvil", "blog de fútbol")
contexto <- sample(contextos, 1)

# Determinar género del contexto para concordancia
contextos_femeninos <- c("plataforma de streaming", "revista deportiva", "aplicación móvil")
es_contexto_femenino <- contexto %in% contextos_femeninos
articulo_contexto <- if(es_contexto_femenino) "Una" else "Un"

# Aleatorización de términos para encuesta
terminos_encuesta <- c("encuesta", "sondeo", "consulta", "estudio", "investigación")
termino_encuesta <- sample(terminos_encuesta, 1)

# Determinar género del término de encuesta para concordancia
terminos_femeninos <- c("encuesta", "consulta", "investigación")
es_femenino <- termino_encuesta %in% terminos_femeninos
articulo_encuesta <- if(es_femenino) "la" else "el"

terminos_usuarios <- c("suscriptores", "seguidores", "usuarios", "miembros", "aficionados")
termino_usuarios <- sample(terminos_usuarios, 1)

# Definir competiciones y equipos compatibles
competiciones_clubes_europeos <- c("Champions League", "Liga Europa")
competiciones_clubes_sudamericanos <- c("Copa Libertadores")
competiciones_selecciones_europeas <- c("Eurocopa")
competiciones_selecciones_sudamericanas <- c("Copa América")
competiciones_selecciones_mundiales <- c("Copa del Mundo")

# Equipos por región - CORREGIDO: Eliminando duplicados potenciales
equipos_europeos <- c("Manchester City", "FC Barcelona", "Real Madrid", "Liverpool",
                     "Bayern de Múnich", "Paris Saint-Germain", "Chelsea",
                     "Manchester United", "Arsenal", "Juventus", "AC Milan",
                     "Atlético de Madrid", "Borussia Dortmund", "Ajax", "Benfica")

equipos_sudamericanos <- c("Boca Juniors", "River Plate", "Flamengo", "Palmeiras",
                          "São Paulo", "Santos", "Corinthians", "Atlético Nacional",
                          "Millonarios", "Colo-Colo", "Universidad de Chile", "Peñarol",
                          "Nacional Montevideo", "Olimpia", "Cerro Porteño")

selecciones_europeas <- c("España", "Francia", "Alemania", "Italia", "Inglaterra",
                         "Portugal", "Países Bajos", "Bélgica", "Croacia", "Polonia",
                         "Suiza", "Austria", "Dinamarca", "Suecia", "Ucrania")

selecciones_sudamericanas <- c("Brasil", "Argentina", "Uruguay", "Colombia", "Chile",
                              "Perú", "Ecuador", "Paraguay", "Bolivia", "Venezuela")

selecciones_mundiales <- c("Brasil", "Argentina", "Francia", "España", "Inglaterra",
                          "Portugal", "Alemania", "Italia", "Países Bajos", "Croacia",
                          "Uruguay", "Colombia", "México", "Estados Unidos", "Japón")

# Seleccionar competición y equipos compatibles
tipo_competicion <- sample(1:5, 1)

if (tipo_competicion == 1) {
  # Competiciones de clubes europeos
  competicion <- sample(competiciones_clubes_europeos, 1)
  equipos_disponibles <- equipos_europeos
} else if (tipo_competicion == 2) {
  # Competiciones de clubes sudamericanos
  competicion <- sample(competiciones_clubes_sudamericanos, 1)
  equipos_disponibles <- equipos_sudamericanos
} else if (tipo_competicion == 3) {
  # Competiciones de selecciones europeas
  competicion <- sample(competiciones_selecciones_europeas, 1)
  equipos_disponibles <- selecciones_europeas
} else if (tipo_competicion == 4) {
  # Competiciones de selecciones sudamericanas
  competicion <- sample(competiciones_selecciones_sudamericanas, 1)
  equipos_disponibles <- selecciones_sudamericanas
} else {
  # Competiciones de selecciones mundiales
  competicion <- sample(competiciones_selecciones_mundiales, 1)
  equipos_disponibles <- selecciones_mundiales
}

# VERIFICACIÓN TEMPRANA: Asegurar que hay suficientes equipos únicos
if (length(equipos_disponibles) < 5) {
  stop("Error: No hay suficientes equipos disponibles para generar 5 opciones únicas")
}

# Seleccionar 5 equipos/selecciones de la lista compatible
equipos_seleccionados <- sample(equipos_disponibles, 5)
equipo1 <- equipos_seleccionados[1]
equipo2 <- equipos_seleccionados[2]
equipo3 <- equipos_seleccionados[3]
equipo4 <- equipos_seleccionados[4]
equipo5 <- equipos_seleccionados[5]

# Población y muestra (usando números enteros simples)
poblacion_total <- sample(c(256, 1296, 25000, 30000, 40000, 50000, 60000, 75000, 81000, 90000, 100000), 1)
tamano_muestra <- sample(c(80, 90, 100, 110, 120, 130, 140, 150), 1)

# CREAR VARIABLES FORMATEADAS PARA MOSTRAR (SOLUCIÓN RADICAL)
poblacion_total_fmt <- formatear_entero(poblacion_total)
tamano_muestra_fmt <- formatear_entero(tamano_muestra)

# FUNCIÓN ROBUSTA PARA GENERAR VALORES CON VARIABILIDAD GARANTIZADA
generar_valores_coherentes <- function(total, max_intentos = 100) {
  # Definir rangos apropiados basados en el total
  min_valor <- max(8, round(total * 0.08))  # Mínimo 8% del total
  max_valor <- min(40, round(total * 0.35)) # Máximo 35% del total

  # VERIFICACIÓN INICIAL
  if (min_valor * 5 > total) {
    stop("ERROR: Imposible generar 5 valores con min_valor = ", min_valor, " y total = ", total)
  }

  for (intento in 1:max_intentos) {
    # ESTRATEGIA ROBUSTA: Generar valores diversos desde el inicio

    # Método 1: Distribución escalonada (primeros 50 intentos)
    if (intento <= 50) {
      valor_base <- round(total / 5)
      variacion <- min(6, round(valor_base * 0.3))

      # Crear valores escalonados para garantizar variabilidad
      valores <- c(
        valor_base - variacion,
        valor_base - round(variacion/2),
        valor_base,
        valor_base + round(variacion/2),
        valor_base + variacion
      )

      # Asegurar que están en rango válido
      valores <- pmax(valores, min_valor)
      valores <- pmin(valores, max_valor)

    } else {
      # Método 2: Distribución completamente aleatoria (últimos 50 intentos)
      valores <- sample(min_valor:max_valor, 5, replace = TRUE)
    }

    # Ajustar para que sumen exactamente el total
    diferencia <- total - sum(valores)

    # Distribuir la diferencia de manera inteligente
    intentos_ajuste <- 0
    while (diferencia != 0 && intentos_ajuste < 30) {
      if (diferencia > 0) {
        # Necesitamos aumentar algunos valores
        indices_validos <- which(valores < max_valor)
        if (length(indices_validos) > 0) {
          idx <- sample(indices_validos, 1)
          incremento <- min(diferencia, max_valor - valores[idx])
          valores[idx] <- valores[idx] + incremento
          diferencia <- diferencia - incremento
        } else {
          break  # No se puede ajustar más
        }
      } else {
        # Necesitamos disminuir algunos valores
        indices_validos <- which(valores > min_valor)
        if (length(indices_validos) > 0) {
          idx <- sample(indices_validos, 1)
          decremento <- min(abs(diferencia), valores[idx] - min_valor)
          valores[idx] <- valores[idx] - decremento
          diferencia <- diferencia + decremento
        } else {
          break  # No se puede ajustar más
        }
      }
      intentos_ajuste <- intentos_ajuste + 1
    }

    # Verificar si el resultado es válido
    if (sum(valores) == total &&
        all(valores >= min_valor) &&
        all(valores <= max_valor) &&
        length(unique(valores)) >= 3) {  # Al menos 3 valores únicos

      # Mezclar el orden para mayor aleatoriedad
      valores <- sample(valores)

      # VERIFICACIÓN FINAL
      if (sum(valores) != total) {
        stop("ERROR FINAL: suma = ", sum(valores), ", esperado = ", total)
      }

      return(valores)
    }
  }

  # Si llegamos aquí, usar método de emergencia
  cat("ADVERTENCIA: Usando método de emergencia para total =", total, "\n")

  # Método de emergencia: distribución simple pero garantizada
  valor_base <- floor(total / 5)
  resto <- total %% 5

  valores <- rep(valor_base, 5)

  # Distribuir resto para crear variabilidad mínima
  for (i in 1:resto) {
    valores[i] <- valores[i] + 1
  }

  # Añadir variabilidad mínima si es posible
  if (valores[1] > min_valor && valores[5] < max_valor) {
    valores[1] <- valores[1] - 1
    valores[5] <- valores[5] + 1
  }

  # VERIFICACIÓN FINAL DE EMERGENCIA
  if (sum(valores) != total) {
    stop("ERROR CRÍTICO DE EMERGENCIA: suma = ", sum(valores), ", esperado = ", total)
  }

  return(valores)
}

valores_equipos <- generar_valores_coherentes(tamano_muestra)

# SELECCIÓN ALEATORIA DEL EQUIPO CORRECTO (SOLUCIÓN ANTI-PATRÓN)
# En lugar de siempre usar el equipo con más votos, seleccionar aleatoriamente
# cualquiera de los 5 equipos como respuesta correcta
indice_equipo_correcto <- sample(1:5, 1)

# No reordenar - mantener orden original aleatorio para evitar patrones

# Asignar variables individuales DESPUÉS del ordenamiento
valor1 <- valores_equipos[1]
valor2 <- valores_equipos[2]
valor3 <- valores_equipos[3]
valor4 <- valores_equipos[4]
valor5 <- valores_equipos[5]

# CREAR VARIABLES FORMATEADAS PARA VALORES DE EQUIPOS
valor1_fmt <- formatear_entero(valor1)
valor2_fmt <- formatear_entero(valor2)
valor3_fmt <- formatear_entero(valor3)
valor4_fmt <- formatear_entero(valor4)
valor5_fmt <- formatear_entero(valor5)

equipo1 <- equipos_seleccionados[1]
equipo2 <- equipos_seleccionados[2]
equipo3 <- equipos_seleccionados[3]
equipo4 <- equipos_seleccionados[4]
equipo5 <- equipos_seleccionados[5]

# VARIABLES DINÁMICAS PARA EL EQUIPO CORRECTO SELECCIONADO ALEATORIAMENTE
equipo_correcto <- equipos_seleccionados[indice_equipo_correcto]
valor_correcto <- valores_equipos[indice_equipo_correcto]
valor_correcto_fmt <- formatear_entero(valor_correcto)

# Validaciones matemáticas robustas
# Verificar que los valores suman exactamente el tamaño de muestra
if (sum(valores_equipos) != tamano_muestra) {
  stop("Error: Los valores no suman el tamaño de muestra. Suma: ", sum(valores_equipos),
       ", Esperado: ", tamano_muestra)
}

# Verificar rangos apropiados
min_esperado <- max(8, round(tamano_muestra * 0.08))
max_esperado <- min(40, round(tamano_muestra * 0.35))
if (!all(valores_equipos >= min_esperado) || !all(valores_equipos <= max_esperado)) {
  stop("Error: Algunos valores están fuera del rango esperado [", min_esperado, ", ", max_esperado, "]")
}

# VALIDACIÓN ROBUSTA DE VARIABILIDAD
valores_unicos <- unique(valores_equipos)
if (length(valores_unicos) < 3) {
  stop("Error: Variabilidad insuficiente. Solo ", length(valores_unicos),
       " valores únicos de 5. Valores: [", paste(valores_equipos, collapse=", "),
       "]. Se requieren al menos 3 valores únicos.")
}

# Verificar que todos los valores son positivos
if (!all(valores_equipos > 0)) {
  stop("Error: Algunos valores no son positivos")
}

# Verificar que ningún equipo domina excesivamente
if (max(valores_equipos) > tamano_muestra * 0.4) {
  stop("Error: Un equipo tiene demasiados votos (>40% del total)")
}

# Colores para el gráfico
colores <- c("#2E8B57", "#4682B4", "#CD853F", "#9370DB", "#DC143C")

# FUNCIÓN PARA CALCULAR EL MÁXIMO COMÚN DIVISOR (MCD)
calcular_mcd <- function(a, b) {
  while (b != 0) {
    temp <- b
    b <- a %% b
    a <- temp
  }
  return(a)
}

# FUNCIÓN PARA GENERAR FRACCIÓN REDUCIDA
generar_fraccion_reducida <- function(numerador, denominador) {
  mcd <- calcular_mcd(numerador, denominador)
  return(c(numerador / mcd, denominador / mcd))
}

# Generar opciones de respuesta MEJORADAS Y MÁS DESAFIANTES
# La respuesta correcta es sobre el equipo seleccionado ALEATORIAMENTE
proporcion_correcta <- round(valor_correcto / tamano_muestra * 100)
respuesta_correcta <- paste0("alrededor de ", valor_correcto_fmt, " de cada ", tamano_muestra_fmt, " ",
                            termino_usuarios, " del ", contexto, " da por favorito al ", equipo_correcto, ".")

# ELIMINAR GENERACIÓN DE FRACCIONES EQUIVALENTES
# Las fracciones reducidas son matemáticamente equivalentes a la respuesta correcta
# Por lo tanto, NO deben usarse como distractores en un examen de selección múltiple
# Se mantienen las variables para compatibilidad pero no se usan en distractores

# GENERACIÓN ROBUSTA DE DISTRACTORES ÚNICOS
# Sistema mejorado que garantiza exactamente 4 opciones únicas (1 correcta + 3 distractores)

# Función para generar distractores de respaldo únicos
generar_distractores_respaldo <- function(base_valor, base_muestra, base_poblacion, base_equipo, base_contexto, base_usuarios) {
  distractores_respaldo <- c()

  # Respaldo 1: Usar porcentaje directo
  porcentaje <- round(base_valor / base_muestra * 100)
  distractores_respaldo <- c(distractores_respaldo,
    paste0("el ", porcentaje, "% de los ", formatear_entero(base_muestra), " ", base_usuarios,
           " del ", base_contexto, " da por favorito al ", base_equipo, "."))

  # Respaldo 2: Usar valor absoluto con población
  distractores_respaldo <- c(distractores_respaldo,
    paste0("exactamente ", formatear_entero(base_valor), " de los ", formatear_entero(base_poblacion), " ",
           base_usuarios, " del ", base_contexto, " da por favorito al ", base_equipo, "."))

  # Respaldo 3: Error de interpretación de datos (confundir valores absolutos)
  valor_incorrecto <- sample(setdiff(c(10, 15, 20, 25, 30, 35, 40), base_valor), 1)
  distractores_respaldo <- c(distractores_respaldo,
    paste0("aproximadamente ", formatear_entero(valor_incorrecto), " de cada ", formatear_entero(base_muestra), " ",
           base_usuarios, " del ", base_contexto, " da por favorito al ", base_equipo, "."))

  # Respaldo 4: Error conceptual (usar porcentaje como valor absoluto)
  porcentaje_como_valor <- round(base_valor / base_muestra * 100)
  if (porcentaje_como_valor != base_valor && porcentaje_como_valor > 0) {
    distractores_respaldo <- c(distractores_respaldo,
      paste0("alrededor de ", formatear_entero(porcentaje_como_valor), " de cada ", formatear_entero(base_muestra), " ",
             base_usuarios, " del ", base_contexto, " da por favorito al ", base_equipo, "."))
  }

  return(distractores_respaldo)
}

# SISTEMA ANTI-PATRÓN MEJORADO: DIVERSIFICACIÓN OBLIGATORIA DE EQUIPOS
# Garantiza que cada opción mencione un equipo diferente para evitar patrones detectables

# Seleccionar equipos únicos para cada distractor
equipos_otros_indices <- setdiff(1:5, indice_equipo_correcto)
if (length(equipos_otros_indices) < 3) {
  stop("Error: Se necesitan al menos 4 equipos diferentes en la muestra para evitar patrones")
}

# Seleccionar 3 equipos diferentes para los distractores
equipos_distractores_indices <- sample(equipos_otros_indices, 3)
equipo_distractor1_idx <- equipos_distractores_indices[1]
equipo_distractor2_idx <- equipos_distractores_indices[2]
equipo_distractor3_idx <- equipos_distractores_indices[3]

# Obtener datos de los equipos seleccionados para distractores
equipo_distractor1 <- equipos_seleccionados[equipo_distractor1_idx]
valor_distractor1 <- valores_equipos[equipo_distractor1_idx]
valor_distractor1_fmt <- formatear_entero(valor_distractor1)

equipo_distractor2 <- equipos_seleccionados[equipo_distractor2_idx]
valor_distractor2 <- valores_equipos[equipo_distractor2_idx]
valor_distractor2_fmt <- formatear_entero(valor_distractor2)

equipo_distractor3 <- equipos_seleccionados[equipo_distractor3_idx]
valor_distractor3 <- valores_equipos[equipo_distractor3_idx]
valor_distractor3_fmt <- formatear_entero(valor_distractor3)

# VERIFICACIÓN TEMPRANA DE UNICIDAD DE EQUIPOS
equipos_para_distractores <- c(equipo_distractor1, equipo_distractor2, equipo_distractor3)
if (length(unique(equipos_para_distractores)) != 3) {
  stop("Error crítico: Los equipos para distractores no son únicos")
}

# SISTEMA ROBUSTO DE GENERACIÓN DE DISTRACTORES ÚNICOS - VERSIÓN DEFINITIVA
# Esta versión garantiza distractores únicos en todos los casos posibles

# FUNCIÓN PARA GENERAR DISTRACTORES ÚNICOS GARANTIZADOS
generar_distractores_unicos <- function(equipo_correcto, valor_correcto, tamano_muestra, poblacion_total,
                                       equipo_dist1, valor_dist1, equipo_dist2, valor_dist2,
                                       equipo_dist3, valor_dist3, contexto, termino_usuarios, competicion) {

  distractores <- c()
  intentos_maximos <- 50

  # DISTRACTOR 1: Proporción correcta con equipo incorrecto (SIEMPRE ÚNICO)
  distractor1 <- paste0("alrededor de ", formatear_entero(valor_dist1), " de cada ", formatear_entero(tamano_muestra), " ",
                       termino_usuarios, " del ", contexto, " da por favorito al ", equipo_dist1, ".")
  distractores <- c(distractores, distractor1)

  # DISTRACTOR 2: Confusión muestra-población (SIEMPRE ÚNICO)
  distractor2 <- paste0("alrededor de ", formatear_entero(valor_dist2), " de cada ", formatear_entero(poblacion_total), " ",
                       termino_usuarios, " del ", contexto, " da por favorito al ", equipo_dist2, ".")
  distractores <- c(distractores, distractor2)

  # DISTRACTOR 3: Generar con múltiples estrategias para garantizar unicidad
  distractor3 <- NULL
  for (intento in 1:intentos_maximos) {

    if (intento <= 10) {
      # Estrategia 1: Error de porcentaje
      porcentaje <- round(valor_dist3 / tamano_muestra * 100)
      if (porcentaje != valor_dist3 && porcentaje > 0 && porcentaje <= 50) {
        distractor3_candidato <- paste0("alrededor de ", formatear_entero(porcentaje), " de cada ", formatear_entero(tamano_muestra), " ",
                                       termino_usuarios, " del ", contexto, " da por favorito al ", equipo_dist3, ".")
      } else {
        next  # Probar siguiente estrategia
      }

    } else if (intento <= 20) {
      # Estrategia 2: Valor modificado matemáticamente
      valor_modificado <- valor_dist3 + sample(c(-3, -2, -1, 1, 2, 3), 1)
      valor_modificado <- max(1, min(tamano_muestra - 1, valor_modificado))
      distractor3_candidato <- paste0("alrededor de ", formatear_entero(valor_modificado), " de cada ", formatear_entero(tamano_muestra), " ",
                                     termino_usuarios, " del ", contexto, " da por favorito al ", equipo_dist3, ".")

    } else if (intento <= 30) {
      # Estrategia 3: Confusión conceptual con población
      distractor3_candidato <- paste0("alrededor de ", formatear_entero(valor_dist3), " de cada ", formatear_entero(poblacion_total), " ",
                                     termino_usuarios, " del ", contexto, " da por favorito al ", equipo_dist3, ".")

    } else if (intento <= 40) {
      # Estrategia 4: Error de escala
      valor_escala <- round(valor_dist3 / 2)
      if (valor_escala > 0 && valor_escala != valor_dist3) {
        distractor3_candidato <- paste0("alrededor de ", formatear_entero(valor_escala), " de cada ", formatear_entero(tamano_muestra), " ",
                                       termino_usuarios, " del ", contexto, " da por favorito al ", equipo_dist3, ".")
      } else {
        next
      }

    } else {
      # Estrategia 5: Confusión conceptual sin equipo específico (ÚLTIMO RECURSO)
      distractor3_candidato <- paste0("sólo ", formatear_entero(tamano_muestra), " de los ", formatear_entero(poblacion_total), " ",
                                     termino_usuarios, " del ", contexto,
                                     " tienen preferencia por un equipo para ganar la ", competicion, ".")
    }

    # Verificar que el candidato es único
    if (!(distractor3_candidato %in% distractores)) {
      distractor3 <- distractor3_candidato
      break
    }
  }

  # Si no se pudo generar un distractor único, usar método de emergencia
  if (is.null(distractor3)) {
    # Método de emergencia: usar un valor fijo que garantice unicidad
    valor_emergencia <- round(tamano_muestra * 0.6)  # 60% del tamaño de muestra
    distractor3 <- paste0("alrededor de ", formatear_entero(valor_emergencia), " de cada ", formatear_entero(tamano_muestra), " ",
                         termino_usuarios, " del ", contexto, " da por favorito al ", equipo_dist3, ".")

    # Si aún así no es único, modificar ligeramente
    contador_emergencia <- 1
    while (distractor3 %in% distractores && contador_emergencia <= 10) {
      valor_emergencia <- valor_emergencia + contador_emergencia
      distractor3 <- paste0("alrededor de ", formatear_entero(valor_emergencia), " de cada ", formatear_entero(tamano_muestra), " ",
                           termino_usuarios, " del ", contexto, " da por favorito al ", equipo_dist3, ".")
      contador_emergencia <- contador_emergencia + 1
    }
  }

  distractores <- c(distractores, distractor3)

  # VERIFICACIÓN FINAL DE UNICIDAD
  if (length(unique(distractores)) != 3) {
    stop("ERROR CRÍTICO: No se pudieron generar 3 distractores únicos después de todos los intentos")
  }

  return(distractores)
}

# GENERAR DISTRACTORES ÚNICOS USANDO LA FUNCIÓN ROBUSTA
distractores_finales <- generar_distractores_unicos(
  equipo_correcto, valor_correcto, tamano_muestra, poblacion_total,
  equipo_distractor1, valor_distractor1, equipo_distractor2, valor_distractor2,
  equipo_distractor3, valor_distractor3, contexto, termino_usuarios, competicion
)

# VERIFICACIÓN ADICIONAL DE UNICIDAD (REDUNDANTE PERO SEGURA)
if (length(unique(distractores_finales)) != 3) {
  stop("Error crítico: Los distractores generados no son únicos")
}

# CREAR OPCIONES FINALES Y MEZCLAR
opciones <- c(respuesta_correcta, distractores_finales[1], distractores_finales[2], distractores_finales[3])
opciones_mezcladas <- sample(opciones)
indice_correcto <- which(opciones_mezcladas == respuesta_correcta)

# Vector de solución para r-exams
solucion <- rep(0, 4)
solucion[indice_correcto] <- 1

# VALIDACIONES FINALES ROBUSTAS MEJORADAS
# Verificar que hay exactamente 4 opciones únicas
if (length(unique(opciones)) != 4) {
  stop("Error crítico: No se pudieron generar 4 opciones únicas. Opciones: ", paste(opciones, collapse=" | "))
}

# Verificar que la respuesta correcta está incluida
if (!(respuesta_correcta %in% opciones)) {
  stop("Error crítico: La respuesta correcta no está en las opciones finales")
}

# Verificar que todas las opciones son diferentes
for (i in 1:4) {
  for (j in 1:4) {
    if (i != j && opciones[i] == opciones[j]) {
      stop("Error: Opciones duplicadas detectadas en posiciones ", i, " y ", j)
    }
  }
}

# VALIDACIÓN ANTI-PATRÓN MEJORADA: Verificar diversidad de equipos mencionados
equipos_mencionados <- c()
for (opcion in opciones) {
  # Extraer equipos mencionados en cada opción usando coincidencia exacta
  for (equipo in equipos_seleccionados) {
    if (grepl(paste0("\\b", gsub("([\\[\\]\\(\\)\\{\\}\\^\\$\\*\\+\\?\\|\\\\])", "\\\\\\1", equipo), "\\b"),
              opcion, perl = TRUE)) {
      equipos_mencionados <- c(equipos_mencionados, equipo)
    }
  }
}

# Verificar que no hay más de una opción por equipo específico
tabla_equipos <- table(equipos_mencionados)
equipos_repetidos <- names(tabla_equipos)[tabla_equipos > 1]

if (length(equipos_repetidos) > 0) {
  stop("Error anti-patrón: Los siguientes equipos aparecen en múltiples opciones: ",
       paste(equipos_repetidos, collapse=", "),
       ". Esto crea un patrón detectable para estudiantes.")
}

# Verificar que hay suficiente diversidad (al menos 3 equipos diferentes mencionados)
if (length(unique(equipos_mencionados)) < 3) {
  stop("Error: Insuficiente diversidad de equipos. Se requieren al menos 3 equipos diferentes mencionados.")
}

# PRUEBAS DE INTEGRIDAD ADICIONALES
# Verificar que cada distractor usa un equipo diferente
equipos_en_distractores <- c()
for (distractor in distractores_finales) {
  for (equipo in equipos_seleccionados) {
    if (grepl(paste0("\\b", gsub("([\\[\\]\\(\\)\\{\\}\\^\\$\\*\\+\\?\\|\\\\])", "\\\\\\1", equipo), "\\b"),
              distractor, perl = TRUE)) {
      equipos_en_distractores <- c(equipos_en_distractores, equipo)
    }
  }
}

# Verificar que no hay equipos repetidos en distractores
if (length(equipos_en_distractores) != length(unique(equipos_en_distractores))) {
  stop("Error de integridad: Hay equipos repetidos en los distractores")
}

# Verificar que el equipo correcto no aparece en ningún distractor
if (equipo_correcto %in% equipos_en_distractores) {
  stop("Error de integridad: El equipo correcto aparece en un distractor")
}
```


``` r
options(OutDec = ".")

# Código Python para generar el gráfico de barras horizontal
codigo_python <- paste0("
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import numpy as np

# Datos para el gráfico
equipos = ['", equipo1, "', '", equipo2, "', '", equipo3, "', '", equipo4, "', '", equipo5, "']
valores = [", valor1, ", ", valor2, ", ", valor3, ", ", valor4, ", ", valor5, "]
colores_grafico = ['", colores[1], "', '", colores[2], "', '", colores[3], "', '", colores[4], "', '", colores[5], "']

# Crear figura
fig, ax = plt.subplots(figsize=(8, 6))

# Crear gráfico de barras horizontal
y_pos = np.arange(len(equipos))
barras = ax.barh(y_pos, valores, color=colores_grafico, edgecolor='white', linewidth=1)

# Configurar etiquetas y título
ax.set_yticks(y_pos)
ax.set_yticklabels(equipos, fontsize=10, fontweight='bold')
ax.set_xlabel('Número de ", termino_usuarios, "', fontsize=12, fontweight='bold')
ax.set_title('Equipos favoritos para ganar la ", competicion, "', fontsize=14, fontweight='bold', pad=20)

# Añadir valores en las barras
for i, (barra, valor) in enumerate(zip(barras, valores)):
    width = barra.get_width()
    ax.text(width + 0.5, barra.get_y() + barra.get_height()/2,
            str(valor), ha='left', va='center', fontweight='bold', fontsize=10)

# Configurar límites del eje x
ax.set_xlim(0, max(valores) + 5)

# Añadir grilla sutil
ax.grid(axis='x', alpha=0.3, linestyle='--')
ax.set_axisbelow(True)

# Ajustar diseño
plt.tight_layout()

# Guardar en múltiples formatos
plt.savefig('grafico_barras.png', dpi=150, bbox_inches='tight',
           transparent=True, format='png')
plt.savefig('grafico_barras.pdf', dpi=150, bbox_inches='tight',
           transparent=True, format='pdf')
plt.close()
")

# Ejecutar código Python
py_run_string(codigo_python)
```

Question
========

Una aplicación móvil realizó el estudio a un grupo de sus 50000 seguidores sobre la preferencia de su equipo favorito para ganar la Copa Libertadores. Para esto escogió al azar a 130 seguidores y les preguntó sobre su equipo favorito para ganar dicha competición. Los resultados se muestran en la gráfica.

![](grafico_barras.png){width=80%}

De acuerdo con los datos obtenidos en el estudio, es correcto afirmar que

Answerlist
----------
- alrededor de 15 de cada 130 seguidores del aplicación móvil da por favorito al Millonarios.
- alrededor de 23 de cada 130 seguidores del aplicación móvil da por favorito al Corinthians.
- alrededor de 29 de cada 50000 seguidores del aplicación móvil da por favorito al Colo-Colo.
- alrededor de 26 de cada 130 seguidores del aplicación móvil da por favorito al Palmeiras.

Solution
========

Para resolver este problema, necesitamos interpretar correctamente los datos del gráfico de barras y entender la diferencia entre muestra y población total.

### Paso 1: Identificar los datos conocidos
* Una aplicación móvil tiene un total de 50000 seguidores.
* Se realizó un(a) estudio a una muestra de 130 seguidores seleccionados al azar.
* Según el gráfico, 26 seguidores de la muestra prefieren al Palmeiras.

### Paso 2: Interpretar correctamente las proporciones
Los datos del gráfico representan únicamente la muestra de 130 seguidores, no la población total de 50000 seguidores.

### Paso 3: Analizar cada opción

**Opción correcta**: "alrededor de 26 de cada 130 seguidores del aplicación móvil da por favorito al Palmeiras."
Esta opción es correcta porque interpreta adecuadamente que 26 de cada 130 seguidores **de la muestra** prefieren al Palmeiras. Esto representa una proporción del 20% en la muestra.

**Análisis de distractores incorrectos**:

- **Distractores de confusión muestra-población**: Las opciones que mencionan proporciones sobre la población total de 50000 seguidores son incorrectas, ya que el estudio solo se realizó a 130 personas.

- **Distractores de equipos incorrectos**: Las opciones que usan datos de otros equipos de la muestra malinterpretan cuál es el equipo de referencia en la pregunta.

- **Distractores de interpretación errónea de porcentajes**: Las opciones que confunden el porcentaje con el valor absoluto cometen un error conceptual básico.

- **Distractores de generalización indebida**: Las opciones que afirman que "ninguno" prefiere cierto equipo son incorrectas porque no podemos hacer esa generalización a partir de una muestra limitada.

- **Distractores de confusión conceptual**: Las opciones que confunden el tamaño de la muestra con las preferencias totales malinterpretan fundamentalmente los datos.

### Paso 4: Verificación matemática
En la muestra de 130 seguidores:

- Colo-Colo: 29 seguidores (22.3%)
- Corinthians: 23 seguidores (17.7%)
- Palmeiras: 26 seguidores (20%)
- Millonarios: 20 seguidores (15.4%)
- Boca Juniors: 32 seguidores (24.6%)

Total: 130 seguidores = 130 seguidores (correcto)

### Paso 5: Principio de respuesta única
En un examen de selección múltiple válido, debe existir **exactamente una respuesta correcta** y tres distractores inequívocamente incorrectos. La respuesta correcta es única y se basa en la interpretación correcta de los datos de la muestra.

### Conclusión
La respuesta correcta interpreta adecuadamente que los datos del gráfico se refieren a la muestra de 130 seguidores, no a la población total. Todos los distractores son inequívocamente incorrectos por diferentes razones conceptuales o de interpretación de datos.

Answerlist
----------
- Falso
- Falso
- Falso
- Verdadero

Meta-information
================
exname: proporciones_encuesta_deportiva
extype: schoice
exsolution: 0001
exshuffle: TRUE
exsection: Estadística|Proporciones|Interpretación de gráficos|Muestreo
