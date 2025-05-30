---
tipo_pregunta: "Selección múltiple con única respuesta"
competencia: "Interpretación y representación"
componente: "Aleatorio y sistemas de datos"
afirmacion: "Interpreta información presentada en tablas y gráficos"
nivel_dificultad: "Medio"
tiempo_estimado: "3 minutos"
autor: "Sistema R-Exams ICFES"
version: "2.0 RADICAL"
output:
  pdf_document:
    keep_tex: true
  html_document: default
  word_document: default
---




``` r
options(OutDec = ".")
options(scipen = 999)
options(digits = 10)

# Establecer semilla para reproducibilidad
set.seed(sample(1:10000, 1))

# FUNCIÓN RADICAL DE FORMATEO ENTERO
formatear_entero <- function(numero) {
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
  competicion <- sample(competiciones_clubes_europeos, 1)
  equipos_disponibles <- equipos_europeos
} else if (tipo_competicion == 2) {
  competicion <- sample(competiciones_clubes_sudamericanos, 1)
  equipos_disponibles <- equipos_sudamericanos
} else if (tipo_competicion == 3) {
  competicion <- sample(competiciones_selecciones_europeas, 1)
  equipos_disponibles <- selecciones_europeas
} else if (tipo_competicion == 4) {
  competicion <- sample(competiciones_selecciones_sudamericanas, 1)
  equipos_disponibles <- selecciones_sudamericanas
} else {
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
poblacion_total <- sample(c(25000, 30000, 40000, 50000, 60000, 75000, 80000, 90000, 100000), 1)
tamano_muestra <- sample(c(80, 90, 100, 110, 120, 130, 140, 150), 1)

# CREAR VARIABLES FORMATEADAS PARA MOSTRAR
poblacion_total_fmt <- formatear_entero(poblacion_total)
tamano_muestra_fmt <- formatear_entero(tamano_muestra)

# FUNCIÓN ROBUSTA PARA GENERAR VALORES CON VARIABILIDAD GARANTIZADA
generar_valores_coherentes <- function(total, max_intentos = 100) {
  min_valor <- max(8, round(total * 0.08))
  max_valor <- min(40, round(total * 0.35))

  if (min_valor * 5 > total) {
    stop("ERROR: Imposible generar 5 valores con min_valor = ", min_valor, " y total = ", total)
  }

  for (intento in 1:max_intentos) {
    if (intento <= 50) {
      valor_base <- round(total / 5)
      variacion <- min(6, round(valor_base * 0.3))
      valores <- c(
        valor_base - variacion,
        valor_base - round(variacion/2),
        valor_base,
        valor_base + round(variacion/2),
        valor_base + variacion
      )
      valores <- pmax(valores, min_valor)
      valores <- pmin(valores, max_valor)
    } else {
      valores <- sample(min_valor:max_valor, 5, replace = TRUE)
    }

    diferencia <- total - sum(valores)
    intentos_ajuste <- 0
    while (diferencia != 0 && intentos_ajuste < 30) {
      if (diferencia > 0) {
        indices_validos <- which(valores < max_valor)
        if (length(indices_validos) > 0) {
          idx <- sample(indices_validos, 1)
          incremento <- min(diferencia, max_valor - valores[idx])
          valores[idx] <- valores[idx] + incremento
          diferencia <- diferencia - incremento
        } else {
          break
        }
      } else {
        indices_validos <- which(valores > min_valor)
        if (length(indices_validos) > 0) {
          idx <- sample(indices_validos, 1)
          decremento <- min(abs(diferencia), valores[idx] - min_valor)
          valores[idx] <- valores[idx] - decremento
          diferencia <- diferencia + decremento
        } else {
          break
        }
      }
      intentos_ajuste <- intentos_ajuste + 1
    }

    if (sum(valores) == total &&
        all(valores >= min_valor) &&
        all(valores <= max_valor) &&
        length(unique(valores)) >= 3) {
      valores <- sample(valores)
      if (sum(valores) != total) {
        stop("ERROR FINAL: suma = ", sum(valores), ", esperado = ", total)
      }
      return(valores)
    }
  }

  # Método de emergencia
  valor_base <- floor(total / 5)
  resto <- total %% 5
  valores <- rep(valor_base, 5)
  for (i in 1:resto) {
    valores[i] <- valores[i] + 1
  }
  if (valores[1] > min_valor && valores[5] < max_valor) {
    valores[1] <- valores[1] - 1
    valores[5] <- valores[5] + 1
  }
  if (sum(valores) != total) {
    stop("ERROR CRÍTICO DE EMERGENCIA: suma = ", sum(valores), ", esperado = ", total)
  }
  return(valores)
}

valores_equipos <- generar_valores_coherentes(tamano_muestra)

# SELECCIÓN ALEATORIA DEL EQUIPO CORRECTO
indice_equipo_correcto <- sample(1:5, 1)

# Asignar variables individuales
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

# VARIABLES DINÁMICAS PARA EL EQUIPO CORRECTO SELECCIONADO ALEATORIAMENTE
equipo_correcto <- equipos_seleccionados[indice_equipo_correcto]
valor_correcto <- valores_equipos[indice_equipo_correcto]
valor_correcto_fmt <- formatear_entero(valor_correcto)

# Validaciones matemáticas robustas
if (sum(valores_equipos) != tamano_muestra) {
  stop("Error: Los valores no suman el tamaño de muestra. Suma: ", sum(valores_equipos),
       ", Esperado: ", tamano_muestra)
}

min_esperado <- max(8, round(tamano_muestra * 0.08))
max_esperado <- min(40, round(tamano_muestra * 0.35))
if (!all(valores_equipos >= min_esperado) || !all(valores_equipos <= max_esperado)) {
  stop("Error: Algunos valores están fuera del rango esperado [", min_esperado, ", ", max_esperado, "]")
}

valores_unicos <- unique(valores_equipos)
if (length(valores_unicos) < 3) {
  stop("Error: Variabilidad insuficiente. Solo ", length(valores_unicos),
       " valores únicos de 5. Valores: [", paste(valores_equipos, collapse=", "),
       "]. Se requieren al menos 3 valores únicos.")
}

if (!all(valores_equipos > 0)) {
  stop("Error: Algunos valores no son positivos")
}

if (max(valores_equipos) > tamano_muestra * 0.4) {
  stop("Error: Un equipo tiene demasiados votos (>40% del total)")
}

# Colores para el gráfico
colores <- c("#2E8B57", "#4682B4", "#CD853F", "#9370DB", "#DC143C")

# ============================================================================
# LÓGICA RADICAL DE DISTRACTORES - SOLUCIÓN DEFINITIVA
# ============================================================================
# PROBLEMA RESUELTO: Los distractores anteriores usaban valores reales de otros
# equipos, creando múltiples respuestas correctas. Esta nueva lógica garantiza
# que SOLO hay una respuesta correcta y tres distractores inequívocamente incorrectos.

# RESPUESTA CORRECTA: Usar el equipo seleccionado aleatoriamente
respuesta_correcta <- paste0("alrededor de ", valor_correcto_fmt, " de cada ", tamano_muestra_fmt, " ",
                            termino_usuarios, " del ", contexto, " da por favorito al ", equipo_correcto, ".")

# GENERAR DISTRACTORES INEQUÍVOCAMENTE INCORRECTOS
# Estos distractores son matemáticamente incorrectos y pedagógicamente útiles

# DISTRACTOR 1: Confusión muestra-población (ERROR CONCEPTUAL)
# Usar el valor correcto pero con la población total en lugar de la muestra
distractor1 <- paste0("alrededor de ", valor_correcto_fmt, " de cada ", poblacion_total_fmt, " ",
                     termino_usuarios, " del ", contexto, " da por favorito al ", equipo_correcto, ".")

# DISTRACTOR 2: Error de interpretación de porcentaje (ERROR MATEMÁTICO)
# Usar el porcentaje como si fuera un valor absoluto
porcentaje_correcto <- round(valor_correcto / tamano_muestra * 100)
# Asegurar que el porcentaje es diferente del valor absoluto para evitar equivalencias
if (porcentaje_correcto == valor_correcto) {
  # Si son iguales, ajustar ligeramente el porcentaje
  porcentaje_correcto <- porcentaje_correcto + sample(c(-2, -1, 1, 2), 1)
  porcentaje_correcto <- max(1, min(50, porcentaje_correcto))  # Mantener en rango razonable
}
distractor2 <- paste0("alrededor de ", formatear_entero(porcentaje_correcto), " de cada ", tamano_muestra_fmt, " ",
                     termino_usuarios, " del ", contexto, " da por favorito al ", equipo_correcto, ".")

# DISTRACTOR 3: Error de cálculo matemático (VALOR INCORRECTO)
# Generar un valor que NO corresponda a ningún equipo de la muestra
valores_prohibidos <- valores_equipos  # No usar ningún valor real de la muestra
valor_incorrecto <- valor_correcto
# Generar un valor diferente que no esté en la muestra
intentos_valor <- 0
while (valor_incorrecto %in% valores_prohibidos && intentos_valor < 20) {
  # Estrategias para generar valor incorrecto
  if (intentos_valor < 5) {
    # Estrategia 1: Sumar/restar una cantidad pequeña
    valor_incorrecto <- valor_correcto + sample(c(-5, -4, -3, -2, -1, 1, 2, 3, 4, 5), 1)
  } else if (intentos_valor < 10) {
    # Estrategia 2: Usar un múltiplo o división
    factor <- sample(c(2, 3), 1)
    if (sample(c(TRUE, FALSE), 1)) {
      valor_incorrecto <- round(valor_correcto * factor)
    } else {
      valor_incorrecto <- round(valor_correcto / factor)
    }
  } else {
    # Estrategia 3: Generar valor completamente aleatorio en rango razonable
    min_rango <- max(5, round(tamano_muestra * 0.05))
    max_rango <- min(50, round(tamano_muestra * 0.45))
    valor_incorrecto <- sample(min_rango:max_rango, 1)
  }

  # Asegurar que está en rango válido
  valor_incorrecto <- max(1, min(tamano_muestra - 1, valor_incorrecto))
  intentos_valor <- intentos_valor + 1
}

# Si después de todos los intentos sigue siendo un valor de la muestra, usar valor fijo
if (valor_incorrecto %in% valores_prohibidos) {
  # Usar un valor que definitivamente no esté en la muestra
  valor_incorrecto <- round(tamano_muestra * 0.6)  # 60% del tamaño de muestra
  if (valor_incorrecto %in% valores_prohibidos) {
    valor_incorrecto <- round(tamano_muestra * 0.7)  # 70% del tamaño de muestra
  }
}

valor_incorrecto_fmt <- formatear_entero(valor_incorrecto)
distractor3 <- paste0("alrededor de ", valor_incorrecto_fmt, " de cada ", tamano_muestra_fmt, " ",
                     termino_usuarios, " del ", contexto, " da por favorito al ", equipo_correcto, ".")

# CREAR LISTA DE DISTRACTORES FINALES
distractores_finales <- c(distractor1, distractor2, distractor3)

# VERIFICACIÓN CRÍTICA: Asegurar que todos los distractores son únicos
if (length(unique(distractores_finales)) != 3) {
  stop("Error crítico: Los distractores generados no son únicos")
}

# VERIFICACIÓN CRÍTICA: Asegurar que ningún distractor es igual a la respuesta correcta
for (i in 1:3) {
  if (distractores_finales[i] == respuesta_correcta) {
    stop("Error crítico: El distractor ", i, " es igual a la respuesta correcta")
  }
}

# CREAR OPCIONES FINALES Y MEZCLAR
opciones <- c(respuesta_correcta, distractores_finales[1], distractores_finales[2], distractores_finales[3])
opciones_mezcladas <- sample(opciones)
indice_correcto <- which(opciones_mezcladas == respuesta_correcta)

# Vector de solución para r-exams
solucion <- rep(0, 4)
solucion[indice_correcto] <- 1

# ============================================================================
# VALIDACIONES FINALES EXHAUSTIVAS - GARANTÍA DE CALIDAD
# ============================================================================

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

# VALIDACIÓN MATEMÁTICA CRÍTICA: Verificar que solo hay una respuesta correcta
# Esta es la validación más importante - detectar múltiples respuestas correctas
respuestas_correctas_detectadas <- 0
for (opcion in opciones) {
  # Extraer el valor numérico de cada opción
  if (grepl(paste0(valor_correcto_fmt, " de cada ", tamano_muestra_fmt), opcion) &&
      grepl(equipo_correcto, opcion, fixed = TRUE)) {
    respuestas_correctas_detectadas <- respuestas_correctas_detectadas + 1
  }
}

if (respuestas_correctas_detectadas != 1) {
  stop("ERROR CRÍTICO: Se detectaron ", respuestas_correctas_detectadas,
       " respuestas correctas. Debe haber exactamente 1.")
}

# PRUEBA DE INTEGRIDAD ADICIONAL: Verificar que los distractores usan valores incorrectos
for (i in 1:3) {
  distractor <- distractores_finales[i]
  # El distractor 1 debe usar población total (incorrecto)
  if (i == 1 && !grepl(poblacion_total_fmt, distractor)) {
    stop("Error: Distractor 1 no usa población total como esperado")
  }
  # Los distractores 2 y 3 deben usar valores diferentes al correcto
  if (i > 1 && grepl(paste0(valor_correcto_fmt, " de cada ", tamano_muestra_fmt), distractor)) {
    stop("Error: Distractor ", i, " usa el valor correcto, lo que lo convierte en respuesta correcta")
  }
}

cat("✅ VALIDACIÓN EXITOSA: Exactamente 1 respuesta correcta y 3 distractores inequívocamente incorrectos\n")
```

✅ VALIDACIÓN EXITOSA: Exactamente 1 respuesta correcta y 3 distractores inequívocamente incorrectos


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

Un portal web deportivo realizó el estudio a un grupo de sus 75000 miembros sobre la preferencia de su equipo favorito para ganar la Eurocopa. Para esto escogió al azar a 90 miembros y les preguntó sobre su equipo favorito para ganar dicha competición. Los resultados se muestran en la gráfica.

![](grafico_barras.png){width=80%}

De acuerdo con los datos obtenidos en el estudio, es correcto afirmar que

Answerlist
----------
- alrededor de 18 de cada 90 miembros del portal web deportivo da por favorito al Italia.
- alrededor de 16 de cada 90 miembros del portal web deportivo da por favorito al Italia.
- alrededor de 16 de cada 75000 miembros del portal web deportivo da por favorito al Italia.
- alrededor de 15 de cada 90 miembros del portal web deportivo da por favorito al Italia.

Solution
========

Para resolver este problema, necesitamos interpretar correctamente los datos del gráfico de barras y entender la diferencia entre muestra y población total.

### Paso 1: Identificar los datos conocidos
* Un portal web deportivo tiene un total de 75000 miembros.
* Se realizó un(a) estudio a una muestra de 90 miembros seleccionados al azar.
* Según el gráfico, 16 miembros de la muestra prefieren al Italia.

### Paso 2: Interpretar correctamente las proporciones
Los datos del gráfico representan únicamente la muestra de 90 miembros, no la población total de 75000 miembros.

### Paso 3: Analizar cada opción

**Opción correcta**: "alrededor de 16 de cada 90 miembros del portal web deportivo da por favorito al Italia."
Esta opción es correcta porque interpreta adecuadamente que 16 de cada 90 miembros **de la muestra** prefieren al Italia. Esto representa una proporción del 17.8% en la muestra.

**Análisis de distractores incorrectos**:

- **Error conceptual muestra-población**: El distractor que menciona proporciones sobre la población total de 75000 miembros es incorrecto, ya que el estudio solo se realizó a 90 personas.

- **Error de interpretación de porcentajes**: El distractor que confunde el porcentaje (17.8%) con el valor absoluto (16) comete un error conceptual básico.

- **Error de cálculo matemático**: El distractor que usa un valor incorrecto (15) no corresponde a los datos reales del gráfico para el Italia.

### Paso 4: Verificación matemática
En la muestra de 90 miembros:

- España: 23 miembros (25.6%)
- Alemania: 18 miembros (20%)
- Italia: 16 miembros (17.8%)
- Croacia: 20 miembros (22.2%)
- Suecia: 13 miembros (14.4%)

Total: 90 miembros = 90 miembros (correcto)

### Paso 5: Principio de respuesta única
En un examen de selección múltiple válido, debe existir **exactamente una respuesta correcta** y tres distractores inequívocamente incorrectos. La respuesta correcta es única y se basa en la interpretación correcta de los datos de la muestra.

### Conclusión
La respuesta correcta interpreta adecuadamente que los datos del gráfico se refieren a la muestra de 90 miembros, no a la población total. Todos los distractores son inequívocamente incorrectos por diferentes razones conceptuales o de interpretación de datos.

Answerlist
----------
- Falso
- Verdadero
- Falso
- Falso

Meta-information
================
exname: proporciones_encuesta_deportiva_v2_RADICAL
extype: schoice
exsolution: 0100
exshuffle: TRUE
exsection: Estadística|Proporciones|Interpretación de gráficos|Muestreo
