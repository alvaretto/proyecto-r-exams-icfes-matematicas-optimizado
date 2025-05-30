---
output:
  word_document: default
  pdf_document:
    keep_tex: true
    extra_dependencies: ["graphicx", "float", "tikz", "colortbl"]
  html_document: default
---



``` r
options(OutDec = ".")  # Asegurar punto decimal en este chunk

# Establecer semilla aleatoria
set.seed(sample(1:10000, 1))

# Aleatorización del contexto del problema (12 opciones)
contextos_educativos <- c(
  "curso vacacional", "taller de verano", "seminario", "programa de capacitación",
  "curso de extensión", "diplomado", "workshop", "capacitación", "entrenamiento",
  "curso intensivo", "programa educativo", "actividad formativa"
)
contexto_seleccionado <- sample(contextos_educativos, 1)

# Aleatorización de la edad de corte (6 opciones)
edades_corte_disponibles <- c(16, 17, 18, 19, 20, 21)
edad_corte_seleccionada <- sample(edades_corte_disponibles, 1)

# Aleatorización de términos para géneros (4 + 3 opciones)
terminos_masculino_disponibles <- c("hombres", "varones", "participantes masculinos", "estudiantes masculinos")
terminos_femenino_disponibles <- c("mujeres", "participantes femeninas", "estudiantes femeninas")
termino_masculino_seleccionado <- sample(terminos_masculino_disponibles, 1)
termino_femenino_seleccionado <- sample(terminos_femenino_disponibles, 1)

# Aleatorización de términos para grupos de edad (3 + 3 opciones)
terminos_menores_disponibles <- c("menores", "con menos", "que tienen menos")
terminos_mayores_disponibles <- c("mayores", "con más", "que tienen más")
termino_menores_seleccionado <- sample(terminos_menores_disponibles, 1)
termino_mayores_seleccionado <- sample(terminos_mayores_disponibles, 1)

# Aleatorización de términos generales (5 + 4 + 4 opciones)
terminos_participantes_disponibles <- c("participantes", "estudiantes", "asistentes", "inscritos", "matriculados")
termino_participantes_seleccionado <- sample(terminos_participantes_disponibles, 1)

terminos_tabla_disponibles <- c("tabla", "cuadro", "matriz", "esquema")
termino_tabla_seleccionado <- sample(terminos_tabla_disponibles, 1)

terminos_proporciones_disponibles <- c("proporciones", "porcentajes", "frecuencias relativas", "distribución")
termino_proporciones_seleccionado <- sample(terminos_proporciones_disponibles, 1)

# Función para generar proporciones aleatorias manteniendo coherencia matemática
generar_proporciones_validas <- function() {
  repeat {
    # Generar valores base para cada celda (multiplicados por 10 para trabajar con enteros)
    p11_base <- sample(5:15, 1)   # Menores masculino
    p12_base <- sample(15:25, 1)  # Menores femenino
    p21_base <- sample(25:35, 1)  # Mayores masculino
    p22_base <- sample(35:45, 1)  # Mayores femenino

    # Normalizar para que sumen 100
    total_base <- p11_base + p12_base + p21_base + p22_base

    # Convertir a proporciones decimales
    p11 <- round(p11_base / total_base, 1)
    p12 <- round(p12_base / total_base, 1)
    p21 <- round(p21_base / total_base, 1)
    p22 <- round(p22_base / total_base, 1)

    # Ajustar para que sumen exactamente 1.0
    suma_actual <- p11 + p12 + p21 + p22
    if (abs(suma_actual - 1.0) <= 0.1) {
      # Ajuste fino para llegar exactamente a 1.0
      diferencia <- 1.0 - suma_actual
      p22 <- p22 + diferencia
      p22 <- round(p22, 1)

      # Verificar que todas las proporciones sean positivas y razonables
      if (p11 > 0 && p12 > 0 && p21 > 0 && p22 > 0 &&
          p11 < 0.3 && p12 < 0.4 && p21 < 0.5 && p22 < 0.6) {
        return(c(p11, p12, p21, p22))
      }
    }
  }
}

# Obtener proporciones válidas
proporciones_generadas <- generar_proporciones_validas()
p_menor_masc <- proporciones_generadas[1]  # P(Menor ∩ Masculino)
p_menor_fem <- proporciones_generadas[2]   # P(Menor ∩ Femenino)
p_mayor_masc <- proporciones_generadas[3]  # P(Mayor ∩ Masculino)
p_mayor_fem <- proporciones_generadas[4]   # P(Mayor ∩ Femenino)

# Scripts de prueba de integridad matemática
test_that("Las proporciones suman 1.0", {
  expect_equal(sum(proporciones_generadas), 1.0, tolerance = 0.01)
})

# Calcular probabilidades marginales
p_masculino <- p_menor_masc + p_mayor_masc
p_femenino <- p_menor_fem + p_mayor_fem
p_menor <- p_menor_masc + p_menor_fem
p_mayor <- p_mayor_masc + p_mayor_fem

# Verificar coherencia de probabilidades marginales
test_that("Probabilidades marginales son coherentes", {
  expect_equal(p_masculino + p_femenino, 1.0, tolerance = 0.01)
  expect_equal(p_menor + p_mayor, 1.0, tolerance = 0.01)
})

# Aleatorización del tipo de pregunta (4 tipos diferentes de probabilidad condicional)
tipos_pregunta_disponibles <- list(
  list(condicion = "femenino", evento = "mayor",
       texto_condicion = termino_femenino_seleccionado,
       texto_evento = paste(termino_mayores_seleccionado, "de", edad_corte_seleccionada, "años"),
       numerador = p_mayor_fem, denominador = p_femenino),
  list(condicion = "masculino", evento = "mayor",
       texto_condicion = termino_masculino_seleccionado,
       texto_evento = paste(termino_mayores_seleccionado, "de", edad_corte_seleccionada, "años"),
       numerador = p_mayor_masc, denominador = p_masculino),
  list(condicion = "femenino", evento = "menor",
       texto_condicion = termino_femenino_seleccionado,
       texto_evento = paste(termino_menores_seleccionado, "de", edad_corte_seleccionada, "años"),
       numerador = p_menor_fem, denominador = p_femenino),
  list(condicion = "masculino", evento = "menor",
       texto_condicion = termino_masculino_seleccionado,
       texto_evento = paste(termino_menores_seleccionado, "de", edad_corte_seleccionada, "años"),
       numerador = p_menor_masc, denominador = p_masculino)
)

pregunta_seleccionada <- sample(tipos_pregunta_disponibles, 1)[[1]]

# Calcular la respuesta correcta
respuesta_correcta_fraccion <- paste0(pregunta_seleccionada$numerador, "/", pregunta_seleccionada$denominador)
respuesta_correcta_decimal <- round(pregunta_seleccionada$numerador / pregunta_seleccionada$denominador, 3)

# Generar distractores plausibles basados en errores conceptuales comunes
distractor1 <- paste0(pregunta_seleccionada$denominador, "/", pregunta_seleccionada$numerador)  # Invertir fracción
distractor2 <- paste0(pregunta_seleccionada$numerador, "/1.0")  # Usar probabilidad conjunta directamente
distractor3 <- paste0(pregunta_seleccionada$numerador, "/", round(1 - pregunta_seleccionada$denominador, 1))  # Usar complemento del denominador

# Crear vector con todas las opciones y mezclarlas
opciones_respuesta <- c(respuesta_correcta_fraccion, distractor1, distractor2, distractor3)
names(opciones_respuesta) <- c("correcta", "distractor1", "distractor2", "distractor3")
opciones_mezcladas <- sample(opciones_respuesta)

# Identificar posición de la respuesta correcta
indice_correcto <- which(opciones_mezcladas == respuesta_correcta_fraccion)

# Crear vector de solución para r-exams
solucion_vector <- rep(0, 4)
solucion_vector[indice_correcto] <- 1

# Aleatorización del dato conocido en el enunciado (4 opciones)
datos_conocidos_disponibles <- list(
  list(valor = p_menor_masc, descripcion = paste(termino_masculino_seleccionado, termino_menores_seleccionado, "de", edad_corte_seleccionada, "años")),
  list(valor = p_menor_fem, descripcion = paste(termino_femenino_seleccionado, termino_menores_seleccionado, "de", edad_corte_seleccionada, "años")),
  list(valor = p_mayor_masc, descripcion = paste(termino_masculino_seleccionado, termino_mayores_seleccionado, "de", edad_corte_seleccionada, "años")),
  list(valor = p_mayor_fem, descripcion = paste(termino_femenino_seleccionado, termino_mayores_seleccionado, "de", edad_corte_seleccionada, "años"))
)

dato_conocido_seleccionado <- sample(datos_conocidos_disponibles, 1)[[1]]
porcentaje_conocido <- round(dato_conocido_seleccionado$valor * 100, 0)
```


``` r
options(OutDec = ".")  # Asegurar punto decimal en este chunk

# Aleatorizar colores de la tabla siguiendo el patrón exitoso de ejemplos funcionales
colores_fondo_disponibles <- c("orange", "blue", "green", "red", "cyan", "purple")
intensidades_disponibles <- c(10, 15, 20, 25, 30)
color_fondo_seleccionado <- sample(colores_fondo_disponibles, 1)
intensidad_seleccionada <- sample(intensidades_disponibles, 1)
color_tabla_final <- paste0(color_fondo_seleccionado, "!", intensidad_seleccionada)

# Función para generar tabla TikZ siguiendo el patrón exitoso de ejemplos funcionales
generar_tabla_contingencia_tikz_robusta <- function(termino_masc, termino_fem, termino_men, termino_may,
                                                   edad, p_men_masc, p_men_fem, p_may_masc, p_may_fem,
                                                   color_tabla, termino_part) {
  # Crear tabla con TikZ usando el patrón exitoso de los ejemplos
  tabla_codigo <- c(
    "\\begin{tikzpicture}",
    "\\node[inner sep=0pt] {",
    "  \\begin{tabular}{|c|c|c|}",
    "    \\hline",
    paste0("    \\rowcolor{", color_tabla, "}"),
    paste0("    \\textbf{Grupo de edad} & \\textbf{", stringr::str_to_title(termino_masc), "} & \\textbf{", stringr::str_to_title(termino_fem), "} \\\\"),
    "    \\hline",
    paste0("    ", stringr::str_to_title(termino_men), " de ", edad, " años & ", p_men_masc, " & ", p_men_fem, " \\\\"),
    "    \\hline",
    paste0("    ", stringr::str_to_title(termino_may), " de ", edad, " años & ", p_may_masc, " & ", p_may_fem, " \\\\"),
    "    \\hline",
    "  \\end{tabular}",
    "};",
    "\\end{tikzpicture}"
  )

  return(tabla_codigo)
}

# Generar código TikZ para la tabla de contingencia
tabla_tikz_codigo <- generar_tabla_contingencia_tikz_robusta(
  termino_masculino_seleccionado, termino_femenino_seleccionado,
  termino_menores_seleccionado, termino_mayores_seleccionado,
  edad_corte_seleccionada, p_menor_masc, p_menor_fem, p_mayor_masc, p_mayor_fem,
  color_tabla_final, termino_participantes_seleccionado
)

# Scripts de prueba de integridad y calidad de funciones
test_that("Código TikZ se genera correctamente", {
  expect_true(length(tabla_tikz_codigo) > 0)
  expect_true(any(grepl("\\\\begin\\{tikzpicture\\}", tabla_tikz_codigo)))
  expect_true(any(grepl("\\\\end\\{tikzpicture\\}", tabla_tikz_codigo)))
  expect_true(any(grepl("\\\\begin\\{tabular\\}", tabla_tikz_codigo)))
  expect_true(any(grepl("\\\\end\\{tabular\\}", tabla_tikz_codigo)))
})

# Prueba de calidad de la función generadora de TikZ
test_that("Función generadora de tabla TikZ es robusta", {
  # Probar con diferentes parámetros
  tabla_prueba <- generar_tabla_contingencia_tikz_robusta(
    "hombres", "mujeres", "menores", "mayores",
    18, 0.1, 0.2, 0.3, 0.4, "blue!20", "participantes"
  )
  expect_true(length(tabla_prueba) > 10)
  expect_true(any(grepl("hombres", tabla_prueba, ignore.case = TRUE)))
  expect_true(any(grepl("mujeres", tabla_prueba, ignore.case = TRUE)))
  expect_true(any(grepl("0\\.1", tabla_prueba)))
  expect_true(any(grepl("0\\.4", tabla_prueba)))
})

# Prueba de coherencia matemática después de los cambios
test_that("Coherencia matemática post-cambios", {
  # Verificar que las proporciones siguen siendo válidas
  suma_total <- p_menor_masc + p_menor_fem + p_mayor_masc + p_mayor_fem
  expect_equal(suma_total, 1.0, tolerance = 0.01)

  # Verificar probabilidades marginales
  p_masc_calculado <- p_menor_masc + p_mayor_masc
  p_fem_calculado <- p_menor_fem + p_mayor_fem
  expect_equal(p_masc_calculado + p_fem_calculado, 1.0, tolerance = 0.01)

  # Verificar que todas las proporciones son positivas
  expect_true(all(c(p_menor_masc, p_menor_fem, p_mayor_masc, p_mayor_fem) > 0))

  # Verificar que ninguna proporción es mayor que 1
  expect_true(all(c(p_menor_masc, p_menor_fem, p_mayor_masc, p_mayor_fem) < 1))
})

# Prueba de calidad de distractores
test_that("Calidad de distractores matemáticos", {
  # Verificar que la respuesta correcta es diferente de todos los distractores
  expect_false(respuesta_correcta_fraccion == distractor1)
  expect_false(respuesta_correcta_fraccion == distractor2)
  expect_false(respuesta_correcta_fraccion == distractor3)

  # Verificar que todos los distractores son diferentes entre sí
  expect_false(distractor1 == distractor2)
  expect_false(distractor2 == distractor3)
  expect_false(distractor1 == distractor3)

  # Verificar que el valor decimal de la respuesta correcta está en rango válido [0,1]
  expect_true(respuesta_correcta_decimal >= 0 && respuesta_correcta_decimal <= 1)
})

# Script de prueba temporal para verificar coherencia de género
test_that("Coherencia de términos de género", {
  expect_true(termino_masculino_seleccionado %in% c("hombres", "varones", "participantes masculinos", "estudiantes masculinos"))
  expect_true(termino_femenino_seleccionado %in% c("mujeres", "participantes femeninas", "estudiantes femeninas"))
  expect_false(termino_masculino_seleccionado == termino_femenino_seleccionado)
})
```

Question
========

En la matriz se muestran las porcentajes de estudiantes en un taller de verano, dependiendo del género y la edad.

\begin{center}
\begin{tabular}{|c|c|c|}
\hline
\textbf{Grupo de edad} & \textbf{Participantes Masculinos} & \textbf{Participantes Femeninas} \\
\hline
Menores de 20 años & 0.1 & 0.2 \\
\hline
Que Tienen Más de 20 años & 0.3 & 0.4 \\
\hline
\end{tabular}
\end{center}

Por ejemplo, el 40% de los estudiantes son participantes femeninas que tienen más de 20 años. Según la matriz, ¿cuál es la probabilidad de que al escoger una persona al azar tenga menores de 20 años, si ya se sabe que es participantes masculinos?

Answerlist
----------
- 0.1/1.0
- 0.4/0.1
- 0.1/0.4
- 0.1/0.6

Solution
========

Para resolver este problema de probabilidad condicional, necesitamos aplicar la fórmula de probabilidad condicional y trabajar con la información de la tabla de contingencia.

### Paso 1: Identificar el tipo de problema
Este es un problema de **probabilidad condicional**, donde buscamos:
$$P(\\text{menores de 20 años} | \\text{participantes masculinos})$$

### Paso 2: Recordar la fórmula de probabilidad condicional
La probabilidad condicional se calcula como:
$$P(A|B) = \\frac{P(A \\cap B)}{P(B)}$$

Donde:
- $A$ = evento de interés (menores de 20 años)
- $B$ = condición dada (participantes masculinos)
- $P(A \\cap B)$ = probabilidad de que ocurran ambos eventos
- $P(B)$ = probabilidad de la condición

### Paso 3: Extraer información de la tabla
De la tabla de contingencia podemos obtener:

**Probabilidades conjuntas:**
- P(menores de 20 años ∩ participantes masculinos) = 0.1
- P(menores de 20 años ∩ participantes femeninas) = 0.2
- P(que tienen más de 20 años ∩ participantes masculinos) = 0.3
- P(que tienen más de 20 años ∩ participantes femeninas) = 0.4

**Probabilidades marginales:**
- P(participantes masculinos) = 0.1 + 0.3 = 0.4
- P(participantes femeninas) = 0.2 + 0.4 = 0.6
- P(menores de 20 años) = 0.1 + 0.2 = 0.3
- P(que tienen más de 20 años) = 0.3 + 0.4 = 0.7

### Paso 4: Aplicar la fórmula
Para nuestro problema específico:
$$P(\\text{menores de 20 años} | \\text{participantes masculinos}) = \\frac{P(\\text{menores de 20 años} \\cap \\text{participantes masculinos})}{P(\\text{participantes masculinos})}$$

Sustituyendo los valores:
$$P(\\text{menores de 20 años} | \\text{participantes masculinos}) = \\frac{0.1}{0.4}$$

### Paso 5: Verificación
Podemos verificar que este resultado tiene sentido:
- El numerador (0.1) representa la probabilidad conjunta del evento y la condición
- El denominador (0.4) representa la probabilidad marginal de la condición
- El resultado 0.1/0.4 = 0.25 está entre 0 y 1, como debe ser toda probabilidad

### Análisis de distractores comunes:
- **0.4/0.1**: Error de invertir numerador y denominador
- **0.1/1.0**: Error de usar solo la probabilidad conjunta sin dividir por la probabilidad de la condición
- **0.1/0.6**: Error de usar el complemento de la probabilidad de la condición

### Conclusión
Por lo tanto, la probabilidad de que una persona tenga menores de 20 años, dado que es participantes masculinos, es **0.1/0.4**.

Answerlist
----------
- Falso
- Falso
- Verdadero
- Falso

Meta-information
================
exname: probabilidad_condicional_tabla_contingencia_mejorado
extype: schoice
exsolution: 0010
exshuffle: TRUE
exsection: Probabilidad|Probabilidad condicional|Tablas de contingencia
exextra[Type]: Cálculo
exextra[Program]: R
exextra[Language]: es
exextra[Level]: 3
