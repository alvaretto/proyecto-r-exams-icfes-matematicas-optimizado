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

# Aleatorización del contexto del problema
contextos <- c(
  "curso vacacional", "taller de verano", "seminario", "programa de capacitación",
  "curso de extensión", "diplomado", "workshop", "capacitación", "entrenamiento",
  "curso intensivo", "programa educativo", "actividad formativa"
)
contexto <- sample(contextos, 1)

# Aleatorización de la edad de corte
edades_corte <- c(16, 17, 18, 19, 20, 21)
edad_corte <- sample(edades_corte, 1)

# Aleatorización de términos para géneros
terminos_masculino <- c("hombres", "varones", "participantes masculinos", "estudiantes masculinos")
terminos_femenino <- c("mujeres", "participantes femeninas", "estudiantes femeninas")
termino_masculino <- sample(terminos_masculino, 1)
termino_femenino <- sample(terminos_femenino, 1)

# Aleatorización de términos para grupos de edad
terminos_menores <- c("menores", "con menos", "que tienen menos")
terminos_mayores <- c("mayores", "con más", "que tienen más")
termino_menores <- sample(terminos_menores, 1)
termino_mayores <- sample(terminos_mayores, 1)

# Aleatorización de términos generales
terminos_participantes <- c("participantes", "estudiantes", "asistentes", "inscritos", "matriculados")
termino_participantes <- sample(terminos_participantes, 1)

terminos_tabla <- c("tabla", "cuadro", "matriz", "esquema")
termino_tabla <- sample(terminos_tabla, 1)

terminos_proporciones <- c("proporciones", "porcentajes", "frecuencias relativas", "distribución")
termino_proporciones <- sample(terminos_proporciones, 1)

# Generación de proporciones aleatorias manteniendo coherencia matemática
generar_proporciones <- function() {
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
proporciones <- generar_proporciones()
p_menor_masc <- proporciones[1]  # P(Menor ∩ Masculino)
p_menor_fem <- proporciones[2]   # P(Menor ∩ Femenino)
p_mayor_masc <- proporciones[3]  # P(Mayor ∩ Masculino)
p_mayor_fem <- proporciones[4]   # P(Mayor ∩ Femenino)

# Verificar que suman 1.0
test_that("Las proporciones suman 1.0", {
  expect_equal(sum(proporciones), 1.0, tolerance = 0.01)
})
```

Test passed 🥳

``` r
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
```

Test passed 🌈

``` r
# Aleatorización del tipo de pregunta (qué probabilidad condicional calcular)
tipos_pregunta <- list(
  list(condicion = "femenino", evento = "mayor",
       texto_condicion = termino_femenino, texto_evento = paste(termino_mayores, "de", edad_corte, "años"),
       numerador = p_mayor_fem, denominador = p_femenino),
  list(condicion = "masculino", evento = "mayor",
       texto_condicion = termino_masculino, texto_evento = paste(termino_mayores, "de", edad_corte, "años"),
       numerador = p_mayor_masc, denominador = p_masculino),
  list(condicion = "femenino", evento = "menor",
       texto_condicion = termino_femenino, texto_evento = paste(termino_menores, "de", edad_corte, "años"),
       numerador = p_menor_fem, denominador = p_femenino),
  list(condicion = "masculino", evento = "menor",
       texto_condicion = termino_masculino, texto_evento = paste(termino_menores, "de", edad_corte, "años"),
       numerador = p_menor_masc, denominador = p_masculino)
)

pregunta_seleccionada <- sample(tipos_pregunta, 1)[[1]]

# Calcular la respuesta correcta
respuesta_correcta_fraccion <- paste0(pregunta_seleccionada$numerador, "/", pregunta_seleccionada$denominador)
respuesta_correcta_decimal <- round(pregunta_seleccionada$numerador / pregunta_seleccionada$denominador, 3)

# Generar distractores plausibles basados en errores comunes
distractor1 <- paste0(pregunta_seleccionada$denominador, "/", pregunta_seleccionada$numerador)  # Invertir fracción
distractor2 <- paste0(pregunta_seleccionada$numerador, "/1.0")  # Usar probabilidad conjunta directamente
distractor3 <- paste0(pregunta_seleccionada$numerador, "/", round(1 - pregunta_seleccionada$denominador, 1))  # Usar complemento del denominador

# Crear vector con todas las opciones
opciones <- c(respuesta_correcta_fraccion, distractor1, distractor2, distractor3)
names(opciones) <- c("correcta", "distractor1", "distractor2", "distractor3")

# Mezclar opciones
opciones_mezcladas <- sample(opciones)

# Identificar posición de la respuesta correcta
indice_correcto <- which(opciones_mezcladas == respuesta_correcta_fraccion)

# Crear vector de solución para r-exams
solucion <- rep(0, 4)
solucion[indice_correcto] <- 1

# Aleatorización del dato conocido en el enunciado
datos_conocidos <- list(
  list(valor = p_menor_masc, descripcion = paste(termino_masculino, termino_menores, "de", edad_corte, "años")),
  list(valor = p_menor_fem, descripcion = paste(termino_femenino, termino_menores, "de", edad_corte, "años")),
  list(valor = p_mayor_masc, descripcion = paste(termino_masculino, termino_mayores, "de", edad_corte, "años")),
  list(valor = p_mayor_fem, descripcion = paste(termino_femenino, termino_mayores, "de", edad_corte, "años"))
)

dato_conocido <- sample(datos_conocidos, 1)[[1]]
porcentaje_conocido <- round(dato_conocido$valor * 100, 0)
```


``` r
options(OutDec = ".")  # Asegurar punto decimal en este chunk

# Generar tabla de contingencia usando TikZ
codigo_tikz <- paste0("
\\begin{tikzpicture}[scale=0.8]
  % Definir colores
  \\definecolor{headercolor}{RGB}{76, 175, 80}
  \\definecolor{cellcolor}{RGB}{240, 248, 255}
  \\definecolor{bordercolor}{RGB}{100, 100, 100}

  % Configurar estilo de nodos
  \\tikzset{
    header/.style={fill=headercolor, text=white, font=\\bfseries, minimum height=0.8cm, minimum width=2.5cm, draw=bordercolor, line width=0.5pt},
    cell/.style={fill=cellcolor, minimum height=0.8cm, minimum width=2.5cm, draw=bordercolor, line width=0.5pt, font=\\large},
    label/.style={font=\\bfseries, minimum height=0.8cm, minimum width=2.5cm}
  }

  % Encabezados de columnas
  \\node[header] at (2.5, 2) {", stringr::str_to_title(termino_masculino), "};
  \\node[header] at (5, 2) {", stringr::str_to_title(termino_femenino), "};

  % Etiquetas de filas
  \\node[header] at (0, 1) {", stringr::str_to_title(termino_menores), " de ", edad_corte, " años};
  \\node[header] at (0, 0) {", stringr::str_to_title(termino_mayores), " de ", edad_corte, " años};

  % Celdas de datos
  \\node[cell] at (2.5, 1) {", p_menor_masc, "};
  \\node[cell] at (5, 1) {", p_menor_fem, "};
  \\node[cell] at (2.5, 0) {", p_mayor_masc, "};
  \\node[cell] at (5, 0) {", p_mayor_fem, "};

  % Título de la tabla
  \\node[label, font=\\Large\\bfseries] at (2.75, 3) {Distribución de ", termino_participantes, " por género y edad};
\\end{tikzpicture}
")

# Guardar código TikZ en archivo temporal
writeLines(codigo_tikz, "tabla_contingencia.tex")
```

Question
========

En la esquema se muestran las frecuencias relativas de asistentes en un diplomado, dependiendo del género y la edad.


```
## Error: LaTeX failed to compile tikzpicture.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips. See tikzpicture.log for more info.
```

Por ejemplo, el 30% de los asistentes son participantes masculinos que tienen más de 17 años. Según la esquema, ¿cuál es la probabilidad de que al escoger una persona al azar tenga que tienen más de 17 años, si ya se sabe que es estudiantes femeninas?

Answerlist
----------
- 0.4/0.6
- 0.4/0.4
- 0.6/0.4
- 0.4/1.0

Solution
========

Para resolver este problema de probabilidad condicional, necesitamos aplicar la fórmula de probabilidad condicional y trabajar con la información de la tabla de contingencia.

### Paso 1: Identificar el tipo de problema
Este es un problema de **probabilidad condicional**, donde buscamos:
$$P(\\text{que tienen más de 17 años} | \\text{estudiantes femeninas})$$

### Paso 2: Recordar la fórmula de probabilidad condicional
La probabilidad condicional se calcula como:
$$P(A|B) = \\frac{P(A \\cap B)}{P(B)}$$

Donde:
- $A$ = evento de interés (que tienen más de 17 años)
- $B$ = condición dada (estudiantes femeninas)
- $P(A \\cap B)$ = probabilidad de que ocurran ambos eventos
- $P(B)$ = probabilidad de la condición

### Paso 3: Extraer información de la tabla
De la tabla de contingencia podemos obtener:

**Probabilidades conjuntas:**
- P(menores de 17 años ∩ participantes masculinos) = 0.1
- P(menores de 17 años ∩ estudiantes femeninas) = 0.2
- P(que tienen más de 17 años ∩ participantes masculinos) = 0.3
- P(que tienen más de 17 años ∩ estudiantes femeninas) = 0.4

**Probabilidades marginales:**
- P(participantes masculinos) = 0.1 + 0.3 = 0.4
- P(estudiantes femeninas) = 0.2 + 0.4 = 0.6
- P(menores de 17 años) = 0.1 + 0.2 = 0.3
- P(que tienen más de 17 años) = 0.3 + 0.4 = 0.7

### Paso 4: Aplicar la fórmula
Para nuestro problema específico:
$$P(\\text{que tienen más de 17 años} | \\text{estudiantes femeninas}) = \\frac{P(\\text{que tienen más de 17 años} \\cap \\text{estudiantes femeninas})}{P(\\text{estudiantes femeninas})}$$

Sustituyendo los valores:
$$P(\\text{que tienen más de 17 años} | \\text{estudiantes femeninas}) = \\frac{0.4}{0.6}$$

### Paso 5: Verificación
Podemos verificar que este resultado tiene sentido:
- El numerador (0.4) representa la probabilidad conjunta del evento y la condición
- El denominador (0.6) representa la probabilidad marginal de la condición
- El resultado 0.4/0.6 = 0.667 está entre 0 y 1, como debe ser toda probabilidad

### Análisis de distractores comunes:
- **0.6/0.4**: Error de invertir numerador y denominador
- **0.4/1.0**: Error de usar solo la probabilidad conjunta sin dividir por la probabilidad de la condición
- **0.4/0.4**: Error de usar el complemento de la probabilidad de la condición

### Conclusión
Por lo tanto, la probabilidad de que una persona tenga que tienen más de 17 años, dado que es estudiantes femeninas, es **0.4/0.6**.

Answerlist
----------
- Verdadero
- Falso
- Falso
- Falso

Meta-information
================
exname: probabilidad_condicional_tabla_contingencia
extype: schoice
exsolution: 1000
exshuffle: TRUE
exsection: Probabilidad|Probabilidad condicional|Tablas de contingencia
exextra[Type]: Cálculo
exextra[Program]: R
exextra[Language]: es
exextra[Level]: 3
