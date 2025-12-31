# Walkthrough: Ejercicio Dispersión y Alcance de Proyectil

Guía detallada del código R/Markdown para el ejercicio ICFES.

---

## Estructura del Archivo .Rmd

```
1. YAML Header (Líneas 1-17)
2. Chunk: setup (Líneas 19-51)
3. Chunk: data_generation (Líneas 53-105)
4. Chunk: version_diversity_test (Líneas 107-152)
5. Chunk: generar_tikz_scatter (Líneas 154-196)
6. Chunk: generar_opciones (Líneas 198-269)
7. Question (Líneas 271-292)
8. Solution (Líneas 294-325)
9. Meta-information (Líneas 327-333)
```

---

## 1. YAML Header (Configuración)

```yaml
---
output:
  html_document: default
  pdf_document:
    keep_tex: true
    extra_dependencies: ["graphicx", "float"]
  word_document: default
icfes:
  competencia: interpretacion_representacion
  nivel_dificultad: 2
  contenido:
    categoria: estadistica
    tipo: no_generico
  contexto: cientifico
  eje_axial: aplicado
  componente: aleatorio
---
```

### Explicación:
- **output**: Define formatos de salida (HTML, PDF, DOCX)
- **keep_tex: true**: Conserva archivo .tex intermedio (útil para debug)
- **extra_dependencies**: Paquetes LaTeX adicionales para gráficos
- **icfes**: Metadatos personalizados para clasificación ICFES

---

## 2. Chunk: setup

```r
```{r setup, include=FALSE}
# Configuración para todos los formatos de salida
Sys.setlocale(category = "LC_NUMERIC", locale = "C")
options(OutDec = ".")
```

### Propósito:
- **include=FALSE**: No muestra este chunk en el output
- **Sys.setlocale**: Asegura formato numérico consistente (punto decimal)
- **OutDec = "."**: Fuerza punto como separador decimal

```r
# Configurar el motor LaTeX globalmente para TikZ
options(tikzLatex = "pdflatex")
options(tikzXelatex = FALSE)
options(tikzLatexPackages = c(
  "\\usepackage{tikz}",
  "\\usepackage{pgfplots}",
  "\\pgfplotsset{compat=1.18}",
  "\\usepackage{amsmath}"
))
```

### Propósito:
- Configura TikZ para usar pdflatex (no XeLaTeX)
- Carga paquetes necesarios: tikz, pgfplots, amsmath
- **pgfplotsset{compat=1.18}**: Usa versión moderna de pgfplots

```r
library(exams)
library(digest)
library(testthat)
library(knitr)

typ <- match_exams_device()
```

### Propósito:
- **exams**: Framework principal para generar exámenes
- **digest**: Genera hashes para verificar unicidad de versiones
- **testthat**: Framework de testing para validar diversidad
- **match_exams_device()**: Detecta formato de salida (html/pdf/etc)

```r
set.seed(sample(1:100000, 1))
```

### Propósito:
- Establece semilla aleatoria DIFERENTE en cada renderizado
- Garantiza que cada versión sea única

---

## 3. Chunk: data_generation

```r
generar_datos <- function() {
  g <- 9.8  # Gravedad (constante)

  # Velocidad inicial aleatoria
  v0 <- sample(seq(10.5, 12.0, 0.1), 1)

  # Número de lanzamientos aleatorio
  n_lanzamientos <- sample(90:110, 1)
```

### Variables Aleatorias:
| Variable | Rango | Propósito |
|----------|-------|-----------|
| `v0` | 10.5-12.0 | Afecta alcance máximo |
| `n_lanzamientos` | 90-110 | Cantidad de puntos en gráfica |

```r
  # Generar ángulos con distribución por zonas
  angulos <- c(
    runif(round(n_lanzamientos * 0.15), 0.05, 0.25),  # 15% zona baja
    runif(round(n_lanzamientos * 0.25), 0.25, 0.55),  # 25% zona media-baja
    runif(round(n_lanzamientos * 0.30), 0.55, 1.0),   # 30% zona central
    runif(round(n_lanzamientos * 0.20), 1.0, 1.35),   # 20% zona media-alta
    runif(round(n_lanzamientos * 0.10), 1.35, 1.55)   # 10% zona alta
  )
```

### Distribución de Ángulos:
```
     |
 30% |        ****
 25% |     ***
 20% |            ***
 15% | **
 10% |               **
     +-------------------> Ángulo (rad)
       0.1  0.4  0.8  1.2  1.5
```

Mayor concentración en zona central (0.55-1.0 rad) donde el alcance es máximo.

```r
  # Calcular alcance teórico
  alcance_teorico <- (v0^2 * sin(2 * angulos)) / g
```

### Fórmula Física:
```
R = v0^2 * sin(2*theta) / g
```
- Alcance máximo cuando theta = 45 grados (pi/4 rad)
- Forma parabólica/senoidal

```r
  # Agregar ruido proporcional al alcance
  ruido_base <- sample(seq(0.35, 0.45, 0.02), 1)
  ruido_factor <- ruido_base * sqrt(pmax(0.3, alcance_teorico / max(alcance_teorico))) * sqrt(alcance_teorico)
  ruido <- rnorm(length(angulos), 0, ruido_factor)
```

### Modelo de Ruido:
- **ruido_base**: Factor aleatorio (0.35-0.45)
- **ruido_factor**: Proporcional a sqrt(alcance)
- Resultado: Mayor dispersión en alcances altos (centro de la parábola)

```
Dispersión:  BAJA  |  ALTA  |  BAJA
             ______|________|______
Alcance:     bajo  | máximo | bajo
Ángulo:      0     |  0.78  | 1.57
```

---

## 4. Chunk: version_diversity_test

```r
test_that("Prueba de diversidad de versiones (datos + texto)", {
  versiones <- list()
  for(i in 1:500) {
    datos_test <- generar_datos()
    texto_test <- list(
      lineal = sample(test_vars_lineal, 1),
      no_lineal = sample(test_vars_no_lineal, 1),
      disperso = sample(test_vars_disperso, 1),
      angulo = sample(test_vars_angulo, 1),
      alcance = sample(test_vars_alcance, 1)
    )
    versiones[[i]] <- digest::digest(list(
      n = datos_test$n_lanzamientos,
      v0 = datos_test$v0,
      ruido = datos_test$ruido_base,
      texto = texto_test
    ))
  }
  n_versiones_unicas <- length(unique(versiones))
  expect_true(n_versiones_unicas >= 300)
})
```

### Propósito:
- Genera 500 versiones de prueba
- Crea hash único por combinación (datos + texto)
- Verifica que al menos 300 sean diferentes
- Si falla, el ejercicio no pasa validación

### Combinaciones Teóricas:
```
Datos:  16 (v0) * 21 (n_lanz) * 6 (ruido) = 2,016
Texto:  4^5 = 1,024
Total:  2,016 * 1,024 = 2,064,384
```

---

## 5. Chunk: generar_tikz_scatter

```r
generar_tikz_dispersion <- function(angulos, alcances) {
  # Generar coordenadas TikZ
  coords <- paste0("    (", sprintf("%.3f", angulos), ", ", sprintf("%.1f", alcances), ")")
  coords_str <- paste(coords, collapse = "\n")
```

### Formato de Coordenadas:
```
    (0.123, 5.4)
    (0.456, 8.2)
    (0.789, 11.7)
    ...
```

```r
  tikz_code <- paste0(
    "\\begin{tikzpicture}\n",
    "\\begin{axis}[\n",
    "    width=12cm,\n",
    "    height=8cm,\n",
    "    xlabel={Ángulo (en radianes)},\n",
    "    ylabel={Alcance horizontal (m)},\n",
    ...
```

### Estructura TikZ/pgfplots:
```latex
\begin{tikzpicture}
\begin{axis}[
    width=12cm,
    height=8cm,
    xlabel={Ángulo (en radianes)},
    ylabel={Alcance horizontal (m)},
    xmin=0, xmax=1.7,
    ymin=0, ymax=15,
    grid=major,
]

\addplot[
    only marks,
    mark=diamond*,
    mark size=2pt,
    color=cyan,
] coordinates {
    (0.123, 5.4)
    (0.456, 8.2)
    ...
};

\end{axis}
\end{tikzpicture}
```

### Resultado Visual:
```
Alcance (m)
    14 |
    12 |        * * *
    10 |      * * * * *
     8 |    * * * * * * *
     6 |  * * *       * * *
     4 | * *             * *
     2 |*                   *
     0 +-----------------------> Ángulo (rad)
       0   0.4   0.8   1.2   1.6
```

---

## 6. Chunk: generar_opciones

```r
# Variantes para cada término
vars_lineal <- c("lineal", "proporcional", "de tipo lineal", "directamente proporcional")
vars_no_lineal <- c("no lineal", "no proporcional", "de tipo no lineal", "parabólico")
vars_disperso <- c("más disperso", "con mayor variabilidad", "más variable", "con mayor dispersión")
vars_angulo <- c("el ángulo", "el ángulo de lanzamiento", "la inclinación inicial", "el ángulo inicial")
vars_alcance <- c("el alcance", "el alcance horizontal", "la distancia recorrida", "el alcance del proyectil")
```

### Sistema de Variación:
Cada término tiene 4 sinónimos que se seleccionan aleatoriamente.

```r
# Seleccionar variantes aleatorias
sel_lineal <- sample(vars_lineal, 1)
sel_no_lineal <- sample(vars_no_lineal, 1)
sel_disperso <- sample(vars_disperso, 1)
sel_angulo <- sample(vars_angulo, 1)
sel_alcance <- sample(vars_alcance, 1)
```

```r
# Construir opciones dinámicas
opciones <- c(
  paste0(sel_lineal, " y ", sel_disperso, " cuanto mayor sea ", sel_angulo, "."),
  paste0(sel_lineal, " y ", sel_disperso, " cuanto mayor sea ", sel_alcance, "."),
  paste0(sel_no_lineal, " y ", sel_disperso, " cuanto mayor sea ", sel_angulo, "."),
  paste0(sel_no_lineal, " y ", sel_disperso, " cuanto mayor sea ", sel_alcance, ".")  # CORRECTA
)
```

### Estructura de Opciones:

| Opción | Relación | Dispersión | Correcta |
|--------|----------|------------|----------|
| A | lineal | ángulo | NO |
| B | lineal | alcance | NO |
| C | no lineal | ángulo | NO |
| D | no lineal | alcance | SÍ |

### Ejemplo de Variación:

**Versión 1:**
```
A. lineal y más disperso cuanto mayor sea el ángulo.
B. lineal y más disperso cuanto mayor sea el alcance.
C. no lineal y más disperso cuanto mayor sea el ángulo.
D. no lineal y más disperso cuanto mayor sea el alcance.
```

**Versión 2:**
```
A. proporcional y con mayor variabilidad cuanto mayor sea la inclinación inicial.
B. proporcional y con mayor variabilidad cuanto mayor sea la distancia recorrida.
C. parabólico y con mayor variabilidad cuanto mayor sea la inclinación inicial.
D. parabólico y con mayor variabilidad cuanto mayor sea la distancia recorrida.
```

---

## 7. Question (Enunciado)

```markdown
Question
========

Un experimento consiste en medir el alcance horizontal de un proyectil
en función del ángulo con el que se lanza (respecto a la horizontal).
En la gráfica se registran los resultados de `r n_lanzamientos`
lanzamientos realizados con la misma velocidad inicial.

[GRÁFICA TikZ]

El comportamiento del alcance respecto al ángulo es

Answerlist
----------
* `r opciones[1]`
* `r opciones[2]`
* `r opciones[3]`
* `r opciones[4]`
```

### Elementos Dinámicos:
- **n_lanzamientos**: Número aleatorio (90-110)
- **Gráfica**: Generada con datos aleatorios
- **Opciones**: Texto con variación aleatoria

---

## 8. Solution (Retroalimentación)

```markdown
Solution
========

**1. Tipo de relación (`r sel_lineal` vs `r sel_no_lineal`):**

La relación sigue la ecuación: R = v0^2 * sin(2*theta) / g

Esta es una función senoidal, por lo tanto **`r sel_no_lineal`**.

**2. Patrón de dispersión:**

- Extremos: puntos más concentrados
- Centro: mayor variabilidad

La dispersión es proporcional a `r sel_alcance`, no a `r sel_angulo`.

Answerlist
----------
* **Falso**. La relación no es `r sel_lineal`...
* **Falso**. Aunque la dispersión sí aumenta con `r sel_alcance`...
* **Falso**. Si bien la relación es `r sel_no_lineal`...
* **Verdadero**. La relación es `r sel_no_lineal`...
```

### Coherencia:
La solución usa las MISMAS variables seleccionadas que las opciones, garantizando coherencia textual.

---

## 9. Meta-information

```yaml
Meta-information
================
exname: dispersion_alcance_proyectil_aleatorio_interpretacion_representacion_n2_v1
extype: schoice
exsolution: `r paste(solucion, collapse="")`
exshuffle: TRUE
exsection: Estadística/Gráficas de Dispersión
```

### Campos:
| Campo | Valor | Descripción |
|-------|-------|-------------|
| exname | (nombre) | Identificador único |
| extype | schoice | Selección única |
| exsolution | 0001 | Posición de respuesta correcta (D) |
| exshuffle | TRUE | Mezclar opciones aleatoriamente |
| exsection | (sección) | Categoría para organización |

### exshuffle: TRUE
**CRÍTICO**: Garantiza que R-exams mezcle las opciones en cada versión, evitando que "D siempre sea correcta".

---

## Diagrama de Flujo del Código

```
+-------------------+
|   YAML Header     |
| (configuración)   |
+--------+----------+
         |
         v
+-------------------+
|   setup chunk     |
| - locale          |
| - TikZ config     |
| - librerías       |
| - semilla random  |
+--------+----------+
         |
         v
+-------------------+
| data_generation   |
| - v0 aleatorio    |
| - n_lanzamientos  |
| - ángulos         |
| - alcances        |
| - ruido           |
+--------+----------+
         |
         v
+-------------------+
| diversity_test    |
| - 500 iteraciones |
| - verificar >=300 |
+--------+----------+
         |
         v
+-------------------+
| generar_tikz      |
| - coordenadas     |
| - código pgfplots |
+--------+----------+
         |
         v
+-------------------+
| generar_opciones  |
| - vars_lineal     |
| - vars_no_lineal  |
| - vars_disperso   |
| - vars_angulo     |
| - vars_alcance    |
| - opciones[]      |
+--------+----------+
         |
         v
+-------------------+
|    Question       |
| - enunciado       |
| - gráfica TikZ    |
| - answerlist      |
+--------+----------+
         |
         v
+-------------------+
|    Solution       |
| - explicación     |
| - feedback        |
+--------+----------+
         |
         v
+-------------------+
| Meta-information  |
| - extype: schoice |
| - exsolution      |
| - exshuffle: TRUE |
+-------------------+
```

---

## Comandos de Renderizado

```r
# Una versión HTML
exams2html("*_opc_D.Rmd", n = 1)

# 10 versiones PDF
exams2pdf("*_opc_D.Rmd", n = 10)

# 100 versiones Moodle
exams2moodle("*_opc_D.Rmd", n = 100, name = "banco_dispersion")

# 30 exámenes NOPS (impresos)
exams2nops("*_opc_D.Rmd", n = 30, institution = "ICFES")
```

---

## Troubleshooting

### Error: "TikZ compilation failed"
```r
# Verificar instalación de pdflatex
Sys.which("pdflatex")

# Verificar paquetes LaTeX
system("kpsewhich pgfplots.sty")
```

### Error: "Less than 300 unique versions"
- Aumentar rangos de variables aleatorias
- Agregar más variantes textuales
- Verificar que set.seed() use semilla aleatoria

### Error: "exshuffle not working"
- Verificar que `exshuffle: TRUE` esté en Meta-information
- No usar `exshuffle: FALSE`

---

## Autor

Documentación generada con Claude Code (Graficador Experto ICFES)

Fecha: 2025-12-30
