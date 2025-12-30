# Walkthrough: Ejercicio Dispersion y Alcance de Proyectil

Guia detallada del codigo R/Markdown para el ejercicio ICFES.

---

## Estructura del Archivo .Rmd

```
1. YAML Header (Lineas 1-17)
2. Chunk: setup (Lineas 19-51)
3. Chunk: data_generation (Lineas 53-105)
4. Chunk: version_diversity_test (Lineas 107-152)
5. Chunk: generar_tikz_scatter (Lineas 154-196)
6. Chunk: generar_opciones (Lineas 198-269)
7. Question (Lineas 271-292)
8. Solution (Lineas 294-325)
9. Meta-information (Lineas 327-333)
```

---

## 1. YAML Header (Configuracion)

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

### Explicacion:
- **output**: Define formatos de salida (HTML, PDF, DOCX)
- **keep_tex: true**: Conserva archivo .tex intermedio (util para debug)
- **extra_dependencies**: Paquetes LaTeX adicionales para graficos
- **icfes**: Metadatos personalizados para clasificacion ICFES

---

## 2. Chunk: setup

```r
```{r setup, include=FALSE}
# Configuracion para todos los formatos de salida
Sys.setlocale(category = "LC_NUMERIC", locale = "C")
options(OutDec = ".")
```

### Proposito:
- **include=FALSE**: No muestra este chunk en el output
- **Sys.setlocale**: Asegura formato numerico consistente (punto decimal)
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

### Proposito:
- Configura TikZ para usar pdflatex (no XeLaTeX)
- Carga paquetes necesarios: tikz, pgfplots, amsmath
- **pgfplotsset{compat=1.18}**: Usa version moderna de pgfplots

```r
library(exams)
library(digest)
library(testthat)
library(knitr)

typ <- match_exams_device()
```

### Proposito:
- **exams**: Framework principal para generar examenes
- **digest**: Genera hashes para verificar unicidad de versiones
- **testthat**: Framework de testing para validar diversidad
- **match_exams_device()**: Detecta formato de salida (html/pdf/etc)

```r
set.seed(sample(1:100000, 1))
```

### Proposito:
- Establece semilla aleatoria DIFERENTE en cada renderizado
- Garantiza que cada version sea unica

---

## 3. Chunk: data_generation

```r
generar_datos <- function() {
  g <- 9.8  # Gravedad (constante)

  # Velocidad inicial aleatoria
  v0 <- sample(seq(10.5, 12.0, 0.1), 1)

  # Numero de lanzamientos aleatorio
  n_lanzamientos <- sample(90:110, 1)
```

### Variables Aleatorias:
| Variable | Rango | Proposito |
|----------|-------|-----------|
| `v0` | 10.5-12.0 | Afecta alcance maximo |
| `n_lanzamientos` | 90-110 | Cantidad de puntos en grafica |

```r
  # Generar angulos con distribucion por zonas
  angulos <- c(
    runif(round(n_lanzamientos * 0.15), 0.05, 0.25),  # 15% zona baja
    runif(round(n_lanzamientos * 0.25), 0.25, 0.55),  # 25% zona media-baja
    runif(round(n_lanzamientos * 0.30), 0.55, 1.0),   # 30% zona central
    runif(round(n_lanzamientos * 0.20), 1.0, 1.35),   # 20% zona media-alta
    runif(round(n_lanzamientos * 0.10), 1.35, 1.55)   # 10% zona alta
  )
```

### Distribucion de Angulos:
```
     |
 30% |        ****
 25% |     ***
 20% |            ***
 15% | **
 10% |               **
     +-------------------> Angulo (rad)
       0.1  0.4  0.8  1.2  1.5
```

Mayor concentracion en zona central (0.55-1.0 rad) donde el alcance es maximo.

```r
  # Calcular alcance teorico
  alcance_teorico <- (v0^2 * sin(2 * angulos)) / g
```

### Formula Fisica:
```
R = v0^2 * sin(2*theta) / g
```
- Alcance maximo cuando theta = 45 grados (pi/4 rad)
- Forma parabolica/senoidal

```r
  # Agregar ruido proporcional al alcance
  ruido_base <- sample(seq(0.35, 0.45, 0.02), 1)
  ruido_factor <- ruido_base * sqrt(pmax(0.3, alcance_teorico / max(alcance_teorico))) * sqrt(alcance_teorico)
  ruido <- rnorm(length(angulos), 0, ruido_factor)
```

### Modelo de Ruido:
- **ruido_base**: Factor aleatorio (0.35-0.45)
- **ruido_factor**: Proporcional a sqrt(alcance)
- Resultado: Mayor dispersion en alcances altos (centro de la parabola)

```
Dispersion:  BAJA  |  ALTA  |  BAJA
             ______|________|______
Alcance:     bajo  | maximo | bajo
Angulo:      0     |  0.78  | 1.57
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

### Proposito:
- Genera 500 versiones de prueba
- Crea hash unico por combinacion (datos + texto)
- Verifica que al menos 300 sean diferentes
- Si falla, el ejercicio no pasa validacion

### Combinaciones Teoricas:
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
    "    xlabel={Angulo (en radianes)},\n",
    "    ylabel={Alcance horizontal (m)},\n",
    ...
```

### Estructura TikZ/pgfplots:
```latex
\begin{tikzpicture}
\begin{axis}[
    width=12cm,
    height=8cm,
    xlabel={Angulo (en radianes)},
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
     0 +-----------------------> Angulo (rad)
       0   0.4   0.8   1.2   1.6
```

---

## 6. Chunk: generar_opciones

```r
# Variantes para cada termino
vars_lineal <- c("lineal", "proporcional", "de tipo lineal", "directamente proporcional")
vars_no_lineal <- c("no lineal", "no proporcional", "de tipo no lineal", "parabolico")
vars_disperso <- c("mas disperso", "con mayor variabilidad", "mas variable", "con mayor dispersion")
vars_angulo <- c("el angulo", "el angulo de lanzamiento", "la inclinacion inicial", "el angulo inicial")
vars_alcance <- c("el alcance", "el alcance horizontal", "la distancia recorrida", "el alcance del proyectil")
```

### Sistema de Variacion:
Cada termino tiene 4 sinonimos que se seleccionan aleatoriamente.

```r
# Seleccionar variantes aleatorias
sel_lineal <- sample(vars_lineal, 1)
sel_no_lineal <- sample(vars_no_lineal, 1)
sel_disperso <- sample(vars_disperso, 1)
sel_angulo <- sample(vars_angulo, 1)
sel_alcance <- sample(vars_alcance, 1)
```

```r
# Construir opciones dinamicas
opciones <- c(
  paste0(sel_lineal, " y ", sel_disperso, " cuanto mayor sea ", sel_angulo, "."),
  paste0(sel_lineal, " y ", sel_disperso, " cuanto mayor sea ", sel_alcance, "."),
  paste0(sel_no_lineal, " y ", sel_disperso, " cuanto mayor sea ", sel_angulo, "."),
  paste0(sel_no_lineal, " y ", sel_disperso, " cuanto mayor sea ", sel_alcance, ".")  # CORRECTA
)
```

### Estructura de Opciones:

| Opcion | Relacion | Dispersion | Correcta |
|--------|----------|------------|----------|
| A | lineal | angulo | NO |
| B | lineal | alcance | NO |
| C | no lineal | angulo | NO |
| D | no lineal | alcance | SI |

### Ejemplo de Variacion:

**Version 1:**
```
A. lineal y mas disperso cuanto mayor sea el angulo.
B. lineal y mas disperso cuanto mayor sea el alcance.
C. no lineal y mas disperso cuanto mayor sea el angulo.
D. no lineal y mas disperso cuanto mayor sea el alcance.
```

**Version 2:**
```
A. proporcional y con mayor variabilidad cuanto mayor sea la inclinacion inicial.
B. proporcional y con mayor variabilidad cuanto mayor sea la distancia recorrida.
C. parabolico y con mayor variabilidad cuanto mayor sea la inclinacion inicial.
D. parabolico y con mayor variabilidad cuanto mayor sea la distancia recorrida.
```

---

## 7. Question (Enunciado)

```markdown
Question
========

Un experimento consiste en medir el alcance horizontal de un proyectil
en funcion del angulo con el que se lanza (respecto a la horizontal).
En la grafica se registran los resultados de `r n_lanzamientos`
lanzamientos realizados con la misma velocidad inicial.

[GRAFICA TikZ]

El comportamiento del alcance respecto al angulo es

Answerlist
----------
* `r opciones[1]`
* `r opciones[2]`
* `r opciones[3]`
* `r opciones[4]`
```

### Elementos Dinamicos:
- **n_lanzamientos**: Numero aleatorio (90-110)
- **Grafica**: Generada con datos aleatorios
- **Opciones**: Texto con variacion aleatoria

---

## 8. Solution (Retroalimentacion)

```markdown
Solution
========

**1. Tipo de relacion (`r sel_lineal` vs `r sel_no_lineal`):**

La relacion sigue la ecuacion: R = v0^2 * sin(2*theta) / g

Esta es una funcion senoidal, por lo tanto **`r sel_no_lineal`**.

**2. Patron de dispersion:**

- Extremos: puntos mas concentrados
- Centro: mayor variabilidad

La dispersion es proporcional a `r sel_alcance`, no a `r sel_angulo`.

Answerlist
----------
* **Falso**. La relacion no es `r sel_lineal`...
* **Falso**. Aunque la dispersion si aumenta con `r sel_alcance`...
* **Falso**. Si bien la relacion es `r sel_no_lineal`...
* **Verdadero**. La relacion es `r sel_no_lineal`...
```

### Coherencia:
La solucion usa las MISMAS variables seleccionadas que las opciones, garantizando coherencia textual.

---

## 9. Meta-information

```yaml
Meta-information
================
exname: dispersion_alcance_proyectil_aleatorio_interpretacion_representacion_n2_v1
extype: schoice
exsolution: `r paste(solucion, collapse="")`
exshuffle: TRUE
exsection: Estadistica/Graficas de Dispersion
```

### Campos:
| Campo | Valor | Descripcion |
|-------|-------|-------------|
| exname | (nombre) | Identificador unico |
| extype | schoice | Seleccion unica |
| exsolution | 0001 | Posicion de respuesta correcta (D) |
| exshuffle | TRUE | Mezclar opciones aleatoriamente |
| exsection | (seccion) | Categoria para organizacion |

### exshuffle: TRUE
**CRITICO**: Garantiza que R-exams mezcle las opciones en cada version, evitando que "D siempre sea correcta".

---

## Diagrama de Flujo del Codigo

```
+-------------------+
|   YAML Header     |
| (configuracion)   |
+--------+----------+
         |
         v
+-------------------+
|   setup chunk     |
| - locale          |
| - TikZ config     |
| - librerias       |
| - semilla random  |
+--------+----------+
         |
         v
+-------------------+
| data_generation   |
| - v0 aleatorio    |
| - n_lanzamientos  |
| - angulos         |
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
| - codigo pgfplots |
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
| - grafica TikZ    |
| - answerlist      |
+--------+----------+
         |
         v
+-------------------+
|    Solution       |
| - explicacion     |
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
# Una version HTML
exams2html("*_opc_D.Rmd", n = 1)

# 10 versiones PDF
exams2pdf("*_opc_D.Rmd", n = 10)

# 100 versiones Moodle
exams2moodle("*_opc_D.Rmd", n = 100, name = "banco_dispersion")

# 30 examenes NOPS (impresos)
exams2nops("*_opc_D.Rmd", n = 30, institution = "ICFES")
```

---

## Troubleshooting

### Error: "TikZ compilation failed"
```r
# Verificar instalacion de pdflatex
Sys.which("pdflatex")

# Verificar paquetes LaTeX
system("kpsewhich pgfplots.sty")
```

### Error: "Less than 300 unique versions"
- Aumentar rangos de variables aleatorias
- Agregar mas variantes textuales
- Verificar que set.seed() use semilla aleatoria

### Error: "exshuffle not working"
- Verificar que `exshuffle: TRUE` este en Meta-information
- No usar `exshuffle: FALSE`

---

## Autor

Documentacion generada con Claude Code (Graficador Experto ICFES)

Fecha: 2025-12-30
