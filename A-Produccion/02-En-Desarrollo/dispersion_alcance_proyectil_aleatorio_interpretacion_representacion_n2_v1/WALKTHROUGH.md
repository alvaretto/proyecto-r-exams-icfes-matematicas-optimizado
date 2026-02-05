# Walkthrough: Ejercicio Dispersión y Tipo de Relación

Guía detallada del código R/Markdown para los 4 archivos de ejercicios ICFES.

---

## Visión General de las 4 Variantes

| Archivo | Modelo Físico | Fórmula | Dispersión |
|---------|---------------|---------|------------|
| opc_A | Ley de Hooke | `x = F/k` | `σ ∝ F` |
| opc_B | Ley de Hooke | `x = F/k` | `σ ∝ √x` |
| opc_C | Proyectil | `R = v₀²sin(2θ)/g` | `σ ∝ θ` |
| opc_D | Proyectil | `R = v₀²sin(2θ)/g` | `σ ∝ √R` |

---

## Estructura Común del Archivo .Rmd

```
1. YAML Header (Líneas 1-17)
2. Chunk: setup (Líneas 19-51)
3. Chunk: data_generation (Líneas 53-101)
4. Chunk: version_diversity_test (Líneas 103-148)
5. Chunk: generar_tikz_scatter (Líneas 150-193)
6. Chunk: generar_opciones (Líneas 195-265)
7. Question (Líneas 267-289)
8. Solution (Líneas 291-322)
9. Meta-information (Líneas 324-330)
```

---

## 1. YAML Header (Común a todos)

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
  componente: aleatorio
  contexto: cientifico
---
```

---

## 2. Chunk: setup (Común a todos)

```r
# Configuración de locale y TikZ
Sys.setlocale(category = "LC_NUMERIC", locale = "C")
options(OutDec = ".")
options(tikzLatex = "pdflatex")

# Librerías
library(exams)
library(digest)
library(testthat)
library(knitr)

# Semilla aleatoria diferente cada vez
set.seed(sample(1:100000, 1))
```

---

## 3. Chunk: data_generation (DIFERENTE por variante)

### opc_A y opc_B: Ley de Hooke (Lineal)

```r
generar_datos <- function() {
  # Constante del resorte aleatoria
  k <- sample(seq(0.8, 1.2, 0.05), 1)
  n_mediciones <- sample(90:110, 1)

  # Fuerzas uniformemente distribuidas
  fuerzas <- c(
    runif(round(n_mediciones * 0.20), 0.5, 3),
    runif(round(n_mediciones * 0.25), 3, 6),
    runif(round(n_mediciones * 0.30), 6, 9),
    runif(round(n_mediciones * 0.25), 9, 12)
  )

  # Modelo LINEAL: x = F/k
  elongacion_teorica <- fuerzas / k

  # Diferencia entre A y B: modelo de ruido
  # ...
}
```

**Diferencia clave A vs B:**

| Archivo | Modelo de Ruido | Código |
|---------|-----------------|--------|
| opc_A | Proporcional a FUERZA | `ruido_factor <- ruido_base * fuerzas` |
| opc_B | Proporcional a ELONGACIÓN | `ruido_factor <- ruido_base * sqrt(elongacion_teorica)` |

### opc_C y opc_D: Proyectil (No Lineal)

```r
generar_datos <- function() {
  g <- 9.8
  v0 <- sample(seq(10.5, 12.0, 0.1), 1)
  n_lanzamientos <- sample(90:110, 1)

  # Ángulos con distribución por zonas
  angulos <- c(
    runif(round(n_lanzamientos * 0.15), 0.05, 0.25),
    runif(round(n_lanzamientos * 0.25), 0.25, 0.55),
    runif(round(n_lanzamientos * 0.30), 0.55, 1.0),
    runif(round(n_lanzamientos * 0.20), 1.0, 1.35),
    runif(round(n_lanzamientos * 0.10), 1.35, 1.55)
  )

  # Modelo NO LINEAL: R = v0^2 * sin(2*theta) / g
  alcance_teorico <- (v0^2 * sin(2 * angulos)) / g

  # Diferencia entre C y D: modelo de ruido
  # ...
}
```

**Diferencia clave C vs D:**

| Archivo | Modelo de Ruido | Código |
|---------|-----------------|--------|
| opc_C | Proporcional a ÁNGULO | `ruido_factor <- ruido_base * angulos * 2.5` |
| opc_D | Proporcional a ALCANCE | `ruido_factor <- ruido_base * sqrt(alcance_teorico)` |

---

## 4. Visualización del Patrón de Dispersión

### opc_A: Lineal + dispersión crece con X (fuerza)

```
Elongación (cm)
    14 |                    * *
    12 |                  * * *
    10 |               * * *
     8 |            * * *
     6 |         * * *
     4 |      * *
     2 |   * *
     0 +-------------------------> Fuerza (N)
       0    2    4    6    8   10   12

       [concentrado]  →  [disperso]
```

### opc_B: Lineal + dispersión crece con Y (elongación)

```
Elongación (cm)
    14 |                    *   *   disperso
    12 |                  * * *     ↑
    10 |               * * *
     8 |            * *
     6 |         * *
     4 |      **
     2 |   **                       ↓
     0 +-------------------------> Fuerza (N)
       0    2    4    6    8   10   12
                                   concentrado
```

### opc_C: No lineal + dispersión crece con X (ángulo)

```
Alcance (m)
    14 |
    12 |        * * *
    10 |      * * * * *      *  *
     8 |    * * * * * * *   *  *  *
     6 |  * * *       * * * *
     4 | * *             * * *
     2 |*                    *  *
     0 +-------------------------> Ángulo (rad)
       0   0.4   0.8   1.2   1.6

       [concentrado]  →  [disperso]
```

### opc_D: No lineal + dispersión crece con Y (alcance)

```
Alcance (m)
    14 |
    12 |        * * *        disperso (alcance alto)
    10 |      * * * * *      ↑
     8 |    * * * * * * *
     6 |  * *         * *    ↓
     4 | *               *   concentrado (alcance bajo)
     2 |*                 *
     0 +-------------------------> Ángulo (rad)
       0   0.4   0.8   1.2   1.6
```

---

## 5. Chunk: generar_opciones

### Estructura común (4 opciones)

```r
opciones <- c(
  paste0(sel_lineal,    " y ", sel_disperso, " cuanto mayor sea ", sel_var_indep, "."),  # A
  paste0(sel_lineal,    " y ", sel_disperso, " cuanto mayor sea ", sel_var_dep, "."),    # B
  paste0(sel_no_lineal, " y ", sel_disperso, " cuanto mayor sea ", sel_var_indep, "."),  # C
  paste0(sel_no_lineal, " y ", sel_disperso, " cuanto mayor sea ", sel_var_dep, ".")     # D
)
```

### Vector de solución por archivo

| Archivo | Solución | Significado |
|---------|----------|-------------|
| opc_A | `c(1,0,0,0)` | Lineal + var independiente |
| opc_B | `c(0,1,0,0)` | Lineal + var dependiente |
| opc_C | `c(0,0,1,0)` | No lineal + var independiente |
| opc_D | `c(0,0,0,1)` | No lineal + var dependiente |

---

## 6. Variantes Textuales

### Archivos A y B (contexto resorte)

| Variable | Opciones |
|----------|----------|
| vars_fuerza | la fuerza, la fuerza aplicada, la carga, la fuerza ejercida |
| vars_elongacion | la elongación, el estiramiento, la deformación, la elongación del resorte |

### Archivos C y D (contexto proyectil)

| Variable | Opciones |
|----------|----------|
| vars_angulo | el ángulo, el ángulo de lanzamiento, la inclinación inicial, el ángulo inicial |
| vars_alcance | el alcance, el alcance horizontal, la distancia recorrida, el alcance del proyectil |

### Comunes a todos

| Variable | Opciones |
|----------|----------|
| vars_lineal | lineal, proporcional, de tipo lineal, directamente proporcional |
| vars_no_lineal | no lineal, no proporcional, parabólico, cuadrático |
| vars_disperso | más disperso, con mayor variabilidad, más variable, con mayor dispersión |

---

## 7. Question y Solution

### Enunciado (Question)

**A/B (resorte):**
> Un experimento consiste en medir la elongación de un resorte en función de la fuerza aplicada...

**C/D (proyectil):**
> Un experimento consiste en medir el alcance horizontal de un proyectil en función del ángulo...

### Retroalimentación (Solution)

Cada archivo incluye:

1. Explicación del tipo de relación (con fórmula)
2. Análisis del patrón de dispersión
3. Conclusión
4. Feedback específico para cada opción

---

## 8. Meta-information

```yaml
exname: [nombre_descriptivo]
extype: schoice
exsolution: `r paste(solucion, collapse="")`
exshuffle: TRUE
```

**CRÍTICO**: `exshuffle: TRUE` garantiza que R-exams mezcle las opciones.

---

## Comandos de Renderizado

```r
library(exams)

# Renderizar variante específica
exams2html("*_opc_A.Rmd", n = 1)
exams2pdf("*_opc_D.Rmd", n = 10)

# Generar banco mezclando variantes
archivos <- list.files(pattern = "_opc_[ABCD]\\.Rmd$")
exams2moodle(sample(archivos, 1), n = 100)
```

---

## Troubleshooting

| Error | Causa | Solución |
|-------|-------|----------|
| TikZ compilation failed | pdflatex no instalado | `sudo pacman -S texlive-bin` |
| Less than 300 unique versions | Poca variabilidad | Aumentar rangos aleatorios |
| exsolution incorrecto | Vector mal definido | Verificar `solucion <- c(...)` |

---

**Última actualización**: 2025-12-30
**Versión**: 2.0 (Consolidado para 4 variantes)
