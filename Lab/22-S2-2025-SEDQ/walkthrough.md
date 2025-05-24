# Walkthrough: Fracciones Reparto Premio v1

Este documento proporciona un análisis detallado del código del archivo 
`fracciones_reparto_premio_v1.Rmd`, explicando cada sección y su funcionalidad.

## Tabla de Contenidos

1. [Metadatos y Configuración](#1-metadatos-y-configuración)
2. [Configuración del Entorno R](#2-configuración-del-entorno-r)
3. [Aleatorización de Variables](#3-aleatorización-de-variables)
4. [Generación de Fracciones](#4-generación-de-fracciones)
5. [Cálculos Matemáticos](#5-cálculos-matemáticos)
6. [Generación de Opciones](#6-generación-de-opciones)
7. [Visualización](#7-visualización)
8. [Estructura del Ejercicio](#8-estructura-del-ejercicio)
9. [Solución Detallada](#9-solución-detallada)
10. [Metainformación](#10-metainformación)

---

## 1. Metadatos y Configuración

```yaml
---
output:
  html_document: default
  pdf_document:
    keep_tex: true
    extra_dependencies: ["graphicx", "float", "tikz", "xcolor"]
  word_document: default
icfes:
  competencia: Resolución de problemas
  componente: Numérico-variacional
  afirmacion: Resuelve problemas que requieren el uso de fracciones y porcentajes
  evidencia: Utiliza fracciones para resolver problemas de reparto proporcional
  nivel: Medio
  tematica: Fracciones y operaciones con fracciones
---
```

**Propósito**: Define los formatos de salida y los metadatos educativos según el marco ICFES.

**Elementos clave**:

- **Múltiples formatos**: HTML, PDF y Word
- **Dependencias LaTeX**: Para gráficos y colores
- **Clasificación ICFES**: Competencia, componente, afirmación, evidencia, nivel y temática

---

## 2. Configuración del Entorno R

```r
```{r setup, include=FALSE}
# Configuración para todos los formatos de salida
Sys.setlocale(category = "LC_NUMERIC", locale = "C")
options(OutDec = ".")

# Configurar el motor LaTeX globalmente
options(tikzLatex = "pdflatex")
options(tikzXelatex = FALSE)
options(tikzLatexPackages = c(
  "\\usepackage{tikz}",
  "\\usepackage{colortbl}",
  "\\usepackage{xcolor}",
  "\\usepackage{graphicx}",
  "\\usepackage{float}"
))

library(exams)
library(reticulate)
library(digest)
library(testthat)
library(knitr)

typ <- match_exams_device()
options(scipen = 999)
knitr::opts_chunk$set(
  warning = FALSE,
  message = FALSE,
  fig.showtext = FALSE,
  fig.cap = "",
  fig.keep = 'all',
  dev = c("png", "pdf"),
  dpi = 150,
  fig.pos = "H"
)

# Configuración para chunks de Python
knitr::knit_engines$set(python = function(options) {
  knitr::engine_output(options, options$code, '')
})

# Asegurar que Python esté correctamente configurado
use_python(Sys.which("python"), required = TRUE)
```

**Propósito**: Establece el entorno de trabajo para generar ejercicios consistentes.

**Elementos clave**:

- **Localización numérica**: Asegura el uso de punto decimal
- **Configuración LaTeX**: Para gráficos TikZ
- **Librerías**: exams, reticulate, digest, testthat, knitr
- **Configuración de figuras**: PNG y PDF con alta resolución
- **Integración Python**: Para generar visualizaciones

---

## 3. Aleatorización de Variables

### 3.1 Sistema de Concordancia de Género para Contextos

```r
# Aleatorización del contexto del problema con concordancia de género
contextos_data <- data.frame(
  nombre = c("ciudad", "localidad", "comunidad", "comarca", "provincia", "zona",
             "municipio", "distrito", "territorio", "sector", "barrio", "pueblo"),
  genero = c("f", "f", "f", "f", "f", "f",
             "m", "m", "m", "m", "m", "m"),
  articulo = c("una", "una", "una", "una", "una", "una",
               "un", "un", "un", "un", "un", "un"),
  stringsAsFactors = FALSE
)
contexto_seleccionado <- contextos_data[sample(nrow(contextos_data), 1), ]
contexto <- contexto_seleccionado$nombre
articulo_contexto <- contexto_seleccionado$articulo
```

**Propósito**: Garantiza concordancia de género entre artículos y sustantivos.

**Innovación**: 

- **Data frame estructurado**: Cada término tiene su género y artículo correspondiente
- **Términos balanceados**: 6 femeninos y 6 masculinos
- **Concordancia automática**: Elimina errores como "un ciudad" o "una municipio"

### 3.2 Sistema de Concordancia para Competencias

```r
# Aleatorización del tipo de competencia con concordancia de género
competencias_data <- data.frame(
  nombre = c("carrera", "competencia atlética", "olimpiada deportiva", 
             "justa deportiva", "prueba atlética", "competencia deportiva",
             "maratón", "torneo deportivo", "evento deportivo", 
             "campeonato", "concurso deportivo", "certamen deportivo"),
  genero = c("f", "f", "f", "f", "f", "f",
             "m", "m", "m", "m", "m", "m"),
  articulo = c("una", "una", "una", "una", "una", "una",
               "un", "un", "un", "un", "un", "un"),
  stringsAsFactors = FALSE
)
competencia_seleccionada <- competencias_data[sample(nrow(competencias_data), 1), ]
competencia <- competencia_seleccionada$nombre
articulo_competencia <- competencia_seleccionada$articulo
```

**Propósito**: Asegura coherencia semántica en tipos de competencia.

**Beneficios**:

- **Variedad temática**: Desde carreras hasta certámenes
- **Coherencia gramatical**: Evita "una maratón" o "un carrera"
- **Escalabilidad**: Fácil agregar nuevos términos

### 3.3 Otros Términos Aleatorios

```r
# Aleatorización del grupo de edad
edades <- c(
  "menores de 15 años", "menores de 16 años", "menores de 14 años",
  "niños y niñas de 10 a 15 años", "jóvenes de 12 a 15 años",
  "estudiantes de primaria y secundaria", "categoría infantil",
  "categoría juvenil", "niños y adolescentes", "estudiantes menores de edad"
)
grupo_edad <- sample(edades, 1)

# Aleatorización del premio total (en millones)
premios_posibles <- c(30, 40, 50, 60, 70, 80, 90, 100, 120, 150)
premio_total <- sample(premios_posibles, 1)
```

**Propósito**: Crear contextos diversos y realistas.

**Características**:

- **Grupos de edad**: Variedad de descripciones para el público objetivo
- **Premios**: Valores en millones, fáciles de calcular y realistas

### 3.4 Términos del Enunciado

```r
# Aleatorización de términos para el enunciado
terminos_premiar <- c("premiar", "recompensar", "reconocer", "galardonar", "incentivar")
termino_premiar <- sample(terminos_premiar, 1)

terminos_participantes <- c("participantes", "competidores", "concursantes", "deportistas")
termino_participantes <- sample(terminos_participantes, 1)

terminos_cuenta <- c("cuenta con", "dispone de", "tiene asignados", "ha destinado", "ha reservado")
termino_cuenta <- sample(terminos_cuenta, 1)

terminos_repartiran <- c("repartirán", "distribuirán", "dividirán", "asignarán", "otorgarán")
termino_repartiran <- sample(terminos_repartiran, 1)

terminos_puestos <- c("primeros puestos", "ganadores", "mejores lugares", "primeros lugares", "mejores posiciones")
termino_puestos <- sample(terminos_puestos, 1)
```

**Propósito**: Enriquecer el vocabulario y evitar repetición.

**Mejoras implementadas**:

- **Coherencia de género**: Eliminados términos problemáticos como "atletas"
- **Concordancia numérica**: "tiene asignados" concuerda con "millones"
- **Variedad semántica**: Múltiples formas de expresar la misma idea

### 3.5 Sistema de Concordancia para Términos de Dinero

```r
# Aleatorización de términos para dinero con concordancia de género
terminos_dinero_data <- data.frame(
  nombre = c("dinero", "premio", "incentivo", "monto"),
  genero = c("m", "m", "m", "m"),
  articulo_este = c("Este", "Este", "Este", "Este"),
  stringsAsFactors = FALSE
)
termino_dinero_seleccionado <- terminos_dinero_data[sample(nrow(terminos_dinero_data), 1), ]
termino_dinero <- termino_dinero_seleccionado$nombre
articulo_este_dinero <- termino_dinero_seleccionado$articulo_este
```

**Propósito**: Manejar la concordancia de "Este/Esta" con términos monetarios.

**Funcionalidad**:

- **Todos masculinos**: Los términos de dinero son masculinos en español
- **Preparado para expansión**: Estructura permite agregar términos femeninos
- **Concordancia automática**: Evita errores como "Esta dinero"

---

## 4. Generación de Fracciones

```r
# Aleatorización de fracciones para los puestos
conjuntos_fracciones <- list(
  c("1/2", "2/5"),  # Suma 9/10, queda 1/10
  c("1/3", "1/2"),  # Suma 5/6, queda 1/6
  c("2/5", "1/2"),  # Suma 9/10, queda 1/10
  c("3/5", "1/4"),  # Suma 17/20, queda 3/20
  c("1/2", "1/3"),  # Suma 5/6, queda 1/6
  c("3/4", "1/8"),  # Suma 7/8, queda 1/8
  c("2/3", "1/5"),  # Suma 13/15, queda 2/15
  c("3/5", "1/3"),  # Suma 14/15, queda 1/15
  c("1/2", "3/8"),  # Suma 7/8, queda 1/8
  c("3/5", "3/10")  # Suma 9/10, queda 1/10
)

# Seleccionar un conjunto aleatorio de fracciones
indice_conjunto <- sample(1:length(conjuntos_fracciones), 1)
fracciones_seleccionadas <- conjuntos_fracciones[[indice_conjunto]]

# Asignar fracciones a los puestos
fraccion_primer_puesto <- fracciones_seleccionadas[1]
fraccion_segundo_puesto <- fracciones_seleccionadas[2]
```

**Propósito**: Generar combinaciones de fracciones matemáticamente válidas.

**Características del diseño**:

- **Conjuntos predefinidos**: Cada conjunto garantiza que la suma sea menor a 1
- **Variedad matemática**: Diferentes denominadores y niveles de dificultad
- **Comentarios explicativos**: Cada conjunto indica qué fracción queda para el tercer puesto
- **Validación implícita**: Todas las combinaciones son matemáticamente correctas

---

## 5. Cálculos Matemáticos

### 5.1 Conversión y Validación

```r
# Convertir fracciones a valores numéricos para cálculos
convertir_fraccion <- function(fraccion) {
  partes <- strsplit(fraccion, "/")[[1]]
  return(as.numeric(partes[1]) / as.numeric(partes[2]))
}

valor_primer_puesto <- convertir_fraccion(fraccion_primer_puesto)
valor_segundo_puesto <- convertir_fraccion(fraccion_segundo_puesto)

# Calcular la fracción y el valor para el tercer puesto
valor_tercer_puesto <- 1 - (valor_primer_puesto + valor_segundo_puesto)

# Verificar que el valor del tercer puesto sea positivo
test_that("El valor del tercer puesto es positivo", {
  expect_true(valor_tercer_puesto > 0)
})
```

**Propósito**: Convertir fracciones a decimales y validar la lógica matemática.

**Elementos clave**:

- **Función de conversión**: Transforma strings como "1/2" a 0.5
- **Cálculo del resto**: El tercer puesto recibe lo que queda
- **Validación automática**: Test unitario asegura valores positivos

### 5.2 Cálculo de Montos

```r
# Calcular los montos en millones para cada puesto
monto_primer_puesto <- premio_total * valor_primer_puesto
monto_segundo_puesto <- premio_total * valor_segundo_puesto
monto_tercer_puesto <- premio_total * valor_tercer_puesto

# Redondear a números enteros si es necesario
monto_primer_puesto <- round(monto_primer_puesto)
monto_segundo_puesto <- round(monto_segundo_puesto)
monto_tercer_puesto <- round(monto_tercer_puesto)

# Ajustar el tercer puesto para asegurar que la suma sea exactamente el premio total
suma_actual <- monto_primer_puesto + monto_segundo_puesto + monto_tercer_puesto
if (suma_actual != premio_total) {
  monto_tercer_puesto <- premio_total - (monto_primer_puesto + monto_segundo_puesto)
}

# Verificar que la suma de los tres montos sea igual al premio total
test_that("La suma de los tres montos es igual al premio total", {
  expect_equal(monto_primer_puesto + monto_segundo_puesto + monto_tercer_puesto, premio_total)
})
```

**Propósito**: Calcular montos exactos y manejar errores de redondeo.

**Características**:

- **Multiplicación directa**: Fracción × premio total
- **Redondeo inteligente**: A números enteros para simplicidad
- **Ajuste de precisión**: El tercer puesto absorbe errores de redondeo
- **Validación final**: Test unitario confirma que la suma es exacta

---

## 6. Generación de Opciones

```r
# Generar opciones de respuesta
respuesta_correcta <- monto_tercer_puesto

# Generar distractores plausibles
distractor1 <- round(premio_total * 0.05)  # 5% del premio total
distractor2 <- round(premio_total * 0.2)   # 20% del premio total
distractor3 <- round(premio_total * 0.3)   # 30% del premio total

# Asegurarse de que todos los distractores son diferentes de la respuesta correcta
if (distractor1 == respuesta_correcta) distractor1 <- distractor1 + 1
if (distractor2 == respuesta_correcta) distractor2 <- distractor2 + 2
if (distractor3 == respuesta_correcta) distractor3 <- distractor3 - 2

# Asegurarse de que todos los distractores son diferentes entre sí
while (length(unique(c(distractor1, distractor2, distractor3))) < 3) {
  if (distractor1 == distractor2) distractor1 <- distractor1 + 1
  if (distractor2 == distractor3) distractor2 <- distractor2 + 2
  if (distractor1 == distractor3) distractor3 <- distractor3 + 3
}

# Crear un vector con todas las opciones y mezclarlas
opciones <- c(respuesta_correcta, distractor1, distractor2, distractor3)
names(opciones) <- c("correcta", "distractor1", "distractor2", "distractor3")
opciones_mezcladas <- sample(opciones)

# Identificar la posición de la respuesta correcta en las opciones mezcladas
indice_correcto <- which(opciones_mezcladas == respuesta_correcta)

# Crear el vector de solución para r-exams
solucion <- rep(0, 4)
solucion[indice_correcto] <- 1
```

**Propósito**: Crear opciones de respuesta realistas y evitar duplicados.

**Estrategia de distractores**:

- **Porcentajes comunes**: 5%, 20%, 30% del premio total
- **Validación de unicidad**: Algoritmo asegura que todas las opciones sean diferentes
- **Mezcla aleatoria**: Las opciones se presentan en orden aleatorio
- **Compatibilidad r-exams**: Vector binario indica la respuesta correcta

---

## 7. Visualización

### 7.1 Configuración de Colores

```r
# Aleatorización de colores para la tabla TikZ
paletas_colores <- list(
  c("#4285F4", "#EA4335", "#FBBC05", "#34A853"),  # Google colors
  c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728"),  # Tableau colors
  c("#003f5c", "#58508d", "#bc5090", "#ff6361"),  # Viridis-like
  c("#0073C2", "#EFC000", "#868686", "#CD534C"),  # IBM colors
  c("#7F3C8D", "#11A579", "#3969AC", "#F2B701")   # Colorbrewer
)
paleta_seleccionada <- sample(paletas_colores, 1)[[1]]
color_encabezado <- paleta_seleccionada[1]
color_primer_puesto <- paleta_seleccionada[2]
color_segundo_puesto <- paleta_seleccionada[3]
color_tercer_puesto <- paleta_seleccionada[4]
```

**Propósito**: Proporcionar variedad visual con paletas de colores profesionales.

**Características**:

- **Paletas temáticas**: Google, Tableau, Viridis, IBM, Colorbrewer
- **Consistencia visual**: Cada paleta tiene colores armoniosos
- **Accesibilidad**: Colores con buen contraste

### 7.2 Generación de Tabla con Python

```python
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib.gridspec import GridSpec
import numpy as np

# Configurar colores
color_encabezado = "', color_encabezado, '"
color_primer_puesto = "', color_primer_puesto, '"
color_segundo_puesto = "', color_segundo_puesto, '"
color_tercer_puesto = "', color_tercer_puesto, '"

# Crear figura y configurar grid
fig = plt.figure(figsize=(8, 3.5))
gs = GridSpec(4, 2, height_ratios=[1, 1, 1, 1], width_ratios=[3, 7])

# [Código de generación de tabla...]

plt.tight_layout(pad=0.5)
plt.savefig("tabla_distribucion.png", dpi=150, bbox_inches="tight")
plt.savefig("tabla_distribucion.pdf", dpi=150, bbox_inches="tight")
plt.close()
```

**Propósito**: Crear una tabla visual atractiva que muestre la distribución del premio.

**Ventajas de usar Python**:

- **Flexibilidad gráfica**: matplotlib ofrece control total sobre el diseño
- **Calidad profesional**: Gráficos de alta resolución
- **Compatibilidad**: Genera PNG y PDF para diferentes usos

---

## 8. Estructura del Ejercicio

### 8.1 Pregunta Principal

```markdown
En `r articulo_contexto` `r contexto` se realizará `r articulo_competencia` `r competencia` para `r grupo_edad`. Para `r termino_premiar` a los `r termino_participantes`, `r if(contexto_seleccionado$genero == "f") "la" else "el"` `r contexto` `r termino_cuenta` `r premio_total` millones de pesos, que se `r termino_repartiran` entre los tres `r termino_puestos`, como se indica a continuación:

¿Qué cantidad de `r termino_dinero` recibirá el tercer puesto?
```

**Propósito**: Presentar el problema de manera clara y contextualizada.

**Elementos de concordancia**:

- **Artículos variables**: `articulo_contexto` y `articulo_competencia`
- **Concordancia condicional**: "la/el" según el género del contexto
- **Vocabulario diverso**: Términos aleatorios enriquecen el lenguaje
- **Tiempo futuro**: "recibirá" mantiene consistencia temporal

### 8.2 Lista de Respuestas

```markdown
Answerlist
----------
- `r opciones_mezcladas[1]` millones.
- `r opciones_mezcladas[2]` millones.
- `r opciones_mezcladas[3]` millones.
- `r opciones_mezcladas[4]` millones.
```

**Propósito**: Presentar las opciones en formato estándar de r-exams.

---

## 9. Solución Detallada

```markdown
### Paso 1: Identificar los datos del problema
- Premio total: `r premio_total` millones de pesos
- Primer puesto: `r fraccion_primer_puesto` del `r termino_dinero` total
- Segundo puesto: `r fraccion_segundo_puesto` del `r termino_dinero` total
- Tercer puesto: el `r termino_dinero` restante

### Paso 2: Convertir las fracciones a decimales
- Primer puesto: `r fraccion_primer_puesto` = `r valor_primer_puesto`
- Segundo puesto: `r fraccion_segundo_puesto` = `r valor_segundo_puesto`

### Paso 3: Calcular la fracción que corresponde al tercer puesto
Para calcular la fracción del tercer puesto, restamos del total (1) las fracciones del primer y segundo puesto:

- Fracción del tercer puesto = 1 - (`r valor_primer_puesto` + `r valor_segundo_puesto`)
- Fracción del tercer puesto = 1 - `r round(valor_primer_puesto + valor_segundo_puesto, 4)`
- Fracción del tercer puesto = `r round(valor_tercer_puesto, 4)`

### Paso 4: Calcular el monto en millones de pesos para el tercer puesto
Multiplicamos la fracción del tercer puesto por el premio total:

- Monto del tercer puesto = `r round(valor_tercer_puesto, 4)` × `r premio_total` millones
- Monto del tercer puesto = `r monto_tercer_puesto` millones de pesos

### Verificación
Comprobemos que la suma de los tres montos es igual al premio total:

- Primer puesto: `r monto_primer_puesto` millones
- Segundo puesto: `r monto_segundo_puesto` millones
- Tercer puesto: `r monto_tercer_puesto` millones
- Total: `r monto_primer_puesto + monto_segundo_puesto + monto_tercer_puesto` millones

Como `r monto_primer_puesto + monto_segundo_puesto + monto_tercer_puesto` = `r premio_total`, confirmamos que nuestra respuesta es correcta.

Por lo tanto, el tercer puesto recibirá `r monto_tercer_puesto` millones de pesos.
```

**Propósito**: Proporcionar una explicación paso a paso del proceso de solución.

**Características pedagógicas**:

- **Estructura clara**: Pasos numerados y organizados
- **Cálculos explícitos**: Cada operación se muestra detalladamente
- **Verificación**: Comprobación final de la respuesta
- **Lenguaje adaptativo**: Usa los términos aleatorios del problema

---

## 10. Metainformación

```markdown
Meta-information
================
exname: fracciones_reparto_premio
extype: schoice
exsolution: `r paste(as.integer(solucion), collapse="")`
exshuffle: TRUE
exsection: Aritmética|Fracciones|Reparto proporcional
```

**Propósito**: Proporcionar metadatos para el sistema r-exams.

**Elementos**:

- **exname**: Identificador único del ejercicio
- **extype**: Tipo de pregunta (selección múltiple)
- **exsolution**: Vector binario con la respuesta correcta
- **exshuffle**: Permite mezclar las opciones
- **exsection**: Categorización temática jerárquica

---

## Características Innovadoras del Código

### 1. Sistema de Concordancia de Género
- **Problema resuelto**: Evita errores como "una maratón" o "el ciudad"
- **Implementación**: Data frames con género y artículos correspondientes
- **Escalabilidad**: Fácil agregar nuevos términos manteniendo coherencia

### 2. Validación Matemática Automática
- **Tests unitarios**: Verifican que los cálculos sean correctos
- **Manejo de redondeo**: Ajusta automáticamente para evitar errores de precisión
- **Coherencia**: Garantiza que la suma de partes igual el total

### 3. Generación de Distractores Inteligente
- **Algoritmo anti-duplicados**: Asegura que todas las opciones sean únicas
- **Distractores realistas**: Basados en porcentajes comunes
- **Validación iterativa**: Corrige automáticamente conflictos

### 4. Integración Python-R
- **Visualizaciones profesionales**: matplotlib para gráficos de calidad
- **Flexibilidad**: Fácil modificar colores y diseño
- **Compatibilidad**: Genera múltiples formatos de imagen

### 5. Aleatorización Inteligente
- **Variabilidad controlada**: Cada elemento puede variar independientemente
- **Coherencia semántica**: Los términos aleatorios mantienen sentido
- **Escalabilidad**: Fácil agregar nuevas variaciones

---

## Conclusión

Este código representa un ejemplo avanzado de generación automática de ejercicios educativos, combinando:

- **Rigor matemático**: Cálculos precisos y validados
- **Coherencia lingüística**: Concordancia de género y número
- **Variabilidad controlada**: Miles de versiones únicas posibles
- **Calidad visual**: Gráficos profesionales y atractivos
- **Estándares educativos**: Alineado con el marco ICFES

La implementación demuestra cómo la tecnología puede crear contenido educativo de alta calidad, manteniendo tanto la precisión matemática como la riqueza lingüística del español.
