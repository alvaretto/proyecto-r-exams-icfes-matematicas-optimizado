---
name: generar-schoice
description: >
  Genera ejercicio R-exams tipo SCHOICE (selección única) - Despu\u00e9s requiere Ciclo de Validación. (project)
  Usa cuando el análisis ICFES indica tipo schoice, necesites ejercicio de opciones múltiples,
  o quieras crear pregunta con 1 respuesta correcta y 3+ distractores.
  SIEMPRE consulta ejemplos funcionales ANTES de generar código.

allowed-tools:
  - Read
  - Write
  - Edit
  - Grep
  - Glob
  - Bash(ls:*)
  - Bash(mkdir:*)
  - Bash(exams2*:*)
  - Bash(Rscript:*)
---

# Generador de Ejercicios SCHOICE (Selección Única)

## 🎯 Propósito de este Skill

Este skill genera archivos .Rmd de tipo **SCHOICE** (Single Choice - selección única) para el sistema R/exams. Es el skill más usado del workflow ICFES, ya que el 70% de ejercicios matemáticos son de selección múltiple.

### Cuándo usar este skill

**Triggers automáticos** (Claude lo usa cuando detecta):
- "Genera un ejercicio de selección múltiple"
- "Crea un archivo SCHOICE"
- "Necesito un ejercicio con opciones"
- Después de completar `/analizar-icfes` con resultado tipo=schoice
- "Convierte este problema en ejercicio .Rmd"

**Invocación manual:**
```
/generar-schoice imagen_ejercicio.png
/generar-schoice "descripción del problema"
```

## 📋 Anatomía de un Archivo .Rmd SCHOICE

### Estructura Completa Obligatoria

Un archivo SCHOICE válido tiene exactamente **7 secciones** en este orden:

```
1. ENCABEZADO YAML (output + header-includes)
   ↓
2. CHUNK setup (librerías + configuración)
   ↓
3. CHUNK data_generation (aleatorización + cálculos)
   ↓
4. CHUNK version_diversity_test (validación 300+ versiones)
   ↓
5. SECCIÓN Question (enunciado + Answerlist)
   ↓
6. SECCIÓN Solution (explicación + Answerlist)
   ↓
7. META-INFORMATION (metadatos R/exams + ICFES)
```

### Sección 1: Encabezado YAML

**Propósito**: Configurar renderizado LaTeX/PDF con soporte TikZ y español.

```yaml
---
output:
  pdf_document:
    keep_tex: true
header-includes:
  - \usepackage{tikz}
  - \usepackage{pgfplots}
  - \pgfplotsset{compat=1.18}
  - \usepackage[spanish]{babel}
  - \decimalpoint
---
```

**Campos obligatorios:**
- `pdf_document` + `keep_tex: true` → permite debugging
- TikZ packages → soporte gráficos vectoriales
- `babel[spanish]` + `\decimalpoint` → separadores correctos (1.234,56)

Ver detalles en: @.claude/rules/codigo-rmd.md

### Sección 2: Chunk setup

**Propósito**: Cargar librerías y configurar entorno R.

```r
```{r setup, include=FALSE}
library(exams)
library(tidyverse)
library(knitr)
# library(reticulate) # Solo si usa Python

# Configuración knitr
knitr::opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  warning = FALSE,
  fig.path = "figures/",
  fig.width = 8,
  fig.height = 5
)

# Configuración Python (si aplica)
# use_python("/usr/bin/python3")
```
```

**Librerías obligatorias:**
- `exams` → motor de generación
- `tidyverse` → manipulación datos
- `knitr` → renderizado

**Librerías opcionales:**
- `reticulate` → solo si usa Python/matplotlib
- `ggplot2` → incluida en tidyverse

### Sección 3: Chunk data_generation

**Propósito**: Generar datos aleatorios + calcular respuesta correcta + crear distractores.

**Template básico:**

```r
```{r data_generation, include=FALSE}
# Función de generación (OBLIGATORIA para aleatorización)
generar_datos <- function() {
  # 1. PARÁMETROS ALEATORIOS
  a <- sample(2:9, 1)
  b <- sample(10:20, 1)
  c <- sample(5:15, 1)

  # 2. CÁLCULO RESPUESTA CORRECTA
  respuesta_correcta <- (a * b) + c

  # 3. DISTRACTORES (mínimo 3)
  distractor1 <- a * b  # error: olvidar sumar c
  distractor2 <- (a + b) + c  # error: cambiar multiplicación por suma
  distractor3 <- a * (b + c)  # error: distributiva incorrecta

  # 4. LISTA DE OPCIONES (correcta + distractores)
  opciones <- c(respuesta_correcta, distractor1, distractor2, distractor3)

  # 5. DETERMINAR POSICIÓN CORRECTA (antes de shuffle)
  posicion_correcta <- 1

  # 6. SHUFFLE (R/exams lo hace automáticamente con exshuffle: TRUE)
  # NO necesitas hacer sample(opciones) aquí

  # 7. RETORNAR LISTA
  list(
    a = a,
    b = b,
    c = c,
    respuesta_correcta = respuesta_correcta,
    opciones = opciones,
    posicion_correcta = posicion_correcta
  )
}

# Ejecutar generación
datos <- generar_datos()

# Extraer variables para usar en Question/Solution
a <- datos$a
b <- datos$b
c <- datos$c
respuesta <- datos$respuesta_correcta
opciones <- datos$opciones
```
```

**Reglas críticas:**

1. **SIEMPRE encapsular en función `generar_datos()`**
   - Permite llamarla múltiples veces en diversity test
   - Evita variables globales inconsistentes

2. **Distractores deben ser plausibles**
   - Basados en errores conceptuales comunes
   - NO usar valores aleatorios arbitrarios
   - Documentar qué error representa cada distractor

3. **NO hacer shuffle manual**
   - R/exams lo hace con `exshuffle: TRUE`
   - Posición correcta siempre es índice de respuesta_correcta

4. **Retornar TODO en la lista**
   - Variables para el enunciado
   - Respuesta correcta
   - Opciones completas

### Sección 4: Chunk version_diversity_test

**Propósito**: VALIDAR que se generan 300+ versiones únicas (requisito ICFES).

```r
```{r version_diversity_test, include=FALSE}
# Test de diversidad (OBLIGATORIO)
test_versiones <- function(n = 300) {
  versiones <- replicate(n, {
    datos <- generar_datos()
    paste(datos$a, datos$b, datos$c, datos$respuesta_correcta, sep = "_")
  }, simplify = TRUE)

  n_unicas <- length(unique(versiones))
  porcentaje <- round(100 * n_unicas / n, 1)

  cat("Versiones únicas:", n_unicas, "/", n, "(", porcentaje, "%)\n")

  if (n_unicas < 250) {
    warning("ALERTA: Menos de 250 versiones únicas. Aumentar rango de parámetros aleatorios.")
  }

  return(n_unicas)
}

# Ejecutar test (comentar después de validar)
# test_versiones(300)
```
```

**Requisitos:**
- Mínimo 250 versiones únicas de 300 intentos (83%)
- Si falla: ampliar rangos en `sample()`
- Comentar después de validar (no ejecutar en cada renderizado)

Ver especificación completa en: @.claude/rules/codigo-rmd.md

### Sección 5: Question

**Propósito**: Presentar el enunciado y las opciones de respuesta.

**Formato Markdown estándar:**

```markdown
Question
========

[Contexto o introducción si aplica]

**Enunciado de la pregunta usando variables dinámicas**

Dado que $a = `r a`$ y $b = `r b`$, calcula el valor de $(a \times b) + `r c`$.

[Gráfico si aplica]
![](grafico.png)

Answerlist
----------
* `r format(opciones[1], big.mark = ".", decimal.mark = ",", scientific = FALSE)`
* `r format(opciones[2], big.mark = ".", decimal.mark = ",", scientific = FALSE)`
* `r format(opciones[3], big.mark = ".", decimal.mark = ",", scientific = FALSE)`
* `r format(opciones[4], big.mark = ".", decimal.mark = ",", scientific = FALSE)`
```

**Reglas críticas:**

1. **Encabezado `Question` con 8 signos `=`**
   - R/exams lo identifica por esta sintaxis exacta

2. **Variables inline con `` `r variable` ``**
   - Inserta valores dinámicos en el texto
   - Se actualiza en cada versión aleatoria

3. **Answerlist con formato específico:**
   - Encabezado `Answerlist` con 10 guiones `-`
   - Cada opción con `* ` (asterisco + espacio)
   - SIEMPRE usar `format()` con separadores españoles

4. **Gráficos con sintaxis Markdown:**
   - `![](nombre_grafico.png)` → simple
   - `![Descripción](nombre_grafico.png){width=80%}` → con opciones

Ver patrones en: @A-Produccion/Ejemplos-Funcionales-Rmd/

### Sección 6: Solution

**Propósito**: Explicar la resolución paso a paso + indicar cuál opción es correcta.

**Formato Markdown estándar:**

```markdown
Solution
========

**Paso 1: Identificar la operación**

Se debe calcular $(a \times b) + c$ con los valores dados.

**Paso 2: Sustituir valores**

$$(`r a` \times `r b`) + `r c`$$

**Paso 3: Resolver multiplicación**

$$`r a * b` + `r c`$$

**Paso 4: Suma final**

$$`r a * b + c`$$

Por lo tanto, la respuesta correcta es **`r format(respuesta, big.mark = ".", decimal.mark = ",")`**.

**Análisis de distractores:**

Answerlist
----------
* **Correcta**: Resultado de $(a \times b) + c$
* Falsa: Olvidó sumar $c$ (solo calculó $a \times b$)
* Falsa: Cambió multiplicación por suma $(a + b) + c$
* Falsa: Aplicó distributiva incorrecta $a \times (b + c)$

Meta-information
================
exname: [nombre descriptivo]
extype: schoice
exsolution: 1000
exshuffle: TRUE
extol: 0.01

exextra[Type]: SCHOICE
exextra[Competencia]: Formulación y Ejecución
exextra[Componente]: Numérico-Variacional
exextra[Afirmacion]: Resuelve operaciones aritméticas básicas
exextra[Evidencia]: Aplica orden de operaciones correctamente
exextra[Nivel]: 1
```

**Reglas críticas:**

1. **Solution Answerlist OBLIGATORIO:**
   - Indica Verdadero/Falso para cada opción
   - Primer item con `**Correcta**:`
   - Resto con `Falsa:` + explicación del error

2. **exsolution código binario:**
   - `1000` = primera opción correcta
   - `0100` = segunda opción correcta
   - `0010` = tercera opción correcta
   - `0001` = cuarta opción correcta
   - Longitud = número de opciones

3. **exshuffle: TRUE OBLIGATORIO:**
   - R/exams baraja opciones automáticamente
   - Asegura versiones únicas

4. **Metadatos ICFES OBLIGATORIOS:**
   - 6 dimensiones completas (ver `/analizar-icfes`)
   - Consistentes con análisis inicial

Ver metadatos completos en: @.claude/rules/codigo-rmd.md

## 🔍 Proceso de Generación Paso a Paso

### PASO 0: ⚠️ SELECCIÓN OBLIGATORIA DE VERSIÓN GRÁFICA

**Si el ejercicio incluye gráficos del workflow graficador:**

```
¿Cuál versión usar para el .Rmd?
1. TikZ (imagen externa)
2. Python (reticulate)
3. R/ggplot2 (RECOMENDADO - nativo)
```

**NO continuar sin respuesta del usuario.**

**Criterios de decisión:**
- **R/ggplot2** → RECOMENDADO (nativo, sin dependencias)
- **Python** → Si gráfico complejo requiere matplotlib/seaborn
- **TikZ** → Si gráfico geométrico preciso vectorial

### PASO 1: Verificar Análisis ICFES

**Confirma que el ejercicio fue clasificado con `/analizar-icfes`**

```bash
# Buscar análisis previo en historial
# O ejecutar /analizar-icfes si no existe
```

**Datos requeridos del análisis:**
- Nivel de dificultad (1-4)
- Competencia (Interpretación/Formulación/Argumentación)
- Componente (Numérico-Variacional/Geométrico-Métrico/Aleatorio)
- Tipo (schoice confirmado)

### PASO 2: Consultar Ejemplos Funcionales

**CRÍTICO: NUNCA generar código sin consultar ejemplos primero**

```bash
# Ejemplos en producción (FUENTE DE VERDAD)
ls /A-Produccion/Ejemplos-Funcionales-Rmd/*.Rmd

# Buscar ejemplos similares
grep -l "Componente.*Numérico" /A-Produccion/Ejemplos-Funcionales-Rmd/*.Rmd
```

**Identificar ejemplo más similar por:**
- Componente ICFES
- Tipo de contenido matemático
- Uso de gráficos (sí/no)

**Leer ejemplo completo y copiar patrones:**
- Estructura de chunks
- Función generar_datos()
- Formato de distractores
- Inclusión de gráficos si aplica

### PASO 3: Generar Nombre con NOMENCLATURA OBLIGATORIA

**Formato OBLIGATORIO:**
```
[ejercicio]_[componente]_[competencia]_n[nivel]_v[version].Rmd
```

**Componentes del nombre:**

| Parte | Valores | Ejemplo |
|-------|---------|---------|
| `[ejercicio]` | Descriptivo snake_case | `ecuacion_cuadratica` |
| `[componente]` | `geometrico_metrico` \| `numerico_variacional` \| `aleatorio` | `numerico_variacional` |
| `[competencia]` | `interpretacion_representacion` \| `formulacion_ejecucion` \| `argumentacion` | `formulacion_ejecucion` |
| `n[nivel]` | `n1` \| `n2` \| `n3` \| `n4` | `n2` |
| `v[version]` | `v1`, `v2`, ... (incremental) | `v1` |

**Ejemplo completo:**
```
ecuacion_cuadratica_numerico_variacional_formulacion_ejecucion_n2_v1.Rmd
```

Ver especificación completa en: @.claude/docs/NOMENCLATURA_ARCHIVOS_RMD.md

### PASO 4: Crear Carpeta en En-Desarrollo

**Estructura obligatoria:**

```bash
# 1. Crear carpeta con nombre del ejercicio
mkdir -p /A-Produccion/En-Desarrollo/[nombre_ejercicio]

# 2. Guardar .Rmd dentro de la carpeta
/A-Produccion/En-Desarrollo/[nombre_ejercicio]/[nombre_completo].Rmd

# 3. Si tiene gráficos, mover archivos relacionados
outputs/[nombre]/
├── [nombre].Rmd
├── output_tikz.tex, output_python.py, output_r.R (si aplica)
├── tikz_final.png, python_final.png, r_final.png (si aplica)
└── analisis_inicial.json, workflow_state.json (si aplica)
```

**Regla de oro:**
- Cada ejercicio = 1 carpeta con su nombre
- .Rmd + todos sus archivos relacionados dentro
- NUNCA archivos .Rmd sueltos en raíz

### PASO 5: Generar Código .Rmd

**Usar template de ejemplo funcional:**

1. Copiar estructura completa del ejemplo similar
2. Adaptar:
   - Variables aleatorias según el problema
   - Cálculo de respuesta correcta
   - Distractores basados en errores conceptuales
   - Enunciado Question con contexto
   - Solution con pasos detallados
3. Mantener idéntico:
   - Encabezado YAML
   - Chunk setup (librerías)
   - Estructura de chunks
   - Formato Answerlist

**NO improvisar estructuras nuevas**

### PASO 6: Validación Inicial (FASE 1)

**Renderizar en los 4 formatos obligatorios:**

```r
# HTML (más rápido, para debugging)
exams2html("ejercicio.Rmd", n = 1, encoding = "UTF-8")

# PDF (formato principal)
exams2pdf("ejercicio.Rmd", n = 1, encoding = "UTF-8")

# DOCX (para revisión)
exams2pandoc("ejercicio.Rmd", n = 1, type = "docx", encoding = "UTF-8")

# NOPS (formato escaneable)
exams2nops("ejercicio.Rmd", n = 1, encoding = "UTF-8")
```

**Capturar errores/warnings:**
- Guardar logs completos
- Identificar errores críticos vs warnings
- NO ignorar warnings (pueden ser errores futuros)

Ver ciclo completo en: @.claude/rules/ciclo-validacion.md

### PASO 7: Ciclo de Validación y Corrección

**Si hay errores → OBLIGATORIO activar SUBFASE 3A:**

```bash
# SUBFASE 3A: Corrección Basada en Ejemplos
# 1. Identificar error específico
# 2. Buscar en ejemplos funcionales solución al mismo error
grep -A 10 "patrón_similar_al_error" /A-Produccion/Ejemplos-Funcionales-Rmd/*.Rmd

# 3. Aplicar solución del ejemplo
# 4. VOLVER A PASO 6 (revalidar)
```

**NO terminar con errores pendientes**

Ver detalles completos en: @.claude/rules/ciclo-validacion.md

### PASO 8: Promoción a Producción

**Solo después de validación exitosa en 4 formatos:**

```bash
/promover-ejercicio [nombre_ejercicio]
```

Esto mueve el ejercicio de `/En-Desarrollo/` a `/Nuevos-Ejercicios/`

## 🎓 Ejemplos Completos de Generación

### Ejemplo 1: Ejercicio Nivel 1 - Aritmética Básica

**Problema:**
> Calcula el área de un rectángulo de base 5 cm y altura 8 cm.

**Clasificación ICFES:**
- Nivel: 1 (aplicación directa)
- Competencia: Formulación y Ejecuación
- Componente: Geométrico-Métrico

**Código data_generation:**

```r
generar_datos <- function() {
  # Parámetros aleatorios
  base <- sample(3:12, 1)
  altura <- sample(5:15, 1)

  # Respuesta correcta
  area <- base * altura

  # Distractores
  d1 <- base + altura  # error: suma en vez de multiplicación
  d2 <- 2 * (base + altura)  # error: confunde con perímetro
  d3 <- (base * altura) / 2  # error: confunde con triángulo

  opciones <- c(area, d1, d2, d3)

  list(
    base = base,
    altura = altura,
    area = area,
    opciones = opciones,
    posicion_correcta = 1
  )
}
```

**Nombre archivo:**
```
area_rectangulo_geometrico_metrico_formulacion_ejecucion_n1_v1.Rmd
```

Ver archivo completo en: @A-Produccion/Ejemplos-Funcionales-Rmd/

### Ejemplo 2: Ejercicio Nivel 3 - Estadística con Gráfico

**Problema:**
> [Gráfico de barras con ventas mensuales]
> ¿En qué mes hubo el mayor incremento porcentual respecto al mes anterior?

**Clasificación ICFES:**
- Nivel: 3 (múltiples pasos: leer gráfico, calcular variaciones, comparar)
- Competencia: Interpretación y Representación
- Componente: Aleatorio

**Código data_generation con gráfico:**

```r
generar_datos <- function() {
  # Generar ventas aleatorias
  meses <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun")
  ventas <- sample(100:500, 6, replace = TRUE)

  # Calcular variaciones porcentuales
  variaciones <- c(0, diff(ventas) / head(ventas, -1) * 100)

  # Mes con mayor incremento
  mes_max <- meses[which.max(variaciones)]

  # Crear gráfico con ggplot2
  df <- data.frame(Mes = factor(meses, levels = meses), Ventas = ventas)
  p <- ggplot(df, aes(x = Mes, y = Ventas)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    theme_minimal() +
    labs(title = "Ventas Mensuales", y = "Ventas (unidades)")

  # Guardar gráfico
  ggsave("ventas.png", plot = p, width = 8, height = 5, dpi = 150)
  include_supplement("ventas.png")

  # Distractores
  opciones <- c(mes_max,
                sample(setdiff(meses, mes_max), 3))

  list(
    meses = meses,
    ventas = ventas,
    mes_max = mes_max,
    opciones = opciones
  )
}
```

**Question section:**
```markdown
Question
========

Observa el siguiente gráfico de ventas mensuales:

![](ventas.png)

¿En qué mes se registró el **mayor incremento porcentual** respecto al mes anterior?

Answerlist
----------
* `r opciones[1]`
* `r opciones[2]`
* `r opciones[3]`
* `r opciones[4]`
```

**Nombre archivo:**
```
incremento_ventas_aleatorio_interpretacion_representacion_n3_v1.Rmd
```

## ⚠️ Errores Comunes a Evitar

### Error 1: Inclusión Incorrecta de Gráficos

**❌ Incorrecto:**
```r
```{r grafico}
p <- ggplot(...) + ...
print(p)
```
```

**Problema**: R/exams NO captura gráficos con `print()` en chunks.

**✓ Correcto:**
```r
```{r data_generation, include=FALSE}
p <- ggplot(...) + ...
ggsave("grafico.png", plot = p, width = 8, height = 5, dpi = 150)
include_supplement("grafico.png")
```
```

```markdown
Question
========
![](grafico.png)
```

**Regla**: Siempre `ggsave()` + `include_supplement()` + referencia Markdown.

### Error 2: Distractores Aleatorios sin Sentido

**❌ Incorrecto:**
```r
distractor1 <- respuesta_correcta + sample(-10:10, 1)
distractor2 <- respuesta_correcta * sample(1:3, 1)
```

**Problema**: No representan errores conceptuales, estudiantes los descartan fácilmente.

**✓ Correcto:**
```r
# Distractores basados en errores comunes
distractor1 <- a * b  # olvida sumar c
distractor2 <- (a + b) + c  # cambia operación
distractor3 <- a * (b + c)  # distributiva incorrecta
```

**Regla**: Cada distractor = 1 error conceptual específico documentado.

### Error 3: Formato de Números sin Separadores

**❌ Incorrecto:**
```markdown
* `r opciones[1]`
```

**Problema**: Muestra números con punto decimal inglés (1234.56).

**✓ Correcto:**
```markdown
* `r format(opciones[1], big.mark = ".", decimal.mark = ",", scientific = FALSE)`
```

**Resultado**: 1.234,56 (formato español correcto).

**Regla**: SIEMPRE usar `format()` con ambos separadores en Answerlist.

### Error 4: Nomenclatura Incorrecta

**❌ Incorrecto:**
```
ejercicio1.Rmd
problema_geometria.Rmd
test_nivel2_v1.Rmd
```

**Problema**: No sigue formato oficial, dificulta organización y búsqueda.

**✓ Correcto:**
```
area_rectangulo_geometrico_metrico_formulacion_ejecucion_n1_v1.Rmd
```

**Regla**: Formato obligatorio con 5 componentes separados por `_`.

Ver especificación en: @.claude/docs/NOMENCLATURA_ARCHIVOS_RMD.md

### Error 5: No Validar Diversidad de Versiones

**❌ Incorrecto:**
```r
# Generar datos sin test
a <- sample(1:3, 1)  # solo 3 valores posibles
b <- sample(1:2, 1)  # solo 2 valores posibles
# Total versiones únicas: 3 × 2 = 6 (INSUFICIENTE)
```

**Problema**: No alcanza mínimo 250 versiones únicas.

**✓ Correcto:**
```r
# Rangos amplios + test
a <- sample(2:20, 1)  # 19 valores
b <- sample(5:30, 1)  # 26 valores
c <- sample(1:15, 1)  # 15 valores
# Total versiones posibles: 19 × 26 × 15 = 7,410

# Validar con test
test_versiones(300)  # Debe dar 250+ únicas
```

**Regla**: Ampliar rangos para asegurar > 250 versiones únicas de 300 intentos.

### Error 6: No Consultar Ejemplos Funcionales

**❌ Incorrecto:**
```
# Generar código desde cero sin consultar ejemplos
# Improvisar estructura de chunks
# Asumir que lógica correcta = código funcional
```

**Problema**: Alto riesgo de errores de compilación, estructura incorrecta, patrones no validados.

**✓ Correcto:**
```bash
# SIEMPRE consultar ANTES de escribir
ls /A-Produccion/Ejemplos-Funcionales-Rmd/
grep -l "Geométrico" /A-Produccion/Ejemplos-Funcionales-Rmd/*.Rmd
# Leer ejemplo similar completo
# Copiar estructura y adaptar
```

**Regla de Oro**: Ejemplos funcionales = Fuente de verdad ABSOLUTA.

Ver principio en: @.claude/rules/documentacion-verificada.md

## ⛔ CONDICIONES CRÍTICAS

### Pre-generación (OBLIGATORIO verificar ANTES de escribir código)

1. ✓ Análisis ICFES completado con `/analizar-icfes`
2. ✓ Ejemplo funcional similar identificado y leído
3. ✓ Nomenclatura calculada según formato oficial
4. ✓ Carpeta destino creada en `/En-Desarrollo/`
5. ✓ Decisión de versión gráfica (si aplica)

### Durante generación (OBLIGATORIO incluir)

1. ✓ Función `generar_datos()` con aleatorización
2. ✓ Test de diversidad 300+ versiones
3. ✓ Distractores basados en errores conceptuales
4. ✓ Formato español en todos los números
5. ✓ Metadatos ICFES completos (6 dimensiones)

### Post-generación (OBLIGATORIO validar)

1. ✓ Renderizado exitoso en 4 formatos (HTML, PDF, DOCX, NOPS)
2. ✓ Gráficos visualmente correctos (si aplica)
3. ✓ Coherencia matemática pregunta-respuesta-distractores
4. ✓ Test de diversidad > 250 versiones únicas
5. ✓ Ciclo de Validación completo (FASE 1→2→3)

**Regla Absoluta**: NO terminar con errores pendientes. VOLVER A FASE 1 después de cada corrección.

## 🔗 Referencias y Documentación

### Archivos de Referencia Obligatorios

- **Ejemplos Funcionales**: @A-Produccion/Ejemplos-Funcionales-Rmd/ (FUENTE DE VERDAD)
- **Nomenclatura**: @.claude/docs/NOMENCLATURA_ARCHIVOS_RMD.md
- **Ciclo Validación**: @.claude/rules/ciclo-validacion.md
- **Metadatos**: @.claude/rules/codigo-rmd.md
- **Patrones de Errores**: @.claude/docs/patrones-errores-conocidos.md

### Reglas del Proyecto (OBLIGATORIAS)

- @.claude/rules/codigo-rmd.md - Reglas pre-edit/write
- @.claude/rules/ciclo-validacion.md - FASE 1→2→3 + SUBFASES
- @.claude/rules/documentacion-verificada.md - Principio de verificación

### Documentación Técnica

- @.claude/docs/WORKFLOW_PASO_A_PASO.md - Flujo completo
- @.claude/docs/TROUBLESHOOTING.md - Solución de problemas
- @.claude/docs/TRES_NIVELES_VALIDACION.md - Validación en profundidad
- @.claude/docs/MEJORES_PRACTICAS_PYTHON_RETICULATE.md - Uso de Python (si aplica)

## 🚀 Integración con Otros Skills

Este skill se integra en el workflow ICFES:

```
analizar-icfes
    ↓
[Resultado: tipo=schoice]
    ↓
generar-schoice ← ESTE SKILL
    ↓
validar-renderizado (FASE 1)
    ↓
[Si errores] → diagnosticar-errores (FASE 3)
    ↓
[Si errores] → corregir-graficos (SUBFASE 3A)
    ↓
[Volver] → validar-renderizado (SUBFASE 3B)
    ↓
validar-coherencia (FASE 2)
    ↓
[Si todo OK] → promover-ejercicio
```

**Skills relacionados:**
- `/analizar-icfes` - Prerequisito (análisis ICFES)
- `/generar-cloze` - Alternativa (ejercicios compuestos)
- `validar-renderizado` - Siguiente paso (FASE 1)
- `diagnosticar-errores` - Corrección (FASE 3)
- `/promover-ejercicio` - Final (promoción a producción)

## 📊 Output Final Esperado

Después de usar este skill, debes tener:

```
/A-Produccion/En-Desarrollo/[nombre_ejercicio]/
├── [nombre_completo].Rmd (archivo principal)
├── [gráficos].png (si aplica)
└── [archivos auxiliares] (si aplica)
```

**Archivo .Rmd debe:**
- ✓ Compilar en 4 formatos sin errores
- ✓ Generar 250+ versiones únicas
- ✓ Tener metadatos ICFES completos
- ✓ Seguir nomenclatura oficial
- ✓ Estar basado en ejemplos funcionales

**Siguiente acción:**
```
# Si compilación exitosa en 4 formatos
/promover-ejercicio [nombre_ejercicio]

# Si hay errores
# → SUBFASE 3A automática (corregir basado en ejemplos)
# → SUBFASE 3B automática (volver a FASE 1)
```

---

**Última actualización**: 2025-12-30
**Versión**: 2.0 (Progressive Disclosure)
**Basado en**: Claude Code best practices (nov 2025) + R/exams documentation
