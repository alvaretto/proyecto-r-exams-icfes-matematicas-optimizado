---
name: generar-cloze
description: >
  Genera ejercicio R-exams tipo CLOZE (pregunta compuesta) - Despu\u00e9s requiere Ciclo de Validación. (project)
  Usa cuando el análisis ICFES indica tipo cloze, necesites pregunta con múltiples partes,
  o quieras combinar opciones múltiples + respuestas numéricas en un solo ejercicio.
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

# Generador de Ejercicios CLOZE (Pregunta Compuesta)

## 🎯 Propósito de este Skill

Este skill genera archivos .Rmd de tipo **CLOZE** (pregunta compuesta con múltiples gaps) para el sistema R/exams. Los ejercicios CLOZE permiten combinar diferentes tipos de respuestas en un solo problema, ideal para evaluaciones complejas que requieren múltiples pasos de razonamiento.

### Cuándo usar CLOZE vs SCHOICE

**Usa CLOZE cuando:**
- El problema requiere **múltiples respuestas** en secuencia
- Necesitas **combinar tipos de pregunta** (selección + numérica + texto)
- El ejercicio tiene **varios pasos** que deben responderse por separado
- Nivel de dificultad **3 o 4** (problemas complejos multifacéticos)

**Usa SCHOICE cuando:**
- Solo hay **1 respuesta final** (aunque requiera varios cálculos)
- Todas las opciones son del **mismo tipo**
- Nivel de dificultad **1 o 2** (problemas más directos)

### Cuándo usar este skill

**Triggers automáticos** (Claude lo usa cuando detecta):
- "Genera un ejercicio compuesto"
- "Crea un archivo CLOZE"
- "Necesito ejercicio con múltiples partes"
- Después de completar `/analizar-icfes` con resultado tipo=cloze
- "Ejercicio que combine opciones y respuestas numéricas"
- Problema con varios subíndices (a), b), c))

**Invocación manual:**
```
/generar-cloze imagen_ejercicio.png
/generar-cloze "descripción del problema compuesto"
```

## 📋 Anatomía de un Archivo .Rmd CLOZE

### Estructura Completa Obligatoria

Un archivo CLOZE válido tiene exactamente **7 secciones** en este orden:

```
1. ENCABEZADO YAML (output + header-includes)
   ↓
2. CHUNK setup (librerías + configuración numérica)
   ↓
3. CHUNK data_generation (aleatorización + múltiples cálculos)
   ↓
4. CHUNK version_diversity_test (validación 300+ versiones)
   ↓
5. SECCIÓN Question (enunciado + GAPS + Answerlist por gap)
   ↓
6. SECCIÓN Solution (explicación + soluciones)
   ↓
7. META-INFORMATION (metadatos CLOZE específicos + ICFES)
```

### Diferencias Clave vs SCHOICE

| Aspecto | SCHOICE | CLOZE |
|---------|---------|-------|
| **Respuestas** | 1 sola | Múltiples (2-6 gaps) |
| **Tipos** | Solo schoice | Mixtos (schoice\|num\|string) |
| **exsolution** | `1000` | `1000\|42.5\|texto` |
| **extol** | `0.01` | `0\|1\|0` (una por gap) |
| **exclozetype** | N/A | `schoice\|num\|string` |
| **Complejidad** | Simple | Alta |

### Sección 1: Encabezado YAML

**Idéntico a SCHOICE** (ver `/generar-schoice`):

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

### Sección 2: Chunk setup

**Similar a SCHOICE pero con configuración numérica adicional:**

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

# CRÍTICO para CLOZE: Evitar notación científica
options(scipen = 999)
```
```

**Diferencia clave**: `options(scipen = 999)` evita que números grandes se muestren en notación científica (1.5e+06 → 1500000).

### Sección 3: Chunk data_generation

**Propósito**: Generar datos aleatorios + calcular **múltiples respuestas** para cada gap.

**Template básico para CLOZE:**

```r
```{r data_generation, include=FALSE}
# Funciones de formateo (CRÍTICAS para CLOZE)
formatear_entero <- function(x) {
  format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
}

formato_estandar <- function(x, decimales = 2) {
  format(round(x, decimales), big.mark = ".", decimal.mark = ",",
         scientific = FALSE, nsmall = decimales, trim = TRUE)
}

# Función de generación (OBLIGATORIA)
generar_datos <- function() {
  # 1. PARÁMETROS ALEATORIOS
  poblacion_inicial <- sample(50:200, 1) * 1000
  tasa_crecimiento <- sample(2:8, 1) / 100  # 0.02 a 0.08
  anios <- sample(5:15, 1)

  # 2. CÁLCULOS INTERMEDIOS (pueden usarse en gaps)
  poblacion_final <- poblacion_inicial * (1 + tasa_crecimiento)^anios
  crecimiento_absoluto <- poblacion_final - poblacion_inicial
  crecimiento_porcentual <- (crecimiento_absoluto / poblacion_inicial) * 100

  # 3. GAP 1 - SCHOICE (opciones múltiples)
  # Respuesta correcta para gap1
  modelo_correcto <- "Crecimiento exponencial"

  # Distractores conceptuales
  opciones_gap1 <- c(
    modelo_correcto,
    "Crecimiento lineal",  # error: ignora el exponente
    "Decrecimiento exponencial",  # error: signo incorrecto
    "Crecimiento logarítmico"  # error: función incorrecta
  )

  # 4. GAP 2 - NUM (respuesta numérica)
  # Población final calculada
  respuesta_gap2 <- round(poblacion_final, 0)

  # 5. GAP 3 - NUM (porcentaje)
  respuesta_gap3 <- round(crecimiento_porcentual, 2)

  # 6. GAP 4 - STRING (texto corto)
  # Si el crecimiento > 50%, es "Alto", sino "Moderado"
  clasificacion <- if (crecimiento_porcentual > 50) "Alto" else "Moderado"

  # 7. RETORNAR TODO
  list(
    poblacion_inicial = poblacion_inicial,
    tasa_crecimiento = tasa_crecimiento,
    anios = anios,
    poblacion_final = poblacion_final,
    crecimiento_absoluto = crecimiento_absoluto,
    crecimiento_porcentual = crecimiento_porcentual,
    opciones_gap1 = opciones_gap1,
    respuesta_gap1_pos = 1,  # Posición de respuesta correcta
    respuesta_gap2 = respuesta_gap2,
    respuesta_gap3 = respuesta_gap3,
    respuesta_gap4 = clasificacion
  )
}

# Ejecutar generación
datos <- generar_datos()

# Extraer variables (facilita uso en Question/Solution)
pob_inicial <- datos$poblacion_inicial
tasa <- datos$tasa_crecimiento
anios <- datos$anios
pob_final <- datos$poblacion_final
opciones_gap1 <- datos$opciones_gap1
```
```

**Reglas críticas para CLOZE:**

1. **SIEMPRE definir funciones de formateo**
   - `formatear_entero()` para números grandes sin decimales
   - `formato_estandar()` para decimales con precisión fija
   - Evita inconsistencias entre versiones

2. **Un cálculo por cada gap**
   - Gap 1 (schoice) → opciones predefinidas + distractores
   - Gap 2 (num) → resultado numérico exacto
   - Gap 3 (num) → otro cálculo numérico
   - Gap 4 (string) → clasificación o texto corto

3. **Retornar TODO en la lista**
   - Todas las variables del enunciado
   - Todas las respuestas de gaps
   - Opciones para gaps tipo schoice

4. **options(scipen = 999)**
   - Crítico para evitar notación científica
   - Ya configurado en chunk setup

### Sección 4: Chunk version_diversity_test

**Similar a SCHOICE pero validando TODOS los gaps:**

```r
```{r version_diversity_test, include=FALSE}
# Test de diversidad (OBLIGATORIO)
test_versiones <- function(n = 300) {
  versiones <- replicate(n, {
    datos <- generar_datos()
    # Combinar TODAS las respuestas en una firma única
    paste(
      datos$poblacion_inicial,
      datos$tasa_crecimiento,
      datos$anios,
      datos$respuesta_gap1_pos,
      datos$respuesta_gap2,
      datos$respuesta_gap3,
      datos$respuesta_gap4,
      sep = "_"
    )
  }, simplify = TRUE)

  n_unicas <- length(unique(versiones))
  porcentaje <- round(100 * n_unicas / n, 1)

  cat("Versiones únicas:", n_unicas, "/", n, "(", porcentaje, "%)\n")

  if (n_unicas < 250) {
    warning("ALERTA: Menos de 250 versiones únicas. Aumentar rangos.")
  }

  return(n_unicas)
}

# Ejecutar test (comentar después de validar)
# test_versiones(300)
```
```

**Diferencia clave**: Incluir respuestas de TODOS los gaps en la firma única.

### Sección 5: Question

**Propósito**: Presentar enunciado con GAPS y Answerlist para gaps tipo schoice/mchoice.

**Formato Markdown con GAPS:**

```markdown
Question
========

**Contexto del problema**

Una ciudad tiene una población inicial de `r formatear_entero(pob_inicial)` habitantes
y crece a una tasa anual del `r formato_estandar(tasa * 100, 1)`% durante `r anios` años.

**a) ¿Qué modelo matemático describe este crecimiento?**

##ANSWER1##

**b) ¿Cuál será la población después de `r anios` años? (en habitantes, sin decimales)**

##ANSWER2##

**c) ¿Qué porcentaje de crecimiento total representa? (con 2 decimales)**

##ANSWER3##

**d) Clasifica el crecimiento como "Alto" (>50%) o "Moderado" (≤50%)**

##ANSWER4##

Answerlist
----------
* Crecimiento exponencial
* Crecimiento lineal
* Decrecimiento exponencial
* Crecimiento logarítmico
```

**Reglas críticas:**

1. **GAPS con sintaxis exacta:**
   - `##ANSWER1##`, `##ANSWER2##`, etc.
   - Numerar correlativamente (1, 2, 3, ...)
   - NO usar `##ANS1##` o variantes

2. **Answerlist SOLO para gaps tipo schoice/mchoice:**
   - Si gap1 es schoice → incluir Answerlist
   - Si gap2 es num → NO incluir opciones
   - Si gap3 es string → NO incluir opciones

3. **Formateo consistente:**
   - Usar `formatear_entero()` para poblaciones
   - Usar `formato_estandar()` para porcentajes
   - Especificar unidades y decimales en el enunciado

4. **Subíndices claros:**
   - **a)**, **b)**, **c)** para cada gap
   - Enunciados específicos por gap
   - Indicar tipo esperado (número, texto, opción)

### Sección 6: Solution

**Propósito**: Explicar resolución paso a paso para TODOS los gaps.

**Formato Markdown para CLOZE:**

```markdown
Solution
========

**a) Modelo matemático**

El crecimiento poblacional con tasa constante sigue la fórmula:

$$P(t) = P_0 \times (1 + r)^t$$

Donde:
- $P_0$ = población inicial = `r formatear_entero(pob_inicial)`
- $r$ = tasa de crecimiento = `r formato_estandar(tasa, 4)`
- $t$ = tiempo en años = `r anios`

Por lo tanto, el modelo es **Crecimiento exponencial**.

**b) Población final**

Sustituyendo en la fórmula:

$$P(`r anios`) = `r formatear_entero(pob_inicial)` \times (1 + `r formato_estandar(tasa, 4)`)^{`r anios`}$$

$$P(`r anios`) = `r formatear_entero(pob_final)` \text{ habitantes}$$

**c) Porcentaje de crecimiento total**

$$\text{Crecimiento %} = \frac{P_{\text{final}} - P_{\text{inicial}}}{P_{\text{inicial}}} \times 100$$

$$\text{Crecimiento %} = \frac{`r formatear_entero(pob_final)` - `r formatear_entero(pob_inicial)`}{`r formatear_entero(pob_inicial)`} \times 100 = `r formato_estandar(datos$crecimiento_porcentual, 2)`\%$$

**d) Clasificación del crecimiento**

Como el crecimiento es `r formato_estandar(datos$crecimiento_porcentual, 2)`%,
y `r ifelse(datos$crecimiento_porcentual > 50, "esto es mayor", "esto es menor o igual")`
al 50%, se clasifica como **`r datos$respuesta_gap4`**.

Meta-information
================
exname: Crecimiento Poblacional Exponencial Compuesto
extype: cloze
exclozetype: schoice|num|num|string
exsolution: 1000|`r datos$respuesta_gap2`|`r formato_estandar(datos$respuesta_gap3, 2)`|`r datos$respuesta_gap4`
extol: 0|1|0.01|0
exshuffle: TRUE

exextra[Type]: CLOZE
exextra[Competencia]: Formulación y Ejecución
exextra[Componente]: Numérico-Variacional
exextra[Afirmacion]: Modela y resuelve problemas de crecimiento exponencial
exextra[Evidencia]: Aplica fórmulas exponenciales y calcula variaciones porcentuales
exextra[Nivel]: 3
```

**Reglas críticas para Meta-information CLOZE:**

1. **exclozetype**: Tipos separados por `|` en orden de gaps
   - `schoice|num|num|string` → 4 gaps
   - Tipos válidos: `schoice`, `mchoice`, `num`, `string`

2. **exsolution**: Respuestas separadas por `|` en orden
   - schoice: código binario `1000` (posición correcta)
   - num: valor numérico con `` `r variable` ``
   - string: texto exacto con `` `r variable` ``
   - **CRÍTICO**: Usar formato_estandar() para nums con decimales

3. **extol**: Tolerancias separadas por `|` en orden
   - schoice: `0` (sin tolerancia, exacto)
   - num grande (>1000): `1` o mayor (tolerancia absoluta)
   - num pequeño (<100): `0.01` a `0.1` (tolerancia decimal)
   - string: `0` (exacto, case-sensitive)

4. **exshuffle: TRUE**
   - Baraja opciones de gaps tipo schoice/mchoice
   - NO afecta gaps num/string

Ver metadatos completos en: @.claude/rules/codigo-rmd.md

## 🔍 Proceso de Generación Paso a Paso

### PASO 0: Determinar Número y Tipo de Gaps

**Antes de generar, definir:**

1. **¿Cuántos gaps necesito?** (2-6 recomendado)
2. **¿Qué tipo cada gap?**
   - schoice: Selección única (4 opciones)
   - mchoice: Selección múltiple (verdadero/falso por opción)
   - num: Respuesta numérica (entero o decimal)
   - string: Texto corto (nombre, clasificación)

3. **¿Qué tolerancia por gap numérico?**
   - Valores >1000: tolerancia ≥ 1
   - Valores 0-100: tolerancia 0.01-0.1
   - Porcentajes: tolerancia 0.01

**Ejemplo de planificación:**
```
Gap 1 (schoice): Tipo de modelo → 4 opciones
Gap 2 (num): Población final → tolerancia 1 (valor grande)
Gap 3 (num): Porcentaje → tolerancia 0.01 (decimal preciso)
Gap 4 (string): Clasificación → tolerancia 0 (exacto)

exclozetype: schoice|num|num|string
extol: 0|1|0.01|0
```

### PASO 1: Verificar Análisis ICFES

**Confirma que el ejercicio fue clasificado con `/analizar-icfes`**

**Datos requeridos:**
- Nivel de dificultad (**típicamente 3 o 4** para CLOZE)
- Competencia
- Componente
- Tipo confirmado = **cloze**

**CLOZE es apropiado si:**
- Nivel ≥ 3 (complejidad alta)
- Requiere múltiples cálculos secuenciales
- Evalúa varios conceptos en un problema

### PASO 2: Consultar Ejemplos Funcionales CLOZE

**CRÍTICO: CLOZE tiene menos ejemplos que SCHOICE, consultar cuidadosamente**

```bash
# Ejemplos CLOZE en producción
grep -l "extype: cloze" /A-Produccion/Ejemplos-Funcionales-Rmd/*.Rmd

# Ejemplo específico de probabilidad condicional
ls /A-Produccion/Ejemplos-Funcionales-Rmd/*probabilidad*cloze*.Rmd

# Buscar por componente
grep -l "Componente.*Aleatorio.*cloze" /A-Produccion/Ejemplos-Funcionales-Rmd/*.Rmd
```

**Identificar ejemplo más similar por:**
- Número de gaps (2-6)
- Tipos de gaps (schoice+num, num+string, etc.)
- Componente ICFES

**Copiar patrones específicos de CLOZE:**
- Funciones de formateo (formatear_entero, formato_estandar)
- Estructura de gaps en Question
- Configuración extol por tipo de gap
- Formato exsolution con ``

### PASO 3: Generar Nombre con NOMENCLATURA OBLIGATORIA

**Idéntico a SCHOICE** (ver `/generar-schoice`):

```
[ejercicio]_[componente]_[competencia]_n[nivel]_v[version].Rmd
```

**Ejemplo CLOZE:**
```
crecimiento_poblacional_numerico_variacional_formulacion_ejecucion_n3_v1.Rmd
probabilidad_condicional_aleatorio_interpretacion_representacion_n3_v1.Rmd
```

### PASO 4: Crear Carpeta en En-Desarrollo

**Idéntico a SCHOICE:**

```bash
mkdir -p /A-Produccion/En-Desarrollo/[nombre_ejercicio]
```

### PASO 5: Generar Código .Rmd CLOZE

**Usar template de ejemplo funcional CLOZE:**

1. Copiar estructura completa del ejemplo similar
2. Adaptar:
   - Parámetros aleatorios según el problema
   - Cálculos para CADA gap (gap1, gap2, gap3, ...)
   - Opciones para gaps tipo schoice/mchoice
   - Distractores conceptuales
   - Funciones de formateo
3. Configurar metadatos CLOZE:
   - exclozetype según tipos de gaps
   - exsolution con `` para valores dinámicos
   - extol apropiado por tipo
4. Mantener idéntico:
   - Encabezado YAML
   - Chunk setup con options(scipen = 999)
   - Estructura de chunks

**NO improvisar configuración de tolerancias**

### PASO 6: Validación Inicial (FASE 1)

**Renderizar en 4 formatos:**

```r
# HTML (más rápido, para debugging)
exams2html("ejercicio_cloze.Rmd", n = 1, encoding = "UTF-8")

# PDF (formato principal)
exams2pdf("ejercicio_cloze.Rmd", n = 1, encoding = "UTF-8")

# DOCX (para revisión)
exams2pandoc("ejercicio_cloze.Rmd", n = 1, type = "docx", encoding = "UTF-8")

# NOPS (formato escaneable - puede fallar con algunos cloze complejos)
exams2nops("ejercicio_cloze.Rmd", n = 1, encoding = "UTF-8")
```

**Validar específicamente para CLOZE:**
- ✓ Gaps numerados correctamente (1, 2, 3, ...)
- ✓ Tipos de gaps correctos (schoice donde hay opciones, num donde hay números)
- ✓ Tolerancias apropiadas (num grandes con tol ≥ 1)
- ✓ Formato de números consistente (español)

### PASO 7: Ciclo de Validación y Corrección

**Errores comunes específicos de CLOZE:**

1. **extol muy pequeño para nums grandes**
   ```
   Error: Respuesta 1.500.000 no acepta 1.500.001
   Solución: Cambiar extol de 0.01 a 1 o mayor
   ```

2. **exsolution sin backticks para valores dinámicos**
   ```
   ❌ exsolution: 1000|42.5|texto
   ✓ exsolution: 1000|`r variable`|`r texto`
   ```

3. **Número de elementos en extol ≠ número de gaps**
   ```
   3 gaps pero extol: 0|1 (solo 2 valores)
   Solución: extol: 0|1|0.01
   ```

**Si hay errores → SUBFASE 3A:**

```bash
# Buscar ejemplo CLOZE funcional similar
grep -l "exclozetype.*num.*num" /A-Produccion/Ejemplos-Funcionales-Rmd/*.Rmd
# Copiar configuración de tolerancias
# Aplicar corrección
# VOLVER A PASO 6
```

Ver ciclo completo en: @.claude/rules/ciclo-validacion.md

### PASO 8: Promoción a Producción

**Solo después de validación exitosa:**

```bash
/promover-ejercicio [nombre_ejercicio]
```

## 🎓 Ejemplos Completos de Generación

### Ejemplo 1: CLOZE Nivel 3 - Probabilidad Condicional

**Problema:**
> En una urna hay 5 bolas rojas y 3 azules. Se extraen 2 bolas sin reposición.
> a) ¿Probabilidad de que ambas sean rojas?
> b) ¿Cuántas combinaciones posibles de 2 bolas de 8 totales?
> c) ¿El evento "segunda roja" depende de "primera roja"?

**Clasificación ICFES:**
- Nivel: 3 (múltiples pasos, probabilidad condicional)
- Competencia: Formulación y Ejecución
- Componente: Aleatorio

**Planificación de gaps:**
```
Gap 1 (num): Probabilidad (decimal con 4 decimales) → tol: 0.0001
Gap 2 (num): Combinaciones (entero) → tol: 0
Gap 3 (schoice): Dependencia (Sí/No) → tol: 0
```

**Código data_generation:**

```r
generar_datos <- function() {
  # Parámetros aleatorios
  rojas <- sample(4:8, 1)
  azules <- sample(2:6, 1)
  total <- rojas + azules

  # Gap 1: Probabilidad ambas rojas
  prob_primera_roja <- rojas / total
  prob_segunda_roja_dado_primera <- (rojas - 1) / (total - 1)
  prob_ambas_rojas <- prob_primera_roja * prob_segunda_roja_dado_primera

  # Gap 2: Combinaciones
  combinaciones <- choose(total, 2)

  # Gap 3: Dependencia
  opciones_gap3 <- c(
    "Sí, son eventos dependientes",
    "No, son eventos independientes",
    "Depende del número de bolas",
    "No se puede determinar"
  )

  list(
    rojas = rojas,
    azules = azules,
    total = total,
    respuesta_gap1 = prob_ambas_rojas,
    respuesta_gap2 = combinaciones,
    opciones_gap3 = opciones_gap3,
    respuesta_gap3_pos = 1
  )
}
```

**Question section:**
```markdown
Question
========

Una urna contiene `r datos$rojas` bolas rojas y `r datos$azules` bolas azules.
Se extraen 2 bolas **sin reposición**.

**a) ¿Cuál es la probabilidad de que ambas sean rojas? (responder con 4 decimales)**

##ANSWER1##

**b) ¿Cuántas combinaciones posibles hay de 2 bolas de `r datos$total` totales?**

##ANSWER2##

**c) ¿El evento "segunda bola roja" depende del resultado de la primera extracción?**

##ANSWER3##

Answerlist
----------
* Sí, son eventos dependientes
* No, son eventos independientes
* Depende del número de bolas
* No se puede determinar
```

**Meta-information:**
```yaml
exname: Probabilidad Condicional Urnas Sin Reposición
extype: cloze
exclozetype: num|num|schoice
exsolution: `r formato_estandar(datos$respuesta_gap1, 4)`|`r datos$respuesta_gap2`|1000
extol: 0.0001|0|0
exshuffle: TRUE
```

**Nombre archivo:**
```
probabilidad_urnas_sin_reposicion_aleatorio_formulacion_ejecucion_n3_v1.Rmd
```

### Ejemplo 2: CLOZE Nivel 4 - Optimización Empresarial

**Problema:**
> Una empresa produce x unidades. Costo = 50x + 1000, Ingreso = 120x.
> a) ¿Función de ganancia G(x)?
> b) ¿Punto de equilibrio?
> c) ¿Ganancia con 100 unidades?
> d) ¿Es rentable? (Sí/No)

**Planificación de gaps:**
```
Gap 1 (string): Función ganancia → "G(x) = 70x - 1000"
Gap 2 (num): Punto equilibrio → tol: 0.01
Gap 3 (num): Ganancia con 100 → tol: 1
Gap 4 (schoice): Rentabilidad → Sí/No
```

**exclozetype:** `string|num|num|schoice`
**extol:** `0|0.01|1|0`

## ⚠️ Errores Comunes a Evitar

### Error 1: Tolerancia Inadecuada para Números Grandes

**❌ Incorrecto:**
```yaml
# Gap con respuesta 1.500.000 habitantes
exsolution: ...|1500000|...
extol: ...|0.01|...
```

**Problema**: Tolerancia 0.01 es demasiado estricta para números de 7 dígitos. Diferencias de redondeo causan fallos.

**✓ Correcto:**
```yaml
exsolution: ...|`r round(datos$poblacion, 0)`|...
extol: ...|1|...  # Tolerancia 1 habitante es razonable
```

**Regla**:
- Valores > 1000 → extol ≥ 1
- Valores 100-1000 → extol 0.1-1
- Valores < 100 → extol 0.01-0.1

### Error 2: Número Incorrecto de Elementos en extol/exclozetype

**❌ Incorrecto:**
```yaml
# 4 gaps pero solo 3 tipos
exclozetype: schoice|num|num
extol: 0|1|0.01
```

**Problema**: exclozetype y extol deben tener exactamente tantos elementos como gaps.

**✓ Correcto:**
```yaml
# 4 gaps = 4 tipos + 4 tolerancias
exclozetype: schoice|num|num|string
extol: 0|1|0.01|0
```

**Regla**: Contar gaps en Question, asegurar longitud idéntica.

### Error 3: Usar Valores Literales en exsolution para Datos Dinámicos

**❌ Incorrecto:**
```yaml
# Respuesta gap2 es 42.5, pero es dinámica
exsolution: 1000|42.5|texto
```

**Problema**: Respuesta 42.5 es fija, pero debería cambiar con cada versión aleatoria.

**✓ Correcto:**
```yaml
exsolution: 1000|`r formato_estandar(datos$respuesta_gap2, 2)`|`r datos$respuesta_gap4`
```

**Regla**: SIEMPRE usar `` `r variable` `` para valores calculados dinámicamente.

### Error 4: No Usar Funciones de Formateo

**❌ Incorrecto:**
```r
# Respuesta numérica sin formateo
respuesta_gap2 <- poblacion_final  # 1500000.8753
```

**Problema**: Decimales espurios, inconsistencia entre versiones, notación científica.

**✓ Correcto:**
```r
# Formateo consistente
formatear_entero <- function(x) {
  format(round(x, 0), big.mark = ".", decimal.mark = ",",
         scientific = FALSE, trim = TRUE)
}

respuesta_gap2 <- round(poblacion_final, 0)  # 1500001
```

**Regla**: Definir y usar formatear_entero() y formato_estandar() consistentemente.

### Error 5: Answerlist para Gaps Numéricos

**❌ Incorrecto:**
```markdown
**b) ¿Cuál es la población final?**

##ANSWER2##

Answerlist
----------
* `r pob_final`
* `r pob_final + 1000`
* `r pob_final - 1000`
```

**Problema**: Gap numérico (num) NO usa Answerlist. Estudiante escribe el número directamente.

**✓ Correcto:**
```markdown
**b) ¿Cuál es la población final? (en habitantes, sin decimales)**

##ANSWER2##

[No incluir Answerlist para gaps tipo num]
```

**Regla**: Answerlist SOLO para gaps tipo schoice/mchoice.

### Error 6: Gaps Desordenados o Mal Numerados

**❌ Incorrecto:**
```markdown
##ANSWER1##
##ANSWER3##  # Falta ANSWER2
##ANSWER2##  # Desordenado
```

**Problema**: R/exams espera gaps en orden secuencial (1, 2, 3, ...).

**✓ Correcto:**
```markdown
##ANSWER1##
##ANSWER2##
##ANSWER3##
```

**Regla**: Numerar correlativamente sin saltos.

### Error 7: No Configurar options(scipen = 999)

**❌ Incorrecto:**
```r
# Sin configuración anti-científica
poblacion <- 1500000
# Se muestra como 1.5e+06 en algunos formatos
```

**Problema**: Notación científica confunde a estudiantes y causa errores de renderizado.

**✓ Correcto:**
```r
# En chunk setup
options(scipen = 999)

# Ahora se muestra como 1.500.000
```

**Regla**: SIEMPRE incluir `options(scipen = 999)` en chunk setup para CLOZE.

## ⛔ CONDICIONES CRÍTICAS

### Pre-generación (OBLIGATORIO verificar ANTES de escribir código)

1. ✓ Análisis ICFES completado (nivel típicamente ≥ 3)
2. ✓ Ejemplo funcional CLOZE similar identificado y leído
3. ✓ Número de gaps definido (2-6)
4. ✓ Tipo de cada gap planificado (schoice|num|string)
5. ✓ Tolerancia por gap calculada según magnitud
6. ✓ Nomenclatura calculada según formato oficial
7. ✓ Carpeta destino creada en `/En-Desarrollo/`

### Durante generación (OBLIGATORIO incluir)

1. ✓ options(scipen = 999) en chunk setup
2. ✓ Funciones formatear_entero() y formato_estandar()
3. ✓ Cálculo separado para CADA gap
4. ✓ Test de diversidad validando TODOS los gaps
5. ✓ Gaps numerados correlativamente (1, 2, 3, ...)
6. ✓ exclozetype con tipos correctos
7. ✓ exsolution con `` para valores dinámicos
8. ✓ extol apropiado por tipo de gap
9. ✓ Answerlist SOLO para gaps schoice/mchoice
10. ✓ Metadatos ICFES completos

### Post-generación (OBLIGATORIO validar)

1. ✓ Renderizado exitoso en 4 formatos (HTML, PDF, DOCX, NOPS)
2. ✓ Todos los gaps funcionan correctamente
3. ✓ Tolerancias permiten respuestas correctas pero rechazan incorrectas
4. ✓ Formato de números consistente (español, sin notación científica)
5. ✓ Test de diversidad > 250 versiones únicas
6. ✓ Coherencia matemática en todas las respuestas
7. ✓ Ciclo de Validación completo (FASE 1→2→3)

**Regla Absoluta**: NO terminar con errores pendientes. CLOZE es más propenso a errores de configuración que SCHOICE.

## 🔗 Referencias y Documentación

### Archivos de Referencia Obligatorios

- **Ejemplos Funcionales CLOZE**: @A-Produccion/Ejemplos-Funcionales-Rmd/ (filtrar por `extype: cloze`)
- **Ejemplos Específicos**: `/06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/09-Probabilidad-Condicionada_Independencia-De-Sucesos/`
- **Nomenclatura**: @.claude/docs/NOMENCLATURA_ARCHIVOS_RMD.md
- **Ciclo Validación**: @.claude/rules/ciclo-validacion.md
- **Metadatos**: @.claude/rules/codigo-rmd.md

### Reglas del Proyecto (OBLIGATORIAS)

- @.claude/rules/codigo-rmd.md - Reglas pre-edit/write
- @.claude/rules/ciclo-validacion.md - FASE 1→2→3 + SUBFASES
- @.claude/rules/documentacion-verificada.md - Principio de verificación

### Documentación Técnica

- @.claude/docs/WORKFLOW_PASO_A_PASO.md - Flujo completo
- @.claude/docs/TROUBLESHOOTING.md - Solución de problemas
- @.claude/docs/TRES_NIVELES_VALIDACION.md - Validación en profundidad

### Documentación R/exams CLOZE

- R/exams cloze documentation: https://www.r-exams.org/intro/dynamic/
- Tipos de gaps: https://www.r-exams.org/tutorials/exams2/

## 🚀 Integración con Otros Skills

Este skill se integra en el workflow ICFES:

```
analizar-icfes
    ↓
[Resultado: tipo=cloze, nivel≥3]
    ↓
generar-cloze ← ESTE SKILL
    ↓
validar-renderizado (FASE 1)
    ↓
[Si errores extol/exsolution] → diagnosticar-errores (FASE 3)
    ↓
[Si errores] → corregir-graficos (SUBFASE 3A)
    ↓
[Volver] → validar-renderizado (SUBFASE 3B)
    ↓
validar-coherencia (FASE 2)
    ↓
[Validar TODOS los gaps]
    ↓
[Si todo OK] → promover-ejercicio
```

**Skills relacionados:**
- `/analizar-icfes` - Prerequisito (análisis ICFES)
- `/generar-schoice` - Alternativa (ejercicios simples)
- `validar-renderizado` - Siguiente paso (FASE 1)
- `diagnosticar-errores` - Corrección (FASE 3, común en CLOZE)
- `/promover-ejercicio` - Final (promoción a producción)

**Diferencias en integración vs SCHOICE:**
- CLOZE tiene más probabilidad de errores en FASE 1 (extol/exclozetype)
- Validación FASE 2 debe verificar coherencia entre TODOS los gaps
- SUBFASE 3A requiere consultar ejemplos CLOZE específicos (menos comunes)

## 📊 Output Final Esperado

Después de usar este skill, debes tener:

```
/A-Produccion/En-Desarrollo/[nombre_ejercicio]/
├── [nombre_completo].Rmd (archivo principal CLOZE)
├── [gráficos].png (si aplica)
└── [archivos auxiliares] (si aplica)
```

**Archivo .Rmd debe:**
- ✓ Compilar en 4 formatos sin errores
- ✓ Generar 250+ versiones únicas
- ✓ Tener 2-6 gaps correctamente configurados
- ✓ exclozetype y extol con número correcto de elementos
- ✓ exsolution con `` para valores dinámicos
- ✓ options(scipen = 999) configurado
- ✓ Funciones de formateo incluidas
- ✓ Metadatos ICFES completos
- ✓ Seguir nomenclatura oficial
- ✓ Estar basado en ejemplos funcionales CLOZE

**Siguiente acción:**
```
# Si compilación exitosa en 4 formatos
/promover-ejercicio [nombre_ejercicio]

# Si hay errores (común en CLOZE)
# → SUBFASE 3A automática (consultar ejemplos CLOZE funcionales)
# → Revisar especialmente: extol, exclozetype, exsolution
# → SUBFASE 3B automática (volver a FASE 1)
```

---

**Última actualización**: 2025-12-30
**Versión**: 2.0 (Progressive Disclosure)
**Basado en**: Claude Code best practices (nov 2025) + R/exams cloze documentation
