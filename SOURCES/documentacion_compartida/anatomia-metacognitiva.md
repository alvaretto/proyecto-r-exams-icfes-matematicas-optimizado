# Anatomía de un Archivo .Rmd Metacognitivo

Un archivo metacognitivo válido tiene exactamente **8 secciones** en este orden:

```
1. ENCABEZADO YAML (output + taxonomías cognitivas)
2. CHUNK setup (librerías + configuración)
3. CHUNK data_generation (pool de errores + aleatorización)
4. CHUNK version_diversity_test (validación 200+ versiones)
5. CHUNK validaciones_matematicas (test_that obligatorios)
6. SECCIÓN Question (patrón metacognitivo + Answerlist)
7. SECCIÓN Solution (análisis de error + reflexión + Answerlist)
8. META-INFORMATION (metadatos R/exams + ICFES + taxonomías cognitivas)
```

---

## Sección 1: Encabezado YAML (Extendido)

```yaml
---
output:
  html_document: default
  word_document: default
  pdf_document:
    keep_tex: true
    extra_dependencies:
      - "graphicx"
      - "float"
      - "amsmath"
icfes:
  competencia: argumentacion        # OBLIGATORIO para metacognitivo
  nivel_dificultad: 3               # Mínimo 2, típico 3
  contenido:
    categoria: [tema_principal]
    tipo: [subtema]
  contexto: [escolar|cotidiano|cientifico]
  eje_axial: [eje1-eje6]
  componente: [aleatorio|geometrico_metrico|numerico_variacional]
---
```

**Diferencias con anatomía básica:**

- Incluye sección `icfes:` con metadatos estructurados
- `competencia: argumentacion` es típica para ejercicios metacognitivos
- `nivel_dificultad: 3` o superior (metacognición = mayor complejidad)

---

## Sección 2: Chunk setup

```r
```{r setup, include=FALSE}
Sys.setlocale(category = "LC_NUMERIC", locale = "C")
options(OutDec = ".")

library(exams)
library(digest)    # Para test de diversidad
library(testthat)  # Para validaciones

typ <- match_exams_device()
options(scipen = 999)
knitr::opts_chunk$set(
  warning = FALSE,
  message = FALSE,
  fig.showtext = FALSE,
  fig.cap = "",
  fig.keep = 'all',
  dev = c("png", "pdf"),
  dpi = 150
)
```
```

**Diferencias:**

- Incluye `library(testthat)` para validaciones
- Incluye `library(digest)` para hash de versiones
- Configuración de locale para consistencia numérica

---

## Sección 3: Chunk data_generation (CRÍTICO)

Esta sección es la más diferente y requiere componentes específicos:

### 3.1 Pool de Contextos Narrativos

```r
```{r data_generation, echo=FALSE, results="hide"}
options(OutDec = ".")

generar_datos <- function() {

  # ============================================================
  # POOL DE CONTEXTOS NARRATIVOS (mínimo 6-8)
  # Variabilidad superficial: mismo problema, diferentes escenarios
  # ============================================================
  contextos <- list(
    list(
      rol = "profesora de estadística",
      protagonista_pool = c("Mariana", "Claudia", "Patricia"),
      contexto_dato = "las calificaciones del examen final",
      unidad = "puntos",
      rango_min = 50,
      rango_max = 100,
      genero_prot = "f"
    ),
    list(
      rol = "entrenador de baloncesto",
      protagonista_pool = c("Carlos", "Andrés", "Miguel"),
      contexto_dato = "los puntos anotados en la temporada",
      unidad = "puntos",
      rango_min = 10,
      rango_max = 35,
      genero_prot = "m"
    )
    # ... mínimo 6-8 contextos
  )
```

### 3.2 Pool de Errores Conceptuales (OBLIGATORIO)

```r
  # ============================================================
  # POOL DE ERRORES CONCEPTUALES CON DIAGNÓSTICO PEDAGÓGICO
  # ============================================================
  errores_conceptuales <- list(
    list(
      codigo = "EST-MTC-01",
      nombre = "Confusión promedio-valor",
      descripcion_corta = "Confundió el promedio con la respuesta buscada",
      descripcion_larga = "Confundió el promedio dado en el enunciado con el valor individual que debía calcular. Este error indica que no comprende la diferencia entre una medida de tendencia central y los datos individuales.",
      causa_raiz = "No diferencia entre estadístico resumen y dato individual",
      calcula = function(promedio, suma_total, suma_conocidas, n_total, n_desconocidos) {
        promedio  # Retorna el valor erróneo
      }
    ),
    list(
      codigo = "EST-MTC-02",
      nombre = "Omisión de división",
      descripcion_corta = "Olvidó dividir la suma de valores desconocidos",
      descripcion_larga = "Calculó correctamente la suma de los valores desconocidos, pero olvidó dividir entre el número de datos faltantes.",
      causa_raiz = "Procedimiento incompleto - falta último paso",
      calcula = function(promedio, suma_total, suma_conocidas, n_total, n_desconocidos) {
        suma_total - suma_conocidas  # No divide
      }
    )
    # ... mínimo 4-6 errores
  )
```

**Estructura de cada error:**

| Campo | Descripción | Uso |
|-------|-------------|-----|
| `codigo` | Identificador único (XXX-YYY-NN) | Referencia y debugging |
| `nombre` | Nombre corto descriptivo | Documentación |
| `descripcion_corta` | 1 oración (max 80 chars) | Opciones de respuesta |
| `descripcion_larga` | Explicación detallada | Sección Solution |
| `causa_raiz` | Diagnóstico pedagógico | Reflexión metacognitiva |
| `calcula` | Función que produce el distractor | Generación de opciones |

### 3.3 Pool de Reflexiones Metacognitivas

```r
  # ============================================================
  # POOL DE REFLEXIONES METACOGNITIVAS
  # ============================================================
  reflexiones_metacognitivas <- list(
    "Identificar errores ajenos nos ayuda a evitar cometerlos nosotros mismos.",
    "Analizar por qué una respuesta es incorrecta fortalece la comprensión.",
    "Los errores más frecuentes en este tipo de problema son: [lista específica].",
    "Cuando identificamos el tipo de error, podemos diseñar estrategias para evitarlo."
  )
```

### 3.4 Generación con Validación de Restricciones

```r
  # ============================================================
  # GENERACIÓN DE DATOS CON RESTRICCIONES
  # ============================================================

  # Seleccionar contexto aleatorio
  ctx <- contextos[[sample(length(contextos), 1)]]
  protagonista <- sample(ctx$protagonista_pool, 1)

  # Generar datos válidos con restricciones
  datos_validos <- FALSE
  intentos <- 0
  max_intentos <- 1000

  while(!datos_validos && intentos < max_intentos) {
    intentos <- intentos + 1

    # [Lógica de generación específica del problema]

    # Verificar restricciones
    if(condicion_1 && condicion_2 && ...) {
      datos_validos <- TRUE
    }
  }

  if(!datos_validos) {
    stop("No se pudieron generar datos válidos después de ", max_intentos, " intentos")
  }
```

### 3.5 Selección de Error y Construcción de Opciones

```r
  # ============================================================
  # SELECCIONAR ERROR A ANALIZAR
  # ============================================================
  error_seleccionado_idx <- sample(1:min(3, length(errores_conceptuales)), 1)
  error_seleccionado <- errores_conceptuales[[error_seleccionado_idx]]

  respuesta_erronea <- error_seleccionado$calcula(
    promedio, suma_total, suma_conocidas, n_total, n_desconocidos
  )

  # Asegurar que respuesta errónea ≠ correcta
  if(respuesta_erronea == valor_correcto) {
    error_seleccionado_idx <- 2  # Usar otro error
    error_seleccionado <- errores_conceptuales[[error_seleccionado_idx]]
    respuesta_erronea <- error_seleccionado$calcula(...)
  }
```

### 3.6 Return Completo

```r
  # ============================================================
  # RETORNAR LISTA COMPLETA
  # ============================================================
  return(list(
    # Contexto
    ctx = ctx,
    protagonista = protagonista,

    # Datos del problema
    valores = ...,
    respuesta_correcta = ...,

    # Error analizado
    error_seleccionado = error_seleccionado,
    respuesta_erronea = respuesta_erronea,

    # Opciones y soluciones
    opciones = ...,
    sol = ...,

    # Reflexión
    reflexion = reflexiones_metacognitivas[[sample(length(reflexiones_metacognitivas), 1)]]
  ))
}

# Ejecutar generación
datos <- generar_datos()

# Extraer variables
# [Variables extraídas para uso en documento]
```
```

---

## Sección 4: Chunk version_diversity_test

```r
```{r version_diversity_test, echo=FALSE, results="hide"}
options(OutDec = ".")

test_that("Prueba de diversidad de versiones", {
  versiones <- list()
  for (i in 1:500) {
    datos_test <- generar_datos()
    hash_datos <- digest::digest(list(
      contexto = datos_test$ctx$rol,
      protagonista = datos_test$protagonista,
      # ... otros campos clave
      error_codigo = datos_test$error_seleccionado$codigo
    ))
    versiones[[i]] <- hash_datos
  }

  n_versiones_unicas <- length(unique(versiones))
  expect_true(n_versiones_unicas >= 200,
              info = paste("Solo se generaron", n_versiones_unicas,
                          "versiones únicas. Se requieren al menos 200."))
})
```
```

**Umbral reducido a 200** para ejercicios con restricciones fuertes (errores conceptuales limitados, contextos específicos).

---

## Sección 5: Chunk validaciones_matematicas (NUEVO)

```r
```{r validaciones_matematicas, echo=FALSE, results="hide"}
options(OutDec = ".")

test_that("Datos del problema son consistentes", {
  # Validar coherencia matemática
  expect_equal(suma_total, promedio * num_estudiantes)
  expect_equal(suma_conocidas, sum(valores_conocidos))
  # ... otras validaciones
})

test_that("Exactamente una opción correcta", {
  expect_equal(sum(sol), 1)
  expect_equal(length(sol), 4)
})

test_that("Respuesta errónea diferente de correcta", {
  expect_true(respuesta_erronea != valor_correcto)
})

test_that("Distractores son únicos", {
  distractores <- c(respuesta_erronea, otros_distractores)
  expect_equal(length(unique(distractores)), length(distractores))
})
```
```

---

## Sección 6: Question (Patrón Metacognitivo)

### Patrón 1: Análisis de Error Ajeno

```markdown
Question
========

`r protagonista`, `r ctx$rol`, [contexto realista con datos dinámicos].

[Tabla o gráfico con datos - usando TikZ o R]

Un compañero de `r protagonista` intentó resolver el problema y obtuvo como respuesta **`r respuesta_erronea` `r ctx$unidad`**.

**¿Cuál error conceptual cometió el compañero para obtener esa respuesta?**

Answerlist
----------
* `r opciones[1]`
* `r opciones[2]`
* `r opciones[3]`
* `r opciones[4]`
```

### Patrón 2: Evaluación de Afirmación

```markdown
Question
========

`r protagonista` afirma: "`r afirmacion`"

¿Por qué esta afirmación es **`r ifelse(es_correcta, "CORRECTA", "INCORRECTA")`**?

Answerlist
----------
* `r opciones[1]`
* `r opciones[2]`
* `r opciones[3]`
* `r opciones[4]`
```

### Patrón 3: Comparación de Procedimientos

```markdown
Question
========

Tres estudiantes resolvieron `r problema`:

- **Estudiante A**: `r procedimiento_A`
- **Estudiante B**: `r procedimiento_B`
- **Estudiante C**: `r procedimiento_C`

¿Cuál estudiante aplicó correctamente `r concepto`?

Answerlist
----------
* Solo A
* Solo B
* A y C
* Ninguno
```

---

## Sección 7: Solution (Análisis Completo)

```markdown
Solution
========

### Análisis del Error

El compañero respondió **`r respuesta_erronea` `r ctx$unidad`**, cuando la respuesta correcta es **`r valor_correcto` `r ctx$unidad`**.

**Error identificado:** `r error_seleccionado$descripcion_larga`

**Código de error:** `r error_seleccionado$codigo`

**Causa raíz:** `r error_seleccionado$causa_raiz`

### Procedimiento Correcto

**Paso 1:** [Descripción del primer paso]

$$[Fórmula LaTeX]$$

**Paso 2:** [Descripción del segundo paso]

$$[Fórmula LaTeX]$$

[Continuar hasta el resultado]

### Reflexión Metacognitiva

`r reflexion`

### Estrategia para Evitar el Error

1. [Paso preventivo 1]
2. [Paso preventivo 2]
3. [Verificación final]

Answerlist
----------
* `r if(sol[1] == 1) "Correcto. Este es el error que cometió el compañero." else "Incorrecto."`
* `r if(sol[2] == 1) "Correcto. Este es el error que cometió el compañero." else "Incorrecto."`
* `r if(sol[3] == 1) "Correcto. Este es el error que cometió el compañero." else "Incorrecto."`
* `r if(sol[4] == 1) "Correcto. Este es el error que cometió el compañero." else "Incorrecto."`
```

---

## Sección 8: Meta-information (Extendida)

```markdown
Meta-information
================
exname: [ejercicio]_[componente]_[competencia]_n[nivel]_schoice_v1
extype: schoice
exsolution: `r paste(sol, collapse = "")`
exshuffle: TRUE
extol: 0.01
exsection: [Area]|[Subarea]|[Tema]

exextra[Type]: SCHOICE
exextra[Program]: R
exextra[Language]: es

# Metadatos ICFES (6 dimensiones)
exextra[Competencia]: Argumentacion
exextra[Componente]: [Aleatorio|Geometrico-Metrico|Numerico-Variacional]
exextra[Afirmacion]: [Descripción específica]
exextra[Evidencia]: [Descripción específica]
exextra[Nivel]: [2|3|4]
exextra[Contexto]: [Escolar|Cotidiano|Cientifico]

# Taxonomías cognitivas (OBLIGATORIAS para metacognitivo)
exextra[DOK]: [2|3|4]
exextra[Bloom]: [Analizar|Evaluar|Crear]
exextra[SOLO]: [Relacional|Abstracto-Extendido]

# Tipo de metacognición
exextra[TipoMetacognicion]: [analisis_error|evaluacion_afirmacion|comparacion_procedimientos]
```

---

## Checklist de Anatomía Metacognitiva

### Pre-generación

- [ ] Pool de contextos narrativos (mínimo 6)
- [ ] Pool de errores conceptuales con códigos (mínimo 4)
- [ ] Pool de reflexiones metacognitivas (mínimo 4)
- [ ] Función generar_datos() completa

### Estructura

- [ ] 8 secciones en orden correcto
- [ ] Chunk validaciones_matematicas presente
- [ ] Question con patrón metacognitivo
- [ ] Solution con todas las subsecciones

### Metadatos

- [ ] 6 dimensiones ICFES presentes
- [ ] DOK, Bloom, SOLO presentes
- [ ] TipoMetacognicion especificado
- [ ] exshuffle: TRUE

### Validaciones

- [ ] Test de diversidad pasa (≥200 versiones)
- [ ] Respuesta errónea ≠ correcta
- [ ] Distractores únicos
- [ ] Coherencia matemática validada

---

**Versión**: 1.0
**Fecha**: 2026-02-06
**Referencia**: .claude/rules/ejercicios-metacognitivos.md
