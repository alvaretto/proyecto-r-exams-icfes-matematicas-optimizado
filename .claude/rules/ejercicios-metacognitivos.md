# Regla: Ejercicios Metacognitivos con Progressive Disclosure

## Principio Fundamental

**TODO ejercicio .Rmd (SCHOICE o CLOZE) generado o modificado DEBE ser metacognitivo, aplicando el patrón Progressive Disclosure.**

Esta regla NO tiene excepciones. Ejercicios puramente procedimentales ("calcule X") son insuficientes para desarrollar aprendizaje profundo.

---

## Definición: Ejercicio Metacognitivo

Un ejercicio metacognitivo va más allá del cálculo correcto e incluye:

1. **Identificación de errores** conceptuales en otros razonamientos
2. **Justificación explícita** del por qué una estrategia funciona
3. **Reflexión** sobre el proceso de solución
4. **Verificación** activa de la propia respuesta
5. **Transferencia** a contextos relacionados

### Fundamento Científico

Basado en Schraw & Dennison (1994) y meta-análisis de Dunlosky et al. (2013):

- **Efecto cuantificado**: d = 0.62 (tamaño de efecto grande)
- Estudiantes con alta metacognición tienen mejor desempeño académico
- La metacognición incluye: monitoreo de comprensión, calibración, corrección autogenerada

---

## Definición: Progressive Disclosure

El ejercicio revela información **gradualmente**, requiriendo diferentes niveles cognitivos en secuencia:

```
Nivel 1: Comprensión básica (¿qué pasó?)
    ↓
Nivel 2: Análisis (¿por qué pasó?)
    ↓
Nivel 3: Evaluación (¿es correcto?)
    ↓
Nivel 4: Síntesis/Creación (¿cómo corregirlo?)
```

### Aplicación Práctica

**SCHOICE con Progressive Disclosure:**

```
Pregunta principal: [Evaluar error de otro estudiante]
    ↓
Opciones: [Cada opción = un tipo de error diferente]
    ↓
Solución: [Análisis del error + procedimiento correcto + reflexión]
```

**CLOZE con Progressive Disclosure:**

```
Parte 1 (schoice): Identificar el error conceptual
    ↓
Parte 2 (num): Calcular la respuesta correcta
    ↓
Parte 3 (mchoice): Evaluar afirmaciones relacionadas
    ↓
Parte 4 (schoice V/F): Transferir a caso específico
```

---

## Estructura Obligatoria: SCHOICE Metacognitivo

### Patrón 1: Análisis de Error Ajeno

```markdown
**Enunciado**: [Contexto realista]
[Otro estudiante] resolvió [problema] y obtuvo [respuesta_erronea].

¿Cuál error conceptual cometió [estudiante] para obtener esa respuesta?

A) [Error tipo 1 - descripción clara del error]
B) [Error tipo 2 - descripción clara del error]  ← Correcta
C) [Error tipo 3 - descripción clara del error]
D) [Error tipo 4 - descripción clara del error]

**Solution**:
- Análisis detallado del error identificado
- Procedimiento correcto paso a paso
- Reflexión metacognitiva
- Estrategia para evitar el error
```

### Patrón 2: Evaluación de Afirmación

```markdown
**Enunciado**: [Contexto realista]
[Persona] afirma: "[Afirmación matemática potencialmente incorrecta]"

¿Por qué esta afirmación es [CORRECTA/INCORRECTA]?

A) [Justificación superficial]
B) [Justificación con causa raíz correcta]  ← Correcta
C) [Confusión conceptual]
D) [Error de terminología]

**Solution**:
- Análisis de la afirmación
- Justificación matemática rigurosa
- Contraejemplo o demostración
- Reflexión sobre el concepto
```

### Patrón 3: Comparación de Procedimientos

```markdown
**Enunciado**: [Contexto realista]
Tres estudiantes resolvieron [problema]:

- Estudiante A: [Procedimiento A]
- Estudiante B: [Procedimiento B]
- Estudiante C: [Procedimiento C]

¿Cuál estudiante aplicó correctamente [concepto]?

A) Solo A
B) Solo B  ← Correcta
C) A y C
D) Ninguno

**Solution**:
- Análisis de cada procedimiento
- Identificación de errores en cada uno
- Procedimiento correcto
- Generalización del concepto
```

---

## Estructura Obligatoria: CLOZE Metacognitivo

### Mínimo 4 Partes con Progressive Disclosure

```r
# Parte 1 (schoice): IDENTIFICAR
# DOK 2-3, Bloom: Analizar
"¿Cuál error conceptual cometió [persona]?"

# Parte 2 (num): CALCULAR
# DOK 2, Bloom: Aplicar
"¿Cuál es el valor correcto?"

# Parte 3 (mchoice): EVALUAR
# DOK 3, Bloom: Evaluar
"Seleccione las afirmaciones correctas sobre [concepto]."

# Parte 4 (schoice V/F): TRANSFERIR
# DOK 3, Bloom: Analizar/Evaluar
"La siguiente afirmación es verdadera o falsa: [caso específico]"
```

### Metadatos Obligatorios CLOZE Metacognitivo

```yaml
extype: cloze
exclozetype: schoice|num|mchoice|schoice
exextra[DOK]: 3
exextra[Bloom]: Evaluar
exextra[SOLO]: Relacional-Extendido
```

---

## Pool de Errores Conceptuales (OBLIGATORIO)

Todo ejercicio metacognitivo DEBE incluir un pool de errores conceptuales documentados:

```r
errores_conceptuales <- list(
  list(
    codigo = "XXX-YYY-01",          # Código único
    nombre = "Nombre descriptivo",   # Para referencia
    descripcion_corta = "...",       # Para opciones
    descripcion_larga = "...",       # Para solución
    causa_raiz = "...",              # Diagnóstico pedagógico
    calcula = function(...) { ... }  # Produce el distractor
  ),
  # Mínimo 4-6 errores por ejercicio
)
```

### Taxonomía de Códigos de Error

| Prefijo | Área | Ejemplo |
|---------|------|---------|
| ALG | Álgebra | ALG-OPE-01 (Inversión operación) |
| ARI | Aritmética | ARI-FRA-01 (Suma fracciones incorrecta) |
| EST | Estadística | EST-MTC-01 (Confusión promedio-valor) |
| GEO | Geometría | GEO-ARE-01 (Confusión área-perímetro) |
| FUN | Funciones | FUN-PEN-01 (Confusión pendiente-intercepto) |

---

## Pool de Reflexiones Metacognitivas (OBLIGATORIO)

Todo ejercicio DEBE incluir reflexiones aleatorias:

```r
reflexiones_metacognitivas <- list(
  "Identificar errores ajenos nos ayuda a evitar cometerlos. La metacognición es fundamental.",
  "Analizar por qué una respuesta es incorrecta fortalece la comprensión profunda.",
  "Los errores más frecuentes son: [lista específica del tema].",
  "Cuando identificamos el tipo de error, podemos diseñar estrategias para evitarlo."
)

reflexion <- reflexiones_metacognitivas[[sample(length(reflexiones_metacognitivas), 1)]]
```

---

## Verificaciones Obligatorias en `data_generation`

```r
# 1. Verificar que respuesta errónea ≠ respuesta correcta
test_that("Error analizado produce respuesta errónea diferente de correcta", {
  expect_true(respuesta_erronea != valor_correcto)
})

# 2. Verificar que distractores son únicos
test_that("Distractores son únicos", {
  expect_equal(length(unique(distractores)), length(distractores))
})

# 3. Verificar coherencia de error conceptual
test_that("Error conceptual es reproducible", {
  resultado <- error_seleccionado$calcula(...)
  expect_equal(resultado, respuesta_erronea)
})
```

---

## Sección Solution Obligatoria

La solución DEBE incluir TODAS estas subsecciones:

```markdown
### Análisis del Error (Parte 1)
**Error identificado:** [descripcion_larga]
**Código de error:** [codigo]
**Causa raíz:** [causa_raiz]

### Procedimiento Correcto (Parte 2)
**Paso 1:** [Descripción + fórmula LaTeX]
$$...$$

**Paso 2:** [Descripción + fórmula LaTeX]
$$...$$

[Continuar hasta el resultado]

### Propiedades del Concepto (Parte 3)
- Afirmación 1: [VERDADERA/FALSA] porque...
- Afirmación 2: [VERDADERA/FALSA] porque...

### Caso Específico (Parte 4)
[Enunciado] → **[Verdadero/Falso]** porque...

### Reflexión Metacognitiva
`r reflexion`

### Estrategia para Evitar el Error
1. [Paso preventivo 1]
2. [Paso preventivo 2]
3. [Verificación final]
```

---

## Integración con Otros Principios

### Con Retrieval Practice

```markdown
# NO dar fórmulas visibles
❌ "Usando la fórmula A = b × h, calcula..."
✓ "¿Por qué el estudiante que usó A = b + h obtuvo un resultado incorrecto?"
```

### Con Dual Coding

```markdown
# Incluir representación visual + verbal
- Tabla con datos
- Gráfico TikZ si aplica
- Descripción textual
- Fórmulas matemáticas
```

### Con Concrete Examples

```markdown
# Pool de contextos narrativos variados
contextos <- list(
  list(rol = "profesora", contexto = "calificaciones", unidad = "puntos"),
  list(rol = "entrenador", contexto = "puntos anotados", unidad = "puntos"),
  # Mínimo 6-8 contextos por ejercicio
)
```

---

## Antipatrones PROHIBIDOS

### 1. Ejercicio Puramente Procedimental (PROHIBIDO)

```markdown
❌ "Calcula el área de un rectángulo con base 8 cm y altura 5 cm."
```

**Por qué es malo**: No hay metacognición, solo cálculo mecánico.

### 2. Distractores Aleatorios (PROHIBIDO)

```r
❌ distractores <- respuesta + sample(-10:10, 3)
```

**Por qué es malo**: No representa errores conceptuales reales.

### 3. Solución Sin Análisis de Error (PROHIBIDO)

```markdown
❌ Solution
========
La respuesta correcta es 40.
```

**Por qué es malo**: No explica errores ni desarrolla metacognición.

### 4. Sin Pool de Errores Documentado (PROHIBIDO)

```r
❌ distractor1 <- respuesta - 5  # ¿Qué error representa?
```

**Por qué es malo**: No hay diagnóstico pedagógico.

---

## Checklist Pre-Generación

Antes de generar cualquier .Rmd:

- [ ] ¿Incluye pool de errores conceptuales con códigos?
- [ ] ¿Cada error tiene `descripcion_corta`, `descripcion_larga`, `causa_raiz`?
- [ ] ¿Hay función `calcula` para cada error?
- [ ] ¿Incluye pool de reflexiones metacognitivas?
- [ ] ¿La estructura es Progressive Disclosure (fácil → difícil)?
- [ ] ¿Solution incluye análisis de error + procedimiento + reflexión?
- [ ] ¿Hay verificaciones test_that para coherencia?

---

## Checklist Post-Generación

Después de generar el .Rmd:

- [ ] ¿Respuesta errónea ≠ respuesta correcta?
- [ ] ¿Distractores son únicos entre sí?
- [ ] ¿Metadatos incluyen DOK, Bloom, SOLO?
- [ ] ¿Nivel DOK ≥ 2 (preferible 3)?
- [ ] ¿Bloom incluye Analizar/Evaluar?
- [ ] ¿Solución tiene todas las subsecciones obligatorias?

---

## Metadatos Obligatorios Adicionales

```yaml
# Taxonomías cognitivas (OBLIGATORIAS)
exextra[DOK]: [2|3|4]              # Webb's Depth of Knowledge
exextra[Bloom]: [Analizar|Evaluar] # Taxonomía Bloom Revisada
exextra[SOLO]: [Relacional|Abstracto-Extendido]  # Estructura SOLO

# Tipo de metacognición
exextra[TipoMetacognicion]: [analisis_error|evaluacion_afirmacion|comparacion_procedimientos]
```

---

## Ejemplo Completo Mínimo

Ver archivo de referencia:
`A-Produccion/03-En-Produccion/.../promedios_borrados_metacognitivo_argumentacion_n3_cloze_v1.Rmd`

---

**Versión**: 1.0
**Fecha**: 2026-02-06
**Estado**: ACTIVO Y OBLIGATORIO
**Excepciones**: NINGUNA

**Fundamento científico**:
- Schraw & Dennison (1994) - Metacognitive awareness
- Dunlosky et al. (2013) - Learning techniques meta-analysis
- Anderson & Krathwohl (2001) - Bloom's Taxonomy Revised
- Webb (1997) - Depth of Knowledge
