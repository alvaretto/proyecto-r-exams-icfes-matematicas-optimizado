---
description: Análisis pedagógico avanzado basado en evidencias científicas y taxonomías cognitivas modernas.
---

# Validador Pedagógico ICFES (2026)

Realiza un análisis pedagógico profundo del ejercicio usando principios científicos del aprendizaje, taxonomías cognitivas modernas y el Marco Conceptual ICFES 2026.

## 🎯 Propósito

Evaluar y optimizar la calidad pedagógica de ejercicios ICFES mediante:
- Clasificación con 3 taxonomías simultáneas (Bloom, SOLO, Webb)
- Validación del Marco Conceptual ICFES (6 dimensiones)
- Análisis avanzado de distractores (6 tipologías de errores)
- Optimización con 7 principios de aprendizaje basados en evidencias
- Puntuación cuantitativa 0-100

## 📊 Los 6 Módulos de Análisis (Módulo 6 opcional)

### Módulo 1: Análisis Cognitivo Multinivel
Clasifica el ejercicio usando 3 taxonomías:
- **Bloom Revisada (2001)**: 6 niveles (Recordar → Crear)
- **SOLO Taxonomy**: 5 niveles estructurales (Pre-estructural → Abstracto extendido)
- **Webb's DOK**: 4 niveles de profundidad (Recall → Extended Thinking)

### Módulo 2: Validación Conceptual ICFES
Verifica las 7 dimensiones obligatorias:
- Competencia (Interpretación, Formulación, Argumentación)
- Componente (Numérico, Espacial-Métrico, Aleatorio, Variacional)
- Afirmación (específica del componente)
- Evidencia (acción observable)
- Nivel (1-4)
- Tarea (descripción en una oración de la labor matemática específica)
- Tipo (SCHOICE, CLOZE)

### Módulo 3: Análisis Avanzado de Distractores
Evalúa calidad de distractores según 6 tipologías:
1. Error Conceptual (misconception)
2. Error Procedimental
3. Sobre-generalización
4. Sub-generalización (rigidez)
5. Fijación Funcional
6. Sesgos Cognitivos

### Módulo 4: Optimización Pedagógica
Aplica 7 principios científicamente validados:
1. Retrieval Practice (Karpicke & Roediger, 2008)
2. Spaced Repetition (Cepeda et al., 2006)
3. Interleaving (Rohrer & Taylor, 2007)
4. Elaborative Interrogation (Dunlosky et al., 2013)
5. Concrete Examples (Bruner, 1966)
6. Dual Coding (Paivio, 1971)
7. Metacognition (Schraw & Dennison, 1994)

### Módulo 5: Meta-evaluación
Genera puntuación compuesta 0-100:
- Alineación Taxonómica (20 pts)
- Validez ICFES (20 pts)
- Calidad de Distractores (25 pts)
- Optimización Pedagógica (20 pts)
- Claridad y Precisión (15 pts)

## 📝 Uso

```bash
/validar-pedagogico [ruta-al-ejercicio.Rmd]
```

**Ejemplo**:
```bash
/validar-pedagogico A-Produccion/03-En-Produccion/.../ejercicio.Rmd
```

## 🔍 Análisis Detallado

El agente realizará:

1. **Lectura del ejercicio** (.Rmd)
2. **Análisis cognitivo multinivel** (3 taxonomías)
3. **Validación ICFES** (6 dimensiones)
4. **Evaluación de distractores** (tipología de errores)
5. **Optimización pedagógica** (7 principios)
6. **Puntuación final** (0-100 con desglose)
7. **Recomendaciones específicas** para mejora

## 📊 Formato de Reporte

```markdown
# ANÁLISIS PEDAGÓGICO: [Nombre del Ejercicio]

## 1. ANÁLISIS COGNITIVO MULTINIVEL

### Bloom Revisada
- Nivel: [1-6]
- Justificación: [...]

### SOLO Taxonomy
- Nivel: [1-5]
- Justificación: [...]

### Webb's DOK
- Nivel: [1-4]
- Justificación: [...]

## 2. VALIDACIÓN ICFES

| Dimensión | Valor | Estado | Observaciones |
|-----------|-------|--------|---------------|
| Competencia | [...] | ✓/✗ | [...] |
| Componente | [...] | ✓/✗ | [...] |
| Afirmación | [...] | ✓/✗ | [...] |
| Evidencia | [...] | ✓/✗ | [...] |
| Nivel | [...] | ✓/✗ | [...] |
| Tarea | [...] | ✓/✗ | [...] |
| Tipo | [...] | ✓/✗ | [...] |

## 3. ANÁLISIS DE DISTRACTORES

| Distractor | Tipo Error | Plausibilidad | Diagnóstico | Calificación |
|-----------|-----------|--------------|-------------|--------------|
| A | [...] | [1-5] | [1-5] | [A+/A/B/C/D/F] |
| B | [...] | [1-5] | [1-5] | [A+/A/B/C/D/F] |
| C | [...] | [1-5] | [1-5] | [A+/A/B/C/D/F] |

## 4. OPTIMIZACIÓN PEDAGÓGICA

Principios aplicados:
- [✓/✗] Retrieval Practice
- [✓/✗] Spaced Repetition
- [✓/✗] Interleaving
- [✓/✗] Elaborative Interrogation
- [✓/✗] Concrete Examples
- [✓/✗] Dual Coding
- [✓/✗] Metacognition

## 5. PUNTUACIÓN FINAL

| Dimensión | Puntos | Máximo |
|-----------|--------|--------|
| Alineación Taxonómica | [X] | 20 |
| Validez ICFES | [X] | 20 |
| Calidad Distractores | [X] | 25 |
| Optimización Pedagógica | [X] | 20 |
| Claridad y Precisión | [X] | 15 |
| **TOTAL** | **[X]** | **100** |

Calificación: [A+/A/B/C/D/F]

## 6. ANÁLISIS TRI (OPCIONAL - Si hay datos de pilotaje)

| Parámetro | Valor Estimado | Objetivo | Estado |
|-----------|----------------|----------|--------|
| Dificultad (b) | [X] | [Rango según nivel] | ✓/✗ |
| Discriminación (a) | [X] | ≥ 1.0 | ✓/✗ |
| Pseudo-azar (c) | [X] | 0.15-0.30 | ✓/✗ |

**Curva Característica del Ítem (CCI)**: [Descripción o gráfico]

## 7. RECOMENDACIONES

### Fortalezas
- [...]
- [...]

### Áreas de Mejora
1. [...]
2. [...]

### Acciones Específicas
- [ ] [...]
- [ ] [...]
```

## 🧠 Base de Conocimiento

El agente consulta automáticamente:
- @.claude/docs/errores-conceptuales-matematicas.md
- @.claude/docs/principios-aprendizaje-evidencias.md
- @.claude/docs/taxonomias-cognitivas-integradas.md
- @.claude/docs/marco-conceptual-icfes-2026.md
- @.claude/docs/diseno-distractores-tipologia.md

## ⚙️ Configuración

**Modelo**: Claude Opus 4.5 (máxima capacidad cognitiva)

**Invocación**: Manual (usuario ejecuta comando)

**Integración**: Consultivo (no bloquea workflow principal)

## 🔗 Workflow Integrado

```
/generar-schoice o /generar-cloze
    │
    ▼
🔄 FASE 1: Validación Automática (renderizado)
    │
    ▼
🔍 FASE 2: Validación Visual (coherencias)
    │
    ▼
📚 /validar-pedagogico (OPCIONAL - Análisis profundo) ← ESTE COMANDO
    │
    ▼
⚡ FASE 3: Correcciones (si necesarias)
    │
    ▼
/promover-ejercicio (si validación exitosa)
```

## 📚 Referencias Científicas

**Base teórica**: 30+ referencias de investigación peer-reviewed
- Cognitive Science (Bloom 2001, Biggs 1982, Webb 1997)
- Learning Sciences (Karpicke 2008, Cepeda 2006, Rohrer 2007)
- Mathematics Education (Clement 1982, Fischbein 1985, Vinner 1980)
- Assessment Design (Haladyna 2002, Radatz 1979)
- ICFES Official Framework (2024-2026)

## ⛔ Diferencia con /analizar-icfes

| Aspecto | /analizar-icfes | /validar-pedagogico |
|---------|----------------|-------------------|
| **Momento** | ANTES de crear .Rmd | DESPUÉS de crear .Rmd |
| **Propósito** | Clasificación inicial | Análisis pedagógico profundo |
| **Input** | Imagen ICFES | Archivo .Rmd completo |
| **Output** | 6 dimensiones básicas | Reporte 5 módulos + puntuación |
| **Profundidad** | Básica | Avanzada (taxonomías, evidencias) |
| **Duración** | ~1 min | ~5-10 min |

---

**Versión**: 1.1.0
**Fecha**: 2026-02-04
**Agente**: PedagogoICFES
**Modelo**: Claude Opus 4.5

**Cambios v1.1**:
- Dimensión "Tarea" agregada (7ª dimensión ICFES)
- Módulo 6 TRI opcional para análisis psicométrico post-pilotaje
- Integración con teoria-respuesta-item.md
