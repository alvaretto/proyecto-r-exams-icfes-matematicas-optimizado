# Reglas Críticas del Sistema (OBLIGATORIAS)

## ⛔ Resumen Ejecutivo

**Todas estas reglas son OBLIGATORIAS y NO tienen excepciones.**

### Las 8 Reglas Fundamentales

1. **Ejercicios metacognitivos** con Progressive Disclosure
2. **Flujo B obligatorio** cuando hay gráficos
3. **Proceso secuencial** TikZ→Python→R (no simultáneo)
4. **5 Coherencias** a verificar en cada validación
5. **Validación visual iterativa** con inspección REAL
6. **Ortografía española** con tildes correctas
7. **Testing automático** permanente con tolerancia cero
8. **Detractor obligatorio** en todas las fases de revisión 🆕

---

## 1. Ejercicios Metacognitivos - OBLIGATORIO 🆕

**Regla detallada**: @.claude/rules/ejercicios-metacognitivos.md

### Principio Fundamental

**TODO ejercicio .Rmd (SCHOICE, CLOZE) DEBE ser metacognitivo con Progressive Disclosure.**

Los ejercicios puramente procedimentales ("calcula X") están **PROHIBIDOS**.

### Qué es un Ejercicio Metacognitivo

Un ejercicio metacognitivo va más allá del cálculo:
- Requiere **identificar errores** de otros
- Exige **justificar** por qué algo es correcto/incorrecto
- Incluye **reflexión** sobre el proceso de solución
- Aplica **verificación** de resultados

### Patrones Metacognitivos Obligatorios

| Patrón | Descripción | Bloom |
|--------|-------------|-------|
| **Análisis de Error Ajeno** | "Juan obtuvo X. ¿Cuál fue su error?" | Analizar |
| **Evaluación de Afirmación** | "María afirma Y. ¿Por qué es incorrecta?" | Evaluar |
| **Comparación de Procedimientos** | "¿Cuál estudiante aplicó correctamente Z?" | Analizar |

### Progressive Disclosure (CLOZE)

Todo ejercicio CLOZE DEBE tener **mínimo 4 partes**:

```
Parte 1 (schoice): IDENTIFICAR el error conceptual
    ↓ Bloom: Analizar | DOK: 3
Parte 2 (num): CALCULAR la respuesta correcta
    ↓ Bloom: Aplicar | DOK: 2
Parte 3 (mchoice): EVALUAR afirmaciones sobre el concepto
    ↓ Bloom: Evaluar | DOK: 3
Parte 4 (schoice V/F): TRANSFERIR a caso específico
    | Bloom: Analizar/Evaluar | DOK: 3
```

### Pool de Errores Conceptuales (OBLIGATORIO)

Todo ejercicio DEBE definir un pool de errores con:

```r
errores_conceptuales <- list(
  list(
    codigo = "XXX-YYY-01",          # Ej: EST-MTC-01
    nombre = "Nombre descriptivo",
    descripcion_corta = "...",       # Para opciones (max 80 chars)
    descripcion_larga = "...",       # Para solución
    causa_raiz = "...",              # Diagnóstico pedagógico
    calcula = function(...) { ... }  # Función que produce el distractor
  )
)
```

### Metadatos Cognitivos OBLIGATORIOS

```yaml
exextra[DOK]: [2|3|4]              # Webb's Depth of Knowledge
exextra[Bloom]: [Analizar|Evaluar]  # Taxonomía de Bloom
exextra[SOLO]: [Relacional|...]     # Taxonomía SOLO
exextra[TipoMetacognicion]: [analisis_error|evaluacion_afirmacion|...]
```

### Sección Solution OBLIGATORIA

```markdown
Solution
========

### Análisis del Error
**Error identificado:** [descripcion_larga]
**Código de error:** [codigo]
**Causa raíz:** [causa_raiz]

### Procedimiento Correcto
**Paso 1:** [descripción + fórmula LaTeX]
...

### Reflexión Metacognitiva
[reflexión aleatoria del pool]
```

### Antipatrones PROHIBIDOS

```markdown
❌ PROHIBIDO: Ejercicio puramente procedimental
"Calcula el área de un rectángulo con base 8 cm"

✓ CORRECTO: Ejercicio metacognitivo
"Un estudiante calculó 8 + 5 = 13 como área. ¿Cuál fue su error?"
```

```r
❌ PROHIBIDO: Distractores aleatorios
distractores <- respuesta + sample(-10:10, 3)

✓ CORRECTO: Distractores basados en errores conceptuales
distractores <- sapply(errores_conceptuales, function(e) e$calcula(...))
```

---

## 2. Flujo B (Graficador Experto) - OBLIGATORIO

**Regla detallada**: @.claude/rules/flujo-b-obligatorio.md

### Principio Fundamental

**SIEMPRE que se detecten gráficos en un ejercicio ICFES, el Flujo B es OBLIGATORIO.**

### Detección Automática

El sistema detecta gráficos en:
- Enunciados con imágenes matemáticas
- Opciones de respuesta con diagramas
- Referencias a "gráfica", "diagrama", "figura", "tabla"

### Bloqueo de Generación

```
SI ejercicio_tiene_graficos AND NOT flujo_b_completado:
    BLOQUEAR generación de .Rmd
    MOSTRAR mensaje de error
    REDIRIGIR a Flujo B
```

### Archivos Requeridos

```
outputs/[nombre_ejercicio]/
├── workflow_state.json          # Estado del flujo
├── output_tikz_vN.tex           # Versión TikZ final
├── output_python_vN.py          # Versión Python final
├── output_r_vN.R                # Versión R final
├── tikz_output_vN.png           # Preview TikZ
├── python_output_vN.png         # Preview Python
└── r_output_vN.png              # Preview R
```

**NO hay excepciones**. Si hay gráficos, hay Flujo B.

---

## 3. Proceso Secuencial del Graficador

**Regla detallada**: @.claude/rules/graficador-secuencial.md

### Orden OBLIGATORIO

```
1. TikZ (dinámico desde R)
   ↓ Iterar hasta ≥95% similitud + coherencias + aprobación usuario
   ↓
2. Python (vía reticulate)
   ↓ Iterar hasta ≥95% similitud + coherencias + aprobación usuario
   ↓
3. R (nativo ggplot2)
   ↓ Iterar hasta ≥95% similitud + coherencias + aprobación usuario
   ↓
4. Selección final por usuario
```

### PROHIBIDO: Generación Simultánea

```r
# ❌ INCORRECTO - NO HACER
generar_tikz() AND generar_python() AND generar_r()  # Simultáneo

# ✓ CORRECTO - Secuencial con aprobación
generar_tikz() → aprobar_tikz()
→ generar_python() → aprobar_python()
→ generar_r() → aprobar_r()
→ seleccionar_version()
```

### Estados del Workflow

```json
{
  "tikz": {
    "estado": "pendiente|en_iteracion|verificando|aprobado",
    "similitud_actual": 0,
    "usuario_aprobo": false
  },
  "python": {
    "estado": "bloqueado|pendiente|en_iteracion|verificando|aprobado"
  },
  "r": {
    "estado": "bloqueado|pendiente|en_iteracion|verificando|aprobado"
  }
}
```

---

## 4. Las 5 Coherencias (Verificación Obligatoria)

**Todas deben verificarse ANTES de aprobar cualquier ejercicio.**

### 1. Coherencia Semántica (Gramática)
- Texto en español correcto
- **TILDES OBLIGATORIAS**: más, ángulo, función, gráfica, dispersión
- Sin errores ortográficos
- Terminología matemática apropiada
- Redacción clara estilo ICFES

### 2. Coherencia Visual-Texto
- Gráfico coincide EXACTAMENTE con enunciado
- Valores en gráfico = valores en texto
- Etiquetas consistentes con la pregunta
- Colores/estilos descritos coinciden con renderizado
- Sin contradicciones visuales

### 3. Coherencia Matemática
- Fórmulas correctas y bien formateadas
- Cálculos verificables paso a paso
- Proporciones y escalas correctas
- Respuesta correcta matemáticamente válida
- Distractores plausibles pero incorrectos
- Sin NaN, Inf, errores numéricos

### 4. Coherencia de Código
- Código dinámico (variables aleatorias, NO hardcoded)
- Compatible con R-exams en 4 formatos (HTML/PDF/DOCX/NOPS)
- Sin dependencias externas no declaradas
- Gráficos generados programáticamente
- Variables R interpoladas correctamente en TikZ/Python
- Diferentes semillas generan ejercicios válidos

### 5. Coherencia General
- Legible en todos los formatos
- Estilo visual consistente ICFES
- Dificultad apropiada al nivel (n1-n4)
- Tiempo de resolución razonable
- Opciones visibles y distinguibles
- Sin elementos confusos

### Checklist de Verificación

```markdown
## Verificación de Coherencias - [Nombre Ejercicio]

- [ ] 1. Semántica: ¿Gramática y ortografía correctas?
- [ ] 2. Visual-Texto: ¿Gráfico coincide con enunciado?
- [ ] 3. Matemática: ¿Fórmulas y cálculos correctos?
- [ ] 4. Código: ¿Dinámico y compatible R-exams?
- [ ] 5. General: ¿Legible y estilo ICFES apropiado?

### Problemas detectados:
[Lista de problemas si existen]

### Acción:
- [ ] Aprobar ejercicio
- [ ] Corregir y volver a validar
```

---

## 5. Validación Visual Iterativa (OBLIGATORIA)

**Regla detallada**: @.claude/rules/ciclo-validacion.md

### Principio Fundamental

**NUNCA marcar como "completado" sin inspección visual REAL.**

### Ciclo Obligatorio

```
1. Renderizar → exams2pdf/html/docx/nops
2. FASE 2A: Validación matemática [AUTOMÁTICA vía hook]
3. FASE 2B: Convertir PDF → PNG [AUTOMÁTICA vía hook]
4. Claude: Read() cada PNG generado
5. Claude: Verificar 5 coherencias VISUALMENTE
6. Claude: Documentar hallazgos con checklist
7. Claude: Solicitar aprobación del usuario
8. ¿Problemas? → Corregir → VOLVER A PASO 1
```

### ⚠️ REGLA CRÍTICA: REPETIR DESPUÉS DE CADA CAMBIO

**Cada vez que se aplica CUALQUIER corrección:**
- VOLVER A RENDERIZAR
- MOSTRAR NUEVO PREVIEW
- VERIFICAR RESULTADO VISUAL
- NUNCA asumir éxito sin verificación

### PROHIBIDO

❌ "El PDF se generó correctamente" sin mostrar imagen
❌ Asumir éxito solo porque no hubo errores de compilación
❌ Saltarse comparación visual con imagen original
❌ Aplicar cambios sin volver a mostrar el resultado

### Herramientas Automáticas

```bash
# FASE 2A (automática vía hook)
Rscript .claude/scripts/validar_coherencia_matematica.R archivo.Rmd

# FASE 2B (automática vía hook)
magick -density 150 archivo.pdf -quality 90 preview.png

# Claude DEBE entonces:
Read("preview.png")  # Mostrar al usuario
# Verificar 5 coherencias
# Documentar y solicitar aprobación
```

---

## 6. Ortografía Española (OBLIGATORIA)

**Regla detallada**: @.claude/rules/ortografia-espanol.md

### Principio Fundamental

**TODO texto en español DEBE incluir tildes correctas.**

### Palabras Frecuentes con Tilde

```
más  según  así  después  también  además  aquí  ahí

ángulo  gráfica  gráfico  función  número  cálculo  método
código  propósito  patrón  máximo  mínimo  análisis  éxito

dispersión  solución  ecuación  relación  variación
descripción  información  configuración  clasificación
```

### Excepciones (ASCII Obligatorio)

**Metadatos R-exams** NUNCA llevan tildes:
```yaml
exname: nombre_sin_tildes
exsection: Numerico-Variacional/Argumentacion
extype: schoice
exsolution: 1000
exextra[Competencia]: Interpretacion  # Sin tilde
exextra[Componente]: Aleatorio
```

**Variables R**: `angulos`, `solucion`, `grafica` (sin tildes para compatibilidad)

### Validación Automática

```bash
# Verificar (sin corregir)
Rscript .claude/scripts/corregir_ortografia_espanol.R archivo.Rmd

# Corregir automáticamente
Rscript .claude/scripts/corregir_ortografia_espanol.R archivo.Rmd --fix
```

### Hook Pre-Commit

El sistema detecta automáticamente errores de ortografía antes de cada commit.

**⚠️ PROHIBIDO**: `git commit --no-verify` para evadir validaciones

---

## 7. Testing Automático (PERMANENTE)

**Regla detallada**: @.claude/rules/testing-obligatorio.md
**Flujo automático**: @.claude/docs/FLUJO_AUTOMATICO_TESTING.md

### Principio Fundamental

**TODOS los cambios son validados automáticamente. Tolerancia cero a regresiones.**

### Sistema de 4 Hooks

#### 1. PreToolUse - Edit/Write
**Hook**: `.claude/hooks/pre-edit-testing.sh`

- Ejecuta ANTES de editar componentes críticos
- BLOQUEA edición si tests actuales fallan
- Componentes críticos: `.claude/scripts/*`, `.claude/hooks/*`, `.claude/rules/*`, `tests/*`

#### 2. PostToolUse - Edit/Write
**Hook**: `.claude/hooks/post-edit-testing.sh`

- Ejecuta DESPUÉS de cualquier Edit/Write
- Valida que el cambio no rompió tests
- Reporta errores con instrucciones de corrección

#### 3. PreToolUse - Bash (git commit/push)
**Hook**: `.claude/hooks/pre-bash-testing.sh`

- **git commit**: Ejecuta suite completa, RECHAZA si falla
- **git push**: Valida suite + verifica sin cambios pendientes
- **⚠️ PROHIBIDO**: `git commit --no-verify`

#### 4. PostToolUse - Bash (exams2*)
**Hook**: `.claude/hooks/post-exams2-validation.sh`

- FASE 2A: Validación matemática automática
- FASE 2B: Preview PNG automático
- Claude DEBE: Leer PNG + Verificar 5 coherencias + Solicitar aprobación

### Cobertura de Tests

| Suite | Tests | Cobertura |
|-------|-------|-----------|
| Validación matemática | 5 | 100% |
| Ortografía española | 5 | 100% |
| Renderizado 4 formatos | 6 | 100% |
| Aleatorización | 4 | 100% |
| Flujo B Graficador | 6 | 100% |
| Regresión | 7 | 100% |
| **TOTAL** | **33+** | **100%** |

### Garantías del Sistema

✅ Ningún cambio rompedor llega a código
✅ 100% de cobertura se mantiene
✅ Commits solo con tests pasando
✅ Push solo con validación completa
✅ Validación automática de .Rmd
✅ Claude no puede romper el sistema
✅ CI/CD adicional en remoto

---

## 🚨 Mensajes de Error Comunes

### Error: Flujo B Incompleto

```
❌ BLOQUEO: Flujo B Obligatorio

Se han detectado gráficos pero el Flujo B no ha sido ejecutado.

ACCIÓN REQUERIDA:
1. Ejecutar /auto-refinar-grafico tikz 95
2. Obtener aprobación usuario para TikZ
3. Ejecutar /auto-refinar-grafico python 95
4. Obtener aprobación usuario para Python
5. Ejecutar /auto-refinar-grafico r 95
6. Obtener aprobación usuario para R
7. Solo entonces generar .Rmd
```

### Error: Tests Fallaron Pre-Commit

```
❌ COMMIT RECHAZADO - TESTS FALLARON

Acciones requeridas:
1. Revisar errores de tests
2. Corregir código
3. Volver a ejecutar: Rscript tests/run_all_tests.R
4. Solo entonces hacer commit

⚠️ PROHIBIDO usar: git commit --no-verify
```

### Error: Validación Visual Omitida

```
❌ VALIDACIÓN INCOMPLETA

No se detectó inspección visual del ejercicio.

ACCIÓN REQUERIDA:
1. Read("preview.png") para mostrar al usuario
2. Verificar las 5 coherencias VISUALMENTE
3. Documentar hallazgos con checklist
4. Solicitar aprobación explícita del usuario

No proceder sin aprobación.
```

### Error: Detractor Omitido

```
❌ FASE 2C OMITIDA

No se ejecutó revisión detractor después de validación visual.

ACCIÓN REQUERIDA:
1. Ejecutar: /detractor auditoria [archivo.Rmd]
2. Revisar objeciones en 4 dominios
3. Corregir objeciones CRÍTICAS/ALTAS si existen
4. Solo continuar a FASE 3 si veredicto es APROBAR

No proceder sin revisión detractor.
```

---

## 8. Detractor Obligatorio en Revisiones - OBLIGATORIO 🆕

**Regla detallada**: @.claude/rules/detractor-obligatorio.md

### Principio Fundamental

**El skill-detractor DEBE invocarse AUTOMÁTICAMENTE en toda fase de revisión.**

El detractor actúa como revisor adversarial que confronta decisiones con fuentes de verdad.

### Puntos de Activación

| Punto | Activación | Bloqueo |
|-------|------------|---------|
| **Post-generación** | Después de `/generar-*` | Si hay objeciones CRÍTICAS/ALTAS |
| **FASE 2C** | Después de preview visual | Si veredicto es RECHAZAR |
| **Pre-promoción** | Antes de `/promover-ejercicio` | Si hay objeciones pendientes |

### Dominios de Revisión

1. **Código R-exams**: exshuffle, metadatos, estructura
2. **Pedagógico**: Progressive Disclosure, metacognición, DOK/Bloom
3. **Visual**: Coherencia gráfico-texto, etiquetas, escalas
4. **Gramática**: Tildes, redacción, terminología

### Formato de Invocación

```bash
# Modo Auditoría (completo)
/detractor auditoria [archivo.Rmd]
/detractor auditoria [directorio/]

# Modo Inline (rápido)
/detractor [pregunta específica]
```

### Umbrales de Severidad

| Nivel | Criterio | Acción |
|-------|----------|--------|
| **Crítica** | Errores matemáticos, pérdida coherencia | BLOQUEAR, corregir inmediato |
| **Alta** | Distractores inválidos, metadatos faltantes | Priorizar corrección |
| **Media** | Mejoras pedagógicas | Agregar a backlog |
| **Baja** | Estilo, convenciones | Ignorar |

### Integración con Ciclo de Validación

```
FASE 1: Renderizado
    ↓
FASE 2A: Validación matemática [hook]
    ↓
FASE 2B: Preview visual [hook]
    ↓
FASE 2C: Detractor [OBLIGATORIO] ← NUEVO
    ↓
FASE 3: Decisión usuario
```

### Prohibiciones

- ❌ **NUNCA** omitir FASE 2C
- ❌ **NUNCA** ignorar objeciones CRÍTICAS/ALTAS
- ❌ **NUNCA** promocionar sin auditoría previa
- ❌ **NUNCA** objetar sin fuente verificable (Nivel 1-2)

### Garantías

✅ Toda decisión confrontada con fuentes de verdad
✅ Sesgo de confirmación eliminado
✅ Calidad pedagógica validada científicamente
✅ Código R-exams cumple estándares oficiales

---

## 📋 Checklist de Cumplimiento

Antes de finalizar CUALQUIER ejercicio:

- [ ] **Ejercicio es metacognitivo** (patrón aplicado, no puramente procedimental)
- [ ] **Pool de errores conceptuales** definido (mínimo 4)
- [ ] **Metadatos cognitivos** presentes (DOK, Bloom, SOLO)
- [ ] **Solution incluye** análisis de error + reflexión metacognitiva
- [ ] Si tiene gráficos → Flujo B completado
- [ ] TikZ/Python/R aprobados secuencialmente (si aplica)
- [ ] 5 coherencias verificadas VISUALMENTE
- [ ] Preview PNG mostrado al usuario
- [ ] **FASE 2C Detractor ejecutada** (veredicto APROBAR) 🆕
- [ ] Ortografía validada (tildes correctas)
- [ ] Tests ejecutados y pasando (100%)
- [ ] Renderizado exitoso en 4 formatos
- [ ] 200+ versiones únicas generadas (250+ si no hay restricciones fuertes)
- [ ] Usuario aprobó explícitamente
- [ ] Documentación actualizada

**Si falta alguno → NO aprobar el ejercicio.**

---

**Versión**: 1.2
**Fecha**: 2026-02-07
**Cambio v1.2**: Añadida regla #8 Detractor Obligatorio en fases de revisión
**Cambio v1.1**: Añadida regla #1 Ejercicios Metacognitivos con Progressive Disclosure
**Módulo de**: @.claude/CLAUDE.md (v3.2.0)
