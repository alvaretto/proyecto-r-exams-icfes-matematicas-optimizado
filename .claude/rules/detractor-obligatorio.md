# Regla: Detractor Obligatorio en Fases de Revisión

## Principio Fundamental

**El skill-detractor DEBE invocarse AUTOMÁTICAMENTE en toda fase de revisión (visual, código, gramática, etc.). NO hay excepciones.**

El detractor actúa como revisor adversarial que confronta decisiones con fuentes de verdad, documentación oficial y evidencia científica.

---

## Puntos de Activación Obligatoria

### 1. FASE 2C: Revisión Detractor (Nuevo)

**Se ejecuta DESPUÉS de FASE 2A (matemática) y FASE 2B (visual)**

```
FASE 2A: Validación matemática [AUTOMÁTICA]
    ↓
FASE 2B: Preview visual [AUTOMÁTICA]
    ↓
FASE 2C: Revisión Detractor [OBLIGATORIA]
    ↓
FASE 3: Decisión y aprobación usuario
```

### 2. Durante Generación de Ejercicios

```
/generar-schoice o /generar-cloze
    ↓
[Ejercicio generado]
    ↓
OBLIGATORIO: /detractor auditoria [archivo.Rmd]
    ↓
[Corregir objeciones si existen]
    ↓
Ciclo de validación (FASE 1-2-3)
```

### 3. Pre-Promoción de Ejercicios

```
/promover-ejercicio
    ↓
OBLIGATORIO: /detractor auditoria [directorio_ejercicio/]
    ↓
Veredicto: APROBAR → Continuar promoción
Veredicto: MODIFICAR/RECHAZAR → Bloquear promoción
```

---

## Dominios de Revisión del Detractor

### 1. Revisión de Código (.Rmd)

**Qué revisa:**
- Coherencia de código R-exams
- Uso correcto de exshuffle, exsolution, extype
- Distractores basados en errores conceptuales (no aleatorios)
- Metadatos ICFES completos (6 dimensiones)
- Metadatos cognitivos (DOK, Bloom, SOLO)
- Pool de errores con funciones calcula()

**Fuentes de verdad:**
- Documentación R-exams (Nivel 1)
- Ejemplos funcionales locales (Nivel 1)
- Vignettes CRAN (Nivel 1)

### 2. Revisión Pedagógica

**Qué revisa:**
- Aplicación de Progressive Disclosure
- Estructura metacognitiva correcta
- Reflexiones pedagógicas apropiadas
- Nivel de dificultad coherente con metadatos
- Distractores diagnosticables

**Fuentes de verdad:**
- Marco Conceptual ICFES 2026 (Nivel 1)
- Dunlosky et al. (2013) Learning techniques (Nivel 1)
- Schraw & Dennison (1994) Metacognition (Nivel 1)
- Anderson & Krathwohl (2001) Bloom revisado (Nivel 1)

### 3. Revisión Visual/Gráfica

**Qué revisa:**
- Coherencia visual-texto (gráfico vs enunciado)
- Etiquetas legibles y correctas
- Escalas y proporciones apropiadas
- Compatibilidad con 4 formatos de salida

**Fuentes de verdad:**
- Estándares visuales ICFES (Nivel 1)
- Documentación TikZ/pgfplots (Nivel 1)
- Buenas prácticas de visualización (Nivel 2)

### 4. Revisión Gramatical/Ortográfica

**Qué revisa:**
- Tildes en palabras frecuentes
- Gramática española correcta
- Redacción estilo ICFES
- Terminología matemática apropiada

**Fuentes de verdad:**
- RAE (Nivel 1)
- Diccionario local `.claude/rules/ortografia-espanol.md` (Nivel 1)

### 5. Coherencia Matemática

**Qué revisa:**
- Fórmulas y ecuaciones correctas
- Cálculos verificables paso a paso
- Proporciones y escalas correctas
- Respuesta correcta matemáticamente válida
- Distractores plausibles pero incorrectos (no absurdos)
- Consistencia entre datos del enunciado y opciones
- Variables sin NA/NaN/Inf

**Fuentes de verdad:**
- Definiciones matemáticas estándar (Nivel 1)
- Wolfram Alpha / verificación simbólica (Nivel 1)
- `.claude/scripts/validar_coherencia_matematica.R` (Nivel 1)

### 6. Coherencia ICFES Metacognitiva

**Qué revisa:**
- Aplicación de Progressive Disclosure (4+ partes en CLOZE)
- Pool de errores conceptuales con códigos y funciones `calcula()`
- Metadatos cognitivos completos (DOK ≥ 2, Bloom, SOLO)
- Sección Solution con 6 subsecciones obligatorias:
  - Análisis del error
  - Procedimiento correcto
  - Propiedades del concepto
  - Caso específico
  - Reflexión metacognitiva
  - Estrategia para evitar el error
- Antipatrón detectado: ejercicio puramente procedimental
- Distractores basados en errores conceptuales reales (no aleatorios)

**Fuentes de verdad:**
- `.claude/rules/ejercicios-metacognitivos.md` (Nivel 1)
- Marco Conceptual ICFES 2026 (Nivel 1)
- Dunlosky et al. (2013) - Learning techniques (Nivel 1)
- Schraw & Dennison (1994) - Metacognitive awareness (Nivel 1)
- Anderson & Krathwohl (2001) - Bloom revisado (Nivel 1)

### 7. Testing y Regresión

**Qué revisa:**
- Tests unitarios existen para componentes críticos
- Cobertura de tests ≥ 100% para scripts de validación
- Tests de diversidad (200+ versiones únicas)
- Sin regresiones en funcionalidad existente
- Git hooks nativos configurados (pre-commit, pre-push)
- CI/CD activo y pasando

**Fuentes de verdad:**
- `tests/testthat/` (Nivel 1)
- `.claude/rules/testing-obligatorio.md` (Nivel 1)
- `.git/hooks/pre-commit`, `.git/hooks/pre-push` (Nivel 1)
- `.github/workflows/ci-testing.yml` (Nivel 1)

### 8. Coherencia Semántica (Nivel 4)

**Qué revisa:**
- Campo `precondicion` declarado en cada error del pool conceptual (Capa A)
- Descripciones de errores coherentes con datos generados — keyword scanner automático (Capa B)
- `calcula()` produce valor diferente al correcto — cross-validación (Capa C)
- 21 reglas de keywords cubren: paridad, modalidad, cuartiles, outliers, simetría, tipo de datos, tamaño de muestra
- Errores `ERR_SEM_A/B/C` (bloqueantes) y `WARN_SEM_B` (bugs latentes)
- Patrón de selección genérico basado en `precondicion` (no filtros hardcoded)

**Fuentes de verdad:**
- `.claude/scripts/validar_coherencia_matematica.R` — `REGLAS_SEMANTICAS_KEYWORDS` (Nivel 1)
- `.claude/rules/ejercicios-metacognitivos.md` — sección "Validación Semántica Automática" (Nivel 1)
- `.claude/rules/codigo-rmd.md` — regla #8 (Nivel 1)
- `tests/testthat/test_validacion_semantica.R` (Nivel 1)

---

## Formato de Revisión Detractor

### Reporte Estructurado (FASE 2C)

```markdown
## Revisión Detractor - [Nombre Ejercicio]

**Fecha**: YYYY-MM-DD
**Dominios revisados**: [código | pedagógico | visual | gramática | matemático | metacognitivo | testing | semántico]

### Objeciones Encontradas

#### [Si hay objeciones]

**Objeción 1: [Título]**
- **Qué se cuestiona**: [código/decisión específica]
- **Por qué** (Fuente Nivel X): "[Cita]" — [Referencia]
- **Riesgo concreto**: [descripción cuantificada si posible]
- **Alternativa propuesta**: [solución específica]
- **Veredicto**: MANTENER | MODIFICAR | REEMPLAZAR

#### [Si no hay objeciones]

✅ **Sin objeciones**

Dominios analizados:
- Código R-exams: Conforme
- Estructura pedagógica: Conforme
- Coherencia visual: Conforme
- Gramática/ortografía: Conforme
- Coherencia matemática: Conforme
- ICFES metacognitivo: Conforme
- Testing/regresión: Conforme

### Veredicto Global

**Estado**: APROBAR | APROBAR CON CAMBIOS | RECHAZAR

### Próximos Pasos

1. [Acción si hay cambios requeridos]
2. [O continuar a FASE 3 si aprobado]
```

---

## Umbrales de Activación

```yaml
defaults:
  severidad_minima: media      # Solo reportar media, alta, crítica
  fuente_minima: 2             # Nivel 1-2 requerido
  max_objeciones: 10           # Priorizar las más importantes
  ignorar_estilistico: true    # No objetar preferencias de estilo
```

### Severidades y Acciones

| Nivel | Criterio | Acción |
|-------|----------|--------|
| Crítica | Errores matemáticos, pérdida de coherencia | BLOQUEAR, corregir inmediatamente |
| Alta | Distractores inválidos, metadatos faltantes | Priorizar corrección |
| Media | Optimizaciones pedagógicas, mejoras menores | Agregar a backlog |
| Baja | Estilo, convenciones menores | Ignorar (delegar a linter) |

### Los dos TIPOS de defecto no se tratan igual (añadido 2026-08-19)

Antes de asignar severidad, clasifica el hallazgo. **Son binarios los de corrección y graduales los
de diagnosticidad**, y confundirlos produce ciclos de corrección que no convergen.

| Tipo | Qué es | Criterio | Efecto en el veredicto |
|---|---|---|---|
| **CORRECCIÓN** | La clave es falsa · hay una segunda clave válida · la Solution afirma algo matemáticamente falso · un distractor es en realidad correcto · el estímulo se contradice | **Binario.** Cualquier instancia lo activa | **BLOQUEANTE ABSOLUTO.** `RECHAZAR` sin discusión |
| **DIAGNOSTICIDAD** | Canales de eliminación: longitud, léxico, signo, magnitud, divisibilidad, posición | **Gradual y comparativo.** Se juzga por el **exceso** frente a la vara oficial (regla #22 §P7-A), no en absoluto | Sólo obliga por encima de **+8 pp**. En el rango del control oficial (≤ +5,3 pp) **no es motivo de rechazo** |

**Dos exigencias que se siguen de la tabla:**

1. **Un hallazgo de diagnosticidad NO se reporta sin su margen.** Una tasa alta con margen < 15 %
   es inexplotable (§P7-B) y **no es un defecto**: reportarla como tal infla la severidad y consume
   pasadas. Si no mediste el margen, dilo — no lo presentes como daño.
2. **Verifica siempre la CORRECCIÓN después de una mejora de diagnosticidad.** Es el orden en que
   este repositorio ya falló: una corrección de canal volvió falsa la clave en el 31,7 % de las
   versiones y sobrevivió a tres auditorías porque nadie volvió a comprobar lo obvio.

**Cuando el ejercicio agote su presupuesto de 3 pasadas** (§P7-D) con sólo residuos de
diagnosticidad dentro de la vara, el veredicto correcto es `APROBAR_CON_CAMBIOS` con los residuos
declarados y sus cifras — **no** un cuarto `RECHAZAR`.

---

## Bloqueos Automáticos

### 1. Bloqueo Pre-Promoción

```
SI /detractor reporta objeciones CRÍTICAS o ALTAS:
    BLOQUEAR /promover-ejercicio
    MOSTRAR objeciones
    REQUERIR corrección antes de continuar
```

### 2. Bloqueo Post-Generación

```
SI /detractor reporta veredicto RECHAZAR:
    MARCAR ejercicio como "requiere revisión"
    NO avanzar a ciclo de validación
    REQUERIR reescritura
```

---

## Integración con Otros Skills

### Con /validar-pedagogico

```
/validar-pedagogico genera reporte
    ↓
/detractor valida las decisiones del reporte
    ↓
Confronta recomendaciones con evidencia científica
```

### Con /generar-schoice y /generar-cloze

```
[Ejercicio generado]
    ↓
/detractor auditoria automática (OBLIGATORIO)
    ↓
Reporta objeciones sobre:
- Estructura metacognitiva
- Pool de errores
- Metadatos cognitivos
- Formato Solution
```

### Con Ciclo de Validación

```
FASE 1: Renderizado
FASE 2A: Validación matemática [hook]
FASE 2B: Preview visual [hook]
FASE 2C: Revisión Detractor [OBLIGATORIO] ← NUEVO
FASE 3: Decisión usuario
```

---

## Invocación Manual vs Automática

### Automática (OBLIGATORIA)

Se invoca automáticamente en:

1. **Post-generación** de cualquier .Rmd
2. **FASE 2C** del ciclo de validación
3. **Pre-promoción** de ejercicios
4. **Post-validación pedagógica**

### Manual (Opcional)

El usuario puede invocar directamente:

```
/detractor auditoria [target]
/detractor [pregunta específica]
```

---

## Antipatrones PROHIBIDOS

### 1. Omitir FASE 2C

```
❌ PROHIBIDO
FASE 2A → FASE 2B → FASE 3 (saltando detractor)

✓ CORRECTO
FASE 2A → FASE 2B → FASE 2C (detractor) → FASE 3
```

### 2. Ignorar Objeciones Críticas/Altas

```
❌ PROHIBIDO
Detractor reporta objeción ALTA → Continuar sin corregir

✓ CORRECTO
Detractor reporta objeción ALTA → Corregir → Re-auditar
```

### 3. Promoción sin Auditoría

```
❌ PROHIBIDO
/promover-ejercicio sin /detractor previo

✓ CORRECTO
/detractor auditoria [ejercicio] → APROBAR → /promover-ejercicio
```

---

## Configuración Proyecto

Archivo `.claude/detractor-config.yaml`:

```yaml
# Activación automática
activacion:
  post_generacion: true       # Después de /generar-*
  fase_2c: true               # En ciclo de validación
  pre_promocion: true         # Antes de /promover-ejercicio
  post_validacion_pedagogico: true

# Umbrales
umbrales:
  severidad_minima: media
  fuente_minima: 2
  max_objeciones: 10

# Dominios obligatorios a revisar (8 dominios)
dominios_obligatorios:
  - codigo_rexams
  - pedagogico
  - visual
  - gramatica
  - coherencia_matematica
  - icfes_metacognitivo
  - testing

# Fuentes de verdad locales
fuentes_locales:
  - .claude/rules/
  - .claude/docs/
  - A-Produccion/Ejemplos-Funcionales-Rmd/

# Bloqueos
bloqueos:
  critica: true               # Bloquear si hay objeciones críticas
  alta: true                  # Bloquear si hay objeciones altas
  media: false                # No bloquear, solo reportar
```

---

## Flujo Completo con Detractor

```
┌─────────────────────────────────────────────────────────┐
│  WORKFLOW COMPLETO CON DETRACTOR OBLIGATORIO            │
├─────────────────────────────────────────────────────────┤
│                                                         │
│  1. /generar-schoice o /generar-cloze                  │
│     ↓                                                   │
│  2. [Ejercicio.Rmd generado]                           │
│     ↓                                                   │
│  3. /detractor auditoria [Ejercicio.Rmd] ← OBLIGATORIO │
│     │                                                   │
│     ├── Objeciones CRÍTICAS/ALTAS → Corregir → (3)     │
│     │                                                   │
│     └── APROBAR → Continuar                            │
│     ↓                                                   │
│  4. FASE 1: Renderizado (HTML/PDF/DOCX/NOPS)           │
│     ↓                                                   │
│  5. FASE 2A: Validación matemática [hook automático]   │
│     ↓                                                   │
│  6. FASE 2B: Preview visual [hook automático]          │
│     ↓                                                   │
│  7. FASE 2C: Revisión Detractor ← OBLIGATORIO          │
│     │                                                   │
│     ├── Objeciones → Corregir → VOLVER A (4)           │
│     │                                                   │
│     └── APROBAR → Continuar                            │
│     ↓                                                   │
│  8. FASE 3: Documentar 5 coherencias + Pedir aprobacion│
│     ↓                                                   │
│  9. Usuario aprueba → Ejercicio "LISTO PARA AULA"      │
│     → Permanece en 02-En-Desarrollo/                   │
│     ↓                                                   │
│  10. NIVEL 3: Aplicar en aula con estudiantes           │
│     ↓                                                   │
│  11. /promover-ejercicio (requiere evidencia Nivel 3)   │
│      → Mover a 03-En-Produccion/ ✅                    │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

---

## Independencia del detractor (OBLIGATORIA)

**El detractor DEBE ser un agente distinto del que escribió o corrigió el artefacto.
Una revisión hecha por el mismo agente sobre su propio trabajo NO es FASE 2C: es
autoevaluación, y el sesgo de confirmación que el detractor existe para romper es
precisamente el suyo.**

### Qué cuenta y qué no

| Situación | ¿Es FASE 2C válida? |
|---|---|
| `Task(subagent_type="AgenteDetractor")` lanzado por el coordinador, que devuelve reporte con `VEREDICTO_DETRACTOR:` | **Sí** |
| `/adversario <archivo>` en sesión propia (framework anti-sicofancia) | **Sí** |
| El coordinador ejecuta los validadores y redacta él las objeciones | **No** — es auditoría propia |
| El coordinador corrigió el `.Rmd` y después lo revisa él mismo | **No** — autoevaluación |
| Un detractor previo que auditó una versión ANTERIOR del archivo | **No** — caducó al editar |

Un veredicto de FASE 2C sellado sin independencia es peor que no tenerlo: da por
verificado justo el punto ciego del autor. Origen: incidente 2026-08-09
(`excedente-almuerzo-proporcional-n4`), donde el defecto de mayor severidad
—un distractor que en una de las tres ramas señalaba información que SÍ resolvía
el problema, rompiendo la unicidad de la clave— era **semántico**, invisible para
todo el arsenal automático (coherencia, diagnosticidad y diversidad en verde), y
apareció solo al razonar sobre el significado de las opciones. Ese es exactamente
el terreno donde la independencia importa.

### Cuál de los dos detractores usar

| Agente | Modelo | Cuándo |
|---|---|---|
| `AgenteDetractor` (`.claude/agents/agente-detractor.md`) | opus | FASE 2C estándar: los 8 dominios de esta regla sobre un `.Rmd` |
| `adversario` (global, `~/.claude/agents/adversario.md`) | sonnet | Cuando se quiera además el framework anti-sicofancia (6 dimensiones, ToF, resistencia multi-turno) o el objeto no sea un `.Rmd` |

No son intercambiables en su salida: el primero reporta por dominios ICFES; el
segundo, hallazgos con convicción epistémica. Para un `.Rmd` en workflow, el
canónico es `AgenteDetractor`.

### Regla de spawn: SIN `name:` (OBLIGATORIA, medida 2026-08-16)

**El detractor se lanza SIEMPRE sin el parámetro `name:`.** No es una preferencia de estilo: el
`name` cambia el **modo de entrega**, no sólo la etiqueta.

| Invocación | Qué recibe quien invoca |
|---|---|
| `Task(subagent_type="AgenteDetractor", …)` **sin `name`** | El **reporte íntegro** como `tool_result` |
| `Agent(subagent_type="AgenteDetractor", name="…", …)` | `"Spawned successfully"` (275-307 chars) y, al terminar, un `idle_notification`. **El reporte no llega jamás** |

Con `name`, el agente pasa a ser un *teammate* (`taskKind: "in_process_teammate"`), cuyo único
canal hacia arriba es `SendMessage({to: "main"})`. Medición que lo fija: en una misma sesión,
**20 de 20** spawns con nombre devolvieron metadata, y los spawns sin nombre devolvieron reportes
de **5.738 a 31.056** caracteres — la variable que discrimina es el `name`, no el tamaño ni el
tipo de agente. Un teammate tampoco puede lanzar otros teammates (*«the team roster is flat»*).

Consecuencia para esta regla: **un `VEREDICTO_DETRACTOR:` ausente tras un spawn con `name` no es
una no-entrega del detractor** — es un error de invocación del coordinador, y se corrige
relanzando sin `name`, no aplicando el protocolo de reintentos.

---

## Protocolo de no-entrega (OBLIGATORIO)

Un subagente puede terminar su turno sin emitir reporte (notificación de "idle" o
"disponible" sin contenido). Eso **no** es un veredicto y **no** cierra la FASE 2C.

### Cómo se detecta

El reporte se considera entregado sólo si su última línea es el marcador:

```
VEREDICTO_DETRACTOR: APROBAR | APROBAR_CON_CAMBIOS | RECHAZAR
```

Sin esa línea, el reporte está **NO ENTREGADO** aunque contenga análisis: puede
venir truncado. El contrato que lo obliga vive en `.claude/agents/agente-detractor.md`
§ "Contrato de entrega".

### Qué hacer

**Paso 0 — RECUPERAR ANTES DE RECLAMAR (obligatorio; añadido 2026-08-16).**
Un reporte que no llegó **casi siempre existe**: en la sesión del 2026-08-15/16, los **11**
subagentes que «no entregaron» habían escrito su reporte completo, con marcador. Reclamar sin
comprobarlo cuesta ~300k tokens de detractor y puede disparar una pasada innecesaria. Antes de
gastar un reintento, buscar el reporte en la transcripción del agente:

```bash
D=~/.claude/projects/<proyecto-slug>/<session-id>/subagents
ls -t "$D"/*<nombre-agente>*.jsonl | head -1
# extraer el ÚLTIMO content[].text de role: assistant de ese .jsonl
```

Si el reporte está ahí y cierra con `VEREDICTO_DETRACTOR:`, **la FASE 2C está cumplida**: se usa
ese reporte y no se reclama nada. Dos cautelas medidas:

- **Comprobar el timestamp del bloque recuperado.** Si es *posterior* al momento en que se recibió
  el `tool_result`, el agente seguía trabajando cuando el harness cortó, y ese reporte llegó fuera
  de plazo — es entrega válida, pero el corte fue real (agotamiento de `maxTurns`), no un problema
  de canal.
- **Si el `tool_result` empezaba por `"Spawned successfully"`**, el detractor se lanzó **con
  `name:`** y por tanto como *teammate*, que no tiene canal de entrega hacia arriba. Eso no es una
  no-entrega del agente sino un error de invocación: relanzarlo **sin `name`** en vez de reclamar.

`TaskOutput` **NO** sirve para esto: está deprecado y su propia descripción desaconseja leer el
`.output` de un `local_agent` (es un symlink a la transcripción íntegra y desborda el contexto).

Sólo si el Paso 0 no encuentra reporte:

1. **Reintento 1** — reclamar el reporte al mismo agente (`SendMessage`), recordándole
   el contrato de entrega y avisándole de cualquier cambio del artefacto desde su
   lanzamiento.
2. **Reintento 2** — lanzar un agente **nuevo** (contexto limpio) sobre la versión
   vigente. No reanudar el anterior: si ya falló en entregar, su contexto arrastra
   el mismo estado.
3. **Tras 2 fallos — PARAR y escalar al usuario.** Está PROHIBIDO:
   - sustituir el detractor por la auditoría propia del coordinador y llamarla FASE 2C;
   - sellar `detractor_fase2c` en `ejercicio_state.json`;
   - declarar el ejercicio listo para la aprobación del paso 11.

   El coordinador **sí puede** revisar por su cuenta y reportar lo que encuentre —
   es trabajo útil—, pero debe **declararlo explícitamente como no independiente**
   y dejar la FASE 2C abierta. Registrar en el reporte al usuario:
   `detractor_fase2c: NO ENTREGADO tras 2 intentos — revisión propia, no independiente`.

### Antipatrón PROHIBIDO

```
❌ El detractor no devolvió nada → audito yo → "Veredicto: APROBAR" → sello el paso 7
✓  El detractor no devolvió nada → Paso 0 (recuperar) → 2 intentos → escalo al usuario
                                 → paso 7 sigue abierto
❌ El detractor no devolvió nada → le reclamo → le reclamo otra vez → escalo
   ...cuando su reporte estaba entero en su transcripción, o cuando lo lancé con `name:`
```

---

## Garantías del Sistema

Con el detractor obligatorio:

1. ✅ **Toda decisión es confrontada** con fuentes de verdad
2. ✅ **Errores conceptuales** son detectados antes de promoción
3. ✅ **Sesgo de confirmación** es eliminado
4. ✅ **Calidad pedagógica** es validada científicamente
5. ✅ **Código R-exams** cumple estándares oficiales
6. ✅ **Coherencias** son verificadas por segunda opinión

---

**Versión**: 1.3
**Fecha**: 2026-08-16
**Estado**: ACTIVO Y OBLIGATORIO
**Excepciones**: NINGUNA
**Skill asociado**: `.claude/skills/skill-detractor/SKILL.md`
**Agente asociado**: `.claude/agents/agente-detractor.md`

### Cambios v1.3 (2026-08-16)

> La v1.2 diagnosticó bien el síntoma —detractores que terminan sin reporte— y cableó la defensa
> correcta para el caso en que el agente calla. Pero **el agente casi nunca callaba**: al medir la
> sesión del 2026-08-15/16, los **11** subagentes que «no entregaron» tenían su reporte completo,
> con marcador, en su transcripción. La defensa se estaba aplicando sobre falsos negativos.

- **DOS CAUSAS MEDIDAS, ninguna era «el agente no entregó»**:
  - **(A) el `name:` cambia el modo de entrega.** `Agent` con `name` crea un *teammate*
    (`taskKind: "in_process_teammate"`), cuyo `tool_result` es siempre `"Spawned successfully"`
    y **cuyo texto final no viaja al padre** — sólo llega un `idle_notification`. Control
    positivo en la misma sesión y el mismo harness: los spawns **sin** `name` devolvieron
    reportes de **5.738 a 31.056** caracteres; los 20 spawns **con** `name`, 275-307 de metadata.
    *La variable que discrimina es el `name`*, no el tamaño ni el tipo de agente — las dos
    hipótesis que quedaban abiertas.
  - **(B) agotamiento de `maxTurns`.** Tres detractores gastaron **33, 34 y 43** usos de
    herramienta contra un `maxTurns: 30`; el harness cerró su entrega con un texto de
    razonamiento intermedio de 96-336 caracteres **mientras seguían ejecutando herramientas**, y
    el reporte real (18-28 KB) apareció en su transcripción minutos después. `maxTurns` sube a
    **60** y el contrato del agente le exige emitir al 70 % del presupuesto.
- **POR ESO «MÁS PROSA EN EL PROMPT» NO ARREGLÓ NADA**: el contrato de la v3.20.2 dice *«tu texto
  final ES el reporte»*, lo cual es **cierto en modo bloqueante y falso para un teammate**.
  Describía un canal que ese modo de arranque no tiene. Los 11 llevaban el contrato en su encargo.
- **NUEVA SECCIÓN — Regla de spawn**: el detractor se lanza SIEMPRE **sin `name:`**, con la tabla
  de qué recibe quien invoca en cada modo. Corolario que evita quemar un detractor entero: un
  `VEREDICTO_DETRACTOR:` ausente tras un spawn con `name` **no es una no-entrega**, es un error de
  invocación; se relanza, no se reclama.
- **NUEVO PASO 0 del protocolo — recuperar antes de reclamar**, con sus dos cautelas medidas
  (comparar el timestamp del bloque recuperado contra el del `tool_result`; y reconocer el
  `"Spawned successfully"` como error de invocación). `TaskOutput` **no sirve**: está deprecado y
  desaconseja leer el `.output` de un `local_agent`.
- **CORRECCIÓN a lo registrado en la v1.2**: era falso que un orquestador hubiera reclamado a su
  detractor «un marcador que sí estaba presente». Reconstruido minuto a minuto: al reclamar
  (15:16:01) el reporte con marcador **aún no existía** — se escribió a las 15:19:15. En esos tres
  casos el protocolo de reintentos **funcionó y era correcto**.

### Cambios v1.2 (2026-08-09)
- **NUEVA SECCIÓN — Independencia del detractor**: el detractor DEBE ser un agente
  distinto del que escribió o corrigió el artefacto. Tabla de qué cuenta como FASE 2C
  válida y qué no (autoevaluación, detractor caducado por edición posterior).
- **NUEVA SECCIÓN — Protocolo de no-entrega**: marcador `VEREDICTO_DETRACTOR:` como
  criterio mecánico de entrega; 2 reintentos (reclamo + agente nuevo) y escalado
  obligatorio al usuario. PROHIBIDO sustituir el detractor por la auditoría propia
  y sellar `detractor_fase2c`.
- **NUEVA TABLA — cuál de los dos detractores usar**: `AgenteDetractor` (canónico
  para `.Rmd` en workflow) vs `adversario` global (anti-sicofancia). La regla no lo
  decía y ambos existían.
- **Contrato de entrega** añadido a `.claude/agents/agente-detractor.md` (+ `maxTurns: 30`,
  que era el único agente de reporte sin declararlo).
- **Origen**: incidente 2026-08-09 en `excedente-almuerzo-proporcional-n4` — tres
  invocaciones consecutivas terminaron en notificación de "disponible" sin reporte,
  y la revisión adversarial acabó haciéndola el mismo agente que había escrito las
  correcciones. El defecto de mayor severidad del ejercicio era **semántico**
  (un distractor que rompía la unicidad de la clave en una de las tres ramas) con
  todo el arsenal automático en verde: es justo el hallazgo que depende de una
  mirada independiente.
- **Test asociado**: `tests/testthat/test_contrato_detractor.R`.

### Cambios v1.1 (2026-02-07)
- **3 nuevos dominios agregados**: coherencia matemática, ICFES metacognitivo, testing
- **8 dominios totales** de revisión adversarial obligatoria
- **Fuentes de verdad** documentadas para cada nuevo dominio
- **Integración** con testing-obligatorio.md y ejercicios-metacognitivos.md
