# `.claude/` Local — Subproyecto `permutaciones-pescadores-venia-n4`

> **Herencia obligatoria.** Este `.claude/` local **hereda** del ecosistema del repositorio raíz
> (`RR/.claude/CLAUDE.md` + `RR/CLAUDE.md`) y **NO lo reemplaza**. Toda regla, hook, skill o
> agente definido en `RR/.claude/` sigue aplicando aquí sin cambios. Este archivo únicamente
> **añade** contexto y particularidades operativas específicas de este ejercicio.
>
> **En caso de conflicto entre una instrucción de este archivo y una regla del repositorio raíz,
> GANA la regla del repositorio raíz.** Este `.claude/` local es estrictamente aditivo. No
> modifica, no reinterpreta y no tiene autoridad para suspender ninguna de las 22 reglas críticas
> de `RR/.claude/CLAUDE.md` ni el gate mecánico de `RR/.claude/hooks/pre-write-rmd-gate.sh`.
>
> Ruta del repositorio raíz: `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams`

---

## Fuente de verdad para retomar

**[`../HANDOFF.md`](../HANDOFF.md)** es el documento de reanudación: objetivos, estado real
verificado, decisiones, hallazgos abiertos, riesgos y siguiente paso. **Léelo primero**, junto con
`../ejercicio_state.json`, antes de explorar el `.Rmd`.

```
Continúa con el proyecto A-Produccion/01-En-PreDesarrollo/permutaciones-pescadores-venia-n4
```
debe disparar la lectura de `HANDOFF.md` + `ejercicio_state.json` como primera acción.

---

## Identidad del ejercicio

| Campo | Valor |
|---|---|
| **Archivo** | `permutaciones_pescadores_metacognitivo_formulacion_n4_schoice_v1.Rmd` |
| **Tipo** | SCHOICE metacognitivo — 4 opciones de **TEXTO** (números). **Sin figura**: Flujo B = false |
| **Nivel ICFES** | N4 (`exextra[Nivel]: 4`, `DOK: 3`, `Bloom: Evaluar`, `SOLO: Relacional`) |
| **Descriptor** | `D4.8` — «Resuelve problemas de conteo que requieren el uso de permutaciones.» |
| **Competencia / Componente** | Formulación y ejecución / Aleatorio |
| **Origen** | `MAT-2026-1-004` (ERA-2026, Sesión 1, pregunta impresa 4; verbatim de `MAT-2026-1-098`) |
| **Evalúa** | Reconocer que al ocupar posiciones el conjunto disponible **decrece** (no la aritmética de `n!`) |
| **Pool de errores** | 5 (`EST-PER-01..05`), se eligen **3** por versión |

---

## Particularidades operativas (léelas ANTES de tocar el `.Rmd`)

Son notas de **por qué el código es como es**. Sin ellas, un agente puede "arreglar" algo que en
realidad es un fix deliberado y reintroducir un defecto ya cerrado.

### 1. Auto-contención obligatoria — los helpers van DENTRO del chunk

`pick_int()`, `safe_sample()` y `fmt()` están definidos dentro de `data_generation`, no en un
archivo externo. **No los extraigas.** `RR/.claude/scripts/validar_diversidad_sustantiva.R` hace
`setwd(tempdir())` y ahí `include_supplement()` falla (medido en el hermano
`desplazamiento-avion-aeropuerto`: 40/40 semillas fallidas). Es la misma invariante que declara el
hermano `plano-cartesiano-barco-n2`.

### 2. `exshuffle: TRUE` es CORRECTO aquí — no lo copies del hermano del avión

Las opciones son de **texto** y la `Solution` identifica la correcta por **contenido** (el valor
numérico) y por el **código** del error, nunca por letra (regla #19). Por eso `exshuffle: TRUE` es
seguro. El hermano `desplazamiento-avion-aeropuerto` usa `exshuffle: FALSE` porque tiene opciones
**gráficas**; ese patrón **no** es intercambiable con éste.

### 3. `n ∈ {4,5,6}` no es una preferencia: es un rango MEDIDO

`N_POOL <- c(4L, 5L, 6L)`. Fuera de ese rango el ejercicio se rompe o se degrada:

- `n ≤ 2`: colisionan dos o más opciones.
- `n == 3`: `n^(n-1) == n² == 9` → opciones duplicadas → `ERR_ANS_C`.
- `n ≥ 7`: la razón máx/clave salta a 23,3× y el distractor mayor se descarta por magnitud sin
  razonar (regla #22, patrón P5).

Umbral fijado en **15×**; la ventana medida entre la última aceptada (10,8×) y la primera
rechazada (23,3×) es un factor 2,2. **No amplíes `N_POOL` sin volver a medir** la unicidad de las
4 opciones y la razón de magnitud sobre TODAS las ternas del pool.

### 4. La excepción canónica (`es_canonica`) no es un caso especial gratuito

Cuando `ctx_idx == 1L && n == 4L`, la selección de errores se **fuerza** a los tres oficiales
(`EST-PER-01/02/03`). Esa versión reproduce el ítem ICFES completo: enunciado, pregunta y las
cuatro opciones (64, 24, 16, 4). Es lo que permite que convivan la regla #1 (pool de 4-6 errores
con selección) y el objetivo de fidelidad al ítem original. **Si eliminas la rama `es_canonica`,
el ejercicio deja de reproducir el ítem oficial** y la invariante I-6 falla.

### 5. Los tres errores oficiales vienen de la ficha, no de la imaginación

`EST-PER-01/02/03` son transcripción generalizada de las *Justificaciones MetaCognitivas* de la
ficha `MAT-2026-1-004`. No los reescribas por estilo. `EST-PER-04/05` sí son ampliación propia
(para cumplir el mínimo de la regla #1) y pueden editarse con más libertad.

### 6. El nombre de `EST-PER-01` fue corregido a propósito

Se llamó «Variación con repetición en lugar de permutación» y **era impreciso**: la fórmula
canónica de variación con repetición es `n^n`, no `n^(n-1)`. Ambos adversarios lo señalaron por
separado el 2026-07-29. El nombre actual —«Repetición sin descontar los elementos ya ubicados»— y
su descripción explican el **doble supuesto** (contar `n-1` posiciones y conservar `n` opciones),
que es lo que la ficha oficial describe al hablar de «tres posiciones» para `n = 4`. **No revertir
al nombre anterior.**

### 7. `verificar_render.R` es verificación, no exportación

Comprueba V1-V8 y devuelve exit 1 si algo falla. **V6 enumera el espacio completo** (3 valores de
`n` × C(5,3) = 30 ternas), no una muestra, y desde el code-review del 2026-07-29 **extrae el pool y
`N_POOL` del `.Rmd`** en vez de reimplementarlos: una copia local del pool quedaba obsoleta en
silencio y V6 seguía anunciando «30/30 todo verde» sobre las fórmulas viejas. **No vuelvas a
hardcodear las fórmulas ahí.** **V5 lee `n` del enunciado**, no de `min(vals)`: desde que el pool
tiene 5 errores, el distractor «cardinal» (que vale `n`) puede no estar en la terna. Si vuelves a
inferir `n` del mínimo, V5 dará falsos positivos.

**V5 compara la cobertura contra `N_VERSIONES`, no consigo misma.** Antes imprimía
`revisadas/revisadas` —tautológico—, así que descartar versiones en silencio se veía igual que
revisarlas todas. Verificado por mutación: con las versiones `n=6` no parseables, V5 reporta
`cobertura incompleta: 7/12`; antes decía «7/7 … es exactamente n!» y exit 0. **No sustituyas el
denominador por `revisadas`.**

### 11. Los tres alias del final del bloque de mezcla NO son redundantes

`valor_correcto`, `opciones_valores` y `error_sel` parecen duplicar a `correcta_val`, `opciones` y
`errores_sel`. **Existen porque `validar_coherencia_matematica.R` busca nombres FIJOS**: sin ellos su
Nivel 5B y su Capa A retornan temprano y el APROBADO de FASE 2A es **vacuo** (probado por mutación:
con la clave falsa y sin alias el validador dice `APROBADO (0 errores)`; con alias dice
`ERR_ANS_B`). El `stopifnot` que los acompaña verifica que `opciones_valores` siga alineado con
`opciones`/`sol` — si lo borras, el alias puede desalinearse y el cross-check pasaría a comparar la
opción equivocada. Detalle y tabla de cobertura en
[`rules/permutaciones-parametricas.md`](rules/permutaciones-parametricas.md).

### 12. Los docs se citan por ANCLA, no por número de línea

`README.md`, `docs/BLUEPRINT.md`, `docs/SYLLABUS.md` y `docs/ROADMAP.md` fijaban ~93 números de
línea del `.Rmd`. El code-review del 2026-07-29 encontró varios ya erróneos (p. ej. el `stopifnot`
de I-3 citado como «línea 142» cuando estaba en la 291) y, como el `.Rmd` se edita, el resto caducaba
en silencio; al obligar este archivo a leer los docs ANTES del `.Rmd`, mandaban al lector al bloque
equivocado. Se sustituyeron por **anclas estables** (nombre de chunk, identificador, subsección) y
la §6 del BLUEPRINT se reindexó por construcción. **Al documentar algo nuevo, cita el identificador
y localízalo con `grep -n`; no escribas números de línea del `.Rmd`.**

### 8. Prohibido `set.seed()` dentro del chunk

Regla #10 de `RR/.claude/rules/codigo-rmd.md`. Verificado el 2026-07-29: el chunk no contiene
`set.seed`, `Sys.time`, `proc.time` ni `Sys.Date`. No lo reintroduzcas.

### 9. `WARN_DIV_BAJA` es esperado y aceptado — no lo "arregles"

`validar_diversidad_sustantiva.R` reporta 3 valores únicos de respuesta correcta (24, 120, 720)
frente a un umbral de 12. Es **estructural**: solo hay tres valores legales de `n`. Ampliar el
rango violaría la particularidad 3. El validador devuelve exit 0; `ERR_DIV_COSMETICA` sí sería un
fallo, y no ocurre. La diversidad real la aportan los 6 contextos, las 10 ternas de error y las 6
reflexiones: **297/300 versiones únicas** medidas.

### 10. El corrector ortográfico daña una referencia si se ejecuta sin cuidado

`RR/.claude/scripts/corregir_ortografia_espanol.R --fix` convierte el nombre de archivo
`codigo-rmd.md` en `código-rmd.md` dentro de comentarios de R (falso positivo: la regla #7 declara
excluir rutas y nombres de archivo). Por eso las referencias a esa regla están escritas como ruta
completa `.claude/rules/codigo-rmd.md`, forma que sí queda excluida. **Si añades una referencia
nueva a un `.md`, escríbela como ruta completa.** Ver `../docs/BACKLOG.md`.

---

## Reglas del repo raíz con mayor peso aquí

| Regla | Por qué importa en este ejercicio |
|---|---|
| #1 `ejercicios-metacognitivos.md` | Pool de 4-6 errores con selección; Solution con subsecciones canónicas; coherencia Nivel↔DOK |
| #7 `ortografia-espanol.md` | Todo el texto visible lleva tildes; los `exextra[...]` van en ASCII |
| #11 `contextos-narrativos-creativos.md` | 6 plantillas como funciones, 6 estructuras, sin «registró» |
| #19 `solution-letter-independence.md` | La Solution identifica por contenido y código, jamás por letra |
| #21 `familias-soluciones-rmd.md` | Familia 1 (sin bucles sin cota) y Familia 5 (`safe_sample`) |
| #22 `diversidad-sustantiva.md` | P5 (distractor eliminable por magnitud) gobierna el umbral 15× |

---

## Prohibido

- Extraer los helpers del chunk a un archivo externo (particularidad 1).
- Cambiar `exshuffle` a `FALSE` (particularidad 2).
- Ampliar o reducir `N_POOL` sin re-medir (particularidad 3).
- Eliminar la rama `es_canonica` o la invariante I-6 (particularidad 4).
- Reescribir `EST-PER-01/02/03` apartándose de la ficha oficial (particularidad 5).
- Revertir el nombre de `EST-PER-01` (particularidad 6).
- Inferir `n` desde `min(vals)` en el verificador (particularidad 7).
- Reimplementar el pool o `N_POOL` dentro de `verificar_render.R` en vez de extraerlos del `.Rmd`,
  o devolver el denominador de cobertura de V5 a `revisadas` (particularidad 7).
- `set.seed()` dentro del chunk (particularidad 8).
- Ampliar `N_POOL` para "resolver" `WARN_DIV_BAJA` (particularidad 9).
- Borrar los alias `valor_correcto` / `opciones_valores` / `error_sel` o su `stopifnot` de
  alineación (particularidad 11).
- Escribir números de línea del `.Rmd` en los docs (particularidad 12).
- Marcar `aprobacion_usuario` en `ejercicio_state.json` sin aprobación humana explícita.

---

## Enlaces

- [`../HANDOFF.md`](../HANDOFF.md) — reanudación
- [`../README.md`](../README.md) — entrada del subproyecto
- [`rules/permutaciones-parametricas.md`](rules/permutaciones-parametricas.md) — contrato del pool
- [`../docs/BLUEPRINT.md`](../docs/BLUEPRINT.md) — arquitectura y decisiones D1/D2/D3
- [`../docs/SYLLABUS.md`](../docs/SYLLABUS.md) · [`../docs/ROADMAP.md`](../docs/ROADMAP.md) · [`../docs/BACKLOG.md`](../docs/BACKLOG.md)

---

**Versión**: 1.0
**Fecha**: 2026-07-29
