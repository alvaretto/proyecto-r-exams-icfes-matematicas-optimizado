# `.claude/` Local — Subproyecto `plano-cartesiano-barco-n2`

> **Alcance:** este archivo aplica **solo** dentro de
> `A-Produccion/01-En-PreDesarrollo/plano-cartesiano-barco-n2/`.
> No sustituye ni modifica el `.claude/` del repo raíz (infraestructura protegida, regla #17):
> **lo complementa** con lo que es específico de este ejercicio.

---

## Fuente de verdad para retomar

Lee en este orden, ANTES de explorar el repositorio o tocar el `.Rmd`:

1. [`../HANDOFF.md`](../HANDOFF.md) — estado de trabajo, decisiones, pendientes.
2. [`../ejercicio_state.json`](../ejercicio_state.json) — los 11 pasos del workflow.
3. [`../docs/BLUEPRINT.md`](../docs/BLUEPRINT.md) — arquitectura e **invariantes I-1 a I-8**.
4. Este archivo — particularidades operativas.

---

## Identidad del ejercicio

| Campo | Valor |
|---|---|
| Ítem ICFES de origen | `MAT-2026-1-022` (cuadernillo 2026-1, pregunta 116, clave C) |
| Tipo | SCHOICE metacognitivo, opciones de **texto** |
| Nivel / DOK / Bloom / SOLO | 2 / 2 / Comprender / Multi-estructural |
| Competencia / Componente | Interpretación y representación / Geométrico-métrico |
| Qué evalúa | Leer el *bounding box* de un objeto en un plano cartesiano |
| Figura | Una sola (`plano_barco.png`), compartida por las 4 opciones |
| Espacio de versiones | **222 preguntas sustantivamente distintas** (enumeración exhaustiva) |

---

## Particularidades operativas (léelas ANTES de tocar el `.Rmd`)

### 1. El `.Rmd` DEBE permanecer auto-contenido — NO extraer `dibujar_barco()` a un archivo externo

`validar_diversidad_sustantiva.R` (regla #22, obligatorio) hace `setwd(tempdir())` y evalúa el chunk
`data_generation` en un `new.env()` **fuera** del pipeline de `xexams()` (verificado leyendo el
script, líneas 100-109). En ese contexto `include_supplement()` no tiene el estado interno que
necesita y falla, arrastrando todo el chunk a error.

El subproyecto hermano `desplazamiento-avion-aeropuerto` **ya intentó y midió** esta
modularización con el mecanismo oficial: los 5 formatos renderizaron bien, pero el validador falló
**40/40 semillas** (`WARN_DIV_INDET`) y hubo que revertirla (ver su `docs/BACKLOG.md`, P1.1).

**No reintentar aquí.** `dibujar_barco()`, `pick_int()` y `safe_sample()` se quedan dentro del chunk.
Si un agente ve código "duplicado" respecto de
`RR/.claude/scripts/snippets_familias_rmd.R`, **es intencional**: son copias con procedencia
declarada, no deuda técnica.

### 2. `prof()` debe valer exactamente `h/2` en el tramo central — de eso depende que la clave sea verdadera

`prof(t)` (líneas 177-183) vale `h/2` para `t ∈ [0.15, 0.85]`. Eso hace que el casco toque `y_min` e
`y_max`, y por tanto que su *bounding box* sea exactamente `[x_min,x_max] × [y_min,y_max]` — que es
literalmente la respuesta correcta.

Si alguien "suaviza" el perfil para que el barco se vea más estilizado y deja de alcanzar `h/2`, **la
clave del ejercicio pasa a ser falsa** y ningún validador sintáctico lo detecta. Verificado por
enumeración exhaustiva: 0/222 casos de desajuste.

### 3. Las 4 exclusiones de `y_pool` no son paranoia

Líneas 35-40. Existen para garantizar que `GEO-COORD-03` (diagonal) tenga siempre **4 puntos
distintos**; si no, ese distractor mostraría un punto repetido y sería descartable de un vistazo.
Verificado exhaustivamente: 0 colisiones y 0 casos de `y_pool` vacío sobre las 222 combinaciones.
No "simplificar" ese filtrado.

### 4. `exshuffle: TRUE` es CORRECTO aquí — no copiar el `exshuffle: FALSE` del hermano

El subproyecto `desplazamiento-avion-aeropuerto` usa `exshuffle: FALSE` + `sample()` interno porque
tiene **opciones gráficas individuales**. Este ejercicio tiene **opciones de texto** y su Solution
identifica las opciones por contenido y por código de error, nunca por letra (regla #19). Por tanto
`exshuffle: TRUE` es lo correcto y no hay que "alinearlo" con el hermano.

### 5. La variación narrativa se limita al protagonista — A PROPÓSITO

La regla #11 (`contextos-narrativos-creativos.md`) pide 6+ plantillas narrativas con 5 tipos de
estructura. Este ejercicio **solo varía el nombre del protagonista** (8 nombres, líneas 135-139)
sobre un enunciado fijo.

Es deliberado: el enunciado y las 4 opciones se conservan **verbatim** del ítem ICFES real
`MAT-2026-1-022`, según la política registrada en la memoria del proyecto
(`feedback_respetar_enunciado_original.md`): al derivar de un ítem oficial se respeta su redacción y
sus opciones, y la metacognición se aporta en la Solution (diagnóstico por distractor), no
reescribiendo el enunciado. Reescribir el contexto narrativo para "cumplir la regla #11"
**destruiría la trazabilidad con el ítem oficial**. Ver el veredicto argumentado en
[`../docs/BACKLOG.md`](../docs/BACKLOG.md).

### 6. `Semillero*.R` y `pcielo*.tex` son FUENTE ACTIVA — no son ruido

`SemilleroUnico_v2.R`, `SemilleroMoodle_v2.R` y `SemilleroCloze.R` son los scripts de exportación
real (usan las plantillas LaTeX institucionales `pcielo.tex`, `pcielo_nosol.tex`, `solpcielo.tex`).
Parecen archivos sueltos de otro proyecto; **no lo son**. No borrarlos ni "limpiarlos".

### 7. `verificar_render.R` ≠ `SemilleroUnico_v2.R`

| Script | Propósito |
|---|---|
| `verificar_render.R` | **Verificación**: 1 versión, sin plantillas, 5 formatos + chequeo P6. Sale con status 1 si algo falla. Es el que se corre al retomar. |
| `SemilleroUnico_v2.R` | **Exportación** real: plantillas `pcielo`, 5 preguntas por examen, webquiz interactivo con `browse = TRUE`. |

No fusionarlos: el segundo abre un navegador y usa plantillas institucionales; no sirve para CI.

### 8. `plano_barco.png` es un artefacto REGENERABLE

Lo produce `dibujar_barco()` en cada render, dentro del directorio temporal de `xexams()`. La copia
que hay en la raíz del subproyecto es de una ejecución manual. No editarla a mano ni tratarla como
fuente.

### 9. `_archivo/prototipo-flujo-b/` NO es la fuente

`grafico_barco_parametrico.R` es el prototipo del Flujo B: tiene los parámetros **hardcoded**
(`x_min <- 4; x_max <- 9; ...`) y comentarios en inglés. Está **superado** por `dibujar_barco()`
dentro del `.Rmd`. Sirve para afinar visualmente el casco de forma aislada; si se toca, el cambio
debe portarse manualmente al `.Rmd`, que es la fuente de verdad.

### 10. NUNCA `set.seed()` dentro del chunk, ni semilla derivada del reloj

Verificado 2026-07-28: el `.Rmd` no contiene `set.seed`, `Sys.time`, `proc.time` ni `Sys.Date`. Este
ejercicio **no** está entre los 9 de `01-En-PreDesarrollo/` que arrastran el Incidente I del
orquestador. No reintroducirlo: colapsaría la diversidad multi-semilla.

---

## Reglas del repo raíz con mayor peso en este ejercicio

`#18` `{width=80%}` anti-`\pandocbounded` (línea 282) · `#19` letter-independence (la Solution
identifica por contenido y por código `GEO-COORD-0x`) · `#20` guard `\newcounter{none}`
(líneas 271-273, presente aunque hoy no haya tablas) · `#21` Familias 1 y 5 (`pick_int`,
`safe_sample`, construcción determinista sin bucles de reintento) · `#22` diversidad sustantiva
(222 preguntas distintas; §P6 no aplica por ser opciones de texto, pero `verificar_render.R` lo
comprueba igual).

---

## Reglas locales de este subproyecto

- [`rules/barco-parametrico.md`](rules/barco-parametrico.md) — contrato del casco paramétrico y
  cómo modificarlo sin invalidar la clave.

---

## Prohibido (heredado + reforzado aquí)

- Tocar cualquier archivo dentro de `RR/.claude/` sin el protocolo de la regla #17 (snapshot +
  `Rscript tests/testthat/test_infraestructura_claude.R`).
- `git commit --no-verify`, `PREPUSH_SKIP_TESTS=1`.
- Editar `03-En-Produccion/` o `Ejemplos-Funcionales-Rmd/`.
- `git add -A` / `git add -u` en este repo: hay archivos ajenos modificados en el árbol de trabajo.
  Agregar **solo** rutas explícitas de este subproyecto.
- Extraer `dibujar_barco()` a un archivo externo (particularidad 1).
- Alterar `prof()` de forma que el casco no alcance `h/2` en el tramo central (particularidad 2).
- Reescribir el enunciado o las opciones para "cumplir" la regla #11 (particularidad 5).
- Cambiar a `exshuffle: FALSE` por analogía con el subproyecto hermano (particularidad 4).

---

**Versión:** 1.0 · **Fecha:** 2026-07-28 · **Estado:** ACTIVO
