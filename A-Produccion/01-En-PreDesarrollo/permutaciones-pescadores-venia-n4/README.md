# Permutaciones de los pescadores en la venia final (SCHOICE metacognitivo, opciones de texto)

> Documento de entrada del subproyecto. Para el estado detallado de trabajo (objetivos,
> decisiones, hallazgos abiertos, riesgos) ver **[`HANDOFF.md`](HANDOFF.md)** — es la fuente
> principal, léelo primero si vas a retomar el desarrollo. *(Pendiente de escribir en esta
> sesión — lo redacta otro agente; el enlace queda listo desde ya.)*

## Qué es este ejercicio

Ejercicio ICFES tipo **SCHOICE** (opción única), **metacognitivo** (Progressive Disclosure con
pool de errores conceptuales, no distractores aleatorios), de **Nivel 4** en la competencia
*Formulación y ejecución*, componente *Aleatorio*. Deriva del ítem real `MAT-2026-1-004`
(simulacro ERA-2026 Matemáticas, Sesión 1, pregunta impresa 4), a su vez verbatim de
`MAT-2026-1-098` del cuadernillo Matemáticas 2026-1. Clave oficial del ítem: **B = 24 = 4!**.

A diferencia de sus dos hermanos del repositorio (`desplazamiento-avion-aeropuerto`, con opciones
gráficas, y `plano-cartesiano-barco-n2`, con opciones de texto pero una figura compartida), este
ejercicio **no tiene ninguna figura** (Flujo B = `false`): las cuatro opciones son valores
numéricos (el factorial de `n` y tres distractores conceptuales, elegidos por versión de un **pool
de cinco**) y el enunciado pide contar de cuántas formas pueden ubicarse `n` pescadores en fila
para una venia. El ítem no evalúa aritmética compleja — evalúa si el estudiante reconoce que el
conteo es **sin reemplazo** (una permutación, `n!`) y no lo confunde con una variación **con**
reemplazo, un producto truncado, el simple cardinal del conjunto, una permutación circular o el
principio aditivo. Ver el detalle pedagógico completo en [`docs/SYLLABUS.md`](docs/SYLLABUS.md).

- **Archivo fuente**: `permutaciones_pescadores_metacognitivo_formulacion_n4_schoice_v1.Rmd`
  (481 líneas, 4 chunks R + 1 guard LaTeX — ver [`docs/BLUEPRINT.md`](docs/BLUEPRINT.md))
- **Estado**: `01-En-PreDesarrollo/` (no promovido; ver [`docs/ROADMAP.md`](docs/ROADMAP.md))
- **Arquitectura técnica**: [`docs/BLUEPRINT.md`](docs/BLUEPRINT.md)
- **Trabajo pendiente priorizado**: [`docs/BACKLOG.md`](docs/BACKLOG.md)

> ### ⚠️ El `.Rmd` DEBE permanecer auto-contenido
>
> No extraigas `pick_int()`, `safe_sample()`, `fmt()` ni el pool `errores_conceptuales` a un
> archivo `.R` externo, ni siquiera con el mecanismo oficial `include_supplement()`. El chunk
> `data_generation` del `.Rmd` lo declara explícitamente en su comentario de
> cabecera: `validar_diversidad_sustantiva.R` (regla #22, obligatorio) hace
> `setwd(tempdir())` y evalúa el chunk en un `new.env()` **fuera** del pipeline de `xexams()`; ahí
> `include_supplement()` no tiene el estado interno que necesita y falla. El subproyecto hermano
> `desplazamiento-avion-aeropuerto` lo intentó y lo midió: 5 formatos renderizaban bien, pero el
> validador falló **40/40 semillas** y hubo que revertirlo — el mismo precedente que documenta
> `plano-cartesiano-barco-n2` en su propio README. Detalle en
> [`docs/BACKLOG.md`](docs/BACKLOG.md) (P1.1) y en `.claude/CLAUDE.md` (pendiente de escribir en
> esta sesión).

## Cómo verificar

Verificación rápida de salud (9 chequeos, `V1`-`V9`), sin plantillas institucionales:

```bash
Rscript verificar_render.R
```

Última corrida (2026-07-30, sobre el pool de 7 de la decisión D4): **todo verde** — HTML, PDF, DOCX y
NOPS renderizan (`V1`-`V4`); la opción marcada como correcta en el XML de Moodle es exactamente `n!`
en **12/12** preguntas exportadas (`V5`); `V6` enumera **exhaustivamente** las 105 ternas posibles
(3 valores de `n` × C(7,3) = 35 combinaciones de errores) y confirma que las 4 opciones son siempre
distintas, ninguna coincide con la correcta y la razón máx/clave se mantiene dentro del umbral de
15×. Sobre las **93 ternas legales** (las que cumplen I-7), el rango de la clave por magnitud es
1.º, 2.º o 3.º —**nunca 4.º**—, queda en la mitad baja el 41,9 % de las veces y «elegir el número
mayor» acierta el 0,0 %; `V9` comprueba además sobre 240 semillas que la selección real del chunk se
queda en ese espacio legal (ver
[`docs/BLUEPRINT.md`](docs/BLUEPRINT.md) §3); el contexto canónico con `n = 4` reproduce
**verbatim** el enunciado y la pregunta de `MAT-2026-1-004` (`V7`); `V8` es N/A informativo porque
el ejercicio no genera imágenes.

Complementariamente, la coherencia matemática, la diversidad sustantiva y la ortografía se
verifican con los scripts del repositorio raíz:

```bash
Rscript ../../../.claude/scripts/validar_coherencia_matematica.R \
  permutaciones_pescadores_metacognitivo_formulacion_n4_schoice_v1.Rmd
# → APROBADO, 0 errores (Capas A/B/C + Nivel 5A-5E)

Rscript ../../../.claude/scripts/validar_diversidad_sustantiva.R \
  permutaciones_pescadores_metacognitivo_formulacion_n4_schoice_v1.Rmd --n 40
# → exit 0 · 40/40 evaluadas · 3 claves posibles (24, 120, 720) · WARN_DIV_BAJA
#   (esperado y aceptado — ver docs/BACKLOG.md P1.2, no es deuda accionable)

Rscript ../../../.claude/scripts/corregir_ortografia_espanol.R \
  permutaciones_pescadores_metacognitivo_formulacion_n4_schoice_v1.Rmd
# → sin errores
```

El detalle completo de la evidencia (incluida la corrida de 300 evaluaciones del
`data_generation` fuera del validador, con **298/300** versiones únicas de render, **89 de las 93**
ternas legales alcanzadas y **16** instancias canónicas entre las 300) está en
[`docs/ROADMAP.md`](docs/ROADMAP.md) §1-2.

## Cómo renderizar los 4 formatos canónicos

Desde una sesión R con working directory en este subproyecto (o usando `edir`/`dir` explícitos,
como hace `verificar_render.R`):

```r
library(exams)
archivo <- "permutaciones_pescadores_metacognitivo_formulacion_n4_schoice_v1.Rmd"

exams2html(archivo, n = 1, dir = "output_html", edir = ".")
exams2pdf(archivo,  n = 1, dir = "output_pdf",  edir = ".")
exams2pandoc(archivo, n = 1, type = "docx", dir = "output_docx", edir = ".")
exams2nops(rep(archivo, 3), n = 1, dir = "output_nops", edir = ".")
```

Después de cada `exams2*()` el hook `post-exams2-validation.sh` del repositorio raíz ejecuta
automáticamente las FASES 2A-2N de validación (matemática, preview visual, letter-independence,
guard de tablas, diversidad estática, etc.) — ver `.claude/rules/ciclo-validacion.md` en la raíz
del repositorio.

## Exportación institucional

El subproyecto ya tiene los scripts `SemilleroUnico_v2.R`, `SemilleroMoodle_v2.R`,
`SemilleroCloze.R` y las plantillas `pcielo.tex` / `solpcielo.tex` / `pcielo_nosol.tex`, que exportan
el ejercicio con el membrete institucional (I. E. Pedacito de Cielo) a PDF/DOCX/Moodle/NOPS y al
webquiz interactivo. Se copiaron del hermano `plano-cartesiano-barco-n2` y la única diferencia
funcional es `archivo_examen`.

```bash
Rscript SemilleroUnico_v2.R     # 1 versión con membrete: PDF + DOCX + HTML interactivo + NOPS
Rscript SemilleroMoodle_v2.R    # banco de 100 preguntas -> XML de importación a Moodle
```

Todo lo que escriben va a `salida/`, que es **derivado** (ignorado por git) y se regenera cuando se
necesite. `verificar_render.R` cubre otro propósito: verificación rápida sin membrete, para CI.

`copias <- 100` en `SemilleroMoodle_v2.R` es el tamaño de banco **decidido por el usuario**
(2026-07-30), documentado en el propio script. Ojo con una confusión fácil: la regla #3 de
`../../../.claude/rules/codigo-rmd.md` («< 200 versiones únicas») se refiere a la **capacidad del
ejercicio** —se valida con `exams2html(n = 200)` y aquí da 298/300—, no al número de preguntas que se
exportan. Si cambias `copias`, deja el porqué escrito: el defecto real del caso análogo en el hermano
del avión fue cambiarlo **sin comentario**. Ver [`docs/BACKLOG.md`](docs/BACKLOG.md) P2.1.

## Estructura de archivos

```
permutaciones-pescadores-venia-n4/
├── README.md                      # Este archivo
├── HANDOFF.md                     # Documento de reanudación — fuente principal
├── docs/
│   ├── SYLLABUS.md                # Qué enseña/evalúa el ítem
│   ├── ROADMAP.md                 # Hitos y objetivos específicos (OE1-OE11)
│   ├── BACKLOG.md                 # Pendientes priorizados
│   └── BLUEPRINT.md               # Arquitectura técnica (decisiones D1-D4, invariantes)
├── .claude/
│   ├── CLAUDE.md                  # 13 particularidades operativas
│   └── rules/
│       └── permutaciones-parametricas.md  # Contrato del pool n! e invariantes I-1..I-7
├── permutaciones_pescadores_..._n4_schoice_v1.Rmd  # FUENTE — auto-contenido, 585 líneas,
│                                  #   4 chunks R (data_generation, question_body, answerlist_q,
│                                  #   solucion) + 1 guard LaTeX
├── ejercicio_state.json           # Estado del workflow (ver nota de sincronización en ROADMAP)
├── verificar_render.R             # FUENTE — verificación rápida (V1-V9)
├── SemilleroUnico_v2.R            # FUENTE — exportación institucional: 1 versión con membrete
├── SemilleroMoodle_v2.R           # FUENTE — exportación institucional: banco Moodle (300 copias)
├── SemilleroCloze.R               # FUENTE — variante CLOZE de la exportación (heredada)
├── pcielo.tex                     # FUENTE — plantilla LaTeX institucional (con solución)
├── pcielo_nosol.tex               # FUENTE — plantilla LaTeX institucional (sin solución)
├── solpcielo.tex                  # FUENTE — plantilla LaTeX de solucionario
├── salida/                        # DERIVADO — exportación institucional (ignorado)
└── verif_render/                  # DERIVADO — salidas de verificar_render.R (ignorado)
```

**Regla fuente vs. derivado**: `verif_render/`, `salida/` y los `.html`/`.pdf`/`.docx`/`.xml`/`.rds`
de la raíz del subproyecto se regeneran con `verificar_render.R` o los `Semillero*.R` — nunca se
editan a mano ni se commitean como si fueran fuente, y el `.gitignore` los cubre. El `.Rmd`,
`verificar_render.R`, los `Semillero*.R` y las plantillas `pcielo*.tex`/`solpcielo.tex` sí son fuente
y deben trackearse. `ejercicio_state.json` es estado persistente del workflow (regla #16), no un
derivado de render.

## Reglas del repositorio que aplican

- `../../../.claude/rules/ejercicios-metacognitivos.md` — regla #1: pool de **siete** errores
  conceptuales con código, `precondicion` y `calcula()` (pool `errores_conceptuales`; 3 → 5 en la
  auditoría adversarial del 2026-07-29 y 5 → 7 en la decisión D4 del 2026-07-30 — la regla exige
  «mínimo 4-6», un piso), de los que se eligen 3 por versión del espacio legal (bloque «Selección de
  los 3 errores que se muestran»), salvo la excepción canónica (decisión D3) que fuerza los 3
  oficiales. Solution con las 6 subsecciones canónicas (chunk `solucion`). Ver la nota de coherencia
  DOK↔Nivel en [`docs/SYLLABUS.md`](docs/SYLLABUS.md) §1 y las decisiones D1/D3/D4 en
  [`docs/BLUEPRINT.md`](docs/BLUEPRINT.md) §4.2, §4.8 y §4.9.
- `../../../.claude/rules/codigo-rmd.md` — regla #8: filtrado genérico por `precondicion`
  declarada (`aplicables <- which(vapply(...))`), no `if` hardcoded; regla #10: sin `set.seed()`
  dentro del chunk (verificado por `test_permutaciones_invariantes.R`).
- `../../../.claude/rules/contextos-narrativos-creativos.md` — regla #11: 6 plantillas
  narrativas, 6 estructuras gramaticales distintas (lista `contextos`), ninguna usa el verbo
  «registró». El contexto 1 es canónico y reproduce verbatim el ítem oficial.
- `../../../.claude/rules/solution-letter-independence.md` — regla #19: la Solution identifica
  cada opción por su contenido y su código de error (cualquiera de los siete `EST-PER-01` a `07`,
  bucle sobre `errores_info` en el chunk `solucion`), nunca por letra. Por eso este ejercicio usa
  `exshuffle: TRUE` sin riesgo de incoherencia si Moodle reordena las opciones.
- `../../../.claude/rules/markdown-tablas-pandoc.md` — regla #20: el guard
  `\@ifundefined{c@none}{\newcounter{none}}{}` está presente al inicio de `Question`,
  aunque este ejercicio no usa tablas Markdown — lo aplican los skills y orquestadores de
  generación como estándar.
- `../../../.claude/rules/familias-soluciones-rmd.md` — regla #21: usa la Familia 1 (`pick_int()`,
  `pick_int()`) y la Familia 5 (`safe_sample()`), declaradas explícitamente en el comentario de
  cabecera del chunk. A diferencia del hermano `plano-cartesiano-barco-n2`, ninguno de los
  pools de este ejercicio (`N_POOL`, contextos, reflexiones) colapsa hoy a longitud 1 — `safe_sample()`
  se usa aquí de forma defensiva/consistente con el patrón del repo, no porque el caso límite se
  dispare actualmente.
- `../../../.claude/rules/diversidad-sustantiva.md` — regla #22: `n` se aleatoriza con
  `safe_sample(N_POOL, 1L)`, nunca es un literal fijo. El espacio de respuestas
  correctas distintas sigue siendo **3** (`n ∈ {4,5,6}` → claves 24/120/720) — los distractores
  oficiales del ítem fijan esa cardinalidad, ver la discusión completa (no accionable sin
  apartarse de la ficha oficial) en [`docs/BACKLOG.md`](docs/BACKLOG.md) P1.2 — pero desde que el
  pool pasó de 3 a 7 errores, el **tipo** de distractor mostrado sí varía: 89 de las 93 ternas
  legales se alcanzan en 300 evaluaciones (ver [`docs/ROADMAP.md`](docs/ROADMAP.md) §1). El patrón
  P4 (predictibilidad posicional) se cubre además con la invariante **I-7**, que impide que la clave
  sea la opción de mayor magnitud — ver [`docs/BACKLOG.md`](docs/BACKLOG.md) H1.

## Enlaces

- [HANDOFF.md](HANDOFF.md) — documento de reanudación (fuente principal)
- [docs/SYLLABUS.md](docs/SYLLABUS.md) — qué enseña y evalúa
- [docs/ROADMAP.md](docs/ROADMAP.md) — hitos y objetivos específicos (OE1-OE11)
- [docs/BACKLOG.md](docs/BACKLOG.md) — pendientes priorizados
- [docs/BLUEPRINT.md](docs/BLUEPRINT.md) — arquitectura técnica
- [.claude/CLAUDE.md](.claude/CLAUDE.md) — índice local: 13 particularidades operativas
- [.claude/rules/permutaciones-parametricas.md](.claude/rules/permutaciones-parametricas.md) —
  contrato local: la clave `n!`, el pool de siete errores conceptuales y las invariantes I-1..I-7
- `../../../.claude/rules/` — reglas obligatorias del repositorio (índice en
  `../../../.claude/CLAUDE.md`)

---

**Versión**: 2.0 (pool de 7 e invariante I-7 tras la decisión D4; exportación institucional presente)
**Fecha**: 2026-07-30
