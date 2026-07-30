# Permutaciones de los pescadores en la venia final (SCHOICE + CLOZE metacognitivos, opciones de texto)

> Documento de entrada del subproyecto. Para el estado detallado de trabajo (objetivos,
> decisiones, hallazgos abiertos, riesgos) ver **[`HANDOFF.md`](HANDOFF.md)** — es la fuente
> principal, léelo primero si vas a retomar el desarrollo.

> **Dos variantes conviven en este subproyecto.** La raíz contiene el **SCHOICE**, fiel verbatim
> al ítem oficial `MAT-2026-1-004` en una sola pregunta (instancia canónica). El subdirectorio
> [`cloze/`](cloze/) contiene una variante **CLOZE** de 6 partes Progressive Disclosure que
> descompone el mismo razonamiento en pasos formativos — **no sustituye** a la SCHOICE, la
> complementa. Ver [«Variante CLOZE»](#variante-cloze-cloze) más abajo.

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
de siete**) y el enunciado pide contar de cuántas formas pueden ubicarse `n` pescadores en fila
para una venia. El ítem no evalúa aritmética compleja — evalúa si el estudiante reconoce que el
conteo es **sin reemplazo** (una permutación, `n!`) y no lo confunde con una variación **con**
reemplazo, un producto truncado, el simple cardinal del conjunto, una permutación circular o el
principio aditivo. Ver el detalle pedagógico completo en [`docs/SYLLABUS.md`](docs/SYLLABUS.md).

- **Archivo fuente (SCHOICE)**: `permutaciones_pescadores_metacognitivo_formulacion_n4_schoice_v1.Rmd`
  (601 líneas, 4 chunks R + 1 guard LaTeX — ver [`docs/BLUEPRINT.md`](docs/BLUEPRINT.md))
- **Estado (SCHOICE)**: `02-En-Desarrollo/` — **aprobado y listo para aula** (11/11 pasos;
  aprobación humana del 2026-07-30). Pendiente la evidencia de Nivel 3 para `03-En-Produccion/`;
  ver [`docs/ROADMAP.md`](docs/ROADMAP.md)
- **Archivo fuente (CLOZE)**: `cloze/permutaciones_pescadores_metacognitivo_formulacion_n4_cloze_v1.Rmd`
  (971 líneas, 9 chunks R + 1 guard LaTeX — ver [«Variante CLOZE»](#variante-cloze-cloze) y
  [`docs/BLUEPRINT.md`](docs/BLUEPRINT.md) §7)
- **Estado (CLOZE)**: `cloze/` — **10/11 pasos** (`cloze/ejercicio_state.json`); falta únicamente
  `aprobacion_usuario`, que es humano (OE12 en [`docs/ROADMAP.md`](docs/ROADMAP.md))
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
> [`docs/BACKLOG.md`](docs/BACKLOG.md) (P1.1) y en `.claude/CLAUDE.md`. La misma restricción
> aplica al chunk `data_generation` de la variante CLOZE (`cloze/`), que declara la misma
> auto-contención en su propio comentario de cabecera.

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

## Variante CLOZE (`cloze/`)

El subdirectorio [`cloze/`](cloze/) contiene una **segunda variante** del mismo ítem: un CLOZE de
**6 partes** Progressive Disclosure, para uso formativo en aula — **no sustituye** a la SCHOICE de
la raíz, que mantiene la fidelidad verbatim al ítem oficial en una sola pregunta. Nace el mismo día
(2026-07-30), reutilizando **íntegro** el contrato paramétrico del SCHOICE (`N_POOL`, clave `n!`,
pool de 7 errores, invariantes I-1..I-7 e instancia canónica) y el patrón de subdirectorio del
hermano [`Rango-Colesterol-Pacientes/Cloze/`](../../01-En-PreDesarrollo/Rango-Colesterol-Pacientes/Cloze/),
que ya convive así con su SCHOICE. Arquitectura completa en
[`docs/BLUEPRINT.md`](docs/BLUEPRINT.md) §7.

- **Archivo fuente**: `cloze/permutaciones_pescadores_metacognitivo_formulacion_n4_cloze_v1.Rmd`
  (971 líneas, 9 chunks R — `data_generation`, `enunciado`, `parte2`, `parte3`, `parte4`, `parte6`,
  `answerlist_q`, `solucion`, `answerlist_s` — + 1 guard LaTeX)
- **Verificador propio**: `cloze/verificar_render.R` (587 líneas, chequeos `V1`-`V11`)
- **Estado**: `cloze/ejercicio_state.json` — 10/11 pasos; falta
  `aprobacion_usuario` (humano)

### Las 6 partes

`exclozetype: schoice|num|schoice|num|mchoice|schoice`, `exshuffle: TRUE`, `extol: 0|0|0|0|0|0`:

| Parte | Tipo | Qué pide |
|---|---|---|
| 1 | `schoice` | La pregunta del ítem oficial: clave `n!` + 3 distractores del pool |
| 2 | `num` | Cuántos pescadores quedan disponibles para el segundo lugar → `n-1` |
| 3 | `schoice` | Identificar, entre 4 descripciones, el error que produce un valor dado (los 3 mostrados en la Parte 1 + 1 error del pool que NO se mostró) |
| 4 | `num` | Conteo **con** repetición: códigos de `n` cifras con dígitos de `{1..n}` repetibles → `n^n` — el ítem espejo `MAT-2026-1-029` convertido en pregunta |
| 5 | `mchoice` | 6 afirmaciones, de las que son verdaderas `k ∈ {2,3,4}` (varía por versión para que «siempre 3 verdaderas» no sea un atajo) |
| 6 | `schoice` V/F | Factor de crecimiento al pasar de `n` a `n+1` elementos |

El Answerlist del enunciado tiene **16** ítems (4+4+6+2, solo los gaps de elección) y el de la
Solution tiene **18** (+1 por cada uno de los 2 gaps `num` — Partes 2 y 4). Es el contrato normal
de R/exams para CLOZE con gaps mixtos, no un descuadre — ver
[`docs/BLUEPRINT.md`](docs/BLUEPRINT.md) §7.3 (invariante C-3).

### NOPS es N/A — no un formato pendiente

`exams2nops()` rechaza **cualquier** ejercicio con `extype: cloze`, sin importar los tipos de gap
que use — verificado en el código fuente de `exams` 2.4.2 (`wrong_type <- ufile[utype == "cloze"]`,
consultado 2026-07-30; la documentación oficial no lo declara explícitamente en prosa, ver
[`docs/BACKLOG.md`](docs/BACKLOG.md) P2.3). Los formatos aplicables a esta variante son **HTML,
PDF, DOCX y Moodle**, no los 4 formatos canónicos que sí aplican a la SCHOICE.

### Cómo verificar la variante CLOZE

```bash
cd cloze/
Rscript verificar_render.R
# → V1-V11 todo verde: V5 12/12 versiones con Parte1 = n!, Parte2 = n-1, Parte4 = n^n;
#   V6 105/105 ternas (93 legales); V9 240/240; V10 los 8 valores por n distintos dos a dos;
#   V11 6/6 afirmaciones coherentes

cd ..
Rscript ../../../.claude/scripts/validar_coherencia_matematica.R \
  cloze/permutaciones_pescadores_metacognitivo_formulacion_n4_cloze_v1.Rmd
# → APROBADO, 0 errores

Rscript ../../../.claude/scripts/validar_diversidad_sustantiva.R \
  cloze/permutaciones_pescadores_metacognitivo_formulacion_n4_cloze_v1.Rmd --n 40
# → exit 0 · WARN_DIV_BAJA (estructural y aceptado, igual que en la SCHOICE — 3 claves posibles)

Rscript ../../../.claude/scripts/corregir_ortografia_espanol.R \
  cloze/permutaciones_pescadores_metacognitivo_formulacion_n4_cloze_v1.Rmd
# → sin errores
```

Sobre 300 evaluaciones del `data_generation`: **300/300** versiones únicas, **90 de las 93** ternas
legales alcanzadas, reparto de `n` 90/113/97 y **12** instancias canónicas. La instancia canónica
(contexto 1, `n = 4`) reproduce `MAT-2026-1-004` **verbatim en la Parte 1**, con sus cuatro
opciones oficiales {24, 64, 16, 4}.

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
│   ├── ROADMAP.md                 # Hitos y objetivos específicos (OE1-OE12)
│   ├── BACKLOG.md                 # Pendientes priorizados
│   └── BLUEPRINT.md               # Arquitectura técnica (decisiones D1-D6, invariantes)
├── .claude/
│   ├── CLAUDE.md                  # 13 particularidades operativas
│   └── rules/
│       └── permutaciones-parametricas.md  # Contrato del pool n! e invariantes I-1..I-7
├── permutaciones_pescadores_..._n4_schoice_v1.Rmd  # FUENTE (SCHOICE) — auto-contenido, 601
│                                  #   líneas, 4 chunks R (data_generation, question_body,
│                                  #   answerlist_q, solucion) + 1 guard LaTeX
├── ejercicio_state.json           # Estado del workflow SCHOICE (ver nota de sincronización en ROADMAP)
├── verificar_render.R             # FUENTE (SCHOICE) — verificación rápida (V1-V9)
├── cloze/                         # Variante CLOZE — no sustituye a la SCHOICE, la complementa
│   ├── permutaciones_pescadores_..._n4_cloze_v1.Rmd  # FUENTE — 971 líneas, 9 chunks R
│   │                              #   (data_generation, enunciado, parte2, parte3, parte4,
│   │                              #   parte6, answerlist_q, solucion, answerlist_s) + 1 guard
│   ├── ejercicio_state.json       # Estado del workflow CLOZE — 10/11 pasos
│   ├── verificar_render.R         # FUENTE — verificación rápida (V1-V11), 587 líneas
│   └── verif_render/              # DERIVADO — salidas de verificar_render.R (ignorado)
├── SemilleroUnico_v2.R            # FUENTE — exportación institucional: 1 versión con membrete
├── SemilleroMoodle_v2.R           # FUENTE — exportación institucional: banco Moodle (100 copias)
├── SemilleroCloze.R               # FUENTE — exportación institucional heredada del hermano;
│                                  #   genérica y sin adaptar, distinta del subdirectorio `cloze/`
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
derivado de render. El mismo patrón fuente/derivado aplica dentro de `cloze/`: su `.Rmd`,
`verificar_render.R` y `ejercicio_state.json` son fuente; `cloze/verif_render/` es derivado.

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
- `../../../.claude/rules/familias-soluciones-rmd.md` — regla #21: el SCHOICE **define**
  `pick_int()` en el comentario de cabecera del chunk, pero no lo invoca en ningún otro punto del
  archivo — queda como código muerto (ver [`docs/BACKLOG.md`](docs/BACKLOG.md) P2.2, decisión
  pendiente). La Familia 1 se cumple por la vía que importa: ningún bucle de reintento — la terna
  de errores se elige enumerando el espacio legal (`utils::combn`) y sorteando un índice con
  `safe_sample()` (Familia 5). La variante CLOZE (`cloze/`) directamente **no define** `pick_int()`,
  por la misma razón declarada en su propio comentario de cabecera. A diferencia del hermano
  `plano-cartesiano-barco-n2`, ninguno de los pools de este ejercicio (`N_POOL`, contextos,
  reflexiones) colapsa hoy a longitud 1 — `safe_sample()` se usa aquí de forma
  defensiva/consistente con el patrón del repo, no porque el caso límite se dispare actualmente.
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
- [docs/ROADMAP.md](docs/ROADMAP.md) — hitos y objetivos específicos (OE1-OE12)
- [docs/BACKLOG.md](docs/BACKLOG.md) — pendientes priorizados
- [docs/BLUEPRINT.md](docs/BLUEPRINT.md) — arquitectura técnica (SCHOICE en §1-6, variante CLOZE
  en §7)
- [.claude/CLAUDE.md](.claude/CLAUDE.md) — índice local: 13 particularidades operativas
- [.claude/rules/permutaciones-parametricas.md](.claude/rules/permutaciones-parametricas.md) —
  contrato local: la clave `n!`, el pool de siete errores conceptuales y las invariantes I-1..I-7
- [cloze/](cloze/) — variante CLOZE de 6 partes (no sustituye a la SCHOICE; ver
  [«Variante CLOZE»](#variante-cloze-cloze) arriba)
- `../../../.claude/rules/` — reglas obligatorias del repositorio (índice en
  `../../../.claude/CLAUDE.md`)

---

**Versión**: 3.0 (documentada la variante CLOZE en `cloze/`; corregida deriva de documentación:
pool «cinco» → siete, 585 → 601 líneas, HANDOFF.md y `.claude/CLAUDE.md` ya existen, banco Moodle
300 → 100 copias, duplicado de `pick_int()` en la regla #21 corregido)
**Fecha**: 2026-07-30
