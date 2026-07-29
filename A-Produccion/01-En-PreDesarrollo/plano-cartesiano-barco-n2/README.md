# Coordenadas de los vértices de un barco en el plano cartesiano (SCHOICE metacognitivo, opciones de texto)

> Documento de entrada del subproyecto. Para el estado detallado de trabajo (objetivos,
> decisiones, hallazgos abiertos, riesgos) ver **[`HANDOFF.md`](HANDOFF.md)** — es la fuente
> principal, léelo primero si vas a retomar el desarrollo.

## Qué es este ejercicio

Ejercicio ICFES tipo **SCHOICE** (opción única), **metacognitivo** (Progressive Disclosure con
pool de errores conceptuales, no distractores aleatorios), de **Nivel 2** en la competencia
*Interpretación y representación*, componente *Geométrico-métrico*. Deriva del ítem real
`MAT-2026-1-022` (cuadernillo 2026-1, pregunta 116).

A diferencia de otros ejercicios del repositorio con opciones gráficas, aquí **las cuatro
opciones de respuesta son de TEXTO** (listas de coordenadas) y hay una única figura compartida
(`plano_barco.png`): un barco dibujado paramétricamente con `ggplot2` sobre un plano cartesiano
de 10×10, y el estudiante debe leer los cuatro vértices `(x, y)` del rectángulo que lo encierra.
El ítem no evalúa cálculo aritmético: evalúa si el estudiante distingue correctamente el eje
horizontal del vertical y lee el rango completo (no solo el centro) del objeto representado. Ver
el detalle pedagógico completo en [`docs/SYLLABUS.md`](docs/SYLLABUS.md).

- **Archivo fuente**: `coordenadas_vertices_plano_cartesiano_metacognitivo_interpretacion_n2_schoice_v1.Rmd`
- **Estado**: `01-En-PreDesarrollo/` (no promovido; ver [`docs/ROADMAP.md`](docs/ROADMAP.md))
- **Arquitectura técnica**: [`docs/BLUEPRINT.md`](docs/BLUEPRINT.md)
- **Trabajo pendiente priorizado**: [`docs/BACKLOG.md`](docs/BACKLOG.md)

> ### ⚠️ El `.Rmd` DEBE permanecer auto-contenido
>
> No extraigas `dibujar_barco()`, `pick_int()` ni `safe_sample()` a un archivo `.R` externo, ni
> siquiera con el mecanismo oficial `include_supplement()`. `validar_diversidad_sustantiva.R`
> (regla #22, obligatorio) hace `setwd(tempdir())` y evalúa el chunk `data_generation` en un
> `new.env()` **fuera** del pipeline de `xexams()`; ahí `include_supplement()` no tiene el estado
> interno que necesita y falla, arrastrando todo el chunk. El subproyecto hermano
> `desplazamiento-avion-aeropuerto` lo intentó y lo midió: 5 formatos renderizaban bien, pero el
> validador falló **40/40 semillas** y hubo que revertirlo. Detalle en
> [`docs/BACKLOG.md`](docs/BACKLOG.md) (P1.1) y en
> [`.claude/CLAUDE.md`](.claude/CLAUDE.md) (particularidad 1).

## Cómo verificar

Verificación rápida de salud (5 formatos + chequeo defensivo de la regla #22 §P6), sin usar las
plantillas institucionales:

```bash
Rscript verificar_render.R
```

Última corrida en esta sesión (2026-07-28): **5/5 OK** (HTML, PDF, DOCX, NOPS, Moodle), sin fuga
de nombre de archivo en el XML. Complementariamente, la coherencia matemática y la diversidad
sustantiva se verifican con los scripts del repositorio raíz:

```bash
Rscript ../../../.claude/scripts/validar_coherencia_matematica.R \
  coordenadas_vertices_plano_cartesiano_metacognitivo_interpretacion_n2_schoice_v1.Rmd
# → APROBADO, 0 errores

Rscript ../../../.claude/scripts/validar_diversidad_sustantiva.R \
  coordenadas_vertices_plano_cartesiano_metacognitivo_interpretacion_n2_schoice_v1.Rmd --n 40
# → PASS, 40/40 evaluadas, 38 valores únicos de la respuesta correcta
```

## Cómo renderizar los 4 formatos canónicos

Desde una sesión R con working directory en este subproyecto (o usando `edir`/`dir` explícitos,
como hacen `verificar_render.R` y `SemilleroUnico_v2.R`):

```r
library(exams)
archivo <- "coordenadas_vertices_plano_cartesiano_metacognitivo_interpretacion_n2_schoice_v1.Rmd"

exams2html(archivo, n = 1, dir = "output_html", edir = ".")
exams2pdf(archivo,  n = 1, dir = "output_pdf",  edir = ".")
exams2pandoc(archivo, n = 1, type = "docx", dir = "output_docx", edir = ".")
exams2nops(rep(archivo, 3), n = 1, dir = "output_nops", edir = ".")
```

Después de cada `exams2*()` el hook `post-exams2-validation.sh` del repositorio raíz ejecuta
automáticamente las FASES 2A-2N de validación (matemática, preview visual, letter-independence,
guard de tablas, diversidad estática, etc.) — ver `.claude/rules/ciclo-validacion.md` en la raíz
del repositorio.

## Cómo exportar (Semillero*.R)

**Los tres scripts `Semillero*.R` y las plantillas `pcielo.tex` / `solpcielo.tex` /
`pcielo_nosol.tex` son FUENTE ACTIVA de exportación — NO son derivados ni ruido.** Deben
trackearse en git y ejecutarse con el working directory en este subproyecto (usan rutas
relativas: `edir = "."`, `dir = "salida"`).

| Script | Propósito | Plantillas que usa |
|---|---|---|
| `SemilleroUnico_v2.R` | Exporta el ejercicio completo a PDF, DOCX, Moodle XML, NOPS y HTML interactivo (`exams2webquiz`), en una sola corrida (`numpreg <- 5`, línea 16) | `template = "solpcielo"` (línea 34, PDF) y `template = "pcielo.tex"` (línea 47, DOCX) |
| `SemilleroMoodle_v2.R` | Genera 300 copias para importar a Moodle (`copias <- 300`, `numpreg <- 1`, líneas 9-10) | Bloques PDF/DOCX comentados por defecto |
| `SemilleroCloze.R` | Plantilla exploratoria orientada a formato híbrido cloze+schoice; no aplica a este ejercicio, que es SCHOICE puro | — |

```r
# Desde el directorio del subproyecto:
source("SemilleroUnico_v2.R")   # genera salida/ con PDF, DOCX, Moodle, NOPS, forms/webquiz
```

`solpcielo.tex` produce el PDF **con solución** (uso docente); `pcielo_nosol.tex` es la variante
sin solución (para aplicar a estudiantes); `pcielo.tex` es la plantilla base que usa
`exams2pandoc()` para DOCX. Ninguna de las tres se regenera automáticamente — son plantillas
LaTeX/Pandoc mantenidas a mano, análogas en función a los templates que trae `R-exams` de fábrica
pero ajustadas al membrete institucional (I. E. Pedacito de Cielo).

## Estructura de archivos

```
plano-cartesiano-barco-n2/
├── .gitignore                     # FUENTE — ignora los derivados que la raíz no cubre
├── README.md                      # Este archivo
├── HANDOFF.md                     # Documento de reanudación — fuente principal
├── docs/
│   ├── SYLLABUS.md                # Qué enseña/evalúa el ítem
│   ├── ROADMAP.md                 # Hitos con fechas
│   ├── BACKLOG.md                 # Pendientes priorizados
│   └── BLUEPRINT.md               # Arquitectura técnica
├── .claude/
│   ├── CLAUDE.md                  # Particularidades operativas para agentes
│   └── rules/
│       └── barco-parametrico.md   # Contrato del casco: cómo tocarlo sin invalidar la clave
├── coordenadas_vertices_..._n2_schoice_v1.Rmd  # FUENTE — el ejercicio, auto-contenido (500 líneas,
│                                  #   5 chunks: data_generation, question_body, answerlist_q,
│                                  #   solucion, answerlist_s)
├── ejercicio_state.json           # Estado del workflow (11/11 pasos completados, aprobado 2026-07-01)
├── verificar_render.R             # FUENTE — verificación rápida (5 formatos + guard regla #22 §P6)
├── SemilleroUnico_v2.R            # FUENTE — exportación completa (PDF/DOCX/Moodle/NOPS/webquiz)
├── SemilleroMoodle_v2.R           # FUENTE — exportación masiva a Moodle (300 copias)
├── SemilleroCloze.R               # FUENTE — plantilla exploratoria cloze+schoice (no aplica aquí)
├── pcielo.tex                     # FUENTE — plantilla Pandoc para DOCX
├── solpcielo.tex                  # FUENTE — plantilla LaTeX con solución (PDF docente)
├── pcielo_nosol.tex               # FUENTE — plantilla LaTeX sin solución (PDF estudiante)
├── plano_barco.png                # DERIVADO — se regenera en cada render (no commitear)
├── coordenadas_..._v1.html        # DERIVADO — salida HTML de una sesión anterior
├── salida/                        # DERIVADO — salidas de SemilleroUnico_v2.R (PDF, DOCX, Moodle, NOPS, webquiz)
├── verif_render/                  # DERIVADO — salidas transitorias de verificar_render.R
├── revision/                      # DERIVADO — 12 versiones (HTML+PDF) + contact sheet para revisión humana
└── _archivo/
    ├── Copia de coordenadas_..._v1.Rmd   # copia obsoleta
    └── prototipo-flujo-b/         # prototipo Flujo B superado (parámetros hardcoded,
        │                          #   comentarios en inglés — reemplazado por el data_generation
        │                          #   actual con aleatorización real, regla #22)
        ├── grafico_barco_parametrico.R
        └── comparacion_flujo_b.png
```

**Regla fuente vs. derivado**: `plano_barco.png` y todo el contenido de `salida/`,
`verif_render/` y `revision/` se regeneran en cada `exams2*()` — nunca se editan a mano ni se
commitean como si fueran fuente. El `.Rmd`, los `Semillero*.R`, `verificar_render.R` y las tres plantillas `.tex` sí
son fuente y deben trackearse.

## Reglas del repositorio que aplican

- `../../../.claude/rules/markdown-imagenes-pdf.md` — regla #18: la única imagen del ejercicio,
  `plano_barco.png`, se referencia con atributo de ancho explícito
  (`![](plano_barco.png){width=80%}`, línea 342), nunca sin `{width=...}`.
- `../../../.claude/rules/solution-letter-independence.md` — regla #19: la Solution identifica
  cada opción por su texto de coordenadas y su código de error (`GEO-COORD-01/02/04`, líneas
  389-393), nunca por letra. Por eso este ejercicio puede usar `exshuffle: TRUE` (línea 489) sin
  romper coherencia si R/exams o Moodle reordenan las opciones.
- `../../../.claude/rules/markdown-tablas-pandoc.md` — regla #20: el guard
  `\@ifundefined{c@none}{\newcounter{none}}{}` está presente por defecto al inicio de `Question`
  (línea 332), aunque este ejercicio no usa tablas Markdown — lo aplican los skills y
  orquestadores de generación como estándar.
- `../../../.claude/rules/familias-soluciones-rmd.md` — regla #21: usa la Familia 1
  (`pick_int()`, sin `repeat` sin cota) y la Familia 5 (`safe_sample()`, protección contra la
  trampa `sample(escalar)`), ambas declaradas explícitamente en el comentario de la línea 12.
  `safe_sample()` es además lo que hace segura la restricción A′: cuando `ancho_barco == 3` el
  pool de `alto_barco` tiene un solo elemento, el caso exacto que la Familia 5 protege.
- `../../../.claude/rules/diversidad-sustantiva.md` — regla #22: `ancho_barco`, `alto_barco`,
  `x_min` y `y_min` se aleatorizan con `pick_int()`/`safe_sample()` (líneas 24-69), no son
  literales fijos. `y_pool` ya no lleva exclusiones (P2.7, 2026-07-28) y `alto_barco` está acotado
  para que `ratio ≥ 2` (P1.1/A′, 2026-07-28): espacio de versiones 222 → 374 → **318**.
  Verificado con `validar_diversidad_sustantiva.R --n 40`: PASS, 37/40 valores únicos de la
  respuesta correcta.

## Enlaces

- [HANDOFF.md](HANDOFF.md) — documento de reanudación (fuente principal)
- [docs/SYLLABUS.md](docs/SYLLABUS.md) — qué enseña y evalúa
- [docs/ROADMAP.md](docs/ROADMAP.md) — hitos con fechas
- [docs/BACKLOG.md](docs/BACKLOG.md) — pendientes priorizados
- [docs/BLUEPRINT.md](docs/BLUEPRINT.md) — arquitectura técnica
- [.claude/CLAUDE.md](.claude/CLAUDE.md) — índice local del subproyecto
- `../../../.claude/rules/` — reglas obligatorias del repositorio (índice en
  `../../../.claude/CLAUDE.md`)

---

**Versión:** 1.2 · **Fecha:** 2026-07-28 (v1.2 — P1.1 cerrado con la opción A′: `ratio ≥ 2` por
construcción, espacio de versiones 374 → 318; citas de línea re-verificadas contra el `.Rmd` de 500
líneas; v1.1 — citas de línea y códigos de error actualizados tras P0.1, P2.5 y P2.7)
