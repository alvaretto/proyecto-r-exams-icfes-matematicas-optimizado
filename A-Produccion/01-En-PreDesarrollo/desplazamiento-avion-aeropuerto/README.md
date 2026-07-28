# Desplazamiento avión→aeropuerto (SCHOICE metacognitivo, opciones gráficas)

> Documento de entrada del subproyecto. Para el estado detallado de trabajo (objetivos,
> decisiones, hallazgos abiertos, riesgos) ver **[`HANDOFF.md`](HANDOFF.md)** — es la fuente
> principal, léelo primero si vas a retomar el desarrollo.

## Qué es este ejercicio

Ejercicio ICFES tipo **SCHOICE** (opción única), **metacognitivo** (Progressive Disclosure con
pool de errores conceptuales, no distractores aleatorios), de **Nivel 3** en la competencia
*Interpretación y Representación*, componente *Geométrico-Métrico*. Deriva del ítem real
`MAT-2026-1-020` (cuadernillo 2026-1, pregunta 114).

Las **cuatro opciones de respuesta son diagramas vectoriales generados dinámicamente** (no
texto): cada opción es un PNG con la cruz de ejes cardinales, un rayo con ángulo y su etiqueta.
El estudiante debe identificar cuál diagrama representa correctamente la nueva posición del
avión respecto al aeropuerto tras avanzar una distancia dada en una dirección dada. El ítem no
evalúa cálculo aritmético: evalúa si el estudiante **integra distancia + dirección** al leer una
representación esquemática. Ver el detalle pedagógico completo en
[`docs/SYLLABUS.md`](docs/SYLLABUS.md).

- **Archivo fuente**: `desplazamiento_avion_aeropuerto_metacognitivo_interpretacion_n3_schoice_v1.Rmd`
- **Estado**: `01-En-PreDesarrollo/` (no promovido; ver [`docs/ROADMAP.md`](docs/ROADMAP.md))
- **Arquitectura técnica**: [`docs/BLUEPRINT.md`](docs/BLUEPRINT.md)
- **Trabajo pendiente priorizado**: [`docs/BACKLOG.md`](docs/BACKLOG.md)

## Cómo renderizar (4 formatos)

Desde una sesión R con working directory en este subproyecto (o usando `edir`/`dir` explícitos,
como hace `SemilleroUnico_v2.R`):

```r
library(exams)
archivo <- "desplazamiento_avion_aeropuerto_metacognitivo_interpretacion_n3_schoice_v1.Rmd"

exams2html(archivo, n = 1, dir = "output_html")
exams2pdf(archivo,  n = 1, dir = "output_pdf")
exams2pandoc(archivo, n = 1, type = "docx", dir = "output_docx")
exams2nops(archivo, n = 1, dir = "output_nops")
```

Después de cada `exams2*()` el hook `post-exams2-validation.sh` del repositorio raíz ejecuta
automáticamente las FASES 2A-2N de validación (matemática, preview visual, letter-independence,
guard de tablas, diversidad estática, etc.) — ver
`.claude/rules/ciclo-validacion.md` en la raíz del repositorio.

Para verificación rápida de salud sin renderizar PDF (barato, recomendado tras cualquier cambio
a `data_generation`):

```bash
Rscript ../../../.claude/scripts/validar_diversidad_sustantiva.R \
  desplazamiento_avion_aeropuerto_metacognitivo_interpretacion_n3_schoice_v1.Rmd --n 40
```

## Cómo exportar (Semillero*.R)

**Los tres scripts `Semillero*.R` y las plantillas `pcielo.tex` / `solpcielo.tex` /
`pcielo_nosol.tex` son FUENTE ACTIVA de exportación — NO son derivados ni ruido.** Deben
trackearse en git y ejecutarse con el working directory en este subproyecto (usan rutas
relativas: `edir = "."`, `dir = "salida"`).

| Script | Propósito | Plantillas que usa |
|---|---|---|
| `SemilleroUnico_v2.R` | Exporta el ejercicio completo a HTML, PDF, DOCX, Moodle XML, NOPS y `exams2forms`/`exams2webquiz` interactivo, en una sola corrida | `template = "solpcielo"` (línea 70, PDF) y `template = "pcielo.tex"` (línea 83, DOCX) |
| `SemilleroMoodle_v2.R` | Genera 300 copias para importar a Moodle (`copias <- 300`, `numpreg <- 1`) | Mismas plantillas, bloques PDF/DOCX comentados por defecto |
| `SemilleroCloze.R` | Variante orientada a formato híbrido cloze+schoice (uso exploratorio) | — |

```r
# Desde el directorio del subproyecto:
source("SemilleroUnico_v2.R")   # genera salida/ con PDF, DOCX, Moodle, NOPS, forms/webquiz
```

`solpcielo.tex` produce el PDF **con solución** (para uso docente); `pcielo_nosol.tex` es la
variante sin solución (para aplicar a estudiantes); `pcielo.tex` es la plantilla base que usa
`exams2pandoc()` para DOCX. Ninguna de las tres se regenera automáticamente — son plantillas
LaTeX/Pandoc mantenidas a mano, análogas en función a los templates que trae `R-exams` de fábrica
pero ajustadas al membrete institucional (I. E. Pedacito de Cielo).

## Estructura de archivos

```
desplazamiento-avion-aeropuerto/
├── desplazamiento_..._v1.Rmd      # FUENTE — el ejercicio (7 chunks; el data_generation
│                                  #   abre con índice de 14 secciones + 5 invariantes)
├── SemilleroUnico_v2.R            # FUENTE — exportación completa (HTML/PDF/DOCX/Moodle/NOPS)
├── SemilleroMoodle_v2.R           # FUENTE — exportación masiva a Moodle (300 copias)
├── SemilleroCloze.R               # FUENTE — variante cloze+schoice (exploratorio)
├── pcielo.tex                     # FUENTE — plantilla Pandoc para DOCX
├── solpcielo.tex                  # FUENTE — plantilla LaTeX con solución (PDF docente)
├── pcielo_nosol.tex               # FUENTE — plantilla LaTeX sin solución (PDF estudiante)
├── ejercicio_state.json           # Estado del workflow (11 pasos); ver §3 de HANDOFF.md
├── referencia_original.png        # Insumo del Flujo B — imagen del cuadernillo ICFES original
├── HANDOFF.md                     # Documento de reanudación — fuente principal del subproyecto
├── README.md                      # Este archivo
├── docs/                          # Documentación local (este pase de trabajo)
│   ├── SYLLABUS.md                # Qué enseña/evalúa el ítem
│   ├── ROADMAP.md                 # Hitos con fechas
│   ├── BACKLOG.md                 # Pendientes priorizados P0/P1/P2
│   └── BLUEPRINT.md               # Arquitectura técnica
├── diagrama_correcta.png          # DERIVADO — se regenera en cada render (no commitear)
├── diagrama_recorrida.png         # DERIVADO — ídem
├── diagrama_suma.png              # DERIVADO — ídem
├── diagrama_perp.png              # DERIVADO — ídem
├── output_html/ output_pdf/       # DERIVADO — salidas de exams2*(); no fuente de verdad
├── output_docx/ output_nops/      # DERIVADO — ídem
├── output_tikz_v1.tex/.pdf        # DERIVADO — artefacto de una iteración pasada del Flujo B
├── output_python_v1.py            # DERIVADO — ídem (versión Python del Flujo B, no seleccionada)
├── output_r_v1.R                  # DERIVADO — ídem (versión R nativa del Flujo B, no seleccionada)
└── _archivo/                      # DERIVADOS históricos archivados el 2026-07-28 (ver abajo)
    ├── salida-semillero/salida/   # corridas previas de los Semillero*.R
    ├── renders-auditoria/VER*/    # capturas de auditoría visual de sesiones pasadas
    └── graficos-obsoletos/        # diagrama_[a-d].svg — nomenclatura de letras (a/b/c/d) ya
                                    # reemplazada por la nomenclatura semántica vigente
                                    # (correcta/recorrida/suma/perp)
```

**Regla fuente vs. derivado**: los 4 `diagrama_*.png` en la raíz del subproyecto y todo el
contenido de `output_*/` se regeneran en cada `exams2*()` — nunca se editan a mano ni se
commitean como si fueran fuente. El `.Rmd`, los `Semillero*.R` y las tres plantillas `.tex` sí
son fuente y deben trackearse.

## Reglas del repositorio que aplican

Ver la lista completa con justificación en [`HANDOFF.md` §8](HANDOFF.md#8-reglas-del-repo-que-aplican-a-este-subproyecto).
Resumen de enlaces directos:

- `../../../.claude/rules/graficos-como-opciones.md` — regla #4/graficos (opciones como PNG
  individuales, sin títulos con letra, `exshuffle: FALSE` + `sample()` interno)
- `../../../.claude/rules/markdown-imagenes-pdf.md` — regla #18 (`{width=...}` obligatorio)
- `../../../.claude/rules/solution-letter-independence.md` — regla #19 (Solution nunca referencia
  la letra de la opción correcta)
- `../../../.claude/rules/markdown-tablas-pandoc.md` — regla #20 (guard `\newcounter{none}`)
- `../../../.claude/rules/diversidad-sustantiva.md` — regla #22 (la respuesta correcta debe
  variar sustantivamente entre versiones; **regla originada en este subproyecto**)
- `../../../.claude/rules/ejercicios-metacognitivos.md` — Progressive Disclosure, pool de
  errores conceptuales, metadatos DOK/Bloom/SOLO
- `../../../.claude/docs/patrones-errores-conocidos.md` — Errores 22, 23 y 24 (los dos últimos
  originados en este subproyecto)

## Enlaces

- [HANDOFF.md](HANDOFF.md) — documento de reanudación (fuente principal)
- [docs/SYLLABUS.md](docs/SYLLABUS.md) — qué enseña y evalúa
- [docs/ROADMAP.md](docs/ROADMAP.md) — hitos con fechas
- [docs/BACKLOG.md](docs/BACKLOG.md) — pendientes priorizados
- [docs/BLUEPRINT.md](docs/BLUEPRINT.md) — arquitectura técnica
