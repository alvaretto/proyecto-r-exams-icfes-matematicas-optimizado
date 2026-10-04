# HANDOFF — barras-campeonato-baloncesto-n3 (última actualización: 2026-10-04)

**Disparador:** «Continúa con barras-campeonato-baloncesto-n3»
**Ciclo vigente:** `mega-prompt-barras-campeonato-baloncesto.md` (alcance: solo SCHOICE, decisión del usuario 2026-10-04).

## PREGUNTA_AL_RETOMAR
«¿Apruebas los cambios del 2026-10-04 en la Solution (concordancia en ajedrez, frase del análisis del error, encabezados) y decides si se incorpora el distractor E8 «borde superior»?»

## Estado en una línea
SCHOICE con D-1…D-14 en verde (2026-10-04, `.Rmd` md5 `0aec92ad`); la aprobación del profesor del 2026-10-03 NO cubre los cambios de hoy en la Solution.

## Objetivos
Fuente: `mega-prompt-barras-campeonato-baloncesto.md` §1 y §4. Copia en memoria:
`project_objetivos_barras_campeonato_baloncesto_n3.md`.

**General:** que el `.Rmd` que llegue al aula tenga cero defectos (matemática, visual,
documentación ICFES, Solution, ortografía, render) en todas las versiones y formatos; «cero» =
batería D-1…D-14 verde con salida real. Además: fidelidad al ítem oficial y retomable con una frase.

| OE | Dominio | Checks | Veredicto final 2026-10-04 (tras el ciclo) |
|---|---|---|---|
| OE1 | Matemática | D-1 D-2 D-3 | CUMPLIDO (D-1..D-3 verdes; verificador del dibujo 100/100) |
| OE2 | Visual | D-4 D-5 D-6 | CUMPLIDO (multicopia 60/120, sin «0.0.») |
| OE3 | Render + Moodle | D-7 D-8 | CUMPLIDO (5 formatos + pandoc RStudio) |
| OE4 | Solution | D-9 | CUMPLIDO (Solution = dibujo 100/100; concordancia y E7 corregidos) |
| OE5 | Documentación ICFES literal | D-10 | CUMPLIDO (6/6 literal, triangulado con la ficha) |
| OE6 | Diagnosticidad + diversidad | D-11 D-12 | CUMPLIDO (§P7 −0,4 pp; 87 únicas) |
| OE7 | Texto + accesibilidad | D-13 D-14 | CUMPLIDO (ortografía, glifos, 7 contextos, alt) |
| OE8 | Fidelidad al ítem oficial | D-2 + Hermes H-2/H-3 | CUMPLIDO (canónica = impreso, lámina revisada) |
| OE9 | Retomable con una frase | este archivo + memoria | CUMPLIDO (HANDOFF + memoria) |

## Decisiones firmadas (no se tocan sin el profesor, H-5)
- Flujo B en TikZ con 94 % de similitud (usuario, 2026-10-02).
- Opción B de la instancia canónica con el 4,5 literal del impreso (total 9,5).
- La trampa del impreso se reproduce tal cual (H-2).

## Hecho en la última sesión (2026-10-04)
- **Multicopia:** `fig_id` por versión en las 6 figuras (Error 39, regla #4 v6.1); el Semillero de 10 preguntas pasó de 6 a 60 imágenes distintas.
- **Encabezados de Solution** sin número y con `id` único por versión (`{.unnumbered #slug-<fig_id>}`); `\setcounter{secnumdepth}{0}` en 85 `solpcielo.tex` del repo.
- **Verificador propio** `verificar_dibujo_clave.R` (lee el TikZ emitido) + suite 37 `tests/testthat/test_barras_campeonato_clave.R` con 5 mutantes. Antes, una clave en la letra equivocada pasaba 100/100 por la FASE 2G.
- **Detractores (3):** 0 defectos de corrección. Aplicado: concordancia de género en ajedrez (10/11 versiones decían «los ganados»), frase de «Análisis del error» que contradecía a E7, frase del borde superior calculada por versión, `stopifnot` de clave tras la mezcla, `test_that` de la regla #1, `fig_id` al final de `data_generation`, `id` renombrados para el gate de ortografía, sin `≠` en el nombre de un test.
- Se quitó el YAML `output:` (decisión del usuario).
- Commits: ver `git log -- .` (Fase 6).

## Siguiente paso concreto
1. Que el profesor revise la Solution en `salida/…_1.pdf` (Semillero de 10 preguntas) y responda la PREGUNTA_AL_RETOMAR.
2. Si aprueba: `workflow-state.sh complete <dir> aprobacion_usuario --ciclo_2026_10_04 "..."`. Si pide E8: abrir un ciclo §P7 nuevo (batería congelada, §P7-C).
3. Evidencia de aula (Nivel 3) antes de `/promover-ejercicio`.

## Bloqueado / pendiente de decisión
- Aprobación del profesor de los cambios del 2026-10-04 (no sellada a propósito).
- Propuesta E8 «borde superior leído como dato» (detractor): amplía la batería §P7 congelada → decisión humana.
- Evidencia de aula (Nivel 3) antes de promover.
- Declarado (no defecto): el `alt` de las opciones solo nombra el tipo de gráfica; es un compromiso accesibilidad ↔ fuga §P6 (D-14).
- **Deuda de infraestructura del repo (fuera de este subproyecto):**
  - El hook `post-exams2-validation.sh` no se dispara con `exams2*(rep(...))`, `file.path()` ni variables.
  - La FASE 2D del arsenal corta los chunks en la primera comilla invertida: un comentario con backticks expone código como prosa.
  - Los demás ejercicios con opciones gráficas sin `fig_id` tienen el Error 39 en exámenes de varias preguntas.
  - Ficha Q15 de Todo-Pajaro: «¿Qué evalúa?» dice «8 ganados, 3 perdidos» (es 4).

## Cómo verificar que todo sigue sano
```bash
cd /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams
F=A-Produccion/01-En-PreDesarrollo/barras-campeonato-baloncesto-n3/barras_campeonato_baloncesto_aleatorio_interpretacion_representacion_n3_schoice_v1.Rmd
Rscript .claude/scripts/validar_coherencia_matematica.R $F
Rscript .claude/scripts/validar_multisemilla.R $F
Rscript .claude/scripts/validar_diversidad_sustantiva.R $F
```

## Línea base de la batería (Fase 2, 2026-10-04, sobre el árbol de trabajo con `fig_id` y `{-}` sin commitear y el YAML `output:` todavía presente)

Artefactos: `scratchpad/base/` de la sesión (A.log hook, B.log validadores, C.log renders).

| Check | Antes (2026-10-04) | Después |
|---|---|---|
| D-1 clave única | ⚠️ **SIN COBERTURA EXTERNA.** `validar_multisemilla.R` 100/100, pero con mutantes: (a) `sol` en la opción equivocada → **0/100 detectado** por 2G y por `validar_coherencia_matematica.R`; (b) distractor = matriz de la clave → 25/100 (solo un `stopifnot` interno). La clave la protegen únicamente los `stopifnot` del propio `.Rmd` || ✅ `verificar_dibujo_clave.R` 100/100 + 5/5 mutantes detectados 100/100; `stopifnot` y `test_that` internos |
| D-2 canónica | ✅ enumeración exacta (1 instancia, forzada): valores, formatos, orden A-D, 4,5 literal, colores, leyendas y títulos iguales a `graficador/originales/` (lámina revisada) || ✅ sin cambios de datos (A/B 100/100 idénticos) |
| D-3 distractor ↔ Solution | ✅ `validar_coherencia_matematica.R` APROBADO (0 errores); P3: la coherencia la garantiza `clasificar()` interno, sin verificador externo || ✅ verificador: párrafos de distractor, casillas distintas y totales = dibujo, 100/100 |
| D-4 barras = datos | ⚠️ PARCIAL: revisión visual N = 4 (canónica + 3 versiones, semilla 12) + FASE 2H 10 semillas PASA. Familia B declarada; falta verificador automático || ✅ verificador automático N = 100 (lee el TikZ) + Semillero x10 revisado |
| D-5 varias copias | ✅ PDF ×3: 18/18 distintas · DOCX ×3: 18/18 · NOPS ×3: 16 (15 + logo) · pandoc RStudio 3.10: PDF 18/18, NOPS ×2: 11 (10 + logo) || ✅ Semillero x10: PDF 60/120, NOPS 50/100, DOCX 60/60 |
| D-6 numeración / width | ✅ 0 «0.0.» en 6 PDF; FASE 2I OK. WARN: `\label` duplicadas en LaTeX con varias copias (invisible) || ✅ 0 «0.0.», 0 «multiply defined», 0 restos de markup |
| D-7 formatos | ✅ HTML, PDF, DOCX, NOPS, Moodle; pandoc 3.10.2 (sistema) y 3.10 (RStudio, forzado con `find_pandoc(dir=)`) || ✅ 5 formatos, archivo final (md5 `0aec92ad`) |
| D-8 Moodle | ✅ 0 violaciones en 30 nombres (n = 5) || ✅ 0 violaciones |
| D-9 Solution | ✅ FASE 2J OK; figura de la Solution = clave en 4/4 versiones revisadas || ✅ verificador + concordancia + frase E7 corregida |
| D-10 ICFES literal | ✅ 6/6 campos idénticos (ASCII) al catálogo oficial y a la ficha Q15 de Todo-Pajaro; Nivel 3 ↔ D3.1; DOK 2 || ✅ sin cambios |
| D-11 §P7 | ✅ PASS, exceso −0,4 pp (techo nulo 31,4 %, N = 100, 22 reglas, 6 familias + relacionales). Canónica: `posicion_c`, `medoide_celdas` y `moda_por_celda` resuelven el ítem OFICIAL (vara §P7-E); en las generadas 30,8 % → por debajo de la vara, §P7-A. No es override || ✅ PASS −0,4 pp (idéntico) |
| D-12 diversidad | ✅ PASS, 87 claves únicas en 100 || ✅ PASS, 87 únicas |
| D-13 texto | ✅ ortografía limpia, 0 glifos, 7 plantillas de 7 tipos, sin «registró» || ✅ ortografía y glifos limpios |
| D-14 alt | ✅ opciones: solo el tipo; enunciado y Solution: con datos || ✅ sin cambios (compromiso declarado) |
| Arsenal (hook) | 0 errores, 5 WARN de la FASE 2F (heurísticas de estilo ICFES: «Tarea», «tabla de metadatos», «Es posible que los estudiantes…»); preexistentes || 0 errores, 5 WARN 2F (los mismos) |

**Hallazgo de P3 sobre el hook:** `post-exams2-validation.sh` solo se activa si el comando contiene `exams2xxx("archivo.Rmd"` literal; con `rep(...)`, `file.path(...)` o una variable sale con `exit 0` sin validar. Hay que invocarlo a mano.

**Detalle externo (Todo-Pajaro, no se toca aquí):** la ficha Q15 2026-1, campo «¿Qué evalúa?», dice «8 ganados, 3 perdidos» para séptimo; la figura y la clave dicen 4.
