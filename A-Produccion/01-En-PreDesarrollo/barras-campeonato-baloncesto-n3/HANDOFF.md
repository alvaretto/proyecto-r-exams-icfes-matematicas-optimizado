# HANDOFF — barras-campeonato-baloncesto-n3 (última actualización: 2026-10-05)

**Disparador:** «Continúa con barras-campeonato-baloncesto-n3»
**Ciclo vigente:** `mega-prompt-barras-campeonato-baloncesto.md` (alcance: solo SCHOICE, decisión del usuario 2026-10-04).

## PREGUNTA_AL_RETOMAR
«¿Apruebas los cambios del 2026-10-04: la Solution (concordancia, frase del análisis del error, encabezados), el distractor E8 «borde superior» y el rótulo «(barras apiladas/agrupadas)» en las opciones no canónicas?»

## Estado en una línea
SCHOICE con E8 y rótulo del tipo, D-1…D-14 en verde (2026-10-04, `.Rmd` md5 `0c9cedab`); la aprobación del profesor del 2026-10-03 NO cubre los cambios de hoy.

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
- **E8 incorporado** (usuario, 2026-10-04): rama con P_E8 = 1/3 de las versiones no canónicas; opciones = clave, E8(M) apilada, E2(M), E6(M). Topología ÚNICA viable tras enumerar los 1 176 tríos (§P7-F); ver «Rama E8».
- **Rótulo del tipo de gráfica** bajo el título de cada opción no canónica (usuario, 2026-10-04, alternativa A del detractor): sin él, E8 es idéntica píxel a píxel a barras SUPERPUESTAS de los datos correctos (Excel, superposición 100 %) y sería clave bajo esa convención en ~29 % de las versiones. La canónica no lo lleva.
- Flujo B en TikZ con 94 % de similitud (usuario, 2026-10-02).
- Opción B de la instancia canónica con el 4,5 literal del impreso (total 9,5).
- La trampa del impreso se reproduce tal cual (H-2).

## Rama E8 (2026-10-04)
- **E8 «Borde superior leído como dato»:** en la apilada (grado de la gráfica abajo) el segmento superior mide M[1,j] − M[2,j], así que su borde llega al dato de la tabla. Exige tabla > gráfica en las dos categorías; la rama genera M con esa condición.
- **Topologías descartadas, medidas** (§P7 por rama, batería congelada, 100 por rama): E8 + cadenas desde la clave → «elige del par gemelo» 51 % (+19,7 pp); E8 + centro oculto → +19,6 pp; E8 + E1 y E1→E2/E3 → la clave era la única con el grado de la tabla dominante (+50,3 pp).
- **Topología elegida** {clave, E8, E2, E6}: rama E8 +1,6 pp, resto no canónico +1,6 pp, agregado N = 100 +1,4 pp (PASS). §P7-D: 2 pasadas de 3 consumidas.
- **Vara §P7-E:** la canónica (ítem oficial) sigue resuelta por medoide/moda/posición; una subpoblación «sin E8» que la incluya marca +13,4 pp (detractor heterogéneo, n = 432) — es la canónica, no un canal: «no canónica» da +2,8 pp en esa misma corrida.
- **Declarado (detractor, BAJA):** en la rama E8 revisar una sola casilla de la fila de la tabla acierta el 100 % (el impreso también lo permite); en la rama sin E8, revisar solo la fila de la gráfica acierta el 85 % (el impreso: 100 %). La batería §P7 no mira el enunciado.

## Hecho en la última sesión (2026-10-04)
- **Multicopia:** `fig_id` por versión en las 6 figuras (Error 39, regla #4 v6.1); el Semillero de 10 preguntas pasó de 6 a 60 imágenes distintas.
- **Encabezados de Solution** sin número y con `id` único por versión (`{.unnumbered #slug-<fig_id>}`); `\setcounter{secnumdepth}{0}` en 85 `solpcielo.tex` del repo.
- **Verificador propio** `verificar_dibujo_clave.R` (lee el TikZ emitido) + suite 37 `tests/testthat/test_barras_campeonato_clave.R` con 5 mutantes. Antes, una clave en la letra equivocada pasaba 100/100 por la FASE 2G.
- **Detractores (3):** 0 defectos de corrección. Aplicado: concordancia de género en ajedrez (10/11 versiones decían «los ganados»), frase de «Análisis del error» que contradecía a E7, frase del borde superior calculada por versión, `stopifnot` de clave tras la mezcla, `test_that` de la regla #1, `fig_id` al final de `data_generation`, `id` renombrados para el gate de ortografía, sin `≠` en el nombre de un test.
- Se quitó el YAML `output:` (decisión del usuario).
- **E8 + rótulo del tipo** (decisiones del usuario): ver «Rama E8». El verificador comprueba también el rótulo (presente y fiel al dibujo fuera de la canónica, ausente en ella); la suite 37 suma 2 mutantes de rótulo (sin rótulo, invertido): 7 mutantes en total.
- Commits: ver `git log -- .` (Fase 6).

## Cotejo de la clave con la ficha de origen (2026-10-05)
- **La clave coincide** en las tres fuentes: plana del cuadernillo (`Originales/pagina_016.jpg` y `pagina_017.jpg`), ficha `MAT-2026-1-015` de Todo-Pajaro e instancia canónica del `.Rmd` **ejecutada** (C · sexto 5/7 · séptimo 8/4 · agrupadas). A, B y D de la canónica también reproducen la plana. N = 100: 100/100 con exactamente una opción igual a M, y es la marcada.
- **La ficha se corrigió** (Todo-Pajaro `969815be9`, `593c1a7e5`, `d7047461a`, `2737c0638`, `ab2a0f9f2`; bitácora `Matematicas/BACKLOG.md` §A143): «¿Qué evalúa?» decía «8 ganados, 3 perdidos» (es 4); la JustMeta de B, «4 y 5 perdidos» (es 4,5 de sexto y 5 de séptimo); y cinco frases presentaban el formato agrupado como criterio de la clave o el apilado como error de A. Ahora coinciden con el diseño de este `.Rmd`: la clave la hacen sus cuatro valores, y una apilada con esos mismos valores también «contiene la información» (por eso el `.Rmd` exige matriz distinta a los distractores y sortea el formato de la clave 50/50).
- **Guardia nueva** en la suite 37 (`tests/testthat/test_barras_campeonato_clave.R`): la instancia canónica debe coincidir con la ficha en letra y cuatro valores, y «¿Qué evalúa?» con «Clave». Lee la ficha de `../Todo-Pajaro` (o `TODO_PAJARO_DIR`); sin ella, se omite. Probada con dos fichas mutantes (la cifra 3 y la clave B): las dos la hacen fallar.

## Siguiente paso concreto
1. Que el profesor revise la Solution en `salida/…_1.pdf` (Semillero de 10 preguntas) y responda la PREGUNTA_AL_RETOMAR.
2. Si aprueba: `workflow-state.sh complete <dir> aprobacion_usuario --ciclo_2026_10_04 "..."`. §P7-D: queda 1 pasada de corrección de diagnosticidad.
3. Evidencia de aula (Nivel 3) antes de `/promover-ejercicio`.

## Bloqueado / pendiente de decisión
- Aprobación del profesor de los cambios del 2026-10-04 (Solution, E8, rótulo), no sellada a propósito.
- Evidencia de aula (Nivel 3) antes de promover.
- Declarado (no defecto): el `alt` de las opciones solo nombra el tipo de gráfica; es un compromiso accesibilidad ↔ fuga §P6 (D-14).
- **Deuda de infraestructura del repo (fuera de este subproyecto):**
  - El hook `post-exams2-validation.sh` no se dispara con `exams2*(rep(...))`, `file.path()` ni variables.
  - La FASE 2D del arsenal corta los chunks en la primera comilla invertida: un comentario con backticks expone código como prosa.
  - Los demás ejercicios con opciones gráficas sin `fig_id` tienen el Error 39 en exámenes de varias preguntas.
  - ~~Ficha Q15 de Todo-Pajaro: «¿Qué evalúa?» dice «8 ganados, 3 perdidos» (es 4).~~ Corregida el 2026-10-05 (ver «Cotejo de la clave con la ficha de origen»).

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
| D-1 clave única | ⚠️ **SIN COBERTURA EXTERNA.** `validar_multisemilla.R` 100/100, pero con mutantes: (a) `sol` en la opción equivocada → **0/100 detectado** por 2G y por `validar_coherencia_matematica.R`; (b) distractor = matriz de la clave → 25/100 (solo un `stopifnot` interno). La clave la protegen únicamente los `stopifnot` del propio `.Rmd` || ✅ `verificar_dibujo_clave.R` 106/106 (e7 = 21, rama E8 = 25) + 7/7 mutantes detectados; `stopifnot` y `test_that` internos |
| D-2 canónica | ✅ enumeración exacta (1 instancia, forzada): valores, formatos, orden A-D, 4,5 literal, colores, leyendas y títulos iguales a `graficador/originales/` (lámina revisada) || ✅ sin cambios de datos (A/B 100/100 idénticos) |
| D-3 distractor ↔ Solution | ✅ `validar_coherencia_matematica.R` APROBADO (0 errores); P3: la coherencia la garantiza `clasificar()` interno, sin verificador externo || ✅ verificador: párrafos de distractor, casillas distintas y totales = dibujo, 100/100 |
| D-4 barras = datos | ⚠️ PARCIAL: revisión visual N = 4 (canónica + 3 versiones, semilla 12) + FASE 2H 10 semillas PASA. Familia B declarada; falta verificador automático || ✅ verificador automático N = 100 (lee el TikZ) + Semillero x10 revisado |
| D-5 varias copias | ✅ PDF ×3: 18/18 distintas · DOCX ×3: 18/18 · NOPS ×3: 16 (15 + logo) · pandoc RStudio 3.10: PDF 18/18, NOPS ×2: 11 (10 + logo) || ✅ Semillero x10: PDF 60/120, NOPS 50/100, DOCX 60/60 |
| D-6 numeración / width | ✅ 0 «0.0.» en 6 PDF; FASE 2I OK. WARN: `\label` duplicadas en LaTeX con varias copias (invisible) || ✅ 0 «0.0.», 0 «multiply defined», 0 restos de markup |
| D-7 formatos | ✅ HTML, PDF, DOCX, NOPS, Moodle; pandoc 3.10.2 (sistema) y 3.10 (RStudio, forzado con `find_pandoc(dir=)`) || ✅ 9/9 renders, archivo final (md5 `0c9cedab`) |
| D-8 Moodle | ✅ 0 violaciones en 30 nombres (n = 5) || ✅ 0 violaciones |
| D-9 Solution | ✅ FASE 2J OK; figura de la Solution = clave en 4/4 versiones revisadas || ✅ verificador + concordancia + frase E7 corregida |
| D-10 ICFES literal | ✅ 6/6 campos idénticos (ASCII) al catálogo oficial y a la ficha Q15 de Todo-Pajaro; Nivel 3 ↔ D3.1; DOK 2 || ✅ sin cambios |
| D-11 §P7 | ✅ PASS, exceso −0,4 pp (techo nulo 31,4 %, N = 100, 22 reglas, 6 familias + relacionales). Canónica: `posicion_c`, `medoide_celdas` y `moda_por_celda` resuelven el ítem OFICIAL (vara §P7-E); en las generadas 30,8 % → por debajo de la vara, §P7-A. No es override || ✅ PASS +1,4 pp agregado; rama E8 +1,6 pp; resto +1,6 pp (100 por rama) |
| D-12 diversidad | ✅ PASS, 87 claves únicas en 100 || ✅ PASS, 88 únicas |
| D-13 texto | ✅ ortografía limpia, 0 glifos, 7 plantillas de 7 tipos, sin «registró» || ✅ ortografía y glifos limpios |
| D-14 alt | ✅ opciones: solo el tipo; enunciado y Solution: con datos || ✅ alt de las opciones = tipo, igual que el rótulo visible |
| Arsenal (hook) | 0 errores, 5 WARN de la FASE 2F (heurísticas de estilo ICFES: «Tarea», «tabla de metadatos», «Es posible que los estudiantes…»); preexistentes || 0 errores, 5 WARN 2F (los mismos) |

**Hallazgo de P3 sobre el hook:** `post-exams2-validation.sh` solo se activa si el comando contiene `exams2xxx("archivo.Rmd"` literal; con `rep(...)`, `file.path(...)` o una variable sale con `exit 0` sin validar. Hay que invocarlo a mano.

**Detalle externo (Todo-Pajaro, no se toca aquí):** la ficha Q15 2026-1, campo «¿Qué evalúa?», dice «8 ganados, 3 perdidos» para séptimo; la figura y la clave dicen 4.
