# MEGA-PROMPT — Endurecimiento integral: barras-campeonato-baloncesto-n3 · versión CLOZE

> Ciclo de cierre de la versión CLOZE (6 partes) de un ejercicio ICFES R/exams. Uso: pegar este
> archivo al inicio de una sesión nueva, o decir «ejecuta mega-prompt-barras-campeonato-baloncesto-cloze.md».
> Adaptado (2026-10-04) del mega-prompt del SCHOICE hermano (`../mega-prompt-barras-campeonato-baloncesto.md`),
> que sigue vigente para el SCHOICE.

## 0. Variables

```yaml
EJERCICIO:        barras-campeonato-baloncesto-n3 (CLOZE)
RAIZ:             /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/barras-campeonato-baloncesto-n3/cloze
RAIZ_SCHOICE:     ..                          # SCHOICE hermano: SOLO LECTURA en este ciclo
REPO_GIT:         /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams   # repo con MUCHOS subproyectos
RAMA:             feat/plantillas-oficiales-rexams
RMD:              barras_campeonato_baloncesto_aleatorio_interpretacion_representacion_n3_cloze_v1.Rmd
RMD_SCHOICE:      ../barras_campeonato_baloncesto_aleatorio_interpretacion_representacion_n3_schoice_v1.Rmd
PARTES:           schoice|num|num|schoice|mchoice|schoice   # P1 ítem oficial · P2 total · P3 segmento E8 · P4 error E1-E6 · P5 5 V/F · P6 V/F
ITEM_FUENTE:      MAT-2026-1-015, cuadernillo Matemáticas 2026-1, pp. 16-17, clave oficial C (= «Gráfica III» en la canónica)
JPG_ORIGINALES:   ../graficador/originales/   # inmutable
VERIFICADOR:      verificar_dibujo_clave_cloze.R   # N = 100; lee el lector TikZ de ../verificar_dibujo_clave.R
PLANTILLA_PDF:    solpcielo_cloze.tex
ESTADO_WORKFLOW:  ejercicio_state.json        # 10/11 (falta aprobacion_usuario)
ARCHIVO_RETORNO:  HANDOFF.md
FRASE_DISPARADORA: "Continúa con el CLOZE de barras-campeonato-baloncesto-n3"
MEMORIA_SLUG:     project_objetivos_barras_campeonato_baloncesto_n3
IDIOMA:           español con tildes (regla #7), sin excepciones
```

## 1. Rol y Gran Objetivo

Eres el responsable de calidad de la versión CLOZE de este ejercicio ICFES y trabajas con el
rigor de un evaluador psicométrico. El **Gran Objetivo** es que el `.Rmd` CLOZE que llegue al
aula tenga **cero defectos** en las **seis partes** (matemática, visualización, documentación
ICFES, Solution, ortografía, render), en **todas** sus versiones y en **todos** los formatos que
admiten cloze. Además: conservar la fidelidad al ítem oficial en la Parte 1, no tocar el
SCHOICE hermano y dejar el subproyecto retomable con una sola frase.

«Cero defectos» significa que **la batería D-1…D-17 de la §4 da verde con salida real pegada**.
Un defecto que la batería no detecta se convierte en un check nuevo y en un test de regresión,
y no se cierra hasta que el check falla con el defecto y pasa sin él.

## 2. Contrato innegociable

1. **Fases en orden estricto**, cada una con su puerta. Si una puerta no pasa, se corrige; no
   basta con documentarlo.
2. **Solo cuenta lo verificado en el artefacto** (PDF/HTML/XML renderizado, salida del
   validador, test que pasa). Un flag en `true` o un subagente que dice «listo» NO es
   evidencia. El `ejercicio_state.json` se audita por artefacto.
3. **Las reglas del repo están por encima de este prompt** (`.claude/CLAUDE.md`,
   `.claude/rules/*.md`; ante un caso no trivial, su íntegro en `.claude/docs/reglas/`).
4. **Decisiones humanas firmadas que NO se «corrigen»** (H-5: relajar nunca sin el profesor):
   - Heredadas del SCHOICE: Flujo B en TikZ (94 %); opción B canónica con el **4,5 LITERAL**;
     trampa del impreso reproducida (H-2); rama E8 con P = 1/3 y {clave, E8 apilada, E2, E6};
     rótulo «(barras apiladas/agrupadas)» fuera de la canónica.
   - Propias del CLOZE (2026-10-04): **Parte 1 = generación del SCHOICE** (misma semilla ⇒
     mismos datos, opciones y orden); **override `exshuffle: FALSE`** con mezcla interna
     (`orden`, `perm4`, `perm5`) y su `ERR_C4` **declarado, no silenciado**; rótulo
     «Gráfica I-IV» **dibujado dentro** de cada figura; Parte 5 con **una afirmación por par**;
     plantilla `solpcielo_cloze.tex` con la instrucción «Cómo marcar»; la evidencia de Nivel 3
     de aula se toma del SCHOICE.
   Si crees que alguna está mal, se lo propones al usuario y no la tocas.
5. **Zonas inmutables:** `A-Produccion/03-En-Produccion/`, `Ejemplos-Funcionales-Rmd/`,
   `../graficador/originales/`, **todo `RAIZ_SCHOICE` fuera de `cloze/`** (incluido
   `../verificar_dibujo_clave.R`, del que el VERIFICADOR toma el lector TikZ). Cambios en
   `.claude/` exigen backup e invariantes (regla #17).
6. **Prohibido:** `git commit --no-verify`, relajar asserts o umbrales, mockear para poner en
   verde, bajar el timeout del hook (300 s), N ≠ 100 en mediciones (regla #23).
7. **§P7-D del CLOZE:** van **2 de 3 pasadas** consumidas. Queda **una**. Después se cierra con
   residuo declarado. Tras CADA corrección se reverifican las 6 claves en el 100 % de las semillas.
8. Si algo queda bloqueado, se completa todo lo demás y se declara qué quedó fuera y por qué.

## 3. Fase 0 — Reanudación (SIEMPRE primero)

1. Lee `ARCHIVO_RETORNO`, `ESTADO_WORKFLOW`, `../HANDOFF.md` (decisiones del SCHOICE), el
   `.claude/CLAUDE.md` del repo y, si existen, `RAIZ/.claude/CLAUDE.md` o `../.claude/CLAUDE.md`.
2. `git -C REPO_GIT status --short -- RAIZ ..` y `git -C REPO_GIT log --oneline -8 -- ..`.
   Ojo: `cloze/` puede estar **sin versionar entero**. Si el estado escrito y el disco no
   coinciden, gana el disco: compara mtimes y md5 (`HANDOFF` declara `17df2bfc`).
3. Pregunta al usuario solo por cambios que no expliquen el HANDOFF ni el estado.
4. Arranque de ≤ 10 líneas: estado, siguiente paso, bloqueos. **No hay pregunta bloqueante**:
   el alcance está fijado (endurecer el CLOZE, sin tocar el SCHOICE). La aprobación del
   profesor (paso 11) queda para el final y no la sella el agente.

## 4. Batería de defectos (fuente única de «cero errores»)

Un check sin comando se marca `SIN COBERTURA` y no cuenta como verde. NOPS no admite cloze:
donde la versión SCHOICE pedía NOPS, aquí la evidencia es el **rechazo** de `exams2nops`.

| ID | Dominio | Qué garantiza | Cómo se verifica |
|---|---|---|---|
| D-1 | Matemática | Las 6 claves son verdaderas desde lo que el estudiante VE; P1 y P4 sin segunda clave; P6 marca el valor de verdad real | `Rscript VERIFICADOR` (N=100, 100 %) + `validar_multisemilla.R` (N=100) + enumeración exhaustiva de P4 (6 errores sobre P con 4 valores distintos ⇒ 6 matrices distintas) |
| D-2 | Matemática | Canónica: P1 = impreso (datos, formatos, orden, clave «Gráfica III», B = 4,5 literal) y P1 ≡ SCHOICE con la misma semilla | Forzar la rama canónica y cotejar celda a celda con `../graficador/originales/`; comparar `mats_op/fmt_op/sol` CLOZE vs SCHOICE en 100 semillas |
| D-3 | Matemática | Cada distractor de P1 y cada opción de P4 = el error del pool que dice la Solution (`calcula()` pura, `precondicion`) | `validar_coherencia_matematica.R` Niveles 4-5 + bloque Solution del VERIFICADOR |
| D-4 | Visual | Barras = matriz; rótulo «Gráfica I-IV» dentro de la figura y fiel al orden; subtítulo del tipo fuera de la canónica | Render real + Read de los PNG (Familia B: declarar N y razón) |
| D-5 | Visual | Varias copias: cada pregunta muestra SUS figuras | `exams2pdf(rep(RMD,3), template=PLANTILLA_PDF)` + `pdfimages -list`: 6 por copia, 15 distintas (la de Solution repite la clave); ídem HTML/DOCX por nombre de archivo |
| D-6 | Visual | Sin «0.0.N»; `{width=}` en toda imagen; sin pie «Figure N»; ids de encabezado únicos por copia | `pdftotext \| grep '0\.0\.'` vacío; hook FASE 2I; grep de `id=` duplicados en HTML×3 |
| D-7 | Render | HTML, PDF, DOCX y Moodle compilan, también con el pandoc de RStudio; NOPS rechazado de forma explícita | `exams2html/pdf/pandoc(docx)/moodle` + `RSTUDIO_PANDOC=...` (regla #20); `exams2nops` → error capturado |
| D-8 | Moodle | Sin fuga por nombre de archivo; **marca = verdad en los 6 gaps** (F4); ningún gap con markup/imagen; ningún `_S` que re-mezcle | `exams2moodle` + grep regla #4 v6.1 (vacío) + parser del XML que recalcule cada gap |
| D-9 | Solution | #19 (sin letra), no enumera en orden, figura de la clave = texto, una sección «Respuesta correcta» por parte, 6 secciones metacognitivas | Hook FASE 2J + bloque Solution del VERIFICADOR + lectura del PDF en una versión apilada y una agrupada |
| D-10 | ICFES | `exextra` literal del catálogo oficial; `Type: CLOZE`; idénticos al SCHOICE salvo Type/Fuente; `DOK ≥ 3 ⇒ Nivel ≥ 3` | Diff de `exextra` CLOZE vs SCHOICE + cotejo con el catálogo canónico de Matemáticas |
| D-11 | Diagnosticidad | P1: §P7 con la batería **congelada** del SCHOICE, exceso ≤ +5,3 pp. P5: mejor marcador léxico ≤ techo nulo por par. P4/P6: posición y balance sin canal | `../verificar_bateria_p7.R` adaptado sin ampliar la batería (§P7-C) + `validar_diagnosticidad.R` (N=100); `WARN_DIAG_INDET` ≠ PASS |
| D-12 | Diversidad | P1 y P5 varían de forma sustantiva; P2-P4/P6 declaradas (claves categóricas o enteros pequeños) | `validar_diversidad_sustantiva.R` (N=100): sin `ERR_DIV_COSMETICA`; `WARN_DIV_BAJA` declarado con cifras |
| D-13 | Texto | Tildes, ningún glifo que rompa pdflatex, narrativa #11, concordancia partidos/partidas en las 6 partes | `corregir_ortografia_espanol.R` + `validar_glifos_latex.R` + barrido de semillas con ajedrez |
| D-14 | Accesibilidad | El alt de las opciones nombra solo rótulo y tipo; el del enunciado y la Solution lleva los datos | grep de `alt_op`/`alt_enun` + HTML renderizado |
| D-15 | Estructura CLOZE | 6 `##ANSWERi##` en orden, uno por tipo; `exclozetype`, `exsolution`, `extol` con 6 campos; Answerlist 15 (enunciado) / 17 (Solution); ≥ 6 partes con Progressive Disclosure | Arsenal (hook) + conteo sobre el `.md` tejido en 100 semillas |
| D-16 | Fugas entre gaps | Los 6 gaps se ven a la vez: ningún texto visible de P2-P6 informa sobre la clave de P1 | Lectura de dependencias del chunk (P, a3, b3, k4, perm5, j6 independientes de `M`/`orden`) + medición N=100: ningún token visible de P3-P6 predice el rótulo de la clave por encima del techo nulo |
| D-17 | Scripts del profesor | Los Semilleros de `cloze/` renderizan **el CLOZE** (no el SCHOICE ni otro ejercicio), sin NOPS y con `PLANTILLA_PDF` | `grep archivo_examen` + ejecución real de 10 preguntas (PDF, DOCX, Moodle, HTML) |

## 5. Fase 1 — Objetivos (`/goal`)

Ejecuta `/goal`. Fija el objetivo general y los OE, y asigna a cada OE uno o varios D-n.
Persiste en `ARCHIVO_RETORNO` (sección «Objetivos») y en `MEMORIA_SLUG` + `MEMORY.md`, con
fechas absolutas. **Puerta G1:** todo OE apunta a checks D-n con comando.

## 6. Fase 2 — Rigor máximo (`/ultra`) y línea base

Ejecuta `/ultra`. **ANTES de tocar nada**, corre D-1…D-17 y guarda la línea base fechada en
`ARCHIVO_RETORNO` (verde / rojo / sin cobertura, con cifra). Los renders van al scratchpad.
**Puerta G2:** la línea base está escrita.

## 7. Fase 3 — Auditoría adversarial independiente

1. **FASE 2C (tercer ciclo):** `AgenteDetractor` (opus), **sin `name:`**, distinto de quien
   escribió o corrigió el `.Rmd` y de los dos detractores previos. Alcance: 8 dominios + los
   overrides firmados del §2.4 + D-15/D-16 (lo específico del CLOZE). Válido solo si termina en
   `VEREDICTO_DETRACTOR:`. Si no llega: Paso 0 (transcripción), 2 reintentos, escalar.
2. **FASE 2C-bis:** `/detractor-hetero RMD` (pendiente desde el ciclo anterior). Complementa;
   no sustituye ni sella `detractor_fase2c`.
3. Cada hallazgo se **verifica antes de aplicarlo** (el detractor puede inventar código o
   errar la aritmética del arreglo). Los descartados se justifican en una línea.
4. Un hallazgo de CORRECCIÓN (clave falsa, segunda clave, Solution falsa) = `RECHAZAR`: se
   corrige y se vuelve a la Fase 2.
**Puerta G3:** 0 CRÍTICAS/ALTAS abiertas y la batería igual o mejor que la línea base.

## 8. Fase 4 — Testing agresivo (intentar romper el ejercicio)

1. **Barrido de semillas:** N=100 en todos los validadores de la Familia A. Enumerar el espacio
   finito de P4 (error × formato) y de P6 (j6 × candidato falso) buscando segundas claves y
   colisiones textuales.
2. **Casos borde:** rama canónica; rama E8; ajedrez (femenino); `cand_f6` con un solo
   candidato; sumas de columna de P iguales a un dato (debe ser imposible); `a3`/`b3` en los
   extremos.
3. **Render real como lo usa el profesor:** los Semilleros de `cloze/` (tras D-17) con
   10 preguntas: PDF con `PLANTILLA_PDF`, DOCX, Moodle, HTML. Se miran las páginas.
4. **Verificar al verificador:** inyectar (a) `sol_p1` en la gráfica equivocada, (b) un
   distractor de P1 igual a la clave, (c) quitar el `fig_id` de una figura, (d) `resp_p2`
   desplazado en 1, (e) una afirmación de P5 con el valor de verdad invertido en el `.Rmd`.
   La batería debe detectar las cinco; si alguna pasa, se crea el test que la detecte.
5. **Regresión:** cada defecto nuevo genera un test que falla antes del arreglo y pasa después.
6. Cierre: `R_TESTS_FULL=1 Rscript tests/run_all_tests.R` en segundo plano (~13 min). Leer
   el **conteo de suites**.
**Puerta G4:** D-1…D-17 verdes con su salida, 5/5 inyecciones detectadas, suite completa verde.

## 9. Fase 5 — Corregir, optimizar, documentar

| # | Frente | Verificación |
|---|---|---|
| 1 | Código del `.Rmd`: familias F1-F6 (regla #21); la Parte 1 sigue siendo copia byte a byte de la generación del SCHOICE | Batería verde tras cada cambio + D-2 |
| 2 | Solution: 6 respuestas por parte + 6 secciones de la regla #1, coherentes con cada rama | Leer 1 PDF por rama (apilada/agrupada, E8, canónica) |
| 3 | `ejercicio_state.json`: nada sellado sin artefacto; resellar con `workflow-state.sh` (nunca a mano) | `workflow-state.sh status` + auditoría por artefacto |
| 4 | Semilleros de `cloze/` apuntando al CLOZE (D-17) | Ejecución real |
| 5 | `HANDOFF.md` con la plantilla de la §11 | Retomar cuesta una frase |
| 6 | `salida/` regenerada con el `.Rmd` final (está en `.gitignore`) | mtime posterior al último cambio del `.Rmd` |
| 7 | Patrones nuevos → `.claude/docs/patrones-errores-conocidos.md` (solo lo 100 % verificado) | Entrada con mensaje exacto, antes/después y tabla por formato |

**Puerta G5:** los 7 frentes verificados.

## 10. Subagentes y economía

- Routing global: búsqueda y validación mecánica → Haiku; implementación, tests y docs →
  Sonnet; detractor → Opus. Indicar `🧠 [Tier]` en cada lanzamiento. Sin `name:`.
- Los reportes de subagentes no son evidencia: se verifica el artefacto.
- Más de 20 edits semánticos en un archivo → script Python con diccionario y backup.
- Renders sin `rm -rf` encadenado; lo temporal al scratchpad.

## 11. Fase 6 — Commit, push y retorno

1. **Pathspec explícito**, nunca `git add -A`/`.`. El árbol tiene cambios ajenos
   (teorema-coseno, `tabla_datos.png`, `.nodeterm/`, archivos sin versionar del SCHOICE en `..`).
   Primero un commit de `cloze/` (sin `salida/`, sin `stress_test_output/`, sin plantillas ni
   scripts ajenos al CLOZE); la infraestructura (`SOURCES/`, `tests/`, `.claude/`) en commits
   separados.
2. `git diff --cached --stat` antes de cada commit. Estilo `tipo(barras-campeonato-baloncesto-n3): descripción`.
3. Push a `RAMA`. Si falla, se reporta textualmente.
4. Reescribe `HANDOFF.md`: disparador, PREGUNTA_AL_RETOMAR, estado en una línea, OE con su
   estado, hecho con SHAs, siguiente paso exacto, bloqueos, tabla de la batería antes/después.
   Actualiza `MEMORIA_SLUG` y `MEMORY.md`.
5. **No promover** ni sellar `aprobacion_usuario`: eso es del profesor, y la promoción exige
   evidencia de aula.

## 12. Definition of Done

- [ ] G1 OE ↔ D-n · [ ] G2 línea base · [ ] G3 detractor (3.er ciclo) + hetero sin CRÍTICAS/ALTAS
- [ ] G4 D-1…D-17 verdes + 5 inyecciones detectadas + suite completa verde
- [ ] G5 7 frentes · [ ] G6 commits con pathspec + push · [ ] G7 HANDOFF + memoria

## 13. Anti-patrones (rechazo automático)

❌ «Compila» = correcto · ❌ validar solo con n=1 · ❌ `grep` con 0 coincidencias tomado como
«sin defecto» sin probar que el patrón ve el caso · ❌ «corregir» la trampa, el 4,5 o el
override `exshuffle: FALSE` · ❌ silenciar el `ERR_C4` · ❌ modificar el SCHOICE hermano ·
❌ sellar `detractor_fase2c` con revisión propia · ❌ una 4.ª pasada de §P7 ·
❌ lenguaje minimizador · ❌ declarar listo sin la salida del comando.

## 14. Reporte final

```
✅ CICLO COMPLETO — barras-campeonato-baloncesto-n3 · CLOZE
Batería:      D-1…D-17  antes <v/r/sc>  →  después <v/r/sc>
Detractor:    <veredicto> · hetero <veredicto> · hallazgos <n> (<n> aplicados, <n> descartados con motivo)
Inyecciones:  <n>/5 detectadas · tests de regresión nuevos: <n>
Suite:        <n>/<n> suites · Semillero 10 preguntas revisado: <sí/no>
Commits:      <shas> · Push: <ok/fallo textual>
Fuera:        <qué y por qué>
Retomar:      "Continúa con el CLOZE de barras-campeonato-baloncesto-n3"
```
