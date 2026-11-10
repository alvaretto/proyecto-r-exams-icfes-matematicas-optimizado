# HANDOFF — CLOZE hermano de diagrama-venn-juegos-mesa-n3 (MAT-2026-07-014)

**Estado (2026-11-09): 11/11, aprobado para el aula.** En el WAIT_USER #3 de la tanda 5 el profesor respondió «a» a la pregunta que
pedía aprobar el `.Rmd` con md5 `889d828e8129e11b0ec5dceda0cf108b`, que no cambió en esta tanda. El coordinador selló
`aprobacion_usuario` con los cuatro md5 comprobados en disco; `detractor_fase2c` ya se había vuelto a sellar con la FASE 2C r4. No está
en producción: falta la evidencia de aula. Sin commit ni push.

Las decisiones firmadas están en `../.claude/CLAUDE.md` § «CLOZE hermano»: C-1 a C-5, «1 b · semillero sí» (tras la tanda 3) y
«H1 c · H2 100» (tras la tanda 4). Esta tanda no añadió ninguna; la aprobación («a») está en la misma sección.

## Archivos

| Archivo | md5 | Qué es |
|---|---|---|
| `diagrama_venn_…_n3_cloze_v1.Rmd` | `889d828e…` | El CLOZE. Sin cambios desde la tanda 3 |
| `verificar_espacio_cloze.R` | `bf11b0a4…` | 12 secciones. En la tanda 5 cambió solo `uso`: cuatro códigos y 17 mutantes. Exit 4 = rojo §P7 de la Parte 1 cubierto por la firma C-4; **no es PASS**. `RMD_MD5` = `889d828e…` |
| `solpcielo_cloze_una_por_pagina.tex` | `290a3f79…` | Decisión 1 b. Sin cambios |
| `SemilleroCloze.R` | `d01bbf0f…` | Tanda 5: `exshuffle = FALSE` solo en `exams2pdf()` y `archivos = 100` |
| `salida/verificar_r7.txt` y `.md5` | — | r7 en modo `todo` sobre esos cuatro md5: **exit 4**, 0 rojos, 0 pendientes, 12 secciones, de 17:38 a 17:54 |
| `salida/fase2c_r4_detractor.md` | ``f88b5a4c…`` | FASE 2C r4 íntegra, con su bloque `CITAS_DETRACTOR` |
| `salida/t5/` | — | Diffs frente a lo que auditó la r3, corrida aislada de `uso`, medición N = 100 del PDF y una página |
| `salida/fase2c_r3_detractor.md`, `salida/paso8_t4/`, `salida/auditoria_6b_t4/`, `salida/paso9_889d828e/`, `salida/literalidad_exextra_t4.txt` | — | Evidencia de la tanda 4, que sigue valiendo sobre `889d828e` |

## Tanda 5

1. **Semillero** (`salida/t5/semillero_r3_a_t5.diff`). `generar_pdf()` pasa `exshuffle = FALSE` a `exams2pdf()`, con un comentario
   que explica por qué; es la única llamada a `exams2pdf()` del semillero. HTML, Moodle y Canvas no lo llevan. `config$archivos`
   pasa de 250 a 100.
2. **Sección `uso`** (`salida/t5/verificador_r6_a_r7.diff`). Cuatro códigos nuevos:
   - `USO_EXSHUFFLE`: en el árbol sintáctico del semillero (sin comentarios, con o sin `exams::`), toda `exams2pdf()` lleva
     `exshuffle = FALSE` y ninguna otra `exams2*()` lleva `exshuffle`.
   - `USO_ARCHIVOS`: `config$archivos` vale 100.
   - `USO_ORDEN_PDF`: en el PDF real de `generar_pdf()`, el gap de la Parte 1 de cada pregunta es «Diagrama I / Diagrama II /
     Diagrama III / Diagrama IV».
   - `USO_CLAVE_PDF` (falla cerrado): la clave que registra `exams2pdf()` (`metainfo$solution` del gap 1, capturada con `trace()` a
     la salida de `exams2pdf`) señala el diagrama que reproduce las viñetas impresas. El camino es: el texto impreso en esa posición
     del gap, su rótulo en el LaTeX compilado, el archivo de ese rótulo y sus regiones, sacadas del entorno de la pregunta
     (capturado con `trace()` sobre `knitr::knit` y emparejado por `fig_id`). No usa `sol_p1` ni la Solution.

   Mutantes nuevos, cada uno con su guarda `materia` sobre lo capturado:

   | Mutante | Sonda esperada | Sondas que cayeron | Veredicto |
   |---|---|---|---|
   | U10 semillero sin `exshuffle = FALSE` | USO_ORDEN_PDF | USO_ORDEN_PDF y USO_EXSHUFFLE (declarada) | cazado_por_su_sonda_y_otra |
   | U11 `exshuffle = FALSE` también en Moodle | USO_EXSHUFFLE | USO_EXSHUFFLE y USO_MOODLE (declarada) | cazado_por_su_sonda_y_otra |
   | U12 `archivos` de vuelta en 250 | USO_ARCHIVOS | USO_ARCHIVOS | cazado_por_su_sonda |
   | U13 `.Rmd` con los rótulos permutados: aísla el orden | USO_ORDEN_PDF | USO_ORDEN_PDF | cazado_por_su_sonda |
   | U14 `.Rmd` con la clave desplazada y su guarda anulada | USO_CLAVE_PDF | USO_CLAVE_PDF | cazado_por_su_sonda |
   | U15 semillero sin `exshuffle` y `.Rmd` con `exshuffle: FALSE`: aísla la estructura del PDF | USO_EXSHUFFLE | USO_EXSHUFFLE | cazado_por_su_sonda |
   | U16 `exshuffle = FALSE` también en HTML: aísla «ninguna otra lo lleva» | USO_EXSHUFFLE | USO_EXSHUFFLE | cazado_por_su_sonda |
   | U17 `.Rmd` con los rótulos sin negrita: falla cerrado | USO_CLAVE_PDF | USO_CLAVE_PDF | cazado_por_su_sonda |

   U1 a U9 siguen cazados por su sonda. Las copias mutadas del `.Rmd` van a `tempdir()`, nunca al archivo real.
3. **Medido en exams 2.4.5:** `exshuffle` es argumento formal de `exams2pdf()` y de `exams2html()`, pero no de `exams2moodle()` ni de
   `exams2canvas()`. Con `exshuffle = FALSE`, `exams2moodle()` aborta («el argumento no fue usado») y `generar_moodle()` devuelve
   FALSE. Por eso U11 tumba también USO_MOODLE, y U16 aísla la componente con HTML, que lo acepta y deja de barajar sin avisar.
4. **Medición única N = 100** (`salida/t5/medir_n100_pdf.R` y `.txt`). `generar_pdf()` real con `preguntas_pdf = 100` y
   `set.seed(20261109)`: 176 s y 323 páginas de 216 × 330 mm. Usó las funciones del verificador antes de su último cambio (U17);
   el detractor r4 la repitió con el verificador final `bf11b0a4` y dio cifras idénticas.
   - Gap de la Parte 1 en el orden I-IV: 100 de 100.
   - Clave registrada por `exams2pdf()` en el diagrama correcto: 100 de 100. Posición de la clave: (a) 25, (b) 20, (c) 32, (d) 23.
   - Preguntas con las figuras partidas: 0. «Cómo marcar» en la portada.
   - Parte 6, verdaderas por posición: 47, 47, 60 y 44 %, con 14 patrones (la posición sale del `.Rmd`, que sortea el patrón y los pares).
5. **Página mirada:** `salida/t5/pagina_pregunta1_semilla1.png`, la pregunta 1 de la semilla 1 de `uso`. El gap sale «Diagrama I /
   Diagrama II / Diagrama III / Diagrama IV». La clave es el Diagrama IV, la hoja de la portada marca (d) y la Solution dice «el
   Diagrama IV». Ese diagrama reproduce las siete viñetas (solo pintura 5, pintura y música 9, solo música 6, centro 5, pintura y
   teatro 10, música y teatro 5, solo teatro 2).
6. **r7** sobre los md5 de arriba: exit 4, 0 rojos y 0 pendientes. Las once secciones que no cambiaron dan la misma salida que en la
   r6, salvo los tiempos.
7. **FASE 2C r4** (`AgenteDetractor` nuevo, sin `name:`): `APROBAR_CON_CAMBIOS`, sin defectos de CORRECCIÓN. Citas 13 de 13
   (`verificar_citas_detractor.py` exit 0, en `salida/t5/citas_r4.txt`; hook: ENTREGA VERIFICADA). Recalculó a mano la pregunta 1
   de la semilla 1, comprobó que la hoja de respuestas es `metainfo$solution` y repitió la medición N = 100 con `bf11b0a4`. Después del veredicto no se cambió nada.

   | Hallazgo | Severidad | Destino |
   |---|---|---|
   | H1: `USO_ARCHIVOS` mira solo `config <- list(...)`; una reasignación posterior (`config$archivos <- 250`) exportaría 250 con la sonda en verde. Hoy no existe | BAJA (COBERTURA) | Residuo declarado |
   | H2: ramas de falla cerrada de `USO_CLAVE_PDF` (cinco), `USO_ORDEN_PDF`, `USO_EXSHUFFLE` y `USO_ARCHIVOS` sin mutante propio (obligación 7); `captura$moodle_orden` se calcula y nada la usa | BAJA (COBERTURA) | Residuo declarado |
   | H3 (a, d): la línea 1548 del verificador remite a este HANDOFF para la N = 100, que no estaba, y la medición no estaba atada al verificador final | BAJA (OTRO) | Atendido en el punto 4 |
   | H3 (b): el conflicto 3 de «CLOZE hermano» decía «Se barajan los textos del gap» sin la excepción del PDF | BAJA (OTRO) | Atendido: una frase en ese conflicto remite a la decisión «H1 c» |
   | H3 (c): la nota sellada de `detractor_fase2c` decía «no se cambió nada tras el veredicto» con el semillero `69194749`; propone `reopen` | BAJA (OTRO) | Sin `reopen`, por instrucción del coordinador: el `.Rmd` no cambió y los pasos 8 a 10 siguen valiendo. El sello nuevo cita la r4 y la cuarta pasada |

8. **Sellos:** no se reabrió ningún paso, porque el `.Rmd` no cambió. `detractor_fase2c` se volvió a sellar citando la r4. Los pasos 8
   a 10 siguen valiendo sobre `889d828e`.

## Aprobación (WAIT_USER #3)

1. **Aprobado para el aula el 2026-11-09.** Respuesta literal del profesor: «a».
   - md5 cableado en `RMD_MD5`: `889d828e8129e11b0ec5dceda0cf108b` (el `.Rmd` no cambia desde la tanda 3).
   - Semillero `d01bbf0f565ea6d651fba05fc795a268` · verificador `bf11b0a423717b3086443b8d3bbe1d28` · plantilla 1 b `290a3f79781b7bb8464e1275d9b342c1`.
2. Condición de uso del PDF impreso: generarlo con `generar_pdf()` de este semillero (plantilla 1 b y `exshuffle = FALSE`).
3. La r4 no deja decisiones pendientes: sus tres hallazgos son BAJA y quedan atendidos o como residuo.

## Residuos declarados

- C-4: la r7 da exit 4, rojo §P7 de la Parte 1 cubierto por la firma; no es PASS.
- V8: p2 a p4 dan `WARN_DIV_BAJA` (12, 17 y 20 de 100), dentro del rango firmado en C-2.
- H3 de la r2 (asimetría de la Parte 6, +0,9 pp) y H3 de la r3 (`uso` no ejecuta el flujo de arriba del semillero, y
  `USO_SEMILLERO` tiene componentes sin mutante propio). La cuarta pasada se limitó a H1 y H2.
- `uso` mide 3 preguntas en la semilla 1 (familia B de la regla #23, declarada). La N = 100 de esta tanda es una medición única.
- La vara §P7 se midió sobre ítems de opción simple; aplicarla gap a gap no está medido para CLOZE.
- El DOCX no deja juntas las cuatro figuras: no se usa para imprimir.
- H1 y H2 de la r4 (ver la tabla de la tanda 5).
- En exams 2.4.5, `exams2moodle()` y `exams2canvas()` no tienen el argumento `exshuffle` y abortan con él. Es un hecho medido, no un
  defecto: el semillero no se lo pasa.
- Sin auditar: Canvas, el DOCX, la nota del MULTIRESPONSE en Moodle (H5 de la r3; probar en `lab`) y el flujo interactivo del semillero.

## Fuera de alcance (visto, no tocado)

`../SemilleroUnico_v2.R` cambió el 2026-11-07 a las 09:10, después de la aprobación del SCHOICE: ya no trae ninguno de los cinco
literales que vigila la sección `uso` de `../verificar_espacio.R` (`solpcielo_una_por_pagina`, `twocolumn`, `samepage`,
`\raggedbottom`, `pdfjam`). Esa sección del SCHOICE daría rojo. La restricción 1 prohíbe tocarlo desde el CLOZE.

## Commit: evidencia que `.gitignore` deja fuera

`.gitignore` ignora `**/salida/`, así que esta evidencia entra con `git add -f`, por nombre. El verificador no lee nada de `salida/`
(escribe en carpetas temporales); del SCHOICE lee `../solpcielo_cloze.tex` y el `.Rmd` firmado.

- **Verificación:** `salida/verificar_r7.txt` y `salida/verificar_r7.md5`.
- **FASE 2C:** `salida/fase2c_r1_detractor.md`, `salida/fase2c_r2_detractor.md`, `salida/fase2c_r3_detractor.md`, `salida/fase2c_r4_detractor.md`.
- **Tanda 5:** `salida/t5/` completa.
- **Pasos 8 a 10 sobre `889d828e`:** `salida/paso8_t4/reporte_paso8.md`, `salida/auditoria_6b_t4/reporte_6b.md`, los seis `.txt` de
  `salida/paso9_889d828e/` y `salida/literalidad_exextra_t4.txt`.

No entran los renders (`pdf/`, `html/`, `moodle/`, `docx/`, `tex/`, `pdf_hook/`, `tex_hook/`) ni las capturas de `paso8_t4/` y
`auditoria_6b_t4/` (unos 53 MB): los renders se regeneran con `render_cloze.R`, y los reportes describen lo que muestran las capturas.

## Próximo paso

El aula: la promoción espera su evidencia (11/11 no es «terminado»). Pendiente: probar en `lab` cómo califica Moodle la Parte 6
(MULTIRESPONSE, H5 de la r3). Sin commit ni push.
