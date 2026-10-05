# HANDOFF — barras-campeonato-baloncesto-n3 / CLOZE (última actualización: 2026-10-04, ciclo 3 del mega-prompt)

**Disparador:** «Continúa con el CLOZE de barras-campeonato-baloncesto-n3»
**Archivo:** `barras_campeonato_baloncesto_aleatorio_interpretacion_representacion_n3_cloze_v1.Rmd` (md5 `791f4178`)
**Prompt del ciclo:** `mega-prompt-barras-campeonato-baloncesto-cloze.md` (batería D-1…D-17).
**SCHOICE hermano:** `../barras_..._schoice_v1.Rmd` (md5 `0c9cedab`). NO se modificó, y sus decisiones firmadas están en `../HANDOFF.md`.

## PREGUNTA_AL_RETOMAR
«¿Apruebas el CLOZE? Revisa en `salida/pdf1.pdf` y `salida/html*.html`: la estructura de 6 partes, el rótulo Gráfica I-IV dibujado en cada figura, el caso de práctica de las Partes 3, 4 y 6, los 5 pares de la Parte 5 con su nuevo párrafo «Por qué» y la instrucción «Cómo marcar». Firma además las 10 afirmaciones de `VERDAD_P5` y decide las dos propuestas de abajo (listas con letra en el PDF; `SemilleroCloze.R`).»

## Estado en una línea
Llevo 10 de 11 pasos. Ciclo 3: la batería D-1…D-17 está verde y la FASE 2C se cerró con un tercer detractor (Opus) más la 2C-bis (deepseek): 0 CORRECCIÓN, 0 ALTA, cambios aplicados y verificados. Faltan la aprobación del profesor y la evidencia de aula.

## Objetivos (`/goal`, 2026-10-04)
Fuente: `mega-prompt-barras-campeonato-baloncesto-cloze.md` §1 y §4 (adaptado del mega-prompt del
SCHOICE a pedido del usuario). Copia en memoria: `project_objetivos_barras_campeonato_baloncesto_n3.md`.

**General:** que el `.Rmd` CLOZE que llegue al aula tenga cero defectos en las seis partes
(matemática, visual, documentación ICFES, Solution, ortografía, render) en todas las versiones y
en todos los formatos que admiten cloze; «cero» = batería D-1…D-17 verde con salida real. Además:
fidelidad al ítem oficial en la Parte 1, SCHOICE hermano intacto y retomable con una frase.

| OE | Dominio | Checks | Veredicto final del ciclo 3 (2026-10-04) |
|---|---|---|---|
| OE1 | Matemática de las 6 partes | D-1 D-2 D-3 | CUMPLIDO (verificador 100/100, P4/P6 exhaustivos, P1 ≡ SCHOICE) |
| OE2 | Visual | D-4 D-5 D-6 | CUMPLIDO (multicopia ×3 y ×10; Familia B declarada) |
| OE3 | Render + Moodle | D-7 D-8 | CUMPLIDO (pandoc RStudio 3.10 efectivo; marcas del XML = exsolution) |
| OE4 | Solution | D-9 | CUMPLIDO (+ «Por qué» de P5) |
| OE5 | ICFES literal | D-10 | CUMPLIDO |
| OE6 | Diagnosticidad + diversidad | D-11 D-12 | CUMPLIDO (`WARN_DIV_BAJA` declarado) |
| OE7 | Texto + accesibilidad | D-13 D-14 | CUMPLIDO |
| OE8 | Estructura CLOZE y fugas entre gaps | D-15 D-16 | CUMPLIDO (D-16 medido, condicionado a lo visible en P1) |
| OE9 | Fidelidad al ítem + SCHOICE intacto | D-2 + md5 `0c9cedab` | CUMPLIDO (el SCHOICE no se tocó) |
| OE10 | Usable por el profesor | D-17 | CUMPLIDO para los 2 Semilleros del ejercicio; `SemilleroCloze.R` pendiente de decisión |
| OE11 | Retomable con una frase | este archivo + memoria | CUMPLIDO |

## Línea base D-1…D-17 (2026-10-04, md5 `17df2bfc`, ANTES de tocar nada)
Renders en el scratchpad de la sesión (`r0/`): HTML×3, PDF×3 (`solpcielo_cloze.tex`), DOCX×3, Moodle×5, PDF×3 con pandoc RStudio.

| ID | Estado | Cifra / salida |
|---|---|---|
| D-1 | VERDE | verificador 100/100; multisemilla 100/100; P4 exhaustivo 1 320 P válidas, 0 segundas claves; P6 exhaustivo 0 casos sin candidato falso |
| D-2 | VERDE | 13/13 canónicas = impreso (I=E1, II=4,5 literal, III clave agrupada, IV=E4); P1 CLOZE ≡ SCHOICE en 10 campos × 100 semillas (0 diferencias) |
| D-3 | VERDE | coherencia Niveles 4-5: 0 ERR_SEM/ERR_ANS (solo `ERR_C4`, override firmado) |
| D-4 | VERDE (Familia B, N=2 copias leídas: canónica y rama E8) | barras = matriz en las 8 opciones; rótulo Gráfica I-IV dentro; subtítulo del tipo solo fuera de la canónica. Stress test 2H: PASA (10 semillas) |
| D-5 | VERDE | PDF×3: 18 imágenes, 15 distintas (los 3 pares repetidos = clave/Solution en la MISMA copia, págs. 4/6, 10/12, 17/18); HTML 18 base64; DOCX 18 media, 18 distintas |
| D-6 | VERDE | `0.0.`/«Figure»: 0 en PDF y PDF-RStudio; 0 `figcaption`; 0 ids duplicados en HTML×3; 2I OK |
| D-7 | VERDE (con reserva: ver ciclo 3) | html/pdf/docx/moodle OK; «pandoc RStudio» OK, pero esa corrida usó en realidad el 3.10.2 de la terminal; `exams2nops` → «the following exercises are cloze exercises» (rechazo explícito) |
| D-8 | VERDE | 30/30 nombres neutrales con sufijo; 5/5 preguntas: 6 gaps, marca XML = exsolution en los 6, sin `_S`, sin markup en gaps |
| D-9 | VERDE | 2J OK; bloque Solution del verificador 100/100; Solution leída en canónica (agrupada) y rama E8 (apilada) |
| D-10 | VERDE | `exextra` = SCHOICE salvo Type/Fuente; Afirmación, Evidencia, Descriptor, Estándar y Competencia literales en el catálogo canónico (normalizado ASCII) |
| D-11 | VERDE | P1 §P7 batería congelada: +1,4 pp, PASS; P5 léxico 0,644 vs techo nulo 0,778 (−13,4 pp); P(V) por posición 0,45-0,55; P4 clave por posición 0,20/0,33/0,21/0,26 (n=100); P6 45 V |
| D-12 | VERDE con WARN declarado | p1 88, p5 51; p2 13, p3 8, p4 8, p6 2 → `WARN_DIV_BAJA` (claves categóricas), sin `ERR_DIV_COSMETICA` |
| D-13 | VERDE | ortografía y glifos limpios; 3 versiones de ajedrez: 0 formas masculinas (control: 76-83 «partidas») |
| D-14 | VERDE | alt de opciones = «Gráfica N: gráfica de barras <tipo>»; enunciado y Solution con datos |
| D-15 | VERDE | 6 `##ANSWERi##` en orden; 100/100 versiones con 6 campos en exclozetype/exsolution/extol y Answerlist 15 |
| D-16 | VERDE con residuo declarado | mejor token visible de P2-P6 → posición de la clave: +5,1 pp (< p95 nulo); → formato: +13,2 pp TODO de la canónica («décimo»: sus grados excluyen sexto/séptimo del caso de práctica); sin canónica −4,4 pp. La canónica ya se reconoce por el enunciado (§P7-E) |
| D-17 | **ROJO** (medido a las 20:07, antes del arreglo) | `SemilleroUnico_v2.R` → `.Rmd` SCHOICE (y NOPS); `SemilleroCloze.R` y `SemilleroMoodle_v2.R` → teorema de Pitágoras |

## Ciclo 3 (2026-10-04): qué se hizo y batería después (md5 `791f4178`)
**Cambios:**
- **D-17:** `SemilleroUnico_v2.R` ahora apunta al CLOZE, con `template = "solpcielo_cloze"` y sin `exams2nops` (comentado con el motivo). `SemilleroMoodle_v2.R` también apunta al CLOZE.
- **Objeción 1 del detractor Opus:** la Parte 5 tiene una explicación «Por qué» por concepto (`porque_p5`, 5 pares). Solo cambia la Solution: no consume RNG ni pasada §P7-D. El verificador tiene una guardia nueva.
- **Objeción 2:** la suite permanente `tests/testthat/test_barras_campeonato_cloze.R` (enganchada a `run_all_tests.R`) tiene 9 mutantes (a-i) y el test de Semilleros. Resultado: 34 expectativas, 0 fallos.
- **Verificar al verificador:**
  - 5/5 inyecciones del mega-prompt detectadas: (a) clave P1, (b) distractor = clave, (c) sin `fig_id`, (d) P2 +1, (e) P5 invertida, cada una con 0/100 versiones que pasan. La (c) también la ve la multicopia: 14 imágenes distintas contra 15.
  - Parser D-8 corregido (fallaba 5/5 en un caso bueno: no leía `=6:0`).
  - **`RSTUDIO_PANDOC` no fuerza la versión:** `find_pandoc()` elige la más alta, así que se usó el 3.10.2 del PATH. Ahora se fuerza con `find_pandoc(dir=, cache=FALSE)` → 3.10 efectivo.

| ID | Antes | Después | Evidencia del después |
|---|---|---|---|
| D-1 | V | V | verificador 100/100; multisemilla 100/100 |
| D-2 | V | V | 13/13 canónicas = impreso; P1 ≡ SCHOICE, 0 diferencias en 10 campos × 100 |
| D-3 | V | V | coherencia: solo `ERR_C4` (override) |
| D-4 | V (N=2) | V (Familia B: N=2 copias de `r0` + 4 páginas del Semillero) | ajedrez y tenis de mesa vistos; rótulos dentro |
| D-5 | V | V | Semillero ×10: 60 imágenes, 50 distintas (10 pares clave/Solution); DOCX 60 media |
| D-6 | V | V | 0 `0.0.`/«Figure» en PDF, PDF-RStudio y PDF ×10 |
| D-7 | V* | V | pandoc RStudio 3.10 **efectivo** (`pandoc_exec()` comprobado); NOPS rechaza cloze |
| D-8 | V | V | 5/5 preguntas: marca = exsolution en los 6 gaps, también con `mchoice = list(shuffle = TRUE)` del Semillero (sin `_S`) |
| D-9 | V | V | 2J OK; «Por qué» de P5 leído en el PDF (pág. 7) |
| D-10 | V | V | sin cambios en `exextra` |
| D-11 | V | V | sin cambios en opciones: P1 +1,4 pp; P5 léxico −13,4 pp |
| D-12 | V (WARN) | V (WARN declarado) | p1 88, p5 51; p2 13, p3 8, p4 8, p6 2 |
| D-13 | V | V | ortografía y glifos limpios tras el cambio |
| D-14 | V | V | sin cambios |
| D-15 | V | V | 0/100 versiones con estructura incorrecta |
| D-16 | V (residuo) | V | El +13,2 pp de «formato» era un defecto de la MÉTRICA, que no condicionaba por lo visible en P1: canónica ⇔ «4,5» visible en P1 (2000/2000) y, dentro de ella, la clave es siempre agrupada (241/241). P2-P6 no añaden información sobre la clave: 0 en la canónica, −4,4 pp fuera de ella. La identificabilidad de la canónica viene de la decisión firmada (4,5 literal, H-5) y de la vara §P7-E del SCHOICE |
| D-17 | R | V | 2/2 Semilleros del ejercicio apuntan al CLOZE; Semillero ×10 ejecutado de verdad (PDF 63 págs., DOCX, Moodle, webquiz) |

**Detractores del ciclo 3:**
- **FASE 2C:** `AgenteDetractor` nuevo, sin `name`, sobre md5 `17df2bfc`. Veredicto `APROBAR_CON_CAMBIOS`: 0 CORRECCIÓN, 0 CRÍTICA, 0 ALTA, 3 MEDIA.
  - Objeciones 1 y 2: aplicadas.
  - Objeción 3 (`SemilleroCloze.R`): declarada, ver propuestas.
- **FASE 2C-bis:** deepseek-v4-flash (`detractor-hetero-deepseek-20261004-202613.md`). Veredicto `APROBAR_CON_CAMBIOS`: 0 CORRECCIÓN.
  - Objeción 1 (D-16 +13,2 pp exige override): **refutada con medición** (fila D-16).
  - Objeción 2 (D-17 desactualizado): la línea base era anterior al arreglo; ver «Después».
  - Objeción 3 (paso 11 abierto): por diseño.
- **§P7-D del CLOZE:** siguen 2 pasadas de 3 (este ciclo no tocó la diagnosticidad).

## Propuestas al profesor (no aplicadas: decisión humana)
1. **Listas con letra en el PDF para P4 y P5** (detractor Opus). Las opciones en línea separadas por «/» obligan a contar barras para saber cuál es la (c). Propuesta: antes de `##ANSWER4##` y `##ANSWER5##`, un bloque `{=latex}` con `enumerate` (a)-(e) que solo vea el PDF. Toca la decisión firmada de `solpcielo_cloze.tex`.
2. **`SemilleroCloze.R`:** es una plantilla genérica de teorema de Pitágoras (copiada el 2025-11-02). Su rama PDF compila un archivo de prueba, no `archivo_examen`, así que no basta con reapuntarlo. Está fuera del commit, sin rastrear: ¿borrarlo?

## Decisiones del usuario (2026-10-04)
- **Flujo B:** se reutiliza el TikZ del SCHOICE.
- **Parte 1:** se hereda tal cual la generación del SCHOICE: rama E8 (P = 1/3), rótulo del tipo fuera de la canónica y canónica igual al impreso, con B = 4,5 literal. Con la misma semilla, la Parte 1 coincide con el SCHOICE (medido en 100/100).
- **OVERRIDE `exshuffle: FALSE` (firmado, H-5).** Con `TRUE`, R/exams mezclaba la lista «Gráfica I-IV» del gap, mientras la hoja del PDF pide (a)-(d) por posición. Quien identificaba bien la gráfica quedaba mal calificado en unas 3 de cada 4 versiones. La mezcla ahora es interna: P1 por `orden`, P4 por `perm4`, P5 por `perm5`, y P6 es V/F en orden fijo. El hook sigue mostrando **`ERR_C4`**: está declarado, no silenciado. Moodle no vuelve a mezclar, porque ningún gap usa `_S`.

## Diseño
| Parte | Tipo | Qué evalúa | Datos |
|---|---|---|---|
| 1 | schoice | Qué gráfica reúne la tabla y la gráfica (ítem oficial) | oficiales |
| 2 | num | Total de una categoría entre los dos grados | oficiales |
| 3 | num | Medida del segmento superior de una barra apilada (E8) | práctica |
| 4 | schoice | Qué error (E1-E6) explica la gráfica de un estudiante | práctica |
| 5 | mchoice | 5 afirmaciones, una por par concepto (V/F); 2-3 verdaderas | generales |
| 6 | V/F | Altura de la barra apilada correcta del caso de práctica | práctica |

- **Fugas:** en un CLOZE los seis gaps se ven a la vez. Ninguna parte con texto visible usa datos oficiales (la 2 los usa, pero es `num`). El caso de práctica se sortea de forma independiente de M y de las opciones.
- **Rótulo dentro de la figura:** como texto aparte, un salto de página del PDF separaba el rótulo de su imagen.
- **Parte 5 por pares:** con los dos miembros de un par a la vista, la afirmación con matiz delataba a la verdadera (41/100 versiones).
- **Plantilla `solpcielo_cloze.tex`:** es una copia de `../solpcielo.tex` con la instrucción «Cómo marcar», porque las opciones se imprimen en línea, separadas por «/» y sin letra. La del SCHOICE no se tocó.

## Evidencia del ciclo 2 (md5 `17df2bfc`)
Ver la línea base de arriba, que la re-midió entera; los 14 mutantes del ciclo 2 se reemplazaron por los 9 permanentes del test.

## Declarado (no defecto)
- **Evidencia de Nivel 3 del aula: se toma del SCHOICE, no de la Parte 1 del CLOZE.** Las Partes 2 a 5 andamian la trampa de la Parte 1 y se ven a la vez:
  - P2 da el total apilado, que descarta E8 por la altura total;
  - P3 nombra E8;
  - P4 enumera E1-E6;
  - P5 trae las propiedades.

  Si el CLOZE se usa para medir, aplicar antes el SCHOICE.
- DOCX: los `##ANSWERi##` salen literales, igual que en los CLOZE aprobados del repo.
- `VERDAD_P5` del verificador es un oráculo humano: el profesor debe firmar las 10 afirmaciones.
- Estratos con n < 20 en N = 100 (canónica 13; E2 y E3 de P4) son NO CONCLUYENTES por estrato.

## Deuda de infraestructura (fuera de este subproyecto)
- `post-exams2-validation.sh`: con ruta relativa, la FASE 2N da un `WARN_DIV_ESTATICA` falso. Sigue sin dispararse con `rep()`, `file.path()` o variables.
- **Regla #20:** «validar con `RSTUDIO_PANDOC=...`» no basta cuando el pandoc del PATH es más nuevo: hay que forzarlo con `rmarkdown::find_pandoc(dir = ..., cache = FALSE)` y comprobar `pandoc_exec()`. Memoria `feedback_rstudio_pandoc_no_se_fuerza`.

## Siguiente paso
1. El profesor responde la PREGUNTA_AL_RETOMAR: aprobación, firma de `VERDAD_P5` y las 2 propuestas.
2. Si aprueba: `workflow-state.sh complete <dir> aprobacion_usuario`.
3. Evidencia de aula (Nivel 3, que se toma del SCHOICE) y después `/promover-ejercicio`.

## Cómo verificar
```bash
cd /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/barras-campeonato-baloncesto-n3/cloze
Rscript verificar_dibujo_clave_cloze.R            # N = 100
F=$PWD/barras_campeonato_baloncesto_aleatorio_interpretacion_representacion_n3_cloze_v1.Rmd
cd ../../../.. && Rscript .claude/scripts/validar_multisemilla.R $F && Rscript .claude/scripts/validar_diversidad_sustantiva.R $F
Rscript -e 'testthat::test_file("tests/testthat/test_barras_campeonato_cloze.R")'   # 9 mutantes + Semilleros (~10 min)
```
