# FASE 2C, ronda 4: detractor de `totales-promedios-reciclaje-n3` (MAT-2026-07-050)

**Veredicto: APROBAR_CON_CAMBIOS.** Los seis cambios corrigen su hallazgo y no introducen ningún defecto de corrección. Tampoco alteran las opciones, la clave, la diagnosticidad ni la canónica. Ninguna objeción es crítica ni alta. Quedan 2 objeciones MEDIAS (testing y deuda de la familia F2) y 2 BAJAS. Ninguna bloquea la FASE 3.

- **Fecha:** 2026-10-29.
- **Versión auditada:** md5 `7d5855dad5b9d6cee745b823c27b835f`, comprobado. La versión de comparación es la de HEAD `17dfc787`, con md5 `cd94b59adfdea7b9c92dc074ce0a1435`, la aprobada en la ronda 3.
- **Independencia:** no escribí ni corregí este artefacto y no audité ninguna ronda anterior. Corro como subagente bloqueante, sin `name:`.
- **Repo:** no modifiqué ningún archivo. Todo lo que renderé está en el scratchpad.

## 1. Los seis cambios, uno por uno

| # | Cambio | ¿Corrige el hallazgo? | ¿Introduce algo? | Evidencia medida |
|---|---|---|---|---|
| 1 | Causa raíz de ERR-TOTAL: «…calcula un total, pero no le resta el del otro grupo» | **Sí.** | Nada. La frase es verdadera: la afirmación compara, y lo que falta es restar. | Línea 263 del `.Rmd`. Visible en la captura a 360 px de `salida/r4/html/html_9.html`. |
| 2 | Se retira `cursos_txt` | **Sí.** | Nada. | La función no aparece en el `.Rmd`. Con las semillas 1 a 1000, el `.Random.seed` tras el chunk es idéntico entre r3 y r4 (0 diferencias): no cambia el consumo de azar. |
| 3 | `encabezado()` con un bloque `{=latex}` `\nobreak…\@afterheading` | **Sí.** | Ninguna fuga en ningún formato. | Ver el detalle debajo de la tabla. |
| 4 | `<span>` con relleno en cada celda de la tabla | **Sí** en HTML y Moodle. | Ninguna fuga. Deuda en la familia F2 (H2). | Ver el detalle debajo de la tabla. |
| 5 | Espacio duro (U+00A0) en «El Bosque» y «La Pradera» de la tabla | **Sí.** | Nada. | Ver el detalle debajo de la tabla. |
| 6 | `explica()` de ERR-TOTAL sin paréntesis alrededor de la fórmula | **Sí.** | Nada. | Ver el detalle debajo de la tabla. |

**Cambio 3 (encabezado al pie de página).**
- **Prueba pareada:** `set.seed(2026)` y `exams2pdf(n = 100)` sobre r3 y sobre r4. Para cada página que no es la última, un detector busca un encabezado en el último renglón o en los dos últimos.
- **Resultado:**
  - En r3 queda un encabezado al pie en **18 de 100 PDF**, siempre «Estrategia para evitar el error» al final de la página 2. Es coherente con el 11 de 60 del paso 8, y sirve de control positivo: el detector caza el defecto.
  - En r4, sobre las mismas versiones: **0 de 100**.
- **Ejemplo (`p002`):** el encabezado queda ahora en la página 2 junto con el primer ítem de la lista. Todos los PDF siguen teniendo 3 páginas.
- **Fugas del bloque `{=latex}`:** busqué `afterheading`, `makeatletter`, `nobreak` y `{=latex}`.
  - Salidas del repo (`salida/r4`): 0 en 20 HTML, 20 preguntas de Moodle, 3 DOCX, 3 PDF y 2 NOPS.
  - Mis renders: 0 en 40 HTML, 40 de Moodle, 40 DOCX, 2 NOPS y 200 PDF.
- **Conversión con la `pandoc()` interna de exams:**
  - a LaTeX, el bloque queda tal cual entre dos párrafos;
  - a HTML, desaparece.

**Cambio 4 (relleno de las celdas).**
- **HTML:** 9 spans por versión (3 de cabecera y 6 de celda). En la captura a 360 px las cabeceras ya no quedan pegadas.
- **Moodle:** 180 spans en el XML del repo (20 × 9) y 360 en el mío (40 × 9).
- **PDF:** pandoc convierte cada span en un grupo `{…}`, sin fuga: `{\textbf{El~Bosque}} & {7 cursos} & {59 kilos}`. `pdftotext` no encuentra `span` en ninguno de los 200 PDF.
- **DOCX:** 0 fugas en 43 archivos.
- **Ancho:** se mide **antes** de envolver, así que la cota de 85 caracteres sigue midiendo lo que se ve en el PDF. Con `--columns=99999` (confirmado en `exams:::pandoc`), el largo de la línea pipe no cambia los anchos.

**Cambio 5 (espacio duro).**
- Está solo en la línea 79, en `filas`. En LaTeX sale `El~Bosque`. En la captura a 360 px (`html_9`), «El Bosque» y «La Pradera» quedan en un solo renglón y la tabla cabe sin desplazamiento horizontal.
- 0 de 1000 versiones tienen el U+00A0 fuera de la tabla (opciones, enunciado, feedback, clave).
- La canónica usa `CONJUNTOS[[1]]`, que no se tocó.
- La FASE 2O del arsenal sobre este md5 da 0 errores.
- En la prosa de la Solution, «la sede El / Bosque» sí se parte a 360 px. Es un corte de renglón normal en prosa y B2 se refería a la tabla, así que no lo cuento como defecto.

**Cambio 6 (paréntesis en ERR-TOTAL).**
- En la captura a 360 px ya no queda ningún «(» huérfano.
- Las frases nuevas son verdaderas por construcción. «No es la diferencia» se sostiene porque la precondición exige *T*_G ≠ *D*. «Falta restarle…, y *T*_G − *T*_P = *D*» es aritmética de la propia versión.
- Ninguna otra explicación de la Solution pone una fórmula entre paréntesis. Los paréntesis que quedan (líneas 542, 599 y 602) encierran cifras en prosa.

**Invariancia de todo lo demás (semillas 1 a 1000, r3 frente a r4).**
- Opciones, `sol`, feedback, enunciado y celdas de la tabla (sin span y con el U+00A0 cambiado por un espacio): **0 diferencias**.
- La Solution, normalizando los encabezados y la causa raíz, solo difiere en las 176 versiones con ERR-TOTAL, y solo en su `explica()`.

**Lo que eso implica:**
- **Corrección:** igual que en la ronda 3. Hay una sola verdadera y es la clave; la Solution y el feedback no traen afirmaciones falsas nuevas (regla #13 v1.2).
- **Diagnosticidad:** igual. `verificar_espacio_ronda4.txt` reproduce exactamente las cifras firmadas en «O-1 corregido»: 81,5 / 74,5 %, S1 en PROMEDIO +8,4 pp, largo +10,4 pp con margen 2,0 %, y la canónica +7,5 pp. El alcance del override **sigue cubriendo** esta versión.
- **Canónica (H-2):** su texto visible no cambia. El span es invisible y el encabezado de la Solution es el mismo texto en negrita.
- **Guardas:** no se tocó ninguna `stopifnot`. Los 18 mutantes de E5 mueren en la ronda 4.

## 2. Los 8 dominios sobre el `.Rmd` completo

- **Código R/exams:** metadatos completos, `exshuffle: TRUE` con opciones de texto, guarda `\newcounter{none}` y la tabla dentro de 85 caracteres. El arsenal da 0 errores sobre el md5 `7d5855da…` (informe de las 19:33, posterior al `.Rmd` de las 19:30). Sus 6 avisos son los heurísticos de siempre (`Tarea`, patrón «Es posible que…»).
  - El truco `intToUtf8(96)` (línea 582) sirve para que el arsenal 2D **sí** vea el chunk entero; no evade nada.
  - Observación: el pandoc del PATH es el 3.10.2, que también emite `\def\LTcaptype{none}`, y la guarda lo cubre.
- **Pedagógico:** las 6 partes de la Solution, el reparto 2 + 2 y 8 plantillas de 8 tipos.
- **Visual:** capturas a 360 px de la tabla, de las opciones y de ERR-TOTAL (N = 1 versión, ver «No auditado») y PDF pareados N = 100.
- **Gramática y ortografía:** tildes correctas. La concordancia por *k* está resuelta: `participaron_txt` y «aporta/aportan».
- **Coherencia matemática:** sin cambios respecto de la ronda 3 (invariancia medida sobre 1000 semillas).
- **ICFES y metacognición:** `exextra` literales de `ITEM_OFICIAL.md`. Nivel 3 con DOK 2 es coherente.
- **Testing:** H1 y H3.
- **Semántico:** el código ERR coincide con la afirmación, igual que en la ronda 3, porque las opciones son idénticas.
- **Invariantes locales y overrides:** L-1 a L-5 están intactas. Contrasté la canónica contra `ITEM_OFICIAL.md` y contra el recorte `orig_pregunta_50.png`, que imprime A-D y la tabla 5/40 y 4/45 tal como están en el `.Rmd`. O-1 corregido cubre esta versión, pero no tiene guarda ejecutable (H1).

## 3. Hallazgos

| ID | Severidad | Hallazgo | Evidencia | Propuesta |
|---|---|---|---|---|
| H1 | **MEDIA · TESTING** (override sin guarda, regla #9 v1.5) | El alcance firmado de «O-1 corregido» («si una medición posterior supera cualquiera de estas cifras… vuelve al profesor») no existe en código. `verificar_espacio.R` sigue comparando contra el 50 % de la premisa tachada y marca las 11 sondas como PENDIENTE DE FIRMA con exit 3. Lo hacía antes de la firma (ronda 3) y lo hace después (ronda 4). Un exit 3 permanente no distingue «dentro del alcance firmado» de «lo superó»: si mañana S_U con las tres pistas sube de 81,5 a 90 %, la salida sería la misma. Es el caso de los Errores 35-37 («una invariante sin guardia no es una invariante»). | `verificar_espacio.R:348-349` compara con `0.5`. El `.R` es de las 11:08 y la firma (`.claude/CLAUDE.md` local) de las 12:20. `verificar_espacio_ronda4.txt:202`: `PENDIENTE DE FIRMA (11; 0 fallos)`. | Codificar la tabla firmada como constantes por sonda y rama: 71,0 / 60,5 / 79,0 / 51,5 / **81,5** en CURSOS; 55,0 / 62,0 / 67,0 / 58,0 / **74,5** en PROMEDIO; S1 en PROMEDIO +8,4 pp; largo +10,4 pp con margen ≤ 2,7 %. Con la cifra ≤ la firmada, `OK: dentro del alcance firmado de O-1 corregido`; por encima, PENDIENTE (exit 3). Añadir un mutante que suba una cuota (por ejemplo, el peso de ERR-TOTAL) y compruebe que sale PENDIENTE. Las semillas son fijas (1001…), así que las cifras son deterministas y el umbral exacto no da falsos rojos. |
| H2 | **MEDIA · CÓDIGO/DEUDA** (regla #21, F2) | El relleno corrige un defecto de la **familia** F2 (el 6b lo describió como «estilo de la familia F2»), pero solo en la copia local. La función canónica `.claude/scripts/snippets_familias_rmd.R` no tiene el parámetro `relleno` ni `inline-block`, así que los demás ejercicios con F2 conservan las cabeceras pegadas. El comentario de la línea 24 sigue diciendo «copia de …snippets_familias_rmd.R», y `test_tabla_responsiva_pdf.R` no cubre la variante: que el span «en PDF y DOCX deja solo el texto» solo está verificado a mano (lo verifiqué yo: `{\textbf{El~Bosque}}` y 0 fugas en 43 DOCX). | `.Rmd:24` y `.Rmd:32`; `snippets_familias_rmd.R:63`, sin `relleno` en la firma. | Llevar `relleno` a la función canónica (con valor por defecto igual al local o `NULL`). Añadir a `test_tabla_responsiva_pdf.R` un caso que compruebe que el span existe en HTML y desaparece en LaTeX y DOCX. Mientras tanto, corregir el comentario de la línea 24 para que diga que es una variante local. No toca este `.Rmd` ni pide otra FASE 2C si se hace solo en el canónico. |
| H3 | **BAJA · TESTING** | La guarda «L-1: canónica verbatim (tabla)» calcula su suma de control sobre `tabla_df`, antes de `tabla_responsiva()`, y no sobre `tabla_md`, que es lo que se muestra. La ronda 4 añade una capa más entre lo vigilado y lo visible (`envolver`). Un fallo en esa capa que cambie el texto de una celda no lo cazaría ninguna guarda. Hoy no hay defecto: lo medí y las celdas visibles son idénticas a las de r3 en 1000 de 1000 semillas. | `.Rmd:513` (suma de control sobre `tabla_df`) y `.Rmd:493` (lo visible sale de `tabla_responsiva`). | En la canónica, extraer las celdas de `tabla_md` (quitar `<span …>`, `</span>`, `**` y U+00A0) y compararlas con las mismas sumas de control (119 / 695221). Añadir el mutante correspondiente a `verificar_espacio.R`. |
| H4 | **BAJA · PROCESO** | `HANDOFF.md` no refleja la ronda 4. Cierra con «Aprobado para llevarlo al aula» y «11 de 11», pero `ejercicio_state.json` tiene la reapertura #3, 6 de 11 pasos y `aprobacion_usuario` des-sellado, y el md5 vigente (`7d5855da…`) no es el aprobado (`cd94b59a…`). Quien lea solo el HANDOFF, como manda la memoria del proyecto, puede llevar r4 al aula como si estuviera aprobada. De paso: los pasos 3 a 6 se sellaron en 177 ms (19:35:58,661 → ,835). Lo audité por artefacto y sí existen: `salida/r4/` y el informe del arsenal con el md5 `7d5855da…`, así que no es un defecto. | `HANDOFF.md:361` y `HANDOFF.md:372`. | Añadir al HANDOFF una sección «Ronda 4» con el md5 nuevo y el estado (reabierta, a la espera de la FASE 2C, el paso 8 y una nueva aprobación) antes del WAIT_USER, y actualizar la línea de memoria del proyecto. |

**CORRECCIÓN: ninguna. DIAGNOSTICIDAD nueva: ninguna** (las opciones son idénticas a r3). **CRÍTICAS: ninguna. ALTAS: ninguna.**

## 4. No auditado

- **Cómo pinta Moodle los spans en el servidor `icfes`.** Solo verifiqué el XML. Si el servidor limpiara el HTML con HTMLPurifier sin `CSS.Trusted`, podría descartar `display:inline-block`. El relleno horizontal sobreviviría y la degradación sería suave, pero no lo medí.
- **DOCX y NOPS rasterizados:** no los miré visualmente; solo busqué fugas en el texto.
- **Pandoc de RStudio (3.8.3):** solo probé el 3.10.2 del PATH.
- **HTML a 360 px:** N = 1 versión (`html_9`, El Bosque + ERR-TOTAL), por el presupuesto de turnos (regla #23, familia B, declarado). Los PDF sí son N = 100 pareados.
- **Verificador:** no lo volví a correr entero; leí su salida de la ronda 4.

## 5. Scripts y salidas

Todos en `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/35e93ef1-04b2-4e1e-8b49-a10ac14fa449/scratchpad/det_r4/`:

- **Comparación r3 frente a r4:** `cmp.R` y `cmp2.R`.
- **Renders:** `render.R`, que produce `pdf_r3/`, `pdf_r4/`, `html_r4/`, `docx_r4/`, `moodle_r4/` y `nops_r4/`.
- **Detector de encabezados al pie:** `huerfanos.py`.
- **Conversión pandoc a LaTeX y HTML:** `pandoc_chk.R`.
- **Capturas a 360 px:** `shots/`.

CITAS_DETRACTOR
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/verificar_espacio.R:349 | por encima del 50 %% que supone la premisa firmada
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/.claude/CLAUDE.md:226 | Si una medición posterior supera cualquiera de estas cifras
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/salida/verificar_espacio_ronda4.txt:202 | === RESULTADO: PENDIENTE DE FIRMA (11; 0 fallos) ===
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd:24 | ## --- Familia F2: tabla responsiva (copia de .claude/scripts/snippets_familias_rmd.R,
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd:32 | relleno = "display:inline-block; padding:0.2em 0.6em;") {
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/scripts/snippets_familias_rmd.R:63 | tabla_responsiva <- function(df, align = NULL, bold_rows = integer(0), ancho_max_pdf = 85) {
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/scripts/snippets_familias_rmd.R | AUSENTE: inline-block
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd:513 | celdas <- paste(c(names(tabla_df), as.vector(t(as.matrix(tabla_df))))
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd:493 | tabla_md <- tabla_responsiva(tabla_md_df, align = c("c", "c", "c"))
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/HANDOFF.md:372 | Aprobado para llevarlo al aula el 2026-10-29; no está en producción.
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/HANDOFF.md:361 | `aprobacion_usuario` queda sellado: 11 de 11.
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS
