# Reporte del paso 8: 5 coherencias de `rapidez_media_error_paso_..._n3_schoice_v1.Rmd`

**md5 verificado:** `904af1ca1271d97c54123218c9db03c0`. Coincide con el esperado. No rendericé nada.

**Veredicto: las 5 coherencias salen OK.** Hay 4 observaciones menores, sin hallazgos que bloqueen. Están al final.

**Alcance de lo que leí**
- **Fuente:** el `.Rmd` completo (578 líneas), `ITEM_OFICIAL.md`, `.claude/CLAUDE.md` y el recorte `orig_pregunta_09.png`.
- **Previews del PDF:** `q-1`, `q-2`, `s-1`, `s-2` y `s-3`. Los dos `q` son el PDF y los tres `s` son el NOPS.
- **Captura canónica:** solo `shots_r4/v07_360.png`.
- **HTML, DOCX, Moodle y NOPS:** extraje su texto.
- **Auditoría visual:** `salida/auditoria_visual_r4.md`.
- **Logs del paso 10:** `salida/paso10_literalidad.txt` y `salida/paso10_coherencia.txt`.
- **Versiones por formato:** cada formato salió de una semilla distinta, así que no se comparan entre sí. Cada uno se verificó contra su propia aritmética.

## Coherencia 1: Semántica

- El ejercicio pide señalar en qué paso de un procedimiento hay un error y por qué. Eso es el objeto del ítem oficial y de `exextra[Evidencia]`, que habla de argumentar a favor o en contra de un procedimiento.
- **En el PDF** (camión, 3:00 p. m. a 5:00 p. m., de 50 km a 130 km), el error está en el paso 2. El texto de la Solution y las razones falsas hablan de ese mismo paso.
- **En el HTML** (furgoneta), el error está en el paso 3 y la Solution lo explica como unidades invertidas.
- **En el DOCX** (moto), el error está en el paso 1, con la suma 160 + 70 = 230. La clave es «90 km y no 230 km».
- **Letter-independence (regla #19):** la Solution dice «la que afirma: «…»» y nunca nombra una letra. El feedback tampoco usa letras.
- **Tildes y concordancia:** no vi fallos de tildes ni de contracciones («al», «del») en los textos leídos.

**OK** — en las tres versiones, el paso señalado, la razón y la Solution hablan del mismo error.

## Coherencia 2: Visual-Texto

No hay gráfica. Comparé lo que se ve con lo que dice el texto, y la canónica contra el impreso.

- **Canónica v07 contra el impreso:**
  - El enunciado es idéntico al impreso (8:00, 20 km, 10:00, 100 km).
  - Paso 1: `100 km − 20 km = 80 km`.
  - Paso 2: `10 h − 8 h = 2 h`.
  - Paso 3: `80 h/2 km = 40 h/km`, con las unidades invertidas. La trampa se conserva.
  - Las 4 opciones son las impresas, con el `−20 km` de la opción D y el `−2 h` de la B. La clave es «paso 3 … 80 km/2 h».
  - La Solution de v07 señala la errata del `−20 km` como «cuenta tampoco correcta: 20 km − 100 km = −80 km». No la corrige en el distractor, como exige L-3.
- **Orden de las opciones:** en v07 aparecen mezcladas (clave en a), porque `exshuffle: TRUE`. El impreso las trae en A a D. No es una divergencia de contenido. Una cosa que la auditoría de HTML no pudo comprobar, que el impreso estuviera a la vista, la comprobé yo contra el recorte.
- **PDF y NOPS:** los símbolos matemáticos salen bien y no hay `$` ni `\text` visibles. El NOPS marca la X en (d), que en esa versión es la clave («paso 2 … 2 h y no 5 h»).
- **Sin texto roto:** en los previews, el PDF no muestra caracteres rotos ni `NA`.

**OK**

## Coherencia 3: Matemática

Verifiqué a mano cada versión que vi.

| Formato | Cuentas | Clave | Distractores |
|---|---|---|---|
| PDF (camión) | 130 − 50 = 80; 5 − 0 = 5; 80/5 = 16, y el correcto es 80/2 = 40 | paso 2, «2 h y no 5 h» | 130 es la posición final; 5 km/80 h son cifras cambiadas; 90 = (50 + 130)/2 es el promedio |
| HTML (furgoneta) | 240 − 90 = 150; 6 − 4 = 2; 150/2 = 75 | paso 3, «150 km/2 h» | 2 h/150 km es el cociente invertido; 3 h = 2 + 1; 330 = 240 + 90 |
| DOCX (moto) | 160 + 70 = 230; 10 − 8 = 2; 230/2 = 115, y el correcto es 90/2 = 45 | paso 1, «90 km y no 230 km» | 10 h es la hora final; 2 h/230 km; 115 = (70 + 160)/2 |
| Canónica (v07) | 100 − 20 = 80; 10 − 8 = 2; 40 | paso 3, «80 km/2 h» | 100 km, −2 h y −20 km |

- **Ninguna razón falsa repara el procedimiento (L-4).** Probé a sustituir cada razón en su paso:
  - PDF: 90 contra 40 × 5 = 200.
  - HTML: 330 contra 75 × 2 = 150, y 3 h contra 150 = 75 × 3.
  - DOCX: 115 contra 45 × 2 = 90, y 10 h contra 230 = 45 × 10.
  - Ninguna da la rapidez correcta, así que no hay una segunda clave.
- **Una sola clave por versión:** en Moodle, las 3 preguntas tienen exactamente una respuesta con fraction 100 (`0,100,0,0 / 0,0,100,0 / 0,0,0,100`, es decir, 12 respuestas).
- **Ningún paso muestra un negativo (L-8):** en las 3 versiones no canónicas, los pasos y la rapidez son positivos. Los negativos solo aparecen como razón falsa en la canónica.
- **Arsenal del paso 10:** la coherencia matemática marca «APROBADO (0 errores)» y la literalidad de `exextra` marca «TOTAL fallos: 0». El paso 9 salió con exit 0 en stresstest, diversidad, diagnosticidad y multisemilla. No volví a ejecutar nada de eso.

**OK**

## Coherencia 4: Código

- **Alcance:** no hay gráfico, así que R, Python y TikZ no aplican.
- **Guardas:** el código R lleva `stopifnot` por cada invariante local.
  - L-1: exactamente un paso está mal y ninguna cuenta falla.
  - L-2: divisibilidad.
  - L-3: textos literales de la canónica.
  - L-4: `repara()`.
  - L-7: la primera hora va antes de la primera distancia.
  - L-8: sin negativos.
  - L-9: razón nueva obligada.
  - Regla #13: exactamente una razón verdadera y es la clave.
- **`exsolution`:** se deriva de `sol`, que sale de `es_verdadera`, función pura sobre los datos de la versión.
- **Chunks:** no vi comillas invertidas dentro de ellos; la valla de código se arma con `intToUtf8(96L)`. Los `−` van como `\u2212` en las cadenas de verificación y no como carácter literal. No lo comprobé con grep sobre los 578 renglones.
- **Pool de errores (regla #1):** tiene 14 entradas con `precondicion` y `calcula`. `calcula()` hace `stop()` fuera de contexto.

**OK**

## Coherencia 5: General

- **Cuatro formatos:** el HTML, el DOCX, el Moodle y el NOPS están presentes y con la fecha de la última pasada. El DOCX trae 37 fórmulas OMML y el HTML las fórmulas en MathML. El log de render solo trae avisos de opciones de pandoc obsoletas (`--mathml`, `--mathjax`).
- **Auditoría de HTML con 100 versiones** (`auditoria_visual_r4.md`, md5 coincidente):
  - 0 desbordes a 360 y 320 px.
  - 4 opciones distintas y una sola «Correcta.» por versión.
  - Cero `$`, `\text` o `NA` en el texto visible.
- **`exextra`:** DOK 2, Bloom Evaluar, SOLO Relacional, Nivel 3.
  - `DOK ≥ 3 ⇒ Nivel ≥ 3`: no se aplica, porque DOK es 2. Nivel 3 con DOK 2 es lo permitido, ya que la recíproca es falsa.
  - Bloom Evaluar es coherente con la tarea de juzgar un procedimiento ajeno.
  - Competencia Argumentación y Nivel 3 coinciden con `ITEM_OFICIAL.md`.
  - Componente Numérico-variacional coincide con la decisión confirmada.
  - La literalidad contra el catálogo está validada por el log del paso 10.
- **Metadatos:** están todos los obligatorios (`exname`, `extype`, `exsolution`, `exshuffle`, `extol` y `exextra`).

**OK**

## Observaciones menores, sin bloqueo

1. **Evidencia de aula:** el Nivel 3 sigue sin evidencia de aula. Lo declara el propio `.claude/CLAUDE.md`. No es un fallo de coherencia.
2. **Redacción de EV-C-CIFRAS:** en la Solution del PDF dice «80 km figura como distancia y 5 h como tiempo: esa escritura les cambia la magnitud a las dos cifras». La frase es correcta, pero «esa escritura» se refiere a la razón falsa y un lector rápido puede leerla como si fuera el paso 3 del procedimiento. Es un tema de claridad.
3. **Canal de longitud:** la cifra del canal de longitud (+8,5 pp en la muestra) está declarada como «BLOQUEA, cubierta por O-1/O-2», pendiente de firma en el paso 11. Lo ya firmado en O-1 no aplica; vale lo que dice O-2.
4. **Espaciado en el PDF de la Solution:** hay un espacio algo más ancho tras «p. m.» en «las 3:00 p. m. El tiempo…». Es tipográfico, sin impacto en el contenido.

## Archivos relevantes

- `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/rapidez_media_error_paso_numerico_variacional_argumentacion_n3_schoice_v1.Rmd`
- `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/ITEM_OFICIAL.md`
- `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/originales/orig_pregunta_09.png`
- `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/salida/auditoria_visual_r4.md`
