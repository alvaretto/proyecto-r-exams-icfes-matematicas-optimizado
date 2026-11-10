# HANDOFF — `diagrama-venn-juegos-mesa-n3` (SCHOICE, MAT-2026-07-014)

**Estado, 2026-11-06:** 11 de 11 pasos sellados. **Aprobado para llevar al aula.** La promoción espera la evidencia de aula. `.Rmd` con md5 `f886dfaa45b80a5ee16f9820908049b5`; `verificar_espacio.R` con md5 `d7271f811e27f9b12766ead32280fa6d`. El CLOZE hermano está en `Cloze/`: 11/11, aprobado el 2026-11-09 (ver `Cloze/HANDOFF.md`).
**YAML de RStudio, resuelto el 2026-11-09:** el 2026-11-06 a las 20:26:11 un Knit de RStudio le añadió al `.Rmd` 6 líneas de YAML (`output: word_document, html_document, pdf_document`) y su md5 pasó a `2608498b77365a9ac2da8f1242b3df0e`; el 2026-11-07 a las 09:09 se volvió a guardar igual. El 2026-11-09, por decisión que el profesor delegó en el coordinador («Decide tú lo mejor a ojos de un perfecto Saber ICFES»), se quitaron esas 6 líneas: el cuerpo desde la línea 7 tenía el md5 firmado, y el `.Rmd` vuelve a tener `f886dfaa…`. No usar Knit sobre este `.Rmd`: para ver el ítem, el semillero. Los PNG, `.html`, `.pdf` y `.docx` de la raíz con hora 20:24-20:26 del 2026-11-06 son de esos Knit y no van al commit.
**Firma del profesor, transmitida por el coordinador:** «firmo tal cual, mantengo I-3, a». Registro con las glosas que leyó: `salida/tabla_firma_p7.md`. Vale para ese `.Rmd`: si cambia, queda sin efecto.
**FASE 2C cerrada:** la ronda 3, con un detractor nuevo sobre ese `.Rmd`, dio APROBAR_CON_CAMBIOS, sin defectos de CORRECCIÓN. La ronda 4 auditó el cableado de la firma, las sondas H17 a H21 y los archivos de uso: APROBAR_CON_CAMBIOS, sin pedir cambios al `.Rmd`.

## Pendiente

1. **Evidencia de aula.** Sin ella no se promueve. Hay que observar si los estudiantes usan el atajo de `EST-menos-cifras-del-enunciado`: si lo usan, se aplica el filtro de `salida/tabla_firma_p7.md`, y eso cambia el `.Rmd`, así que exige otro detractor y otra firma.
2. **Prueba real de impresión y escaneo en hojas oficio**, antes de imprimir un lote NOPS.
3. **FASE 2C-bis antes de promover.** El 2026-11-06 ningún motor entregó: deepseek, ollama-cloud y codex saturados o sin saldo, y el local sin marcador. Reintentar con `bash .claude/scripts/detractor-hetero.sh <dir>`.

## Alcance firmado de §P7: variantes (techo nulo 32,2 %)

| Regla | N = 100 | Exacta (11 000 000) | Exceso sobre el techo de N = 100 |
|---|---|---|---|
| `MAG-suma-segunda-menor` | 100,0 % | 100,00 % | +67,8 pp |
| `EST-menos-cifras-del-enunciado` | 65,3 % | 69,55 % | +37,4 pp |
| `MAG-menor-maximo` | 48,0 % | 53,03 % | +20,8 pp |
| `MAG-suma-no-extrema` | 50,0 % | 50,00 % | +17,8 pp |
| `MAG-sin-el-mayor-numero` | 50,0 % | 50,00 % | +17,8 pp |
| `REL-no-extrema-en-coincidencias` | 50,0 % | 50,00 % | +17,8 pp |
| `EST-primer-dato-en-la-zona-doble` | 50,0 % | 50,00 % | +17,8 pp |
| `EST-total-en-zona-de-un-conjunto` | 50,0 % | 49,46 % | +17,3 pp |

Las exactas son de la enumeración de la FASE 2C r2 (`salida/fase2c_r2_detractor.md`), contrastadas con el código real de las reglas sobre 20 000 versiones. `EST-menos-cifras-del-enunciado` da 0,0 % en el impreso: las variantes quedan más expuestas que el original en esa regla.

**Canónica, 24 órdenes, techo nulo 38,5 %.** +61,5 pp: `MAG-suma-segunda-menor`, `MAG-menor-maximo`, `FOR-menos-dos-cifras`, `FOR-cadena-corta`, `LEX-con-repetidos`. +11,5 pp: `MAG-suma-no-extrema`, `MAG-sin-el-mayor-numero`, `MAG-centro-mayor`, `DIV-suma-par`, `REL-no-extrema-en-coincidencias`, `EST-total-en-zona-de-un-conjunto`, `EST-primer-dato-en-la-zona-doble`.

El orden A < clave < B < D de las sumas es algebraico, en las 11 000 000 versiones y en el impreso: estructural (§P7-F), no se rediseña. Márgenes medidos de las reglas MAG: `salida/tabla_firma_p7.md`.

## Condiciones de uso

`SemilleroUnico_v2.R`, en esta carpeta, genera PDF, DOCX, Moodle, NOPS y un cuestionario interactivo con estas condiciones, 10 preguntas por versión. Escribe en `salida/semillero/`. El 2026-11-07 a las 09:10 se había reemplazado por la copia genérica de `perifericos/`, que imprime con la plantilla compartida, deja el NOPS a una columna y no lo centra en oficio. El 2026-11-09 se restauró la versión del 2026-11-06, con las 10 preguntas y el cuestionario de esa copia y un tope de 100 en `copias`. Corrió completo el 2026-11-09 con exit 0, en 74 s y sin escribir fuera de `salida/`:
- PDF de 31 páginas de 216 × 330 mm, con las cuatro figuras de cada pregunta en una página (p2:4 p3:1 … p29:4 p30:1).
- NOPS de 12 páginas A4, con 4 figuras por página de p3 a p12, y su versión `_oficio`, de 216 × 330 mm.
- Moodle con 1 versión (semilla 1). Cuestionario `WEBQUIZ_OK` sin red: 175 fórmulas compuestas y 0 recursos externos.

Vistas: la página 2 del PDF y la 3 del NOPS en oficio, con la clave recalculada desde las viñetas. `SemilleroCloze.R` y `SemilleroMoodle_v2.R` de esta carpeta son copias de `perifericos/` que apuntan a otros ejercicios (`totales_promedios…` y `probabilidad_fichas…`, con 250 copias): no son de este ítem y no van al commit.

- **PDF:** plantilla local `solpcielo_una_por_pagina.tex`, copia de `A-Produccion/perifericos/solpcielo.tex` con un `\clearpage` por pregunta. Rehecha el 2026-11-09 desde la versión oficial en disco (regla #26), igual que las copias locales `pcielo.tex`, `pcielo_nosol.tex` y `solpcielo.tex`. Hoja de 216 × 330 mm y figuras de 5,5 cm. Con cinco preguntas, cada una deja sus cuatro opciones en una página (p2:4 p3:1 p5:4 p6:1 p8:4 p9:1 p11:4 p12:1 p14:4 p15:1, contando la figura de la Solution), en 3 semillas y en el semillero. Con una sola pregunta al inicio: 100 de 100, base de `d)` entre 289,8 y 290,5 mm, límite 310.
- **NOPS:** `exams2nops` fija A4. El semillero usa `twocolumn = TRUE, samepage = TRUE, header = "\\raggedbottom"`: 4 figuras por página en las cinco preguntas, sin hueco entre el enunciado y las viñetas. Con `duplex = TRUE` la página 2 sale en blanco a propósito, para impresión a doble cara; `duplex = FALSE` la quita (6 páginas en vez de 7).
- **Imprimir NOPS en oficio:** usar `*_nops_1_oficio.pdf`, que `pdfjam` centra sin escalar en 216 × 330 mm, a tamaño real.
- **Cuestionario interactivo:** `*_interactivo.html`, para celulares sin internet. Lo genera `.claude/scripts/webquiz_autocontenido.R`, que incrusta MathJax y lo comprueba en un navegador sin red. `exams2webquiz()` solo, como en la copia genérica, carga MathJax desde internet y sin red deja las fórmulas en TeX crudo.
- **Escanear NOPS:** simulado el 2026-11-06 con hoja marcada a mano en imagen (identificador, registro y tres respuestas), no con papel real. Directo: leídas A4, centrada y ajustada en 216 × 330 mm; error en la esquina de 216 × 330 y en 216 × 356. Con `Auxiliares/exams2nops-escaner/normalizar_escaneos_nops.py`: las 8 colocaciones probadas leídas sin error, incluidas la esquina, 216 × 356, un giro de 0,3° y un escaneo más claro, y las marcas naranja, verde claro, amarilla y azul. Detalle en `salida/escaneo_simulado/lecturas.md`. Falta la prueba con papel, escáner real y `nops_control_calidad.R`. Si el normalizador da `FALLA`, volver a escanear la hoja.

## Límites declarados

- **`exams2pdf` con `solpcielo.tex` compartida:** las preguntas que empiezan a media página parten sus opciones. Multicopia de 3: p2:4 p3:1 p4:3 p5:2 p6:2 p7:3. Por eso el semillero usa la plantilla local.
- **DOCX:** 5,5 cm, pero las opciones no quedan juntas y ninguna sonda lo mira. Probado también con hoja oficio y márgenes de `solpcielo` en un `--reference-doc`: siguen partidas (2 + 2). No usarlo para imprimir este ítem.
- **Moodle:** en el servidor icfes (Moodle 5.1.7) `forceclean` es 0, y `format_text` con `noclean`, que es lo que usan las preguntas, conserva el `style` intacto. Probado por CLI, sin escribir en el servidor. Si `forceclean` se activara, el purificador deja solo `width:5.5cm`: unos 208 px fijos. Sin probar: una pregunta real en el navegador.
- **`.Rmd`:** la pregunta no está en el `stopifnot` de I-2. La guarda vive en `verificar_espacio.R`. Si el `.Rmd` se vuelve a tocar, añadirla ahí y lanzar otro detractor.
- **Figura contra el recorte:** marco de 0,86 % del ancho frente a 0,97-0,99 %, círculos de 0,52 % frente a 0,54-0,58 %, y dígitos algo más grandes y negros. Corregirlo cambia el `.Rmd` y anula la firma; TikZ dio 100/100 en el comparador.
- **Hook:** 2 avisos de FASE 2F, de plantilla antigua: falta el encabezado «Información de la Pregunta» y no existe `letra_correcta`, que la regla #19 no exige. Quitarlos también cambiaría el `.Rmd`.
- **`pcielo_nosol.tex`:** no es de este ítem. `exams2pandoc` falla también con `swisscapital.Rmd` del paquete, y `pcielo.tex` funciona con los dos. Tres reparaciones probadas en copia no funcionaron, así que la plantilla sigue igual. Ningún semillero la usa.

## Verificador

`Rscript --no-init-file verificar_espacio.R todo` tarda unos 8 min. **Exit 4 = rojo cubierto por la firma, solo en modo `todo`: no es PASS, y 0 no ocurre con este ítem.** Exit 1 = alguna sonda en rojo. Exit 3 = sin cubrir: `FIRMADA` en FALSE, otro md5 del `.Rmd`, una regla sobre +8 pp fuera de su lista, una regla por encima de su cifra, una cifra firmada que no se reproduce, alcance sin evaluar o un modo distinto de `todo`. Las listas son dos, regla por regla, sin tope por familia ni por estrato, y se comparan con las cifras exactas de `salida/firma_p7/exacto.py`, que se invoca con `python3 -I` y tarda alrededor de un minuto. También da exit 3 si `exacto.py` cambia de md5, si Python falla, si la lista de reglas no son las 34 de la batería o si falta alguna de las 10 secciones. La sección `uso` genera PDF y NOPS de 5 preguntas con los argumentos del semillero. Último registro: `salida/verificar_espacio_20261109.txt` (2026-11-09, 7 min), en esta carpeta y con la plantilla oficial de `perifericos/` en disco: exit 4, sin rojos, 0 pendientes y 113 mutantes muertos por su sonda. Ese mismo día, antes de que el profesor declarara oficial la copia en disco (regla #26), se corrió en un espejo con la plantilla de git (`salida/verificar_espacio_espejo_20261109.txt`, el mismo resultado): la copia en disco aún no tenía `\pandocbounded` ni el contador `none`, y `USO_PLANTILLA`, que compara con ella, daba rojo aquí. Con las definiciones repuestas en la oficial y la plantilla local rehecha desde ella, da verde. El registro del 2026-11-06, `salida/verificar_espacio_firma.txt`, se corrió en espejo por el YAML. Esta versión del verificador no la ha auditado ningún detractor.

Si cambia una plantilla de contexto, hay que actualizar `MD5_CONTEXTOS` después de leerla. Cambiar el ancho de las figuras afecta a `cableado`, `solucion`, `papel` y `moodle`.

## FASE 2C

| Ronda | md5 auditado | Veredicto | Reporte |
|---|---|---|---|
| r1 | `fa00bf48…` | RECHAZAR | `salida/fase2c_r1_detractor.md` |
| r2 | `7a594069…` | APROBAR_CON_CAMBIOS | `salida/fase2c_r2_detractor.md` |
| r3 | `f886dfaa…` | APROBAR_CON_CAMBIOS | `salida/fase2c_r3_detractor.md` |
| r4 | `f886dfaa…`, fuera de alcance; verificador `3d58f455…` y archivos de uso | APROBAR_CON_CAMBIOS | `salida/fase2c_r4_detractor.md` |

Las 3 pasadas de corrección de §P7-D están gastadas.

## Commit: evidencia que `.gitignore` deja fuera

`.gitignore` ignora `**/salida/`, así que para un commit esta evidencia entra con `git add -f`, por nombre. Sin los archivos que lee `verificar_espacio.R`, el verificador no da exit 4:

- **Los lee el verificador:** `salida/firma_p7/exacto.py`, `salida/flujo_b/geom_venn.json`, `salida/flujo_b/tikz/venn_tikz.R`.
- **Firma:** `salida/tabla_firma_p7.md`, `salida/firma_p7/exacto.txt`, `salida/firma_p7/filtro.py`, `salida/firma_p7/filtro.txt`, `salida/firma_p7/margenes.py`, `salida/firma_p7/margenes.txt`.
- **Escaneo simulado:** `salida/escaneo_simulado/a.R`, `salida/escaneo_simulado/leer.R`, `salida/escaneo_simulado/lecturas.md`.
- **FASE 2C:** `salida/fase2c_r1_detractor.md`, `salida/fase2c_r2_detractor.md`, `salida/fase2c_r3_detractor.md`, `salida/fase2c_r4_detractor.md`, `salida/diff_r2_r3_rmd.txt`.
- **Medición:** `salida/verificar_espacio_firma.txt`, `salida/verificar_espacio_espejo_20261109.txt`, `salida/verificar_espacio_20261109.txt`, `salida/paso9_10_r3.txt`.
- **Instrumento del Flujo B:** `salida/flujo_b/SPEC_VENN.md`, `salida/flujo_b/comparar_venn.py`, `salida/flujo_b/instrumento.md5`.

## Archivos

Vigente: `salida/render_r3/` y `salida/preview_*_oficio-*.png`. Obsoletos, no usar como evidencia: `salida/render/`, `salida/render_r2/` (A4), `salida/verificar_espacio_r2.txt`, `salida/verificar_espacio_r3.txt`, `salida/verificar_espacio_final.txt` y `salida/paso9_10_r2.txt`. `salida/parche_pendiente_fase2c.py` quedó superado: no aplicarlo.
