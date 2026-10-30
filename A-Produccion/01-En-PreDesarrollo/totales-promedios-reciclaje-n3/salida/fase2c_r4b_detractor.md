# Auditoría del detractor, FASE 2C, ronda 4b: `totales-promedios-reciclaje-n3` (SCHOICE)

**Fecha:** 2026-10-29
**Artefacto:** `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd`, md5 `e293d5c812b1f9790e2bb777b62de9a5`. Lo comprobé con `md5sum`.
**Base de comparación:** `git show HEAD:` de ese archivo. Comprobé que su md5 es `cd94b59adfdea7b9c92dc074ce0a1435`, la versión aprobada.
**Independencia:** no escribí ni corregí este artefacto y no audité ninguna ronda anterior.
**Objeciones:** 4 en total: 0 de CORRECCIÓN, 0 críticas, 0 altas, 1 media y 3 bajas.

## Resumen

El cambio de la ronda 4b hace lo que dice, y lo medí por mi cuenta. Las cabeceras quedan separadas por al menos 13,5 px; en la versión aprobada eran 6,0 px. A 360 y a 320 px la tabla no deja de caber en más versiones que en la aprobada. No encontré fugas de marcado en ninguno de los cinco formatos. El texto visible de la Question es idéntico al de la versión aprobada en 100 de 100 versiones pareadas, y entre ellas hay 9 canónicas.

Desde la versión aprobada, la Solution solo cambia en la causa raíz y en `explica()` de ERR-TOTAL. Las dos afirmaciones son verdaderas: lo comprobé con el álgebra y en las 22 de 100 versiones donde aparecen. Las opciones y `exsolution` no cambian, así que §P7 y O-1 quedan igual.

La corrección del encabezado huérfano funciona y sostiene el resultado:

| Versión | Encabezados huérfanos | Saltos de página |
|---|---|---|
| r4b | 0 | 229 |
| r3, la aprobada | 42 | 230 |
| Mutante que quita el bloque | 51 | 230 |

Los cuatro hallazgos son deuda y testing. Ninguno toca la clave, las opciones ni la Solution.

## Lo que medí (pregunta 1: el cambio de la ronda 4b)

**Método.** Rendericé todo en un directorio temporal fuera del repo:
`/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/35e93ef1-04b2-4e1e-8b49-a10ac14fa449/scratchpad/det4b.kZXD/`

- **HTML:** `set.seed(s); exams2html(..., n = 1)` con s = 1..100, para r3 y r4b. Son 200 renders: N = 100 por versión, pareados por semilla.
- **Medición en Chromium:** headless, cada versión en un iframe sin barra de desplazamiento de 360 y de 320 px. Mido la caja `overflow-x` (304 y 264 px), el `scrollWidth`, el ancho mínimo de la tabla (min-content, que no depende de la caja), la separación entre los rectángulos de glifo de las cabeceras que comparten renglón, y el `innerText` en busca de fugas.

**1a. ¿Separa las cabeceras en HTML y Moodle? Sí.**

| Ancho | r3, separación mínima | r4b, separación mínima |
|---|---|---|
| 360 px | 7,2 / 7,0 px | 13,8 / 13,5 px |
| 320 px | 6,0 / 9,5 px | 16,3 / 15,5 px |

- Las cabeceras de r4b se dibujan a 14,4 px.
- Las celdas del cuerpo pasan de 2 renglones como máximo en r3 («El Bosque» partido) a 1 renglón.
- Moodle (`exams2moodle`, n = 100): las 100 preguntas llevan el `<span>` de cabecera con `padding-left:0.5em; font-size:0.9em`.

**1b. ¿La tabla deja de caber en su caja en más versiones que la aprobada? No, ni a 360 ni a 320 px.**

- En mi arnés: 0 de 100 en r3 y 0 de 100 en r4b, a los dos anchos. Pareado por semilla: ninguna versión desborda solo en r4b.
- Contra las cajas de la medición entregada (289 y 249 px, que incluyen 15 px de barra): con el ancho mínimo, 0 contra 0 a 289 px y 9 contra 9 a 249 px. Coincide con lo que se entregó.
- Matiz: ver H2. Con cajas de menos de 240 px, r4b desborda en más versiones (49 contra 37 a 230 px).

**1c. Fugas visibles. Ninguna.**

| Formato | Muestra | Fugas |
|---|---|---|
| HTML (`innerText`) | 100 | 0 |
| Moodle (CDATA sin etiquetas) | 100 preguntas | 0 |
| PDF | 1 de 100 ítems + 30 sueltos | 0 |
| NOPS | 10 ítems | 0 |
| DOCX | 20 | 0 |

Busqué `<span`, `font-size`, `padding`, `{=latex}`, `\nobreak`, `\makeatletter`, `\@afterheading`, `~`, `**` e `inline-block`. El espacio duro llega como U+00A0 a Moodle (14) y a DOCX (8), y en el PDF «El Bosque» sale sin `~`.

**1d. ¿Cambia algún texto visible? No.**

- Comparé la Question de r3 y de r4b, quitando antes los `<span>` y pasando U+00A0 a espacio: es idéntica en 100 de 100 versiones, incluidas las 9 canónicas de la muestra.
- Lo único visual que cambia es el tamaño de las cabeceras en HTML y Moodle (90 %). La regla #24 H-2 no se ve afectada, porque ningún texto de la canónica cambia.

## Lo cambiado desde la versión aprobada (pregunta 2)

- **Afirmaciones nuevas (regla #13 v1.2).** El diff de la Solution entre r3 y r4b, sobre las 100 semillas, solo muestra dos cosas:
  - La causa raíz de ERR-TOTAL, en 22 de 100: «calcula un total, pero no le resta el del otro grupo». Es verdadera.
  - `explica()` de ERR-TOTAL: «La cifra T_G es el total de G: n_G × a_G = T_G kilos. No es la diferencia: falta restarle el total de P, y T_G − T_P = D kilos.» Es verdadera, porque x = T_G ≠ D cuando T_P > 0.
  - El resto (la retroalimentación por opción y las demás `explica()`) no cambia. Rehíce el álgebra de ERR-PARCIAL y ERR-PROM en las dos ramas: D = k·a_G − n_P·δ (rama CURSOS) y D = n_G·δ − k·a_P (rama PROMEDIO). Es correcta.
- **Opciones y diagnosticidad (§P7).** Las opciones y la clave son idénticas en las 100 semillas pareadas, así que §P7 no puede cambiar.
  - Comprobé que `FIRMA_O1` de `verificar_espacio.R` coincide celda a celda con la tabla firmada del `.claude/CLAUDE.md` local (71,0/55,0, 60,5/62,0, 79,0/67,0, 51,5/58,0 y 81,5/74,5).
  - `salida/verificar_espacio_ronda4b.txt` da `APROBADO (0 fallos)`. El canal de largo en la rama PROMEDIO (+10,4 pp, margen del 2,0 %) es el residuo §P7-B ya declarado.
- **Guardas.**
  - L-1, L-2, L-3 y L-5 y la regla #13 v1.1 siguen en `data_generation`, sin tocar.
  - El mutante M20 (`x, "</span>")`) sigue encontrando su objetivo en la línea 53 del `.Rmd` vigente, y sale OK.
  - M19 sale OK: se sale de lo firmado en 3 de 10 cifras.
  - `celdas_md()` elimina el `<span>` nuevo con su `font-size`, y la sonda H3 r4 sobre la canónica da OK.
  - Se borró `cursos_txt`; `grep` no encuentra ninguna referencia.
- **Encabezado huérfano al pie del PDF.** Corregido y portante: lo muestra la tabla del resumen. En los 30 PDF sueltos de r4b hay 0 huérfanos. El detector cubre los 10 tipos de encabezado: 1000 de 1000 detectados. Lo que falta es una guarda ejecutable: ver H1.

## Los 8 dominios (pregunta 3)

- **Código R/exams:** metadatos completos, `exshuffle: TRUE` con opciones de texto, F1, F5 y la guarda `\newcounter{none}` presentes. Sin hallazgos nuevos.
- **Pedagógico:** pool de 7 errores con `precondicion` y `calcula`, Solution con las 6 secciones y pool de reflexiones. Sin hallazgos.
- **Visual:** ver la pregunta 1 y H2.
- **Gramática:**
  - El texto nuevo lleva tildes.
  - La concordancia es correcta: «participó 1 curso» frente a «participaron k cursos», «aporta» frente a «aportan», y «esa sede» o «esa jornada».
- **Coherencia matemática:** ver la pregunta 2.
- **ICFES metacognitivo:** `exextra` coincide con L-4. DOK 2 con Nivel 3 es válido, porque solo rige `DOK ≥ 3 ⇒ Nivel ≥ 3`.
- **Testing:** H1.
- **Semántico (Nivel 4):** las `descripcion_*` y la causa raíz nueva de ERR-TOTAL describen lo que hace `calcula()`.
- **Overrides e invariantes locales (v1.5):** cotejé L-1, el impreso y `ITEM_OFICIAL.md` contra el recorte `orig_pregunta_50.png`: el texto, la tabla y las cuatro opciones coinciden. O-1 corregido está cableado tal como se firmó.
- **Arsenal post-render:** 0 errores y 7 advertencias genéricas que ya existían en la versión aprobada («Tarea», «letra_correcta», el patrón «Es posible que…»). No dependen del cambio.

## Hallazgos

| ID | Severidad | Hallazgo | Evidencia | Propuesta |
|---|---|---|---|---|
| H1 | BAJA · TESTING | La corrección del encabezado huérfano (`encabezado()` con `\nobreak` y `\@afterheading`) no tiene guarda ejecutable: `verificar_espacio.R` no la mira y el arsenal tampoco. Hoy funciona, pero si alguien revierte `encabezado()` en una ronda futura, nada falla. | Mi mutante, que deja solo la negrita, produce 51 huérfanos en 230 saltos de página; r4b, 0 en 229. `grep` de `afterheading` y `nobreak` en `verificar_espacio.R`: 0 coincidencias. | Añadir a `verificar_espacio.R` una sonda estructural sobre la Solution (todo encabezado en negrita va seguido del bloque `{=latex}` con `\nobreak`) y un mutante que quite el bloque. Alternativa más cara: `exams2pdf(rep(rmd, 100))` y comprobar con `pdftotext` el último renglón de cada página. |
| H2 | BAJA · VISUAL | El espacio duro de «El Bosque» y «La Pradera» (ronda 4, dentro del cambio desde la aprobada) sube el ancho mínimo de la tabla en las 12 de 100 versiones de ese conjunto, hasta +21,4 px (s006: de 217 a 238,4 px). Con cajas de 240 px o más, el desborde es igual al de r3 (9 contra 9 a 249 px; 0 contra 0 desde 260 px). Con cajas más estrechas empeora: a 230 px, 49 contra 37. El comentario «9/100 a 320 px, igual que sin relleno» solo vale para la caja del arnés de `exams2html`. No hay desborde de página, porque la caja tiene `overflow-x:auto`. | Medición pareada del ancho mínimo con N = 100 (`m_r3_360.json` y `m_r4b_360.json` en el directorio temporal). | Medir el ancho real de `.qtext` en el Moodle del colegio a 320 px. Si baja de 240 px, aceptar el desplazamiento horizontal o quitar el espacio duro solo en «La Pradera». Si no baja, documentar el umbral en el comentario. |
| H3 | BAJA · PROCESO | `HANDOFF.md` sigue declarando vigente el md5 aprobado y no menciona las rondas 4 ni 4b. Sigue abierto desde el H4 de la FASE 2C de la ronda 4. Quien lea solo el HANDOFF, como manda la memoria del proyecto, cree que el archivo vigente es el aprobado. | Línea 291: «md5 `cd94b59a…`, el vigente.»; `e293d5c8` no aparece en el archivo. | Añadir al HANDOFF la sección de las rondas 4 y 4b: md5 vigente, pasos des-sellados y que falta la nueva aprobación del profesor. |
| H4 | MEDIA · DEUDA (regla #21, F2) | La separación de cabeceras corrige un defecto de la familia F2, pero solo en la copia local. `snippets_familias_rmd.R` no tiene `padding-left` ni `inline-block`, así que los demás ejercicios que usan F2 conservan las cabeceras a 4-6 px. Sigue abierto desde el H2 de la ronda 4; está declarado en el comentario, no resuelto. | `grep -c "padding-left\|inline-block"` en `.claude/scripts/snippets_familias_rmd.R` da 0, sin cambios desde el 2026-10-22. | Llevar la variante F05 (cabecera al 90 %, 0,5 em a la izquierda desde la columna 2) a `tabla_responsiva()` canónica, como parámetro con ese valor por defecto, con su caso en `test_tabla_responsiva_pdf.R` y la medición en 100 versiones como evidencia. No bloquea este ejercicio. |

## Veredicto

Ningún hallazgo es de CORRECCIÓN, crítico ni alto, así que nada bloquea la FASE 3. El cambio de la ronda 4b cumple los cuatro puntos pedidos, y lo cambiado desde la versión aprobada no introduce afirmaciones falsas, no cambia las opciones ni la diagnosticidad y no rompe ninguna guarda. H1 a H4 son deuda y testing. La FASE 2C queda en condiciones de sellarse con estos residuos anotados, pero la versión vigente necesita una nueva aprobación del profesor (el paso 11 está des-sellado).

## Próximos pasos

1. H3: actualizar `HANDOFF.md` antes de la WAIT_USER de aprobación.
2. H1: añadir la sonda y el mutante del encabezado a `verificar_espacio.R`.
3. H2: medir la caja real de `.qtext` en el Moodle del colegio a 320 px.
4. H4: llevar la variante a la F2 canónica, como tarea del repo y no de este ejercicio.

Todos los renders y las mediciones están en `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/35e93ef1-04b2-4e1e-8b49-a10ac14fa449/scratchpad/det4b.kZXD/`:
- Renders: `html/`, `pdf/`, `pdf1/`, `nops/`, `docx/` y `moodle/`.
- Mediciones y scripts: `m_*.json`, `harness.py`, `orf.py`, `cmp.py` y `src/mut_sin_valla.Rmd`.

No modifiqué ningún archivo del repo.

CITAS_DETRACTOR
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd:591 | encabezado <- function(t) cat(
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/verificar_espacio.R | AUSENTE: afterheading
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd:84 | espacio duro en las celdas, para que a 360 px
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd:34 | 0/100 a 360 px y 9/100 a
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/HANDOFF.md:291 | md5 `cd94b59a…`, el vigente.
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/HANDOFF.md | AUSENTE: e293d5c8
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd:31 | Variante local: la función de snippets_familias_rmd.R no lo tiene.
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/scripts/snippets_familias_rmd.R | AUSENTE: padding-left
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS
