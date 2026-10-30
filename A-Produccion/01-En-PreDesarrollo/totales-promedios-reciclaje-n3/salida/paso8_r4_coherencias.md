**Veredicto: COHERENCIAS_CON_OBSERVACIONES.** No hay ningún defecto de corrección ni de visibilidad. Las observaciones son código muerto de gravedad BAJA (coherencia 4) y tres notas informativas.

**Qué medí.** Archivo: md5 `7d5855dad5b9d6cee745b823c27b835f`, copiado a un directorio temporal. No toqué nada del repo. Directorio de trabajo: `/tmp/claude-1000/r4Qajf`, con el oráculo en `oracle.py` y los adaptadores `run_html.py`, `run_pdf.py`, `run_docx.py`, `run_moodle.py`.

El oráculo lee lo que se muestra: texto de las opciones, celdas de la tabla y Solution. No usa las variables internas. Para cada versión:
- Calcula los totales desde la tabla.
- Evalúa cada opción: sede que declara ganadora, perfil de cursos y promedio, y cifra citada.
- Exige exactamente una opción verdadera, y que sea la marcada.
- Verifica cada ecuación y cada frase de la Solution y del feedback por opción.
- Busca markup visible.

Lo validé con 6 mutantes (clave movida, celda alterada, D cambiada en la Solution, feedback rotado, cifra de opción alterada, material del encabezado cambiado). Los detectó todos.

| Formato | Versiones (canónicas) | Fallos |
|---|---|---|
| HTML (`exams2html`) | 130 (14) | 0 |
| PDF | 46 (6): 24 en un PDF de 55 pp. y 22 PDF sueltos (66 pp. unidos) | 0 |
| DOCX (`exams2pandoc type="docx"`, extraído con pandoc) | 12 (2) | 0 |
| Moodle XML (`exams2moodle`) | 24 (1) | 0 |

Las 14 canónicas en HTML son el 10,8 %, de acuerdo con L-5. Entre las 116 no canónicas, la rama CURSOS salió 54 veces y PROMEDIO 62. Con las canónicas, CURSOS es el 52,3 %.

El HTML cubrió las 8 plantillas, `sede` y `jornada`, los 3 materiales, los 6 conjuntos (con «El Bosque» y «La Pradera» con espacio duro) y la clave repartida entre las 4 posiciones (33/28/34/35).

Los PDF tienen 0 encabezados en negrita huérfanos al pie de página y ningún texto fuera del margen (xMax 516,6 de 595,3 pt). Ni los PDF ni el resto de formatos muestran el `~` del espacio duro. Revisé visualmente una página PDF, una página DOCX (vía LibreOffice) y un HTML a 360 px.

`pdftotext` extrae mal los PDF: `«` `»` `—` `¿` aparecen como caracteres de control o como `¾`, y se pierden `fi` y `fl`. Los restauré en el adaptador y confirmé en la imagen que los glifos sí se ven.

**Tabla por coherencia (N medido, HTML; los demás formatos con 0 fallos en las mismas verificaciones)**

| Coherencia | ¿Cumple? | Evidencia | Chequeos HTML |
|---|---|---|---|
| 1 Semántica | Sí | Las 8 plantillas de enunciado traen la misma sede, material y criterio que el encabezado y la Solution. Las sedes van en el orden de la tabla. El reparto es 2+2 y una sola opción de G cita D. | 1216 |
| 2 Visual-texto | Sí | Tabla de 3 columnas y 2 filas en los 4 formatos. Cada celda tiene su `<span>` con relleno en HTML y Moodle. No hay `**`, `$`, `\times`, `:::`, `{=latex}`, `\nobreak`, `<span` ni `{width` visibles. En HTML y Moodle el `\nobreak` desaparece. | 1286 |
| 3 Matemática | Sí | Con N = 130 y 14 canónicas: exactamente una opción verdadera y es la de `exsolution`. Totales, «Ganó G, con TG − TP = D» y la frase de los dos y dos son correctos. Los 3 análisis de distractores y el feedback de cada opción son verdaderos para su versión. La cifra de cada distractor sale de la tabla. Ningún perfil declarado es falso. Con la lectura alternativa «el total fue de N kilos» no cambia ningún veredicto en las no canónicas. | 4030 |
| 4 Código | Sí, con código muerto | No hay `letra_correcta` ni «Opción A-D» en la Solution (regla #19). Los `exextra` coinciden con `ITEM_OFICIAL.md` en las 130 versiones. Las guardas están presentes. Ver los hallazgos BAJA. | 260 (`exextra`) más revisión estática |
| 5 General | Sí | DOK 2 con Nivel 3, sin violar `DOK ≥ 3 ⇒ Nivel ≥ 3`. Las 14 canónicas HTML y las de PDF, DOCX y Moodle reproducen el impreso: enunciado con «recogido» y encabezado con «recolectado»; tabla 5×40 y 4×45; las 4 opciones verbatim, incluidas «sede de secundaria» y «ganó porque»; pregunta verbatim. Sin «1 cursos» ni «participó 2+». Concuerdan sede y jornada con su artículo. | 850 |

**Hallazgos**

*CRÍTICA / ALTA / MEDIA:* Ninguno.

*BAJA (coherencia 4, código muerto), en el `.Rmd`:*
- B1. `contexto_tipo` se asigna en las líneas 355 y 371 y no se lee nunca. Por eso el campo `tipo` de cada plantilla de `contextos` (p. ej. línea 87) tampoco sirve.
- B2. En `verdad()`, la rama `dif_cursos` (línea 196) es inalcanzable: ningún molde declara `afirma = "dif_cursos"`.
- B3. En la rama canónica, `b_debajo <- 1L` y `lado_gdis <- "encima"` (línea 355) no se leen en esa rama. Se leen solo en la no canónica.
- B4. Los 11 moldes de `MOLDES` reciben el parámetro `m` (material) y ninguno lo usa. `afirmacion()` lo pasa igual.

*INFORMATIVAS (sin acción obligatoria):*
- I1. Una versión no canónica puede dar un enunciado idéntico al canónico: conjunto primaria/secundaria, papel y plantilla institucional, aprox. 0,6 % de las versiones (me salió una en las 130; sus datos eran 7×30 y 6×43). No confunde: la tabla trae «recogido», no «recolectado», y los datos son distintos.
- I2. A 360 px la tabla muestra la barra de desplazamiento horizontal propia del `div` con `overflow-x:auto`. No se corta nada.
- I3. Los residuos declarados siguen ahí: la clave canónica verbatim admite la lectura «el total fue de 20 kilos» (O-1, regla #13 v1.2), y los canales §P7 de O-1. Esta ronda no los midió.

**Alcance no medido.** No remedí la diagnosticidad §P7 ni la enumeración exacta del espacio; es lo que cubren `verificar_espacio.R` y el paso 9. Tampoco rendericé NOPS. Para PDF y DOCX usé 46 y 12 versiones, pasando por extracción de texto. Para PDF la regla #23 pide declarar el N de los renders reales: son 46 PDF y 12 DOCX.
