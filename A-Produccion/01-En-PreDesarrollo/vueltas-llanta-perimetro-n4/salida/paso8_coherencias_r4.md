# Paso 8, ronda 4 — 5 coherencias (`AgenteValidadorVisual`, lanzado sin `name:`)

- md5 del `.Rmd`: `85773027d040e2ce4b45eda03afa362c` (comprobado por el agente). Renders: `salida/render_r4/`.
  Archivos de trabajo: `salida/coherencias_r4/`.
- Cobertura: PDF v1 completo como imagen (2 páginas); texto de PDF v1-v5 e imagen de enunciado y opciones de v2-v5;
  4 HTML; 2 DOCX (v2 convertido a PDF); 2 NOPS (texto); 10 preguntas del XML de Moodle, comprobadas con un script.
- Semántica: OK. K concuerda con las unidades en las 10 preguntas de Moodle (cm/m 100; mm/m y m/km 1000); d y la letra
  coinciden entre enunciado, opciones y Solution; la canónica es verbatim; ninguna salida muestra «101/120».
- Visual-texto: OK, con dos observaciones del extractor de texto (`pdftotext` muestra «¾» por «¿» y pierde la ligadura «fi»;
  la imagen del PDF está bien) y de la plantilla (rótulos «Problem»/«Question»; NOPS en A4, sin opciones gráficas).
- Matemática: OK. En las 10 preguntas de Moodle la única opción con `fraction="100"` vale K·d/P; en PDF v1-v5 y HTML
  v1-v2 la marcada es la correcta, emparejada por contenido, y las cifras del feedback y del caso específico son correctas.
- Código: OK. `exextra` coincide con `ITEM_OFICIAL.md` (`validar_literalidad_exextra.py`: 0 fallos); DOK 2 con Nivel 4 es
  admisible; sin letras de opción en la Solution; el análisis recorre los errores en el orden del banco. Observación: la
  Solution muestra los códigos internos VL-A… (no son letras de opción).
- General: OK. Tildes, concordancia llanta/rueda frente a rodillo y contracciones correctas; cada distractor se describe
  de forma coherente con la expresión que se ve.
- Veredicto: **OK_CON_OBSERVACIONES** (ninguna exige corregir).
