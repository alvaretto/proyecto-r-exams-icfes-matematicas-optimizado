# Paso 6b, ronda 4 — auditoría visual HTML (`auditor-visual-html`, lanzado sin `name:`)

- md5 del `.Rmd`: `85773027d040e2ce4b45eda03afa362c` (comprobado por el auditor).
- N = 24 versiones (Familia B de la regla #23, cifra declarada), capturas a 320, 360 y 1024 px con chromium headless.
  Capturas y medición en `salida/auditoria_visual_r4/`. Leídas a ojo: `contact_320.png`, `v02_320.png`, `v13_360.png`,
  `v24_320.png`, `v01_1024.png`; las demás, con comprobaciones programáticas sobre las 24.
- Hallazgos: CRÍTICO, ALTO y MEDIO, ninguno. BAJO: encabezados «Exam 1», «Question», «Solution» en inglés, de la plantilla
  de `exams2html` (el aula usa Moodle).
- Sin fugas de markup (`<span` visible, `\dfrac`, `$`, `NA`), sin math sin renderizar, sin opciones duplicadas, sin mojibake.
- Sin desborde horizontal: `scrollWidth` 305 a 320 px y 345 a 360 px.
- Separación vertical entre opciones consecutivas (`getBoundingClientRect`, 3 huecos × 24 versiones por viewport): mínimo
  9,6 px a 320 y a 360 px, igual al de la ronda 3; sin solapamiento ni huecos de 0 px.
- Veredicto: **APTO_VISUAL**.
