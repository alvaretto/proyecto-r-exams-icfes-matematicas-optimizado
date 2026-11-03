# Paso 8 (coherencias_5), tanda S2d: reporte íntegro del AgenteValidadorVisual (sin name), sacado de su transcripción

Las cinco coherencias cumplen. No hay hallazgos CRÍTICA, ALTA ni MEDIA, y la clave está bien en todas las versiones que medí. Anoto tres observaciones BAJA y un residuo ya firmado en O-1.

**md5 del .Rmd:** `63e2713e6ac0bdbf250f4dc33901b2ec` al empezar y al terminar, sin cambios. No escribí fuera de `SUB/salida/ciclo_20261102/s2d/paso8/`. El `git status` de SUB muestra otros archivos modificados o sin seguimiento (`verificar_espacio.R`, `.claude/CLAUDE.md` local, `Semillero*.R`, `*.tex`) que ya estaban así y no son míos.

## N renderizado por formato

| Formato | N pedido | N real | Semilla | Canónicas | Salida |
|---|---|---|---|---|---|
| HTML | 100 | 100 | `set.seed(8001)` único | 7 | `SUB/salida/ciclo_20261102/s2d/paso8/html/h1..h100.html` |
| PDF | ≥20 | 70 (30 con 8002 y 40 con 8011) | 8002 y 8011 | 3 (`pdf/p09`, `pdf/p18`, `pdf2/q20`) | `.../pdf/`, `.../pdf2/` |
| DOCX | ≥10 | 50 (20 con 8003 y 30 con 8012) | 8003 y 8012 | 5 (`docx/d14`, `docx2/e5`, `e21`, `e25`, `e30`) | `.../docx/`, `.../docx2/` |
| Moodle | ≥24 | 40 | 8004 | 4 | `.../moodle/m.xml` |
| NOPS | ≥2 | 2 | 8005 | 0 | `.../nops/n1.pdf`, `n2.pdf`, `n.rds` |

Las 5 corridas dieron exit 0. Los únicos avisos son "Deprecated --mathml/--mathjax" de pandoc. No hay `LaTeX Error` ni `Undefined control sequence`.

NOPS no tiene canónica porque con n=2 y P_CANON=0,10 no salió ninguna. Las canónicas se cubren con HTML, PDF, DOCX y Moodle.

## Las 5 coherencias

| # | Coherencia | Resultado | Evidencia |
|---|---|---|---|
| 1 | Semántica | Cumple | En las 100 versiones HTML hay 2 sedes o jornadas, cursos, promedio por curso y gana el mayor TOTAL. La pregunta es idéntica al impreso. El reparto 2+2 (dos afirmaciones nombran a G, dos a P) se cumple 100/100. No hay dominancia en ninguna versión: 57 son rama PROMEDIO y 43 rama CURSOS, canónicas incluidas. Ninguna opción tiene una segunda lectura razonable que la haga verdadera, salvo las declaradas (ver hallazgos). |
| 2 | Visual-texto | Cumple | La tabla mostrada coincide con las cifras de las opciones, la Solution y el feedback en 100/100 HTML y 40/40 Moodle. Detalle de la canónica abajo. |
| 3 | Matemática | Cumple | En 100/100 HTML, calculando total = cursos × promedio desde la tabla mostrada, exactamente una afirmación es verdadera y es la nombrada en «La respuesta correcta es la que afirma». En Moodle, 40/40 con una sola opción de fraction 100 y esa es la verdadera (4 canónicas). En NOPS (`n.rds`) la solución marcada coincide con la verdadera en las 2 preguntas. D = T_G − T_P coincide con la Solution en 100/100. Las aritméticas «n×a=T» y «T_G−T_P=D» de la Solution aparecen bien en 100/100, y el feedback marca «Verdadero» solo en la opción verdadera, 100/100. |
| 4 | Código | Cumple | Ver el detalle debajo de esta tabla. |
| 5 | General | Cumple | DOK 2, Bloom Evaluar y Nivel 3 son coherentes: Bloom Evaluar ⇒ Nivel ≥ 3 y DOK < 3 no activa la implicación. En 100 HTML, la búsqueda de 25 palabras sin tilde (mas, segun, despues, razon, dias…) dio 0 apariciones. Todas las versiones envuelven la tabla en `overflow-x:auto` (100/100), y no hay desborde evidente. |

**Detalle de la coherencia 4 (código):**
- **Metadatos:** `exname`, `extype: schoice`, `exsolution` por `mchoice2string`, `exshuffle: TRUE` y todos los `exextra[...]` están presentes y con tildes.
- **Regla #19:** la Solution no identifica opciones por letra en 100/100 HTML (0 coincidencias de «Opción A-D» o «(a)»).
- **INC-SOLUTION-ORDEN:** la prosa no enumera opciones en orden. Los 3 distractores de «Análisis de los distractores» van por rol y citan la afirmación literal. Esa cita coincide con una opción del Answerlist, y su feedback es «Falso». Esto cumple 100/100, y la clave citada no está entre los distractores.
- **`pandocbounded`:** 0 usos. Cada `.tex` solo trae la definición no-op `\providecommand{\pandocbounded}[1]{#1}`. Los `exercise1.tex` de las corridas PDF y NOPS tienen 0 apariciones.
- **Tablas Markdown y glifos:** la guardia `@ifundefined{c@none}` está en `SUB/...schoice_v1.Rmd:619`. `validar_glifos_latex.R` respondió «OK: sin glifos que rompan pdflatex».
- **PDF:** los 70 compilan (3 páginas, o 4 en 2 casos). Abrí como imagen `p09` páginas 1 y 2 (canónica, con la Solution) y NOPS `n1` páginas 1 y 3. No vi tablas cortadas, glifos rotos ni fórmulas partidas.
- **DOCX:** los 50 contienen `<w:tbl>`, la sección «Respuesta correcta», los códigos «ERR-» y las fórmulas.

**Canónica contra el original (`orig_pregunta_50.png`)** (HTML h3, PDF p09 y docx):
- **Enunciado:** idéntico en palabras, con «recogido» en el enunciado.
- **Tabla:** celdas idénticas, con «recolectado» en el encabezado, 5 cursos/40 kilos y 4 cursos/45 kilos. Las sedes van en negrita.
- **Pregunta y opciones:** pregunta idéntica y las cuatro opciones verbatim, con «de secundaria» en A y «ganó porque» sin «el concurso» en C.
- **Diferencias, todas deliberadas o de formato:**
  1. El espacio duro U+00A0 va entre «40/20/5» y «kilos», tres veces (L-6).
  2. El orden de las opciones y las letras se sortean (`exshuffle: TRUE`).
  3. Es una tabla Markdown plana, sin el color naranja de los encabezados ni los bordes del original.
  4. Falta el cursor «I» del escaneo junto a «¿cuál», que es un artefacto y no está en la plana.
  5. Se añade una Solution con la capa metacognitiva, que el original no tiene.
  6. Se omiten el número «50.» y las burbujas de letra (A)-(D).

## Hallazgos

- **BAJA 1 (residuo ya firmado en O-1):** en 20 de las 93 versiones no canónicas (21,5 %), una opción del lado de G cita T_G como «en x kilos». Con la lectura de «x» como el total, esa opción también sería verdadera. Calza con el 17-21 % firmado en `SUB/.claude/CLAUDE.md` (≈ líneas 212-217) y con su juicio de que no es una lectura razonable. No es CORRECCIÓN, y no verifiqué las cifras de la batería §P7.
- **BAJA 2 (declarado, H-2):** la clave canónica «el total fue de 20 kilos más» admite la lectura «el total fue de 20 kilos». Eso la volvería falsa sin volver verdadera otra opción. Se conserva por la regla #13 v1.2 y la regla #24.
- **BAJA 3 (cobertura):** NOPS no incluye canónica (N=2, no salió ninguna). El `.tex` envoltorio de `exams2pdf` se sobrescribe, así que la comprobación de `pandocbounded` en el contenido solo cubre el último `exercise1.tex`. La compilación limpia de los 70 PDF sustenta el resto.
- **CRÍTICA, ALTA y MEDIA:** Ninguno.

## Rutas de los artefactos

Todos bajo `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/salida/ciclo_20261102/s2d/paso8/`:
- `html/`, `pdf/`, `pdf2/`, `tex/`, `tex2/`, `docx/`, `docx2/`
- `moodle/m.xml`
- `nops/` y `tex_nops/`
- `png/` (vistas previas)
- `log_*.txt`

VEREDICTO_COHERENCIAS: COHERENCIAS_OK
