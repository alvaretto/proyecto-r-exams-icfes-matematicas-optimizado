# Cuadernillos Saber 11 Matemáticas en oficio a doble columna: investigación y propuesta

Fecha: 2026-11-13 (fases 0 a 3 del prompt optimizado) y 2026-11-14 (implementación).
- Estado: la arquitectura del §5, APROBADA por el profesor, **está implementada** según el PRP
  `PRPs/motor-cuadernillos-oficio-2col.md`. El uso, las guardas, los tiempos y los límites están en el **§10**.
- Los 33 `.Rmd` de `01-`, `02-` y `04-` se corrigieron (§4; commit `db6d9cc3`).
- Las reglas de `.claude/` que cambian (regla 1 de `codigo-rmd.md` y regla #26) están actualizadas en la v3.44.0.

Convenciones: **[MEDIDO]** = medido en este equipo (R/exams 2.4.5, pandoc 3.11, pdfTeX de TeX Live 2026);
**[FUENTE]** = respaldado por la fuente citada; **[NO VERIFICADO]** = no se pudo respaldar.

---

## 1. Decisiones del profesor (fase 1)

| Tema | Decisión |
|---|---|
| Papel | Oficio, 216 × 330 mm (no legal, 216 × 356) |
| Hoja de respuestas | NOPS aparte, escaneable |
| Integridad de la pregunta | **Se permite partir** entre columnas o páginas (máxima densidad) |
| Letra | 10 pt, como el ICFES |
| Figuras más anchas que la columna | Se reducen a la columna; si el texto queda ilegible, **se regenera la gráfica en TikZ** |
| Figuras TikZ | **Vectoriales en PDF y NOPS** (excepción a la regla 1 de `codigo-rmd.md`, aprobada por el profesor) |
| Word (.docx) | Réplica del PDF: oficio y dos columnas, para retocar e imprimir |
| Impresora | Doble cara, blanco y negro |
| Versiones | **Una por estudiante** (grupos de menos de 25) |
| Tamaño del examen | Fin de periodo, o simulacro en **2 sesiones de 25 preguntas** |
| Primera página | Encabezado compacto (los datos del estudiante van en el NOPS) |
| Para el profesor | Clave por versión y retroalimentación completa; la retroalimentación **se comparte en digital, no se imprime** |

---

## 2. Línea base frente a prototipo [MEDIDO]

Las mismas 8 preguntas (5 de `02-`, 3 de `04-`), la misma semilla y el mismo medidor (`prototipo/medir_pdf.py`).
Línea base = plantilla oficial `perifericos/solpcielo.tex` con la Solution oculta (es lo que recibe el estudiante).

| Medida | Línea base (oficial, 1 columna) | Prototipo (`cuadernillo_oficio_2col.tex`) |
|---|---|---|
| Papel | 216 × 330 mm | 216 × 330 mm |
| Caras para 8 preguntas | 5 (portada + 4) = 3 hojas a doble cara | **2 = 1 hoja a doble cara** |
| Preguntas partidas | 1 de 8 | 2 de 8 (permitido por el profesor) |
| Caracteres por línea (líneas completas) | ~115 (mediana 79) | **~56** (mediana) |
| Figuras rasterizadas | 6 de 8 | 6 de 8 (vienen así de los `.Rmd`; ver §3.2) |
| Overfull `\hbox` | 0 | 0 (primera versión: 3, de 27, 141 y 163 pt; corregido, ver §3.2) |
| Fuentes sin incrustar | 0 | 0 (con un gráfico de `plot()` de R: Helvetica sin incrustar, ver §3.2) |
| Texto vectorial por tamaño | — | 92 % en 10 pt; mínimo 7 pt |

Con 10 preguntas (las 8 más una tabla Markdown y una prueba sintética de figuras): 3 caras de contenido y una cara en
blanco al final para que el total sea par (doble cara). Hojas de contacto: `prototipo/evidencia/hoja_base.png` y
`hoja_proto10b.png`.

Estimación, no medida: a esa densidad, una sesión de 25 preguntas ocuparía unas 6 a 7 caras (4 hojas). Se medirá en la
implementación.

---

## 3. Hallazgos

### 3.1 Doble columna

- **`multicols` no admite flotantes**: «only page-wide floats (i.e., star-forms) can be used within the scope of the
  environment»; «floats and marginpars are not allowed in the current implementation». [FUENTE: manual de multicol,
  resumen y §2.4, https://ctan.csail.mit.edu/macros/latex/required/tools/multicol.pdf]. En R/exams eso ocurre con los gráficos de
  knitr (`plot()`, `ggplot()`): su texto alternativo («plot of chunk …») hace que pandoc los envuelva en `figure`.
  **Solución medida**: `float` con `\floatplacement{figure}{H}`. El gráfico aparece en su sitio. Además, el
  pie «Figura 1: plot of chunk grafico» se anula en la plantilla con `etoolbox`.
- **`longtable` no funciona en varias columnas**: «longtable now issues an error if started in the scope of
  \twocolumn, or the multicols environment», con el error `longtable not in 1-column mode`. [FUENTE: manual de longtable,
  https://ctan.csail.mit.edu/macros/latex/required/tools/longtable.pdf]. Pandoc escribe toda tabla Markdown
  como `longtable`. **Solución medida, sin posprocesar el `.tex`**: la plantilla redefine `longtable` como `tabular`
  dentro de un `adjustbox{max width=\linewidth}`, anula `\endhead`/`\endlastfoot` y pone el `\bottomrule` al final.
  Una tabla `kable()` de 4 columnas sale bien en la columna. El intento anterior (`01-…/ExamenLegal2Col.R`) la
  reemplazaba con `gsub` por `tabular{ccc}` fijo y recompilaba por fuera de R/exams.
  Alternativa no probada: el paquete `multicoltab` [FUENTE: https://ctan.org/pkg/multicoltab].
- **Si algún día se exige integridad**: un `tcolorbox` sin `breakable` se puede usar dentro de `multicols` sin cuidados
  especiales [FUENTE: manual de tcolorbox, §19.6]. Con la decisión «se permite partir» no hace falta. Solo se dejaron
  `\clubpenalty = \widowpenalty = 10000` para que una línea no quede sola al pie o en la cabeza de una columna.

### 3.2 Figuras

- **Vectorial al imprimir, sin condicionales prohibidos** [MEDIDO]. `match_exams_device()` (R/exams 2.4.5) devuelve:

  | Función | Dispositivo | Figura TikZ resultante |
  |---|---|---|
  | `exams2pdf()` | `pdf` | `\includegraphics{triangulo.pdf}`, vectorial: 0 imágenes raster en `pdfimages -list` |
  | `exams2nops()` | `pdf` | ídem |
  | `exams2pandoc()` (docx) | `png` | `word/media/*.png` |
  | `exams2html()` | `png` | `<img src="data:image/png…">` |
  | `exams2moodle(svg = TRUE)` / `(svg = FALSE)` | `svg` / `png` | `triangulo.svg` / `triangulo.png` en el XML |

  Con `include_tikz(..., format = match_exams_device(), markup = "markdown")`, cada formato recibe lo suyo, sin
  `is_latex_output()` (que es FALSE en los 5 pipelines). 72 archivos de `Ejemplos-Funcionales-Rmd/` ya lo usan. Hoy
  la regla 1 de `codigo-rmd.md` muestra `format = "png"` como el patrón correcto. El profesor aprobó la excepción
  «vectorial al imprimir», y documentarla es parte de la implementación (§5).
- **`\setkeys{Gin}{max width=\linewidth}` no basta** [MEDIDO]: no actúa cuando pandoc ya trae `width=` (3 Overfull).
  **Solución medida**: envolver cada `\includegraphics` en `\adjustbox{max width=\linewidth}{…}` (0 Overfull).
  `max width` reduce solo lo que no cabe [FUENTE: manual de adjustbox, §4.3].
- **El costo de reducir es el texto de la figura** [MEDIDO]:
  - Los PNG de matplotlib se guardaron a 150 ppp y se imprimen a 323-340 ppi efectivos. Eso equivale a reducirlos al
    45 %, y sus rótulos (unos 10 pt en origen) quedan en unos 4,5 pt.
  - `pasteleria_…` dibuja su TikZ en PNG y se imprime a **183 ppi**.
  - El texto vectorial sí se puede medir en el PDF final (`pdftohtml -xml`): mínimo 7 pt en el prototipo.
- **Blanco y negro** [MEDIDO]. En la torta de `01-S2-2025-SEDQ-grafico_circular_bienes_v0`, los sectores azul, morado
  y rojo quedan casi del mismo gris (`prototipo/evidencia/torta_color_vs_gris.png`). Hay que regenerarla con
  niveles de gris o tramas.
- **Fuentes sin incrustar** [MEDIDO]. Un gráfico de `plot()` de R en PDF deja Helvetica sin incrustar (el dispositivo
  `pdf()` no incrusta las 14 fuentes base). Se corrige con `cairo_pdf` o con `embedFonts()`; queda para la
  implementación.

### 3.3 Hoja NOPS aparte y cuadernillo propio

- **Coinciden si comparten la matriz de semillas** [MEDIDO, `prototipo/pruebas/poc_nops.R`].
  `exams2nops(seed = S)` y `exams2pdf(seed = S, template = cuadernillo)` dieron el mismo texto y la misma clave en
  14 de 16 pares (7 ejercicios × 2 versiones).
- **El número de versión impreso coincide con el ID del NOPS** (`26111300001`, `26111300002`). Se pasa
  `header = list(ID = function(i) …)` con la misma fórmula interna de `exams2nops` (`Date2ID`: `aammdd` + 5 dígitos).
- **El par que falló es grave.** `fracciones_reparto_premio_v1.Rmd` ni siquiera se reproduce consigo mismo, porque su
  línea 236 hace `set.seed(NULL)  # Asegurar aleatoriedad real`. Con NOPS, la hoja calificaría con una clave distinta
  de la impresa. `set.seed(NULL)` aparece en 5 archivos, pero `Exportaciones…` sí se reprodujo. Por eso la guarda
  debe ser **la doble corrida medida** (mismo texto y misma clave con la misma semilla), no un grep.
- **`exams2nops(twocolumn = TRUE)` falla si una pregunta trae una tabla Markdown** [MEDIDO, 2026-11-13]:
  «longtable not in 1-column mode». Como del NOPS solo se usa la hoja de respuestas (las preguntas van en el
  cuadernillo propio), el NOPS se genera con `twocolumn = FALSE`.
- **Fuga de estado entre ejercicios** [MEDIDO]. Seis ejercicios redefinían de forma global el motor `python` de
  knitr con uno que no ejecuta nada (`knit_engines$set(python = function(options) engine_output(...))`). R/exams teje
  todos los ejercicios de un examen en la misma sesión de R, así que los chunks `{python}` de cualquier ejercicio
  posterior dejaban de ejecutarse **en silencio**: la figura desaparecía del PDF, del HTML y de Moodle.
  `validar_stresstest.R` mide cada ejercicio por separado y no lo ve. Hace falta la prueba con un examen real que
  junte varios ejercicios (§4).

### 3.4 Word (.docx)

- **Pandoc toma de la plantilla de Word el tamaño de página, los márgenes y las columnas** (el `w:sectPr` de su
  `document.xml`). [FUENTE: manual de pandoc, `--reference-doc`, https://pandoc.org/MANUAL.html; código
  `src/Text/Pandoc/Writers/Docx.hs`, función `extractPageLayout`].
  [MEDIDO] Con `exams2pandoc(..., options = "--reference-doc=<ruta absoluta>")`, la salida trae
  `<w:pgSz w:w="12246" w:h="18709"/>` (216 × 330 mm) y `<w:cols w:num="2" w:sep="1"/>`, 4 figuras PNG y 39 ecuaciones
  nativas de Word (OMML). LibreOffice lo abre en 216 × 330 mm, a dos columnas (`evidencia/word_oficio_2col_libreoffice.pdf`).
- **Trampa medida**: `exams2pandoc()` evalúa `options` después de cambiar de directorio (evaluación perezosa de R). Con
  una ruta relativa, pandoc no encuentra la plantilla («not found in resource path»). La ruta absoluta se calcula
  **antes** de la llamada.
- Pendiente para la implementación:
  - una plantilla `.tex` sin Solution para el Word (la de prueba usó `plain.tex`, que la incluye);
  - la letra sans en el estilo Normal de la plantilla de Word.
  - Las limitaciones conocidas de TeX → OMML (`\pmod`, `siunitx`, arrays con `@{}`) [FUENTE: issues de
    https://github.com/jgm/texmath que reportó la búsqueda, p. ej. #241, #131 y #132; no los revisé uno a uno].

### 3.5 Estándares de diseño y legibilidad

- **Cuadernillos ICFES del repo** (`AnythingLLM-Config/IA ICFES LLMs/Math/Cuadernillos EPA/`, 11.° de 2021 y 2023)
  [MEDIDO]: tamaño carta, una columna, opciones A-D. Texto en Tahoma de 10 pt (73-84 % de los caracteres) y rótulos
  de 7 a 9 pt (en 2023, un 1 % del texto a 6 pt).
- **Longitud de línea**: 45-75 caracteres es lo «generalmente aceptable», y nunca más de 95 [FUENTE: NAEP Style
  Guide, https://naepdev.github.io/NAEP-Style-Guide/ux/typography.css.html, guía digital]. La doble columna lleva la
  línea de ~115 a ~56 caracteres: dentro del rango.
- **Opciones en vertical**: directriz de Haladyna, Downing y Rodriguez (2002), *Applied Measurement in Education*
  15(3), 309-333 [FUENTE secundaria; no se pudo abrir el original].
- **12-14 pt y rótulos de 14 pt en pruebas accesibles** (UNC, Smarter Balanced) [NO VERIFICADO]. Son guías de
  accesibilidad, no de cuadernillos regulares, y el profesor eligió 10 pt, como el ICFES.
- **No encontré una fuente con una cifra de «preguntas por página»**, ni una que mida el efecto de partir una
  pregunta en el desempeño [NO VERIFICADO]. Por eso la densidad se gobierna con los criterios del §6 y no con un
  número externo.

---

## 4. Defectos de los `.Rmd` de los exámenes: corregidos el 2026-11-13 (sin commit)

Los corrigieron agentes `implementador` en worktrees, uno por carpeta. El coordinador revisó los diffs, los integró y
volvió a medir.

**Medición por archivo** (`validar_stresstest.R`, N = 100, los 33 `.Rmd` de `01-`, `02-` y `04-`):

| Archivo | Antes | Después | Corrección |
|---|---|---|---|
| `01-…/2023-Matematicas-11-2-04-Op-D-V2` | `ERR_STRESS_TIMEOUT` | PASS | 4 bucles sin tope → construcción directa + `for` acotado + `stopifnot` |
| `01-…/2023-Matematicas-11-2-04-Op-D` | `ERR_STRESS_TIMEOUT` (8 semillas, p. ej. 6, 10, 16) | PASS | ídem |
| `01-…/2023-Matematicas-11-2-04-Op-B-V2`, `Op-C`, `Op-C-V2` | PASS | PASS | bucles con tope (preventivo) |
| `01-…/schoice-DVenn_All_GenMus_02`, su `Copia de …` y `…_01_fixed` | `ERR_STRESS_ERROR` (semillas 45 y 85 sin clave única) + opciones repetidas en 11 semillas | PASS | `generate_distractors()` reescrita con rangos disjuntos + `stopifnot` |
| `01-…/p25_SAI_CFA11S123` (CLOZE) | `##ANSWER1##` repetido en las 100 semillas | PASS | dos huecos numéricos (`num\|num`), uno por celda. **Cambia la puntuación: 2 partes en lugar de 1** |
| `01-…/I_1796473-Opc-{A2,A2v2,B2,C2v2,D2v2}` | PASS (pero ver la fuga de §3.3) | PASS | figuras a tamaño final, vectoriales y en grises; «un(a)» resuelto; bucles enumerados; motor `python` declarado |
| `02-…/fracciones_reparto_premio_v1` | `ERR_STRESS_TEXTO` + no reproducible | PASS | sin `set.seed(NULL)`; figura a tamaño final y vectorial; bucle con tope |
| `02-…/fracciones_reparto_premio_v2..v4` | no reproducible | PASS | ídem |
| `02-…/vaso-cilindrico-v6` | no reproducible (el mensaje de tinytex solo sale la primera vez) | PASS | `message = FALSE` en el chunk de la figura |
| `02-…/11_C01_G09_2020_Tipo1` | opciones repetidas (40 de 2000 semillas) | PASS (0 de 5000) | filas inválidas se regeneran con respaldo determinista + `stopifnot` |
| `02-…/01-S2-2025-SEDQ-grafico_circular_bienes_v0` | PASS | PASS | concordancia de género («una encuesta», «Las estadísticas», «del negocio»); torta en grises distintos; bucles |
| `04-…/cateto_…_n2_v1_1` | opciones repetidas (semillas 12 y 75) | PASS | distractores filtrados por texto visible + `stopifnot` |
| `04-…/ExportacionesGraficos…_n3_v1` | PASS | PASS | `\n` literal corregido; barras y torta con tramas, a tamaño final y vectoriales |
| `04-…/pasteleria_…_n2_v1` | PASS | PASS | TikZ vectorial (`format = typ`), dibujado al ancho de la columna (texto de 8-10 pt, días sin montarse) |
| Los 6 de SEDQ, fracciones v1-v4 y Exportaciones | — | PASS | eliminada la redefinición global del motor `python` (§3.3) |
| Los otros 10 `.Rmd` | PASS | PASS | sin cambios |

**Prueba con examen real** (11 preguntas de las tres carpetas, 3 versiones, hoja NOPS + cuadernillo propio con la
misma matriz de semillas):
- **33 de 33 pares** con el mismo texto y la misma clave en el NOPS y en el cuadernillo;
- **33 de 33** idénticos en una segunda corrida;
- **33 de 33** con una sola clave.

Cada cuadernillo: 216 × 330 mm, 4 caras, 0 imágenes rasterizadas y todas las fuentes incrustadas. Texto vectorial de
7 pt como mínimo, salvo un exponente («48²», 6 pt) en el rótulo del dibujo del cilindro de la versión 2.

**Segunda ronda («corrige todo», 2026-11-13):**
- **Formato numérico del ICFES** (coma decimal y punto de miles, medido en el cuadernillo 2023: «7.000», «5,5»)
  en el texto visible y en los rótulos de las figuras de los 33 archivos. Solo cambia la presentación: `exsolution` y
  `extol` no cambian. En `$…$` la coma va como `{,}`.
- **Cilindro (vaso)**: el rótulo «π × r² = …» va fuera del contorno, con el exponente ≥ 7 pt.
- **Venn (5 archivos)**: cada conjunto se distingue sin color, con borde continuo, discontinuo o punteado y el nombre
  junto al círculo.
- **Tablas TikZ de I_1796473**: de ~13 pt a 9 pt, con escala ≈ 1.
- **Guard `\newcounter{none}`** en los 6 archivos con tabla Markdown.
- **TeX crudo en la Solution de `cateto`** («\sqrt{5}»): ahora va compuesto en `$…$`.
- **Coherencia de valores en I_1796473-A2**: la tabla, las barras y la torta muestran el mismo dato. Los encuestados
  pasan a múltiplos de 20, así que las cantidades son enteras por construcción.
- **Media-Mediana-Moda**:
  - «lo cuál» → «lo cual»;
  - la instrucción del CLOZE dice «con coma o con punto; por ejemplo: 73,25». Comprobado en las fuentes: Moodle
    (`apply_units` de `qtype_numerical`) y `exams2forms/webex.js` aceptan los dos formatos.
- **`exextra` oficiales en los 33 archivos** (13 familias): literales del catálogo, con `validar_literalidad_exextra.py`
  en exit 0 en los 33 y sin claves duplicadas.
  - La guía oficial del ICFES de Evaluar para Avanzar 2023 (11-2) imprime la clasificación de las preguntas 1, 4 y 9;
    las demás son clasificación propia.
  - Decisiones del profesor: 11_C01 en «Interpretación y representación»; Nivel 3 para los ítems de porcentajes;
    `cateto` en Nivel 3, renombrado a `…_n3_v1_1.Rmd`, con las 9 referencias de la carpeta 04 actualizadas.
- **Verificación final:**
  - `validar_stresstest.R`: PASS limpio en los 33.
  - Examen real de 11 preguntas × 3 versiones: 33 de 33 pares coinciden entre la hoja NOPS y el cuadernillo; texto
    vectorial ≥ 7 pt; 0 imágenes rasterizadas; ningún número en formato inglés ni TeX crudo.
- **Fuera de alcance, por decisión del profesor:** la carpeta `Copia de 04-Examen-Fin-de-Periodo-4/` queda sin tocar.

---

## 5. Arquitectura propuesta (IMPLEMENTADA: ver §10)

> Este apartado se conserva como se aprobó el 2026-11-13. Lo que la implementación midió distinto está en el §10.6:
> el NOPS va con `twocolumn = FALSE`, la clave es un PDF generado y no una plantilla, la retroalimentación se llama
> `retro_celular.tex`, el medidor sigue en `prototipo/medir_pdf.py`, y la salida va en `salida/<examen>/sesion-<k>/`.
> Las cuatro plantillas ya no están en `plantillas/`: el 2026-11-14 pasaron a `A-Produccion/perifericos/` como versión
> oficial (decisión 5 del §7), y el motor las lee de allí.

```
00-Cuadernillos-Oficio-2Col/
├── plantillas/
│   ├── cuadernillo_oficio_2col.tex   # estudiante: deriva de perifericos/solpcielo.tex (cambios listados en su cabecera)
│   ├── clave_por_version.tex         # tabla compacta: versión × pregunta → letra
│   ├── retroalimentacion_digital.tex # una columna, para pantalla (formato por decidir, §7)
│   └── referencia_oficio_2col.docx   # Word: oficio, 2 columnas, filete, 10 pt sans
├── generar_cuadernillos.R            # semillero: lista de .Rmd, n ≤ 100, fecha, sesión → todo con UNA matriz de semillas
├── verificar_cuadernillos.R          # guardas del §6 con mutantes; exit ≠ 0 = no se imprime
├── medir_pdf.py                      # medidor (ya existe en prototipo/)
└── salida/<fecha>_<sesion>/          # cuadernillos, NOPS en oficio, clave, retroalimentación, docx
```

Flujo de `generar_cuadernillos.R`:

1. **Sorteo de semillas.** Fija la matriz `n × preguntas` y la guarda en `salida/…/semillas.rds`. Es la fuente única
   de las cinco salidas.
2. **Hojas de respuestas.** `exams2nops(seed = S, twocolumn = TRUE, samepage = TRUE, header = "\\raggedbottom",
   duplex = TRUE)`. Se extrae la hoja de respuestas y se centra en oficio con `pdfjam` (regla #26).
3. **Cuadernillos.** `exams2pdf(seed = S, template = "cuadernillo_oficio_2col",
   header = list(ID = mismo Date2ID))`: uno por estudiante, más un PDF combinado para imprimir de una vez (cada
   cuadernillo con número par de caras).
4. **Clave.** Tabla de versión × pregunta a partir del `.rds` de `exams2nops`, el mismo que usa `nops_eval()`.
5. **Retroalimentación.** Una por versión, con la Solution, en una columna, para compartir en digital.
6. **Word.** `exams2pandoc(seed = S, options = "--reference-doc=<absoluta>")`, opcional.
7. **Simulacro.** Dos corridas (sesión 1 y sesión 2, 25 preguntas cada una), con su propia matriz y su propio NOPS.

Cambios fuera de la carpeta, que la implementación debe hacer con su protocolo:

- **Regla 1 de `codigo-rmd.md`** (compacta y texto íntegro) + `CHANGELOG_CLAUDE_MD.md`: la excepción «vectorial al
  imprimir» con `format = match_exams_device()`, con la tabla de §3.2 como evidencia. Es infraestructura protegida
  (regla #17): snapshot de `.claude/` antes del cambio y `test_infraestructura_claude.R` después.
- Pasa por **PRP** (`/generate-prp` → `/execute-prp`, con la compuerta del adversario), porque toca más de 3 archivos.

---

## 6. Criterios de éxito (propuestos con valores, a partir de lo medido)

| # | Criterio | Cómo se mide | Prototipo |
|---|---|---|---|
| C1 | Papel 216 × 330 mm en cuadernillo, NOPS (`*_oficio.pdf`) y Word | `pdfinfo`; `w:pgSz` del docx | ✔ cuadernillo y Word |
| C2 | 100 % de fuentes incrustadas | `pdffonts` | ✔ (✘ con `plot()` de R: §3.2) |
| C3 | 0 Overfull `\hbox` > 1 pt | `.log` | ✔ |
| C4 | Texto vectorial ≥ 7 pt (como los rótulos del ICFES) | `pdftohtml -xml` | ✔ (mínimo 7) |
| C5 | Raster ≥ 300 ppi efectivos y reducción ≥ 0,7 (rótulos de 10 pt → ≥ 7 pt); si no, «regenerar en TikZ» | `pdfimages -list` + ppp del PNG | ✘ 4 figuras de matplotlib (~4,5 pt) y 2 PNG de pastelería (183 ppi) |
| C6 | Reproducibilidad: misma semilla → mismo texto y misma clave, en exams2pdf × 2 y en exams2nops | doble corrida | ✘ `fracciones_v1` |
| C7 | ID impreso = ID del NOPS, en el 100 % de las versiones | `pdftotext` + nombres del `.rds` | ✔ |
| C8 | Número par de caras por cuadernillo | `pdfinfo` | ✔ |
| C9 | Tope de tiempo por ejercicio (un bucle sin tope no cuelga la generación) | `setTimeLimit`/`timeout` | ✘ (colgó con `Op-D-V2`) |
| C10 | Figuras distinguibles en gris | hoja de contacto en gris, revisión visual | ✘ torta de SEDQ |
| C11 | Máximo 100 versiones (regla #26) | `stopifnot` | — |

Cada guarda lleva un mutante que la haga fallar (p. ej., quitar el `adjustbox` → C3 rojo; `set.seed(NULL)` → C6 rojo).

---

## 7. Decisiones del profesor (todas cerradas el 2026-11-13)

Antes eran preguntas abiertas. El profesor las respondió en la sesión del 2026-11-13; se transcribe su respuesta literal.

1. **Formato de la retroalimentación digital**: «PDF tamaño celular», una columna, una por versión, para compartir por
   WhatsApp. No se imprime.
2. **Etiqueta «Escenario:»**: «Quitarla». La pregunta empieza con su número, como en el ICFES.
3. **Defectos heredados del §4**: «Corrige todo». Están corregidos en los 33 `.Rmd` (§4).
4. **Figuras de matplotlib con rótulos < 7 pt**: «reducir a la columna y, de ser necesario, regenerar la gráfica en
   TikZ». Hecho: están a tamaño final, vectoriales y legibles en gris (§4).
5. **Promoción a `perifericos/`**: «Ya, al terminar el proyecto», **sin condición de aula**. La pregunta ofrecía también
   «Después de usarla en el aula (Recomendado)» y «Nunca».
6. **Letra**: «Helvetica, como la oficial».

---

## 8. Cómo reproducir

```bash
# desde una carpeta con copias de los .Rmd en ej/ y las plantillas del prototipo al lado
Rscript prototipo/pruebas/prototipo.R
python3 -I prototipo/medir_pdf.py tex8/proto8_1.pdf --log tex8/proto8_1.log --columnas 2
Rscript prototipo/pruebas/poc_nops.R      # tabla texto= clave= repro= por versión y pregunta
Rscript prototipo/pruebas/poc_docx.R      # Word en oficio a dos columnas
```

El medidor reconoce el inicio de cada pregunta por «Escenario:» y el fin por la última opción «d)» (modo `--marcas texto`,
el de la plantilla oficial vieja). Con las plantillas del motor usa las anclas `qini.N` / `qfin.N` (modo `dests`, que
elige solo si el PDF las trae).

---

## 9. Dónde ver las muestras

| Carpeta | Qué hay | Estado de los `.Rmd` |
|---|---|---|
| `muestras/` | Examen real de 11 preguntas (de los exámenes 01, 02 y 04) en 3 versiones, **generado por el motor** el 2026-11-14 (`configuraciones/examen_real_11.R`): `cuadernillo_<ID>.pdf` (el del estudiante: oficio, 2 columnas, 4 caras, sin retroalimentación), `cuadernillo_docente_<ID>.pdf` (el del docente: las mismas preguntas con la clave y la retroalimentación, 8 caras), `hoja_respuestas_<ID>.pdf` (hoja NOPS centrada en oficio, 2 caras), `retro_<ID>.pdf` (retroalimentación de 90 × 160 mm para el celular), `cuadernillo_<ID>.docx` (Word en oficio a 2 columnas), `clave.pdf` (clave por versión), y `revision_gris_v1.png` y `revision_gris_vN.png` (las 4 caras de la primera y de la última versión, como saldrían en la impresora en blanco y negro). `<ID>` = `26111300001`, `26111300002` y `26111300003` | **Después** de todas las correcciones (§4) y con las plantillas del 2026-11-14 (§10.6) |
| `prototipo/evidencia/` | Línea base con la plantilla oficial (`linea_base_8preg_oficial.pdf`) frente al prototipo a 2 columnas (`prototipo_8preg_2col.pdf`, `prototipo_10preg_2col.pdf`), Word en oficio (`word_oficio_2col.docx` y su conversión con LibreOffice), torta en color frente a gris | **Antes** de corregir los `.Rmd` (sirve para comparar) |
| `prototipo/pruebas/` | Scripts que generan todo lo anterior; `prueba_examen_real.R` es el que produce `muestras/` | — |

`muestras/` se regenera con el motor, que nunca escribe ahí (esa ruta no pasa su guarda): se genera en `salida/regen/` y se
copia. Desde `00-Cuadernillos-Oficio-2Col/`:

```bash
Rscript generar_cuadernillos.R configuraciones/examen_real_11.R --dir-salida salida/regen --limpiar   # debe salir con 0
# después, con los ID de salida/regen/sesion-1/firmas.rds, copiar a muestras/ los cinco archivos de cada versión,
# clave/clave.pdf y las dos hojas en gris
```

Las muestras del 2026-11-13 (`*_version_*` y `hojas_version1_*.png`) llevaban el defecto de Sweave del §10.6 (toda figura
ampliada al ancho de la columna) y no traían retroalimentación ni Word: se retiraron al regenerar. Quedan en el historial
de git (commit `7dcbd282`). Lo regenerado es idéntico, píxel a píxel, a lo que se revisó con la vista (C10): 115 páginas de
cuadernillos y retroalimentación comparadas, 0 distintas. La comparación se repitió después de añadir el cuadernillo del
docente y la opción `partir_preguntas`: otra vez 115 páginas y 0 distintas (control: dos versiones distintas difieren en
sus 4 páginas). Los tres `cuadernillo_docente_<ID>.pdf` se añadieron entonces.

El mismo día, con el visto bueno del profesor, se corrigió la forma de la retroalimentación de tres ejercicios (preguntas
y claves, sin cambios): en `pasteleria_sabores…n2_v1.Rmd`, un `cat()` separaba con espacios y
dejaba los asteriscos a la vista («** Jueves **») y un espacio antes de los dos puntos; en
`ExportacionesGraficosEstadisticaInterpretacion_n3_v1.Rmd`, tres renglones del cálculo salían pegados en uno; en
`vaso-cilindrico-v6.Rmd`, dos listas de la rama del área total salían pegadas a su párrafo. Después: examen real con
exit 0 (341 s), cuadernillos del estudiante idénticos píxel a píxel (12 páginas, 0 distintas) y ningún «**» en el texto de
los 9 PDF. Las muestras del docente y de la retroalimentación son las de esa corrida.

---

## 10. Uso del motor (implementado el 2026-11-13 y el 2026-11-14)

Todo lo de este apartado está **[MEDIDO]** en este equipo (R/exams 2.4.5, pandoc 3.11, pdfTeX de TinyTeX, poppler,
LibreOffice), salvo donde dice otra cosa.

### 10.1 Comandos

Desde `Auxiliares/Examenes_Fin_de_Periodo/00-Cuadernillos-Oficio-2Col/`:

```bash
# 1. Copiar una configuración y editarla: título, fecha, número de estudiantes y lista de .Rmd de cada sesión
cp configuraciones/examen_real_11.R configuraciones/mi_examen.R

# 2. Generar las cinco salidas y verificarlas (termina con la tabla C1-C15)
Rscript generar_cuadernillos.R configuraciones/mi_examen.R              # la primera vez
Rscript generar_cuadernillos.R configuraciones/mi_examen.R --limpiar    # para repetir sobre la misma salida

# Opciones: solo algunas salidas, o la salida en otra carpeta (debe quedar dentro de una carpeta «salida»)
Rscript generar_cuadernillos.R configuraciones/mi_examen.R --salidas cuadernillo,hoja,clave --limpiar
Rscript generar_cuadernillos.R configuraciones/mi_examen.R --dir-salida salida/prueba --limpiar

# 3. Volver a verificar una sesión ya generada, o comprobar que las guardas siguen vivas
Rscript verificar_cuadernillos.R salida/mi-examen/sesion-1
Rscript verificar_cuadernillos.R --mutantes
```

Código de salida del motor: **0** = C1-C9 y C11-C15 en verde (los avisos `WARN` se listan y no bloquean); **1** = alguna
guarda en `FAIL`; **2** = algo no se pudo verificar (`NO_VERIFICABLE`) o la carpeta de salida no pasó la guarda de ruta
(`ERR_DIR_SALIDA`). **C10 nunca cuenta como aprobada**: es la revisión visual en gris y se hace mirando.

El motor se niega a escribir en una carpeta que ya tiene contenido, salvo con `--limpiar`; y `--limpiar` solo borra una
carpeta que el propio motor creó (lleva el marcador `.cuadernillos_salida`).

**Estudiante y docente a la vez.** La salida `cuadernillo` produce siempre dos PDF por versión, con la misma plantilla y
las mismas semillas: el del estudiante (sin retroalimentación) y el del docente (las mismas preguntas en el mismo orden,
con la clave en la primera página y la retroalimentación debajo de cada pregunta). No hay que pedir el segundo.

**Partir o no las preguntas** (`partir_preguntas` en la configuración). Por defecto es `TRUE`: una pregunta puede empezar
en una columna y terminar en la siguiente, que es como caben más preguntas por hoja. Con `partir_preguntas = FALSE` cada
pregunta queda entera en una columna; ocupa más caras, y una pregunta más alta que la columna no cabe (la guarda C3 lo
reprueba). Lo medido con cada valor está en el §10.6.

Las plantillas están en `A-Produccion/perifericos/`: `cuadernillo_oficio_2col.tex`, `retro_celular.tex`,
`pandoc_cuadernillo.tex` y `referencia_oficio_2col.docx`. Son versión oficial (regla #26): no se editan sin instrucción del
profesor. El motor las busca allí por la raíz git; la variable `CUADERNILLOS_PLANTILLAS` apunta a otra carpeta, y la usan los
mutantes para dañar una copia y nunca las oficiales.

### 10.2 Qué produce

Por cada sesión, en `salida/<examen>/sesion-<k>/`, con `<ID>` = fecha `aammdd` + cinco cifras (el número de versión):

| Archivo | Para qué |
|---|---|
| `cuadernillos/cuadernillo_<ID>.pdf` y `cuadernillos_imprimir.pdf` | Cuadernillo del estudiante: oficio, 2 columnas, número par de caras, **sin retroalimentación**. El combinado se imprime de una vez a doble cara |
| `docente/cuadernillo_docente_<ID>.pdf` y `docente_imprimir.pdf` | Cuadernillo del docente: las mismas preguntas de esa versión, con la clave en la primera página y la retroalimentación debajo de cada pregunta. Lleva «DOCENTE» en el encabezado de todas las páginas |
| `hojas/hoja_respuestas_<ID>.pdf` y `hojas_imprimir.pdf` | Hoja NOPS centrada en oficio, de 2 caras (la segunda en blanco) |
| `clave/clave.csv` y `clave/clave.pdf` | Clave por versión, para el profesor |
| `retro/retro_<ID>.pdf` | Retroalimentación de 90 × 160 mm para el celular, con la clave y la Solution de cada pregunta |
| `word/cuadernillo_<ID>.docx` | Réplica en Word (oficio, 2 columnas, sin Solution) para retocar |
| `nops/nops_.rds` | Lo que necesita `nops_eval()` para calificar las hojas escaneadas |
| `revision_gris_v1.png`, `revision_gris_vN.png` y `revision_gris_*_p<K>.png` | Primera y última versión en gris, a 50 y a 150 ppp, para la revisión visual (C10) |
| `semillas.rds`/`.csv`, `firmas.rds`, `preflight.log`, `motor.log`, `verificacion.txt`/`.json`, `tex/` | Trazabilidad: la matriz de semillas, las huellas de cada salida, el pre-vuelo y los `.log` de LaTeX |

### 10.3 Guardas y mutantes

`verificar_cuadernillos.R --mutantes` construye 26 copias dañadas (de una plantilla, del motor, de una configuración o de un
ejercicio de prueba) y exige que la guarda correspondiente falle. Los 26 mueren.

| Guarda | Qué certifica | Mutantes |
|---|---|---|
| C1 | Papel: cuadernillos y hojas de 216 × 330 mm, retro de 90 × 160 mm; Word en oficio a 2 columnas, sin marcadores `##` y **sin imágenes ni tablas más anchas que su columna** | M1, M13, M16, M16b |
| C2 | Todas las fuentes incrustadas | M2 |
| C3 | Ningún `Overfull \hbox` de más de 1 pt, en los dos cuadernillos y en la retro; y **ninguna página de los cuadernillos con tinta a menos de 10 mm del borde inferior** (el margen es de 15 mm) | M3, M3b, M17b |
| C4 | Texto vectorial de 7 pt o más. El texto en fuentes Type 3 no se puede medir: `NO_VERIFICABLE` | M4, M4b |
| C5 | Imágenes raster de 300 ppi o más (150 a 299: aviso), en cuadernillos, retro y Word | M5 |
| C6 | Mismo texto y misma clave en la hoja NOPS, el cuadernillo, la retro y el Word | M6 |
| C7 | El número de versión impreso es el ID de la hoja NOPS | M7 |
| C8 | Cuadernillos con caras pares; hojas de 2 caras exactas, la segunda sin tinta | M8, M10b |
| C9 | Un ejercicio que se cuelga se corta y se nombra. Tope por ejercicio: 60 + 10 × N segundos (N = versiones), o el `presupuesto_s` de la configuración | M9 |
| C10 | Revisión visual en gris: **manual** | — |
| C11 | Límites: 1 a 100 versiones, 45 preguntas, tipos y opciones que admite la hoja NOPS, y la guarda de ruta | M11, M11b, M14 |
| C12 | Ningún ejercicio cambia el estado de knitr para los siguientes | M12, M12b |
| C13 | **Numeración impresa de las preguntas: 1..Q en cada cuadernillo (estudiante y docente) y en cada retro** | M15, M15b |
| C14 | Con `partir_preguntas = FALSE`, ninguna pregunta empieza en una columna y termina en otra. Con el valor por defecto no aplica, e informa cuántas se partieron | M17 |
| C15 | **El cuadernillo del estudiante no trae retroalimentación; el del docente y la retro traen la de cada pregunta.** Se comprueba con testigos: trozos de la Solution de cada pregunta que no están en ningún enunciado. Sin testigos, `NO_VERIFICABLE` | M18, M18b |

Suite: `tests/testthat/test_cuadernillos_oficio.R` (30 bloques), registrada en `tests/run_all_tests.R` con su `watch`.

### 10.4 Tiempos medidos

| Corrida | Tamaño | Tiempo |
|---|---|---|
| Camino feliz (5 ejercicios de prueba, 5 salidas) | 5 preguntas × 2 versiones | 25,7 s |
| Examen real (`examen_real_11.R`) | 11 preguntas × 3 versiones | 346 s (5 min 46 s) con el cuadernillo del docente; antes de añadirlo, 280 y 281 s |
| Simulacro (`simulacro_2x25.R`) | 2 sesiones × 25 preguntas × 2 versiones | 903 s (15 min 3 s) con el cuadernillo del docente: unos 7 min y medio por sesión; antes de añadirlo, 728 s |
| Capacidad (`capacidad_1x25.R`) | 1 sesión × 25 preguntas × 25 versiones | 4 538 s (75 min 38 s), exit 0. **Sin el cuadernillo del docente**, que se añadió después |
| `verificar_cuadernillos.R --mutantes` | 26 mutantes | 251 s (4 min 11 s); con los 22 de antes, 149 s |
| Suite `test_cuadernillos_oficio.R` (incluye los mutantes) | 30 bloques, 235 expectativas | 349 s (5 min 49 s) |

Por etapa, en el examen real (11 × 3): pre-vuelo 50,3 s; hojas NOPS 70,6 s; clave 0,4 s; cuadernillos del estudiante
54,2 s; cuadernillos del docente 54,0 s; retroalimentación 53,8 s; Word 50,2 s; hojas en gris 3,7 s; el resto (unos 9 s)
es la verificación. El camino feliz y la capacidad se midieron antes de añadir el cuadernillo del docente: les falta
esa etapa.
En el simulacro, por sesión (25 × 2; primera y segunda sesión): pre-vuelo 64,4 y 66,3 s; hojas NOPS 86,9 y 86,1 s;
cuadernillos del estudiante 70,6 y 70,2 s; cuadernillos del docente 70,5 y 70,5 s; retroalimentación 71,3 y 70,9 s; Word
69,3 y 68,7 s; hojas en gris 7,8 y 7,6 s. Las dos sesiones, con C1-C15 en verde y testigos de la retroalimentación en 50 de
50 preguntas.
En la capacidad (25 × 25): pre-vuelo 845,4 s; hojas NOPS 914,8 s; clave 0,4 s; cuadernillos 889,2 s; retroalimentación
931,3 s; Word 902,9 s; hojas en gris 7,8 s; verificación, unos 46 s. Se midió mientras en el mismo equipo (12 núcleos)
corrían otras pruebas del motor: es una cota superior. Salieron 25 cuadernillos de 8 caras (200 páginas y 16 MB en
`cuadernillos_imprimir.pdf`), retroalimentaciones de 74 a 78 páginas y 261 MB en total.

Tamaño de lo que sale: el examen real de 11 preguntas ocupa **4 caras** (2 hojas) por cuadernillo y de 34 a 35 páginas de
retroalimentación; una sesión de 25 preguntas ocupa **8 caras** (4 hojas) y de 75 a 77 páginas. El cuadernillo del docente
ocupa 8 caras con 11 preguntas y 16 con 25. El §2 estimaba de 6 a 7 caras para 25 preguntas: con estos ejercicios, que
traen muchas figuras, son 8.

**Cuánto tarda un examen completo [ESTIMADO a partir de lo anterior, no medido].** Cada etapa vuelve a tejer todos los
ejercicios de cada versión, así que el tiempo crece con el número de versiones: con 25 preguntas, unos 36 s por versión
y por etapa (NOPS: 86,9 s con 2 versiones y 914,8 s con 25; cuadernillos: 70,6 s y 889,2 s).

- El cuadernillo del docente es una etapa más, igual a la del estudiante (54,0 s frente a 54,2 s en el examen real;
  70,5 s frente a 70,6 y 70,2 s en el simulacro). Con 25 × 25 serían unos 890 s más: cerca de **90 minutos por sesión**.
- Un simulacro de 2 sesiones × 25 preguntas × 25 versiones tardaría el doble: cerca de **3 horas**, sin atenderlo. Conviene
  lanzarlo la víspera.
- Con menos versiones baja en proporción: 6 versiones de las dos sesiones, unos 45 minutos.

### 10.5 Calificar las hojas escaneadas

La hoja de cada estudiante es la de `exams2nops()`, centrada en oficio sin escalar. Su número es el del cuadernillo.

1. Escanear las hojas. Si el escáner las coloca en la esquina o las ajusta al ancho, pasarlas antes por
   `Auxiliares/exams2nops-escaner/normalizar_escaneos_nops.py` (regla #26).
2. `exams::nops_scan()` sobre las imágenes, y `Auxiliares/exams2nops-escaner/nops_control_calidad.R` sobre su resultado.
3. `exams::nops_eval(register = <lista del grupo>, solutions = "salida/<examen>/sesion-<k>/nops/nops_.rds", scans = <zip de nops_scan>, …)`.

**[MEDIDO] solo en simulación**: una hoja del examen real (`hoja_respuestas_26111300002.pdf`), rasterizada a 300 ppp y sin
marcar, pasa por `nops_scan()`, que lee el número `26111300002` y el tipo de hoja `011` (11 preguntas); ese número está en
`names()` de `nops_.rds`. **Falta la prueba con papel y escáner reales**, igual que en la regla #26.

### 10.6 Lo que la implementación midió distinto del plan

El PRP manda que, si una medición lo contradice, gane la medición y se declare. Estas son todas.

**Sobre R/exams, LaTeX y pandoc**

- R/exams inserta el `header` en la PRIMERA línea de la plantilla que contenga la marca `\exinput{header}`, aunque sea un
  comentario. Nombrarla en un comentario dio «Undefined control sequence \ID».
- `exams2pdf()` compila con `tinytex::latexmk`, que borra el `.log`. Con `options(tinytex.clean = FALSE)` se conserva
  (`tmp_1.log` en `texdir`). `exams2pdf()` crea `texdir` sin `recursive`: el motor crea antes la carpeta padre.
- El primer `include_tikz()` de la sesión imprime «Loading required namespace: tinytex» DENTRO del PDF. El motor carga
  `tinytex` antes de generar.
- `exams2pandoc()` sustituye cualquier clave de `header` escrita entre dobles almohadillas, y su plantilla necesita
  exactamente 9 líneas «#-». En el Word, un título con «&» y «_» sale bien sin escapar.
- `file.rename()` falla entre `/tmp` y `/home`: los archivos se copian y se borran.
- **Nombre de las copias con 10 o más versiones.** R/exams rellena el índice de cada copia con ceros hasta el ancho de `n`
  (`nops_01.pdf` … `nops_25.pdf`). El motor buscaba `nops_1.pdf`: con menos de 10 versiones los dos nombres coinciden, y
  todas las corridas habían sido de 2 o 3. Lo destapó la corrida de capacidad, que abortó en las hojas a los 27 minutos.
  Ahora el nombre sale de `nombre_copia()`, con la misma fórmula de R/exams, y la suite corre el motor con 10 versiones.
- `qpdf` sale con 3 («operation succeeded with warnings») con el PDF de `exams2nops()`: se usa `--warning-exit-0`.
- `Sys.readlink()` da `NA` en una ruta que no existe y `nzchar(NA)` es `TRUE`: la guarda de ruta la tomaba por enlace.
- **`Sweave.sty` ampliaba toda figura y toda tabla** (defecto del prototipo): fija `\setkeys{Gin}{width=0.8\textwidth}` y
  `adjustbox` hereda ese ancho. Una figura pedida a 5 cm salía a 257 pt. Con `\usepackage[nogin]{Sweave}` se respeta el ancho
  pedido (3 cm = 85,4 pt; 5 cm = 142,3 pt) y solo se reduce lo que no cabe. Las muestras del 2026-11-13 llevaban el defecto.
- **Numeración de las preguntas.** Cuando una lista numerada del ejercicio no empieza en 1, pandoc escribe
  `\setcounter{enumi}{N}`. Con las preguntas numeradas por `enumi`, la retroalimentación del examen real numeró
  1, 2, 3, **2, 3, 4, 5, 6, 7, 8, 9**: la lista «2. Comparar las medianas» de la Solution 3 cambió el número de la pregunta 4
  y corrió las siete siguientes, mientras la clave del encabezado decía 1..11. Documentado como Error 45 en
  `.claude/docs/patrones-errores-conocidos.md`, con la medición en PDF, NOPS, Word y HTML. Las dos plantillas numeran ahora con listas
  propias (`\newlist{preguntas}`, `\newlist{opciones}`), y las listas de pandoc recuperan la etiqueta del autor («1.», «2.»)
  en lugar de «a)». El cuadernillo quedó idéntico píxel a píxel (0 píxeles distintos en 4 páginas × 3 versiones; control:
  146 033 entre dos páginas distintas). De ahí la guarda C13.
- **Fuentes de mapa de bits.** El texto de ancho fijo y los símbolos que Helvetica no trae («→», «×» y «÷» en un bloque de
  código) salían de las fuentes EC/TC, que en este TinyTeX no tienen versión escalable: pdflatex las incrusta como Type 3.
  Las plantillas usan Latin Modern para eso (`\ttdefault` y las familias de sustitución de TS1). El kernel elige la
  sustitución según la familia en uso: como `rm` y `sf` son Helvetica, manda `\rmsubstdefault`.
- **Retroalimentación.** En 82 mm, la sangría de dos niveles dejaba las figuras de las opciones a 68 mm y sus rótulos de
  8 pt bajaban a 6,3 pt. Ahora las preguntas y las opciones no llevan sangría; si una opción empieza con una figura, su
  letra queda sola en su renglón y la figura va debajo, a 82 mm (7,6 pt). Un bloque de código parte el renglón (`fvextra`,
  `breaklines`; `breakanywhere` no compila con pdflatex y caracteres UTF-8). `\emergencystretch` evita que una cifra larga
  desborde (33 pt con «1 - (0,4705882353 + …»).
- **Word.** pandoc calcula el ancho de las imágenes y de las tablas contra el ancho útil de la página (19,58 cm), no
  contra la columna (9,50 cm) ni contra la sangría de la lista (720 twips por nivel). En el examen real, 14 de 17 imágenes
  se salían de su espacio, dos medían 19,58 cm, y la tabla de la pregunta 3 (rejilla de 13,97 cm) mostraba en LibreOffice
  una sola columna de datos. El motor reduce cada imagen y cada tabla a su espacio (`ajustar_docx_a_columna()`).
- Las figuras TikZ llegan al Word como PNG de 650 píxeles (`exams::tex2image(resize = 650)`): entre 201 y 254 ppi. Es el
  aviso de C5 en el examen real. Subirlo es cosa de cada ejercicio: `include_tikz(..., resize = 1200)`.

**Decisiones del profesor del 2026-11-14** (respondidas en la sesión, con las mediciones a la vista)

- **Letra de las opciones gráficas: arriba.** LaTeX apoya por abajo una imagen que empieza un renglón, así que la letra de
  una opción que es una figura o una tabla quedaba al pie de ella y pegada a la figura siguiente: en la página 3 del examen
  real, la «c)» se leía con la gráfica de la opción d). Ahora su borde superior se alinea con la letra (`valign=t`, solo
  cuando la figura o la tabla es lo primero de una opción). Las páginas sin opciones gráficas quedaron idénticas píxel a
  píxel; la retroalimentación no cambia (103 páginas, 0 distintas), porque allí la letra ya iba sola en su renglón.
- **Tope de tiempo del pre-vuelo: 60 + 10 × N segundos por ejercicio**, en lugar de los 30 + 5 × N del PRP. Con N = 25,
  `I_1796473-Opc-A2.Rmd` (una tabla TikZ y tres gráficas de matplotlib por versión) tardó 143,1 y 141,4 s de los 155 s
  que tenía: unos 5,7 s por versión. Desde unas 44 versiones lo habría cortado sin estar colgado.
- **Plantillas oficiales `solpcielo.tex` y `solpcielo_cloze.tex`: corregidas** con las mismas listas propias. Medido antes
  de tocarlas: tres preguntas salían numeradas 1, 5 y 6, y una lista «3., 4.» del enunciado salía como «a), b)». Después:
  1, 2 y 3, y la lista con sus números. Los cinco ejercicios de los tres subproyectos que copian esas plantillas
  (`serie-seno-aproximacion-n3`, `diagrama-venn-juegos-mesa-n3` y `vueltas-llanta-perimetro-n4`, SCHOICE y CLOZE) dan
  PDF idénticos píxel a píxel con la plantilla anterior y con la nueva.
- **Partir las preguntas: sí, por defecto.** Instrucción: «Priorizar partirlas, teniendo en cuenta la LEY de acomodar la
  mayor cantidad de preguntas respetando los estándares de calidad visual». `partir_preguntas` vale `TRUE` si la
  configuración no lo nombra; con `FALSE`, cada pregunta va en una caja que `multicols` no puede partir. Medido:

  | Examen | Partiendo (por defecto) | Enteras (`partir_preguntas = FALSE`) |
  |---|---|---|
  | Real, 11 preguntas × 3 versiones | 4 caras en las tres versiones; exit 0 | 6, 4 y 6 caras; exit 0, ninguna pregunta partida |
  | `venn_12_versiones.R`: 12 versiones de un ítem con cuatro diagramas | 8 caras (7 con contenido); exit 0 | 8 caras; **exit 1**: la pregunta no cabe en la primera página y la tinta llega a 7,0 mm del borde inferior (C3) |

  Añadir la opción no cambió el cuadernillo por defecto: quedó idéntico píxel a píxel al anterior (4 páginas, 0 píxeles
  distintos).
- **Cuadernillo del docente junto con el del estudiante.** Instrucción: «cuando solicite impresión a doble columna, se
  generen en simultáneo la versión para el estudiante (sin retroalimentación) y la versión para el docente (con
  retroalimentación)». Es la misma plantilla con `\Docente{1}`: muestra la Solution debajo de cada pregunta, bajo el
  título «Retroalimentación», pone la clave en la primera página y «DOCENTE» en el encabezado de todas. Sale de las mismas
  semillas, así que cada pregunta es la que recibe el estudiante con ese número de versión. En el examen real ocupa
  8 caras (4 el del estudiante) y añade una etapa como la del cuadernillo (54,0 s; la del estudiante, 54,2 s). La paginación no coincide con la
  del estudiante, porque la retroalimentación ocupa espacio: coinciden el número y el orden de las preguntas.

**Sobre las guardas**

- **C3 tenía un falso verde, de dos maneras.** El `.log` de LaTeX no es UTF-8. Con `options(encoding = "native.enc")`,
  `gregexpr(perl = TRUE)` solo avisa y no devuelve coincidencias; con `"UTF-8"` (la que fija el `.Rprofile` de la raíz del
  repo), `readLines()` corta la lectura en el primer byte inválido. Sobre el log real del 2026-11-13, con tres `Overfull`
  de hasta 58,6 pt, la lectura de antes veía 0 o 1 según desde dónde se lanzara. Ahora se leen los bytes (`readBin`).
- **C3 mide también la tinta.** Una pregunta entera más alta que la columna no produce ningún `Overfull \hbox`, que es
  lo que C3 buscaba en el log: se sale por abajo. Cada página de los dos cuadernillos se rasteriza a 40 ppp y se mide
  dónde termina la tinta; a menos de 10 mm del borde inferior (el margen es de 15 mm) es `FAIL`. El ejercicio de prueba
  `m_alta` (un enunciado largo y una figura de 27 cm) lo ejercita en los dos sentidos: partida cabe, entera se sale.
- **C14 y el ancla de inicio.** Con las preguntas enteras, el ancla `qini.N` puesta después de `\item` se quedaba en la
  columna anterior cuando la pregunta saltaba completa a la siguiente, y la pregunta parecía partida: un falso `FAIL`.
  El ancla va ahora dentro de la etiqueta del número, que viaja con la pregunta. El mutante M15 se rehízo para
  conservarla, y muere por lo que debe: «números impresos 1,3 (esperado 1..2)».
- **C15 y sus testigos.** Lo que no debe llegar al estudiante es el texto de la Solution, no la palabra
  «Retroalimentación». El motor guarda en `firmas.rds`, por pregunta, hasta 3 trozos de 20 a 30 letras de su Solution que
  no están en ningún enunciado del examen. El verificador lee el texto de cada PDF y exige que ninguno esté en el del
  estudiante, y que al menos uno de cada pregunta esté en el del docente y en la retro. Cada elemento de la lista de la
  Solution se corta por separado: un trozo que cruzaba dos elementos dio un falso `FAIL`, porque en el PDF va entre ellos
  la letra de la opción. En el examen real hay testigos en 33 de 33 preguntas. Sin testigos, `NO_VERIFICABLE`.
- **C4 y las fuentes Type 3.** poppler *estima* el tamaño de una fuente Type 3. Un «×» de 10,95 pt salía como 5 pt, y
  rótulos de 5 pt en la fuente que matplotlib escribe por defecto (`pdf.fonttype = 3`) salen como 8 pt: C4 los habría
  aprobado. Ese texto no aprueba ni reprueba: deja C4 en `NO_VERIFICABLE`. Remedio en el ejercicio:
  `plt.rcParams["pdf.fonttype"] = 42`. El detalle de C4, además, ya no se corta en 12 líneas: el corte escondía los rótulos
  de 6 pt de dos gráficas detrás de 12 signos Type 3.
- **C5 sustituye la «reducción ≥ 0,7» del §6**: los ppi efectivos ya incorporan la reducción y el texto de las figuras
  vectoriales se mide con C4. En el Word, un PNG de knitr a 300 ppp queda a 299,0 ppi porque pandoc redondea la resolución
  hacia abajo: el umbral es 300 con 1 % de tolerancia.
- **C6**: la única normalización del texto es la de las figuras (`FIG`), porque el mismo ejercicio trae `.pdf` en
  `exams2pdf()` y `.png` en el Word. No hizo falta ninguna otra: las cuatro salidas coinciden (33 de 33).
- **C8** mide tinta (la cara rasterizada tiene un solo color), no texto: la plantilla de NOPS deja en la cara 2 un punto
  invisible que `pdftotext` devuelve como «.».
- **C12**: knitr 1.51 restaura `opts_chunk`, `opts_knit` y `knit_hooks` al terminar cada documento; `knit_engines` no. Por
  eso el escaneo estático solo reprueba `knit_engines$set(` (salvo la restauración `python = reticulate::eng_python`); las
  otras tres llamadas se informan y las vigila la comparación del estado. Cargar un paquete puede AÑADIR motores (`glue`
  añade 3): se informa y no es fuga; sí lo es que un motor que ya existía cambie o desaparezca.

### 10.7 Límites conocidos

- **Suplementos.** `include_supplement()` busca en el directorio de trabajo, no junto al `.Rmd` (`xexams()` usa `getwd()`
  si no recibe `edir`). Un ejercicio que lo use solo funciona si el motor se lanza desde la carpeta de ese ejercicio.
  Ninguno de los 33 `.Rmd` de los exámenes lo usa; en `A-Produccion/` lo usan 5 de 452.
- **Figuras TikZ en el Word**: entre 201 y 254 ppi (aviso de C5). El PDF las lleva vectoriales.
- **Ejercicios muy lentos.** El tope del pre-vuelo (60 + 10 × N segundos por ejercicio) cubre con holgura al más pesado
  medido (5,7 s por versión). Un ejercicio que tarde más de 10 s por versión necesita su `presupuesto_s` en la configuración.
- **Preguntas enteras.** Con `partir_preguntas = FALSE`, una pregunta más alta que la columna (30,3 cm; 27,8 cm en la
  primera página) no cabe, y el motor no la reduce: C3 falla y hay que volver al valor por defecto. Además gasta más
  papel: 6 caras en lugar de 4 en dos de las tres versiones del examen real.
- **Lista numerada en un enunciado** (Error 45). En el Word, la lista del ejercicio sale con letras («a., b.») en lugar de
  sus números; el cuadernillo, el del docente y la retroalimentación la conservan. La hoja NOPS no se ve afectada, porque
  del NOPS solo se usa la hoja. Ninguno de los 33 `.Rmd` de los exámenes trae una lista numerada en el enunciado.
- **Imágenes dentro de una celda** de una tabla del Word: no se reducen a la celda, solo a la columna.
- **C10 es manual.** El verificador deja las hojas en gris; alguien tiene que mirarlas.
- **Escaneo**: probado solo en simulación (§10.5).
- **Copias viejas de `solpcielo.tex`.** En el repo hay 144 archivos `solpcielo*.tex`. Solo las dos oficiales y la plantilla
  local del SCHOICE del diagrama de Venn llevan la corrección de la numeración; 131 copias ya eran versiones anteriores
  (34 de ellas en `03-En-Produccion/`, que no se toca) y 10 eran copias idénticas de la oficial anterior. Se rehacen en el
  próximo ciclo de cada ejercicio (regla #26).
