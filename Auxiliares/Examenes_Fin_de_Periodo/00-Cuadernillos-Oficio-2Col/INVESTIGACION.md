# Cuadernillos Saber 11 Matemáticas en oficio a doble columna: investigación y propuesta

Fecha: 2026-11-13. Fases 0 a 3 del prompt optimizado. **No hay nada implementado**: hay un prototipo medido en
`prototipo/` y una propuesta que espera la aprobación del profesor. Las carpetas `01-`, `02-` y `04-`,
`A-Produccion/perifericos/` y las reglas de `.claude/` no se modificaron.

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

## 5. Arquitectura propuesta

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

## 7. Decisiones abiertas para el profesor

1. **Formato de la retroalimentación digital.** Un PDF de una columna en tamaño pantalla (se ve en el celular sin
   internet y se comparte en WhatsApp), o el HTML autocontenido de `webquiz_autocontenido.R`.
2. **Etiqueta «Escenario:»**. La plantilla oficial la pone en cada pregunta y cuesta una línea por pregunta. El ICFES
   no la usa. ¿Se conserva?
3. **Defectos heredados del §4**: ¿se corrigen en las copias de `01-`, `02-` y `04-` antes de usarlas, o solo se
   bloquean con las guardas C6 y C9?
4. **Figuras de matplotlib con rótulos < 7 pt**: ¿se regeneran en TikZ todas las que marque C5, o pregunta a pregunta?
   Para los ejercicios de `A-Produccion/`, regenerar una figura pasa por el Flujo B (reglas #2 y #3).
5. **Promoción a `perifericos/`**: si la plantilla nueva funciona en el aula, ¿pasa a ser versión oficial (regla #26)?
6. **Letra**: Helvetica (la de la plantilla oficial) o una sans más cercana a la Tahoma del ICFES.

---

## 8. Cómo reproducir

```bash
# desde una carpeta con copias de los .Rmd en ej/ y las plantillas del prototipo al lado
Rscript prototipo/pruebas/prototipo.R
python3 -I prototipo/medir_pdf.py tex8/proto8_1.pdf --log tex8/proto8_1.log --columnas 2
Rscript prototipo/pruebas/poc_nops.R      # tabla texto= clave= repro= por versión y pregunta
Rscript prototipo/pruebas/poc_docx.R      # Word en oficio a dos columnas
```

El medidor reconoce el inicio de cada pregunta por «Escenario:» y el fin por la última opción «d)». Si se quita la
etiqueta (decisión 2), hay que darle otra marca de inicio (`--inicio`).

---

## 9. Dónde ver las muestras

| Carpeta | Qué hay | Estado de los `.Rmd` |
|---|---|---|
| `muestras/` | Examen real de 11 preguntas (de los exámenes 01, 02 y 04) en 3 versiones: `cuadernillo_version_*.pdf` (oficio, 2 columnas) y su hoja NOPS `hoja_respuestas_version_*.pdf` con el mismo número de versión; `hojas_version1_color.png` y `hojas_version1_gris.png` (las 4 caras de la versión 1, en color y como saldrían en la impresora en blanco y negro) | **Después** de todas las correcciones (§4) |
| `prototipo/evidencia/` | Línea base con la plantilla oficial (`linea_base_8preg_oficial.pdf`) frente al prototipo a 2 columnas (`prototipo_8preg_2col.pdf`, `prototipo_10preg_2col.pdf`), Word en oficio (`word_oficio_2col.docx` y su conversión con LibreOffice), torta en color frente a gris | **Antes** de corregir los `.Rmd` (sirve para comparar) |
| `prototipo/pruebas/` | Scripts que generan todo lo anterior; `prueba_examen_real.R` es el que produce `muestras/` | — |

`muestras/` se regenera con `prototipo/pruebas/prueba_examen_real.R`, desde una carpeta con copias de los `.Rmd` en `ej/`
y la plantilla `cuadernillo_oficio_2col.tex` al lado.
