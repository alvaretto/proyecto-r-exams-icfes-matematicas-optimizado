---
name: "PRP — Motor de cuadernillos Saber 11 Matemáticas en oficio a doble columna (cuadernillo + hoja NOPS + clave + retroalimentación + Word)"
fecha: 2026-11-13
entrada: Auxiliares/Examenes_Fin_de_Periodo/00-Cuadernillos-Oficio-2Col/INVESTIGACION.md (§5 arquitectura aprobada, §6 criterios C1-C11)
decision_del_profesor: "Aprobado" (arquitectura de §5), 2026-11-13, más las decisiones de la tabla «Decisiones del profesor»
compuerta_previa: /adversario sobre este PRP antes de /execute-prp
confianza_one_pass: 7/10
---

## Core Principles
1. **Contexto completo**: todo lo que el agente necesita está aquí o en una ruta citada.
2. **Bucles de validación**: cada tarea cierra con un comando ejecutable. Cada guarda del verificador lleva **su mutante**, que debe hacerla fallar.
3. **Medir, no suponer**: cada decisión de este PRP sale de una medición de la sesión del 2026-11-13. Si en la implementación una medición contradice el PRP, gana la medición y se declara.
4. **Reglas globales**: `CLAUDE.md` global y del repo.
   - Subagentes sin `name:`.
   - Ediciones en lote, con script.
   - Nada de `--no-verify`, `git stash` ni `checkout`, que descartan cambios ajenos.
   - Commit solo si el profesor lo pide.

---

## Goal

Un **motor reproducible** en `Auxiliares/Examenes_Fin_de_Periodo/00-Cuadernillos-Oficio-2Col/`. Desde un archivo de configuración con la lista de `.Rmd`, produce para un grupo de N estudiantes (una versión por estudiante, N ≤ 100), y por cada sesión, cinco salidas sacadas de **una sola matriz de semillas**:

1. **Cuadernillo** por estudiante: PDF en oficio de 216 × 330 mm, dos columnas, Helvetica a 10 pt, encabezado compacto con el número de versión, número par de caras. Además, un PDF combinado para imprimir de una vez.
2. **Hoja de respuestas NOPS** por estudiante, solo la hoja, centrada en oficio. Su ID es el mismo número de versión del cuadernillo. Se conserva el `.rds` para `nops_eval()`.
3. **Clave por versión**: CSV y PDF.
4. **Retroalimentación** por versión: PDF tamaño celular, a una columna, con la Solution, para compartir por WhatsApp.
5. **Word** por versión: `.docx` en oficio a dos columnas, sin Solution, para retocar e imprimir.

Además: un **verificador** (`verificar_cuadernillos.R`) con las guardas C1-C12 y sus mutantes, una **suite** en `tests/run_all_tests.R` y la **documentación de las reglas** que cambian (#1 de `codigo-rmd.md` y #26). Al final, la **promoción** de las plantillas a `A-Produccion/perifericos/` como versión oficial.

## Why
- El profesor imprime exámenes de fin de periodo y simulacros (2 sesiones de 25 preguntas) en oficio, a doble cara y en blanco y negro. Hoy, con la plantilla oficial a una columna, 8 preguntas ocupan 5 caras con líneas de ~115 caracteres. El prototipo medido las deja en 2 caras con ~56 caracteres por línea.
- La hoja NOPS y el cuadernillo propio solo coinciden si comparten la matriz de semillas. Se midió: 33 de 33 pares iguales en texto y clave. Hoy no hay ninguna herramienta que garantice eso.
- La sesión del 2026-11-13 encontró defectos que ningún gate del repo veía:
  - la fuga del motor `python` entre ejercicios;
  - `exams2nops(twocolumn = TRUE)` falla con tablas;
  - figuras rasterizadas a 183 ppi.
  El verificador los convierte en guardas.

## What

### Decisiones del profesor (no se reabren)

| Tema | Decisión |
|---|---|
| Papel | Oficio de 216 × 330 mm (no legal) |
| Columnas, letra | 2 columnas; Helvetica de 10 pt (como la plantilla oficial) |
| Integridad | **Se permite partir** una pregunta entre columnas o páginas |
| Etiqueta «Escenario:» | **Se quita**: la pregunta empieza con su número |
| Primera página | Encabezado compacto (los datos del estudiante van en la hoja NOPS) |
| Impresión | Doble cara, blanco y negro: número par de caras por cuadernillo; figuras distinguibles en gris |
| Respuestas | Hoja NOPS **aparte**, generada con `twocolumn = FALSE`. Es una **excepción declarada a la regla #26**, que hoy pide `twocolumn = TRUE` para el NOPS con opciones gráficas. Aquí el PDF de `exams2nops` solo aporta la hoja (las preguntas van en el cuadernillo propio), y `twocolumn = TRUE` falla con tablas Markdown («longtable not in 1-column mode», MEDIDO). La Task 10 amplía esa línea de la regla |
| Versiones | **Una por estudiante**, como máximo 100 (regla #26) |
| Tamaño | Fin de periodo, o simulacro de **2 sesiones de 25 preguntas**, cada una con su matriz y su NOPS |
| Figuras | TikZ **vectorial** con `format = match_exams_device()` (excepción a la regla 1 de `codigo-rmd.md`, aprobada) |
| Retroalimentación | **PDF tamaño celular**, una por versión. No se imprime |
| Profesor | Clave por versión + retroalimentación completa |
| Word | Réplica del PDF (oficio, 2 columnas) para retocar e imprimir |
| Promoción | Al terminar el proyecto, las plantillas pasan a `A-Produccion/perifericos/` como **archivos nuevos** (no se modifica ninguno existente). **Sin condición de aula**: el profesor lo respondió el 2026-11-13 a la pregunta «¿Cuándo pasa la plantilla nueva a ser la versión oficial en A-Produccion/perifericos/?», que ofrecía «Después de usarla en el aula (Recomendado)», «Ya, al terminar el proyecto» y «Nunca». Su respuesta: «Ya, al terminar el proyecto». Con eso queda cerrado el §7.5 de INVESTIGACION.md |
| Fuera de alcance | `Auxiliares/Examenes_Fin_de_Periodo/Copia de 04-Examen-Fin-de-Periodo-4/` no se toca |

### Success Criteria (las guardas del verificador)
- [ ] **C1** Papel:
  - cuadernillos y hojas de respuestas a 216 × 330 mm (± 1 mm);
  - retroalimentación a 90 × 160 mm (± 1 mm);
  - Word con `w:pgSz` 12246 × 18709 twips (± 5) y `w:cols w:num="2"`, y **ningún «##»** en `word/document.xml`: un marcador de la plantilla sin sustituir es FAIL.
- [ ] **C2** El 100 % de las fuentes están incrustadas (`pdffonts`), en cuadernillos, hojas y retroalimentación.
- [ ] **C3** No hay `Overfull \hbox` de más de 1 pt en los `.log` de los cuadernillos **ni de la retroalimentación** (columna de 82 mm: una tabla ancha se nota aquí).
- [ ] **C4** Texto vectorial de 7 pt o más (`pdftohtml -xml -zoom 1`), en **cuadernillos y retroalimentación**. Una racha de 1 o 2 caracteres entre 5,5 y 7 pt (exponentes, subíndices) es **WARN** y se lista, no FAIL.
- [ ] **C5** Imagen raster con 300 ppi efectivos o más, en **cuadernillos y retroalimentación** (`pdfimages -list`) **y en el Word**. En el Word se mide con los píxeles de cada imagen de `word/media/` frente a su ancho en `document.xml`: ppi = px / (cx / 914400), con cx en EMU. El Word se genera con `exams2pandoc(resolution = 300)`, porque el valor por defecto es 100. Entre 150 y 299 ppi es WARN y se lista; por debajo de 150 es FAIL. Este criterio y C4 SUSTITUYEN la «reducción ≥ 0,7» del §6 de INVESTIGACION.md: los ppi efectivos ya incorporan la reducción, y el texto de las figuras vectoriales se mide directamente. Se declara en §10.
- [ ] **C6** Reproducibilidad: para cada versión y pregunta, la huella del texto (`question` + `questionlist`) y la clave (`metainfo$solution`) son **idénticas** en las cuatro corridas: NOPS, cuadernillo, retroalimentación y Word. La huella se calcula sobre el texto NORMALIZADO: antes de `digest()`, cada referencia de figura (`\includegraphics[…]{archivo.ext}` o `![](archivo.ext){…}`) se sustituye por «FIG». Lo midió el adversario: el mismo ejercicio TikZ trae `fig.pdf` en `exams2pdf` y `fig.png` en `exams2pandoc`, así que sin normalizar C6 fallaría siempre. El «33 de 33» comparó solo NOPS con PDF: el Word se mide por primera vez aquí. Si C6 falla SOLO en la columna del Word por una diferencia de formato y no de contenido, se mide esa diferencia y se añade a una LISTA CERRADA de normalizaciones. Cada entrada lleva su guarda en el test: debe aplicarse a esa diferencia y no ocultar un número cambiado. Se declara en §10. Cualquier otra diferencia es FAIL.
- [ ] **C7** El ID impreso en cada cuadernillo y en su retroalimentación es igual al ID de su hoja NOPS (`names()` del objeto de `exams2nops`), en el 100 % de las versiones.
- [ ] **C8** Cada cuadernillo tiene un número par de caras. **Cada hoja de respuestas tiene exactamente 2 caras**: la hoja y su reverso en blanco. Así el PDF combinado se imprime a doble cara sin que la hoja de un estudiante caiga al reverso de la de otro. MEDIDO: con `duplex = TRUE`, la página 2 del PDF de `exams2nops` sale en blanco. C8 comprueba que `pdftotext` de esa cara no trae texto.
- [ ] **C9** Tope de tiempo en el pre-vuelo: un bucle sin tope no cuelga la generación, y se nombra el ejercicio. El pre-vuelo es UN proceso hijo en segundo plano (`processx::process`, o `system2(wait = FALSE)` guardando su PID) y un VIGILANTE en el padre que lee el log de progreso cada 2 s. Si un «INICIO j» pasa de `presupuesto_s` sin su «FIN j», el vigilante mata al hijo y nombra el ejercicio j. `presupuesto_s` = `config$presupuesto_s` si existe y si no 30 + 5 × N, **por ejercicio**. Así un cuelgue se corta en ≤ `presupuesto_s`, no en Q × `presupuesto_s`. El tope total de respaldo (Q × `presupuesto_s`) solo cubre un fallo del vigilante.
- [ ] **C10** Hoja de contacto en gris de la versión 1, para revisión **visual**. Estado MANUAL: el verificador imprime «REVISIÓN VISUAL PENDIENTE: <ruta>» y nunca cuenta C10 como PASS. El exit 0 del verificador certifica C1-C9, C11 y C12. Muestra y criterio: hojas en gris a 50 ppp de DOS versiones, la primera y la última (`revision_gris_v1.png` y `revision_gris_vN.png`). C10 pasa si en cada figura las categorías o regiones se distinguen entre sí en gris, en la hoja de contacto a 50 ppp, y si los rótulos se leen en las páginas en gris a 150 ppp (`revision_gris_v1_p<K>.png` y `revision_gris_vN_p<K>.png`). A 50 ppp un rótulo de 7 pt mide unos 5 px y no se puede leer. El coordinador lo revisa con Read, figura por figura, y lo declara en su reporte de aprobación (Task 13): C10 se cierra SOLO ahí, con la lista de figuras revisadas. El verificador no cambia su salida: verificacion.txt sigue diciendo «PENDIENTE», y es lo esperado. La aceptación final es del profesor, en papel.
- [ ] **C11** Límites:
  - entre 1 y 100 estudiantes;
  - como máximo 45 preguntas por sesión (límite NOPS);
  - en una sesión con NOPS, solo `schoice` o `mchoice` y **como máximo 5 opciones por pregunta** (la hoja NOPS tiene casillas a-e);
  - `dir_salida` pasa la guarda de ruta: dentro de una carpeta «salida», y para `--limpiar`, con el marcador del motor (ERR_DIR_SALIDA, Task 3);
  - todo se comprueba en el pre-vuelo, antes de la primera llamada costosa.
- [ ] **C12** Estado global de knitr: ningún ejercicio cambia el estado de knitr para los siguientes. Se comprueba de dos formas:
  - **escaneo estático** (regex sobre el `.Rmd`);
  - **instantánea dinámica** en el pre-vuelo, con LÍNEA BASE NORMALIZADA.
    - Antes del primer ejercicio, el hijo hace `knitr::knit_engines$set(python = reticulate::eng_python)`. Así, un ejercicio que solo RESTAURA ese motor (la excepción permitida, que usan los 5 I_1796473) no cambia nada.
    - Lo midió el adversario: el `deparse()` del motor python de knitr es distinto del de `reticulate::eng_python`. Sin normalizar, C12 fallaría en esos 5.
    - Compara solo los componentes que un **control negativo** demuestre estables. El control son los fixtures limpios más uno que solo restaura el motor python.
    - Componentes: `names()` de los motores, el **código** de cada motor (`deparse()`, sin su entorno) y `opts_chunk$get()` sin las claves volátiles que mida ese control.
    - El código de los motores es el componente que sostiene la guarda: sobre él actúa el mutante indirecto.
- [ ] Cada guarda C1-C9, C11 y C12 tiene un mutante que la hace fallar; `verificar_cuadernillos.R --mutantes` sale con 0 solo si mueren todos. **C12 lleva dos mutantes**: uno que el escaneo estático ve y otro indirecto que solo ve la instantánea. El indirecto reemplaza el motor python con `do.call`, y se detecta por el código del motor aunque `opts_chunk` resulte volátil. Un mutante que muere por un `stop()` con el código de su guarda en el mensaje (p. ej. M11, en `leer_config`) cuenta como muerto.
- [ ] El ejemplo real (las 11 preguntas de `muestras/`, 3 versiones) pasa C1-C9, C11 y C12, con C10 revisado y declarado. `muestras/` se regenera con lo que el motor escribe en `salida/regen/sesion-1/`, que la Task 9 copia: el motor nunca escribe en `muestras/`, porque esa ruta no pasa la guarda.
- [ ] La suite `tests/testthat/test_cuadernillos_oficio.R` pasa y está registrada con su `watch`; `Rscript tests/sincronizar_doc_suites.R --check` sale con 0.
- [ ] Las reglas compacta e íntegra de #1 (`codigo-rmd`) y #26 (`material-aula`), `CHANGELOG_CLAUDE_MD.md` y la versión de `.claude/CLAUDE.md` están actualizadas; `test_infraestructura_claude.R` y `test_presupuesto_contexto_reglas.R` pasan.

---

## All Needed Context

### Documentation & References
```yaml
- file: Auxiliares/Examenes_Fin_de_Periodo/00-Cuadernillos-Oficio-2Col/INVESTIGACION.md
  why: >
    §2 línea base frente a prototipo (cifras); §3 hallazgos con fuente o medición (multicols, longtable, adjustbox,
    match_exams_device, NOPS, Word); §4 corrección de los 33 .Rmd; §5 arquitectura aprobada; §6 criterios; §9 muestras.

- file: Auxiliares/Examenes_Fin_de_Periodo/00-Cuadernillos-Oficio-2Col/prototipo/cuadernillo_oficio_2col.tex
  why: >
    Base de la plantilla final, YA MEDIDA (0 Overfull, flotantes [H], sin pies, longtable→tabular, adjustbox, número par
    de caras). Cambios pendientes: quitar «Escenario:», añadir anclas qini/qfin, macros de encabezado (\Titulo,
    \Institucion, \Sesion), quitar \exmchoice/\extext, que no se usan.

- file: Auxiliares/Examenes_Fin_de_Periodo/00-Cuadernillos-Oficio-2Col/prototipo/pruebas/prueba_examen_real.R
  why: >
    Patrón medido de una sola matriz de semillas: exams2nops(seed = S) y exams2pdf(seed = S, header = list(ID = id)), más
    la comparación de firmas. Es el núcleo de C6 y C7. Dio 33 de 33 pares.

- file: Auxiliares/Examenes_Fin_de_Periodo/00-Cuadernillos-Oficio-2Col/prototipo/medir_pdf.py
  why: >
    Medidor de C1-C5 y C8 (papel, raster_ppi, texto_min_pt, fuentes, Overfull). Hay que añadirle el modo de anclas
    (pdfinfo -dests), porque la etiqueta «Escenario:» desaparece.

- file: Auxiliares/Examenes_Fin_de_Periodo/00-Cuadernillos-Oficio-2Col/prototipo/referencia_oficio_2col.docx
  why: >
    reference.docx con sectPr 12246×18709 twips, 2 columnas y filete. MEDIDO: pandoc 3.11 lo respeta (§3.4). Falta la letra
    sans (Arial/Liberation Sans) en Normal y docDefaults.

- file: A-Produccion/perifericos/solpcielo.tex
  why: Plantilla oficial de la que deriva todo (oficio, Helvetica, guard none, \pandocbounded). NO se modifica.

- file: A-Produccion/perifericos/SemilleroUnico_v2.R
  why: >
    Convenciones de semillero: .exams_generation_mode <- TRUE, stopifnot(copias <= 100), exams2nops(language = "es",
    institution = ..., duplex = TRUE), webquiz_autocontenido vía git rev-parse.

- file: A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/verificar_espacio.R
  why: >
    PATRÓN de verificador con secciones, mutantes por reemplazo exacto en COPIAS ("MUTANTE MAL CONSTRUIDO" si el patrón no
    aparece exactamente 1 vez, líneas 742-760 y 861-925) y salida por función pura decidir_exit().

- file: .claude/scripts/validar_stresstest.R
  why: >
    PATRÓN de proceso hijo vigilado por `timeout` (setTimeLimit no corta Python ni system()); códigos ERR_/WARN_; exit
    0/1/2 (2 = NO VERIFICABLE, nunca PASS).

- file: .claude/rules/material-aula.md  +  .claude/docs/reglas/material-aula.md
  why: >
    Regla #26: oficio, tope de 100, NOPS centrado con pdfjam (snippet exacto en el texto íntegro, «NOPS en oficio»),
    perifericos/ oficial en disco. Se AMPLÍA en la Task 10 y, tras la promoción, en la Task 12b.

- file: .claude/rules/codigo-rmd.md  +  .claude/docs/reglas/codigo-rmd.md (líneas 15-56)
  why: Regla 1 (format = "png"). Se añade la excepción match_exams_device() con su tabla medida (Task 10).

- file: tests/run_all_tests.R (líneas 104-190)
  why: Formato de registro de suites: list(nombre, archivo, critico, watch). En modo quick, una suite con watch se salta si nada cambiado coincide.

- file: tests/sincronizar_doc_suites.R
  why: >
    Regenera la tabla de suites de .claude/docs/VALIDATOR_COBERTURA_ICFES.md; test_doc_suites_sincronizada.R falla si
    diverge. CUIDADO (MEDIDO el 2026-11-13 con git diff --stat): ese .md trae 69 líneas AJENAS sin commit y
    tests/run_all_tests.R trae 6. Al registrar la suite, editar solo lo propio. NO se hace commit durante la construcción. En el cierre
    (Task 13) se OFRECE un commit con solo los hunks propios de los dos archivos (git hash-object -w + git update-index
    --cacheinfo, memoria «preferencias del usuario», punto 5). Nunca git stash ni git checkout sobre ellos. Mientras haya cambios ajenos, un --check en rojo puede venir de ellos: comparar contra HEAD
    antes de culpar a la suite nueva.

- url: https://rdrr.io/cran/exams/man/exams2nops.html
  why: Argumentos seed, startid, date, duplex, twocolumn, language, reglength; el .rds para nops_eval.
- url: https://rdrr.io/cran/exams/man/exams2pdf.html
  why: template (ruta), header (lista de cadenas o funciones del índice), texdir, seed.
- url: https://rdrr.io/cran/exams/man/exams2pandoc.html
  why: options (se pasan a rmarkdown::pandoc_convert), template, solution = FALSE.
- url: https://rdrr.io/cran/exams/man/match_exams_call.html
  why: match_exams_device() (pdf en exams2pdf/exams2nops; png en pandoc/html; svg o png en moodle según svg =).
- url: https://ctan.csail.mit.edu/macros/latex/required/tools/multicol.pdf
  critical: «only page-wide floats (i.e., star-forms) can be used within the scope of the environment».
- url: https://ctan.csail.mit.edu/macros/latex/required/tools/longtable.pdf
  critical: «longtable now issues an error if started in the scope of \twocolumn, or the multicols environment».
- url: https://ctan.csail.mit.edu/macros/latex/contrib/adjustbox/adjustbox.pdf
  section: §4.3 max width («if the constraint is not already met, otherwise the content is unchanged»).
- url: https://pandoc.org/MANUAL.html  (opción --reference-doc)
  critical: El writer docx copia el sectPr del reference (src/Text/Pandoc/Writers/Docx.hs, extractPageLayout). MEDIDO con 3.11.
```

### Current Codebase Tree (relevante)
```bash
Auxiliares/Examenes_Fin_de_Periodo/
├── 00-Cuadernillos-Oficio-2Col/
│   ├── INVESTIGACION.md
│   ├── muestras/            # 3 cuadernillos + 3 hojas de respuestas + hojas de contacto (examen real)
│   └── prototipo/
│       ├── cuadernillo_oficio_2col.tex   referencia_oficio_2col.docx   medir_pdf.py
│       ├── evidencia/       # antes/después (línea base, prototipo, Word, torta en gris)
│       └── pruebas/         # linea_base.R  prototipo.R  poc_nops.R  poc_docx.R  prueba_examen_real.R
├── 01-ExamenFinPeriodo-1/   02-Examen_Fin_de_Periodo_2/   04-Examen-Fin-de-Periodo-4/   # 33 .Rmd corregidos (db6d9cc3)
└── Copia de 04-Examen-Fin-de-Periodo-4/   # NO TOCAR
A-Produccion/perifericos/    # oficial en disco: pcielo*.tex  solpcielo*.tex  Semillero*.R
tests/run_all_tests.R  tests/sincronizar_doc_suites.R  tests/testthat/
```

### Desired Codebase Tree
```bash
Auxiliares/Examenes_Fin_de_Periodo/00-Cuadernillos-Oficio-2Col/
├── generar_cuadernillos.R        # NUEVO. CLI: Rscript generar_cuadernillos.R <config.R> [--salidas cuadernillo,hoja,clave,retro,word]
│                                 #   [--dir-salida <ruta>] [--limpiar]
├── verificar_cuadernillos.R      # NUEVO. CLI: Rscript verificar_cuadernillos.R <dir_salida> | --mutantes
├── configuraciones/
│   ├── examen_real_11.R          # NUEVO. Las 11 preguntas de muestras/ (3 estudiantes), el ejemplo de referencia
│   ├── simulacro_2x25.R          # NUEVO. Humo de 2 sesiones de 25 (los 11 repetidos), N = 2: IDs entre sesiones
│   └── capacidad_1x25.R          # NUEVO. Capacidad: 1 sesión de 25 con N = 25 (una vez; 2 × 25 se extrapola)
├── plantillas/                   # NUEVO (Task 1): cuadernillo_oficio_2col.tex, retro_celular.tex, pandoc_cuadernillo.tex,
│                                 #   referencia_oficio_2col.docx. Fuente única hasta la Task 12; desaparece al promover
├── pruebas/fixtures/             # NUEVO. .Rmd sintéticos mínimos para el camino feliz y los mutantes (rápidos, sin Python salvo C12)
├── prototipo/                    # Evidencia: su .tex, su .docx, evidencia/ y pruebas/ NO se editan; el .tex y el .docx se COPIAN a plantillas/
├── prototipo/medir_pdf.py        # El único archivo de prototipo/ que SÍ se edita (--marcas dests); lo usan el motor y el verificador
├── muestras/                     # REGENERADO con el motor (Task 9)
└── INVESTIGACION.md              # §10 «Uso» (Task 11)
A-Produccion/perifericos/         # Task 12 (promoción): ARCHIVOS NUEVOS
├── cuadernillo_oficio_2col.tex   retro_celular.tex   pandoc_cuadernillo.tex   referencia_oficio_2col.docx
tests/testthat/test_cuadernillos_oficio.R   # NUEVO + registro en tests/run_all_tests.R
.claude/rules/{codigo-rmd,material-aula}.md  .claude/docs/reglas/{codigo-rmd,material-aula}.md   # MODIFICADOS
.claude/docs/CHANGELOG_CLAUDE_MD.md  .claude/CLAUDE.md (Versión/Fecha)                              # MODIFICADOS
```
**Directorio de plantillas, una sola variable:**
`DIR_PLANTILLAS <- Sys.getenv("CUADERNILLOS_PLANTILLAS", unset = <por defecto>)`.
- La leen el motor, el verificador y los mutantes.
- El valor por defecto es `00-Cuadernillos-Oficio-2Col/plantillas/` hasta la Task 12 y `<raíz git>/A-Produccion/perifericos/` desde la Task 12.
- Los mutantes que alteran una plantilla (M1, M3, M8) copian TODAS las plantillas a un tempdir, mutan la copia y lanzan el motor con `CUADERNILLOS_PLANTILLAS=<tempdir>`. Así funcionan igual antes y después de la promoción, y jamás tocan los archivos oficiales.

### Known Gotchas & Library Quirks (todos MEDIDOS el 2026-11-13 salvo indicación)
```
# CRITICAL: knitr::is_latex_output() es FALSE en los 5 pipelines de R/exams. Usar match_exams_device().
# CRITICAL: \setkeys{Gin}{max width=\linewidth} NO actúa si pandoc ya trae width= (3 Overfull medidos). Usar el
#           envoltorio: \let\includegraphicsoriginal\includegraphics
#           \renewcommand{\includegraphics}[2][]{\adjustbox{max width=\linewidth}{\includegraphicsoriginal[#1]{#2}}}
#           Medido además: width=3cm → 85,4 pt y 8cm → 227,6 pt (se respetan); 14cm → 273,8 pt = columna.
# CRITICAL: multicols descarta flotantes. Un gráfico de knitr con alt («plot of chunk …») sale como figure: \floatplacement{figure}{H}
#           (paquete float; quitar floatrow, que choca) y \AtBeginEnvironment{figure}{\renewcommand{\caption}[2][]{}}.
# CRITICAL: longtable falla en multicols. En la plantilla: \renewenvironment{longtable}[2][] → tabular dentro de
#           adjustbox{max width}, \def\endhead{} … y \def\bottomrule{} (NO \let…\relax: \noalign necesita expansión), con
#           \origbottomrule al cierre. Ya está en el prototipo y funciona con kable().
# CRITICAL: exams2nops(twocolumn = TRUE) FALLA con una tabla Markdown («longtable not in 1-column mode»). Usar twocolumn = FALSE:
#           del NOPS solo se usa la hoja.
# CRITICAL: ID de NOPS = paste0(format(date, "%y%m%d"), formatC(startid + i - 1, width = 5, flag = "0")) (Date2ID en el código de
#           exams2nops). El encabezado del cuadernillo debe usar la MISMA fórmula. Dos sesiones con la misma fecha → startid
#           distinto (sesión 1: 1; sesión 2: 101) para que los IDs no choquen.
# CRITICAL: R/exams teje TODOS los ejercicios de un examen en la MISMA sesión de R. Un .Rmd que hace knit_engines$set(python = …)
#           o opts_chunk$set(…) cambia a los siguientes. Pasó: 6 .Rmd apagaban los chunks {python} posteriores y la figura
#           desaparecía sin aviso. validar_stresstest.R (por archivo) NO lo ve. → C12.
# CRITICAL: exams2pandoc evalúa `options` DESPUÉS de cambiar de directorio (evaluación perezosa). Calcular la ruta absoluta
#           ANTES: ref <- normalizePath(...); exams2pandoc(..., options = paste0("--reference-doc=", ref)).
# CRITICAL: setTimeLimit no corta Python ni system() (validar_stresstest.R, cabecera). Un cuelgue se corta con `timeout` sobre un
#           proceso hijo, y el progreso por ejercicio se escribe a un archivo para nombrar al culpable.
# CRITICAL: exams2pdf y exams2pandoc AGREGAN el índice de copia a `name`. MEDIDO por el adversario: name = "cuadernillo_26111300001"
#           con n = 1 da cuadernillo_261113000011.pdf. Generar con name = "tmp_" y después file.rename() al nombre final de la
#           tabla de nombres (Data Models). Nunca derivar el ID del nombre de archivo de R/exams.
# CRITICAL: el mismo ejercicio TikZ trae \includegraphics{fig.pdf} en exams2pdf y fig.png en exams2pandoc (MEDIDO). Toda
#           comparación de texto entre formatos se hace sobre el texto NORMALIZADO (figuras → «FIG»). Ver C6.
# CRITICAL: deparse(knitr::knit_engines$get("python")) ≠ deparse(reticulate::eng_python) (MEDIDO). El pre-vuelo normaliza la
#           línea base a reticulate::eng_python antes de comparar estados. Ver C12.
# GOTCHA: exams2pandoc(solution = FALSE) quita la Solution del .docx (MEDIDO). El encabezado «Question» viene de la plantilla
#         plain.tex de exams (system.file("pandoc", "plain.tex", package = "exams")): usar una plantilla propia sin
#         ##Questionheader## ni bloques de Solution.
# VERIFICADO (código de exams:::make_exams_write_pdf, 2.4.5): exams2pdf escribe cada elemento de `header` como
#           \<nombre>{<valor>} en la línea «%% \exinput{header}» de la plantilla. Los valores pueden ser funciones del índice.
#           Por eso \Titulo, \Institucion, \Sesion y \Clave funcionan si la plantilla los define con \newcommand, igual que \ID
#           y \Date. R/exams NO escapa esos valores: escribe \Titulo{<valor>} tal cual (verificado en el mismo código), y un
#           «&» o un «_» rompe pdflatex. Los escapa la función escapar_latex() del MOTOR (Task 3, main()), con test unitario
#           (Task 11). En el Word (pandoc) el texto va SIN escapar. El camino feliz del test usa un título con «&» y «_».
# GOTCHA: exams2pdf compila con tinytex, que borra los .log. Para C3: texdir por versión + recompilar con pdflatex (como en el
#         prototipo), o options(tinytex.clean = FALSE). MEDIR cuál deja el .log y documentarlo.
# GOTCHA: El dispositivo pdf() de R no incrusta Helvetica (MEDIDO con plot()). C2 lo marca. Remedio en el EJERCICIO:
#         dev = "cairo_pdf" en ese chunk. Nunca un opts_chunk$set global (C12).
# GOTCHA: MPLBACKEND=Agg (Error 41) y .exams_generation_mode <- TRUE (salta test_that dentro de los .Rmd), como los semilleros.
# GOTCHA: pdftohtml redondea los tamaños a enteros: «7» puede ser 6,6 pt. C4 usa ≥ 7 sobre ese redondeo, coherente con lo medido.
# GOTCHA: .gitignore ignora **/salida/ y **/nops*.pdf. Las salidas no se comitean. Las muestras que SÍ se comitean no pueden
#         empezar por «nops» (usar hoja_respuestas_*).
# GOTCHA: el hook post-exams2-validation.sh corre FASES 2A-2O cuando un comando Bash contiene exams2* y un .Rmd existente
#         (300 s por ejercicio). El motor se ejecuta como Rscript <script> (el comando no contiene exams2): es lo previsto,
#         porque el hook valida ejercicios en desarrollo, no el motor.
# GOTCHA: el pre-commit revisa la ortografía de los .Rmd en stage. En los fixtures: comentarios con tildes y NINGÚN identificador
#         que termine en -cion/-sion (memoria feedback_gate_ortografia_identificadores_sion). Nunca --fix ni --no-verify.
# GOTCHA (zsh): `cp` es `cp -i` (usar command cp -f) y un glob sin coincidencias aborta (usar bash -c 'shopt -s nullglob; …').
# GOTCHA: exams2pdf(files, n = 1, seed = S[i, , drop = FALSE]) en un bucle por versión da el mismo texto que una llamada con
#         n = N (la semilla se aplica por ejercicio). C6 lo comprueba de todos modos: si difiere, gana la medición y se declara.
```

---

## Implementation Blueprint

### Data Models & Structure
```r
# configuraciones/<examen>.R: un archivo R que deja en el entorno una lista `config`. Rutas relativas al propio config.
# La ruta de --dir-salida (CLI), en cambio, es relativa al directorio de trabajo.
config <- list(
  titulo      = "Evaluación de Matemáticas: fin del periodo 4",
  institucion = "I. E. Pedacito de Cielo, La Tebaida, Quindío",
  fecha       = as.Date("2026-11-20"),
  estudiantes = 24L,                    # versiones, de 1 a 100 (C11)
  semilla     = 20261120L,              # semilla maestra: set.seed(semilla + s) → matriz de la sesión s
  presupuesto_s = NULL,                 # opcional: segundos por ejercicio en el pre-vuelo (C9); por defecto 30 + 5 * N
  sesiones    = list(
    list(nombre = "Sesión 1", startid = 1L,   ejercicios = c("../../04-Examen-Fin-de-Periodo-4/cateto_…_n3_v1_1.Rmd", "...")),
    list(nombre = "Sesión 2", startid = 101L, ejercicios = c("...")) ),
  salidas     = c("cuadernillo", "hoja", "clave", "retro", "word"),
  dir_salida  = "../salida/fin-periodo-4"  # relativa al config: 00-Cuadernillos-Oficio-2Col/salida/fin-periodo-4, en
                                        # **/salida/ (gitignore). Si NO está vacío, el motor se NIEGA (exit 2)
                                        # salvo con --limpiar (guarda de ruta y marcador .cuadernillos_salida, en leer_config).
                                        # La CLI --dir-salida <ruta> lo sustituye (Task 9 y repeticiones)
)

# TABLA ÚNICA DE NOMBRES (la usan el motor, el verificador, la Task 9 y los niveles de validación; ID = id_version(...)):
#   cuadernillos/cuadernillo_<ID>.pdf       (2 o más caras, número par)  + cuadernillos/cuadernillos_imprimir.pdf (pdfunite)
#   hojas/hoja_respuestas_<ID>.pdf          (EXACTAMENTE 2 caras: hoja + reverso en blanco) + hojas/hojas_imprimir.pdf
#   retro/retro_<ID>.pdf                    (90 × 160 mm)
#   word/cuadernillo_<ID>.docx
#   clave/clave.csv   clave/clave.pdf       revision_gris_v1.png (C10)
# Se generan con name = "tmp_" y file.rename(): R/exams agrega el índice de copia al name (ver Gotchas).
#
# Salida por sesión: <dir_salida>/sesion-<k>/, con k = posición de la sesión en config$sesiones (sesion-1, sesion-2). El slug
#   NO se deriva del nombre de la sesión, que lleva tildes y espacios.
#   semillas.rds / semillas.csv         matriz N × Q (filas = versiones, columnas = ejercicios)
#   firmas.rds                          list(ids, ejercicios, huella = list(nops, cuadernillo, retro, word) [N×Q chr], clave [N×Q chr])
#   preflight.log                       progreso por ejercicio (para C9) e instantáneas de knitr (para C12)
#   tex/cuadernillos/<ID>/  tex/retro/<ID>/   .tex y .log para C3 (cuadernillos y retroalimentación)
#   nops/                               PDF crudos de exams2nops + nops_.rds (para nops_eval)
#   verificacion.txt                    salida de verificar_cuadernillos.R (incluye «REVISIÓN VISUAL PENDIENTE: …»)
#   + los archivos de la tabla de nombres
```

### Tasks (in execution order)
```yaml
Task 1 — Plantillas (plantillas/):
  CREATE plantillas/cuadernillo_oficio_2col.tex:
    - MIRROR: prototipo/cuadernillo_oficio_2col.tex (medido).
    - CHANGE: \newenvironment{question}{\item\hypertarget{qini.\arabic{enumi}}{}}{\hypertarget{qfin.\arabic{enumi}}{}}  (sin «Escenario:»).
    - ADD: macros de encabezado \Titulo, \Institucion, \Sesion (mismo patrón que \ID y \Date, con \makeatletter), usadas en el
      encabezado compacto y en fancyhdr («\myTitulo · Versión \myID · Página X de Y»).
    - REMOVE: \exmchoice, \exnum, \exstring y \extext (no hay questionnaire). Grep previo: deben existir exactamente esas 4,
      y \exclozechoice NO existe en el prototipo.
    - KEEP: papersize {216mm,330mm}, multicols 2 con \columnseprule 0.2pt, el envoltorio adjustbox, float [H], los pies anulados,
      la redefinición de longtable, el guard none, \pandocbounded, secnumdepth 0, Helvetica y el bloque de número par de caras.
    - Comentario de cabecera: de dónde deriva y cada cambio frente a perifericos/solpcielo.tex (como solpcielo_una_por_pagina.tex).
  CREATE plantillas/retro_celular.tex:
    - papersize {90mm,160mm}; 11 pt; márgenes de 4 mm; UNA columna; Helvetica; el mismo envoltorio adjustbox; float [H]; sin pies.
    - question: \item con número; la Solution SE MUESTRA: \newenvironment{solution}{\par\textbf{Retroalimentación}\par}{}.
    - Encabezado en la página 1: título, «Versión \myID» y «Clave: \myClave» (macro \Clave pasada por header).
    - longtable: en una columna funciona, pero se conserva el guard none y el \LTcaptype.
  CREATE plantillas/pandoc_cuadernillo.tex:
    - PARTIR de system.file("pandoc", "plain.tex", package = "exams") SIN editarla a ojo. Sus bloques están separados por líneas
      «#-», y quitar líneas sueltas puede dejar un \item vacío o un «#-» desbalanceado.
      - Primero, con un fixture y solution = FALSE, ver qué bloques «#-» se activan.
      - Después, quitar los bloques COMPLETOS de ##Questionheader## y de la Solution (##Solutionheader##, ##Solution##,
        ##Solutionlist##), conservando la estructura «#-» del resto.
      - Al inicio, una línea con el título y la versión.
    - MEDIR ANTES de fijar la plantilla, con un fixture y exams2pandoc(n = 1, header = list(ID = "X1", Titulo = "X2")):
      qué marcadores sustituye (##ID##, ##Date## y si acepta claves arbitrarias como ##Titulo##).
      - Si no sustituye las claves arbitrarias, el título va con la clave que SÍ sustituye, o se arma en R y se pasa por ##ID##.
      - Anotar el resultado en el log de trabajo; la Task 11 lo pasa al §10 de INVESTIGACION.md.
      - El verificador (C1) falla si queda algún «##» en word/document.xml.
  CREATE plantillas/referencia_oficio_2col.docx como COPIA de prototipo/referencia_oficio_2col.docx (el prototipo no se toca):
    - styles.xml: w:rFonts ascii/hAnsi/cs = "Arial" en docDefaults y Normal; tamaño 20 (10 pt). Con script Python (zip), como el POC.

Task 2 — Medidor con anclas:
  MODIFY prototipo/medir_pdf.py:
    - ADD --marcas {texto,dests} (por defecto: dests si `pdfinfo -dests` trae qini.*; si no, texto).
    - dests: parsear líneas «<pág> [ XYZ <x> <y> null ] "qini.N"». Columna = 0 si x < ancho/2. Partida si (pág, col) de qini.N ≠ qfin.N.
      MEDIDO: el formato de salida de pdfinfo -dests es exactamente ese (prueba en multicols, 3 preguntas).
    - ADD tamaños: lista de rachas < 7 pt con su longitud (para C4 WARN frente a FAIL).
    - KEEP el modo texto («Escenario:» / «d)») para medir la plantilla oficial vieja.

Task 3 — Motor (generar_cuadernillos.R). Funciones puras y testeables; main() al final; source() seguro (sin efectos al cargar):
  - leer_config(ruta, dir_salida_cli = NULL): source en un entorno nuevo; validar campos; rutas relativas → absolutas;
      C11 (1 ≤ N ≤ 100; Q ≤ 45 por sesión). dir_salida = --dir-salida si se pasó (relativa al directorio de trabajo); si no,
      config$dir_salida (relativa al directorio del config). La base se fija ANTES de la guarda.
      GUARDA DE RUTA, antes de escribir o borrar:
      - normalizePath(dir_salida, mustWork = FALSE) debe quedar ESTRICTAMENTE dentro de una carpeta llamada «salida»: algún
        componente de la ruta es «salida» y no es el último. No hay excepción para tempdir(), porque el tempdir() de R
        cambia en cada proceso y un Rscript hijo nunca lo ve igual. Los tests usan rutas <tmp_del_test>/salida/<caso>.
      - No puede ser la raíz git, ni el directorio del motor, ni un ancestro de alguno de los dos.
      - No puede resolver a otro sitio por un enlace simbólico.
      - Normalización: si dir_salida todavía no existe, normalizePath(mustWork = FALSE) la devuelve sin resolver «..». Se
        normaliza el ancestro EXISTENTE más largo y se le añade el resto; si en el resultado queda un componente «.» o «..»,
        exit 2.
      - MARCADOR: al crear dir_salida, el motor escribe en él un archivo vacío `.cuadernillos_salida`. --limpiar solo borra
        si ese marcador existe. Un directorio que existe, no está vacío y no tiene marcador NUNCA se borra: exit 2. Así no
        cuenta como salida del motor una carpeta ajena que tenga «salida» en su ruta.
      Si no cumple → exit 2 con «ERR_DIR_SALIDA», sin borrar nada.
  - matriz_semillas(semilla, s, N, Q): set.seed(semilla + s); matrix(sample.int(1e6, N*Q), nrow = N). Guardar .rds y .csv.
  - id_version(fecha, startid, i): la fórmula Date2ID exacta (ver Gotchas). Test unitario contra names(exams2nops(...)).
  - preflight(ejercicios, S, dir):
      - UN proceso hijo `Rscript preflight_hijo.R …` lanzado en segundo plano (processx::process; o system2(wait = FALSE)
        guardando su PID).
      - Un VIGILANTE en el padre lee preflight.log cada 2 s y mata al hijo si un «INICIO j» supera presupuesto_s sin su
        «FIN j». presupuesto_s = config$presupuesto_s o 30 + 5 × N, POR EJERCICIO.
      - Respaldo: un timeout total de Q × presupuesto_s.
      - Al empezar, el hijo normaliza la línea base: knitr::knit_engines$set(python = reticulate::eng_python).
      En UNA sesión, para j en orden:
      escribe «INICIO j» en preflight.log; toma la instantánea estado(); hace xexams(ejercicio_j, n = N,
      seed = S[, j, drop = FALSE], dir = tempfile()) con el driver por defecto; toma la instantánea de nuevo; escribe
      «FIN j tipos=<…> max_opciones=<…> estado=<igual|CAMBIA:componente>».
      - Si el vigilante (o el respaldo) mata al hijo → C9 FAIL con «ejercicio = el último INICIO sin FIN». Quien lo mata lo
        escribe en preflight.log: «terminado por vigilante: ejercicio j» o «terminado por respaldo». M9 los distingue así.
      - Si cambia el estado → C12 FAIL con el nombre del ejercicio y el componente que cambió.
      - Si una sesión con NOPS trae un tipo que no es schoice/mchoice, o más de 5 opciones en alguna versión → C11 FAIL.
      Además, escaneo ESTÁTICO de C12 (regex sobre el .Rmd): knit_engines\$set\(, opts_chunk\$set\(, opts_knit\$set\(,
      knit_hooks\$set\(. La única excepción permitida es `knit_engines$set(python = reticulate::eng_python)`.
  - estado(): se CALIBRA ANTES con un CONTROL NEGATIVO.
      - Los fixtures limpios (f_texto, f_tikz, f_tabla, f_plot) y f_restaura_python, que solo hace knit_engines$set(python =
        reticulate::eng_python), se tejen en una sesión con la línea base normalizada. Se toman instantáneas antes y después de cada uno.
      - Quedan en estado() solo los componentes que ese control mantiene iguales: names(knitr::knit_engines$get()); el
        código de cada motor, vapply(knit_engines$get(), function(f) paste(deparse(f), collapse = "\n"), ""), sin el
        entorno de la closure; y knitr::opts_chunk$get() sin las claves que el control vea cambiar.
      - Las claves excluidas y la razón se escriben en un comentario del código.
      - opts_knit y knit_hooks entran SOLO si el control demuestra que son estables. Si no, quedan cubiertos solo por el escaneo estático, y se declara.
  - generar_hojas(): exams2nops(ejercicios, n = N, seed = S, startid, date = fecha, language = "es", institution, title,
      duplex = TRUE, twocolumn = FALSE, dir = nops/, name = "nops_"). Por cada versión: qpdf <pdf> --pages <pdf> 1-2 -- tmp
      (la hoja y su reverso en blanco: MEDIDO, la página 2 sale en blanco con duplex = TRUE);
      pdfjam --papersize '{216mm,330mm}' --noautoscale true → hojas/hoja_respuestas_<ID>.pdf (2 caras). Luego pdfunite →
      hojas_imprimir.pdf, que queda con un número par de caras, seguro a doble cara. Devuelve el objeto (huellas + claves).
  - clave_desde(xn): letras = LETTERS[which(metainfo$solution)] (en mchoice se unen). Escribe clave.csv (version, p01..pQ) y
      clave.pdf (tabular LaTeX compilado con pdflatex; oficio; filas = versiones).
  - generar_cuadernillos(): para i en 1..N: exams2pdf(ejercicios, n = 1, seed = S[i, , drop = FALSE],
      template = file.path(DIR_PLANTILLAS, "cuadernillo_oficio_2col.tex"), header = list(ID = id, Date = …, Titulo = …,
      Institucion = …, Sesion = …), texdir = tex/cuadernillos/<ID>, dir = <tmp>, name = "tmp_"), y después
      file.rename(<tmp>/tmp_1.pdf, cuadernillos/cuadernillo_<ID>.pdf). Al final, pdfunite → cuadernillos_imprimir.pdf.
  - generar_retro(): igual, con retro_celular.tex, texdir = tex/retro/<ID>, header Clave = clave_str[i] («1 b · 2 d · …») y
      el nombre final retro/retro_<ID>.pdf.
  - generar_word(): ref <- normalizePath(file.path(DIR_PLANTILLAS, "referencia_oficio_2col.docx")) ANTES; por versión,
      exams2pandoc(…, n = 1, seed = S[i, , drop = FALSE], type = "docx", template = pandoc_cuadernillo.tex, solution = FALSE,
      resolution = 300,
      options = paste0("--reference-doc=", ref), header = <lo que mida la Task 1>, dir = <tmp>, name = "tmp_"), y después
      file.rename(<tmp>/tmp_1.docx, word/cuadernillo_<ID>.docx).
  - firmas: por cada salida, huella[i, j] = digest(normalizar(c(question, questionlist))) y clave[i, j] → firmas.rds.
      - normalizar() sustituye cada referencia de figura por «FIG», con regex sobre \\includegraphics(\[[^]]*\])?\{[^}]*\} y
        !\[[^]]*\]\([^)]*\)(\{[^}]*\})?.
      - Test: f_tikz da huellas iguales entre PDF y Word, y cambiar un número del texto sí cambia la huella. La normalización
        no oculta cambios reales.
  - hoja_gris(): pdftoppm -r 50 -gray de cuadernillo_<ID_1>.pdf y de cuadernillo_<ID_N>.pdf → montage (magick) →
      revision_gris_v1.png y revision_gris_vN.png (C10). Además, pdftoppm -r 150 -gray de esas dos versiones →
      revision_gris_v1_p<K>.png y revision_gris_vN_p<K>.png, una por página, para leer los rótulos.
  - main():
      - rechaza un dir_salida no vacío salvo con --limpiar (exit 2), siempre después de la guarda de ruta de leer_config;
      - escapar_latex(x): función pura del MOTOR, porque R/exams no escapa el header. Carácter a carácter (strsplit + tabla
        de reemplazos + paste0) sobre \ & % $ # _ { } ~ ^, en una sola pasada, para que las llaves de \textbackslash{} no se
        escapen otra vez. Se aplica a los textos del config que van a header en exams2pdf (titulo, institucion, nombre de
        sesión); al Word no;
      - Sys.setenv(MPLBACKEND = "Agg"); .exams_generation_mode <- TRUE;
      - por sesión: preflight → hoja → clave → cuadernillo
      → retro → word → firmas → gris → system2(Rscript, verificar_cuadernillos.R, <dir sesión>). Exit = el del verificador.
      Log legible con tiempos por etapa.

Task 4 — Verificador (verificar_cuadernillos.R):
  - Secciones C1-C12 sobre <dir sesión>. Cada una devuelve list(id, estado ∈ {PASS, WARN, FAIL, MANUAL, NO_VERIFICABLE}, detalle).
  - C1/C2/C4/C5/C8: llama a medir_pdf.py por cada PDF (cuadernillos, hojas, retro) con --columnas 2/1 según el tipo.
    - C4 y C5 aplican a cuadernillos Y retro. C5 también al Word: ppi = px de word/media/* / (cx / 914400).
    - C8: los cuadernillos con número par de caras; las hojas con exactamente 2 caras y la segunda sin texto (pdftotext).
    - Word: unzip document.xml (pgSz, cols y ausencia de «##»).
  - C3: .log de tex/cuadernillos/<ID>/ y tex/retro/<ID>/. Si no hay log, recompila el .tex con pdflatex (2 pasadas) en un
    directorio temporal y lee el log.
  - C6: firmas.rds: identical() entre las cuatro huellas NORMALIZADAS y las claves, para cada (i, j).
    - Si falta una salida (no pedida), la omite y lo dice.
    - Los IDs y la lista de archivos salen SIEMPRE de firmas.rds, igual que en pdfunite; nunca de un glob del directorio.
  - C7: pdftotext -l 1 de cuadernillo_<ID>.pdf y de retro_<ID>.pdf debe contener <ID>; los IDs de firmas = names(xn).
  - C9/C11/C12: leen preflight.log y firmas.rds.
  - C10: estado MANUAL. Imprime y escribe en verificacion.txt: «REVISIÓN VISUAL PENDIENTE: <revision_gris_v1.png> y
    <revision_gris_vN.png> (criterio: categorías distinguibles en gris a 50 ppp; rótulos legibles en las páginas a 150 ppp;
    aceptación final del profesor)».
  - decidir_exit(resultados): función pura. 1 si hay algún FAIL; 2 si hay NO_VERIFICABLE; 0 si no.
    - El exit 0 certifica C1-C9, C11 y C12. WARN y MANUAL no bloquean, pero se imprimen.
    - El texto final dice explícitamente que C10 queda pendiente de revisión visual.
    - Imprime una tabla y escribe verificacion.txt.
  - --mutantes: para cada mutante, construye en un tempdir una COPIA con un reemplazo EXACTO.
    - La copia puede ser de la plantilla (todas las plantillas a un tempdir, más CUADERNILLOS_PLANTILLAS=<tempdir>), del motor
      o del config.
    - stop("MUTANTE MAL CONSTRUIDO") si el patrón no aparece exactamente 1 vez.
    - Corre el motor sobre pruebas/fixtures con N = 2 y exige que SU guarda dé FAIL.
    - Nunca escribe en DIR_PLANTILLAS real.
    - Imprime id, guarda, esperado y obtenido; exit 1 si alguno sobrevive.
      M1  C1   plantilla: papersize={216mm,330mm} → {216mm,356mm}
      M2  C2   fixture con plot() de R (pdf() sin incrustar)
      M3  C3   plantilla sin el \renewcommand{\includegraphics} + fixture con imagen de 14 cm
      M4  C4   fixture TikZ con \tiny de ≥ 3 caracteres, escalado a la columna
      M5  C5   fixture con PNG de 72 ppp a {width=100%}
      M6  C6   fixture con set.seed(NULL) en data_generation
      M7  C7   motor: la fórmula del ID del encabezado con startid + i (desfase de 1)
      M8  C8   plantilla sin el bloque «\ifodd\value{page}\else…» + fixture de 1 cara
      M9  C9   config de 2 ejercicios: f_texto PRIMERO y después el fixture con while (TRUE) {}; config$presupuesto_s = 20
               (respaldo = 2 × 20 = 40 s). Muere solo si C9 da FAIL nombrando el ejercicio 2 Y preflight.log dice «terminado
               por vigilante» a ≤ 30 s de su «INICIO 2». Si lo corta el respaldo, el mutante SOBREVIVE: el vigilante no se probó
      M10b C8  hoja: tomar solo la página 1 del NOPS (qpdf … 1 en lugar de 1-2) → la hoja queda con 1 cara
      M11 C11  config con estudiantes = 101 (muere por stop() con «C11» en leer_config); y M11b: fixture schoice con 6
               opciones en una sesión con NOPS
      M12 C12  fixture A con knit_engines$set(python = function(o) knitr::engine_output(o, o$code, '')) + fixture B con chunk {python}
               (lo ven el escaneo estático y la instantánea)
      M12b C12 fixture con cambio INDIRECTO del motor: do.call(knitr::knit_engines$set, list(python = function(o)
               knitr::engine_output(o, o$code, ''))) (la regex no lo ve; lo ve la instantánea por el código del motor)
      M13 C1   plantilla de Word con un marcador inexistente (##NoExiste##) → «##» en document.xml
      M14 C11  tres casos, cada uno debe abortar con exit 2 «ERR_DIR_SALIDA» SIN borrar nada (un archivo testigo sigue ahí):
               (a) --dir-salida .. --limpiar; (b) config con dir_salida = "."; (c) --dir-salida <tmp>/salida/ajena --limpiar sobre
               un directorio no vacío SIN el marcador .cuadernillos_salida

Task 5 — Fixtures (pruebas/fixtures/): .Rmd schoice mínimos y rápidos:
  - f_texto.Rmd (texto + fórmula);
  - f_tikz.Rmd (include_tikz con format = match_exams_device());
  - f_tabla.Rmd (kable → longtable, con el guard none);
  - f_plot.Rmd (gráfico de knitr con dev = "cairo_pdf");
  - f_restaura_python.Rmd (solo knit_engines$set(python = reticulate::eng_python); parte del control negativo de C12);
  - los de los mutantes (M2, M4, M5, M6, M9, M11b, M12A/B, M12b).
  - f_texto, f_tikz, f_tabla, f_plot y f_restaura_python son además el CONTROL NEGATIVO de C12 (Task 3, estado()): con ellos
    el pre-vuelo debe dar «estado=igual» en todos.
  Ortografía: tildes en comentarios y ningún identificador en -cion/-sion.

Task 6 — Configuraciones:
  - configuraciones/examen_real_11.R: los 11 .Rmd de muestras/ (rutas relativas al config: ../../01-…, ../../02-…,
    ../../04-…; el cateto ya es _n3_), N = 3, fecha 2026-11-13 (los IDs 26111300001..3 coinciden con las muestras actuales),
    UNA sesión con startid 1, dir_salida = "../salida/examen-real-11". Su sesión queda en salida/examen-real-11/sesion-1/.
    Los 11 son schoice (MEDIDO: clave única en 33 de 33 pares).
  - configuraciones/simulacro_2x25.R: humo de DOS sesiones de 25 (startid 1 y 101), N = 2, dir_salida =
    "../salida/simulacro-2x25" (sesion-1 y sesion-2). Lista FIJA: los 11 de examen_real_11.R repetidos en orden hasta 25,
    rep_len(ejercicios_11, 25), igual en las dos sesiones; así C11 no puede fallar por un CLOZE o un num ajeno al motor.
    NO es un examen para usar (repite ejercicios en una sesión); se declara en su cabecera. Mide que los IDs no chocan.
  - configuraciones/capacidad_1x25.R: la sesión 1 del simulacro (la misma lista fija) con N = 25, dir_salida =
    "../salida/capacidad-1x25". Es la prueba de CAPACIDAD: se corre UNA vez en la Task 7, se mide el tiempo total y por
    etapa, y el de 2 × 25 se extrapola (× 2) y se declara como extrapolado.

Task 7 — Correr el ejemplo real y el simulacro; ajustar hasta que C1-C12 pasen.
  - Cada repetición sobre la misma salida lleva --limpiar: el motor se niega a escribir en un directorio no vacío.
  - Una corrida de capacidad_1x25.R (N = 25), UNA sola vez: tiempo total y por etapa al log de trabajo. Si pasa de 60 min,
    se declara así y no se repite.
  - Medir los tiempos por etapa y el de --mutantes, y anotarlos en un log de trabajo. La suite completa todavía no existe: se
    mide en la Task 11, que es la que crea el §10 de INVESTIGACION.md y pasa ahí todos los tiempos.
  - Mirar con Read revision_gris_v1.png, la página 1 de un cuadernillo, la página 1 de una retroalimentación (a 360 px de ancho,
    como en un celular) y el Word convertido con LibreOffice (soffice --headless con perfil propio
    -env:UserInstallation=file://<scratch>/lo_perfil).

Task 8 — Mutantes: verificar_cuadernillos.R --mutantes con exit 0 (los 16 mueren: M1-M9, M10b, M11, M11b, M12, M12b, M13, M14).

Task 9 — Regenerar muestras/ con el motor, sin riesgo para las actuales:
  1. command cp -a muestras/ <scratch>/respaldo_muestras/ (ANTES de todo).
  2. Generar, desde 00-Cuadernillos-Oficio-2Col/, con:
     Rscript generar_cuadernillos.R configuraciones/examen_real_11.R --dir-salida salida/regen --limpiar
     La ruta queda fuera de muestras/, tiene el componente «salida», y git la ignora (**/salida/). --dir-salida es relativa al
     directorio de trabajo: queda en 00-Cuadernillos-Oficio-2Col/salida/regen/, y la sesión única en SR=salida/regen/sesion-1.
     Los pasos 3 y 4 leen de ahí.
  3. Solo si el verificador sale con 0 y C10 ya se revisó: contar primero (3 de cada tipo, como en el Level 3; si falta uno,
     PARAR). Después, con los IDs leídos de $SR/firmas.rds, copiar a muestras/ uno por uno (command cp -f, sin globs):
     $SR/cuadernillos/cuadernillo_<ID>.pdf, $SR/hojas/hoja_respuestas_<ID>.pdf, $SR/retro/retro_<ID>.pdf,
     $SR/word/cuadernillo_<ID>.docx, $SR/clave/clave.pdf, $SR/revision_gris_v1.png y $SR/revision_gris_vN.png.
     Ninguno empieza por «nops». El motor NO genera hojas en color: no se copia ninguna.
  4. Antes de borrar, comprobar con ls que el conjunto nuevo está completo. Después, borrar de muestras/ SOLO estos archivos
     del esquema viejo, listándolos antes con ls:
     - *_version_* (cuadernillos y hojas de respuestas);
     - hojas_version1_color.png y hojas_version1_gris.png, que el patrón *_version_* no atrapa.
  5. Actualizar la tabla de §9 de INVESTIGACION.md con los nombres nuevos.

Task 10 — Reglas (infraestructura protegida, regla #17):
  - ANTES: copia de los 6 archivos a <scratch>/respaldo_reglas/ (git también los guarda).
  - codigo-rmd.md (compacta): en «Nunca», punto 1: «… Usar include_tikz(..., format = match_exams_device(), markup = "markdown")
    (PDF vectorial en exams2pdf/exams2nops; PNG en Word/HTML; SVG o PNG en Moodle; medido el 2026-11-13) o format = "png"».
    Mantener ≤ 5 000 caracteres.
  - docs/reglas/codigo-rmd.md (íntegra): la tabla medida de §3.2 de INVESTIGACION.md y la razón.
  - material-aula.md (compacta e íntegra), regla #26, SOLO lo verificable antes de la promoción:
    - con cuadernillo propio, el NOPS va con twocolumn = FALSE (twocolumn = TRUE falla con tablas: medido);
    - una matriz de semillas para todas las salidas, con startid distinto por sesión;
    - la fuga entre ejercicios (C12).
    - La «versión oficial» con las plantillas nuevas NO se escribe aquí: los archivos no existen en perifericos/ hasta la
      Task 12, y la regla no puede afirmarlo antes (documentacion-verificada.md). La escribe la Task 12b.
  - TOPE de las compactas: cada .claude/rules/*.md ≤ 5 000 caracteres (test_presupuesto_contexto_reglas.R). MEDIDO el
    2026-11-13 con wc -m: material-aula.md 3 259 y codigo-rmd.md 2 209. Medir ANTES y DESPUÉS de editar, y otra vez en la
    Task 12b. Si una pasa del tope, el detalle va al íntegro (.claude/docs/reglas/), no a la compacta.
  - CHANGELOG_CLAUDE_MD.md (arriba del todo): entrada 3.44.0 con lo de esta tarea. .claude/CLAUDE.md: solo «Versión»
    (3.44.0) y «Fecha».
  - DESPUÉS: Rscript tests/testthat/test_infraestructura_claude.R; Rscript tests/testthat/test_presupuesto_contexto_reglas.R.

Task 11 — Suite y documentación:
  - CREATE tests/testthat/test_cuadernillos_oficio.R:
    - id_version() igual a names(exams2nops()) (N = 2, fixture);
    - parseo de pdfinfo -dests sobre un PDF fijo;
    - clave_desde() sobre un objeto conocido;
    - escapar_latex() sobre «a & b_c 50 % #1 $ {x} ~ ^ \» da la cadena esperada, y esa cadena compila en un .tex mínimo;
    - decidir_exit(): con un FAIL → 1; con NO_VERIFICABLE (y sin FAIL) → 2; con solo PASS/WARN/MANUAL → 0;
    - control negativo de C12: el pre-vuelo de f_texto, f_tikz, f_tabla y f_plot da «estado=igual» en los cuatro;
    - camino feliz: motor con 2 fixtures × N = 2 y un título con «&» y «_» → verificador con exit 0, y hojas de exactamente 2 caras;
    - guarda de ruta: los tres casos de M14 abortan con ERR_DIR_SALIDA y el archivo testigo sigue en su sitio; el camino feliz
      escribe en <tmp_del_test>/salida/feliz y deja el marcador .cuadernillos_salida;
    - verificar_cuadernillos.R --mutantes con exit 0.
    - skip_if_not() si faltan pdflatex, qpdf, pdfjam o pdfunite.
  - MODIFY tests/run_all_tests.R:
    list(nombre = "Cuadernillos oficio 2 columnas (Regla #26)", archivo = "tests/testthat/test_cuadernillos_oficio.R",
         critico = TRUE, watch = c("00-Cuadernillos-Oficio-2Col", "cuadernillo_oficio_2col", "retro_celular",
                                   "pandoc_cuadernillo", "test_cuadernillos_oficio")).
  - Rscript tests/sincronizar_doc_suites.R, y después --check con exit 0. Si --check sale con 1, leer el diff que imprime:
    si solo trae la fila de la suite nueva, regenerar; si trae diferencias de los cambios AJENOS sin commit, no tocarlas y
    declararlo en el reporte.
    - NO se hace commit aquí, ni en ninguna tarea antes del cierre.
    - Al editar tests/run_all_tests.R y VALIDATOR_COBERTURA_ICFES.md, tocar solo lo propio. MEDIDO: los dos traen cambios
      ajenos sin commit (6 y 69 líneas).
    - Dejar anotada la receta del commit selectivo (solo los hunks propios: git hash-object -w + update-index --cacheinfo)
      para OFRECERLA en la Task 13.
    - Nunca git stash ni git checkout.
  - INVESTIGACION.md: §10 «Uso» (comandos, tiempos medidos, cómo escanear con nops_eval) y §5 marcado como implementado.

Task 12 — Promoción a perifericos/ (decisión del profesor: «Ya, al terminar el proyecto»):
  - ANTES: R_TESTS_FULL=1 Rscript tests/run_all_tests.R (regla #8: la suite completa es obligatoria antes de promover).
    Leer «Suites ejecutadas» y «Fallidas: 0». Si falla una suite por cambios AJENOS, compararla contra HEAD y declararlo.
  - Comprobar que NINGUNO de los 4 archivos existe en perifericos/. Si existe uno, PARAR y preguntar al profesor.
    Copiar con `cp -n` (sin sobrescribir) y rutas absolutas, porque plantillas/ y perifericos/ no resuelven desde un mismo
    directorio:
    RAIZ=$(git rev-parse --show-toplevel); P="$RAIZ/Auxiliares/Examenes_Fin_de_Periodo/00-Cuadernillos-Oficio-2Col/plantillas"
    D="$RAIZ/A-Produccion/perifericos"
    for f in cuadernillo_oficio_2col.tex retro_celular.tex pandoc_cuadernillo.tex referencia_oficio_2col.docx; do
      [ -e "$D/$f" ] && { echo "YA EXISTE $D/$f: PARAR"; break; }
      command cp -n "$P/$f" "$D/$f" && cmp "$P/$f" "$D/$f" && echo "OK $f"; done
    Deben salir los 4 «OK». Si falta uno, PARAR.
  - Cambiar el valor POR DEFECTO de DIR_PLANTILLAS a <raíz git>/A-Produccion/perifericos (git rev-parse --show-toplevel,
    como SemilleroUnico_v2.R). El override CUADERNILLOS_PLANTILLAS se mantiene para los mutantes.
    Borrar plantillas/ del proyecto (fuente única), y solo después de los cmp.
  - Repetir, en este orden y todos en verde:
    - la Task 7 (ejemplo real, con --limpiar);
    - la Task 8 (los 16 mutantes);
    - Rscript -e 'r <- as.data.frame(testthat::test_file("tests/testthat/test_cuadernillos_oficio.R")); quit(status = as.integer(sum(r$failed) > 0 || any(r$error)))' (exit 0). MEDIDO: test_file() solo NO cambia el exit aunque falle un test.

Task 12b — Regla #26: versión oficial (solo después de la promoción verificada):
  - Requisito: los 4 «OK» de la Task 12 y sus repeticiones en verde. Si no, NO se escribe nada y se informa.
  - ANTES: copia de los archivos de regla que se tocan a <scratch>/respaldo_reglas_12b/ (regla #17).
  - material-aula.md (compacta e íntegra): en «Versión oficial de plantillas y semilleros», las 4 plantillas nuevas de
    perifericos/ (cuadernillo_oficio_2col.tex, retro_celular.tex, pandoc_cuadernillo.tex, referencia_oficio_2col.docx) y
    el motor que las usa (00-Cuadernillos-Oficio-2Col/generar_cuadernillos.R).
  - CHANGELOG_CLAUDE_MD.md: completar la MISMA entrada 3.44.0 (aún sin commit) con la promoción. No se abre otra versión.
  - wc -m de la compacta (≤ 5 000); test_infraestructura_claude.R y test_presupuesto_contexto_reglas.R en verde.

Task 13 — Cierre:
  - El mismo comando de la Task 12, con quit(status = …), explícito y sin depender de su watch.
  - Rscript tests/run_all_tests.R (modo quick). Su reporte debe listar «Cuadernillos oficio 2 columnas (Regla #26)» entre las
    ejecutadas y dar «Fallidas: 0». Si falla una suite por cambios AJENOS, compararla contra HEAD y declararlo.
  - Reporte al profesor: tabla C1-C12, tiempos, rutas de muestras/ y las revisiones visuales. C10 se cierra aquí, con la
    lista de figuras revisadas en gris.
  - OFRECER el commit con la lista explícita de archivos propios y la receta de hunks propios de la Task 11. No hacerlo sin
    que lo pida (memoria «preferencias del usuario», punto 5).
```

### Per-Task Pseudocode (lo crítico)
```r
# Task 3: preflight_hijo.R (proceso aparte, en segundo plano; el padre lo vigila con el log de progreso, ver C9)
args <- commandArgs(TRUE)                 # ruta del manifiesto: ejercicios, S (rds), N, ruta del log
m <- readRDS(args[1]); library(exams); Sys.setenv(MPLBACKEND = "Agg"); .exams_generation_mode <- TRUE
knitr::knit_engines$set(python = reticulate::eng_python)   # línea base normalizada (C12)
CLAVES_VOLATILES <- character(0)   # las fija el control negativo de la Task 3; se documentan aquí con su razón
estado <- function() {             # SUBORDINADO a la calibración de la Task 3: solo lo que el control demuestra estable
  motores <- knitr::knit_engines$get()
  oc <- knitr::opts_chunk$get()
  list(motores = sort(names(motores)),
       codigo  = vapply(motores, function(f) paste(deparse(f), collapse = "\n"), ""),   # código, sin el entorno
       chunk   = oc[setdiff(sort(names(oc)), CLAVES_VOLATILES)])
}
for (j in seq_along(m$ejercicios)) {
  cat("INICIO", j, m$ejercicios[j], "\n", file = m$log, append = TRUE)
  e0 <- estado()
  x  <- exams::xexams(m$ejercicios[j], n = m$N, seed = m$S[, j, drop = FALSE], dir = tempfile())
  e1 <- estado()
  tipos <- unique(vapply(x, function(v) v[[1]]$metainfo$type, ""))
  maxop <- max(vapply(x, function(v) length(v[[1]]$questionlist), 1L))   # C11: ≤ 5 con NOPS (M11b); comprobar con su fixture
  cambia <- names(e0)[!mapply(identical, e0, e1)]          # qué componente cambió
  cat("FIN", j, paste0("tipos=", paste(tipos, collapse = "/")), paste0("max_opciones=", maxop),
      if (length(cambia) == 0) "estado=igual" else paste0("estado=CAMBIA:", paste(cambia, collapse = ",")), "\n",
      file = m$log, append = TRUE)
}
# GOTCHA: xexams() debe recibir dir (no tempdir() compartido) y el driver por defecto. VERIFICAR la firma de xexams en 2.4.5
#         (file, n, nsamp, driver, dir, edir, tdir, sdir, verbose, points, seed, rds).
# GOTCHA: si el estado cambia, el MISMO hijo siguió con el estado contaminado: eso es lo que reproduce el examen real.
# GOTCHA: con el driver por defecto de xexams, match_exams_device() puede no dar «pdf». El pre-vuelo no reproduce las figuras
#         del PDF: mide tiempo (C9), tipos y opciones (C11) y estado de knitr (C12). Las figuras las miden C2-C5.

# Task 4: decidir_exit (función pura, testeable)
decidir_exit <- function(r) { e <- vapply(r, `[[`, "", "estado")
  if (any(e == "FAIL")) 1L else if (any(e == "NO_VERIFICABLE")) 2L else 0L }
```

### Integration Points
```yaml
REGLAS: .claude/rules/{codigo-rmd,material-aula}.md + .claude/docs/reglas/ (dos capas; la compacta ≤ 5 000 caracteres).
TESTS: tests/run_all_tests.R (suite con watch) + tests/sincronizar_doc_suites.R (tabla generada).
PERIFERICOS: archivos nuevos en A-Produccion/perifericos/ (Task 12); ningún archivo existente cambia.
ESCANEO: nops_eval() usa nops/nops_.rds de la sesión (lo escribe exams2nops). Documentar en §10 que las hojas escaneadas
         se evalúan con ese .rds.
```

## Validation Loop

### Level 1: Sintaxis
```bash
cd Auxiliares/Examenes_Fin_de_Periodo/00-Cuadernillos-Oficio-2Col
for f in generar_cuadernillos.R verificar_cuadernillos.R configuraciones/*.R; do Rscript -e "invisible(parse(file='$f'))" || echo "FALLA $f"; done
python3 -I -m py_compile prototipo/medir_pdf.py
D="${CUADERNILLOS_PLANTILLAS:-plantillas}"      # tras la Task 12: A-Produccion/perifericos
ls "$D"/cuadernillo_oficio_2col.tex "$D"/retro_celular.tex "$D"/pandoc_cuadernillo.tex "$D"/referencia_oficio_2col.docx   # se compilan en el Level 3 (no hay chktex)
for f in pruebas/fixtures/*.Rmd; do Rscript ../../../.claude/scripts/corregir_ortografia_espanol.R "$f" | grep -q 'ERRORES ORTOGRÁFICOS ENCONTRADOS' && echo "ORTOGRAFÍA: $f"; done   # sin salida = limpio
```

### Level 2: Unit Tests
```bash
Rscript -e 'r <- as.data.frame(testthat::test_file("tests/testthat/test_cuadernillos_oficio.R")); quit(status = as.integer(sum(r$failed) > 0 || any(r$error)))'; echo "exit=$?"   # desde la raíz; 0 = sin fallos (test_file() solo NO cambia el exit)
# Casos: id_version() frente a exams2nops; parseo de -dests; clave_desde(); decidir_exit(); camino feliz N = 2; --mutantes.
```

### Level 3: Integración (salida real)
```bash
cd Auxiliares/Examenes_Fin_de_Periodo/00-Cuadernillos-Oficio-2Col
S=salida/examen-real-11/sesion-1        # dir_salida del config (Task 6) + slug de la sesión 1
Rscript generar_cuadernillos.R configuraciones/examen_real_11.R --limpiar ; echo "exit=$?"   # esperado 0; tabla C1-C12 sin FAIL
Rscript verificar_cuadernillos.R "$S" ; echo "exit=$?"                              # 0 (C10 = MANUAL)
Rscript verificar_cuadernillos.R --mutantes ; echo "exit=$?"                        # 0: los 16 mutantes mueren
# Contar ANTES de los bucles: un glob vacío no imprime nada y parecería verde. N = 3 → 3 archivos de cada tipo.
for p in cuadernillos/cuadernillo_ hojas/hoja_respuestas_ retro/retro_ word/cuadernillo_; do
  n=$(find "$S/${p%%/*}" -maxdepth 1 -type f -name "${p#*/}*" | wc -l); [ "$n" -eq 3 ] && echo "OK $p: 3" || echo "FALTA $p: $n de 3"; done
for h in "$S"/hojas/hoja_respuestas_*.pdf; do pdfinfo "$h" | grep Pages; done        # 2 cada una
for f in "$S"/cuadernillos/cuadernillo_*.pdf; do pdfinfo "$f" | grep -E 'Pages|Page size'; done   # pdfinfo acepta UN archivo
for f in "$S"/retro/retro_*.pdf; do pdfinfo "$f" | grep 'Page size'; done           # 255.1 x 453.5 pts (90 x 160 mm)
for f in "$S"/word/cuadernillo_*.docx; do unzip -p "$f" word/document.xml | grep -oE '<w:pgSz[^>]*>|<w:cols[^>]*>' | sort -u; done
Rscript generar_cuadernillos.R configuraciones/simulacro_2x25.R --limpiar ; echo "exit=$?"   # 0
Rscript -e 'for (k in 1:2) print(readRDS(sprintf("salida/simulacro-2x25/sesion-%d/firmas.rds", k))$ids)'   # …00001-2 y …00101-2
# Revisión visual (Read): revision_gris_v1.png, página 1 de cuadernillo y de retro (360 px), Word en LibreOffice.
```

### Level 4: Repo
```bash
Rscript tests/sincronizar_doc_suites.R --check ; echo "exit=$?"
Rscript tests/testthat/test_infraestructura_claude.R
Rscript tests/testthat/test_presupuesto_contexto_reglas.R
Rscript tests/run_all_tests.R          # quick: leer «Suites ejecutadas», y que la nueva se ejecute
```

## Final Validation Checklist
- [ ] C1-C12 en verde en el ejemplo real (C10 = MANUAL, revisado con Read y declarado).
- [ ] Los 16 mutantes mueren (`--mutantes` con exit 0), también después de la Task 12, vía CUADERNILLOS_PLANTILLAS.
- [ ] Control negativo de C12: los 4 fixtures limpios dan «estado=igual».
- [ ] Hojas de respuestas de exactamente 2 caras; hojas_imprimir.pdf con número par de caras.
- [ ] C10: hojas en gris (50 ppp) y páginas en gris (150 ppp) de la primera y la última versión revisadas con Read, con el criterio de C10 y
      declaradas en el reporte. El exit 0 del verificador no lo incluye; la aceptación final es del profesor.
- [ ] Simulacro 2 × 25 (N = 2): IDs distintos entre sesiones. Capacidad (1 × 25, N = 25): tiempo medido; el de 2 × 25, extrapolado y declarado; todo en §10.
- [ ] `muestras/` regenerado con el motor; sin nombres «nops*».
- [ ] `codigo-rmd.md` (punto 1 de «Nunca») y regla #26 (`material-aula.md`) en sus dos capas, CHANGELOG y versión; invariantes
      y presupuesto de contexto en verde. La regla #1 (ejercicios metacognitivos) NO se toca. La «versión oficial» de la
      regla #26 se escribe solo en la Task 12b, después de la promoción verificada.
- [ ] Suite registrada con su watch; doc de suites sincronizado (solo los hunks propios).
- [ ] Suite completa (R_TESTS_FULL=1) en verde ANTES de promover.
- [ ] Plantillas promovidas a perifericos/ como archivos nuevos (cp -n + cmp); el motor las lee de allí; Tasks 7 y 8 y la suite
      test_cuadernillos_oficio.R repetidas en verde después de la promoción.
- [ ] Con un dir_salida no vacío, el motor se niega sin --limpiar; IDs y archivos salen de firmas.rds. La guarda de ruta rechaza
      «.», «..» y un directorio ajeno sin marcador (M14) sin borrar nada.
- [ ] La suite nueva sale con exit ≠ 0 si falla un test (comando con quit(status = …)); run_all_tests.R la lista y da «Fallidas: 0».
- [ ] C6 con huella normalizada: f_tikz da lo mismo en PDF y en Word, y un número cambiado sí cambia la huella.
- [ ] El commit se ofrece; ninguna tarea lo hace antes, y no se hace sin el pedido del profesor.

---

## Anti-Patterns to Avoid
- ❌ `knitr::is_latex_output()` para ramificar (FALSE siempre bajo R/exams).
- ❌ `\setkeys{Gin}{max width=…}` como único límite de ancho (no actúa con width=).
- ❌ `exams2nops(twocolumn = TRUE)` cuando el NOPS solo aporta la hoja (falla con tablas).
- ❌ Un `opts_chunk$set()` o `knit_engines$set()` global en un `.Rmd` (contamina el examen). El remedio va en el chunk.
- ❌ `setTimeLimit()` como tope (no corta Python ni system()): proceso hijo y `timeout`.
- ❌ Una ruta relativa en `options` de `exams2pandoc` (se evalúa en otro directorio).
- ❌ Marcar C10 como PASS sin mirar la hoja en gris.
- ❌ Un mutante que «pasa» porque el reemplazo no ocurrió: exigir exactamente 1 coincidencia.
- ❌ Editar perifericos/solpcielo.tex u otro archivo existente de perifericos/ (solo archivos NUEVOS).
- ❌ `--no-verify`, `git stash`, `git checkout --` o `--fix` del corrector de ortografía.
- ❌ Tocar `Copia de 04-Examen-Fin-de-Periodo-4/`.

## Confianza one-pass: 7/10
Lo medido (plantilla, NOPS = cuadernillo, Word, -dests) quita los riesgos grandes. Quedan tres por medir en la implementación:
1. si `xexams()` en el hijo reproduce el estado global igual que `exams2pdf()`;
2. qué placeholders de header sustituye `exams2pandoc` en una plantilla propia;
3. el tiempo total de un simulacro de 2 × 25 con N = 25: se MIDE una sesión de 25 con N = 25 (capacidad_1x25.R), y el de dos sesiones se extrapola (× 2) y se declara como extrapolado.
