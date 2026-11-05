# R/exams: versiones y novedades verificadas

> Qué trae cada versión de `exams` y `exams2forms`, comprobado contra el NEWS, el código fuente y este
> repositorio. Última medición: 2026-11-05. Para repetirla, ver «Repetir la medición». Los resúmenes
> de novedades escritos por IA mezclan funciones de hace años con novedades: se comprueban aquí antes
> de actuar.

## Estado

Salida de `Rscript .claude/scripts/vigia_rexams.R`, cotejada a mano con el NEWS de CRAN:

| Paquete | Instalada | `renv.lock` | CI | CRAN | Desarrollo |
|---|---|---|---|---|---|
| `exams` | 2.4-2 (2025-04-29) | 2.4-3 | la del día, sin fijar | 2.4-5 (2026-10-08) | 2.4-5 |
| `exams2forms` | 0.2-0 (2025-05-02) | 0.2-0 | no se instala | 0.2-2 (2026-07-31) | 0.2-3 |

Pandoc del equipo: 3.11 en terminal y 3.10 en RStudio. No hay ninguna versión anunciada para 2027: la rama principal de
[`exams` en Codeberg](https://codeberg.org/zeileis/exams) coincide con CRAN, y ni su NEWS ni el
[blog oficial](https://www.r-exams.org/general/) publican una hoja de ruta.

## Un resumen sobre «novedades 2027» (2026-11-04)

Llegó un resumen con cinco novedades «de cara a 2027». Ninguna es de 2027: las cuatro primeras ya
estaban en la versión instalada, y la quinta no está respaldada tal como se afirma.

| Afirmación | Veredicto | Desde | Lo que el resumen no dice |
|---|---|---|---|
| `stresstest_exercise()` con `timeout` | Real | 2.4-2, abr. 2025 | Solo corta código R: `system("sleep 25")` con límite de 3 s tardó 25 s. Aborta y pierde todas las semillas si el ejercicio deja un objeto que `as.data.frame()` no admite (ggplot, funciones): 20 de 373 ejercicios, 10 de los 47 de `01-En-PreDesarrollo` |
| `num_to_schoice(maxit)` | Real | 2.4-2, abr. 2025 | Su valor por defecto es `Inf`, sin tope. Solo un `maxit` negativo lanza error. Ningún ejercicio activo usa esta función |
| `exams2forms` y `show_filename`, `show_tolerance`, `auto` | Real | 0.1-0; los argumentos, 0.2-0, may. 2025 | «Estándar» es una opinión. Sin internet no muestra las fórmulas |
| `nops_fix()` | Real | 2.4-1, jul. 2024; formulario interactivo, 2.4-2 | «Reduce drásticamente los errores» no está medido en ninguna fuente |
| Soporte LMS actualizado «garantizando» importaciones sin errores | Sin respaldo | | El NEWS no recoge esa garantía. `vvcanvas` existe y tiene [tutorial oficial](https://www.r-exams.org/tutorials/vvcanvas/) (sept. 2025), pero es un cliente de la API de Canvas, y este repo usa Moodle |

## Lo que sí toca este repositorio

| Versión | Cambio | Efecto aquí |
|---|---|---|
| exams 2.4-2 | Corrige puntos negativos en `schoice` dentro de CLOZE en Moodle | Ya instalado |
| exams 2.4-3 | `exams2pdf` reescribe las figuras duplicadas en cualquier línea | Ver «Figuras en multicopia» |
| exams 2.4-3 | `seed` en `exams2moodle`, `exams2qti12` y `exams2qti21` | Permite exportar exactamente las versiones validadas |
| exams 2.4-3 | `answerlist()` toma el `markup` del ejercicio, no `"latex"` | Cambia 1 `.Rmd`, para bien: `construccion_segmentos_circulos.Rmd` metía LaTeX crudo en Markdown |
| exams 2.4-3 | `add_cloze()`, `format_metainfo()`, `match_exams_markup()` y `answerlist(write = FALSE)` | Sin uso en el repo |
| exams 2.4-3 | Orden nuevo de argumentos en `exams2nops()` | Sin efecto: de las 112 llamadas activas, ninguna pasa más de dos argumentos por posición |
| exams 2.4-3 | **`answerlist(markup=)` se ignora: bug, corregido en 2.4-4** | `renv.lock` fija justo 2.4-3: no restaurar esa versión |
| exams 2.4-4 | Contador `{none}` en las plantillas del paquete | Ver «Tablas en PDF» |
| exams 2.4-5 | Repositorio en Codeberg; `exams2ilias()` sin mantenimiento; CLOZE `string` con regex y `\|` | Sin uso en el repo |
| exams2forms 0.2-1 | `converter` deja de existir en `exams2forms()` | Sin uso en el repo |

`inst/exercises` solo gana `penguins` entre 2.4-2 y 2.4-5, y ya está en
`SOURCES/plantillas/rexams-oficiales/`.

## Medición de 2.4-2 a 2.4-5 (2026-11-04 y 05)

R 4.6.1 y pandoc 3.11. La candidata se instaló aparte: la biblioteca del usuario sigue en 2.4-2.
Corpus: 373 ejercicios de los 444 `.Rmd` de `A-Produccion`; se excluyen las fuentes que deja
`exams2webquiz`, las copias `.backup_*`, los directorios `.Rmd` y los archivos sin metadatos.

Resultados crudos en `~/.cache/rexams-versiones/`: `comp_final.log`, `n100/`, `n100_tope2000/` y
`suite.log` (53 de 53 suites).

### Contenido (knit y lectura, 3 semillas)

| Resultado | Ejercicios |
|---|---|
| Texto idéntico en las dos versiones | 318 |
| Cambia con 2.4-5 | 1: `construccion_segmentos_circulos.Rmd`, para bien (`answerlist()`) |
| No determinista: la misma versión no se repite | 24: 14 de `Lab-Manjaro`, 9 de producción y 1 de pre-desarrollo |
| Falla igual en las dos | 29 |
| Falla solo con 2.4-2 | 1: `mediana_salas_cine`, por azar: usa `set.seed(Sys.time)` y su `test_that` falló en esa corrida |
| **Falla solo con 2.4-5** | **0** |

En los 24 no deterministas la comparación no dice nada sobre la versión.

### Formatos (muestra de 60)

Resultado estable de tres pasadas de repetición de esta etapa, con la misma muestra, la misma
función hija y 8 procesos. Las tres coinciden entre sí. No incluye los tres casos de `comp_final.log`
que no se sostienen, explicados abajo.

| Formato | Idéntico | Texto distinto | No determinista | Falla solo con 2.4-2 | Falla solo con 2.4-5 | Falla en las dos | No aplica |
|---|---|---|---|---|---|---|---|
| HTML | 48 | 1 | 7 | 0 | 0 | 4 | 0 |
| PDF | 45 | 1 | 4 | 3 | 0 | 7 | 0 |
| DOCX | 50 | 1 | 5 | 0 | 0 | 4 | 0 |
| NOPS | 33 | 1 | 4 | 2 | 0 | 4 | 16 |
| Moodle | 48 | 1 | 7 | 0 | 0 | 4 | 0 |

- **Texto distinto:** es `construccion_segmentos_circulos.Rmd` en los cinco formatos. Diferenciado a
  mano: con 2.4-2 el HTML no muestra las opciones, el XML de Moodle trae 0 `<answer>` y el PDF deja
  `**Incorrecto**` sin formato. Con 2.4-5 salen las 4 opciones y el 100 % queda en la clave correcta,
  «3 círculos». Es la corrección de `answerlist()`.
- **Falla solo con 2.4-2:** los 5 casos son `No counter 'none' defined`, el arreglo de 2.4-4:
  `Copia de interpretacion_grafica_viaje_vers3.Rmd` y `logica_puntaje_campeonato_v1.Rmd` en PDF y
  NOPS, y `fourfold2.Rmd` en PDF.
- **No determinista:** 7 ejercicios cuya base no coincide consigo misma. En ellos la comparación no
  dice nada sobre la versión.

El `TEXTO_DISTINTO` masivo de la primera pasada (38 en NOPS, 33 en Moodle) era ruido del normalizador:
espacios de `pdftotext -layout` y PNG en `<file encoding="base64">`, que guardan su fecha. Con el
normalizador corregido no queda ninguno de esos.

#### Tres casos de `comp_final.log` que no se sostienen

`comp_final.log` terminó en `COMPARAR_REVISAR`, y además del caso anterior trae
`FALLA_SOLO_CANDIDATA` en el PDF de `Copia de Geometria_vuelo.Rmd` y en el HTML de
`Adopcion_Mascotas_…-Opc-B2v2.Rmd`, y `TEXTO_DISTINTO` en el Moodle de
`Copia de probabilidad_condicional_…_v2.Rmd`. Los tres son fallos sueltos de LaTeX en el lado de
2.4-5, entre las 07:24 y las 07:26, y no un efecto de la versión:

- Ninguno reaparece en las tres pasadas de repetición, y en ellas no hay ninguna `FALLA_SOLO_CANDIDATA`.
- Los dos primeros tampoco fallan en 18 repeticiones por versión, 12 de ellas con 8 procesos a la
  vez, y en todas la salida coincide entre versiones.
- El tercero se reprodujo a propósito: con `tex2image()` forzado a fallar sale su misma huella,
  `c23920df`, con las dos versiones. El ejercicio atrapa el error con un `tryCatch` y pinta otra
  tabla, así que el fallo de LaTeX se ve como texto distinto.
- `tex2image()`, `include_tikz()` y `exams2pdf()` tienen el mismo código en las dos versiones.
  `make_exams_write_pdf()`, por donde pasa el caso del PDF, sí cambia.

La causa del fallo de LaTeX no quedó identificada: el hijo del comparador no guarda el log, y el
registro del sistema no muestra falta de memoria ni errores del kernel.

### Tablas en PDF

Con pandoc 3.11, un `.Rmd` con tabla Markdown y sin el guard de la regla #20 no compila:
`No counter 'none' defined`. Censo de los 55 ejercicios con figura legada o tabla sin guard:

| Resultado de `exams2pdf(rep(f, 3))` | Ejercicios |
|---|---|
| Compilan con las dos versiones | 39 |
| **No compilan con 2.4-2 y sí con 2.4-5** | **9**, todos de `03-En-Produccion`: 6 de `comparar_medianas_baterias_celulares`, 2 de `accidentalidad_vial_genero`, 1 de `promedios_inverso_suma_a_promedio` |
| Fallan en las dos | 6: 3 de `vuelo_acrobatico_mejorado` y 3 por LaTeX |
| No concluyente | 1: `mediana_salas_cine`, intermitente por `set.seed(Sys.time)` |

**Actualizar `exams` no arregla los Semilleros.** Usan plantillas propias que no definen `{none}` ni
`\pandocbounded`: con `solpcielo.tex`, esos ejercicios fallan también con 2.4-5. Estas dos líneas
antes de `\begin{document}` los hacen compilar con las dos versiones, también junto al guard de los
`.Rmd` (probado en una copia con 4 ejercicios):

```latex
\providecommand{\pandocbounded}[1]{#1}
\makeatletter\@ifundefined{c@none}{\newcounter{none}}{}\makeatother
```

### Figuras en multicopia

El arreglo de 2.4-3 funciona en casos construidos: de 1 a 3 imágenes distintas sin `{width}`, con
`&#8203;` o con texto en la línea, y sigue fallando con dos imágenes del mismo nombre en una línea.
En el corpus real su efecto es **nulo**: los 39 que compilan con las dos versiones dan el mismo
número de imágenes. El legado ya usaba el patrón que funcionaba. Ver la regla #4.

### NOPS de punta a punta

Con hojas de `exams2nops()` marcadas con cruces en sus coordenadas reales y leídas con `nops_scan()`,
`Daten.txt` reproduce lo dibujado y `nops_control_calidad.R` halla las 5 anomalías sembradas en 4
hojas, sin falsos positivos. Una hoja con tabla no compila con 2.4-2; con 2.4-5 sí.

### Exportar lo que se validó

`exams2moodle(seed =)` (2.4-3) repite el XML salvo el nombre de la categoría (medido en un SCHOICE).
`validar_stresstest.R` usa `seed =`, no `set.seed()`, porque `xexams()` vuelve a sembrar antes de
leer el ejercicio, que es donde `exshuffle` mezcla las opciones. Así la semilla `s` es la versión
`s` de:

```r
exams2moodle(archivo_examen, n = 100, seed = matrix(1:100, ncol = 1), ...)  # exams >= 2.4-3
```

No aplica a los ejercicios no deterministas: su azar no depende de esa semilla.

## Defectos que destapó el barrido

Barrido de `validar_stresstest.R` con N = 100 sobre los 161 ejercicios activos: 83 de
`03-En-Produccion`, 47 de `01-En-PreDesarrollo` y 31 de `02-En-Desarrollo`, sin `Lab-Manjaro`. Nueve
dan error: 6 en producción y 3 en desarrollo. Los de `03-En-Produccion` son inmutables y quedan
declarados.

| Zona | Ejercicio | Defecto |
|---|---|---|
| Producción | `2023-Matematicas-11-2-09-Op-M.Rmd` | `while` sin cota: cuelga en 22 de 100 semillas (Error 22) |
| Producción | `vuelo_acrobatico_mejorado_A/B/C.Rmd` | Python arrastra estado entre versiones: cada una pasa suelta, pero falla con `n > 1` (60, 87 y 86 de 100 semillas) |
| Producción | `mediana_salas_cine_formulacion_ejecucion_n2_v1.Rmd` | `set.seed(Sys.time)`: su `test_that` falla de forma intermitente (4 de 100) |
| Producción | `probabilidad_intervalos_curva_…_tikz_cloze_v1_2.Rmd` | Su propio `stop()` corta la semilla 83: «Las opciones generadas no son suficientemente diferentes» |
| Producción | `11_C01_G09_2020_Tipo1.Rmd` y su copia | Aviso: dos distractores idénticos en la semilla 12 (`duplicated items in question list`); la clave no cambia |
| Producción | `fracciones_reparto_premio_v1..v4`, `exportaciones_graficos_tebailandia` y copias, `conversion_unidades_area` | No deterministas: `set.seed(NULL)` o `random` de Python sin sembrar |
| En desarrollo | `01-probabilidad_condicional_diagrama_arbol_…_n3_v1.Rmd` | Tres líneas `NA` en el texto visible, en las 100 semillas |
| En desarrollo | `01-consumo_familiar_generico_…_n2_v1.Rmd` | `test_that` interno falla en 4 de 100 semillas |
| En desarrollo | `Avert.Rmd` | `sample()` mayor que la población en la semilla 31 |
| En desarrollo | `seleccion_canciones_cd_diagrama_arbol_n2_v1.Rmd` | Aviso: la clave cae siempre en la cuarta opción (100 de 100): `sol <- 4` con `exshuffle: FALSE` |
| Pre-desarrollo | `distribucion_contagiados_…_schoice_v2.Rmd` | No determinista: `set.seed(Sys.time)` |
| Pre-desarrollo | `grafica_circular_consumo_agua_…_schoice_v1.Rmd` | Aviso: la clave cae siempre en la opción B (100 de 100), por orden fijo «verbatim del ítem» |

En `01-En-PreDesarrollo`, sin `Lab-Manjaro`, los 47 ejercicios no dan ningún error. Dos de
`barras-campeonato-baloncesto-n3` quedaron en `WARN_STRESS_INDET` (96 y 93 de 100), porque con 8
procesos a la vez el tope de 1500 s no alcanza. Repetidos con `--total 2000` dan 100 de 100 en 627 y
663 s, por debajo de los 1200 s que usa por defecto el paso 9.

## Decisiones pendientes

1. **Añadir las dos líneas a las plantillas** (`A-Produccion/perifericos/*.tex`): no depende de
   actualizar. `solpcielo.tex` tiene cambios sin commit, y hay 114 copias de cada plantilla.
2. **Actualizar a `exams` 2.4-5 y `exams2forms` 0.2-2** y regenerar `renv.lock`, que fija 2.4-3,
   justo la versión con el bug de `answerlist(markup=)`. Va con el ADR-003
   (`.claude/docs/ADR/003-entorno-fijado-pc-render.md`, aún sin commit). El diferencial está
   cerrado: ninguna regresión se sostiene, con el límite de los ejercicios no deterministas.
3. **Exportar con `seed`** en los Semilleros, tras actualizar.
4. **Qué hacer con los defectos de la tabla.** Los de producción requieren una versión nueva, porque
   esa carpeta es inmutable.
5. **Repetir antes de clasificar en el comparador.** Hoy repite la base solo ante `TEXTO_DISTINTO`, y
   no guarda el log de LaTeX: una falla suelta sale como `FALLA_SOLO_CANDIDATA`.

## Repetir la medición

```bash
Rscript .claude/scripts/vigia_rexams.R                       # qué hay nuevo
Rscript -e 'dir.create("/tmp/cand"); install.packages(c("exams", "exams2forms"), lib = "/tmp/cand")'
Rscript .claude/scripts/comparar_version_exams.R --lib /tmp/cand --todos --n 3 --jobs 8 --formatos 60
```

Un `.Rprofile` que antepone `~/R/library` hace inútil `R_LIBS`: por eso `--lib`. No editar los
validadores mientras corre una comparación: un cambio a mitad fabrica diferencias.

Una `FALLA_SOLO_CANDIDATA` o un `TEXTO_DISTINTO` se repiten antes de creerlos: el comparador repite la
base solo ante `TEXTO_DISTINTO`, nunca ante una falla, y no repite la candidata. Un `tryCatch` del
ejercicio alrededor de `include_tikz()` convierte un fallo de LaTeX en texto distinto.


## Herramientas

| Script | Para qué | Salida |
|---|---|---|
| `.claude/scripts/vigia_rexams.R` | Versiones instalada, lock, CRAN y desarrollo, más el NEWS posterior a la instalada, marcado por tema | `VIGIA_AL_DIA`, `VIGIA_HAY_VERSION_NUEVA`, `VIGIA_NO_VERIFICABLE` |
| `.claude/scripts/comparar_version_exams.R` | Qué cambia en los ejercicios con otra versión, en una biblioteca aparte | `COMPARAR_SIN_REGRESIONES`, `COMPARAR_REVISAR`, `COMPARAR_NO_VERIFICABLE` |
| `.claude/scripts/validar_stresstest.R` | Compila el ejercicio completo N = 100, sin LaTeX, con límite por semilla. Paso 9 de los orquestadores | `PASS`, `ERR_STRESS_*`, `WARN_STRESS_*` |
| `.claude/scripts/webquiz_autocontenido.R` | Cuestionario HTML que funciona sin internet; `--revision` da la vista del docente | `WEBQUIZ_OK`, `WEBQUIZ_FALLA` |
| `Auxiliares/exams2nops-escaner/nops_control_calidad.R` | Triaje de escaneos antes de `nops_eval()` | `NOPS_LISTO_PARA_EVALUAR`, `NOPS_REVISAR` |

### Por qué `validar_stresstest.R` no envuelve `stresstest_exercise()`

Conserva su contrato: semillas `1:n`, `setTimeLimit` y `maxit` negativo. Llama a `xexams()` por cuatro
razones medidas:

- La función aborta al final en 20 de 373 ejercicios, y se pierden todas las semillas.
- Descarta el texto, y sin él no se ven las fugas de `NA` ni se comparan versiones.
- Corre en `.GlobalEnv`, donde el ejercicio puede pisar al validador.
- Solo traduce el error de tiempo si el mensaje está en inglés, y este R responde en español.

Las semillas corren en un proceso hijo vigilado por `timeout`: si `setTimeLimit` no alcanza a cortar
un `system()` o una llamada a Python, el corte duro conserva las semillas hechas y nombra la que quedó
abierta. Corren en un solo proceso, como `exams2moodle(n = 100)`. Un fallo que no se reproduce con
una versión suelta puede ser estado arrastrado en Python, y es real para las exportaciones con
`n > 1`.

### Cuestionario sin internet

`exams2webquiz()` deja un HTML autocontenido salvo en un punto: carga MathJax de
`mathjax.rstudio.com` por JavaScript. Sin red, las fórmulas salen como `\(f(x) = x^{2}\)`. El
argumento `mathjax = TRUE` del Semillero no lo evita, y `math_method: mathml` tampoco basta, porque
las opciones de respuesta van en HTML crudo. `webquiz_autocontenido.R` incrusta MathJax 3.2.2 con
salida SVG (2 MB, descargado una vez y aceptado solo si su SHA-256 coincide) y comprueba en Chromium
sin red que todas las fórmulas se componen.

```bash
Rscript .claude/scripts/webquiz_autocontenido.R ejercicio.Rmd --n 3 --dir salida_webquiz --titulo "Taller"
Rscript .claude/scripts/webquiz_autocontenido.R ejercicio.Rmd --n 1 --revision   # vista del docente
```

Medido en 24 ejercicios activos (tablas, PNG como opciones, TikZ, CLOZE): 24 correctos, sin TeX crudo ni recursos externos, y legibles a 360 px en los dos revisados. La vista `--revision` (`auto = TRUE`) trae las respuestas marcadas: nunca se entrega. Límites: las
fórmulas dentro de un desplegable no se pueden componer, y las respuestas siguen en el HTML, solo
ofuscadas, así que sirve para practicar y no para calificar. No se probó en un celular físico.

## Referencias cruzadas

| Tema | Dónde |
|---|---|
| Índice del sistema | `.claude/CLAUDE.md`, «Referencias Rápidas» |
| Paso 9, compilación completa | `.claude/agents/orquestador-schoice.md`, `.claude/agents/orquestador-cloze.md`, `.claude/skills/revisar-schoice/SKILL.md`, `.claude/skills/revisar-cloze/SKILL.md` |
| Muestra N = 100 | Regla #23: `.claude/rules/muestra-estandar-validacion.md` |
| `while` y `repeat` sin cota | Regla #21 F1 y Error 22: `.claude/rules/familias-soluciones-rmd.md`, `.claude/docs/patrones-errores-conocidos.md` |
| Figuras en multicopia | Regla #4: `.claude/rules/graficos-como-opciones.md` |
| `\pandocbounded` y plantillas propias | Regla #18 y Error 16: `.claude/rules/markdown-imagenes-pdf.md` |
| Tablas y contador `{none}` | Regla #20 y Error 21: `.claude/rules/markdown-tablas-pandoc.md` |
| Suites de estas herramientas | `.claude/docs/VALIDATOR_COBERTURA_ICFES.md`, sección «Suites de Test» |
| Escaneos NOPS | `Auxiliares/exams2nops-escaner/scaner_exams2nops.md` |
| Entorno fijado y `renv.lock` | ADR-003, aún sin commit |
