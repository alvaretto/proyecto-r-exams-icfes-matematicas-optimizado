# Regla #26 — Material del aula: papel oficio, NOPS, Moodle y celulares sin internet

> Versión compacta, la que se carga en cada sesión: `.claude/rules/material-aula.md`. Este es el texto íntegro: origen,
> mediciones, procedimientos e historial. Se lee antes de crear o cambiar un semillero, una plantilla de impresión o una
> exportación, y antes de modificar la regla.

## Principio

Lo que llega al aula se verifica en la salida real que usa el profesor: la hoja oficio impresa, la hoja NOPS escaneada,
el XML de Moodle y el celular sin red. Que el `.Rmd` pase sus validaciones (arsenal, detractor, diversidad) no prueba
nada de eso: todas miran el ejercicio, no el papel ni el aula. Los defectos de esta regla aparecieron con el `.Rmd`
aprobado y sin un solo rojo en sus validaciones.

## Origen

Todo salió de `A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3` (MAT-2026-07-014), SCHOICE y CLOZE:

| Fecha | Hecho |
|---|---|
| 2026-08-15 | El profesor: «Ya no se necesitan 300 preguntas. Son y serán máximo 100.» |
| 2026-11-06 | El profesor imprime PDF y NOPS en hojas oficio. Se mide el SCHOICE en 216 × 330 mm: figuras de 5,5 cm, plantilla local de una pregunta por página, NOPS a dos columnas y centrado en oficio con `pdfjam`, y escaneo simulado. |
| 2026-11-06 | Un Knit de RStudio le antepone 6 líneas de YAML al `.Rmd` firmado: su md5 pasa de `f886dfaa…` a `2608498b…`, y el verificador da exit 3 (`FIRMA_MD5`). |
| 2026-11-07 | El semillero del SCHOICE se reemplaza por la copia genérica de `perifericos/`: vuelve la plantilla compartida, el NOPS a una columna y en A4, y desaparecen los cinco literales que vigila su sección `uso`. |
| 2026-11-09 | FASE 2C r3 del CLOZE (H1): en el PDF impreso, el gap de la Parte 1 sale barajado y «Cómo marcar» pide la letra por posición. El profesor elige `exshuffle = FALSE` solo en `exams2pdf()` («H1 c») y baja las exportaciones a 100 («H2 100»). |
| 2026-11-09 | El profesor declara oficial la copia en disco de `perifericos/` («La versión oficial siempre estará en …/A-Produccion/perifericos»), que no coincidía con git en seis archivos, y aprueba sumarle cuatro mejoras («Agregar las 4»). |

## Mediciones

| Qué | Resultado | Fuente |
|---|---|---|
| PDF de 3 preguntas con `solpcielo.tex` compartida | Figuras por página p2:4 p3:1 p4:3 p5:2 p6:2 p7:3: las que empiezan a media página se parten | `diagrama-venn-juegos-mesa-n3/HANDOFF.md`, «Límites declarados» |
| PDF de 5 preguntas con `\clearpage` por pregunta | p2:4 p3:1 p5:4 p6:1 … p14:4 p15:1: las cuatro opciones juntas y la figura de la Solution en la página siguiente | sección `uso` de `verificar_espacio.R` |
| PDF de 10 preguntas del semillero (2026-11-09) | 31 páginas de 216 × 330 mm, el mismo patrón en las 10 | `diagrama-venn-juegos-mesa-n3/HANDOFF.md`, «Condiciones de uso» |
| NOPS con `twocolumn = FALSE` | Las opciones se parten hasta con figuras de 5 cm | memoria del 2026-11-06 |
| NOPS con `twocolumn = TRUE, samepage = TRUE` | 4 figuras por página en las 5 preguntas (2026-11-06) y en las 10 (2026-11-09) | sección `uso` y corrida del semillero |
| Escaneo simulado del NOPS | Leídas: A4 sin cambios, centrada al 100 % en 216 × 330 y ajustada al ancho y centrada. Error: al 100 % en la esquina de 216 × 330 y centrada en 216 × 356. Con `normalizar_escaneos_nops.py`, las 8 colocaciones probadas se leen | `diagrama-venn-juegos-mesa-n3/salida/escaneo_simulado/lecturas.md` |
| Gap de un CLOZE impreso, sin `exshuffle = FALSE` | 1 de 4 preguntas en el orden I-IV | coordinador, 2026-11-09 |
| Gap de un CLOZE impreso, con `exshuffle = FALSE` en `exams2pdf()` | 4 de 4 y 9 de 9; con N = 100, el gap en el orden I-IV y la clave registrada en el diagrama correcto en 100 de 100, con la clave en (a) 25, (b) 20, (c) 32 y (d) 23 | `Cloze/salida/t5/medir_n100_pdf.txt`, repetido por el detractor r4 |
| `exshuffle` como argumento (exams 2.4.5) | Formal en `exams2pdf()` y `exams2html()`; `exams2moodle()` y `exams2canvas()` abortan con él («el argumento no fue usado») | mutantes U11 y U16 de `Cloze/verificar_espacio_cloze.R` |
| Moodle con `seed = matrix(seq_len(n), ncol = 1)` | Dos corridas dan el mismo XML; sin `seed`, difieren. Repetido el 2026-11-09 con `SemilleroMoodle_v2.R` y `SemilleroCloze.R` de `perifericos/` (3 versiones, dos corridas cada uno) | `REXAMS_VERSIONES.md` y prueba del 2026-11-09 |
| `exams2webquiz()` sin red | Las fórmulas salen como `\(f(x) = x^{2}\)` | `REXAMS_VERSIONES.md`, «Cuestionario sin internet» |
| `webquiz_autocontenido.R`, SCHOICE del diagrama de Venn, 10 versiones | `WEBQUIZ_OK`: 175 fórmulas compuestas sin red, 0 recursos externos; trae el botón de solución | corrida del semillero, 2026-11-09 |
| Quitar el YAML del Knit | El cuerpo desde la línea 7 tenía el md5 firmado; sin el YAML, el verificador vuelve a exit 4 | `diagrama-venn-juegos-mesa-n3/HANDOFF.md` |

## Procedimientos

### Versión oficial de `perifericos/`

La copia en disco de `A-Produccion/perifericos/` es la oficial. Si git no coincide con ella, git se pone al día con la
copia en disco, no al revés. Una mejora para esos archivos se propone al profesor y se aplica en disco con su visto bueno;
después se hace el commit. Las copias de cada ejercicio (`pcielo.tex`, `solpcielo.tex`, los semilleros) derivan de la
oficial: cuando ella cambia, se rehacen, y la sección `uso` del ejercicio lo detecta si no se hizo (`USO_PLANTILLA` compara
la plantilla local con la oficial en disco).

Desde el 2026-11-14 son también versión oficial las cuatro plantillas del motor de cuadernillos en oficio a dos columnas
(`Auxiliares/Examenes_Fin_de_Periodo/00-Cuadernillos-Oficio-2Col/generar_cuadernillos.R`): `cuadernillo_oficio_2col.tex`
(cuadernillo del estudiante), `retro_celular.tex` (retroalimentación de 90 × 160 mm), `pandoc_cuadernillo.tex` y
`referencia_oficio_2col.docx` (Word). El profesor decidió promoverlas «ya, al terminar el proyecto», sin condición de aula;
se copiaron como archivos nuevos y el motor las lee de `perifericos/`. Sus mutantes dañan una copia en un directorio temporal
(`CUADERNILLOS_PLANTILLAS`), nunca las oficiales.

### Numeración de las preguntas: listas propias, no `enumerate`

Cuando una lista numerada de un ejercicio no empieza en 1 (por ejemplo «3. Sumar…»), pandoc escribe en el `.tex`
`\setcounter{enumi}{N}`. Una plantilla que numera las preguntas con `enumerate` usa ese mismo contador: el número de la
pregunta siguiente cambia, y la lista del ejercicio sale con la etiqueta de segundo nivel («a)») en lugar de los números que
escribió el autor. Medido el 2026-11-14:

| Plantilla | Antes | Después |
|---|---|---|
| `retro_celular.tex`, examen real de 11 preguntas | 1, 2, 3, **2**, 4, 5, 6, 7, 8, 9 (la clave del encabezado decía 1 a 11) | 1 a 11 |
| `solpcielo.tex`, tres preguntas, la primera con las listas «3., 4.» y «5.» | 1, **5**, **6**; la lista como «a), b)» | 1, 2, 3; la lista como «3., 4.» |

Por instrucción del profesor del 2026-11-14 («Corregirla ahora»), `solpcielo.tex` y `solpcielo_cloze.tex` numeran con dos
listas propias de `enumitem` (`\newlist{preguntas}{enumerate}{1}` y `\newlist{opciones}{enumerate}{1}`), igual que las
plantillas del cuadernillo. Las etiquetas de las preguntas («1.») y de las opciones («a)») no cambian. Comprobado con ocho
conjuntos de ejercicios reales, página a página, a 100 ppp: los cinco ejercicios de los tres subproyectos que copian estas
plantillas (`serie-seno-aproximacion-n3`, `diagrama-venn-juegos-mesa-n3` y `vueltas-llanta-perimetro-n4`, SCHOICE y
CLOZE) dan 0 píxeles distintos; lo único que cambia es la etiqueta de las listas numeradas y el número de la pregunta
siguiente. La plantilla local `diagrama-venn-juegos-mesa-n3/solpcielo_una_por_pagina.tex` se rehízo desde la oficial nueva
y su verificador volvió a su estado aprobado (`verificar_espacio.R todo`, exit 4, sin rojos en sus sondas).

Lo que queda igual: de los 144 archivos `solpcielo*.tex` del repo, 131 ya eran versiones anteriores a la oficial (34 en
`03-En-Produccion/`, que no se toca) y 10 son copias idénticas de la oficial anterior, en esos tres subproyectos. No se
rehicieron: sus ejercicios dan el mismo PDF con una y con otra, y la aprobación sellada del CLOZE del diagrama de Venn
incluye el md5 de su plantilla local. Se rehacen en el próximo ciclo de cada ejercicio. `pcielo.tex` y `pcielo_nosol.tex` no
tienen el defecto: las convierte pandoc, que no comparte contadores entre listas.

### Plantilla local de una pregunta por página

Copia de la plantilla oficial con dos cambios: una línea de comentario que dice de dónde sale y `\clearpage` al empezar el
entorno `question`.

```latex
%% Copia de A-Produccion/perifericos/solpcielo.tex. Único cambio: \clearpage, para que cada pregunta empiece arriba y sus cuatro opciones queden juntas.
\newenvironment{question}{\clearpage\item \textbf{Escenario:}\newline}{}
```

En un CLOZE se parte de `solpcielo_cloze.tex`, que conserva el recuadro «Cómo marcar». Se usa con
`template = "solpcielo_una_por_pagina"` (o el nombre del CLOZE) en `exams2pdf()` del semillero.

### NOPS en oficio

```r
exams2nops(rep(archivo_examen, numpreg), n = copias, ..., duplex = TRUE,
           twocolumn = TRUE, samepage = TRUE, header = "\\raggedbottom")
for (pdf in list.files(dir_salida, paste0("^", nombre_nops, "[0-9]+\\.pdf$"), full.names = TRUE))
  system2("pdfjam", c("--papersize", shQuote("{216mm,330mm}"), "--noautoscale", "true", "--quiet",
                      shQuote(pdf), "-o", shQuote(sub("\\.pdf$", "_oficio.pdf", pdf))))
```

Se imprime el `*_oficio.pdf` a tamaño real. Falta la prueba con papel y escáner reales: la del 2026-11-06 fue simulada.

### Examen con cuadernillo propio (hoja NOPS aparte)

Para un examen de fin de periodo o un simulacro, el estudiante recibe un cuadernillo en oficio a dos columnas y, aparte, la
hoja de respuestas NOPS. Lo arma el motor `Auxiliares/Examenes_Fin_de_Periodo/00-Cuadernillos-Oficio-2Col/generar_cuadernillos.R`
y lo vigila `verificar_cuadernillos.R` (guardas C1 a C15; todas menos C10, que es la revisión visual, con su mutante). Cinco normas salen de sus mediciones
(2026-11-13 y 2026-11-14, R/exams 2.4.5, pandoc 3.11):

- **El NOPS va con `twocolumn = FALSE`.** Es una excepción al procedimiento «NOPS en oficio» de arriba, que pide
  `twocolumn = TRUE` cuando las preguntas se leen en el PDF de `exams2nops()`. Aquí ese PDF solo aporta la hoja (páginas 1 y
  2, con `duplex = TRUE`; la segunda sale en blanco), y `twocolumn = TRUE` aborta con una tabla Markdown: «longtable not in
  1-column mode».
- **Una sola matriz de semillas** (versiones × preguntas) para la hoja, los dos cuadernillos, la clave, la retroalimentación
  y el Word: `exams2nops(seed = S)` y, por versión, `exams2pdf(seed = S[i, , drop = FALSE])`. El número de versión impreso es el
  ID del NOPS (`aammdd` + cinco cifras desde `startid`). Dos sesiones con la misma fecha llevan `startid` distintos (1 y 101)
  para que sus IDs no choquen. Medido con 11 ejercicios reales y 3 versiones: el texto normalizado y la clave coinciden en
  las cuatro salidas (33 de 33).
- **Fuga entre ejercicios.** R/exams teje todos los ejercicios de un examen en una misma sesión de R. Un `.Rmd` que hace
  `knit_engines$set(python = …)` cambia el motor para los siguientes: seis ejercicios apagaban los chunks `{python}` de los
  que venían después y la figura desaparecía sin aviso. `validar_stresstest.R` mide cada archivo por separado y no lo ve; se
  ve con el examen completo (guarda C12: escaneo del `.Rmd` y comparación del estado de knitr antes y después de cada
  ejercicio). El remedio va en el chunk, nunca con un `knit_engines$set()` global; la única llamada global admitida es la que
  restaura el motor: `knit_engines$set(python = reticulate::eng_python)`.
- **Estudiante y docente a la vez** (instrucción del profesor, 2026-11-14). Pedir el cuadernillo produce siempre dos PDF por
  versión, con la misma plantilla y las mismas semillas: el del estudiante, sin retroalimentación, y el del docente, con la
  clave en la primera página y la retroalimentación debajo de cada pregunta. La guarda C15 comprueba con trozos de cada
  Solution que ninguna llega al del estudiante y que todas están en el del docente. Medido con el examen de 11 preguntas:
  4 caras el del estudiante y 8 el del docente; con una sesión de 25 preguntas, 8 y 16.
- **Las preguntas se parten por defecto** (instrucción del profesor, 2026-11-14: «Priorizar partirlas, teniendo en cuenta la
  LEY de acomodar la mayor cantidad de preguntas respetando los estándares de calidad visual»). `partir_preguntas = FALSE`
  deja cada pregunta entera en una columna. Medido: el examen de 11 preguntas pasa de 4 caras a 6 en dos de sus tres
  versiones; y un ítem cuya pregunta mide casi una columna (12 versiones del diagrama de Venn) no cabe en la primera página:
  la tinta llega a 7,0 mm del borde inferior y la guarda C3 lo reprueba.

El uso, los tiempos medidos y los límites conocidos del motor están en el §10 de su `INVESTIGACION.md`.

### CLOZE impreso con gráficas rotuladas

El `.Rmd` sigue con `exshuffle: TRUE` (regla 6 de `codigo-rmd.md`). Solo la llamada que imprime deja de barajar:

```r
exams2pdf(rep(archivo_examen, config$preguntas_pdf), n = 1, ...,
          template = "solpcielo_cloze_una_por_pagina", exshuffle = FALSE)
```

La clave sigue sorteada, porque el `.Rmd` mezcla las gráficas antes de rotularlas. HTML, Moodle y Canvas no llevan el
argumento: ahí el estudiante elige el texto, no una letra. La sección `uso` exige el orden I-IV en el PDF real y que la clave
que registra `exams2pdf()` apunte a la gráfica correcta, con mutantes que quitan el argumento, lo ponen en Moodle o en HTML,
y permutan los rótulos.

### Exportaciones

Máximo 100 versiones en Moodle, Canvas y HTML. En Moodle, `seed = matrix(seq_len(copias), ncol = 1)`, para exportar las
semillas 1 a 100 que valida `validar_stresstest.R` en el paso 9. Un `stopifnot(copias <= 100)` en el semillero hace
visible el tope. El umbral de diversidad mide la capacidad del ejercicio, no lo que se exporta. Desde el 2026-11-13 es de 100 únicas
sobre 150 renders (regla #3 de `codigo-rmd.md`; antes, 250 sobre 300): alcanza para exportar 100 versiones distintas.

### Cuestionario sin internet

```r
Rscript .claude/scripts/webquiz_autocontenido.R archivo.Rmd --n 10 --dir salida --nombre <nombre>_interactivo \
        --titulo "Evaluación Interactiva de Matemáticas ICFES"
```

Desde un semillero, la ruta del script sale de `git rev-parse --show-toplevel`; si el exit no es 0, el cuestionario no va
al aula. `SemilleroUnico_v2.R` de `perifericos/` y el del SCHOICE del diagrama de Venn ya lo hacen así.

### Semillero del ejercicio y su sección `uso`

Si el ejercicio tiene condiciones de impresión medidas, su semillero local:

1. lleva una cabecera que dice de dónde salió, qué cambia frente a `perifericos/` y que no se reemplace por la copia genérica;
2. escribe en `salida/semillero/` (o la carpeta que declare su `HANDOFF.md`);
3. está vigilado por una sección `uso` del verificador del ejercicio: los literales de impresión, exactamente una vez cada
   uno; la plantilla local frente a la oficial; un render real de varias preguntas (figuras por página, tamaño de hoja,
   orden del gap); y un mutante por cada componente.

Ejemplos: `diagrama-venn-juegos-mesa-n3/verificar_espacio.R` (sección `uso`, 5 literales) y
`diagrama-venn-juegos-mesa-n3/Cloze/verificar_espacio_cloze.R` (sección `uso`, mutantes U1 a U17).

Segundo caso (2026-11-13): `serie-seno-aproximacion-n3`, SCHOICE sin opciones gráficas. Su `SemilleroUnico_v2.R` centra el NOPS en
oficio con `pdfjam` (`--noautoscale`) sin `twocolumn`, `samepage` ni plantilla de una pregunta por página, y lo vigila
`salida/verificar_uso_semillero.R`: literales de los dos semilleros, un render real de 2 preguntas con las medidas de `pdfinfo` y
un mutante sin el paso de `pdfjam` (detalle en su `HANDOFF.md`, «Semilleros y papel oficio (2026-11-13)»).

### YAML de un Knit de RStudio

No se pulsa Knit sobre un `.Rmd` de R/exams; para ver el ítem, el semillero o `exams2html()`. Si el YAML aparece:

1. no se quita sin el visto bueno del profesor;
2. se comprueba que ningún script dependa del desfase de líneas (en el diagrama de Venn, el script de empalme y E1 del CLOZE
   aceptaban las dos formas);
3. `tail -n +7 archivo.Rmd > temporal`, se comprueba que el md5 del temporal es el firmado y solo entonces se sobrescribe,
   con una copia del archivo con YAML guardada antes.

## Defensa y alcance

No hay guarda global: ningún validador del repo mira el papel, la hoja NOPS ni el celular. La defensa es la sección `uso` de
cada ejercicio que se imprime y los orquestadores al crear su semillero. Una guarda global sobre los semilleros (tope de 100,
`seed`, cuestionario) exige antes un barrido: muchos semilleros copiados a los ejercicios llevan 250 o 300 copias y
`exams2webquiz()` solo, y un gate sin barrido daría rojos en masa. Los semilleros aprobados antes de esta regla se ajustan
en el próximo ciclo de su ejercicio: el de `diagrama-venn-juegos-mesa-n3/Cloze` exporta a Moodle sin `seed`.

## Referencias cruzadas

- Regla #4 (`graficos-como-opciones.md`): opciones gráficas; su impresión remite aquí.
- Regla #18 (`markdown-imagenes-pdf.md`) y regla #20 (`markdown-tablas-pandoc.md`): lo que las plantillas de `perifericos/`
  definen y lo que cada ejercicio debe traer igual.
- Regla #23 (`muestra-estandar-validacion.md`): N = 100 para medir; el tope de exportación de 100 vive aquí.
- `codigo-rmd.md`, reglas 6 (`exshuffle`) y 15 (Knit).
- `patrones-errores-conocidos.md`, Errores 42 y 43.
- `REXAMS_VERSIONES.md`: `seed` en los Semilleros y cuestionario sin internet.
- `.claude/agents/orquestador-cloze.md`, Incidente G: gráficas rotuladas en el enunciado y su versión impresa.
- `Auxiliares/exams2nops-escaner/`: `normalizar_escaneos_nops.py` y `nops_control_calidad.R`.

## Historial

| Versión | Fecha | Cambio |
|---|---|---|
| 1.0 | 2026-11-09 | Regla nueva. Sale de `diagrama-venn-juegos-mesa-n3` y de dos decisiones del profesor: `perifericos/` en disco es la versión oficial y recupera la semilla, el tope de 100, `\pandocbounded` y `none`, y el cuestionario sin internet. |
| 1.2 | 2026-11-14 | Examen con cuadernillo propio y hoja NOPS aparte: `twocolumn = FALSE` como excepción declarada, una sola matriz de semillas con `startid` por sesión, y la fuga del estado de knitr entre ejercicios. Sale del motor de `00-Cuadernillos-Oficio-2Col/`. Sus cuatro plantillas pasan a ser versión oficial en `perifericos/`. `solpcielo.tex` y `solpcielo_cloze.tex` numeran las preguntas con listas propias, por instrucción del profesor. El cuadernillo sale siempre en dos versiones, estudiante y docente, y las preguntas se parten por defecto (`partir_preguntas`), también por instrucción del profesor. |
