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
visible el tope. El umbral de diversidad de 250 únicas sobre 300 (regla #3 de `codigo-rmd.md`) mide la capacidad del
ejercicio, no lo que se exporta, y no cambia.

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
