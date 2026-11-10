# Auditoría Detractor, FASE 2C r2: CLOZE hermano de `diagrama-venn-juegos-mesa-n3` (MAT-2026-07-014)

**Fecha**: 2026-11-09
**Agente**: `AgenteDetractor` nuevo. No escribí ni corregí nada de lo auditado, y no soy el detractor de la r1.

**Objetos auditados.** Comprobé los md5 en disco al empezar y coinciden con el encargo:
- `.Rmd`: `889d828e8129e11b0ec5dceda0cf108b`.
- `verificar_espacio_cloze.R`: `0bce7172b17d8f8619185764c57c3a92`.
- La sección «CLOZE hermano» de `SUB/.claude/CLAUDE.md`.

**Repo**: no lo modifiqué. Todo lo que ejecuté corrió sobre copias en `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/3530c5eb-2f14-461c-b6c9-761d2d82ab0b/scratchpad/r2_detractor/`.

**Objeciones**: 3. Ninguna es de CORRECCIÓN, crítica ni alta: hay 1 media y 2 bajas.

## Resumen ejecutivo

No encontré ningún defecto de CORRECCIÓN: las seis claves, la Solution y la retroalimentación son verdaderas.

**Mi comprobación de la corrección.** Escribí un oráculo propio que lee el texto. No usa `FORMULAS` del `.Rmd` ni el oráculo del verificador. Lo corrí sobre 120 versiones: las semillas 1 a 100 más 20 canónicas, 25 canónicas en total. Resultado: 0 fallas en:
- la clave de la Parte 1, calculada desde las viñetas;
- los cuatro NUMERICAL contra `exsolution`;
- las marcas de la Parte 6;
- la línea nueva de la Parte 5 en la Solution: texto plano, sumandos iguales a las regiones y total correcto;
- la aritmética de la retroalimentación de los gaps 2 a 5.

**El cambio de la Parte 5 llega igual a los cuatro formatos.** Lo revisé en HTML, XML de Moodle, PDF y DOCX: «Se suman las siete zonas: 14 + 24 + 18 + 14 + 10 + 19 + 23 = 122.», con la suma correcta. Los renders son de las 14:56 y 14:57, posteriores al `.Rmd` (14:54).

**Corrida r5 del verificador: completa.** Terminó a las 15:13:38 con `exit=4`, 0 rojos, 0 pendientes, el md5 coincide y corrieron las 11 secciones.
- `solucion`: 720 versiones sin códigos, y 21 de 21 mutantes cazados solo por su sonda.
- `papel`: 100 de 100 con las cuatro figuras en la página 2 y ninguna tinta fuera de la caja; PP1 a PP3 caen por su sonda.
- `pantalla`: 0 códigos a 320 y 360 px, y PM1 a PM5 caen por su sonda en la semilla 1 a 320 px.

**Hallazgos:**
- **El más importante (H1, MEDIA).** La sección `papel` mide solo PDF de una pregunta, que siempre empieza arriba de la página. Las condiciones de uso de la I-5, que rige la Parte 1, nunca se portaron al CLOZE. Medido: con la plantilla del CLOZE y varias preguntas por PDF, las cuatro figuras de la segunda pregunta quedan partidas en dos páginas en 5 de 5 PDF.
- **H2 y H3 (BAJAS)**: completitud del contrato de mutación, y un canal de superficie en la Parte 6 que se mide y se declara.

## Hallazgos

### H1 · MEDIA · Las condiciones de uso de la I-5 (PDF con varias preguntas) no están portadas al CLOZE, y `papel` sale en verde con el defecto que la I-5 describe

**Qué dicen las invariantes y qué mide el verificador:**
- La I-5 rige la Parte 1 del CLOZE (sección «CLOZE hermano»).
- Su texto fija una condición de uso: `exams2pdf` con varias preguntas, solo con `solpcielo_una_por_pagina.tex`, «porque con `solpcielo.tex` las que empiezan a media página parten sus opciones». En el SCHOICE la vigila la sección `uso`.
- En el CLOZE, `sondas_papel` renderiza siempre `exams2pdf(copia, n = 1L, …)`, una pregunta por PDF. Esa pregunta empieza arriba de la página 2, después de la hoja de respuestas.
- Ni el verificador, ni el HANDOFF, ni la sección «CLOZE hermano» declaran qué plantilla usar al imprimir varias preguntas.

**Qué medí.** Render real, familia B. Hice 5 PDF por plantilla, cada uno con 3 copias del CLOZE: `seed = matrix(c(s, s+100, s+200), 1, 3)`, con s = 1..5. Son 15 preguntas por plantilla, y la cifra es exploratoria. Leí las páginas de las 15 imágenes con `pdfimages -list` (por pregunta, las 4 figuras y la de la Solution):

| Plantilla | Páginas de las imágenes (igual en los 5 PDF) | Pregunta 2 |
|---|---|---|
| `solpcielo_cloze.tex` | 3 3 3 3 4 · **5 5 6 6** 6 · 8 8 8 8 9 | **partida en 5 de 5** |
| `solpcielo_una_por_pagina.tex` | 2 2 2 2 3 · 5 5 5 5 6 · 8 8 8 8 9 | juntas en 5 de 5, y 15 de 15 en total |

**Riesgo.** En un simulacro impreso que incluya este CLOZE con su plantilla, el estudiante ve dos diagramas en una página y dos en la siguiente. Es justo lo que el profesor descartó en la H9 del SCHOICE. No afecta la nota ni la clave.

**Propuesta, de menor a mayor costo:**
1. Declarar la condición en «CLOZE hermano»: para PDF con varias preguntas, solo `../solpcielo_una_por_pagina.tex`. Medido: 15 de 15 grupos de cuatro figuras juntos.
2. Añadir a `papel` un caso con varias preguntas: `rep(RMD, 3)` con esa plantilla y el código `PAPEL_FIGURAS` por pregunta. Su mutante cambia la plantilla a `solpcielo_cloze.tex`, y medí que debe morir por esa sonda. Hay que declarar el N, por ser familia B.
3. Alternativa robusta, para medir: encerrar las dos filas de la Parte 1 en un `\begin{minipage}{\linewidth}` crudo en el chunk `enunciado`. Es código propio del CLOZE, no el bloque firmado. Así no se parten con ninguna plantilla, pero cambia el md5 y obliga a volver a correr todo.

### H2 · BAJA · Contrato de mutación incompleto en las secciones nuevas: «sonda y otra» se acepta sin declararla, hay componentes sin mutante propio, y `solucion` no ve la prosa estática

**(a) «Cazado por su sonda y otra» se acepta en silencio.** `solucion`, `papel` y `pantalla` aceptan `cazado_por_su_sonda_y_otra` sin exigir que se declare (líneas 1198, 1280 y 1388). El contrato dice que un mutante muere solo por su sonda, y en `mutantes` la M11 sí declara la otra. Hoy no se activa, porque los 29 mutantes de esas secciones cayeron solo por su sonda, pero la guarda lo deja pasar.

**(b) Componentes sin mutante propio.**
- `SOL_OTRA_ENCUESTA` solo tiene a S15, que cubre la Parte 3. Las plantillas p2, p4 y p5 no tienen mutante.
- La p5 es justo el párrafo que cambió en esta pasada. Además, al salir de `$…$` dejó de estar bajo `SOL_ARIT`, que solo lee matemáticas. Hoy su única guarda aritmética es la plantilla p5.
- `SOL_AFIRMACION` solo tiene a S12, que cubre «totales»; la componente «trampa» no tiene mutante.
- `SOL_CONCORDANCIA` solo tiene a S17, que cubre `ellos`.

**Lo que medí.** Escribí 6 mutantes propios en memoria, con la capa plantilla y la línea base limpia en la misma semilla. Todos mueren por la sonda esperada:

| Mutante | Sonda que lo caza | Semilla |
|---|---|---|
| P5, un sumando +1 | `SOL_OTRA_ENCUESTA` | 1 |
| P5, total +1 | `SOL_OTRA_ENCUESTA` | 1 |
| P5, una zona omitida | `SOL_OTRA_ENCUESTA` | 1 |
| «pero no t2» en la trampa | `SOL_AFIRMACION` | 1 |
| «restados» solo en la Solution | `SOL_CONCORDANCIA` | 9 |
| «ellos» solo en la Solution | `SOL_CONCORDANCIA` | 2 |

Las sondas funcionan. Lo que falta es que el archivo lleve esa prueba.

**(c) `solucion` no ve la prosa estática.** `texto_solucion` evalúa solo el chunk `solucion`. Un párrafo escrito como Markdown estático en la Solution, fuera de los chunks, no lo ve. La I-4.7 dice que «un párrafo nuevo… da rojo», pero eso solo vale para el texto que emite el chunk. Lo único que lo para es el md5 cableado, y en esta pasada el coordinador lo volvió a cablear. No medí si `v6` o `moodle` lo detectan.

**Propuesta:**
- Aceptar «y otra» solo con un campo declarado en el mutante, por ejemplo `otra = "…"`.
- Añadir como S22 a S25 los mutantes de la tabla, empezando por los dos de la Parte 5.
- Añadir la sonda `SOL_ESTATICO`: entre `Solution` y `Meta-information`, fuera de los dos chunks, solo puede haber líneas en blanco. Su mutante inserta una línea estática.

### H3 · BAJA · DIAGNOSTICIDAD (canal nuevo: se mide y se declara, sin obligación, §P7-C) · La Parte 6 hereda una asimetría de los datos

**De dónde sale.** El generador sortea `p12` de forma simétrica respecto del centro. Pero el filtro de aceptación pide más a las respuestas que a los datos: las respuestas no pueden estar entre los números impresos en la Parte 1. Como el centro es respuesta y `p12` es dato, la simetría se rompe. Y el orden de las dos zonas en cada afirmación sale de los datos.

**Lo que medí** (N = 100):
- La probabilidad de que el centro supere a `p12` es 0,36. En los otros pares va de 0,48 a 0,53.
- La mejor regla de superficie (par, palabra y qué zona va primero), aprendida y medida en la misma muestra, acierta el 59,8 % de las afirmaciones.
- Su techo nulo, con 200 permutaciones globales de la verdad, da una media de 58,9 % y un p95 de 61,3 %. Exceso: **+0,9 pp**, sin canal.
- Las celdas del par centro/`p12` dan 0,71 y 0,33, pero con n = 17 y 15. Por la regla #23 son **NO CONCLUYENTES**.

**Propuesta.** Declararlo como residuo medido en el HANDOFF. No hay que tocar el `.Rmd`.

## Verificado sin objeción

- **CORRECCIÓN.**
  - Mi oráculo dio 0 fallas en 120 versiones (ver el resumen).
  - En la sección `moodle` de la r5: 100 preguntas sin códigos, la imagen de la clave coincide con el entorno de su semilla en 100 de 100, y ninguno de los 400 nombres de archivo es no neutral.
  - Las afirmaciones absolutas de la Solution están atadas a sus `stopifnot`, como pide la regla #13 v1.1.
- **E1 y la firma C-4.**
  - E1 da 0 diferencias en 100 variantes y 20 canónicas.
  - `dibujo_opciones` es idéntico al del SCHOICE, así que la I-4.6 queda cubierta por identidad.
  - Las 9 reglas de las variantes y las 12 de la canónica quedan dentro de lo firmado.
  - Ningún cruce de las Partes 2 a 6 hacia la Parte 1 llega a +8 pp, y el control sin la V11 dispara: la regla se vuelve aplicable en 46 de 100.
- **Contrato en las secciones nuevas.**
  - Cada mutante declara su sonda.
  - `mutar` exige que el patrón aparezca una sola vez.
  - Hay línea base sin mutar en la misma semilla o muestra.
  - `materia` comprueba que el cambio se materialice, en los mutantes de `data_generation`.
  - Cada mutante cambia una sola cosa.
- **Muestras (regla #23).**
  - `solucion`: 100 versiones por rama en las variantes y 20 en la canónica; es un estrato válido, sin n < 20.
  - `papel` y `pantalla`: N = 100, familia B.
- **La caja que mide `pantalla` es más estricta que la de Moodle.** A 320 px las figuras miden 248 px y la de la Solution 264 px. La caja del arnés es entonces de 264 px, frente a los 270 px de `.qtext` en el Moodle del colegio (medición del 2026-10-30, en la memoria del proyecto). A 360 px, las figuras miden 280 px.
- **Legibilidad, solo estimada por cálculo, sin captura.** A 248 px, la cifra de 14,5 bp a 216 ppp se ve de unos 15 px CSS de alto. La comprobación visual (6b) sigue pendiente.

## No auditado

- La 6b visual a 320, 360 y 1024 px sobre 889d828e. Está pendiente según el HANDOFF.
- La maqueta del DOCX.
- El render con MathJax en el Moodle real.
- Si `v6` o `moodle` detectan prosa estática (H2c).
- La sección `mutantes` (tandas 1 y 2) solo la revisé por su resultado en la r5, porque no cambió en esta pasada.

## Próximos pasos (pasada 3, la última por §P7-D)

1. **H1**: declarar la plantilla para varias preguntas y añadir el caso con varias preguntas a `papel`, con su mutante.
2. **H2**: añadir los mutantes de la Parte 5, la declaración obligatoria de «y otra» y `SOL_ESTATICO`.
3. **H3**: declarar el residuo, sin cambios en el `.Rmd`.

CITAS_DETRACTOR
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/.claude/CLAUDE.md:90 | con varias preguntas, con la plantilla local
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/.claude/CLAUDE.md:125 | I-1 a I-5 y la firma del 2026-11-06 rigen la Parte 1.
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/verificar_espacio_cloze.R:1225 | exams2pdf(copia, n = 1L, seed = semilla, name = "x"
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/verificar_espacio_cloze.R | AUSENTE: una_por_pagina
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/verificar_espacio_cloze.R:1198 | res %in% c("cazado_por_su_sonda", "cazado_por_su_sonda_y_otra")
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/verificar_espacio_cloze.R:1129 | list(id = "S15_otra_encuesta_con_otra_cifra", sonda = "SOL_OTRA_ENCUESTA"
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/verificar_espacio_cloze.R:1030 | Se suman las siete zonas: ([0-9 +]+) = (
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/verificar_espacio_cloze.R:835 | for (x in regmatches(l, gregexpr(
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/verificar_espacio_cloze.R:828 | x <- utils::capture.output(eval(sol, new.env(parent = e)))
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/diagrama_venn_juegos_mesa_aleatorio_interpretacion_representacion_n3_cloze_v1.Rmd:626 | al_menos2, ".** Se suman las siete zonas
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/diagrama_venn_juegos_mesa_aleatorio_interpretacion_representacion_n3_cloze_v1.Rmd:386 | p12 <- (ctr - 13L + sample(1:11, K, TRUE)) %% 12L + 13L
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/diagrama_venn_juegos_mesa_aleatorio_interpretacion_representacion_n3_cloze_v1.Rmd:396 | ok_resp <- rowSums(matrix(resp %in% impresos_p1, ncol = 4L)) == 0L
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/diagrama_venn_juegos_mesa_aleatorio_interpretacion_representacion_n3_cloze_v1.Rmd:438 | if (verdad != v6[i]) { t0 <- x; x <- y; y <- t0 }
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS
