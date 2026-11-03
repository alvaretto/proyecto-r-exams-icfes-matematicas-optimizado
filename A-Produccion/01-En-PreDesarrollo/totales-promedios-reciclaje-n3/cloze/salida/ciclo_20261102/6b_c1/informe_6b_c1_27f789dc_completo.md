<!-- Reporte íntegro del auditor-visual-html (6b pareada, tanda C1), recuperado de su transcripción (Paso 0 de la regla #9) por el orquestador el 2026-11-03. -->

# Informe 6b C1 pareada: CLOZE `totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1`

**Versiones:** NUEVO `27f789dc` y REFERENCIA `f98f9dda`, ambas renderizadas con `exams2html(..., mathjax = TRUE)`.

**N declarado:** 100 semillas por versión (semillas 1 a 100), a 360 y 320 px. Son 400 páginas medidas, con 0 errores y `innerWidth` igual al ancho pedido (360 y 320).

**Informe en disco:** el informe íntegro que pediste (`informe_6b_c1_27f789dc.md`) quedó muy abreviado. Los datos completos están en `metricas.json` y `resumen_texto.txt`, y el contenido útil está en este texto.

## Instrumento

- Usé `cap_s1b2.py` con tres cambios, copiados a `cap4.py`, `cap5.py` y `cap6.py`.
  - **Región:** cada medida se etiqueta como Parte 1 (P1) o Partes 2 a 6 (R). P1 abarca el enunciado y las opciones de la Parte 1, la lista «Respuestas · Parte 1», el párrafo «Parte 1.» de «Procedimiento correcto», el análisis «Parte 1 · …» de los distractores, «En la Parte 1,» de «Caso específico» y el feedback (a).
  - **Red bloqueada:** Chromium corre con `--host-resolver-rules` para que la red quede bloqueada.
  - **Medición repetida:** repetí la medición completa y salió idéntica (0 diferencias en 400 páginas).
- El bloqueo de red fue necesario. Con red, el script de MathJax de la CDN a veces reemplazaba el MathML nativo: `mathCount` valía 0 en la mitad de las páginas y las métricas no se repetían. Una primera corrida con red no es válida y la descarté.
- Con el MathML nativo, la tipografía de las fórmulas no es la de MathJax local de Moodle.
- En `exams2html` las opciones del CLOZE salen en un solo `<p>` unidas con « / », y los controles de Moodle no se ven.

## Métricas de L-6 (cortes/casos; semillas 1 a 100)

| Métrica | Región | NUEVO 360 | NUEVO 320 | REF 360 | REF 320 |
|---|---|---|---|---|---|
| Fórmula + unidad partidas (unidad fuera de la fórmula) | P1 | 0/0 | 0/0 | 160/1158 | 274/1158 |
| Fórmula + unidad dentro de la fórmula, partida (`kilos.` dentro del MathML) | P1 | 0/1158 | 0/1158 | n/a | n/a |
| Signo huérfano tras fórmula | P1 | 0 (sin casos) | 0 (sin casos) | 0/42 | 0/42 |
| Signo huérfano tras otro elemento | P1 | 0/100 | 0/100 | 0/100 | 0/100 |
| Renglón que empieza con espacio tras fórmula | P1 | 0/1002 | 0/1002 | 0/1264 | 0/1264 |
| «cifra kilos/curso(s)» partidos en texto | P1 | 0/2003 | 0/2003 | 317/2003 | 335/2003 |
| «El Bosque»/«La Pradera» partidos fuera de la tabla | P1 | 0/475 | 0/475 | 79/475 | 98/475 |
| Cifra-unidad y El/La dentro de la tabla | P1 | 0/400 y 0/24 | 0/400 y 0/24 | 0/400 y 0/24 | 0/400 y 0/24 |
| Fórmula + unidad partidas | R | 259/1249 | 206/1249 | 259/1249 | 206/1249 |
| Renglón que empieza con espacio tras fórmula | R | 259/1249 | 206/1249 | 259/1249 | 206/1249 |
| «cifra kilos/curso(s)» partidos en texto | R | 0/1355 | 0/1355 | 0/1355 | 0/1355 |
| Signo huérfano en R | R | sin casos | sin casos | sin casos | sin casos |

- **Parte 1:** el NUEVO da 0 en todo y la REFERENCIA, usada como control positivo, es positiva en cifra-unidad, El/La y fórmula + unidad.
- **Mismo evento contado dos veces:** en R, «renglón que empieza con espacio» y «fórmula + unidad partidas» son el mismo corte (el U+00A0 inicial arrastra la unidad al renglón siguiente).
- **Comparación pareada en R:** las dos versiones dan idéntico resultado. Es 259/1249 a 360 y 206/1249 a 320 en ambas, así que la invariancia de las Partes 2 a 6 se confirma en lo visual.
- **Desbordes:** 0 en ambas versiones, con 0 spills y 0 tablas fuera de su caja o con scroll. Ninguna fórmula quedó partida por dentro (`mathSplit` = 0).
- **Alturas:** nuevo − referencia a 360 px da 0 a +54 px, con media +25,9 px y 84/100 semillas distintas. A 320 px da −18 a +36 px, con media +4,5 px y 47/100 distintas. La altura media del documento es de unos 8100 px a 360 px y 9190 px a 320 px.

## BAJA conocidos del CLOZE: siguen iguales en el nuevo

- **(a) BAJA 1:** en la Solution de las Partes 3 a 6, la unidad queda en el renglón siguiente a la fórmula en 259/1249 casos a 360 px y 206/1249 a 320 px. Son exactamente las mismas cifras que en la referencia. Sigue sin tratarse.
- **(b) «sirven» y «cuadrado.»:**
  - «sirven» (el encabezado «Parte 2 · Lecturas del promedio que no sirven») queda solo en su renglón a 360 px en 100/100 semillas, en las dos versiones.
  - «cuadrado.» queda solo a 320 px en 32 opciones de 400 (el conteo es por opción, 4 por semilla), igual en las dos versiones.
- **(c) Palabra suelta al final de una opción de la Parte 2:** a 320 px hay 107/400 opciones (`cursos.`, `grado.`, `grados.`, `cuadrado.`) y a 360 px 59/400. Los valores son idénticos en NUEVO y REFERENCIA. En las viñetas de la Solution hay 67/300 con `cursos.` suelto a 320 px y 59/300 con `divisiones.` suelto a 360 px, también iguales en ambas versiones.

## Hallazgos

- **BAJO (nuevo, introducido por esta tanda):** en la Parte 6 de la Solution, a 360 px, «diferencia.» queda sola en el último renglón en 99/100 semillas. En la referencia eso ocurre en 0/100, y a 320 px en 0/100 en ambas versiones. La causa es el texto nuevo («si y solo si … es mayor que la diferencia»). Es cosmético: la lectura no se rompe. Lo vi en `recortes/nuevo_s007_360_P1.png` (bloque Parte6).
- **BAJO (estético):** en la Parte 1 el NUEVO muestra la unidad dentro de la fórmula, en tipografía serif y más pequeña que el texto («= 200 kilos.»). En la referencia la unidad iba fuera, en sans normal. Es el efecto esperado de la ley L-6, pero se nota en `nuevo_s007_360_P1.png` y `nuevo_s001_360_P1.png`. Con MathJax local en Moodle la tipografía puede diferir, y esto no se midió.
- **BAJO (frente a la referencia):** la altura sube de media unos 26 px a 360 px, por el texto nuevo.
- **Anomalías (markup, math sin renderizar, `##ANSWERi##`, tildes, partes faltantes):** ninguna.
  - No hay `##ANSWER` visibles, MathML nativo en las 400 páginas, tildes correctas y las 6 partes presentes.
  - El `<ol type="a">` vacío del enunciado es el comportamiento habitual de `exams2html` con CLOZE, no un defecto.

## Lectura a ojo

Leí 5 imágenes apiladas a resolución completa. Cada una reúne 6 recortes, así que son 30 recortes en total.

- **NUEVO:** semilla 7 a 360 y a 320 px, semilla 1 a 360 y a 320 px, y semilla 4 a 320 px. No leí a ojo las semillas 4 a 360 px.
- **REFERENCIA:** semilla 7 a 320 px.
- **Contenido de cada imagen:** enunciado y opciones de P1, «Respuestas · Parte 1», el procedimiento de P1, «Caso específico», el párrafo «si y solo si» de la Parte 6 y el feedback (a).
- Las imágenes de las semillas 1, 4 y 7 a 360 y 320 px se generaron como `recortes/nuevo_sNNN_*_P1.png` y `recortes/ref_s007_*_P1.png`.

## Límites

- Es un render de `exams2html` y no de Moodle. No hay controles de gap, MathJax local ni el ancho de caja del Moodle del colegio.
- Las capturas se hicieron con Chromium headless en Linux y su configuración de fuentes, no con un móvil real.
- La captura de página completa se corta a 9000 px de alto. A 320 px la altura media es de 9190 px (los máximos son aún mayores), así que en muchas semillas el final de la página, incluidas las últimas partes del feedback, queda fuera de la imagen. Las métricas de texto y de DOM no dependen de la captura y sí cubren todo el documento.
- El recorte de la región de feedback (a) se tomó de la primera `<li>` del último `<ol type="a">`. No se midieron las demás partes del feedback por separado, pero sí entran en las métricas globales de R.

## Rutas

- Directorio de la entrega (en el repo): `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/salida/ciclo_20261102/6b_c1/`. Contiene `informe_6b_c1_27f789dc.md`, `metricas.json`, `resumen_texto.txt`, `alturas.txt`, `cap4.py`, `cap5.py`, `cap6.py` y `recortes/`.
- Material fuera del repo: los HTML, las capturas y los perfiles de Chromium están en `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/42032564-7410-4fd7-a51e-6d62e3e6af85/scratchpad/6b_c1/`.

VEREDICTO_6B: APTO_CON_OBSERVACIONES
