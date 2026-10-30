## Auditoría Visual HTML — totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1 (ronda 4)

**Veredicto: APTO_CON_OBSERVACIONES**

**Versiones renderizadas:** N = 100 (cifra completa, sin reducir), 100 OK y 0 fallidas, con `exams2html()` en semillas consecutivas 1..100.
**Viewports:** 360 px, 1024 px y 320 px. Las capturas de pantalla son completas y no hubo truncamiento.
**Canónicas:** 9 versiones con 5 cursos y 40 kilos frente a 4 cursos y 45 kilos (Primaria y Secundaria). Son v07, v27, v42 y v47, más 5 más, y hay 22 versiones con ERR-TOTAL. Sus semillas son 7, 27, 42 y 47; las otras cinco no las listé, las identifica el recuento por datos de tabla.
**Capturas revisadas con la vista:** v07_360 (canónica) y v03_360 (ERR-TOTAL más scroll), más los recortes c03 y c06.
**Medición por DOM:** un script de Chromium recorrió las 100 versiones a 360, 320 y 1024 px. Mide posiciones de cada fragmento de texto y de cada «(». Las versiones 1..100 se enlazan en los nombres como v011.html ... v1001.html.

### Los tres puntos que pediste

**B1 (cabeceras pegadas): corregido.**
- Separación horizontal entre el texto de dos cabeceras: 23,2 a 47,2 px en 100 de 100 versiones, a 360, 320 y 1024 px.
- Separación entre cajas de `<span>` (padding incluido): 4 a 28 px. El mínimo de 4 px es el `border-spacing` de 2 px más el padding de 1+1 px de `th`. Es la cifra que la ronda anterior llamaba «4 px», pero no es separación entre textos.
- Separación entre textos de celdas del cuerpo: 27,7 a 53,4 px a 360 px, 27,7 a 45,2 px a 320 px y 67,7 a 197,7 px a 1024 px.
- Versiones con separación entre cabeceras < 10 px: 0 de 100. Con < 15 px: 0 de 100.
- A 1024 px las cabeceras caben en una línea. A 360 y 320 px se parten en 2 a 4 renglones en 100 de 100 versiones. Es esperable y se lee bien en c03.png.

**B2 («El Bosque» y «La Pradera» partidos): corregido.**
- Hay 12 versiones con esas sedes. Se parten en 0 de 12 a 360, 320 y 1024 px (usan espacio no separable, `\xa0`).
- Ninguna otra etiqueta de la primera columna se parte en las 100 versiones, a ningún ancho.
- A 360 px: la tabla mide entre 289 y 317,6 px dentro de una caja de 289 px. Sobresale en 49 de 100 versiones, hasta 28,6 px. El contenedor tiene `overflow-x:auto`, así que no desborda la página: `scrollWidth` del documento nunca supera el ancho de ventana.
- A 320 px: la caja mide 249 px y la tabla entre 256,6 y 317,6 px. Sobresale en 100 de 100 versiones, entre 7,6 y 68,6 px. La tabla desborda su caja y queda dentro de un scroll horizontal. No desborda la página. Ver hallazgo 1.

**B3 («(» huérfano en Solution de ERR-TOTAL): corregido.**
- Revisé las 22 versiones con ERR-TOTAL a 360, 320 y 1024 px. Inspeccioné todos los «(» del documento. Ninguno queda solo al final de renglón: 0 casos, tampoco con espacio detrás.

### Rastros de markup
- En el HTML crudo: 0 apariciones de `&lt;span`, `{=latex}`, `nobreak`, `afterheading`, `makeatletter`, `:::`, `$$` o `##ANSWER`.
- En el texto visible (`innerText`): 0 coincidencias de `<span`, `{=latex}`, `nobreak`, `afterheading`, `makeatletter`, `:::`, `$$`, `##ANSWER`, `NA`, `NaN`, `Inf`, `NULL`, `**`, `Ã` o `Â`.
- Los `<span style="display:inline-block; padding...">` del HTML son marcado legítimo de celdas. No se ven como texto.
- Todas las versiones tienen 8 elementos de lista tipo `a` (4 opciones y 4 de estrategia). No hay opciones duplicadas, ni Solution vacía o truncada, ni mojibake, ni tildes faltantes en las dos versiones que leí completas (v07 y v03).

### Hallazgos por severidad

**Crítico:** Ninguno.
**Alto:** Ninguno.

**Medio**
1. A 320 px la tabla no cabe en su caja en 100 de 100 versiones, y a 360 px en 49 de 100 (por ejemplo v03, v01, v05, v06, v07 y v11).
   - En c06.png (v06 a 320 px) la tercera columna, «Promedio de cartón recogido por curso», queda cortada por la caja: se ve «Promed… 40 kilo…».
   - El estudiante tiene que desplazar la tabla en horizontal para ver el promedio, que es el dato clave del ejercicio. No hay indicador visible de que hay scroll.
   - A 360 px el recorte es de hasta 28,6 px y puede cortar la última cifra.
   - Es un scroll intencional dentro de una caja, no un desborde de página, por eso va como medio y no como crítico. El requisito de que la tabla sea legible sin desplazarse no se cumple a 320 px.

**Bajo**
2. A 360 y 320 px las cabeceras ocupan 2 a 4 renglones en todas las versiones («Promedio de plástico recogido por curso» llega a 4). La tabla queda alta y estrecha. Es cosmético.
3. Mi medición de «separación entre cabeceras» depende de la definición: entre cajas es 4 px, entre textos es ≥ 23 px. Si el criterio del revisor anterior era la caja, B1 sigue en 4 px por el `border-spacing`, aunque visualmente ya no se ven pegadas.

### Cobertura
| Categoría | Resultado |
|---|---|
| Markup/render roto | Ninguno |
| Math sin renderizar | Ninguno |
| Coherencia tabla-texto | Ninguno (v07 y v03 coinciden con tabla, opciones y Solution; 100 versiones sin opciones duplicadas) |
| Estructura/partes | Ninguno |
| Layout/responsividad | 1 medio (scroll de tabla a 320 y 360 px), 1 bajo |
| Tipografía/codificación | Ninguno |
| Anomalías entre versiones | Ninguno (22 ERR-TOTAL estructuralmente iguales al resto; canónicas sin diferencias) |

### Rutas
- Capturas y HTMLs: `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/35e93ef1-04b2-4e1e-8b49-a10ac14fa449/scratchpad/shots/` (`vNN_360.png`, `vNN_320.png`, `vNN_1024.png`, `vNNN1.html`, `contact_*.png`, `manifest.txt`). El contact sheet de 100 versiones no sirve para triaje, así que no lo usé.
- Recortes: `.../shots/c03.png` y `.../shots/c06.png`.
- Mediciones JSON: `.../scratchpad/m/res_360.json`, `res_320.json` y `res_1024.json`, con el script `.../scratchpad/m/run.py`.
- No se modificó ningún archivo del repo.

### Fix sugerido (no aplicado)
Reducir el `padding` de los `<span>` de celda (0,6 em a 0,3 em) o fijar `font-size` menor de la tabla en pantallas estrechas, para que 289 px y 249 px bastaran. Con eso la tabla (256 a 318 px) quedaría dentro de la caja sin scroll.
