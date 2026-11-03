# Auditoría visual 6b pareada, tanda C1b (NUEVO 6f6f1b13 frente a REFERENCIA 27f789dc)

N = 100 semillas (1 a 100) por versión, anchos 360 y 320 px: 400 páginas (200 por versión). 0 errores de captura; innerWidth comprobado = 360 y 320.
Render: exams2html(n=1, mathjax=TRUE) con set.seed(s); Chromium headless con red bloqueada y CDP setDeviceMetricsOverride. Instrumento: cap7.py (cap4.py de la C1 + sonda del párrafo «Parte 6.»), agregador agg.py, render.R. Cifras en metricas.json.

Sonda del párrafo: por cada palabra (separada solo por espacio normal; «27 kilos» con U+00A0 es una unidad) se toma Range.getClientRects(); las fórmulas se miden por su <math>; los renglones se agrupan por centro vertical (tolerancia 8 px). Se obtienen renglones, unidades e palabras visibles del último renglón, cortes cifra/«kilos» (rango del dígito frente al de la «k»), desborde (scrollWidth y cajas frente al párrafo).
Control positivo: la REFERENCIA a 360 px da «diferencia.» sola en 99/100 (la C1 daba 99/100) y 0/100 a 320 px (igual que la C1). La sonda dispara donde debe.
Nota de comparabilidad: los HTML de la C1 no se conservaron; mis cifras absolutas de Partes 2-6 (263/1249 a 360, 209/1249 a 320) difieren levemente de las de la C1 (259 y 206), y P1 espacio-inicial n = 1005 frente a 1002. Es igual en las dos versiones de esta tanda, así que la comparación pareada es válida.

## Párrafo «Parte 6. Cada hipótesis»
| Métrica | NUEVO 360 | REF 360 | NUEVO 320 | REF 320 |
|---|---|---|---|---|
| Renglones (media; rango) | 13,51 (13-14) | 10,99 (10-11) | 15,59 (15-16) | 12,00 (12) |
| Último renglón con UNA palabra visible | 0/100 | 99/100 | 0/100 | 0/100 |
| Último renglón con una sola unidad (con U+00A0, 2 palabras) | 35/100 | 99/100 | 54/100 | 0/100 |
| Palabras del último renglón (hist.) | 2:35, 4:26, 5:5, 6:33, 7:1 | 1:99, 7:1 | 2:54, 3:5, 4:9, 5:32 | 3:100 |
| Cortes cifra / «kilos» (pares) | 0/400 | 0/300 | 0/400 | 0/300 |
| Desborde horizontal del párrafo | 0 | 0 | 0 | 0 |
| Unidades partidas por dentro | 0 | 0 | 0 | 0 |

## L-6, Parte 1 (idénticas en las dos versiones, 0 cortes)
fórmula+unidad dentro 0/1158; signo tras otro elemento 0/100; renglón con espacio inicial tras fórmula 0/1005; cifra+kilos/cursos 0/2003 (tabla 0/400); «El Bosque»/«La Pradera» 0/475 (tabla 0/24). Igual en NUEVO y REF, a 360 y 320 px.
Partes 2-6 (residuo conocido, BAJA 1): fórmula+unidad partidas y espacio inicial: 263/1249 (360) y 209/1249 (320) en AMBAS versiones. Cifra+kilos/cursos en Partes 2-6: 0/1455 NUEVO (0/1355 REF; +100 pares nuevos de la Parte 6, ninguno cortado).

## Documento completo
Desbordes de página 0; spills 0; tablas fuera de su caja 0; tablas con scroll 0; fórmulas partidas por dentro 0; `##ANSWER`, `:::`, pandocbounded, `$$`, `\frac`, `\text`, `**`, NA/NaN/NULL, «~kilos» (tilde de espacio duro sin resolver): 0 archivos de 100 en ambas versiones.
Altura media de página: 360 px: NUEVO 8144,8 frente a REF 8099,5 (+45,4 px, +0,56 %); 320 px: 9254,2 frente a 9189,6 (+64,6 px, +0,70 %).
Conclusiones del párrafo nuevo en las 100 semillas (render): ganado/perdido 38, ganado/seguido ganando 19, no le habría alcanzado/perdido 11, no le habría alcanzado/seguido ganando 32. Las cuatro aparecen, y las dos frases se resuelven en 100/100.

## Imágenes leídas (8, a resolución completa, en recortes/)
NUEVO 360: s001 (ganado/perdido), s010 (no alcanzado/perdido), s005 (ganado/seguido ganando). NUEVO 320: s011 (no alcanzado/seguido ganando), s002 (ganado/perdido), s016 (no alcanzado/seguido ganando). REF: s001 a 360 y a 320.

## Hallazgos
1. [BAJO] A 320 px (visto en s002, s011, s016; en 3 de 6 imágenes NUEVO) el primer renglón separa la conclusión de la primera frase o de la segunda: «habría | ganado.», «habría | seguido ganando.». El espacio duro solo cubre las dos últimas palabras del párrafo; no hay renglón huérfano, solo corte cosmético entre «habría» y su participio. No se cuantificó la frecuencia sobre las 100 semillas.
2. [BAJO] El párrafo crece 2,5 renglones a 360 px y 3,6 a 320 px (+0,56 %/+0,70 % de altura de página). Es el contenido añadido, no un defecto.
3. Resuelto frente a la C1: «diferencia.» sola en el último renglón a 360 px (99/100 en la REF) pasa a 0/100 en el NUEVO; el último renglón nunca tiene una sola palabra visible, y los 400 pares cifra/«kilos» quedan sin cortar.
No hay hallazgos CRÍTICO, ALTO ni MEDIO. Residuo heredado de Partes 2-6 (fórmula+unidad partidas 263/1249 y 209/1249) idéntico entre versiones.

VEREDICTO_6B: APTO_CON_OBSERVACIONES
