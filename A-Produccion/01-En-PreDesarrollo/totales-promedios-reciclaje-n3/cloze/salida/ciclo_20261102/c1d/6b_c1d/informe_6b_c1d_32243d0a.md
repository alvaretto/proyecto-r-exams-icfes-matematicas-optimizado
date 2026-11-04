# 6b pareada C1d — NUEVO 32243d0a vs REF ef2c96dc (N = 100, 360 y 320 px)

Render: 100 + 100 HTML, 0 ERR. Captura CDP: 400 mediciones (2 versiones x 100 semillas x 2 anchos; el "800" del encargo era un recuento erróneo), errores 0, iw [320, 360].

## E. Control positivo (REF r): TODAS las cifras reproducen la tanda anterior
360: H2 0/57, 19/51, 19/100, 0/43, 100/100; renglones 14,62 (14-15); una palabra 0; una unidad 19; hist {2:19,4:11,5:9,6:61}; H 816480. OK.
320: H2 0/57, 19/51, 19/100, 0/43, 0/100; renglones 16,62 (16-17); una palabra 0; una unidad 24; hist {2:24,3:17,4:6,5:53}; H 927276. OK.
L-6: 0/1158, 0/100, 0/1005, 0/2003 (tabla 0/400), 0/475 (tabla 0/24). Residuo P2-6: 263/1249 (360), 209/1249 (320); cifra-unidad P2-6 0/1455. OK.
El instrumento y el entorno no cambiaron: las cifras del NUEVO son VERIFICABLES.

## A. Párrafo «Parte 6.»
| | 360 NUEVO | 360 REF | 320 NUEVO | 320 REF |
|---|---|---|---|---|
| renglones media (rango) | 15,62 (15-16) | 14,62 (14-15) | 18,59 (18-19) | 16,62 (16-17) |
| último renglón 1 palabra | 0/100 | 0/100 | 0/100 | 0/100 |
| último renglón 1 unidad | 19/100 | 19/100 | 54/100 | 24/100 |
| hist. palabras último | {2:19,4:11,5:9,6:32,7:29} | {2:19,4:11,5:9,6:61} | {2:54,3:5,4:9,5:32} | {2:24,3:17,4:6,5:53} |
| corte cifra/«kilos» | 0/400 | 0/400 | 0/400 | 0/400 |
| desborde horizontal | 0 | 0 | 0 | 0 |
| palabras partidas por dentro | 0 | 0 | 0 | 0 |
Frases nuevas: «si el promedio es mayor,» queda partida en TODAS las versiones (360: «si el promedio / es mayor,»; 320: «si / el promedio es mayor,»). «el grado que perdió» entero a 360 (100/100), partido a 320 («el grado que / perdió», 100/100). La frase es constante, no depende de la semilla.

## B. L-6 Parte 1 (idéntica en las dos versiones, 0 cortes)
0/1158, 0/100, 0/1005, 0/2003 (tabla 0/400), 0/475 (tabla 0/24), en 360 y 320. Residuo P2-6 formula+unidad: 263/1249 (360), 209/1249 (320) en ambas; cifra-unidad P2-6 0/1455. Sin diferencias.

## C. Documento completo
Desbordes 0, spills 0, tablas fuera 0, fórmulas partidas 0 en ambas. Altura suma H: 360 818280 vs 816480 (+1800 px, +0,22 %); 320 930822 vs 927276 (+3546 px, +0,38 %). Fugas (##ANSWER, :::, pandocbounded, $$, \frac, **, ~kilos, NA/NaN/NULL) sobre 100 HTML por versión: 0 en todas.

## D. H2 (contar_h2.py)
| versión ancho | habría/ganado | habría/seguido ganando | A o B | no le/habría alcanzado | habría/sumado |
|---|---|---|---|---|---|
| n 360 | 0/57 | 19/51 | 19/100 | 0/43 | 0/100 |
| n 320 | 57/57 | 43/51 | 84/100 | 43/43 | 100/100 |
| r 360 | 0/57 | 19/51 | 19/100 | 0/43 | 100/100 |
| r 320 | 0/57 | 19/51 | 19/100 | 0/43 | 0/100 |

## F. Lectura visual (10 recortes a resolución completa en recortes/)
NUEVO: n_s001, s005, s010, s011 a 360 y 320; REF: r_s001 a 360 y 320. Cubren ganado/perdido, ganado/seguido ganando, no le alcanzado/perdido, no le alcanzado/seguido ganando. La frase nueva se lee entera, sin palabras partidas ni desborde. A 320 se ven cortes «habría / ganado.» (s001, s005), «no le / habría alcanzado» (s010, s011) y «habría / sumado» (s001, s005, s010, s011). A 360, «habría / sumado» desaparece frente a la REF (mejora).

## Hallazgos
- MEDIO: a 320 px, regresión de H2 por la frase nueva (más larga): «habría ⏎ ganado.» 0/57 -> 57/57, «habría ⏎ seguido ganando.» 19/51 -> 43/51, «no le ⏎ habría alcanzado» 0/43 -> 43/43; páginas con A o B 19/100 -> 84/100. En la REF eran 0/57 y 0/43.
- BAJO: a 320, «habría ⏎ sumado» 100/100 (REF 0/100). A 360, mejora: 100/100 -> 0/100 (el corte pasa a otro sitio, los H2 de 360 iguales a REF).
- BAJO: a 320, último renglón con una sola unidad (habría perdido.) 54/100 vs 24/100.
- BAJO: «si el promedio es mayor,» partida en 100/100 a ambos anchos; a 320 «el grado que / perdió» partido. Cortes normales sin ley violada.
- BAJO: +1 renglón (360) y +2 (320) en el párrafo; documento +0,22 % / +0,38 % de altura.
- Ninguno CRÍTICO ni ALTO. L-6, desbordes, fugas, palabras partidas: 0.
Nota: NO se verificó el criterio de aceptación de H2 (ley del ciclo) porque no se me dio; el MEDIO se reporta como diferencia pareada frente a REF.

VEREDICTO_6B: APTO_CON_OBSERVACIONES
