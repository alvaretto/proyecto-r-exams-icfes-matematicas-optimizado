# 6b pareada, tanda C1c (NUEVO ef2c96dc frente a REF 6f6f1b13)
N=100 semillas (1-100) por versión, 360 y 320 px: 400 páginas por versión (800 en total), 0 errores, innerWidth 360 y 320 comprobado. md5 verificados (ef2c96dc…, 6f6f1b13…). Instrumento cap7_r5.py sin modificar, agg.py de la C1b. Cifras: metricas.json, contar_h2_c1c.txt.
## A. Párrafo «Parte 6.»
| Métrica | NUEVO 360 | REF 360 | NUEVO 320 | REF 320 |
|---|---|---|---|---|
| Renglones media (rango) | 14,62 (14-15) | 13,51 (13-14) | 16,62 (16-17) | 15,59 (15-16) |
| Último renglón con UNA palabra | 0/100 | 0/100 | 0/100 | 0/100 |
| Último renglón con una sola unidad | 19/100 | 35/100 | 24/100 | 54/100 |
| Hist. palabras último renglón | 2:19, 4:11, 5:9, 6:61 | 2:35, 4:26, 5:5, 6:33, 7:1 | 2:24, 3:17, 4:6, 5:53 | 2:54, 3:5, 4:9, 5:32 |
| Cortes cifra/«kilos» | 0/400 | 0/400 | 0/400 | 0/400 |
| Desborde horizontal | 0 | 0 | 0 | 0 |
| Palabras partidas por dentro | 0 | 0 | 0 | 0 |
## B. L-6 (Parte 1) y residuo
Idénticas en NUEVO y REF, 0 cortes: fórmula+unidad dentro 0/1158; signo tras otro elemento 0/100; espacio inicial tras fórmula 0/1005; cifra+kilos/cursos 0/2003 (tabla 0/400); El Bosque/La Pradera 0/475 (tabla 0/24), a 360 y 320.
Residuo Partes 2-6 (fórmula+unidad partidas): 263/1249 (360) y 209/1249 (320), igual en las dos versiones. Cifra+unidad Partes 2-6: 0/1455.
## C. Documento completo
Desbordes de página 0; spills 0; tablas fuera de caja 0; tablas con scroll 0; fórmulas partidas 0. Fugas (##ANSWER, :::, pandocbounded, $$, \frac, **, NA/NaN/NULL, ~kilos): 0/100 HTML en ambas (`\text` aparece solo dentro de <annotation> de MathML, 0 fuera).
Altura media: 360: NUEVO 8164,8 / REF 8144,8 (+20,0 px, +0,25 %); 320: 9272,8 / 9254,2 (+18,5 px, +0,20 %).
## D. Frecuencias de H2 (contar_h2.py, por conteo)
| Versión/ancho | habría⏎ganado. | habría⏎seguido ganando. | A o B (págs.) | no le⏎habría alcanzado | habría⏎sumado |
|---|---|---|---|---|---|
| NUEVO 360 | 0/57 | 19/51 | 19/100 | 0/43 | 100/100 |
| REF 360 | 0/57 | 29/51 | 29/100 | 0/43 | 41/100 |
| NUEVO 320 | 0/57 | 19/51 | 19/100 | 0/43 | 0/100 |
| REF 320 | 57/57 | 43/51 | 84/100 | 43/43 | 100/100 |
## E. Control positivo
REF reproduce EXACTAMENTE las cifras de la r5 (320: 57/57, 43/51, 84/100, 43/43, 100/100; 360: 0/57, 29/51, 29/100, 0/43, 41/100) y las métricas del párrafo de la C1b (0/100 una palabra; 13,51 y 15,59 renglones). Ninguna frecuencia NO VERIFICABLE.
## Lectura visual: 10 imágenes (recortes/)
NUEVO: s001, s005, s010, s011 a 360 y a 320 (8; conclusiones: habría ganado/perdido, ganado/seguido ganando, no le habría alcanzado/perdido, no alcanzado/seguido ganando); REF: s001 a 360 y 320. La frase «si el promedio es mayor,» queda entera en un renglón en las 8 (a 360 en «entre los totales: si el promedio es mayor,»; a 320 en «totales: si el promedio es mayor, el»). Se confirma a ojo «habría / sumado» a 360 (s001, s005) y «habría / seguido ganando.» (s005 a 360 y 320).
## Hallazgos
1. [BAJO] «habría ⏎ seguido ganando.» sigue en 19/51 versiones (19/100 páginas) en los dos anchos (REF 29/51 y 43/51); s005_360 y s005_320.
2. [BAJO] «habría ⏎ sumado» pasa a 100/100 a 360 px (REF 41/100) en todos los casos (s001, s005, s010, s011 a 360); a 320 baja de 100/100 a 0/100. Es desplazamiento del corte, no defecto nuevo de otra clase.
3. [BAJO] El párrafo gana 1,11 renglones (360) y 1,03 (320); página +0,25 % / +0,20 %.
4. Mejoras: «habría⏎ganado.» y «no le⏎habría alcanzado» pasan de 57/57 y 43/43 a 0 a 320 px; el último renglón con una sola unidad baja de 35 a 19 y de 54 a 24.
5. Residuo heredado de Partes 2-6 (263/1249 y 209/1249), idéntico a REF, BAJO.
Sin CRÍTICO, ALTO ni MEDIO. Ninguna cifra de B, C ni mala partición de «si el promedio es mayor».
VEREDICTO_6B: APTO_CON_OBSERVACIONES
