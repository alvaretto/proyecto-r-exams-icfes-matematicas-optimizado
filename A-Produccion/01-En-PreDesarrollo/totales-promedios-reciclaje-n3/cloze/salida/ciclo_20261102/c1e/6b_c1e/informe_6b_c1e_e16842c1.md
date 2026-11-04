# Auditoría 6b pareada C1e — NUEVO e16842c1 vs REF 32243d0a (2026-11-03)
Render 100/100 semillas por versión; captura CDP 400 páginas, 400 errores: 0, iw [320, 360]. md5 verificados.

## A. Párrafo «Parte 6.» (N=100)
| métrica | n 360 | r 360 | n 320 | r 320 |
|---|---|---|---|---|
| renglones media (rango) | 16,40 (16-17) | 15,62 (15-16) | 19,28 (19-20) | 18,59 (18-19) |
| último renglón 1 palabra | 0/100 | 0/100 | 0/100 | 0/100 |
| último renglón 1 unidad | 40/100 | 19/100 | 79/100 | 54/100 |
| hist. palabras último | {2:40,4:38,5:3,6:16,7:3} | {2:19,4:11,5:9,6:32,7:29} | {2:79,3:6,4:3,5:12} | {2:54,3:5,4:9,5:32} |
| cortes cifra/«kilos» | 0/400 | 0/400 | 0/400 | 0/400 |
| desborde horizontal / palabras partidas | 0 / 0 | 0 / 0 | 0 / 0 | 0 / 0 |
El «último renglón con una unidad» (p. ej. «habría perdido.», «37 kilos: habría perdido.») sube +21 pp (360) y +25 pp (320); es un renglón final corto de 2 palabras, no huérfano de una palabra. Los renglones suben +0,78 (360) y +0,69 (320).

## B. L-6 Parte 1 y residuo (idénticos en las dos versiones, ambos anchos)
fórmula+unidad dentro 0/1158; signo tras otro 0/100; espacio inicial 0/1005; cifra-unidad 0/2003 (tabla 0/400); El/La Bosque 0/475 (tabla 0/24). Residuo Partes 2-6 fórmula+unidad: 263/1249 (360) y 209/1249 (320), igual en n y r; cifra-unidad Partes 2-6 0/1455.

## C. Documento completo
spills 0, tablas fuera 0, tablas con scroll 0, mathSplit 0, desborde de página 0 (ambas versiones y anchos). Altura (suma H): n360 819684 vs r360 818280 = +1404 px (+0,17 %; +14,0 px/página); n320 932064 vs r320 930822 = +1242 px (+0,13 %; +12,4 px/página). Fugas en 100 HTML por versión (##ANSWER, :::, pandocbounded, $$, \frac, **, NA, NaN, NULL, ~kilos): 0 en n y 0 en r.

## D. H2 (residuo DECLARADO por el profesor, 2026-11-03; BAJO declarado)
| versión ancho | habría/ganado. | habría/seguido ganando | páginas con A o B | no le/habría alcanzado | habría/sumado |
|---|---|---|---|---|---|
| n 360 | 0/57 | 32/51 | 32/100 | 0/43 | 0/100 |
| n 320 | 0/57 | 33/51 | 33/100 | 0/43 | 0/100 |
| r 360 | 0/57 | 19/51 | 19/100 | 0/43 | 0/100 |
| r 320 | 57/57 | 43/51 | 84/100 | 43/43 | 100/100 |
Frase nueva (n): literal presente 100/100 (360) y 100/100 (320); en r 0 y 0. «habría/ganado con»: 100/100 a 360, 0 a 320. «habría/perdido con»: 0 y 0. Otros cortes (con un/curso, de cada/uno, mayor,/sí;, menor,/no.): 0 en todos.
Nota: la frase «habría ⏎ ganado con» ocurre en el 100 % a 360 px (decisión declarada). H2 en n baja a 320 (84→33 páginas) y sube a 360 (19→32 páginas «habría seguido ganando»).

## E. Control positivo REF r: coincide EXACTAMENTE
360: H2 0/57, 19/51, 19/100, 0/43, 0/100; renglones 15,62 (15-16); una palabra 0/100; una unidad 19/100; hist {2:19,4:11,5:9,6:32,7:29}; H 818280. 320: H2 57/57, 43/51, 84/100, 43/43, 100/100; 18,59 (18-19); 0/100; 54/100; hist {2:54,3:5,4:9,5:32}; H 930822. L-6 0/1158, 0/100, 0/1005, 0/2003 (tabla 0/400), 0/475 (tabla 0/24); residuo 263/1249 y 209/1249; cifra-unidad P2-6 0/1455; cifra-«kilos» párrafo 0/400. Todo coincide; el instrumento y el entorno no cambiaron, las cifras del NUEVO son verificables.

## F. Lectura visual (recortes en recortes/)
10 recortes leídos a resolución completa: n s001, s005, s010, s011 a 360 y 320 (conclusiones: ganado/perdido; ganado/seguido ganando; no le habría alcanzado/perdido; no le habría alcanzado/seguido ganando) y r s001 (360), r s005 (320). La frase nueva se lee entera, sin palabras partidas ni desborde. «si el promedio es mayor, sí;» queda entero en un renglón en 360 (s001) y 320 (s001, s005); en 320 s010/s011 corta «si el promedio ⏎ es mayor, sí; si es ⏎ menor, no.» sin romper «mayor,/sí;». «el grado que perdió» queda entero en todos los recortes salvo corte natural entre renglones sin partir palabras. REF en los recortes: el párrafo lee con la frase de referencia y renglón final largo.

## Hallazgos
- BAJO (declarado): H2 «habría ⏎ ganado con» 100/100 a 360 px y «habría ⏎ seguido ganando» 32/51 (360) y 33/51 (320); «no le ⏎ habría alcanzado» 0/43 en n (mejora frente a REF 320, 43/43).
- BAJO: el último renglón con una sola unidad sube de 19 a 40/100 (360) y de 54 a 79/100 (320); +0,7 renglones de media y +0,13-0,17 % de altura. Sin huérfanos de una palabra.
- CRÍTICO / ALTO / MEDIO: ninguno. Fugas, desbordes, spills, tablas, fórmulas partidas, L-6: 0.
