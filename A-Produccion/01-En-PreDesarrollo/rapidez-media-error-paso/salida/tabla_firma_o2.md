# Tabla de firma O-2 — rapidez-media-error-paso

Para el paso 11. **Ninguna de estas cifras está firmada:** `FIRMADA <- FALSE` en `verificar_espacio.R`, y el verificador sale con exit 3.

- `.Rmd` md5 `904af1ca1271d97c54123218c9db03c0`. Las líneas son de `verificar_espacio.R` md5 `3a84582d846d7766a7aa7d9a41a839ae`.
- Enumeración exacta (`salida/verificar_r12.txt`): 9 564 estados, 131 586 conjuntos, 0 abortos, 0 errores de regla. Coincide con `opcion_c/c2.json` y con la tabla H3 de la FASE 2C r4.
- Exceso en pp sobre el techo de 28,8 %. Contiene toda combinación regla-ámbito de exceso >= +8 **entre las reglas medidas** (`MEDIBLES` del verificador): 64 filas, 31 en no canónicas y 33 en mezcla. 1 es de la batería congelada; las demás, vigiladas.
- «Impreso» es la tasa en la instancia canónica. «Exige calcular» dice si hay que hacer una operación con los datos; comparar dos números o ver si uno está en el enunciado no cuenta.
- Cada glosa se contrastó con la línea citada antes de escribirla. La FASE 2C r4 contrastó las 18 primeras reglas: `NM` estaba mal y aquí ya va corregida.
- En `EST` sin rótulos, «no aparece en el enunciado ni en los pasos» excluye los rótulos «Paso n». Está por debajo de +8 (+4,53) y no tiene fila, pero entra en `EST-1`, `EST+-1` y `EST+-1-par`.

| Regla | Glosa | Línea | Exacta | Exceso | Ámbito | Exige calcular | Impreso |
|---|---|---|---|---|---|---|---|
| `RE` | Elige las opciones de los pasos 1 y 2 cuyo paso muestra como resultado un número que aparece en el enunciado, horas incluidas. Es decir, las que rechazan un dato. En la práctica señala el paso con `− 0`. | 581; lo que muestra el paso, 557 | 50,00 % | +21,20 | no canónicas | no | 25 % |
| `RE` | Elige las opciones de los pasos 1 y 2 cuyo paso muestra como resultado un número que aparece en el enunciado, horas incluidas. Es decir, las que rechazan un dato. En la práctica señala el paso con `− 0`. | 581; lo que muestra el paso, 557 | 48,75 % | +19,95 | mezcla, `P_CANON` = 0,05 | no | 25 % |
| `RE-con-fraccion` | `RE`, y además las opciones del paso 3 cuando la fracción que muestra ese paso empieza por un número del enunciado. | 582; lo que muestra el paso, 557 | 43,75 % | +14,95 | no canónicas | no | 25 % |
| `RE-con-fraccion` | `RE`, y además las opciones del paso 3 cuando la fracción que muestra ese paso empieza por un número del enunciado. | 582; lo que muestra el paso, 557 | 42,81 % | +14,01 | mezcla, `P_CANON` = 0,05 | no | 25 % |
| `U` | Elige la opción del paso 3 que escribe las dos cifras que muestra el paso 3, en el mismo orden: solo cambia las unidades. | 583 | 42,52 % | +13,72 | no canónicas | no | 100 % |
| `U` | Elige la opción del paso 3 que escribe las dos cifras que muestra el paso 3, en el mismo orden: solo cambia las unidades. | 583 | 45,39 % | +16,59 | mezcla, `P_CANON` = 0,05 | no | 100 % |
| `NM` | Descarta las opciones cuya cifra afirmada supera lo que rechazan: el resultado del paso, en los pasos 1 y 2, y el numerador de la fracción que muestra el paso, en el paso 3. Empatar no descarta. | 584; lo que muestra el paso, 557 | 39,42 % | +10,62 | no canónicas | no | 33,33 % |
| `NM` | Descarta las opciones cuya cifra afirmada supera lo que rechazan: el resultado del paso, en los pasos 1 y 2, y el numerador de la fracción que muestra el paso, en el paso 3. Empatar no descarta. | 584; lo que muestra el paso, 557 | 39,11 % | +10,31 | mezcla, `P_CANON` = 0,05 | no | 33,33 % |
| `NM+frac` | `NM`, y descarta las opciones del paso 3 cuya fracción afirmada es menor que 1. | 602; 594, 596 | 56,08 % | +27,28 | no canónicas | no | 33,33 % |
| `NM+frac` | `NM`, y descarta las opciones del paso 3 cuya fracción afirmada es menor que 1. | 602; 594, 596 | 54,94 % | +26,14 | mezcla, `P_CANON` = 0,05 | no | 33,33 % |
| `NM+pasos` | `NM`, y descarta las cifras afirmadas que aparecen en los pasos. | 602; 594 | 54,70 % | +25,90 | no canónicas | no | 25 % |
| `NM+pasos` | `NM`, y descarta las cifras afirmadas que aparecen en los pasos. | 602; 594 | 53,22 % | +24,42 | mezcla, `P_CANON` = 0,05 | no | 25 % |
| `NM+kmh` | `NM`, y descarta las fracciones en h/km. | 603; 594, 595 | 53,88 % | +25,08 | no canónicas | no | 33,33 % |
| `NM+kmh` | `NM`, y descarta las fracciones en h/km. | 603; 594, 595 | 52,86 % | +24,06 | mezcla, `P_CANON` = 0,05 | no | 33,33 % |
| `NM+X0` | `NM`, y descarta las cifras afirmadas que repiten un número del enunciado. | 603; 594 | 50,30 % | +21,50 | no canónicas | no | 50 % |
| `NM+X0` | `NM`, y descarta las cifras afirmadas que repiten un número del enunciado. | 603; 594 | 50,28 % | +21,48 | mezcla, `P_CANON` = 0,05 | no | 50 % |
| `NM+X0+frac` | `NM+X0`, y descarta las fracciones menores que 1. | 604; 594, 596 | 64,45 % | +35,65 | no canónicas | no | 50 % |
| `NM+X0+frac` | `NM+X0`, y descarta las fracciones menores que 1. | 604; 594, 596 | 63,73 % | +34,93 | mezcla, `P_CANON` = 0,05 | no | 50 % |
| `NM+kmh+frac` | `NM+kmh`, y descarta las fracciones menores que 1. | 604; 594, 595, 596 | 64,26 % | +35,46 | no canónicas | no | 33,33 % |
| `NM+kmh+frac` | `NM+kmh`, y descarta las fracciones menores que 1. | 604; 594, 595, 596 | 62,71 % | +33,91 | mezcla, `P_CANON` = 0,05 | no | 33,33 % |
| `NM+pos+frac` | `NM+frac`, y descarta las negativas. | 605; 594, 596 | 63,17 % | +34,37 | no canónicas | no | 100 % |
| `NM+pos+frac` | `NM+frac`, y descarta las negativas. | 605; 594, 596 | 65,01 % | +36,21 | mezcla, `P_CANON` = 0,05 | no | 100 % |
| `X0+` | Descarta las opciones cuya cifra afirmada repite, en valor absoluto, un número del enunciado, horas incluidas, y las negativas. En una fracción mira el numerador. | 567 (`x0`), 585 | 37,07 % | +8,27 | no canónicas | no | 100 % |
| `X0+` | Descarta las opciones cuya cifra afirmada repite, en valor absoluto, un número del enunciado, horas incluidas, y las negativas. En una fracción mira el numerador. | 567 (`x0`), 585 | 40,21 % | +11,41 | mezcla, `P_CANON` = 0,05 | no | 100 % |
| `EST-1` | `EST` sin rótulos, y descarta las opciones de los pasos 1 y 2 de la forma «N + 1 y no N». | 606; 597 | 44,55 % | +15,75 | no canónicas | no | 25 % |
| `EST-1` | `EST` sin rótulos, y descarta las opciones de los pasos 1 y 2 de la forma «N + 1 y no N». | 606; 597 | 43,57 % | +14,77 | mezcla, `P_CANON` = 0,05 | no | 25 % |
| `EST+-1` | `EST-1`, y descarta las negativas. | 606; 597 | 46,26 % | +17,46 | no canónicas | no | 25 % |
| `EST+-1` | `EST-1`, y descarta las negativas. | 606; 597 | 45,20 % | +16,40 | mezcla, `P_CANON` = 0,05 | no | 25 % |
| `EST+-1-par` | `EST+-1`, y cuando dos opciones de un mismo paso difieren en 1, descarta la mayor. | 607; 597, 598 | 49,87 % | +21,07 | no canónicas | no | 25 % |
| `EST+-1-par` | `EST+-1`, y cuando dos opciones de un mismo paso difieren en 1, descarta la mayor. | 607; 597, 598 | 48,62 % | +19,82 | mezcla, `P_CANON` = 0,05 | no | 25 % |
| `E1` | Pasos 1 y 2: conserva las positivas que no repiten un número del enunciado y no superan la mayor distancia del enunciado, en el paso 1, o la mayor hora, en el paso 2. Las del paso 3 se conservan todas. | 586, 587 | 38,68 % | +9,88 | mezcla, `P_CANON` = 0,05 | no | 100 % |
| `E2` | `E1`, y en el paso 3 descarta las fracciones en h/km. | 587 (`e2`) | 46,31 % | +17,51 | no canónicas | no | 100 % |
| `E2` | `E1`, y en el paso 3 descarta las fracciones en h/km. | 587 (`e2`) | 49,00 % | +20,20 | mezcla, `P_CANON` = 0,05 | no | 100 % |
| `E3` | `E2`, y en el paso 3 descarta las fracciones menores que 1. | 587 (`e3`) | 53,29 % | +24,49 | no canónicas | no | 100 % |
| `E3` | `E2`, y en el paso 3 descarta las fracciones menores que 1. | 587 (`e3`) | 55,63 % | +26,83 | mezcla, `P_CANON` = 0,05 | no | 100 % |
| `E3-1` | `E3`, y descarta «N + 1 y no N». | 608; 597 | 73,73 % | +44,93 | no canónicas | no | 100 % |
| `E3-1` | `E3`, y descarta «N + 1 y no N». | 608; 597 | 75,05 % | +46,25 | mezcla, `P_CANON` = 0,05 | no | 100 % |
| `E3-1-par` | `E3-1`, y cuando dos opciones de un mismo paso difieren en 1, descarta la mayor. | 608; 597, 598 | 78,05 % | +49,25 | no canónicas | no | 100 % |
| `E3-1-par` | `E3-1`, y cuando dos opciones de un mismo paso difieren en 1, descarta la mayor. | 608; 597, 598 | 79,14 % | +50,34 | mezcla, `P_CANON` = 0,05 | no | 100 % |
| `M0` | Elige las opciones del paso con la marca: `+` o `− 0 km` en el paso 1, `− 0 h` en el paso 2, `h/km` en el paso 3. | 590, 591 | 75,00 % | +46,20 | no canónicas | no | 100 % |
| `M0` | Elige las opciones del paso con la marca: `+` o `− 0 km` en el paso 1, `− 0 h` en el paso 2, `h/km` en el paso 3. | 590, 591 | 76,25 % | +47,45 | mezcla, `P_CANON` = 0,05 | no | 100 % |
| `M2` | `M0`, y además positiva y sin repetir un número del enunciado. | 591 | 80,00 % | +51,20 | no canónicas | no | 100 % |
| `M2` | `M0`, y además positiva y sin repetir un número del enunciado. | 591 | 81,00 % | +52,20 | mezcla, `P_CANON` = 0,05 | no | 100 % |
| `M3` | `M0`, y además la condición de `E3`. | 591, 587 | 90,68 % | +61,88 | no canónicas | no | 100 % |
| `M3` | `M0`, y además la condición de `E3`. | 591, 587 | 91,15 % | +62,35 | mezcla, `P_CANON` = 0,05 | no | 100 % |
| `M3-par` | `M3`, y cuando dos opciones de un mismo paso difieren en 1, descarta la mayor. | 609; 598 | 95,44 % | +66,64 | no canónicas | no | 100 % |
| `M3-par` | `M3`, y cuando dos opciones de un mismo paso difieren en 1, descarta la mayor. | 609; 598 | 95,67 % | +66,87 | mezcla, `P_CANON` = 0,05 | no | 100 % |
| `MAG-segundo-afirmado` | Elige la opción con la segunda cifra afirmada más grande, con su signo. En una fracción cuenta el numerador. Empates al rango menor. | 419 | 37,16 % | +8,36 | mezcla, `P_CANON` = 0,05 | no | 100 % |
| `entre-media` | Conserva las cifras de los pasos 1 y 2 que caen entre las dos lecturas de su magnitud, y descarta la media de las dos distancias. | 610; 601, 600 | 47,97 % | +19,17 | no canónicas | sí, una media | 25 % |
| `entre-media` | Conserva las cifras de los pasos 1 y 2 que caen entre las dos lecturas de su magnitud, y descarta la media de las dos distancias. | 610; 601, 600 | 46,82 % | +18,02 | mezcla, `P_CANON` = 0,05 | sí, una media | 25 % |
| `E3-1-par-media` | `E3-1-par`, y descarta la media de las dos distancias. | 610; 600 | 98,47 % | +69,67 | no canónicas | sí, una media | 100 % |
| `E3-1-par-media` | `E3-1-par`, y descarta la media de las dos distancias. | 610; 600 | 98,54 % | +69,74 | mezcla, `P_CANON` = 0,05 | sí, una media | 100 % |
| `X1b` | Pone lo que afirma la opción en lugar del resultado de su paso, deja el otro paso como lo muestra el procedimiento, recalcula la rapidez y conserva las opciones en las que el número sale entero. En el paso 3 divide la fracción afirmada, sea km/h o h/km. | 567 (`xb`), 569; sustitución en 500 | 44,31 % | +15,51 | no canónicas | sí | 25 % |
| `X1b` | Pone lo que afirma la opción en lugar del resultado de su paso, deja el otro paso como lo muestra el procedimiento, recalcula la rapidez y conserva las opciones en las que el número sale entero. En el paso 3 divide la fracción afirmada, sea km/h o h/km. | 567 (`xb`), 569; sustitución en 500 | 43,35 % | +14,55 | mezcla, `P_CANON` = 0,05 | sí | 25 % |
| `X1` | Lo mismo, y conserva las que además dan positivo y en km/h. | 567 (`xa`, `xb`), 569 | 58,23 % | +29,43 | no canónicas | sí | 50 % |
| `X1` | Lo mismo, y conserva las que además dan positivo y en km/h. | 567 (`xa`, `xb`), 569 | 57,82 % | +29,02 | mezcla, `P_CANON` = 0,05 | sí | 50 % |
| `R10-150` | Sustituye igual y conserva las que dan de 10 a 150 km/h, extremos incluidos. | 568 (`r3`), 570 | 41,12 % | +12,32 | no canónicas | sí | 50 % |
| `R10-150` | Sustituye igual y conserva las que dan de 10 a 150 km/h, extremos incluidos. | 568 (`r3`), 570 | 41,57 % | +12,77 | mezcla, `P_CANON` = 0,05 | sí | 50 % |
| `R20-120` | Sustituye igual y conserva las que dan de 20 a 120 km/h, extremos incluidos. | 568 (`r2`), 570 | 48,38 % | +19,58 | no canónicas | sí | 50 % |
| `R20-120` | Sustituye igual y conserva las que dan de 20 a 120 km/h, extremos incluidos. | 568 (`r2`), 570 | 48,46 % | +19,66 | mezcla, `P_CANON` = 0,05 | sí | 50 % |
| `R20-120+ent` | `R20-120`, y además el número sale entero. | 568, 567, 571 | 65,47 % | +36,67 | no canónicas | sí | 50 % |
| `R20-120+ent` | `R20-120`, y además el número sale entero. | 568, 567, 571 | 64,69 % | +35,89 | mezcla, `P_CANON` = 0,05 | sí | 50 % |
| `R30-90` | Sustituye igual y conserva las que dan de 30 a 90 km/h, extremos incluidos: el rango del generador. | 568 (`r1`), 570 | 59,39 % | +30,59 | no canónicas | sí | 50 % |
| `R30-90` | Sustituye igual y conserva las que dan de 30 a 90 km/h, extremos incluidos: el rango del generador. | 568 (`r1`), 570 | 58,92 % | +30,12 | mezcla, `P_CANON` = 0,05 | sí | 50 % |

**Por debajo de +8, sin fila.** Batería: `DIV-mult10` +7,74 (mezcla +7,58) y `FMT-empieza-km` +5,17. Vigiladas, en no canónicas: `entre` +6,70, `E1` +6,65, `EST+` +5,36, `X0` +5,15, `X1a` +5,07 y `EST-sin-rotulos` +4,53. `Q` da 25,00 %: no aplica.

**Dos filas por regla.** `E1` y `MAG-segundo-afirmado` solo pasan de +8 en la mezcla. Las demás aparecen en los dos ámbitos, y se firman en los dos.

## Lo que esta tabla no agota

La FASE 2C r4 evaluó 96 568 conjunciones de 1 a 3 cláusulas, con 29 rasgos más 17 propios de las razones nuevas, y **6 623 llegan a +8 pp** (`salida/medicion_exacta/det4/busca.out`; con la clave permutada, el máximo es 25,47 %). No se pueden listar una por una, y §P7-E no admite un tope por familia. **El profesor debe decidir qué firma y qué declara como no agotado.** Las 15 reglas `NM+…`, `EST…-1…`, `E3-1…`, `M3-par` y `entre-media` de la tabla son las que la r4 propuso como representativas, no el conjunto.

Dos causas estructurales, de la r4:

1. En los pasos 1 y 2 la clave afirma menos de lo que rechaza en el 100 % de los casos: tras L-8, todo error cometido infla el número. `EV-T-CUENTA` sube en el 81,61 %. Por eso L-9 no compite con `NM`: `NM+frac` era +27,30 antes de O-2 y es +27,28 ahora.
2. `EV-T-CUENTA` sale en el 52,63 % de las versiones, y en el 46,85 % se lee como «N + 1 y no N»: se descarta a ojo. `EST` sin rótulos pasa de +22,15 a +4,53 con la obligación, pero descartando «N + 1 y no N» vuelve a +15,75. Contra quien no calcula, la defensa real de L-9 es `EV-D-PROM`.
