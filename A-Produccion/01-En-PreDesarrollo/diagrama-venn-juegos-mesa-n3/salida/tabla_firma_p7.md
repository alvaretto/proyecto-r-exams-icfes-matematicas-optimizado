# Firma del profesor — `diagrama-venn-juegos-mesa-n3`

**Fecha:** 2026-11-06.
**Respuesta literal:** «firmo tal cual, mantengo I-3, a».
**Vale para:** `.Rmd` con md5 `f886dfaa45b80a5ee16f9820908049b5`. Si el `.Rmd` cambia, la firma queda sin efecto y hay que medir de nuevo.
**Líneas citadas:** las de `verificar_espacio.R` con md5 `b51920766863f44aa1b13157e5b53d73`, el que estaba en disco al firmar.

## Qué se le mostró al firmar

- **Tasa:** probabilidad de acertar con el atajo, eligiendo al azar entre los diagramas que deja. Sin atajo, 25 %.
- **Exceso:** diferencia con el techo que se alcanza por azar: 32,2 % en las variantes y 38,5 % en la canónica.
- **Se firma** lo que pase de +8 pp.

## Variantes: 11 000 000 de versiones, techo nulo 32,2 %

| Regla | Línea | Atajo, como se le glosó | Tasa exacta | Exceso |
|---|---|---|---|---|
| `MAG-suma-segunda-menor` | 849 | Sumar los siete números de cada diagrama y elegir la segunda suma más baja | 100,00 % | +67,8 pp |
| `EST-menos-cifras-del-enunciado` | 866 | Elegir el diagrama con menos números que aparecen en el enunciado | 69,55 % | +37,4 pp |
| `MAG-menor-maximo` | 852 | Elegir el diagrama cuyo número más grande sea el menor | 53,03 % | +20,8 pp |
| `MAG-suma-no-extrema` | 851 | Descartar la suma más alta y la más baja | 50,00 % | +17,8 pp |
| `MAG-sin-el-mayor-numero` | 853 | Descartar los diagramas con el número más grande | 50,00 % | +17,8 pp |
| `REL-no-extrema-en-coincidencias` | 864 | Contar coincidencias zona por zona con los otros diagramas y descartar el de más y el de menos | 50,00 % | +17,8 pp |
| `EST-primer-dato-en-la-zona-doble` | 868 | Quedarse con los diagramas que traen el dato «pero no» en la zona de esos dos conjuntos | 50,00 % | +17,8 pp |
| `EST-total-en-zona-de-un-conjunto` | 867 | Descartar los diagramas con un total en una zona de un solo conjunto | 49,46 % | +17,3 pp |

La siguiente regla es `LEX-con-repetidos`, con 33,42 % (+1,2 pp), y no entra en la firma.

Con cuatro decimales de `salida/firma_p7/exacto.txt`, las tasas son 100,000, 69,553, 53,032, 50,000, 50,000, 50,000, 50,000 y 49,456 %. Esa enumeración es independiente de la de la FASE 2C r2 y coincide con ella.

## Canónica: 24 órdenes, techo nulo 38,5 %. Es el impreso (H-2)

| Exceso | Tasa | Reglas |
|---|---|---|
| +61,5 pp | 100,0 % | `MAG-suma-segunda-menor`, `MAG-menor-maximo`, `FOR-menos-dos-cifras` (menos números de dos cifras, línea 860), `FOR-cadena-corta` (menos dígitos en total, línea 861), `LEX-con-repetidos` (trae un número repetido, línea 862) |
| +11,5 pp | 50,0 % | `MAG-suma-no-extrema`, `MAG-sin-el-mayor-numero`, `MAG-centro-mayor` (centro más grande, línea 854), `DIV-suma-par` (suma par, línea 855), `REL-no-extrema-en-coincidencias`, `EST-total-en-zona-de-un-conjunto`, `EST-primer-dato-en-la-zona-doble` |

## Lo que se le dijo para decidir

- **Orden de las sumas:** es algebraico y también está en el impreso (25 < 32 < 61 < 78). Las reglas de suma exigen sumar 28 números. En el 67,5 % de las variantes, la suma de la clave queda a menos de 15 % de la más cercana.
- **Margen de las otras dos reglas MAG:** bajo 15 % en el 33,1 % de los casos a favor de la clave para `MAG-menor-maximo`, y en 0 % para `MAG-sin-el-mayor-numero`.
- **`EST-menos-cifras-del-enunciado`:** 69,55 % en las variantes y 0,0 % en el impreso. El distractor A siempre trae dos cifras del enunciado sin operar, y la clave solo una, salvo coincidencias. En el impreso la clave trae tres por coincidencia (7, 3, 7). Las variantes premian un atajo que en el original falla.

## Alternativa que el profesor no eligió

Filtrar las versiones para que la clave repita al menos tantas cifras del enunciado como el distractor A. Medido sobre el modelo enumerado, no en el `.Rmd` (`salida/firma_p7/filtro.py` y `exacto.txt`):

| | Tal cual (firmado) | Con el filtro |
|---|---|---|
| Tasa de `EST-menos-cifras-del-enunciado` | 69,55 % | 34,8 % (+2,6 pp) |
| Versiones | 11 000 000 | 5 133 405 |
| Clave con alguna zona vacía | 14,5 % | 21,7 % |
| `MAG-menor-maximo` | 53,03 % | 54,9 % |

**Condición acordada:** si en el aula se ve que los estudiantes usan ese atajo, se aplica este filtro. Eso cambia el `.Rmd`, así que exigiría otro detractor y una firma nueva.

## I-3 frente a la regla #1

Firmado: se mantiene I-3. Los tres distractores de la Guía oficial van en todas las versiones. Cambian los datos y un error adicional de la Solution, sorteado entre dos, pero no el tipo de error de las opciones.

## Aprobación

`a`: aprobado para llevar al aula. La promoción espera la evidencia de aula.

## Medido después de la firma (2026-11-06)

No se le mostró al firmar. Se le informa en el reporte siguiente, y no cambia lo firmado salvo que él decida otra cosa. Script y salida: `salida/firma_p7/margenes.py` y `margenes.txt`.

| Regla | Margen < 15 % cuando favorece a la clave | Margen medio |
|---|---|---|
| `EST-menos-cifras-del-enunciado` | 0,0 %, en 9 452 819 versiones | 47,9 % |
| `REL-no-extrema-en-coincidencias` | 0,0 %, en 11 000 000 versiones | 42,9 % |

`EST-primer-dato-en-la-zona-doble` y `EST-total-en-zona-de-un-conjunto` comprueban si un número está o no, sin gradación: el margen no aplica. §P7-B no exime a ninguna de estas cuatro.

Las dos cifras de 0,0 % dicen poco: son conteos enteros de 0 a 7, así que el margen mínimo posible ya es 1/7 = 14,3 %. La conclusión se sostiene porque §P7-B exime lo imperceptible y no lo contable.

### El techo de la columna «Exceso» (FASE 2C r4, H24)

El exceso que se le mostró resta el techo de N = 100 (32,2 %) a una tasa exacta. El techo exacto, con la clave permutada sobre las 11 000 000 de versiones, es 25,03 %. Las 8 reglas firmadas siguen por encima del corte y suben 7,2 pp, pero una regla fuera de la lista cruza los +8 pp:

| Regla | Tasa exacta | Sobre 32,2 % | Sobre 25,03 % |
|---|---|---|---|
| `EST-menos-cifras-del-enunciado` | 69,553 % | +37,4 pp | +44,5 pp |
| `LEX-con-repetidos` | 33,418 % | +1,2 pp | **+8,39 pp** |
| `DIV-suma-par`, `DIV-suma-impar` | 29,545 % | −2,7 pp | +4,5 pp |

La decisión sobre `LEX-con-repetidos` es del profesor (H-5) y queda pendiente.
