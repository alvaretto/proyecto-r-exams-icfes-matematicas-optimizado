<!-- Reporte íntegro del auditor-visual-html de la tanda S2a (6b pareada, .Rmd 63e2713e frente a 6c9d7705), copiado por el orquestador de su mensaje final del 2026-11-03. El archivo informe_6b_63e2713e.md es el resumen breve que escribió el propio auditor. -->

## Auditoría visual 6b pareada, S2a, `.Rmd` 63e2713e

**Veredicto: APTO_VISUAL.** La métrica de signo huérfano da 0 en las tres capas (HTML, PDF y Moodle), y los controles positivos reproducen las cifras esperadas. No hay hallazgos CRÍTICOS, ALTOS, MEDIOS ni BAJOS.

**N declarado (familia B):** 100 semillas por versión y por ancho (360 y 320 px). Fueron 1000 mediciones con 0 errores, y `innerWidth` salió 360 y 320. El PDF tiene 100 ejercicios y Moodle 101 preguntas (n = 100, `set.seed(1)`).

**Desviación del instrumento.** `python3 -I` no encuentra `websockets`, que está en el site de usuario. Ejecuté `cap_s1b2.py` sin `-I`, desde un directorio fuera del repo y con el perfil en `/tmp/claude-1000/s2a_6b/perfil`. El script no se tocó (md5 1d0117ed…). `resumir_s1b2.py`, `pdf_renglones_bbox.py` y `medir_moodle_s2a.py` sí corrieron con `-I`.

**Comparación con 6c9d7705.** `cmp` da 58 HTML idénticos y 42 distintos entre final y ant. Esos 42 son exactamente la lista de semillas con ERR-RESTA. La confirmé por conteo, no semilla por semilla.

### 1. Métricas HTML (salida de `resumir_s1b2.py`)

360 px:

| versión | fórmula+unidad partidas | signo tras fórmula | signo tras otro elemento | renglón con espacio inicial tras fórmula | cifra-unidad | El/La | fórmula partida | desbordes | tabla fuera | altura vs ant |
|---|---|---|---|---|---|---|---|---|---|---|
| final | 0/1158 | **0/0** | 0/100 | 0/927 | 0/2403 | 0/499 | 0 | 0 | 0 (100 tablas) | 0 más alto, 0 más bajo, ±0 px |
| ant | 0/1158 | 0/42 | 0/100 | 0/885 | 0/2403 | 0/499 | 0 | 0 | 0 | referencia |
| base 4cffbe8b | **132**/1158 | 0/42 | 0/100 | 132/1264 | 0/2403 | 0/499 | 0 | 0 | 0 | más alto en 29, más bajo en 34, máx ±18 px |
| v1 5cf5b12f | 0/1158 | **34**/772 | 0/100 | 0/534 | 0/2403 | 0/499 | 0 | 0 | 0 | ±0 px |
| e293d5c8 | 139/1158 | 0/42 | 0/100 | 0/1264 | **341**/2403 | **74**/499 | 0 | 0 | 0 | más alto en 18, más bajo en 43, máx ±36 px |

320 px:

| versión | fórmula+unidad partidas | signo tras fórmula | signo tras otro elemento | renglón con espacio inicial tras fórmula | cifra-unidad | El/La | fórmula partida | desbordes | tabla fuera | altura vs ant |
|---|---|---|---|---|---|---|---|---|---|---|
| final | 0/1158 | **0/0** | 0/100 | 0/927 | 0/2403 | 0/499 | 0 | 0 | 0 | ±0 px |
| ant | 0/1158 | 0/42 | 0/100 | 0/885 | 0/2403 | 0/499 | 0 | 0 | 0 | referencia |
| base | **259**/1158 | 0/42 | 0/100 | 259/1264 | 0/2403 | 0/499 | 0 | 0 | 0 | más alto en 15, más bajo en 5, máx ±18 px |
| v1 | 0/1158 | **5**/772 | 0/100 | 0/534 | 0/2403 | 0/499 | 0 | 0 | 0 | más bajo en 2, máx ±18 px |
| e293d5c8 | 259/1158 | 0/42 | 0/100 | 0/1264 | **347**/2403 | **55**/499 | 0 | 0 | 0 | más alto en 14, más bajo en 5, máx ±18 px |

### 2. Signo huérfano tras toda fórmula

- **final:** denominador 0 a 360 y a 320 px. Ningún signo es el primer carácter tras un `<math>`.
- **ant:** 42 signos tras fórmula (los de ERR-RESTA), 0 partidos. Es lo esperado.
- **final frente a ant:** el renglón con espacio inicial pasa de 885 a 927. Los +42 son los espacios de « Esa resta» que ahora siguen al `<math>` que contiene el punto. Ninguno quedó partido, y todas las alturas son idénticas a ant (±0 px).

### 3. Controles positivos

Las cifras esperadas y las obtenidas coinciden en las 8 comprobaciones. Una cifra distinta habría invalidado el instrumento.

| control | ancho | esperado | obtenido |
|---|---|---|---|
| base: fórmula+unidad partidas | 360 | 132 | 132 |
| base: fórmula+unidad partidas | 320 | 259 | 259 |
| v1: signo huérfano | 360 | 34 | 34 |
| v1: signo huérfano | 320 | 5 | 5 |
| e293: cifra-unidad | 360 | 341 | 341 |
| e293: cifra-unidad | 320 | 347 | 347 |
| e293: El/La | 360 | 74 | 74 |
| e293: El/La | 320 | 55 | 55 |

### 4. PDF (100 ejercicios)

Medición con `pdf_renglones_bbox.py`; salida en `pdf.txt`.

| PDF | renglones que empiezan por un signo | que empiezan por la unidad | tras una fórmula |
|---|---|---|---|
| final 63e2713e | 0 | 151 | 0 |
| 2a5e59a1 (V1′) | 0 | 151 | 0 |

Las cifras son idénticas. Las 151 de «unidad» son las de cada frase («curso:», «cursos,»), y 0 de ellas va tras una fórmula.

### 5. Moodle (n = 100, `medir_moodle_s2a.py`)

| versión | signo tras el cierre de toda fórmula | signo dentro de la fórmula, sin unidad |
|---|---|---|
| final | **0** | 40 |
| ant | 40 (control esperado: 40) | 0 |
| v1 | 773 (control esperado: 773) | 0 |

En las tres versiones: 101 preguntas, 1307 fórmulas, 0 espacios especiales y `math_mathml` 0. Los 40 puntos de ant pasaron a estar dentro de la fórmula en final, y las demás métricas no cambian. El 40 de Moodle frente al 42 del HTML viene de que no todas las semillas del HTML están en la muestra de 101 de Moodle; no lo comprobé.

### 6. Lectura a ojo: 12 recortes, 6 pares final/ant

Leí los 12 recortes a resolución completa, ×2. Los 6 pares son seeds 1, 4 y 10 a 360 px, y seeds 1, 4 y 10 a 320 px. Cada par es final junto a ant, centrado en el párrafo «La cifra … sale de restar los promedios: … Esa resta…». Están en `S2A/6b/crops/`, con nombres `final_sNNN_AAA.png` y `ant_sNNN_AAA.png`.

- Los 6 pares se ven idénticos a ojo. El punto queda pegado a la cifra («= 15.», «= 12.», «= 13.») y «Esa resta…» sigue en el mismo renglón. No hay espacio raro entre la cifra y el punto.
- El punto dentro de la fórmula (serif, del `<math>`) y el de fuera (sans) no se distinguen de forma apreciable. Es el mismo aspecto que ya tenía el «48 kilos.» de la misma Solution.
- La fórmula no desborda y no hay punto huérfano.

Limitación: no encontré semillas con la fórmula cerca del final del renglón. Medí el borde derecho de la fórmula en las 42 semillas con ERR-RESTA:
- **360 px:** acaba en x = 134 (26 semillas), 126 (13) o 133 (3).
- **320 px:** x = 217, 209 y 216.

Siempre queda a mitad de renglón, porque la fórmula es corta (δ numérico). El peor caso de corte de renglón no se da con el texto actual en estos dos anchos. Eso explica que la métrica dé 0 partidos, y también que ant diera 0/42 y que final lo siga dando.

### Hallazgos

Ninguno, en ninguna categoría:
- Fórmula+unidad partidas: 0.
- Signo huérfano: 0.
- Espacio inicial partido: 0.
- Cifra-unidad y El/La: 0.
- Fórmula partida por dentro: 0.
- Desbordes y tablas fuera de su caja: 0.
- Alturas: diferencia 0 px frente a ant.
- PDF: 0 renglones con signo inicial y 0 con la unidad tras fórmula.
- Moodle: 0 signos tras el cierre de una fórmula.

Fuera de alcance: la apariencia exacta de los radios en Moodle, que `exams2html` no muestra.

### Archivos
En `.../salida/ciclo_20261102/s2a/6b/`:
- `informe_6b_63e2713e.md` (resumen breve; este mensaje es el informe completo)
- `metricas.json`
- `resumen.txt`
- `pdf.txt`
- `moodle.txt`
- `crops/` (12 PNG)

El script `crops.py` y las capturas de trabajo quedaron en `/tmp/claude-1000/s2a_6b/`.
