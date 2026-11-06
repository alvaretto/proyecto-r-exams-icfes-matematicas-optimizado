## Auditoría Visual HTML — rapidez_media_error_paso_numerico_variacional_argumentacion_n3_schoice_v1

**md5:** 904af1ca1271d97c54123218c9db03c0 al empezar y al terminar (coincide con el esperado).
**Versiones renderizadas:** 100 (OK: 100, fallidas: 0). Seeds 1 a 100. No rendericé nada. Usé las capturas y los HTML de `shots_r4`.
**Viewports:** 360 px y 320 px.
**Capturas revisadas:** 32 individuales (16 versiones × 2 anchos). Ambos anchos: 1, 2, 3, 4, 5, 7, 8, 9, 13, 18, 25, 27, 35, 47, 54, 66. La v8 es la de mayor `maxmath` a 320 px (181) que no estaba en la lista. No leí los contact sheets, porque el control de las 100 versiones lo hice por programa.

### Control por programa (100 versiones, 360 y 320 px)
- **Desborde:** en las 200 mediciones `off` está vacío y `sw` es igual a `W` (360 o 320), nunca mayor. `box` vale 0 en todas.
- **`maxmath`:** el máximo es 181 px. El ancho disponible `bw` es 329 px a 360 y 289 px a 320, así que `maxmath` queda siempre por debajo.
- **Texto visible:** busqué `**`, `$`, `\`, `\text`, `NA`, `NaN`, `NULL`, U+FFFD, `Ã`, `Â`, «a el», «de el» y `..`, con U+00A0 normalizado. Hubo 0 coincidencias en las 200 mediciones.
- **Opciones:**
  - Las 100 versiones tienen 4 opciones, todas distintas.
  - Los 4 textos de feedback también son distintos entre sí.
  - Hay exactamente una «Correcta.» por versión.
  - Esto lo comprobé parseando el HTML.
- **Negativos en los pasos 1, 2 y 3:** 0 versiones tienen un resultado u operando negativo en esos pasos. Revisé con regex todas las líneas `Paso N.` del enunciado. Las opciones sí afirman negativos (por ejemplo «−20 km»), como está previsto.

### Control por imagen (16 versiones en 360 y 320 px)
- **Cita «…» con fórmula:** se ve completa y sin cortes en todas.
- **Encabezados de la Solution:** están todos y en el orden esperado (Respuesta correcta, Análisis del error, Procedimiento correcto, Propiedades del concepto, Caso específico, Análisis de las demás razones, Reflexión metacognitiva, Estrategia para evitar el error).
- **EV-D-PROM:** aparece en v2, v3, v4, v9, v13, v35 y en las demás donde corresponde. Dice «es la posición a mitad de camino entre las dos lecturas…». Las cifras cuadran (por ejemplo, 140 km en v2).
- **EV-T-CUENTA:** N = M + 1 en todas las que la tienen.

| Versión | Marcas | Intervalos |
|---|---|---|
| v1 | 5 | 4 |
| v3 | 3 | 2 |
| v9 | 4 | 3 |
| v18 | 4 | 3 |
| v25 | 4 | 3 |

- **Feedback final:** una sola «Correcta.» y su letra coincide con la opción correcta en las 32 capturas.
- **Canónicas (v7, v27, v47, v54, v66):**
  - Enunciado y pasos idénticos: 100 km − 20 km = 80 km; 10 h − 8 h = 2 h; 80 h/2 km = 40 h/km.
  - La correcta es «En el paso 3, ya que el cociente es 80 km/2 h».
  - Los distractores son los mismos cuatro textos del impreso (−20 km, −2 h, «100 km y no 80 km»).
  - La Solution lleva EV-D-FINAL, EV-D-SIGNO y EV-T-SIGNO.
  - Lo que cambia entre ellas es el orden de las opciones (shuffle). Solo v7 y v54 tienen la correcta en primer lugar; en v27 queda en d, en v47 en b y en v66 en d.
  - **NO VERIFICABLE:** la correspondencia exacta de la posición de cada opción con el impreso. No tuve el impreso a la vista, y el orden por semilla es aleatorio por diseño.

### Hallazgos

**[NO-CRÍTICO] Solution de EV-D-SIGNO con un negativo en la prosa** — v7, v27, v35, v47, v54, v66, v1/v2 y otras donde sale la razón.
- Qué se ve (p. ej. v07_360.png): «Además, la cuenta de ese argumento tampoco es correcta: 20 km − 100 km = −80 km».
- Por qué se anota: aparece un negativo, pero es la refutación de la opción y no un paso del procedimiento. No viola la regla de que los pasos 1 a 3 no lleven negativos.
- Frecuencia: las versiones con EV-D-SIGNO.

**[NO-CRÍTICO] En las canónicas, el orden de las opciones varía con la semilla** — v27, v47, v66 frente a v7 y v54.
- Qué se ve: la primera opción cambia entre canónicas (v27_360.png, v47_360.png, v66_360.png).
- Por qué se anota: si el impreso fija un orden, estas versiones no lo reproducen. No pude comprobarlo (ver NO VERIFICABLE más arriba).

### Cobertura por categoría
| Categoría | Resultado |
|---|---|
| Markup/render roto | ninguno (0 coincidencias en 200 mediciones; sin `##ANSWER`, sin `NA`/`NaN`/`NULL` en las 32 capturas) |
| Math sin renderizar | ninguno (MathML correcto en las 32 capturas; ningún `$`, `\` ni `\text` visible) |
| Coherencia tabla-texto | ninguno (4 opciones distintas en las 100; una sola Correcta; N = M + 1 en EV-T-CUENTA; cifras de EV-D-PROM coherentes con los pasos) |
| Estructura/partes | ninguno (encabezados completos y feedback con 4 entradas en las 16 versiones vistas por ancho) |
| Layout/responsividad @360 y @320 | ninguno (`off` vacío, `sw` ≤ `W`, `maxmath` ≤ 181 frente a 289 de ancho disponible a 320 px) |
| Tipografía/codificación | ninguno (tildes correctas, sin mojibake ni U+FFFD, sin «a el»/«de el»/«..») |
| Anomalías cross-versión | ninguno (sin negativos en los pasos; sin versiones vacías ni truncadas) |

### Veredicto
**APTO_VISUAL.** Las 100 versiones pasan los controles de desborde, texto, opciones y negativos en 360 y 320 px, y las 16 versiones revisadas por imagen salen correctas. Lo único que queda es no verificable: el orden de las opciones frente al impreso.
