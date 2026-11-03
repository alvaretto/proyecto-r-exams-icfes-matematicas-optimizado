<!-- Reporte final del auditor-visual-html (6b pareada de la tanda S1b, .Rmd 5cf5b12f = V1), copiado literal
     de su transcripción (agent-a28eca014be7fa2d7.jsonl, sesión 25cb5b83, 2026-11-03T01:31:21.969Z) por el coordinador el 2026-11-03.
     Las rutas /tmp/claude-1000/.../scratchpad/orph_*.json que cita ya no existen; la medición que las sustituye es
     ../puntuacion/metricas_punt.json. -->

## Auditoría visual 6b pareada, tanda S1b (SCHOICE totales-promedios-reciclaje-n3)

El cambio de md5 4cffbe8b → 5cf5b12f cumple su objetivo, pero introduce un residuo nuevo. Cuando la fórmula queda al final de un renglón, el punto final puede quedar solo en el renglón siguiente.

**Alcance.**
- Se midieron 100 semillas × 360 y 320 px, en las versiones 5cf5b12f, 4cffbe8b y e293d5c8.
- No se capturó nada nuevo con Chromium para las 400 capturas. Para el conteo de signos huérfanos sí usé CDP (`Runtime.evaluate`) sobre el HTML, con un perfil temporal fuera del repo.
- Armé 24 pares (12 semillas × 2 anchos) en `s1b/6b/pares/`. Los recortes ampliados están en `s1b/6b/crops/`.
- Leí a resolución completa 7 recortes (14 columnas): s001 a 360 y 320, s003 a 360, s005 a 360, s007 a 360, s012 a 360 y s013 a 320. No llegué a los 8 pedidos. Los 17 pares restantes quedaron sin leer a ojo.

### Métricas (fórmula+unidad partidas, de 1158)

| Ancho | e293d5c8 | 4cffbe8b | 5cf5b12f |
|---|---|---|---|
| 360 px | 139 | 132 | **0** |
| 320 px | 259 | 259 | **0** |

| Otras métricas (iguales a 360 y 320 en 5cf5b12f) | Resultado |
|---|---|
| «cifra kilos/curso(s)» en texto partidos | 0/2003 (era 341 y 347 en e293d5c8) |
| «El/La Bosque/Pradera» partidos | 0/475 |
| Desbordes horizontales | 0 |
| Tabla fuera de su caja | 0 |
| Renglón que empieza con espacio tras una fórmula | 0 (4cffbe8b: 132 a 360 y 259 a 320) |

- Mi sonda independiente de «renglón que empieza con espacio tras `</math>`» reproduce las cifras de la base: 132 y 259. Es el control positivo.
- «kilos» sola tras una fórmula: base 86 («kilos.») + 45 («kilos») = 131 a 360 px, y 183 + 32 = 215 a 320 px. Con 5cf5b12f quedan 0.
- Enunciado, opciones y tabla se ven idénticos en los pares leídos.

### Grep sobre los 100 HTML nuevos

| Patrón | Resultado |
|---|---|
| `\text{` fuera de `<math>`/`<annotation>` | 0 |
| `\ ` fuera de `<math>`/`<annotation>` | 0 |
| `$` fuera de `<math>`/`<annotation>` | 0 |
| `&lt;span` / `&lt;div` visibles | 0 |
| `</math>` seguido de «kilos» | 0 (control positivo en la base: 1158) |

La unidad ahora va en `<mtext mathvariant="normal">kilos</mtext>` dentro del `<math>`.

### Hallazgos

**MEDIO. Punto final huérfano tras la fórmula.**
- Qué se ve (`par_s005_360.png`, `par_s001_360.png`, `par_s007_360.png`): en el procedimiento correcto, la línea «La sede secundaria: 5 × 59 = 295 kilos» llena el renglón. El «.» baja solo al renglón siguiente, con sangría de lista.
- Frecuencia: 34 instancias de «.» solo a 360 px, repartidas en semillas 1, 5, 6, 7, 11, 13, 17, 18, 27, 29, 34, 35, 38, 41, 42… (muestra de las 15 primeras). A 320 px son 5, en las semillas 24, 25, 40, 53 y 59, más 2 «:» solos.
- Causa: antes la unidad ocupaba el salto; ahora el salto cae entre el `<math>` y el signo de puntuación.
- Es legible y no hay desborde. No se ve ni en el enunciado ni en las opciones. Sigue siendo un renglón huérfano, del mismo tipo que «kilos» sola, y solo cambió el elemento huérfano.

**BAJO. Tipografía de «kilos» dentro de la fórmula.**
- Qué se ve: el `mtext` MathML usa la fuente matemática, serif y un poco más pequeña que el cuerpo sans-serif. Lo veo en `par_s003_360.png` («= 180 kilos.»), `par_s001_320.png` y `par_s012_360.png`.
- Juicio: el número y los operadores de la fórmula ya son serif, así que «kilos» queda coherente con la fórmula. El contraste está con «kilos más» y «kilos.» del texto de alrededor, donde el cuerpo es sans-serif. Es un cambio de fuente a mitad de frase en renglones como «= 48 **kilos** más.».
- Es aceptable como BAJO: no afecta la lectura ni la comprensión. Lo cosmético se arreglaría con `\mathrm`/`\textsf` o sacando la unidad, y eso lo decide el profesor.

**BAJO. Fórmula completa en renglón propio.**
- Qué se ve: «Ganó la sede primaria, con» queda en un renglón y la fórmula entera pasa al siguiente, porque el bloque matemático ya no se parte. Pasa en `par_s001_360.png`, `par_s005_360.png` y `par_s012_360.png`.
- Es un efecto esperado: parte de una frase queda con un renglón más corto. La altura de la página cambia (más alta en 34 semillas, más baja en 29, máx ±18 px a 360 px).

**CRÍTICO, ALTO: ninguno.** No encontré markup filtrado, math sin renderizar, desbordes, tabla fuera de su caja, ni diferencias en enunciado u opciones.

### Cobertura por categoría

| Categoría | Resultado |
|---|---|
| Markup/render roto | ninguno |
| Math sin renderizar | ninguno |
| Coherencia tabla-texto | ninguno (leídos s001, s003, s005, s007, s012, s013) |
| Estructura/partes | ninguno |
| Layout/responsividad a 360 y 320 | 1 MEDIO (punto huérfano), 1 BAJO (fórmula en renglón propio) |
| Tipografía/codificación | 1 BAJO (serif de «kilos» en la fórmula) |
| Anomalías cross-versión | ninguno aparte de la lista de semillas con punto huérfano |

### Rutas
- Pares: `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/salida/ciclo_20261102/s1b/6b/pares/par_sNNN_{360,320}.png`, con NNN = 001, 003, 005, 007, 012, 013, 020, 033, 050, 064, 077, 095.
- Recortes: `.../s1b/6b/crops/crop_sNNN_{360,320}.png`.
- Mediciones del punto huérfano: `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/25cb5b83-4c8d-4385-b10a-261162425777/scratchpad/orph_final.json` y `.../orph_base.json`. El script es `.../scratchpad/orph.py`.

### Fix sugerido
No probado, para que lo decida el profesor: incluir el signo final dentro del `\text`/de la fórmula, por ejemplo `$a \times b = T\ \text{kilos}.$`. Así el punto no puede separarse. Hay que medir de nuevo, porque cambia el texto de la Solution.

VEREDICTO_6B: APTO_CON_OBSERVACIONES
