<!-- Reporte final del auditor-visual-html (6b pareada de la tanda S1b relanzada, .Rmd 6c9d7705 = V1′), copiado literal de
     su notificación de entrega por el orquestador-schoice el 2026-11-03 (17 llamadas, 226 s). informe_6b_6c9d7705.md, en
     esta carpeta, es la versión corta que escribió el propio auditor. -->

## Auditoría visual 6b pareada, S1b relanzada (`.Rmd` md5 `6c9d7705`, V1′)

**Veredicto técnico.** No hay hallazgos CRÍTICOS, ALTOS ni MEDIOS. El punto o los dos puntos solos en un renglón, que dejaba V1, desaparecen en las 100 semillas a 360 y a 320 px.

**Instrumento.** `cap_s1b2.py` y `resumir_s1b2.py` tal como estaban, con `python3`; ancho por CDP, `innerWidth` 320 y 360; 800 trabajos de medida, 0 errores.

**Familia B.** N = 100 semillas por versión y ancho. 600 PNG (semillas 1 a 100 de final, 4cffbe8b y V1, a ambos anchos) en `/tmp/claude-1000/shots_6b`, fuera del repo. Perfil de Chromium en `/home/bootcamp/.cache/s1b2/`.

**`cmp` contra `render_2a5e59a1`.** Los 100 HTML son idénticos, semilla por semilla: 0 distintos.

| Métrica (N = 100, 360 / 320 px) | final | 4cffbe8b | e293d5c8 | 5cf5b12f (V1) |
|---|---|---|---|---|
| 1. Fórmula y unidad partidas (de 1158) | 0 / 0 | 132 / 259 | 139 / 259 | 0 / 0 |
| 2. Signo huérfano tras una fórmula (de 42 en final; 772 en V1) | 0 / 0 | 0 / 0 | 0 / 0 | 34 / 5 |
| 3. Renglón que empieza con un espacio tras una fórmula | 0/885 · 0/885 | 132/1264 · 259/1264 | 0/1264 · 0/1264 | 0/534 · 0/534 |
| 4. Cifra-unidad partidas (de 2403) | 0 / 0 | 0 / 0 | 341 / 347 | 0 / 0 |
| 4. «El/La» Bosque/Pradera partidos (de 499) | 0 / 0 | 0 / 0 | 74 / 55 | 0 / 0 |
| 5. Desbordes, tabla fuera de su caja, fórmula partida por dentro | 0 / 0 | 0 / 0 | 0 / 0 | 0 / 0 |
| 6. Altura frente a 4cffbe8b | +34 / −29, ±18 px · +5 / −15, ±18 px | referencia | −23, ±36 px · −1, ±18 px | igual que final |

- Controles positivos: 4cffbe8b, 132/1158 y 259 en 1 y 3; V1, 34 semillas a 360 px y 5 a 320 px (24, 25, 40, 53, 59; 3 puntos y 2 dos puntos) en 2; e293d5c8, 341/74 y 347/55 en 4.
- Grep sobre los 100 HTML finales, fuera de `<math>` y `<annotation>`: `\text{` 0, `\ ` 0, `$` 0, `&lt;span` 0, `&lt;div` 0.

**Lectura a ojo.** 9 imágenes a resolución completa: 1 par entero (semilla 1 a 360 px) y 8 recortes de la Solution en tres columnas (4cffbe8b, V1, final): semillas 1, 5 y 7 a 360 px; 24, 25, 40, 53 y 59 a 320 px. En V1 se ve el signo solo en las 8; en el final, en ninguna. Enunciado, opciones y tabla idénticos en lo leído. Sin markup filtrado ni matemática sin renderizar.

**Hallazgos.**
- CRÍTICO, ALTO, MEDIO: ninguno.
- BAJO 1. «kilos.» dentro de la fórmula sale en fuente serif y más pequeña que el cuerpo (viñetas de totales, «= 23 kilos más»). Igual que V1, ya declarado.
- BAJO 2. Cuando la fórmula no cabe, pasa entera al renglón siguiente y deja uno corto antes («La sede secundaria:», «Ganó la sede central, con», «y»): semillas 1, 24, 25, 40, 53 y 59. Es la causa de las diferencias de altura (hasta 18 px).
- BAJO 3. A 320 px, «= 210 kilos.» queda cerca del borde derecho de la columna (semilla 53). Desborde medido: 0.

**Fuera de alcance:** solo HTML; PDF y Moodle no se midieron en esta 6b (los midió el orquestador, `../../relanzada/resumen_medicion_s1b2.txt`).

**Rutas:** `6b/relanzada/` — `metricas.json`, `metricas_cap.json`, `resumen.txt`, `cmp_vs_2a5e59a1.txt`, `pares/` (10), `crops/` (10; 8 leídos).

VEREDICTO_6B: APTO_CON_OBSERVACIONES
