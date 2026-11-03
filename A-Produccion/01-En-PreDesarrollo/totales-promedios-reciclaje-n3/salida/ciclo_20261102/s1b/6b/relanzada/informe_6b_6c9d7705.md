# 6b pareada, S1b relanzada: .Rmd md5 6c9d7705 (V1')
Instrumento: S1B/relanzada/cap_s1b2.py y resumir_s1b2.py (los dados, sin cambios), python3, CDP con innerWidth comprobado (320 y 360). Medido de nuevo por mí, 800 jobs, 0 errores. Familia B: N = 100 semillas por versión y ancho para las métricas; capturas: 100 semillas x 3 versiones (final, 4cffbe8b, V1) x 2 anchos = 600 PNG en /tmp/claude-1000/shots_6b (fuera del repo).
cmp con render_2a5e59a1: 100/100 HTML idénticos (cmp -s, semilla por semilla); 0 distintos.

| Métrica (N=100) | final 360/320 | 4cffbe8b | e293d5c8 | 5cf5b12f (V1) |
|---|---|---|---|---|
| 1 fórmula+unidad partidas (de 1158) | 0 / 0 | 132 / 259 | 139 / 259 | 0 / 0 |
| 2 signo huérfano tras fórmula (de 42 en final; 772 en V1) | 0 / 0 | 0 / 0 | 0 / 0 | 34 (semillas 1,5,6,7,11,13,17,18,27,29,34,35,38,41,42...) / 5 (24,25,40,53,59) |
| 3 renglón con espacio inicial tras fórmula | 0/885 · 0/885 | 132/1264 · 259/1264 | 0/1264 · 0/1264 | 0/534 · 0/534 |
| 4 cifra-unidad partidas (de 2403) / El-La (de 499) | 0 / 0 | 0 / 0 | 341 · 74 / 347 · 55 | 0 / 0 |
| 5 desbordes · tabla fuera de caja · fórmula partida por dentro | 0 · 0 · 0 | 0 · 0 · 0 | 0 · 0 · 0 | 0 · 0 · 0 |
| 6 altura vs 4cffbe8b | más alto 34, más bajo 29, máx ±18 px (360); 5 / 15, ±18 (320) | ref | 0/23, ±36 (360) | igual que final |
Controles positivos: 4cffbe8b da 132/1158 y 259 (métricas 1 y 3); V1 da 34 y 5 en la 2; e293d5c8 da 341/74 en la 4. Todos reproducen lo esperado.
Grep sobre 100 HTML finales, fuera de <math> y <annotation>: `\text{` 0, `\ ` 0, `$` 0, `&lt;span` 0, `&lt;div` 0.

Pares leídos a resolución completa (tira base|V1|final): s001, s005, s007 (360); s024, s025, s040, s053, s059 (320) = 8 de Solution (crops/sol_*), más el par completo s001_360. Pares en pares/, recortes en crops/.
Lectura: en V1 el punto o los dos puntos quedan solos en s001, s005, s007 (360) y s024, s025, s040, s053, s059 (320); en el final desaparecen en las 8. Enunciado, opciones y tabla idénticos entre columnas en los pares leídos. Sin markup filtrado ni matemática sin renderizar.

Hallazgos
- CRÍTICO / ALTO / MEDIO: ninguno.
- BAJO 1: «kilos.» dentro de la fórmula sale en fuente serif y más pequeña que el texto (visible en todos los pares; sobre todo en las viñetas y «= 23 kilos más»). Frecuencia: todas las semillas con fórmula+unidad. Igual que V1, ya declarado.
- BAJO 2: cuando la fórmula no cabe, pasa entera al renglón siguiente y deja un renglón corto previo («La sede secundaria:», «Ganó la sede central, con», «y»); s001, s024, s025, s040, s053, s059. Hay semillas más altas (34 a 360, 5 a 320, máx 18 px).
- BAJO 3: en la lista de la Solution a 320, «= 210 kilos.» llega casi al borde derecho de la columna (s053); sin desborde (0 medido).
