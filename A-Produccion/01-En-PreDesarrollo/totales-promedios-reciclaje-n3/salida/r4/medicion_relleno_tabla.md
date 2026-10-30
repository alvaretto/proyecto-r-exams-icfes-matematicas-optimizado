# Desborde de la tabla en HTML: versión aprobada (r3) frente a variantes de relleno (ronda 4)

Semillas 1..100 (exams2html, una versión por semilla), Chromium headless, caja = div overflow-x:auto (289 px a 360, 249 px a 320).

```
base    360px: n=100 tabla > caja en 0/100; separación mínima entre textos de cabecera 5.8 px; desborde de página 0
base    320px: n=100 tabla > caja en 9/100 (exceso máx 11.0 px); separación mínima entre textos de cabecera 4.0 px; desborde de página 0
pad06   360px: n=100 tabla > caja en 49/100 (exceso máx 29.0 px); separación mínima entre textos de cabecera 23.2 px; desborde de página 0
pad06   320px: n=100 tabla > caja en 100/100 (exceso máx 69.0 px); separación mínima entre textos de cabecera 23.2 px; desborde de página 0
pad04   360px: n=100 tabla > caja en 9/100 (exceso máx 9.0 px); separación mínima entre textos de cabecera 16.8 px; desborde de página 0
pad04   320px: n=100 tabla > caja en 88/100 (exceso máx 49.0 px); separación mínima entre textos de cabecera 16.8 px; desborde de página 0
pad03   360px: n=100 tabla > caja en 0/100; separación mínima entre textos de cabecera 13.7 px; desborde de página 0
pad03   320px: n=100 tabla > caja en 49/100 (exceso máx 40.0 px); separación mínima entre textos de cabecera 13.6 px; desborde de página 0
pad025  360px: n=100 tabla > caja en 0/100; separación mínima entre textos de cabecera 14.2 px; desborde de página 0
pad025  320px: n=100 tabla > caja en 49/100 (exceso máx 35.0 px); separación mínima entre textos de cabecera 12.0 px; desborde de página 0
L06     360px: n=100 tabla > caja en 0/100; separación mínima entre textos de cabecera 15.0 px; desborde de página 0
L06     320px: n=100 tabla > caja en 49/100 (exceso máx 30.0 px); separación mínima entre textos de cabecera 13.6 px; desborde de página 0
L05     360px: n=100 tabla > caja en 0/100; separación mínima entre textos de cabecera 13.0 px; desborde de página 0
L05     320px: n=100 tabla > caja en 49/100 (exceso máx 27.0 px); separación mínima entre textos de cabecera 12.0 px; desborde de página 0
L04     360px: n=100 tabla > caja en 0/100; separación mínima entre textos de cabecera 11.0 px; desborde de página 0
L04     320px: n=100 tabla > caja en 49/100 (exceso máx 24.0 px); separación mínima entre textos de cabecera 10.4 px; desborde de página 0
I04 (inline, padding-left 0.4em, sin inline-block) 320px: 49/100 (exceso máx 17 px); N0 (spans sin estilo, sin espacio duro) 320px: 9/100 = base
F05 (cabeceras 0.9em; padding-left 0.5em en columnas 2-3) 360px: 0/100, separación mín 13.5 px; 320px: 9/100 (exceso máx 9 px), separación mín 11.2 px
F06 (cabeceras 0.85em; padding-left 0.6em) 360px: 0/100, 15.0 px; 320px: 9/100 (exceso máx 11 px), 12.2 px
```
r4b     360px: n=100 tabla > caja en 0/100; separación mínima entre textos de cabecera 13.5 px; desborde de página 0
r4b     320px: n=100 tabla > caja en 9/100 (exceso máx 9.0 px); separación mínima entre textos de cabecera 11.2 px; desborde de página 0
r4b     1024px: n=100 tabla > caja en 0/100; separación mínima entre textos de cabecera 11.2 px; desborde de página 0
