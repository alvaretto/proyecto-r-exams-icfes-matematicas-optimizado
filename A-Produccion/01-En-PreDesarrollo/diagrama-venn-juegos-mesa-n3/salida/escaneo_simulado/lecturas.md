# Escaneo NOPS simulado (2026-11-06)

Hoja de respuestas de `exams2nops` a 300 ppp, con marcas pintadas en la imagen sobre las coordenadas de `read_nops_answers()`: registro 1234567 y respuestas b, d, a. **No hubo papel, impresora ni escáner.** `a.R` genera la hoja y `leer.R` lee con `nops_scan()`.

| Colocación de la hoja A4 | Directo | Con `normalizar_escaneos_nops.py` |
|---|---|---|
| A4 sin cambios | leída | leída |
| Centrada al 100 % en 216 × 330 mm | leída | leída |
| Ajustada al ancho y centrada en 216 × 330 mm | leída | leída |
| Esquina en 216 × 330 mm | error | leída |
| Centrada en 216 × 356 mm | error | leída |
| Esquina en 216 × 356 mm | error | leída |
| Esquina en 216 × 356 mm, girada 0,3° | sin medir | leída |
| Centrada en 216 × 356 mm, más clara | sin medir | leída |

«Leída» significa id `26110600001`, registro `1234567` y respuestas `01000 00010 10000`.

Con la corrección de tinta clara, las marcas naranja, verde claro, amarilla y azul en la esquina de 216 × 330 mm también se leyeron bien.

La FASE 2C r4 midió 95 casos más con manchas, bandas oscuras, giros y hojas cortadas: 0 lecturas equivocadas (`salida/fase2c_r4_detractor.md`).
