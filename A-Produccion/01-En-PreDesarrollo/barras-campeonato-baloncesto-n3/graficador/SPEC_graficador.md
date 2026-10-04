# Spec del Graficador — barras-campeonato-baloncesto-n3 (paso 2b, regla #3)

Directorio base: `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/barras-campeonato-baloncesto-n3/graficador/`
Originales recortados (referencia de fidelidad, NO editar): `originales/`

## Figuras a reproducir (inventario H-3, verificado mirando el JPG)

| Archivo original | Tipo | Contenido exacto |
|---|---|---|
| `orig_enunciado_barras_septimo.png` | barras simples (enunciado) | Título 2 líneas negrita "Gráfica de información del / campeonato para grado séptimo". Eje y "Número de partidos" (rotado), ticks 0..8 paso 1, líneas de cuadrícula horizontales grises finas. 2 barras: "Partidos ganados" = 8 (magenta ~#FF1A8C), "Partidos perdidos" = 4 (verde ~#00B33C). Sin leyenda. Solo ejes izquierdo e inferior. |
| `orig_opcion_A_apilada.png` | barras APILADAS (opción) | Título "Informe de partidos del campeonato". Eje y "Número de partidos" 0..13 paso 1, cuadrícula. Categorías x: "Partidos ganados", "Partidos perdidos". Segmento inferior = Grado séptimo (cian ~#1EAAD8), superior = Grado sexto (rojo-naranja ~#F2501E). Valores A: ganados séptimo 5 + sexto 8 (total 13); perdidos séptimo 7 + sexto 4 (total 11). Leyenda a la derecha: cuadrado rojo "Grado sexto", cuadrado cian "Grado séptimo" (en ese orden, de arriba abajo). |
| `orig_opcion_B_apilada.png` | barras APILADAS | Igual estilo que A. Valores B: ganados séptimo 8 + sexto 4 (total 12); perdidos séptimo 5 + sexto ≈4,5 (total ≈9,5 — así aparece en el impreso; reprodúcelo tal cual para la comparación). Eje y 0..13. |
| `orig_opcion_C_agrupada.png` | barras AGRUPADAS (clave) | Título igual. Eje y 0..8 paso 1. Por categoría, dos barras contiguas: izquierda Grado sexto (rojo), derecha Grado séptimo (cian). Valores C: ganados sexto 5, séptimo 8; perdidos sexto 7, séptimo 4. Leyenda derecha igual que A. |
| `orig_opcion_D_agrupada.png` | barras AGRUPADAS | Igual estilo que C. Valores D: ganados sexto 5, séptimo 7; perdidos sexto 8, séptimo 4. |

Las letras "A.", "B.", "C.", "D." del impreso NO se reproducen (regla #4: sin títulos ni rótulos con letra).
La tabla del grado sexto se emitirá como tabla Markdown en el `.Rmd`, NO como gráfico: no la reproduzcas.

## Requisito de parametrización (regla #22: las figuras son DINÁMICAS por versión)

Implementa DOS funciones (o equivalentes en tu lenguaje) que reciban TODO como parámetro, sin valores literales del original dentro:

1. `barras_simples(valores[2], categorias[2], colores[2], titulo, etiqueta_y, archivo_png)`
   — eje y de 0 a `max(valores)` (entero), ticks enteros paso 1 si max <= 15 (si no, paso 2).
2. `barras_opcion(matriz 2x2 [grupo x categoria], grupos[2], categorias[2], colores[2], modo = "agrupada"|"apilada", titulo, etiqueta_y, archivo_png)`
   — agrupada: grupo 1 a la izquierda, grupo 2 a la derecha dentro de cada categoría; eje y 0..max(valores).
   — apilada: grupo 2 ABAJO, grupo 1 ARRIBA (como en el impreso); eje y 0..max(totales).
   — leyenda a la derecha con el grupo 1 arriba y el grupo 2 abajo, siempre.
   — los valores pueden ser enteros de 1 a 14 y no decimales (salvo el 4,5 de B, solo para la comparación).
   — tamaño de salida fijo (todas las opciones del mismo tamaño en píxeles, ~ 6 x 4,2 pulgadas a 150 dpi) para que en el Answerlist no haya una opción más grande que otra.
   — sin título con letra, sin texto que revele cuál es correcta, nombre de archivo neutro recibido como argumento.

Texto en español con tildes ("Número de partidos", "Grado séptimo", "Gráfica de información…"). En TikZ usa `\usepackage[utf8]{inputenc}` o escribe las tildes como `\'{e}`/`\'{u}` si hace falta.

## Entregables (en `graficador/<lenguaje>/`)

- El código generador (archivo fuente).
- Renders canónicos con los valores del impreso: `<lenguaje>_barras_septimo.png`, `<lenguaje>_opcion_A.png`, `<lenguaje>_opcion_B.png`, `<lenguaje>_opcion_C.png`, `<lenguaje>_opcion_D.png`.
- Un render con valores NO canónicos para demostrar parametrización: `<lenguaje>_param_agrupada.png` con matriz [[3,11],[9,6]] y `<lenguaje>_param_apilada.png` con la misma matriz.
- Un archivo `<lenguaje>_reporte.txt` con: similitud por figura (0-100, rúbrica de 6 categorías de `.claude/skills/comparar-similitud-visual/` — Colores 20, Posiciones 20, Valores 20, Proporciones 15, Estilos 15, Elementos 10), iteraciones usadas, desviaciones residuales listadas una a una, ventajas/desventajas del lenguaje.

## Bucle

Itera automáticamente (máx. 10 iteraciones) hasta ≥ 98 en CADA figura. Compara mirando las dos imágenes (Read del original y del render). La cifra de VALORES debe ser 20/20 siempre: un valor de barra mal es fallo, no desviación. Prohibido pedir aprobación intermedia.

## Estado de la implementación en el `.Rmd` (actualizado 2026-10-04)

- Lenguaje elegido: **TikZ** (WAIT_USER #2, 2026-10-02; 94 % de similitud, decisión firmada).
- Funciones: `tikz_barras_simples()` y `tikz_barras_opcion()` sobre `cuerpo_tikz()`, lienzo fijo de
  15,24 × 10,67 cm; ejes compartidos por formato (`ymax_por_fmt`).
- **Nombres de archivo** (regla #4 v6.1, Error 39): `grafica_enunciado_<fig_id>.png`,
  `diagrama_<letra>_<fig_id>.png` y `grafica_solucion_<fig_id>.png`, con `fig_id` hexadecimal de
  8 cifras sorteado al final de `data_generation`, el mismo en las seis figuras de la versión.
  Sin el sufijo, un examen de varias preguntas mostraba en todas las figuras de una sola versión.
- Verificación de que el dibujo representa los datos: `verificar_dibujo_clave.R` (lee el TikZ
  emitido, N = 100) y `tests/testthat/test_barras_campeonato_clave.R`.
