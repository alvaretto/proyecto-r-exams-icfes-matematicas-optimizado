# Spec del gráfico — `diagrama-venn-juegos-mesa-n3` (Flujo B, paso 2b)

Rutas relativas a `A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/`.

## Qué es la figura

Cada opción de un SCHOICE es un diagrama de Venn de tres conjuntos, en su propio PNG. Hay que reproducir la figura de
`Originales/orig_opcion_{A,B,C,D}.png` como función dinámica: recibe siete enteros y tres rótulos, y devuelve el PNG. Las cuatro
opciones de una versión salen de la misma función; solo cambian los números. Nada fuera de los números puede distinguir una
opción de otra (invariante I-1 de `.claude/CLAUDE.md`, léela): trazo, grosor, tamaño de letra y color son siempre los mismos.

Referencias: primaria, los cuatro recortes. Secundaria, de trazo y color, `salida/flujo_b/ref_cuadernillo_{A,B,C,D}.png`, que es el
cuadernillo oficial vectorial. **Prohibido** usar las figuras `q014-*.png` de Todo-Pajaro: están regeneradas con IA.

Del recorte **no** se imitan el ruido, el desenfoque ni el tono amarillo del escaneo. Tu figura lleva los cuatro lados del marco.

Nota del 2026-11-06: esta geometría se midió con las primeras cajas, que en A y C dejaban fuera la línea izquierda del marco. Los
recortes actuales la incluyen y conservan todo el contenido de los anteriores, así que las cifras de `geom_venn.json` no cambian.

**Cambio del 2026-11-06, decisión del profesor en el WAIT_USER #2:** se usa TikZ, con los números alineados a la izquierda como en la plana. Sustituye
el centrado de la primera versión de este spec. `x_izq` e `y` se midieron región por región sobre los cuatro recortes rehechos, con valores de una y
dos cifras y con cada recorte referido al centro de sus tres círculos, porque los marcos del impreso no miden lo mismo (A y C, 232; B y D, 229,5). Una
y dos cifras comparten borde a 0,75 unidades o menos; el desplazamiento respecto a las anclas centradas va de 6,6 a 8,2 unidades según la región.
Solo `tikz/` sigue vigente. `python/` y `r/` quedan como registro de la comparación, medidos con la vara anterior.

**Cambio del 2026-11-06, FASE 2C r1:** el marco pasa de 2,4 a 1,7 unidades. El 2,4 se fijó a ojo: AgenteValidadorVisual midió 1,51 en el recorte y unas 1,7 en el
cuadernillo oficial, que es la referencia de trazo. `geom_venn.json`, `tikz/venn_tikz.R` y el `.Rmd` llevan 1,7; `python/` y `r/` siguen siendo registro.

## Geometría (fija, no la reinterpretes)

`salida/flujo_b/geom_venn.json` tiene todas las cifras, medidas por ajuste de mínimos cuadrados sobre los recortes. Unidades: 1 unidad =
1 px del recorte, origen en el eje del trazo de la esquina superior izquierda del marco, y hacia abajo.

- Marco de 231 × 239 unidades, negro, trazo 1,7. Lienzo con 3 unidades de margen blanco por lado.
- PNG de 711 × 735 px (3 px por unidad), fondo blanco opaco, con antialias.
- Tres círculos de radio 67,8, trazo 2,2, sin relleno, con los colores del JSON. Azul verdoso arriba a la izquierda, granate arriba a la
  derecha, verde abajo al centro.
- Siete números negros en negrita, **alineados a la izquierda, como en la plana**: la pluma va en `x_izq - lsb_numero`, de modo que el borde izquierdo de la tinta caiga en `x_izq` y el centro
  vertical de la tinta en `y`. Una y dos cifras comparten `x_izq`, y el número de dos cifras crece hacia la derecha. Alto de dígito 14,5 unidades.
- Tres rótulos negros en negrita, fuera de los círculos, con `base` como línea de base: r1 ajustado a la izquierda en x = 7,5; r2 ajustado
  a la derecha en x = 221; r3 ajustado a la izquierda en x = 163,5.
- Nada más. Sin título, sin letra A-D, sin totales, sin números fuera de los círculos, sin leyenda.

Orden de las regiones, el mismo en todo el proyecto: `s1` (solo primero), `p12` (primero y segundo), `s2` (solo segundo), `ctr` (centro),
`p13` (primero y tercero), `p23` (segundo y tercero), `s3` (solo tercero).

Fuente: Nimbus Sans Narrow Bold o la más parecida del lenguaje. El impreso usa una sans condensada en negrita. Si cambias de fuente,
ajusta el tamaño para que el dígito mida 14,5 unidades de alto y `88` quepa con holgura en el centro. Si la fuente puede faltar en otra
máquina, cae a una sans negrita sin romper el render.

## Contrato de código (igual en los tres lenguajes)

Archivo `salida/flujo_b/<lenguaje>/venn_<lenguaje>.R`, que define en R:

```r
GEOM_VENN <- list(...)   # todas las cifras del JSON, escritas como literales: el .Rmd final es autocontenido y no lee ese archivo
dibujar_venn <- function(regiones, rotulos, archivo, geom = GEOM_VENN) { ... }
```

- `regiones`: 7 enteros, de 0 a 99, en el orden de arriba. `rotulos`: 3 cadenas UTF-8. `archivo`: ruta del PNG; puede ser relativa.
- Sin parámetros de estilo. Todo sale de `geom`.
- Valida con `stopifnot` el largo, el rango y que sean enteros.
- **No toca el generador aleatorio.** Sin `sample`, `set.seed` ni nada que cambie `.Random.seed`; compruébalo con `identical()` antes y
  después. Las validaciones multisemilla dependen de eso.
- Las coordenadas salen de `geom`. Un verificador externo va a extraer `GEOM_VENN` y a comprobar que toda la caja de tinta de cada número cae dentro
  de los círculos de su región y fuera de los demás, así que ninguna coordenada puede quedar escrita a mano aparte.
- Escribe el número como entero: `17`, nunca `17.0` ni `17,0`.
- La `é`, la `ó` y demás tildes de los rótulos tienen que salir bien.

Archivo `salida/flujo_b/<lenguaje>/generar_<lenguaje>.R`: carga `venn_<lenguaje>.R` y produce, en esa misma carpeta, `out_A.png` … `out_D.png`,
`out_ext.png` y `out_ext2.png`, e imprime los segundos por figura medidos sobre 8 figuras o más.

## Casos de prueba

| Salida | Regiones | Rótulos |
|---|---|---|
| `out_A.png` | 0, 10, 2, 3, 4, 5, 1 | Parqués, Monopoly, Bingo |
| `out_B.png` | 17, 3, 20, 7, 0, 1, 13 | Parqués, Monopoly, Bingo |
| `out_C.png` | 7, 3, 9, 7, 0, 1, 5 | Parqués, Monopoly, Bingo |
| `out_D.png` | 17, 10, 20, 3, 7, 8, 13 | Parqués, Monopoly, Bingo |
| `out_ext.png` | 88, 88, 88, 88, 88, 88, 88 | Baloncesto, Atletismo, Natación |
| `out_ext2.png` | 0, 11, 7, 19, 4, 11, 1 | Ajedrez, Dominó, Cartas |

`out_ext` es el caso de la invariante I-5: dos cifras a la vez en el centro y en las tres zonas dobles, con los rótulos más largos admitidos.

## Medición

`salida/flujo_b/comparar_venn.py` puntúa contra la geometría y es la vara común de los tres lenguajes. Está congelado: **no lo edites**, y
si crees que se equivoca, dilo en tu reporte. Córrelo con `python3`, sin `-I` (necesita scipy del sitio de usuario). `--json` guarda el detalle.

```bash
python3 salida/flujo_b/comparar_venn.py salida/flujo_b/<lenguaje>/out_A.png --regiones 0,10,2,3,4,5,1 --json salida/flujo_b/<lenguaje>/med_A.json
python3 salida/flujo_b/comparar_venn.py salida/flujo_b/<lenguaje>/out_B.png --regiones 17,3,20,7,0,1,13 --json salida/flujo_b/<lenguaje>/med_B.json
python3 salida/flujo_b/comparar_venn.py salida/flujo_b/<lenguaje>/out_C.png --regiones 7,3,9,7,0,1,5 --json salida/flujo_b/<lenguaje>/med_C.json
python3 salida/flujo_b/comparar_venn.py salida/flujo_b/<lenguaje>/out_D.png --regiones 17,10,20,3,7,8,13 --json salida/flujo_b/<lenguaje>/med_D.json
python3 salida/flujo_b/comparar_venn.py salida/flujo_b/<lenguaje>/out_ext.png --regiones 88,88,88,88,88,88,88 --sin-ocr --json salida/flujo_b/<lenguaje>/med_ext.json
python3 salida/flujo_b/comparar_venn.py salida/flujo_b/<lenguaje>/out_ext2.png --regiones 0,11,7,19,4,11,1 --sin-ocr --json salida/flujo_b/<lenguaje>/med_ext2.json
```

Exit 0 = puntaje ≥ 98 y ningún chequeo crítico en rojo. Se exige exit 0 en los seis casos. Además mira tus PNG con `Read` y compáralos con el
recorte: el comparador no ve todo. Itera automáticamente, sin pedir aprobación, hasta un máximo de 10 iteraciones. Si no llegas, entrega lo mejor
y di qué falta.
