# MAT-2026-07-017 — ítem oficial, transcripción verbatim

> **Por qué existe este archivo.** El recorte de `Originales/` es una imagen: no se puede citar con grep. Este archivo es su
> transcripción verbatim, cotejada el 2026-11-13 contra el recorte y contra la ficha de alineación.
>
> **Contrasta contra este archivo y, ante la duda, contra el recorte; nunca contra tu recuerdo.**

## Fuente

- Simulacro: SAI «Nivelación 6» de grado 10 (Los Tres Editores), julio de 2026, Matemáticas, **pregunta impresa 17**, plana
  impresa 6, columna izquierda, abajo.
- Plana: [`Originales/pagina_005.jpg`](Originales/pagina_005.jpg), 1572 × 2171 px, md5 `08b9a8895878634b8cb9f69705c13971`.
  La plana trae las opciones de la pregunta 16 (cuatro gráficas de barras) y las preguntas 17 a 20; aquí solo cuenta la 17.
- Recorte, sin reescalar, revisado a ojo el 2026-11-13:

| Archivo | Caja (izquierda, arriba, ancho, alto) | md5 |
|---|---|---|
| `Originales/orig_pregunta_17.png` | (150, 1385, 650, 430) | `f8b465466a8b4dbf7219d43db409edc6` |

- Ficha de alineación: `/home/bootcamp/Proyectos-2026/Todo-Pajaro/Alineacion-curricular-de-items/Simulacros/Simulacros-Decimo/julio-2026/Matematicas/Alineacion-curricular-de-items-Matematicas-Decimo-Jul-2026.md`,
  sección `### MAT-2026-07-017 — Aproximación con una serie` (líneas 1659 a 1695 el 2026-11-13).

## Enunciado (verbatim de la plana)

> Para analizar el comportamiento de una onda de sonido, Ignacio utiliza la siguiente serie para hacer cálculos a partir de la
> función seno:
>
> sen(x) = x − x³/3! + x⁵/5! − …
>
> Si Ignacio utiliza la serie solamente con los primeros tres términos para aproximar el valor de **sen(1)**, ¿qué valor obtendría?

La fórmula va centrada, con las fracciones en vertical; «sen(x)» y «sen(1)» van en negrita cursiva.

## Opciones (con su letra oficial)

| Letra | Plana SAI | Instancia canónica |
|---|---|---|
| A | 55/120 | `\dfrac{55}{120}` |
| B | 185/120 | `\dfrac{185}{120}` |
| **C** | 101/120 | `\dfrac{101}{120}` |
| D | 141/120 | `\dfrac{141}{120}` |

**Clave: C.** Triangulación de la ficha 3/3 concordante (proveedor SAI, Claude y Gemini sobre la plana). Las fracciones van sin
simplificar, en una cuadrícula de 2 × 2 (A y B arriba, C y D abajo). No hay figura: el ítem es texto, una fórmula y cuatro
fracciones (decisión del profesor del 2026-11-13: sin gráficas).

## Resolución

1 − 1/3! + 1/5! = 1 − 1/6 + 1/120 = 120/120 − 20/120 + 1/120 = 101/120 (aproximadamente 0,8417; sen 1 es aproximadamente 0,8415).

## Mecanismo de cada distractor (de la ficha)

| Opción | Cálculo | Error |
|---|---|---|
| A | 1 − 1/2 − 1/24 = 55/120 | usa 2! y 4! en lugar de 3! y 5!, y resta todos los términos |
| B | 1 + 1/2 + 1/24 = 185/120 | usa 2! y 4! y suma todos los términos |
| D | 1 + 1/6 + 1/120 = 141/120 | suma los tres términos sin respetar el signo menos |

## Clasificación

Valores literales de la ficha. Competencia, Componente, Afirmación, Evidencia, Descriptor y Estándar se cotejaron el 2026-11-13
contra el catálogo canónico (`/home/bootcamp/Proyectos-2026/Todo-Pajaro/Alineacion-curricular-de-items/Matematicas/catalogos-oficiales-mat/`):
coinciden los seis (la Evidencia es `FyE_E2`; el Estándar está en `Variacional_10-11`).

| Campo | Valor |
|---|---|
| Competencia | Formulación y ejecución |
| Componente | Numérico-variacional |
| Afirmación | Frente a un problema que involucre información cuantitativa, plantea e implementa estrategias que lleven a soluciones adecuadas. |
| Evidencia | Ejecuta un plan de solución para un problema que involucra información cuantitativa o esquemática. |
| Descriptor | D3.7 — Selecciona información necesaria para resolver problemas que involucran operaciones aritméticas. |
| Nivel | 3 |
| Grado sugerido | 10°-11° |
| Estándar asociado | Utilizo las técnicas de aproximación en procesos infinitos numéricos. (10°-11°, Pensamiento variacional) |
| Genérico | No |
| Contenido | Álgebra y Cálculo |
| Tema Específico | Aproximación por series |

> **Advertencia para quien edite este archivo.** `validar_literalidad_exextra.py --item` lee como «Campo | Valor» **toda** fila de
> dos columnas del archivo, y si una clave se repite gana la última. Cualquier otra tabla de este archivo debe tener tres columnas o
> más.

### Observaciones declaradas para el profesor (no cambian la clasificación)

| Observación | Detalle | Tratamiento |
|---|---|---|
| Rótulo del editor «A: 2.3» | Decodificado: evidencia 2.3, `FyE_E3` («Resuelve un problema que involucra información cuantitativa o esquemática.»); la ficha asigna `FyE_E2` | Decidido por la regla #27: se mantiene la evidencia de la ficha (2 de 3). Ver § «Triangulación de la clasificación (regla #27)» |
| D3.7 como descriptor comodín | El agente `normalizador-niveles-mat` trata D3.7 como comodín que pide justificación específica. La justificación aquí: el ítem pide seleccionar de la serie los términos que se usan (los primeros n) y operar con fracciones y factoriales | Se usa la ficha; residuo declarado en el paso 10 |
| Nivel 3 con DOK 2 | Solo DOK ≥ 3 obliga a Nivel ≥ 3 (regla #1). El obstáculo que sostiene el Nivel 3: el signo alternado y los factoriales impares (3!, 5!) | La Solution lo nombra; no se infla el DOK |

## Triangulación de la clasificación (regla #27)

Rótulo impreso (literal): «A: 2.3 M / Compt.: Numérico-variacional / Comp.: Formulación y ejecución»

- **Clave de lectura:** «A: X.Y» es la evidencia X.Y de la *Guía de orientación Saber 11.° 2026-2*, Tablas 14 a 16 (págs. 31-33,
  md5 del PDF `de3957834512ac791e9faebc3cf44c6f`). La comprobó el coordinador con `pdftotext`. «2.3» es «Resuelve un problema que
  involucra información cuantitativa o esquemática.» (`FyE_E3`). «M» no se pudo decodificar: no vota.
- **Voto P (propia a ciegas):** `ClasificadorICFES` sobre el recorte sin rótulo (caja 150, 1395, 790, 1778 de `pagina_005.jpg`), solo
  con `evidencias-mat.json`; cerró con «CEGUERA: cumplida». Evidencia `FyE_E2` con confianza media y `FyE_E3` como segunda opción,
  porque el enunciado ya fija el plan (los tres primeros términos) y el estudiante lo ejecuta.

| Dimensión | Impreso (E) | Ficha (F) | Propia a ciegas (P) | Decisión | Razón |
|---|---|---|---|---|---|
| Competencia | Formulación y ejecución | Formulación y ejecución | Formulación y ejecución | Formulación y ejecución | 3 de 3 |
| Componente | Numérico-variacional | Numérico-variacional | Numérico-variacional | Numérico-variacional | 3 de 3 |
| Afirmación | La de la competencia 2 | La de Formulación y ejecución | La de Formulación y ejecución | La de Formulación y ejecución | 3 de 3 |
| Evidencia | 2.3 «Resuelve un problema que involucra información cuantitativa o esquemática.» | 2.2 «Ejecuta un plan de solución para un problema que involucra información cuantitativa o esquemática.» | 2.2 «Ejecuta un plan de solución…» (confianza media; segunda opción, 2.3) | 2.2 «Ejecuta un plan de solución…» | 2 de 3, con la ficha en la mayoría |

Nivel, descriptor y estándar no vienen en el rótulo y siguen la ficha.

## Regla de uso

**La instancia impresa (x = 1, tres términos, 101/120) quedó FUERA del espacio de versiones** por decisión del profesor del
2026-11-13. Con x = ±1 y todas las opciones sobre un denominador común, la clave es siempre la fracción más cercana a sen(x):
el error de la suma parcial es menor que media unidad del denominador. Así, quien calculaba sen(1) con la calculadora acertaba
siempre sin usar la serie. Este archivo conserva la transcripción verbatim como fuente.

Diseño vigente (pasadas de corrección 2 y 3; enumeración exacta en `salida/enumerar_diseno_r2.R`):

- **Datos:** x en {2, 3, −2, −3} y n en {3, 4} términos, lo que da 8 claves distintas. La pregunta es la del cuadernillo y solo cambia el
  número. La serie se muestra con tantos términos como se usan.
- **Opciones:** sin simplificar, sobre el factorial del último término usado, como el impreso.
- **Distractores:** siempre la trampa de la calculadora, SS-C, que es la fracción k/N! más cercana al valor real de sen(x) en
  radianes. Los otros dos salen de un banco de 10 errores, con el número de opciones a cada lado de la clave sorteado entre los repartos factibles (la clave nunca es la extrema del lado de la trampa) y una cascada que prefiere un distractor
  de valor absoluto menor o igual a 1 (política P2).
- **Tope de valor (pasada 4):** ningún distractor del banco pasa de 4 en valor absoluto; es la `precondicion` de cada error. Es el tope
  entero más estricto (el mínimo exacto es 3,525) que deja al menos 3 errores elegibles además de SS-C en las 8 ramas (`salida/enumeracion_tope_r4.txt`). Con x = ±3 y tres
  términos, los tres elegibles quedan del mismo lado de la clave, que es la mayor (x = 3) o la menor (x = −3) en esa rama.
- **Versiones únicas:** 150 de 150 renders (criterio del profesor del 2026-11-13: al menos 100 de 150).

## Decisiones del profesor (literales)

| Fecha | Pausa o pregunta | Respuesta literal | Consecuencia |
|---|---|---|---|
| 2026-11-13 | Pausa 1, Flujo B | «No, sin gráficas» | `flujo_b.requerido = false` |
| 2026-11-13 | D1, atajo de la calculadora | «No aceptarlo» · «Cambiar el 1 por 2 o 3» | x en {2, 3, −2, −3}, distractor SS-C obligatorio, sin la instancia impresa |
| 2026-11-13 | D2, forma de las fracciones | «Otra opción sin simplificar» | Equilibrar la divisibilidad. Con abs(x) ≥ 2 la clave siempre se simplifica. Enumeración exacta (FASE 2C r3, H2), sobre las versiones en que la regla aplica: «la irreducible» −19,8 pp; «la que se simplifica» +8,3 pp; «numerador múltiplo de x» +8,3 pp; «no múltiplo de 5» +12,7 pp (diluidas: +6,2, +6,2 y +4,7). Tras la pasada 4: +8,3, +8,3 y +15,0 pp (diluidas: +6,2, +6,2 y +5,7). Firmado por el profesor el 2026-11-13 (§ «Firma de los atajos») |
| 2026-11-13 | D3, versiones de dos términos | «Quitarlas» | n en {3, 4} |
| 2026-11-13 | Umbral de versiones únicas | Al menos 100 distintas de 150 renders | Sustituye a 250 de 300 en este ejercicio |
| 2026-11-13 | Pendientes menores | «Resuelve los pendientes menores. Commit. Push» | Pasada 4: ningún distractor del banco pasa de 4 en valor absoluto; una igualdad por ecuación en la Solution; decimales en texto. No firma los atajos ni aprueba el ejercicio |
| 2026-11-13 | YAML del Knit de RStudio | «No, dejarlas» | El `.Rmd` conserva el YAML; su md5 con YAML (`b9d489a9…`) es el firmado |
| 2026-11-13 | Firma, tabla anterior a la pasada 4 | «Firmar los atajos de la tabla y aprobar» | Firma de las reglas de la tabla y aprobación |
| 2026-11-13 | Firma, seis cifras nuevas o cambiadas en la pasada 4 | «Firmar también estos seis y aprobar» | Firma completa (§ «Firma de los atajos»); paso 11 sellado |

## Firma de los atajos (§P7-E, 2026-11-13)

Firmados por el profesor el 2026-11-13 para el `.Rmd` de md5 `b9d489a9dd838630875a4958146b9bc8`, en dos pasos: «Firmar los atajos de la tabla y aprobar» y «Firmar también estos seis y aprobar». Cifras de la enumeración exacta (88 configuraciones), contadas sobre las versiones en que la regla aplica, en puntos sobre el 25 % del azar. El alcance está cableado en `salida/verificar_bateria_p7.R`: hoy da ROJO_CUBIERTO (exit 3), y si el md5 cambia da FIRMA_NO_CUBRE (exit 4).

| Atajo (en lenguaje llano) | Regla | Cifra firmada (pp) | Origen |
|---|---|---|---|
| la segunda opción más cercana al valor de la calculadora | `est_2a_cerca_sen` | +75,0 | batería |
| de las dos opciones más parecidas, la más lejana del seno | `rel_par_lejos_sen` | +75,0 | batería |
| de las dos más parecidas, la de numerador múltiplo de x | `rel_par_mult_x` | +62,5 | batería |
| de las dos más parecidas, la que se simplifica | `rel_par_reducible` | +62,5 | batería |
| entre las de valor entre −1 y 1, las que se simplifican | `est_rango_reducible` | +26,2 | batería |
| entre las de valor entre −1 y 1, descartar la más cercana al seno | `est_rango_no_cerca_sen` | +26,1 | batería |
| la más cercana a x − x³/6 | `est_cerca_S2` | +25,0 (margen mediano 1 %: inexplotable) | batería |
| entre −1 y 1, la mayor en valor absoluto | `est_rango_mayor_abs` | +25,0 | batería |
| una de las dos opciones más parecidas | `rel_par_mas_cercano` | +25,0 (margen mediano 90 %) | batería |
| de las dos más parecidas, la mayor en valor absoluto | `rel_par_mayor_abs` | +25,0 | batería |
| de las dos más parecidas, la menor en valor absoluto | `rel_par_menor_abs` | +25,0 | batería |
| la que tiene el signo de x | `est_signo_x` | +17,4 (antes +15,8) | batería |
| la más cercana a x | `est_cerca_x` | +16,7 (antes +15,0; margen mediano 13 %: inexplotable) | batería |
| entre x − x³/6 y x | `est_entre_S2_y_x` | +15,1 | batería |
| numerador que no es múltiplo de 5 | `div_no_mult5` | +15,0 (antes +12,7) | batería |
| entre −1 y 1, la menor en valor absoluto | `est_rango_menor_abs` | +13,3 | batería |
| valor absoluto menor que el de x | `est_abs_menor_x` | +10,8 | batería |
| la que está entre −1 y 1 | `est_rango_seno` | +10,0 | batería |
| la que se simplifica | `div_reducible` | +8,3 | batería |
| descartar la más cercana al seno | `est_no_cerca_sen` | +8,3 | batería |
| numerador múltiplo de x | `div_mult_x` | +8,3 | batería |
| la más cercana a la opción de la calculadora | (fuera de batería) | +75,0 | detractor r4 (`salida/fase2c_r4_detractor.md`, H2) |
| entre las de numerador múltiplo de x, la más cercana al seno | (fuera de batería) | +50,0 | detractor r4 (`salida/fase2c_r4_detractor.md`, H2) |
| si x es positivo la mayor, si es negativo la menor | (fuera de batería) | +8,3; acierto del 100 % en la rama x = ±3 con tres términos | detractor r4 (`salida/fase2c_r4_detractor.md`, H2) |

## Residuo declarado

| Residuo | Cifra | Origen |
|---|---|---|
| La clave es la opción extrema (la mayor con x = 3, la menor con x = −3) | 100 % de la rama x = ±3 con tres términos; 50 % con cuatro; rango agregado 27,1 / 22,9 / 22,9 / 27,1 % | Tope de valor de la pasada 4 |
| Ecuaciones de la Solution que se desplazan dentro de su caja en Moodle a 320 px (contenido de 270 px) | 16 ecuaciones en 14 de 100 versiones, hasta 13 px; la página no desborda | `salida/p6b_desborde_moodle_270_310.txt` |
| Claves distintas | 8 (`WARN_DIV_BAJA`) | Decisiones D1 y D3 |
