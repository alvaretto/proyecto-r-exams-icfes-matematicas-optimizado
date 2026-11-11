# Firma §P7-E pendiente — `vueltas-llanta-perimetro-n4` (MAT-2026-07-028)

**Estado:** **firmada** por el profesor el 2026-11-10 en la pausa 3 de la ronda 4: «Aprobar y firmar». Registro al final, en «Firma del profesor».
**Vale para:** `.Rmd` con md5 `85773027d040e2ce4b45eda03afa362c`. Si el `.Rmd` cambia, esta tabla queda sin efecto y hay que medir de nuevo.
**Líneas citadas:**
- `salida/verificar_vueltas.R`, md5 `6e3d38fc4089f4039c659c599cf26b74`.
- `.claude/scripts/bateria_referencia_icfes.R`, md5 `c099a9e2ce1de2eb51eb7e1eaa1262e3` (batería congelada).
- `salida/detractor_r3/reglas_r3.R`, md5 `36958f8960208b113fa6167684099fc3` (reglas de conjunto de la FASE 2C r3, H2).

**Alcance cableado:** `salida/alcance_r4.R`, md5 `8678860de2afd072f35675f9e96ae30c`, con 36 reglas. Veredicto del verificador sobre este md5: `ROJO_CUBIERTO`, 0 fallos y 0 rojos sin alcance (`salida/paso9_verificador.txt`).

## Qué se le muestra al firmar

- **Tasa:** probabilidad de acertar con el atajo, eligiendo al azar entre las opciones que deja. Sin atajo, 25 %.
- **Tasas exactas:** salen de enumerar las 1248 combinaciones de las versiones nuevas (objeto × distancia × letra × celda de la tabla de pesos), cada una con su probabilidad, y la canónica en sus 24 órdenes. No dependen de ninguna muestra. Dos tandas de 300 versiones reales distintas caen dentro de 1,62 errores típicos de la cifra exacta, y las 600 versiones nuevas distintas de esas tandas están en la enumeración con sus cuatro opciones (la salida del verificador las cuenta dos veces: 802 y 800).
- **Exceso:** diferencia con el techo que alcanza el azar con 100 versiones (regla #23), por familia de reglas. Se toma el menor techo de dos tandas de semillas, que es el más exigente.

| Familia | Qué agrupa | Techo en las nuevas | Techo en la canónica |
|---|---|---|---|
| B | Batería congelada, con la anchura medida en la hoja y no en el código | 31,9-32,3 % | 39,2 % |
| V1 | Vector del detractor r1 | 27,1-27,9 % | 30,1 % |
| V2 | «Contiene ÷» y marcas únicas | 28,4-29,6 % | 31,5 % |
| V3 | Reglas de conjunto del detractor r3 | 29,0-29,5 % | 32,1 % |

- **Se firma** lo que pase de +8 pp. Firmar no lo vuelve verde: el verificador lo sigue mostrando como rojo cubierto (exit 3), nunca como PASS.
- **La cifra de «la más ancha» que se le dio al decidir era de muestra.** Se le dijo «67 de cada 100 versiones nuevas». La exacta es **60,0 de cada 100**: el 67,5 % salía de una muestra de 100 versiones.

## Versiones nuevas (el 90 % de las versiones)

Hay tres formas de escribir la clave, y su peso es fijo. La forma de **fracción única**, (100 × 150)/P, es el 70 % de las versiones nuevas. La de **fracción a la izquierda**, 100/P × 150, es el 15 %, y la de **número a la izquierda**, 150 × 100/P, el otro 15 %.

### Todas juntas

| Regla (familia) | Línea | Atajo, en palabras de aula | Tasa exacta | Exceso |
|---|---|---|---|---|
| `la mas ancha (TeX)` (B) | verificador 138 (reemplaza a batería 47) | Elegir la expresión que ocupa más ancho en la hoja | 60,0 % | +27,9 pp |
| `ancho TeX (tol 1 pt)` (V3) | reglas_r3 22 | La misma, contando como iguales los anchos que difieren en menos de 1 punto (un tercio de milímetro) | 60,0 % | +30,6 pp |
| `letra abajo (conjunto)` (V3) | reglas_r3 25 | Elegir una expresión con la letra del perímetro (P o p) sola debajo de una raya de fracción | 56,5 % | +27,0 pp |
| `contiene x (conjunto)` (V3) | reglas_r3 28 | Elegir una expresión con el signo × | 42,3 % | +12,9 pp |

### Fracción única (70 % de las nuevas)

| Regla (familia) | Línea | Atajo, en palabras de aula | Tasa exacta | Exceso |
|---|---|---|---|---|
| `la mas ancha (TeX)` (B) | verificador 138 | Elegir la expresión que ocupa más ancho en la hoja | 75,0 % | +42,7 pp |
| `ancho TeX (tol 1 pt)` (V3) | reglas_r3 22 | La misma, con la tolerancia de 1 punto | 75,0 % | +45,5 pp |
| `letra abajo (conjunto)` (V3) | reglas_r3 25 | Elegir una expresión con la letra del perímetro (P o p) sola debajo de una raya de fracción | 67,3 % | +37,7 pp |

### Fracción a la izquierda (15 % de las nuevas)

| Regla (familia) | Línea | Atajo, en palabras de aula | Tasa exacta | Exceso |
|---|---|---|---|---|
| `contiene x (conjunto)` (V3) | reglas_r3 28 | Elegir una expresión con el signo × | 66,7 % | +37,6 pp |
| `marca: unica con producto` (V2) | verificador 153 y 165 | Elegir la única expresión con el signo × | 62,5 % | +32,9 pp |
| `centralidad` (V1) | verificador 160 | Elegir la expresión de la que las otras parecen variaciones | 50,0 % | +22,9 pp |
| `medoide` (V1) | verificador 161 | Elegir la expresión que menos cambios necesita para volverse cualquiera de las otras | 50,0 % | +22,9 pp |
| `un operador` (V3) | reglas_r3 31 | Elegir una expresión con un solo signo de operación (× o ÷) | 41,7 % | +12,6 pp |
| `estimulo: contiene la relacion K/V y otra operacion` (B) | verificador 134 | Buscar la expresión que contiene «la constante del enunciado (100 o 1000) dividida entre el perímetro» y además hace otra cuenta (la regla que el profesor aceptó tras la ronda 1) | 41,7 % | +9,8 pp |
| `tres_datos` (V1) | verificador 162 | Descartar las expresiones a las que les falta la constante (100 o 1000), el perímetro o la distancia | 37,5 % | +10,4 pp |

### Número a la izquierda (15 % de las nuevas)

| Regla (familia) | Línea | Atajo, en palabras de aula | Tasa exacta | Exceso |
|---|---|---|---|---|
| `empieza por la distancia` (V3) | reglas_r3 35 | Elegir una expresión que empieza por la distancia del recorrido | 75,0 % | +45,8 pp |
| `contiene x (conjunto)` (V3) | reglas_r3 28 | Elegir una expresión con el signo × | 66,7 % | +37,5 pp |
| `multiplica por una fraccion` (V3) | reglas_r3 37 | Elegir una expresión en la que algo se multiplica por una fracción | 66,7 % | +37,5 pp |
| `marca: unica con producto` (V2) | verificador 153 y 165 | Elegir la única expresión con el signo × | 62,5 % | +33,4 pp |
| `marca: unica sin empieza_fraccion` (V2) | verificador 154 y 165 | Elegir la única expresión que no empieza con una fracción | 62,5 % | +33,4 pp |
| `marca: unica con empieza_numero` (V2) | verificador 158 y 165 | Elegir la única expresión que empieza con un número | 62,5 % | +33,4 pp |
| `centralidad` (V1) | verificador 160 | Elegir la expresión de la que las otras parecen variaciones | 50,0 % | +22,9 pp |
| `medoide` (V1) | verificador 161 | Elegir la expresión que menos cambios necesita para volverse cualquiera de las otras | 50,0 % | +22,9 pp |
| `un operador` (V3) | reglas_r3 31 | Elegir una expresión con un solo signo de operación | 41,7 % | +12,5 pp |
| `estimulo: contiene la relacion K/V y otra operacion` (B) | verificador 134 | Buscar la expresión que contiene «la constante del enunciado (100 o 1000) dividida entre el perímetro» y además hace otra cuenta | 41,7 % | +9,5 pp |
| `tres_datos` (V1) | verificador 162 | Descartar las expresiones a las que les falta la constante (100 o 1000), el perímetro o la distancia | 37,5 % | +10,4 pp |

## Canónica (10 % de las versiones): el impreso, que se reproduce tal cual (regla #24 H-2)

| Regla (familia) | Línea | Atajo, en palabras de aula | Tasa exacta | Exceso |
|---|---|---|---|---|
| `marca: unica sin termina_fraccion` (V2) | verificador 154 y 165 | Elegir la única expresión que termina en un número y no en una fracción | 100,0 % | +68,5 pp |
| `estimulo: contiene la relacion K/V y otra operacion` (B) | verificador 134 | Buscar la expresión que contiene «la constante del enunciado (100 o 1000) dividida entre el perímetro» y además hace otra cuenta | 100,0 % | +60,8 pp |
| `tres_datos` (V1) | verificador 162 | Descartar las expresiones a las que les falta la constante (100 o 1000), el perímetro o la distancia | 50,0 % | +19,9 pp |
| `ancho TeX (tol 1 pt)` (V3) | reglas_r3 22 | Elegir la más ancha, con la tolerancia de 1 punto | 50,0 % | +17,9 pp |
| `letra abajo (conjunto)` (V3) | reglas_r3 25 | Elegir una expresión con la letra del perímetro (P o p) sola debajo de una raya de fracción | 50,0 % | +17,9 pp |
| `contiene x (conjunto)` (V3) | reglas_r3 28 | Elegir una expresión con el signo × | 50,0 % | +17,9 pp |
| `un operador` (V3) | reglas_r3 31 | Elegir una expresión con un solo signo de operación | 50,0 % | +17,9 pp |
| `contiene K/V del enunciado` (V3) | reglas_r3 33 | Elegir una expresión que contiene la relación del enunciado (100/P en el impreso) | 50,0 % | +17,9 pp |
| `la mas ancha (TeX)` (B) | verificador 138 | Elegir la expresión que ocupa más ancho en la hoja | 50,0 % | +10,8 pp |
| `la que MAS comparte` (B) | batería 84 | Elegir la expresión que tiene en común con las otras más clases de signos (raya de fracción, × y ÷) | 50,0 % | +10,8 pp |
| `ultimo numero mayor` (B) | batería 114 | Elegir la expresión cuyo último número es el más grande | 50,0 % | +10,8 pp |

## Márgenes (§P7-B)

- **La más ancha:** en el 52,5 % de las versiones nuevas la clave es la única más ancha, con un margen mediano del 27 %. En todas esas versiones el margen es de 15 % o más, así que el atajo es explotable. En la fracción única pasa en el 75,0 % de sus versiones, con el mismo margen.
- **Centralidad:** el margen mediano de su índice es de 3 a 9 %, pero §P7-B exime lo imperceptible, no lo contable. La convergencia (cada distractor es la clave con un solo cambio) se puede contar, y el medoide, que mide lo mismo, tiene margen de 15 % o más en el 50 % de las versiones de las formas a la izquierda. Es explotable (FASE 2C r4, H1b).
- **Medoide:** el margen mediano es de 18 a 33 %.
- **Reglas de conjunto** (×, la P abajo, empieza por la distancia, la única con…): son marcas que se ven o no se ven. No tienen margen.

## ¿El atajo coincide con razonar bien? (glosa de constructo: FASE 2C r3 H2 y r4 H1c)

| Atajo | ¿Coincide con razonar bien? |
|---|---|
| La letra del perímetro sola debajo de una raya de fracción | En parte: es «a más perímetro, menos vueltas», la propiedad que enuncia la Solution. Con el producto arriba y la letra sola abajo acierta el 87,5 % en la fracción única (sonda propia del detractor r4, fuera de las familias) |
| Contiene ×, la única con ×, multiplica por una fracción | En parte: es «hay que multiplicar por la distancia» |
| Estímulo: la constante entre el perímetro más otra cuenta | Sí: quien modela bien busca 100/P (o 1000/P) y lo multiplica por la distancia |
| Usa los tres datos, un solo operador | En parte: descartar lo que no usa todos los datos es una estrategia legítima |
| La más ancha, empieza por la distancia, las marcas únicas de forma, centralidad, medoide, la que más comparte, último número mayor | No: son rasgos de superficie |

**Artefacto, sin firma (FASE 2C r4, H3c):** «la más larga» contada en caracteres del código acierta el 50,0 % en las dos formas a la izquierda y el 32,9 % en las versiones nuevas. Solo la vería un estudiante si las fórmulas salieran en TeX crudo (celulares sin red, regla #26).

## Lo que se le dice para decidir

- **Por qué hay tantas reglas.** La escritura de la ronda 2 deja la clave como «la fórmula más ancha» en la fracción única, que es el 70 % de las versiones nuevas. En las otras dos formas, cada una del 15 %, en la mitad de las versiones la clave es la única con el signo ×; en la otra mitad también lo llevan otras dos opciones, y la delata la convergencia (centralidad y medoide, 50 %) (FASE 2C r4, H1a). Muchas filas son el mismo rasgo visto por dos sondas: «contiene ×» y «la única con ×»; «la más ancha» con y sin tolerancia.
- **La regla del estímulo** («la constante del enunciado (100 o 1000) dividida entre el perímetro» más otra cuenta) es la que el profesor aceptó tras la ronda 1. Coincide con el razonamiento correcto: quien modela bien busca 100/P y lo multiplica por la distancia.
- **Lo que no cambia:** la clave es verdadera y única en todas las combinaciones del diseño (oráculo algebraico 1249 de 1249) y en 602 versiones reales distintas leídas del texto. Ninguna versión muestra la errata «101/120».
- **H3b exacta:** la misma forma de escribir la clave aparece en el 63,0 % de todas las versiones (70,0 % de las nuevas). La sonda avisa desde el 90 %.

## Firma del profesor (2026-11-10)

> **Firmada por el profesor el 2026-11-10, hacia las 20:10**, en la pausa 3 (WAIT_USER #3) de la ronda 4, en sesión
> directa con el coordinador. Respuesta literal: **«Aprobar y firmar»**. Regla #24 **H-5**.

- **Pregunta que leyó:** «Ejercicio de la llanta (10 de 11 pasos, 0 errores en las respuestas): ¿lo aprueba para llevarlo
  al aula, firmando las 36 pistas de la tabla tal como están? La principal es "elegir la fórmula más ancha", que acierta
  60 de cada 100 versiones nuevas.»
- **Lo que se le mostró antes de preguntar:**
  - cero errores en las respuestas en cuatro revisiones independientes; la última recorrió las 1248 combinaciones;
  - la respuesta correcta escrita como la escribe quien modela el problema, y las opciones separadas en el celular;
  - la corrección de la cifra: «la más ancha» acierta 60 de cada 100 versiones nuevas, no 67 como se le dijo al decidir
    tras la r3 (el 67 salía de una muestra);
  - las cinco pistas principales, con su cifra y si coinciden con razonar bien: la más ancha (60; 75 en la fracción
    única; no), la letra del perímetro sola debajo de la raya (56; en parte), empieza por la distancia (75 en
    `numero_izq`; no), la que tiene × (67 en una forma minoritaria; en parte) y, en la versión del cuadernillo, la única
    que termina en un número (100; no, pero viene del ítem oficial y no se toca);
  - la ruta de esta tabla completa;
  - los dos residuos menores sin atender (H2 y H4 de la FASE 2C r4), que cambian el `.Rmd` y quedan para su próximo cambio;
  - la recomendación del coordinador: aprobar, porque no quedan pasadas, la pista principal es de apariencia y se puede
    observar en clase, y las de la canónica vienen del ítem oficial.
- **Opciones que vio:** «Aprobar y firmar» (se marca el último paso; la firma queda atada a este archivo y deja de valer si
  cambia; puede llevarlo a clase y, si ve que los estudiantes eligen «la más ancha», lo anota como evidencia), «Rechazar y
  pedir cambios» (otra medición y otro revisor; ya no quedan pasadas) y «Pausar» (queda en 10 de 11).
- **Qué firma:** las 36 reglas de las secciones de arriba, con sus cifras exactas, cableadas en `salida/alcance_r4.R` (md5
  `8678860de2afd072f35675f9e96ae30c`). Vale para el `.Rmd` con md5 `85773027d040e2ce4b45eda03afa362c` y el verificador
  `salida/verificar_vueltas.R` con md5 `6e3d38fc4089f4039c659c599cf26b74`, que da exit 3 (`ROJO_CUBIERTO`): firmar no lo
  vuelve PASS. Si cualquiera de los tres cambia, la firma queda sin efecto y hay que medir de nuevo.
- **Condición:** si en el aula se ve que los estudiantes eligen «la más ancha» u otro atajo de superficie de esta tabla, se
  anota como evidencia. Corregirlo cambia el `.Rmd` y exige otra medición, otro detractor y otra firma.
- **Residuos aceptados con la aprobación:** H2 y H4 de la FASE 2C r4, ambos de severidad baja y ninguno de CORRECCIÓN.
- **No es producción:** la promoción a `03-En-Produccion/` espera la evidencia de aula. Antes de promover se recomienda la
  FASE 2C-bis (`/detractor-hetero`).
