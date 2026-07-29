# Backlog — Coordenadas de vértices en el plano cartesiano (barco)

> Pendientes priorizados. `P0` bloquea la promoción a `02-En-Desarrollo/`; `P1` es deuda que hay
> que resolver antes de escalar el patrón; `P2` es diferible.
> Ver [`ROADMAP.md`](ROADMAP.md) para los gates de promoción.

---

## P0 — Bloqueante para promoción

### P0.1 — `GEO-COORD-03` era eliminable por su forma — ✅ **RESUELTO 2026-07-28**

**Origen:** auditoría adversarial (2026-07-28). **Verificado de forma independiente por enumeración
exhaustiva sobre las 222 versiones** (el espacio vigente en ese momento; tras retirar las
exclusiones de `y_pool` en P2.7 el espacio creció a 374 y la estructura 2×2 se re-confirmó sobre el
espacio ampliado — ver la tabla de re-verificación en P2.7, más abajo).

Los cuatro vértices de cualquier rectángulo alineado a los ejes tienen una estructura fija: **2
valores de x × 2 valores de y, en las 4 combinaciones** — la forma `(A,C); (A,D); (B,C); (B,D)`.

| Opción | Estructura 2×2 | Medición |
|---|---|---|
| Correcta | Sí | **222/222** |
| `GEO-COORD-01` (inversión de ejes) | Sí | **222/222** |
| `GEO-COORD-02` (rango reducido) | Sí | **222/222** |
| `GEO-COORD-03` (diagonal) | **No** | **0/222** |
| `GEO-COORD-03`: los 4 puntos con forma `(v,v)` | — | **222/222** |

`GEO-COORD-03` es el **único** distractor cuyos cuatro puntos son de la forma `(v, v)`, es decir
colineales sobre la recta `y = x`. Cuatro puntos colineales **nunca** pueden ser los vértices de una
figura bidimensional.

**Riesgo concreto:** un estudiante que reconozca ese hecho descarta la opción **por la forma del
texto**, sin mirar la figura ni leer una sola coordenada del barco. Eso sube su probabilidad de
acierto por azar de 25 % a 33 %. Además, el error que dice diagnosticar («confunde la forma del
barco con una distribución diagonal») es poco plausible como error real de lectura de un plano.

Es el análogo, en estructura de texto, del patrón **P5** de la regla #22 (distractor *outlier*
eliminable de un vistazo) y del «Formato Equilibrado» de `graficos-como-opciones.md`.

**Por qué es P0 y no P1:** degrada la validez psicométrica del ítem en el **100 %** de las
versiones, no en un subconjunto. Un distractor que se descarta sin usar el conocimiento evaluado no
cumple su función.

#### Resolución aplicada

`GEO-COORD-03` (diagonal) se **retiró** y se sustituyó por **`GEO-COORD-04` — «Desplazamiento de
una unidad al contar la cuadrícula»**:

```r
desplaz <- if (x_max < grid_max) 1L else -1L
dist_desplaz <- paste0(
  "(", x_min + desplaz, ", ", y_min, "); (",
  x_min + desplaz, ", ", y_max, "); (",
  x_max + desplaz, ", ", y_min, ") y (",
  x_max + desplaz, ", ", y_max, ")."
)
```

Diagnostica un error de lectura frecuente y real: contar los **cuadros** de la cuadrícula en vez de
las **marcas** del eje, o empezar a contar desde el primer cuadro que ocupa el barco en lugar de
desde la línea donde empieza.

**La dirección del desplazamiento es adaptativa** (`+1` si hay margen a la derecha, `−1` si el barco
ya toca el borde). Eso evita el problema que tenía la propuesta original —salirse de la grilla con
`x_max + 1 > 10`— **sin recortar ni una sola versión**: cuando `x_max = 10` se cumple
`x_min = 10 − ancho ≥ 4`, así que el desplazamiento a la izquierda siempre cabe. El chunk lo
verifica con `stopifnot(x_min + desplaz >= 1L, x_max + desplaz <= grid_max)`.

#### Criterio de cierre — verificado

Enumeración exhaustiva sobre las 222 versiones, tras el cambio:

| Comprobación | Antes | Después |
|---|---|---|
| Correcta con estructura 2×2 | 222/222 | 222/222 |
| `GEO-COORD-01` con estructura 2×2 | 222/222 | 222/222 |
| `GEO-COORD-02` con estructura 2×2 | 222/222 | 222/222 |
| **Tercer distractor con estructura 2×2** | **0/222** | **222/222** ✅ |
| Las 4 opciones distintas entre sí | 222/222 | 222/222 |
| Desplazamiento fuera de la grilla | — | **0** |
| Espacio de versiones | 222 | **222** (sin pérdida) |
| Respuestas correctas distintas | 222 | **222** |
| *Bounding box* = clave (invariante I-2) | 222/222 | 222/222 |

Ejemplo real del XML de Moodle renderizado (correcta `x ∈ [5,9]`, `y ∈ [1,2]`):

```
correcta      (5, 1); (5, 2); (9, 1) y (9, 2).
GEO-COORD-04  (6, 1); (6, 2); (10, 1) y (10, 2).     <- misma forma que la correcta
GEO-COORD-02  (7, 1); (7, 2); (8, 1) y (8, 2).
GEO-COORD-01  (1, 5); (2, 5); (1, 9) y (2, 9).
```

Ninguna opción se distingue ya por su forma: hay que leer las coordenadas del barco.

**Re-validación completa (sobre el espacio de 222 vigente entonces):**
`validar_coherencia_matematica.R` → APROBADO 0 errores · `validar_diversidad_sustantiva.R --n 40` →
PASS, 36 valores únicos · `verificar_render.R` → 5/5 formatos OK, sin fuga P6. **Re-confirmado tras
P2.7** sobre el espacio ampliado a 374 — ver la tabla de re-verificación en P2.7, más abajo.

---

### Resto de la batería de verificación: sin defectos de corrección

Ninguna de estas comprobaciones encontró problemas:

| Verificación | Resultado |
|---|---|
| `validar_coherencia_matematica.R` (Niveles 1-5 + Capas semánticas A-D) | APROBADO, 0 errores |
| `validar_diversidad_sustantiva.R --n 40` | PASS, 36 valores únicos |
| Enumeración exhaustiva del espacio de versiones | 222/222 combinaciones con clave correcta, 0 colisiones, 0 `y_pool` vacíos |
| `verificar_render.R` (5 formatos) | 5/5 OK |
| Reglas #18 / #19 / #20 / #21 / #22 §P6 | OK |
| Incidentes I (reseed por reloj) y L (ecuación sin indentar) del orquestador | No aplican: 0 coincidencias |

> Medición hecha sobre el espacio de 222 vigente en el momento (antes de P2.7). Tras retirar las
> exclusiones de `y_pool` el espacio creció a 374 y se re-ejecutó la batería completa — ver P2.7.

---

## P1 — Deuda de desarrollo

### P1.1 — El casco no se lee como barco en el 27 % de las versiones — ✅ **RESUELTO 2026-07-28** (opción D + opción A′)

> **Estado final:** se atacaron **los dos** mecanismos. La **opción D** (radio de las bandas acotado
> por el ancho) resolvió el mecanismo dominante —la mancha negra fusionada—. La **opción A′**
> (`ratio ≥ 2`, decisión del usuario) resolvió el residual del contorno eliminando del sorteo las 56
> versiones de `ratio 1.5`, las únicas que la inspección visual califica de inaceptables. Espacio de
> versiones **374 → 318** (−15,0 %). Ver «Resolución del residual — opción A′» al final del ítem.

> **Re-medición sobre el espacio ampliado (2026-07-28, sesión posterior).** La distribución de
> ratios se volvió a medir por enumeración exhaustiva sobre las **374** combinaciones vigentes tras
> P2.7. El resultado y los costes actualizados de las opciones están en «Re-medición post-P2.7» al
> final del ítem. **La proporción de versiones degeneradas prácticamente no cambió** (27,0 % → 27,8 %),
> pero los costes absolutos de la opción A sí, y la tabla de opciones los citaba desactualizados.

**Medición (2026-07-28).** La forma del casco depende de la relación de aspecto
`ratio = ancho_barco / alto_barco`. Distribución sobre las **222** combinaciones válidas vigentes en
el momento de esta medición (antes de P2.7):

| ratio | Combinaciones | % | Lectura visual |
|---|---|---|---|
| 1.5 | 32 | 14.4 % | **Cápsula redondeada — no se lee como barco** |
| 2.0 | 28 | 12.6 % | **Degradado** |
| 2.5 | 24 | 10.8 % | Aceptable |
| 3.0 | 57 | 25.7 % | Correcto |
| 4.0 | 32 | 14.4 % | Correcto |
| 5.0 | 27 | 12.2 % | Correcto |
| 6.0 | 22 | 9.9 % | Correcto (el mejor) |

**60 de 222 versiones (27,0 %) tienen `ratio ≤ 2`.** Inspección visual directa del caso
`ancho = 3, alto = 2` (`x ∈ [3,6]`, `y ∈ [5,7]`): la figura es una cápsula de esquinas redondeadas;
además las dos bandas oscuras decorativas y el puente se fusionan en una sola mancha.

**Causa raíz — medida analíticamente, no supuesta.** Son **dos** mecanismos independientes:

1. **El contorno.** La proa y la popa ocupan una fracción **fija** del 15 % de la longitud
   (`prof(t)`, `t < 0.15` y `t > 0.85`). Con `w/h` grande, eso da un afinado suave y un tramo
   central largo — un barco. Con `w/h` pequeño, la proa debe subir `h/2` en sólo `0.15·w` de
   recorrido horizontal: el afinado se vuelve casi vertical y la silueta degenera en cápsula.
2. **Los adornos (el mecanismo dominante).** Las dos bandas oscuras (`b1_df`, `b2_df`) tienen
   **radio proporcional a `h`** (`h·0.46` y `h·0.40`) pero sus centros están separados por una
   **fracción de `w`** (`w·0.03` y `w·0.16`). Cuando `h` es máximo y `w` mínimo, los radios crecen
   mientras la separación se encoge, y las dos bandas se solapan hasta fundirse en una sola mancha:

   | ancho | alto | Solape entre banda 1 y banda 2 |
   |---|---|---|
   | 3 | 2 | **72,3 %** — fusionadas |
   | 4 | 2 | **65,2 %** — fusionadas |
   | 5 | 2 | 58,2 % |
   | 6 | 2 | 51,1 % |
   | 3-6 | 1 | 51,1 % → 8,7 % |

**Corrección de un diagnóstico erróneo.** Dos auditorías independientes reportaron que el
rectángulo del puente «se sale del contorno del casco» con `ancho = 3, alto = 2`. **Eso es falso**,
y se puede demostrar sin renderizar: el borde derecho del puente cae siempre en

```
t = (cx + w·0.32 + w·0.05 − x_min) / w = 0.5 + 0.37 = 0.87
```

es decir, en una posición **invariante de escala**, independiente de `ancho` y `alto`. Ahí la
semialtura del casco vale `prof(0.87)·h = 0.4655·h` y la semialtura del puente vale `0.25·h`, de
modo que el puente ocupa **el 53,7 % del espacio disponible en las 8 combinaciones**, sin
excepción. Nunca sale del casco.

Lo que ambas auditorías **vieron** correctamente es la mancha negra fusionada; lo que atribuyeron
mal es su causa. El síntoma es real, el mecanismo es el solape de las bandas, no el puente. Anotado
aquí para que un fix futuro toque el parámetro correcto.

**Impacto.** **No afecta la corrección del ítem**: el *bounding box* sigue siendo exactamente
`[x_min,x_max] × [y_min,y_max]` en las 222 versiones (verificado), así que la clave es válida
también en las 60 versiones degeneradas. El daño es de **fidelidad narrativa**: el enunciado habla
de un juego de barcos y en algo más de una de cada cuatro versiones el estudiante ve una cápsula.

**Intento de fix descartado (documentado para que nadie lo repita).** Se probó hacer la fracción de
proa adaptativa al aspecto: `fp <- max(0.15, min(0.35, h/w))`. Resultado medido:

- Preservó la invariante I-2 (222/222 combinaciones con *bounding box* correcto).
- Preservó las 222 versiones (no recorta el espacio de parámetros).
- **Pero no resolvió el caso 1.5**: la cápsula se convirtió en una almendra simétrica, que tampoco
  se lee como barco — con una huella casi cuadrada ningún ajuste de afinado lo consigue.
- **Y produjo una regresión en los ratios medios**: a `ratio` 2.5 y 3.0 (81 combinaciones, 36,5 %
  del total) el `fp` saltaba de 0.15 a ~0.33-0.35, convirtiendo un casco correcto en una almendra.

**Se revirtió.** El `.Rmd` conserva la geometría original; sólo se añadió el comentario de la
invariante I-2 sobre `prof()`.

**Opciones para resolverlo (decisión del usuario, hay un trade-off real):**

| Opción | Qué hace | Coste |
|---|---|---|
| **A** | Restringir el sorteo a `ratio ≥ 2.5` (`alto_barco = 2` sólo con `ancho_barco ≥ 5`) | *(coste re-medido sobre 374 — ver «Re-medición post-P2.7»)* Espacio de versiones 374 → **270 preguntas distintas** (−27,8 %). Sigue muy por encima de lo que exigen los validadores: `validar_diversidad` cuenta renders únicos (270 × 8 protagonistas × 4 reflexiones × 24 órdenes ≈ 207 000) |
| **B** | Rediseñar el perfil del casco para que funcione a cualquier aspecto (p. ej. proa asimétrica con popa roma explícita, en vez de un perfil casi simétrico) | Trabajo de diseño gráfico + re-verificación completa. Conserva las 374 versiones |
| **C** | Aceptar el 27 % como está | Coste 0. El ítem es correcto; sólo pierde fidelidad narrativa en algunas versiones |
| **D** | Atacar el mecanismo dominante: acotar el radio de las bandas por el ancho, p. ej. `r1 <- min(h*0.46, w*0.28)` y `r2 <- min(h*0.40, w*0.24)`, de modo que dejen de solaparse cuando `w` es pequeño | Conserva **las 222 versiones**. No toca `prof()` ni la invariante I-2 (las bandas son decorativas, no participan del *bounding box*). Requiere re-inspección visual de las 8 combinaciones |

**Recomendación:** **opción D**, y sólo si no basta, combinarla con **A**.

La opción D ataca el mecanismo que las mediciones señalan como dominante (el solape de bandas,
72,3 % en el caso peor) sin recortar el espacio de versiones ni tocar la geometría del casco, que es
lo que sostiene la clave. La opción A es más contundente pero paga 60 preguntas; tenerla como
respaldo es razonable si tras D el contorno sigue sin leerse como barco a `ratio 1.5`.

**Precedente de esta sesión (leer antes de intentar un fix):** se probó una **quinta** vía —hacer la
fracción de proa adaptativa al aspecto— y se midió que **empeoraba** el resultado global. Los
detalles están más abajo. La lección es que en este casco los parámetros están acoplados y cualquier
cambio debe medirse sobre **las 8 combinaciones**, no sobre el caso que motivó el arreglo.

#### Resolución aplicada — opción D (2026-07-28)

El factor se **calibró midiendo**, no eligiendo a ojo. Barrido de `rb <- min(h, w·k)` sobre las 8
combinaciones:

| `w × h` | k=1.00 (antes) | k=0.35 | k=0.30 | **k=0.25** | k=0.20 |
|---|---|---|---|---|---|
| 3 × 2 | **72,3 %** | 53,1 % | 46,4 % | **37,0 %** | 22,8 % |
| 4 × 2 | 65,2 % | 53,1 % | 46,4 % | **37,0 %** | 22,8 % |
| 5 × 2 | 58,2 % | 53,1 % | 46,4 % | **37,0 %** | 22,8 % |
| 6 × 2 | 51,1 % | 51,1 % | 46,4 % | **37,0 %** | 22,8 % |
| 3 × 1 | 51,1 % | 51,1 % | 46,4 % | **37,0 %** | 22,8 % |
| 4 × 1 | 37,0 % | 37,0 % | 37,0 % | **37,0 %** | 22,8 % |
| 5 × 1 | 22,8 % | 22,8 % | 22,8 % | **22,8 %** | 22,8 % |
| 6 × 1 | 8,7 % | 8,7 % | 8,7 % | **8,7 %** | 8,7 % |

Se eligió **k = 0,25**: baja el peor caso de 72,3 % a 37,0 % —el mismo nivel que `4 × 1`, que ya se
veía bien— y **no altera los casos alargados**: `5 × 1` y `6 × 1` quedan idénticos, verificado
comparando el render antes/después.

```r
rb <- min(h, w * 0.25)   # y las bandas usan rb en vez de h para su radio
```

Las bandas son decorativas: **no participan del *bounding box***, así que este cambio no toca la
invariante I-2 (re-verificado: 222/222 sin desajuste).

**Verificación visual (`w=3, h=2`, el caso peor):** antes, las dos medialunas y el puente formaban
una sola mancha negra; ahora los tres elementos se distinguen individualmente. **Residual honesto:**
la silueta sigue siendo una cápsula redondeada, no un barco — eso es el mecanismo 1 (contorno), y
las opciones **A** (restringir a `ratio ≥ 2.5`, cuesta 104 versiones sobre el espacio actual) y
**B** (rediseñar el perfil) siguen disponibles si se quiere resolver también.

#### Re-medición post-P2.7 (2026-07-28, sesión posterior)

La distribución de ratios de arriba se midió sobre 222 combinaciones. Tras P2.7 el espacio es de
374, así que se **re-enumeró exhaustivamente** (no muestreo) para que la decisión pendiente se tome
con cifras vigentes:

| ratio | Combinaciones | % | Lectura visual (verificada con render ampliado ×2) |
|---|---|---|---|
| 1.5 | 56 | 15,0 % | **Octágono simétrico — no se lee como barco** |
| 2.0 | 48 | 12,8 % | Cápsula alargada, aún simétrica — marginal |
| 2.5 | 40 | 10,7 % | Ya se lee como casco (proa y popa afinadas) |
| 3.0 | 95 | 25,4 % | Correcto |
| 4.0 | 54 | 14,4 % | Correcto |
| 5.0 | 45 | 12,0 % | Correcto |
| 6.0 | 36 | 9,6 % | Correcto (el mejor) |

**104 de 374 versiones (27,8 %) tienen `ratio ≤ 2`** — la proporción apenas se movió respecto del
27,0 % medido sobre 222, pero el coste absoluto de recortar sí cambió.

**Precisión sobre el mecanismo residual.** La inspección ampliada de los cuatro casos muestra que lo
que falla no es «el afinado es demasiado brusco», sino que **el perfil es casi simétrico**: proa
(`t^0.7`) y popa (`t^0.5`) son apenas distinguibles, de modo que la silueta no tiene dirección. A
`ratio ≥ 2.5` la elongación compensa esa falta de dirección y la figura se lee como casco; a 1.5 y
2.0 no hay elongación que la compense. Esto acota el alcance de la opción **B**: no basta con
retocar el exponente del afinado, hay que introducir **asimetría explícita** proa/popa.

**Costes actualizados de las opciones (medidos por enumeración):**

| Opción | Restricción | Versiones resultantes | Coste |
|---|---|---|---|
| **C** | Ninguna (aceptar) | **374** | 0 — 104 versiones (27,8 %) siguen siendo cápsulas |
| **A′** *(nueva)* | `ratio ≥ 2` (`alto = 2` sólo con `ancho ≥ 4`) | **318** | −56 versiones (−15,0 %). Elimina sólo el caso claramente malo; conserva el marginal |
| **A** | `ratio ≥ 2.5` (`alto = 2` sólo con `ancho ≥ 5`) | **270** | −104 versiones (−27,8 %). Elimina cápsula y marginal |
| **B** | Rediseño del perfil con asimetría proa/popa | **374** | Diseño gráfico + re-verificación completa de la invariante I-2 |

La variante **A′** no estaba contemplada en la tabla original y es el punto medio real: cuesta la
mitad que A y elimina el caso que la inspección visual señala como inaceptable.

**Criterio de cierre:** tras aplicar la opción elegida, re-ejecutar la enumeración exhaustiva
(0 desajustes de *bounding box*), `validar_diversidad_sustantiva.R --n 40` (PASS) y
`verificar_render.R` (5/5), e inspeccionar visualmente los dos casos extremos de forma que queden
en el espacio resultante.

#### Resolución del residual — opción A′ (2026-07-28, decisión del usuario)

**Cambio aplicado.** `alto_barco` deja de sortearse libremente en `1:2` y pasa a depender de
`ancho_barco`, de modo que `ratio = ancho/alto ≥ 2` por construcción:

```r
alto_pool  <- if (ancho_barco >= 4L) 1L:2L else 1L
alto_barco <- safe_sample(alto_pool, 1L)
stopifnot(ancho_barco / alto_barco >= 2)
```

El `stopifnot` no es decorativo: convierte la restricción en una invariante verificable en tiempo de
ejecución, no en una convención que un edit futuro pueda romper en silencio. `safe_sample` (Familia
5, regla #21) cubre el caso `alto_pool` de longitud 1 —`ancho_barco == 3`—, donde `sample()` habría
caído en la trampa del escalar.

**Por qué A′ y no A ni B.** A′ elimina exactamente las 56 versiones que la inspección visual
ampliada califica de inaceptables (`ratio 1.5`, octágono simétrico) y conserva las 48 marginales
(`ratio 2.0`, cápsula alargada). A costaba el doble (104 versiones) por eliminar además un caso que
no es inaceptable; B conservaba las 374 pero exige rediseño con asimetría proa/popa y ya hay un
intento fallido documentado en este mismo ítem.

**Verificación completa tras el cambio (enumeración exhaustiva, no muestreo):**

| Verificación | Antes (374) | Después (318) |
|---|---|---|
| Combinaciones válidas | 374 | **318** |
| Respuestas correctas distintas | 374 | **318** (biyección conservada) |
| Ratio mínimo | 1.5 | **2.0** |
| Versiones con `ratio < 2` | 104 (27,8 %) | **0** |
| *Bounding box* = clave (invariante I-2) | 374/374 | **318/318**, 0 desajustes |
| Colisiones entre las 4 opciones | 0 | **0** |
| Estructura 2×2 en las 4 opciones | 374/374 | **318/318** en las cuatro |
| Desplazamiento de `GEO-COORD-04` fuera de grilla | 0 | **0** |
| `y_pool` vacío | 0 | **0** |
| `validar_coherencia_matematica.R` | APROBADO | **APROBADO**, 0 errores |
| `validar_diversidad_sustantiva.R --n 40` | PASS, 38 únicos | **PASS**, 37 únicos |
| `verificar_render.R` | 5/5 + P6 | **5/5 + P6 sin fuga** |

**Inspección visual de los extremos del espacio resultante** (render ampliado ×2): el caso más
compacto es ahora `w=4, h=2` (`ratio 2.0`) y el más alargado `w=6, h=1` (`ratio 6.0`). En ambos las
dos medialunas y el puente se distinguen individualmente (efecto de la opción D) y la silueta se
lee como casco. El octágono simétrico ya no puede aparecer.

**Renders posibles tras A′:** 318 × 8 protagonistas × 4 reflexiones × 24 órdenes = **244 224**.

**Residual restante:** ninguno bloqueante. Las 48 versiones de `ratio 2.0` son cápsulas alargadas
—aceptables, no inaceptables—. Si en el futuro se quisiera eliminarlas también, la opción **A**
(`ratio ≥ 2.5`, 318 → 270) y la **B** (rediseño con asimetría proa/popa, conserva las 318) siguen
documentadas arriba, con la advertencia de que B requiere asimetría explícita y no un retoque del
exponente del afinado.

---

### P1.2 — Modularizar `dibujar_barco()` a un archivo externo — **BLOQUEADO por incompatibilidad de herramienta**

**No es un pendiente de "cuando haya tiempo": está medido y bloqueado.**

`validar_diversidad_sustantiva.R` (regla #22, obligatorio) crea un directorio temporal, hace
`setwd(tmp)` y evalúa el chunk `data_generation` en un `new.env()` **fuera** del pipeline de
`xexams()` (verificado leyendo el script, líneas 100-109). En ese contexto `include_supplement()`
—el mecanismo **oficial** de R/exams para archivos suplementarios— no dispone del estado interno
que necesita y falla, arrastrando todo el chunk a error.

El subproyecto hermano `desplazamiento-avion-aeropuerto` lo intentó con el patrón oficial: los 5
formatos renderizaron correctamente, pero el validador falló **40/40 semillas** con
`WARN_DIV_INDET`. Hubo que revertir (ver su `docs/BACKLOG.md`, P1.1). El fallback
`if (file.exists("R/helper.R")) source(...)` tampoco funciona, porque el validador ya hizo
`setwd(tmp)` y la ruta relativa se resuelve contra el temporal vacío.

**Mecanismo confirmado contra fuente primaria** (`cran/exams`, `R/xexams.R`, consultado
2026-07-28):

```r
dir_temp <- if(is.null(tdir)) tempfile() else file_path_as_absolute(tdir)
file.copy(file_path, file.path(dir_temp, file_Rnw))
setwd(dir_temp)
```

Sólo se copia el archivo del ejercicio. Ningún `.R` auxiliar llega al temporal por sí solo.

**Criterio de desbloqueo:** adaptar `RR/.claude/scripts/validar_diversidad_sustantiva.R` para que
soporte ejercicios modularizados — copiando también los auxiliares (`R/*.R`) al tempdir antes de
evaluar, o evaluando el chunk con el `cwd` del ejercicio. Es trabajo sobre una **herramienta
compartida** que afecta a todos los ejercicios del repo, no sobre este subproyecto. Hasta
entonces, el `.Rmd` permanece auto-contenido (invariante I-1).

**Lo que sí se modularizó en esta sesión** (lo que la restricción permite): el material externo al
render — `docs/`, `.claude/` local, `_archivo/prototipo-flujo-b/`, y `verificar_render.R` como
herramienta de verificación separada de `SemilleroUnico_v2.R` (exportación).

---

### P1.3 — Regla #11 (contextos narrativos): sólo varía el protagonista — **RESUELTO como decisión de diseño**

La regla #11 (`contextos-narrativos-creativos.md`) exige un pool de 6+ plantillas narrativas con al
menos 5 estructuras gramaticales distintas. Este ejercicio varía **únicamente el nombre del
protagonista** (8 nombres, líneas 154-158) sobre un enunciado fijo.

**Veredicto: no es una violación que haya que corregir.** La regla #11 gobierna ejercicios de
contexto **inventado**, donde la narrativa es libre. Este ítem deriva de un ítem ICFES **real**
(`MAT-2026-1-022`) y conserva su enunciado y sus cuatro opciones *verbatim*, según la política
registrada en la memoria del proyecto (`feedback_respetar_enunciado_original.md`): al derivar de un
ítem oficial se respeta su redacción, y la aportación metacognitiva va en la Solution (diagnóstico
por distractor con códigos `GEO-COORD-0x`), no en reescribir el enunciado.

Reescribir el contexto narrativo para "cumplir" la regla #11 destruiría la trazabilidad con el
ítem oficial y con su clave. La variación del protagonista es la única aleatorización compatible
con esa política.

**Acción:** ninguna sobre el `.Rmd`. Queda documentado en
[`../.claude/CLAUDE.md`](../.claude/CLAUDE.md) (particularidad 5) para que ningún agente futuro lo
"corrija".

---

### P1.4 — El pool de errores no tiene `precondicion` ni `calcula()` — **RESUELTO como patrón legítimo**

La regla #1 (`ejercicios-metacognitivos.md`) describe pools de errores con un campo `precondicion`
(cuándo aplica el error) y una función `calcula()` (que produce el distractor). Aquí `errores_info`
(líneas 112-151) guarda `codigo` / `nombre` / `texto` / `diagnostico`, y los distractores se
construyen con `paste0` (líneas 55-91).

**Veredicto: no es un defecto.** Los campos `precondicion` y `calcula()` existen para pools donde
el distractor es un **valor numérico derivado** de los datos y cuya aplicabilidad depende de
propiedades de la muestra (paridad de `n`, modalidad, existencia de cuartiles…). Aquí los
distractores son **cadenas de coordenadas** construidas directamente a partir de las mismas cuatro
variables que generan la respuesta correcta:

- No hay condición de aplicabilidad que declarar: los tres errores aplican siempre, y su unicidad
  está garantizada por construcción (exclusiones de `y_pool` + `stopifnot`, verificado
  exhaustivamente).
- No hay función `calcula()` sobre la que verificar determinismo, así que la Capa D de la
  validación semántica no tiene nada que comprobar — y en efecto
  `validar_coherencia_matematica.R` reporta APROBADO con las Capas A-D en OK.

**Acción:** ninguna. Documentado en [`BLUEPRINT.md`](BLUEPRINT.md) §4.4.

---

## P2 — Diferible

### P2.1 — Promoción a `02-En-Desarrollo/`
Requiere cerrar P1.1 (decisión del usuario) y la suite completa del repo en verde. Ver
[`ROADMAP.md`](ROADMAP.md) §3.

### P2.2 — Validación Nivel 3 en aula → `03-En-Produccion/`
Requiere aplicación con estudiantes reales y análisis de diagnosticidad por distractor. Ver
[`ROADMAP.md`](ROADMAP.md) §4. Es el gate que la validación automática **no** puede sustituir.

### P2.3 — Artefactos derivados sin regla de exclusión en git — ✅ **RESUELTO 2026-07-28**

Se añadió un **`.gitignore` local** al subproyecto, en vez de tocar el del repo raíz (que ya venía
modificado en el árbol de trabajo por trabajo ajeno a este subproyecto).

**Auditoría previa.** Antes de escribirlo se comprobó, con `git check-ignore -q` y su código de
salida, qué cubría ya la raíz:

| Artefacto | ¿Lo ignora la raíz? |
|---|---|
| `salida/*`, `output_*/`, `*.html` de la raíz del SP | Sí |
| `verif_render/*.html`, `*.pdf`, `*.docx`, `*.xml` | Sí |
| `verif_render/*.rds` (metadatos de `exams2nops()`) | **No** |
| `plano_barco.png` | **No** — la raíz tiene una **negación** `!**/*.png` (línea 94) que des-ignora todos los PNG del repo |

Por eso el `.gitignore` local tiene alcance **deliberadamente mínimo**: solo esas dos brechas más
un `*.rds` defensivo. Duplicar lo que ya cubre la raíz crearía dos fuentes de verdad que se
desincronizan.

```gitignore
/plano_barco.png    # anclado: NO debe afectar a _archivo/prototipo-flujo-b/comparacion_flujo_b.png
/verif_render/
*.rds
```

**La barra inicial de `/plano_barco.png` es esencial**: sin ella la regla también atraparía
`_archivo/prototipo-flujo-b/comparacion_flujo_b.png`, que **sí está trackeado** como artefacto
histórico del Flujo B. Verificado con `git ls-files` que es el único `.png` trackeado del
subproyecto, y con `git check-ignore` que sigue sin ignorarse tras el cambio.

**Criterio de cierre — verificado:** `git status` del subproyecto queda limpio (sin untracked);
`plano_barco.png`, `verif_render/` y cualquier `.rds` de la raíz quedan ignorados; el PNG archivado
sigue trackeado.

### P2.5 — La Solution tenía 4 de las 6 subsecciones canónicas — ✅ **RESUELTO 2026-07-28**
**Origen:** auditoría del detractor (2026-07-28).

La regla #1 (`ejercicios-metacognitivos.md`, «Sección Solution Obligatoria») lista seis
subsecciones. El chunk `solucion` incluía cuatro: *Respuesta correcta* + *Análisis de cada opción*
(cubre «Análisis del error»), *Procedimiento correcto*, *Reflexión metacognitiva* y *Estrategia
para evitar el error*.

Faltaban:

- **Propiedades del concepto** — p. ej.: en un par ordenado la primera coordenada es siempre la
  horizontal; un rectángulo alineado a los ejes queda determinado por los extremos de ambos rangos
  y tiene exactamente 4 vértices.
- **Caso específico (transferencia)** — p. ej.: «si el barco se desplazara 2 unidades a la derecha,
  ¿cuáles serían los nuevos vértices?».

#### Resolución aplicada

Se insertaron ambas subsecciones entre *Procedimiento correcto* y *Reflexión metacognitiva*
(líneas 397-415 y 416-437 del `.Rmd`):

- **Propiedades del concepto** (líneas 397-415): cuatro propiedades — el orden del par ordenado no
  es intercambiable; un rectángulo de lados paralelos a los ejes queda determinado por dos valores
  de x y dos de y; el rectángulo que encierra un objeto usa el mínimo y el máximo de cada eje; los
  cuatro vértices nunca están alineados.
- **Caso específico** (líneas 416-437): transferencia — si el barco se desplaza 1 unidad en
  vertical (dirección adaptativa según el margen disponible), el rango en x no cambia y solo se
  desplazan las segundas coordenadas de los 4 vértices; refuerza que los dos ejes se leen por
  separado.

La Solution tiene ahora las **6 subsecciones canónicas**. Verificado en el XML de Moodle
renderizado: las 7 cabeceras `### ` están presentes (*Respuesta correcta*, *Análisis de cada
opción*, *Procedimiento correcto*, *Propiedades del concepto*, *Caso específico*, *Reflexión
metacognitiva*, *Estrategia para evitar el error*).

**Impacto:** el ejercicio ya era metacognitivo (diagnóstico por distractor con código de error,
reflexión y estrategia); las dos subsecciones nuevas añaden profundidad conceptual y de
transferencia, coherente con la regla #1.

### P2.6 — `sample()` interno redundante con `exshuffle: TRUE`
**Origen:** auditoría del detractor (2026-07-28). Severidad BAJA.

Las líneas 106-109 mezclan las opciones con `perm <- sample(4L)` y además `exshuffle: TRUE` vuelve a
mezclarlas. Ambos mecanismos son coherentes entre sí (R/exams reordena `questionlist`,
`solutionlist` y `exsolution` con la misma permutación), así que **no hay bug**: sólo lógica
redundante. Se puede simplificar dejando que `exshuffle` haga todo el trabajo, o mantenerlo como
control explícito. No urge.

### P2.7 — Las exclusiones de `y_pool` habían quedado obsoletas — ✅ **RESUELTO 2026-07-28** (alcance mayor al previsto)
**Origen:** consecuencia de resolver P0.1 (2026-07-28).

Las cuatro exclusiones de `y_pool` (`y_min ≠ x_min`, `y_max ≠ x_max`, `y_min ≠ x_max`,
`y_max ≠ x_min`) se habían introducido para garantizar que el retirado `GEO-COORD-03` (diagonal)
tuviera 4 puntos distintos. La hipótesis inicial era que sólo las dos últimas podían ser
innecesarias, porque las dos primeras «seguían haciendo falta para que `GEO-COORD-01` (inversión)
no colapsara sobre la correcta».

#### Medición por enumeración exhaustiva

| Configuración | Versiones | Correctas únicas | Colisiones |
|---|---|---|---|
| Las 4 exclusiones (antes) | 222 | 222 | 0 |
| Solo las 2 primeras | 286 | 286 | 0 |
| **Ninguna (aplicado)** | **374** | **374** | **0** |

**Se retiraron las CUATRO exclusiones**, no solo las dos últimas: la medición mostró que ninguna
era necesaria.

#### Hallazgo: la justificación de `y_min ≠ x_min` era falsa

La exclusión `y_min != x_min` se justificaba como «evita que `GEO-COORD-01` (inversión) colapse
sobre la correcta». **Esa justificación era falsa**: la inversión nunca puede igualar a la
correcta, porque exigiría a la vez `y_min == x_min` **y** `y_max == x_min`, o sea
`alto_barco == 0`, imposible con `alto_barco >= 1`. El `stopifnot(length(unique(all_opts)) == 4L)`
(línea 103) es la red de seguridad real — no las exclusiones del pool.

**El espacio de versiones sube de 222 a 374 (+68 %).**

#### Re-verificación completa sobre el espacio de 374 (2026-07-28)

```
Versiones: 374   respuestas correctas distintas: 374
Estructura 2x2 correcta      : 374/374
Estructura 2x2 GEO-COORD-01  : 374/374
Estructura 2x2 GEO-COORD-02  : 374/374
Estructura 2x2 GEO-COORD-04  : 374/374
4 opciones distintas         : 374/374
Desplazamiento fuera grilla  : 0
Caso de transferencia (P2.5) fuera de grilla : 0
Bounding box roto (invariante I-2) : 0
```

Más: `validar_coherencia_matematica.R` → APROBADO 0 errores · `validar_diversidad_sustantiva.R
--n 40` → **PASS, 38 valores únicos** (antes 36 — el espacio ampliado mejoró la diversidad) ·
`verificar_render.R` → 5/5 formatos OK sin fuga P6 · ortografía 0 errores.

Renders distintos posibles: **374 × 8 protagonistas × 4 reflexiones × 24 órdenes = 287 232**.

**Acción:** ninguna pendiente. El comentario del `.Rmd` (líneas 30-46) documenta la medición y el
hallazgo de la justificación falsa.

### P2.4 — `SemilleroCloze.R` no aplica a este ejercicio
Es una plantilla exploratoria de formato cloze+schoice; este ejercicio es SCHOICE puro. Se conserva
por consistencia con los demás subproyectos, pero es candidato a mover a `_archivo/` si se confirma
que no se va a usar.

---

## Referencias cruzadas

- [`../README.md`](../README.md) · [`../HANDOFF.md`](../HANDOFF.md)
- [`BLUEPRINT.md`](BLUEPRINT.md) (invariantes I-1 a I-8) · [`SYLLABUS.md`](SYLLABUS.md) ·
  [`ROADMAP.md`](ROADMAP.md)
- [`../.claude/CLAUDE.md`](../.claude/CLAUDE.md) ·
  [`../.claude/rules/barco-parametrico.md`](../.claude/rules/barco-parametrico.md)
- Hermano con el mismo bloqueo de modularización:
  `../../desplazamiento-avion-aeropuerto/docs/BACKLOG.md` (P1.1)

---

**Versión:** 1.2 · **Fecha:** 2026-07-28 (v1.2 — **P1.1 cerrado**: re-medición de ratios sobre el
espacio de 374, opción **A′** aplicada por decisión del usuario, espacio 374 → 318, `ratio ≥ 2` por
construcción; costes de las opciones A/B actualizados; v1.1 — P2.5 y P2.7 resueltos: Solution con 6
subsecciones canónicas; retiradas las 4 exclusiones de `y_pool`, espacio de versiones 222 → 374)
