# Regla #22 — Diversidad Sustantiva (la respuesta correcta DEBE variar)

## Principio Fundamental

**La diversidad de un ejercicio DEBE ser SUSTANTIVA: los datos numéricos y el contenido de la respuesta correcta deben cambiar entre versiones. Un conteo alto de "versiones únicas del render" NO es evidencia suficiente — mide el envoltorio narrativo (contexto, orden de opciones, reflexiones), NO la sustancia. Un ejercicio donde la respuesta correcta es siempre idéntica NO tiene diversidad real.**

Esta regla NO tiene excepciones. Aplica a SCHOICE y CLOZE, a cualquier tipo de opción (texto, gráficos, valores numéricos), y en todos los directorios de desarrollo.

---

## Origen: incidente 2026-06-27 (desplazamiento-avion-aeropuerto)

Un ejercicio SCHOICE reportó **"288/300 versiones únicas"** tras el pipeline completo (orquestador, detractor, validación de diversidad). Sin embargo, la opción correcta era **siempre el mismo diagrama** en todas las versiones:

- `distancia_total <- 100`, `angulo <- 50`, `distancia_avanzada <- 30` — valores hardcoded como literales sin `sample`/`runif`.
- Las opciones gráficas se copiaban con `file.copy()` desde PNGs estáticos, NO se generaban dinámicamente.
- El detractor alucinó estructura de código: "simuló" en vez de ejecutar el chunk real, por lo que sus afirmaciones de corrección estaban basadas en campos inventados.

**El conteo de 288/300 medía la FORMA** (8 contextos × protagonistas × 24 órdenes de opciones × 6 reflexiones), **NO la SUSTANCIA** (los datos numéricos del diagrama correcto eran siempre 100/50/30). Pasó el orquestador, el detractor y la validación de diversidad basada en conteo de renders.

**Por eso existe esta regla y el script `validar_diversidad_sustantiva.R`**: captura exactamente este caso — diversidad cosmética con respuesta correcta invariante.

---

## Patrones PROHIBIDOS

### ❌ P1: Parámetros numéricos hardcoded que determinan la respuesta

```r
# ❌ PROHIBIDO — la respuesta correcta es SIEMPRE la misma
distancia_total   <- 100   # literal fijo
angulo            <- 50    # literal fijo
distancia_avanzada <- 30   # literal fijo
```

Los parámetros que determinan CUÁL opción es la correcta (sus valores numéricos, textos clave, dimensiones) DEBEN aleatorizarse con `sample`/`runif`/`rnorm`/`rbinom` u otras funciones R de generación aleatoria.

### ❌ P2: PNGs estáticos copiados como opciones gráficas

```r
# ❌ PROHIBIDO — los mismos 4 PNGs en toda corrida
file.copy("diagramas/correcto.png", "opcion_A.png")
file.copy("diagramas/distractor1.png", "opcion_B.png")
```

Las imágenes que representan opciones gráficas DEBEN generarse **dinámicamente** por versión (ggplot2, TikZ, matplotlib/reticulate), parametrizadas con las variables aleatorias del `data_generation`. Un PNG estático copiado siempre produce el mismo contenido visual — es diversidad cero para esa opción.

### ❌ P3: "Diversidad" reportada solo por conteo de versiones del render

```r
# ❌ INSUFICIENTE — 288/300 versiones únicas NO significa que la respuesta varíe
exams2html("ejercicio.Rmd", n = 300)
# → 288 unique: conteo de contextos × órdenes × reflexiones (forma), NO la respuesta
```

El conteo de versiones únicas del render (`exams2html(n=300)`) mide si el **envoltorio** difiere (distintos contextos narrativos, distintos órdenes de opciones, distintas reflexiones). **No garantiza** que los datos numéricos o el contenido gráfico de la respuesta correcta cambien. Un ejercicio con 8 contextos × 4 órdenes de opciones produce 32 versiones únicas aunque la respuesta correcta sea siempre la misma.

### ❌ P4: Predictibilidad POSICIONAL/ORIENTACIONAL de la respuesta correcta

```r
# ❌ PROHIBIDO — la respuesta correcta SIEMPRE en el mismo cuadrante/posición/orientación
dibujar_diagrama("correcta.png", ..., modo = "ne")   # siempre noreste
dibujar_diagrama("distractor.png", ..., modo = "ne") # los distractores también
# → el estudiante aprende "la correcta apunta arriba-derecha" sin analizar los datos
```

Aun cuando el **valor** de la respuesta correcta varíe entre versiones (distinta distancia, distinto número), si su **posición, orientación o cuadrante visual es siempre el mismo**, el estudiante predice la correcta por su ubicación, no por el contenido. Casos: la opción correcta siempre en el primer cuadrante de un plano; la barra correcta siempre la más alta; el gráfico correcto siempre en la misma celda de la grilla; la afirmación correcta siempre con cierta estructura.

**Trampa del validador**: `validar_diversidad_sustantiva.R` extrae un *fingerprint del VALOR* de la respuesta correcta. Si el valor varía (p.ej. la distancia), reporta `PASS` **aunque la posición/orientación sea invariante**. Por eso la diversidad por valor NO basta: hay que aleatorizar también la dimensión posicional/orientacional. Incidente real: `desplazamiento-avion-aeropuerto` (2026-06-28) — el validador daba 39/40 valores únicos pero la correcta SIEMPRE caía en el cuadrante NE.

**Defensa**: aleatorizar la orientación/posición global de la escena por versión (p.ej. cuadrante ∈ {NE, NO, SE, SO}), aplicando la MISMA transformación a todas las opciones (preserva la estructura relativa correcta) y reflejándola en el texto del enunciado (la descripción de dirección/posición debe ser coherente con la transformación elegida). Verificación: renderizar ≥8 versiones y confirmar que la respuesta correcta aparece en posiciones/orientaciones distintas.

#### P4-bis — Variante SEMÁNTICA: el VEREDICTO de la clave es invariante (2026-08-08)

El caso más traicionero de P4 no es visual sino textual, y estaba descrito en la lista de arriba («la afirmación correcta siempre con cierta estructura») sin que **ningún validador lo midiera**.

En un ítem de conclusión binaria —opciones que empiezan por «Sí, porque…» / «No, porque…»— la clave puede tener **siempre el mismo veredicto** aunque su valor numérico varíe en cada versión. Ocurre cuando la afirmación que el estudiante debe evaluar es falsa **por construcción**: si el enunciado siempre propone un valor obtenido con el procedimiento erróneo, la respuesta correcta es siempre «No».

```r
# ❌ PROHIBIDO — la afirmación evaluada es falsa en el 100 % de las versiones
afirmacion_min <- comp_largo_max   # complemento lineal: nunca coincide con el
afirmacion_max <- comp_largo_min   # complemento del producto -> clave siempre "No"

# ✅ CORRECTO — se sortea si la afirmación es verdadera o falsa
afirmacion_es_verdadera <- if (is_canonical) FALSE else sample(c(TRUE, FALSE), 1)
```

**Por qué ninguna defensa previa lo veía:**

| Validación | Qué mide | Por qué no lo detecta |
|---|---|---|
| `validar_diagnosticidad.R` H2 | Que la clave sea la **única** con su prefijo, dentro de una versión | Con balance 2+2 nunca es única → 0 % |
| `validar_diversidad_sustantiva.R` | Que el **valor** de la clave varíe | Varía (decenas de valores únicos); el veredicto no es un valor |
| Balance Sí/No 2+2 | Que cada versión tenga 2 y 2 | Es intra-versión: se cumple en todas y aun así la clave es siempre «No» |

**Impacto:** el estudiante que aprende el patrón descarta la mitad de las opciones sin razonar — de 25 % a 50 % de acierto por azar.

**Defensa cableada:** sonda **H3** de `validar_diagnosticidad.R` (cross-versión). Mide la frecuencia de la primera palabra de la clave a lo largo de las versiones: 100 % → `ERR_DIAG_SUPERFICIAL` (bloqueante, exit 1); ≥90 % → aviso. Descarta las versiones en que todas las opciones comparten prefijo, para no penalizar ítems donde el prefijo no informa.

**Nota de diseño:** si el ítem reproduce un cuadernillo oficial, la instancia canónica conserva el veredicto del ítem real (allí la clave es la que es) y son las **demás versiones** las que alternan. Cuando se añade una clave alternativa con el veredicto opuesto, hay que **excluir del pool a su gemela**: dos opciones con el mismo rango y veredictos contrarios convierten el ítem en irresoluble.

#### La propia defensa crea deuda — tres cosas que hay que revisar DESPUÉS de aplicarla

Añadir una clave alternativa no es un cambio local: **cambia qué significa el enunciado** en la mitad de las versiones, y con ello la premisa sobre la que se escribió el pool existente. Verificado en `area-jardin-lote-porcentaje-n4` (2026-08-09), donde los tres puntos fallaron a la vez y todo el arsenal seguía en verde.

1. **Las guardas anti-colisión deben recorrer TODAS las claves, no la vigente.** Una guarda que compara cada candidato contra `descripcion_corta` de la clave de esa versión solo protege la rama cuya clave comparte plantilla con el distractor. En la otra rama la clave se redacta distinto, el literal no coincide y el distractor pasa afirmando el rango correcto con el veredicto contrario. La clave **NO vigente** es la firma exacta de esa colisión. → Error 28.

2. **Los distractores escritos para una sola clave pueden quedar incoherentes.** Si eran coherentes porque la afirmación del enunciado *era* lo que ellos afirman, al alternar la afirmación dejan de serlo: declaran un veredicto y su justificación apoya el contrario (`INC-SINO-BINARIO`, defecto 1). Medido: 81 de 600 versiones (13,5 %), todas en la rama nueva. → Error 29.

3. **La sonda H1 no ve lo que pasa dentro de cada rama.** Un ítem con clave alternante tiene **dos ramas estructuralmente distintas**, y H1/H2 promedian sobre todas las versiones sin condicionar por rama: un reparto 100 % / 0 % se lee como ~50 % y queda bajo el umbral del 70 %. Al corregir el punto 2 excluyendo un distractor, si ese era el único más largo que la clave, la clave pasa a ser **determinísticamente** la más larga de su rama. Medido: 100 % dentro de la rama, `PASS` en el agregado, **50,5 % de acierto sin razonar** frente al 25 % de azar. → Error 30.

**Regla operativa:** tras aplicar §P4-bis, medir H1/H2 **condicionando por rama** (agrupar por el flag que la define y recalcular dentro de cada grupo). El `PASS` agregado de `validar_diagnosticidad.R` no acredita ese caso; hoy es verificación manual. Y al igualar longitudes, comprobar que no se crea la **señal inversa**: si la clave pasa a no ser NUNCA la más larga, «descartar la más larga» sube el azar de 25 % a 33 %.

#### El molde uniforme de opciones ciega a H2 y a H3 — sonda H3b (2026-08-10)

Cuando las cuatro opciones comparten primera palabra —el caso típico es un ítem cuyas opciones son
**preguntas** (`¿Cuál es…?`), o cualquier molde con encabezado común— **dos de las tres sondas dejan
de medir, y el script lo callaba**. Verificado en el código, no en la documentación:

| Mecanismo | Consecuencia |
|---|---|
| `pw` descarta el `¿` inicial y toma el primer token alfanumérico | las 4 opciones dan `cuál` |
| H2 exige que la clave sea la **única** con su prefijo | **0 % por construcción**, nunca dispara |
| La guarda de H3 exige ≥2 prefijos distintos | `pwc` queda vacío |
| La impresión de H3 va bajo `if (length(pwc) >= 5L)` | **la fila H3 no se imprime**: no sale «0 %», no sale nada |

Un `PASS` en esas condiciones **no acredita** que el tipo de clave varíe: es «sin medición»
disfrazado de «sin señal». Es el mismo modo de fallo que dio origen a H3, un piso más abajo.

**Defensa cableada — sonda H3b (cross-versión, por contenido).** Mide lo mismo que H3 —¿la clave es
siempre del mismo tipo?— pero sobre la **firma de contenido** de la opción: texto en minúsculas, sin
dígitos ni puntuación, espacios colapsados. Así, «¿Cuál es la ubicación…?» y «¿Cuál es el área del
lote de 50 por 120 metros?» tienen firmas distintas aunque compartan prefijo, y los parámetros
numéricos no cuentan como variación.

- **Guarda análoga a la de H3**: la firma debe discriminar *dentro* de la versión
  (`length(unique(sg)) >= 2`). Las opciones puramente numéricas colapsan a cadena vacía al quitar
  dígitos, así que quedan excluidas — es la misma razón por la que H3 exige ≥2 prefijos.
- **Bloquea solo cuando es relevo de una sonda ciega** (prefijo uniforme en ≥90 % de las versiones).
  Si H3 puede medir, H3b es una segunda lectura del mismo fenómeno y se queda en aviso. No es
  timidez: con H3b bloqueando siempre, un fixture de `test_diagnosticidad.R` que existe para probar
  que H1 **no** dispara pasaba a ROJO — es decir, una sonda nueva cambiaba el veredicto de un caso
  ya revisado por un motivo que nadie había mirado.
- **La ceguera se declara siempre**, dispare o no H3b: el reporte imprime `H2/H3 CIEGAS` con el
  porcentaje de versiones afectadas y la frase «el 0 % de H2 NO es ausencia de señal, es ausencia de
  medición». Si además la firma no discrimina, imprime `H3b: NO MEDIBLE` y exige un verificador
  propio del ejercicio. **Nunca se deja un hueco pasando por aprobado.**

Origen: dry-run de `MAT-2026-1-010` (2026-08-10), ítem cuyas cuatro opciones son preguntas.
Tests: `tests/testthat/test_diagnosticidad.R` (4 casos nuevos: ceguera declarada, H3b caza clave
invariante con exit 1, H3b calla con clave sorteada de un pool, H3b no gobierna cuando H3 aplica).

---

### ❌ P5: Distractor direccional/posicional como OUTLIER obvio (eliminable de un vistazo)

```r
# ❌ PROHIBIDO — el distractor de "dirección equivocada" es un giro de 180° (la flecha apunta al revés)
dibujar_diagrama("distractor_dir.png", ..., th_axis = (th_axis + 180) %% 360, dist = otra_distancia)
# → el estudiante descarta "la que apunta al lado contrario" sin analizar; además su longitud única lo delata
```

Un distractor que se distingue por un rasgo saliente y obvio (apunta exactamente al revés, es el único con otra longitud, el único con otro formato, el único en otro cuadrante muy alejado) se elimina por percepción, no por razonamiento. Esto degrada el poder diagnóstico aunque el resto del ítem sea correcto. Es el gemelo conceptual del **Formato Equilibrado** de `graficos-como-opciones.md` (≥2 opciones comparten el formato de la correcta).

**Defensa**: el distractor direccional/posicional debe ser un **cuasi-acierto plausible** que comparta los rasgos salientes de la correcta y se diferencie SOLO en la dimensión evaluada. Para "dirección equivocada", preferir un **reflejo respecto al eje (lado opuesto: este↔oeste) a la distancia correcta** antes que un giro de 180°: misma magnitud y mismo ángulo, solo cambia el lado → obliga a verificar la dirección. Incidente: `desplazamiento-avion-aeropuerto` (2026-06-28) — el distractor de dirección pasó de 180°-opuesto (a otra distancia, outlier evidente) a **espejo este↔oeste a la distancia correcta** (cuasi-acierto). Coherente con que el nombre del error describa el error real (era "perpendicular" pero se dibujaba a 180°).

### ❌ P6: Fuga de la respuesta por metadato NO VISUAL (nombre de archivo, orden, id)

```r
# ❌ PROHIBIDO — el nombre de archivo delata el rol semántico fuera del contenido visual
ggsave("diagrama_correcta.png", plot = p_correcta, ...)
ggsave("diagrama_perp.png", plot = p_distractor_direccion, ...)
# → invisible en exams2html()/exams2pdf() (imagen embebida/base64), pero el XML de
#   exams2moodle() referencia el archivo por nombre: src="@@PLUGINFILE@@/diagrama_correcta.png"
```

Los patrones P1-P5 cubren fugas en el **contenido visual o numérico** de la opción (valores, posición, formato). P6 cubre una dimensión distinta: cualquier **metadato que acompaña a la opción sin ser parte de lo que el estudiante lee o ve directamente**, pero que es recuperable por un canal técnico — nombre de archivo, orden alfabético/de creación, id del elemento HTML/XML, clase CSS, o cualquier atributo que revele el rol (correcta/distractor) de la opción. Estos metadatos no se detectan revisando el HTML renderizado ni el PDF compilado: ambos ocultan o embeben el artefacto. El canal de fuga solo se manifiesta en exportaciones que referencian recursos por nombre (Moodle vía `exams2moodle()`, QTI y formatos similares).

**Defensa**: generar cada opción con un identificador neutral (letra: `diagrama_a.png`, `diagrama_b.png`...) asignado **DESPUÉS** de la mezcla interna con `sample()` — nunca antes, y nunca basado en el rol semántico de la opción. Verificar explícitamente exportando a Moodle y haciendo `grep` del XML resultante en busca de nombres de rol filtrados:

```bash
Rscript -e 'library(exams); exams2moodle("archivo.Rmd", n = 1, dir = "moodle_output")'
grep -oE 'diagrama_[a-z]+\.png' moodle_output/*.xml | sort -u
# Esperado: solo diagrama_a.png / diagrama_b.png / diagrama_c.png / diagrama_d.png
```

Incidente: `desplazamiento-avion-aeropuerto` (2026-07-28) — ver Error 25 en `patrones-errores-conocidos.md` y regla `graficos-como-opciones.md` §"Canal de fuga: el nombre de archivo delata la respuesta en Moodle".

---

### ❌ P7: Batería de eliminación SIN cierre por familias de dimensión

Los patrones P1–P6 nombran canales de fuga **concretos**. P7 es distinto: no es un canal,
es un defecto **del verificador**. Dice cómo hay que medir para que un «no encontré nada»
signifique algo.

> **Una batería de reglas de eliminación necesita CIERRE POR FAMILIAS DE DIMENSIÓN
> —magnitud, divisibilidad, signo, posición, formato, léxico—, no sólo por mínimo y máximo.**

**Cómo apareció (dos veces, en dos ejercicios distintos):** el verificador medía seis reglas
intra-celda y **ninguna tocaba la divisibilidad**. El canal real —**47,4 %** de acierto sin
razonar— estaba justo en esa familia sin sonda. El informe lo leyó como «sin señal». El
principio que se sigue de ahí:

> **Una batería incompleta no mide «sin señal», mide «SIN SONDA».**

Es el mismo modo de fallo que la ceguera de H2/H3 documentada en §P4-bis, un piso más
arriba: allí eran dos sondas concretas las que dejaban de medir en silencio; aquí es una
familia entera de reglas que nadie escribió.

#### Las cuatro exigencias (ninguna es opcional)

**1. Cobertura declarada.** Cada una de las seis familias debe tener sonda, o estar
declarada **no aplicable con su justificación** (`signo = "todas las magnitudes son
positivas"`). Una familia sin sonda ni declaración **invalida el veredicto completo**: el
resultado es `SIN_COBERTURA`, nunca `PASS`.

**2. Techo nulo, y el veredicto SOBRE EL EXCESO — nunca sobre la tasa absoluta.** Un máximo
sobre miles de combinaciones **está inflado por selección**: con 4 opciones una regla acierta
~25 % por azar, pero el *máximo* de ~19 reglas ronda el 35 %. Se calibra **permutando cuál
opción es la clave y dejando las reglas intactas**. Medición del ejercicio que originó la
lección: máximo **69,6 %**, techo nulo **34,8 %**, exceso **+35 pp**.

**Sin esa calibración el número no significa nada.** Comparar el 69,6 % contra el 25 % del
azar puro exagera el hallazgo; compararlo contra su techo nulo lo mide.

> **El umbral absoluto medía, en parte, el TAMAÑO DE LA BATERÍA.** Medición sobre **468 ítems
> oficiales únicos** de 6 cuadernillos de Matemáticas ICFES:

| Población | k reglas | tasa atómica | techo nulo | **exceso** |
|---|---|---|---|---|
| Oficiales 468, vara universal | 25 | 34,8 % | 27,8 % | **+7,0** |
| Oficiales 468, sin familia posición | 19 | 27,4 % | 27,0 % | **+0,4** |
| Ejercicio en revisión, vara universal | 25 | 30,4 % | 31,5 % | **−1,1** |
| Ejercicio en revisión, vara valor | 20 | 31,5 % | 30,6 % | **+0,9** |
| Oficiales 42 numéricos, vara valor | 20 | 31,0 % | 33,0 % | **−2,1** |
| Ejercicio en revisión, batería completa | 91 | 47,0 % | 33,6 % | **+13,4** |

> La última fila está medida con la convención de nulo exacto (exigencia 4). Con la
> convención 0/1 que el helper usaba antes —y que sorteaba una opción entre los
> supervivientes— la MISMA batería sobre los MISMOS datos leía 43,5 % / 35,4 % / **+8,1 pp**:
> el sorteo deprimía el máximo e inflaba el techo, y dejaba el ítem justo en el corte. **El
> borde era un artefacto de la puntuación**, y se resolvió en contra del ítem.

La **misma población** da 27,4 % con 19 reglas y 34,8 % con 25, y su techo nulo se mueve con
ella (27,0 → 27,8). **El exceso es invariante; la tasa absoluta no.** Y ningún ítem de
ninguna población, con 19 a 91 reglas, superó el **47,0 %**: un umbral del 70 % exigía aislar
la clave en 3 de cada 4 instancias, es decir era **inalcanzable, no severo** — y una puerta
que no se puede cruzar se aprende a ignorar igual que una que siempre está en rojo.

**Los cortes (calibrados, no elegidos):**

| Exceso | Veredicto | Por qué ahí |
|---|---|---|
| **≤ +2 pp** | sin canal | Ahí caen las **cuatro comparaciones limpias** (+0,4 · −1,1 · +0,9 · −2,1). Bajo H0 a N = 100 el exceso mide media +0,1 pp y **sd 2,2 pp**: +2 pp es ~1 sd, cero medido |
| **+2 a +8 pp** | **zona gris** | Ahí cae el **+7,0 pp de los 468 ítems oficiales**, que no es ruido (a N = 468 la sd baja a 1,0 pp) sino el **sesgo posicional real de los cuadernillos**. Un ítem que no filtra más que el corpus oficial no se declara defectuoso, pero tampoco se absuelve |
| **≥ +8 pp** | canal real | ~p99,5 del ruido a N = 100 (p95 = +4,0 pp; máximo de 500 réplicas H0 = +9,6 pp) y justo por encima de lo que filtra el corpus oficial |

**El nulo es UNIFORME sobre las posiciones, y eso es deliberado.** Permutar el vector de
claves observado preservaría su marginal y volvería invisible el canal posicional puro
(«la clave siempre está en la primera»), que es justo uno de los que la familia `posicion`
existe para cazar. El precio es que un sesgo posicional real cuenta como exceso — y por eso
los oficiales miden +7,0 pp: está **contemplado en la calibración**, no es un defecto.

**3. Declaración de incertidumbre, con la anchura MEDIDA.** La banda ya no es una constante:
es `k_sigma × sd(techo nulo)` de la propia corrida, y el helper la publica. Un `PASS` exige
que el exceso siga por debajo del corte de canal **incluso sumándole 2 sd del ruido**; un
`BLOQUEA` exige que siga por encima del corte de ruido **incluso restándoselas**. A N = 100
la sd ronda 2,2 pp y la zona gris (6 pp de ancho) la cubre; a **N = 30 la sd sube a 4-5 pp** y
entonces ni la ausencia se acredita → `NO_CONCLUYENTE`. Es la misma disciplina que la regla
#23 impone a los estratos con n < 20, y hace que la muestra estándar N = 100 sea **exigible
por el estadístico**, no sólo por decreto. **PROHIBIDO redondearlo a `PASS`.**

**4. La convención de puntuación tiene nulo EXACTO — y por eso las tasas son comparables.**

> `score = 1/|S|` si la clave sobrevive · `0` si no · `1/n` si `|S| = 0`

Con clave uniforme, **`E[score] = 1/n` exactamente para toda regla**, sea cual sea su
selectividad: `P(clave ∈ S) = |S|/n` y el pago es `1/|S|`. Verificable por enumeración (lo
hace el test). La convención 0/1 con la abstención puntuando 0 **no** la tiene: el nulo de
cada regla depende de cuánto se abstenga, así que el máximo mezclaba reglas con nulos
distintos. Es además la convención de `acierto()` en los verificadores por ejercicio: las dos
cifras **son** ahora la misma magnitud, que es lo que antes obligaba a declararlas
incomparables y a que un `PASS` del helper conviviera con un `RECHAZADO` del auditor.

#### Helper (la parte genérica ya está resuelta)

`.claude/scripts/bateria_eliminacion.R` — `nueva_regla()`, `evaluar_bateria()`,
`imprimir_bateria()`, `exit_bateria()`. Aporta cobertura, techo nulo y banda; **las reglas
siguen siendo propias de cada ejercicio**, porque la divisibilidad sólo aplica a claves
enteras, el signo sólo donde hay negativos y la posición sólo donde hay disposición
espacial. Veredictos: `PASS` (exit 0) · `BLOQUEA` · `SIN_COBERTURA` · `NO_CONCLUYENTE` ·
`UMBRAL_DEGENERADO` (exit 1).

`UMBRAL_DEGENERADO` es un cuarto guardián que salió al construir el helper. **Cambió de
causa, no de espíritu**: cuando el veredicto era absoluto disparaba si el techo nulo
alcanzaba el umbral (una batería de puro ruido lo cruzaría); ahora dispara cuando el techo
nulo **satura** y deja menos margen que el propio corte —`1 − techo_nulo < corte_canal`—, con
lo que ni una regla omnisciente al 100 % produciría un exceso suficiente. Las dos formas
dicen lo mismo: **el criterio no discrimina nada**. Se llega ahí por *demasiadas reglas para
demasiado pocas versiones*, así que el guardián acabó siendo una defensa de la regla #23:
con 3 versiones y 200 reglas el techo nulo roza el 100 % y el helper se niega a opinar.

```r
source(".claude/scripts/bateria_eliminacion.R")
bateria <- list(
  # `fn` devuelve el CONJUNTO superviviente: lógico de longitud n, un índice, o NA.
  nueva_regla("la mayor",     "magnitud",      function(o) which.max(as.numeric(o))),
  nueva_regla("la unica par", "divisibilidad", function(o) {
    i <- which(as.numeric(o) %% 2 == 0); if (length(i) == 1L) i else NA_integer_ }),
  nueva_regla("dentro de la celda", "signo",   function(o) f_cell(o)),   # lógico: 1/|S|
  # ... una por familia, o declararla no aplicable
)
res <- evaluar_bateria(bateria, opciones, claves,          # cortes: los calibrados
                       familias_no_aplicables = c(signo = "todas las magnitudes son positivas"))
imprimir_bateria(res); quit(status = exit_bateria(res))
```

**El argumento `umbral` sigue existiendo pero NO decide**: se conserva para poder imprimir la
cifra absoluta que los verificadores antiguos citaban, y para dejar constancia impresa de que
ya no gobierna. Un test fija que cambiarlo no puede mover el veredicto.

#### Residuo declarado: la batería RELLENADA

El criterio por exceso tiene un vector de evasión que el absoluto no tenía. **Añadir reglas
que ningún estudiante usaría sube el techo nulo sin mover el máximo observado**, así que el
exceso baja y un canal real puede acabar en la zona gris. El helper **no puede detectarlo**:
no sabe qué regla es plausible para un estudiante, y esa es exactamente la clase de juicio
que §P7 deja —a propósito— en manos de quien escribe la batería.

Dos mitigaciones, ninguna automática:

1. **Regla de conducta**: las reglas de una batería son *estrategias que un estudiante podría
   descubrir*, no relleno. Ampliar la batería para cubrir una familia sin sonda es lo que §P7
   pide; ampliarla con variantes que nadie aplicaría es hacerle trampa al verificador.
2. **La cifra que lo hace visible**: el helper publica también `exceso_atomico` —la regla top
   contra su **propio nulo exacto** (`1/n`), que **no depende de k**—. En una batería
   rellenada esa cifra se queda grande mientras el exceso sobre el techo se desploma; verlas
   juntas delata el relleno. Un test fija el mecanismo para que nadie lo descubra por sorpresa.

#### §P7-A — CRITERIO DE ACEPTACIÓN: la vara es el examen real, no el cero

> **Añadido 2026-08-19 tras un ciclo de 11 pasadas y 4 auditorías (~5 M tokens) sobre un solo
> ítem.** §P7 nació como **diagnóstico** —«¿hay canal?»— y se estaba aplicando como **condición de
> aprobación** —«corregir hasta que dé PASS»—. Esa lectura no tiene condición de parada, y la
> medición dice por qué: **es inalcanzable por construcción.**

**EL DATO QUE FIJA LA VARA** (426 ítems oficiales de Matemáticas deduplicados, batería universal de
28 reglas, pre-registro sellado antes de tocar datos — ver `ref_vara_p7_items_ecuacion`):

| Población | n | exceso | veredicto del helper |
|---|---:|---:|---|
| **Corpus oficial ICFES completo** | 426 | **+4,6 pp** | zona gris → `NO_CONCLUYENTE` |
| Control oficial | 399 | **+5,3 pp** | zona gris → `NO_CONCLUYENTE` |

**Los ítems del ICFES no sacan `PASS` limpio.** Exigírselo a un ejercicio generado es exigirle más
que al examen real, y contradice lo que la propia §P7 declara: *un ítem que no filtra más que el
examen real no se declara defectuoso*.

**LA BATERÍA DE LA VARA ESTÁ VERSIONADA** en `.claude/scripts/bateria_referencia_icfes.R`, con su
pre-registro en `.claude/docs/PRE-REGISTRO-vara-p7.md`. Hasta el 2026-08-19 vivía sólo en un
scratchpad, de modo que las cifras de arriba **no eran reproducibles**. Para comparar contra la vara
hay que usar **esa** batería: un exceso medido con una batería propia del ejercicio **no es
comparable** (§P7-C).

> ⚠️ **CEGUERA MEDIDA Y CORREGIDA — `n1()` vs `nlast()`.** La versión original extraía el valor con
> el **primer** número de la opción. En cualquier molde que empiece por un numeral —«En el paso
> **2**, la ecuación debió ser: 90R = 630.000»— eso lee el **número de paso**, no el valor. Medido:
> **aplicabilidad 0 %** en sus cinco reglas de divisibilidad, un canal real del **41,9 %** pasando
> por debajo, y un `PASS` de **−4,2 pp** sobre un ítem que con la sonda correcta mide **+9,4 pp**.
> El corpus oficial contiene ítems con ese molde, así que **la ceguera afectaba también a la vara**.
> Para el valor, usar **`nlast()`**; `n1()` sólo sirve si la opción es una cifra desnuda.
> Es el mismo modo de fallo que §P7 nombra —*sin sonda leído como sin señal*— dentro del propio
> instrumento que fija la vara.

**Criterio vigente, en tres escalones:**

| Exceso | Veredicto de ACEPTACIÓN |
|---|---|
| ≤ **+5,3 pp** (rango del control oficial) | **ACEPTABLE.** No se persigue más. `NO_CONCLUYENTE` aquí **no** es motivo de rechazo |
| +5,3 a **+8 pp** | Aceptable **declarando el residuo con su cifra** en el reporte |
| > **+8 pp** | Corregir — es la única franja que obliga |

#### §P7-B — Frecuencia sin margen NO es un defecto

**Ninguna sonda de eliminación se persigue sin medir antes su margen relativo.** Si el margen de la
clave sobre su rival más próximo es **< 15 %** (el umbral que este repositorio ya calibró para H1),
la señal **no es explotable** y el residuo se declara cerrado.

Medido en el ciclo de referencia: una tasa del **57,9 %** con margen del **3,3 %** —dos caracteres
sobre setenta— se persiguió durante dos pasadas antes de que una auditoría demostrara que ningún
estudiante puede usarla. **La frecuencia sin margen engaña**, y perseguirla cuesta pasadas enteras.

#### §P7-C — La batería se CONGELA al inicio del ejercicio

Las reglas se fijan **antes** de la primera medición y no se amplían a mitad de ciclo. Añadir sondas
entre pasadas **cambia la vara sobre la marcha**: el máximo sube porque hay más reglas, no porque el
ítem haya empeorado, y el `exceso_atomico` deja de ser comparable entre pasadas.

Si una auditoría descubre una familia sin sonda —lo cual es legítimo y esperable—, la sonda se añade
y **se re-mide el histórico completo con la batería nueva**, o se declara que las cifras anteriores
no son comparables. Lo que está prohibido es encadenar pasadas con baterías distintas y tratar sus
excesos como una serie.

#### §P7-D — Presupuesto: 3 pasadas de corrección

Agotadas, el ejercicio **se cierra con el residuo declarado** y pasa a la decisión del profesor.
Está PROHIBIDO encadenar pasadas indefinidamente contra un umbral.

**La razón no es el coste, es el riesgo medido:** en el ciclo de referencia, la pasada que llevó §P7
de +17,8 a +6,3 pp fue la que volvió **matemáticamente falsa la clave** en el 31,7 % de las
versiones. **Perseguir la diagnosticidad introdujo el único defecto de corrección del ciclo**, y
ningún gate automático lo detectó — lo encontró la auditoría independiente, cuatro pasadas después.

Corolario operativo: **tras cualquier mejora de diagnosticidad, verificar que la clave sigue siendo
verdadera.** No es una comprobación redundante; es la que este repositorio se saltó.

#### Por qué NO se cableó dentro de `validar_diagnosticidad.R`

Se evaluó y se descartó **a propósito**. Sus sondas H1/H2/H3/H3b son genéricas porque miden
propiedades del texto (longitud, prefijo, contenido) que existen en cualquier ítem. Las
familias de P7 **no lo son**: una batería automática de divisibilidad sobre un ítem de
opciones textuales no aplicaría nunca, y el script imprimiría un `PASS` sobre una familia
que jamás sondeó — recreando exactamente «sin sonda» reportado como «sin señal», el defecto
que esta sección existe para cerrar. El repositorio ya aprendió esa lección con H3b:
**declarar la ceguera vale más que añadir una sonda débil.** Por eso lo genérico
—calibración y cobertura— vive en el helper, y las reglas se escriben por ejercicio.

**Test:** `tests/testthat/test_bateria_eliminacion.R` (33 aserciones). Su control decisivo
es el caso **«mismos datos, sonda retirada»**: con el canal real al 100 % en divisibilidad,
al quitar esa sonda la batería reporta **19 %** —una cifra baja y tranquilizadora— y el
helper igualmente **se niega a declarar PASS**.

---

## Patrón Correcto

### ✅ Aleatorizar los parámetros que determinan la respuesta

```r
# ✅ CORRECTO — la respuesta correcta varía entre versiones
distancia_total    <- sample(60:150, 1)
angulo             <- sample(25:70, 1)
distancia_avanzada <- sample(10:(distancia_total - 10), 1)
# → la opción correcta (el diagrama con ESOS valores) difiere en cada semilla
```

Cada variable que influye en **cuál opción es la correcta** (qué diagrama mostrar, qué valor calcular, qué afirmación aplicar) DEBE depender de al menos una variable aleatoria.

### ✅ Generar gráficos dinámicamente por versión

```r
# ✅ CORRECTO — gráfico regenerado con los parámetros aleatorios de esta semilla
p_correcto <- ggplot(...) +
  geom_segment(aes(x = 0, y = 0, xend = distancia_avanzada, yend = 0)) +
  geom_arc(r = distancia_total, angle = angulo) +
  ...
ggsave("opcion_correcta.png", p_correcto, ...)
```

### ✅ Verificar diversidad sustantiva antes de promover

```bash
# Verificación obligatoria en el orquestador (paso 9)
Rscript .claude/scripts/validar_diversidad_sustantiva.R ejercicio.Rmd --n 100
```

Si la salida contiene `ERR_DIV_COSMETICA` → BLOQUEAR. El ejercicio tiene diversidad solo cosmética y no puede avanzar a aprobación.

---

## Detección Automática

### Script: `validar_diversidad_sustantiva.R`

**Ubicación**: `.claude/scripts/validar_diversidad_sustantiva.R`

Ejecuta `n` versiones del `data_generation` con semillas dispersas, extrae un **fingerprint del CONTENIDO de la respuesta correcta** (no de su posición ni del render completo), y emite:

| Veredicto | Exit | Descripción | Acción requerida |
|-----------|------|-------------|------------------|
| `PASS` | 0 | La respuesta correcta varía suficientemente | Ninguna |
| `ERR_DIV_COSMETICA` | 1 | La respuesta correcta es INVARIANTE | **BLOQUEAR — DEFECTO CRÍTICO** |
| `WARN_DIV_BAJA` | 0 | Varía, pero poco (< 30% de variaciones distintas) | Revisar rangos de aleatorización |
| `WARN_DIV_INDET` | 0 | No se pudo identificar la respuesta correcta en el entorno | Verificación manual |

**Uso**:

```bash
Rscript .claude/scripts/validar_diversidad_sustantiva.R <ruta_al_.Rmd> --n 100
```

El script NO requiere que el `.Rmd` renderice a PDF/HTML — extrae y evalúa directamente el chunk `data_generation`. Es barato y rápido (40 evals de data_generation, sin pdflatex).

### Hook: FASE 2N — Detección estática barata (post-exams2)

El hook `post-exams2-validation.sh` ejecuta FASE 2N con detección estática (grep, sin correr el script pesado):

- Si el bloque `data_generation` usa `file.copy(` para PNGs que se referencian como opciones, o no aparece ninguna función de aleatorización (`sample(`/`runif(`/`rnorm(`/`rbinom(`/`rpois(`) en `data_generation` → emite `WARN_DIV_ESTATICA` (ADVERTENCIA no bloqueante a nivel hook).
- Esta fase NO ejecuta `validar_diversidad_sustantiva.R` (la validación dinámica es responsabilidad del orquestador en el paso 9, por coste/timeout del hook).

### Integración en orquestadores (paso 9)

El paso 9 (`validar_diversidad`) de ambos orquestadores (`orquestador-schoice.md` y `orquestador-cloze.md`) exige, ADEMÁS del conteo tradicional de versiones:

1. Ejecutar `Rscript .claude/scripts/validar_diversidad_sustantiva.R <ruta> --n 100`.
2. Si `ERR_DIV_COSMETICA` (exit 1) → **DEFECTO BLOQUEANTE** — no avanzar a aprobación.
3. Solo si `PASS` o `WARN_DIV_BAJA`/`WARN_DIV_INDET` (exit 0) → continuar.

---

## Códigos de Error

| Código | Tipo | Descripción | Severidad |
|--------|------|-------------|-----------|
| `ERR_DIV_COSMETICA` | Script (exit 1) | Respuesta correcta INVARIANTE entre versiones | **BLOQUEANTE** |
| `WARN_DIV_BAJA` | Script (exit 0) | La respuesta varía pero poco (< 30%) | Informativo |
| `WARN_DIV_INDET` | Script (exit 0) | No se pudo identificar la respuesta correcta para fingerprint | Informativo |
| `WARN_DIV_ESTATICA` | Hook FASE 2N | `file.copy(` para PNGs de opciones o ausencia de funciones aleatorias en data_generation | Advertencia (hook) |
| `BLOQUEA` (§P7) | `bateria_eliminacion.R` (exit 1) | El **exceso** sobre el techo nulo alcanza el corte de canal (**≥ +8 pp**) incluso descontando 2 sd del ruido medido | **BLOQUEANTE** |
| `SIN_COBERTURA` (§P7) | `bateria_eliminacion.R` (exit 1) | Una familia de dimensión sin sonda ni declaración: la batería no mide, sólo calla | **BLOQUEANTE** |
| `NO_CONCLUYENTE` (§P7) | `bateria_eliminacion.R` (exit 1) | El exceso cae en la **zona gris** (+2 a +8 pp), o el ruido medido es tan ancho que la muestra no acredita ni la ausencia | **NO es PASS** |
| `UMBRAL_DEGENERADO` (§P7) | `bateria_eliminacion.R` (exit 1) | El techo nulo **satura**: deja menos margen que el corte, así que ni una regla omnisciente lo cruzaría. Demasiadas reglas para demasiado pocas versiones | **BLOQUEANTE** |

---

## Tests Asociados

| Test | Suite | Verifica |
|------|-------|---------|
| `tests/testthat/test_diversidad_sustantiva.R` | Nueva (suite #20) | Fixture con respuesta FIJA → exit 1 / `ERR_DIV_COSMETICA`; fixture con respuesta ALEATORIA → exit 0 / `PASS` |
| `tests/testthat/test_bateria_eliminacion.R` | Suite #32 | §P7: control positivo (canal real cazado y atribuido a su familia), control negativo (ítem sano = `PASS`), **«mismos datos, sonda retirada»** (`SIN_COBERTURA` con una cifra baja y tranquilizadora), **«más reglas de ruido suben la tasa pero NO el exceso»**, nulo exacto de la convención por enumeración, zona gris, muestra corta, techo nulo saturado, y que `umbral` no pueda mover el veredicto. **79 aserciones**, verificadas por mutación (5 mutantes, 5 cazados) |

---

## Antipatrones PROHIBIDOS (resumen)

| Antipatrón | Por qué está prohibido |
|-----------|----------------------|
| Literales numéricos hardcoded como parámetros de la respuesta | La respuesta es invariante — diversidad cero |
| `file.copy(png_estatico, opcion_X.png)` en data_generation | El contenido visual de la opción no cambia entre semillas |
| "288/300 versiones únicas" como evidencia de diversidad | Mide el envoltorio (contextos, orden), no la sustancia |
| Confiar en el detractor para detectar este bug | El detractor puede "simular" en vez de ejecutar el chunk real, alucinando estructura de código |

---

## Excepciones (NINGUNA)

No hay excepciones a esta regla. Incluso ejercicios cuya respuesta correcta tiene rango limitado (p.ej. solo 3 posibles valores) DEBEN aleatorizar entre esos 3 valores — aunque `WARN_DIV_BAJA` sea esperable, `ERR_DIV_COSMETICA` no lo es nunca.

Si por diseño pedagógico un ejercicio necesita comparar exactamente los mismos datos siempre (caso muy excepcional), documentar el ADR correspondiente y obtener aprobación humana explícita antes de eximir el ejercicio.

---

## Referencias

- `validar_diversidad_sustantiva.R` — `.claude/scripts/validar_diversidad_sustantiva.R`
- Incidente 2026-06-27 — ejercicio `desplazamiento-avion-aeropuerto`
- Incidente 2026-07-28 (P6) — ejercicio `desplazamiento-avion-aeropuerto` — Error 25 en `patrones-errores-conocidos.md`
- `feedback_diversidad_cosmetica.md` — memoria del proyecto
- `feedback_detractor_alucina_codigo.md` — por qué el detractor no es suficiente
- `feedback_fuga_nombre_archivo_moodle.md` — memoria del proyecto (P6)
- Regla #21 (`familias-soluciones-rmd.md`) — Familia 1 (sin cuelgue), Familia 5 (safe_sample)
- Regla `graficos-como-opciones.md` §"Canal de fuga: el nombre de archivo delata la respuesta en Moodle"

---

**Versión:** 1.6
**Fecha:** 2026-08-15
**Estado:** ACTIVO Y OBLIGATORIO
**Excepciones:** NINGUNA
**Aplica a:** todo archivo `.Rmd` SCHOICE o CLOZE en desarrollo o revisión.

### Cambios v1.6 (2026-08-15) — §P7: el veredicto pasa de la TASA al EXCESO

> El umbral del 70 % duró un día. Una medición sobre **468 ítems oficiales únicos** de 6
> cuadernillos de Matemáticas ICFES demostró que **medía la cosa equivocada**: en buena parte,
> el tamaño de la batería. Este cambio no se hace para que un ejercicio pase — el ejercicio
> que lo destapó sigue **RECHAZADO**, y con más errores que antes.

- **LA DERIVA, MEDIDA**: la misma población da **27,4 %** con 19 reglas y **34,8 %** con 25, y
  su techo nulo se mueve con ella (27,0 → 27,8). **El exceso es invariante; la tasa no.**
  Las cuatro comparaciones limpias de la tabla caen todas en |exceso| ≤ 2,1 pp.
- **EL UMBRAL ERA INALCANZABLE, NO SEVERO**: ningún ítem de ninguna población, con 19 a 91
  reglas, superó el **47,0 %**. Pedir 70 % era pedir aislar la clave en 3 de cada 4
  instancias. Una puerta que no se puede cruzar se aprende a ignorar igual que una que
  siempre está en rojo — es la misma patología que la FASE 2G en falso ROJO permanente.
- **DOS UMBRALES CONTRADICTORIOS SOBRE EL MISMO ARTEFACTO, cerrados**: el helper daba `PASS`
  (43,5 % < 70 %) mientras el `auditoria_propia.R` del ejercicio **RECHAZABA** por su propio
  45 %. Ahora los dos juzgan por el exceso y **leen los cortes del helper**, que es la única
  fuente; el auditor hace `source()` del helper y **aborta si no lo encuentra**, para que la
  duplicación no pueda reaparecer en silencio.
- **CORTES CALIBRADOS, no elegidos**: ≤ +2 pp sin canal · +2 a +8 pp **zona gris** · ≥ +8 pp
  canal real. El +2 es ~1 sd del ruido a N = 100 (medido: media +0,1 pp, sd 2,2 pp); el +8 es
  ~p99,5 de ese ruido **y** queda justo por encima del **+7,0 pp que filtran los propios
  cuadernillos oficiales**. La referencia no es «cero fuga», es «no más que el examen real».
- **EL +7,0 pp DEL CORPUS NO ES RUIDO**: a N = 468 la sd baja a 1,0 pp, así que son ~7 sd. Es
  el **sesgo posicional real** de los cuadernillos, que aparece porque el nulo es uniforme
  sobre las posiciones. Se mantiene uniforme **a propósito**: permutar el vector de claves
  observado volvería invisible el canal posicional puro, que es justo lo que la familia
  `posicion` existe para cazar.
- **CONVENCIÓN CON NULO EXACTO**: `score = 1/|S|` · `0` · `1/n` si nadie sobrevive da
  `E[score] = 1/n` **exactamente para toda regla**, sea cual sea su selectividad. La 0/1 con
  la abstención valiendo 0 no lo cumple —el nulo de cada regla dependía de cuánto se
  abstuviera— y era, junto con los umbrales, la razón declarada de que las dos cifras fueran
  «incomparables aunque coincidan». Ahora son la misma magnitud.
- **LA BANDA DEJA DE SER UNA CONSTANTE**: pasa a `2 × sd(techo nulo)` **medido en la corrida**.
  Un `PASS` exige que el exceso no alcance el corte ni sumándole 2 sd; un `BLOQUEA`, que no
  baje del corte de ruido restándoselas. Efecto colateral útil: a **N = 30** la sd sube a
  4-5 pp y ni la ausencia se acredita, así que **la muestra estándar N = 100 (regla #23) pasa
  a ser exigible por el estadístico**, no sólo por decreto.
- **`UMBRAL_DEGENERADO` cambia de causa, no de espíritu**: ahora dispara si `1 − techo_nulo <
  corte_canal`, es decir si el techo **satura** y ni una regla omnisciente produciría exceso
  suficiente. Se llega ahí con demasiadas reglas para demasiado pocas versiones.
- **LA LAGUNA DEL CIERRE CRUZADO, cerrada y medida**: el bloque `(K4)` del ejercicio reportaba
  **67,0 %** con un techo nulo que (a) se estimaba con `max` sobre 3 réplicas —estimador
  **sesgado al alza**, y un techo alto **rebaja** el exceso, o sea favorece al artefacto
  auditado— y (b) **no decidía nada**. Recalibrado con `mean` sobre 8 réplicas:
  **techo nulo 36,3 % (sd 1,0 pp) → EXCESO +30,8 pp**, casi 4× el corte. Mismo tratamiento a
  `(K3)` (+25,9 pp) y a `(L)` (+14,2 pp), donde además el nulo se calculaba sobre la población
  **con** canónicas y se comparaba contra un observado **sin** ellas.
- **DOS CRITERIOS, NINGUNO SUSTITUYE AL OTRO**: el **exceso** dice si hay canal (existencia
  estadística, comparable entre bloques y poblaciones); el **marginal sobre la deducción
  necesaria** dice cuánto añade el atajo a quien ya razona (pregunta pedagógica). Cualquiera
  puede rechazar por su cuenta; ninguno absuelve de lo que dice el otro.
- **TEST: 33 → 79 aserciones**, con dos controles nuevos que son la lección hecha aserción:
  **«más reglas de ruido suben la tasa pero NO el exceso»** (y se comprueba que con un umbral
  absoluto intermedio las dos baterías caerían a lados distintos sobre los MISMOS datos), y el
  **nulo exacto por enumeración** para n = 3, 4, 5 y todo |S|. Verificado por **mutación: 5
  mutantes, 5 cazados** (veredicto por tasa absoluta, corte descalibrado, abstención
  puntuando 0, techo nulo por máximo, margen de ruido retirado), sobre **copias** del helper.
- **UN BUG DEL PROPIO FIXTURE, cazado por su control**: las reglas de ruido derivaban su
  índice de `i` con aritmética lineal y **colapsaban en cuatro comportamientos distintos**, así
  que ni el máximo subía ni el techo nulo saturaba: el fixture decía tener k reglas y tenía
  cuatro. Se detectó porque `UMBRAL_DEGENERADO` no disparaba con 200 reglas y 3 versiones.
  Ahora cada regla lleva su tabla de consulta sorteada al construirla.
- **RESIDUO DECLARADO 1 — la batería rellenada**: el criterio por exceso abre un vector de
  evasión que el absoluto no tenía. Añadir reglas que nadie usaría **sube el techo nulo sin
  mover el máximo**, así que erosiona el exceso y puede empujar un canal real a la zona gris.
  El helper no puede detectarlo —no sabe qué es plausible— y por eso publica `exceso_atomico`
  (la regla top contra su propio nulo exacto `1/n`, independiente de k): en una batería
  rellenada esa cifra se queda alta mientras el exceso se desploma. Test que fija el mecanismo.
- **RESIDUO DECLARADO 2**: los bloques `(I)` (reglas atómicas) y `(K)` (pares) del auditor siguen
  con umbrales absolutos (45 % y 50 %) sin techo nulo propio. También son máximos sobre
  baterías, así que arrastran el mismo defecto de escala; hoy **rechazan igualmente**, de modo
  que la deuda no cambia ningún veredicto, pero no está saldada.

### Cambios v1.5 (2026-08-15) — §P7: cierre por familias de dimensión

- **NUEVO §P7**, y es de otra naturaleza que P1–P6: no nombra un canal de fuga, nombra un
  defecto **del verificador**. La lección ya había aparecido en **dos ejercicios distintos**
  y sólo vivía en el verificador de uno de ellos.
- **Lo que la originó**: una batería de seis reglas intra-celda donde **ninguna tocaba la
  divisibilidad**, que era justo donde estaba el canal real (**47,4 %**). Se reportó como
  «sin señal». Principio: *una batería incompleta no mide «sin señal», mide «sin sonda»*.
- **Tres exigencias, ninguna opcional**: (1) **cobertura** de las seis familias —magnitud,
  divisibilidad, signo, posición, formato, léxico—, con declaración justificada para las
  inaplicables; (2) **techo nulo** por permutación de la clave con las reglas intactas
  (medido en el incidente: máximo **69,6 %** contra techo **34,8 %**, exceso **+35 pp** —
  sin esa calibración el número no significa nada); (3) **banda de incertidumbre** de 5 pp,
  porque a N = 100 un máximo sobre ~19 reglas no es reproducible tirada a tirada.
- **NUEVO HELPER**: `.claude/scripts/bateria_eliminacion.R`. Aporta sólo la parte genérica
  —cobertura, techo nulo, banda—; **las reglas siguen siendo por ejercicio**, y se explica
  por qué eso no es pereza sino diseño.
- **CUARTO GUARDIÁN, aparecido al construirlo**: `UMBRAL_DEGENERADO`. Si el techo nulo
  alcanza el umbral, **el umbral no discrimina**: hasta una batería de ruido lo cruzaría.
  Un gate que siempre falla se aprende a ignorar igual que uno que nunca falla.
- **DECISIÓN DE ALCANCE DOCUMENTADA**: se evaluó cablearlo dentro de
  `validar_diagnosticidad.R` y **se descartó a propósito**. Una batería automática de
  divisibilidad sobre opciones textuales no aplicaría nunca y el script imprimiría `PASS`
  sobre una familia que jamás sondeó — recreando el defecto que §P7 existe para cerrar.
  Mismo criterio que fijó H3b: declarar la ceguera vale más que añadir una sonda débil.
- **NUEVA SUITE (32 en el runner)**: `test_bateria_eliminacion.R`, 33 aserciones. Su control
  decisivo es **«mismos datos, sonda retirada»**: con el canal real al 100 %, quitar esa
  sonda hace que la batería reporte **19 %** y el helper **siga negándose a dar PASS**.
- **DOS BUGS DEL PROPIO TEST, cazados por sus controles**: (a) `expect_lt`/`expect_gt` **no
  aceptan `info =`** —tres aserciones reventaban por eso, no por el helper—; y (b) el caso
  de la banda se construyó acercando el umbral al máximo de un ítem **sano**, lo que empuja
  el umbral **por debajo del techo nulo** y hace que el veredicto correcto pase a ser
  `UMBRAL_DEGENERADO`. El helper tenía razón y el escenario estaba mal: hizo falta un canal
  **parcial**. Además, sortear ese canal con probabilidad 0,66 aterrizó en **61 %** y sacó
  al test de su propia banda — es la irreproducibilidad que la exigencia (3) describe,
  reproducida dentro del test que la prueba; el fixture pasa a usar un conteo **exacto**.

### Cambios v1.4 (2026-08-10)
- **NUEVA SUBSECCIÓN en §P4-bis — «El molde uniforme de opciones ciega a H2 y a H3»**, con la tabla
  del mecanismo verificada contra el código de `validar_diagnosticidad.R` (no contra su
  documentación): `pw` descarta el `¿`, H2 sale 0 % por construcción, la guarda de H3 deja `pwc`
  vacío y el `if (length(pwc) >= 5L)` hace que **la fila H3 ni se imprima**.
- **NUEVA SONDA H3b** (cross-versión, por **contenido normalizado** de la clave: sin dígitos ni
  puntuación). Mide la invariancia del **tipo** de clave cuando el prefijo no puede.
- **Calibración de relevo, deliberada**: H3b bloquea **solo** si el prefijo es uniforme en ≥90 % de
  las versiones; si H3 puede medir, se queda en aviso. Con H3b bloqueando siempre, un fixture de
  `test_diagnosticidad.R` que existe para probar que H1 **no** dispara pasaba a ROJO. Una sonda
  nueva que cambia el veredicto de casos ya revisados no es más rigor: es ruido.
- **La ceguera se declara siempre**, dispare o no la sonda: `H2/H3 CIEGAS` con su porcentaje, y
  `H3b: NO MEDIBLE` cuando la firma tampoco discrimina, exigiendo verificador propio del ejercicio.
- **4 tests nuevos** en `tests/testthat/test_diagnosticidad.R` (10 → 24 aserciones), incluido el
  control de que el fixture prueba lo que dice (H2 en 0 % y H3 sin medir) y el de no-regresión de
  la calibración de relevo.
- **Origen**: dry-run de `MAT-2026-1-010`, ítem cuyas cuatro opciones son preguntas `¿Cuál es…?`.

### Cambios v1.3 (2026-08-09)
- **NUEVA SUBSECCIÓN en §P4-bis — «La propia defensa crea deuda»**: tres verificaciones
  obligatorias DESPUÉS de introducir una clave alternativa. La v1.2 describía qué defensa aplicar
  pero no que aplicarla **cambia la premisa sobre la que se escribió el pool existente**.
- **Origen**: `area-jardin-lote-porcentaje-n4` (2026-08-09). Los tres puntos fallaron a la vez
  con el arsenal completo en verde: (1) la guarda anti-colisión solo cubría la clave vigente
  (3/600 versiones con dos opciones del mismo rango y veredictos opuestos); (2) los distractores
  escritos para la clave única quedaron declarando un veredicto que su justificación contradice
  (81/600 = 13,5 %); (3) al corregir (2) excluyendo el único distractor más largo que la clave,
  la clave quedó siendo **determinísticamente** la más larga de su rama — 100 % dentro de la
  rama, `PASS` en el agregado de H1, **50,5 % de acierto sin razonar** frente al 25 % de azar.
- **Punto ciego del arsenal declarado**: H1/H2 promedian sobre versiones **sin condicionar por
  rama**. En un ítem con clave alternante las dos ramas son estructuralmente distintas y un
  reparto 100 %/0 % se lee como ~50 %. Hoy la medición por rama es manual.
- **Advertencia de la señal inversa**: igualar longitudes hasta que la clave no sea NUNCA la más
  larga tampoco es neutro — habilita una heurística de eliminación que sube el azar a 33 %.
- **Errores nuevos en el catálogo**: 28, 29 y 30 de `patrones-errores-conocidos.md`.

### Cambios v1.2 (2026-08-08)
- **NUEVO SUB-PATRÓN PROHIBIDO**: §P4-bis — Variante **semántica** de P4: el **veredicto** de la
  clave es invariante entre versiones aunque su **valor** cambie. Ocurre en ítems de conclusión
  binaria («Sí, porque…»/«No, porque…», verdadero/falso, aumenta/disminuye) cuando la afirmación
  evaluada es falsa por construcción.
- **Origen**: `area-jardin-lote-porcentaje-n4` — **60/60 versiones con clave "No"** y todo el
  arsenal en verde. §P4 ya describía el caso en una frase («la afirmación correcta siempre con
  cierta estructura») sin que **nada** lo midiera.
- **Por qué las defensas previas no lo veían**: H2 exige que la clave sea la única con su prefijo
  dentro de una versión — con balance 2+2 nunca lo es (0 %); `validar_diversidad_sustantiva.R` mide
  el VALOR de la clave, que sí variaba; y el balance 2+2 es **intra-versión**, se cumple en todas
  mientras el veredicto sigue constante. Impacto: 25 % → 50 % de acierto por azar.
- **Defensa cableada nueva**: sonda **H3** de `.claude/scripts/validar_diagnosticidad.R`, la
  **primera cross-versión** del arsenal — 100 % → `ERR_DIAG_SUPERFICIAL` (exit 1, bloqueante);
  ≥90 % → aviso.
- **Corrección de deriva documental**: §P4-bis entró con el commit `162063c0` (2026-08-08) sin
  actualizar este pie, que siguió declarando v1.1 (2026-07-28) sobre un cuerpo ya modificado.
  Detectado al auditar los orquestadores el 2026-08-08.

### Cambios v1.1 (2026-07-28)
- **NUEVO PATRÓN PROHIBIDO**: P6 — Fuga de la respuesta por metadato NO VISUAL (nombre de archivo, orden alfabético/de creación, id del elemento, cualquier atributo que revele el rol de la opción)
- **Origen**: Error 25 (`patrones-errores-conocidos.md`) — PNGs con nombres semánticos (`diagrama_correcta.png`) visibles en texto plano dentro del XML de `exams2moodle()`, aunque invisibles en HTML/PDF (imagen embebida/base64)
- **Verificación asociada nueva**: `exams2moodle()` + `grep` del XML resultante — HTML/PDF NO son suficientes para detectar este patrón
- **Referencias cruzadas**: regla `graficos-como-opciones.md` v6.0 §"Canal de fuga: el nombre de archivo delata la respuesta en Moodle"

### Cambios v1.0 (2026-06-27)
- Versión inicial: patrones P1-P5, script `validar_diversidad_sustantiva.R`, hook FASE 2N (`WARN_DIV_ESTATICA`)
- Origen: incidente `desplazamiento-avion-aeropuerto` — 288/300 versiones únicas con respuesta correcta invariante (diversidad cosmética)
