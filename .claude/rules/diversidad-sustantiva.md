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
Rscript .claude/scripts/validar_diversidad_sustantiva.R ejercicio.Rmd --n 40
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
Rscript .claude/scripts/validar_diversidad_sustantiva.R <ruta_al_.Rmd> --n 40
```

El script NO requiere que el `.Rmd` renderice a PDF/HTML — extrae y evalúa directamente el chunk `data_generation`. Es barato y rápido (40 evals de data_generation, sin pdflatex).

### Hook: FASE 2N — Detección estática barata (post-exams2)

El hook `post-exams2-validation.sh` ejecuta FASE 2N con detección estática (grep, sin correr el script pesado):

- Si el bloque `data_generation` usa `file.copy(` para PNGs que se referencian como opciones, o no aparece ninguna función de aleatorización (`sample(`/`runif(`/`rnorm(`/`rbinom(`/`rpois(`) en `data_generation` → emite `WARN_DIV_ESTATICA` (ADVERTENCIA no bloqueante a nivel hook).
- Esta fase NO ejecuta `validar_diversidad_sustantiva.R` (la validación dinámica es responsabilidad del orquestador en el paso 9, por coste/timeout del hook).

### Integración en orquestadores (paso 9)

El paso 9 (`validar_diversidad`) de ambos orquestadores (`orquestador-schoice.md` y `orquestador-cloze.md`) exige, ADEMÁS del conteo tradicional de versiones:

1. Ejecutar `Rscript .claude/scripts/validar_diversidad_sustantiva.R <ruta> --n 40`.
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

---

## Tests Asociados

| Test | Suite | Verifica |
|------|-------|---------|
| `tests/testthat/test_diversidad_sustantiva.R` | Nueva (suite #20) | Fixture con respuesta FIJA → exit 1 / `ERR_DIV_COSMETICA`; fixture con respuesta ALEATORIA → exit 0 / `PASS` |

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

**Versión:** 1.4
**Fecha:** 2026-08-10
**Estado:** ACTIVO Y OBLIGATORIO
**Excepciones:** NINGUNA
**Aplica a:** todo archivo `.Rmd` SCHOICE o CLOZE en desarrollo o revisión.

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
