# Especificación — CLOZE **v2** de `excedente-almuerzo-proporcional-n4`

> Documento de entrada para el `orquestador-cloze`. **Leer completo antes del paso 3
> (`generacion_rmd`).** El contrato paramétrico de esta familia YA EXISTE y está verificado:
> este documento lo pasa como **INSUMO**, no como algo a re-derivar. Lo único que hay que
> diseñar de nuevo son los seis puntos de la §3.

**Ruta destino:** `A-Produccion/01-En-PreDesarrollo/excedente-almuerzo-proporcional-n4/cloze_v2/`
(directorio nuevo, hermano de `cloze/`; el v1 se conserva intacto como línea base comparable).

**Nombre del ejercicio:** `excedente_almuerzo_numerico_variacional_argumentacion_n4_cloze_v2`

---

## 1. Insumos — leer ANTES de escribir una sola línea

| Insumo | Ruta | Qué aporta |
|---|---|---|
| Especificación de la familia | `../ESPECIFICACION.md` | §1 origen y fuentes, §2 metadatos oficiales, §3 instancia canónica verbatim, §5 invariantes I-1..I-8, §6 pool de errores `PRO-SUF-01..06`, §7 reglas del repo |
| HANDOFF del v1 | `cloze/HANDOFF.md` | decisión de diseño Ruta (B), invariantes propias C-1..C-3, resultados medidos, **y los dos caveats del verificador** |
| Implementación de referencia | `cloze/…_cloze_v1.Rmd` | estructura de 6 partes, `exclozetype`, plantillas narrativas, pools |
| Verificador de referencia | `cloze/verificar_render.R` | 6 fases, 3 mutantes con contrato de sonda |

**No re-derivar** el espacio paramétrico (P, n, T, c), la aritmética (E = T − P, q = P/n,
a = c/T·E), la instancia canónica ni el pool `PRO-SUF-*`. Están fijados y verificados sobre
300 semillas.

**C-1 se mantiene: el CLOZE es SIEMPRE TIPO 1** (los cuatro datos dados). No es una omisión
respecto del SCHOICE hermano, que sí alterna 1/2a/2b: en TIPO 2a el excedente E no es
computable (falta T) y en 2b el aporte a tampoco (falta c), así que los gaps `num` de las
Partes 1-2 no tendrían respuesta. La dimensión de **suficiencia de información** —que es el
descriptor D4.9— se evalúa en los gaps de razonamiento (Partes 3-6), y en particular en la
Parte 6, que retira hipotéticamente T, c o n. Esa es la Ruta (B) del v1 y se conserva.

---

## 2. Qué se conserva sin cambios respecto del v1

- Metadatos oficiales (§2 de `ESPECIFICACION.md`): D4.9, Argumentación, Numérico-variacional,
  Nivel 4, DOK 3, Bloom `Evaluar`.
- Estructura de 6 partes y `exclozetype: num|num|schoice|mchoice|schoice|schoice`.
- Invariantes **I-1..I-7** (I-8 es N/A por C-1) y **C-1..C-3**.
- Instancia canónica I-6 y su comprobación carácter por carácter.
- Las tres mutaciones del verificador (clave falsa, E negativo, `dato_retirado=n` con
  respuesta "No") **con contrato de sonda**: cada mutante declara qué sonda debe matarlo y la
  fase falla si muere por otra.

---

## 3. Qué DEBE cambiar — defectos medidos del v1 (2026-08-06)

Cada punto trae la medición que lo motiva. No son hipótesis.

### D1 · Diagnosticidad de las opciones (V9) — construir la paridad, no corregirla después

Medido con `validar_diagnosticidad.R --n 40` sobre el v1:

| Gap | Sonda | Tasa | Margen mediano |
|---|---|---|---|
| p3 | única más larga | **100 %** de las versiones | 7 % |
| p5 | única más corta | 72 % | 12 % |
| p6 | única más larga | **100 %** | 5 % |

Ninguna dispara hoy porque el margen queda por debajo del 15 %, pero la señal de ORDEN está
al 100 % en dos gaps: la correcta ocupa el extremo siempre. En el v1 eso se arregló **a
posteriori**, recortando texto hasta igualar longitudes. En el v2 la paridad se construye:

1. Cada gap de selección única (`p3`, `p5`, `p6`) redacta sus opciones con longitudes
   **parejas por diseño**, y `data_generation` lo comprueba:
   `stopifnot(abs(nchar(correcta) - median(nchar(otras))) / median(nchar(otras)) < 0.10)`.
2. La correcta **no** puede ser sistemáticamente el extremo. Nueva invariante propia:
   **C-4** — sobre ≥ 300 semillas, para cada gap de selección única, la opción correcta es la
   única más larga en ≤ 60 % de las versiones y la única más corta en ≤ 60 %, y su margen
   relativo H1 nunca alcanza el 15 %. Se verifica en `verificar_render.R`, no solo con el
   script del paso 9.
3. Paso 9: `validar_diagnosticidad.R --n 40` debe dar `PASS` **y** la `NOTA DE ORDEN` debe
   transcribirse al reporte. Un `PASS` con nota al 100 % no es "sin señal": es "señal
   demasiado pequeña". Con C-4 no debería aparecer ninguna nota al 100 %.

### D2 · Ortografía — escribirla bien de entrada

El corrector ampliado encuentra en el v1 **61 correcciones automáticas** que la versión
anterior del diccionario no veía (`Sí,` ×11, `-ción` ×29, `demás` ×5, `consumió` ×3,
`pequeño`, `explícitamente`, `están`…) más 20 casos marcados `REVISION_MANUAL` de los que 19
eran defectos reales (`fórmula` ×12, `sería` ×3, `cuánto`, `¿Cómo`, `Por qué`, interrogativas
sin `¿`). El v1 se declaró "ortografía limpia" en su día porque el corrector estaba ciego.

Obligación en el v2:

- Todo texto visible al estudiante se escribe **con tildes y con ñ** desde el primer borrador.
- Criterio de cierre: `corregir_ortografia_espanol.R` da **0 correcciones automáticas** y cada
  caso `REVISION_MANUAL` se declara uno por uno en el reporte como *defecto corregido* o
  *falso positivo* (con la frase que lo justifica). "Limpio" sin esa declaración no vale.
- Atención particular a: pretéritos (`consumió`, `aportó`), `fórmula`, `sería`, `demás`,
  `¿Cuál`/`¿Cómo`/`¿Cuánto` con signo de apertura, `ñ` (`pequeño`, `diseño`, `mañana`).

### D3 · Pools del tamaño correcto (Error 27 / pre-flight 24)

En el v1, `afirmaciones_verdaderas` tiene **3 entradas para 3 huecos**: las tres verdaderas de
la Parte 5 son SIEMPRE las mismas y solo cambia su orden. Es exactamente el patrón que el
pre-flight 24 prohíbe, y ningún validador lo detecta. Lo mismo, en otra forma, en la Parte 4:
sus 5 opciones son un conjunto estático.

Obligación en el v2:

| Parte | Pool mínimo | Selección |
|---|---|---|
| 3 (schoice) | ≥ 5 justificaciones erróneas (del pool `PRO-SUF-*`) | 3 por versión con `sample()` |
| 4 (mchoice) | 3 datos necesarios (T, P, c) + ≥ 5 no necesarios | los 3 necesarios + 2 no necesarios, mezclados |
| 5 (schoice) | ≥ 5 afirmaciones falsas **y** ≥ 6 verdaderas | 1 falsa + 3 verdaderas |
| 6 (schoice) | ≥ 3 distractores por cada valor de `dato_retirado` | 3 por versión |

Consecuencia para el verificador: **KEY_P4 no puede seguir comparando contra el patrón fijo
`c(TRUE,TRUE,TRUE,FALSE,FALSE)`**. Debe clasificar cada opción **por contenido** (¿es uno de
los tres datos necesarios?) y comparar con `sol_p4`. Un patrón fijo de posiciones deja de ser
comprobación en cuanto el conjunto varía.

### D4 · Arreglar los tres defectos del verificador heredado

Los dos primeros están documentados en `cloze/HANDOFF.md` §Caveats; el tercero se encontró el
2026-08-06 al parchear el v1:

1. **Guarda inalcanzable (Fase 4).** `env_mut_b$E <- -50000L` seguido de `if (env_mut_b$E > 0)`:
   la condición se evalúa sobre un dato al que se le acaba de forzar la propiedad contraria, no
   puede fallar nunca. Es código muerto que se documentaba como guarda. **Eliminarlo** o
   sustituirlo por una comprobación que sí pueda fallar (p. ej. que la mutación de TEXTO haya
   hecho match antes de knitear).
2. **I-1 tautológica en Fase 1.** `E` se lee de `combos`, tabla ya filtrada por `E > 0`, así que
   "I-1: 0 errores en 300 semillas" no discrimina nada. **Recalcular E desde P y T leídos del
   entorno knitteado**, que es lo que ve el estudiante, no desde la tabla que ya filtró.
3. **Sonda muerta en KEY_P5.** El indicador `"La proporcion del consumo individual"` se comparaba
   con `fixed = TRUE` contra un texto que el `.Rmd` emite como `"La proporción…"`: nunca podía
   coincidir, así que KEY_P5 comprobaba 2 de las 3 afirmaciones verdaderas y firmaba las 3.
   En el v2, **toda sonda literal debe verificarse contra el texto realmente emitido** (extraer
   la cadena del entorno knitteado, no transcribirla a mano).

Regla general que estos tres casos ilustran y que el v2 debe respetar: **una comprobación que no
puede fallar no es una comprobación**. Por cada sonda nueva, demostrar que existe una mutación
que la mata.

### D5 · Diversidad por gap (V8) declarada, no agregada

`validar_diversidad_sustantiva.R --n 40` en modo CLOZE midió 6/6 gaps con `PASS` en el v1, pero
la propia V8 advierte de su límite: la huella de un gap `schoice`/`mchoice` es el TEXTO de la
opción marcada, y ese texto interpola el contexto narrativo (`ctx$miembros`, protagonista), así
que **varía aunque el concepto correcto sea siempre el mismo**. Las Partes 3 y 4 del v1 puntúan
`ok` con conjunto de opciones estático.

Obligación en el v2: transcribir la tabla por gap **y** declarar, gap a gap, si lo que varía es
el concepto o solo el envoltorio narrativo. Con D3 aplicado, las Partes 3-6 deben variar de
concepto; las Partes 1-2 (`num`) varían de valor por construcción.

---

## 4. Criterio de éxito (reportar con la salida real del comando, sin adjetivos)

1. `ejercicio_state.json` de `cloze_v2/` con los 11 pasos completados.
2. Render OK en **HTML, PDF, DOCX** + exportación **Moodle**. NOPS es **N/A esperado**:
   `exams2nops()` rechaza cualquier `extype: cloze` antes de mirar `exclozetype` (verificado en
   el código de `exams` 2.4.2). No es un error a corregir ni a "intentar de otra forma".
3. Arsenal post-exams2 (FASES 2A-2N) sin errores bloqueantes; detractor con veredicto **APROBAR**.
4. **V1-V9** en verde, con V9 documentado como en D1.
5. `verificar_render.R` propio: I-1..I-7 + C-1..**C-4** sobre ≥ 300 semillas, I-6 canónica, y
   los **3 mutantes bajo contrato de sonda** + un **cuarto mutante** que corrompa la paridad de
   longitudes de un gap y demuestre que **C-4 lo mata** (si no lo mata, C-4 no sirve).
6. Diversidad: ≥ 250 versiones únicas sobre ≥ 300 semillas + tabla por gap + declaración D5.
7. Ortografía según D2 (0 automáticas + declaración de los ambiguos) y letter-independence limpio.
8. `HANDOFF.md` de `cloze_v2/` con: contrato heredado, qué cambió respecto del v1 (esta §3, con
   los números de antes y después), resultados de las 4 mutaciones y destino reservado.

**Pausas humanas:** solo la aprobación final (paso 11). Flujo B = `n` (ítem sin figura,
verificado contra los escaneos originales, §1 de `ESPECIFICACION.md`); la de lenguaje gráfico no
aplica.

---

## 5. Lo que NO debe hacer el v2

- No convertir el ítem en "¿cuál error cometió Fulano?" — la carga metacognitiva va en la
  `Solution` (regla de fidelidad, §3 de `ESPECIFICACION.md`).
- No tocar el `cloze/` del v1: es la línea base comparable.
- No re-derivar el espacio paramétrico ni "mejorar" la aritmética de la instancia canónica.
- No usar la omisión de **n** como caso indeterminable: `a = c/T·(T−P)` no depende de n, así
  que produciría una versión con clave falsa (§4 de `ESPECIFICACION.md`). En la Parte 6, retirar
  n es precisamente el caso cuya respuesta correcta es **"Sí"**.

---

**Fecha:** 2026-08-06
**Estado:** ENTRADA PARA EL ORQUESTADOR — no es documentación del ejercicio terminado
