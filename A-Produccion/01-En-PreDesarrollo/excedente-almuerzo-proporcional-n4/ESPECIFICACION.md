# Especificación — `excedente_almuerzo_numerico_variacional_argumentacion_n4_v1`

> Documento de entrada para el `orquestador-schoice`. **Leer completo antes del paso 4
> (`generacion_rmd`).** Las invariantes I-1..I-8 y la decisión de diseño de la §4 son
> vinculantes: si alguna resulta imposible de satisfacer, PARAR y reportar, no improvisar.

---

## 1. Origen y fuentes de verdad

Ítem oficial **`MAT-2026-1-017`** — ERA-2026 (Experiencia Real de Aplicación), Matemáticas,
Sesión 1, pregunta impresa **17**. Es verbatim de la pregunta 111 del cuadernillo
Matemáticas 2026-1 (`MAT-2026-1-111`).

| Fuente | Ruta | Sección |
|---|---|---|
| Ficha de alineación | `/home/bootcamp/Proyectos-2026/Todo-Pajaro/Alineacion-curricular-de-items/Simulacros/Alineacion-Curricular-de-items-ERA-2026/Matematicas/Alineacion-curricular-de-items-Matematicas-ERA-2026.md` | `### MAT-2026-1-017` (línea ~1482) |
| Transcripción | `/home/bootcamp/Proyectos-2026/Todo-Pajaro/Alineacion-curricular-de-items/Simulacros/Alineacion-Curricular-de-items-ERA-2026/Matematicas/transcripcion-preguntas-matematicas-era-2026.md` | `### Pregunta 17 (archivo: q017)` (línea ~405) |
| Escaneo original | `.../Matematicas/Originales/pagina_004.jpg` | columna derecha inferior (enunciado) |
| Escaneo original | `.../Matematicas/Originales/pagina_005.jpg` | columna izquierda superior (pregunta + opciones) |

**Verificado el 2026-08-06 contra ambos escaneos**: el ítem 17 es **texto puro, sin figura,
sin tabla y sin gráfica**. Sus vecinos 14, 15, 18, 20 y 21 sí traen imagen, pero el 17 no.
Por eso `flujo_b = "n"` viene preconfirmado por el usuario. Si al releer los JPGs se detecta
una figura, **PARAR** y avisar en vez de asumir.

---

## 2. Metadatos oficiales (copiar literal — NO reinterpretar)

| Campo | Valor |
|---|---|
| Competencia | Argumentación |
| Componente | Numérico-variacional |
| Nivel ICFES | 4 |
| Descriptor | **D4.9** — "Justifica si hay falta de información en una situación problema para tomar una decisión." |
| Afirmación | "Valida procedimientos y estrategias matemáticas utilizadas para dar solución a problemas." |
| Evidencia | "Establece la validez o pertinencia de una solución propuesta a un problema dado." |
| Estándar asociado | "Justifico el uso de representaciones y procedimientos en situaciones de proporcionalidad directa e inversa." (6°-7°, Pensamiento numérico) |
| Tema específico | Proporcionalidad — distribución proporcional de costos y suficiencia de información |
| Contenido | Álgebra y Cálculo |
| Grado sugerido | 7°-8° |
| Genérico | No |
| Clave del ítem original | **C** |

Coherencia obligatoria Nivel↔DOK (regla `ejercicios-metacognitivos.md`): Nivel 4 ⇒
`exextra[DOK] >= 3` y `exextra[Bloom]` ∈ {Analizar, Evaluar}. Propuesto: DOK 3,
Bloom `Evaluar`, SOLO `Relacional`, `TipoMetacognicion: evaluacion_afirmacion`.

Los campos `exname`, `exsection`, `exextra[Competencia]`, `exextra[Componente]` van **sin
tildes** (ASCII, regla `ortografia-espanol.md` §Excepciones). Todo texto visible al
estudiante va **con tildes**.

---

## 3. Ítem original — instancia canónica (reproducir VERBATIM)

**Enunciado:**

> Una empresa destina $300.000 para invitar a sus 10 empleados a un almuerzo en un
> restaurante. Debido a que el total de la cuenta por pagar fue de $500.000, se propone que
> el excedente sea pagado de manera proporcional al valor del pedido que hizo cada empleado.
>
> De acuerdo con la propuesta, si un empleado consumió solamente un jugo de $5.000, ¿es
> posible determinar el valor que este empleado debe pagar?

**Opciones:**

| | Texto |
|---|---|
| A | Sí, porque el empleado consumió menos de lo que le correspondía y, por lo tanto, esta persona no debe pagar nada. |
| B | No, porque el procedimiento planteado no contemplaba que uno de los empleados pudiera tener un consumo inferior a $30.000. |
| **C** | **Sí, porque se conoce el valor total de la cuenta y se puede calcular la proporción de esta que corresponde al jugo que consumió el empleado.** ← CLAVE |
| D | No, porque falta conocer los valores de los pedidos de los demás empleados para saber quién debe pagar más y quién debe pagar menos. |

**Aritmética de la instancia canónica:** P = 300.000, n = 10, T = 500.000, c = 5.000
⇒ excedente E = T − P = 200.000 ; cuota equitativa q = P/n = 30.000 (el umbral que cita la
opción B) ; aporte proporcional a = c/T × E = 5.000/500.000 × 200.000 = **$2.000**.

### Regla de fidelidad (memoria `feedback_respetar_enunciado_original`)

Este ejercicio es una **familia paramétrica con instancia canónica**, igual que el
subproyecto hermano `permutaciones-pescadores-venia-n4`:

- Existe una condición de parámetros que **reproduce el ítem oficial palabra por palabra**
  (enunciado + las 4 opciones). El verificador debe comprobarlo (I-6).
- Las demás versiones **conservan la estructura y el registro del ítem oficial**; solo
  cambian cifras, contexto narrativo y el caso lógico (§4).
- **La carga metacognitiva va en la sección `Solution`** (diagnóstico por distractor a
  partir de las Justificaciones MetaCognitivas de la ficha), **NO** en reescribir las
  opciones. **PROHIBIDO** convertir el ítem en "¿cuál error cometió Fulano?".

---

## 4. Decisión de diseño tomada por el usuario — RUTA (a): alternar el caso lógico

**Problema que resuelve:** la clave del ítem original es una frase argumentativa fija. Si
solo se aleatorizan las cifras, el TEXTO de la opción correcta es invariante entre versiones
y el paso 9 (`validar_diversidad_sustantiva.R`, regla #22) puede bloquear con
`ERR_DIV_COSMETICA`.

**Decisión (usuario, 2026-08-06):** la familia **alterna el caso lógico** por versión. Esto
no distorsiona el ítem: el descriptor **D4.9 trata precisamente sobre juzgar si falta
información**, así que incluir casos genuinamente indeterminables es *más* fiel al
descriptor, no menos.

### TIPO 1 — determinable (clave de categoría "Sí")

Se informan P, n, T y c. La respuesta es que **sí** se puede determinar: basta la razón
c/T aplicada al excedente E = T − P.

Reparto de categorías entre las 4 opciones: **2 "Sí" + 2 "No"** (una "Sí" correcta, una "Sí"
errónea, dos "No" erróneas). La categoría por sí sola no resuelve el ítem.

### TIPO 2 — NO determinable (clave de categoría "No")

Se **omite un dato imprescindible**. Dos sub-variantes válidas y solo dos:

| Sub-variante | Dato omitido | Por qué es indeterminable |
|---|---|---|
| 2a | **T** (total de la cuenta) | Sin T no existe la razón c/T ni se conoce E = T − P |
| 2b | **c** (valor del consumo del empleado) | Sin c no hay numerador para la razón c/T |

**ATENCIÓN — trampa a evitar:** omitir **n** NO hace indeterminable el problema, porque
a = c/T × E no depende de n (n solo alimenta la cuota equitativa q = P/n que cita el
distractor del umbral). Omitir n produciría una versión con clave FALSA. **PROHIBIDO usar la
omisión de n como sub-variante de TIPO 2.**

Reparto de categorías en TIPO 2: **2 "No" + 2 "Sí"** (una "No" correcta por la razón
correcta, una "No" errónea por razón equivocada, dos "Sí" erróneas). El distractor "No, pero
por la razón equivocada" es el más potente del ítem y es obligatorio.

### Consecuencia verificable

`Rscript .claude/scripts/validar_diversidad_sustantiva.R <rmd> --n 40` debe dar **exit 0**
y la clave debe variar **en categoría (Sí/No) y en contenido textual** a lo largo de las
versiones. Reportar el conteo real de cada tipo sobre las 40 semillas (se espera un reparto
razonablemente balanceado, ninguna categoría por debajo del 25 %).

---

## 5. Familia paramétrica e invariantes (verificación obligatoria)

**Parámetros:** `P` (presupuesto de la empresa), `n` (número de empleados), `T` (total de la
cuenta), `c` (consumo del empleado del caso), `tipo` ∈ {1, 2a, 2b}.

**Derivados:** `E = T − P` (excedente), `q = P/n` (cuota equitativa), `a = c/T × E` (aporte
proporcional del empleado).

| # | Invariante | Aplica a |
|---|---|---|
| **I-1** | `E = T − P > 0` (la cuenta siempre supera el presupuesto) | todos |
| **I-2** | `q = P/n` es entero **y** `c < q` — si no, el distractor del umbral ("consumo inferior a $q") pierde sentido | tipo 1, 2a |
| **I-3** | `a = c/T × E` es un entero en pesos (valor monetario limpio) | tipo 1 |
| **I-4** | `a > 0` — si a = 0, el distractor "no debe pagar nada" dejaría de ser un error | tipo 1 |
| **I-5** | Cifras verosímiles en pesos colombianos: P, T múltiplos de 50.000; c múltiplo de 500; `n` ∈ [5, 20]; formato con separador de miles (`$300.000`) | todos |
| **I-6** | La instancia canónica (P=300.000, n=10, T=500.000, c=5.000 ⇒ q=30.000, E=200.000, a=2.000) **pertenece al espacio paramétrico** y su render reproduce enunciado y las 4 opciones **carácter por carácter** respecto de la §3 | tipo 1 |
| **I-7** | Todo texto **emitido** por el `.Rmd` está libre de caracteres Unicode que rompan LaTeX (en particular **U+2212** MINUS SIGN en lugar de guion ASCII, y comillas tipográficas sueltas). Verificar por `grep -P` **antes** de renderizar — memoria `feedback_campo_no_emitido_no_esta_probado` | todos |
| **I-8** | En TIPO 2 el dato omitido **no aparece en ninguna parte** del enunciado renderizado (ni en la Solution antes de explicarlo). Verificar por búsqueda del valor en el HTML | tipo 2a, 2b |

**Patrón de generación exigido** (regla #21, Familia 1 — construcción determinista, **PROHIBIDO
`repeat`/`while` sin cota**, Error 22): no buscar `c` por reintento hasta que `a` sea entero.
Construir al revés: sortear `a` objetivo entero y derivar `c = a·T/E`, aceptando solo las
combinaciones (P, T, a) que hagan `c` entero y `c < q`. Enumerar el espacio de combinaciones
válidas y sortear sobre él con `sample()`. Usar `pick_int()` / `safe_sample()` (Familia 5,
`.claude/scripts/snippets_familias_rmd.R`) en todo muestreo cuyo soporte pueda colapsar a un
solo valor.

---

## 6. Pool de errores conceptuales

Mínimo **6** errores con `codigo`, `nombre`, `descripcion_corta`, `descripcion_larga`,
`causa_raiz`, `precondicion(params)` y `calcula()` **determinista** (prohibido `sample`/
`runif`/`rnorm` dentro de `calcula()` — Capa D, `ERR_SEM_D`). Selección por `sample()` sobre
los aplicables, nunca un filtro hardcoded (pre-flight 20 del orquestador: pool ≥ 4-6 con
`sample()`, para que el *tipo* de error varíe entre versiones y no solo las cifras —
Error 27).

Semilla del pool, derivada de las Justificaciones MetaCognitivas oficiales de la ficha
(prefijo propuesto `PRO-SUF-` = proporcionalidad / suficiencia de información):

| Código | Error | Precondición | Corresponde a |
|---|---|---|---|
| `PRO-SUF-01` | "Consumió menos que la cuota ⇒ no paga nada": aplica una regla práctica y no reconoce que cualquier consumo, por pequeño que sea, genera una proporción calculable del excedente | tipo 1, 2a | opción **A** oficial |
| `PRO-SUF-02` | "Umbral mínimo de la cuota": compara c con q = P/n y concluye que por debajo de ese umbral no aplica la regla; confunde reparto **proporcional al consumo** con reparto **igualitario con piso** | I-2 satisfecha | opción **B** oficial |
| `PRO-SUF-03` | "Faltan los pedidos de los demás": cree que la razón exige el desglose individual de los otros n−1 empleados, cuando la fórmula solo requiere c y T | todos | opción **D** oficial |
| `PRO-SUF-04` | "Basta el presupuesto": usa P (lo que aporta la empresa) en vez de T (el total de la cuenta) como referente de la proporción | tipo 2a | distractor "Sí" de TIPO 2 |
| `PRO-SUF-05` | "El excedente se reparte en partes iguales": divide E entre n e ignora la proporcionalidad al consumo | todos | error clásico adicional |
| `PRO-SUF-06` | "El dato omitido es irrelevante": en TIPO 2 afirma que se puede calcular igual porque el dato faltante "se deduce"; no distingue dato **derivable** de dato **ausente** | tipo 2a, 2b | distractor "Sí" de TIPO 2 |

`PRO-SUF-03` cumple doble función: en TIPO 1 es un "No" erróneo; en TIPO 2 es el **"No" con
la categoría correcta pero la razón equivocada** — el distractor más potente del ítem.

---

## 7. Reglas del repositorio con verificación explícita

Reportar cada una como **cumplida / no aplica**, con la evidencia (comando + salida), nunca
de forma declarativa.

| Regla | Qué exige aquí |
|---|---|
| **#19** letter-independence | `Solution` identifica opciones por **contenido o código de error**, jamás por letra ni `r letra_correcta` ni "Opción A-D". Opciones de TEXTO ⇒ `exshuffle: TRUE`. La **prosa** de la Solution no puede enumerar las opciones en orden (memoria `feedback_solution_enumera_en_orden`): agrupar por categoría (los "Sí" erróneos / los "No" erróneos), nunca reproducir la lista |
| **#22** diversidad sustantiva | `validar_diversidad_sustantiva.R --n 40` exit 0 (§4) |
| **#21** familias | `pick_int`/`safe_sample`; sin `repeat` sin cota; `eq_display()` si hay ecuación display |
| **#18** imágenes | No aplica si no se emite ninguna imagen — dejarlo constatado. Si se emitiera, `{width=...}` obligatorio |
| **#20** guard `none` | No aplica si no se emite tabla Markdown — dejarlo constatado. Si se emitiera, bloque raw `{=latex}` con `\@ifundefined{c@none}{\newcounter{none}}{}` al inicio de `Question` |
| **#7** ortografía | Tildes en todo texto visible; `corregir_ortografia_espanol.R` limpio |
| **#11** contextos narrativos | ≥ 6 plantillas narrativas como **funciones**, ≥ 5 estructuras distintas, "registró" en ≤ 25 %. El registro debe seguir siendo el de un ítem ICFES (empresa/restaurante y análogos: cooperativa y proveedor, colegio y salida pedagógica, junta de acción comunal y evento, etc.) |
| **#10** RNG | `set.seed()` en chunks de test guarda y restaura `.Random.seed`. **PROHIBIDO** reseedear con `Sys.time()`/`proc.time()` dentro de `data_generation` (Incidente I) |

---

## 8. Entregables y criterio de éxito

Reportar cada punto **con la salida real del comando**, sin adjetivos y sin el calificador
"significativo" (antipatrón #4 de `ciclo-validacion.md`).

1. `ejercicio_state.json` con los **11 pasos** completados.
2. Render OK en **HTML, PDF, DOCX y NOPS** + exportación **Moodle** (`exams2moodle`).
3. Arsenal post-exams2 (FASES 2A-2N) **sin errores bloqueantes**; detractor con veredicto
   **APROBAR**.
4. `validar_diversidad_sustantiva.R --n 40` **exit 0** + conteo exacto de versiones únicas
   sobre **≥ 300** semillas + reparto TIPO 1 / TIPO 2a / TIPO 2b.
5. **`verificar_render.R` propio** que enumere el espacio paramétrico y compruebe
   **I-1..I-8** sobre ≥ 300 semillas, incluida la reproducción exacta de la instancia
   canónica (I-6).
6. **Prueba de mutación del verificador** (memoria `feedback_clave_geometrica_sin_validador`):
   introducir a propósito (a) una clave falsa y (b) una versión TIPO 2 que omita `n` en vez
   de `T`, y **demostrar que el verificador detecta ambas**. Si no las detecta, el
   verificador no sirve — corregirlo antes de continuar.
7. Ortografía limpia y letter-independence limpio (hook FASE 2J + `test_letter_independence.R`).
8. `README.md` y `HANDOFF.md` del subproyecto con: parámetros, invariantes, decisión de la
   §4, resultados de la prueba de mutación y destino reservado en `03-En-Produccion/`
   (categoría por confirmar con el usuario; candidata: `01-Numeros-Reales/`).

**Pausas humanas restantes:** solo **una**, la aprobación final del paso 11. La de Flujo B ya
está resuelta (`n`, §1) y la de lenguaje gráfico no aplica porque no hay figura.

---

**Fecha:** 2026-08-06
**Estado:** ENTRADA PARA EL ORQUESTADOR — no es documentación del ejercicio terminado
