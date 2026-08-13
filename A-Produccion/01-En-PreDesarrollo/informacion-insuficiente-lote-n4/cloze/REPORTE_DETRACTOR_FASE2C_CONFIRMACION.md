## Pasada de confirmación — FASE 2C (regla #9 v1.2)

**Artefacto**: `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/informacion-insuficiente-lote-n4/cloze/informacion_insuficiente_lote_geometrico_metrico_argumentacion_n4_cloze_v1.Rmd` (1831 líneas, mtime 09:31)
**Modo**: lectura del código vigente, sin mediciones. Las cifras del coordinador se toman como dadas.
**Agente**: distinto del que auditó en la primera pasada y distinto del que aplicó los cambios.

### Verificación previa de que las cifras dadas cubren el código vigente

Antes de aceptar «re-verificadas tras los cambios» comprobé que la re-medición **no puede** estar mirando código viejo: `auditoria_propia.R:16-19` lee el `.Rmd` de disco y re-parsea el chunk `data_generation` en cada corrida (`readLines` → `parse` → `eval`), y **no** hay `readRDS` en el script. El `envs.rds` de las 08:17 (anterior a los cambios) es artefacto de otra medición y el auditor no lo consume. Queda una dependencia declarada: que la corrida se ejecutara después de las 09:31; el mtime del script (09:37) es indicio, no prueba.

---

## Resultado por objeción

| # | Severidad original | Estado | Correcta |
|---|---|---|---|
| 1 | CRÍTICA-ALTA | Implementada | Sí, y la guarda cubre el caso real |
| 2 | ALTA | Implementada | Sí, **los cinco** escalones cubiertos + aserción en C-1 |
| 3 | MEDIA-ALTA | Implementada | Sí, verificada contra `COMBOS_INFO` |
| 4 | MEDIA-ALTA | Implementada | Sí, y la ampliación de G4b era **necesaria**, no cosmética |
| 5 | MEDIA | Implementada | Sí el cambio; **no** su justificación escrita |
| 6 | MEDIA (documentar) | Implementada | Sí, la declaración es fiel — verificado el 52/86 |

---

### Objeción 1 — `contextos[[6]]` + guarda G7b — CONFIRMADA

`cloze/…_v1.Rmd:120` ahora emite «—El lote **es rectangular**, tiene {MED} y hay que repartirlo…». Cubre el caso duro (`ctx6 ∧ !info_L`): `FIJ-09` (`det = TRUE` siempre, `razon` = «El enunciado dice que el lote es rectangular») ya no cita una frase inexistente.

**G7b** (líneas 1498-1508) recorre las **siete** plantillas con **ambas** formas de `MED`. Que pruebe las dos formas es correcto aunque hoy sea redundante: ninguna plantilla ramifica por `MED`, así que basta con una — pero si alguna lo hiciera, la omisión volvería a colarse solo en la rama `!info_L`, que es justo donde dolía.

**No rompe la literalidad**: `contextos[[1]]` no se tocó; G7a (1491-1497) sigue contrastando su salida verbatim contra `OFICIAL_ENUNCIADO`, y G7 (1511-1520) sigue intacta. G7b es solo lectura, determinista, y corre en todas las versiones.

Nit (BAJA): G7b busca el substring desnudo `"rectangular"`; pasaría una plantilla futura que dijera «no es rectangular» o que solo mencionara «franjas rectangulares». Endurecimiento barato si alguna vez se toca: exigir la forma afirmativa (`"lote es rectangular"|"lote rectangular"|"forma rectangular"`).

Efecto lateral positivo no declarado: la `razon` de `FIJ-01` («Se multiplican las dos medidas del lote»), que la primera pasada señaló al margen por presuponer rectángulo, queda cubierta por el mismo fix.

### Objeción 2 — `cods[idx_p6]` en todos los escalones — CONFIRMADA, sin hueco

Recorrí el `switch` completo (líneas 1365-1373). Cobertura de `cods[idx_p6]`:

- lv1 → `veto_p4_1` (línea 1327-1328) ✔
- lv2, lv3 → `veto_p4_2` (línea 1340, ahora con `idx_p6`) ✔
- lv4, lv5 → expresión en línea con `cods[idx_p6]` ✔

**No queda ningún escalón sin cubrir.** La aserción gemela existe en C-1 (línea 1447): `!any(cods[idx_p6] %in% cods[idx_p4])`, con el comentario que explica por qué faltaba. La prueba de mutación que reporta el coordinador (revertir → 49/100, sonda A8 caza) es lo que convierte esto en verificado y no en «no dispara, luego no hay defecto».

**Hallazgo nuevo (BAJA) — la cascada tiene 5 escalones pero 4 configuraciones.** `veto_p4_2` es *literalmente* `unique(c(veto, cods[par_23], cods[idx_p5], cods[idx_p6]))`, que es la misma expresión que lv4, con el mismo `n_op = 4L` y el mismo `podar_op = TRUE`. Es decir: **lv3 ≡ lv4**. No es código muerto —`armar_p4` reordena `podar()` al azar, así que lv4 aporta 12 reintentos más— pero sí tiene dos consecuencias:

1. El comentario de lv4 (líneas 1366-1371) dice «Lo único que se relaja aquí es el número de opciones, de 5 a 4», y eso describe la transición **2→3**, no 3→4.
2. `auditoria_propia.R:235` tabula `nivel_excl_p4`. Un ejercicio reportado «en escalón 4» **no** está más degradado que uno en escalón 3, así que esa tabla no es el medidor de degradación que aparenta.

Además, la cabecera del bloque (líneas 1320-1323) documenta **tres** niveles mientras el bucle recorre `1:5`. Fix sugerido, sin cambio de comportamiento: usar `veto_p4_2` en lv4 y documentar los cinco escalones (o colapsar lv3/lv4 y declarar el reintento).

### Objeción 3 — `UBI-02` ramificada — CONFIRMADA, y la rama nueva dice lo correcto

Verificado contra `COMBOS_INFO` (líneas 234-239): **el único combo con `!info_T` es el 5** (`L=T, T=F, A=F, N=T`, w=14), y ahí `A` también falta. Por tanto la frase añadida «y tampoco fija un orden para asignar las partes» es **verdadera** en la única rama donde se renderiza. La rama `else` aplica en los combos 1 y 3 (`info_T ∧ !info_A`), donde «no fija ningún orden» también es verdadera.

**No contradice a `UBI-01`**: son gemelas declaradas (`PARES_EXCLUYENTES`, línea 818), así que `podar()`/`vecinos()` impiden que coexistan en la misma parte. Y en el escenario que motivó la objeción (Parte 5 con `h_p5 = "T"` y clave `UBI-02`) las dos frases consecutivas ya no atribuyen la falta de respuesta a hechos distintos.

Coherencia con G4b comprobada en las dos direcciones: la rama `!info_T` contiene «no dice por dónde pasan los cortes» → la caza el segundo `stopifnot` (1422) si alguna vez se emitiera con `info_T`; la rama `info_T` contiene «no fija ningún orden» → la caza el primero (1417) si se emitiera con `info_A`. Ambas están gateadas por su condición complementaria. ✔

Nit (BAJA, no lo tocaría): con `h_p5 = "T"` la Solution imprime ahora «no dice por dónde pasan los cortes» dos veces seguidas (`frase_falta` + `motivo`). Es redundancia, ya no contradicción. Si algún día se retoca, empezar el motivo por «Sin ese dato, ni siquiera está definida…» lo resuelve.

### Objeción 4 — `POR-08` reescrita + G4b ampliada — CONFIRMADA, y la ampliación era obligatoria

`motivo`/`contra` (líneas 566-574) ya no presuponen franjas paralelas, no citan medidas y no tienen rama `info_L`. Compatibles con `POR-06` (líneas 524-527) y con `POR-07` en su rama `!info_T` (líneas 751-754), que son las que podían coexistir bajo «No se pueden responder». La contradicción que denunciaba la objeción está cerrada.

**La ampliación de G4b no es cosmética, es lo que evita perder la guarda.** El texto nuevo ya no contiene «por dónde pasan los cortes» ni «cómo se cortan» ni «trazado»: sin añadir `cómo se divide el lote` al regex (línea 1422), G4b habría dejado de cubrir `POR-08` **en silencio**, que es el modo de fallo más caro (una guarda que ya no mira nada sigue estando verde). Comprobado que el patrón casa: `no (dice|…).{0,30}(…|c[oó]mo se divide el lote|…)` contra «El enunciado no dice cómo se divide el lote». Y sigue gateada por `!info_T ||`, así que no puede misfire.

Residuo (BAJA, **pre-existente, no introducido**): la misma presuposición que se le quitó a `POR-08` sobrevive en el `motivo` de `POR-09` rama `else` («según el lado al que sean paralelos los cortes», líneas 767-770) y en el helper `contra_cortes`. `POR-08` y `POR-09` ya no pueden co-mostrarse (son gemelas desde la objeción 5), pero `POR-09` y `POR-06` sí pueden (ambas indeterminadas en el combo 5, no gemelas), y el `contra` de `POR-06` admite piezas en L o triángulos. La tensión es más débil que la original —`POR-09` ilustra un caso, no cierra el universo— y la dejo como nota, no como objeción.

### Objeción 5 — par declarado — CONFIRMADA; **su justificación escrita es falsa**

El par `c("GEO-INF-POR-08", "GEO-INF-POR-09")` está en `PARES_EXCLUYENTES` (línea 835) y la glosa se corrigió al criterio de arrastre que el resto del archivo aplica. Eso está bien.

**Hallazgo nuevo (MEDIA)** — el comentario de las líneas 832-833 dice:

> «No le quita a POR-08 su papel de segunda clave en la Parte 6: `elegir_p6` solo usa `vecinos()` para filtrar los distractores frente a la clave ya elegida.»

Es inexacto, y el código lo desmiente tres líneas más abajo. `elegir_p6` recibe un **pool ya filtrado**:

```r
veto_clave <- vecinos(clave$codigo)                                   # 1085
cand_p6_1 <- which(!(cods %in% veto) & det_todas)                     # 1087  (veto = vecinos de las 4 de P1)
cand_p6_2 <- which(!(cods %in% unique(c(codigos_p1, veto_clave))) & det_todas)
cand_p6_3 <- which(!(cods %in% veto_clave) & det_todas)               # 1089
```

Con el par nuevo, `vecinos("GEO-INF-POR-09")` pasa a incluir `POR-08`, así que **`POR-08` queda fuera del pool de la Parte 6 —también como CLAVE— cuando `POR-09` aparece en la Parte 1** (nivel 1) o es su clave (niveles 2-3). Y eso muerde justo en las dos ramas que motivaron su existencia:

- rama `-T-N` (combo 1, w=12): `POR-09` es indeterminada → puede ser la **clave** de P1 → `POR-08` cae en `veto_clave` en los tres niveles.
- rama `LT--` (combo 3, w=8): `POR-09` está determinada → puede ser **distractor** de P1 → `POR-08` cae en `cand_p6_1`.

**Por qué no lo escalo por encima de MEDIA**: la auditoría propia mide exactamente eso. El bloque D (`auditoria_propia.R:188-219`) corre `sonda_rama(sub, "op_p6", "sol_p6", …)` **por rama**, con `MIN_ESTRATO = 20` y un `NO CONCLUYENTE` explícito para estratos cortos; a N=300 las dos ramas quedan en n≈42 y n≈28, por encima del umbral. Si el par nuevo hubiera devuelto la clave de P6 a la invariancia en esas ramas, habría salido `D: H3b invariante en P6 …`. El coordinador reporta 0 hallazgos con todos los estratos dictaminados. El riesgo está **medido y cerrado**; lo que queda mal es **la razón escrita en el archivo**, y una razón falsa es lo que falla la próxima vez que alguien edite este bloque sin volver a medir.

Reemplazo sugerido para 832-833: *«POR-08 SÍ queda fuera del pool de la Parte 6 cuando POR-09 aparece en la Parte 1 (`cand_p6_1..3` se construyen restando `veto`/`veto_clave`, que ya vienen expandidos por `vecinos()`). Lo que sostiene su papel de segunda clave es que eso ocurre solo en una fracción de las versiones; comprobado por rama en el bloque D de `auditoria_propia.R` (H3b de P6, estratos dictaminados a N=300).»*

### Objeción 6 — residuo de `h_pref` declarado — CONFIRMADA, y la declaración es fiel

Verifiqué las tres piezas del enunciado del comentario (líneas 1200-1217), no solo su presencia:

1. **El mecanismo**: con un solo hecho callado, cualquier clave indeterminada de P1 depende de él ⟹ `falt_p1` lo contiene ⟹ `h_pref = setdiff(callados_h, falt_p1) = ∅` ⟹ en la cascada `hs <- if (lv <= 2L && length(h_pref) > 0L) h_pref else callados_h` (línea 1275) se cae a `callados_h`, que es ese único hecho. ✔
2. **El peso**: ramas de omisión única = combo 2 (calla L, w=26) y combo 4 (calla N, w=26); total de pesos 12+26+8+26+14 = 86. **52/86 = 60,5 %**. ✔
3. **Que no sea MÁS del 60 %**: comprobé en la tabla `DEP` (líneas 790-801) que **ninguna** pregunta depende a la vez de `L` y de `A` (`UBI` = TAN/TA, `POR` = LTN/T/TN/LT, `LOT` = L, `FIJ` = N o vacío). Por eso en los combos de doble omisión `h_pref` nunca queda vacío y el residuo es exactamente 52/86, ni un punto más.

La mitigación (≥2 opciones con el sustantivo del hecho) era opcional y no se aplicó — coherente con el enunciado de la objeción. **Pendiente que no está en el archivo**: la objeción pedía declararlo también «en el reporte de aprobación». Eso vive fuera del `.Rmd` y es del coordinador en el paso 11.

---

## ¿Los cambios introdujeron algún defecto nuevo?

Revisé los cuatro textos visibles al estudiante que se tocaron (plantilla de `ctx6`; `motivo`/`contra` de `UBI-02`; `motivo`/`contra` de `POR-08`) y las dos modificaciones estructurales.

- **Corrección**: ninguno. No hay clave falsa, ni opción no respondible marcada como respondible, ni aserción que pueda romperse por los cambios.
- **Ortografía**: los textos nuevos llevan todas las tildes (`explicó`, `así`, `está`, `cuál`, `porción`, `aún`, `quién`, `cómo`, `única`). Coherente con el «0 en texto visible» reportado.
- **Regla #19**: ningún texto nuevo cita letras ni posiciones.
- **Coherencia entre preguntas co-mostrables**: comprobada para el combo 5 (el único con `!info_T`, donde `POR-08` es indeterminada). `POR-08` ↔ `POR-06` y `POR-08` ↔ `POR-07` ahora concuerdan; `POR-08` ↔ `POR-09` es imposible por el par nuevo.
- Los tres hallazgos nuevos son los ya descritos: **MEDIA** el comentario de 832-833, **BAJA** el `lv3 ≡ lv4` con su comentario desplazado, **BAJA** la redundancia de P5 con `h_p5 = "T"`.

Observación adicional (BAJA, **pre-existente y sistémica**, no atribuible a estos cambios): la Parte 6 renderiza `clave_p6$motivo` (líneas 1735 y 1801) bajo un marco contrafactual («Si el enunciado *no hubiera dicho X*, la pregunta … dejaría de tener respuesta: …»), pero los `motivo` están redactados en indicativo presente («El enunciado no dice…»). En esa versión el enunciado **sí** lo dice. Afecta por igual a toda clave de P6, no solo a `POR-08`, y el marco condicional lo hace legible. Si se toca alguna vez, un «Sin ese dato, …» antepuesto lo resuelve para todas.

---

## Por qué APROBAR y no APROBAR_CON_CAMBIOS

Las seis objeciones están implementadas y son correctas. Ningún hallazgo nuevo alcanza severidad **alta o crítica**, que son las que bloquean según `.claude/detractor-config.yaml`. El único **MEDIA** vive en un comentario: corregirlo no altera ni una línea de salida, así que aplicarlo ahora volvería a caducar la FASE 2C —obligando a otra pasada— **a cambio de cero cambio en lo que ve el estudiante**. Recomiendo registrarlo como backlog y aplicarlo junto con la próxima edición sustantiva del bloque, no antes.

**Backlog resultante** (ninguno bloqueante):
1. Reescribir el comentario de `…_v1.Rmd:832-833` con el mecanismo real (`cand_p6_1..3` restan `veto`/`veto_clave`, ya expandidos por `vecinos()`), citando el bloque D como lo que sostiene la conclusión.
2. `lv4` → usar `veto_p4_2`; documentar los cinco escalones en la cabecera (hoy dice tres) y mover el comentario de «se relaja el número de opciones» al escalón que sí lo hace.
3. Declarar el residuo de `h_pref` (60 %) también en el reporte de aprobación del paso 11.

**Dominios no auditados en esta pasada** (fuera de encargo, cifras tomadas como dadas del coordinador): renderizado y formatos, ortografía automatizada, diagnosticidad/diversidad/multisemilla/coherencia, fuga inter-parte medida, prueba de mutación de A8, y la fidelidad verbatim de la instancia canónica más allá de comprobar que G7/G7a/G7b la cubren y que los cambios no la tocan.

VEREDICTO_DETRACTOR: APROBAR