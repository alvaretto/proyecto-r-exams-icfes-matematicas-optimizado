# Auditoría Detractor — FASE 2C (CLOZE `informacion_insuficiente_lote_..._n4_cloze_v1.Rmd`)

**Fecha**: 2026-08-13 · **Modo**: lectura crítica (sin mediciones; las cifras entregadas por el coordinador se toman como dadas)
**Objeciones**: 7 (1 crítica-alta, 2 altas, 2 medias-altas, 1 media, 1 grupo bajo)

## Resumen ejecutivo

El motor es sólido y las guardas mecánicas (G1–G7, C-1..C-3) cubren bien lo que se puede comprobar por construcción. **G6 (`det == det_bajo(DEP, K_ACTUAL)`) valida que `det` y `DEP` digan lo mismo, pero no que `DEP` diga la verdad**: es un espejo, no una fuente. Toda mi auditoría vive en ese hueco, y ahí encontré un defecto que hace que una opción marcada **«sí se puede responder» no sea respondible** en una de las siete plantillas narrativas, más una fuga inter-parte que el propio archivo cierra para P2/P3 y deja abierta para P6.

---

## Objeción 1 — `contextos[[6]]` nunca dice que el lote sea rectangular; `GEO-INF-FIJ-09` lo afirma igual (CRÍTICA-ALTA)

**Qué se cuestiona**: líneas 108-112 (plantilla 6, «Diálogo implícito») frente a `GEO-INF-FIJ-09` (líneas 698-704, `det = TRUE`).

Las siete plantillas se leyeron una por una. Seis declaran la forma:

| ctx | texto | ¿dice «rectangular»? |
|---|---|---|
| 1 | «un lote rectangular de …» | sí |
| 2 | «el lote es rectangular y tiene …» | sí |
| 3 | «un lote rectangular de …» | sí |
| 4 | «un lote rectangular de …» | sí |
| 5 | «el lote, de forma rectangular y con …» | sí |
| **6** | **«—El lote tiene {MED} y hay que repartirlo…»** | **NO** |
| 7 | «un lote rectangular de …» | sí |

`FIJ-09` («¿Cuál es la forma del lote?») está declarada **siempre determinada**, con `valor = "rectangular"` y `razon = "El enunciado dice que el lote es rectangular."` En `contextos[[6]]` esa frase **no existe en el enunciado**, y el caso duro es `ctx6 ∧ !info_L` (combos 1 y 2, peso 38/86): el enunciado queda en «—El lote tiene 12.000 metros cuadrados de área y hay que repartirlo en partes iguales entre los 8 socios—», que no restringe la forma en absoluto. La frase de trazado («paralelas al lado más corto») implica que tiene lados, no que sea un rectángulo.

**Riesgo concreto**, en tres capas:
1. `FIJ-09` puede ser **distractor de la Parte 1** (está en `idx_det`; los filtros de `ternas_validas` son léxicos, no semánticos). En ctx6 la Parte 1 tendría **dos** opciones no respondibles → se rompe la unicidad de la clave, que es justo lo que evalúa el ítem.
2. En la Parte 4 se marcaría `sol_p4 = 1` para una pregunta indeterminada.
3. La `razon` se imprime al estudiante en la Solution y en `fb_p1`/`fb_p4`/`fb_p5` **citando una frase que el enunciado no contiene**. Un estudiante de N4 que hace exactamente lo que el descriptor D4.9 pide —revisar qué dice y qué no dice el enunciado— es penalizado por acertar.

Ningún validador lo ve: es coherencia texto↔texto entre dos bloques a 600 líneas de distancia, y un `grep "rectangular"` devuelve seis aciertos y parece limpio. `G7a` solo contrasta `contextos[[1]]`.

Nota: el defecto lo **introduce la ampliación del 2026-08-12**. Antes de `FIJ-09` ninguna pregunta interrogaba la forma del lote, así que la omisión de ctx6 era inocua (salvo por la `razon` de `FIJ-01`, «Se multiplican las dos medidas del lote», que también presupone rectángulo).

**Alternativa propuesta** (una línea, y no toca la canónica ni G7a):

```r
  list(  # 6. Dialogo implicito
    posesivo = "", b_sing = "socio", b_plur = "socios",
    plantilla = function(MED, NT) paste0(
      "—El lote es rectangular, tiene ", MED, " y hay que repartirlo en partes ",
      "iguales entre los ", NT, "socios —explicó el administrador de la sociedad.")),
```

Y, para que no reincida, una guarda barata junto a G7a:

```r
stopifnot(all(vapply(contextos, function(cc)
  grepl("rectangular", cc$plantilla("X", "N "), fixed = TRUE), logical(1))))
```

**Veredicto**: MODIFICAR (bloqueante).

---

## Objeción 2 — `veto_p4_2` deja de excluir `cods[idx_p6]`: Parte 4 y Parte 6 pueden mostrar la misma pregunta (ALTA)

**Qué se cuestiona**: líneas 1267-1269 y 1301-1302.

```r
veto_p4_1 <- unique(c(veto, unlist(lapply(c(cods[par_23], cods[idx_p5]), vecinos)),
                      cods[idx_p6]))
veto_p4_2 <- unique(c(veto, cods[par_23], cods[idx_p5]))   # <- idx_p6 desaparece
```

`cods[par_23]` y `cods[idx_p5]` sobreviven en **los cinco escalones**; `cods[idx_p6]` solo existe en el escalón 1. En los escalones 2-5 la Parte 4 puede ofrecer literalmente el mismo texto que la Parte 6.

**Por qué es fuga y no mera repetición**: las cuatro opciones de la Parte 6 están **todas determinadas hoy** (`stopifnot` líneas 1085-1086), y el molde lo transparenta — la clave «dejaría de tener respuesta» y las otras tres «seguirían teniendo respuesta». Resolver la Parte 6 acredita que las cuatro se responden con el enunciado original, así que cualquiera de ellas que reaparezca en la Parte 4 se marca «sí» sin razonar. Es exactamente el mecanismo que el archivo describe y cierra para P2/P3, con su medición, en el comentario de las líneas 1296-1301 («Al omitirlo se abrió una fuga inter-parte en el 22,5 % de las versiones»).

Además, el comentario que justifica la excepción (líneas 1256-1259) dice «basta con no repetir su texto literal» — y **eso es precisamente lo que los escalones 2-5 dejan de garantizar**. El código no cumple ni el mínimo que su propio comentario declara. El bloque C-1 (líneas 1358-1368) tampoco lo asevera: hay `!any(cods[idx_p5] %in% cods[idx_p4])`, no hay el gemelo para `idx_p6`.

**Alternativa propuesta** (simétrica a como ya se trata `par_23`, una línea + los dos escalones finales):

```r
veto_p4_2 <- unique(c(veto, cods[par_23], cods[idx_p5], cods[idx_p6]))
# y en los escalones 4 y 5:
list(unique(c(veto, cods[par_23], cods[idx_p5], cods[idx_p6])), 4L),
list(unique(c(veto, cods[par_23], cods[idx_p5], cods[idx_p6])), 3L)
```

Más la aserción que falta en C-1: `stopifnot(!any(cods[idx_p6] %in% cods[idx_p4]))`.

Si el cupo no aguanta el veto en el escalón 5, esa es información útil (hay que ampliar el banco), no una razón para abrirlo en silencio.

**Veredicto**: MODIFICAR.

---

## Objeción 3 — `UBI-02` culpa al hecho equivocado cuando falta el trazado; la Solution de la Parte 5 se contradice a sí misma (MEDIA-ALTA)

**Qué se cuestiona**: `GEO-INF-UBI-02`, `motivo` y `contra` sin ramificar (líneas 392-396), con `DEP = "TA"`.

`UBI-01`, su hermana, **sí** ramifica su `motivo` por `info_T` (líneas 355-368). `UBI-02` no: su motivo es siempre «El enunciado no fija ningún orden de asignación: cualquiera de los {socios} podría recibir esa porción.» En la rama `L--N` (combo 5, `w = 14`), donde faltan **T y A**, ese motivo es incompleto en un punto que importa: sin trazado, «la porción que da a la carretera» ni siquiera está definida —pueden ser varias, o ninguna distinguida—, así que la causa primaria es T, no A.

**Riesgo concreto — contradicción visible al estudiante.** La Parte 5 es reachable con `h_p5 = "T"` y `clave_p5q = UBI-02` (`dep_incluye("GEO-INF-UBI-02","T")` es TRUE, y `UBI-02` es indeterminada en esa rama). La Solution imprimiría entonces, líneas 1602-1604:

> El enunciado **no dice por dónde pasan los cortes**, y la pregunta «¿Cuál de los socios recibe la porción que da a la carretera?» depende justamente de ese dato: por eso queda sin respuesta.
> El enunciado **no fija ningún orden de asignación**: cualquiera de los socios podría recibir esa porción.

Dos frases seguidas atribuyendo la falta de respuesta a dos hechos distintos. Lo mismo en `fb_p5` (`paste0("Correcto. ", e$motivo)`). Es el mismo defecto de género que la §P4-bis persigue: la justificación no está condicionada a la rama.

**Alternativa propuesta**, calcada de `UBI-01`:

```r
       motivo = if (!info_T) paste0(
           "El enunciado no dice por dónde pasan los cortes, así que ni siquiera ",
           "está definida cuál es la porción que da a la carretera; y tampoco fija ",
           "un orden para asignar las partes.")
         else paste0("El enunciado no fija ningún orden de asignación: cualquiera ",
                     "de los ", b_plur, " podría recibir esa porción."),
       contra = if (!info_T) paste0(
           "Sin saber por dónde pasan los cortes, ni siquiera está determinado qué ",
           "porción da a la carretera, y menos aún a quién le toca.")
         else paste0("Nada en el enunciado impide que esa porción le toque al ",
                     "primero de los ", b_plur, " ni que le toque al último: las dos ",
                     "reparticiones dejan partes iguales y cumplen lo dicho.")),
```

**Veredicto**: MODIFICAR.

---

## Objeción 4 — `POR-08` presupone las franjas que `POR-06` y `POR-07` declaran no dadas, y su contraejemplo es una tautología (MEDIA-ALTA)

**Qué se cuestiona**: `GEO-INF-POR-08`, `motivo` y `contra` (líneas 529-542).

Cuando `!info_T`, `POR-08` es indeterminada y su motivo dice:

> «…las franjas pueden trazarse **paralelas a un lado del lote o al otro**, y en cada caso los cortes van en una dirección distinta.»

Es decir: presupone que el reparto **es** en franjas paralelas a un lado y que lo único abierto es cuál. Pero `POR-06`, que tiene `det = info_T` **idéntico** —o sea, es indeterminada exactamente en las mismas versiones— afirma lo contrario en su `contra` (líneas 501-504):

> «El lote se puede partir en franjas rectangulares, pero también en **piezas en forma de L o en triángulos** que tengan todos la misma área.»

Y `POR-07` (líneas 712-722) sostiene la misma postura correcta que `POR-06`. Como `POR-06` y `POR-08` **no** están en `PARES_EXCLUYENTES`, `podar()` las deja coexistir: pueden aparecer juntas bajo «**No se pueden responder:**» en la Solution de la Parte 4, con dos relatos incompatibles de qué deja abierto el enunciado.

Segundo problema, en la rama `info_L` (que es la única viva, porque `!info_T ⇒ info_L` en los cinco combos): `contra = contra_cortes("el lado de 50 m", "el lado de 120 m")` se renderiza como

> «Con franjas paralelas al lado de 50 m la respuesta sería **el lado de 50 m**; con franjas paralelas al de 120 m sería **el lado de 120 m**.»

Es cierto y es vacío: repite la hipótesis como conclusión. Todos los demás contraejemplos del banco muestran una magnitud **derivada** que cambia (perímetro, cerca, razón); éste muestra la premisa. Como argumento no enseña nada, y `POR-08` es además la pregunta que más se apoya en su `contra` por ser cualitativa.

**Alternativa propuesta** (elimina la tautología y la contradicción de una vez, y de paso hace innecesaria la rama `info_L`, con lo que ningún texto suyo cita medidas):

```r
       motivo = paste0(
         "El enunciado no dice cómo se divide el lote: ni siquiera que los cortes ",
         "sean paralelos a alguno de sus lados. Mientras eso no se fije, la ",
         "pregunta no tiene una respuesta única."),
       contra = paste0(
         "El lote se puede partir en franjas paralelas a uno de sus lados, en ",
         "franjas paralelas al otro, o en piezas que no sigan ninguna de las dos ",
         "direcciones. Todos esos repartos dejan partes iguales y cumplen el ",
         "enunciado.")),
```

**Veredicto**: MODIFICAR.

---

## Objeción 5 — `POR-08` nombra el valor de `POR-09` y no están declaradas gemelas (MEDIA)

**Qué se cuestiona**: `PARES_EXCLUYENTES` (líneas 778-793) frente a `POR-08` (`DEP="T"`, `valor = "el lado de {lado_fijo} m"`) y `POR-09` (`DEP="LT"`, `valor = "{lado_fijo} m"`).

Cuando `info_L ∧ info_T` —combos 3 y 4, `w = 34/86` ≈ **40 %** del sorteo— las dos están determinadas y **la respuesta de una es literalmente el número de la otra**: «los cortes son paralelos al lado de 50 m» ⟹ «cada corte mide 50 m».

El comentario de las líneas 515-518 argumenta que no son gemelas porque `POR-06` pregunta por la FORMA y `POR-09` por la LONGITUD. Eso es correcto **como taxonomía**, pero el criterio que usa este archivo en el resto de `PARES_EXCLUYENTES` no es taxonómico sino de arrastre: `c("GEO-INF-POR-09","GEO-INF-LOT-04")` está declarada con la glosa «el corte mide un lado del lote» y esas dos ni siquiera coinciden numéricamente siempre (`lado_fijo` puede ser el lado corto). El par `POR-08`/`POR-09` es **más estrecho** que uno ya declarado. Y el propio archivo escribe, dos comentarios antes (líneas 787-789): «ganar huecos saltándose una equivalencia real es exactamente el atajo que `PARES_EXCLUYENTES` existe para cerrar».

**Reconozco el trade-off, y verifiqué que aquí no muerde**: `POR-08` existe para dar a la Parte 6 una segunda clave posible en las ramas `-T-N` y `LT--`. Declararla gemela **no** le quita ese papel, porque `elegir_p6` no veta `cods[par_23]` en ningún escalón y solo usa `vecinos()` para filtrar los distractores frente a la clave elegida. El efecto real del cambio es el deseado: vetar `POR-08` de la Parte 4 cuando `POR-09` es P2/P3 (escalón 1 de `veto_p4_1`, que sí expande por `vecinos`) e impedir que coexistan dentro de una misma parte.

**Alternativa propuesta**:

```r
  c("GEO-INF-POR-08", "GEO-INF-POR-09")   # la direccion del corte da su longitud
```

**Salvedad importante, para no ser injusto con el diseño**: la *co-determinación* redundante es **sistémica y preexistente**, no algo que introduzca `POR-08`. `LOT-01..05` comparten `DEP="L"` y, como el área siempre se conoce, cualquiera de las cinco determina el par (largo, ancho) y por tanto a las otras cuatro — pero solo `LOT-01`/`LOT-05` están declaradas gemelas. Así que la Parte 4 ya muestra rutinariamente pares co-decididos (p. ej. `LOT-02` y `LOT-04`). Eso **no compromete la corrección** (nunca pueden marcarse de forma inconsistente) y degrada solo la diagnosticidad. Lo señalo para que la objeción 5 se lea como lo que es —cerrar un caso concreto y barato— y no como un rediseño del banco.

**Veredicto**: MODIFICAR (o MANTENER con la equivalencia declarada por escrito, si se prefiere conservar el cupo; pero entonces conviene corregir la glosa del comentario, que hoy justifica la excepción por un criterio distinto del que el resto del archivo aplica).

---

## Objeción 6 — La Parte 5 anuncia el hecho que bloquea a la clave de la Parte 1 en las ramas de omisión única (MEDIA)

**Qué se cuestiona**: la cascada de `h_pref` (líneas 1154-1159) y el comentario que la descarta.

`h_pref <- setdiff(callados_h, falt_p1)`. En una rama que calla **un solo** hecho, la clave de la Parte 1 es indeterminada ⟹ depende de ese hecho ⟹ `h_pref = ∅` ⟹ la Parte 5 nombra en voz alta, en su enunciado, el único dato que falta. Ramas de omisión única: combo 2 (`calla L`, w=26) y combo 4 (`calla N`, w=26) = **52/86 ≈ 60 %** del peso.

El comentario lo descarta con «allí el hecho callado se deduce de leer el enunciado, así que no añade nada que no estuviera ya a la vista». **Esa defensa es circular respecto del constructo**: deducir qué dato falta *es* la habilidad que evalúa el descriptor D4.9. El archivo mismo abre con la premisa correcta (líneas 25-31): «El estudiante ve las seis partes a la vez». Con el hecho ya nombrado, la Parte 1 pasa de «descubrir qué falta **y** mapearlo a la pregunta» a solo lo segundo, y como los tres distractores están determinados —ninguno depende del hecho nombrado— la clave es la única opción que lo necesita.

No es un error de corrección y no propongo cambiar el molde de la Parte 5 (los dos rediseños ya medidos, documentados en las líneas 1098-1114, cierran esa puerta con datos). Propongo dos cosas:

1. **Declararlo**, con su peso, en el bloque de comentarios y en el reporte de aprobación, en lugar de darlo por inocuo. Un residuo medido y declarado es defendible; uno argumentado es lo que la §P4-bis enseñó a desconfiar.
2. Si se quiere mitigar: cuando `h_p5 ∈ falt_p1`, exigir en `ternas_validas` que **≥2** opciones de la Parte 1 contengan el sustantivo clave del hecho nombrado (`cortes` / `medidas` / `partes` / `orden`). Los filtros actuales equilibran `cada`/`met`/`porc` y longitud, pero **no** el vocabulario del hecho, que es justo el canal que este anuncio abre.

**Veredicto**: MODIFICAR (documentación obligatoria; mitigación opcional).

---

## Objeciones bajas (agrupadas, no bloqueantes)

- **Línea 15**: el encabezado dice «banco de 20 preguntas»; son **25** desde la ampliación del 2026-08-12. `DEP` tiene 25 entradas. Deriva documental de un día.
- **`UBI-01`**: se declara `DEP="TAN"`, pero su `valor` cuando está determinada («la franja que le corresponda por su orden en el acta, contando desde la carretera») **no usa N para nada** — esa frase sería igual de válida sin saber el número de partes. El `motivo` sí argumenta bien por qué N hace falta («no se sabe dónde empieza ni dónde termina la franja»). Para que `valor` ejercite la dependencia declarada, convendría que citara la extensión: «la franja de {fmt_dec(b_a)} m de ancho que ocupa el lugar que le da el acta, contando desde la carretera».
- **`fmt_mil` en los feedbacks de P2/P3** imprime «12.000» mientras el enunciado pide escribir «solo dígitos y sin puntos de separación de miles». El enunciado es explícito, así que no induce error, pero la retroalimentación muestra un formato que el gap rechaza.

---

## Sin objeción — verificado por lectura

- **Fuga de medidas cuando `!info_L`**: limpia. `valor` solo se renderiza para preguntas determinadas (Solution P1/P4/P5, `tabla_resumen`, los cuatro `fb_*`), y todas las que citan `largo`/`ancho` tienen `det` con `L`. `contra_cortes` está gateada por `info_L` en sus cuatro usos (`contra_porcion` rama `else`, `UBI-01`, `POR-08`, `POR-09`); `contra_dims` presenta las dos parejas como igualmente posibles, que es su función. **`POR-08` en particular no filtra medidas**: con `!info_L` su `valor` es «el lado más corto» y su `razon` no cita números; su `motivo`/`contra` solo se renderizan con `!info_T`, y `!info_T ⇒ info_L` en los cinco combos vivos.
- **Regla #19 (letter-independence)**: ninguna letra ni posición en toda la sección Solution; las opciones se identifican por texto entre comillas o por código.
- **Incidente Q (la prosa no enumera en el orden interno)**: cumplido en las cuatro partes de elección. P1 lista «las otras tres» citando su texto completo; P4 agrupa por veredicto; P5 agrupa en «sí tienen respuesta» / «tampoco, pero por otro motivo»; P6 no enumera. `tabla_resumen` se ordena por categoría (`which(!es_nd_p1)`, luego la clave) y su primera columna es el texto. Inmune a cualquier re-barajado, interno o de Moodle.
- **`det ↔ DEP` del resto del banco**: coherentes. `POR-01..05` = `LTN` ✓; `POR-06` = `T` ✓ (franjas paralelas ⟹ rectángulo, sin necesidad de L ni N); `POR-07` = `TN` ✓; `POR-09` = `LT` ✓ (la longitud del corte no depende del número de cortes); `LOT-01..05` = `L` ✓; `FIJ-02..05,07` = `N` ✓ (el área siempre se conoce); `FIJ-01`, `FIJ-06`, `FIJ-08` siempre determinadas ✓ — verifiqué que las **siete** plantillas dicen «en partes iguales», que es lo que sostiene a `FIJ-08`. La única que falla es `FIJ-09` (objeción 1).
- **Unicidad de la clave**: mecánicamente asegurada en las cuatro partes de elección (G1/G1b para P1; `sum(afectada_p5)==1` con identidad contra `sol_p5` para P5; el doble `det_bajo` bajo `k_p6` para P6; `sol_p4` derivado de `det` para P4). **La única vía por la que se rompe es la objeción 1**: `FIJ-09` marcada determinada sin serlo en `contextos[[6]]`. No encontré ninguna otra versión con dos opciones válidas en la misma parte.
- **`POR-06` vs `FIJ-09` en la misma parte** («forma de la porción» / «forma del lote»): no es defecto sino acierto — no son co-determinadas (`T` vs siempre) y el punto 4 de «Estrategia para evitar el error» enseña exactamente esa distinción.
- **G4b** (la justificación de la clave no puede culpar a un hecho declarado): sus cuatro regex cubren el `motivo` de `POR-08` («no dice por dónde pasan los cortes»), consistente con que solo sea clave cuando `!info_T`.

---

## Próximos pasos, priorizados

1. **Objeción 1** — añadir «rectangular» a `contextos[[6]]` + la guarda de las siete plantillas. Re-render y re-medir `A limpio` (hoy 0/400) sobre las versiones de ese contexto.
2. **Objeción 2** — `cods[idx_p6]` en `veto_p4_2` y en los escalones 4-5, más la aserción en C-1. Si algún escalón deja de encontrar cupo, reportarlo en vez de relajarlo.
3. **Objeciones 3 y 4** — ramificar `UBI-02` por `info_T`; reescribir `motivo`/`contra` de `POR-08` sin presuponer franjas.
4. **Objeción 5** — decidir: declarar el par `POR-08`/`POR-09`, o dejarlo y corregir la glosa del comentario.
5. **Objeción 6** — declarar el residuo con su peso (~60 %) en el reporte de aprobación.
6. Tras aplicar 1-4, **la FASE 2C caduca**: los cambios tocan textos que se muestran al estudiante y la selección de opciones. Requiere una pasada de confirmación con agente distinto sobre la versión vigente (regla #9 v1.2).

**Dominios no auditados en esta pasada**: renderizado y formatos, ortografía, métricas de diagnosticidad/diversidad/multisemilla (tomadas como dadas del coordinador), y la fidelidad verbatim de la instancia canónica más allá de comprobar que G7/G7a la cubren.

VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS