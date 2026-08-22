# HANDOFF — `tasa_caminata_velocidad_maxima_..._n4_cloze_v1`

**Fecha:** 2026-08-22 · **Estado:** **11/11 — APROBADO para aula** por el profesor (2026-08-22).
**Override §5b: NO RATIFICADO** — ver §0.
**Ubicación:** `01-En-PreDesarrollo/tasa-caminata-velocidad-maxima-n4/cloze/`
**Origen:** `MAT-2026-1-047` — ERA-2026 Sesión 2, **pregunta impresa 47**.
**Clasificación:** Argumentación · Numérico-variacional · **Nivel 4** · D4.2 · clave oficial **B**.
**Hermano:** el SCHOICE del directorio padre. Este CLOZE **hereda su contrato paramétrico y su
pool de 11 errores**; su Parte 5 **reproduce** el ítem oficial **con su cálculo ya resuelto en las
Partes 2 y 3** — esa diferencia no es cosmética, ver §5b. Leer antes `../HANDOFF.md`.

> **Núcleo matemático.** «Más de `tasa` minutos por kilómetro» es una cota **superior** de
> velocidad: `v < 60/tasa` km/h, y en `H` horas se recorren menos de `(60/tasa)·H` km. La cota
> es condición **necesaria, no suficiente**: que el reporte quepa en ella NO demuestra que el
> recorrido fuera a pie.

---

## 0. DECISIÓN DEL PROFESOR — 2026-08-22

Dos decisiones, y **no dicen lo mismo**:

| Decisión | Resultado |
|---|---|
| **Aprobación para el aula (paso 11)** | **APROBADO.** `aprobacion_usuario` sellado. Esta versión (`_v1`) es la que se lleva a estudiantes |
| **Ratificación del OVERRIDE de §5b** (canal del 76,2 % en la Parte 5) | **NO RATIFICADO.** El profesor eligió **rediseñar el andamiaje**: que las Partes 2 y 3 dejen de entregar `vel` y `cota` antes de la Parte 5 |

**Cómo se lee esta combinación.** La v1 se aprueba porque es un artefacto completo y verificado
—corrección 0/100 con control positivo, 6/6 formatos, V1-V9—, no porque el canal se dé por bueno.
El override de §5b **no se acepta**: se ataca su causa. El rediseño se construye como **`_v2` en
paralelo**, sin tocar esta v1, siguiendo el precedente de `excedente-almuerzo-proporcional-n4`.

**Consecuencia para quien lea §5b:** ese apartado documenta el residuo **medido**, y sigue siendo
la descripción correcta del defecto. Lo que ha caducado es su *conclusión* —«se cierra igualmente
por §P7-D»—. La batería sigue en ROJO, y ahora además con una decisión humana escrita de que ese
rojo **se paga**, no se absuelve.

---

## 0-bis. Saneamiento de ortografía para poder commitear (2026-08-22)

El corrector de la regla #7 salía **`exit 1` con 6 hits** sobre este `.Rmd`, y como el hook
`pre-commit` decide por `grep "ERRORES"`, **el commit habría sido rechazado**; el remedio previsible
(`git commit --no-verify`) está PROHIBIDO. Los hits eran el valor de campo `defecto = "conclusion"`
—termina en `-sion` y el corrector no distingue identificador de prosa— y comentarios que lo citan.

Renombrado el **valor** a `"inferencia"` en el `.Rmd` y en `auditoria_propia_cloze.R`, que lo tenía
cableado. **Verificado que el ejercicio NO cambia para el estudiante**: render con las semillas
fijas de `render.R`, **160 → 160 líneas de texto visible, 0 diferencias**. El campo `defecto` sólo
aparece en un `stopifnot` y nunca se emite. **La aprobación 11/11 sigue en pie.**

`Cuantos más` permanece **sin tilde**: es la correlativa relativa de la RAE, y el `exit 2` la
clasifica correctamente como `REVISION_MANUAL`.

## 1. Estructura — Progressive Disclosure de 6 partes

`exclozetype: schoice|num|num|schoice|schoice|mchoice`

| Parte | Tipo | Nivel | Qué evalúa |
|---|---|---|---|
| 1 | schoice | identificar | Qué **tipo** de cota fija el criterio (techo / piso) |
| 2 | num | calcular | La cota de velocidad, `60/tasa` |
| 3 | num | calcular | La cota de distancia, `vel·H` |
| 4 | schoice | evaluar | Qué se concluye al comparar el reporte con la cota |
| 5 | schoice | argumentar | **El ítem oficial**: qué argumento sustenta la decisión |
| 6 | mchoice | transferir | Propiedades del razonamiento con cotas (6 afirmaciones) |

**La Parte 1 sortea su EJE** (velocidad o tiempo). No es adorno: con eje fijo la clave sería
invariante («un valor máximo») en el 100 % de las versiones. Al sortearlo, la clave alterna entre
«máximo» y «mínimo», que son **dos lecturas del mismo criterio** — y distinguirlas es justo lo que
el ítem evalúa. Es §P4-bis aplicado a un gap de identificación.

---

## 2. Gate Hermes (regla #24) — re-ejecutado, no heredado

| | |
|---|---|
| **H-4** | Número **impreso** 47, leído en `pagina_014.jpg` (el folio impreso de la página es 19) |
| **H-1** | La pregunta 47 **no contiene figura**: es texto puro. La tabla de esa página es de la 48 → `flujo_b = false` justificado por lo VISTO |
| **H-2** | Screening de trampa deliberada **negativo** |
| **H-3** | Enunciado, pregunta («la decisión **de la** suspensión») y las 4 opciones verificados carácter a carácter contra el escaneo, incluida la coma idiosincrásica de la opción C |

`verificar_canonica_cloze.R`: **18 instancias canónicas / 600 corridas, 0 divergencias**.
**Límite declarado:** ese script compara contra un texto codificado en sí mismo, así que prueba
ausencia de **deriva**, no fidelidad. La fidelidad la sostiene la lectura visual del JPG.

---

## 3. Corrección — verificada, con control positivo

| Comprobación | Resultado |
|---|---|
| Corrección de las **6 claves** (`auditoria_propia_cloze.R`, N=100) | **0 errores** |
| Segunda clave en P5 (predicado de **SOLIDEZ**, no de identidad) | **0** |
| Segunda clave / incoherencia veredicto↔justificación en P4 | **0** |
| Marca alineada con la verdad recomputada en P4, P5 y P6 (Familia 4) | **0 desalineaciones** |
| **Control positivo** — mutante sin las DOS defensas del Error 33 | **51 / 400 segundas claves** |
| Multisemilla (N=100) | **0 fallos** |
| Unicidad de producto (regla #3) | **300 / 300** (umbral 250) |

**El control positivo es lo que da valor al cero.** Costó dos intentos: el primero relajaba sólo
la precondición por regiones de `NUM-TAS-08` y el mutante **moría por la segunda defensa del
propio `.Rmd`** (`stopifnot(all(es_malo))`), de modo que mi sonda externa nunca llegaba a medirse
y reportaba 0/400 — es el **Incidente S dentro del control positivo**. Hay que relajar **ambas**.

---

## 4. Diagnosticidad — tres pasadas, y qué se aprendió en cada una

**Presupuesto §P7-D: 3 pasadas. Agotado.**

| Pasada | Qué se cerró | p1 | Efecto colateral |
|---|---:|---:|---|
| 1 | Molde de apertura correlacionado con el rol (p1 y p4) | — | `ERR_DIAG_SUPERFICIAL` → **V9 PASS** |
| 2 | **Fuga léxica**: la clave era la única con el token `menos` (dentro de «al menos») | +30,9 → **+10,5 pp** | abrió el par gemelo de longitud |
| 3 | **Par gemelo de longitud**: la simetría clave↔inversa las hacía gemelas | +10,5 → **+2,7 pp** | ninguno medido |

La pasada 2 ilustra el **desplazamiento de canal** que §P7-D describe: cerrar el léxico abrió el
relacional. Y la causa del segundo canal fue **el propio fix del primero** — hacer la clave y su
inversa simétricas las vuelve gemelas por construcción. Esa simetría **no se puede deshacer**: un
distractor que invierte la desigualdad tiene que ser paralelo a la clave o se descarta por forma
(regla #22 §P5). La vía fue romper la **unicidad** del par igualando las cuatro longitudes, que es
la alternativa que el HANDOFF del hermano declara costeable.

### Estado final de la batería §P7 (28 reglas, 6 familias + relacional)

| gap | máximo | regla top | **exceso** | veredicto |
|---|---:|---|---:|---|
| p1 | 33,3 % | par con los mismos numerales | **+2,7 pp** | `NO_CONCLUYENTE` (zona gris) |
| p4 | 31,3 % | par con los mismos numerales | **+0,3 pp** | **`PASS`** |
| p5 | 42,7 % | par con los mismos numerales | **+11,1 pp** | **`BLOQUEA`** ← residuo |

**p1 y p4 están por debajo de la vara oficial** (corpus ICFES +4,6 pp, control +5,3 pp): §P7-A los
declara aceptables. `NO_CONCLUYENTE` **no es un rechazo**: es «no se acredita ni la ausencia».

La familia `relacional` de §P7-E **no existe todavía** en `bateria_eliminacion.R` (v1.6, anterior a
la regla #22 v1.7). Sus tres reglas van etiquetadas `formato` y prefijadas `REL:`, igual que hizo el
hermano. La familia `signo` **sí se sondea** (veredicto binario a pie / no a pie): declararla no
aplicable habría sido el error que §P7 nombra.

Un **bug de mi propia sonda** corregido en la pasada 3: `par_long_gemela` devolvía el primer par de
`which(..., arr.ind)` en vez del **grupo**, y subestimaba el superviviente cuando hay tres o más
gemelas (score 1/2 en vez de 1/3). Corregirlo la vuelve **más** estricta, así que no es ampliar la
batería a mitad de ciclo (§P7-C).

---

## 5. RESIDUO — **OVERRIDE heredado + agravante propio del CLOZE**

### 5a. El par gemelo numérico (heredado del hermano, §5 de `../HANDOFF.md`)

La clave de la P5 cita `{vel, cota, R}` y `NUM-TAS-05` cita los mismos tres numerales, porque su
error consiste justamente en **invertir el sentido de esa misma cota**. Medido: **+11,1 pp**.
La batería **sigue reportando `BLOQUEA` y se deja en ROJO a propósito**.

> **Condición innegociable, heredada:** el día que alguien haga que la batería reporte `PASS` para
> poder cerrar, este ejercicio pasa a ser el precedente de cómo se apaga un gate.

### 5b. AGRAVANTE PROPIO DEL FORMATO CLOZE — medido, no supuesto

Las Partes 2 y 3 **entregan `vel` y `cota` al estudiante antes** de la Parte 5. Medición propia
(`canal_cloze_p5.R`, N=100, nulo exacto 25 %):

| Regla | Tasa | Exceso | Grupo superviviente |
|---|---:|---:|---:|
| «elige la que cita a la vez `vel` y `cota`» (medición propia) | **47,0 %** | **+22,0 pp** | 2 opciones |
| «la única que enuncia AMBAS cotas» (medición del detractor, N=300) | **76,2 %** | **+51,2 pp** | 157/300 versiones con la clave única |

En el SCHOICE hermano ese atajo exige **reconocer** un par convergente; en el CLOZE el andamiaje
**regala los dos números**, así que pasa a ser «busca los que acabo de calcular». Es una agravación
real del formato y se declara como tal. **Mi cifra (47,0 %) subestimaba**: la regla correcta del
detractor mide **76,2 %**.

**Por qué ningún gate lo vio, y es estructural:** `bateria_eliminacion.R` recibe **sólo el vector de
opciones** — nunca el estímulo ni los otros gaps. Un canal que nace de lo que OTRO gap entrega le es
invisible por construcción. Es `feedback_bateria_p7_no_ve_el_estimulo.md` materializado, y en CLOZE
es peor que en SCHOICE porque hay seis gaps alimentándose entre sí.

**LEAK-A y LEAK-B (de O3, declarados, no corregidos):** otras dos afirmaciones de la Parte 6
filtran información de gaps previos en el **79,7 %** y el **40,3 %** de las versiones. El detractor
las dejó explícitamente como «declarar, no corregible barato»: cerrarlas exigiría reescribir el pool
de P6 entero, lo que sí sería una pasada nueva.

**Por qué se cierra igualmente, y por qué eso es un OVERRIDE y no una absolución:**

1. **La fuente oficial tiene el canal peor.** Enumeración **exacta** sobre la instancia canónica
   (determinista, no muestral — no le aplica el mínimo de 20 por estrato de la regla #23): en el
   ítem oficial, «más numerales citados», «única que enuncia dos cotas» y «única que dice *menos*»
   **resuelven el ítem entero** (score 1/1). El CLOZE se queda en 1/2. Filtra **menos** que su
   fuente, y §P7-A dice que un ítem que no filtra más que el examen real no se declara defectuoso.
   *Cautela:* «primera opción» también salió 1/1 en la canónica, pero eso es artefacto de la
   semilla —la clave cayó en posición 1— y `exshuffle: TRUE` la re-mezcla. **No cuenta.**
2. **El grupo residual de 2 exige el concepto evaluado.** El atajo reduce de 4 a 2; pasar de 2 a 1
   obliga a distinguir «menos de» (techo) de «al menos» (piso), que es exactamente lo que evalúa
   la Parte 1. El andamiaje conduce a la respuesta **por la vía que el diseño pretende**.
3. **Presupuesto agotado** (§P7-D, 3 pasadas).

**§P7-B NO lo exime**: exime lo imperceptible, no lo contable, y «dos opciones citan los mismos
números» es discreto y contable. Por eso queda escrito, con su cifra, y **la decisión es del
profesor** (regla #24 **H-5**: relajar nunca es autónomo).

### 5b-bis. Canales de longitud POR RAMA — medidos, y EXENTOS por §P7-B

El Incidente U advierte que la batería **agrega sin condicionar por rama**, y aquí se cumplió: el
agregado de p4 daba `PASS` (+0,3 pp) porque **las dos ramas se cancelan** —en una la clave es la
más corta y en la otra la más larga—. Medido por separado (`rama_p7.R`, N=400):

| gap · rama | n | tasa | margen mediano | % versiones con margen ≥ 15 % |
|---|---:|---:|---:|---:|
| p4 · supera (la más corta) | 221 | 52,9 % | **+3,5 %** | 18,6 % |
| p4 · cabe (la más larga) | 179 | 63,5 % | **+7,6 %** | **0,0 %** |
| p5 · supera (la más corta) | 221 | 57,9 % | **+4,2 %** | 27,1 % |
| p5 · cabe (la más larga) | 179 | — | **−17,8 %** | 0,0 % |

**No son defectos que obliguen.** §P7-B exime la frecuencia sin margen, y aquí el margen mediano
es de 3,5 a 7,6 % sobre un umbral de explotabilidad del 15 %: la clave es nominalmente la más
larga o la más corta por una diferencia que ningún estudiante puede usar. Es el mismo fenómeno que
el centroide léxico del hermano (99 % de tasa, 3,4 % de margen, exento) y la cifra de p5 —57,9 %—
coincide casi exactamente con la suya.

**La lección de método**: perseguir estas tasas sin medir antes el margen habría costado pasadas
enteras sobre canales imperceptibles. *Margen antes que frecuencia.*

### 5c. `NUM-TAS-11` sigue marcado como RELLENO

Heredado sin cambios de `../HANDOFF.md` §5b. Es eliminable sin matemática en el 100 % de sus
apariciones. Primer candidato a sustituir si el ítem se revisa.

---

## 6. Arsenal (versión vigente)

| | |
|---|---|
| Renderizado | **5/5** — html · pdf · pdf+solución · docx · moodle |
| NOPS | **N/A** — `exams2nops()` rechaza cualquier `extype: cloze`; mensaje verificado (`cloze exercises`), no tragado |
| Coherencia matemática · Multisemilla (N=100) | APROBADO (0 errores) · APROBADO (0 fallos) |
| **V1** conteo | 6 `##ANSWERi##` = 6 tipos = 6 partes; **6 gaps en el XML** de Moodle |
| **V2** orden e inmediatez | cada `##ANSWERi##` tras su parte, 1→6 |
| **V3** exsolution/extol por gap | `schoice|num|num|schoice|schoice|mchoice`, `extol` 6 bloques |
| **V4** mínimo 6 partes | 6 |
| **V5** gráficas-opción | **N/A** (0 imágenes; 0 en gaps verificado sobre el XML) |
| **V6** prosa de Solution | OK — **agrupa** por veredicto, no enumera en orden interno (0/100 discrepancias, emparejado por contenido) |
| **V7** unicidad ampliada | OK — `afirm_V` y `afirm_F` disjuntos por construcción, con `stopifnot` |
| **V8** diversidad por gap | **6/6 gaps medidos**, ninguno invariante |
| **V9** diagnosticidad | **PASS** |
| Ortografía (#7) · Glifos (#25) | exit 2 (sólo 1 ambiguo, falso positivo) · sin glifos que rompan pdflatex |
| #18 · #19 · #20 | N/A (0 imágenes) · 0 hits · guard `\newcounter{none}` presente en el `.tex` |
| Fuga por nombre de archivo (§P6) | **N/A** — 0 imágenes, 0 `@@PLUGINFILE@@` en el XML |
| Render en **R limpio** (`--vanilla`, `exams::`) | html y pdf OK — el `.Rmd` es auto-contenido |

### Diversidad por gap (V8) — declaración obligatoria

| Gap | Únicos/100 | Script | Declaración | Justificación |
|---|---:|---|---|---|
| p1 | 8 | baja | variable | 2 ejes × 4 velocidades = 8 claves posibles; el **espacio conceptual** está acotado |
| p2 | 4 | baja | variable | es `60/tasa` con `TASAS` de 4 elementos: **la cardinalidad del parámetro**, no fijeza |
| p3 | 15 | baja | variable | `vel × H`, 20 combinaciones con colisiones legítimas (10×12 = 5×24) |
| p4 | 87 | ok | variable | — |
| p5 | 97 | ok | variable | — |
| p6 | 93 | ok | variable | — |

`WARN_DIV_BAJA` en p1/p2/p3 es **esperado y no bloqueante**: `ERR_DIV_COSMETICA` exige que
**todos** los gaps sean invariantes, y ninguno lo es. Ampliar `TASAS` subiría esas cifras pero
obligaría a re-auditar el pool heredado entero (las precondiciones de `NUM-TAS-04`, `-06`, `-07`,
`-08` dependen de `tasa`), y el contrato paramétrico es **heredado**: no se toca sin motivo.

### Falsos positivos declarados (no son defectos)

| Reporte | Por qué es falso positivo |
|---|---|
| Arsenal FASE 2D: `'solucion'→'solución'`, `'codigo'→'código'` | Son **identificadores de código** (nombre de chunk `{r solucion}` y campo `codigo=` de una lista R). **0 ocurrencias en el HTML renderizado**. El corrector canónico de la regla #7, que distingue código de texto, da exit 2 |
| Corrector #7: `'cuantos'→'cuántos'` | «**Cuantos más** minutos toma…, mayor es…» es la construcción **correlativa relativa**: va sin tilde (RAE). El script lo marca `REVISION_MANUAL` a propósito |
| Arsenal: «Variable `letra_correcta` no encontrada» | Es **exactamente lo que la regla #19 exige**. El arsenal busca el patrón que la #19 prohíbe |
| Arsenal: «Falta tabla de metadatos / análisis de distractores / patrón *Es posible que los estudiantes…*» | Plantilla de Solution **SCHOICE clásico**. La Solution de este CLOZE tiene las 6 subsecciones metacognitivas del paso 4 |
| Answerlist de Solution con 20 entradas frente a 18 del enunciado | **Contrato de R/exams**: los gaps `num` aportan feedback en Solution pero no opciones en el enunciado. Falso positivo recurrente de los auditores |

---

## 6-bis. FASE 2C — auditoría independiente (`APROBAR_CON_CAMBIOS`)

Detractor independiente (no escribió el `.Rmd`), reporte de 31.612 caracteres, **9 objeciones**.

| # | Sev. | Objeción | Estado |
|---|---|---|---|
| O1 | ALTA | Canal léxico de GRUPO en P4: «las que dicen *cota*» aislaba clave+1 (**+12,2 pp**; 50 % en la rama «supera») porque CON-04 y CON-05 no citaban la cota y salen en el 100 % de esa rama | **APLICADA y MEDIDA** |
| O2 | ALTA | P5 se resuelve al **76,2 %** emparejando las cotas que las Partes 2 y 3 acaban de dar | **DECLARADA** (§5b) |
| O3 | ALTA | `afirm_V[1]` imprimía `vel` —la respuesta de la Parte 2— en el 46,7 % de las versiones | **APLICADA** |
| O4 | MEDIA | El enunciado de P4 pedía «la conclusión», pero lo que discrimina es la **premisa** | **APLICADA** |
| O5 | MEDIA | `P1-NADA` era literalmente verdadero bajo lectura estricta | **APLICADA** |
| O6 | MEDIA | Tres defectos del verificador: A9 circular, bloque (E) contaba posición, P4 por identidad | **APLICADA** (a, b, c) |
| O7 | MEDIA | Las cifras §P7 a N=100 subestiman | **DECLARADA** (abajo) |
| O8 | MEDIA | Mi premisa del Incidente R era **falsa** | **CORRECCIÓN ACEPTADA** |
| O9 | BAJA | Menor de redacción | — |

**O1 — resultado medido tras aplicar.** CON-04 y CON-05 pasan a citar la cota. La regla
«GRUPO: las que dicen *cota*» cae a **aplicable 0,0 %** (las cuatro opciones la citan ahora) y su
tasa al **25,0 %**, el nulo exacto: el canal desaparece. p4 pasa de +0,3 a **+0,1 pp**. **No apareció
ningún canal nuevo** — el máximo sigue siendo el par de numerales. Corrección re-verificada
**después** (§P7-D): **0 errores/100**, control positivo 51/400, `ver_p4_txt` inalterado.
Esto **no consumió una 4.ª pasada**: cerró una **ceguera del instrumento** (§P7-E) — la forma de
regla «el GRUPO que dice X» no existía en la batería; todas eran «la ÚNICA que…».

**O8 — el detractor me corrigió, y tenía razón.** Yo afirmé que el CLOZE emite campos del pool que
el SCHOICE no emitía (`nombre`, `causa_raiz`) y que por tanto aplicaba el Incidente R. **Es falso**:
el SCHOICE ya los emite en su Solution (líneas 846-851). La premisa era mía y era equivocada.

**O7 — la vara a N=100 es indulgente justo donde hay canal.** Las cifras §P7 no se reproducen a
N=300: p4 **+0,3 → +4,5 pp**, p5 **+11,1 → +14,0 pp**. El estándar de reporte sigue siendo **N=100**
(regla #23) y no se cambia; lo que no puede hacerse es callarlo, porque **el sesgo va hacia la
indulgencia precisamente en el gap con canal real**. Las cifras de este documento son a N=100.

### Lo que el detractor declaró NO haber auditado

Los 5 renders y el arsenal compartido; el gate visual Hermes contra el escaneo (no abrió el JPG);
el verbatim de la instancia canónica; la Solution más allá de que agrupa por código; y las cifras
de las pasadas 1-3, que tomó de este documento **sin recomprobarlas**. Todo eso está medido en
§§2-4, pero **por el orquestador, no por un tercero independiente**.

## 7. Qué falta

1. **FASE 2C — CERRADA** con `APROBAR_CON_CAMBIOS`; los cambios exigidos que no costaban una
   pasada están aplicados (§6-bis). Queda **sin aplicar la vía (b) de O2**: el propio detractor la
   costeó y compra diagnosticidad pagándola en otra dimensión (fuerza `NUM-TAS-11`, ya marcado
   RELLENO). Registrada como opción para la revisión post-aula.
2. **Paso 11 — APROBADO** el 2026-08-22 (§0). En la misma decisión el profesor **NO ratificó** el
   override de §5b y encargó **rediseñar el andamiaje** en una **`_v2`**: que las Partes 2 y 3
   dejen de entregar `vel` y `cota` antes de la Parte 5. Esta v1 **no se toca** por ese encargo.
3. **Nada está commiteado.** `git add` pendiente de decisión.
4. El SCHOICE hermano **no se tocó**: sigue en 10/11 esperando su propia aprobación.
5. Tras el aula (Nivel 3) → `/promover-ejercicio`.

---

## 8. Trampas encontradas en este ciclo, para quien siga

- **Un control positivo puede morir por la defensa equivocada.** Relajar una sola de las dos
  defensas del Error 33 hacía que el mutante abortara en el `stopifnot` del `.Rmd` y mi sonda
  externa reportara 0/400 — indistinguible de «limpio». Hay que relajar **todas** las defensas que
  cubren el mismo defecto, y la guarda de «mutante mal construido» debe comprobar **cada una**.
- **La cobertura de V8 depende de los NOMBRES de las variables.** Sin los alias `opciones_p6`,
  `exsol_p2`, `exsol_p3`, el script medía **3 de 6 gaps** y emitía `NOTA DE COBERTURA`. Tres gaps
  quedaban SIN VERIFICAR y el resumen se leía como si estuvieran bien.
- **Un `if/else` partido a nivel top-level de un chunk no es R válido.** Costó el primer smoke.
- **`--vanilla` no carga el `.Rprofile`** que añade la biblioteca personal: un «no hay paquete
  llamado exams» ahí es del entorno, no del `.Rmd`. Hay que fijar `.libPaths()` explícitamente.
- **Cerrar un canal léxico con simetría crea un canal de longitud.** Es inevitable cuando el
  distractor debe ser paralelo a la clave; la salida no es romper la simetría sino romper la
  **unicidad** del grupo gemelo.
- **Un pool nuevo no está auditado por nadie.** Los pools de la P4 (`conclusiones`) y de la P6
  (`afirm_V`/`afirm_F`) **no existen en el hermano**: se blindaron con `stopifnot` de las dos
  condiciones de las que depende su clasificación (`tasa != vel` y `cota_baja < cota < cota_alta`),
  para que ampliar `TASAS` no los rompa en silencio.

---

## 9. Artefactos

| Archivo | Qué es |
|---|---|
| `tasa_caminata_..._n4_cloze_v1.Rmd` | El ejercicio |
| `auditoria_propia_cloze.R` | Corrección de las 6 claves + solidez + control positivo + batería §P7 + vara canónica |
| `verificar_canonica_cloze.R` | Ausencia de deriva de la instancia canónica |
| `canal_cloze_p5.R` | Canal propio del formato CLOZE (§5b) |
| `rama_p7.R` · `margen_rama.R` | Batería condicionada por rama (Incidente U) y su margen (§P7-B) |
| `p6_balance.R` | Balance V/F y consistencia del gap `mchoice` |
| `unicidad_cloze.R` · `v6_check.R` · `smoke_cloze.R` | Regla #3 · V6 · smoke N=100 |
| `render.R` | Los 5 formatos + comprobación del N/A de NOPS |
| `salida/` | html, pdf, pdf+solución, docx, moodlequiz.xml |

Los scripts llevan **nombre único por rol** a propósito: el scratchpad se comparte con los
subagentes y en el ciclo del hermano un script propio fue sobrescrito por el de un detractor
(`../HANDOFF.md` §8).
