# HANDOFF — `tasa_caminata_velocidad_maxima_..._n4_cloze_v2`

**Fecha:** 2026-08-22 · **11/11 APROBADO** (profesor, 2026-08-22) · elegido para el aula · **Estado:** 10/11 — falta la aprobación del profesor
**Última pasada:** cierre de la **Objeción 2** (pool de la Parte 4) — ver §5-ter y §5-quater. Es la
**pasada 2 de 3** (§P7-D). **md5 vigente:** `67b6500b648af0c7ddeb1ccd280258fc`.
**Origen:** `MAT-2026-1-047` — ERA-2026 Sesión 2, **pregunta impresa 47**
**Predecesora:** `../cloze/` (**v1, APROBADA 11/11 — NO SE TOCA**). Leer su `HANDOFF.md` §0 y §5b.
**Hermano:** `../` (SCHOICE, 10/11).

---

## 0. Por qué existe esta v2

El profesor tomó **dos decisiones que no dicen lo mismo**: aprobó la v1 para el aula, y **NO ratificó
el override de su §5b**. Pidió cerrar la causa del canal rediseñando el andamiaje. Esta v2 es ese
rediseño. La v1 sigue siendo válida y aprobada; la v2 no la sustituye por defecto.

---

## 1. El defecto, medido — y la restricción que invalida la solución obvia

En la v1, las Partes 2 y 3 pedían `vel` y `cota` **del caso oficial**, y la clave de la Parte 5 los
imprime siempre. Medido sobre 300 versiones:

| Fuga | v1 |
|---|---:|
| LEAK-A — la respuesta de P2 aparece en las opciones de P5 | **100 %** |
| LEAK-B — la respuesta de P3 aparece en las opciones de P5 | **100 %** |

**En un CLOZE de R/exams no hay revelación secuencial**: los 6 gaps van en un solo `<questiontext>`
y se muestran a la vez. Por eso **reordenar las partes no cierra nada**, y por eso P2 y P3 eran
gaps de **lectura**, no de cálculo, para cualquiera que mirase abajo.

---

## 2. El rediseño: desacoplar el andamiaje del caso que se juzga

P2, P3 y P4 pasan a operar sobre un **caso de práctica** con su propia `pr$tasa` y `pr$H`. El
estudiante ejecuta el procedimiento completo sobre él y después debe aplicarlo **él** al caso
oficial en P5.

**Guarda dura de disjunción**, verificada en cada versión: ningún numeral del caso de práctica
coincide con `tasa`, `H`, `vel`, `cota`, `reporte` **ni con los derivados** `tasa·H`, `vel·24`,
`cota/2` — que las opciones de P5 llegan a imprimir (NUM-TAS-04, -08, -09). Sin los derivados la
fuga volvía por una puerta que la guarda estrecha no miraba: LEAK-B se quedaba en 4,3 %.

### ¿Sigue siendo Progressive Disclosure legítimo?

Sí, y la demanda **sube**. El arco es: **P1** concepto (¿techo o piso?) → **P2-P4** procedimiento
completo sobre un caso guiado → **P5** transferencia al caso oficial → **P6** generalización. Es el
patrón **«ejemplo trabajado → transferencia»**, que en la taxonomía SOLO es precisamente
*Abstracto-Extendido*: aplicar el principio a un caso nuevo.

No son «dos ejercicios con grapas» porque el caso de práctica **no es otro tema**: usa el mismo
criterio («más de X minutos por kilómetro»), la misma cadena de razonamiento y el mismo tipo de
decisión. Lo único que cambia son los datos — que es lo que define un ejemplo trabajado.

Lo que se pierde: la continuidad «calculo la cota de **este** caso y juzgo **este** argumento». Lo
que se gana: en la v1 el estudiante podía **copiar** las dos cifras; en la v2 tiene que **rehacer**
el procedimiento. Esa es la demanda N4 que el andamiaje regalaba.

---

## 3. Criterios de aceptación — medidos

| Criterio | v1 | **v2** | Veredicto |
|---|---:|---:|---|
| LEAK-A | 100 % | **0,3 %** | **CERRADO** |
| LEAK-B | 100 % | **2,3 %** | **CERRADO** (el residuo es coincidencia de *subcadena* en la sonda —«48» dentro de «480»—, no fuga real) |
| Corrección de las 6 claves | 0/100 | **0/100** | verificada **después** del rediseño (§P7-D) |
| Control positivo de la sonda de solidez (P5) | 51/400 | **49/400** | dispara: el cero está acreditado |
| Predicado de solidez de **P4** + sus dos controles | no existía | **0/100 · 100/100 · 0/100** | §5-ter |
| Batería §P7 p4 | +0,1 pp | **−1,0 pp** | `PASS` (tras §5-ter) |
| Batería §P7 p1 | +2,7 pp | **+2,9 pp** | zona gris, bajo la vara oficial (+4,6/+5,3) |
| Batería §P7 p5 (relectura O6) | — | **≈ +14 pp** | la cifra correcta; ver §4 |
| Batería §P7 p5 | +11,1 pp | **+10,9 pp** | ver §4 |

### Criterio 1 — el hallazgo que corrige el encargo

El encargo pedía que «la única que enuncia ambas cotas correctas» cayera del 76,2 % «a un nivel
comparable al del SCHOICE hermano». **Medí la misma sonda sobre el hermano:**

| Artefacto | Tasa | Clave única en |
|---|---:|---:|
| **SCHOICE hermano** | **77,8 %** | 167/300 |
| CLOZE v1 (P5) | 75,7 % | 154/300 |
| **CLOZE v2 (P5)** | **75,8 %** | — |

**El canal es INTRÍNSECO AL ÍTEM, no lo introduce el andamiaje**: la clave del ítem oficial es la
única que enuncia una cota calculada, y eso ocurre igual en el SCHOICE, donde no hay andamiaje
ninguno. El criterio estaba **cumplido desde la v1** (2,1 pp por debajo del hermano) y **ningún
rediseño del andamiaje puede moverlo** — sólo podría moverlo cambiar las opciones del ítem oficial,
que H-2/H-3 prohíben.

Lo que el andamiaje **sí** causaba, y lo que esta v2 cierra, es LEAK-A/LEAK-B.

---

## 4. Residuo declarado

**p5, +10,9 pp — heredado e intrínseco.** Es el par gemelo numérico del §5 del hermano: la clave
cita `{vel, cota, R}` y `NUM-TAS-05` los mismos, porque su error es invertir el sentido de esa misma
cota. **No lo causa el andamiaje** (el SCHOICE lo tiene igual) y cerrarlo exigiría tocar las
opciones del ítem oficial. La batería **se deja en ROJO**, no se apaga.

**WARN_DIV_BAJA en p1/p2/p3**: cardinalidad del espacio (`TASAS` tiene 4 elementos), no fijeza.
Ningún gap es invariante.

**Falsos positivos declarados** (idénticos a la v1): `Cuantos más` es la correlativa relativa (sin
tilde, RAE) y `defecto = "conclusion"` es un **valor de campo comparado en código**, no texto
visible.

---

## 5. Arsenal

| | |
|---|---|
| Renderizado | **5/5** — html · pdf · pdf+sol · docx · moodle. NOPS N/A (mensaje `cloze exercises` verificado) |
| V1 · V2 · V3 · V4 | 6 `##ANSWERi##` = 6 tipos = 6 partes, en orden · 6 gaps en el XML |
| V5 · V6 · V7 | N/A (0 imágenes) · OK · OK |
| V8 diversidad por gap | **6/6 medidos**, ninguno invariante |
| V9 diagnosticidad | **PASS** |
| Coherencia · Multisemilla (N=100) | APROBADO (0 errores) · APROBADO (0 fallos) |
| Unicidad de producto (regla #3) | **300/300** (umbral 250) |
| Glifos (#25) · regla #19 · #20 | exit 0 · 0 hits · guard presente |

---

## 5-bis. FASE 2C — detractor independiente · `APROBAR_CON_CAMBIOS`

Auditoría anclada a un md5 concreto; **cualquier edición posterior la caduca** (regla #9 v1.2).
Encontró **algo que yo no vi**, y es la lección de este ciclo:

### La misma fuga, desplazada un gap (Objeción 1, ALTA — CERRADA)

`CON-04` de la Parte 4 interpolaba `pr$vel`, que es **exactamente la respuesta de la Parte 2**:
impresa verbatim en **el 66 % de las versiones** (100 % en la rama «supera», donde `CON-04` sale
siempre). **No nació del desacople: sobrevivió a él** — el texto pasó de imprimir `vel` a imprimir
`pr$vel` y siguió imprimiendo la respuesta del gap de al lado.

Yo medí la fuga hacia P5 y **no la medí hacia P4**. Medido tras el fix: **66,0 % → 0,0 %**
(global y en la rama «supera»), con la corrección re-verificada **antes** de la diagnosticidad.

### Otras objeciones aplicadas

| Obj. | Qué | Estado |
|---|---|---|
| 3 (MEDIA) | La guarda no cubría el cuarto derivado, `reporte·tasa` (NUM-TAS-07) | **Añadido** |
| 4 (MEDIA) | El comentario afirmaba «ya no puede copiar ninguna cifra» — que la O1 refutaba. Y el patrón **no** es «ejemplo trabajado» (ahí la solución se *muestra*; aquí se *resuelve*): es un **par de problemas isomorfos**, transferencia cercana | **Reescrito** |
| 8 (BAJA) | La redacción «describe correctamente esa comparación» es **portante**: sin ella `CON-03` sería discutible en una rama | **Marcada NO SIMPLIFICAR** |

### Residuos que el detractor declara y NO se corrigen

- ~~**Objeción 2 (MEDIA)**~~ — **CERRADA en la pasada siguiente, por orden del profesor. Ver §5-ter.**
- **Objeción 6 (BAJA)** — a la batería le falta la forma **GRUPO** de «dos cotas»: el residuo real de
  p5 es **≈ +14 pp**, no +10,9. No cambia el veredicto (ya era `BLOQUEA`) ni depende del andamiaje
  (el SCHOICE lo tiene igual). **La cifra correcta es ésta.**
- **Objeción 5** — el `ejercicio_state.json` se selló **en lote, 480 ms antes** de que existieran los
  artefactos que atestigua. El sello es **declarativo**; la evidencia son las mediciones de §3.
- **Objeción 7** — `--fix` de ortografía desincronizaría `auditoria_propia_cloze.R:137`, que tiene
  cableado el literal `"conclusion"`. **No auto-corregir.**

### Desviación declarada frente al impreso (H-3)

El stem de P5 antepone una frase puente que **no está en el impreso** y **re-sirve** `tasa` y
`reporte`. El texto del ítem oficial es verbatim (0 divergencias, 18/600); **su entorno no lo es**, y
esa frase alivia una demanda de recuperación que el original sí impone. Es consecuencia del
rediseño, no descuido.

### Lo que el detractor NO auditó — y qué se cerró después

Declaró fuera de su cobertura: los 5 renders más allá de `grep`, el pandoc de RStudio (regla #20),
V8/V9/multisemilla/unicidad, la literalidad de los `exextra` contra el catálogo y las seis
subsecciones de la Solution.

**Cerrados en la pasada 2** (§5-ter), todos con evidencia:

| Hueco | Cómo se cerró |
|---|---|
| Maquetación de los dos escenarios en **PDF** | PDF → PNG e **inspección visual**: los dos escenarios quedan separados por filete horizontal y encabezado en negrita («Antes de juzgar ese caso, practique…» / «Vuelva ahora al caso del corredor»). Sin solapes ni desbordes |
| **pandoc de RStudio** (regla #20) | `RSTUDIO_PANDOC` → **pandoc 3.8.3**, `exams2pdf()` **OK**. El guard `\@ifundefined{c@none}` está presente y el entorno del profesor compila |
| **Literalidad de los `exextra`** contra el catálogo canónico | Comparación carácter a carácter: `Afirmacion`, `Evidencia`, `Descriptor` (D4.2) y `Estandar` son **literales exactos** de `evidencias-mat.json`, `niveles-mat.json` (`CANONICO_INMUTABLE`) y `estandares-mat-ebc.json`. D4.2 está en `mapeo_codigos_proyecto_a_oficial` ⇒ Nivel 4, coherente con `exextra[Nivel]: 4` y con `DOK 3 ⇒ Nivel ≥ 3` |
| **Seis subsecciones de la Solution** | Presentes en el HTML renderizado: «Qué hay que decidir», las cinco por parte (identificar · calcular · comparar · transferir · propiedades), «Reflexión metacognitiva» y «Cómo evitar el error la próxima vez» |
| **¿El gate nuevo es satisfacible?** | **400/400** versiones evaluadas sin un solo aborto, con serie de semillas independiente (`s·4409+23`). Más el smoke (100), multisemilla (100) y unicidad (300): ~900 versiones sin que el `stop()` se dispare |

**DOCX abierto de verdad** (pasada 2): 84 párrafos, 1 tabla `<w:tbl>`, 0 imágenes, sin fugas de
markup. Dos hallazgos que parecían defectos y **no lo son** — los dos verificados contra la **v1
aprobada**, que se comporta igual:

- **`##ANSWER1..6##` aparecen literales en el DOCX.** Es propiedad de `exams2pandoc()` con
  `extype: cloze` (v1 aprobada: **6**; v2: **6**), no una regresión. El DOCX no es el canal de
  entrega de un CLOZE — lo es Moodle, donde los 6 gaps sí se resuelven.
- **«la velocidad queda acotada por ⟨hueco⟩ kilómetros por hora»**: el número **sí está**, como
  **OMML** (`<m:t>`, 46 runs; v1: 29). El hueco era artefacto de extraer sólo `<w:t>`. *Una sonda
  que no mira donde vive el dato no mide ausencia: mide su propia ceguera.*

Sigue **sin auditar**: la auditoría visual masiva HTML (`auditor-visual-html`, paso 6b), que no se
ejecutó en ningún ciclo de esta v2.

---


## 5-ter. PASADA 2 (§P7-D) — cierre de la Objeción 2: el pool de la Parte 4

**Encargo del profesor (2026-08-22):** corregir la Objeción 2. Es la **pasada 2 de 3**.

### El defecto, medido

En la rama `pr$excede == TRUE` («el reporte supera la cota») el pool producía **2 conjuntos de
opciones distintos y nada más**:

| Conjunto | Frecuencia en la rama |
|---|---:|
| `CON-04 + CON-05 + CON-06 + CLAVE` | 61,2 % |
| `CON-03 + CON-04 + CON-05 + CLAVE` | 38,8 % |

`CON-04` y `CON-05` salían en **49/49** versiones de esa rama: con `excede = TRUE` eran los únicos
con el veredicto de la clave que sobrevivían a sus precondiciones, y
`stopifnot(length(mismo_p4) >= 2L)` se cumplía **con igualdad**. Es el **Error 27** puro: varían los
números, no el **tipo** de error. Ningún validador del arsenal lo mide —`validar_diversidad_sustantiva.R`
mide la variación del **valor** de la clave, no la del tipo de distractor— y p4 estaba en `PASS`.

> **Ojo al medir**: la rama que gobierna P4 es `pr$excede` (caso de **práctica**), no `excede` (caso
> oficial). Con la variable equivocada salen 9 conjuntos y el defecto desaparece. Fue mi primer
> intento de medición y era falso.

### El orden importaba: primero el predicado, después el pool

**Escribí el predicado de solidez ANTES de tocar el pool**, y no es ceremonia: es la lección que le
costó un ciclo entero al SCHOICE hermano (§2 de `../HANDOFF.md`). Allí `NUM-TAS-08` resultó ser un
argumento **sólido** en el 2,8 % de las versiones y todo el arsenal seguía verde, porque el predicado
comprobaba **identidad con la clave** (`cota_km == cota`) en vez de **solidez**.
*Un argumento puede ser sólido sin ser el mismo argumento.*

Ampliar el pool de P4 sin ese predicado es la receta exacta para meter una segunda clave.

#### Cómo está construido (`solido_p4()`, chunk `data_generation`)

Cada opción **declara su estructura** y el evaluador es **genérico**, de modo que cubre conclusiones
futuras con umbral alternativo sin que nadie anticipe la región donde se volverían sólidas:

| Campo | Qué declara |
|---|---|
| `umbral()` | el umbral de **distancia** (km) que la opción invoca; `NA` si no invoca ninguno bien formado (compara contra una velocidad, contra minutos, o lo sustituye por una impresión) |
| `sentido` | `"superior"` (techo) · `"inferior"` (piso: lectura falsa) · `NA` |
| `rel` | lo que la opción **afirma** sobre el reporte frente a ese umbral |
| `concluye` | `no_pie` · `no_basta` · `compatible` · `demostrado_pie` · `sin_juicio` |

El criterio da un **techo** (`cota = vel·H`), así que:

- «no pudo hacerse a pie» se sigue de `R > u` **sólo si `u >= cota`** (u es un techo válido);
- «no basta para descartar» se sigue de `R < u` **sólo si `u <= cota`**;
- «queda demostrado que fue a pie» **nunca** se sigue (necesario ≠ suficiente);
- «no se puede decidir» **nunca** se sigue (la cota es exacta y basta).

**Es una precondición que ABORTA EL RENDER**, no una medición a posteriori: `stopifnot` de que la
clave es sólida, y `stop()` si alguna conclusión del pool lo es. Es la única forma de defensa que no
se reabre al volver a ampliar el pool.

#### El predicado ya pagó su coste antes de ejecutarse

Al diseñar la ampliación consideré un distractor «promediar en vez de acotar»: *«Los R kilómetros dan
un promedio por hora que cualquiera puede sostener, de modo que son compatibles con un recorrido a
pie»*. Declarado con honestidad (`umbral = cota`, `sentido = "superior"`, `rel = "<"`,
`concluye = "compatible"`), el predicado lo marca **SÓLIDO** en toda la rama «cabe»: `R/H < vel` ⟺
`R < vel·H`, que es **el mismo argumento de la clave con otra aritmética**. Habría sido una segunda
clave en el ~50 % de las versiones. **Se descartó antes de escribirlo.**

### Verificación — corrección PRIMERO, diagnosticidad después

| Comprobación | Resultado |
|---|---|
| Segundas claves en P4 (predicado de solidez, N=100, semillas `s·4409+23` independientes) | **0 / 100** |
| **Control positivo** — argumento sólido con texto **distinto** del de la clave | **100 / 100 dispara** |
| **Control negativo** — mismo molde con umbral inválido (cota de una hora) | **0 / 100** (no marca de más) |
| Corrección de las **6 claves** (`auditoria_propia_cloze.R`, N=100) | **0 errores** |
| Control positivo de la sonda de solidez de P5 | **49 / 400** dispara |
| Coherencia `veredicto` ↔ `concluye` ↔ regex `ver_p4_txt` | `stopifnot`, 0 fallos |

El control positivo es lo que da valor al cero, y **el de P4 prueba algo que el verificador viejo no
podía**: su mutante tiene texto distinto del de la clave, así que la comprobación de identidad
textual (`identical(cc$texto, texto_p4_clave)`) lo dejaba pasar.

### El pool: 6 → 9 conclusiones

| Código | Veredicto | Error conceptual | Por qué NO es sólido |
|---|---|---|---|
| `CON-07` | TRUE | Cota de **una hora** aplicada al recorrido completo | `umbral = pr$vel`, y `vel >= vel·H` es falso mientras `HORIZONTES` no contenga `h = 1`. Su umbral **sí** es un techo legítimo — sólo que del horizonte equivocado: es la especie exacta del **Error 33**, y por eso se declara con su umbral real y decide el predicado, no yo |
| `CON-08` | TRUE | Minutos por kilómetro comparados con kilómetros | error de categoría → `umbral = NA` |
| `CON-09` | FALSE | Falacia de la información insuficiente (pide datos que el criterio no usa) | `concluye = "sin_juicio"`, que nunca se sigue |

`CON-09` lleva `precondicion = function() pr$excede` **por diagnosticidad, no por aplicabilidad**:
`CON-06` es la otra que concluye «no se puede decidir», y en esa rama `opuesto_p4` elige **una sola**,
de modo que no pueden coincidir. Juntas habilitarían «elimina las dos que se rinden» → 1/2 sin razonar.

### Resultado

| Métrica | Antes | Después |
|---|---:|---:|
| Conjuntos distintos, rama **supera** | **2** | **15** |
| Conjuntos distintos, rama **cabe** | 9 | **14** |
| Código más frecuente en «supera» | CON-04 y CON-05 al **100 %** | CON-05 al **61,2 %** |
| Batería §P7 **p4** | −0,0 pp `PASS` | **−1,0 pp `PASS`** |
| Batería §P7 p1 / p5 | +2,9 / +10,9 pp | **+2,9 / +10,6 pp** (intactos) |
| Fuga: respuesta de P2 o P3 impresa en las opciones de P4 (Objeción 1) | 0 % | **0 %** |
| V8 · V9 · unicidad · multisemilla | 6/6 · PASS · 300/300 · 100/100 | **igual** |

La rama «cabe» **no se degradó**: mejoró de 9 a 14 conjuntos.

### El fix abrió un canal, y por eso hubo que medir el vector completo

Las tres conclusiones nuevas, **en su redacción larga inicial**, eran todas más largas que cualquier
variante de la clave. En la rama «supera», `mismo_p4` pasó a elegir dos de cuatro opciones **todas**
por encima de la clave:

| Sonda H1 «la clave es la única más corta», rama supera | Frecuencia | Margen mediano | ¿Explotable? |
|---|---:|---:|---|
| Antes de la pasada (N=100) | 38,8 % | 10,4 % | no (< 15 %) |
| Con los textos largos (N=100) | **61,2 %** | **24,5 %** | **sí** |
| Con los textos acortados (N=100) | **36,7 %** | **2,4 %** | no |
| Con los textos acortados (**N=400**, medición del detractor) | **45,3 %** | **4,5 %** | no |

**O4 del detractor**: la tasa a N=100 subestima en ~8,6 pp la de N=400. El **veredicto no cambia**
—los dos márgenes siguen muy por debajo del 15 %— pero **«36,7 %» no es una constante del ítem**, y
una diferencia de 2,1 pp frente al 38,8 % previo **no es una mejora medida**: es ruido a esa N. Lo
que sí está medido es el **derrumbe del margen** (10,4 % → 2,4 %/4,5 %), que es lo que decide.

Es el **desplazamiento de canal** que §P7-D describe, y lo causó mi propio fix. Se corrigió **donde
nació** —en los tres textos nuevos, que no los había auditado nadie todavía— y **no** tocando la
clave ni las seis conclusiones anteriores: la variante corta de la clave (`avk4 == 1`) es justamente
lo que la pasada 3 de la v1 introdujo para que la clave no ocupara una posición fija en el orden de
longitud. **El objetivo no era igualar longitudes sino que el pool ENCUADRE a la clave**: que haya
distractores por debajo y por encima.

Efecto colateral medido y **declarado**: en la rama «cabe», «la clave es la única más larga» sube de
19,6 % a **43,1 %**, con margen **12,1 %** (p90 13,8 %) — **por debajo del umbral de explotabilidad
del 15 %**, luego inexplotable por §P7-B. Es la misma figura que el hermano documenta en su §8: *la
frecuencia sin margen engaña*.

### RESIDUO DECLARADO — el canal de longitud se DESPLAZÓ DE RAMA, y el agregado lo esconde

La batería agregada da p4 en **`PASS`, exceso −1,0 pp**. **Eso no acredita nada aquí**, y hay que
decirlo: las dos ramas son estructuralmente distintas y sus canales apuntan en **direcciones
opuestas**, así que se cancelan al promediar. Es el **Error 30** (la sonda agrega sin condicionar por
rama), y la regla #22 v1.3 obliga a medir por rama. Medido (batería reducida de 14 reglas, N=200; sus
cifras **no** son comparables con las 31 reglas del verificador — §P7-C):

| rama | antes de la pasada | después | dirección |
|---|---|---|---|
| **supera** | «la más corta» 44,1 % · **+13,7 pp** · `BLOQUEA` | «la más corta» 39,8 % · **+9,3 pp** · `BLOQUEA` | **mejora** |
| **cabe** | «primera opción» 29,0 % · **−1,6 pp** · `PASS` | «la más larga» 44,9 % · **+14,1 pp** · `BLOQUEA` | **empeora** |

**CORRECCIÓN A MI PROPIA ATRIBUCIÓN (O3 del detractor).** Yo escribí que el fix «abrió» el canal de
la rama «cabe». Es cierto para la sonda **incondicional** de arriba, y **falso para la que importa**.
El detractor midió la sonda **condicional** —el estudiante que hizo P3 conoce el veredicto, descarta
la opción opuesta y elige entre las TRES restantes— con un A/B contra el pool de 6 y las mismas
semillas:

| «la más larga del trío», rama cabe | pool de 6 (pre-pasada) | pool de 9 (ahora) |
|---|---:|---:|
| acierto (azar 33,3 %) | **69,3 %** | **69,3 %** |
| margen mediano | **+12,1 %** | **+12,1 %** |

**Idéntico: el canal es PREEXISTENTE y esta pasada no lo introdujo ni lo agravó.** El salto de la
sonda incondicional (19,6 % → 43,1 %) es real pero mide otra cosa: `CON-07`/`CON-08`, cortas y de
veredicto TRUE, desplazaron a `CON-04`/`CON-05` como opción **opuesta**, así que la clave se volvió
más saliente frente a una opuesta más corta — un canal que **sólo puede usar quien no sabe el
veredicto, es decir quien no ha hecho el ejercicio**. La cifra que importa no se movió.

**Se declara y NO se persigue**: margen **+12,1 % < 15 %** ⇒ inexplotable por §P7-B (sólo el 11,1 %
de esa rama supera el umbral), el encargo lo prohíbe, y §P7-D deja una sola pasada.

**Y el `PASS` agregado de p4 (−1,0 pp) NO acredita la ausencia de canal en ninguna rama**: promedia
0,0 % (supera) con 49,2 % (cabe) → 22,0 % ≈ azar. Es el **Error 30** literal. Un `PASS` que no
distingue «sin canal» de «dos canales que se cancelan» es la clase de verde que este repositorio ya
se ha comido dos veces.

**Lo que atenúa el hallazgo, y es medición, no atenuante retórico:** la regla de la batería
(«¿es la única más larga?») **no filtra por margen**. La sonda calibrada del repo sí, y con el filtro
del 15 % el canal casi desaparece: `validar_diagnosticidad.R` reporta para p4 **H1 más-larga 1 %
(margen 12 %)** y **H1 más-corta 8 % (margen 4 %)** → **V9 `PASS`**. Es decir: la propiedad
«ser la más larga» ocurre a menudo, pero por **12 caracteres sobre 130** — dos palabras—, que es
justo lo que §P7-B declara inexplotable.

**Ambas ramas quedan bajo la vara oficial en margen y ninguna la supera en algo perceptible**, pero
el rojo por rama **se deja escrito, no se apaga**. Si el profesor gasta la pasada 3 aquí, la vía es
la misma que funcionó en «supera»: dar a las conclusiones **de veredicto FALSE** un rango de
longitudes que encuadre a la clave larga de la rama «cabe» (`avk4 ∈ {2,3}`, 130 caracteres), sin
tocar la clave.


---

## 5-quater. FASE 2C de la pasada 2 — detractor independiente · `APROBAR_CON_CAMBIOS`

Lanzado **sin `name:`** (con `name` sería *teammate* y su reporte no llegaría), sobre el md5
`ac35e809a22f1efccc9a6401ac98b6e8`. Agente distinto del que escribió el código. **5 objeciones:
0 críticas · 0 altas · 3 medias · 2 bajas. Ningún defecto de corrección.**

Reconstruyó el predicado leyendo los nueve textos y evaluándolos **él** contra el criterio, sin usar
las declaraciones del archivo, y **confirmó las cifras** (0 errores, 49/400, +2,9/−1,0/+10,6 pp,
15/14 conjuntos, fuga 0,0 %, V9 `PASS`, renders frescos). Añadió un **control causal que yo no
había hecho**: un mutante = estado pre-pasada (pool truncado a 6) con las mismas 400 semillas →
**2 menús en la rama supera**, frente a **18** con el pool de 9. *El «2» reproduce el defecto: la
corrección es real, no una coincidencia de muestreo.*

| Obj. | Sev. | Qué | Estado |
|---|---|---|---|
| **1** | MEDIA | **El predicado era genérico sobre el UMBRAL pero LISTA BLANCA sobre la CONCLUSIÓN**: `sin_juicio` estaba cableado a `FALSE`, y en la rama «cabe» «no se puede decidir» **es** lo que se sigue. Una opción así sería una segunda clave y el predicado habría dicho `FALSE` sin mirar su umbral. Hoy no ocurre, pero **no gracias al predicado**: CON-06 se salva por una premisa falsa que el predicado no comprueba, y CON-09 por una precondición puesta **por diagnosticidad**. La defensa descansaba sobre una restricción tomada por otra razón — la firma exacta del Error 33 | **APLICADA**: la etiqueta se parte en `sin_juicio_por_cota_invalida` (nunca sólido) y `sin_juicio_indecidible` (**condicional**: sólido si `R < u <= cota`) |
| **2** | MEDIA | `CON-07` declaraba `defecto = "premisa"` y su premisa es **verdadera** por su propia precondición (`pr$R > pr$vel`); lo que falla es la inferencia. Según la taxonomía del propio archivo eso es `"conclusion"` | **APLICADA** |
| **3** | MEDIA | El canal condicional del 69,3 % en la rama «cabe» no estaba declarado, y el `PASS` agregado de p4 lo promedia hasta hacerlo desaparecer (Error 30) | **APLICADA al reporte** (§5-ter). El artefacto **no se toca**: preexistente y margen < 15 % |
| **4** | BAJA | La cifra «36,7 % / 2,4 %» no se sostiene a N=400 (45,3 % / 4,5 %) | **APLICADA al reporte** |
| **5** | BAJA | Referente ambiguo en `CON-07` («esa cota … en una hora» = `cota/H`) y `CON-08` («minutos por kilómetro de esa cota» son del **criterio**). No los vuelve sólidos ni dispara ninguna sonda: es legibilidad | **NO se toca.** Reescribir un texto de opción reabre la calibración de longitud que esta pasada acaba de cerrar. **Nota para una eventual pasada 3** |

**Verificación de que O1 y O2 son neutrales en medición** (era la condición para aplicarlas dentro de
esta misma pasada): corrección **0/100** y **0/200** con serie independiente · solidez **0/400** con
control positivo **400/400** y negativo **0/400** · batería **+2,9 / −1,0 / +10,6 pp**, idénticas ·
renders **5/5**. **Cero desviación**, como el detractor había previsto.

**Consecuencia formal:** aplicar sus cambios **caduca su propio veredicto** (regla #9 v1.2). El md5
auditado fue `ac35e809…`; el vigente es **`67b6500b648af0c7ddeb1ccd280258fc`**. La FASE 2C queda
**cerrada con `APROBAR_CON_CAMBIOS` y sus cambios aplicados**, pero el estado final **no está
re-auditado por un tercero**. Es la pasada de confirmación que el hermano documenta en su historial.

### Lo que este detractor declara NO haber auditado

Los 5 renders más allá de `mtime`+`grep` (no abrió PDF ni DOCX), el pandoc de RStudio, V8/multisemilla/
unicidad/coherencia, la instancia canónica, la literalidad de los `exextra` y las seis subsecciones de
la Solution. **Todos ellos los medí yo en esta pasada** (§5-bis, tabla de huecos cerrados) — con la
salvedad de que esa verificación **es propia, no independiente**.

---

## 5-quinquies. FASE 2C DE CIERRE — tercera auditoría independiente · `APROBAR_CON_CAMBIOS`

Encargada por el profesor (2026-08-22) para cerrar la brecha que la pasada 2 dejó abierta: el
detractor anterior auditó el md5 `ac35e809…` y **después** se le aplicaron O1 y O2, de modo que su
veredicto caducó sobre el archivo que existía. Auditor **independiente**, sin `name:`, semillas
`s·8191+37`, md5 comprobado al principio y al final.

### Lo que da este ciclo y la autoverificación NO podía dar

**Ningún defecto de corrección, y no por muestreo: por ENUMERACIÓN EXACTA** del espacio de
parámetros — **412 combinaciones** de `tasa × H × excede × R` — con un predicado reimplementado
desde el **texto** de cada opción, sin usar `umbral()`, `sentido`, `rel` ni `concluye`. 0
conclusiones sólidas en P4, 0 en P5, 0 variantes de clave insólidas. Las 6 claves recomputadas por
él: 0 discrepancias. Marca↔verdad sobre el **XML de Moodle** (el canal de entrega, no el HTML):
0 fallos en 40 versiones, 240 comprobaciones a nivel de afirmación en P6.

**Y el control que faltaba: O1 era PORTANTE, no higiene.** El coordinador había verificado que O1
y O2 eran *neutrales en medición* — lo cual prueba que no rompen nada, **no que sirvan**. Medido
con la misma opción, las mismas semillas y el predicado como única variable:

| Predicado | Aborta el render | Segundas claves emitidas |
|---|---:|---:|
| **post-O1** (vigente) | 101/200 | 0 |
| **pre-O1** (`sin_juicio` ⇒ `FALSE`) | 0/200 | **47/200** |

Antes de O1 este artefacto habría publicado una segunda clave en el **23,5 % de las versiones**
(≈46 % de la rama «cabe») **sin que ningún `stopifnot` dijera nada**.

**Gate Hermes ejecutado por un tercero por primera vez en esta v2**: recortó `pagina_014.jpg` y
verificó contra el **impreso** el enunciado, la pregunta («la decisión **de la** suspensión») y las
**cuatro opciones** de P5, coma idiosincrásica de C incluida. H-1 y H-2 confirmados.

### Las cinco objeciones y su destino

| # | Sev. | Qué | Estado |
|---|---|---|---|
| **O2** | MEDIA | **El fichero no era commiteable.** El corrector salía `exit 1` (9 hits) y el hook `pre-commit` decide por `grep "ERRORES"` ⇒ **commit RECHAZADO**; el remedio previsible (`--no-verify`) está PROHIBIDO por la regla #7 | **APLICADA** |
| **O1** | MEDIA | El predicado ancla la **conclusión** al texto pero deja el **umbral** sin anclar — la asimetría que en el hermano dejó ciego a `ver_op()` dos veces | **APLICADA** |
| **O3** | MEDIA | El `.Rmd` afirmaba de sí mismo que el pool «ENCUADRA a la clave». **Falso**: en la rama «supera» la clave no es la más larga **0 de 194 veces** | **APLICADA** (comentario) |
| **O5** | BAJA | «y **el último** renuncia…» en la `Solution` — la palabra que un lector proyecta sobre «la última opción», y `exshuffle` la desmiente | **APLICADA** |
| **O4** | BAJA | `CON-04`/`CON-07` gemelas numéricas: **+2,2 pp**, muy por debajo del corte. Pedía comprobar si la batería tiene una regla **relacional** | **COMPROBADO: SÍ la tiene** — `REL: par con los mismos numerales`, `REL: par con el mismo molde`, `REL: par de longitud gemela` (líneas 301-303). Nada que añadir (§P7-C) |

**O2 en detalle.** Los 9 hits eran el valor de campo `defecto = "conclusion"` —termina en `-sion`, y
el corrector no distingue identificador de prosa— más un comentario con `precondicion`. Fix:
renombrar el **valor** a `"inferencia"` en el `.Rmd` **y en `auditoria_propia_cloze.R`, en el mismo
commit**, porque estaba cableado ahí. `--fix` estaba descartado: habría puesto una tilde dentro de
un identificador. **`Cuantos más` NO se toca**: es la correlativa relativa de la RAE, y el exit 2
la clasifica correctamente como juicio humano.

> **El mismo defecto afectaba a la v1 aprobada** (6 hits). Se saneó igual, y se verificó que **el
> texto visible no cambia**: render con semillas fijas, **160 → 160 líneas, 0 diferencias**. El
> campo `defecto` sólo vive en un `stopifnot` y nunca se emite. La aprobación de la v1 sigue en pie.

### O3 — el residuo que esta objeción deja MEDIDO y declarado

En la rama «supera», «descartar la más larga» rinde **33,3 %** frente al 25 % de azar (**+8,3 pp**)
y su **margen mediano es 16,4 %** — por encima del corte del 15 %, o sea **perceptible**, y §P7-B
**no lo exime**. Lo que la pasada 2 midió fue el margen de la regla **contraria** («la más corta»:
2,4 %/4,5 %, inexplotable) y no el de ésta.

**No se persigue**, y la razón no es que sea inofensivo: §P7-D está en **2 de 3** pasadas y esa rama
**ya está publicada en `BLOQUEA`** (§5-ter), así que es una segunda regla dentro de un rojo
existente, no un canal nuevo sin declarar. Si el profesor gasta la pasada 3, el vector costeado es
**alargar** una de `CON-04/05/07/08` sin tocar la clave.

### Trampa de método que el propio auditor reportó

Su primera pasada dio **16/40 fallos** en P4. Era **su parser**: extraía la `tasa` **oficial** en
vez de la de práctica, porque la frase «más de N minutos caminar un kilómetro» aparece en los dos
escenarios. *Una sonda que mide mal no mide el artefacto: mide su propia ceguera.*

### Lo que este detractor declara NO haber auditado

La batería §P7 del ejercicio (**no la re-ejecutó**: las cifras `+2,9 / −1,0 / +10,6 pp` las tomó del
HANDOFF sin comprobar), V8, multisemilla, unicidad y coherencia matemática, la **inspección visual
de PDF y DOCX**, la literalidad de los `exextra`, pandoc de RStudio, los valores canónicos de
P1-P4/P6, y el juicio pedagógico v1 vs v2.

> **Aplicar O1-O5 caduca este veredicto** (regla #9). md5 auditado `67b6500b…`. El estado posterior
> lo verificó el coordinador —corrección 0/100, canónica 18/600 sin divergencias, 300/300 únicas,
> 5/5 renders, gate de ortografía `PASARIA` en ambos ficheros— y **eso es autoverificación, no
> independencia**. Se declara como tal.

---

## 5-sexies. Paso 6b — auditoría visual, CERRADA

Nunca se había completado en ningún ciclo. Se renderizaron **24 versiones de cada ejercicio** (v1 y
v2) capturadas a **360 px y 1024 px**; **24/24 renderizan sin fallo ni truncamiento en ambos**. La
inspección la hizo el **coordinador** (declarado: **no independiente**) sobre las capturas.

**El riesgo principal era la maquetación de los dos escenarios de la v2, y está resuelto** con tres
capas redundantes: dos líneas horizontales separadoras, encabezados en negrita («Antes de juzgar
ese caso, practique el procedimiento con esta situación distinta» / «Vuelva ahora al caso del
afiliado») y **cada parte repite a qué caso pertenece** («En *ese caso de práctica*», «Siempre en
*el caso de práctica*»). La `Solution` mantiene la marca: «Partes 2 y 3 — el cálculo, **en el caso
de práctica**».

Sin fugas de markup, math renderizado correctamente, **sin desbordes a 360 px**, y la `Solution`
agrupa por **código de error** (`NUM-TAS-05/06/07`), nunca por letra — regla #19 verificada en el
render, no sólo por `grep`.

**Falso positivo resuelto:** las «**1.080 horas**» que llamaron la atención son `NUM-TAS-07 —
Minutos contados como horas`, distractor **deliberado** con su causa raíz declarada en la propia
Solution (`90 km × 12 min/km = 1.080` **minutos**, leídos como horas). Aparece en 3 de 24 versiones.
Reconocer que 1.080 horas son 45 días caminando **es** parte de la competencia evaluada.

---

## 6. Qué falta

1. **Paso 11 — APROBADO** por el profesor el 2026-08-22, que además **eligió esta v2 para el aula**
   (la v1 queda aprobada y archivada como variante de andamiaje acoplado).
2. **Evidencia de aula (Nivel 3)** → después, `/promover-ejercicio`. Es el gate que falta para
   salir de `01-En-PreDesarrollo/`; la aprobación del profesor NO lo sustituye.
3. **Pasada 3 de §P7-D disponible.** Candidatos por orden: el residuo de O3 —alargar una de
   `CON-04/05/07/08` para encuadrar la clave por el lado «supera»— y el canal preexistente del
   69,3 %, que el detractor de la pasada 2 y el coordinador coincidieron en **no perseguir**.
4. `../cloze/` (v1, 11/11) y `../` (SCHOICE, 10/11 esperando su propia aprobación) **intactos**.
   El SCHOICE hermano sigue sin aprobar: es decisión del profesor y nadie más la sella.

---

## 7. Trampas de este ciclo

- **La guarda estrecha no basta.** Cubrir los 5 numerales del caso oficial dejaba LEAK-B en 4,3 %
  porque las opciones de P5 imprimen **derivados** (`tasa·H`, `vel·24`, `cota/2`).
- **Medir la referencia antes de perseguir el canal.** El 76 % parecía un defecto del CLOZE; medirlo
  sobre el SCHOICE hermano (77,8 %) mostró que es del ítem. Sin esa medición habría rediseñado
  contra un objetivo inalcanzable.
- **Un `sed` con `\;` es destructivo**: borró backslashes de la ecuación de P5 y rompió los 5
  renders. El smoke **no lo detectó** porque sólo evalúa `data_generation`, no los chunks de emisión.

### Trampas de la pasada 2

- **La rama que gobierna P4 es `pr$excede`, no `excede`.** Medir con la variable del caso oficial da
  9 conjuntos y hace desaparecer el defecto. Cometido y corregido en la primera medición.
- **Escribir el predicado DESPUÉS de ampliar el pool habría sido inútil**: el candidato «promediar en
  vez de acotar» parecía un distractor impecable y era **el argumento de la clave con otra
  aritmética**. Lo descartó el predicado, no el criterio de quien lo redactaba.
- **Acortar tres textos movió el canal de una rama a la otra.** Hay que medir el vector completo —las
  dos ramas, las dos direcciones de H1— después de cada fix, no sólo la dimensión que se tocó.
- **El pool creció y con él la ortografía «rota»**: `defecto = "conclusion"` pasa de 5 a 6 hits. Es el
  falso positivo ya declarado (valor de campo comparado en código). **Sigue sin auto-corregirse**:
  `--fix` lo desincronizaría de `auditoria_propia_cloze.R:137`.
