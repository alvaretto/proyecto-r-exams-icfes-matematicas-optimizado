# HANDOFF — `tasa_caminata_velocidad_maxima_..._n4_schoice_v1`

**Fecha:** 2026-08-22 · **Estado:** 9/11 pasos · **Ubicación:** `01-En-PreDesarrollo/`
**Origen:** `MAT-2026-1-047` — ERA-2026 Sesión 2, **pregunta impresa 47**; ítem verbatim de la
pregunta 133 del cuadernillo Matemáticas 2026-1.
**Clasificación:** Argumentación · Numérico-variacional · **Nivel 4** · D4.2 · clave oficial **B**.

> **Núcleo matemático.** «Más de `tasa` minutos por kilómetro» es una cota **superior** de
> velocidad: `v < 60/tasa` km/h, y en `H` horas se recorren menos de `(60/tasa)·H` km. El ítem
> pide decidir cuál argumento **sustenta** la decisión tomada sobre un reporte de `R` km.

---

## 1. Gate Hermes (regla #24) — ejecutado sobre el escaneo, no sobre la ficha

| | |
|---|---|
| **H-4** | El número **impreso** es 47, leído en `Originales/pagina_014.jpg`. No derivado del mapeo página↔pregunta |
| **H-1** | La pregunta 47 **no contiene figura**: es texto puro. La tabla de esa página pertenece a la 48 → `flujo_b.requerido = false`, justificado por lo VISTO |
| **H-2** | Screening de trampa deliberada **negativo** (ninguno de los siete patrones) |
| **H-3** | Instancia canónica **verbatim** contra el escaneo, incluida la coma idiosincrásica de la opción C («un recorrido así, corresponde entonces»). 0 divergencias en 14-18 instancias |

**Hallazgo que corrige al insumo, no al ejercicio:** la ficha de alineación transcribe «la decisión
**de suspensión**» y aplana el enunciado a un párrafo. El escaneo imprime «la decisión **de la
suspensión**» en dos párrafos, y el `.Rmd` sigue al escaneo. Conviene avisar a quien mantiene la
ficha. Es la lección H-1 al revés: la descripción textual era la equivocada.

---

## 2. Corrección — verificada, con control positivo

| Comprobación | Resultado |
|---|---|
| Versiones con **más de una** opción sólida | **0 / 400** |
| Versiones con la **clave no sólida** | **0 / 400** |
| Violaciones de la precondición de NUM-TAS-08 | **0 / 400** |
| **Control positivo** (versión pre-fix) | **5-8 / 400** segundas claves detectadas |

Medido con un **predicado de solidez** —premisa verdadera + conclusión que se sigue—, no con el
predicado de identidad que tenía el archivo. Series de semillas independientes (`s*7331+19`,
`s*2003+41`) distintas de las del generador. El control positivo es lo que da valor al cero: una
sonda que no dispara sobre el defecto conocido no prueba nada.

> **La lección más cara del ciclo.** El defecto de corrección lo introdujo una pasada que
> perseguía diagnosticidad: `NUM-TAS-08` resultó ser un **argumento sólido** en el 2,8 % de las
> versiones, porque sus dos premisas se deducen del mismo criterio que la clave y sólo cambia el
> horizonte. No se detectó porque `es_malo` comprobaba **identidad con la clave**
> (`cota_km == cota`), no **solidez**: un argumento puede ser sólido sin ser el mismo argumento.
> Corregido en dos capas — precondición por regiones **y** predicado reescrito.

---

## 3. Arsenal (versión vigente)

| | |
|---|---|
| Renderizado | **6/6** — html · pdf · pdf+solución · docx · nops · moodle |
| Coherencia matemática · Multisemilla (N=100) | APROBADO (0 errores) · APROBADO (0 fallos) |
| Diversidad sustantiva · Diagnosticidad | PASS · PASS |
| Ortografía (#7) · Glifos (#25) | exit 0 · sin glifos que rompan pdflatex |
| #18 · #19 · #20 | N/A (0 imágenes) · 0 hits · guard `\newcounter{none}` presente |
| Unicidad de producto (regla #3) | **296/300** (umbral 250) |

---

## 4. Los siete canales cerrados — y por qué eran uno solo

| # | Canal | Antes | Después |
|---|---|---:|---:|
| 1 | menor primer número (`magnitud`) | +14,9 pp | **+0,4 pp** |
| 2 | recuento de magnitudes citadas | +18,5 pp | **+6,8 pp** |
| 3 | «única que enuncia dos cotas» (`formato`) | +36,6 pp | **+3,2 pp** |
| 4 | grupo de molde de apertura | +20,2 pp | **+3,6 pp** |
| 5 | «la más corta» | +9,7 pp | **+2,5 pp** |
| 6 | centroide léxico | +25 a +38 pp | **exento §P7-B** (margen 3,4-5,3 %; ≥96 % de versiones bajo el 15 %) |
| 7 | **par que cita los mismos numerales** | +20,6 pp | **+14,7 pp** ← RESIDUO ACEPTADO |

**Diagnóstico unificado:** no son siete defectos independientes. Son siete lecturas del mismo
hecho estructural — **la clave pertenece siempre al subgrupo de opciones que calculan bien el
techo**, y ese subgrupo es de 2-3 de 4. Cada cierre desplazaba el canal a otra cara del mismo
hecho, que es exactamente el ciclo que §P7-D describe y por el que existe su límite de pasadas.

**Tres veces la misma lección de §P7**, y las tres en este ejercicio: faltaba «menor primer
número» en `magnitud`, faltaba «dos cotas» en `formato`, y faltaba **la familia relacional
entera** (las 25 reglas miraban cada opción por separado; ninguna miraba relaciones entre pares).
*Una batería incompleta no mide «sin señal», mide **sin sonda**.*

---

## 5. RESIDUO — **OVERRIDE** registrado (regla #24 H-5)

> **Se cierra con el par gemelo numérico como residuo, mediante OVERRIDE DELIBERADO de un
> criterio calibrado.** No se declara «aceptable»: el canal **es explotable** y §P7-B **no lo
> exime** —exime lo imperceptible, no lo contable, y aquí la diferencia es «dos opciones citan los
> mismos numerales», discreta y contable—. Se cierra igualmente por las razones de abajo.
> Relajar un gate **no es autónomo**: por eso queda escrito, con su cifra, y no absorbido.

**El canal.** La clave cita `{vel, cota, R}`. `NUM-TAS-05` cita los mismos tres numerales —porque
su error consiste justamente en invertir el sentido de esa misma cota—, de modo que «hay dos
opciones con los mismos números, elige una de las dos» acierta el **41,5 %** frente al 25 % de
azar. La batería propia lo mide y **sigue reportando `BLOQUEA` (+10,2 pp)**: el residuo está
publicado, no silenciado.

> **Condición innegociable del cierre:** el día que alguien haga que la batería reporte `PASS`
> para poder cerrar, este ejercicio pasa a ser el precedente de cómo se apaga un gate. La batería
> se deja en rojo a propósito.

### Fundamento 1 (decisivo) — la fuente oficial tiene el canal PEOR

Medido sobre la **instancia canónica**, que reproduce el ítem oficial verbatim:

| Regla de eliminación | Ítem **OFICIAL** | Versiones generadas (n=382) |
|---|---|---:|
| «elige la única que calcula una cota» | **la resuelve** — enumeración exacta del único ítem oficial (`tasa=6, H=24, R=300`, opciones fijas): la clave B es la única que enuncia una cota calculada | **31,1 %** (+6,1 pp) |
| «elige del par con los mismos numerales» | falla (el par gemelo es A-D, y no contiene la clave) | 41,5 % (+16,5 pp) |

No es una tasa muestral con `n` pequeño: la instancia canónica es **determinista**, así que es
**enumeración exacta**, no estimación — la tercera excepción declarada de la regla #23.

**El máximo explotable pasó de resolver el ítem a 41,5 %.** Rechazar el generado por un canal del
41,5 % obligaría a rechazar el ítem del ICFES por uno que lo resuelve entero. Esa asimetría es
insostenible y por sí sola justifica no seguir (§P7-A: *un ítem que no filtra más que el examen
real no se declara defectuoso*; aquí filtra **menos** que su fuente).

### Fundamento 2 (empírico) — rendimiento decreciente con colateral creciente

La 7.ª pasada atacó el canal **por la vía que no homogeneiza** (hacer que el par dejara de ser
único). Resultado medido: lo movió de **+20,6 a +14,7 pp** —un 29 % de reducción— y a cambio
introdujo **una anáfora sin antecedente** en el 16,2 % de las versiones y **un distractor
eliminable sin hacer una cuenta** en el 100 % de las suyas (`NUM-TAS-11`, ver §5b). Es rendimiento
decreciente con colateral creciente, sobre un canal que la fuente oficial tiene peor. Se cierra
por **§P7-D** con residuo declarado: ésta fue la **octava** pasada sobre un presupuesto de tres.

> **Nota de método.** Una versión anterior de este documento fundamentaba el cierre en que cerrar
> el canal «exigiría homogeneizar las cuatro opciones y perder la función diagnóstica de las
> cualitativas». Ese fundamento **se retiró**: es más fuerte de lo que se puede demostrar. El
> canal no exige homogeneizar las cuatro —bastaría un tercer miembro permanente del grupo de
> numerales para bajar el score de 1/2 a 1/3— y la alternativa de hacerlo con `NUM-TAS-09`, sin
> tocar ninguna opción cualitativa, **no se costeó**. Se sustituye por los dos fundamentos de
> arriba, que son verificables. *Un argumento de diseño no costeado no sostiene un override.*

### 5b. `NUM-TAS-11` queda marcado como RELLENO

Su `causa_raiz` («una cota superior marca un techo, no un objetivo») es una confusión real, pero
**no produce su conclusión**: un estudiante que la sufre y ve `R < cota` concluye «caminó menos de
lo que podía», no «el registro está incompleto y debe descartarse». Ese salto lo produce la
necesidad de que su veredicto caiga en el grupo minoritario. Y es **eliminable sin matemática en
el 100 % de sus apariciones** (84/84): es el singleton de veredicto, se descarta leyendo sólo las
conclusiones.

**Atenuante estructural:** el slot minoritario lo tiene que ocupar alguien, y el reparto 3-1 es el
del cuadernillo oficial; si no fuera `NUM-TAS-11` sería `03` o `05`, igual de gratis de eliminar.
Lo específico suyo es que **además no aporta razonamiento propio**. No se retira en esta pasada
—hacerlo sería la novena sobre un presupuesto de tres—, pero queda registrado como el primer
candidato a sustituir si el ítem se revisa.

### Condición de revisión

Si en aula se observa que los estudiantes eliminan por convergencia numérica en vez de por el
sentido de la desigualdad, las vías abiertas son: (1) un tercer miembro permanente del grupo de
numerales —candidato costeable: que `NUM-TAS-09` reenuncie la cota verdadera antes de partirla—,
o (2) sustituir `NUM-TAS-11` por un distractor con razonamiento propio.

## 6. Historial de pasadas

| Pasada | Qué encontró | Resultado |
|---|---|---|
| Orquestador | 9/11 pasos, §P7 en +3,8 pp (batería propia) | `parcial`, 59/60 turnos |
| Detractor 1 | 8 objeciones; la clave **sobre-afirmaba** «sí pueden hacerse a pie» (falacia necesario⇒suficiente) en el 45,8 % | corregido |
| Detractor 2 | canal de magnitud «menor primer número» +14,9 pp, **ausente en el ítem oficial** | corregido |
| Detractor 3 | **SEGUNDA CLAVE** (2,8 %) introducida por el fix anterior + canal de formato +29 pp | corregido |
| Detractor 4 | par gemelo numérico +21,4 pp; midió que el arreglo obvio empeora | residuo aceptado |
| Detractor 5 | cierre: **0/600 sin defecto de corrección**; `NUM-TAS-11` es relleno; `ver_op()` ciego otra vez (14 %); retira el fundamento de constructo | **APROBAR_CON_CAMBIOS** |

---

## 7. Qué falta

1. **FASE 2C — CERRADA** con `APROBAR_CON_CAMBIOS`. Los cambios exigidos se aplicaron: regex de
   `ver_op()` corregido, `stopifnot` de coherencia texto<->veredicto añadido al `.Rmd` (la ceguera
   futura ahora **aborta el render** en vez de producir cifras mal etiquetadas), anáfora de
   `NUM-TAS-10` cerrada —parche que el propio detractor verificó neutro sobre copia— y este
   documento reescrito. Re-verificado después: **0/600** corrección, 6/6 formatos, arsenal en verde.
2. **Paso 11 — aprobación del profesor.** Es juicio pedagógico y no lo sella nadie más.
3. **Nada está commiteado.** `git add` pendiente de decisión.
4. Reportar a quien mantiene la ficha `MAT-2026-1-047` la divergencia de transcripción (§1).
5. Tras el aula (Nivel 3) → `/promover-ejercicio`.

## 8. Trampas encontradas, para quien siga

- **`verificar_canonica.R` no verifica fidelidad al cuadernillo**: compara contra un texto
  codificado en el propio script. Su «0 divergencias» prueba ausencia de **deriva**, no fidelidad.
  Lo que sostiene la fidelidad es la lectura visual de `pagina_014.jpg`.
- **El scratchpad se comparte con los subagentes.** Un script propio fue sobrescrito por el de un
  detractor y su salida casi se reporta como resultado propio. Un resultado que no puedes atribuir
  a tu propio script no es un resultado.
- **`which.max` sobre grupos empatados infla la medición**: desempata por el orden interno, que
  `exshuffle: TRUE` destruye antes de que el estudiante lo vea. La convención correcta es
  **abstenerse en empates** (+6,2 pp real frente a +12,6 pp artefactual).
- **Margen antes que frecuencia (§P7-B)**: el centroide léxico acierta hasta el 99 % y es
  **inexplotable** (margen 3,4 %). Perseguirlo habría costado pasadas enteras.


## 9. Cobertura de la auditoría de cierre — lo que NO se auditó

El detractor de la pasada de cierre **declaró sus límites**, y conviene no leer su APROBAR como
cobertura total. NO verificó: las seis familias de canales ni la búsqueda de un octavo (excluido
de su encargo; las cifras de molde / más corta / recuento las tomó del reporte **sin comprobar**),
los 6 renders, el arsenal compartido, el **gate visual Hermes contra el escaneo** (no abrió el
JPG), el verbatim canónico, ni la sección `Solution` más allá de que agrupa por código.

Su cobertura fue: **corrección semántica de las opciones**, **legitimidad de los dos errores
añadidos**, **arbitraje del cierre** y —fuera de encargo— **integridad del instrumento de medida**.

Lo que él no cubrió sí está medido en este documento (§§1-4), pero por el coordinador, no por un
tercero independiente. Se declara para que la próxima revisión sepa dónde mirar.
