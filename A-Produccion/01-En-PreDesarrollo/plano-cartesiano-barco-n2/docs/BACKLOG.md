# Backlog — Coordenadas de vértices en el plano cartesiano (barco)

> Pendientes priorizados. `P0` bloquea la promoción a `02-En-Desarrollo/`; `P1` es deuda que hay
> que resolver antes de escalar el patrón; `P2` es diferible.
> Ver [`ROADMAP.md`](ROADMAP.md) para los gates de promoción.

---

## P0 — Bloqueante para promoción

### P0.1 — `GEO-COORD-03` es eliminable por su forma, sin razonar geometría — **ABIERTO**

**Origen:** auditoría adversarial (2026-07-28). **Verificado de forma independiente por enumeración
exhaustiva sobre las 222 versiones.**

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

**Dirección de fix (requiere diseño, no es un parche listo):** sustituirlo por un error que
**conserve la estructura 2×2**. Por ejemplo, un desplazamiento de una unidad al contar la
cuadrícula —`(x_min+1, y_min); (x_min+1, y_max); (x_max+1, y_min); (x_max+1, y_max)`— que es un
error de lectura frecuente y estructuralmente indistinguible de las demás opciones. Antes de
adoptarlo hay que:

1. Verificar que no se sale de la grilla (`x_max + 1 ≤ 10`), lo que obliga a acotar `x_min`.
2. Verificar que no colisiona con ninguna otra opción en las 222 versiones.
3. Actualizar `errores_info$diagonal` (código, nombre y diagnóstico) en consecuencia.
4. Re-ejecutar la enumeración exhaustiva, `validar_diversidad_sustantiva.R` y `verificar_render.R`.

**Criterio de cierre:** los cuatro textos de opción comparten la estructura 2×2 en las 222
versiones (misma comprobación de la tabla de arriba, con `GEO-COORD-03` pasando a 222/222).

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

---

## P1 — Deuda de desarrollo

### P1.1 — El casco no se lee como barco en el 27 % de las versiones — **ABIERTO, requiere decisión**

**Medición (2026-07-28).** La forma del casco depende de la relación de aspecto
`ratio = ancho_barco / alto_barco`. Distribución sobre las 222 combinaciones válidas:

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
| **A** | Restringir el sorteo a `ratio ≥ 2.5` (`alto_barco = 2` sólo con `ancho_barco ≥ 5`) | Espacio de versiones 222 → **162 preguntas distintas** (−27 %). Sigue muy por encima de lo que exigen los validadores: `validar_diversidad` cuenta renders únicos (162 × 8 protagonistas × 4 reflexiones × 24 órdenes ≈ 124 000) |
| **B** | Rediseñar el perfil del casco para que funcione a cualquier aspecto (p. ej. proa asimétrica con popa roma explícita, en vez de un perfil casi simétrico) | Trabajo de diseño gráfico + re-verificación completa. Conserva las 222 versiones |
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

**Criterio de cierre:** tras aplicar la opción elegida, re-ejecutar la enumeración exhaustiva
(0 desajustes de *bounding box*), `validar_diversidad_sustantiva.R --n 40` (PASS) y
`verificar_render.R` (5/5), e inspeccionar visualmente los dos casos extremos de forma que queden
en el espacio resultante.

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
protagonista** (8 nombres, líneas 135-139) sobre un enunciado fijo.

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
(líneas 95-132) guarda `codigo` / `nombre` / `texto` / `diagnostico`, y los distractores se
construyen con `paste0` (líneas 54-76).

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

### P2.3 — Artefactos derivados sin regla de exclusión en git
`plano_barco.png`, `salida/`, `verif_render/` y el `.html` suelto de la raíz son derivados que se
regeneran en cada render. Hoy no están cubiertos por ninguna regla de exclusión y aparecen como
untracked. No se tocó el `.gitignore` del repo raíz en esta sesión porque ya venía modificado en el
árbol de trabajo por otro trabajo ajeno a este subproyecto. **Acción sugerida:** añadir un
`.gitignore` local al subproyecto cuando se resuelva el estado del `.gitignore` raíz.

### P2.5 — La Solution tiene 4 de las 6 subsecciones canónicas
**Origen:** auditoría del detractor (2026-07-28).

La regla #1 (`ejercicios-metacognitivos.md`, «Sección Solution Obligatoria») lista seis
subsecciones. El chunk `solucion` incluye cuatro: *Respuesta correcta* + *Análisis de cada opción*
(cubre «Análisis del error»), *Procedimiento correcto*, *Reflexión metacognitiva* y *Estrategia
para evitar el error*.

Faltan:

- **Propiedades del concepto** — p. ej.: en un par ordenado la primera coordenada es siempre la
  horizontal; un rectángulo alineado a los ejes queda determinado por los extremos de ambos rangos
  y tiene exactamente 4 vértices.
- **Caso específico (transferencia)** — p. ej.: «si el barco se desplazara 2 unidades a la derecha,
  ¿cuáles serían los nuevos vértices?».

**Impacto:** bajo. El ejercicio ya es metacognitivo (diagnóstico por distractor con código de
error, reflexión y estrategia). Las dos subsecciones añadirían profundidad, no corrigen un defecto.

**Acción sugerida:** insertarlas entre *Procedimiento correcto* y *Reflexión metacognitiva*.
Conviene resolverlo **junto con P0.1**, porque el rediseño del tercer distractor obliga a tocar el
mismo bloque de la Solution.

### P2.6 — `sample()` interno redundante con `exshuffle: TRUE`
**Origen:** auditoría del detractor (2026-07-28). Severidad BAJA.

Las líneas 89-92 mezclan las opciones con `perm <- sample(4L)` y además `exshuffle: TRUE` vuelve a
mezclarlas. Ambos mecanismos son coherentes entre sí (R/exams reordena `questionlist`,
`solutionlist` y `exsolution` con la misma permutación), así que **no hay bug**: sólo lógica
redundante. Se puede simplificar dejando que `exshuffle` haga todo el trabajo, o mantenerlo como
control explícito. No urge.

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

**Versión:** 1.0 · **Fecha:** 2026-07-28
