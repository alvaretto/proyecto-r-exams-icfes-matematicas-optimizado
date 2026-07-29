# HANDOFF — Subproyecto `plano-cartesiano-barco-n2`

> **Documento de reanudación.** Si retomas este subproyecto después de una pausa, lee este archivo
> y `ejercicio_state.json` **antes** de explorar el repositorio. Todo lo necesario para continuar
> sin re-descubrir contexto está aquí.

| Campo | Valor |
|---|---|
| **Ruta (SP)** | `A-Produccion/01-En-PreDesarrollo/plano-cartesiano-barco-n2` |
| **Repo raíz (RR)** | `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams` |
| **Ejercicio** | `coordenadas_vertices_plano_cartesiano_metacognitivo_interpretacion_n2_schoice_v1.Rmd` (483 líneas, 5 chunks) |
| **Tipo** | SCHOICE metacognitivo, **opciones de texto**, una sola figura compartida |
| **Origen ICFES** | `MAT-2026-1-022` (cuadernillo 2026-1, pregunta 116, clave C) |
| **Rama** | `main` |
| **Commit fundacional** | `fc5a8c1a` (2026-07-01) |
| **Última sesión** | 2026-07-28 |
| **Frase de reanudación** | «Continúa con el proyecto plano-cartesiano-barco» |

---

## 1. Objetivo general

Producir y mantener un **ejercicio ICFES SCHOICE metacognitivo de Nivel 2** (competencia
*Interpretación y representación*, componente *Geométrico-métrico*) sobre lectura de las
coordenadas de los vértices que encierran un barco en un plano cartesiano, derivado del ítem real
`MAT-2026-1-022`.

El ítem **no evalúa cálculo aritmético**: evalúa si el estudiante distingue el eje horizontal del
vertical y lee el **rango completo** del objeto representado (no sólo su centro). Los tres
distractores son errores conceptuales documentados (`GEO-COORD-01/02/04`), no ruido numérico.

**No confundir con el subproyecto hermano** `desplazamiento-avion-aeropuerto`: aquél tiene 4
opciones **gráficas** y usa `exshuffle: FALSE`; éste tiene opciones de **texto** y usa
`exshuffle: TRUE`. Los patrones no son intercambiables.

---

## 2. Objetivos específicos

Ver la tabla completa con veredictos y evidencia en [`docs/ROADMAP.md`](docs/ROADMAP.md) §2.
Resumen: **OE1-OE7 cumplidos y verificados**, OE8 (modularización) parcial por bloqueo de
herramienta, OE9 (auditoría) con un hallazgo abierto, OE10-OE11 (promoción y aula) pendientes.

Estos OE **no existían declarados** antes del 2026-07-28: el subproyecto no tenía `README.md`,
`ROADMAP.md`, `.claude/` local ni memoria. Se declararon en esa sesión y se persistieron en
`~/.claude/projects/<slug>/memory/project_objetivos_plano_cartesiano_barco.md`.

---

## 3. Estado real del ejercicio (verificado 2026-07-28)

| Verificación | Comando | Resultado |
|---|---|---|
| Coherencia matemática, Niveles 1-5 + Capas semánticas A-D | `Rscript RR/.claude/scripts/validar_coherencia_matematica.R <rmd>` | **APROBADO, 0 errores** |
| Diversidad sustantiva (regla #22) | `Rscript RR/.claude/scripts/validar_diversidad_sustantiva.R <rmd> --n 40` | **PASS**, 40/40 evaluadas, **38** valores únicos (antes 36, tras ampliar el espacio en P2.7) |
| Render 4 formatos + Moodle | `Rscript verificar_render.R` | **5/5 OK** |
| Regla #18 (`{width=}`) | grep | OK (línea 317 del original, hoy desplazada por el comentario I-2) |
| Regla #19 (letter-independence) | grep sobre la sección Solution | OK, 0 coincidencias |
| Regla #20 (guard `\newcounter{none}`) | grep | Presente |
| Regla #22 §P6 (fuga por nombre de archivo) | `exams2moodle` + grep del XML | OK — único archivo `plano_barco.png` |
| Incidente I (reseed por reloj) | doble grep | No aplica: sin `set.seed`, sin `Sys.time/proc.time/Sys.Date` |
| Invariantes de infraestructura I-1 a I-9 | `Rscript RR/tests/testthat/test_infraestructura_claude.R` | **11 bloques en verde** |

### Espacio de versiones — medido exhaustivamente, no muestreado

Enumeración completa de las combinaciones `(ancho_barco, alto_barco, x_min, y_min)`:

- **374 combinaciones válidas → 374 respuestas correctas distintas** (biyección). Eran 222 hasta
  que P2.7 (2026-07-28) retiró las 4 exclusiones de `y_pool` — una de ellas justificada de forma
  falsa (ver `docs/BACKLOG.md` P2.7) — y amplió el espacio un 68 %.
- **0** casos de `y_pool` vacío → el `stopifnot` del pool no puede dispararse.
- **0** colisiones entre las 4 opciones → el `stopifnot` de unicidad no puede dispararse.
- **0** casos donde el *bounding box* del casco difiera de `[x_min,x_max] × [y_min,y_max]` →
  **la clave es correcta en el 100 % del espacio de versiones**, no sólo en las semillas probadas.

Renders distintos posibles: 374 × 8 protagonistas × 4 reflexiones × 24 órdenes = **287 232**.

---

## 4. Qué se hizo en esta sesión (2026-07-28)

1. **`/goal`**: se detectó que el subproyecto no tenía ningún objetivo declarado. Se declararon
   OE1-OE11 y se persistieron en memoria.
2. **Verificación completa** de los validadores + enumeración exhaustiva del espacio de versiones
   (esto último es nuevo: antes sólo había muestreo por semillas).
3. **Reestructuración**: se crearon `docs/`, `.claude/` y `_archivo/`; se archivó el prototipo del
   Flujo B (`grafico_barco_parametrico.R`, con parámetros hardcoded, superado por `dibujar_barco()`
   dentro del `.Rmd`) y la copia obsoleta del `.Rmd`.
4. **`verificar_render.R`**: nueva herramienta de verificación (5 formatos + chequeo P6), separada
   de `SemilleroUnico_v2.R` (exportación real).
5. **Documentación**: `README.md`, `docs/{SYLLABUS,ROADMAP,BACKLOG,BLUEPRINT}.md`, este `HANDOFF.md`,
   `.claude/CLAUDE.md` (10 particularidades operativas) y `.claude/rules/barco-parametrico.md`.
6. **Cableado de orquestadores** (repo raíz, con el protocolo de la regla #17: snapshot +
   verificación de invariantes):
   - `orquestador-schoice`: pre-flight check **19** + **Incidente M**.
   - `orquestador-cloze`: pre-flight check **23** + **Incidente O**.
   - Ambos obligan a leer el `.claude/` **local** del subproyecto antes de tocar su `.Rmd`.
7. **Un cambio aplicado y revertido**, documentado en [`docs/BACKLOG.md`](docs/BACKLOG.md) P1.1: se
   intentó una proa adaptativa al aspecto del casco; preservaba la invariante y las 222 versiones,
   pero no resolvía el caso peor y **regresaba** los ratios medios (36,5 % de las versiones). Se
   revirtió. El `.Rmd` conserva la geometría original más un comentario que documenta la
   invariante I-2.
8. **P2.5 resuelto**: se añadieron a la Solution las dos subsecciones canónicas que faltaban de la
   regla #1 — *Propiedades del concepto* (líneas 397-415) y *Caso específico* (líneas 416-437). La
   Solution tiene ahora las 6 subsecciones canónicas; verificado en el XML de Moodle renderizado.
   Detalle en [`docs/BACKLOG.md`](docs/BACKLOG.md) P2.5.
9. **P2.7 resuelto, con más alcance del previsto**: se retiraron las **4** exclusiones de `y_pool`
   (no solo 2, como se había previsto), tras medir que ninguna era necesaria — la justificación de
   `y_min ≠ x_min` («evita que `GEO-COORD-01` colapse sobre la correcta») resultó ser **falsa**. El
   espacio de versiones sube de 222 a **374** (+68 %). Re-verificación completa: coherencia
   matemática APROBADO, diversidad sustantiva PASS (38 valores únicos), render 5/5, estructura 2×2
   374/374 en las tres opciones que la requieren. Detalle en [`docs/BACKLOG.md`](docs/BACKLOG.md)
   P2.7.

---

## 5. Hallazgos abiertos

### 5.0 — `GEO-COORD-03` era eliminable por su forma (P0.1) — ✅ **RESUELTO 2026-07-28**

Los vértices de un rectángulo alineado a los ejes tienen siempre estructura **2×2**: dos valores de
x combinados con dos de y. `GEO-COORD-03` (diagonal) la cumplía en **0/222** versiones (el espacio
vigente entonces) — sus cuatro puntos eran siempre `(v,v)`, colineales — así que se podía descartar
por la forma del texto sin mirar la figura.

**Se retiró y se sustituyó por `GEO-COORD-04`** («desplazamiento de una unidad al contar la
cuadrícula»), que conserva la estructura 2×2 en **222/222** versiones (re-confirmado en
**374/374** tras ampliar el espacio en P2.7). La dirección del desplazamiento es adaptativa (`+1`, o
`−1` si el barco toca el borde derecho), lo que evita salirse de la grilla **sin perder ni una
versión**. Detalle y tabla de verificación en [`docs/BACKLOG.md`](docs/BACKLOG.md) P0.1.

### 5.1 — El casco no se lee como barco en el 27 % de las versiones (P1.1) — 🟡 **RESUELTO EN PARTE**

Se aplicó la **opción D**: acotar el radio de las bandas por el ancho (`rb <- min(h, w * 0.25)`),
con el factor calibrado midiendo el solape sobre las 8 combinaciones. El solape en el caso peor baja
de **72,3 % a 37,0 %** y los casos alargados (`5×1`, `6×1`) quedan **idénticos**. Verificado
visualmente: donde antes había una sola mancha negra, ahora se distinguen las dos medialunas y el
puente.

**Residual:** la silueta a `ratio 1.5` sigue siendo una cápsula redondeada más que un barco. Eso es
el mecanismo del contorno, que la opción D no aborda. Si quieres resolverlo también, siguen
disponibles la opción **A** (restringir a `ratio ≥ 2.5`, cuesta 60 de las 222 versiones — medición
sobre el espacio vigente al momento del análisis, no re-medida tras P2.7) y la **B** (rediseñar el
perfil). **No es bloqueante**: la clave es correcta en las 374.

**Ojo con dos cosas si vuelves sobre esto:**

- Dos auditorías reportaron que «el puente se sale del casco». **Es falso** y está demostrado: el
  borde derecho del puente cae siempre en `t = 0.87`, posición invariante de escala, ocupando el
  53,7 % del espacio disponible en las 8 combinaciones. El síntoma que vieron (mancha negra) es
  real; la causa que le atribuyeron, no.
- Ya se probó una vía que **empeoró** el resultado (proa adaptativa al aspecto). Ver el registro en
  BACKLOG P1.1 antes de reintentarla.

### 5.2 — Modularización del `.Rmd` bloqueada (P1.2)

Bloqueada por incompatibilidad entre `include_supplement()` y
`validar_diversidad_sustantiva.R`. Medido en el subproyecto hermano: 40/40 semillas fallidas. No
reintentar sin resolver antes el criterio de desbloqueo (adaptar el validador, que es herramienta
compartida). Detalle en [`docs/BACKLOG.md`](docs/BACKLOG.md) P1.2.

### 5.2 — `ejercicio_state.json` tiene dos pasos desactualizados por el cambio de distractor

> **Nota (post P2.5/P2.7):** el `.Rmd` recibió dos cambios adicionales en la misma sesión —
> la Solution ganó dos subsecciones (P2.5) y `y_pool` perdió sus 4 exclusiones (P2.7) — así que la
> distancia entre lo que `aprobacion_usuario` describe y el ejercicio actual **aumentó**, no se
> resolvió. La recomendación de re-confirmar la aprobación (final de esta sección) aplica con más
> razón todavía.

El estado sigue marcando los 11 pasos como completados, pero dos de ellos son **anteriores** al
cambio del 2026-07-28 y ya no describen el ejercicio actual:

| Paso | Marcado | Por qué quedó desactualizado |
|---|---|---|
| `detractor_fase2c` | `APROBAR`, 2026-07-01 | La auditoría del 2026-07-28 sí encontró un bloqueante (`GEO-COORD-03`). El veredicto de julio 1 se emitió sobre una versión con un distractor que ya no existe |
| `aprobacion_usuario` | completado, 2026-07-01 | La aprobación se dio sobre un ejercicio cuyo tercer distractor era el de la diagonal. El ítem que ve hoy el estudiante tiene `GEO-COORD-04` en su lugar |

**No se modificó el JSON**: cambiar un estado de aprobación humana no es decisión de un agente.
Antes de promover a `02-En-Desarrollo/` conviene **re-confirmar la aprobación** sobre la versión
actual, o registrar explícitamente que el cambio de distractor no la invalida.

### 5.3 — Decisiones cerradas (no reabrir sin motivo nuevo)

- **Regla #11 (contextos narrativos)**: sólo varía el protagonista, y es correcto — el enunciado y
  las opciones se conservan *verbatim* del ítem ICFES oficial. Ver BACKLOG P1.3.
- **Pool sin `precondicion`/`calcula()`**: es un patrón legítimo aquí, porque los distractores son
  cadenas de coordenadas, no valores calculados. Ver BACKLOG P1.4.

### 5.4 — P2.5 (Solution con 6 subsecciones) — ✅ **RESUELTO 2026-07-28**

Ver punto 8 de §4 y [`docs/BACKLOG.md`](docs/BACKLOG.md) P2.5. No queda ninguna acción pendiente.

### 5.5 — P2.7 (exclusiones de `y_pool`) — ✅ **RESUELTO 2026-07-28**, alcance mayor al previsto

Ver punto 9 de §4 y [`docs/BACKLOG.md`](docs/BACKLOG.md) P2.7. No queda ninguna acción pendiente;
no reintroducir exclusiones sin volver a medir (invariante I-3 de `docs/BLUEPRINT.md`).

---

## 6. Riesgos

| Riesgo | Mitigación |
|---|---|
| Un agente "limpia" el código duplicado y extrae `dibujar_barco()` a un archivo externo | `.claude/CLAUDE.md` particularidad 1 + invariante I-1 + pre-flight 19 del orquestador |
| Un agente "suaviza" `prof()` para que el barco se vea mejor → la clave pasa a ser falsa sin error de sintaxis | `.claude/rules/barco-parametrico.md` (contrato C1-C3) + comentario de la invariante I-2 en el propio `.Rmd` |
| Se copia el `exshuffle: FALSE` del subproyecto hermano por analogía | `.claude/CLAUDE.md` particularidad 4 |
| Se borran `Semillero*.R` o `pcielo*.tex` por parecer ruido | `.claude/CLAUDE.md` particularidad 6 + README §Cómo exportar |
| Las citas de línea de la documentación se desplazan al editar el `.Rmd` | `docs/BLUEPRINT.md` §6 lista anclas de control para re-verificar |

---

## 7. Cómo retomar

### Comandos de arranque

```bash
cd "$RR/A-Produccion/01-En-PreDesarrollo/plano-cartesiano-barco-n2"

# 1. Estado del workflow (11 pasos)
../../../.claude/scripts/workflow-state.sh status .

# 2. Qué cambió desde la última sesión
git log --oneline -5 -- .
git status --short -- .

# 3. Salud del ejercicio (sale con status 1 si algo falla)
Rscript verificar_render.R

# 4. Corrección y diversidad
Rscript ../../../.claude/scripts/validar_coherencia_matematica.R \
  coordenadas_vertices_plano_cartesiano_metacognitivo_interpretacion_n2_schoice_v1.Rmd
Rscript ../../../.claude/scripts/validar_diversidad_sustantiva.R \
  coordenadas_vertices_plano_cartesiano_metacognitivo_interpretacion_n2_schoice_v1.Rmd --n 40
```

### Siguiente paso concreto

**P0.1, P1.1 (en parte), P2.5 y P2.7 están cerrados** (2026-07-28). No queda ningún bloqueante
abierto. Lo que sigue, por orden de valor:

**1. Decidir si el residual de P1.1 importa.** La silueta a `ratio 1.5` sigue sin leerse como barco
(27 % de las versiones, medido sobre el espacio de 222 vigente entonces). No es bloqueante — la
clave es correcta en las 374 — pero si quieres resolverlo, las opciones A y B siguen documentadas
en [`docs/BACKLOG.md`](docs/BACKLOG.md) P1.1.

**2. Promoción a `02-En-Desarrollo/`** — ver [`docs/ROADMAP.md`](docs/ROADMAP.md) §3. Todos los
criterios de promoción están cumplidos.

Después de cualquier cambio:

```bash
Rscript verificar_render.R
Rscript ../../../.claude/scripts/validar_diversidad_sustantiva.R <rmd> --n 40
Rscript ../../../.claude/scripts/validar_coherencia_matematica.R <rmd>
# + enumeración exhaustiva (script en .claude/rules/barco-parametrico.md §Verificación)
# + comprobación de estructura 2×2 de las 4 opciones sobre las 374 versiones (BACKLOG P0.1, P2.7)
```

Tras cerrar P0.1, P1.1, P2.5 y P2.7, el subproyecto queda listo para el gate de promoción a
`02-En-Desarrollo/`
([`docs/ROADMAP.md`](docs/ROADMAP.md) §3).

---

## 8. Reglas del repo que aplican

`#16` workflow state enforcement · `#18` `{width=...}` anti-`\pandocbounded` · `#19`
letter-independence · `#20` guard `\newcounter{none}` · `#21` Familias 1 y 5 (`pick_int`,
`safe_sample`) · `#22` diversidad sustantiva · `#17` infraestructura protegida (se aplicó su
protocolo al cablear los orquestadores).

Reglas locales: [`.claude/CLAUDE.md`](.claude/CLAUDE.md) ·
[`.claude/rules/barco-parametrico.md`](.claude/rules/barco-parametrico.md).

---

**Versión:** 1.1 · **Fecha:** 2026-07-28 (v1.1 — P2.5 y P2.7 resueltos: Solution con 6 subsecciones
canónicas, espacio de versiones 222 → 374, diversidad 38/40 únicos)
