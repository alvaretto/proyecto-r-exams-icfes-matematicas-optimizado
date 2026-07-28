# HANDOFF — Subproyecto `desplazamiento-avion-aeropuerto`

> **Documento de reanudación.** Si retomas este subproyecto después de una pausa, lee este
> archivo y `ejercicio_state.json` ANTES de explorar el repositorio. Todo lo que necesitas
> para continuar sin re-descubrir contexto está aquí.

| Campo | Valor |
|---|---|
| **Ruta (SP)** | `A-Produccion/01-En-PreDesarrollo/desplazamiento-avion-aeropuerto` |
| **Repo raíz (RR)** | `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams` |
| **Ejercicio** | `desplazamiento_avion_aeropuerto_metacognitivo_interpretacion_n3_schoice_v1.Rmd` |
| **Tipo** | SCHOICE metacognitivo con **opciones gráficas** (4 PNG generados dinámicamente) |
| **Origen ICFES** | `MAT-2026-1-020` (cuadernillo 2026-1, pregunta 114) |
| **Rama / remote** | `main` → `git@github.com:alvaretto/proyecto-r-exams-icfes-matematicas-optimizado.git` |
| **Última sesión de trabajo** | 2026-07-28 (commits `08b0130b`, `1e5482c9`, `defe2f24`) |
| **Este handoff** | 2026-07-28 (actualizado al cierre de la sesión de la tarde) |

---

## 1. Objetivo general

Producir y mantener un **ejercicio ICFES SCHOICE metacognitivo de Nivel 3** (competencia
*Interpretación y Representación*, componente *Geométrico-Métrico*) sobre desplazamiento
avión→aeropuerto, derivado del ítem real `MAT-2026-1-020`, en el que **las cuatro opciones de
respuesta son diagramas vectoriales generados dinámicamente** — y consolidarlo como **patrón
replicable** para todo ejercicio futuro con opciones gráficas.

El ejercicio no evalúa cálculo: evalúa si el estudiante **integra distancia + dirección** al
leer una representación esquemática. Los tres distractores son errores conceptuales
documentados (`GEO-DES-01/02/03`), no ruido numérico.

## 2. Objetivos específicos

| OE | Objetivo | Estado | Evidencia |
|---|---|---|---|
| **OE1** | `.Rmd` funcional que renderice en los 4 formatos (HTML/PDF/DOCX/NOPS) | ✅ HECHO | `ejercicio_state.json` paso `renderizado_4_formatos`; `output_*/` |
| **OE2** | Diversidad **sustantiva** (regla #22): el **valor** de la respuesta correcta varía entre versiones | ✅ HECHO | 2026-07-28: **200/200** versiones únicas del render y **39/40** valores únicos de la respuesta correcta; espacio de 1332 enunciados distintos (§5.3). Parámetros vía `sample()`, sin `file.copy` |
| **OE3** | Diversidad **posicional** (Error 24): la correcta no cae siempre en el mismo cuadrante | ✅ HECHO | `dd5f10d1` — pool `orientaciones` (NE/NO/SE/SO) aleatorizado |
| **OE4** | Legibilidad de diagramas: la etiqueta del ángulo no se solapa (Error 23) | ✅ HECHO | `169ab8c6` + `287afc01` — piso `R_fit >= 50` |
| **OE5** | Distractor direccional **plausible**, no outlier eliminable de un vistazo (regla #22 §P5) | ✅ HECHO | `779d7383` — espejo este↔oeste a la distancia correcta, en vez de giro de 180° |
| **OE6** | **Modularizar**: extraer helpers reutilizables del `.Rmd` (561 líneas) | 🚫 **BLOQUEADO** | Intentado el 2026-07-28 con `include_supplement()`: los 5 formatos renderizan, pero rompe `validar_diversidad_sustantiva.R` (40/40 semillas en error). Revertido. Ver `docs/BACKLOG.md` P1.1 |
| **OE7** | **Documentar**: README, Syllabus, Roadmap, Backlog, BluePrint | ✅ HECHO | `README.md` + `docs/{SYLLABUS,ROADMAP,BACKLOG,BLUEPRINT}.md` (2026-07-28) |
| **OE8** | **Cablear orquestadores**: propagar reglas #22 / Errores 23-24 al wrapper de comando | ✅ HECHO | Los 4 wrappers/agentes + Incidente "distractor extremo por construcción" + pre-flight #14/#18 |
| **OE9** | `.claude/` local del subproyecto | ✅ HECHO | `.claude/CLAUDE.md` + `.claude/rules/diagramas-vectoriales.md` + `.gitignore` |
| **OE10** | Promover a `02-En-Desarrollo/` | ⏳ PENDIENTE | **Criterio fijado por el usuario el 2026-07-28 (decisión D6): la promoción a `02-En-Desarrollo` NO depende de la validación técnica, sino de haber testeado el ejercicio en campo con estudiantes.** La re-validación técnica ya está hecha (ver §3) y no habilita por sí sola el movimiento |
| **OE11** | Validación Nivel 3 (aula, estudiantes reales) → `03-En-Produccion/` | ⏳ FUTURO | Requisito de `/promover-ejercicio` |

---

## 3. Estado real del ejercicio (verificado 2026-07-28)

`ejercicio_state.json` declara los **11 pasos completados**. Los pasos
`renderizado_4_formatos`, `coherencias_5` y `validar_diversidad` fueron **re-confirmados el
2026-07-28** (commit `defe2f24`), de modo que ya reflejan los fixes de los Errores 23/24, H1
(fuga en Moodle), H2 (diagramas degenerados) y H3 (distractor extremo).

### Re-validación del 2026-07-28 (evidencia)

| Verificación | Resultado |
|---|---|
| `validar_diversidad_sustantiva.R --n 40` | **PASS** — 35 valores únicos, 0 errores, 0 indeterminadas |
| Renderizado HTML / PDF / DOCX / NOPS / **Moodle** | **5/5 OK** |
| H1 — nombres de archivo en el XML de Moodle | cerrada: solo `diagrama_a/b/c/d.png` |
| H3 — rank de longitud de la correcta (40 versiones, medición independiente) | rank 1 en 8/40 (20 %), rank 2 en 24/40, rank 3 en 8/40 → sin extremo sistemático |
| Legibilidad: longitud en píxeles de la opción más corta | mediana 55 px; toca el piso de 30 px en **3/40 versiones (7,5 %)** |
| Inspección visual FASE 2B (2 semillas × 4 opciones, ampliadas ×2) | sin solapes de etiqueta; ejes, rótulos y arcos coherentes con el enunciado |
| `tests/run_all_tests.R` | **20/20 suites en verde** (683 s) |

### Diversidad tras retirar el reseed por reloj (H6) — medición

Pregunta natural al quitar `set.seed(Sys.time()...)`: *¿se pierde variedad al renderizar?* **No.**
El reseed no era la fuente de la variedad, sino un sustituto de ella: R-exams no reinicia la
semilla en cada versión, deja correr su flujo de números aleatorios, así que la versión *k+1*
continúa donde quedó la *k* y sale distinta. Y al arrancar, R inicializa ese flujo desde el reloj
y el PID por su cuenta — justo lo que las 2 líneas borradas hacían a mano encima de algo que ya
ocurría.

| Escenario | Resultado |
|---|---|
| Un solo render de 60 versiones (`xexams(n=60)`) | **60/60 únicas** |
| Dos sesiones de R independientes, 5 versiones cada una | huellas distintas (`20c87cf7 42bab918 …` vs `d2696b8a a909acc2 …`) |
| Diversidad sustantiva (valor de la respuesta correcta, 40 semillas) | **39/40 únicos** — mejor que los 35/40 previos al cambio |

Lo que **sí** cambia, y es lo que se buscaba: si alguien fija la semilla a propósito
(`set.seed(K)` antes de `exams2*`), ahora el examen se repite. Antes el reloj lo pisaba. Eso es
lo que permite reproducir una versión defectuosa para corregirla y regenerar el examen exacto que
vio un curso.

**Efecto en los exportadores** (verificado): en `SemilleroUnico_v2.R` y `SemilleroMoodle_v2.R` el
`set.seed` está **comentado** (con nota explícita de dejar que `exams2*` maneje las semillas), así
que para producción no cambia nada. El único `set.seed` activo es `SemilleroCloze.R:95`, dentro de
`prueba_rapida()` — ahí el cambio es a favor: esa prueba ahora rinde **siempre la misma versión**,
que es lo que uno quiere de una prueba de humo.

### Anatomía del `.Rmd` (561 líneas, 7 chunks)

| Chunk | Líneas | Función |
|---|---|---|
| `data_generation` | 1–424 | Parámetros aleatorios, dibujo de los 4 PNG, pool de errores, contextos narrativos, mezcla, `test_that` |
| `enunciado` | 433–435 | Texto del contexto |
| `answerlist_opciones` | 442–446 | Emite `![](diagrama_*.png){width=70%}` |
| `solution_setup` | 451–459 | Mapeos internos descripción↔opción |
| `analisis_diagramas` | 463–484 | Descripción de cada opción |
| `diagrama_correcto_solucion` | 503–507 | PNG de la correcta (por `indice_correcto`, **no** por letra) |
| `explicacion_errores` | 511–527 | `causa_raiz` de cada distractor |

**Función central:** `dibujar_diagrama()` (líneas 54–115) — dibuja con `grid` la cruz de ejes
cardinales, el rayo con ángulo, el arco y las etiquetas. Contiene el fix del Error 23 en la
línea 94 (`R_fit <- max(50, ...)`).

**Parámetros aleatorizados:** `distancia_total` (80–150), `distancia_avanzada` (20–60 filtrada),
`angulo_direccion` (30–70), `orient` (uno de 4 cuadrantes), contexto narrativo, reflexión
metacognitiva. **Derivados:** `distancia_restante`, `escala_px_km`, `dir_desc`.

**Los 4 PNG** se generan con `dibujar_diagrama()`, nunca con `file.copy`:
`diagrama_correcta.png` (correcta), `diagrama_recorrida.png` (GEO-DES-02),
`diagrama_suma.png` (GEO-DES-03), `diagrama_perp.png` (GEO-DES-01, espejo).

### Defensas verificadas presentes

`\newcounter{none}` guard (429–431) · `{width=...}` en todas las imágenes (444, 506) ·
letter-independence: `letra_correcta` solo se usa internamente, nunca se emite al estudiante ·
`exshuffle: FALSE` + `sample()` interno (correcto para opciones gráficas) ·
`calcula()` puras con `precondicion` declarada · sin `repeat` sin cota (usa `Filter`).

---

## 4. Decisiones tomadas en esta sesión (y su porqué)

| # | Decisión | Porqué |
|---|---|---|
| D1 | Syllabus/Roadmap/Backlog/BluePrint van en **`SP/docs/`** | Viajan con el ejercicio al promoverlo y sirven de plantilla para el siguiente |
| D2 | **Crear `SP/.claude/` local** | Subproyecto autocontenido sin tocar la infraestructura protegida de `RR/.claude/` |
| D3 | Ruido → **`SP/_archivo/`** (mover, no borrar) | Reversible; nada se pierde |
| D4 | **NO promover** a `02-En-Desarrollo` en este pase | El refactor invalida parte de la validación previa; re-validar primero |
| D6 | **La promoción a `02-En-Desarrollo` exige prueba de campo con estudiantes** (2026-07-28) | Decisión explícita del usuario. La validación técnica (renderizado, diversidad, coherencias, tests) es condición necesaria pero **no suficiente**: el ejercicio se queda en `01-En-PreDesarrollo` hasta que se aplique en aula. Deja obsoleta la lectura previa de que OE10 se habilitaba al terminar la re-validación |
| D5 | **`Semillero*.R` y `pcielo*.tex` NO son ruido** | ⚠️ **Corrección sobre el plan inicial.** `SemilleroUnico_v2.R` referencia el `.Rmd` y usa `template = "solpcielo"` / `"pcielo.tex"` en líneas **activas** (70, 83). Archivarlos rompe la exportación a PDF/Moodle. Son **código fuente a trackear**, y sus rutas son relativas al directorio del ejercicio → **no moverlos** |

---

## 5. Hallazgos abiertos (entrada directa al trabajo pendiente)

> **Sesión 2026-07-28.** Revisión adversarial doble + auditoría visual + medición sobre 200
> versiones y enumeración exhaustiva de las 37 combinaciones válidas. Resultados abajo.

### 5.0 — Hallazgos de la revisión adversarial (2026-07-28)

| # | Hallazgo | Confirmado por | Estado |
|---|---|---|---|
| **H1** | **Fuga de la respuesta por nombre de archivo.** `exams2moodle` emite `src="@@PLUGINFILE@@/diagrama_correcta.png"`. Un estudiante que inspeccione el HTML en Moodle ve la respuesta sin razonar. En HTML puro NO ocurre (R-exams incrusta base64); el canal afectado es **Moodle**, que es el destino real (`SemilleroMoodle_v2.R`). Viola `graficos-como-opciones.md`, que exige nombres neutrales `diagrama_a.png`. | Adversario A + verificación directa del XML | 🔧 **CORREGIDO 2026-07-28** (renombrado neutral post-mezcla) |
| **H2** | **Diagramas degenerados.** Con `dt=80, da=60` → `dr=20` y `escala=120/140`, el vector correcto mide **17,1 px**: el punto queda sobre el origen y la etiqueta flota a 58 px. Afecta a **2/37 combinaciones (5,4 %)**; el adversario midió 4/60 (6,7 %) por muestreo. | Barrido visual propio + Adversario A | 🔧 **CORREGIDO 2026-07-28** (umbral `f=0.25`, mín. 30 px) |
| **H3** | **El distractor `GEO-DES-03` (suma) es siempre el vector más largo.** No es estadístico sino **algebraico**: `escala_px_km = 120/(dt+da)` ⟹ `Lpx_suma ≡ 120` px exactos; y `dt+da > dr` y `> da` siempre. La correcta **nunca** ocupa el rank 1 (rank 2 en 168/200, rank 3 en 32/200). Enumeración exhaustiva: 37/37. Atajo: *"la más larga nunca es la correcta"* descarta una opción sin razonar → regla #22 §P5. | Medición propia + Adversario A + Adversario B | ✅ **RESUELTO 2026-07-28** — pool ampliado a 6 errores (1 fijo + 2 sorteados de 5) + escala desacoplada. Ver §5.2 |
| **H5** | **La lista del «Procedimiento correcto» reiniciaba la numeración.** La ecuación en display estaba a columna 0 dentro de la lista ordenada; pandoc cerraba la enumeración y abría otra, así que en PDF el estudiante leía «(a) Dirección final» justo después de «(d) Nueva distancia». | Inspección visual FASE 2B del PDF | 🔧 **CORREGIDO 2026-07-28** (`defe2f24`) — ecuación indentada 3 espacios dentro del ítem 4; verificado (a)→(e) |
| **H6** | **El ejercicio ignora la semilla de R-exams.** Líneas 11-12: `seed_global <- as.integer(Sys.time()) %% 100000 + sample(1:99999,1)` seguido de `set.seed(seed_global)`. Verificado empíricamente: `set.seed(42)` dos veces produce `dt=130,ang=45` y luego `dt=140,ang=55`. Consecuencia: **ninguna validación multi-semilla es reproducible aquí** — un fallo visto en la semilla N no se puede volver a provocar, y el PASS de diversidad lo garantiza en parte el reloj. No es exclusivo: 11 `.Rmd` del repo usan el patrón, 2 ya en `03-En-Produccion`. Ninguna otra línea usa `seed_global` (quitarlo = 2 líneas). | Prueba empírica propia (misma semilla, dos corridas) | 🔧 **CORREGIDO 2026-07-28** — reseed retirado por decisión del usuario; en su lugar queda un comentario que explica por qué no se debe reintroducir. Verificado: `set.seed(42)` ahora da el mismo ejercicio en corridas sucesivas; diversidad **PASS 39/40 valores únicos** (subió desde 35/40); 5 formatos OK; rank de la correcta 10/22/8 (sin regresión) |
| **H7** | **Piso de legibilidad de 30 px, apretado.** En 3/40 versiones (7,5 %) la opción más corta cae exactamente en el piso `f=0.25`; ahí el arco y la etiqueta del ángulo sobresalen del propio vector (radio de etiqueta ≥50 px > longitud del vector). Legible, pero visualmente estrecho. Subir `f` lo suavizaría a costa de reducir el espacio de combinaciones válidas. | Medición propia (40 versiones) + inspección visual | ⏳ BACKLOG — observación, no bloqueante |
| **H4** | **El rótulo numérico permite descartar 2 de 4.** Cada diagrama muestra su distancia ("20 km"). El estudiante calcula `dt−da` y descarta los dos cuyo rótulo no coincide, quedando solo correcta vs. espejo. Reduce la parte del ítem que exige razonar la dirección. | Adversario A | ⏳ BACKLOG — decisión de diseño pedagógico |

### 5.2 — Resolución de H3 (2026-07-28)

**Pool ampliado de 3 a 6 errores conceptuales**, con `GEO-DES-01` (espejo) **siempre presente** —
es el discriminador central del ítem — y **2 sorteados** de los otros cinco:

| Código | Error conceptual | Longitud vs. correcta | Precondición |
|---|---|---|---|
| `GEO-DES-01` | Dirección reflejada (espejo del eje) | **igual** | siempre |
| `GEO-DES-02` | Usa la distancia recorrida | menor o mayor | siempre |
| `GEO-DES-03` | Suma en vez de restar | mayor | siempre |
| `GEO-DES-04` | Posición inicial sin actualizar | mayor | siempre |
| `GEO-DES-05` | Ángulo medido desde el eje perpendicular | **igual** | `ángulo ≠ 45` |
| `GEO-DES-06` | Resta aplicada dos veces | **menor** | `avanzada < total/2` y `total ≠ 3·avanzada` |

`GEO-DES-05` y `GEO-DES-06` son los que rompen el sesgo que advertía el adversario: uno conserva
la magnitud correcta y el otro produce un vector **más corto** que la correcta, de modo que los
distractores dejan de estar sistemáticamente "por encima".

Cambios de mecánica:

- **Escala desacoplada:** `escala_px_km <- 120 / max(distancias_finales)` — ya no deriva de
  `distancia_total + distancia_avanzada`, que era lo que fijaba a `GEO-DES-03` en 120 px exactos.
- **Selección por enumeración, sin bucles de reintento** (regla #21, Familia 1): se enumeran todas
  las parejas de candidatos aplicables con `combn()`, se filtran las que cumplen distancias
  positivas + 4 diagramas distintos + `min/max ≥ 0.25` (legibilidad), y se sortea entre las
  válidas. Mediana de 6 parejas válidas por versión; 0 versiones sin pareja viable.
- El filtro `f_legibilidad` sobre `distancia_avanzada` se **eliminó**: la legibilidad se garantiza
  ahora sobre las 4 opciones realmente elegidas, que es más preciso.

**Verificación del criterio de aceptación (60 versiones del chunk real, 0 errores):**

| Criterio | Antes | Ahora |
|---|---|---|
| La correcta alcanza rank 1 de longitud | 0/200 (0 %) | **9/60 (15 %)** |
| Dominancia máxima del extremo | `GEO-DES-03` 100 % | **43,3 % (largo) / 58,3 % (corto)** |
| Legibilidad `px_min` | — | **30,0 px; 0 versiones por debajo** |
| Opciones que comparten longitud con la correcta | 1 (la longitud delataba) | **siempre ≥2** |
| Combinaciones distintas de distractores | 1 (fija) | **10** |

La última fila es la garantía estructural: **la longitud nunca identifica la respuesta por sí
sola**, porque la correcta y su espejo siempre miden lo mismo. Quien intente "la más larga" o "la
más corta" se queda con dos candidatas y debe comparar dirección — que es lo que el ítem evalúa.

### 5.3 — Espacio de versiones: ¿cuántas preguntas distintas puede dar el ejercicio?

Conteo exacto sobre el código (no estimación), a 2026-07-28:

| Factor | Valores | Qué cambia |
|---|---|---|
| Pares `(distancia_total, distancia_avanzada)` válidos | **37** | los números del enunciado y de las 4 opciones |
| `angulo_direccion` — `seq(30, 70, by = 5)` | **9** | la dirección dibujada y el rótulo del ángulo |
| `orient` — cuadrante NE/NO/SE/SO | **4** | la orientación de toda la escena y el texto de dirección |
| **Enunciados matemáticamente distintos** | **37 × 9 × 4 = 1332** | |
| Pareja de distractores (además de `GEO-DES-01`, siempre presente) | 10 posibles, mediana **6** válidas por versión | qué errores conceptuales se contrastan |
| Contextos narrativos (cada uno con varios protagonistas) | **8** | la situación del enunciado |
| Reflexiones metacognitivas | **6** | solo la Solution |
| Orden de las 4 opciones (`sample()` interno) | **24** | la posición de la correcta |

El umbral del repo (regla #3 de `codigo-rmd.md`) es **200+ versiones únicas**, y el espacio
sustantivo por sí solo —sin contar contexto, orden ni reflexión— es de **1332 enunciados**.

**Medición del 2026-07-28 (tras retirar el reseed):** `xexams(n = 200)` → **200/200 versiones
únicas**, sin una sola colisión. El umbral de la regla #3 se cumple con holgura; el registro
previo de "294/300" es del estado anterior al cambio.

El límite práctico no es la combinatoria del ejercicio sino el muestreo: al pedir muchas más
versiones que 200, alguna colisión aparece por el problema del cumpleaños (extracción con
reemplazo de un espacio finito), no por falta de variedad del generador.

### 5.1 — Infraestructura

1. **6 de 10 agentes ICFES no arrancaban.** `agente-detractor`, `clasificador-icfes`,
   `corrector-coherencia`, `diagnosticador-errores`, `pedagogo-icfes` y `validador-visual`
   declaraban `tools:` en **minúscula**; Claude Code no reconoce esos nombres y rechaza la
   instanciación ("would be spawned with zero tools"). Consecuencia: la **regla #9 (detractor
   obligatorio en FASE 2C) llevaba tiempo sin poder ejecutarse**.
   ✅ **CORREGIDO** + nueva invariante **I-9** con test que falla si alguien revierte.
   ⚠️ **El registro de agentes se cachea al iniciar sesión**: el fix no surte efecto hasta abrir
   una sesión nueva. Al retomar, verifícalo lanzando `AgenteDetractor`.
2. **Gap de cableado (OE8).** `RR/.claude/agents/orquestador-schoice.md` **sí** documenta la
   regla #22 (L193, L201, L217), el Error 24 (L206), el Error 23 (Incidente G, L219) y
   `ERR_DIV_COSMETICA` en el paso 9 (L264). Pero `RR/.claude/commands/orquestador-schoice.md`
   era un **wrapper que no mencionaba ninguno**. ✅ **CORREGIDO** en ambos wrappers, más
   Incidente nuevo ("Distractor extremo por construcción algebraica") y pre-flight check #14
   (schoice) / #18 (cloze) en los agentes.
3. **README del repo desactualizado.** Declara *"Sistema v3.2.2 — Febrero 2026"* y *"mínima de
   300 versiones únicas"*, cuando `.claude/CLAUDE.md` va en **v3.17.1 (2026-06-27)** y la regla #3
   de `codigo-rmd.md` fija el umbral en **200+**. ⏳ pendiente.
2. **README del repo desactualizado.** Declara *"Sistema v3.2.2 — Febrero 2026"* (L440/L444)
   cuando `.claude/CLAUDE.md` va en **v3.17.1 (2026-06-27)**. No menciona este subproyecto ni
   las reglas #22 / Errores 23-24.
3. **`referencia_original.png` borrado sin commitear** (`D` en `git status`). Es la imagen
   fuente del cuadernillo y el insumo del Flujo B → **restaurar** con
   `git checkout -- <ruta>` salvo decisión contraria explícita.
4. **Artefactos sin trackear** que no deben commitearse: los 4 `diagrama_*.png` (se regeneran),
   `VER/`, `VER70/`, `salida/`, `.codex`.
5. **SVGs huérfanos**: `diagrama_[a-d].svg` corresponden a una nomenclatura anterior
   (`a/b/c/d`) que ya no coincide con los PNG vigentes (`correcta/perp/recorrida/suma`).
6. **Formato equilibrado**: las 4 opciones son gráficas (formato único). La regla
   `graficos-como-opciones.md` §Formato Equilibrado está satisfecha por construcción, pero
   conviene dejarlo escrito en el BluePrint para que no se reabra en cada auditoría.

---

## 6. Riesgos

| Riesgo | Mitigación |
|---|---|
| El refactor de `dibujar_diagrama()` a un helper externo rompe la **auto-contención** que R-exams exige (copia el `.Rmd` a un `edir` temporal) | Copiar el helper dentro del chunk, o `source()` con ruta resuelta en tiempo de render; validar con los 4 formatos, no solo HTML |
| Commit contaminado | El repo tiene ~15 archivos modificados/untracked **ajenos** al subproyecto. **Nunca `git add -A`** |
| Regresión silenciosa de diversidad | `Rscript .claude/scripts/validar_diversidad_sustantiva.R <rmd> --n 40` tras **cada** cambio al `data_generation`; exit 1 = bloqueante |
| Falso "todo verde" | El runner marca fallo si `failed>0` o `error>0`; leer la salida real, no el resumen |

---

## 7. Cómo retomar

Cuando escriba **`Continúa con el proyecto <ruta del SP>`**, la primera acción debe ser:

1. Leer este `HANDOFF.md` y `ejercicio_state.json`.
2. Responder con: estado actual + OEs pendientes + el siguiente paso concreto.
3. **No** re-explorar el repositorio ni repetir el descubrimiento ya consolidado aquí.

### Comandos de arranque

```bash
cd /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/desplazamiento-avion-aeropuerto

# 1. Estado del workflow
../../../.claude/scripts/workflow-state.sh status .

# 2. Qué cambió desde la última sesión
git -C ../../.. log --oneline -8 -- .
git -C ../../.. status --short -- .

# 3. Salud del ejercicio (bloqueante si exit 1)
Rscript ../../../.claude/scripts/validar_diversidad_sustantiva.R \
  desplazamiento_avion_aeropuerto_metacognitivo_interpretacion_n3_schoice_v1.Rmd --n 40

# 4. Salud del ecosistema
Rscript ../../../tests/run_all_tests.R              # 20 suites
Rscript ../../../tests/testthat/test_infraestructura_claude.R   # invariantes I-1..I-8
```

### Siguiente paso concreto

**El ejercicio está técnicamente validado y se queda en `01-En-PreDesarrollo`.** La
re-validación completa está hecha (§3) y H3/H5 quedaron resueltos. Por decisión D6, lo único que
habilita OE10 es la **prueba de campo con estudiantes** — no hay más trabajo técnico que sea
prerrequisito del movimiento.

Al retomar, quedan dos cosas abiertas, ninguna bloqueante y ninguna técnica:

1. **H7 — piso de 30 px**: la opción más corta toca el piso en ~10 % de las versiones y ahí el
   diagrama queda visualmente estrecho. Subir `f` por encima de 0.25 lo suaviza a costa de
   reducir el espacio de combinaciones válidas de distractores.
2. **H4 — el rótulo numérico permite descartar 2 de 4**: decisión de diseño pedagógico (si se
   quita el rótulo de distancia, el ítem exige estimar la magnitud a ojo).

Ambas son decisiones de diseño, no defectos. El siguiente hito real es la **prueba de campo**.

<details>
<summary>Contexto histórico de H3 (resuelto)</summary>

**Decisión pendiente tuya: el hallazgo H3** (el distractor `GEO-DES-03` es el vector más largo en
el 100 % de los casos, por identidad algebraica). Está documentado como **P0 en `docs/BACKLOG.md`**
con la propuesta técnica y su criterio de aceptación. Requiere rediseñar el pool de distractores
— es decir, redactar errores conceptuales nuevos —, así que cambia el contenido pedagógico del
ítem y no se aplicó por iniciativa propia.

Si decides abordarlo, el orden sugerido es: (1) resolver H3, (2) re-validar los 4 formatos +
diversidad + rank de la correcta, (3) promover a `02-En-Desarrollo` (OE10).

</details>

**OE6 (modularización) está BLOQUEADO — no reintentar sin desbloquear primero.** Se intentó el
2026-07-28 extrayendo estos bloques a `SP/R/helpers_diagramas.R` con el mecanismo oficial
`include_supplement()`; los 5 formatos renderizaban bien, pero `validar_diversidad_sustantiva.R`
falló en 40/40 semillas (evalúa el chunk aislado en un tempdir, donde `include_supplement()` no
tiene contexto). Se revirtió. El helper extraído quedó en
`_archivo/propuesta-modularizacion/helpers_diagramas.R`, listo para reactivarse. Criterio de
desbloqueo en `docs/BACKLOG.md` (P1.1).

Bloques candidatos, cuando se desbloquee:

| Bloque | Líneas | Helper propuesto |
|---|---|---|
| Pool de 4 cuadrantes cardinales | 29–34 | `orientaciones_cardinales()` |
| Dibujo de diagrama vectorial polar | 54–115 | `dibujar_diagrama()` |
| Wrappers `grid` (`ln`, `tx`, `pl`, `cir`) | 62–65 | `snippets_graficos_grid.R` |
| Pool de contextos narrativos | 166–319 | `contextos_narrativos_navegacion()` |
| Pool de reflexiones metacognitivas | 393–400 | `reflexiones_vectores()` |
| Batería `test_that` de SCHOICE gráfico | 404–423 | `tests_schoice_grafico()` |

OE7 (docs), OE8 (cableado) y OE9 (`.claude/` local) quedaron **completados** el 2026-07-28.

---

## 8. Reglas del repo que aplican a este subproyecto

`#4` gráficos como opciones · `#6` `exshuffle` · `#18` `{width=...}` anti-`\pandocbounded` ·
`#19` letter-independence · `#20` guard `\newcounter{none}` · `#21` familias de soluciones ·
`#22` diversidad sustantiva ← **originada aquí**.

Errores del catálogo (`.claude/docs/patrones-errores-conocidos.md`):
**22** (L2343, `repeat` sin cota) · **23** (L2432, etiquetas solapadas) ← originado aquí ·
**24** (L2485, predictibilidad posicional) ← originado aquí.

**Prohibido:** `git commit --no-verify`, `PREPUSH_SKIP_TESTS=1`, editar
`03-En-Produccion/` o `Ejemplos-Funcionales-Rmd/`.

---

*Actualiza este archivo al cerrar cada sesión de trabajo.*
