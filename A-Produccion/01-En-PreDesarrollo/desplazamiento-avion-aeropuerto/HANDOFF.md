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
| **Última sesión de trabajo** | 2026-06-28 (commits `287afc01`, `779d7383`) |
| **Este handoff** | 2026-07-28 |

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
| **OE2** | Diversidad **sustantiva** (regla #22): el **valor** de la respuesta correcta varía entre versiones | ✅ HECHO | 294/300 versiones únicas; parámetros vía `sample()`, sin `file.copy` |
| **OE3** | Diversidad **posicional** (Error 24): la correcta no cae siempre en el mismo cuadrante | ✅ HECHO | `dd5f10d1` — pool `orientaciones` (NE/NO/SE/SO) aleatorizado |
| **OE4** | Legibilidad de diagramas: la etiqueta del ángulo no se solapa (Error 23) | ✅ HECHO | `169ab8c6` + `287afc01` — piso `R_fit >= 50` |
| **OE5** | Distractor direccional **plausible**, no outlier eliminable de un vistazo (regla #22 §P5) | ✅ HECHO | `779d7383` — espejo este↔oeste a la distancia correcta, en vez de giro de 180° |
| **OE6** | **Modularizar**: extraer helpers reutilizables del `.Rmd` (561 líneas) | 🚫 **BLOQUEADO** | Intentado el 2026-07-28 con `include_supplement()`: los 5 formatos renderizan, pero rompe `validar_diversidad_sustantiva.R` (40/40 semillas en error). Revertido. Ver `docs/BACKLOG.md` P1.1 |
| **OE7** | **Documentar**: README, Syllabus, Roadmap, Backlog, BluePrint | ✅ HECHO | `README.md` + `docs/{SYLLABUS,ROADMAP,BACKLOG,BLUEPRINT}.md` (2026-07-28) |
| **OE8** | **Cablear orquestadores**: propagar reglas #22 / Errores 23-24 al wrapper de comando | ✅ HECHO | Los 4 wrappers/agentes + Incidente "distractor extremo por construcción" + pre-flight #14/#18 |
| **OE9** | `.claude/` local del subproyecto | ✅ HECHO | `.claude/CLAUDE.md` + `.claude/rules/diagramas-vectoriales.md` + `.gitignore` |
| **OE10** | Promover a `02-En-Desarrollo/` | ⏳ PENDIENTE | Decisión: **no** en este pase; re-validar tras el refactor |
| **OE11** | Validación Nivel 3 (aula, estudiantes reales) → `03-En-Produccion/` | ⏳ FUTURO | Requisito de `/promover-ejercicio` |

---

## 3. Estado real del ejercicio (verificado 2026-07-28)

`ejercicio_state.json` declara los **11 pasos completados** con `aprobacion_usuario` el
2026-06-27T21:03. **Pero hay una segunda ronda de fixes del 2026-06-28 que ese estado NO
refleja** (Errores 23 y 24). Al retomar, considera el paso `validar_diversidad` y
`coherencias_5` como *pendientes de re-confirmación*, no como firmes.

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
| **H3** | **El distractor `GEO-DES-03` (suma) es siempre el vector más largo.** No es estadístico sino **algebraico**: `escala_px_km = 120/(dt+da)` ⟹ `Lpx_suma ≡ 120` px exactos; y `dt+da > dr` y `> da` siempre. La correcta **nunca** ocupa el rank 1 (rank 2 en 168/200, rank 3 en 32/200). Enumeración exhaustiva: 37/37. Atajo: *"la más larga nunca es la correcta"* descarta una opción sin razonar → regla #22 §P5. | Medición propia + Adversario A + Adversario B | ⏳ **P0 del BACKLOG** — requiere rediseño del pool, decisión del usuario |
| **H4** | **El rótulo numérico permite descartar 2 de 4.** Cada diagrama muestra su distancia ("20 km"). El estudiante calcula `dt−da` y descarta los dos cuyo rótulo no coincide, quedando solo correcta vs. espejo. Reduce la parte del ítem que exige razonar la dirección. | Adversario A | ⏳ BACKLOG — decisión de diseño pedagógico |

**Sobre H3 — advertencia de diseño para quien lo resuelva:** el fix recomendado por el adversario
es ampliar el pool a 5 errores y elegir 3 por versión, **más** desacoplar `escala_px_km` del valor
de un distractor concreto (derivarla del máximo efectivamente dibujado). Cuidado: casi todo error
de "resta incompleta" produce valores ≥ `distancia_restante`, así que el pool ampliado **debe**
incluir al menos un error que no dependa de una resta incompleta (p. ej. variantes de dirección
con la magnitud correcta), o el sesgo "distractores más grandes que la correcta" se reproduce.

**Criterio de aceptación de H3:** sobre ≥40 versiones, la respuesta correcta alcanza el rank 1 de
longitud en una fracción no trivial de casos, y ningún distractor ocupa el rango extremo en el
100 %.

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

**Decisión pendiente tuya: el hallazgo H3** (el distractor `GEO-DES-03` es el vector más largo en
el 100 % de los casos, por identidad algebraica). Está documentado como **P0 en `docs/BACKLOG.md`**
con la propuesta técnica y su criterio de aceptación. Requiere rediseñar el pool de distractores
— es decir, redactar errores conceptuales nuevos —, así que cambia el contenido pedagógico del
ítem y no se aplicó por iniciativa propia.

Si decides abordarlo, el orden sugerido es: (1) resolver H3, (2) re-validar los 4 formatos +
diversidad + rank de la correcta, (3) promover a `02-En-Desarrollo` (OE10).

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
