# `.claude/` Local — Subproyecto `desplazamiento-avion-aeropuerto`

> **Herencia obligatoria.** Este `.claude/` local **hereda** del ecosistema del repositorio raíz
> (`RR/.claude/CLAUDE.md` + `RR/CLAUDE.md`) y **NO lo reemplaza**. Toda regla, hook, skill o
> agente definido en `RR/.claude/` sigue aplicando aquí sin cambios. Este archivo únicamente
> **añade** contexto y particularidades operativas específicas de este ejercicio.
>
> **En caso de conflicto entre una instrucción de este archivo y una regla del repositorio raíz,
> GANA la regla del repositorio raíz.** Este `.claude/` local es estrictamente aditivo. No
> modifica, no reinterpreta y no tiene autoridad para suspender ninguna de las 22 reglas críticas
> de `RR/.claude/CLAUDE.md` (índice) ni el gate mecánico de `RR/.claude/hooks/pre-write-rmd-gate.sh`.
>
> Ruta del repositorio raíz: `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams`

---

## Fuente de verdad para retomar

**`HANDOFF.md`** (en la raíz de este subproyecto) es el documento de reanudación. Contiene
objetivos generales/específicos, estado real verificado, decisiones tomadas, hallazgos abiertos,
riesgos y el siguiente paso concreto. **Léelo primero**, junto con `ejercicio_state.json`, antes
de explorar el `.Rmd` o el repositorio.

```
Continúa con el proyecto A-Produccion/01-En-PreDesarrollo/desplazamiento-avion-aeropuerto
```
debe disparar la lectura de `HANDOFF.md` + `ejercicio_state.json` como primera acción, sin
re-descubrir contexto ya consolidado.

---

## Identidad del ejercicio

| Campo | Valor |
|---|---|
| **Archivo** | `desplazamiento_avion_aeropuerto_metacognitivo_interpretacion_n3_schoice_v1.Rmd` |
| **Tipo** | SCHOICE metacognitivo — **4 opciones son diagramas vectoriales PNG generados dinámicamente** |
| **Nivel ICFES** | N3 (`exextra[Nivel]: 3`, `DOK: 3`, `Bloom: Analizar`, `SOLO: Relacional`) |
| **Competencia** | Interpretación y Representación |
| **Componente** | Geométrico-Métrico |
| **Origen** | `MAT-2026-1-020` (cuadernillo ICFES 2026-1, pregunta 114) |
| **Evalúa** | Integración distancia + dirección al leer un diagrama esquemático (NO cálculo aislado) |
| **Distractores** | 3 errores conceptuales documentados: `GEO-DES-01/02/03` (no ruido numérico) |

---

## Particularidades operativas (léelas ANTES de tocar el `.Rmd`)

Estas son notas de **por qué el código es como es**. Sin ellas, un agente puede "arreglar" algo
que en realidad es un fix deliberado de un error ya documentado y reintroducir el bug.

### 1. `dibujar_diagrama()` es la función central — el piso `R_fit >= 50` es intencional

Definida en el chunk `data_generation`, dibuja con `grid` la cruz de ejes cardinales, el rayo con
ángulo, el arco y las etiquetas. La línea `R_fit <- max(50, (8 + 11 * cos(semi)) / sin(semi))`
es el **fix del Error 23** (`.claude/docs/patrones-errores-conocidos.md` del repo raíz, L2432):
para ángulos grandes (cuña ancha, p. ej. 70°) la fórmula sin piso da un radio ~30 y la etiqueta
"NN°" queda clipada por la línea casi horizontal. El piso 50 (no 34, que fue el primer intento
insuficiente) da holgura suficiente en **todo** el rango de ángulos aleatorizados (30°–70°).

**NO bajar este piso** sin volver a renderizar y verificar visualmente diagramas en TODO el rango
30°–70° (el peor caso NO es el ángulo más pequeño, es el más grande — lo verificado en la sesión
2026-06-28 tras revisión a escala ×2.4).

### 2. `escala_px_km` NO debe acoplarse al valor de un distractor concreto

`escala_px_km <- 120 / (distancia_total + distancia_avanzada)` (línea 120) deriva la escala del
**máximo valor efectivamente dibujado**, no de una constante ni del valor de una opción específica.

**Hallazgo 2026-07-28**: una versión anterior de esta línea acoplaba la escala al valor exacto
del distractor `GEO-DES-03` (suma), haciendo que ese diagrama midiera **siempre 120 px exactos**
en cualquier versión — una regularidad visual detectable sin leer los datos. Si se modifica esta
línea, verificar que ningún distractor conserva una longitud de píxel constante entre semillas
(cross-check visual, no solo el valor numérico en km).

### 3. El pool `orientaciones` (4 cuadrantes cardinales) rompe la predictibilidad de cuadrante

Bloque `orientaciones <- list(...)` (NE/NO/SE/SO) al inicio del chunk `data_generation`. Es el
fix del **Error 24** (`patrones-errores-conocidos.md`, L2485, originado en este ejercicio): sin
este pool, el diagrama correcto caía siempre en el mismo cuadrante visual y el estudiante podía
predecir la respuesta por posición, no por contenido (violación de la regla #22 §P4 del repo
raíz — diversidad sustantiva). **No eliminar este pool** ni fijar `orient` a un valor constante.

### 4. El distractor `GEO-DES-01` comparte longitud con la correcta A PROPÓSITO

`GEO-DES-01` ("dirección reflejada") se dibuja a la **misma distancia** (`distancia_restante`)
que la opción correcta, pero con el ángulo medido hacia el lado opuesto del eje (espejo
este↔oeste, NO un giro de 180°). Es el fix de la regla #22 §P5 del repo raíz (evitar un
distractor-outlier eliminable de un vistazo por tener otra longitud). **Esto es intencional**:
NO "corregir" para que tenga una distancia distinta — eso reintroduciría el outlier obvio que la
sesión 2026-06-28 (commit `779d7383`) eliminó deliberadamente.

### 5. `Semillero*.R` y `pcielo*.tex` son FUENTE ACTIVA — no son ruido

`SemilleroUnico_v2.R`, `SemilleroCloze.R`, `SemilleroMoodle_v2.R`, `pcielo.tex` y
`pcielo_nosol.tex` referencian el `.Rmd` y usan plantillas (`template = "solpcielo"` /
`"pcielo.tex"`) en líneas activas de exportación a PDF/Moodle. **Sus rutas son relativas al
directorio del ejercicio.** No moverlos a `_archivo/`, no renombrarlos, no "limpiarlos" como si
fueran residuos de sesiones anteriores — romper su ruta relativa rompe la exportación.

### 6. Los PNG `diagrama_*.png` son artefactos regenerables

`diagrama_correcta.png`, `diagrama_recorrida.png`, `diagrama_suma.png`, `diagrama_perp.png` los
genera `dibujar_diagrama()` en cada render — **nunca** con `file.copy()` (eso reintroduciría la
regresión de diversidad cosmética, regla #22 del repo raíz, originada precisamente en este
ejercicio el 2026-06-27). No editarlos a mano, no commitearlos (ver `.gitignore` local).

### 7. El chunk tiene ÍNDICE e INVARIANTES en su cabecera — respétalos

Desde el 2026-07-28, `data_generation` abre con un índice de sus 14 secciones (`§1`…`§14`) y una
lista de **5 invariantes** que el chunk garantiza. Romper una invariante es un defecto, no un
cambio de estilo:

| # | Invariante | Defensa de |
|---|---|---|
| I1 | Ninguna letra (a/b/c/d) se asigna antes de la mezcla | regla #19 + regla #22 §P6 (Error 25) |
| I2 | Los 4 PNG se generan por versión, nunca se copian | regla #22 (diversidad sustantiva) |
| I3 | La correcta comparte longitud con ≥1 distractor | regla #22 §P5 |
| I4 | La opción más corta respeta el ratio de legibilidad vigente | H7 (`docs/BACKLOG.md` P1.4) |
| I5 | Sin bucles de reintento sin cota | regla #21 Familia 1 (Error 22) |

Al editar el chunk, mantén el índice sincronizado con los banners `# ---- §N — … ----`.

### 8. Umbral de legibilidad en CASCADA — no lo conviertas en umbral único

`RATIOS_LEGIBILIDAD <- c(0.40, 0.35, 0.30, 0.25)` (§7 del chunk). Cada versión se queda en el
escalón más alto que le sea factible y baja de a uno. **No lo "simplifiques" a un valor único**:
con 0.25 fijo aparecen diagramas degenerados (vector de 30 px, con el arco y las etiquetas fuera
del propio vector) y con ≥0.45 hay versiones donde ninguna pareja de distractores cumple y el
`stopifnot` revienta el render. Detalle y mediciones en `.claude/rules/diagramas-vectoriales.md`
§4 y en `docs/BACKLOG.md` P1.4.

### 9. Tres distractores conservan la magnitud A PROPÓSITO

`GEO-DES-01` (espejo del lado), `GEO-DES-05` (ángulo desde el eje perpendicular) y `GEO-DES-07`
(ángulo desde el eje cardinal opuesto) muestran **la misma distancia que la correcta**. No es
redundancia: es lo que impide que el estudiante calcule el valor y descarte por el rótulo sin
mirar el dibujo (hallazgo H4). En el 16 % de las versiones las cuatro opciones comparten rótulo.
Si añades un distractor de este tipo, primero **demuestra** que su dirección no colisiona con
ninguna otra opción en ningún cuadrante ni ángulo del rango.

### 10. NUNCA `set.seed()` dentro del chunk

Verificado en el fuente de `exams:::xexams()`: el control del RNG es del llamador. Sin argumento
`seed` no se fija semilla por versión (el flujo RNG avanza solo y las versiones ya difieren); con
`seed` se fija por versión y se restaura `.Random.seed` — es el mecanismo documentado para
reproducir una versión concreta. Un `set.seed()` derivado del reloj dentro del chunk anula ese
mecanismo y deja sin valor diagnóstico a toda validación multi-semilla. Retirado el 2026-07-28
(H6); el comentario de §1 explica por qué no debe volver.

### 11. El ejercicio DEBE permanecer auto-contenido — NO extraer funciones a archivos externos

**Bloqueado 2026-07-28** (ver `docs/BACKLOG.md`, ítem P1.1): se intentó modularizar
`dibujar_diagrama()`/`km()`/`.cols` a `R/helpers_diagramas.R` con el mecanismo oficial
`include_supplement()` + `source()`. Los 5 formatos renderizaron bien, pero
`../../../../.claude/scripts/validar_diversidad_sustantiva.R` (regla #22, obligatorio) falló 40/40
semillas: evalúa `data_generation` aislado en un tempdir fuera del pipeline de `xexams()`,
contexto donde `include_supplement()` no tiene estado interno. **No reintentar la
modularización** hasta que el validador soporte ejercicios con helpers externos — el `.Rmd` debe
seguir con `dibujar_diagrama()`/`km()`/`.cols` dentro del chunk `data_generation`. Detalle
completo, causa raíz y criterio de desbloqueo en `docs/BACKLOG.md` (P1.1); helper ya extraído y
listo para reactivar en `_archivo/propuesta-modularizacion/helpers_diagramas.R`.

---

## Reglas del repo raíz con mayor peso en este ejercicio

`#4` gráficos como opciones individuales · `#6` `exshuffle: FALSE` + `sample()` interno
(obligatorio aquí por ser opciones gráficas con Solution que referencia la opción correcta por
contenido, no por letra) · `#18` `{width=...}` anti-`\pandocbounded` · `#19`
letter-independence (`letra_correcta` es solo interno, nunca se emite al estudiante) · `#20`
guard `\newcounter{none}` · `#21` familias de soluciones reutilizables · **`#22` diversidad
sustantiva — originada en este ejercicio** (incidente 2026-06-27, ver
`RR/.claude/rules/diversidad-sustantiva.md`).

Errores del catálogo (`RR/.claude/docs/patrones-errores-conocidos.md`):
**22** (`repeat` sin cota) · **23** (etiquetas solapadas, originado aquí) · **24**
(predictibilidad posicional, originado aquí).

Regla local adicional de este subproyecto: `.claude/rules/diagramas-vectoriales.md` (ver abajo).

---

## Reglas locales de este subproyecto

- **`.claude/rules/diagramas-vectoriales.md`** — cómo construir diagramas vectoriales de opción
  (etiqueta de ángulo, escala global, orientación, verificación de rango posicional). Formaliza
  las particularidades operativas 1–3 de arriba como regla verificable.

---

## Prohibido (heredado + reforzado para este subproyecto)

- Tocar cualquier archivo dentro de `RR/.claude/` (infraestructura protegida, regla #17 del repo
  raíz). Este `.claude/` local solo existe dentro de este subproyecto.
- `git commit --no-verify`, `PREPUSH_SKIP_TESTS=1`.
- Editar `03-En-Produccion/` o `Ejemplos-Funcionales-Rmd/` del repo raíz.
- `git add -A` en este repositorio — hay ~15 archivos ajenos modificados/untracked en el árbol
  de trabajo del repo raíz; solo agregar archivos explícitos de este subproyecto.
- Bajar el piso `R_fit >= 50`, acoplar `escala_px_km` a un distractor concreto, eliminar el pool
  `orientaciones`, o "corregir" la distancia del distractor `GEO-DES-01` — ver particularidades
  operativas 1, 2, 3 y 4 arriba.
- Convertir la cascada `RATIOS_LEGIBILIDAD` en un umbral único, quitar los distractores que
  conservan la magnitud, o reintroducir un `set.seed()` en el chunk — particularidades 8, 9 y 10.

---

**Versión:** 2.0
**Fecha:** 2026-07-28 (v2.0 — particularidades 7-10: índice e invariantes del chunk, cascada de
legibilidad, distractores que conservan magnitud, prohibición de `set.seed()`; v1.0 mismo día)
**Estado:** ACTIVO
**Alcance:** Solo dentro de `A-Produccion/01-En-PreDesarrollo/desplazamiento-avion-aeropuerto/`
