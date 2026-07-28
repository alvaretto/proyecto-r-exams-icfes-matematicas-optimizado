# Backlog — Desplazamiento avión→aeropuerto

> Contexto completo en [`../HANDOFF.md`](../HANDOFF.md). Cada ítem tiene un criterio de
> aceptación **verificable** (un comando o una comprobación concreta), no una descripción vaga.

## P0 — Bloqueante para promoción

### P0.1 — Distractor `GEO-DES-03` extremo por construcción (regla #22 §P5)

**Hallazgo confirmado por medición + demostración algebraica, 2026-07-28.**

**Evidencia (demostración)**:

`escala_px_km = 120 / (distancia_total + distancia_avanzada)` (`.Rmd` línea 99). El diagrama
`diagrama_suma.png` se dibuja con `dist_km = distancia_total + distancia_avanzada` (línea 103),
por lo tanto su longitud en píxeles es:

```
L_suma = (distancia_total + distancia_avanzada) × escala_px_km
       = (distancia_total + distancia_avanzada) × [120 / (distancia_total + distancia_avanzada)]
       = 120 px            ← identidad algebraica, SIEMPRE exacto, para cualquier semilla
```

Como `distancia_avanzada > 0` y `distancia_total > 0` (generados por `sample()`, líneas 11-14),
se cumple **siempre**: `distancia_total + distancia_avanzada > distancia_total > distancia_restante`
y `distancia_total + distancia_avanzada > distancia_avanzada`. Por lo tanto `diagrama_suma.png`
(error `GEO-DES-03`) es, por construcción, **el vector de mayor longitud entre las cuatro
opciones en el 100% de los casos** — nunca compite por ese lugar, lo gana siempre.

**Evidencia (medición)**: enumeración exhaustiva de las 37/37 combinaciones válidas de
`(distancia_total, distancia_avanzada, angulo_direccion)` permitidas por los filtros del
`data_generation` y muestreo aleatorio de 200/200 corridas — en el 100% de los casos
`GEO-DES-03` ocupa el rank 1 de longitud (el vector más largo). La respuesta correcta **nunca**
ocupa el rank 1: ocupa el rank 2 en 168/200 casos y el rank 3 en 32/200 casos (según cuál de
`distancia_restante` / `distancia_avanzada` sea mayor).

**Por qué es un defecto pedagógico real (no cosmético)**: un estudiante puede aplicar el atajo
"la opción más larga nunca es la correcta" y descartar `GEO-DES-03` **sin razonar sobre
distancia ni dirección**, sin siquiera leer el enunciado con atención. Esto reduce el poder
discriminante del ítem — el estudiante que adivina por el atajo se ve favorecido frente al que
razona, lo opuesto del propósito metacognitivo del ejercicio. Viola
`../../../.claude/rules/diversidad-sustantiva.md` §P5 (distractor identificable como outlier
por un rasgo saliente en vez de por análisis).

**Fix recomendado**:

1. Ampliar el pool de errores conceptuales de 3 a **5** y elegir **3 por versión** (con
   `sample()`), de modo que `GEO-DES-03` no aparezca en todas las versiones y, cuando aparezca,
   no siempre sea el único competidor por el rank 1.
2. **Desacoplar `escala_px_km` de un distractor concreto**: en vez de derivarla de
   `distancia_total + distancia_avanzada` (que coincide exactamente con el valor de un
   distractor), derivarla del **máximo efectivamente dibujado** entre las opciones seleccionadas
   para esa versión (`escala_px_km <- 120 / max(valores_dibujados)`), de modo que ningún
   distractor concreto quede "pre-asignado" al valor 120 px por diseño.
3. **Advertencia de diseño** (a tener en cuenta al ampliar el pool): casi cualquier error de
   "resta incompleta" o "suma en vez de resta" produce, por su propia naturaleza aritmética, un
   valor `>= distancia_restante`. Si los 2 errores nuevos del pool ampliado son variantes de este
   mismo patrón (todas suman o combinan de más), el sesgo "los distractores son siempre más
   grandes que la correcta" se reproduce aunque la longitud exacta de 120 px deje de ser fija. El
   pool ampliado debe incluir **al menos un error que no dependa de una resta incompleta** —
   p. ej. variantes de dirección que conserven la magnitud correcta (análogas a `GEO-DES-01`,
   que ya cumple esta propiedad) — para que la longitud de los distractores no esté sesgada
   sistemáticamente hacia "más grande que la correcta".

**Criterio de aceptación** (verificable, sin calificadores subjetivos): sobre **≥40 versiones**
generadas con semillas distintas,

- la respuesta correcta alcanza el **rank 1 de longitud** (el vector más largo de las 4
  opciones) en **al menos una fracción no trivial** de los casos (es decir, no en 0/40 como
  ocurre hoy), y
- **ningún distractor** ocupa el rank extremo (más largo o más corto) en el **100%** de las
  versiones.

Script de verificación sugerido: extender
`Rscript ../../../.claude/scripts/validar_diversidad_sustantiva.R` o un script ad-hoc que, para
cada semilla, calcule las 4 longitudes en píxeles y registre el rank de la opción marcada como
correcta y el rank de cada código de error.

---

## P1 — Deuda de desarrollo (no bloqueante, pero necesaria antes de escalar el patrón)

### P1.1 — Modularizar helpers del `.Rmd` a `SP/R/` (OE6) — **BLOQUEADO por incompatibilidad de herramienta**

**Estado: BLOQUEADO (2026-07-28).** No es un pendiente simple de "hacer cuando haya tiempo": se
**intentó**, se **midió** el resultado, y el resultado deshabilita el validador obligatorio de la
regla #22. No reintentar sin resolver primero el criterio de desbloqueo de abajo.

**Qué se intentó**: extraer `dibujar_diagrama()`/`km()`/`.cols` del chunk `data_generation` a
`SP/R/helpers_diagramas.R`, cargándolo con el mecanismo **oficial** de R/exams para archivos
suplementarios (ver `../../../.claude/docs/AUTOCONTENCION_REXAMS.md`):

```r
include_supplement("helpers_diagramas.R", dir = "R")
source("helpers_diagramas.R")
```

**Resultado**: los 5 formatos (HTML/PDF/DOCX/NOPS/Moodle) renderizaron correctamente con este
patrón. **Pero** `../../../.claude/scripts/validar_diversidad_sustantiva.R --n 40` falló en
**40/40 semillas** con `WARN_DIV_INDET` (ninguna evaluación del `data_generation` tuvo éxito).

**Causa raíz** (verificada leyendo el script, líneas 100-109):

```r
tmp <- file.path(tempdir(), paste0("divsust_", Sys.getpid())); dir.create(tmp, ...)
old <- getwd(); on.exit({ setwd(old); unlink(tmp, recursive = TRUE) }, add = TRUE)
for (i in seq_len(n)) {
  setwd(tmp)
  env <- new.env(parent = globalenv())
  ...
  okr <- tryCatch({ suppressWarnings(suppressMessages(eval(expr, envir = env))); TRUE },
                   error = function(e) FALSE)
  setwd(old)
  ...
```

El validador crea un directorio temporal, hace `setwd(tmp)` y evalúa el chunk `data_generation`
**aislado** en un `new.env()`, **fuera** del pipeline de `xexams()`. En ese contexto,
`include_supplement()` no tiene el estado interno que necesita
(`.exams_get_internal("xexams_dir_exercises")`, ver `AUTOCONTENCION_REXAMS.md`) y falla —
arrastrando todo el chunk a error, de ahí el 40/40.

Se probó un fallback:

```r
if (file.exists("R/helpers_diagramas.R")) {
  source("R/helpers_diagramas.R")
} else {
  include_supplement("helpers_diagramas.R", dir = "R")
  source("helpers_diagramas.R")
}
```

**Tampoco funciona**: el validador ya hizo `setwd(tmp)` (línea 104 del script) antes de evaluar el
chunk, así que la ruta relativa `"R/helpers_diagramas.R"` se resuelve contra `tmp` (vacío), no
contra el directorio del ejercicio — `file.exists()` da `FALSE` y cae al `include_supplement()`,
que falla igual.

**Decisión**: se **revirtió** la modularización. El `.Rmd` vuelve a ser auto-contenido —
`dibujar_diagrama()`/`km()`/`.cols` están de nuevo dentro del chunk `data_generation`, con un
comentario (`.Rmd` líneas 45-52) que explica por qué, para que un agente futuro no "limpie" esto
como código repetido y reintroduzca el bug.

**Verificación post-reversión**: `validar_diversidad_sustantiva.R --n 40` → `PASS` (40/40
versiones evaluadas, 35 valores únicos, 0 errores); 5 formatos renderizan OK; 0 fugas de nombre en
el XML de Moodle.

**El helper extraído ya existe**, no hace falta rehacerlo desde cero: está en
`SP/_archivo/propuesta-modularizacion/helpers_diagramas.R`, listo para reactivar en cuanto se
cumpla el criterio de desbloqueo.

**Criterio de desbloqueo**: adaptar `../../../.claude/scripts/validar_diversidad_sustantiva.R`
para que soporte ejercicios modularizados — p. ej. copiando también los archivos auxiliares
(`R/*.R`) al tempdir antes de evaluar, o evaluando el chunk con el `cwd` del ejercicio en vez de un
tempdir aislado. Hasta entonces, este ejercicio (y cualquier otro con opciones gráficas generadas
dinámicamente) **debe permanecer auto-contenido**.

Bloques identificados para una futura extracción (línea actual en el `.Rmd` auto-contenido,
vigente tras la reversión):

| Bloque | Líneas actuales | Helper propuesto |
|---|---|---|
| Pool de 4 cuadrantes cardinales | 29-34 | `orientaciones_cardinales()` |
| Dibujo de diagrama vectorial polar | 54-115 | `dibujar_diagrama()` |
| Wrappers `grid` (`ln`, `tx`, `pl`, `cir`) | 62-65 | `snippets_graficos_grid.R` |
| Pool de contextos narrativos | 166-319 | `contextos_narrativos_navegacion()` |
| Pool de reflexiones metacognitivas | 393-400 | `reflexiones_vectores()` |
| Batería `test_that` de SCHOICE gráfico | 404-423 | `tests_schoice_grafico()` |

**Prioridad**: se mantiene fuera de P0 (el `.Rmd` funciona correctamente auto-contenido; es deuda
de mantenibilidad, no un defecto del ejercicio), pero deja de ser un P1 "cuando haya tiempo": es un
**bloqueado** que depende de trabajo en una herramienta compartida
(`validar_diversidad_sustantiva.R`), no del propio ejercicio.

### P1.2 — Re-confirmar `validar_diversidad` y `coherencias_5` de `ejercicio_state.json`

`ejercicio_state.json` marca estos dos pasos como completados el **2026-06-27T16:18-16:20**,
**antes** de los fixes de Error 23 (`169ab8c6`, `287afc01`) y Error 24 (`dd5f10d1`) del
2026-06-28. El JSON no refleja que el `.Rmd` cambió después de esa validación.

**Criterio de aceptación**:
```bash
Rscript ../../../.claude/scripts/validar_diversidad_sustantiva.R \
  desplazamiento_avion_aeropuerto_metacognitivo_interpretacion_n3_schoice_v1.Rmd --n 40
# exit 0 esperado (PASS o WARN_DIV_BAJA, nunca ERR_DIV_COSMETICA)
```
y documentar las 5 coherencias (`../../../.claude/rules/codigo-rmd.md`) contra el `.Rmd` vigente,
actualizando `ejercicio_state.json` con un nuevo timestamp tras la re-confirmación.

**Prioridad P1**: condición previa a OE10 (promoción), pero no bloquea el uso actual del
ejercicio en `01-En-PreDesarrollo/`.

### P1.3 — Cablear el wrapper del comando `orquestador-schoice` (OE8)

`RR/.claude/agents/orquestador-schoice.md` documenta la regla #22, el Error 24 y
`ERR_DIV_COSMETICA`, pero `RR/.claude/commands/orquestador-schoice.md` (el wrapper que invoca el
usuario con `/orquestador-schoice`) no menciona ninguno de los tres. Quien invoque el comando en
vez del agente directamente no recibe esas salvaguardas.

**Criterio de aceptación**: `grep -c "regla #22\|ERR_DIV_COSMETICA\|Error 24" RR/.claude/commands/orquestador-schoice.md`
retorna ≥1 (hoy retorna 0).

**Prioridad P1**: no afecta a este ejercicio (ya generado), pero afecta a **cualquier ejercicio
futuro con opciones gráficas** que se genere invocando el comando en vez del agente.

---

## P2 — Mejoras diferibles

### P2.1 — Promoción a `02-En-Desarrollo/` (OE10)

Condicionada a resolver P0.1 y re-confirmar P1.2. Ejecutar `/promover-ejercicio` (o el paso
equivalente) una vez ambos estén cerrados. Ver [`ROADMAP.md` §3](ROADMAP.md#3-vía-a-02-en-desarrollo).

**Criterio de aceptación**: el directorio del ejercicio existe en
`A-Produccion/02-En-Desarrollo/` con `ejercicio_state.json` actualizado y sin degradar ningún
paso previamente completado.

### P2.2 — Validación Nivel 3 en aula (OE11)

Requiere aplicación real con estudiantes; no tiene fecha objetivo fijada (depende de la
programación académica). Ver [`ROADMAP.md` §4](ROADMAP.md#4-gate-de-validación-nivel-3-aula-para-03-en-produccion).

**Criterio de aceptación**: evidencia documentada de que los distractores `GEO-DES-01/02/03`
discriminan según lo esperado en una aplicación real (no simulada), requisito de
`/promover-ejercicio` para `03-En-Produccion/`.

### P2.3 — SVGs huérfanos en `_archivo/graficos-obsoletos/`

Los archivos `diagrama_[a-d].svg` usan la nomenclatura anterior (`a/b/c/d`) que ya no coincide
con los PNG vigentes (`correcta/perp/recorrida/suma`). Ya están archivados (no en la raíz del
subproyecto); este ítem es solo para decidir si se eliminan definitivamente o se conservan como
referencia histórica.

**Criterio de aceptación**: decisión explícita documentada (mantener en `_archivo/` o eliminar);
no es una acción automática.

## Referencias cruzadas

- [`../HANDOFF.md`](../HANDOFF.md) — contexto completo, decisiones, riesgos
- [`ROADMAP.md`](ROADMAP.md) — hitos y qué bloquea qué
- [`SYLLABUS.md`](SYLLABUS.md) — nota de discrepancia DOK/Nivel en `ejercicio_state.json` (P1.2)
- [`BLUEPRINT.md`](BLUEPRINT.md) — contrato de `dibujar_diagrama()` afectado por P0.1 y P1.1
- `../../../.claude/rules/diversidad-sustantiva.md` — regla #22, §P5 (base del hallazgo P0.1)
- `../../../.claude/docs/patrones-errores-conocidos.md` — Error 24 (predictibilidad posicional,
  antecedente directo del mismo tipo de sesgo que P0.1 ataca en la dimensión de longitud)
