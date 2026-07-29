# HANDOFF — `permutaciones-pescadores-venia-n4`

| Campo | Valor |
|---|---|
| **Ruta** | `A-Produccion/01-En-PreDesarrollo/permutaciones-pescadores-venia-n4/` |
| **Repo raíz** | `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams` |
| **Ejercicio** | `permutaciones_pescadores_metacognitivo_formulacion_n4_schoice_v1.Rmd` |
| **Tipo** | SCHOICE metacognitivo · opciones de TEXTO · sin figura (Flujo B = false) |
| **Origen ICFES** | `MAT-2026-1-004` (ERA-2026, Sesión 1, pregunta impresa 4) |
| **Sesión fundacional** | 2026-07-29 |
| **Frase de reanudación** | `Continúa con el proyecto A-Produccion/01-En-PreDesarrollo/permutaciones-pescadores-venia-n4` |

> Al retomar: lee **este archivo** y `ejercicio_state.json` ANTES de explorar el `.Rmd`, y después
> [`.claude/CLAUDE.md`](.claude/CLAUDE.md) (12 particularidades operativas) y
> [`.claude/rules/permutaciones-parametricas.md`](.claude/rules/permutaciones-parametricas.md)
> (contrato de las 6 invariantes). Ahí está el porqué del código; sin eso, un agente "arregla" fixes
> deliberados.

---

## 1. Objetivo general

Producir y mantener un **ejercicio ICFES SCHOICE metacognitivo de Nivel 4** (competencia
*Formulación y ejecución*, componente *Aleatorio*, descriptor **D4.8**) sobre el conteo de
**permutaciones lineales** de `n` elementos distintos, derivado del ítem real `MAT-2026-1-004`.

El ítem **no evalúa la aritmética del factorial**: evalúa si el estudiante reconoce que al ocupar
posiciones sucesivas el conjunto disponible **decrece**, frente a las estrategias que lo tratan
como constante. Los distractores son errores conceptuales documentados, no ruido numérico.

**No confundir con los hermanos.** `plano-cartesiano-barco-n2` comparte el patrón (opciones de
texto, `exshuffle: TRUE`, ítem verbatim) pero es N2 geométrico.
`desplazamiento-avion-aeropuerto` usa opciones **gráficas** y `exshuffle: FALSE`: su patrón **no**
es intercambiable con éste.

---

## 2. Objetivos específicos

Tabla completa con evidencia en [`docs/ROADMAP.md`](docs/ROADMAP.md) §2. Resumen al 2026-07-29:
**OE1-OE9 cumplidos y verificados**; **OE10** (promoción a `02-En-Desarrollo/`) y **OE11**
(evidencia Nivel 3 en aula) pendientes.

Los OE se declararon en esta misma sesión y se persistieron en
`~/.claude/projects/<slug>/memory/project_objetivos_permutaciones_pescadores_venia_n4.md`.

---

## 3. Estado real verificado (2026-07-29)

| Verificación | Comando | Resultado |
|---|---|---|
| Render 4 formatos | `Rscript verificar_render.R` | HTML, PDF, DOCX, NOPS: OK |
| Clave = `n!` en Moodle | idem, V5 | 12/12 preguntas |
| Unicidad y magnitud | idem, V6 | **30/30 ternas** (enumeración exhaustiva), razón máx/clave 1,0×–10,8× |
| Instancia canónica verbatim | idem, V7 | contexto 1 con n=4 == `MAT-2026-1-004` |
| Coherencia matemática | `validar_coherencia_matematica.R` | **APROBADO, 0 errores** (Capas A/B/C + Nivel 5A-5E). Desde el code-review el Nivel 5B **se ejecuta de verdad**: antes retornaba temprano por nombres de variable y su «OK» era vacuo (probado por mutación) |
| Diversidad sustantiva | `validar_diversidad_sustantiva.R --n 40` | exit 0 · 3 claves · `WARN_DIV_BAJA` (esperado, ver §5.2) |
| Diversidad de render | 300 evaluaciones del `data_generation` | **297/300 versiones únicas** · 10/10 ternas de error · 16 instancias canónicas |
| Ortografía | `corregir_ortografia_espanol.R` | sin errores |
| Letter-independence (#19) | grep de `letra_correcta` / «Opción [A-D]» | 0 coincidencias |
| Prueba de mutación de la clave | ver regla local | I-5 aborta la generación; desactivada, V5 detecta 8/8 |

`ejercicio_state.json`: **10/11 pasos**. Falta solo `aprobacion_usuario`.

---

## 4. Qué se hizo en esta sesión (2026-07-29)

1. Se adoptó la clasificación oficial de la ficha `MAT-2026-1-004` sin re-derivarla, y se verificó
   la paridad literal de Afirmación, Evidencia (`FyE_E3`), Descriptor (`D4.8`) y Estándar EBC
   contra los catálogos canónicos de Todo-Pajaro.
2. Se midió el rango legal de `n` por enumeración antes de fijarlo (decisión D2).
3. Se escribió el `.Rmd` con 6 contextos narrativos (el primero canónico y verbatim) y pool de
   errores derivado de las Justificaciones MetaCognitivas oficiales.
4. Se leyó el HTML renderizado —no solo el veredicto de los validadores— y eso destapó tres
   defectos invisibles para el arsenal: un `\times` fuera de modo matemático que se evaporaba, y
   texto sin tildes visible al estudiante.
5. Se construyó `verificar_render.R` (V1-V8) y se validó por mutación que V5 detecta una clave
   falsa.
6. Auditoría adversarial con **dos agentes independientes**. Veredicto consolidado:
   *APROBAR CON CAMBIOS*. Se aplicaron los cambios (§5.1) y se re-validó todo.
7. Se sincronizó `ejercicio_state.json` (pasos 3-10) y se escribió toda la documentación.
8. **Code-review de alta intensidad sobre la rama** (36 candidatos → 28 confirmados → 10 defectos
   distintos, 8 refutados). Aplicados 9 de 10; ver §5.3.

---

## 5. Hallazgos y decisiones

### 5.1 Cambios exigidos por la auditoría — aplicados

- **Pool de errores 3 → 5, se eligen 3** (hallazgo ALTA). La regla #1 exige «Mínimo 4-6 errores por
  ejercicio»; con pool == slots el **tipo** de error nunca variaba. Se añadieron `EST-PER-04`
  (permutación circular `(n-1)!`) y `EST-PER-05` (principio aditivo `n(n+1)/2`). Medición posterior:
  10/10 ternas alcanzadas, versiones únicas 280 → **297**/300.
  - *Matiz honesto*: el adversario afirmó que el 100 % de los ejemplos comparables del codebase usa
    pool > slots con `sample()`. Al verificarlo encontré un ejercicio **ya promovido** con pool=3 y
    sin `sample()`. El cambio se hizo por el texto explícito de la regla, no por unanimidad del
    codebase.
- **`EST-PER-01` renombrado y su prosa reescrita** (consenso de ambos adversarios). Ver
  [`.claude/CLAUDE.md`](.claude/CLAUDE.md) particularidad 6.
- **Nueva decisión D3 y nueva invariante I-6**: la instancia canónica fuerza los tres errores
  oficiales, de modo que reproduce el ítem ICFES completo (enunciado + las cuatro opciones).

### 5.2 Decisiones cerradas — no reabrir sin autorización

| Decisión | Contenido |
|---|---|
| **Flujo B = false** | El ítem no tiene figura ni en el JPG ni en la ficha. Motor Hermes: N/A |
| **DOK 3 / Bloom Evaluar** | El Nivel 4 es canónico e intocable; el DOK 3 se justifica por la carga metacognitiva de la Solution, no por la aritmética. Resuelve la incompatibilidad DOK2↔N4 de la tabla del repo |
| **`n ∈ {4,5,6}`** | Medido, no elegido. Ver particularidad 3 |
| **`WARN_DIV_BAJA` se acepta** | Estructural: solo hay 3 claves legales. Ampliar el rango violaría la particularidad 3. `ERR_DIV_COSMETICA` sí sería fallo y no ocurre |
| **Enunciado verbatim en la instancia canónica** | La metacognición vive en la Solution, no reescribiendo el enunciado |

### 5.3 Code-review de alta intensidad (2026-07-29) — 9 de 10 aplicados

El patrón dominante no fue «salida rota» sino **capas de verificación que se citaban como evidencia
verde estando vacías**. Tres de los diez defectos eran verificadores que no verificaban.

| # | Defecto | Estado |
|---|---|---|
| 1 | I-3 es unilateral: la clave nunca queda entre las 2 opciones menores (50 % de adivinanza) | ⏸️ **NO aplicado** — escalado a H1, requiere decisión pedagógica humana (§5.4) |
| 2 | La suite de invariantes fijaba la ruta `01-En-PreDesarrollo/`: al promover, fallaría y **bloquearía todo push** del repo, apagando además la guarda I-5 vía `skip_if_not` | ✅ localización por nombre |
| 3 | El Nivel 5B del validador genérico **nunca se ejecutaba** (buscaba `valor_correcto`/`opciones_valores`/`error_sel`; el chunk usaba otros nombres) → APROBADO vacuo | ✅ tres alias + `stopifnot` de alineación; probado por mutación |
| 4 | Ediciones sin commitear en el árbol **inmutable** `03-En-Produccion/` (YAML reordenado + 5 PNG git-lfs) | ✅ revertido a `stash@{1}` (recuperable) |
| 5 | V6 decía «enumeración EXHAUSTIVA» sobre una **copia hardcoded** del pool, no el del `.Rmd` | ✅ extrae pool y `N_POOL` del `.Rmd` |
| 6 | V5 imprimía `revisadas/revisadas` (tautológico): descartar versiones en silencio se veía verde | ✅ compara contra `N_VERSIONES`; probado por mutación (`7/12`) |
| 7 | V7 hacía `eval(parse(...))` sin `try()`: un encabezado de chunk renombrado abortaba el script y se perdía todo el reporte V1-V8 | ✅ extracción validada + `try()` |
| 8 | `copias <- 300` → `100` sin comentario en el `SemilleroMoodle_v2.R` del **hermano del avión**: banco Moodle por debajo del estándar de ≥200 | ✅ revertido a `stash@{0}` |
| 9 | `repo_root` con `system(intern=TRUE)` sin `ignore.stderr` ni fallback → `RMD` literal `"NA/A-Produccion/..."` | ✅ versión endurecida de la suite hermana |
| 10 | ~93 números de línea del `.Rmd` en los docs, varios ya erróneos (I-3 citado en «línea 142», estaba en la 291) | ✅ 83 convertidos a anclas; §6 del BLUEPRINT reindexada por construcción |

**Cierre**: `verificar_render.R` V1-V8 todo verde · `test_permutaciones_invariantes.R` 0 fail / 0 error
/ **0 skip** (antes 8 de 9 tests podían apagarse) · `validar_coherencia_matematica.R` APROBADO ·
`validar_diversidad_sustantiva.R --n 40` exit 0 · ortografía sin errores ·
`tests/run_all_tests.R` **22/22 suites, 0 fallidas** (660 s).

**Refutados** (8, no accionar): numeración no monotónica de los pre-flight del orquestador; la rama
`es_canonica` «salta» el filtro `aplicables` (las 5 precondiciones son incondicionales, no hay estado
alcanzable); `set.seed()` en el helper del test (es un test, no el chunk); `encoding="UTF-8"` ausente
en los `exams2*` del verificador; `errores_info` recalcula `calcula(n)`; el «Caso específico» de la
Solution colisiona con `EST-PER-01` en `n=5`; el `.gitkeep` del avión en esta rama.

---

### 5.4 Hallazgos abiertos

- **H1 — La clave nunca queda entre las 2 opciones menores** (🔴 ABIERTO, requiere decisión humana).
  Antes catalogado como BAJA «observado sin acción»; el code-review del 2026-07-29 lo midió y lo
  **reclasificó**. Medición exhaustiva de las 30 ternas: el rango de la clave por magnitud es 3.º en
  18/30 y 4.º en 12/30, **nunca 1.º ni 2.º**. Descartar las dos menores deja una adivinanza al 50 %
  (no 25 %) y «elegir el mayor» acierta en el 40 % de las versiones. El razonamiento con que se cerró
  antes era erróneo: que el rango varíe entre 3.º y 4.º **no** diluye el patrón, porque ambos están en
  la mitad alta. I-3 no lo detecta porque es unilateral: cuando la clave es el máximo, su ratio vale
  1,0×. Corregirlo exige añadir fórmulas > `n!` al pool → cambia el contenido evaluado, obliga a
  re-medir las 60 ternas resultantes y colisiona con OE1 (el propio ítem oficial pone la clave en 3.º).
  Mitigación ya aplicada: V6 falla si el rango llegara a ser un valor único, reporta
  `clave/mayor distractor` (hasta 20,0×) y emite un AVISO. Ver [`docs/BACKLOG.md`](docs/BACKLOG.md) §H1.
- **Falso positivo del corrector ortográfico** (BAJA): `--fix` convierte `codigo-rmd.md` en
  `código-rmd.md` dentro de comentarios. Mitigado escribiendo la referencia como ruta completa.
  Falta reportarlo al script del repo raíz.
- **Sin scripts de exportación institucional**: este subproyecto no tiene `SemilleroUnico_v2.R` ni
  plantillas `pcielo*.tex` como el hermano barco. Evaluar si los necesita.

---

## 6. Riesgos

| Riesgo | Mitigación cableada |
|---|---|
| Un agente "modulariza" y saca los helpers del chunk | Particularidad 1 + el propio `validar_diversidad_sustantiva.R` fallaría |
| Un agente copia `exshuffle: FALSE` del hermano del avión | Particularidad 2 |
| Un agente amplía `N_POOL` para silenciar `WARN_DIV_BAJA` | Particularidades 3 y 9 + I-1/I-3 |
| Un agente borra la rama `es_canonica` al "simplificar" | Particularidad 4 + invariante I-6 |
| Un agente "limpia" el `stopifnot` de I-5 | V5 del verificador lo atrapa (probado por mutación) |
| Se ejecuta el corrector ortográfico y rompe una referencia | Particularidad 10 |

---

## 7. Cómo retomar

```bash
cd /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams
bash .claude/scripts/workflow-state.sh status A-Produccion/01-En-PreDesarrollo/permutaciones-pescadores-venia-n4
cd A-Produccion/01-En-PreDesarrollo/permutaciones-pescadores-venia-n4
Rscript verificar_render.R
```

**Próximo paso concreto**: el ejercicio está técnicamente cerrado y a la espera de **revisión
humana**. El paso 11 (`aprobacion_usuario`) no puede sellarlo un agente. Material para revisar:
`verif_render/` contiene HTML, PDF, DOCX, NOPS y el XML de Moodle de la última corrida.

Tras la aprobación: OE10 (promoción a `02-En-Desarrollo/`) y, más adelante, OE11 (aplicación en
aula, requisito de Nivel 3 para `03-En-Produccion/`).

**Destino reservado en producción**:
`03-En-Produccion/06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/07-Probabilidad_Principios-Aditivo-Multiplicativo-Conteo/`

---

## 8. Enlaces

[`README.md`](README.md) · [`.claude/CLAUDE.md`](.claude/CLAUDE.md) ·
[`.claude/rules/permutaciones-parametricas.md`](.claude/rules/permutaciones-parametricas.md) ·
[`docs/SYLLABUS.md`](docs/SYLLABUS.md) · [`docs/ROADMAP.md`](docs/ROADMAP.md) ·
[`docs/BACKLOG.md`](docs/BACKLOG.md) · [`docs/BLUEPRINT.md`](docs/BLUEPRINT.md)

Ficha oficial del ítem: `Todo-Pajaro/Alineacion-curricular-de-items/Simulacros/Alineacion-Curricular-de-items-ERA-2026/Matematicas/Alineacion-curricular-de-items-Matematicas-ERA-2026.md` (líneas 965-996).
Ítem espejo `MAT-2026-1-029` (mismo D4.8, conteo **con** repetición): líneas 1946-1985.

---

**Versión**: 1.0
**Fecha**: 2026-07-29
