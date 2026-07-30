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

## 3. Estado real verificado (2026-07-30)

Todo lo de esta tabla se volvió a ejecutar el 2026-07-30 sobre la versión vigente del `.Rmd` (pool de
7 + invariante I-7). No es evidencia heredada.

| Verificación | Comando | Resultado |
|---|---|---|
| Render 4 formatos | `Rscript verificar_render.R` | HTML, PDF, DOCX, NOPS: OK |
| Clave = `n!` en Moodle | idem, V5 | 12/12 preguntas |
| Unicidad y magnitud | idem, V6 | **105/105 ternas** (enumeración exhaustiva del espacio), razón máx/clave 1,0×–10,8× (umbral 15×) |
| Cierre de H1 — espacio | idem, V6 | espacio legal **93/105**; rango de la clave **1/2/3, nunca 4.º**; mitad baja **41,9 %**; «elegir el mayor» **0,0 %**; clave/mayor distractor ≤ **0,50×** |
| Cierre de H1 — selección | idem, **V9** (nuevo) | **240/240** versiones: toda terna con ≥1 distractor > `n!`; 89 ternas distintas alcanzadas |
| Instancia canónica verbatim | idem, V7 | contexto 1 con n=4 == `MAT-2026-1-004` |
| Invariantes I-1..I-7 | `Rscript tests/run_one_suite.R tests/testthat/test_permutaciones_invariantes.R` | **0 fail / 0 error / 0 skip** |
| Coherencia matemática | `validar_coherencia_matematica.R` | **APROBADO, 0 errores** (Capas A/B/C + Nivel 5A-5E). El Nivel 5B **se ejecuta de verdad** desde el code-review: antes retornaba temprano por nombres de variable y su «OK» era vacuo (probado por mutación) |
| Diversidad sustantiva | `validar_diversidad_sustantiva.R --n 40` | exit 0 · 3 claves · `WARN_DIV_BAJA` (esperado, ver §5.2) |
| Diversidad de render | 300 evaluaciones del `data_generation` | **298/300 versiones únicas** · 89 de 93 ternas legales · 16 instancias canónicas · reparto de `n` 105/95/100 |
| Ortografía | `corregir_ortografia_espanol.R` | sin errores |
| Letter-independence (#19) | grep de `letra_correcta` / «Opción [A-D]» | 0 coincidencias |
| Prueba de mutación de la clave | ver regla local | I-5 aborta la generación; desactivada, V5 detecta 8/8 |

`ejercicio_state.json`: **10/11 pasos**. Falta solo `aprobacion_usuario`.

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
   distintos, 8 refutados). Aplicados 10 de 10 (el último, el 2026-07-30); ver §5.3.

## 4 bis. Qué se hizo el 2026-07-30

1. Se **confirmó la decisión D4** con el usuario: el hallazgo H1 estaba marcado en el BACKLOG como
   «decisión pendiente del usuario» y una sesión posterior al HANDOFF v1.0 ya había ejecutado una de
   las dos salidas (ampliar el pool) sin que constara la autorización. Autorizada.
2. Se **midió el barrido de configuraciones** que el `.Rmd` citaba como «BLUEPRINT §3.1» — sección
   que no existía. Se documentó con 6 configuraciones reproducibles, extrayendo las fórmulas del
   `.Rmd` real. Hallazgo del propio barrido: «pool de 5 + I-7» **no era viable** (deja el rango de la
   clave fijo en 3.º, patrón posicional puro que V6 rechaza). Ver [`docs/BLUEPRINT.md`](docs/BLUEPRINT.md) §3.1.
3. Se **corrigió una cifra no verificable**: el `.Rmd` y el test decían «barrido de 9
   configuraciones»; no se pudo confirmar y se ajustó a las 6 realmente medidas.
4. Se **re-ejecutó el arsenal completo** sobre la versión vigente (la verificación anterior era de las
   20:29 y el `.Rmd` se había editado a las 20:41 — la evidencia no cubría el código actual).
5. Se **re-midió la diversidad** con el pool de 7: 297 → **298/300** versiones únicas.
6. Se **sincronizó toda la documentación** con el código. Los 8 documentos seguían describiendo pool
   de 5, invariantes I-1..I-6, 30 ternas, `V1-V8` y H1 abierto. Incluye resolver una **colisión de
   numeración**: el código usa `I-7` para «la clave nunca es la mayor», mientras el BLUEPRINT usaba
   `I-7` para «auto-contenido» — las invariantes meta se corrieron a I-8/I-9/I-10.
7. Se **corrigió `copias <- 100` → `300`** en `SemilleroMoodle_v2.R` (defecto #8 del code-review,
   replicado al copiar el script del hermano) y se documentó el porqué en el propio script.
8. Se cerraron en el BACKLOG **H1** y **P2.1** (scripts de exportación institucional, ya presentes).

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
verde estando vacías**. Tres de los diez defectos eran verificadores que no verificaban. El décimo
(H1) quedó pendiente de decisión humana y se cerró el 2026-07-30 — ver §5.5.

| # | Defecto | Estado |
|---|---|---|
| 1 | I-3 es unilateral: la clave nunca queda entre las 2 opciones menores (50 % de adivinanza) | ✅ **aplicado el 2026-07-30** — decisión D4 autorizada: pool 5→7 + invariante I-7. Ver §5.5 |
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

- **Falso positivo del corrector ortográfico** (BAJA): `--fix` convierte `codigo-rmd.md` en
  `código-rmd.md` dentro de comentarios de R. Mitigado escribiendo la referencia como ruta completa
  (particularidad 10). Falta reportarlo al mantenedor del script del repo raíz — es una herramienta
  compartida, no un defecto de este ejercicio. Ver [`docs/BACKLOG.md`](docs/BACKLOG.md) P1.1.

No hay ningún otro hallazgo abierto. Los dos que estaban en esta sección se cerraron el 2026-07-30:
**H1** (§5.5) y los **scripts de exportación institucional**, que ya existen y están verificados por
ejecución ([`docs/BACKLOG.md`](docs/BACKLOG.md) P2.1).

---

### 5.5 Cierre del hallazgo H1 — decisión D4 (2026-07-30)

**H1 era el único defecto pedagógico vivo** y el único punto del proyecto que un agente no podía
cerrar solo: el BACKLOG lo declaraba «decisión pendiente del usuario». Autorizado el 2026-07-30.

**El defecto.** Con el pool de 5, cuatro de las cinco fórmulas eran menores que `n!`, así que la clave
quedaba en 3.º lugar el 60 % de las veces y era **la mayor** el 40 % restante — nunca en la mitad
baja. Descartar las dos opciones menores sin saber combinatoria subía la adivinanza del 25 % al
**50 %**, y «elegir el número mayor» acertaba 2 de cada 5 versiones. `I-3` no lo veía por ser
unilateral: cuando la clave es el máximo su ratio vale 1,0× y la guarda pasa trivialmente.

**El fix, en dos mitades que no funcionan solas** (barrido en [`docs/BLUEPRINT.md`](docs/BLUEPRINT.md) §3.1):

1. `EST-PER-06` (`(n+1)!`) y `EST-PER-07` (`2·n!`), errores conceptuales diagnósticos por derecho
   propio y ambos mayores que `n!`.
2. La invariante **I-7**: toda terna lleva al menos un distractor mayor que `n!`. Sin ella el pool de
   7 aún deja un 11,4 % de ternas con la clave como máxima.

| métrica | antes | después |
|---|---:|---:|
| rango de la clave | 3.º / 4.º | **1.º / 2.º / 3.º** |
| mitad baja | 0,0 % | **41,9 %** |
| «elegir el mayor» acierta | 40,0 % | **0,0 %** |
| clave / mayor distractor | hasta 20,00× | ≤ **0,50×** |
| versiones únicas (300) | 297 | **298** |

**Las cuatro objeciones que lo bloqueaban, resueltas.** Dos eran reales (cambia el contenido evaluado
→ requería autorización; obliga a re-medir → hecho, 105/105 + V9 + suite + arsenal). Las otras dos
**no se sostuvieron al medirlas**: no colisiona con OE1, porque los tres errores oficiales incluyen
`EST-PER-01` (`64 > 24`) y la terna canónica cumple I-7 por sí sola —`MAT-2026-1-004` se sigue
reproduciendo verbatim con su clave en 3.º—; y no roza D2, porque las fórmulas nuevas valen 7,0× y
2,0× en el peor `n`, por debajo del 10,8× que ya aportaba `EST-PER-01`. La asimetría resultante es
deliberada: **fidelidad en la instancia canónica, mitigación en las variantes**.

**Guardas de no-regresión (fallan, no avisan):** V6 re-mide las tres cifras en cada corrida; V9
comprueba la selección real sobre 240 semillas; el test fija `expect_gte(length(pool), 7L)`.
Documentado como particularidad **13** en [`.claude/CLAUDE.md`](.claude/CLAUDE.md), invariante **I-7**
en [`.claude/rules/permutaciones-parametricas.md`](.claude/rules/permutaciones-parametricas.md) y
decisión **D4** en [`docs/BLUEPRINT.md`](docs/BLUEPRINT.md) §4.9.

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
| Un agente "simplifica" el pool a 5 o borra el filtro `legales` | Particularidad 13 + `stopifnot` de I-7 + V6/V9 + `expect_gte(pool, 7L)`: **reintroduce H1** |
| Un agente cambia la enumeración del espacio legal por un `repeat` | Particularidad 13 (Error 22, regla #21 Familia 1: cuelga el render) |
| Un agente lee el HANDOFF viejo y "corrige" el código hacia pool de 5 | Toda la documentación quedó sincronizada el 2026-07-30; este riesgo se materializó una vez y es la razón de §4 bis punto 6 |

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

- `verif_render/` — HTML, PDF, DOCX, NOPS y el XML de Moodle de la última corrida (verificación).
- `salida/` — la exportación **institucional** con membrete (PDF, DOCX, HTML interactivo, NOPS y XML
  de Moodle). Ojo: ese banco se generó con `copias <- 100`; tras la corrección a 300, regenéralo con
  `Rscript SemilleroMoodle_v2.R` si vas a importarlo a Moodle.
- La instancia canónica (contexto 1 con `n=4`) es la que hay que comparar contra el ítem oficial
  `MAT-2026-1-004`: debe coincidir verbatim, enunciado y las cuatro opciones `{24, 64, 16, 4}`.

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

**Versión**: 2.0 (estado re-verificado el 2026-07-30; H1 cerrado con la decisión D4; documentación
sincronizada con el código)
**Fecha**: 2026-07-30
