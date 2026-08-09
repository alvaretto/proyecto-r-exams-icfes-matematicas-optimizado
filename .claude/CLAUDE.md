# Sistema de Generación Automatizada de Ejercicios ICFES R/exams

## 🎯 Índice Principal

Este archivo funciona como **índice central** del sistema. Para información detallada, consulte los módulos especializados:

### 📋 Información General
- **Propósito**: Automatizar creación y validación de ejercicios ICFES tipo SCHOICE/CLOZE
- **Tecnologías**: R/exams, TikZ, Python/matplotlib, R/ggplot2
- **Formatos soportados**: HTML, PDF, DOCX, NOPS
- **Versiones por ejercicio**: 250+ únicas aleatorias

### ⛔ Reglas Críticas (OBLIGATORIAS)
@.claude/docs/REGLAS_CRITICAS.md

**Resumen de reglas fundamentales:**
1. **Ejercicios metacognitivos** con Progressive Disclosure → @.claude/rules/ejercicios-metacognitivos.md
2. **Flujo B obligatorio** cuando hay gráficos → @.claude/rules/flujo-b-obligatorio.md
3. **Proceso secuencial** TikZ→Python→R (98% fidelidad, usuario decide) → @.claude/rules/graficador-secuencial.md
4. **Gráficos como opciones individuales** (PNGs separados) → @.claude/rules/graficos-como-opciones.md
5. **5 Coherencias** a verificar siempre (Semántica, Visual-Texto, Matemática, Código, General)
6. **Validación visual iterativa** OBLIGATORIA → @.claude/rules/ciclo-validacion.md
7. **Ortografía española** con tildes → @.claude/rules/ortografia-espanol.md
8. **Testing automático** permanente → @.claude/rules/testing-obligatorio.md
9. **Detractor obligatorio** en fases de revisión → @.claude/rules/detractor-obligatorio.md
10. **Validación _neg_ opciones repetidas** → @.claude/rules/validacion-neg-opciones-repetidas.md
11. **Contextos narrativos creativos** (no mecánicos) → @.claude/rules/contextos-narrativos-creativos.md
12. **Validación semántica automática** (Nivel 4: descripción ↔ datos) → @.claude/rules/ejercicios-metacognitivos.md (sección Validación Semántica)
13. **Validación correctitud respuesta** (Nivel 5: multi-semilla + cross-check) → @.claude/rules/validacion-correctitud-respuesta.md
14. **Routing de modelos obligatorio** (Opus/Sonnet/Haiku por complejidad) → @.claude/rules/modelo-routing-obligatorio.md
15. **Stress Test Visual** (FASE 2H: renderizado masivo + análisis anomalías) → @.claude/skills/stress-test-visual/SKILL.md
16. **Workflow State Enforcement** (gate mecánico PreToolUse + estado persistente) → @.claude/rules/workflow-state-enforcement.md
17. **Infraestructura `.claude/` protegida** (backups + verificación de invariantes I-1 a I-7 antes/después de plataformas externas) → @.claude/rules/infraestructura-protegida.md
18. **Markdown-imágenes-PDF (anti `\pandocbounded`)** → @.claude/rules/markdown-imagenes-pdf.md
    Toda imagen `.png/.jpg/.svg/.pdf` emitida vía Markdown (directa o `cat()`) en `.Rmd` DEBE incluir atributo `{width=...}`. Pandoc 3.x sin width genera `\pandocbounded` no definido en LaTeX → rompe `exams2pdf()`. Coupled con regla #6 ampliada (`exshuffle: FALSE` para Solution con letra explícita). Errores 16-17 documentados.
19. **Solution letter-independence** (NUNCA `r letra_correcta` ni "Opción [A-D]" en Solution) → @.claude/rules/solution-letter-independence.md
    Defensa permanente contra Error 19. La sección Solution debe identificar opciones por contenido/código de error, NUNCA por letra/posición, porque Moodle (y otros LMS) pueden re-shufflear las opciones de forma independiente al `exshuffle` de R-exams, rompiendo coherencia letra ↔ contenido para el estudiante. Capas: hook FASE 2J + test_letter_independence.R + detractor.
20. **Markdown-tablas-pandoc (guard contador `none`)** → @.claude/rules/markdown-tablas-pandoc.md
    Defensa permanente contra Error 21. Todo `.Rmd` con tabla Markdown (`kable(format="markdown")` o `cat("| ...")`) DEBE incluir, al inicio de `Question`, el bloque raw LaTeX `` ```{=latex}\makeatletter\@ifundefined{c@none}{\newcounter{none}}{}\makeatother``` ``. pandoc ≥3.7 (RStudio bundlea 3.8.3, distinto del 3.6 de terminal) envuelve `longtable` con `\def\LTcaptype{none}`, contador que la plantilla de R-exams no define → `exams2pdf/exams2nops` fallan con `No counter 'none' defined`. Gemelo del Error 16. Capas: generación (skills + orquestador) + hook FASE 2K (`ERR_TABLA_NONE`) + test_markdown_tablas_none_guard.R + validación con pandoc de RStudio.
21. **Familias de Soluciones Reutilizables** → @.claude/rules/familias-soluciones-rmd.md
    Índice operativo de patrones probados + librería de helpers `@.claude/scripts/snippets_familias_rmd.R`. Aplicar las familias relevantes en toda generación/corrección: **F1** generación sin cuelgue (`pick_int`/`construir_valores_con_rango`, nunca `repeat` sin cota — Error 22); **F2** tablas responsivas cross-formato (`tabla_responsiva`, fenced div `::: {style=overflow-x:auto}` que sobrevive DOCX como `<w:tbl>` y PDF como longtable); **F3** ecuaciones display responsivas (`eq_display`); **F4** coherencia de marcas en CLOZE (sol alineado por construcción + verificación marca-vs-verdad); **F5** trampa `sample(escalar)` (`pick_int`/`safe_sample`); **F6** opciones gráficas de diagramas vectoriales cardinales (`dibujar_diagrama_cardinal`/`orientaciones_cardinales`/`seleccionar_combinacion_con_cascada`/`renombrar_opciones_neutral`: orientación sorteada por versión, cascada de umbrales de legibilidad en vez de umbral único con `stopifnot`, renombrado neutral POST-mezcla y distractores que conserven la magnitud de la correcta — Errores 23-26). Test: test_data_generation_no_hang.R.
22. **Diversidad Sustantiva** (respuesta correcta debe variar entre versiones, no solo el envoltorio narrativo) → @.claude/rules/diversidad-sustantiva.md
    Defensa contra diversidad cosmética. Un conteo alto de "versiones únicas del render" NO garantiza que los datos numéricos / contenido gráfico de la respuesta correcta cambien entre semillas. Prohibido: parámetros hardcoded como literales, PNGs de opciones copiados con `file.copy`. Defensa: hook FASE 2N (`WARN_DIV_ESTATICA`) + script `validar_diversidad_sustantiva.R --n 40` en orquestador paso 9 (`ERR_DIV_COSMETICA` bloqueante) + test_diversidad_sustantiva.R. Incidente: `desplazamiento-avion-aeropuerto` (2026-06-27) — 288/300 versiones únicas con respuesta correcta invariante.

### 🛠️ Comandos y Skills
@.claude/docs/COMANDOS_Y_SKILLS.md

**Comandos principales:**
- `/analizar-icfes`, `/generar-schoice`, `/generar-cloze`
- `/orquestador-schoice`, `/orquestador-cloze` - Pipeline end-to-end (11 pasos, 3 pausas humanas) 🆕
- `/revisar-schoice`, `/revisar-cloze` - Revisión completa pasos 4-11 del workflow
- `/skill-retroalimentacion` - Generación científica de sección Solution
- `/validar-pedagogico` - Análisis pedagógico avanzado basado en evidencias
- `/detractor auditoria [target]` - Revisión adversarial en 8 dominios
- `/auto-refinar-grafico [tikz|python|r]`
- `/estado-graficador`, `/exportar-graficos`, `/promover-ejercicio`

### 🔧 Sistema de Hooks y Testing
@.claude/docs/HOOKS_Y_TESTING.md

**Sistema automático permanente:**
- 2 hooks activos (PreToolUse: gate .Rmd + recordatorio tildes; PostToolUse: arsenal post-render)
- Gate mecánico: `pre-write-rmd-gate.sh` bloquea .Rmd sin `ejercicio_state.json`
- 100% cobertura de tests (12 suites, 130+ tests)
- CI/CD con GitHub Actions
- Tolerancia cero a regresiones

### 📁 Estructura del Repositorio
@.claude/docs/ESTRUCTURA_REPOSITORIO.md

**Directorios principales:**
```
A-Produccion/
├── 01-En-PreDesarrollo/         # Experimentación
├── 02-En-Desarrollo/            # En proceso
├── 03-En-Produccion/            # Validados (por categoría ICFES)
└── Ejemplos-Funcionales-Rmd/    # FUENTE DE VERDAD

.claude/
├── rules/                       # Reglas obligatorias
├── docs/                        # Documentación modular
├── hooks/                       # Hooks de validación
├── scripts/                     # Scripts de validación
├── skills/                      # Agent Skills
└── commands/                    # Slash Commands
```

### 📚 Documentación Técnica Detallada

#### Workflows y Validación
- @.claude/docs/WORKFLOW_PASO_A_PASO.md
- @.claude/docs/TRES_NIVELES_VALIDACION.md
- @.claude/docs/FLUJO_AUTOMATICO_TESTING.md
- @.claude/docs/TROUBLESHOOTING.md

#### Testing y Calidad
- @.claude/docs/ECOSISTEMA_TESTING.md
- @.claude/rules/testing-obligatorio.md
- @.claude/rules/documentacion-verificada.md

#### Código y Desarrollo
- @.claude/rules/codigo-rmd.md
- @.claude/docs/NOMENCLATURA_ARCHIVOS_RMD.md
- @.claude/docs/MEJORES_PRACTICAS_PYTHON_RETICULATE.md
- @.claude/docs/patrones-errores-conocidos.md

#### Casos Resueltos
- @.claude/docs/casos-resueltos/

### 🔗 Referencias Rápidas

| Necesito... | Ver documento |
|-------------|---------------|
| Iniciar desarrollo de ejercicio | @.claude/docs/WORKFLOW_PASO_A_PASO.md |
| Resolver error conocido | @.claude/docs/patrones-errores-conocidos.md |
| Entender hooks de testing | @.claude/docs/HOOKS_Y_TESTING.md |
| Configurar gráficos | @.claude/docs/REGLAS_CRITICAS.md + Flujo B |
| Gráficos como opciones SCHOICE | @.claude/rules/graficos-como-opciones.md |
| Workflow Graficador (98% + 3 lenguajes) | @.claude/rules/graficador-secuencial.md |
| Generar retroalimentación científica | @.claude/skills/skill-retroalimentacion/SKILL.md |
| Ver comandos disponibles | @.claude/docs/COMANDOS_Y_SKILLS.md |
| Validar ortografía | @.claude/rules/ortografia-espanol.md |
| Ejecutar tests | `tests/run_all_tests.R` |
| Revisar decisiones/código | @.claude/rules/detractor-obligatorio.md |
| Routing de modelos (Opus/Sonnet/Haiku) | @.claude/rules/modelo-routing-obligatorio.md |
| Stress test visual multi-semilla | @.claude/skills/stress-test-visual/SKILL.md |
| Revisar ejercicio SCHOICE existente | @.claude/skills/revisar-schoice/SKILL.md |
| Revisar ejercicio CLOZE existente | @.claude/skills/revisar-cloze/SKILL.md |
| Pipeline end-to-end SCHOICE (11 pasos) | @.claude/agents/orquestador-schoice.md + `/orquestador-schoice` |
| Pipeline end-to-end CLOZE (11 pasos) | @.claude/agents/orquestador-cloze.md + `/orquestador-cloze` |

### ⚙️ Configuración del Sistema

- **Settings Claude**: @.claude/settings.json
- **CI/CD**: @.github/workflows/ci-testing.yml
- **Tests**: `tests/testthat/` (24 suites enganchadas a `tests/run_all_tests.R`)
- **Hooks**: `.claude/hooks/` (2 scripts activos cargados por settings.json)

---

## 📌 Metainformación

**Versión**: 3.20.1 (segunda pasada del mega-prompt sobre ambos orquestadores: exigencias que no tenían dónde ejecutarse ni dónde declararse)
**Fecha**: 2026-08-08
**Basado en**: Documentación oficial Claude Code (nov 2025)

### Cambios v3.20.1 (2026-08-08)

> Segunda pasada completa de `mega-prompt-endurecimiento-orquestadores.md` sobre cada gemelo, ahora
> con las 9 fases y las 8 puertas. Los 7 hallazgos son del **mismo tipo**: exigencias documentadas
> en un pre-flight o un incidente que luego **no aparecen en la checklist que se ejecuta**, o que no
> tienen **ningún campo donde declararse** en el contrato de salida. No son reglas nuevas: son
> reglas que existían sin punto de cumplimiento.

- **`orquestador-schoice` — la validación realista no comprobaba tres cosas que el propio archivo
  exige**: (a) el guard del contador `none` (Incidente E, regla #20), que es un fallo que **solo se
  manifiesta en el entorno del usuario** — RStudio bundlea pandoc ≥3.7 y la terminal 3.6 —, así que
  si no se comprueba ahí no se comprueba en ninguna parte; (b) el barrido de U+2212 al reutilizar un
  pool ajeno (Incidente O); (c) la fuga por nombre de archivo en el XML de Moodle (§P6 / Error 25).
  Los tres estaban en el gemelo CLOZE y aquí faltaban.
- **`orquestador-cloze` — el punto 8 de su validación realista ya ejecutaba `exams2moodle()`** para
  comprobar que ningún gap contiene imágenes, es decir, **tenía el XML abierto delante**, y no le
  hacía el `grep` de nombres que su propio pre-flight 15 exige. Nuevo paso **8b**, que reutiliza ese
  XML sin re-exportar.
- **Contratos de salida — exigencias sin campo donde declararse**: SCHOICE gana `graficas_opcion`,
  `formato_equilibrado` y `fuga_nombre_moodle`; CLOZE gana `fuga_nombre_moodle`. El pre-flight pedía
  verificaciones cuyo resultado no se reportaba en ningún sitio.
- **Paso 5 del SCHOICE**: añade `exams2moodle` cuando hay opciones gráficas — es el **único** canal
  que expone el nombre de archivo (HTML y PDF embeben la imagen).
- **La literalidad ICFES queda marcada como JUICIO HUMANO en ambos**: ningún script del arsenal
  compara los `exextra[…]` contra los JSON del catálogo canónico. Con V1-V9 automatizadas alrededor,
  era fácil suponer que un «paso 10 OK» certificaba también el descriptor, y no lo hace.
- **Dos rangos `FASES 2A-2J` blindados como dato histórico, no «corregidos»**: en mayo de 2026 ese
  era el arsenal completo. Reescribirlos a 2N habría falseado la crónica del incidente; llevan ahora
  la aclaración de la fecha.
- **Referencias falsas introducidas y corregidas en la misma pasada**: «pre-flight 15b» y «21c»
  apuntaban a checks inexistentes, porque ambos archivos tienen checks **numerados** `12b`, `12c` y
  `16b`. Son apartados `(b)` y `(c)`, y así se escriben.
- **Mutación (G5) sobre casos que NINGÚN hook cubre**: SCHOICE 4 mutantes —fuga de nombre en el XML
  y **veredicto invariante cazado nominalmente por H3** (100 % de 40 versiones frente al 55 % del
  control sano)—; CLOZE 3 nuevos (fuga, H3 por gap, V4 con 5 partes), 7 acumulados. **0 desviados**
  en ambos.
- **El Incidente S se reprodujo dentro del propio verificador, dos veces**: primero un harness que
  daba tres falsos negativos porque el comando simulado llevaba comillas escapadas que el regex del
  hook no reconoce (salía en silencio), y después un mutante que **no mutaba el artefacto** — el
  nombre del PNG se genera con `paste0`, no como literal, así que el `sed` no tocaba nada. Ambos los
  atrapó la guarda que distingue «sonda limpia» de «sonda nunca ejecutada». Sin esa guarda, las dos
  veces habrían contado como verde.
- **Verificación**: SCHOICE +1.5 %, CLOZE +0.7 % (presupuesto +8 %); 25/25 y 26/26 pre-flight
  declarados = reales; 16 filas en ambas máquinas de estados; 0 referencias muertas fuera de los
  bloques marcados como ilustrativos; runner completo 24/24 suites, 0 fallidas; I-1..I-9 en verde.

### Cambios v3.20.0 (2026-08-08)

> Esta entrada cubre los **15 commits** del bloque 2026-08-06 → 2026-08-08. El changelog se había
> quedado en la v3.19.0 mientras entraban V8, V9, la sonda H3 y la ampliación del corrector: es
> deriva documental, no cambios nuevos. Se registra en bloque para que el índice deje de omitirlos.

- **NUEVO VALIDADOR — DIAGNOSTICIDAD (V9)**: `.claude/scripts/validar_diagnosticidad.R`. El arsenal
  medía corrección, formato, unicidad y diversidad, pero **nada medía si los distractores
  discriminan**: un ítem puede tener opciones únicas, clave correcta y datos que cambian en cada
  versión, y aun así resolverse sin leer el contenido. Sondas **H1** más-larga/más-corta, **H2**
  prefijo y **H3** veredicto invariante. `ERR_DIAG_SUPERFICIAL` (exit 1) cuando la clave se
  identifica así en el 100 % de las versiones; `WARN_DIAG_SUPERFICIAL` entre el umbral y el 99 %.
- **EL MARGEN ES PARTE DE LA SONDA**: H1 exige además un margen relativo **≥ 15 %** sobre el rival
  más próximo. Sin él, un gap ya igualado a propósito (8 caracteres medianos sobre 115 → 7 %) seguía
  reportando 100 % y quedaba bloqueado por una diferencia que ningún estudiante puede explotar.
  Umbral calibrado contra las dos versiones medidas del mismo gap: original 32 % y 21 % siguen
  cazadas, corregido 7 % y 5 % ya no. Se imprime siempre el margen mediano y una **NOTA DE ORDEN**,
  para que «no dispara» no se confunda con «no hay señal».
- **NUEVA SONDA H3 — regla #22 §P4-bis** (`diversidad-sustantiva.md`): en un ítem de conclusión
  binaria («Sí, porque…»/«No, porque…») la clave puede tener SIEMPRE el mismo veredicto aunque su
  valor numérico cambie. Medido en `area-jardin-lote-porcentaje-n4`: **60/60 versiones con clave
  "No"** y todo el arsenal en verde. Las tres defensas que parecían cubrirlo miran otra cosa — H2
  exige que la clave sea la única con su prefijo (con balance 2+2 nunca lo es → 0 %),
  `validar_diversidad_sustantiva.R` mide el VALOR (que sí variaba) y el balance 2+2 es
  intra-versión. Impacto: 25 % → 50 % de acierto por azar. **Primera sonda cross-versión** del
  arsenal.
- **DIVERSIDAD POR GAP (V8)**: `validar_diversidad_sustantiva.R` opera en **modo CLOZE** — descubre
  las claves por gap (`sol_pN`/`opciones_pN`/`exsol_pN`), declara su **cobertura** y emite
  `WARN_DIV_GAP_FIJO` nombrando los invariantes. `ERR_DIV_COSMETICA` queda reservado al caso en que
  **todos** los gaps son invariantes. Límite declarado: en gaps cuyo texto interpola el contexto
  narrativo, el script cuenta variación de envoltorio como sustantiva, así que V8 exige declaración
  explícita por gap (`variable` | `fija-justificada`).
- **VALIDACIONES CLOZE V1–V7 → V1–V9** en `orquestador-cloze`, con V8 y V9 cableadas en el paso 9,
  el contrato de salida y el checklist de aprobación.
- **CORRECTOR DE ORTOGRAFÍA — el agujero era la FORMA de la lista, no su longitud**:
  `corregir_ortografia_espanol.R` firmaba «✓ limpio» sobre un `.Rmd` que emitía al estudiante
  `formula`×14, `Si, porque`×11, `demas`×7 y `consumio`×4; la regla #7 y el hook de pre-commit se
  apoyan en esa salida, así que el defecto atravesaba las tres capas con un limpio firmado. Ahora:
  regla **morfológica** por sufijo `-ción/-sión/-xión` (cubre vocabulario abierto sin enumerarlo),
  reglas **contextuales** auto-corregibles solo donde la lectura alternativa es gramaticalmente
  imposible (`Si,`/`Si.` → `Sí`, interrogativos tras `¿`), y un segundo diccionario
  **`diccionario_ambiguo`** cuyas formas se reportan como `REVISION_MANUAL` y que `--fix` **nunca**
  toca. Un archivo con ambiguos ya no se declara limpio. Barrido `--fix` aplicado a los `.Rmd` sin
  código embebido de `01-` y `02-`.
- **ROUTING — alias genérico en los 10 agentes**: `claude-opus-4-6` seguía activo, pero el Opus de
  agosto 2026 es Claude Opus 5, tres generaciones por delante. Los IDs pinneados envejecen en
  silencio, así que los 10 agentes pasan a `opus`/`sonnet`/`haiku`, que resuelven al modelo vigente
  de cada tier. Arrastró dos Sonnet 4.5 pinneados que no estaban reportados.
- **CICLO DE ENDURECIMIENTO DE LOS DOS ORQUESTADORES** (`35d7d2e0`, `1ca6f6ad`, `f5b4f88c`,
  `93b24724`), guiado por `.claude/docs/mega-prompt-endurecimiento-orquestadores.md` (movido desde
  `.claude/agents/`, donde confundía la auditoría del directorio, y parametrizado por
  ARTEFACTO_UNICO/GEMELO_SOLO_LECTURA). En CLOZE, el hallazgo con consecuencias operativas fueron
  **cinco residuos** de la versión vieja del Incidente E («NOPS N/A esperado con gaps num/string»),
  falsa desde el 2026-07-30: `exams2nops()` rechaza **cualquier** `extype: cloze` antes de mirar
  `exclozetype`, así que un orquestador que leyera la máquina de estados habría tratado como error
  real un rechazo esperado. Más: `WAIT_USER #3` imprimía «mínimo 4» contra el V4 de 6 — el único
  sitio espejo donde el umbral se MUESTRA en vez de gobernar una decisión, y por eso sobrevivió;
  conteo de pre-flight 24→26 reales; FASE 2N ausente del check 12; `decisiones_humanas` sin declarar
  en el esquema de inputs; bloque «Qué se persiste y qué no» (2b/2c/6b no los registra
  `workflow-state.sh`); fuga por nombre de PNG en el XML de Moodle (§P6), que **mover las gráficas al
  enunciado no elimina**. En SCHOICE: dry-run que no podía fallar, `preflight_failed` ausente de su
  propio contrato de salida, reporte sin bloque de mutantes pese a que el Incidente P lo exige, y
  citas cruzadas al gemelo **por letra** (las letras no coinciden entre gemelos).
- **UN `—` SIN RAZÓN ES UNA HIPÓTESIS**: la tabla de IDs `INC-*` marcaba `INC-SOLUTION-ORDEN` como
  N/A para SCHOICE y era falso — el mecanismo es de `exshuffle`, no del tipo de ítem. Ahora los `—`
  de ambos gemelos llevan su razón escrita en las dos direcciones.
- **CORRECCIÓN DE POLÍTICA — paso 11** (`7f3abf69` revierte `1ca6f6ad`): se había cableado que
  `aprobacion_usuario` «exige evidencia de aula (Nivel 3)». Es falso. El paso 11 es la aprobación
  **del profesor** y se sella ANTES del aula: es lo que habilita llevar el ejercicio a estudiantes.
  La evidencia de Nivel 3 es el gate de `/promover-ejercicio` hacia `03-En-Produccion/`. Evidencia
  en disco: `permutaciones-pescadores-venia-n4` tiene `aprobacion_usuario: true` (11/11) y vive en
  `02-En-Desarrollo/`; con la política revertida ese estado sería inalcanzable.
- **NUEVOS SUBPROYECTOS**: `excedente-almuerzo-proporcional-n4` (SCHOICE + CLOZE v1, y **CLOZE v2
  aprobado para aula**, 11/11, D1-D5 cerrados) y `area-jardin-lote-porcentaje-n4` (SCHOICE N4 de
  argumentación derivado de `MAT-2026-1-026`, con los porcentajes redondeados **en el origen** para
  que el ruido de coma flotante no llegue al enunciado).
- **VERIFICACIÓN DEL BLOQUE**: runner completo **24/24 suites, 0 fallidas**; invariantes I-1..I-9 en
  verde; 10 agentes; 21 archivos en `.claude/rules/` (22 reglas en este índice).

### Cambios v3.19.0 (2026-07-30)
- **NUEVA VARIANTE CLOZE**: `permutaciones-pescadores-venia-n4/cloze/` — 6 partes Progressive
  Disclosure (`schoice|num|schoice|num|mchoice|schoice`) sobre el mismo contrato paramétrico que su
  SCHOICE hermano (I-1..I-7 + instancia canónica), más tres invariantes propias **C-1..C-3**.
  Verificador propio `cloze/verificar_render.R` (**V1-V11**). Verificado: V1-V11 verde, 300/300
  versiones únicas, coherencia APROBADO, diversidad exit 0, ortografía limpia, y **prueba de
  mutación** que confirma que V5 detecta una clave falsa y V11 detecta la desincronización de orden.
- **CORRECCIÓN FACTUAL — Incidente E del `orquestador-cloze`**: decía que el N/A de NOPS era
  «esperado **con gaps num/string**» y que un CLOZE 100 % schoice/mchoice **sí** debía renderizar
  NOPS. **Era falso.** `exams2nops()` rechaza **cualquier `extype: cloze`** antes de mirar
  `exclozetype` (verificado en el código de `exams` 2.4.2). Un orquestador que siguiera la versión
  vieja habría marcado como error real un rechazo esperado. La restricción **no está documentada
  oficialmente** por R/exams (consultado 2026-07-30).
- **CORRECCIÓN DE DERIVA — V4 del `orquestador-cloze`**: exigía «mínimo 4 partes» cuando el estándar
  del repositorio subió a **6** el 2026-06-04. La subida se había aplicado a los skills pero no a
  esta validación, así que el orquestador habría aprobado un CLOZE que el estándar rechaza.
- **NUEVO INCIDENTE Q** (`orquestador-cloze`): la **prosa** de la Solution enumera opciones en orden
  y `exshuffle` la desincroniza. Modo de fallo **vecino a la regla #19 pero distinto**: la #19
  prohíbe citar la **letra**; esto es enumerar en un **orden** que R/exams cambia después. Medido
  sobre el HTML. Fix: la prosa agrupa por categoría, nunca reproduce la lista. **Nueva validación
  V6** (empareja veredictos **por contenido**, no por posición).
- **NUEVO INCIDENTE R / O** (`orquestador-cloze` y `orquestador-schoice`): *un campo que no se emite
  no está probado*. `descripcion_corta` era dato muerto en el SCHOICE y contenía **U+2212**, que
  rompe LaTeX; explotó al crear la variante CLOZE, que sí lo emite. Comprobación cableada en los
  pre-flight de ambos orquestadores.
- **NUEVA VALIDACIÓN V7** (`orquestador-cloze`): unicidad **ampliada** cuando una parte ofrece
  opciones tomadas de fuera del conjunto ya mostrado. La unicidad habitual no lo cubre porque solo
  mira los elementos seleccionados.

### Cambios v3.18.0 (2026-07-29)
- **NUEVO PATRÓN DE ERROR 27**: pool de errores conceptuales del mismo tamaño que el número de distractores. Verde en todo el arsenal y aun así el tipo de error nunca varía entre versiones. Documentado en `patrones-errores-conocidos.md`.
- **CABLEADO DE ORQUESTADORES**: `orquestador-schoice.md` gana el pre-flight check 20 y el Incidente N; `orquestador-cloze.md` gana el pre-flight check 24 y el Incidente P. Ambos exigen pool ≥4-6 con `sample()` y re-enumeración exhaustiva del espacio de combinaciones tras ampliarlo.
- **PUNTO CIEGO DOCUMENTADO**: la Capa B (21 keywords semánticas) es específica de estadística descriptiva; en combinatoria no tiene reglas aplicables y su APROBADO no acredita corrección conceptual del pool. En esos dominios la carga de la prueba recae en invariantes propias del ejercicio y en un verificador que enumere el espacio completo.
- **NUEVO SUBPROYECTO**: `permutaciones-pescadores-venia-n4` — SCHOICE N4, descriptor D4.8, derivado del ítem real `MAT-2026-1-004` (ERA-2026 Sesión 1, pregunta 4). Familia paramétrica n∈{4,5,6} con clave n!; pool de 5 errores y excepción canónica que reproduce el ítem oficial verbatim (enunciado + las 4 opciones). Verificado: 4 formatos + Moodle 12/12 clave = n!, 30/30 ternas, 297/300 versiones únicas, coherencia APROBADO, letter-independence limpio.
- **DESTINO RESERVADO**: `03-En-Produccion/06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/10-Combinatoria_Permutaciones-Variaciones-Combinaciones/permutaciones_pescadores_venia_n4/`.

### Cambios v3.17.1 (2026-06-27)
- **FIX WAIT_USER MODO SUBAGENTE**: `orquestador-schoice` y `orquestador-cloze` rechazaban respuestas humanas reenviadas vía `SendMessage` durante `WAIT_USER` ("no puedo aceptar una confirmación reenviada por el coordinador"). **Causa raíz**: los agentes no tenían instrucciones para el caso subagente, donde `SendMessage` es el único canal de entrada. **Fix**: nueva sección "Regla fundamental WAIT_USER en modo subagente" en ambos agentes — aceptan `SendMessage` como input humano válido, nunca rechazan por "venir del coordinador", interpretan contenido literalmente. Resuelve ciclo infinito de 3+ reintentos fallidos por WAIT_USER.
- **AMBOS ORQUESTADORES**: `.claude/agents/orquestador-schoice.md` y `.claude/agents/orquestador-cloze.md` actualizados con la misma sección.

### Cambios v3.17.0 (2026-06-16)
- **VALIDADOR — SOPORTE _neg_ VARIANTE B**: `validar_coherencia_matematica.R` daba falsos positivos en SCHOICE `_neg_` **Variante B** (texto sinónimo). Fix de causa raíz:
  - `validar_5c_unicidad` (Nivel 5C) AUTO-DETECTA Variante A vs B desde el entorno (`etiquetas_mezcladas`/`opciones_pre_mezcla` con `error`+`correcta*`). Variante B valida estructura semántica + textos distintos; Variante A sigue exigiendo (N-1) idénticas + 1 diferente → elimina falso `ERR_ANS_C`.
  - `construir_params_desde_env` respeta el objeto `params` del ejercicio (m, b, corte_x…) además de (n, datos_ord) → elimina falso `ERR_SEM_A` en Capa A. Ver regla #10 `validacion-neg-opciones-repetidas.md` v2.1.
- **NUEVO EJERCICIO**: `grafica_funcion_lineal_metacognitivo_argumentacion_n3_schoice_neg_v1` (SAI2-PS-26) — SCHOICE `_neg_` Variante B sobre propiedades de f(x)=mx+b (pool de 5 errores FUN-LIN-01..05 con precondiciones). Verificado: correctitud exhaustiva (52 pares (m,b)×5 = 248 evals, 0 violaciones), 250/250 versiones únicas, 4 formatos, letter-independence, coherencia APROBADO.
- **NUEVO TEST**: `tests/testthat/test_neg_variante_b.R` (8 tests; guardas verificadas contra el código pre-fix). Runner: **19 suites** (era 18).
- **TEST VISUAL ROBUSTO**: `test_neg_visual_distinctness` (EST-BOX-03) ahora compara **píxeles** (`png::readPNG`, <1%) en vez del tamaño de archivo PNG (proxy ruidoso, daba 6.8% en imágenes pixel-idénticas → falso positivo dependiente del entorno).
- **RUNNER ENDURECIDO**: `tests/run_all_tests.R` (`ejecutar_suite`) marca una suite como **fallida** si `test_file()` reporta `failed>0` o `error>0`, no solo si el script revienta. Antes reportaba "100%" ocultando fallas a nivel de expectativa.
- **RUNNER AISLADO POR SUBPROCESO**: `ejecutar_suite` ahora ejecuta cada suite en su propio proceso R (`tests/run_one_suite.R`, veredicto vía exit status) → elimina la contaminación cruzada de estado global entre suites (Python/reticulate de proceso único, RNG, options). Resuelve `test_cloze_n3` (renderiza un ejercicio reticulate **inmutable** de 03-En-Produccion) que pasaba aislado pero fallaba al correr tras otra suite Python (`__main__` sin `producto_seleccionado`). Validado: 19/19 verde real (0 Failure/Error en todo el log).
- **ENTORNO**: recompilado `dplyr` desde fuente (su `.so` quedó con `undefined symbol: R_shallow_duplicate_attr` tras actualizar a R 4.6.0) — desbloquea `test_cloze_n3`. Ver memoria `ref_dplyr_recompile_tras_upgrade_R.md`.
- **MEMORIA**: `feedback_detractor_alucina_codigo.md` (el detractor puede fabricar estructura de código al "simular"; verificar contra el `.Rmd` real).

### Cambios v3.16.0 (2026-06-15)
- **ORQUESTADOR-CLOZE — INCIDENTE G**: gráficas-opción dentro de un gap CLOZE no se renderizan en Moodle. Un gap CLOZE (`{1:MULTICHOICE:...}`) muestra sus opciones como texto plano y descarta el HTML → las `<img>` desaparecen ("no se ven los gráficos en el Paso N"). Distinto del SCHOICE puro, donde cada opción es un `<answer>` con HTML completo y las imágenes-opción sí funcionan.
- **FIX OBLIGATORIO**: en CLOZE las gráficas-opción van ROTULADAS (I, II, III, IV) en el **enunciado** de la parte (vía chunk `results='asis'` con `![](...){width=...}`) y las opciones del gap son **texto** ("Gráfica I"…). El rótulo es contenido (no la letra A-D) → compatible con letter-independence (regla #19).
- **VALIDACIÓN CLOZE V5** (bloqueante, N/A si no hay gráficas-opción): verifica sobre el XML de Moodle que ningún gap `MULTICHOICE/MULTIRESPONSE` contiene `<img`/`@@PLUGINFILE@@`, que el Answerlist del enunciado usa texto, y la coherencia rótulo↔respuesta correcta. Las validaciones CLOZE pasan de **V1–V4 a V1–V5**; los incidentes documentados de **6 (A–F) a 7 (A–G)**.
- **AGENTE/COMANDO actualizados**: `.claude/agents/orquestador-cloze.md` (Incidente G, V5, pre-flight check 15, regla especial de auto-corrección, restricción absoluta, WAIT_USER #3, reporte y contrato de salida) + `.claude/commands/orquestador-cloze.md`.
- **HOOK FASE 2L**: `post-exams2-validation.sh` detecta estáticamente imágenes Markdown en el Answerlist del enunciado de un CLOZE → `ERR_CLOZE_V5` (bloqueante). El recordatorio de auditoría visual HTML pasa de FASE 2L a **FASE 2M**. Rango de fases del hook: ahora 2A–2M.
- **NUEVO TEST**: `tests/testthat/test_cloze_graficas_no_en_gap.R` (10 tests, controles +/- + barrido de todos los CLOZE). Runner: **18 suites** (era 17), 100% en verde; allowlist legacy vacío (0 violadores al 2026-06-15).
- **EJERCICIO PILOTO**: `grafica_funcion_lineal_metacognitivo_interpretacion_n3_cloze_v1.Rmd` (SAI2-PS-26) — Parte 1 migrada al patrón (gráficas I–IV en enunciado + opciones de texto); verificado HTML/PDF/Moodle (`{1:MULTICHOICE:Gráfica I~…~=Gráfica III~…}`, 0 imágenes en gaps).
- **MEMORIA**: `feedback_cloze_graficas_no_en_gap_moodle.md`.
- **REGLA**: `graficos-como-opciones.md` cubría SCHOICE puro; el caso CLOZE queda documentado vía Incidente G del orquestador (la regla sigue válida para SCHOICE).

### Cambios v3.15.0 (2026-06-03)
- **NUEVO ORQUESTADOR CLOZE (real)**: `.claude/agents/orquestador-cloze.md` (547 líneas, Opus, maxTurns 65) + `.claude/commands/orquestador-cloze.md` (71 líneas). Gemelo fiel del `orquestador-schoice`: 11 pasos, 3 WAIT_USER (Flujo B, lenguaje gráfico, aprobación), dry-run y reanudación.
- **VALIDACIONES CLOZE V1–V4** (bloqueantes): V1 nº `##ANSWERi##` = nº tipos `exclozetype` = nº partes; V2 orden/inmediatez de `##ANSWERi##` (regla #14); V3 `exsolution`/`extol` por gap (num/string/schoice/mchoice); V4 Progressive Disclosure ≥4 partes.
- **6 INCIDENTES CLOZE** documentados: A `##ANSWERi##` fuera de orden, B `\pandocbounded` (regla #18), C letter-independence en sub-partes (regla #19), D colapso de pools `mchoice`, E NOPS N/A esperado con gaps num/string (no es error), F guard contador `none` para tablas (regla #20).
- **INFRAESTRUCTURA**: I-5 sube de 8 a **9 agentes** (real esta vez) — `test_infraestructura_claude.R` y `infraestructura-protegida.md` actualizados. Resuelve la inconsistencia del changelog v3.12.0.

### Cambios v3.14.0 (2026-06-03)
- **NUEVA REGLA #20**: `markdown-tablas-pandoc.md` — guard del contador `none` para tablas Markdown. pandoc ≥3.7 (RStudio bundlea 3.8.3, ≠ 3.6 de terminal) envuelve `longtable` con `\def\LTcaptype{none}`; la plantilla R-exams no define `none` → `exams2pdf/exams2nops` fallan con `No counter 'none' defined`. Gemelo del Error 16 (pandocbounded): env-específico por versión de pandoc.
- **ERROR 21**: documentado en `patrones-errores-conocidos.md` — fix = bloque raw `{=latex}` con `\@ifundefined{c@none}{\newcounter{none}}{}` al inicio de Question (se ignora en HTML/DOCX; guardia evita redefinir en NOPS multi-ítem).
- **HOOK FASE 2K**: `post-exams2-validation.sh` detecta tablas Markdown sin guard → `ERR_TABLA_NONE` (bloqueante).
- **NUEVO TEST**: `tests/testthat/test_markdown_tablas_none_guard.R` (2 tests) — detección estática en `.Rmd` de 01/02/03. Runner: 16 suites (era 15).
- **GENERACIÓN**: `generar-schoice`, `generar-cloze` (skills) y `orquestador-schoice` (Incidente E + pre-flight check 11) incluyen el guard por defecto en ejercicios con tablas.
- **EJERCICIO**: `rango_colesterol_metacognitivo_interpretacion_n3_schoice_v1` — fix aplicado y verificado con pandoc 3.8.3 y 3.6 (PDF, NOPS×3, HTML, DOCX). Commit `d22caf93`.
- **MEMORIA**: `feedback_pandoc_ltcaptype_none.md`.
- **FIX MASIVO REGLA #20**: guard `\newcounter{none}` insertado en los 15 `.Rmd` legacy de `01-En-PreDesarrollo/` y `02-En-Desarrollo/` con tablas Markdown (verificado render pandoc 3.8.3). Los 11 de `03-En-Produccion/` (inmutable) quedan en el allowlist permanente del test.
- **INCONSISTENCIA DETECTADA (changelog)**: el `orquestador-cloze` documentado en v3.12.0 nunca había sido commiteado (git sin registro de su creación). **Reconstruido en v3.15.0** (ver abajo); las referencias del índice y la invariante I-5 (9 agentes) se restauraron.

### Cambios v3.13.0 (2026-05-14)
- **NUEVA SECCIÓN**: Formato Equilibrado en `graficos-como-opciones.md` (v5.0) — al menos 2 opciones deben compartir el formato de la correcta
  - Previene que el estudiante adivine por formato sin verificar datos (ej: 3 tortas + 1 barra cuando la correcta siempre es torta)
  - Catálogo de distractores por formato: GRAF-TOR-01, GRAF-TOR-02, GRAF-TOR-03, GRAF-BAR-01
  - Verificación obligatoria en `data_generation`: `stopifnot(n_formato_correcto >= 2)`
- **ERROR 18**: Format-based guessing vulnerability — documentado en `patrones-errores-conocidos.md`
  - Detectado en v1 de `distribucion-contagiados`: la torta siempre era correcta y la barra siempre incorrecta
  - Fix: v2 con 2 barras + 2 tortas, correcta = barras
- **ERROR 20 (GRAF-BAR-01)**: Nuevo patrón de distractor — barras con categorías correctas pero alturas permutadas
  - Pasa verificación de categorías, solo falla en verificación de valores por categoría
  - Permite equilibrio 2+2 sin sacrificar calidad de distractores
- **EJERCICIO**: `distribucion_contagiados_metacognitivo_interpretacion_n3_schoice_v2.Rmd` — correcta = barras, 2+2 equilibrio
- **MEMORIA**: 2 nuevas memorias de feedback persistente (format-diversity, GRAF-BAR-01)
- **DOCS**: `INDICE_LECCIONES.md` actualizado con errores 18/20 + v3.13.0

### Cambios v3.12.0 (2026-05-14)
- **NUEVO ORQUESTADOR CLOZE**: `/orquestador-cloze` — pipeline end-to-end de 11 pasos para ejercicios CLOZE, gemelo de `/orquestador-schoice`.
  - Agente: `.claude/agents/orquestador-cloze.md` (Opus, 65 turnos, 400+ líneas)
  - Comando: `.claude/commands/orquestador-cloze.md` (wrapper que valida y delega)
  - Soporta: 4+ partes Progressive Disclosure, exclozetype multi-gap, validaciones V1-V4 específicas CLOZE
  - Mismos 3 WAIT_USER que SCHOICE (Flujo B, lenguaje gráfico, aprobación)
  - NOPS tratado como N/A esperado cuando hay gaps num/string (no como error)
  - 5 incidentes documentados específicos CLOZE (##ANSWERi## fuera de orden, pandocbounded, letter-independence en sub-partes, colapso pools mchoice, NOPS falso error)
- **INFRAESTRUCTURA**: I-5 (invariante de agentes) sube de 8 a 9 agentes con la adición de orquestador-cloze
- **DOCS**: `COMANDOS_Y_SKILLS.md` actualizado con documentación completa de ambos orquestadores + tabla comparativa SCHOICE vs CLOZE
- **REFERENCIAS CRUZADAS**: `CLAUDE.md` índice actualizado con los 2 orquestadores

### Cambios v3.11.0 (2026-05-12)
- **NUEVA REGLA #19**: `solution-letter-independence.md` — prohíbe `r letra_correcta` y literal "Opción [A-D]" en la sección Solution. Defensa contra re-shuffle externo (Moodle "Shuffle answers").
- **HOOK FASE 2J**: `post-exams2-validation.sh` agrega detector de patrones P1-P4 en Solution. Errores `ERR_SOL_LETRA_R`, `ERR_SOL_LETRA_CAT`, `ERR_SOL_LETRA_LITERAL` (todos bloqueantes).
- **NUEVO TEST**: `tests/testthat/test_letter_independence.R` (4 tests). Lista legacy con 8 .Rmd conocidos (action item de fix pendiente).
- **SCRIPT ORTOGRAFÍA**: `corregir_ortografia_espanol.R` arregla 3 falsos positivos sistemáticos: variables R entre strings concatenados (esta_en_string roto), anchors Markdown `{#...}`, rutas/nombres de archivo en comentarios.
- **ORQUESTADOR-SCHOICE**: pre-flight checks 8-10 (regla #19, test, hook FASE 2J), Incidente C en lecciones absorbidas, paso 4 de validación realista.
- **TESTS**: 13 suites enganchadas al runner (era 12).
- **ERROR 19 EN CATÁLOGO**: documentado en `patrones-errores-conocidos.md`.
- **EJERCICIO PILOTO**: `Comparacion-Lineas-Temporales-Schoice` renombrado desde `Comparacion-Lineas-Fertilizantes-Rusia-China` + 5 fixes (commit `86a4b211`).
- **REGLA #6 EXCEPCIÓN #2 OBSOLETA**: la excepción "exshuffle:FALSE permitido si Solution referencia letra" queda obsoleta porque la regla #19 prohíbe esa referencia.

### Cambios v3.10.0 (2026-05-03)
- **NUEVA REGLA #18**: `markdown-imagenes-pdf.md` — anti `\pandocbounded undefined`. Toda imagen Markdown en `.Rmd` debe llevar `{width=...}`.
- **REGLA #6 AMPLIADA** (codigo-rmd.md): `exshuffle: FALSE` también obligatorio cuando Solution referencia `r letra_correcta` o "Opción [A-D]" hardcoded.
- **NUEVO TEST DE REGRESIÓN**: `tests/testthat/test_pandocbounded_y_solution_coherence.R` (10 tests) — análisis estático de Markdown sin width + combinación exshuffle:TRUE+letra explícita.
- **HOOK FASE 2I**: `post-exams2-validation.sh` agrega detección automática de `\pandocbounded` en `.tex` recientes y patrones Markdown sin width en el `.Rmd`.
- **AGENTE orquestador-schoice**: pre-flight checks ampliados (#6, #7) + sección "Lecciones absorbidas de sesiones previas" con incidentes A-B + validación realista obligatoria.
- **SKILL generar-schoice**: patrones obligatorios documentados con ejemplos correcto/prohibido.
- **MEMORIA GLOBAL**: 3 nuevos archivos de feedback persistente (`feedback_pandocbounded.md`, `feedback_exshuffle_solution_coherence.md`, `feedback_validation_realista.md`).
- **ERRORES 16-17 EN CATÁLOGO**: documentados en `patrones-errores-conocidos.md` (líneas 1782-2024).
- **INDICE_LECCIONES.md**: actualizado con sección 2.5 (pipeline render PDF + coherencia Solution).
- **TESTS**: 12 suites enganchadas a runner (era 11).

### Cambios v3.8.0 (2026-04-10)
- **INFRAESTRUCTURA: resuelto drift hooks/tests/CI/docs** — una sola fuente de verdad por componente.
- **Runner ejecuta 12 suites** (antes 10): `test_cloze_n3.R` y `test_stress_test_visual.R` enganchadas al runner y al modo quick.
- **CI simplificado**: `.github/workflows/ci-testing.yml` reemplazado por un único job `tests-full` que invoca `Rscript tests/run_all_tests.R`. Upgrades a `actions/checkout@v4` y `actions/upload-artifact@v4`.
- **Hooks muertos eliminados**: `pre-edit-testing.sh`, `post-edit-testing.sh` y 3 docs stub. `settings.json` carga únicamente los 2 hooks activos (`pre-write-rmd-gate.sh`, `post-exams2-validation.sh`).
- **Fix aritmético en `post-exams2-validation.sh`**: el conteo del arsenal ya no se duplica cuando el script reporta `Total ERRORES: N` y además falla con exit≠0.
- **Fix ruta en `test_stress_test_visual.R`**: source basado en `git rev-parse --show-toplevel` — la suite ya corre desde cualquier cwd.

### Cambios v3.7.0 (2026-03-23)
- **SKILLS DE REVISIÓN**: `/revisar-schoice` y `/revisar-cloze` ejecutan pasos 4-11 del workflow
  - Skill SCHOICE: `.claude/skills/revisar-schoice/SKILL.md` (model_recommendation: sonnet)
  - Skill CLOZE: `.claude/skills/revisar-cloze/SKILL.md` (model_recommendation: sonnet)
  - Detectan automáticamente paso pendiente y retoman workflow interrumpido
  - Validaciones específicas por tipo (exsolution binario, ##ANSWERi##, exclozetype)
- **SKILLS GENERACIÓN v4.0**: `/generar-schoice` y `/generar-cloze` ahora cubren 11 pasos completos
  - 5 pasos antes ausentes: retroalimentación, detractor, coherencias, diversidad, validar-icfes
  - `workflow-state.sh init` + `complete` integrado en cada paso
  - Diagrama de integración actualizado con flujo completo
- **25 SKILLS** (era 23): +revisar-schoice, +revisar-cloze
- **DOCS**: `COMANDOS_Y_SKILLS.md` actualizado con los nuevos skills

### Cambios v3.6.0 (2026-02-14)
- **STRESS TEST VISUAL MULTI-SEMILLA**: Renderiza N veces con exams2pdf(), analiza anomalías, genera PNGs
  - Script R: `SOURCES/scripts_validacion/stress_test_visual.R` (~450 líneas)
  - Skill: `.claude/skills/stress-test-visual/SKILL.md` (model_recommendation: sonnet)
  - Tests: `tests/testthat/test_stress_test_visual.R` (28 tests)
  - Anomalías detectadas: ANOM_COMPILE, ANOM_DUP_OPT, ANOM_DIST_EQ_CORR, ANOM_POS_FIJA, ANOM_BAJA_VAR, ANOM_NA_INF, ANOM_CTX_REPET, ANOM_NEG_PATRON
- **FASE 2H nueva**: Integrada en hook `post-exams2-validation.sh` v6.0
  - Se ejecuta automáticamente después de FASE 2G si no hay errores previos
  - 10 semillas por defecto, renderizado real con exams2pdf()
  - Claude inspecciona PNGs de semillas sospechosas
- **11 SUITES DE TESTING** (era 10): 110+ tests (era 82+)
- **23 SKILLS** (era 22): +stress-test-visual (Sonnet)
- **Regla #15 nueva**: Stress Test Visual automático y permanente

### Cambios v3.5.0 (2026-02-14)
- **CAPA D: DETERMINISMO DE calcula()**: Nueva capa de validación semántica
  - Análisis estático: `deparse()` escanea `sample(`, `runif(`, `rnorm(`, etc.
  - Test empírico: ejecuta `calcula()` 2 veces con mismos args, compara resultados
  - `ERR_SEM_D`: error seleccionado no determinista (bloqueante)
  - `WARN_SEM_D`: error en pool no determinista (bug latente, informativo)
- **FIX EST-MTC-03**: `calcula()` usaba `sample(datos_ord)` — reemplazado por `datos_presentados`
  - `set.seed()` del multi-semilla enmascaraba el no-determinismo
  - Firma estándar ahora: `function(datos_ord, datos_presentados = NULL)`
- **REGLA**: `calcula()` DEBE ser función pura — PROHIBIDO `sample/runif/rnorm` dentro
- **6 tests nuevos** en `test_validacion_semantica.R` para Capa D
- **Docs actualizados**: `ejercicios-metacognitivos.md`, `validacion-correctitud-respuesta.md`

### Cambios v3.4.0 (2026-02-14)
- **ROUTING DE MODELOS OBLIGATORIO**: Cada skill/agente usa el modelo apropiado por complejidad
  - Opus 4.6: 6 skills (generación .Rmd, detractor, retroalimentación, análisis pedagógico) + 2 agentes
  - Sonnet 4.5: 9 skills (generación gráficos, comparación visual, diagnóstico) + 3 agentes
  - Haiku 4.5: 7 skills (validaciones, estado, transferencia, promoción) + 1 agente
- **6 AGENTES actualizados**: Modelos obsoletos (claude-3-5-sonnet, opus-4-5) → modelos actuales
- **22 SKILLS con `model_recommendation`**: Metadata en frontmatter YAML
- **16 SKILLS con bloque ROUTING**: Delegación obligatoria via `Task(model=X)`
- **Regla #14 nueva**: `.claude/rules/modelo-routing-obligatorio.md`
- **Doc de referencia**: `.claude/docs/MODELO_ROUTING.md` (tabla completa)
- **Ahorro estimado**: 50-60% en tokens/costos sin degradar calidad

### Cambios v3.3.0 (2026-02-14)
- **VALIDACIÓN CORRECTITUD NIVEL 5**: Cross-check respuesta marcada vs valor correcto
  - 5A: Evaluación de exsolution dinámico (`` `r expr` ``)
  - 5B: Cross-check respuesta marcada vs valor_correcto calculado
  - 5C: Unicidad de opciones en runtime (digest::digest)
  - 5D: Validación de rangos matemáticos (mediana, cuartiles, probabilidades)
  - 5E: Distractor ≠ respuesta correcta
- **VALIDACIÓN MULTI-SEMILLA**: Script `validar_multisemilla.R` (20 semillas rápido, 100 exhaustivo)
- **FASE 2G nueva**: Multi-semilla rápida integrada en hook post-exams2
- **10 SUITES DE TESTING** (era 9): 82+ tests (era 68+)
  - Nueva suite: `test_correctitud_respuesta.R` (14 tests)
- **Errores nuevos**: ERR_ANS_A/B/C/D/E (todos bloqueantes)
- **Regla #13 nueva**: Validación correctitud respuesta automática y permanente

### Cambios v3.2.3 (2026-02-13)
- **VALIDACIÓN SEMÁNTICA NIVEL 4**: Sistema de 3 capas integrado globalmente
  - Capa A: Precondiciones declaradas (`precondicion` en cada error del pool)
  - Capa B: Scanner automático de 21 keywords semánticas
  - Capa C: Cross-validación `calcula()` ≠ valor correcto
- **8 DOMINIOS DETRACTOR** (era 7): agregado `coherencia_semantica`
- **9 SUITES DE TESTING** (era 6): 68+ tests (era 33+)
  - Nueva suite: `test_validacion_semantica.R` (35 tests)
  - Nueva suite: `test_media_mediana_moda.R` (3 tests)
  - Nueva suite: `test_neg_visual_distinctness.R` (3 tests)
- **Errores semánticos**: ERR_SEM_A/B/C y WARN_SEM_B documentados
- **Bug fix**: tryCatch scoping en R (asignaciones no propagaban al scope externo)
- **Regla #12 nueva**: Validación semántica automática (descripción ↔ datos)

### Cambios v3.2.2 (2026-02-07)
- **GRÁFICOS COMO OPCIONES INDIVIDUALES**: Nueva regla `.claude/rules/graficos-como-opciones.md`
  - Cada opción gráfica DEBE ser PNG separado (diagrama_a.png, etc.)
  - PROHIBIDO usar `grid.arrange()` para mostrar opciones juntas
  - Answerlist DEBE referenciar imágenes individuales
- **GRAFICADOR SECUENCIAL v2.0**: Actualizado `.claude/rules/graficador-secuencial.md`
  - Umbral de fidelidad: 95% → **98%**
  - Iteraciones: Manuales → **AUTOMÁTICAS**
  - Lenguajes: SIEMPRE generar **TikZ + Python + R** (los tres)
  - Decisión final: Claude NO puede elegir → **USUARIO SIEMPRE DECIDE**
- **3 NUEVOS PATRONES DE ERROR** documentados en `patrones-errores-conocidos.md`:
  - Error 4: Gráficos en grid (no individuales)
  - Error 5: EST-BOX-01 escala incompatible
  - Error 6: sample() sin rango suficiente

### Cambios v3.2.1 (2026-02-07)
- **7 DOMINIOS DE REVISIÓN**: código, pedagógico, visual, gramática, matemático, metacognitivo, testing
- **Nuevos dominios**:
  - `coherencia_matematica`: Fórmulas, cálculos, proporciones, distractores plausibles
  - `icfes_metacognitivo`: Progressive Disclosure, pool errores, metadatos DOK/Bloom/SOLO
  - `testing`: Cobertura tests, git hooks nativos, CI/CD
- **Integración mejorada** con testing-obligatorio.md y ejercicios-metacognitivos.md

### Cambios v3.2 (2026-02-07)
- **DETRACTOR OBLIGATORIO**: Skill-detractor se ejecuta automáticamente en fases de revisión
- **Nueva regla**: `.claude/rules/detractor-obligatorio.md`
- **FASE 2C añadida**: Revisión adversarial después de validación visual
- **Ciclo de validación v4.0**: FASE 1 → 2A → 2B → 2C (detractor) → FASE 3
- **Puntos de activación**: Post-generación, FASE 2C, pre-promoción
- **Config por defecto**: `.claude/detractor-config.yaml`

### Cambios v3.1 (2026-02-06)
- **EJERCICIOS METACOGNITIVOS OBLIGATORIOS**: Todo .Rmd debe aplicar Progressive Disclosure
- **Nueva regla**: `.claude/rules/ejercicios-metacognitivos.md`
- **Skills actualizados**: generar-schoice v3.0, generar-cloze v3.0
- **Nueva referencia**: `anatomia-metacognitiva.md` para estructura de 8 secciones
- **Pool de errores conceptuales**: Ahora obligatorio con códigos y funciones `calcula()`
- **Metadatos cognitivos**: DOK, Bloom, SOLO ahora obligatorios
- **Antipatrones documentados**: Ejercicios puramente procedimentales PROHIBIDOS

### Cambios v3.0 (2026-02-04)
- **MODULARIZACIÓN COMPLETA**: CLAUDE.md ahora es índice central
- **Nuevos módulos**:
  - `REGLAS_CRITICAS.md` - Consolidación de reglas obligatorias
  - `COMANDOS_Y_SKILLS.md` - Referencia completa de comandos
  - `HOOKS_Y_TESTING.md` - Sistema automático de validación
  - `ESTRUCTURA_REPOSITORIO.md` - Organización del proyecto
- **Mejora de navegación**: Enlaces directos a cada módulo
- **Tabla de referencias rápidas**: Acceso inmediato por necesidad

### Cambios v2.7 (2026-02-03)
- Sistema de Testing Automático PERMANENTE
- 4 hooks activos configurados
- Garantía: IMPOSIBLE romper el sistema
- PROHIBIDO: `git commit --no-verify`

### Cambios v2.6 (2026-02-03)
- Ecosistema de Testing Agresivo implementado
- COBERTURA 100% ALCANZADA: 9 suites, 68+ tests unitarios
- CI/CD automático con GitHub Actions

### Historial Completo
Ver @.claude/docs/CHANGELOG.md para historial detallado de cambios v2.2-v2.5

---

**Principio Fundamental**: Este sistema garantiza calidad mediante validación automática permanente. NO hay forma de evadir las protecciones de testing. Toda modificación es validada antes y después de su aplicación.
