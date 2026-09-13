# Sistema de Generación Automatizada de Ejercicios ICFES R/exams

## 🎯 Índice Principal

Este archivo funciona como **índice central** del sistema. Para información detallada, consulte los módulos especializados: se leen bajo demanda, no se importan (ver «Política de contexto»).

### 📋 Información General
- **Propósito**: Automatizar creación y validación de ejercicios ICFES tipo SCHOICE/CLOZE
- **Tecnologías**: R/exams, TikZ, Python/matplotlib, R/ggplot2
- **Formatos soportados**: HTML, PDF, DOCX, NOPS
- **Versiones por ejercicio**: 250+ únicas aleatorias

### 📏 Política de contexto (desde v3.25.0)

Este archivo entra en el contexto de **cada sesión y de cada subagente**, así que se mantiene corto:
- **Sin importaciones `@`.** Las rutas de este índice son punteros: se leen con `Read` cuando la tarea lo requiere. Un `@` aquí vuelve a cargar ese archivo en todas las sesiones y en todos los subagentes.
- **Changelog fuera del índice.** Toda entrada de cambios va a `.claude/docs/CHANGELOG_CLAUDE_MD.md`, arriba del todo. Aquí solo se actualizan **Versión** y **Fecha** de la sección Metainformación.
- **Presupuesto**: este índice no debe superar ~15 000 tokens (~60 KB). Una regla nueva = una entrada de un párrafo aquí + su archivo en `.claude/rules/`.
- Los gates que garantizan la calidad son mecánicos (hooks de `.claude/settings.json`, `tests/run_all_tests.R`, invariantes I-1..I-10) y no dependen de que un documento esté en contexto.

### ⛔ Reglas Críticas (OBLIGATORIAS)
Consolidado: `.claude/docs/REGLAS_CRITICAS.md` (leer bajo demanda).

**Resumen de reglas fundamentales:**
1. **Ejercicios metacognitivos** con Progressive Disclosure → .claude/rules/ejercicios-metacognitivos.md
2. **Flujo B obligatorio** cuando hay gráficos → .claude/rules/flujo-b-obligatorio.md
3. **Proceso secuencial** TikZ→Python→R (98% fidelidad, usuario decide) → .claude/rules/graficador-secuencial.md
4. **Gráficos como opciones individuales** (PNGs separados) → .claude/rules/graficos-como-opciones.md
5. **5 Coherencias** a verificar siempre (Semántica, Visual-Texto, Matemática, Código, General)
6. **Validación visual iterativa** OBLIGATORIA → .claude/rules/ciclo-validacion.md
7. **Ortografía española** con tildes → .claude/rules/ortografia-espanol.md
8. **Testing automático** permanente → .claude/rules/testing-obligatorio.md
9. **Detractor obligatorio** en fases de revisión → .claude/rules/detractor-obligatorio.md
   Desde la v1.2: el detractor DEBE ser un agente **distinto** del que escribió o corrigió el
   artefacto (autoevaluación ≠ FASE 2C), su reporte se considera entregado sólo si cierra con
   el marcador `VEREDICTO_DETRACTOR:`, y si no entrega tras 2 intentos se **escala al usuario**
   — PROHIBIDO sustituirlo por la auditoría propia del coordinador y sellar `detractor_fase2c`.
   **Desde la v1.3 (2026-08-16): el detractor se lanza SIEMPRE sin `name:`** — el `name` lo
   convierte en *teammate* y su texto final **no llega jamás** a quien lo invoca (sólo
   `"Spawned successfully"` + `idle_notification`), así que un veredicto ausente ahí es un
   **error de invocación**, no una no-entrega: se relanza, no se reclama. Y antes de gastar
   reintentos rige el **Paso 0 — recuperar el reporte de la transcripción**: los 11 casos
   medidos lo tenían escrito entero.
   **Desde la v1.4 (2026-09-08): segundo detractor heterogéneo** (`/detractor-hetero`, cascada
   DeepSeek → GLM → GPT → local, mismo prompt de sistema y mismo marcador) como FASE 2C-bis,
   recomendado antes de promover y tras cada ciclo RECHAZAR → corrección; **complementa, no
   sustituye** al `AgenteDetractor` ni sella `detractor_fase2c`.
   **Desde la v1.5 (2026-09-13): su alcance incluye overrides firmados e invariantes locales**
   (`.claude/CLAUDE.md` del subproyecto, regla #17), no sólo el `.Rmd` — encontró un override con
   el exceso §P7 mal medido y dos invariantes en prosa sin guarda ejecutable (Errores 35-37).
10. **Validación _neg_ opciones repetidas** → .claude/rules/validacion-neg-opciones-repetidas.md
11. **Contextos narrativos creativos** (no mecánicos) → .claude/rules/contextos-narrativos-creativos.md
12. **Validación semántica automática** (Nivel 4: descripción ↔ datos) → .claude/rules/ejercicios-metacognitivos.md (sección Validación Semántica)
13. **Validación correctitud respuesta** (Nivel 5: multi-semilla + cross-check) → .claude/rules/validacion-correctitud-respuesta.md
14. **Routing de modelos obligatorio** (Opus/Sonnet/Haiku por complejidad) → .claude/rules/modelo-routing-obligatorio.md
15. **Stress Test Visual** (FASE 2H: renderizado masivo + análisis anomalías) → .claude/skills/stress-test-visual/SKILL.md
16. **Workflow State Enforcement** (gate mecánico PreToolUse + estado persistente) → .claude/rules/workflow-state-enforcement.md
17. **Infraestructura `.claude/` protegida** (backups + verificación de invariantes I-1 a I-7 antes/después de plataformas externas) → .claude/rules/infraestructura-protegida.md
18. **Markdown-imágenes-PDF (atributo `{width=...}` obligatorio)** → .claude/rules/markdown-imagenes-pdf.md
    Toda imagen `.png/.jpg/.svg/.pdf` emitida vía Markdown (directa o `cat()`) en `.Rmd` DEBE incluir atributo `{width=...}`. Pandoc ≥3.2.1 sin width envuelve el `\includegraphics` en `\pandocbounded`. **Actualizado 2026-08-15**: R/exams ≥ 2.4-1 ya define ese macro como **no-op** en todas sus plantillas, así que el `Undefined control sequence` **ya no se reproduce** con las plantillas del paquete (sí con plantilla propia). La regla sigue vigente por otra razón: al ser no-op, `\pandocbounded` **no controla el tamaño**. En la misma pasada se retiró el **Patrón B** (condicional `is_latex_output()`), que **pierde la imagen en el PDF**. Coupled con regla #6 ampliada. Errores 16-17 documentados.
19. **Solution letter-independence** (NUNCA `r letra_correcta` ni "Opción [A-D]" en Solution) → .claude/rules/solution-letter-independence.md
    Defensa permanente contra Error 19. La sección Solution debe identificar opciones por contenido/código de error, NUNCA por letra/posición, porque Moodle (y otros LMS) pueden re-shufflear las opciones de forma independiente al `exshuffle` de R-exams, rompiendo coherencia letra ↔ contenido para el estudiante. Capas: hook FASE 2J + test_letter_independence.R + detractor.
20. **Markdown-tablas-pandoc (guard contador `none`)** → .claude/rules/markdown-tablas-pandoc.md
    Defensa permanente contra Error 21. Todo `.Rmd` con tabla Markdown (`kable(format="markdown")` o `cat("| ...")`) DEBE incluir, al inicio de `Question`, el bloque raw LaTeX `` ```{=latex}\makeatletter\@ifundefined{c@none}{\newcounter{none}}{}\makeatother``` ``. pandoc ≥3.7 (RStudio bundlea 3.8.3, distinto del 3.6 de terminal) envuelve `longtable` con `\def\LTcaptype{none}`, contador que la plantilla de R-exams no define → `exams2pdf/exams2nops` fallan con `No counter 'none' defined`. Gemelo del Error 16. Capas: generación (skills + orquestador) + hook FASE 2K (`ERR_TABLA_NONE`) + test_markdown_tablas_none_guard.R + validación con pandoc de RStudio.
21. **Familias de Soluciones Reutilizables** → .claude/rules/familias-soluciones-rmd.md
    Índice operativo de patrones probados + librería de helpers `.claude/scripts/snippets_familias_rmd.R`. Aplicar las familias relevantes en toda generación/corrección: **F1** generación sin cuelgue (`pick_int`/`construir_valores_con_rango`, nunca `repeat` sin cota — Error 22); **F2** tablas responsivas cross-formato (`tabla_responsiva`, fenced div `::: {style=overflow-x:auto}` que sobrevive DOCX como `<w:tbl>` y PDF como longtable); **F3** ecuaciones display responsivas (`eq_display`); **F4** coherencia de marcas en CLOZE (sol alineado por construcción + verificación marca-vs-verdad); **F5** trampa `sample(escalar)` (`pick_int`/`safe_sample`); **F6** opciones gráficas de diagramas vectoriales cardinales (`dibujar_diagrama_cardinal`/`orientaciones_cardinales`/`seleccionar_combinacion_con_cascada`/`renombrar_opciones_neutral`: orientación sorteada por versión, cascada de umbrales de legibilidad en vez de umbral único con `stopifnot`, renombrado neutral POST-mezcla y distractores que conserven la magnitud de la correcta — Errores 23-26). Test: test_data_generation_no_hang.R.
22. **Diversidad Sustantiva** (respuesta correcta debe variar entre versiones, no solo el envoltorio narrativo) → .claude/rules/diversidad-sustantiva.md
    **§P7-A..D (2026-08-19) — CRITERIO DE ACEPTACIÓN, no sólo de diagnóstico.** Medido sobre **426 ítems oficiales**: el corpus del ICFES marca **+4,6 pp** y su control **+5,3 pp**, ambos en zona gris — **ningún ítem oficial saca `PASS` limpio**, así que exigírselo a un ejercicio generado es exigirle más que al examen real. Aceptable **≤ +5,3 pp**; obliga sólo **> +8 pp**. **§P7-B**: frecuencia con margen < 15 % es **inexplotable** y no es defecto. **§P7-C**: la batería se **congela** al inicio (ampliarla a mitad de ciclo cambia la vara). **§P7-D**: **3 pasadas** de corrección como máximo, luego cierre con residuo declarado — porque en el ciclo que originó la política la pasada que más mejoró §P7 fue la que **volvió falsa la clave** en el 31,7 % de las versiones. *Perseguir la diagnosticidad produjo el único defecto de corrección.*
    Defensa contra diversidad cosmética. Un conteo alto de "versiones únicas del render" NO garantiza que los datos numéricos / contenido gráfico de la respuesta correcta cambien entre semillas. Prohibido: parámetros hardcoded como literales, PNGs de opciones copiados con `file.copy`. Defensa: hook FASE 2N (`WARN_DIV_ESTATICA`) + script `validar_diversidad_sustantiva.R --n 100` en orquestador paso 9 (`ERR_DIV_COSMETICA` bloqueante) + test_diversidad_sustantiva.R. Incidente: `desplazamiento-avion-aeropuerto` (2026-06-27) — 288/300 versiones únicas con respuesta correcta invariante. **§P7-E (v1.7, 2026-08-22)**: la cobertura por familias no basta si todas las reglas miran las opciones **por separado** — hace falta al menos una **relacional entre pares** (Errores 33-34), y todo canal se mide también sobre la **instancia canónica** por enumeración exacta antes de perseguirlo. **§P7 (v1.5, 2026-08-15) — cierre por familias de dimensión**: a diferencia de P1-P6, no nombra un canal de fuga sino un defecto **del verificador**. Toda batería de reglas de eliminación debe cerrar por las **seis familias** (magnitud, divisibilidad, signo, posición, formato, léxico), declarando las inaplicables; calibrar contra el **techo nulo** permutando la clave (un máximo sobre muchas reglas está inflado por selección: 69,6 % observado contra 34,8 % de techo); y declarar **NO CONCLUYENTE** cuando el máximo cae a menos de 5 pp del umbral. *Una batería incompleta no mide «sin señal», mide «sin sonda»* — el canal real (47,4 %) estaba en la única familia sin sonda. Helper: `.claude/scripts/bateria_eliminacion.R`; test: test_bateria_eliminacion.R (suite 32).
23. **Muestra estándar de validación: N = 100** → .claude/rules/muestra-estandar-validacion.md
    **Toda medición estadística sobre versiones usa `N = 100`.** Un único número, cableado en código ejecutable, NO elegible por sesión, agente ni handoff. Aplica a `validar_diagnosticidad.R`, `validar_diversidad_sustantiva.R`, `validar_multisemilla.R`, verificadores propios del ejercicio y smokes: invocarlos **sin `--n`** ya da el estándar. Origen: el repo tenía **cinco tamaños rivales** (5/10/20/30/40) y ninguna fuente única, así que cada agente elegía el suyo —algunos 400—; la instrucción verbal del profesor no se sostuvo porque no vivía en nada ejecutable. Excepción **declarada**: las muestras de **renderizado real** (`stress_test_visual.R`, `auditor-visual-html`) cuestan un PDF o una captura por unidad — su N debe **declararse siempre en el reporte** junto al resultado. NO confundir con el umbral de producto de 250+ versiones únicas sobre 300 (regla #3), que no cambia. Timeout del hook subido a 300 s para que quepa (170 s medidos). Test: test_muestra_estandar.R (suite 29).
24. **Hermes — triaje y fidelidad de figuras de cuadernillo** → .claude/rules/hermes-imagenes-icfes.md
    Estrategia importada desde Todo-Pajaro (`motor-hermes`, v1.9.0, 13 lecciones validadas sobre lotes reales 2026-07-03 → 2026-08-05). Antes de reproducir CUALQUIER figura de un ítem escaneado hay que **mirar el recorte del JPG**: la descripción textual (`[FIGURA: …]`, ficha de alineación) SOBRE-clasifica sistemáticamente. Cinco exigencias: **H-1** gate visual (la decisión `flujo_b` se justifica con lo VISTO, no con el `.md`); **H-2** ⛔ la trampa deliberada ES la pregunta — reproducir la figura *incluidos sus errores*, jamás normalizar (incidente Q067: "corregir" la gráfica habría hecho verdadera una opción falsa), con screening de 7 patrones de enunciado; **H-3** gate de fidelidad **por tipo** en 4 ramas — celda-a-celda para tablas, **inventario bidireccional de rótulos** para geometría (atrapa la etiqueta *agregada* que un checklist de forma no ve), checklist dirigido para curvas; **H-4** ancla en el número IMPRESO (los mapeos página↔pregunta tienen desfase acumulado) y crop al borde del contenido; **H-5** asimetría de seguridad — endurecer es autónomo, **relajar nunca**. El motor ejecutable NO se forkea aquí: fuente única en `$MOTOR_HERMES` de Todo-Pajaro. Copia congelada de la estrategia: `.claude/skills/hermes-imagenes/SKILL.md`.

25. **Glifos Unicode que rompen pdflatex** → .claude/rules/glifos-latex-prohibidos.md
    Un `✓` (U+2713) **literal** en texto Markdown visible impide compilar el PDF, y el fallo es **invisible en HTML** porque no pasa por LaTeX: por eso sobrevivió meses en `03-En-Produccion/` sin que nada lo detectara — ningún validador del arsenal miraba los caracteres del fuente. **110 glifos medidos** con `exams2pdf()`, y tres resultados contradicen la intuición: las flechas `← ↑ →` **compilan** pero `↔ ⇒ ⇔ ↺` no (el bloque Unicode no es homogéneo); el modo math **no salva** (`$a ≤ b$` falla igual, hay que usar `$\le$`); y un glifo **sólo en comentario R** es inocuo. La severidad está **calibrada**, no elegida, midiendo causalmente los 63 `.Rmd` afectados (render del original vs. del mismo archivo con los glifos sustituidos): en **Markdown** acierta 16/16 → `ERR_GLIFO_LATEX` bloqueante; en **código R** sólo 1/24 → `WARN_GLIFO_LATEX`, porque bloquear ahí habría marcado 23 archivos que sí compilan (una cadena R puede no emitirse nunca). Tildes españolas y `× ÷ ° ² → — •` están medidos como seguros y NO se tocan. Capas: detector único `validar_glifos_latex.R` + hook FASE 2O + `test_glifos_latex.R` (suite 33) con allowlist de 29 legacy que **no admite altas**.

### 🛠️ Comandos y Skills
Referencia completa: `.claude/docs/COMANDOS_Y_SKILLS.md`

**Comandos principales:**
- `/analizar-icfes`, `/generar-schoice`, `/generar-cloze`
- `/orquestador-schoice`, `/orquestador-cloze` - Pipeline end-to-end (11 pasos, 3 pausas humanas) 🆕
- `/revisar-schoice`, `/revisar-cloze` - Revisión completa pasos 4-11 del workflow
- `/skill-retroalimentacion` - Generación científica de sección Solution
- `/validar-pedagogico` - Análisis pedagógico avanzado basado en evidencias
- `/detractor auditoria [target]` - Revisión adversarial en 8 dominios
- `/detractor-hetero [ruta.Rmd|dir]` - Segundo detractor de otra familia de modelos (FASE 2C-bis; complementa, no sustituye) 🆕
- `/auto-refinar-grafico [tikz|python|r]`
- `/estado-graficador`, `/exportar-graficos`, `/promover-ejercicio`

### 🔧 Sistema de Hooks y Testing
Referencia completa: `.claude/docs/HOOKS_Y_TESTING.md`

**Sistema automático permanente:**
- 2 hooks activos (PreToolUse: gate .Rmd + recordatorio tildes; PostToolUse: arsenal post-render)
- Gate mecánico: `pre-write-rmd-gate.sh` bloquea .Rmd sin `ejercicio_state.json`
- 100% cobertura de tests (12 suites, 130+ tests)
- CI/CD con GitHub Actions
- Tolerancia cero a regresiones

### 📁 Estructura del Repositorio
Referencia completa: `.claude/docs/ESTRUCTURA_REPOSITORIO.md`

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
- .claude/docs/WORKFLOW_PASO_A_PASO.md
- .claude/docs/TRES_NIVELES_VALIDACION.md
- .claude/docs/FLUJO_AUTOMATICO_TESTING.md
- .claude/docs/TROUBLESHOOTING.md

#### Testing y Calidad
- .claude/docs/ECOSISTEMA_TESTING.md
- .claude/rules/testing-obligatorio.md
- .claude/rules/documentacion-verificada.md

#### Código y Desarrollo
- .claude/rules/codigo-rmd.md
- .claude/docs/NOMENCLATURA_ARCHIVOS_RMD.md
- .claude/docs/MEJORES_PRACTICAS_PYTHON_RETICULATE.md
- .claude/docs/patrones-errores-conocidos.md

#### Casos Resueltos
- .claude/docs/casos-resueltos/

### 🔗 Referencias Rápidas

| Necesito... | Ver documento |
|-------------|---------------|
| Iniciar desarrollo de ejercicio | .claude/docs/WORKFLOW_PASO_A_PASO.md |
| Resolver error conocido | .claude/docs/patrones-errores-conocidos.md |
| Entender hooks de testing | .claude/docs/HOOKS_Y_TESTING.md |
| Configurar gráficos | .claude/docs/REGLAS_CRITICAS.md + Flujo B |
| Gráficos como opciones SCHOICE | .claude/rules/graficos-como-opciones.md |
| Workflow Graficador (98% + 3 lenguajes) | .claude/rules/graficador-secuencial.md |
| Generar retroalimentación científica | .claude/skills/skill-retroalimentacion/SKILL.md |
| Ver comandos disponibles | .claude/docs/COMANDOS_Y_SKILLS.md |
| Validar ortografía | .claude/rules/ortografia-espanol.md |
| Ejecutar tests | `tests/run_all_tests.R` |
| Revisar decisiones/código | .claude/rules/detractor-obligatorio.md |
| Routing de modelos (Opus/Sonnet/Haiku) | .claude/rules/modelo-routing-obligatorio.md |
| Stress test visual multi-semilla | .claude/skills/stress-test-visual/SKILL.md |
| Revisar ejercicio SCHOICE existente | .claude/skills/revisar-schoice/SKILL.md |
| Revisar ejercicio CLOZE existente | .claude/skills/revisar-cloze/SKILL.md |
| Pipeline end-to-end SCHOICE (11 pasos) | .claude/agents/orquestador-schoice.md + `/orquestador-schoice` |
| Pipeline end-to-end CLOZE (11 pasos) | .claude/agents/orquestador-cloze.md + `/orquestador-cloze` |
| Ver sintaxis oficial de R/exams (referencia externa, NO estándar ICFES) | `SOURCES/plantillas/rexams-oficiales/CATALOGO.md` |
| Saber si un canal es estructural antes de rediseñar | .claude/rules/diversidad-sustantiva.md §P7-F |
| Entender por qué un cero puede no probar nada | .claude/docs/patrones-errores-conocidos.md Error 37 |
| Historial de cambios de este índice | `.claude/docs/CHANGELOG_CLAUDE_MD.md` |

### ⚙️ Configuración del Sistema

- **Settings Claude**: `.claude/settings.json`
- **CI/CD**: `.github/workflows/ci-testing.yml`
- **Tests**: `tests/testthat/` (25 suites enganchadas a `tests/run_all_tests.R`)
- **Hooks**: `.claude/hooks/` (2 scripts activos cargados por settings.json)

---

## 📌 Metainformación

**Versión**: 3.27.0 (§P7-F enumerar el espacio · alcance del detractor a overrides e invariantes · H-4 reforzada)
**Fecha**: 2026-09-13
**Basado en**: Documentación oficial Claude Code (nov 2025)
**Historial de cambios**: `.claude/docs/CHANGELOG_CLAUDE_MD.md` (v2.6 → actual; no se importa) · `.claude/docs/CHANGELOG.md` (v2.2 → v2.5)
**Copia íntegra del índice anterior (v3.24.0) y script de reversión**: `.claude/backups/2026-09-07-recorte-contexto/`
**Reversión de las fases 2 y 3 (OpenClaw + detractor heterogéneo)**: `.claude/backups/2026-09-08-fase2-3/revertir.sh`

---

**Principio Fundamental**: Este sistema garantiza calidad mediante validación automática permanente. NO hay forma de evadir las protecciones de testing. Toda modificación es validada antes y después de su aplicación.
