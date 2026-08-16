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
   Desde la v1.2: el detractor DEBE ser un agente **distinto** del que escribió o corrigió el
   artefacto (autoevaluación ≠ FASE 2C), su reporte se considera entregado sólo si cierra con
   el marcador `VEREDICTO_DETRACTOR:`, y si no entrega tras 2 intentos se **escala al usuario**
   — PROHIBIDO sustituirlo por la auditoría propia del coordinador y sellar `detractor_fase2c`.
10. **Validación _neg_ opciones repetidas** → @.claude/rules/validacion-neg-opciones-repetidas.md
11. **Contextos narrativos creativos** (no mecánicos) → @.claude/rules/contextos-narrativos-creativos.md
12. **Validación semántica automática** (Nivel 4: descripción ↔ datos) → @.claude/rules/ejercicios-metacognitivos.md (sección Validación Semántica)
13. **Validación correctitud respuesta** (Nivel 5: multi-semilla + cross-check) → @.claude/rules/validacion-correctitud-respuesta.md
14. **Routing de modelos obligatorio** (Opus/Sonnet/Haiku por complejidad) → @.claude/rules/modelo-routing-obligatorio.md
15. **Stress Test Visual** (FASE 2H: renderizado masivo + análisis anomalías) → @.claude/skills/stress-test-visual/SKILL.md
16. **Workflow State Enforcement** (gate mecánico PreToolUse + estado persistente) → @.claude/rules/workflow-state-enforcement.md
17. **Infraestructura `.claude/` protegida** (backups + verificación de invariantes I-1 a I-7 antes/después de plataformas externas) → @.claude/rules/infraestructura-protegida.md
18. **Markdown-imágenes-PDF (atributo `{width=...}` obligatorio)** → @.claude/rules/markdown-imagenes-pdf.md
    Toda imagen `.png/.jpg/.svg/.pdf` emitida vía Markdown (directa o `cat()`) en `.Rmd` DEBE incluir atributo `{width=...}`. Pandoc ≥3.2.1 sin width envuelve el `\includegraphics` en `\pandocbounded`. **Actualizado 2026-08-15**: R/exams ≥ 2.4-1 ya define ese macro como **no-op** en todas sus plantillas, así que el `Undefined control sequence` **ya no se reproduce** con las plantillas del paquete (sí con plantilla propia). La regla sigue vigente por otra razón: al ser no-op, `\pandocbounded` **no controla el tamaño**. En la misma pasada se retiró el **Patrón B** (condicional `is_latex_output()`), que **pierde la imagen en el PDF**. Coupled con regla #6 ampliada. Errores 16-17 documentados.
19. **Solution letter-independence** (NUNCA `r letra_correcta` ni "Opción [A-D]" en Solution) → @.claude/rules/solution-letter-independence.md
    Defensa permanente contra Error 19. La sección Solution debe identificar opciones por contenido/código de error, NUNCA por letra/posición, porque Moodle (y otros LMS) pueden re-shufflear las opciones de forma independiente al `exshuffle` de R-exams, rompiendo coherencia letra ↔ contenido para el estudiante. Capas: hook FASE 2J + test_letter_independence.R + detractor.
20. **Markdown-tablas-pandoc (guard contador `none`)** → @.claude/rules/markdown-tablas-pandoc.md
    Defensa permanente contra Error 21. Todo `.Rmd` con tabla Markdown (`kable(format="markdown")` o `cat("| ...")`) DEBE incluir, al inicio de `Question`, el bloque raw LaTeX `` ```{=latex}\makeatletter\@ifundefined{c@none}{\newcounter{none}}{}\makeatother``` ``. pandoc ≥3.7 (RStudio bundlea 3.8.3, distinto del 3.6 de terminal) envuelve `longtable` con `\def\LTcaptype{none}`, contador que la plantilla de R-exams no define → `exams2pdf/exams2nops` fallan con `No counter 'none' defined`. Gemelo del Error 16. Capas: generación (skills + orquestador) + hook FASE 2K (`ERR_TABLA_NONE`) + test_markdown_tablas_none_guard.R + validación con pandoc de RStudio.
21. **Familias de Soluciones Reutilizables** → @.claude/rules/familias-soluciones-rmd.md
    Índice operativo de patrones probados + librería de helpers `@.claude/scripts/snippets_familias_rmd.R`. Aplicar las familias relevantes en toda generación/corrección: **F1** generación sin cuelgue (`pick_int`/`construir_valores_con_rango`, nunca `repeat` sin cota — Error 22); **F2** tablas responsivas cross-formato (`tabla_responsiva`, fenced div `::: {style=overflow-x:auto}` que sobrevive DOCX como `<w:tbl>` y PDF como longtable); **F3** ecuaciones display responsivas (`eq_display`); **F4** coherencia de marcas en CLOZE (sol alineado por construcción + verificación marca-vs-verdad); **F5** trampa `sample(escalar)` (`pick_int`/`safe_sample`); **F6** opciones gráficas de diagramas vectoriales cardinales (`dibujar_diagrama_cardinal`/`orientaciones_cardinales`/`seleccionar_combinacion_con_cascada`/`renombrar_opciones_neutral`: orientación sorteada por versión, cascada de umbrales de legibilidad en vez de umbral único con `stopifnot`, renombrado neutral POST-mezcla y distractores que conserven la magnitud de la correcta — Errores 23-26). Test: test_data_generation_no_hang.R.
22. **Diversidad Sustantiva** (respuesta correcta debe variar entre versiones, no solo el envoltorio narrativo) → @.claude/rules/diversidad-sustantiva.md
    Defensa contra diversidad cosmética. Un conteo alto de "versiones únicas del render" NO garantiza que los datos numéricos / contenido gráfico de la respuesta correcta cambien entre semillas. Prohibido: parámetros hardcoded como literales, PNGs de opciones copiados con `file.copy`. Defensa: hook FASE 2N (`WARN_DIV_ESTATICA`) + script `validar_diversidad_sustantiva.R --n 100` en orquestador paso 9 (`ERR_DIV_COSMETICA` bloqueante) + test_diversidad_sustantiva.R. Incidente: `desplazamiento-avion-aeropuerto` (2026-06-27) — 288/300 versiones únicas con respuesta correcta invariante. **§P7 (v1.5, 2026-08-15) — cierre por familias de dimensión**: a diferencia de P1-P6, no nombra un canal de fuga sino un defecto **del verificador**. Toda batería de reglas de eliminación debe cerrar por las **seis familias** (magnitud, divisibilidad, signo, posición, formato, léxico), declarando las inaplicables; calibrar contra el **techo nulo** permutando la clave (un máximo sobre muchas reglas está inflado por selección: 69,6 % observado contra 34,8 % de techo); y declarar **NO CONCLUYENTE** cuando el máximo cae a menos de 5 pp del umbral. *Una batería incompleta no mide «sin señal», mide «sin sonda»* — el canal real (47,4 %) estaba en la única familia sin sonda. Helper: `.claude/scripts/bateria_eliminacion.R`; test: test_bateria_eliminacion.R (suite 32).
23. **Muestra estándar de validación: N = 100** → @.claude/rules/muestra-estandar-validacion.md
    **Toda medición estadística sobre versiones usa `N = 100`.** Un único número, cableado en código ejecutable, NO elegible por sesión, agente ni handoff. Aplica a `validar_diagnosticidad.R`, `validar_diversidad_sustantiva.R`, `validar_multisemilla.R`, verificadores propios del ejercicio y smokes: invocarlos **sin `--n`** ya da el estándar. Origen: el repo tenía **cinco tamaños rivales** (5/10/20/30/40) y ninguna fuente única, así que cada agente elegía el suyo —algunos 400—; la instrucción verbal del profesor no se sostuvo porque no vivía en nada ejecutable. Excepción **declarada**: las muestras de **renderizado real** (`stress_test_visual.R`, `auditor-visual-html`) cuestan un PDF o una captura por unidad — su N debe **declararse siempre en el reporte** junto al resultado. NO confundir con el umbral de producto de 250+ versiones únicas sobre 300 (regla #3), que no cambia. Timeout del hook subido a 300 s para que quepa (170 s medidos). Test: test_muestra_estandar.R (suite 29).
24. **Hermes — triaje y fidelidad de figuras de cuadernillo** → @.claude/rules/hermes-imagenes-icfes.md
    Estrategia importada desde Todo-Pajaro (`motor-hermes`, v1.9.0, 13 lecciones validadas sobre lotes reales 2026-07-03 → 2026-08-05). Antes de reproducir CUALQUIER figura de un ítem escaneado hay que **mirar el recorte del JPG**: la descripción textual (`[FIGURA: …]`, ficha de alineación) SOBRE-clasifica sistemáticamente. Cinco exigencias: **H-1** gate visual (la decisión `flujo_b` se justifica con lo VISTO, no con el `.md`); **H-2** ⛔ la trampa deliberada ES la pregunta — reproducir la figura *incluidos sus errores*, jamás normalizar (incidente Q067: "corregir" la gráfica habría hecho verdadera una opción falsa), con screening de 7 patrones de enunciado; **H-3** gate de fidelidad **por tipo** en 4 ramas — celda-a-celda para tablas, **inventario bidireccional de rótulos** para geometría (atrapa la etiqueta *agregada* que un checklist de forma no ve), checklist dirigido para curvas; **H-4** ancla en el número IMPRESO (los mapeos página↔pregunta tienen desfase acumulado) y crop al borde del contenido; **H-5** asimetría de seguridad — endurecer es autónomo, **relajar nunca**. El motor ejecutable NO se forkea aquí: fuente única en `$MOTOR_HERMES` de Todo-Pajaro. Copia congelada de la estrategia: `.claude/skills/hermes-imagenes/SKILL.md`.

25. **Glifos Unicode que rompen pdflatex** → @.claude/rules/glifos-latex-prohibidos.md
    Un `✓` (U+2713) **literal** en texto Markdown visible impide compilar el PDF, y el fallo es **invisible en HTML** porque no pasa por LaTeX: por eso sobrevivió meses en `03-En-Produccion/` sin que nada lo detectara — ningún validador del arsenal miraba los caracteres del fuente. **110 glifos medidos** con `exams2pdf()`, y tres resultados contradicen la intuición: las flechas `← ↑ →` **compilan** pero `↔ ⇒ ⇔ ↺` no (el bloque Unicode no es homogéneo); el modo math **no salva** (`$a ≤ b$` falla igual, hay que usar `$\le$`); y un glifo **sólo en comentario R** es inocuo. La severidad está **calibrada**, no elegida, midiendo causalmente los 63 `.Rmd` afectados (render del original vs. del mismo archivo con los glifos sustituidos): en **Markdown** acierta 16/16 → `ERR_GLIFO_LATEX` bloqueante; en **código R** sólo 1/24 → `WARN_GLIFO_LATEX`, porque bloquear ahí habría marcado 23 archivos que sí compilan (una cadena R puede no emitirse nunca). Tildes españolas y `× ÷ ° ² → — •` están medidos como seguros y NO se tocan. Capas: detector único `validar_glifos_latex.R` + hook FASE 2O + `test_glifos_latex.R` (suite 33) con allowlist de 29 legacy que **no admite altas**.

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
| Ver sintaxis oficial de R/exams (referencia externa, NO estándar ICFES) | `SOURCES/plantillas/rexams-oficiales/CATALOGO.md` |

### ⚙️ Configuración del Sistema

- **Settings Claude**: @.claude/settings.json
- **CI/CD**: @.github/workflows/ci-testing.yml
- **Tests**: `tests/testthat/` (25 suites enganchadas a `tests/run_all_tests.R`)
- **Hooks**: `.claude/hooks/` (2 scripts activos cargados por settings.json)

---

## 📌 Metainformación

**Versión**: 3.22.0 (un umbral absoluto mide, en parte, el tamaño de tu propia batería)
**Fecha**: 2026-08-15
**Basado en**: Documentación oficial Claude Code (nov 2025)

### Cambios v3.22.0 (2026-08-16)

> El repositorio llevaba meses juzgando la diagnosticidad de sus ítems contra un umbral que
> **nadie había calibrado contra nada**. Al medirlo sobre 468 ítems oficiales del ICFES resultó
> ser inalcanzable —ninguna población, con 19 a 91 reglas, pasó del 47 %— y, peor, resultó medir
> en parte **el tamaño de la propia batería**. Un ítem no puede estar aprobado y rechazado a la
> vez, y hasta hoy lo estaba.

- **LA VARA QUE FALTABA: 468 ítems oficiales, 6 cuadernillos.** Se midió la batería de reglas de
  eliminación sobre el corpus real para saber si el 47 %/67 % de un ejercicio generado era un
  defecto nuestro o el nivel normal de un ítem bien construido. **La métrica ya traía su propio
  nulo exacto** y eso es lo que hizo comparables las poblaciones sin inventar equivalencias: con
  `score = 1/|S|` si la clave sobrevive, `E[score] = 1/n` **para toda regla, en toda instancia**,
  sea cual sea su selectividad (verificado por enumeración).
- **RESULTADO: en lo comparable, EN LÍNEA.** Vara universal limpia: oficiales **+0,4 pp**, nuestro
  ejercicio **−1,1 pp**. Vara de valor: oficiales **−2,1 pp**, nuestro **+0,9 pp**. Las cuatro son
  ruido alrededor del techo nulo. **El 47 %/67 % no tiene contraparte medible**: exige la
  estructura «punto H + pendiente p/q», y en 468 ítems oficiales existe **exactamente uno** con
  ella. No es que los oficiales pasen la prueba — es que la prueba no se les puede aplicar.
- **DOS AUTOCORRECCIONES DEL PROPIO MEDIDOR**, ambas en contra de su primer titular. (a) Su lectura
  inicial ponía el ítem oficial en el **percentil 100** de explotabilidad; al deduplicar vio que en
  ese ítem la celda contiene **una sola** opción, de modo que 15 reglas distintas compartían
  superviviente y se contaban 15 veces — con dedup baja al **percentil 27**, y **82 % de nuestras
  versiones son iguales o peores**. (b) Detectó un sesgo posicional en el corpus (clave en C el
  34,8 %, χ²=42,26) y **lo retiró como afirmación sobre el ICFES**: ninguna clave de ese corpus es
  oficial, se derivaron por votación de modelos, y un sesgo de los modelos es indistinguible del
  editorial con esos datos.
- **§P7 → v1.6: EL VEREDICTO LO DECIDE EL EXCESO**, no la tasa absoluta. Razón medida: la misma
  población da **27,4 % con 19 reglas y 34,8 % con 25**, y el techo nulo se mueve con ella
  (27,0 → 27,8). **El exceso es invariante; la tasa no.** Cortes calibrados por simulación bajo H0
  (500 réplicas, k ∈ {6,19,25,74,91}, N ∈ {30,100,300}): **+2 pp ≈ 1 sd** a N=100 y **+8 pp ≈
  p99,5**. El +7,0 pp del corpus oficial **no es ruido** —a N=468 la sd baja a 1,0 pp, son ~7 sd—
  y por eso queda en zona gris: *un ítem que no filtra más que el examen real no se declara
  defectuoso, pero tampoco se absuelve*.
- **RECONCILIACIÓN MECÁNICA, NO POR PROSA.** El helper daba `PASS` (43,5 % < 70 %) al mismo
  artefacto que `auditoria_propia.R` rechazaba por su 45 %. Ahora el verificador hace `source()`
  del helper y **aborta si no lo encuentra**: los cortes tienen **una sola fuente**. Se unificó
  además la convención de puntuación, que era la otra mitad del problema — el propio archivo
  declaraba que las dos cifras «no son comparables aunque coincidan».
- **EL TECHO NULO DEL CIERRE CRUZADO, QUE NUNCA SE CALCULÓ BIEN.** `(K4)` lo estimaba con **`max`
  sobre 3 réplicas**: un estimador sesgado al alza, e inflar el techo **rebaja** el exceso, así que
  **el sesgo iba a favor del artefacto auditado**. Con media sobre 8 réplicas: 67,0 % contra
  **36,3 %** (sd 1,0) ⇒ exceso **+30,8 pp**, casi 4× el corte.
- **EL BORDE DE 0,1 pp NO EXISTÍA.** Se había reportado el ejercicio en +8,1 pp contra un corte de
  +8 — la clase de coincidencia que hace sospechar de quien refija un umbral. Era **artefacto de
  la convención vieja**, que deprimía el máximo e inflaba el techo. Con nulo exacto, la misma
  batería sobre los mismos datos mide **+13,4 pp**. **El cambio de criterio endureció el ejercicio,
  no lo indultó**: pasó de 4 a 5 errores y el helper de `PASS` a `BLOQUEA`.
- **RESIDUO DECLARADO POR EL PROPIO CAMBIO**: el criterio por exceso abre un vector que el absoluto
  no tenía — **rellenar la batería con reglas implausibles sube el techo nulo sin mover el máximo**
  y erosiona el exceso. El helper no puede detectarlo (no sabe qué regla es plausible). Mitigado
  con regla de conducta en §P7 y publicando `exceso_atomico`, independiente de k.

- **REGLA #25 — GLIFOS UNICODE QUE ROMPEN pdflatex** (`glifos-latex-prohibidos.md`). Un ejercicio
  de `03-En-Produccion/` llevaba tiempo sin compilar en PDF por un `✓` (U+2713) en encabezados
  Markdown de su Solution, y **ningún validador miraba los caracteres del fuente**.
- **SE MIDIERON 110 GLIFOS UNO A UNO en vez de suponer**, y la mayoría de los «sospechosos»
  **compilan**: `← ↑ →` sí y `↔ ⇒ ⇔` no; `•` sí y `▪ ‣` no; **todos** los tipográficos
  (`— – " " … ° ‰`) sí; `± × ÷` sí. De **213** `.Rmd` con no-ASCII, **196 no requerían nada**.
- **CLASIFICACIÓN CAUSAL, no correlación**: sólo cuenta como roto si el original falla **y** el
  mismo archivo con el glifo sustituido compila. **17 rotos de 63 candidatos.** Y el dato que
  calibra el gate: zona **Markdown 16 rotos / 0 inocuos (100 %)** frente a zona **código R 1 / 23
  (4 %)** — de ahí las dos severidades. Bloquear en código R habría marcado 23 archivos sanos.
- **UN BUG DEL PROPIO GATE, cazado por su control end-to-end**: la fase se escribió con ruta
  relativa y el hook no hace `cd`, así que **nunca se ejecutaba**. El test del detector aislado
  pasaba en verde. *Un gate mudo se ve igual que un gate limpio.*

- **EL CORRECTOR DE ORTOGRAFÍA NO SÓLO CALLABA: CORROMPÍA.** Además del exit ciego (v3.21.0),
  `esta_en_string()` trataba **todo lo entrecomillado** como texto visible corregible, sin
  distinguir el valor de un atributo HTML de la prosa. `class="ex-opcion"` se reportaba como falta
  y `--fix` lo «corregía» a `class="ex-opción"`: **una tilde dentro de un identificador CSS**.
  Detectado al revisar el diff antes de commitear, cuando ya había entrado en 3 `.Rmd` de
  producción dejándolos inconsistentes con el cuarto de su familia.
- **EL FIX PROTEGE, NO EXCLUYE**: blinda el bloque `atributo="valor"` completo durante el
  reemplazo, así que en una **línea mixta** corrige el texto visible y deja el atributo intacto.
  Excluir la línea entera habría apagado el corrector del que depende la regla #7 — el control
  negativo anti-sobre-exclusión es el que autoriza el cambio.
- **LA PRUEBA DE QUE EL DEFECTO ERA DE LA HERRAMIENTA**: un cuarto `.Rmd` de la misma familia,
  **sin modificar en el repositorio**, también estaba bloqueado y **se desbloqueó sin tocarlo**.
  Si se hubieran «corregido» los archivos para que pasaran, hoy habría tildes en identificadores
  CSS en cuatro ficheros de producción y el defecto seguiría vivo.

- **PRODUCCIÓN: la tabla de la Solution no llegaba a HTML/Moodle** en 3 `.Rmd` de
  `Probabilidad-Intervalos-Curva-13-S1-2024B` (inmutabilidad levantada por autorización explícita
  del profesor). `include_tikz()` con su `markup="tex"` por defecto emite LaTeX crudo que **pandoc
  descarta al escribir HTML**, y la pérdida **no deja señal visual**: queda un `<p></p>` y el texto
  fluye alrededor. Medido 5 → **6 `<img>`**, verificado por hash perceptual; PDF byte-idéntico.
  El XML exportado (39 MB, 300 preguntas) tenía **0 menciones** de `tabla_solucion`.
  Despliegue bajado a **100** (`copias <- 300`, `archivos = 250`) por el estándar del profesor.
- **DEUDA DECLARADA, NO SALDADA**: los bloques `(I)` y `(K)` de `auditoria_propia.R` siguen con
  umbrales **absolutos** sin techo nulo propio. Hoy rechazan igualmente, así que no cambian ningún
  veredicto — pero arrastran el mismo defecto de escala que esta versión corrige.

### Cambios v3.21.0 (2026-08-15)

> Tres encargos independientes con un hilo común: **un verificador que no puede fallar no
> verifica**. Uno salía con 0 imprimiendo sus propios errores; otro medía complejidad con una
> regla que confundía dos escalas distintas; el tercero declaraba «sin señal» lo que en realidad
> era «sin sonda».

- **TERCER GATE CIEGO DEL REPOSITORIO, MEDIDO Y CERRADO** (tras la FASE 2G y la FASE 2I).
  `corregir_ortografia_espanol.R` imprimía `ERRORES ORTOGRÁFICOS ENCONTRADOS: 15` y **salía con
  exit 0**. No era una hipótesis: se midió con fixture y redirección a archivo (nunca por tubería).
  Causa en el código: `corregir_archivo()` devuelve `invisible(FALSE)`, pero el bloque
  `if (!interactive())` **descartaba ese retorno y nunca llamaba a `quit()`**, así que Rscript se
  caía por el final del script con 0.
- **CONTRATO DE EXIT NUEVO**: `0` nada pendiente · `1` errores auto-corregibles sin aplicar ·
  `2` sólo casos ambiguos (`REVISION_MANUAL`, que `--fix` no toca por diseño). Los cinco escenarios
  verificados uno a uno, y el mutante sobre **copia** (revertir a `quit(status = 0)`) confirma que
  la aserción distingue: mutante 0, real 1.
- **LOS CONSUMIDORES SE COMPROBARON ANTES DE TOCAR EL EXIT**, que era la parte con riesgo:
  `.git/hooks/pre-commit` tiene `set -e` pero neutraliza el status con `|| true` y decide por
  `grep "ERRORES"`; el hook plantilla usa **tubería**, cuyo exit es el del `grep`; y
  `run_one_suite.R` sólo cuenta `failed`/`error`, no *warnings*. **Ninguno se rompe** — verificado
  simulando ambos hooks y corriendo las dos suites que invocan el script.
- **EL ESTADO NO VIAJA POR EL VALOR DE RETORNO**, a propósito: `corregir_directorio()` lo consume
  con `sapply` + `sum(!resultados)`, así que cambiarlo a entero **invertiría en silencio** el
  conteo del resumen. Va por un entorno propio y el contrato lógico queda intacto.
- **5 tests nuevos** en `test_ortografia_espanol.R` con control positivo (faltas → exit ≠ 0),
  negativo (limpio → 0), ambiguo (→ 2), post-`--fix`, y **uno que comprueba que el `pre-commit`
  sigue leyendo la salida y no el exit** — es el que autoriza el cambio.

- **LA TABLA DOK↔NIVEL LLEVABA 6 MESES EQUIVOCADA** (regla de ejercicios metacognitivos → **v1.1**).
  Marcaba **DOK 2 como «incompatible» con N3/N4**, lo que sólo es cierto si el Nivel mide
  complejidad del ítem. **No la mide.** Verificado contra el catálogo canónico
  (`niveles-mat.json`, `CANONICO_INMUTABLE`), no citado de segunda mano:

  | Evidencia | Cita literal |
  |---|---|
  | `puntajes_oficiales` | `"N1": "0 a 35"` · `"N2": "36 a 50"` · `"N3": "51 a 70"` · `"N4": "71 a 100"` |
  | `encabezado_oficial` | `"El evaluado que se ubica en el nivel {N}, demuestra que…"` |
  | `grep -ri 'DOK\|Bloom\|Webb'` sobre los 6 archivos del catálogo | **cero coincidencias** |

- **EL SUJETO GRAMATICAL ES «El evaluado», no la pregunta**: el Nivel es una **banda de puntaje**
  del estudiante, no una escala cognitiva del ítem. De ahí que **un ítem rutinario pueda ser
  empíricamente difícil** (pendiente fraccionaria negativa, manejo de signos) y discriminar en N4
  con DOK 2. La tabla vieja empujaba a **inflar el DOK declarado** para cuadrar con el Nivel: el
  ítem no cambiaba, cambiaba la etiqueta.
- **SE CONSERVA LO QUE SÍ SE SOSTIENE**: `DOK ≥ 3 ⇒ Nivel ≥ 3` es una implicación **en una sola
  dirección** y sigue vigente; la recíproca `Nivel ≥ 3 ⇒ DOK ≥ 3` queda declarada **falsa**, con
  tabla de las cuatro implicaciones y su veredicto.
- **CHECKLIST CORREGIDO**: «¿Bloom incluye Analizar/Evaluar?» exigía algo que un ítem legítimo de
  la competencia **Formulación y ejecución** no cumple (su verbo es *Aplicar*). Pasa a comprobar la
  **correspondencia** entre el verbo declarado y lo que el ítem exige.
- **ALCANCE VERIFICADO, no supuesto**: `grep -rn 'DOK' .claude/scripts/ SOURCES/scripts_validacion/
  tests/ .claude/hooks/` → **0 coincidencias**. Ningún gate ejecutable leía la tabla, así que la
  corrección es documental. Esa misma ausencia es la razón por la que la redacción importa: es lo
  único que sostiene el criterio.

- **REGLA #22 → v1.5, NUEVO §P7: cierre por familias de dimensión.** A diferencia de P1–P6, no
  nombra un canal de fuga: nombra un defecto **del verificador**. La lección ya había salido en
  **dos ejercicios distintos** y sólo vivía en el verificador de uno.
  *Una batería incompleta no mide «sin señal», mide **SIN SONDA**.* En el incidente, seis reglas
  intra-celda y **ninguna** tocaba la divisibilidad — donde estaba el canal real (**47,4 %**).
- **TRES EXIGENCIAS**: (1) **cobertura** de las seis familias (magnitud, divisibilidad, signo,
  posición, formato, léxico), con declaración justificada para las inaplicables; (2) **techo nulo**
  por permutación de la clave con las reglas intactas —medido: máximo **69,6 %** contra techo
  **34,8 %**, exceso **+35 pp**; sin esa calibración el número no significa nada—; (3) **banda de
  incertidumbre** de 5 pp, porque a N = 100 un máximo sobre ~19 reglas no es reproducible tirada a
  tirada. `NO_CONCLUYENTE` sale con **exit 1**: redondearlo a `PASS` es justo el fallo a evitar.
- **NUEVO HELPER**: `.claude/scripts/bateria_eliminacion.R` (`nueva_regla`, `evaluar_bateria`,
  `imprimir_bateria`, `exit_bateria`). Sólo la parte **genérica**; las reglas siguen siendo por
  ejercicio, porque la divisibilidad sólo aplica a claves enteras y el signo sólo donde hay
  negativos.
- **CUARTO GUARDIÁN, aparecido al construirlo**: `UMBRAL_DEGENERADO`. Si el techo nulo alcanza el
  umbral, **el umbral no discrimina** —hasta una batería de ruido lo cruzaría—. Un gate que
  siempre falla se aprende a ignorar igual que uno que nunca falla.
- **NO SE CABLEÓ EN `validar_diagnosticidad.R`, y se explica por qué**: una batería automática de
  divisibilidad sobre opciones textuales no aplicaría nunca y el script imprimiría `PASS` sobre una
  familia que jamás sondeó — recreando el defecto que §P7 cierra. Mismo criterio que fijó H3b:
  **declarar la ceguera vale más que añadir una sonda débil.**
- **NUEVA SUITE (32 en el runner)**: `test_bateria_eliminacion.R`, 33 aserciones. Control decisivo:
  **«mismos datos, sonda retirada»** — con el canal real al 100 %, quitar esa sonda hace que la
  batería reporte **19 %** (cifra baja y tranquilizadora) y el helper **siga negándose a dar PASS**.
- **DOS BUGS DEL PROPIO TEST, cazados por sus controles**: (a) `expect_lt`/`expect_gt` **no aceptan
  `info =`**; tres aserciones reventaban por eso y no por el helper. (b) El caso de la banda se
  construyó acercando el umbral al máximo de un ítem **sano**, lo que hunde el umbral por debajo del
  techo nulo y hace que el veredicto correcto sea `UMBRAL_DEGENERADO`: **el helper tenía razón y el
  escenario estaba mal**. Además, sortear el canal con probabilidad 0,66 aterrizó en **61 %** y sacó
  al test de su propia banda — la irreproducibilidad de la exigencia (3), reproducida dentro del
  test que la prueba. El fixture pasa a un conteo **exacto**.

- **VECTOR DE REINYECCIÓN QUE SEGUÍA ABIERTO**: el barrido de `is_latex_output` dejó las 12
  referencias de `.claude/` como históricas con advertencia, **salvo `snippets_familias_rmd.R`** —
  la librería de helpers que la regla #21 manda **copiar dentro del `.Rmd`**, y que conservaba el
  patrón desnudo mientras su copia en el `.md` sí llevaba el aviso de «RAMA MUERTA». Anotadas ambas
  ocurrencias sin tocar el comportamiento (la decisión de no tocar las Familias 2 y 3 se respeta).

- **VERIFICACIÓN DE CIERRE DE DOS FIXES AJENOS** (ningún agente anterior los había comprobado):
  - **`markup = "markdown"` en producción: FUNCIONA**, con control negativo. HTML **5 → 6 `<img>`**
    en los 3 `.Rmd`, reproducible. **Pero la hipótesis compuesta sólo se cumple en 1 de 3**: los dos
    CLOZE **no compilan a PDF ni con el fix ni sin él**, por un `✓` (U+2713) en los encabezados de
    su Solution — defecto **preexistente e independiente**, que nadie había visto porque nadie había
    ejecutado `exams2pdf()` real sobre ellos.
  - **Los `n` de despliegue SÍ se habían bajado**: `archivos = 250 → 100` y `copias <- 300 → 100`
    (confirmado por `git diff`). El grep que no los encontraba buscaba `n = 300`, y el patrón real
    es `copias <-` / `archivos =`.
  - **Declarado, no corregido**: esos 5 archivos están modificados **sin commitear dentro de
    `03-En-Produccion/`**, que la regla #2 declara inmutable.

> La v3.20.9 midió que dos patrones documentados pierden la figura y los retiró. Faltaba la
> pregunta que de verdad importa: **¿cuántos ejercicios los estaban usando?** El barrido encontró
> que los dos patrones retirados **no tienen ni una instancia viva** — y, buscándolos, apareció un
> tercer modo de pérdida que nadie había descrito, activo en **tres ejercicios de producción**.

- **BARRIDO COMPLETO DE `A-Produccion/`**: 139 `.Rmd` con `include_tikz` o `is_latex_output`
  (59 en `01-`, 16 en `02-`, 30 en `03-`, 34 en `Ejemplos-Funcionales`); **218 usos** de
  `include_tikz` y **21 bloques** `is_latex_output`, clasificados uno a uno con medición cuando la
  lectura no bastaba.
- **LOS DOS PATRONES RETIRADOS NO ESTÁN INSTANCIADOS**: cero `.Rmd` con el Patrón B de la regla #18
  (`is_latex_output()` ? `\includegraphics` : `<img>`) y cero con el de `codigo-rmd.md` #1
  (`include_tikz` : `include_graphics`). El único fichero que combina ambos símbolos —
  `ortocentro_alturas_triangulo_geometria_n2_v1.Rmd` — tiene su `include_graphics` **comentado**.
  Las correcciones de la v3.20.9 son **preventivas**: no hay deuda que pagar en los ejercicios.
- **TERCER MODO DE PÉRDIDA, NO DOCUMENTADO HASTA HOY — `include_tikz()` sin `markup`.** Su valor por
  defecto es `markup = "tex"`, que emite **`\includegraphics` LaTeX crudo**; por la misma regla de
  enrutado que la v3.20.9 midió, eso **se descarta en HTML y en Moodle**. Medido sobre fixture:
  HTML de **544 B con 0 `<img>`** (un `<p></p>` vacío donde iba la figura) frente a **16.439 B con
  1 `<img>`** usando `markup = "markdown"`; en Moodle, **XML de 1.321 B con 0 `<img>`** frente a
  17.268 B con 1. PDF y DOCX conservan la imagen en ambos casos — por eso el defecto sobrevive a
  cualquier revisión que mire el PDF.
- **CONFIRMACIÓN NORMATIVA**: en los `.Rmd` **oficiales** de R/exams (`SOURCES/plantillas/`)
  `include_tikz` lleva **siempre** `markup = "markdown"` o `"none"`; el default `"tex"` solo aparece
  en los `.Rnw`, donde el destino sí es LaTeX. El default está pensado para Rnw, no para Rmd.
- **TRES EJERCICIOS DE `03-En-Produccion/` AFECTADOS** (familia `probabilidad_intervalos_curva`,
  6 usos): el chunk `solution_table`, con `results='asis'`, llama `include_tikz` sin `markup`, así
  que **la tabla de la Solution no llega al estudiante** en HTML ni en Moodle. Control positivo
  sobre **copia** mutada con `markup = "markdown"`: **5 → 6 `<img>`** (146.791 → 170.313 B). El
  enunciado y sus cuatro opciones **sí** llegan: se emiten aparte con `![](…){width=40%}`. Son
  **inmutables** (regla #2): se reportan, no se tocan.
- **EL CRITERIO QUE EVITA EL FALSO POSITIVO ES `results=`**, y costó dos pasadas descubrirlo: de los
  218 usos, **14 no pasan `markup`** y por tanto emiten LaTeX crudo — pero **8 de esos 14 viven en
  chunks `results="hide"`**, donde `include_tikz` actúa solo como **generador del PNG** y su markup
  se descarta (la imagen se emite después con `![](…){width=…}`, Patrón A). Clasificar por la
  llamada, sin mirar el chunk, habría marcado como rotos tres archivos de producción que funcionan.
  Reparto medido: **171** `markdown/asis` · **20** `none/hide` · **8** `ausente/hide` · **7**
  `markdown/hide` · **6** `variable/asis` (rama muerta) · **6 `ausente/asis` → ROTO**.
- **DOS DEFECTOS DE LA PROPIA SONDA, CAZADOS POR SUS CONTROLES**: (a) contaba `include_tikz()`
  mencionado en **comentarios** (`# NO llamamos a include_tikz() aquí`) — 4 falsos positivos, uno
  de ellos en un archivo que se habría reportado como roto sin serlo; (b) marcaba como «sin markup»
  las llamadas con `markup = <variable>`. Tras corregir ambas, el conjunto pasó de **8 archivos
  sospechosos a 3 rotos reales**.
- **`markup = markup_tikz` ES RAMA MUERTA, NO BUG**: 6 usos calculan
  `if (identical(typ,"moodle")) "moodle" else "markdown"`, y `"moodle"` **no es un valor válido**
  (`match.arg` lo rechaza: *'arg' should be one of "tex", "markdown", "none"*). No explota porque
  **`typ` nunca vale `"moodle"`**: verificado ejecutando `exams2moodle()` sobre el ejercicio real →
  **OK, 5 `<img>`**. Es el mismo idioma de rama muerta que la v3.20.9 marcó en las Familias 2 y 3.
- **LOS 21 BLOQUES `is_latex_output` SON INOCUOS**: 15 son los fenced div de `eq_display`/
  `tabla_responsiva` (ambas ramas emiten el mismo `$$…$$`; solo cambia el div que pandoc descarta),
  4 tienen la **rama viva en `markup = "markdown"`** — que sirve a los cinco destinos — y **2 no son
  usos: son comentarios** que documentan por qué NO usar el patrón. Medido sobre
  `volumen_cilindro_…_n2_v1.Rmd`: HTML con 1 `<img>`, 0 fugas de `includegraphics`.
- **EL RESIDUO DE LA v3.20.9 SON 7 UBICACIONES, NO 6 — Y UNA ES EJECUTABLE**:
  `.claude/skills/diagnosticar-errores/scripts/diagnosticar-error.R:33` responde al error `ERR_G1`
  («Graficas no visualizadas») con la recomendación literal *«Usar renderizado condicional con
  `is_latex_output()`»*. No es prosa: es el mecanismo por el que el patrón retirado **se reinyecta
  automáticamente** justo cuando alguien tiene una figura perdida. Debería ser el primero de los
  siete en corregirse.
- **VERIFICADO**: runner completo con `R_TESTS_FULL=1` → **31 suites ejecutadas de 31 declaradas,
  0 saltadas, 0 fallidas, cobertura 100 %, exit real 0** (medido por redirección a archivo, no por
  tubería). Invariantes **I-1..I-10 en verde** (214 aserciones, exit real 0), re-ejecutadas
  **después** de editar este archivo. Las dos suites de la v3.20.9 (30 y 31) están enganchadas al
  runner y pasan. Snapshot previo de `.claude/` disponible en
  `.claude.pre-fix-gates-20260815-111825.tar.gz`.
- **NUEVA REGLA #24 — HERMES, triaje y fidelidad de figuras de cuadernillo** (importada el
  2026-08-15 desde Todo-Pajaro, `motor-hermes` v1.9.0, 13 lecciones validadas sobre lotes reales
  2026-07-03 → 2026-08-05). Entró en el índice sin quedar registrada en el changelog, que es la
  misma deriva documental que la v3.20.0 tuvo que remediar en bloque. Cinco exigencias: **H-1** gate
  visual — la decisión `flujo_b` se justifica con el **recorte del JPG**, nunca con el `[FIGURA: …]`
  de la ficha, que SOBRE-clasifica; **H-2** la trampa deliberada **ES** la pregunta, así que la
  figura se reproduce *incluidos sus errores* (incidente Q067: «corregir» la gráfica habría vuelto
  verdadera una opción falsa); **H-3** gate de fidelidad **por tipo** en 4 ramas, con inventario
  **bidireccional** de rótulos en geometría — el que atrapa la etiqueta *agregada* que un checklist
  de forma no ve; **H-4** anclar en el número **impreso** (los mapeos página↔pregunta acumulan
  desfase); **H-5** asimetría de seguridad — endurecer es autónomo, **relajar nunca**. El motor
  ejecutable **no se forkea**: fuente única en `$MOTOR_HERMES`. Copia congelada de la estrategia en
  `.claude/skills/hermes-imagenes/SKILL.md` (21 KB) — puntero verificado, existe.
- **CORRECCIÓN AL DIAGNÓSTICO HEREDADO — `git worktree add` NO FALLA.** Se daba por inhabilitado el
  aislamiento en worktree para todo agente futuro. Ejecutado de verdad: **exit 0**, worktree usable
  (`git status` limpio, 499 `.Rmd`, PNGs materializados como binarios reales, 1139 archivos LFS).
  Lo que hay son **dos errores cosméticos** de `.git/hooks/post-checkout`, que es un **heredoc mal
  cerrado**: el propio `EOF` y la línea `chmod +x .git/hooks/post-checkout` quedaron **dentro** del
  script (de ahí `línea 6: EOF: orden no encontrada` y un `chmod` que falla porque en un worktree
  `.git` es un archivo, no un directorio). `core/poblar_ontologia.R` **sí existe** y el hook cumple
  su función antes de fallar. Fix: borrar las tres líneas sobrantes. **No commiteado**: `.git/hooks/`
  no está bajo control de versiones y el fichero es del entorno local del usuario.

### Cambios v3.20.9 (2026-08-15)

> Dos defectos de infraestructura que llevaban meses en pie **por la misma razón**: nadie había
> medido lo que la documentación afirmaba. Uno hacía que un gate bloqueante fallara siempre; el
> otro, que una rama documentada no se ejecutara nunca. Ambos pasaron todas las revisiones porque
> su efecto era invisible en la salida que se inspecciona.

- **LA FASE 2I MARCABA `ERROR 16` EN TODO `.Rmd`, TUVIERA O NO IMÁGENES.** El bloque 2I.2 usaba
  `grep -l 'pandocbounded'`, que **no distingue la definición del uso**. Desde **R/exams 2.4-1**
  todas las plantillas del paquete traen `\providecommand{\pandocbounded}[1]{#1}` (verificado en
  `exams/tex/{plain,plain8,exam,form,solution,plain-highlight}.tex` de la 2.4-2 instalada), así que
  ese `grep` casaba en cualquier `.tex`. Medido sobre `pendiente-rectas-paralelas-n4`, ejercicio
  **sin una sola imagen**: **6 de sus 8 `.tex` disparaban; usos reales = 0**. Y sobre un fixture con
  un chunk que dibuja: 2 disparos, ambos inocuos. Es la patología que este repo ya sufrió con la
  FASE 2G en falso rojo permanente — el día que aparezca un `\pandocbounded{\includegraphics…}` real
  sin definición, nadie lo mirará.
- **EL FIX NO ES «AFINAR EL GREP», ES MODELAR LA CONDICIÓN DE FALLO**: el `Undefined control
  sequence` exige **uso** (`\pandocbounded{`) **y ausencia de definición** en el conjunto del render.
  Se comprueban las dos cosas por separado, y la definición se busca en **todos** los `.tex` del
  render porque el preámbulo vive en `plain*.tex` mientras el uso está en `exercise*.tex`. Con eso
  el gate se auto-calibra: verde con las plantillas del paquete, rojo con plantilla propia o con
  exams < 2.4-1, que es exactamente el residuo que la regla #18 ya declaraba.
- **LAS DOS FIRMAS SE DECLARAN COMO VARIABLES** (`PB_USO_RE`, `PB_DEF_RE`) para que el test las
  **extraiga del hook real** en vez de mantener una copia paralela que podría divergir en silencio.
- **`ERROR 16` YA NO ES UN FALLO DE COMPILACIÓN, Y CONVIENE SABERLO**: medido con
  `cat("![](fig.png)\n")` **sin** width bajo `exams2pdf()` → el `.tex` lleva
  `\pandocbounded{\includegraphics[keepaspectratio]{fig.png}}` y **compila** (PDF de 15 KB con la
  imagen). La regla #18 **no se relaja**: el `NEWS.md` de exams dice que el macro es no-op y que el
  tamaño «still has to be controlled in one of the previously available ways», así que `{width=...}`
  sigue siendo obligatorio — por **tamaño**, no por compilación.

- **`knitr::is_latex_output()` ES SIEMPRE FALSE BAJO R/exams — la regla #21 afirmaba lo contrario.**
  Decía literalmente que es «el ÚNICO discriminador (TRUE = PDF/NOPS; FALSE = HTML/Moodle/DOCX)».
  Medido con una sonda que escribe el valor a disco desde `data_generation`, por los cinco
  pipelines: **html FALSE · pdf FALSE · docx FALSE · nops FALSE · moodle FALSE**. Causa
  arquitectónica, no bug de nadie: R/exams **siempre** teje a Markdown y delega la conversión en
  **pandoc**, de modo que durante el `knit` no hay destino LaTeX que detectar.
- **QUIEN ENRUTA ES PANDOC, POR TIPO DE BLOQUE** (medido con fixtures renderizados): Markdown
  `![](f.png){width=}` llega a los dos destinos; **LaTeX crudo** sobrevive solo a LaTeX y se
  descarta en HTML; **HTML crudo** sobrevive solo a HTML y **se descarta al escribir LaTeX**.
- **CONSECUENCIA MEDIDA — DOS PATRONES DOCUMENTADOS PERDÍAN LA FIGURA EN EL PDF**, y los dos en
  silencio (compilan, sin warning, ningún validador lo ve):
  - **Regla #18 Patrón B** (`is_latex_output()` ? `\includegraphics` : `<img>`): `.tex` con **0
    `\includegraphics` y 0 `<img>`**. PDF de 9 KB frente a 15 KB del mismo ejercicio con imagen.
    **RETIRADO.** Sustituto **Patrón B'** verificado: emitir **ambos** sin condicional y dejar que
    pandoc descarte el que no toca → PDF 1 `\includegraphics` / 0 `<img>`; HTML 1 `<img>` / 0
    `includegraphics`. Cero fuga cruzada.
  - **`codigo-rmd.md` regla #1** (`is_latex_output()` ? `include_tikz` : `include_graphics`): la
    rama TikZ nunca corre, y `knitr::include_graphics()` bajo R/exams emite un
    `<div class="figure"><img …></div>` que el escritor LaTeX descarta — en el `.tex` solo queda el
    texto literal `plot of chunk <nombre>`. **El patrón «corregía» una figura perdida en HTML
    perdiéndola en PDF.** Sustituto medido: `include_tikz(..., markup = "markdown")`, **una sola
    llamada sin condicional** → PDF `\includegraphics[width=8cm,…]{g.png}`, HTML `<img>`.
- **LAS FAMILIAS 2 y 3 NO SE TOCAN, Y SE EXPLICA POR QUÉ**: sus ramas `is_latex_output()` son código
  muerto, pero el efecto es **inocuo por accidente** — se emite siempre el fenced div y el escritor
  LaTeX lo descarta. Verificado sobre el repositorio completo: **0 de 487 `.tex` contienen
  `overflow-x`**, frente a 6 HTML que sí lo llevan. Cambiar los helpers habría divergido de las
  copias embebidas en los `.Rmd` sin arreglar nada. Se marcan las ramas como `RAMA MUERTA` para que
  nadie **copie el idioma** a un sitio donde las dos ramas emitan cosas distintas.
- **DOS SUITES NUEVAS (30 y 31 en el runner)**: `test_fase2i_pandocbounded_detector.R` (18
  aserciones; control positivo uso-sin-definición, dos controles negativos, el caso real
  uso+definición, y una aserción de regresión que **deja constancia medible** de que el detector
  viejo fallaba en 2 de 3 casos inocuos) y `test_is_latex_output_rexams.R` (17 aserciones; vuelve a
  **medir** el valor en runtime con `exams2html`/`exams2pandoc(latex)`/`exams2pdf`, más una guarda
  documental que impide que una regla vuelva a mostrar el condicional sin declarar que es FALSE).
  Ambos detectores son **funciones de la ruta**, ejercitados con mutantes en `tempdir()`; mutar
  archivos reales ya dejó una vez el arsenal roto en disco.
- **VERIFICADO**: `bash -n` del hook OK · suites nuevas 18/18 y 17/17 · mutante del hook (revertir al
  `grep` ingenuo) **cazado**, nombrando las tres aserciones que fallan · invariantes I-1..I-10 en
  verde · detector nuevo sobre `.tex` reales: 0 disparos donde el viejo daba 6.
- **RESIDUO DECLARADO, NO OCULTADO**: la creencia falsa sobre `is_latex_output()` está **propagada a
  otros seis archivos** que esta pasada NO corrige — `.claude/skills/corregir-error-imagen/`,
  `.claude/skills/corregir-graficos/`, `.claude/skills/diagnosticar-errores/` (los tres prescriben
  el condicional a la hora de **generar** código) y `.claude/docs/patrones-errores-conocidos.md`
  (§Error 1 y §Error 16). Cada uno usa una variante distinta del patrón y merece su propia
  medición antes de reescribirlo.

### Cambios v3.20.8 (2026-08-10)

> El repositorio tenía **dos nomenclaturas de archivo rivales conviviendo en nueve sitios**, y
> ninguna reconciliaba a la otra. Ganaba la equivocada por una razón puramente mecánica: era la
> única cableada en comprobaciones ejecutables. La pregunta del profesor —«¿estás nombrando el
> `.Rmd` según la nomenclatura oficial vigente?»— destapó que yo mismo acababa de reproducirla.

- **LA DERIVA, MEDIDA**: `NOMENCLATURA_ARCHIVOS_RMD.md` prescribía
  `[ejercicio]_[componente]_[competencia]_n[nivel]_v[N]` y **no estaba cableado en ningún sitio**.
  En paralelo, desde la regla de ejercicios metacognitivos (v3.1, 2026-02-06) creció un formato de
  facto, `[ejercicio]_metacognitivo_[competencia_corta]_n[nivel]_[tipo]_v[N]`, **sí cableado** en el
  regex de los dos comandos orquestadores, los dos skills generadores y la regla #10. Sobre los 142
  `.Rmd` tocados desde febrero: **53 seguían el de facto, 10 el documentado**.
- **QUÉ SE HABÍA PERDIDO**: la palabra `metacognitivo` es **constante** en todos los ejercicios
  desde que la regla #1 la hizo universal, así que ocupaba la ranura del **componente ICFES** sin
  aportar un bit; y la competencia se acortó, perdiendo su forma oficial.
- **DECISIÓN DEL PROFESOR**: vuelve a regir el formato documentado, **extendido con la ranura de
  tipo** y sin `metacognitivo`:
  `[ejercicio]_[componente]_[competencia]_n[nivel]_[tipo]_v[N].Rmd` (+ `_neg` opcional).
- **LA RANURA DE TIPO NO ES OPCIONAL, Y SE DEMOSTRÓ**: adoptar el formato documentado *literalmente*
  era **imposible**. El repo tiene **13 familias** con dos o tres variantes del mismo enunciado
  (`area_jardin_lote_…` schoice + cloze; `diagrama_caja_estaturas_…` con tres), que sin la ranura
  colisionan en el mismo nombre de archivo. Medido antes de proponer, no supuesto.
- **ALCANCE — de aquí en adelante**: allowlist legacy de **299 archivos** en
  `tests/testthat/nomenclatura-legacy.txt`, con cabecera que declara que **no admite altas y solo
  puede decrecer**. Los de `03-En-Produccion/` son inmutables (regla #2) y permanecen indefinidamente.
  Conformes hoy: 13.
- **TRES CAPAS, no prosa**: (1) **gate PreToolUse** que bloquea la **creación** de un `.Rmd` fuera de
  formato con un mensaje que enumera los cuatro errores frecuentes; (2) `test_nomenclatura_rmd.R`
  (**25 aserciones**, suite **28** del runner); (3) verificación de que las **citas** del formato en
  skills, comandos y reglas no vuelvan a divergir de la fuente única.
- **EL GATE SOLO MIRA AL CREAR**, nunca al editar: bloquear la edición dejaría **congelados** los 299
  legacy. Es lo que hace que «de aquí en adelante» sea implementable sin allowlist dentro del hook.
- **`NOMENCLATURA_ARCHIVOS_RMD.md` → v2.0**: fuente única declarada en la cabecera, semántica del
  sufijo `_neg` como **disparador mecánico** (`validar_5c_unicidad` hace
  `grepl("_neg_", basename(...))`, así que ponerlo en un ítem que no cumple la regla #10 vuelca al
  validador a la rama contraria), tabla nombre ↔ `exextra[...]`, rutas actualizadas a `01-/02-/03-`
  y §Historial con la medición de la deriva.
- **TRES BUGS DEL PROPIO TEST, cazados por sus controles**: (a) `system2(stdin=)` espera **una ruta
  de archivo**, no una conexión — con `textConnection()` el hook ni se ejecutaba y el fallo salía
  como «sh: línea 1: 5: No existe el fichero», que no parece un bug del test; (b) el discriminador
  de citas marcaba **referencias a archivos legacy reales** (`promedios_borrados_…` vive en
  `03-En-Produccion` y es inmutable: «corregirlas» habría roto punteros vivos); (c) y marcaba
  también la línea que **prohíbe** el patrón viejo — no distinguía «usa el patrón» de «advierte
  contra el patrón», castigando justo la documentación que arregla el problema. El discriminador
  final exige `_metacognitivo_` **entre guiones bajos** más un marcador de plantilla, y lleva sus
  dos controles negativos escritos.
- **Verificado**: `test_nomenclatura_rmd.R` 25/25 con el gate ejercitado de verdad (JSON por stdin,
  bloqueo real en 2 nombres inválidos, no-bloqueo en el válido y en la edición de un legacy).
  Runner completo: **28/28 suites, 0 fallidas, cobertura 100 %, exit real 0**.

- **REGLA #20 VERIFICADA EMPÍRICAMENTE POR PRIMERA VEZ (y una objeción refutada).** Una auditoría
  reportó que el guard `\newcounter{none}` era **inerte**, porque no aparece en
  `out_tex/pandoc1.latex`. La observación es cierta pero la inferencia era falsa: ese archivo es
  artefacto de `exams2pandoc(type="latex")`, **otra ruta de conversión** que la de `exams2pdf`.
  A/B controlado por el pipeline real (pandoc 3.8.3 de RStudio, misma tabla Markdown, única
  variable el guard):

  | Variante | Resultado |
  |---|---|
  | Sin guard | **FALLA** — `! LaTeX Error: No counter 'none' defined.` |
  | `` ```{=latex} `` (el que prescribe la regla #20) | **COMPILA** |
  | Chunk R con `results='asis'` | FALLA |
  | LaTeX crudo en línea | COMPILA |

  Conclusiones: (a) el **Error 21 sigue reproduciéndose** con pandoc ≥3.8.1, así que la regla #20
  protege algo real; (b) la forma prescrita **funciona**; (c) la variante por chunk **no** sirve
  —conviene saberlo antes de que alguien la proponga como alternativa «más limpia»—; y (d) que el
  hook FASE 2K compruebe el `.Rmd` y no el `.tex` es **correcto**, porque el guard actúa a nivel de
  fuente. Se había llegado a calificar ese check de «verde que no significa nada» por dar por buena
  la causa ajena sin comprobarla.
  **Residuo declarado**: en la salida de `exams2pandoc(type="latex")` el guard efectivamente no
  aparece; quien compile ESE `.latex` a mano no está protegido. No es la ruta con la que el repo
  produce PDFs.

### Cambios v3.20.7 (2026-08-10)

> La v3.20.2 diagnosticó bien el agujero —un agente de reporte que termina el turno en silencio— y
> lo cerró **solo en `agente-detractor.md`**. Los dos orquestadores, que son igual de agentes de
> reporte y además los que más caro salen por corrida, se quedaron fuera. Volvió a pasar hoy.

- **INCIDENTE 2026-08-10**: un `dry-run` de `orquestador-schoice` sobre `MAT-2026-1-010` terminó
  **dos veces consecutivas** en notificación de «disponible» sin reporte —una tras el reclamo del
  protocolo de no-entrega, con agente nuevo y contexto limpio—. En `dry-run` el daño es total: el
  plan auditado es el **único** producto y no deja rastro en disco, así que no hay nada que
  reconstruir. El disco vacío es además **el estado correcto** de un dry-run, de modo que
  «no dejó archivos» no distingue «auditó y calló» de «no hizo nada».
- **CONTRATO DE ENTREGA en AMBOS ORQUESTADORES**, calcado del que ya funciona en el detractor: el
  texto final de retorno ES el reporte; prohibido terminar sin él, escribirlo a un archivo o
  anunciarlo; entrega parcial declarada si se agota el presupuesto. Cierra con el marcador
  `VEREDICTO_ORQUESTADOR: completado | parcial | abortado | dry_run | preflight_failed`.
- **EL MARCADOR REUSA EL VOCABULARIO EXISTENTE**, no inventa uno paralelo: sus cinco valores son
  los mismos del campo `exit_status` del contrato de salida, y el test compara ambos **como
  conjuntos**. Dos sitios espejo del mismo vocabulario es justo la forma de deriva que este repo
  ya sufrió con el «mínimo 4» de WAIT_USER #3 frente al V4 de 6.
- **CLÁUSULA EXPLÍCITA DE `dry-run`**: el contrato dice que también aplica ahí. Sin esa frase, un
  agente puede leer «reporte final» como «reporte de la ejecución» y considerar que un dry-run no
  lo necesita — que es exactamente lo que se observó.
- **NUEVA SUITE (27 en el runner)**: `test_contrato_entrega_orquestadores.R`, 33 aserciones.
  Verifica sección, marcador, prohibición del silencio, cláusula dry-run, `maxTurns`, coherencia
  marcador↔`exit_status` por archivo, que **los gemelos compartan marcador** entre sí, y que el
  detractor **conserve el suyo distinto** (unificarlos rompería la validación de quien invoca).
- **EL DETECTOR ES UNA FUNCIÓN DE LA RUTA**, no un bloque con rutas fijas: eso permite el control
  positivo con **4 mutantes sobre fixtures en `tempdir()`** —sin sección, sin marcador, vocabulario
  divergente, sin cláusula dry-run— sin tocar los archivos reales. Mutar el archivo real y
  restaurarlo después ya dejó una vez el arsenal roto en disco (`validar_multisemilla.R`, 2026-08-09).
- **DOS BUGS DEL PROPIO TEST, cazados por su control positivo**: (a) la frase de la cláusula
  dry-run lleva `**` y backticks, que como regex son operadores de repetición inválidos — hay que
  compararla con `fixed = TRUE`; (b) el control positivo usaba **un solo `&&` encadenado** y, al
  fallar, el mensaje culpó al sub-check equivocado (`vocabulario_ok`, cuando el roto era
  `cubre_dry_run`). Ahora comprueba sub-check por sub-check y nombra al culpable. Un control
  positivo que miente sobre QUÉ falló es media defensa.
- **Verificado**: suite nueva 33/33 con los 4 mutantes cazados · invariantes I-1..I-10 en verde
  (214/214) · `test_contrato_detractor.R` 16/16 sin regresión.
- **LÍMITE DECLARADO**: un subagente carga su definición **desde HEAD**, no desde el disco, así que
  este contrato NO rige para agentes ya registrados ni para la corrida que lo motivó. Empieza a
  aplicar tras el commit, en sesión nueva.
- **CORRECCIÓN AL DIAGNÓSTICO, medida**: el contrato NO era la causa del silencio. Recuperando la
  transcripción del subagente (`…/subagents/agent-<nombre>-<hash>.jsonl`, 267 KB) se comprobó que
  el agente **sí emitió** el reporte completo y **sí cerró con el marcador**: lo que falló fue el
  canal de entrega al padre, que solo transmitió la notificación de «disponible». El contrato queda
  como higiene correcta —el hueco existía— pero no habría evitado este fallo. **Vía de recuperación
  cuando vuelva a pasar: leer el último bloque `text` de esa transcripción.**

- **PUNTO CIEGO DEL ARSENAL CERRADO — regla #22 → v1.4, nueva sonda H3b.** Lo encontró el propio
  dry-run: cuando las cuatro opciones comparten primera palabra (ítems cuyas opciones son
  **preguntas**, `¿Cuál es…?`), **dos de las tres sondas dejan de medir y el script lo callaba**.
  Verificado en el código: `pw` descarta el `¿` → las 4 dan `cuál`; H2 exige que la clave sea la
  única con su prefijo → **0 % por construcción**; la guarda de H3 exige ≥2 prefijos → `pwc` vacío
  y, bajo `if (length(pwc) >= 5L)`, **la fila H3 ni se imprime**. Un `PASS` así es «sin medición»
  leído como «sin señal» — el mismo modo de fallo que originó H3, un piso más abajo.
- **H3b mide por CONTENIDO** (texto en minúsculas, sin dígitos ni puntuación), con guarda análoga a
  la de H3 (la firma debe discriminar dentro de la versión; las opciones numéricas colapsan y
  quedan fuera). Medido sobre fixtures: clave de tipo fijo **100 % → exit 1**; tipo sorteado de un
  pool de 4 → **33 %, PASS**.
- **CALIBRACIÓN DE RELEVO, y por qué**: H3b bloquea **solo** si el prefijo es uniforme en ≥90 % de
  las versiones. La primera versión bloqueaba siempre y puso en **ROJO un fixture existente** que
  existe para probar que H1 *no* dispara —sus opciones llevan prefijos distintos, así que H2/H3 sí
  aplican ahí—. Una sonda nueva que cambia el veredicto de casos ya revisados no es más rigor.
- **LA CEGUERA SE DECLARA SIEMPRE**, dispare o no: `H2/H3 CIEGAS` con su porcentaje y la frase «el
  0 % de H2 NO es ausencia de señal, es ausencia de medición»; y `H3b: NO MEDIBLE` cuando la firma
  tampoco discrimina, exigiendo verificador propio del ejercicio.
- **`test_diagnosticidad.R`: 10 → 24 aserciones** (4 casos nuevos), con control de que el fixture
  prueba lo que dice —H2 en 0 % y H3 sin medir— y no-regresión de la calibración de relevo.
- **TRAMPA REINCIDENTE, cometida y corregida aquí mismo**: se midió el exit de `Rscript … | grep |
  tail` y salió 0 sobre un caso que **sí** bloqueaba. Es la trampa que la v3.20.5 ya documentó
  (`cmd | grep …; echo $?` mide el exit del **pipe**). El exit real —1 y 0— se confirmó redirigiendo
  a archivo, sin tubería.
- **DECISIÓN DE ALCANCE (documentada a petición del usuario)**: el dry-run proponía un verificador
  *ad-hoc* del ejercicio. Se decidió **cerrar antes el punto ciego en el arsenal compartido**,
  porque un verificador por ejercicio dejaría a H2/H3 igual de ciegas para todo ítem futuro con
  molde uniforme de opciones. El verificador propio sigue siendo necesario para el caso en que
  H3b resulte `NO MEDIBLE`; ya no lo es para éste.

### Cambios v3.20.6 (2026-08-09)

> Auditoría del gemelo CLOZE de `area-jardin-lote-porcentaje-n4` con la misma vara que su hermano.
> Encontró un defecto al **100 %** de las versiones, y **la corrección introdujo uno peor** que solo
> apareció porque el detractor la auditó. Es la tercera vez en la misma sesión que un fix desplaza
> el defecto de canal: semántica → longitud → **léxico**.

- **DEFECTO ORIGINAL, 600/600 versiones**: en la Parte 6 (conclusión binaria) una opción decía
  «**Sí**, porque queda libre [un rango que desmiente lo afirmado]». No era descuido sino una
  **restricción imposible**: con balance 2 Sí + 2 No y todas las opciones justificadas por un rango,
  solo el rango afirmado hace coherente un «Sí», así que el segundo tenía que mentirse.
  Decisión del profesor entre tres alternativas: **los «Sí» se justifican por MÉTODO, los «No» por
  RANGO**. Resultado: 600/600 → **0/600**.
- **NUEVO PATRÓN — ERROR 32, la fuga léxica**: al sustituir cuatro cadenas homogéneas por siete
  justificaciones redactadas a mano, el ítem pasó a resolverse **sin leer el enunciado al 88,4 %**
  (azar 25 %) — peor que el defecto §P4-bis que el diseño ya vigila. Tres causas medidas: (a) solo
  la justificación correcta contenía «jardín», y ese método nunca es distractor, así que su
  **presencia o ausencia predecía la rama en 800/800**; (b) 4 de 6 erróneas eran prescriptivas
  («hay que…», «basta…») y la correcta declarativa, con lo que «elige el declarativo» acertaba el
  100 %; (c) sesgo algebraico del pool —`1-ab` supera a 4 de los 6 métodos— con lo que «elige el
  rango mayor» acertaba el 77 %. Fix: **molde único y paralelo** más **estratificación del
  distractor por el lado del rango**. Medido: **88,4 % → 24,2 %**.
- **PUNTO CIEGO NUEVO, declarado en ambos orquestadores**: un `PASS` de `validar_diagnosticidad.R`
  **no acredita ausencia de fuga léxica**. H2 mide la **primera palabra** y H3 la invariancia del
  veredicto; **ninguna sonda inspecciona el vocabulario del cuerpo de la opción**. Prueba de
  aceptación ejecutable en el Error 32: ningún token de más de 2 caracteres puede ser exclusivo de
  la clave en ≥70 % de las versiones, ni dentro de cada rama por separado.
- **Otras tres objeciones del detractor, cerradas y medidas**: la Solution imprimía «entre 45 % y
  45 %» cuando el rango afirmado es puntual (10,7 % de las versiones → 0/40, aplicando el
  `fmt_rango()` que el propio archivo declara para eso); y su prosa de verificación recorría el
  orden **pre**-permutación, con lo que la clave encabezaba la lista en 300/300 mientras el
  estudiante veía otro orden (→ 26 %, lo esperable al azar). Vecino del Incidente Q.
- **Correcciones a lo que yo mismo había reportado**: el `ejercicio_state.json` del CLOZE declaraba
  **60 versiones únicas** y lo di por incumplimiento de la regla #3; medido, son **300/300**. Y el
  `set.seed` que detecté era un **comentario** que dice «PROHIBIDO set.seed()».
- **Lo que el CLOZE hace MEJOR que su hermano**: la colisión de rangos (Error 28) está prevenida
  **por construcción** — filtra los métodos con rango duplicado y el que iguale al correcto. 0/600.
- **Pendiente declarado**: `WARN_DIV_BAJA` en el gap p3 (7 valores únicos de 40), preexistente y no
  bloqueante. El CLOZE queda en **10/11**: su aprobación es del profesor.

### Cambios v3.20.5 (2026-08-09)

> Cierre definitivo del bloque. Se aplica el refinamiento que la v3.20.4 dejó medido pero sin
> aplicar, y las lecciones que solo vivían en la memoria privada del asistente pasan al repositorio,
> donde cualquier agente las lee. **Una lección que solo está en la memoria de quien la aprendió no
> es una defensa: es una anécdota.**

- **REFINAMIENTO DE SALENCIA APLICADO**: `GEO-ARE-07` (145 → 81 caracteres) y `GEO-ARE-02`
  (99 → 65). Resultado medido sobre 800 versiones, **mejor que la proyección del detractor**:
  ventaja de la opción más larga sobre la segunda **+25,3 % → +12,9 %** (mediana), p90
  **+76,8 % → +27,1 %** — por debajo del margen del 15 % con el que el repo calibró la sonda H1, es
  decir, deja de ser perceptible. Y lo que la proyección no anticipaba: **la clave pasa a ser a
  veces la más larga (9,2 %)**, así que «descartar la más larga» deja de ser gratis y pasa a ser
  arriesgada. El validador oficial confirma la caída: margen H1 **26 % → 3 %**.
- **NUEVO INVARIANTE I-10** (regla #17 → v1.4): los cuatro validadores compartidos de
  `.claude/scripts/` son **symlinks** a `SOURCES/scripts_validacion/`. El invariante fija sus dos
  mitades: que el enlace resuelva a un archivo existente, y que **ningún archivo regular haya
  suplantado a un symlink conocido** —eso dejaría dos copias divergentes del mismo validador, y cuál
  se ejecuta dependería de la ruta invocada—. Detector verificado sobre un fixture temporal con los
  tres casos (enlace correcto, archivo regular suplantando, enlace roto).
- **CUATRO TRAMPAS DE MEDICIÓN** añadidas a la tabla «Puntos ciegos de mi propio arsenal» de **ambos**
  orquestadores. Las cuatro producen verdes o rojos falsos y las cuatro se cometieron en esta sesión:
  `cmd | grep …; echo $?` mide el exit del **pipe**, no del comando; editar `.claude/scripts/` no
  toca el código que corre; mutar el archivo **real** deja el repo roto si el paso de restaurar se va
  a segundo plano; y un `grep` que no encuentra nada puede ser «no hay defecto» o «el patrón no
  coincide» —comillas tipográficas, `.latex` en vez de `.tex`, un glob contando 0 sobre CERO
  archivos—, así que **toda sonda necesita un control positivo que demuestre que dispara**.
- **`area-jardin-lote-porcentaje-n4` → 11/11**: aprobación humana explícita el 2026-08-09. Queda
  **listo para llevar al aula**; sigue en `01-En-PreDesarrollo/` porque el gate de
  `/promover-ejercicio` es la evidencia de Nivel 3 con estudiantes, no la aprobación del profesor.
- **DEUDA DEL ÁRBOL DE TRABAJO SALDADA**: se commitean los cambios acumulados de otros subproyectos
  tras verificarlos uno a uno — `excedente-almuerzo-proporcional-n4` (su propio `verificar_render.R`
  da APROBADO con los mutantes muriendo cada uno por su sonda), el PNG LFS de
  `migraciones-exteriores-lineas-n2` (inspeccionado visualmente), el reordenamiento del bloque
  `output:` de `grafica-circular-consumo-agua`, el destino reservado en `Estructura_Repositorio` y
  `.gitignore`. **Revertido** el `archivos = 1200` de `permutaciones-…/cloze/SemilleroCloze.R`: era
  resto de un experimento local, fuera del rango de todos sus hermanos (10–500).

### Cambios v3.20.4 (2026-08-09)

> Cierre del bloque anterior: se aplican las dos objeciones que el detractor dejó abiertas y se
> resuelve de raíz el Error 31, que la v3.20.3 solo había documentado.

- **ERROR 31 RESUELTO — `validar_multisemilla.R`**. Resolución de la propia ruta en cuatro pasos
  aislados (`--file=` de `commandArgs` → `sys.frame` dentro de `tryCatch` → `git rev-parse
  --show-toplevel` → rutas relativas) y **aborto con `stop()`** si ninguna candidata existe. Esto
  último corrige un **segundo defecto latente** que la v3.20.3 no había visto: el bucle de rutas
  relativas podía terminar sin cargar nada y continuar, y el fallo salía mucho después como «no se
  pudo encontrar la función». Verificado en los cuatro modos de invocación —sin argumentos, desde la
  raíz por el symlink, desde un cwd ajeno y vía `source()`—, midiendo el exit **real**, sin tuberías
  que lo enmascaren. **La FASE 2G informa de verdad por primera vez**: 20/20 semillas, exit 0.
- **TRAMPA DE EDICIÓN**: el archivo real es `SOURCES/scripts_validacion/validar_multisemilla.R`;
  `.claude/scripts/` contiene solo un **symlink** (modo `120000` en git). Editar la ruta de
  `.claude/scripts/` no surte efecto. Lo mismo vale para `validar_coherencia_matematica.R`,
  `corregir_ortografia_espanol.R` y `arsenal_validacion_completa.R`.
- **NUEVA SUITE CRÍTICA (26 en el runner)**: `test_validar_multisemilla_invocable.R`. Barre TODO el
  arsenal buscando `sys.frame(<literal>)` fuera de `tryCatch`, comprueba la invocabilidad real bajo
  `Rscript` desde un `tempdir()` y fija el contrato del fix. **El detector es del índice literal, no
  de `sys.frame` a secas**: la primera versión daba un falso positivo en `stress_test_visual.R:34`,
  que usa `sys.frame(i)` dentro de `for (i in seq_len(sys.nframe()))` y es correcto. Verificado por
  mutación sobre una **copia en `/tmp`** — la primera vez se mutó el archivo real y el paso de
  restaurar quedó pendiente en un job en segundo plano, dejando unos minutos el validador roto en
  disco. Mutar siempre una copia.
- **OBJECIÓN 1 DEL DETRACTOR APLICADA**: la clave alternativa `GEO-ARE-09` pasa a **conclusión
  desnuda**, con el mismo registro que `GEO-ARE-04`. Medido sobre 800 versiones: «elegir la más
  larga» **50,5 % → 0,0 %**; «la que dice *producto*» **62,9 % → 25,0 %** (= azar). H1 del validador
  oficial: 50 % → **0 %**.
- **RESIDUO DECLARADO, NO OCULTADO**: la clave pasa a no ser NUNCA la más larga, así que «descartar
  la más larga» rinde **33,3 %** frente al 25 % de azar. Es la señal inversa que la regla #22 v1.3
  advierte, y baja la ventaja de 25 puntos a 8. Su causa es estructural del formato: todas las
  justificaciones «Sí» son más largas que todas las «No». Queda abierto.
- **PASADA DE CONFIRMACIÓN DEL DETRACTOR** (aplicar sus cambios vuelve a caducar su veredicto, así
  que se le pidió confirmar su propia implementación). Cerró 1, 2 y 3 **por ejecución** —1584
  corridas, 0 errores— y encontró **dos defectos en la entrada que se había añadido para resolver
  su objeción 2**, ambos corregidos:
  - **Comparar intervalos donde la propiedad es puntual**: `descripcion_larga` de `GEO-ARE-10`
    yuxtaponía `[correct_min, correct_max]` y `[comp_largo_max, comp_largo_min]` y afirmaba que el
    primero «supera a ambos». Es cierto **valor a valor**, pero como intervalos **se solapan en 30
    de los 99 combos**, así que el estudiante leía una contradicción. Reescrito en forma puntual,
    explicando además por qué los extremos pueden solaparse.
  - **La única opción sin cifras**: `GEO-ARE-10` era la única sin ningún dígito en el 100 % de las
    versiones en que aparecía (49,9 % del total) y, como nunca es la clave, se descartaba de un
    vistazo. El residuo real era **36,2 %**, no 33,3 %. Ahora cita el complemento del ancho —un
    porcentaje **lineal**, no de área, así que `afirma_rango_area` sigue siendo `FALSE` con la misma
    lógica que `GEO-ARE-02`—. Medido: versiones con una sola opción sin dígitos **49,9 % → 0 %**;
    heurística combinada **36,2 % → 33,3 %**; coherencia conclusión↔justificación 0/600 con el
    control negativo activo.
- **REFINAMIENTO NO APLICADO, con su medición**: acortar `GEO-ARE-07` (145→~80) y `GEO-ARE-02`
  (99→~72) **no baja** la tasa del 33,3 %, pero hunde la ventaja perceptible de la opción más larga
  sobre la segunda de **+25,3 % a +7,3 %** (mediana) — por debajo del margen ≥15 % con el que el
  propio repo calibró la sonda H1, es decir, por debajo de lo explotable. Verificado sobre copia
  (1188 corridas). No se aplica porque su efecto pedagógico sobre esos dos distractores es juicio
  humano.
- **OBJECIÓN 2 APLICADA**: `GEO-ARE-01` se marca `afirma_rango_area = TRUE` — emite exactamente los
  mismos dos números que `GEO-ARE-07`, así que arrastraba el mismo defecto en 391 de 1584 versiones;
  el criterio anterior para distinguirlos (¿dice «del área»?) era de superficie. Como eso dejaba el
  pool «sí» de esa rama con **un solo** elemento y margen 0, se añade **`GEO-ARE-10`** (error
  conceptualmente distinto, sin rango de área propio, coherente en ambas ramas). Pool: 9 → **10**.
  Márgenes medidos: `min(idx_si) = 2`, `min(idx_no_d) = 3`.
- **Verificado tras los cambios**: 5 formatos + LaTeX en R limpio, `pandocbounded` 0, coherencia
  `APROBADO`, diversidad `PASS`, diagnosticidad `PASS` (H1 0 % · H2 0 % · H3 50 %), multisemilla
  20/20, ortografía limpia, 600/600 versiones sin violar invariantes, mutante
  `cazado_por_su_sonda`, 0 incoherencias Sí/No con control negativo activo, instancia canónica
  intacta.

### Cambios v3.20.3 (2026-08-09)

> Primera aplicación real del contrato de entrega de la v3.20.2, y funcionó: el detractor falló la
> primera vez, el marcador `VEREDICTO_DETRACTOR:` lo detectó como NO ENTREGADO, el reclamo del
> protocolo lo recuperó y su objeción ALTA resultó ser **contra la corrección que había aplicado el
> coordinador**. Es exactamente para eso que la regla #9 v1.2 exige independencia.

- **REGLA #22 → v1.3**, nueva subsección en §P4-bis: **«La propia defensa crea deuda»**. La v1.2
  decía qué defensa aplicar contra el veredicto invariante, pero no que aplicarla **cambia la
  premisa sobre la que se escribió el pool existente**. Tres verificaciones obligatorias después de
  introducir una clave alternativa, medidas en `area-jardin-lote-porcentaje-n4` sobre 600 versiones:
  (a) las guardas anti-colisión deben recorrer **todas** las claves — comparar contra la vigente
  solo protege una rama, y la clave NO vigente es la firma exacta de la colisión (3/600 con dos
  opciones del mismo rango y veredictos opuestos); (b) los distractores escritos para la clave única
  quedan declarando un veredicto que su justificación contradice (81/600 = **13,5 %**); (c) al
  corregir (b) excluyendo el único distractor más largo que la clave, la clave queda
  **determinísticamente** la más larga de su rama.
- **PUNTO CIEGO DEL ARSENAL DECLARADO**: `validar_diagnosticidad.R` **agrega sobre versiones sin
  condicionar por rama**. En un ítem con clave alternante las dos ramas son estructuralmente
  distintas, así que un reparto 100 %/0 % se lee como ~50 % y pasa bajo el umbral del 70 %. Medido:
  clave identificable por longitud en el **100 %** de una rama con `PASS` en el agregado, y
  **50,5 %** de acierto sin razonar frente al 25 % de azar. Es el mismo punto ciego que dio origen a
  la sonda H3 — un patrón que solo existe *entre* versiones no lo ve una sonda que mira *cada* una.
  Hoy la medición por rama es **manual**; queda declarada como tal, no simulada.
- **ADVERTENCIA DE LA SEÑAL INVERSA**: igualar longitudes hasta que la clave no sea NUNCA la más
  larga tampoco es neutro — habilita «descartar la más larga», que sube el azar de 25 % a 33 %.
  Un fix de diagnosticidad puede **desplazar el defecto de canal** (semántica → longitud → léxico);
  hay que volver a medir el ítem completo tras cada fix, no solo la dimensión corregida.
- **4 PATRONES NUEVOS** en `patrones-errores-conocidos.md`: **28** (exclusión por texto que solo
  cubre la clave vigente), **29** (§P4-bis reabre `INC-SINO-BINARIO`), **30** (la sonda agrega sin
  condicionar por rama), **31** (`validar_multisemilla.R` roto).
- **`validar_multisemilla.R` ESTÁ ROTO** (Error 31): la línea 21 resuelve su propia ruta con
  `dirname(sys.frame(1)$ofile)`, que bajo `Rscript` **revienta antes** de que la guarda `is.null()`
  de la línea 22 pueda ejecutarse — el fallback por rutas conocidas es **código inalcanzable**.
  Verificado que falla con cualquier `.Rmd`, incluido un ejemplo canónico intacto, y **sin
  argumentos**. El hook lo invoca así en FASE 2G, de modo que esa fase es un **falso ROJO
  permanente** en todo el repositorio, y un gate que siempre falla se aprende a ignorar. Fix de una
  línea (`tryCatch`) **pendiente de aplicar**: es infraestructura compartida y quedó fuera del
  alcance de la sesión que lo detectó. Mientras tanto, ambos orquestadores tienen instrucción de
  declarar la cobertura multisemilla como **NO VERIFICABLE**, nunca como verde.
- **AMBOS ORQUESTADORES** ganan los incidentes gemelos `INC-CLAVE-ALTERNATIVA` (R en SCHOICE, U en
  CLOZE) e `INC-MULTISEMILLA-ROTO` (S / V), con sus dos filas en la tabla de IDs estables. Ninguno
  lleva `—`: el mecanismo de la clave alternante es del ítem binario, no del tipo, y el del script
  roto es del arsenal, así que **los dos aplican a los dos gemelos**.
- **EJERCICIO**: `area_jardin_lote_..._n4_schoice_v1` — 6 correcciones verificadas por enumeración de
  600 versiones, prueba de mutación con contrato de sonda (`cazado_por_su_sonda`) y control negativo.
  Incluye literalidad del paso 10 restaurada contra el catálogo canónico (Afirmación y Evidencia
  estaban de-acentuadas) y una contracción `a el`/`de el` visible al estudiante en 8,5 % de las
  versiones que el corrector de ortografía no ve. **Sigue en 10/11**: dos objeciones del detractor
  quedan abiertas a decisión del profesor, porque tocan el texto de la clave.

### Cambios v3.20.2 (2026-08-09)

> Un agente cuyo trabajo es romper el sesgo de confirmación no servía de nada si su reporte no
> llegaba: la FASE 2C acababa cerrándose con la autoevaluación del mismo agente que había escrito
> el código. El agujero no era el análisis — era **el canal de entrega y la falta de una regla de
> independencia**.

- **CONTRATO DE ENTREGA en `agente-detractor.md`**: su texto final **es** el reporte. Prohibido
  terminar el turno sin él, escribirlo a un archivo, o anunciar que lo entregará. Si se queda sin
  presupuesto, entrega parcial declarando los dominios `no auditado`. Cierra siempre con
  `VEREDICTO_DETRACTOR: APROBAR | APROBAR_CON_CAMBIOS | RECHAZAR`, marcador que permite a quien
  invoca comprobar **mecánicamente** que el reporte llegó entero: sin esa línea, NO ENTREGADO
  aunque contenga análisis. Añadido además `maxTurns: 30` — era el único agente de reporte que no
  declaraba presupuesto (el `adversario` global sí).
- **REGLA #9 → v1.2**, dos secciones nuevas. **Independencia**: el detractor DEBE ser un agente
  distinto del que escribió o corrigió el artefacto, con tabla de qué cuenta como FASE 2C válida
  (incluye el caso del detractor **caducado** por una edición posterior). **Protocolo de
  no-entrega**: 2 reintentos (reclamo al mismo agente → agente nuevo con contexto limpio) y luego
  **escalado obligatorio al usuario**; PROHIBIDO sustituir el detractor por la auditoría propia y
  sellar `detractor_fase2c`. El coordinador puede revisar por su cuenta, pero debe declararlo
  **no independiente** y dejar la fase abierta.
- **TABLA — cuál de los dos detractores usar**: `AgenteDetractor` (canónico para `.Rmd` en
  workflow, 8 dominios) vs `adversario` global (anti-sicofancia). Ambos existían y ninguna regla
  decía cuándo usar cada uno.
- **NUEVO TEST**: `tests/testthat/test_contrato_detractor.R` (16 aserciones) — verifica el
  contrato, `maxTurns`, ambas secciones de la regla, y que agente y regla usen **el mismo**
  marcador (si divergen, la comprobación de entrega se rompe en silencio). Runner: **25 suites**.
  Verificado por mutación: al renombrar el marcador en el agente, el test falla nombrando la causa.
- **Origen**: incidente 2026-08-09 en `excedente-almuerzo-proporcional-n4`. Tres invocaciones
  consecutivas terminaron en notificación de «disponible» sin reporte. El defecto de mayor
  severidad del ejercicio era **semántico** —un distractor que en una de las tres ramas señalaba
  información que SÍ resolvía el problema, rompiendo la unicidad de la clave— y todo el arsenal
  automático estaba en verde: coherencia APROBADO, diagnosticidad PASS, diversidad sin objeción.
  Es exactamente la clase de hallazgo que depende de una mirada independiente.

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
