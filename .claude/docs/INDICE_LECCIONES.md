# Índice Maestro de Lecciones Aprendidas

> **Propósito:** mapa unificado de TODAS las fuentes de lecciones, errores y decisiones del proyecto ICFES R/exams. Si tienes una pregunta del tipo "¿esto ya pasó? ¿hay un fix conocido?", **empieza por aquí**.

**Última actualización:** 2026-07-28 (Errores 21-24 agregados retroactivamente al catálogo §2 — existían en `patrones-errores-conocidos.md` pero faltaban en el índice; secuencia numérica 21-26 ahora continua)
**Mantenedor:** Álvaro Ángel Molina

---

## Cómo usar este índice

1. Identifica la **categoría** del problema (ortografía, render, semántica, infraestructura, etc.).
2. Salta a la sección correspondiente para ver las lecciones documentadas.
3. Cada lección apunta a su archivo de origen con la línea/sección exacta.
4. Si no encuentras tu caso → es una lección nueva: documéntala usando `templates/retrospectiva-sesion.md`.

---

## 1. Categorías y archivos fuente

| Categoría | Archivo principal | Tipo |
|---|---|---|
| Errores de renderizado `.Rmd` | `.claude/docs/patrones-errores-conocidos.md` (Errores 1-3) | Catálogo |
| Errores de gráficos | `.claude/docs/patrones-errores-conocidos.md` (Errores 4-6) | Catálogo |
| Errores de coherencia semántica/RNG | `.claude/docs/patrones-errores-conocidos.md` (Errores 7-10, 22) | Catálogo |
| Errores de infraestructura `.claude/` | `.claude/docs/patrones-errores-conocidos.md` (Errores 11-15) | Catálogo |
| Errores de pipeline render PDF + coherencia Solution | `.claude/docs/patrones-errores-conocidos.md` (Errores 16-21) | Catálogo |
| Errores de diseño de opciones gráficas | `.claude/docs/patrones-errores-conocidos.md` (Errores 18 y 20) + `.claude/rules/graficos-como-opciones.md` §Formato Equilibrado | Catálogo + Normativo |
| Errores de legibilidad geométrica y fuga de información en diagramas dinámicos | `.claude/docs/patrones-errores-conocidos.md` (Errores 23-26) + `.claude/rules/graficos-como-opciones.md` §Canal de fuga + `.claude/rules/diversidad-sustantiva.md` §P4/P5/P6 + regla local `A-Produccion/01-En-PreDesarrollo/desplazamiento-avion-aeropuerto/.claude/rules/diagramas-vectoriales.md` | Catálogo + Normativo |
| Reglas absolutas (20) | `.claude/CLAUDE.md` + `.claude/rules/*.md` | Normativo |
| Decisiones arquitectónicas | `.claude/docs/ADR/*.md` | Inmutable |
| Casos resueltos individuales | `.claude/docs/casos-resueltos/*.md` | Histórico |
| Lecciones de usuario / preferencias | `~/.claude/projects/.../memory/MEMORY.md` | Personal |
| Cambios por versión | `.claude/CLAUDE.md` (sección Cambios) + `.claude/docs/CHANGELOG.md` | Cronológico |
| Plantilla para nueva lección | `.claude/docs/templates/retrospectiva-sesion.md` | Template |

---

## 2. Catálogo de errores con fix verificado

### 2.1 Renderizado y compilación

| # | Síntoma | Causa raíz | Fix | Fuente |
|---|---|---|---|---|
| 1 | "File 'imagen.png' not found" en exams2pdf | `include_tikz()` genera PNG en `/tmp/` no accesible | Renderizado condicional `if(knitr::is_latex_output())` | `patrones-errores-conocidos.md` §Error 1 |
| 2 | "argumento no numérico" en `abs()` | Variable string pasada a función matemática | Verificar tipos en `data_generation` con `is.numeric()` | §Error 2 |
| 3 | Imágenes Python no visibles en PDF | `reticulate` + matplotlib sin `plt.savefig()` explícito | Guardar PNG con ruta absoluta + `cat("![](ruta)")` | §Error 3 |

### 2.2 Gráficos como opciones

| # | Síntoma | Causa raíz | Fix | Fuente |
|---|---|---|---|---|
| 4 | Gráficos en grid (no individuales) | `grid.arrange()` para mostrar opciones juntas | Cada opción como PNG separado + answerlist con `![](diagrama_a.png)` | §Error 4 + regla `graficos-como-opciones.md` |
| 5 | Gráfico aplastado por escala | `EST-BOX-01` produce valores fuera de rango | Excluir errores fuera de rango en `errores_validos_para_grafico` | §Error 5 |
| 6 | "cannot take a sample larger than the population" | Rango `1:N` con `replace=FALSE` y `size>N` | Ampliar rango o usar `replace=TRUE` o reducir `size` | §Error 6 |
| **18** | **Estudiante identifica opción correcta por formato sin verificar datos** | **Solo 1 opción del formato correcto (ej: 3 tortas + 1 barra)** | **Formato equilibrado: min 2 del mismo formato que la correcta** | **§Error 18 + regla `graficos-como-opciones.md` §Formato Equilibrado** |
| **20** | **GRAF-BAR-01 — Barras con categorías correctas pero alturas permutadas** | **Verificación incompleta: solo categorías, no valores por categoría** | **Distractor GRAF-BAR-01: permutar frecuencias entre categorías con guardia anti-coincidencia** | **§Error 20** |

### 2.3 Coherencia semántica y aleatoriedad

| # | Síntoma | Causa raíz | Fix | Fuente |
|---|---|---|---|---|
| 7 | Descripción de error contradice los datos | Falta `precondicion` en error del pool | Agregar `precondicion = function(params) ...` | §Error 7 + regla `ejercicios-metacognitivos.md` Capa A |
| 8 | 2/200 versiones únicas (debería ser 287/300) | `set.seed()` en chunk de test sin restaurar `.Random.seed` | Guardar y restaurar `.Random.seed` en bloque test | §Error 8 + regla `codigo-rmd.md` #10 |
| 9 | `##ANSWERi##` después de Parte 2 (CLOZE) | Placeholder mal ubicado | Cada `##ANSWERi##` inmediatamente después de su pregunta | §Error 9 + regla `codigo-rmd.md` #12 |
| 10 | Crash en `while (calcula() == valor)` | `calcula()` retorna `NA` para algunas semillas | Guardia `is.na(resultado)` antes de comparación | §Error 10 + regla `codigo-rmd.md` #9 |
| **22** | **Render se congela sin error (~1-2% de semillas; `exams2html/pdf(n=200)` o el multi-semilla no terminan)** | **`repeat`/`while` en `data_generation` resamplea hasta una condición matemáticamente imposible para ciertos datos** | **Construcción determinista del valor objetivo (`pick_int`) en vez de reintentar; si el bucle es inevitable, contador + `max_intentos`; validar con stress test de timeout por semilla** | **§Error 22 + regla `familias-soluciones-rmd.md` Familia 1** |

### 2.4 Infraestructura `.claude/` (lecciones sesión Ruflo, 2026-05-03)

| # | Síntoma | Causa raíz | Fix | Fuente |
|---|---|---|---|---|
| 11 | Hooks ICFES inactivos tras Ruflo init | `settings.json` sobrescrito por wrapper externo | Re-enganchar hooks en paralelo (ruta B convivencia) | §Error 11 + regla #17 |
| 12 | `CLAUDE.md` raíz reemplazado por plantilla genérica | `init` sobrescribe sin preguntar | Mezcla con priority ICFES arriba | §Error 12 + ADR-001 |
| 13 | MCP `ruflo` ✗ Failed to connect | Paquete fantasma o roto | `claude mcp remove ruflo` (cubierto por ruv-swarm + flow-nexus) | §Error 13 |
| 14 | `npm error Invalid Version` en `claude-flow doctor` | Dependencia con `package.json` malformado | Aceptar CLI a medias; no usar en flujo crítico | §Error 14 |
| 15 | "Memory package not available" en SessionStart | Bridge sin paquete npm de embeddings | Vivir sin embeddings; `MEMORY.md` cubre | §Error 15 |

### 2.5 Pipeline render PDF + coherencia Solution (sesión orquestador-schoice 2026-05-03)

| # | Síntoma | Causa raíz | Fix | Fuente |
|---|---|---|---|---|
| 16 | `! Undefined control sequence. l.5 \pandocbounded` al compilar PDF | pandoc 3.x envuelve `\includegraphics` en `\pandocbounded{...}` no definido en templates LaTeX de R-exams cuando `![](file.png)` no tiene atributo `width` | Reemplazar `![](file.png)` por `cat("![](file.png){width=80%}\n")` en bloque R | §Error 16 + regla `markdown-imagenes-pdf.md` |
| 17 | Solution dice "Opción A" pero answerlist marca (c) como correcta | `exshuffle: TRUE` re-mezcla opciones después de que `letra_correcta` ya fue evaluada, sin actualizar el texto de la Solution | `exshuffle: FALSE` + mezcla interna con `sample()` (ya aleatoriza); `letra_correcta` calculada DESPUÉS del sample | §Error 17 + regla `codigo-rmd.md` #6 + `graficos-como-opciones.md` |
| **18** | **Estudiante identifica opción correcta por formato gráfico sin verificar datos** | **Solo 1 opción del formato correcto (ej: 3 tortas + 1 barra cuando la correcta es torta)** | **Formato equilibrado: al menos 2 opciones deben compartir el formato de la correcta (ej: 2 barras + 2 tortas)** | **§Error 18 + regla `graficos-como-opciones.md` §Formato Equilibrado** |
| **20** | **GRAF-BAR-01 — Barras con categorías correctas pero alturas permutadas** | **El estudiante verifica solo categorías, no valores por categoría. Las alturas parecen plausibles (son valores reales) pero están mal asignadas** | **Permutar `frecuencias` con guardia `while(!all(perm == orig))`; usar como distractor de barras para lograr 2+2 equilibrio** | **§Error 20** |
| **21** | **`! LaTeX Error: No counter 'none' defined.` al compilar PDF/NOPS desde RStudio** | **Pandoc ≥3.7 (RStudio bundlea 3.8.3, distinto del 3.6 de terminal) envuelve `longtable` con `\def\LTcaptype{none}`; la plantilla LaTeX de R-exams no define ese contador** | **Bloque raw `{=latex}` con `\@ifundefined{c@none}{\newcounter{none}}{}` al inicio de `Question`; validar con el pandoc bundleado de RStudio** | **§Error 21 + regla `markdown-tablas-pandoc.md` (regla #20)** |

### 2.6 Legibilidad geométrica y fuga de información en diagramas dinámicos (sesión `desplazamiento-avion-aeropuerto`, 2026-06-28 y 2026-07-28)

| # | Síntoma | Causa raíz | Fix | Fuente |
|---|---|---|---|---|
| **23** | **Etiqueta de texto (ángulo) solapada con línea/punto en un diagrama dinámico, solo en algunas versiones** | **Radio del label solo dependía de la longitud del vector (`Lpx`), no del ángulo de la cuña ni del ancho del texto; piso insuficiente para ángulos grandes** | **Radio consciente del ángulo y ancho de texto (`max(50, (8+11·cos(semi))/sin(semi))`) + esquive del marcador móvil; verificar el caso extremo a alta magnificación (×2.4)** | **§Error 23 + regla local `A-Produccion/01-En-PreDesarrollo/desplazamiento-avion-aeropuerto/.claude/rules/diagramas-vectoriales.md`** |
| **24** | **Respuesta correcta predecible por posición/cuadrante aunque su valor varíe entre versiones** | **Orientación/cuadrante de la escena fijo (siempre `"ne"`); la diversidad por VALOR enmascara la predictibilidad posicional** | **Aleatorizar la orientación global (NE/NO/SE/SO) con la misma transformación para todas las opciones + texto coherente; distractor de dirección = reflejo este↔oeste a la distancia correcta (no outlier de 180°)** | **§Error 24 + regla `diversidad-sustantiva.md` §P4 y §P5 (regla #22)** |
| **25** | **El nombre del archivo PNG revela la opción correcta en el XML de `exams2moodle()`** | **PNGs guardados con nombre semántico (`diagrama_correcta.png`) en vez de letra neutral; invisible en HTML/PDF (imagen embebida/base64) pero visible en texto plano dentro del XML de Moodle** | **Renombrar a letra neutral (`diagrama_a.png`) DESPUÉS de la mezcla `sample()`; verificar con `exams2moodle()` + `grep` del XML** | **§Error 25 + regla `graficos-como-opciones.md` §Canal de fuga + regla #22 §P6** |
| **26** | **Diagrama con vector casi nulo (~17 px) cuando una magnitud es pequeña respecto a la escena total** | **Escala de dibujo calculada sobre la SUMA de magnitudes, sin piso de legibilidad por vector individual** | **Filtro de proporción mínima `(mayor - menor) >= 0.25*(mayor + menor)` (f=0.25 por barrido) + línea guía punteada cuando la etiqueta se separa del marcador** | **§Error 26** |

---

## 3. Reglas críticas (20, todas obligatorias)

| # | Regla | Archivo | Categoría |
|---|---|---|---|
| 1 | Ejercicios metacognitivos con Progressive Disclosure | `ejercicios-metacognitivos.md` | Pedagógica |
| 2 | Flujo B obligatorio cuando hay gráficos | `flujo-b-obligatorio.md` | Workflow |
| 3 | Graficador secuencial TikZ→Python→R 98% | `graficador-secuencial.md` | Visual |
| 4 | Gráficos como opciones individuales | `graficos-como-opciones.md` | Visual |
| 5 | 5 Coherencias (Sem/Vis-Tex/Mat/Cód/Gen) | `ciclo-validacion.md` | Validación |
| 6 | Validación visual iterativa | `ciclo-validacion.md` | Validación |
| 7 | Ortografía española con tildes | `ortografia-espanol.md` | Calidad |
| 8 | Testing automático permanente | `testing-obligatorio.md` | Ingeniería |
| 9 | Detractor obligatorio en revisión | `detractor-obligatorio.md` | Calidad |
| 10 | Validación `_neg_` opciones repetidas | `validacion-neg-opciones-repetidas.md` | Pedagógica |
| 11 | Contextos narrativos creativos | `contextos-narrativos-creativos.md` | Calidad |
| 12 | Validación semántica automática (Capa A/B/C) | `ejercicios-metacognitivos.md` §Capas | Validación |
| 13 | Validación correctitud respuesta Nivel 5 | `validacion-correctitud-respuesta.md` | Validación |
| 14 | Routing modelos Opus/Sonnet/Haiku | `modelo-routing-obligatorio.md` | Ingeniería |
| 15 | Stress Test Visual FASE 2H | (en `stress-test-visual` SKILL) | Validación |
| 16 | Workflow State Enforcement (gate) | `workflow-state-enforcement.md` | Workflow |
| 17 | Infraestructura `.claude/` protegida | `infraestructura-protegida.md` | Ingeniería |
| **18** | **Markdown-imágenes para PDF (anti `\pandocbounded`)** | **`markdown-imagenes-pdf.md`** | **Ingeniería** |

---

## 4. Decisiones arquitectónicas (ADR)

| ADR | Título | Estado | Fecha |
|---|---|---|---|
| 001 | Convivencia Ruflo + ICFES en `.claude/` | Aceptado | 2026-05-03 |

---

## 5. Casos resueltos individuales

Documentación detallada de incidentes específicos:

| Caso | Fecha | Tema |
|---|---|---|
| `2025-12-19-cilindro-tikz.md` | 2025-12-19 | TikZ cilindro 3D |
| `2025-12-21-recta-abs-formateado.md` | 2025-12-21 | Formato `abs()` en recta |
| `2025-01-XX-recta-abs-formateado.md` | 2026-01-XX | Continuación caso recta |

Para nuevos casos: usar `templates/retrospectiva-sesion.md`.

---

## 6. Lecciones por versión (CHANGELOG resumido)

Ver `.claude/CLAUDE.md` sección "Cambios v3.X" o `.claude/docs/CHANGELOG.md`. Resumen:

| Versión | Fecha | Lección clave |
|---|---|---|
| 3.0 | 2026-02-04 | Modularización completa de CLAUDE.md |
| 3.1 | 2026-02-06 | Ejercicios metacognitivos obligatorios |
| 3.2 | 2026-02-07 | Detractor obligatorio (FASE 2C) |
| 3.2.1 | 2026-02-07 | 7 dominios de revisión |
| 3.2.2 | 2026-02-07 | Gráficos como opciones individuales |
| 3.2.3 | 2026-02-13 | Validación semántica Nivel 4 (3 capas) |
| 3.3.0 | 2026-02-14 | Validación correctitud Nivel 5 |
| 3.4.0 | 2026-02-14 | Routing de modelos obligatorio |
| 3.5.0 | 2026-02-14 | Determinismo de `calcula()` (Capa D) |
| 3.6.0 | 2026-02-14 | Stress Test Visual (FASE 2H) |
| 3.7.0 | 2026-03-23 | Skills de revisión (`/revisar-schoice`, `/revisar-cloze`) |
| 3.8.0 | 2026-04-10 | Drift hooks/tests/CI/docs resuelto |
| 3.9.0 | 2026-05-03 | Convivencia Ruflo + regla #17 + orquestador-schoice |
| **3.10.0** | **2026-05-03** | **Errores 16-17 + regla #18 anti-`\pandocbounded` + FASE 2I + test_pandocbounded_y_solution_coherence.R** |
| **3.13.0** | **2026-05-14** | **Errores 18 y 20 + Formato Equilibrado en opciones gráficas (v5.0 de graficos-como-opciones.md) + GRAF-BAR-01** |

---

## 7. Memoria persistente del usuario

Archivos en `~/.claude/projects/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/memory/`:

- `MEMORY.md` — índice
- `user_preferencias.md` — español, sin calificadores subjetivos
- `ref_entorno.md` — MAX_OUTPUT_TOKENS=128000, MCPs
- `feedback_*.md` — guías de comportamiento aprendidas
- `procedimiento-*.md` — procedimientos largos (NotebookLM, etc.)

> Estos archivos NO están versionados en el repo (privacidad). Se cargan automáticamente en SessionStart.

---

## 8. Antes de implementar algo nuevo, consulta...

| Pregunta | Recurso a consultar primero |
|---|---|
| "¿Cómo genero un .Rmd SCHOICE?" | `.claude/skills/generar-schoice/SKILL.md` o `Task(subagent_type="orquestador-schoice")` |
| "¿Cómo manejo los gráficos?" | Regla `flujo-b-obligatorio.md` + `graficador-secuencial.md` |
| "¿Por qué falló el render?" | `.claude/docs/patrones-errores-conocidos.md` Errores 1-6, 16 |
| "¿Por qué se solapan etiquetas en un diagrama dinámico?" | Error 23 — radio del label consciente del ancho del texto + `1/sin(ángulo/2)`; validar el caso extremo (ángulo mínimo) |
| "¿La respuesta correcta es predecible por su posición/cuadrante?" | Error 24 + regla #22 §P4 — aleatorizar orientación global (NE/NO/SE/SO); el validador de diversidad por VALOR no detecta predictibilidad posicional |
| "¿Mi distractor de dirección/posición se elimina de un vistazo?" | Error 24 + regla #22 §P5 — no usar outlier obvio (180°, longitud/formato único); usar cuasi-acierto plausible (reflejo este↔oeste a la distancia correcta). Gemelo del Formato Equilibrado |
| "¿El nombre de archivo de mis opciones gráficas puede filtrar la respuesta?" | Error 25 + regla #22 §P6 + `graficos-como-opciones.md` §Canal de fuga — invisible en HTML/PDF, visible en el XML de `exams2moodle()`; renombrar a letra POST-mezcla y verificar con `grep` |
| "¿Mi diagrama dinámico se ve degenerado (vector casi nulo) en algunas versiones?" | Error 26 — filtro de proporción mínima entre magnitudes de la escena (f=0.25 por barrido), no solo validez matemática |
| "¿Cómo evito patrones detectables en opciones gráficas?" | Error 18 + `.claude/rules/graficos-como-opciones.md` §Formato Equilibrado |
| "¿Cómo diseño un buen distractor de barras?" | Error 20 (GRAF-BAR-01) — alturas permutadas |
| "¿Por qué solo se generan N versiones únicas?" | Error 8 + regla `codigo-rmd.md` #10 |
| "¿Qué hago si el detractor reporta una objeción?" | Regla `detractor-obligatorio.md` §Bifurcación |
| "¿Cómo manejo Ruflo y los hooks ICFES juntos?" | ADR-001 + regla #17 |
| "¿Cómo retomo un ejercicio interrumpido?" | `workflow-state.sh status <dir>` + `next <dir>` |
| "¿Qué modelo uso para mi sub-tarea?" | Regla `modelo-routing-obligatorio.md` |

---

## 9. Cómo agregar una nueva lección

1. Identifica si es:
   - **Error con fix verificado** → agregar a `patrones-errores-conocidos.md` (numerar el siguiente).
   - **Decisión arquitectónica** → crear `ADR/00X-<titulo>.md`.
   - **Caso individual narrativo** → `casos-resueltos/<fecha>-<tema>.md`.
   - **Cambio normativo** → nueva regla en `.claude/rules/<nombre>.md` y agregar al `.claude/CLAUDE.md`.
2. Actualiza este índice (sección apropiada).
3. Si el cambio afecta tests, agrega o modifica la suite correspondiente en `tests/testthat/`.
4. Commit con prefijo `docs(claude):` o `feat(infra):` según corresponda.

---

## 10. Anti-patrones de documentación (no hacer)

- ❌ Crear lecciones en archivos sueltos sin actualizar este índice.
- ❌ Documentar "posibles soluciones" sin verificar (regla `documentacion-verificada.md`).
- ❌ Borrar entradas obsoletas — marcarlas con `⚠️ OBSOLETO - ver <nuevo>` y mantener el histórico.
- ❌ Usar calificadores subjetivos ("significativo", "importante") en reportes (regla `ciclo-validacion.md` §Antipatrón #4).
- ❌ Duplicar contenido. Si una lección ya existe en otro archivo, **enlaza** desde aquí.

---

**Versión del índice:** 1.2 (2026-07-28 — agregados retroactivamente Errores 21-24 al catálogo §2: fila 22 en §2.3, fila 21 en §2.5, §2.6 renombrada "Legibilidad geométrica y fuga de información en diagramas dinámicos" con filas 23-24 antepuestas a 25-26; secuencia 21-26 continua)
**Próxima revisión:** cuando se agregue una nueva categoría o el catálogo de errores supere los 30 entries.
