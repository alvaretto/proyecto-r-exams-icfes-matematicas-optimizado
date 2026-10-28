# Cobertura del Agente Validator CE en el Proyecto ICFES

## Principio

El agente `validator` del Context Engineering global tiene la función de "crear tests post-implementación y validar funcionalidad". En el proyecto ICFES R/exams, esta función está cubierta de forma **más comprensiva y automatizada** por la infraestructura existente.

---

## Mapeo: Validator CE → Infraestructura ICFES

| Responsabilidad Validator CE | Cobertura ICFES | Mecanismo |
|------------------------------|-----------------|-----------|
| Crear tests post-implementación | Suites testthat (lista y cifras en «Las suites de test») | `tests/testthat/test_*.R` |
| Validar que cambios no rompen nada | Hook post-exams2 (FASES 2A-2O) | `post-exams2-validation.sh` |
| Ejecutar tests automáticamente | Git hooks nativos + CI/CD | `.git/hooks/pre-commit`, `.git/hooks/pre-push` → `.claude/hooks/pre-push.sh`, `.github/workflows/ci-testing.yml` |
| Verificar sintaxis/lint | Ortografía española de los `.Rmd` en stage + `bash -n` de los hooks | `.git/hooks/pre-commit` → `corregir_ortografia_espanol.R`; invariante I-6 de `test_infraestructura_claude.R` |
| Validar cobertura de tests | Runner unificado de todas las suites | `tests/run_all_tests.R` |
| Prevenir regresiones | Test de regresión + detractor | `test_regression_suite.R`, FASE 2C del ciclo de validación (regla #9) |
| Validación visual | Preview automático PDF→PNG | FASE 2B del hook post-exams2 |

---

## Las suites de test

La lista vive en `tests/run_all_tests.R`; la columna «Qué valida» copia su campo `nombre`.

<!-- SUITES:INICIO — generado por tests/sincronizar_doc_suites.R; no editar a mano -->
Cifras (derivadas del runner): 45 suites, 408 bloques `test_that` y 8 suites sin `watch`.

| # | Archivo | Qué valida |
|---|---------|-----------|
| 1 | `test_validacion_matematica.R` | Validación Matemática |
| 2 | `test_ortografia_espanol.R` | Ortografía Española |
| 3 | `test_renderizado_4_formatos.R` | Renderizado 4 Formatos |
| 4 | `test_aleatorization_diversity.R` | Aleatorización y Diversidad |
| 5 | `test_flujo_b_graficador.R` | Flujo B (Graficador) |
| 6 | `test_regression_suite.R` | Tests de Regresión |
| 7 | `test_neg_visual_distinctness.R` | Distintividad Visual \_neg\_ |
| 8 | `test_media_mediana_moda.R` | Media-Mediana-Moda CLOZE Metacognitivo |
| 9 | `test_validacion_semantica.R` | Validación Semántica (Nivel 4) |
| 10 | `test_correctitud_respuesta.R` | Correctitud de Respuesta (Nivel 5) |
| 11 | `test_cloze_n3.R` | CLOZE N3 (Trigonometría) |
| 12 | `test_stress_test_visual.R` | Stress Test Visual Multi-Semilla |
| 13 | `test_infraestructura_claude.R` | Infraestructura .claude/ (Regla #17) |
| 14 | `test_pandocbounded_y_solution_coherence.R` | Pandocbounded + coherencia Solution (Errores 16-17, Regla #18) |
| 15 | `test_letter_independence.R` | Letter-independence en Solution (Error 19, Regla #19) |
| 16 | `test_markdown_tablas_none_guard.R` | Tablas Markdown guard 'none' (Error 21, Regla #20) |
| 17 | `test_data_generation_no_hang.R` | data\_generation sin cuelgue (Error 22, bucle repeat sin cota) |
| 18 | `test_cloze_graficas_no_en_gap.R` | CLOZE gráficas-opción fuera del gap (V5, Incidente G) |
| 19 | `test_neg_variante_b.R` | \_neg\_ Variante B + params propio (validador 5C / Capa A) |
| 20 | `test_contrato_detractor.R` | Contrato de entrega e independencia del detractor (Regla #9 v1.2) |
| 21 | `test_citas_detractor.R` | Citas verificables del detractor: hook SubagentStop (Regla #9 v1.6) |
| 22 | `test_contrato_entrega_orquestadores.R` | Contrato de entrega de los orquestadores (paridad con el detractor) |
| 23 | `test_nomenclatura_rmd.R` | Nomenclatura oficial de .Rmd (fuente única + gate + citas) |
| 24 | `test_muestra_estandar.R` | Muestra estándar de validación N=100 (regla #23) |
| 25 | `test_validar_multisemilla_invocable.R` | Invocabilidad de validar\_multisemilla (Error 31) |
| 26 | `test_diversidad_sustantiva.R` | Diversidad Sustantiva (Regla #22) |
| 27 | `test_diagnosticidad.R` | Diagnosticidad de distractores (V9, margen relativo de H1) |
| 28 | `test_barco_bbox_invariante.R` | Invariante I-2 barco (bbox del casco == clave) |
| 29 | `test_permutaciones_invariantes.R` | Invariantes I-1..I-7 permutaciones (clave == n!) |
| 30 | `test_diversidad_grafica_circular.R` | Diversidad grafica circular (tabla, meses y par del error) |
| 31 | `test_fase2i_pandocbounded_detector.R` | FASE 2I: detector \pandocbounded distingue definicion de uso |
| 32 | `test_post_exams2_pipefail.R` | Hook post-render: activacion, 2N y 2O sin carrera de SIGPIPE (pipefail) |
| 33 | `test_is_latex_output_rexams.R` | is\_latex\_output() es FALSE en los pipelines de R/exams |
| 34 | `test_tabla_responsiva_pdf.R` | Familia F2: tabla\_responsiva() no deja tablas fuera del margen del PDF |
| 35 | `test_bateria_eliminacion.R` | Bateria de eliminacion: cierre por familias + techo nulo (regla #22 P7) |
| 36 | `test_glifos_latex.R` | FASE 2O: glifos Unicode que rompen pdflatex (ERR\_GLIFO\_LATEX) |
| 37 | `test_render_html_shots_truncamiento.R` | render\_html\_shots: capturas moviles truncadas en silencio |
| 38 | `test_workflow_state_flujo_b.R` | workflow-state: flujo\_b no se sella con la sola decision (regla #16) |
| 39 | `test_workflow_state_reopen.R` | workflow-state: reopen des-sella desde un paso y registra el motivo (regla #16) |
| 40 | `test_presupuesto_contexto_reglas.R` | Presupuesto de contexto de .claude/rules/ (compactas + texto integro) |
| 41 | `test_barras_campeonato_clave.R` | barras-campeonato-baloncesto-n3: clave y figuras leidas del dibujo (+7 mutantes) y cotejo con la ficha MAT-2026-1-015 |
| 42 | `test_barras_campeonato_cloze.R` | barras-campeonato-baloncesto-n3 CLOZE: 6 claves leidas del dibujo y del texto (+5 mutantes, Semilleros) |
| 43 | `test_post_exams2_informe.R` | Hook post-render: el informe llega al modelo en JSON, avisa si no halla el .Rmd y 2I-2L con ruta relativa (v6.2) |
| 44 | `test_post_exams2_cwd.R` | Hook post-render v6.3: el ejercicio sale del .Rmd y no del cwd; no escribe en el repo ni sella (7 casos + mutantes) |
| 45 | `test_doc_suites_sincronizada.R` | Documento de cobertura sincronizado: tabla de suites generada desde el runner, fases y version del hook |

En modo rápido (`R_TESTS_QUICK=1`, el del pre-push) corren siempre las 8 suites sin `watch` (1, 2, 5, 7, 9, 10, 13 y 40). Las otras 37 corren solo si cambió algún archivo que vigilan.
<!-- SUITES:FIN -->

---

## El Hook post-exams2 — Validación Continua

`post-exams2-validation.sh` (v6.3) es un hook `PostToolUse` sobre `Bash`, con timeout de 300 s (`.claude/settings.json`). Corre solo si el comando Bash termina con 0: si falla, Claude Code dispara `PostToolUseFailure` y el hook no corre (verificado el 2026-10-28).

Se activa cuando el comando contiene una llamada `exams2*("archivo.Rmd")` (comillas dobles o simples), con ruta absoluta o relativa al `cwd` de la sesión. Si el primer argumento no es una cadena literal (una variable, `rep(...)`, `c(...)`), no se activa. Si el `.Rmd` no existe visto desde ese `cwd`, avisa al modelo de que el arsenal no corrió.

Antes del arsenal hace `cd` a ese `cwd`. Hasta la v6.1, con ruta relativa y el proceso del hook en otro directorio (por ejemplo, invocado a mano desde la raíz), las fases 2I-2L callaban: 0 errores contra 2 con ruta absoluta, con el mismo `.Rmd`. Ejecuta 15 fases:

| Fase | Qué hace | Efecto en el resumen |
|------|----------|----------------------|
| 2A | Coherencia matemática (`validar_coherencia_matematica.R`) | Error si el script falla |
| 2B | Preview PDF → PNG con `magick -density 150` del PDF más reciente (menos de 5 min) | Error si la conversión falla; aviso si no hay PDF o falta `magick` |
| 2C-2F | `arsenal_validacion_completa.R`: opciones únicas (2C), ortografía (2D), metadatos ICFES (2E), estructura metacognitiva (2F) | Suma los totales que reporta el script |
| 2G | Multi-semilla con N = 100 (regla #23, `validar_multisemilla.R`) | Error; solo corre si no hubo errores antes |
| 2H | Stress test visual: 10 semillas con render real (`SOURCES/scripts_validacion/stress_test_visual.R`) | Error; solo corre si no hubo errores antes |
| 2I | Errores 16-17: imagen Markdown sin `{width=...}`, `\pandocbounded` usado sin definir en los `.tex`, `exshuffle: TRUE` con letra en la Solution | Error |
| 2J | Letter-independence de la Solution (regla #19): `ERR_SOL_LETRA_R`, `ERR_SOL_LETRA_CAT`, `ERR_SOL_LETRA_LITERAL` | Error |
| 2K | Tabla Markdown sin el guard del contador `none` (regla #20): `ERR_TABLA_NONE` | Error |
| 2L | Imágenes dentro del gap de un CLOZE (V5, Incidente G): `ERR_CLOZE_V5` | Error |
| 2M | Recordatorio de la auditoría visual HTML (`auditor-visual-html`); no la ejecuta | Ninguno; solo aparece si no hubo errores |
| 2N | Señales estáticas de diversidad cosmética (regla #22): `WARN_DIV_ESTATICA` | Aviso |
| 2O | Glifos que rompen pdflatex (regla #25): `ERR_GLIFO_LATEX` en Markdown visible, `WARN_GLIFO_LATEX` en código R | Error o aviso |

Al final suma errores y advertencias. Si en el `cwd` hay `ejercicio_state.json` y no hubo errores, sella los pasos 5 y 6 del workflow (`renderizado_4_formatos`, `arsenal_post_render`) y muestra el siguiente.

Frente al validator genérico:
- Se dispara solo tras cada render.
- Cubre validación matemática, visual, ortográfica, semántica y estructural.
- Genera previews visuales para inspección humana.

**Cómo llega el informe al modelo** (v6.2, verificado en vivo el 2026-10-28). Con exit 0, Claude Code manda el texto plano de un `PostToolUse` al log de depuración, no al modelo ([referencia de hooks](https://code.claude.com/docs/en/hooks.md#exit-code-0)). Por eso el hook escribe en stdout un único objeto JSON con tres campos:
- `hookSpecificOutput.additionalContext`: el informe sin marcos. Si no cabe, van de cada fase el encabezado y las líneas con ❌, ⚠️, `ERR_`, `WARN_` o `ERROR`, más el bloque final. Claude Code corta ese campo en 10 000 caracteres y del exceso solo muestra una vista previa de 2 000, así que el hook se queda en 9 000.
- `decision: "block"` + `reason`, si hay errores: Claude Code los pone junto al resultado del comando como «hook blocking error».
- `systemMessage`: una línea para el usuario con los totales y la ruta del informe.

El informe completo queda en `~/.cache/post-exams2-validation/<nombre>.txt`; con `POST_EXAMS2_INFORME_DIR` se cambia el directorio. Test: `tests/testthat/test_post_exams2_informe.R`, que corre el hook real con un proyecto falso; contra la v6.1 da 11 fallos y 4 errores.

**Lo que el hook NO hace:**
- **No impide el render.** Corre después, con el `.Rmd` ya renderizado. `decision: "block"` no deshace nada: solo hace que el modelo reciba la orden de corregir.
- **No corre si el comando falla ni si vence el timeout.** En el segundo caso Claude Code descarta toda su salida, pero el `.txt` en disco conserva lo que alcanzó a escribir.
- **Hasta la v6.1 su informe no llegaba a la conversación.** Disparado por Claude Code, el hook corría (lo prueba el atime de un `.Rmd` de prueba), pero su salida no aparecía; a mano, con la misma entrada, imprimía 116 líneas con 6 errores. Para correrlo a mano: sección «El hook post-render» de `A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/HANDOFF.md`.

---

## Git Hooks Nativos — Pre-Commit + Pre-Push

| Hook | Archivo | Qué valida |
|------|---------|-----------|
| Pre-commit | `.git/hooks/pre-commit` | Ortografía de los `.Rmd` en stage (`corregir_ortografia_espanol.R`); la suite completa solo con `PRECOMMIT_TESTS=1` |
| Pre-push | `.git/hooks/pre-push` → `.claude/hooks/pre-push.sh` (versionado) | Suite en modo rápido (`R_TESTS_QUICK=1`) sobre los archivos del push |

`.claude/hooks/pre-commit-ortografia.sh` es una plantilla de instalación que solo revisa ortografía. El `pre-commit` instalado hoy es otro script: revisa la ortografía y además corre los tests opcionales.

---

## CI/CD — GitHub Actions

Archivo: `.github/workflows/ci-testing.yml`

- Job único `tests-full` que ejecuta `Rscript tests/run_all_tests.R`
- Se activa en push y PR a `main` y `develop`, y a diario a las 02:00 UTC

---

## Conclusión

El agente `validator` CE **NO necesita ser invocado explícitamente** en este proyecto. Su rol está cubierto de forma más completa por:

1. **Hook post-exams2-validation.sh** → validación continua post-renderizado (su informe se lee invocándolo a mano; ver arriba)
2. **Suites testthat** → bloques `test_that` de unidad, integración y regresión (cifras en «Las suites de test»)
3. **Git hooks nativos** → ortografía en el pre-commit y suite en modo rápido en el pre-push
4. **CI/CD** → suite completa en GitHub Actions
5. **Detractor** → revisión adversarial en 8 dominios (FASE 2C del ciclo de validación, regla #9; no es la 2C del hook, que revisa opciones únicas)

Si se desea invocar una validación rápida similar al validator CE:

```bash
# Equivalente al validator CE en este proyecto: corre las suites sin `watch` y las que
# vigilan algún archivo cambiado frente a HEAD o todavía sin empujar.
R_TESTS_QUICK=1 Rscript tests/run_all_tests.R
```

---

**Versión**: 1.2 (2026-10-28: hook v6.2, cuyo informe llega al modelo; la tabla de suites la genera `tests/sincronizar_doc_suites.R` y la vigila `tests/testthat/test_doc_suites_sincronizada.R`). La 1.1 describía el hook v6.1 con la tabla escrita a mano; la 1.0, el hook v6.0 con 10 fases y 19 suites.
**Fecha**: 2026-10-28
