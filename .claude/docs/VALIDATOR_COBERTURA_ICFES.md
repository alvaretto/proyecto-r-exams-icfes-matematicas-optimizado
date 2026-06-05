# Cobertura del Agente Validator CE en el Proyecto ICFES

## Principio

El agente `validator` del Context Engineering global tiene la función de "crear tests post-implementación y validar funcionalidad". En el proyecto ICFES R/exams, esta función está cubierta de forma **más comprensiva y automatizada** por la infraestructura existente.

---

## Mapeo: Validator CE → Infraestructura ICFES

| Responsabilidad Validator CE | Cobertura ICFES | Mecanismo |
|------------------------------|-----------------|-----------|
| Crear tests post-implementación | 15 suites testthat (130+ tests) | `tests/testthat/test_*.R` |
| Validar que cambios no rompen nada | Hook post-exams2 (FASE 2A-2J) | `post-exams2-validation.sh` |
| Ejecutar tests automáticamente | Git hooks nativos + CI/CD | `.git/hooks/pre-commit`, `.github/workflows/ci-testing.yml` |
| Verificar sintaxis/lint | Ortografía española + bash -n | `pre-commit-ortografia.sh`, `corregir_ortografia_espanol.R` |
| Validar cobertura de tests | Runner unificado con 15 suites | `tests/run_all_tests.R` |
| Prevenir regresiones | Test de regresión + detractor | `test_regression_suite.R`, FASE 2C |
| Validación visual | Preview automático PDF→PNG | FASE 2B del hook post-exams2 |

---

## Las 15 Suites de Test — Validación Unitaria

| # | Suite | Qué valida | Tipo |
|---|-------|-----------|------|
| 1 | `test_validacion_matematica.R` | Niveles 1-4: sintaxis, numérico, estructural, semántico | Unit |
| 2 | `test_validacion_semantica.R` | Capa A/B/C: precondiciones, keywords, cross-validación (35 tests) | Unit |
| 3 | `test_correctitud_respuesta.R` | Nivel 5A-5E: exsolution dinámico, cross-check, unicidad (14 tests) | Unit |
| 4 | `test_aleatorization_diversity.R` | ≥250/300 versiones únicas por ejercicio | Unit |
| 5 | `test_renderizado_4_formatos.R` | Renderizado real HTML/PDF/DOCX/NOPS | Integration |
| 6 | `test_flujo_b_graficador.R` | Workflow Graficador Experto (TikZ+Python+R) | Integration |
| 7 | `test_infraestructura_claude.R` | Invariantes I-1 a I-7 de infraestructura protegida | Meta |
| 8 | `test_ortografia_espanol.R` | Tildes y ortografía española en .Rmd | Style |
| 9 | `test_pandocbounded_y_solution_coherence.R` | Errores 16-17: Markdown sin width + exshuffle/Solution | Regression |
| 10 | `test_letter_independence.R` | Error 19: Solution sin referencias a letras | Regression |
| 11 | `test_media_mediana_moda.R` | Ejercicio específico validado (3 tests) | Integration |
| 12 | `test_neg_visual_distinctness.R` | Opciones _neg_ visualmente distintas (3 tests) | Unit |
| 13 | `test_stress_test_visual.R` | Stress test multi-semilla (28 tests) | E2E |
| 14 | `test_cloze_n3.R` | Ejercicios CLOZE nivel 3 | Integration |
| 15 | `test_regression_suite.R` | Regresiones generales del sistema | Regression |

---

## El Hook post-exams2 — Validación Continua

El hook `post-exams2-validation.sh` (v6.0) ejecuta **10 fases automáticas** después de cada `exams2*()`:

```
FASE 2A: Coherencia Matemática (.R script)
FASE 2B: Preview Visual (PDF → PNG via magick)
FASE 2C: Opciones Únicas (gráficos diferentes)
FASE 2D: Ortografía Española (tildes)
FASE 2E: Metadatos ICFES (6 dimensiones)
FASE 2F: Estructura Metacognitiva (Solution completa)
FASE 2G: Multi-semilla rápida (20 semillas, Nivel 5)
FASE 2H: Stress Test Visual (10 semillas, renderizado real + PNGs)
FASE 2I: Anti-\pandocbounded + coherencia Solution (Errores 16-17, regla #18)
FASE 2J: Letter-independence en Solution (Error 19, regla #19)
```

Esto es **más comprehensivo** que el validator genérico porque:
- Se ejecuta automáticamente (no hay que invocarlo manualmente)
- Cubre validación matemática, visual, ortográfica, semántica y estructural
- Bloquea la continuación si hay errores
- Genera previews visuales para inspección humana

---

## Git Hooks Nativos — Pre-Commit + Pre-Push

| Hook | Archivo | Qué valida |
|------|---------|-----------|
| Pre-commit | `.git/hooks/pre-commit` | Ortografía en .Rmd modificados |
| Pre-push | `.git/hooks/pre-push` | Suite completa de tests |

---

## CI/CD — GitHub Actions

Archivo: `.github/workflows/ci-testing.yml`

- Job único `tests-full` que ejecuta `Rscript tests/run_all_tests.R`
- Se activa en push y PR a main

---

## Conclusión

El agente `validator` CE **NO necesita ser invocado explícitamente** en este proyecto. Su rol está cubierto de forma más completa por:

1. **Hook post-exams2-validation.sh** → validación continua post-renderizado
2. **15 suites testthat** → 130+ tests unitarios, integración y regresión
3. **Git hooks nativos** → bloqueo pre-commit y pre-push
4. **CI/CD** → validación remota en GitHub Actions
5. **Detractor** → revisión adversarial en 8 dominios (FASE 2C)

Si se desea invocar una validación rápida similar al validator CE:

```bash
# Equivalente al validator CE en este proyecto:
R_TESTS_QUICK=1 Rscript tests/run_all_tests.R
```

---

**Versión**: 1.0
**Fecha**: 2026-05-23
