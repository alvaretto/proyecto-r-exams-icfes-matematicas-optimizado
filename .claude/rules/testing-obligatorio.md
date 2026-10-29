# Regla #8 — Testing Obligatorio y Automático (versión compacta)

> Texto íntegro —hooks git completos, contrato de stdin del pre-push, mensajes de bloqueo, métricas—: `.claude/docs/reglas/testing-obligatorio.md`.

**Principio.** Todo cambio pasa por validación de tests. Tolerancia cero a regresiones.

- **Suite:** `Rscript tests/run_all_tests.R`; completa con `R_TESTS_FULL=1` (obligatoria antes de promover o push relevante).
- **Pre-commit** (`.git/hooks/pre-commit`): ortografía de `.Rmd` staged; tests con `PRECOMMIT_TESTS=1`.
- **Pre-push:** canónico versionado en `.claude/hooks/pre-push.sh`; `.git/hooks/pre-push` es un wrapper que delega (`exec bash .../pre-push.sh "$@"`). Captura stdin una sola vez (`git lfs pre-push` lo consume): nunca pasar el stdin real a otro consumidor antes de calcular el rango. Si no parsea refs, NO exportar `R_TESTS_CHANGED_FILES`.
- **Leer el conteo de suites, no sólo el veredicto:** el modo quick salta suites; si una saltada cubre el commit, correr la completa.
- **Guarda nueva ⇒ su `watch`:** al añadir una guarda a una suite con `watch` (`tests/run_all_tests.R`), los archivos que vigila entran en ese `watch`; comprobarlo con el criterio de `.suite_afectada()`. Si no, el modo quick salta la guarda justo cuando cambia lo vigilado (2026-10-29: guardas sobre los orquestadores).
- Cambios en `.claude/` o `tests/` → correr los tests de regresión antes de commit.
- Diagnóstico: `PREPUSH_DEBUG_DETECT=1`.

**Prohibido:** `git commit --no-verify`, deshabilitar hooks, comentar tests que fallan, mockear para que pasen.

**Versión:** 1.2 (2026-10-29: guarda nueva ⇒ su `watch`) · compacta desde 2026-09-29 · Excepciones: NINGUNA.
