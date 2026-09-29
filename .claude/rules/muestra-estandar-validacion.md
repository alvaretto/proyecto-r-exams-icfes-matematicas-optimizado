# Regla #23 — Muestra Estándar de Validación: N = 100 (versión compacta)

> Texto íntegro —deriva de los cinco N, costes medidos, ejemplo del estrato `LT--`—: `.claude/docs/reglas/muestra-estandar-validacion.md`.

**Principio.** Toda medición estadística sobre versiones usa **N = 100**, cableado como default en código. Invocar sin `--n` ya da el estándar.

- **Familia A** (sólo `data_generation`): `validar_diagnosticidad.R`, `validar_diversidad_sustantiva.R`, `validar_multisemilla.R` (el real está en `SOURCES/scripts_validacion/`, I-10) y verificadores propios: N = 100 sin excepción.
- **Familia B** (render real: `stress_test_visual.R`, `auditor-visual-html`): objetivo 100; si se usa menos, **declarar la cifra y la razón** junto al resultado.
- N ≠ 100 sólo en: depuración (se reporta el de 100), Familia B declarada, enumeración exhaustiva de un espacio finito, o análisis estratificado.
- **Estratos:** un estrato con n < 20 es **NO CONCLUYENTE** (nunca verde ni rojo); `N_necesario = N × 20 / n_min`; el verificador no puede sellar con estratos sin medir.
- No confundir con el umbral de producto 250+ únicas sobre 300 (regla #3 de `codigo-rmd.md`).
- Prohibido subir "por si acaso" (p. ej. 400) o copiar el N de un handoff.

**Defensa:** defaults = 100; timeout del hook ≥ 300 s (nunca bajarlo); `tests/testthat/test_muestra_estandar.R`.

**Versión:** 1.0 · compacta desde 2026-09-29.
