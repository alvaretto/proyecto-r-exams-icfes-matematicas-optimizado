# Regla #10 — Validación `_neg_`: opciones repetidas (versión compacta)

> Texto íntegro —tests completos de las Variantes A y B, guía de opciones sinónimas, antipatrones con código—: `.claude/docs/reglas/validacion-neg-opciones-repetidas.md`.

**Principio.** Todo ejercicio con `_neg_` en el nombre (formato en `.claude/docs/NOMENCLATURA_ARCHIVOS_RMD.md`) incluye el test genérico de su patrón; todo ejercicio SIN `_neg_` verifica que TODAS las opciones son únicas.

- **Lógica negativa:** (N-1) opciones correctas equivalentes + 1 error; `sol` marca la opción con el **error**. Pregunta con `***NO***`. DOK 3, Bloom Evaluar.
- **Variante A (datos/gráficos):** `digest::digest()` → exactamente 2 hashes, frecuencias N-1 y 1, y el hash único es el marcado en `sol`; `colores_opciones` con N colores neutrales todos distintos.
- **Variante B (texto):** opciones etiquetadas `correcta1..N-1` + `error`; las correctas son **paráfrasis** (textos todos distintos, nunca copiar-pegar); la posición de `error` coincide con `sol`.
- **Sin `_neg_`:** `length(unique(hashes)) == length(letras)`.
- Solution: por qué la marcada es incorrecta y por qué las demás sí son correctas.

**Defensa:** `validar_5c_unicidad` (Nivel 5C, FASE 2A) auto-detecta la variante (`etiquetas_mezcladas`/`opciones_pre_mezcla`); test `tests/testthat/test_neg_variante_b.R`; detractor dominio `codigo_rexams`.

**Versión**: 2.1 · compacta desde 2026-09-29 · Excepciones: NINGUNA.
