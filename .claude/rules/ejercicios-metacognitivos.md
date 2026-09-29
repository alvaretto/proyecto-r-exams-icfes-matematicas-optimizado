# Regla #1 — Ejercicios Metacognitivos con Progressive Disclosure (versión compacta)

> Texto íntegro —patrones con ejemplos, taxonomía de códigos, tabla de 21 keywords semánticas, fundamento científico, v1.1—: `.claude/docs/reglas/ejercicios-metacognitivos.md`.

**Principio.** Todo `.Rmd` (SCHOICE o CLOZE) es metacognitivo y aplica Progressive Disclosure (comprender → analizar → evaluar → corregir). Prohibido el ejercicio puramente procedimental. Sin excepciones.

## Estructura
- **SCHOICE**: análisis de error ajeno, evaluación de afirmación o comparación de procedimientos; cada opción = un error conceptual.
- **CLOZE**: mínimo 4 partes (estándar del repo: 6): identificar (schoice) → calcular (num) → evaluar (mchoice) → transferir (V/F).
- **Pool de errores** (4-6): `codigo`, `nombre`, `descripcion_corta`, `descripcion_larga`, `causa_raiz`, `precondicion = function(params)`, `calcula`. Selección genérica filtrando por `precondicion`, nunca hardcoded.
- `calcula()` es **pura**: prohibido `sample`/`runif`/`rnorm`/… dentro; si depende del orden mostrado, usar `datos_presentados`.
- Pool de reflexiones metacognitivas; `test_that` de: errónea ≠ correcta, distractores únicos, `calcula` reproducible y no NA, precondición cumplida.
- **Solution** con: Análisis del error · Procedimiento correcto · Propiedades del concepto · Caso específico · Reflexión metacognitiva · Estrategia para evitar el error.
- Metadatos: `exextra[DOK]`, `[Bloom]` (el verbo REAL: *Aplicar* es legítimo), `[SOLO]`, `[TipoMetacognicion]`.

## Validación semántica automática (Nivel 4, `validar_coherencia_matematica.R`)
Capa A precondición (`ERR_SEM_A`) · Capa B keywords en descripciones (`ERR_SEM_B`/`WARN_SEM_B`) · Capa C calcula ≠ correcto (`ERR_SEM_C`) · Capa D determinismo (`ERR_SEM_D`/`WARN_SEM_D`).

## Nivel ICFES ↔ DOK
El Nivel ICFES es una **banda de puntaje del evaluado** (N1 0-35 … N4 71-100), no una escala cognitiva; DOK/Bloom son taxonomías externas. Sólo vale `DOK ≥ 3 ⇒ Nivel ≥ 3`; la recíproca es falsa (un ítem DOK 2 puede ser N4 por dificultad empírica). Nunca inflar el DOK para cuadrar; justificar un Nivel alto nombrando el obstáculo empírico.

**Versión**: 1.1 · compacta desde 2026-09-29 · Excepciones: NINGUNA.
