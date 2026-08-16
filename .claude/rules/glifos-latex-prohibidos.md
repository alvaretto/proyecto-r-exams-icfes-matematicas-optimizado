# Regla #25 — Glifos Unicode que rompen pdflatex

## Principio Fundamental

**Ningún `.Rmd` puede contener, en texto Markdown visible, un carácter Unicode que pdflatex no sepa componer. El caso emblemático es `✓` (U+2713): su presencia impide compilar el PDF, y el fallo es INVISIBLE en HTML.**

Las tildes españolas (`á é í ó ú ñ ü ¿ ¡`) son **obligatorias** por la regla #7 y **compilan sin problema**: esta regla no las toca. Tampoco toca los símbolos medidos como seguros (`× ÷ ° ² ³ º § « » · ± — – … ‰ • ← ↑ → £ €`).

---

## Origen: el defecto que sobrevivió meses (2026-08-15)

Un ejercicio de `03-En-Produccion/` llevaba tiempo sin compilar en PDF sin que nada lo detectara:

```
! LaTeX Error: Unicode character ✓ (U+2713) not set up for use with LaTeX
```

Confirmado en `probabilidad_intervalos_curva_..._n2_tikz_cloze_v1.Rmd` (7 ocurrencias) y su gemelo `..._v1_2.Rmd` (9), con la plantilla por defecto de R/exams **y** con la del proyecto (`solpcielo`).

**Por qué sobrevivió**: en HTML no afecta —no pasa por LaTeX— y **ningún validador del arsenal miraba los caracteres del fuente**. Quien validaba con `exams2html()` veía todo verde.

---

## Lo que se MIDIÓ (no se supuso)

Se renderizó `exams2pdf()` sobre un `.Rmd` mínimo por cada glifo, con el carácter en texto visible: **110 glifos distintos** más 2 controles (ASCII puro y tildes españolas), y 14 repetidos en contexto matemático. Tres resultados contradicen la intuición y por eso se documentan:

| Suposición razonable | Lo que dio la medición |
|---|---|
| "las flechas rompen" | `← ↑ →` **COMPILAN**; `↔ ⇒ ⇔ ↺` **NO**. El bloque Unicode no es homogéneo |
| "en modo math funcionan" | `$a ≤ b$` **falla igual** que `≤` en prosa. El math **no salva** |
| "si el archivo tiene el glifo, está roto" | Un glifo **sólo en comentario R** es inocuo: el archivo **compila** |

### Compilan (seguros, no tocar)

`× ÷ ² ³ ° º § « » · ± ª — – … ‰ • “ ” ‘ ’ ← ↑ → £ € ã ö ø œ` + tildes españolas.

### Rompen (prohibidos en Markdown visible)

- **Checks y cruces**: `✓ ✔ ✗ ✘ ✅ ❌`
- **Operadores matemáticos**: `√ ≤ ≥ ≠ − ∈ ∩ ∪ ⊂ ∑ ∏ ∫ ∞ ≈ ∅ ∀ ∃ ≡ ∝ ∠ ∴`
- **Letras griegas**: `π α β γ θ λ μ σ Ω Δ`
- **Flechas**: `↔ ⇒ ⇔ ↺` (pero NO `← ↑ →`)
- **Viñetas**: `‣ ▪` (pero NO `•`)
- **Marcos**: `│ ─ ┌ ┐ └ ┘` · **Subíndices**: `₁ ₂` · **Letterlike**: `ℹ`
- **IPA**: `ɛ ɑ ɔ ɶ ɒ ʌ ɤ ɯ ɨ ʉ` · **Emoji**: todos · **Selector de variación**: U+FE0F

---

## Severidad por zona: calibrada, no elegida

La zona decide si bloquea. Se midió sobre los **63 `.Rmd` del repositorio** que tenían glifos rompedores, comparando el render del original contra el del mismo archivo con los glifos sustituidos — prueba **causal**, para no atribuir al glifo un fallo ajeno:

| Zona | Rotos / concluyentes | Precisión | Severidad |
|---|---|---|---|
| Texto **Markdown** | **16 / 16** | **100 %** | `ERR_GLIFO_LATEX` — **bloqueante** |
| Sólo **código R** | 1 / 24 | 4 % | `WARN_GLIFO_LATEX` — advertencia |
| **Comentario R** | — | — | informativo (medido inocuo) |

**Por qué el código R no bloquea**: bloquear ahí habría marcado **23 archivos que sí compilan**. Una cadena R puede no emitirse nunca. El caso real que lo ilustra —y que justifica avisar igual— es:

```r
if (identical(typ, "pandoc")) "≤" else "\\le"
```

El **mismo** carácter es inocuo o letal según la rama, y eso **no es decidible estáticamente**.

---

## Fix

| En vez de | Escribir |
|---|---|
| `✓` decorativo al final de un encabezado | quitarlo (el texto ya suele decir "correcta") |
| `≤ ≥ ≠` en prosa | `<=`, `>=`, `!=` |
| `≤ π Δ √ ∩ ∈` en matemáticas | `$\le$`, `$\pi$`, `$\Delta$`, `$\sqrt{}$`, `$\cap$`, `$\in$` |
| emoji | quitarlo |

**El modo math no es un escape del glifo literal**: `$a ≤ b$` falla. Hay que usar el **comando** `\le`, no el carácter.

---

## Defensa Automática (3 capas)

1. **Detector único** — `.claude/scripts/validar_glifos_latex.R`. Rangos Unicode con rompedores medidos, menos las excepciones medidas que compilan. Un solo motor para hook y test, para que no deriven.
2. **Hook FASE 2O** — `post-exams2-validation.sh`. `ERR_GLIFO_LATEX` suma error; `WARN_GLIFO_LATEX` avisa. El hook corre **después** del render: no puede impedirlo, sólo marcar el resultado. Su valor es cazar el caso en que sólo se hizo `exams2html()`.
3. **Test de regresión** — `tests/testthat/test_glifos_latex.R` (suite del runner), con allowlist `glifos-latex-legacy.txt` que **NO admite altas y sólo puede decrecer**. Lleva control positivo (glifo visible dispara), dos controles negativos (comentario R y tildes/símbolos seguros no disparan) y un control de que la excepción de flechas no se coma su bloque.

---

## Excepciones

Los **29 archivos legacy** del allowlist, ninguno de los cuales admite compañía nueva. Los de `03-En-Produccion/` son inmutables (regla #2) y permanecen hasta que el usuario levante la inmutabilidad, como hizo con `Probabilidad-Intervalos-Curva-13-S1-2024B`.

---

**Versión:** 1.0
**Fecha:** 2026-08-15
**Estado:** ACTIVO Y OBLIGATORIO
**Aplica a:** todo `.Rmd` que renderice en PDF/NOPS.
