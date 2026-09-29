# Regla #25 — Glifos Unicode que rompen pdflatex (versión compacta)

> Texto íntegro —110 glifos medidos, calibración causal sobre 63 `.Rmd`—: `.claude/docs/reglas/glifos-latex-prohibidos.md`.

**Principio.** Ningún `.Rmd` contiene, en Markdown visible, un carácter que pdflatex no componga. Caso emblemático `✓` (U+2713): rompe el PDF y es invisible en HTML.

- **Seguros:** tildes españolas, `× ÷ ² ³ ° º § « » · ± — – … ‰ •`, comillas tipográficas, `← ↑ →`, `£ €`.
- **Rompen:** `✓ ✔ ✗ ✘ ✅ ❌`; `√ ≤ ≥ ≠ − ∈ ∩ ∪ ⊂ ∑ ∏ ∫ ∞ ≈ ∅ ∀ ∃ ≡ ∝ ∠ ∴`; griegas `π α β γ θ λ μ σ Ω Δ`; `↔ ⇒ ⇔ ↺`; `‣ ▪`; marcos `│ ─ ┌`; subíndices `₁ ₂`; `ℹ`; IPA; todo emoji; U+FE0F.
- **El modo math no salva:** `$a ≤ b$` falla; usar `$\le$`, `$\pi$`, `$\Delta$`, `$\sqrt{}$`…; en prosa `<=`, `>=`, `!=`.
- **Severidad por zona:** Markdown → `ERR_GLIFO_LATEX` (bloqueante); código R → `WARN_GLIFO_LATEX`; comentario R → inocuo.

**Defensa:** `.claude/scripts/validar_glifos_latex.R` + hook FASE 2O + `test_glifos_latex.R` con allowlist legacy de 29 archivos que **no admite altas**.

**Versión:** 1.0 · compacta desde 2026-09-29.
