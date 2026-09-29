# Regla #17 — Infraestructura `.claude/` Protegida (versión compacta)

> Texto íntegro —comandos de verificación de cada invariante, incidente Ruflo 2026-04-25, tabla de conflictos, convivencia ADR-001—: `.claude/docs/reglas/infraestructura-protegida.md`.

**Principio.** `.claude/` (CLAUDE.md, settings.json, hooks/, rules/, agents/, skills/, scripts/) es zona protegida: todo cambio pasa por backup verificable + verificación post-cambio + reversibilidad. Sin degradación silenciosa. ICFES prevalece sobre cualquier plataforma externa.

## Invariantes (test: `Rscript tests/testthat/test_infraestructura_claude.R`)
- **I-1** `CLAUDE.md` raíz identifica el repo como ICFES en su primera línea.
- **I-2** `.claude/CLAUDE.md` es el índice ICFES de reglas críticas.
- **I-3** `settings.json` engancha `pre-write-rmd-gate.sh` (PreToolUse Write|Edit) y `post-exams2-validation.sh` (PostToolUse Bash).
- **I-4** Las reglas existen en `.claude/rules/` y no están vacías (texto íntegro en `.claude/docs/reglas/`).
- **I-5** Los 10 agentes ICFES existen.
- **I-6** Los 4 hooks `.sh` son ejecutables y pasan `bash -n`; `.git/hooks/pre-push` delega en `.claude/hooks/pre-push.sh`.
- **I-7** Existe `.claude.pre-ruflo-20260425-123652.tar.gz` (no borrarlo).
- **I-8** Hash SHA-256 de `.claude/helpers/*.cjs` coincide con `tests/testthat/ruflo-helpers.sha256`.
- **I-9** `tools:` de los agentes en PascalCase (`Read`, `Bash`…).
- **I-10** Los 4 validadores compartidos de `.claude/scripts/` son symlinks a `SOURCES/scripts_validacion/` (editar allí).

## Antes de `init`, `init --force`, `doctor --fix` o cambios masivos a `.claude/`
1. `tar -czf .claude.pre-<plataforma>-<TS>.tar.gz .claude/` (+ copias de settings.json y CLAUDE.md).
2. Ejecutar. 3. Verificar invariantes. 4. Si fallan: revertir (archivo → tarball → `git checkout`).

Prohibido: `init --force`/`doctor --fix` sin backup; reemplazar CLAUDE.md por plantilla genérica; editar helpers internos de Ruflo; confiar en wrappers que no invocan los hooks ICFES.

**Versión:** 1.3 · compacta desde 2026-09-29 · Excepciones: NINGUNA.
