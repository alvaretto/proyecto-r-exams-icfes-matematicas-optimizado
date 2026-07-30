# Regla #17 — Infraestructura `.claude/` Protegida

## Principio Fundamental

**La infraestructura ICFES en `.claude/` (CLAUDE.md, settings.json, hooks/, rules/, agents/ ICFES, skills/ ICFES, scripts/) es zona protegida. Toda modificación que la afecte DEBE pasar por backup verificable + verificación post-cambio + reversibilidad explícita. NO se acepta degradación silenciosa de las invariantes ICFES.**

Esta regla NO tiene excepciones. Aplica a cambios manuales del usuario, instalaciones de plataformas externas (claude-flow, ruflo, ruv-swarm, flow-nexus), reinstalaciones (`init`, `init --force`, `init --wizard`) y migraciones (`doctor --fix`, scripts de upgrade).

---

## Origen: lecciones de la sesión Ruflo (2026-04-25)

El 2026-04-25 una instalación de Ruflo (`ruflo init` o equivalente) reemplazó silenciosamente:

| Archivo | Estado pre-Ruflo | Estado post-Ruflo (silencioso) | Detección |
|---|---|---|---|
| `.claude/settings.json` | 2 hooks ICFES (`pre-write-rmd-gate.sh`, `post-exams2-validation.sh`) | 12 hooks Ruflo (`hook-handler.cjs`) sin invocar los ICFES | 8 días después (sesión 2026-05-03) |
| `CLAUDE.md` (raíz) | Identidad ICFES con regla "16 reglas críticas" | "# Claude Code Configuration - RuFlo V3" genérico | Misma sesión |
| `.claude/CLAUDE.md` | Índice ICFES de 16 reglas | Intacto (suerte: no fue tocado) | Misma sesión |
| `.gitignore` | Sin entradas Ruflo | Con `.claude-flow/data/`, `.codex/`, `CLAUDE.local.md` | Diff visible |

**Impacto:** durante 8 días, 7 de 16 reglas críticas estuvieron documentadas pero su enforcement automático estaba roto (gate workflow, validación matemática post-render, preview visual, recordatorio tildes, validación semántica Nivel 4, correctitud Nivel 5, Stress Test Visual). Cualquier `.Rmd` creado en ese período pudo saltarse el gate y la validación post-exams2.

---

## Invariantes ICFES inviolables

Estos hechos DEBEN ser ciertos en cualquier momento. Si una intervención los rompe, hay que revertirla o re-establecerlos antes de cerrar la sesión.

### I-1 — Identidad ICFES en `CLAUDE.md` raíz

**Invariante:** la primera línea no-vacía de `CLAUDE.md` (raíz) debe identificar el repo como ICFES.

**Verificación:**
```bash
head -1 CLAUDE.md | grep -qE "(ICFES|Repositorio ICFES R/exams)"
```

**Si falla:** restaurar el bloque ICFES priority desde `git log` (commits que tocaron `CLAUDE.md`) o desde `git show <commit>:CLAUDE.md`.

### I-2 — Índice de 17 reglas en `.claude/CLAUDE.md`

**Invariante:** `.claude/CLAUDE.md` debe ser el índice ICFES con las **17 reglas críticas** (16 originales + esta regla #17).

**Verificación:**
```bash
grep -c "^[0-9]\{1,2\}\. \*\*" .claude/CLAUDE.md  # debe retornar >= 17
grep -q "Sistema de Generación Automatizada de Ejercicios ICFES" .claude/CLAUDE.md
```

**Si falla:** comparar contra `git show HEAD:.claude/CLAUDE.md` y restaurar.

### I-3 — Hooks ICFES enganchados en `settings.json`

**Invariante:** `.claude/settings.json` debe cargar al menos los dos hooks ICFES en sus matchers correctos.

**Verificación:**
```bash
python3 -c "
import json, sys
s = json.load(open('.claude/settings.json'))
pre = [h['command'] for m in s['hooks']['PreToolUse']
       if m['matcher'] in ('Write|Edit|MultiEdit', 'Write|Edit')
       for h in m['hooks']]
post = [h['command'] for m in s['hooks']['PostToolUse']
        if m['matcher'] == 'Bash'
        for h in m['hooks']]
assert any('pre-write-rmd-gate.sh' in c for c in pre), 'gate ICFES desenganchado'
assert any('post-exams2-validation.sh' in c for c in post), 'post-exams2 desenganchado'
print('I-3 OK')
"
```

**Si falla:** restaurar desde `.claude/settings.json.pre-icfes-rehook-*` o re-aplicar la edición convivencia (ver ADR-001).

### I-4 — Reglas ICFES presentes y no-vacías

**Invariante:** las 17 reglas deben existir como archivos en `.claude/rules/`.

**Verificación:**
```bash
test $(ls .claude/rules/*.md 2>/dev/null | wc -l) -ge 16
```

**Si falla:** restaurar desde `.claude.pre-ruflo-20260425-123652.tar.gz` o `git checkout`.

### I-5 — Agentes ICFES presentes

**Invariante:** los 10 agentes del workflow ICFES (clasificador, pedagogo, detractor, validador-visual, diagnosticador, corrector-coherencia, adversario, **orquestador-schoice**, **orquestador-cloze**, **auditor-visual-html**) deben existir.

**Verificación:**
```bash
for a in clasificador-icfes pedagogo-icfes agente-detractor validador-visual \
         diagnosticador-errores corrector-coherencia adversario \
         orquestador-schoice orquestador-cloze auditor-visual-html; do
  test -f ".claude/agents/${a}.md" || { echo "FALTA: $a"; exit 1; }
done
echo 'I-5 OK'
```

### I-6 — Hooks ejecutables y sintaxis válida

**Invariante:** los `.sh` ICFES deben ser ejecutables y `bash -n` debe pasar.
Son **4** desde 2026-07-29: los 3 originales más `pre-push.sh`, la versión
canónica versionada del hook `pre-push` (antes vivía solo en `.git/hooks/`,
fuera de control de versiones — ver regla #8 de `testing-obligatorio.md`).

**Verificación:**
```bash
for h in .claude/hooks/{pre-write-rmd-gate,post-exams2-validation,pre-commit-ortografia,pre-push}.sh; do
  test -x "$h" && bash -n "$h" || { echo "FAIL: $h"; exit 1; }
done
echo 'I-6 OK'
```

**Nota sobre `pre-push.sh`:** el que git ejecuta es `.git/hooks/pre-push`, que
**no** es versionable. Debe ser un wrapper que delegue en el canónico
propagando stdin y `"$@"` (el contrato `pre-push` depende de ambos). Si el
wrapper falta o deja de delegar, el hook corregido no se aplica aunque I-6 pase.
Verificación complementaria:
```bash
grep -q 'pre-push.sh' .git/hooks/pre-push && echo 'wrapper OK' || echo 'FAIL: wrapper no delega'
```

### I-7 — Backup pre-Ruflo preservado

**Invariante:** mientras existan riesgos de re-instalación, el tarball `.claude.pre-ruflo-20260425-123652.tar.gz` debe existir como red de seguridad.

**Verificación:**
```bash
test -f .claude.pre-ruflo-20260425-123652.tar.gz
```

**Si falla:** alertar y NO continuar. El backup es la única forma de restaurar el estado pre-Ruflo si todo lo demás fracasa.

### I-8 — Integridad de los helpers Ruflo (añadido v1.1, 2026-07-01)

**Invariante:** los helpers `.claude/helpers/{hook-handler,intelligence,statusline}.cjs`
—que se **autoejecutan en cada tool-use** con privilegios completos de Node— no deben
cambiar de contenido sin revisión. Se fija su hash SHA-256 de referencia; cualquier
`npx claude-flow update`/`init` que los altere debe detectarse y auditarse.

Motivación: auditoría de seguridad 2026-07-01. El código actual de estos helpers está
auditado y limpio (sin red, sin `exec`/`require` controlable por entrada externa), pero
Ruflo puede sobrescribirlos en cualquier actualización (ver antipatrón #5). El hash es la
única defensa contra una alteración silenciosa de la superficie que se autoejecuta.

**Verificación:**
```bash
sha256sum -c tests/testthat/ruflo-helpers.sha256
```

**Si falla:** revisar el `diff` del helper alterado buscando red (`fetch`/`http`),
ejecución (`child_process`/`exec`/`eval`), o exfiltración de entorno (`process.env` enviado
fuera). Solo si el cambio es benigno, regenerar la referencia:
`sha256sum .claude/helpers/*.cjs > tests/testthat/ruflo-helpers.sha256`.

**Nota de cadena de suministro (MCP de terceros):**
- `.mcp.json` NO debe usar `@latest` en `npx` para MCP de terceros: fijar versión o retirarlos.
- Los MCP instalados pero sin cablear (p. ej. `.mcps/`) son superficie latente: retirarlos o
  auditar sus `node_modules` (`npm audit` + grep de `postinstall`) antes de cablearlos.

**Test asociado:** `tests/testthat/test_infraestructura_claude.R` (I-8).

### I-9 — Herramientas de agentes en PascalCase válido (añadido v1.2, 2026-07-28)

**Invariante:** todo agente `.claude/agents/*.md` que declare `tools:` en su frontmatter
debe listar los nombres de herramienta en **PascalCase canónico** (`Read`, `Write`, `Edit`,
`MultiEdit`, `Bash`, `Grep`, `Glob`, `Task`, `WebFetch`, `WebSearch`, `TodoWrite`,
`NotebookEdit`). Claude Code **no reconoce** variantes en minúscula (`read`, `glob`,
`bash`, ...); un agente con `tools:` en minúscula se instancia **sin ninguna herramienta**
("would be spawned with zero tools — refusing").

Motivación: incidente 2026-07-28. 6 de los 10 agentes ICFES (`agente-detractor.md`,
`clasificador-icfes.md`, `corrector-coherencia.md`, `diagnosticador-errores.md`,
`pedagogo-icfes.md`, `validador-visual.md`) declaraban `tools:` en minúscula
(ej. `tools: [read, glob, grep, bash]`). El defecto pasó desapercibido porque el
frontmatter es sintácticamente válido YAML — solo se manifiesta al intentar lanzar el
agente vía `Task`, momento en que Claude Code rechaza la instanciación por falta de
herramientas. Fix aplicado: los 6 archivos se corrigieron a PascalCase, preservando
exactamente el mismo conjunto de capacidades por agente.

**Verificación:**
```bash
Rscript tests/testthat/test_infraestructura_claude.R
```
(el bloque `test_that("I-9: ...")` recorre cada `.claude/agents/*.md`, extrae la línea
`tools:` — con o sin corchetes — y falla si algún nombre no empieza en mayúscula o no
pertenece al conjunto válido).

**Si falla:** localizar el agente reportado y corregir manualmente la línea `tools:` al
PascalCase correspondiente (`read`→`Read`, `write`→`Write`, `glob`→`Glob`, `grep`→`Grep`,
`bash`→`Bash`, `webfetch`→`WebFetch`, `websearch`→`WebSearch`), sin alterar el conjunto de
capacidades ni ninguna otra línea del frontmatter. Re-ejecutar el test hasta que pase.

**Test asociado:** `tests/testthat/test_infraestructura_claude.R` (I-9).

---

## Procedimiento obligatorio antes de cualquier instalación/upgrade externo

Aplica antes de ejecutar cualquiera de:

- `npx @claude-flow/cli@latest init|init --force|init --wizard|doctor --fix`
- `npx ruflo@latest init`, `ruflo init`
- `npx ruv-swarm init`
- Comandos similares de plataformas que escriban en `.claude/`

### Paso 1 — Snapshot completo

```bash
TS=$(date +%Y%m%d-%H%M%S)
tar -czf ".claude.pre-${PLATAFORMA}-${TS}.tar.gz" .claude/
cp .claude/settings.json ".claude/settings.json.pre-${PLATAFORMA}-${TS}"
cp CLAUDE.md "CLAUDE.md.pre-${PLATAFORMA}-${TS}"
echo "Backup creado: .claude.pre-${PLATAFORMA}-${TS}.tar.gz"
```

### Paso 2 — Ejecutar la instalación

```bash
npx <plataforma>@latest <comando>
```

### Paso 3 — Verificar invariantes I-1 a I-9

Ejecutar el script `tests/testthat/test_infraestructura_claude.R` (creado por esta misma regla) o el equivalente:

```bash
Rscript tests/testthat/test_infraestructura_claude.R
```

Si **alguna invariante falla** → revertir con paso 4. NO continuar con la instalación nueva como si todo estuviera bien.

### Paso 4 — Reversibilidad si algo falla

```bash
# Plan A: revertir un solo archivo
cp ".claude/settings.json.pre-${PLATAFORMA}-${TS}" .claude/settings.json
cp "CLAUDE.md.pre-${PLATAFORMA}-${TS}" CLAUDE.md

# Plan B: revertir todo .claude/
rm -rf .claude/
tar -xzf ".claude.pre-${PLATAFORMA}-${TS}.tar.gz"

# Plan C (último recurso): si el snapshot se corrompió
git checkout -- .claude/ CLAUDE.md
```

---

## Conflictos entre plataformas externas y reglas ICFES

Cuando una herramienta externa (Ruflo, claude-flow, ruv-swarm, flow-nexus) recomienda algo que contradice una regla ICFES:

**ICFES prevalece. Sin excepciones.**

Esta regla está documentada en:
- `CLAUDE.md` raíz (sección "Reglas absolutas para este repo").
- `.claude/CLAUDE.md` (regla #17).
- `~/.claude/CLAUDE.md` global (sección "Adversario Documentado" + "Regla Obligatoria Global: Routing de Modelos").
- `ADR-001-convivencia-ruflo-icfes.md`.

Patrones específicos de conflicto y su resolución:

| Recomendación externa | Regla ICFES violada | Decisión |
|---|---|---|
| Ruflo: "usa hierarchical-mesh + 15 agentes" | #14 routing de modelos por complejidad | Ignorar; usar el routing ICFES |
| Ruflo: "init --force re-genera todo" | I-1 a I-9 | NO ejecutar sin snapshot previo |
| claude-flow doctor --fix | Puede tocar settings.json | Solo `doctor` (sin --fix) hasta validar diff |
| Ruflo skill X duplica skill ICFES | #6, #8, #16 | Mantener el ICFES, marcar el Ruflo como "no usar" |
| auto-memory bridge sin paquete | (memoria N2) | Vivir sin él hasta tener tiempo de instalar limpiamente |

---

## Convivencia con Ruflo (estado actual, 2026-05-03)

Decisión documentada en `ADR-001-convivencia-ruflo-icfes.md`:

- **Ruta seleccionada: B (convivencia)**.
- Hooks Ruflo + hooks ICFES coexisten en `settings.json` (3 PreToolUse Write|Edit|MultiEdit + 2 PostToolUse Bash).
- `CLAUDE.md` raíz mezclado: ICFES priority arriba (47 líneas) + Ruflo V3 abajo (244 líneas, marcado como descriptivo).
- MCP `ruflo` desregistrado (fallaba al conectarse). Quedan `ruv-swarm` y `flow-nexus` para funciones similares.
- Daemon Ruflo desactivado (bloque `claudeFlow.daemon` removido de `settings.json`).
- Memoria persistente Ruflo NO inicializada (paquete npm faltante; los `MEMORY.md` siguen siendo la fuente).
- Backup pre-Ruflo (`.claude.pre-ruflo-20260425-123652.tar.gz`) preservado como red de seguridad.

---

## Antipatrones PROHIBIDOS

### 1. Ejecutar `init --force` o `doctor --fix` sin backup

```bash
# ❌ PROHIBIDO
npx @claude-flow/cli@latest init --force

# ✓ CORRECTO
TS=$(date +%Y%m%d-%H%M%S)
tar -czf ".claude.pre-init-${TS}.tar.gz" .claude/
npx @claude-flow/cli@latest init --force
Rscript tests/testthat/test_infraestructura_claude.R
```

### 2. Sobrescribir `CLAUDE.md` con plantilla genérica de plataforma

Si una herramienta externa propone reemplazar `CLAUDE.md`, **rechazar** o aplicar el patrón de mezcla (ver `ADR-001-convivencia-ruflo-icfes.md` §"Mezcla de CLAUDE.md raíz").

### 3. Confiar en wrappers genéricos para invocar hooks ICFES

Verificación obligatoria:
```bash
grep -E "rmd-gate|post-exams2|ortografia|\.claude/hooks/" .claude/helpers/<wrapper>.cjs 2>/dev/null
```
Si retorna 0 coincidencias → el wrapper NO está invocando los hooks ICFES; hay que engancharlos directamente en `settings.json` (ver ADR-001).

### 4. Eliminar el tarball pre-Ruflo

`.claude.pre-ruflo-20260425-123652.tar.gz` (1.2 MB) es la única red de seguridad para volver al estado pre-Ruflo. **No lo borres** sin reemplazarlo por otro snapshot equivalente.

### 5. Modificar `.claude/helpers/hook-handler.cjs` u otros archivos Ruflo internos

Esos archivos pueden ser sobrescritos por cualquier `npx claude-flow update`. Si necesitas funcionalidad nueva, hazlo en el lado ICFES (settings.json + hooks/*.sh), no en el lado Ruflo.

---

## Test de regresión asociado

Esta regla tiene un test automático que verifica las 7 invariantes:

`tests/testthat/test_infraestructura_claude.R`

Se ejecuta automáticamente en `tests/run_all_tests.R` y en pre-push. Si falla, el commit/push se bloquea hasta que las invariantes se restauren.

---

## Referencias

- `ADR-001-convivencia-ruflo-icfes.md` — decisión arquitectónica.
- `.claude/docs/patrones-errores-conocidos.md` Errores 11-15 — casos concretos detectados en sesión 2026-05-03.
- `.claude/docs/INDICE_LECCIONES.md` — mapa unificado.
- Backup pre-Ruflo: `.claude.pre-ruflo-20260425-123652.tar.gz`.
- Backup pre-rehook: `.claude/settings.json.pre-icfes-rehook-20260503-171742`.

---

**Versión:** 1.3
**Fecha:** 2026-07-29 (v1.3 — I-6 pasa de 3 a 4 hooks con `pre-push.sh` + sub-invariante I-6b: contrato de stdin del pre-push y wrapper que delega; v1.2 2026-07-28 — I-9 PascalCase de `tools:` en agentes; v1.1 2026-07-01 — I-8 integridad helpers Ruflo + nota cadena de suministro; v1.0 2026-05-03)
**Estado:** ACTIVO Y OBLIGATORIO
**Excepciones:** NINGUNA
**Aplica a:** todo el ecosistema `.claude/` y archivos raíz `CLAUDE.md`, `CLAUDE.local.md`.
