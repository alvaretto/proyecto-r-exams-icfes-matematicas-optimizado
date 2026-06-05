---
name: validate
description: Validación comprensiva del proyecto ICFES R/exams. 5 fases — lint, types/schemas, style/ortografía, unit tests, e2e.
model_recommendation: sonnet
---

# /validate — Comando de Validación Comprensiva ICFES R/exams

Ejecuta validación completa del proyecto en 5 fases progresivas.
Cada fase depende de la anterior. Si una fase falla, corregir antes de continuar.

---

## FASE 1: Linting — Sintaxis y Errores Estáticos

### 1A. R — lintr sobre directorios clave
```bash
Rscript -e '
library(lintr)
# Solo archivos R en Produccion y scripts de validación
lint_dir("../A-Produccion/Ejemplos-Funcionales-Rmd/", pattern = "\\.Rmd$")
' 2>&1 | head -50
```

### 1B. Bash — validación sintáctica de hooks y scripts
```bash
for f in .claude/hooks/*.sh .claude/scripts/*.sh; do
  bash -n "$f" && echo "✓ $f" || echo "❌ ERROR sintáctico: $f"
done
```

### 1C. R — sintaxis de archivos .Rmd críticos
```bash
Rscript -e '
# Verificar que .Rmd en Produccion no tengan errores de sintaxis R
files <- list.files("A-Produccion/03-En-Produccion", pattern = "\\.Rmd$",
                     recursive = TRUE, full.names = TRUE)
for (f in files[1:5]) {
  cat("Verificando:", basename(f), "\n")
  tryCatch({
    knitr::purl(f, output = tempfile(), quiet = TRUE)
    cat("  ✓ Sintaxis OK\n")
  }, error = function(e) cat("  ❌", e$message, "\n"))
}
'
```

**Gate FASE 1**: Todos los `bash -n` deben pasar. Si hay errores R, reportar y decidir.

---

## FASE 2: Type Checking — Schemas JSON y Estructura

### 2A. Validar exercise_state.json contra schema
```bash
Rscript -e '
library(jsonvalidate)
schema <- "../.claude/schemas/ejercicio_state.schema.json"
# Buscar todos los exercise_state.json
states <- list.files("A-Produccion", pattern = "ejercicio_state\\.json$",
                      recursive = TRUE, full.names = TRUE)
for (s in states) {
  validator <- jsonvalidate::json_validator(schema, engine = "ajv")
  result <- validator(s)
  if (result) {
    cat("✓", s, "\n")
  } else {
    cat("❌", s, "--", attr(result, "errors"), "\n")
  }
}
' 2>&1
```

### 2B. Validar invariantes I-1 a I-7 de infraestructura protegida
```bash
echo "=== Verificando Invariantes I-1 a I-7 ==="
echo ""

# I-1: Identidad ICFES en CLAUDE.md raíz
head -1 CLAUDE.md | grep -qE "(ICFES|Repositorio ICFES R/exams)" && echo "✓ I-1: Identidad ICFES en CLAUDE.md" || echo "❌ I-1: Identidad ICFES faltante"

# I-2: Índice de 17+ reglas en .claude/CLAUDE.md
echo "I-2: Reglas en .claude/CLAUDE.md: $(grep -c '^[0-9]\{1,2\}\. \*\*' .claude/CLAUDE.md)"

# I-3: Hooks ICFES enganchados en settings.json
python3 -c "
import json
s = json.load(open('.claude/settings.json'))
pre = [h['command'] for m in s['hooks']['PreToolUse'] for h in m.get('hooks', [])]
post = [h['command'] for m in s['hooks']['PostToolUse'] for h in m.get('hooks', [])]
assert any('pre-write-rmd-gate.sh' in c for c in pre), 'I-3 FAIL: gate ICFES desenganchado'
assert any('post-exams2-validation.sh' in c for c in post), 'I-3 FAIL: post-exams2 desenganchado'
print('✓ I-3: Hooks ICFES enganchados')
" 2>&1

# I-4: Reglas ICFES presentes
echo "I-4: Archivos en .claude/rules/: $(ls .claude/rules/*.md 2>/dev/null | wc -l)"

# I-5: Agentes ICFES presentes
for a in clasificador-icfes pedagogo-icfes agente-detractor validador-visual diagnosticador-errores corrector-coherencia adversario orquestador-schoice; do
  test -f ".claude/agents/${a}.md" && echo "✓ I-5: $a" || echo "❌ I-5 FALTA: $a"
done

# I-6: Hooks ejecutables y sintaxis válida
for h in .claude/hooks/{pre-write-rmd-gate,post-exams2-validation,pre-commit-ortografia}.sh; do
  test -x "$h" && bash -n "$h" && echo "✓ I-6: $(basename $h)" || echo "❌ I-6 FAIL: $h"
done

# I-7: Backup pre-Ruflo preservado
test -f .claude.pre-ruflo-20260425-123652.tar.gz && echo "✓ I-7: Backup pre-Ruflo presente" || echo "⚠ I-7: Backup no encontrado"
```

### 2C. Validar schemas JSON adicionales
```bash
Rscript -e '
library(jsonvalidate)
schemas <- list.files(".claude/schemas", pattern = "\\.schema\\.json$", full.names = TRUE)
for (schema_file in schemas) {
  cat("Schema:", basename(schema_file), "\n")
  tryCatch({
    v <- jsonvalidate::json_validator(schema_file, engine = "ajv")
    cat("  ✓ Schema válido\n")
  }, error = function(e) cat("  ❌", e$message, "\n"))
}
'
```

**Gate FASE 2**: Invariantes I-1 a I-7 deben pasar. Schemas JSON deben ser válidos.

---

## FASE 3: Style — Ortografía Española y Convenciones

### 3A. Ortografía en archivos .Rmd de desarrollo
```bash
Rscript .claude/scripts/corregir_ortografia_espanol.R A-Produccion/02-En-Desarrollo/ 2>&1 | tail -30
```

### 3B. Verificar regla #18: Markdown sin width en .Rmd
```bash
echo "=== Buscando imágenes Markdown sin {width=...} (Error 16) ==="
find A-Produccion/02-En-Desarrollo/ -name "*.Rmd" -exec grep -lnE '!\[[^]]*\]\([^)]+\.(png|jpg|jpeg|svg|pdf)\)' {} \; 2>/dev/null | while read f; do
  HITS=$(grep -nE '!\[[^]]*\]\([^)]+\.(png|jpg|jpeg|svg|pdf)\)' "$f" | grep -vE '!\[[^]]*\]\([^)]+\.(png|jpg|jpeg|svg|pdf)\)\{[^}]*width')
  if [ -n "$HITS" ]; then
    echo "❌ $f:"
    echo "$HITS" | sed 's/^/     /'
  fi
done
```

### 3C. Verificar regla #19: Solution letter-independence
```bash
echo "=== Buscando referencias a letras en Solution (Error 19) ==="
find A-Produccion/ -name "*.Rmd" -path "*/02-En-Desarrollo/*" -o -path "*/03-En-Produccion/*" | while read f; do
  SOL_BLOCK=$(awk '/^Solution[[:space:]]*$/{in_sol=1;next} /^Meta-information[[:space:]]*$/{in_sol=0} in_sol==1{print}' "$f" 2>/dev/null)
  if echo "$SOL_BLOCK" | grep -qE '`r[[:space:]]+(letra_correcta|letras\[)' 2>/dev/null; then
    echo "❌ ERR_SOL_LETRA_R: $f"
  fi
  if echo "$SOL_BLOCK" | grep -qE '\*\*Opci[oó]n[[:space:]]+[A-D]\b' 2>/dev/null; then
    echo "❌ ERR_SOL_LETRA_LITERAL: $f"
  fi
done
```

### 3D. Convenciones de nombres de archivo
```bash
echo "=== Verificando nomenclatura de archivos ==="
# Deben seguir: [tema]_[subtema]_metacognitivo_[competencia]_n[nivel]_[tipo]_v[N].Rmd
find A-Produccion/ -name "*.Rmd" ! -path "*/Ejemplos-Funcionales-Rmd/*" | while read f; do
  BASENAME=$(basename "$f")
  if ! echo "$BASENAME" | grep -qE '_metacognitivo_'; then
    echo "⚠ SIN metacognitivo en nombre: $BASENAME"
  fi
done
```

**Gate FASE 3**: 0 errores de ortografía. 0 violaciones de reglas #18 y #19.

---

## FASE 4: Unit Testing — Suite Completa

### 4A. Modo rápido (pre-push)
```bash
echo "=== Suite Completa (15 suites) ==="
R_TESTS_FULL=1 Rscript tests/run_all_tests.R 2>&1
```

### 4B. Si FASE 4A falla, identificar suites rotas
```bash
Rscript -e '
library(testthat)
suites <- list.files("tests/testthat", pattern = "^test_.*\\.R$", full.names = TRUE)
for (s in suites) {
  cat("\n=== ", basename(s), "===\n")
  tryCatch({
    testthat::test_file(s)
  }, error = function(e) cat("❌ ERROR:", e$message, "\n"))
}
'
```

**Gate FASE 4**: TODAS las suites deben pasar. Si alguna falla, corregir causa raíz antes de continuar.

---

## FASE 5: End-to-End — Flujos Completos de Usuario

### 5A. E2E-1: Crear ejercicio SCHOICE desde cero (sin gráficos)
```bash
# Simular el flujo completo de 11 pasos para un ejercicio de prueba
echo "=== E2E-1: Workflow SCHOICE sin gráficos ==="
echo "1. Verificar que /analizar-icfes funciona"
echo "2. Verificar que el gate pre-write-rmd bloquea sin state"
echo "3. Verificar que /generar-schoice produce .Rmd metacognitivo"
echo "4. Verificar renderizado 4 formatos"
echo "5. Verificar arsenal post-render 0 errores"
echo "6. Verificar diversidad ≥250 versiones"
echo ""
echo "Para ejecutar este flujo real:"
echo "  1. Crear directorio: mkdir A-Produccion/01-En-PreDesarrollo/test-e2e"
echo "  2. Iniciar state: .claude/scripts/workflow-state.sh init A-Produccion/01-En-PreDesarrollo/test-e2e --tipo schoice"
echo "  3. Analizar: /analizar-icfes [descripción del problema]"
echo "  4. Completar state y preguntar Flujo B"
echo "  5. Generar: /generar-schoice"
echo "  6. El hook post-exams2 se ejecuta automático"
```

### 5B. E2E-2: Validar ejercicio existente en Producción
```bash
echo "=== E2E-2: Validación de ejercicio en 03-En-Produccion ==="
# Tomar un ejercicio de producción y validarlo completamente
EJERCICIO=$(find A-Produccion/03-En-Produccion/ -name "*metacognitivo*.Rmd" -type f | head -1)
if [ -n "$EJERCICIO" ]; then
  echo "Ejercicio: $EJERCICIO"
  echo ""
  echo "1. Renderizado 4 formatos..."
  Rscript -e "exams::exams2html('$EJERCICIO', n = 1)" 2>&1 | tail -3
  Rscript -e "exams::exams2pdf('$EJERCICIO', n = 1)" 2>&1 | tail -3
  echo ""
  echo "2. Arsenal post-render..."
  echo "   (ejecutado automáticamente por hook post-exams2)"
  echo ""
  echo "3. Diversidad..."
  Rscript -e "
  versions <- list()
  for (i in 1:300) {
    set.seed(i)
    v <- exams::exams2html('$EJERCICIO', n = 1, quiet = TRUE)
    versions[[i]] <- digest::digest(v)
  }
  cat('   Versiones únicas:', length(unique(versions)), '/ 300\n')
  "
else
  echo "No se encontraron ejercicios en 03-En-Produccion/"
fi
```

### 5C. E2E-3: Verificar integridad del sistema de hooks
```bash
echo "=== E2E-3: Integridad del Sistema de Hooks ==="
echo "Hooks activos en settings.json:"
python3 -c "
import json
s = json.load(open('.claude/settings.json'))
for event in ['PreToolUse', 'PostToolUse']:
    for m in s['hooks'].get(event, []):
        matcher = m.get('matcher', '*')
        for h in m.get('hooks', []):
            cmd = h.get('command', '')[:80]
            print(f'  {event}[{matcher}]: {cmd}...')
" 2>&1

echo ""
echo "Hooks ICFES propios:"
for h in .claude/hooks/*.sh; do
  if [ -x "$h" ]; then
    echo "  ✓ $(basename $h) (ejecutable)"
  else
    echo "  ❌ $(basename $h) (NO ejecutable)"
  fi
done

echo ""
echo "Git hooks:"
for h in pre-commit pre-push; do
  if [ -x ".git/hooks/$h" ]; then
    echo "  ✓ $h (activo)"
  else
    echo "  ⚠ $h (no instalado)"
  fi
done
```

### 5D. E2E-4: Stress test visual de muestra
```bash
echo "=== E2E-4: Stress Test Visual ==="
EJERCICIO=$(find A-Produccion/03-En-Produccion/ -name "*metacognitivo*.Rmd" -type f | head -1)
if [ -n "$EJERCICIO" ] && [ -f "SOURCES/scripts_validacion/stress_test_visual.R" ]; then
  echo "Ejecutando stress test para: $(basename $EJERCICIO)"
  Rscript SOURCES/scripts_validacion/stress_test_visual.R "$EJERCICIO" --n 5 --output-dir /tmp/stress_test 2>&1 | tail -20
else
  echo "No se encontró ejercicio o script de stress test"
fi
```

---

## Resumen de Validación

```
╔══════════════════════════════════════════════════════╗
║  VALIDACIÓN COMPLETA ICFES R/EXAMS                   ║
╠══════════════════════════════════════════════════════╣
║  FASE 1: Lint (R + Bash)                             ║
║  FASE 2: Types/Schemas (JSON + Invariantes I-1..7)   ║
║  FASE 3: Style (Ortografía + Reglas #18 #19)         ║
║  FASE 4: Unit (15 suites, 130+ tests)                ║
║  FASE 5: E2E (Workflows completos de usuario)        ║
╚══════════════════════════════════════════════════════╝
```

## Reglas de Ejecución

1. **Progresivo**: Ejecutar fases en orden. Si una falla, corregir antes de continuar.
2. **Automático**: Las fases 1-4 pueden ejecutarse sin intervención.
3. **Fase 5 manual**: Requiere tener ejercicios en el pipeline.
4. **Fix real**: Si un test falla, corregir el código, NO el test.
5. **PROHIBIDO**: `--no-verify`, mockear para pasar tests, comentar tests fallidos.

## Uso Diario

```bash
# Validación rápida (pre-commit):
R_TESTS_QUICK=1 Rscript tests/run_all_tests.R

# Validación completa (pre-push o CI):
Rscript tests/run_all_tests.R

# Validación ultra-completa (5 fases):
/validate
```
