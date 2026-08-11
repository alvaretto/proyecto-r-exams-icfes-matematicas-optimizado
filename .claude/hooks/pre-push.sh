#!/bin/bash
# =============================================================================
# Git Pre-Push Hook - Validación Final (versión canónica versionada)
# =============================================================================
# Ejecuta validación completa antes de push:
# 1. Git LFS (si está configurado)
# 2. Verificación de cambios sin commit
# 3. Tests de regresión (modo quick con detección de archivos cambiados)
#
# NUNCA usar: git push --no-verify
#
# INSTALACIÓN: .git/hooks/pre-push debe delegar a este archivo:
#   #!/bin/bash
#   exec bash "$(git rev-parse --show-toplevel)/.claude/hooks/pre-push.sh" "$@"
#
# CONTRATO DE STDIN (git pre-push): git entrega por stdin una línea por ref:
#   <local_ref> <local_sha> <remote_ref> <remote_sha>
# Ese stdin se puede leer UNA SOLA VEZ. `git lfs pre-push` también lo consume,
# así que se captura primero y se reparte a cada consumidor. Ver PREPUSH_DEBUG_DETECT.
# =============================================================================

set -e

echo "═══════════════════════════════════════"
echo "  PRE-PUSH: Validación Final"
echo "═══════════════════════════════════════"

# -----------------------------------------------------------------------------
# 0. CAPTURAR STDIN UNA SOLA VEZ
# -----------------------------------------------------------------------------
# Crítico: `git lfs pre-push` (paso 1) lee stdin hasta EOF. Si se le pasa el
# stdin real, el bucle de detección (paso 3) lee vacío, el rango del push nunca
# se calcula y la selección de suites queda guiada solo por los cambios locales
# sin commitear → suites relevantes saltadas con reporte "100% cobertura".
PREPUSH_STDIN=$(cat || true)

# -----------------------------------------------------------------------------
# 1. GIT LFS (si está disponible)
# -----------------------------------------------------------------------------
if command -v git-lfs >/dev/null 2>&1; then
    printf '%s\n' "$PREPUSH_STDIN" | git lfs pre-push "$@" 2>/dev/null || true
fi

# -----------------------------------------------------------------------------
# 2. VERIFICAR CAMBIOS SIN COMMIT
# -----------------------------------------------------------------------------
echo ""
echo "📋 Verificando estado del repositorio..."

if ! git diff-index --quiet HEAD -- 2>/dev/null; then
    echo ""
    echo "⚠️ =============================================="
    echo "⚠️ ADVERTENCIA: Hay cambios sin commit"
    echo "⚠️ =============================================="
    echo ""
    echo "Archivos modificados:"
    git diff --name-only HEAD
    echo ""
    echo "Considera hacer commit de estos cambios."
    echo ""
fi

# -----------------------------------------------------------------------------
# 3. DETECCIÓN DE ARCHIVOS CAMBIADOS (para el modo quick del runner)
# -----------------------------------------------------------------------------
ZERO_SHA="0000000000000000000000000000000000000000"
CHANGED_FILES=""
REFS_PARSED=0

while read -r local_ref local_sha remote_ref remote_sha; do
    [ -z "$local_sha" ] && continue
    REFS_PARSED=$((REFS_PARSED + 1))

    # Borrado de rama (local_sha vacío/cero): no aporta archivos
    if [ "$local_sha" = "$ZERO_SHA" ]; then
        continue
    fi

    if [ "$remote_sha" = "$ZERO_SHA" ]; then
        # Rama nueva en el remoto: diff contra la base con main
        NEW_FILES=$(git diff --name-only "main...$local_sha" 2>/dev/null \
                    || git diff --name-only "origin/main...$local_sha" 2>/dev/null \
                    || git show --pretty="" --name-only "$local_sha" 2>/dev/null \
                    || true)
    else
        # Rama existente: diff entre lo que ya está en el remoto y lo local
        NEW_FILES=$(git diff --name-only "$remote_sha..$local_sha" 2>/dev/null || true)
    fi

    CHANGED_FILES=$(printf '%s\n%s' "$CHANGED_FILES" "$NEW_FILES")
done <<< "$PREPUSH_STDIN"

# Agregar cambios locales no commiteados (también pueden romper tests)
LOCAL_CHANGES=$(git diff --name-only HEAD 2>/dev/null || true)
CHANGED_FILES=$(printf '%s\n%s' "$CHANGED_FILES" "$LOCAL_CHANGES")
# Normalizar: quitar líneas vacías y duplicados
CHANGED_FILES=$(printf '%s\n' "$CHANGED_FILES" | grep -v '^[[:space:]]*$' | sort -u || true)
N_CHANGED=$(printf '%s\n' "$CHANGED_FILES" | grep -c . || true)

if [ "$REFS_PARSED" -gt 0 ]; then
    # Detección confiable: el runner puede saltar suites sin relación
    export R_TESTS_CHANGED_FILES="$CHANGED_FILES"
    echo "🔎 Detección: $REFS_PARSED ref(s) en el push, $N_CHANGED archivo(s) afectado(s)"
else
    # Sin refs por stdin (invocación manual, stdin consumido, etc.): NO exportar
    # una lista parcial. Sin la variable, el runner usa su detección propia
    # (@{push}..) en vez de confiar en datos incompletos.
    unset R_TESTS_CHANGED_FILES
    echo "🔎 Detección: sin refs por stdin → se delega la detección al runner"
fi

# Modo diagnóstico: imprime la detección y termina sin correr tests.
#   printf '%s\n' "<local_ref> <local_sha> <remote_ref> <remote_sha>" \
#     | PREPUSH_DEBUG_DETECT=1 bash .claude/hooks/pre-push.sh origin <url>
if [ "${PREPUSH_DEBUG_DETECT:-0}" = "1" ]; then
    echo ""
    echo "── PREPUSH_DEBUG_DETECT: archivos detectados ──"
    printf '%s\n' "$CHANGED_FILES"
    echo "── fin (tests no ejecutados) ──"
    exit 0
fi

# -----------------------------------------------------------------------------
# 4. TESTS DE REGRESIÓN
# -----------------------------------------------------------------------------
# Desactivar con: PREPUSH_SKIP_TESTS=1 git push
# O permanentemente: git config hooks.prepush-tests false

SKIP_TESTS="${PREPUSH_SKIP_TESTS:-0}"
RUN_TESTS=$(git config --get hooks.prepush-tests 2>/dev/null || echo "true")

if [ "$SKIP_TESTS" = "1" ]; then
    echo ""
    echo "🧪 Tests omitidos (PREPUSH_SKIP_TESTS=1)"
elif [ "$RUN_TESTS" = "false" ]; then
    echo ""
    echo "🧪 Tests desactivados (hooks.prepush-tests=false)"
else
    echo ""
    echo "🧪 Ejecutando suite de tests..."

    if [ -f "tests/run_all_tests.R" ]; then
        if ! R_TESTS_QUICK=1 Rscript tests/run_all_tests.R; then
            echo ""
            echo "❌ =============================================="
            echo "❌ PUSH RECHAZADO: Tests fallaron"
            echo "❌ =============================================="
            echo ""
            echo "Acciones requeridas:"
            echo "1. Revisar errores de tests"
            echo "2. Corregir el código"
            echo "3. Ejecutar: R_TESTS_FULL=1 Rscript tests/run_all_tests.R"
            echo "4. Solo entonces: git push"
            echo ""
            exit 1
        fi
    else
        echo "   ⚠️ tests/run_all_tests.R no encontrado"
        echo "   Continuando sin tests..."
    fi
fi

# -----------------------------------------------------------------------------
# ÉXITO
# -----------------------------------------------------------------------------
echo ""
echo "✅ =============================================="
echo "✅ PRE-PUSH: Validación exitosa"
echo "✅ =============================================="
echo ""

exit 0
