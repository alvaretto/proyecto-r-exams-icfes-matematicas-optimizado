#!/bin/bash
# Pre-bash testing hook
# Intercepta comandos git para validar tests antes de commit/push

COMMAND="$1"

# Detectar git commit (excepto --amend)
if [[ "$COMMAND" == *"git commit"* ]] && [[ "$COMMAND" != *"--amend"* ]]; then
    echo ""
    echo "🔒 =============================================="
    echo "🔒 BLOQUEO PRE-COMMIT ACTIVADO"
    echo "🔒 =============================================="
    echo ""
    echo "Ejecutando suite completa de tests..."
    echo ""

    # Ejecutar suite completa
    Rscript tests/run_all_tests.R

    if [ $? -ne 0 ]; then
        echo ""
        echo "❌ =============================================="
        echo "❌ COMMIT RECHAZADO - TESTS FALLARON"
        echo "❌ =============================================="
        echo ""
        echo "Acciones requeridas:"
        echo "1. Revisar errores de tests arriba ↑"
        echo "2. Corregir el código que causó la falla"
        echo "3. Volver a ejecutar: Rscript tests/run_all_tests.R"
        echo "4. Solo entonces hacer commit"
        echo ""
        echo "⚠️ PROHIBIDO usar: git commit --no-verify"
        echo "⚠️ Ese comando evade protecciones de calidad"
        echo ""
        exit 1
    fi

    echo ""
    echo "✅ =============================================="
    echo "✅ TESTS PASARON - COMMIT PERMITIDO"
    echo "✅ =============================================="
    echo ""
fi

# Detectar git push
if [[ "$COMMAND" == *"git push"* ]]; then
    echo ""
    echo "🔒 =============================================="
    echo "🔒 BLOQUEO PRE-PUSH ACTIVADO"
    echo "🔒 =============================================="
    echo ""

    # 1. Verificar que no hay cambios sin commit
    if ! git diff-index --quiet HEAD -- 2>/dev/null; then
        echo "❌ PUSH RECHAZADO"
        echo "Razón: Hay cambios sin commit"
        echo ""
        echo "Ejecutar primero:"
        echo "  git add ."
        echo "  git commit -m \"mensaje\""
        echo ""
        exit 1
    fi

    # 2. Ejecutar suite completa
    echo "Ejecutando validación final..."
    echo ""
    Rscript tests/run_all_tests.R

    if [ $? -ne 0 ]; then
        echo ""
        echo "❌ =============================================="
        echo "❌ PUSH RECHAZADO - TESTS FALLARON"
        echo "❌ =============================================="
        echo ""
        echo "La suite de tests debe pasar antes de push"
        echo ""
        exit 1
    fi

    # 3. Verificar CI/CD
    if [ ! -f ".github/workflows/ci-testing.yml" ]; then
        echo ""
        echo "⚠️ ADVERTENCIA: CI/CD no configurado"
        echo "Se recomienda configurar GitHub Actions para validación remota"
        echo ""
    fi

    echo ""
    echo "✅ =============================================="
    echo "✅ VALIDACIÓN FINAL COMPLETA - PUSH PERMITIDO"
    echo "✅ =============================================="
    echo ""
fi

exit 0
