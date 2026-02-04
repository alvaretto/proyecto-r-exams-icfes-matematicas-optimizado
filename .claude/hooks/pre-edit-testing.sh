#!/bin/bash
# Pre-edit testing hook
# Ejecuta tests relevantes antes de modificar archivos críticos

FILE_PATH="$1"

# Detectar si es componente crítico
if [[ "$FILE_PATH" == *.claude/scripts/* ]] || \
   [[ "$FILE_PATH" == *.claude/hooks/* ]] || \
   [[ "$FILE_PATH" == *.claude/rules/* ]] || \
   [[ "$FILE_PATH" == *tests/* ]]; then

    echo "⚠️ COMPONENTE CRÍTICO DETECTADO: $FILE_PATH"
    echo "Ejecutando tests de validación..."

    # Ejecutar tests relacionados
    if [[ "$FILE_PATH" == *validar_coherencia_matematica.R ]]; then
        echo "→ Ejecutando tests de validación matemática..."
        Rscript -e "library(testthat); test_file('tests/testthat/test_validacion_matematica.R', reporter='summary')" 2>&1 | tail -20
    elif [[ "$FILE_PATH" == *corregir_ortografia_espanol.R ]]; then
        echo "→ Ejecutando tests de ortografía..."
        Rscript -e "library(testthat); test_file('tests/testthat/test_ortografia_espanol.R', reporter='summary')" 2>&1 | tail -20
    elif [[ "$FILE_PATH" == *test_*.R ]]; then
        echo "→ Test modificado - Ejecutando suite completa..."
        Rscript tests/run_all_tests.R 2>&1 | tail -30
    else
        echo "→ Ejecutando tests de regresión general..."
        Rscript -e "library(testthat); test_file('tests/testthat/test_regression_suite.R', reporter='summary')" 2>&1 | tail -20
    fi

    if [ $? -ne 0 ]; then
        echo ""
        echo "❌ =============================================="
        echo "❌ EDICIÓN BLOQUEADA - TESTS FALLARON"
        echo "❌ =============================================="
        echo ""
        echo "Archivo: $FILE_PATH"
        echo ""
        echo "ACCIÓN REQUERIDA:"
        echo "1. Los tests actuales están fallando"
        echo "2. Corregir problemas antes de editar"
        echo "3. NO proceder con edición hasta que tests pasen"
        echo ""
        exit 1
    fi

    echo ""
    echo "✅ Tests pasaron - Procediendo con edición"
    echo ""
fi

exit 0
