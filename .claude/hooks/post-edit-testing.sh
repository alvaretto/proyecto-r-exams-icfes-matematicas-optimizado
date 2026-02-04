#!/bin/bash
# Post-edit testing hook
# Ejecuta tests después de modificar archivos para validar cambios

FILE_PATH="$1"

echo ""
echo "🔍 Validando cambios en: $FILE_PATH"
echo ""

# Detectar tipo de archivo modificado
if [[ "$FILE_PATH" == *validar_coherencia_matematica.R ]]; then
    echo "→ Script de validación matemática modificado"
    echo "→ Ejecutando tests de validación matemática..."
    Rscript -e "library(testthat); test_file('tests/testthat/test_validacion_matematica.R', reporter='summary')" 2>&1 | tail -20

elif [[ "$FILE_PATH" == *corregir_ortografia_espanol.R ]]; then
    echo "→ Script de ortografía modificado"
    echo "→ Ejecutando tests de ortografía..."
    Rscript -e "library(testthat); test_file('tests/testthat/test_ortografia_espanol.R', reporter='summary')" 2>&1 | tail -20

elif [[ "$FILE_PATH" == *.Rmd ]]; then
    echo "→ Archivo .Rmd modificado"
    echo "→ Ejecutando tests de renderizado..."
    Rscript -e "library(testthat); test_file('tests/testthat/test_renderizado_4_formatos.R', reporter='summary')" 2>&1 | tail -20

elif [[ "$FILE_PATH" == *test_*.R ]]; then
    echo "→ Test modificado"
    echo "→ Ejecutando suite completa..."
    Rscript tests/run_all_tests.R 2>&1 | tail -30

elif [[ "$FILE_PATH" == *.claude/scripts/* ]] || \
     [[ "$FILE_PATH" == *.claude/hooks/* ]] || \
     [[ "$FILE_PATH" == *.claude/rules/* ]]; then
    echo "→ Configuración Claude modificada"
    echo "→ Ejecutando tests de regresión..."
    Rscript -e "library(testthat); test_file('tests/testthat/test_regression_suite.R', reporter='summary')" 2>&1 | tail -20
else
    echo "→ Archivo no crítico - Omitiendo tests"
    exit 0
fi

if [ $? -ne 0 ]; then
    echo ""
    echo "❌ =============================================="
    echo "❌ TESTS FALLARON DESPUÉS DEL CAMBIO"
    echo "❌ =============================================="
    echo ""
    echo "Archivo: $FILE_PATH"
    echo ""
    echo "⚠️ ACCIÓN REQUERIDA:"
    echo "1. El cambio introdujo un error"
    echo "2. Revisar el código modificado"
    echo "3. Corregir el problema"
    echo "4. Los tests deben pasar antes de commit"
    echo ""
    exit 1
fi

echo ""
echo "✅ Todos los tests pasaron después del cambio"
echo ""
exit 0
