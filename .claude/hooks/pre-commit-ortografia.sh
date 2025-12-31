#!/bin/bash
# =============================================================================
# Hook: pre-commit-ortografia.sh
# Propósito: Validar ortografía española en archivos .Rmd antes de commit
#
# INSTALACIÓN:
#   cp .claude/hooks/pre-commit-ortografia.sh .git/hooks/pre-commit
#   chmod +x .git/hooks/pre-commit
#
# O agregar a pre-commit existente:
#   .claude/hooks/pre-commit-ortografia.sh
# =============================================================================

echo "🔍 Validando ortografía en archivos .Rmd..."

# Obtener archivos .Rmd modificados en este commit
RMD_FILES=$(git diff --cached --name-only --diff-filter=ACM | grep '\.Rmd$')

if [ -z "$RMD_FILES" ]; then
    echo "✅ No hay archivos .Rmd modificados."
    exit 0
fi

# Ruta al script de validación
SCRIPT_PATH=".claude/scripts/corregir_ortografia_espanol.R"

if [ ! -f "$SCRIPT_PATH" ]; then
    echo "⚠️ Script de validación no encontrado: $SCRIPT_PATH"
    echo "   Continuando sin validación ortográfica..."
    exit 0
fi

# Variables para tracking
ERRORES_TOTALES=0
ARCHIVOS_CON_ERRORES=""

# Validar cada archivo .Rmd
for file in $RMD_FILES; do
    if [ -f "$file" ]; then
        # Ejecutar validación y capturar salida
        OUTPUT=$(Rscript "$SCRIPT_PATH" "$file" 2>/dev/null | grep -E "^(Línea|ERRORES ORTOGRÁFICOS)")

        if echo "$OUTPUT" | grep -q "ERRORES ORTOGRÁFICOS"; then
            # Extraer número de errores
            NUM_ERRORES=$(echo "$OUTPUT" | grep "ERRORES ORTOGRÁFICOS" | grep -oP '\d+')
            if [ -n "$NUM_ERRORES" ]; then
                ERRORES_TOTALES=$((ERRORES_TOTALES + NUM_ERRORES))
                ARCHIVOS_CON_ERRORES="$ARCHIVOS_CON_ERRORES\n  - $file ($NUM_ERRORES errores)"
            fi
        fi
    fi
done

# Reportar resultado
if [ $ERRORES_TOTALES -gt 0 ]; then
    echo ""
    echo "=========================================="
    echo "⚠️  ERRORES ORTOGRÁFICOS DETECTADOS: $ERRORES_TOTALES"
    echo "=========================================="
    echo -e "Archivos afectados:$ARCHIVOS_CON_ERRORES"
    echo ""
    echo "Para corregir automáticamente:"
    echo "  Rscript .claude/scripts/corregir_ortografia_espanol.R <archivo> --fix"
    echo ""
    echo "O para ver detalles:"
    echo "  Rscript .claude/scripts/corregir_ortografia_espanol.R <archivo>"
    echo ""
    echo "El commit continuará, pero considera corregir estos errores."
    echo "=========================================="
    echo ""
fi

# Siempre permitir el commit (warning, no bloqueo)
exit 0
