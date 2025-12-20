#!/bin/bash
# .claudecode/workflows/validation_chain.sh
# Ejecutor principal de la cadena de validación

set -euo pipefail

# Obtener directorio base del proyecto
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

# Configuración
RMD_FILE="${1:-}"
MAX_ITERATIONS=5
ITERATION=0
CURRENT_FILE="$RMD_FILE"
WORK_DIR="/tmp/rexams_validation_workflow_$$"
VALIDATION_REPORT=""
FIX_REPORT=""
ERROR_ANALYSIS=""

# Colores
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Validar argumento
if [ -z "$RMD_FILE" ]; then
    echo -e "${RED}❌ Error: Debes proporcionar un archivo .Rmd${NC}" >&2
    echo "Uso: $0 <archivo.Rmd>" >&2
    exit 1
fi

# Convertir a ruta absoluta si es relativa
if [[ ! "$RMD_FILE" = /* ]]; then
    RMD_FILE="$(cd "$(dirname "$RMD_FILE")" && pwd)/$(basename "$RMD_FILE")"
fi

CURRENT_FILE="$RMD_FILE"

# Crear directorio de trabajo
mkdir -p "$WORK_DIR"

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "🔗 CADENA DE VALIDACIÓN AUTOMÁTICA ICFES"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Archivo: $RMD_FILE"
echo "Directorio de trabajo: $WORK_DIR"
echo ""

# Función de limpieza
cleanup() {
    # No eliminar backups automáticamente, dejar para revisión manual
    if [ -d "$WORK_DIR" ]; then
        echo ""
        echo -e "${BLUE}📁 Logs de trabajo guardados en: $WORK_DIR${NC}"
    fi
}

trap cleanup EXIT

# PASO 1: Validación Inicial
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}PASO 1: Validación Inicial${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"

VALIDATION_REPORT="${WORK_DIR}/validation_report.json"

# Ejecutar validadores básicos
VALIDATION_ERRORS=0
VALIDATION_WARNINGS=0

if ! bash "$PROJECT_ROOT/.claudecode/examples/basico/01-pre-commit-yaml-validator.sh" "$CURRENT_FILE" > "${WORK_DIR}/validation_yaml.txt" 2>&1; then
    ((VALIDATION_ERRORS++))
fi

if ! bash "$PROJECT_ROOT/.claudecode/examples/basico/03-metadata-icfes-validator.sh" "$CURRENT_FILE" > "${WORK_DIR}/validation_metadata.txt" 2>&1; then
    ((VALIDATION_ERRORS++))
fi

# Contar advertencias
VALIDATION_WARNINGS=$(grep -c "⚠" "${WORK_DIR}/validation_yaml.txt" "${WORK_DIR}/validation_metadata.txt" 2>/dev/null || echo 0)

if [ $VALIDATION_ERRORS -eq 0 ]; then
    echo -e "${GREEN}✓ Validación inicial pasada${NC}"
    VALIDATION_STATUS="PASSED"
else
    echo -e "${YELLOW}⚠ Validación inicial encontró $VALIDATION_ERRORS errores y $VALIDATION_WARNINGS advertencias${NC}"
    VALIDATION_STATUS="FAILED"
fi

# Generar reporte JSON
cat > "$VALIDATION_REPORT" <<EOF
{
  "status": "$VALIDATION_STATUS",
  "errors": $VALIDATION_ERRORS,
  "warnings": $VALIDATION_WARNINGS,
  "file": "$CURRENT_FILE",
  "timestamp": "$(date -Iseconds)"
}
EOF

# PASO 2: Corrección Automática (si hay errores críticos)
if [ "$VALIDATION_STATUS" = "FAILED" ] && [ $VALIDATION_ERRORS -gt 0 ]; then
    echo ""
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BLUE}PASO 2: Corrección Automática${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    
    # Crear backup
    cp "$CURRENT_FILE" "${CURRENT_FILE}.backup.$(date +%Y%m%d_%H%M%S)"
    
    echo -e "${YELLOW}⚠ Corrección automática requiere integración con agente Claude Code${NC}"
    echo -e "${BLUE}💡 En producción, aquí se ejecutaría el agente auto_fixer_icfes${NC}"
    echo -e "${BLUE}💡 Por ahora, revisa manualmente los errores en: ${WORK_DIR}/validation_*.txt${NC}"
    
    FIXED_FILE="${CURRENT_FILE}.fixed"
    cp "$CURRENT_FILE" "$FIXED_FILE"
    CURRENT_FILE="$FIXED_FILE"
fi

# BUCLE PRINCIPAL: Validar renderizado iterativamente
ITERATION=0
RENDER_SUCCESS=false

while [ $ITERATION -lt $MAX_ITERATIONS ] && [ "$RENDER_SUCCESS" = false ]; do
    ITERATION=$((ITERATION + 1))
    
    echo ""
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BLUE}PASO 3.$ITERATION: Renderizado de Prueba (Iteración $ITERATION/$MAX_ITERATIONS)${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    
    RENDER_DIR="${WORK_DIR}/render_${ITERATION}"
    mkdir -p "$RENDER_DIR"
    
    # Ejecutar renderizado
    if bash "$PROJECT_ROOT/.claudecode/examples/avanzado/07-skill-render-validator.sh" "$CURRENT_FILE" "$RENDER_DIR" > "${RENDER_DIR}/render_output.txt" 2>&1; then
        echo -e "${GREEN}✅ Renderizado exitoso en iteración $ITERATION${NC}"
        RENDER_SUCCESS=true
        break
    else
        echo -e "${RED}❌ Renderizado falló en iteración $ITERATION${NC}"
        
        # PASO 4: Análisis de Errores
        echo ""
        echo -e "${BLUE}PASO 4.$ITERATION: Análisis de Errores${NC}"
        
        ERROR_LOG="${RENDER_DIR}/render_errors.txt"
        ERROR_ANALYSIS="${RENDER_DIR}/error_analysis.json"
        
        if [ -f "$ERROR_LOG" ]; then
            echo -e "${YELLOW}⚠ Analizando errores...${NC}"
            
            # Extraer tipo de error básico del log
            ERROR_TYPE="UNKNOWN"
            if grep -qi "latex.*error\|LaTeX.*Error" "$ERROR_LOG"; then
                ERROR_TYPE="LATEX_ERROR"
            elif grep -qi "package.*not found\|Package.*not found" "$ERROR_LOG"; then
                ERROR_TYPE="PACKAGE_MISSING"
            elif grep -qi "syntax.*error\|Error.*:" "$ERROR_LOG"; then
                ERROR_TYPE="R_SYNTAX"
            elif grep -qi "yaml.*error\|YAML" "$ERROR_LOG"; then
                ERROR_TYPE="YAML_SYNTAX"
            fi
            
            # Extraer mensaje de error (primera línea significativa)
            ERROR_MSG=$(grep -i "error\|Error" "$ERROR_LOG" | head -1 | cut -c1-200 | sed 's/"/\\"/g' || echo "Error desconocido")
            
            cat > "$ERROR_ANALYSIS" <<EOF
{
  "error_type": "$ERROR_TYPE",
  "error_message": "$ERROR_MSG",
  "location": {
    "file": "$CURRENT_FILE",
    "line_number": null
  },
  "severity": "ERROR",
  "suggested_fix": [],
  "iteration": $ITERATION
}
EOF
            
            echo -e "${BLUE}📋 Tipo de error identificado: $ERROR_TYPE${NC}"
            echo -e "${BLUE}📄 Log de error completo: $ERROR_LOG${NC}"
        fi
        
        # PASO 5: Corrección Dirigida (solo si no es última iteración)
        if [ $ITERATION -lt $MAX_ITERATIONS ] && [ -f "$ERROR_ANALYSIS" ]; then
            echo ""
            echo -e "${BLUE}PASO 5.$ITERATION: Corrección Dirigida${NC}"
            
            echo -e "${YELLOW}⚠ Corrección dirigida requiere integración con skill error_analyzer${NC}"
            echo -e "${BLUE}💡 En producción, aquí se ejecutaría corrección específica según error_type${NC}"
            
            # Para demostración, crear nueva versión (en producción esto aplicaría correcciones reales)
            NEW_FIXED="${CURRENT_FILE}.iter${ITERATION}"
            cp "$CURRENT_FILE" "$NEW_FIXED"
            CURRENT_FILE="$NEW_FIXED"
        fi
    fi
done

# RESUMEN FINAL
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "📊 RESUMEN DE VALIDACIÓN"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

if [ "$RENDER_SUCCESS" = true ]; then
    echo -e "${GREEN}✅ ÉXITO: Archivo renderizado correctamente${NC}"
    echo "Archivo final: $CURRENT_FILE"
    echo "Iteraciones necesarias: $ITERATION"
    echo "Archivos generados en: $RENDER_DIR"
    exit 0
else
    echo -e "${RED}❌ FALLO: No se pudo renderizar después de $MAX_ITERATIONS iteraciones${NC}"
    echo "Archivo con errores: $CURRENT_FILE"
    echo ""
    echo -e "${YELLOW}📋 Próximos pasos sugeridos:${NC}"
    echo "1. Revisar logs en: $WORK_DIR"
    echo "2. Ver análisis de errores: ${WORK_DIR}/render_${ITERATION}/error_analysis.json"
    echo "3. Consultar guia_estilo_icfes.md para correcciones manuales"
    echo "4. Revisar ejemplos funcionales en A-Produccion/Ejemplos-Funcionales-Rmd/"
    exit 1
fi
