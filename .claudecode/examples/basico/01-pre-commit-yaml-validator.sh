#!/bin/bash
# Ejemplo Básico 01: Pre-commit Hook para Validación de YAML en archivos .Rmd
# Nivel: Básico
# Propósito: Detectar errores de sintaxis YAML y campos faltantes antes del commit

# Configuración
RMD_FILE="$1"
ERRORS=0
WARNINGS=0

# Colores para output
RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

echo "🔍 Validando YAML en: $RMD_FILE"

# 1. Validar que el archivo existe
if [ ! -f "$RMD_FILE" ]; then
    echo -e "${RED}❌ Error: Archivo no encontrado${NC}"
    exit 1
fi

# 2. Extraer bloque YAML (entre --- y ---)
YAML_BLOCK=$(awk '/^---$/{flag=1; next} /^---$/{flag=0} flag' "$RMD_FILE")

if [ -z "$YAML_BLOCK" ]; then
    echo -e "${RED}❌ Error: No se encontró bloque YAML (---)${NC}"
    ((ERRORS++))
    exit 1
fi

# 3. Validar campos obligatorios en YAML
echo "📋 Verificando campos obligatorios..."

REQUIRED_FIELDS=(
    "output"
    "header-includes"
)

for field in "${REQUIRED_FIELDS[@]}"; do
    if ! echo "$YAML_BLOCK" | grep -q "$field"; then
        echo -e "${RED}❌ Campo faltante: $field${NC}"
        ((ERRORS++))
    else
        echo -e "${GREEN}✓ Campo presente: $field${NC}"
    fi
done

# 4. Validar latex_engine específicamente
if ! echo "$YAML_BLOCK" | grep -q "latex_engine.*xelatex"; then
    echo -e "${YELLOW}⚠ Advertencia: latex_engine debería ser 'xelatex' para TikZ${NC}"
    ((WARNINGS++))
fi

# 5. Validar paquetes LaTeX críticos en header-includes
echo "📦 Verificando paquetes LaTeX críticos..."

REQUIRED_PACKAGES=(
    "tikz"
    "babel"
    "amsmath"
)

for package in "${REQUIRED_PACKAGES[@]}"; do
    if ! echo "$YAML_BLOCK" | grep -q "$package"; then
        echo -e "${YELLOW}⚠ Paquete recomendado faltante: $package${NC}"
        ((WARNINGS++))
    else
        echo -e "${GREEN}✓ Paquete presente: $package${NC}"
    fi
done

# 6. Validar sintaxis YAML básica (usando Python si está disponible)
if command -v python3 &> /dev/null; then
    echo "🔬 Validando sintaxis YAML..."
    
    TEMP_YAML=$(mktemp)
    echo "$YAML_BLOCK" > "$TEMP_YAML"
    
    python3 -c "import yaml; yaml.safe_load(open('$TEMP_YAML'))" 2>/dev/null
    YAML_SYNTAX=$?
    
    rm "$TEMP_YAML"
    
    if [ $YAML_SYNTAX -ne 0 ]; then
        echo -e "${RED}❌ Error de sintaxis YAML detectado${NC}"
        ((ERRORS++))
    else
        echo -e "${GREEN}✓ Sintaxis YAML válida${NC}"
    fi
fi

# 7. Resumen
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
if [ $ERRORS -eq 0 ] && [ $WARNINGS -eq 0 ]; then
    echo -e "${GREEN}✅ Validación completada sin errores ni advertencias${NC}"
    exit 0
elif [ $ERRORS -eq 0 ]; then
    echo -e "${YELLOW}⚠ Validación completada con $WARNINGS advertencias${NC}"
    exit 0
else
    echo -e "${RED}❌ Validación falló con $ERRORS errores y $WARNINGS advertencias${NC}"
    exit 1
fi
