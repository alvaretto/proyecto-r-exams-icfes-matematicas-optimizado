#!/bin/bash
# Ejemplo Básico 02: Validador de Caracteres Especiales LaTeX
# Nivel: Básico
# Propósito: Detectar caracteres especiales sin escape que rompen renderizado LaTeX

# Configuración
RMD_FILE="$1"
ERRORS=0
WARNINGS=0

# Colores
RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m'

echo "🔍 Validando caracteres especiales LaTeX en: $RMD_FILE"

# 1. Validar archivo
if [ ! -f "$RMD_FILE" ]; then
    echo -e "${RED}❌ Error: Archivo no encontrado${NC}"
    exit 1
fi

# 2. Extraer contenido fuera de bloques de código (solo markdown/texto plano)
# Excluir bloques de código R, Python, TikZ
CONTENT=$(awk '
BEGIN { in_code_block=0; in_chunk=0 }
/^```/ { 
    if (in_code_block==0) { in_code_block=1; next }
    else { in_code_block=0; next }
}
/^```\{r/ || /^```\{python/ { in_chunk=1; next }
/^```$/ && in_chunk==1 { in_chunk=0; next }
in_code_block==0 && in_chunk==0 { print }
' "$RMD_FILE")

# 3. Definir caracteres problemáticos y sus escapes correctos
declare -A PROBLEMATIC_CHARS=(
    ["&"]="\\&"
    ["%"]="\\%"
    ["$"]="\\\$"
    ["#"]="\\#"
    ["_"]="\\_"
    ["{"]="\\{"
    ["}"]="\\}"
)

# Caracteres que necesitan contexto especial
declare -A SPECIAL_CHARS=(
    ["^"]="\\^{}"
)

# 4. Función para detectar caracteres sin escape
check_unescaped_chars() {
    local char=$1
    local escape=$2
    local line_num=0
    
    while IFS= read -r line; do
        ((line_num++))
        
        # Saltar líneas que ya tienen el escape correcto
        if echo "$line" | grep -q "\\\\$char\|$escape"; then
            continue
        fi
        
        # Detectar caracteres sin escape
        if echo "$line" | grep -q "[^\\]$char\|^$char"; then
            # Contexto: no debe estar dentro de código inline (backticks)
            if ! echo "$line" | grep -q '`.*'"$char"'.*`'; then
                echo -e "${YELLOW}⚠ Línea $line_num: Carácter '$char' sin escape${NC}"
                echo -e "   ${BLUE}   Sugerencia: Reemplazar por '$escape'${NC}"
                echo -e "   ${BLUE}   Contexto: ${line:0:60}...${NC}"
                ((WARNINGS++))
            fi
        fi
    done <<< "$CONTENT"
}

# 5. Verificar cada carácter problemático
echo "📝 Verificando caracteres especiales..."

for char in "${!PROBLEMATIC_CHARS[@]}"; do
    escape="${PROBLEMATIC_CHARS[$char]}"
    echo "🔎 Verificando: '$char' → '$escape'"
    check_unescaped_chars "$char" "$escape"
done

# 6. Verificación especial para ^ (necesita contexto matemático)
echo "🔎 Verificando: '^' (requiere contexto matemático o \\^{})"
while IFS= read -r line; do
    line_num=$(echo "$line" | grep -n . | head -1 | cut -d: -f1 || echo "0")
    
    # Detectar ^ fuera de contexto matemático ($...$ o \[...\])
    if echo "$line" | grep -q '[^$\\]^[^$]' && ! echo "$line" | grep -q '\^{}'; then
        # No es un exponente en modo matemático válido
        if ! echo "$line" | grep -q '\$.*\^.*\$'; then
            echo -e "${YELLOW}⚠ Posible '^' sin escape (verificar contexto matemático)${NC}"
            ((WARNINGS++))
        fi
    fi
done <<< "$CONTENT"

# 7. Verificar que los escapes no estén duplicados (\\& es correcto, \\\\& es incorrecto)
echo "🔍 Verificando escapes duplicados..."
if echo "$CONTENT" | grep -q '\\\\\\\\.*[&%$#_{}]'; then
    echo -e "${YELLOW}⚠ Detectados posibles escapes duplicados (\\\\\\)${NC}"
    echo -e "   ${BLUE}   En markdown, use un solo backslash para escape${NC}"
    ((WARNINGS++))
fi

# 8. Resumen
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
if [ $ERRORS -eq 0 ] && [ $WARNINGS -eq 0 ]; then
    echo -e "${GREEN}✅ No se detectaron caracteres problemáticos${NC}"
    exit 0
else
    echo -e "${YELLOW}⚠ Validación completada: $WARNINGS advertencias encontradas${NC}"
    echo -e "${BLUE}💡 Revisa las líneas indicadas y aplica los escapes necesarios${NC}"
    exit 0  # Advertencias no bloquean, solo informan
fi
