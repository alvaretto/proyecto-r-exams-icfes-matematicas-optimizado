#!/bin/bash
# Ejemplo Básico 03: Validador de Metadatos ICFES
# Nivel: Básico
# Propósito: Validar presencia y valores correctos de metadatos ICFES en archivos .Rmd

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

echo "🔍 Validando metadatos ICFES en: $RMD_FILE"

# 1. Validar archivo
if [ ! -f "$RMD_FILE" ]; then
    echo -e "${RED}❌ Error: Archivo no encontrado${NC}"
    exit 1
fi

# 2. Extraer bloque YAML
YAML_BLOCK=$(awk '/^---$/{flag=1; next} /^---$/{flag=0} flag' "$RMD_FILE")

if [ -z "$YAML_BLOCK" ]; then
    echo -e "${RED}❌ Error: No se encontró bloque YAML${NC}"
    exit 1
fi

# 3. Definir campos obligatorios de metadatos ICFES
declare -A REQUIRED_FIELDS=(
    ["icfes.competencia"]="interpretacion_representacion|formulacion_ejecucion|argumentacion"
    ["icfes.nivel_dificultad"]="^[1-4]$"
    ["icfes.contenido.categoria"]="algebra_calculo|geometria|estadistica"
    ["icfes.contenido.tipo"]="generico|no_generico"
    ["icfes.contexto"]="familiar|laboral|comunitario|matematico"
    ["icfes.eje_axial"]="eje[1-4]"
    ["icfes.componente"]="geometrico_metrico|numerico_variacional|aleatorio"
)

# 4. Función para extraer valor de campo anidado en YAML
get_yaml_field() {
    local field_path=$1
    local yaml_content=$2
    
    # Convertir ruta de puntos a búsqueda YAML
    # icfes.competencia → buscar "competencia:" bajo "icfes:"
    IFS='.' read -ra PARTS <<< "$field_path"
    
    local depth=0
    local in_section=false
    local found_value=""
    
    while IFS= read -r line; do
        # Detectar inicio de sección icfes
        if echo "$line" | grep -q "^icfes:"; then
            in_section=true
            depth=0
            continue
        fi
        
        # Si estamos fuera de sección icfes y encontramos otra clave de nivel superior, salir
        if ! $in_section && echo "$line" | grep -q "^[a-zA-Z]"; then
            break
        fi
        
        # Si estamos en sección icfes
        if $in_section; then
            # Detectar nivel de indentación
            local indent=$(echo "$line" | sed 's/^[[:space:]]*//' | sed "s/\(.*\).*/\1/" | wc -c)
            local indent_count=$((indent - 1))
            
            # Buscar el campo específico
            for i in "${!PARTS[@]}"; do
                local part="${PARTS[$i]}"
                if echo "$line" | grep -q "^[[:space:]]\{$indent_count\}$part:"; then
                    # Extraer valor
                    found_value=$(echo "$line" | sed "s/.*$part:[[:space:]]*//" | sed 's/"//g' | sed "s/'//g")
                    break 2
                fi
            done
        fi
    done <<< "$YAML_BLOCK"
    
    echo "$found_value"
}

# 5. Verificar cada campo obligatorio
echo "📋 Verificando campos obligatorios de metadatos ICFES..."

for field in "${!REQUIRED_FIELDS[@]}"; do
    expected_pattern="${REQUIRED_FIELDS[$field]}"
    
    # Extraer valor del campo
    field_value=$(get_yaml_field "$field" "$YAML_BLOCK")
    
    if [ -z "$field_value" ]; then
        echo -e "${RED}❌ Campo faltante: $field${NC}"
        ((ERRORS++))
    else
        # Validar patrón
        if echo "$field_value" | grep -qE "$expected_pattern"; then
            echo -e "${GREEN}✓ $field = '$field_value' (válido)${NC}"
        else
            echo -e "${RED}❌ $field = '$field_value' (inválido)${NC}"
            echo -e "   ${BLUE}   Valores esperados: $expected_pattern${NC}"
            ((ERRORS++))
        fi
    fi
done

# 6. Validación adicional: nivel_dificultad debe ser número entre 1-4
nivel=$(get_yaml_field "icfes.nivel_dificultad" "$YAML_BLOCK")
if [ -n "$nivel" ]; then
    if [[ ! "$nivel" =~ ^[1-4]$ ]]; then
        echo -e "${RED}❌ nivel_dificultad debe ser un número entre 1 y 4${NC}"
        ((ERRORS++))
    fi
fi

# 7. Validación de coherencia: competencia y componente deben ser compatibles
competencia=$(get_yaml_field "icfes.competencia" "$YAML_BLOCK")
componente=$(get_yaml_field "icfes.componente" "$YAML_BLOCK")

if [ -n "$competencia" ] && [ -n "$componente" ]; then
    # Validaciones de coherencia básicas (pueden expandirse)
    if [[ "$componente" == "aleatorio" ]] && [[ "$competencia" == "formulacion_ejecucion" ]]; then
        echo -e "${YELLOW}⚠ Advertencia: Combinación componente='aleatorio' con competencia='formulacion_ejecucion' puede ser inusual${NC}"
        ((WARNINGS++))
    fi
fi

# 8. Resumen
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
if [ $ERRORS -eq 0 ] && [ $WARNINGS -eq 0 ]; then
    echo -e "${GREEN}✅ Todos los metadatos ICFES son válidos${NC}"
    exit 0
elif [ $ERRORS -eq 0 ]; then
    echo -e "${YELLOW}⚠ Validación completada con $WARNINGS advertencias${NC}"
    exit 0
else
    echo -e "${RED}❌ Validación falló: $ERRORS errores encontrados${NC}"
    exit 1
fi
