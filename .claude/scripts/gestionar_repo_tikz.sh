#!/bin/bash

# Script de gestión del Repositorio-Graficas-TikZ
# Proporciona utilidades para listar, buscar, validar y reindexar el repositorio

# Obtener directorio del script y calcular ruta absoluta del repositorio
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
REPO_DIR="$PROJECT_ROOT/Repositorio-Graficas-TikZ"
INDICE_FILE="$REPO_DIR/indice.json"

# Colores para output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Función de ayuda
mostrar_ayuda() {
    echo "Uso: $0 [comando] [opciones]"
    echo ""
    echo "Comandos disponibles:"
    echo "  listar [categoria]     Lista todas las gráficas, opcionalmente filtradas por categoría"
    echo "  buscar \"texto\"          Busca gráficas por texto en descripción o tags"
    echo "  validar                 Valida la integridad del repositorio"
    echo "  reindexar               Regenera el índice completo del repositorio"
    echo "  ayuda                   Muestra esta ayuda"
    echo ""
    echo "Ejemplos:"
    echo "  $0 listar geometria"
    echo "  $0 buscar \"cilindro volumen\""
    echo "  $0 validar"
    echo "  $0 reindexar"
}

# Verificar que el repositorio existe
verificar_repositorio() {
    if [ ! -d "$REPO_DIR" ]; then
        echo -e "${RED}Error: Repositorio no encontrado en $REPO_DIR${NC}"
        exit 1
    fi
    
    if [ ! -f "$INDICE_FILE" ]; then
        echo -e "${YELLOW}Advertencia: Índice no encontrado. Ejecuta 'reindexar' primero.${NC}"
    fi
}

# Listar gráficas
listar_graficas() {
    local categoria="$1"
    
    verificar_repositorio
    
    echo -e "${BLUE}=== Gráficas TikZ en Repositorio ===${NC}\n"
    
    if [ -z "$categoria" ]; then
        # Listar todas las categorías
        echo "Categorías disponibles:"
        for cat_dir in "$REPO_DIR"/*/; do
            if [ -d "$cat_dir" ]; then
                categoria_nombre=$(basename "$cat_dir")
                echo -e "\n${GREEN}$categoria_nombre${NC}"
                listar_en_categoria "$categoria_nombre"
            fi
        done
    else
        # Listar solo la categoría especificada
        if [ -d "$REPO_DIR/$categoria" ]; then
            echo -e "${GREEN}Categoría: $categoria${NC}\n"
            listar_en_categoria "$categoria"
        else
            echo -e "${RED}Error: Categoría '$categoria' no encontrada${NC}"
            exit 1
        fi
    fi
}

# Listar gráficas en una categoría
listar_en_categoria() {
    local categoria="$1"
    local categoria_path="$REPO_DIR/$categoria"
    local encontradas=0
    
    find "$categoria_path" -name "*.tikz" -type f | while read -r tikz_file; do
        encontradas=$((encontradas + 1))
        nombre_base=$(basename "$tikz_file" .tikz)
        subcategoria=$(basename "$(dirname "$tikz_file")")
        
        # Intentar leer metadata si existe
        json_file="${tikz_file%.tikz}.json"
        if [ -f "$json_file" ]; then
            descripcion=$(jq -r '.descripcion // "Sin descripción"' "$json_file" 2>/dev/null || echo "Sin descripción")
            tags=$(jq -r '.tags | join(", ")' "$json_file" 2>/dev/null || echo "")
            validado=$(jq -r '.validado // false' "$json_file" 2>/dev/null || echo "false")
            
            validado_icon="❌"
            if [ "$validado" = "true" ]; then
                validado_icon="✅"
            fi
            
            echo "  $validado_icon $nombre_base"
            echo "     Subcategoría: $subcategoria"
            echo "     Descripción: $descripcion"
            if [ -n "$tags" ]; then
                echo "     Tags: $tags"
            fi
            echo ""
        else
            echo "  ⚠️  $nombre_base (sin metadata)"
            echo "     Subcategoría: $subcategoria"
            echo ""
        fi
    done
    
    if [ $encontradas -eq 0 ]; then
        echo "  (No se encontraron gráficas en esta categoría)"
    fi
}

# Buscar gráficas
buscar_graficas() {
    local texto_busqueda="$1"
    
    if [ -z "$texto_busqueda" ]; then
        echo -e "${RED}Error: Proporciona un texto de búsqueda${NC}"
        echo "Uso: $0 buscar \"texto a buscar\""
        exit 1
    fi
    
    verificar_repositorio
    
    echo -e "${BLUE}=== Buscando: '$texto_busqueda' ===${NC}\n"
    
    local encontradas=0
    
    # Buscar en archivos JSON (metadata)
    find "$REPO_DIR" -name "*.json" -type f | while read -r json_file; do
        # Buscar en descripción y tags
        if grep -qi "$texto_busqueda" "$json_file" 2>/dev/null; then
            encontradas=$((encontradas + 1))
            nombre_base=$(basename "$json_file" .json)
            categoria=$(basename "$(dirname "$(dirname "$json_file")")")
            subcategoria=$(basename "$(dirname "$json_file")")
            
            descripcion=$(jq -r '.descripcion // "Sin descripción"' "$json_file" 2>/dev/null || echo "Sin descripción")
            tags=$(jq -r '.tags | join(", ")' "$json_file" 2>/dev/null || echo "")
            
            echo -e "${GREEN}✓ $nombre_base${NC}"
            echo "   Categoría: $categoria > $subcategoria"
            echo "   Descripción: $descripcion"
            if [ -n "$tags" ]; then
                echo "   Tags: $tags"
            fi
            echo "   Ruta: $json_file"
            echo ""
        fi
    done
    
    if [ $encontradas -eq 0 ]; then
        echo -e "${YELLOW}No se encontraron gráficas que coincidan con '$texto_busqueda'${NC}"
    else
        echo -e "${GREEN}Total encontradas: $encontradas${NC}"
    fi
}

# Validar integridad del repositorio
validar_repositorio() {
    verificar_repositorio
    
    echo -e "${BLUE}=== Validando Repositorio TikZ ===${NC}\n"
    
    local errores=0
    local advertencias=0
    
    # Verificar estructura de directorios
    echo "Verificando estructura de directorios..."
    for categoria in geometria estadistica probabilidad; do
        if [ ! -d "$REPO_DIR/$categoria" ]; then
            echo -e "${YELLOW}⚠ Advertencia: Categoría '$categoria' no existe${NC}"
            advertencias=$((advertencias + 1))
        fi
    done
    
    # Verificar archivos TikZ
    echo -e "\nVerificando archivos TikZ..."
    find "$REPO_DIR" -name "*.tikz" -type f | while read -r tikz_file; do
        nombre_base=$(basename "$tikz_file" .tikz)
        dir_tikz=$(dirname "$tikz_file")
        
        # Verificar que existe JSON correspondiente
        json_file="$dir_tikz/$nombre_base.json"
        if [ ! -f "$json_file" ]; then
            echo -e "${RED}✗ Error: Falta metadata JSON para $nombre_base.tikz${NC}"
            errores=$((errores + 1))
        else
            # Validar JSON
            if ! jq empty "$json_file" 2>/dev/null; then
                echo -e "${RED}✗ Error: JSON inválido en $json_file${NC}"
                errores=$((errores + 1))
            fi
        fi
        
        # Verificar que existe PNG (advertencia, no error)
        png_file="$dir_tikz/$nombre_base.png"
        if [ ! -f "$png_file" ]; then
            echo -e "${YELLOW}⚠ Advertencia: Falta preview PNG para $nombre_base.tikz${NC}"
            advertencias=$((advertencias + 1))
        fi
    done
    
    # Verificar índice
    echo -e "\nVerificando índice..."
    if [ -f "$INDICE_FILE" ]; then
        if ! jq empty "$INDICE_FILE" 2>/dev/null; then
            echo -e "${RED}✗ Error: Índice JSON inválido${NC}"
            errores=$((errores + 1))
        else
            echo -e "${GREEN}✓ Índice válido${NC}"
        fi
    else
        echo -e "${YELLOW}⚠ Advertencia: Índice no existe. Ejecuta 'reindexar'${NC}"
        advertencias=$((advertencias + 1))
    fi
    
    # Resumen
    echo -e "\n${BLUE}=== Resumen de Validación ===${NC}"
    if [ $errores -eq 0 ] && [ $advertencias -eq 0 ]; then
        echo -e "${GREEN}✓ Repositorio válido${NC}"
    else
        if [ $errores -gt 0 ]; then
            echo -e "${RED}✗ Errores encontrados: $errores${NC}"
        fi
        if [ $advertencias -gt 0 ]; then
            echo -e "${YELLOW}⚠ Advertencias: $advertencias${NC}"
        fi
    fi
    
    exit $errores
}

# Regenerar índice
reindexar() {
    verificar_repositorio
    
    echo -e "${BLUE}=== Regenerando Índice ===${NC}\n"
    
    # Crear estructura JSON inicial
    cat > "$INDICE_FILE" << 'EOF'
{
  "version": "1.0",
  "fecha_actualizacion": "",
  "total_graficas": 0,
  "categorias": {
    "geometria": {
      "total": 0,
      "subcategorias": {
        "cilindros": [],
        "rectas": [],
        "parabolas": []
      }
    },
    "estadistica": {
      "total": 0,
      "subcategorias": {
        "barras": [],
        "puntos": [],
        "histogramas": []
      }
    },
    "probabilidad": {
      "total": 0,
      "subcategorias": {
        "arboles_decision": [],
        "diagramas_venn": []
      }
    }
  },
  "graficas": []
}
EOF
    
    # Obtener fecha actual
    fecha_actual=$(date +"%Y-%m-%d")
    
    # Contar y agregar gráficas
    local total=0
    
    find "$REPO_DIR" -name "*.tikz" -type f | while read -r tikz_file; do
        nombre_base=$(basename "$tikz_file" .tikz)
        categoria=$(basename "$(dirname "$(dirname "$tikz_file")")")
        subcategoria=$(basename "$(dirname "$tikz_file")")
        
        json_file="${tikz_file%.tikz}.json"
        
        if [ -f "$json_file" ]; then
            # Leer metadata del JSON
            metadata=$(cat "$json_file")
        else
            # Crear metadata básica
            metadata=$(jq -n \
                --arg id "$nombre_base" \
                --arg cat "$categoria" \
                --arg subcat "$subcategoria" \
                '{id: $id, categoria: $cat, subcategoria: $subcat, descripcion: "Sin descripción", tags: [], validado: false}')
        fi
        
        # Agregar al índice usando jq
        jq --argjson graf "$metadata" \
           --arg cat "$categoria" \
           --arg subcat "$subcategoria" \
           '.graficas += [$graf] |
            .categorias[$cat].subcategorias[$subcat] += [$graf.id] |
            .categorias[$cat].total += 1 |
            .total_graficas += 1 |
            .fecha_actualizacion = "'"$fecha_actual"'"' \
           "$INDICE_FILE" > "${INDICE_FILE}.tmp" && mv "${INDICE_FILE}.tmp" "$INDICE_FILE"
        
        total=$((total + 1))
    done
    
    echo -e "${GREEN}✓ Índice regenerado${NC}"
    echo "Total de gráficas indexadas: $total"
    echo "Índice guardado en: $INDICE_FILE"
}

# Procesar comando
case "$1" in
    listar)
        listar_graficas "$2"
        ;;
    buscar)
        buscar_graficas "$2"
        ;;
    validar)
        validar_repositorio
        ;;
    reindexar)
        reindexar
        ;;
    ayuda|help|--help|-h)
        mostrar_ayuda
        ;;
    *)
        echo -e "${RED}Error: Comando no reconocido: $1${NC}\n"
        mostrar_ayuda
        exit 1
        ;;
esac

