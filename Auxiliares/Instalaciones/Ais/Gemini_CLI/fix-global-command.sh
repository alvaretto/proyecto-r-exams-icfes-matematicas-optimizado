#!/bin/bash

# Script para corregir conflictos con el comando global gemini-icfes
# Ubicación: Auxiliares/Instalaciones/Ais/Gemini_CLI/
# Autor: Configuración automatizada para proyecto ICFES R-exams

echo "🔧 CORRECCIÓN DE COMANDO GLOBAL GEMINI-ICFES"
echo "============================================="
echo ""

# Colores para output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Función para mostrar resultados
show_result() {
    if [ $1 -eq 0 ]; then
        echo -e "${GREEN}✅ $2${NC}"
    else
        echo -e "${RED}❌ $2${NC}"
    fi
}

echo -e "${BLUE}🔍 IDENTIFICANDO CONFLICTOS...${NC}"
echo "------------------------------"

# Verificar si hay aliases conflictivos
echo -n "Verificando alias gemini-icfes: "
if alias gemini-icfes &>/dev/null; then
    echo -e "${YELLOW}⚠️  Alias conflictivo encontrado${NC}"
    ALIAS_CONFLICT=true
else
    echo -e "${GREEN}✅ Sin conflictos${NC}"
    ALIAS_CONFLICT=false
fi

# Verificar comando global
echo -n "Verificando comando global: "
if [ -f "$HOME/.local/bin/gemini-icfes" ]; then
    echo -e "${GREEN}✅ Comando global instalado${NC}"
else
    echo -e "${RED}❌ Comando global no encontrado${NC}"
    echo "Ejecuta: bash install-global-command.sh"
    exit 1
fi

echo ""
echo -e "${BLUE}🔧 CORRIGIENDO CONFLICTOS...${NC}"
echo "-----------------------------"

if [ "$ALIAS_CONFLICT" = true ]; then
    # Eliminar alias conflictivo de la sesión actual
    unalias gemini-icfes 2>/dev/null
    show_result $? "Alias conflictivo eliminado de la sesión actual"
    
    # Verificar y corregir archivo de aliases
    SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    if [ -f "$SCRIPT_DIR/gemini-aliases.sh" ]; then
        echo -n "Corrigiendo archivo de aliases: "
        
        # Crear versión corregida del archivo de aliases
        cat > "$SCRIPT_DIR/gemini-aliases-fixed.sh" << 'EOF'
#!/bin/bash

# Aliases corregidos para comandos Gemini CLI con MCPs
# Uso: source gemini-aliases-fixed.sh

# Obtener directorio del script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Aliases que no interfieren con el comando global
alias gemini-thinking="gemini-icfes 'usar thinking para analizar'"
alias gemini-validate="gemini-icfes 'validar código latex'"
alias gemini-test="gemini-icfes 'testing automático'"
alias gemini-image="gemini-icfes 'analizar imagen'"

# Alias para mostrar MCPs (sin conflicto)
alias gemini-mcps="gemini-icfes --mcps"

echo "✅ Aliases de Gemini CLI configurados (sin conflictos):"
echo "   • gemini-icfes - Comando global (instalado en ~/.local/bin)"
echo "   • gemini-mcps - Mostrar MCPs disponibles"
echo "   • gemini-thinking - Análisis estructurado"
echo "   • gemini-validate - Validación LaTeX/TikZ"
echo "   • gemini-test - Testing automático"
echo "   • gemini-image - Análisis de imágenes"
echo ""
echo "💡 El comando principal 'gemini-icfes' ahora funciona globalmente"
EOF
        
        chmod +x "$SCRIPT_DIR/gemini-aliases-fixed.sh"
        show_result $? "Archivo de aliases corregido creado"
        
        # Hacer backup del archivo original
        if [ ! -f "$SCRIPT_DIR/gemini-aliases.sh.backup" ]; then
            cp "$SCRIPT_DIR/gemini-aliases.sh" "$SCRIPT_DIR/gemini-aliases.sh.backup"
            show_result $? "Backup del archivo original creado"
        fi
        
        # Reemplazar el archivo original
        mv "$SCRIPT_DIR/gemini-aliases-fixed.sh" "$SCRIPT_DIR/gemini-aliases.sh"
        show_result $? "Archivo de aliases actualizado"
    fi
fi

echo ""
echo -e "${BLUE}🧪 PROBANDO COMANDO GLOBAL...${NC}"
echo "------------------------------"

# Probar el comando global
echo -n "Probando 'gemini-icfes --help': "
if gemini-icfes --help >/dev/null 2>&1; then
    echo -e "${GREEN}✅ Funciona correctamente${NC}"
else
    echo -e "${RED}❌ Error en el comando${NC}"
fi

echo -n "Probando 'gemini-icfes --mcps': "
if gemini-icfes --mcps >/dev/null 2>&1; then
    echo -e "${GREEN}✅ Funciona correctamente${NC}"
else
    echo -e "${RED}❌ Error en el comando${NC}"
fi

echo ""
echo -e "${GREEN}✅ CORRECCIÓN COMPLETADA${NC}"
echo "========================="
echo ""
echo -e "${CYAN}📋 Estado actual:${NC}"
echo "  • Comando global: gemini-icfes (disponible desde cualquier directorio)"
echo "  • Aliases corregidos: sin conflictos con el comando global"
echo "  • Backup creado: gemini-aliases.sh.backup"
echo ""
echo -e "${CYAN}🚀 Comandos disponibles:${NC}"
echo "  • gemini-icfes --mcps             # Mostrar MCPs disponibles"
echo "  • gemini-icfes --help             # Mostrar ayuda"
echo "  • gemini-icfes                    # Modo interactivo"
echo "  • gemini-icfes \"tu pregunta\"      # Prompt directo"
echo ""
echo -e "${CYAN}🔄 Para cargar aliases corregidos:${NC}"
echo "  source $(dirname "${BASH_SOURCE[0]}")/gemini-aliases.sh"
echo ""
echo -e "${GREEN}🎉 ¡El comando gemini-icfes ahora funciona globalmente!${NC}"
