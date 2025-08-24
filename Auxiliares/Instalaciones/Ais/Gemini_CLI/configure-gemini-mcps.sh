#!/bin/bash

# Script de configuración de MCPs para Gemini CLI
# Ubicación: Auxiliares/Instalaciones/Ais/Gemini_CLI/
# Autor: Configuración automatizada para proyecto ICFES R-exams
# Fecha: $(date)

echo "⚙️ CONFIGURACIÓN DE MCPs PARA GEMINI CLI"
echo "========================================"
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

# Obtener directorio del script y proyecto
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../../.." && pwd)"

echo -e "${BLUE}📋 VERIFICANDO PRERREQUISITOS...${NC}"
echo "-----------------------------------"

# Verificar Gemini CLI
echo -n "Verificando Gemini CLI: "
if command -v gemini &> /dev/null; then
    GEMINI_VERSION=$(gemini --version 2>/dev/null || echo "instalado")
    echo -e "${GREEN}✅ $GEMINI_VERSION${NC}"
else
    echo -e "${RED}❌ Gemini CLI no encontrado${NC}"
    echo "Por favor, instala Gemini CLI primero"
    exit 1
fi

# Verificar configuración MCP
echo -n "Verificando configuración MCP: "
if [ -f "$PROJECT_ROOT/.mcp-config.json" ]; then
    echo -e "${GREEN}✅ Encontrada${NC}"
else
    echo -e "${RED}❌ No encontrada${NC}"
    echo "Ejecuta: bash install-mcps.sh"
    exit 1
fi

echo ""
echo -e "${BLUE}⚙️ CONFIGURANDO INTEGRACIÓN CON GEMINI CLI...${NC}"
echo "----------------------------------------------"

# Crear configuración específica para Gemini CLI
cat > "$PROJECT_ROOT/.gemini-mcp-config.json" << EOF
{
  "mcpServers": {
    "thinking": {
      "command": "node",
      "args": ["$PROJECT_ROOT/.mcps/thinking-mcp/index.js"],
      "env": {}
    },
    "playwright-fixed": {
      "command": "node", 
      "args": ["$PROJECT_ROOT/.mcps/playwright-mcp-fixed/index.js"],
      "env": {}
    },
    "latex-validator": {
      "command": "node",
      "args": ["$PROJECT_ROOT/.mcps/latex-validator-mcp/index.js"],
      "env": {}
    },
    "image-analysis": {
      "command": "node",
      "args": ["$PROJECT_ROOT/.mcps/image-analysis-mcp/index.js"],
      "env": {}
    },
    "context7": {
      "command": "node",
      "args": ["$PROJECT_ROOT/.mcps/context7-mcp/dist/index.js"],
      "env": {
        "UPSTASH_REDIS_REST_URL": "\${UPSTASH_REDIS_REST_URL}",
        "UPSTASH_REDIS_REST_TOKEN": "\${UPSTASH_REDIS_REST_TOKEN}"
      }
    },
    "brave-search": {
      "command": "node",
      "args": ["$PROJECT_ROOT/.mcps/brave-search-mcp/index.js"],
      "env": {
        "BRAVE_API_KEY": "\${BRAVE_API_KEY}"
      }
    },
    "filesystem": {
      "command": "node",
      "args": ["$PROJECT_ROOT/.mcps/filesystem-servers/src/filesystem/dist/index.js"],
      "env": {
        "ALLOWED_DIRECTORIES": "$PROJECT_ROOT"
      }
    }
  }
}
EOF

show_result $? "Configuración Gemini MCP creada"

# Crear script optimizado para usar con MCPs
cat > "$SCRIPT_DIR/gemini-icfes-mcps.sh" << 'EOF'
#!/bin/bash

# Script optimizado para usar Gemini CLI con MCPs en proyecto ICFES
# Uso: bash gemini-icfes-mcps.sh [prompt]

# Obtener directorio del script y proyecto
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../../.." && pwd)"

# Verificar configuración
if [ ! -f "$PROJECT_ROOT/.gemini-mcp-config.json" ]; then
    echo "❌ Configuración MCP no encontrada"
    echo "Ejecuta: bash configure-gemini-mcps.sh"
    exit 1
fi

# Cambiar al directorio del proyecto
cd "$PROJECT_ROOT"

# Configurar variables de entorno si existen
if [ -f "$SCRIPT_DIR/mcp-env-setup.sh" ]; then
    source "$SCRIPT_DIR/mcp-env-setup.sh" 2>/dev/null
fi

# Ejecutar Gemini CLI con configuración MCP
if [ $# -eq 0 ]; then
    # Modo interactivo
    echo "🚀 Iniciando Gemini CLI con MCPs para proyecto ICFES..."
    echo "📋 MCPs disponibles: thinking, playwright-fixed, latex-validator, image-analysis"
    echo "💡 Usa comandos como: 'analizar ejercicio', 'validar latex', 'testing web'"
    echo ""
    gemini --config-file .gemini-mcp-config.json
else
    # Modo prompt directo
    gemini --config-file .gemini-mcp-config.json -p "$*"
fi
EOF

chmod +x "$SCRIPT_DIR/gemini-icfes-mcps.sh"
show_result $? "Script optimizado creado"

# Crear alias para facilitar uso
cat > "$SCRIPT_DIR/gemini-aliases.sh" << 'EOF'
#!/bin/bash

# Aliases para comandos Gemini CLI con MCPs
# Uso: source gemini-aliases.sh

# Obtener directorio del script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Alias principales
alias gemini-icfes="bash $SCRIPT_DIR/gemini-icfes-mcps.sh"
alias gemini-thinking="bash $SCRIPT_DIR/gemini-icfes-mcps.sh 'usar thinking para analizar'"
alias gemini-validate="bash $SCRIPT_DIR/gemini-icfes-mcps.sh 'validar código latex'"
alias gemini-test="bash $SCRIPT_DIR/gemini-icfes-mcps.sh 'testing automático'"
alias gemini-image="bash $SCRIPT_DIR/gemini-icfes-mcps.sh 'analizar imagen'"

echo "✅ Aliases de Gemini CLI configurados:"
echo "   • gemini-icfes - Modo interactivo con MCPs"
echo "   • gemini-thinking - Análisis estructurado"
echo "   • gemini-validate - Validación LaTeX/TikZ"
echo "   • gemini-test - Testing automático"
echo "   • gemini-image - Análisis de imágenes"
EOF

chmod +x "$SCRIPT_DIR/gemini-aliases.sh"
show_result $? "Aliases creados"

echo ""
echo -e "${BLUE}🔧 CONFIGURANDO INTEGRACIÓN CON VSCODE...${NC}"
echo "-------------------------------------------"

# Actualizar configuración VSCode para MCPs
if [ -f "$SCRIPT_DIR/vscode-mcp-tasks.json" ]; then
    # Agregar tareas para nuevos MCPs
    cat > "$SCRIPT_DIR/vscode-mcp-tasks-updated.json" << EOF
{
    "version": "2.0.0",
    "tasks": [
        {
            "label": "Gemini: Análisis Estructurado",
            "type": "shell",
            "command": "bash",
            "args": ["$SCRIPT_DIR/gemini-icfes-mcps.sh", "usar thinking para analizar el problema"],
            "group": "build",
            "presentation": {
                "echo": true,
                "reveal": "always",
                "focus": false,
                "panel": "new"
            }
        },
        {
            "label": "Gemini: Validar LaTeX/TikZ",
            "type": "shell",
            "command": "bash",
            "args": ["$SCRIPT_DIR/gemini-icfes-mcps.sh", "validar código latex del archivo actual"],
            "group": "build"
        },
        {
            "label": "Gemini: Testing Automático",
            "type": "shell",
            "command": "bash",
            "args": ["$SCRIPT_DIR/gemini-icfes-mcps.sh", "testing automático del ejercicio"],
            "group": "test"
        },
        {
            "label": "Gemini: Analizar Imagen",
            "type": "shell",
            "command": "bash",
            "args": ["$SCRIPT_DIR/gemini-icfes-mcps.sh", "analizar imagen para replicación tikz"],
            "group": "build"
        }
    ]
}
EOF
    
    mv "$SCRIPT_DIR/vscode-mcp-tasks-updated.json" "$SCRIPT_DIR/vscode-mcp-tasks.json"
    show_result $? "Tareas VSCode actualizadas"
else
    show_result 1 "Archivo de tareas VSCode no encontrado"
fi

echo ""
echo -e "${GREEN}✅ CONFIGURACIÓN COMPLETADA${NC}"
echo "============================"
echo ""
echo -e "${CYAN}📋 Archivos creados:${NC}"
echo "  • $PROJECT_ROOT/.gemini-mcp-config.json"
echo "  • $SCRIPT_DIR/gemini-icfes-mcps.sh"
echo "  • $SCRIPT_DIR/gemini-aliases.sh"
echo "  • $SCRIPT_DIR/vscode-mcp-tasks.json (actualizado)"
echo ""
echo -e "${CYAN}🚀 Comandos disponibles:${NC}"
echo "  • bash $SCRIPT_DIR/gemini-icfes-mcps.sh"
echo "  • source $SCRIPT_DIR/gemini-aliases.sh"
echo "  • gemini-icfes (después de cargar aliases)"
echo ""
echo -e "${YELLOW}⚠️  PRÓXIMOS PASOS:${NC}"
echo "  1. Cargar aliases: source $SCRIPT_DIR/gemini-aliases.sh"
echo "  2. Probar MCPs: bash $SCRIPT_DIR/test-mcps.sh"
echo "  3. Usar Gemini: gemini-icfes"
echo ""
echo -e "${GREEN}🎉 ¡MCPs configurados y listos para usar!${NC}"
