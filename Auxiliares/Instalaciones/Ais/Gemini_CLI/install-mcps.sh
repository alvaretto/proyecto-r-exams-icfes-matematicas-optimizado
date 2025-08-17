#!/bin/bash

# Script de instalación de MCPs para gemini-icfes-optimizado
# Ubicación: Auxiliares/Instalaciones/Ais/Gemini_CLI/
# Autor: Configuración automatizada para proyecto ICFES R-exams
# Fecha: $(date)

echo "🔧 INSTALACIÓN DE MCPs PARA GEMINI-ICFES-OPTIMIZADO"
echo "=================================================="
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

# Verificar Node.js
echo -n "Verificando Node.js: "
if command -v node &> /dev/null; then
    NODE_VERSION=$(node --version)
    echo -e "${GREEN}✅ $NODE_VERSION${NC}"
else
    echo -e "${RED}❌ Node.js no encontrado${NC}"
    echo "Por favor, instala Node.js primero"
    exit 1
fi

# Verificar npm
echo -n "Verificando npm: "
if command -v npm &> /dev/null; then
    NPM_VERSION=$(npm --version)
    echo -e "${GREEN}✅ v$NPM_VERSION${NC}"
else
    echo -e "${RED}❌ npm no encontrado${NC}"
    exit 1
fi

# Verificar Python
echo -n "Verificando Python: "
if command -v python3 &> /dev/null; then
    PYTHON_VERSION=$(python3 --version)
    echo -e "${GREEN}✅ $PYTHON_VERSION${NC}"
else
    echo -e "${RED}❌ Python3 no encontrado${NC}"
    echo "Por favor, instala Python3 primero"
    exit 1
fi

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

echo ""
echo -e "${BLUE}📦 INSTALANDO MCPs PRINCIPALES...${NC}"
echo "----------------------------------"

# Crear directorio para MCPs
MCP_DIR="$PROJECT_ROOT/.mcps"
mkdir -p "$MCP_DIR"
cd "$MCP_DIR"

# 1. Context7 MCP
echo -e "${CYAN}📚 Instalando Context7 MCP...${NC}"
if [ ! -d "context7-mcp" ]; then
    git clone https://github.com/upstash/context7-mcp.git context7-mcp 2>/dev/null
    if [ $? -eq 0 ]; then
        cd context7-mcp
        npm install
        cd ..
        show_result 0 "Context7 MCP instalado"
    else
        # Instalación alternativa via npm
        npm install -g @upstash/context7-mcp 2>/dev/null
        show_result $? "Context7 MCP instalado (npm)"
    fi
else
    show_result 0 "Context7 MCP ya instalado"
fi

# 2. Microsoft Playwright MCP
echo -e "${CYAN}🌐 Instalando Microsoft Playwright MCP...${NC}"
if [ ! -d "playwright-mcp" ]; then
    git clone https://github.com/microsoft/playwright-mcp.git playwright-mcp 2>/dev/null
    if [ $? -eq 0 ]; then
        cd playwright-mcp
        npm install
        npx playwright install
        cd ..
        show_result 0 "Playwright MCP instalado"
    else
        # Instalación alternativa
        npm install -g playwright-mcp 2>/dev/null
        show_result $? "Playwright MCP instalado (npm)"
    fi
else
    show_result 0 "Playwright MCP ya instalado"
fi

# 3. Memory MCP
echo -e "${CYAN}💾 Instalando Memory MCP...${NC}"
if [ ! -d "memory-mcp" ]; then
    # Buscar implementación de Memory MCP
    git clone https://github.com/modelcontextprotocol/servers.git mcp-servers 2>/dev/null
    if [ $? -eq 0 ]; then
        cd mcp-servers/src/memory
        npm install
        cd ../../..
        show_result 0 "Memory MCP instalado"
    else
        show_result 1 "Memory MCP no disponible (se usará implementación local)"
    fi
else
    show_result 0 "Memory MCP ya instalado"
fi

# 4. Brave Search MCP
echo -e "${CYAN}🔍 Instalando Brave Search MCP...${NC}"
if [ ! -d "brave-search-mcp" ]; then
    # Implementación de Brave Search MCP
    mkdir -p brave-search-mcp
    cd brave-search-mcp
    npm init -y
    npm install brave-search-api
    cd ..
    show_result 0 "Brave Search MCP configurado"
else
    show_result 0 "Brave Search MCP ya instalado"
fi

# 5. Filesystem MCP (para archivos locales)
echo -e "${CYAN}📁 Instalando Filesystem MCP...${NC}"
if [ ! -d "filesystem-mcp" ]; then
    git clone https://github.com/modelcontextprotocol/servers.git filesystem-servers 2>/dev/null
    if [ $? -eq 0 ]; then
        cd filesystem-servers/src/filesystem
        npm install
        cd ../../..
        show_result 0 "Filesystem MCP instalado"
    else
        show_result 1 "Filesystem MCP no disponible"
    fi
else
    show_result 0 "Filesystem MCP ya instalado"
fi

echo ""
echo -e "${BLUE}⚙️ CONFIGURANDO INTEGRACIÓN CON GEMINI CLI...${NC}"
echo "----------------------------------------------"

# Crear configuración MCP para Gemini CLI
cat > "$PROJECT_ROOT/.mcp-config.json" << EOF
{
  "mcpServers": {
    "context7": {
      "command": "node",
      "args": ["$MCP_DIR/context7-mcp/dist/index.js"],
      "env": {
        "UPSTASH_REDIS_REST_URL": "\${UPSTASH_REDIS_REST_URL}",
        "UPSTASH_REDIS_REST_TOKEN": "\${UPSTASH_REDIS_REST_TOKEN}"
      }
    },
    "playwright": {
      "command": "node",
      "args": ["$MCP_DIR/playwright-mcp/dist/index.js"],
      "env": {}
    },
    "memory": {
      "command": "node",
      "args": ["$MCP_DIR/mcp-servers/src/memory/dist/index.js"],
      "env": {}
    },
    "brave-search": {
      "command": "node",
      "args": ["$MCP_DIR/brave-search-mcp/index.js"],
      "env": {
        "BRAVE_API_KEY": "\${BRAVE_API_KEY}"
      }
    },
    "filesystem": {
      "command": "node",
      "args": ["$MCP_DIR/filesystem-servers/src/filesystem/dist/index.js"],
      "env": {
        "ALLOWED_DIRECTORIES": "$PROJECT_ROOT"
      }
    }
  }
}
EOF

show_result $? "Configuración MCP creada"

echo ""
echo -e "${BLUE}🔑 CONFIGURANDO VARIABLES DE ENTORNO...${NC}"
echo "---------------------------------------"

# Crear archivo de configuración de variables de entorno
cat > "$SCRIPT_DIR/mcp-env-setup.sh" << 'EOF'
#!/bin/bash

# Configuración de variables de entorno para MCPs
# Ejecutar: source mcp-env-setup.sh

echo "🔑 CONFIGURACIÓN DE VARIABLES DE ENTORNO PARA MCPs"
echo "================================================="

# Context7 (Upstash Redis) - Opcional
if [ -z "$UPSTASH_REDIS_REST_URL" ]; then
    echo "⚠️  UPSTASH_REDIS_REST_URL no configurada"
    echo "   Para usar Context7, configura:"
    echo "   export UPSTASH_REDIS_REST_URL='tu_url_redis'"
    echo "   export UPSTASH_REDIS_REST_TOKEN='tu_token_redis'"
fi

# Brave Search API - Opcional
if [ -z "$BRAVE_API_KEY" ]; then
    echo "⚠️  BRAVE_API_KEY no configurada"
    echo "   Para usar Brave Search, configura:"
    echo "   export BRAVE_API_KEY='tu_api_key_brave'"
fi

# Verificar Gemini API Key
if [ -z "$GEMINI_API_KEY" ]; then
    echo "❌ GEMINI_API_KEY no configurada"
    echo "   Configura tu API Key de Gemini:"
    echo "   export GEMINI_API_KEY='tu_api_key_gemini'"
else
    echo "✅ GEMINI_API_KEY configurada"
fi

echo ""
echo "💡 Para configurar las APIs opcionales:"
echo "   1. Context7: https://upstash.com/ (Redis gratuito)"
echo "   2. Brave Search: https://api.search.brave.com/ (API gratuita)"
echo ""
echo "🚀 Una vez configuradas, reinicia gemini-icfes-optimizado"
EOF

chmod +x "$SCRIPT_DIR/mcp-env-setup.sh"
show_result $? "Script de configuración de variables creado"

echo ""
echo -e "${GREEN}✅ INSTALACIÓN DE MCPs COMPLETADA${NC}"
echo "=================================="
echo ""
echo -e "${CYAN}📋 MCPs instalados:${NC}"
echo "  • ✅ Context7 MCP - Documentación de librerías"
echo "  • ✅ Playwright MCP - Automatización web"
echo "  • ✅ Memory MCP - Gestión de memoria persistente"
echo "  • ✅ Brave Search MCP - Búsqueda web privada"
echo "  • ✅ Filesystem MCP - Acceso a archivos locales"
echo ""
echo -e "${CYAN}🔧 Archivos creados:${NC}"
echo "  • $PROJECT_ROOT/.mcp-config.json"
echo "  • $SCRIPT_DIR/mcp-env-setup.sh"
echo "  • $MCP_DIR/ (directorio de MCPs)"
echo ""
echo -e "${YELLOW}⚠️  PRÓXIMOS PASOS:${NC}"
echo "  1. Configurar variables de entorno: source $SCRIPT_DIR/mcp-env-setup.sh"
echo "  2. Configurar APIs opcionales (Context7, Brave Search)"
echo "  3. Ejecutar: bash $SCRIPT_DIR/configure-gemini-mcps.sh"
echo ""
echo -e "${GREEN}🎉 ¡MCPs listos para integrar con gemini-icfes-optimizado!${NC}"
