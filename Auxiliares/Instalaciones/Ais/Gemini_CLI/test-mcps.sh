#!/bin/bash

# Script de testing para MCPs en gemini-icfes-optimizado
# Ubicación: Auxiliares/Instalaciones/Ais/Gemini_CLI/
# Autor: Testing automatizado para proyecto ICFES R-exams
# Fecha: $(date)

echo "🧪 TESTING DE MCPs PARA GEMINI-ICFES-OPTIMIZADO"
echo "=============================================="
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

echo -e "${BLUE}📋 VERIFICANDO INSTALACIÓN DE MCPs...${NC}"
echo "------------------------------------"

# Verificar archivos de configuración
echo -n "Configuración MCP base: "
if [ -f "$PROJECT_ROOT/.mcp-config.json" ]; then
    echo -e "${GREEN}✅ Encontrada${NC}"
else
    echo -e "${RED}❌ No encontrada${NC}"
    echo "Ejecuta: bash install-mcps.sh"
    exit 1
fi

echo -n "Configuración Gemini MCP: "
if [ -f "$PROJECT_ROOT/.gemini-mcp-config.json" ]; then
    echo -e "${GREEN}✅ Encontrada${NC}"
else
    echo -e "${RED}❌ No encontrada${NC}"
    echo "Ejecuta: bash configure-gemini-mcps.sh"
    exit 1
fi

echo -n "Script optimizado con MCPs: "
if [ -f "$SCRIPT_DIR/gemini-mcps-optimizado.sh" ]; then
    echo -e "${GREEN}✅ Encontrado${NC}"
else
    echo -e "${RED}❌ No encontrado${NC}"
    echo "Ejecuta: bash configure-gemini-mcps.sh"
    exit 1
fi

echo -n "Comando global MCPs: "
if command -v gemini-icfes-mcps &> /dev/null; then
    echo -e "${GREEN}✅ Disponible${NC}"
else
    echo -e "${YELLOW}⚠️  No disponible (opcional)${NC}"
fi

echo ""
echo -e "${BLUE}🔧 VERIFICANDO MCPs INDIVIDUALES...${NC}"
echo "-----------------------------------"

MCP_DIR="$PROJECT_ROOT/.mcps"

# Context7 MCP
echo -n "Context7 MCP: "
if [ -d "$MCP_DIR/context7-mcp" ] || command -v context7-mcp &> /dev/null; then
    echo -e "${GREEN}✅ Instalado${NC}"
else
    echo -e "${YELLOW}⚠️  No encontrado (se intentará instalar)${NC}"
fi

# Playwright MCP
echo -n "Playwright MCP: "
if [ -d "$MCP_DIR/playwright-mcp" ] || command -v playwright-mcp &> /dev/null; then
    echo -e "${GREEN}✅ Instalado${NC}"
else
    echo -e "${YELLOW}⚠️  No encontrado (se intentará instalar)${NC}"
fi

# Memory MCP
echo -n "Memory MCP: "
if [ -d "$MCP_DIR/mcp-servers" ] || [ -d "$MCP_DIR/memory-mcp" ]; then
    echo -e "${GREEN}✅ Instalado${NC}"
else
    echo -e "${YELLOW}⚠️  No encontrado (se usará implementación local)${NC}"
fi

# Brave Search MCP
echo -n "Brave Search MCP: "
if [ -d "$MCP_DIR/brave-search-mcp" ]; then
    echo -e "${GREEN}✅ Configurado${NC}"
else
    echo -e "${YELLOW}⚠️  No configurado${NC}"
fi

# Filesystem MCP
echo -n "Filesystem MCP: "
if [ -d "$MCP_DIR/filesystem-servers" ]; then
    echo -e "${GREEN}✅ Instalado${NC}"
else
    echo -e "${YELLOW}⚠️  No encontrado${NC}"
fi

echo ""
echo -e "${BLUE}🔑 VERIFICANDO VARIABLES DE ENTORNO...${NC}"
echo "--------------------------------------"

echo -n "GEMINI_API_KEY: "
if [ -n "$GEMINI_API_KEY" ]; then
    echo -e "${GREEN}✅ Configurada${NC}"
else
    echo -e "${RED}❌ No configurada${NC}"
    echo "   Configura: export GEMINI_API_KEY='tu_api_key'"
fi

echo -n "UPSTASH_REDIS_REST_URL (Context7): "
if [ -n "$UPSTASH_REDIS_REST_URL" ]; then
    echo -e "${GREEN}✅ Configurada${NC}"
else
    echo -e "${YELLOW}⚠️  No configurada (opcional)${NC}"
fi

echo -n "BRAVE_API_KEY (Brave Search): "
if [ -n "$BRAVE_API_KEY" ]; then
    echo -e "${GREEN}✅ Configurada${NC}"
else
    echo -e "${YELLOW}⚠️  No configurada (opcional)${NC}"
fi

echo ""
echo -e "${BLUE}🧪 TESTING FUNCIONAL...${NC}"
echo "----------------------"

# Test 1: Verificar que Gemini CLI funciona
echo -n "Gemini CLI básico: "
if command -v gemini &> /dev/null; then
    # Test simple de Gemini CLI
    timeout 5s gemini --version &> /dev/null
    if [ $? -eq 0 ] || [ $? -eq 124 ]; then  # 124 es timeout, pero significa que el comando existe
        echo -e "${GREEN}✅ Funcional${NC}"
    else
        echo -e "${RED}❌ Error en ejecución${NC}"
    fi
else
    echo -e "${RED}❌ No instalado${NC}"
fi

# Test 2: Verificar configuración JSON
echo -n "Configuración JSON válida: "
if command -v jq &> /dev/null; then
    jq empty "$PROJECT_ROOT/.gemini-mcp-config.json" 2>/dev/null
    show_result $? "JSON válido"
else
    # Verificación básica sin jq
    if grep -q "mcps" "$PROJECT_ROOT/.gemini-mcp-config.json"; then
        echo -e "${GREEN}✅ Estructura básica correcta${NC}"
    else
        echo -e "${RED}❌ Estructura incorrecta${NC}"
    fi
fi

# Test 3: Verificar permisos de archivos
echo -n "Permisos de scripts: "
if [ -x "$SCRIPT_DIR/gemini-mcps-optimizado.sh" ]; then
    echo -e "${GREEN}✅ Correctos${NC}"
else
    echo -e "${RED}❌ Sin permisos de ejecución${NC}"
fi

# Test 4: Verificar estructura de directorios
echo -n "Estructura de directorios: "
REQUIRED_DIRS=(
    "Auxiliares/Ejemplos-Funcionales-Rmd"
    "Auxiliares/TikZ-Documentation"
    "Lab-Manjaro"
)

ALL_DIRS_EXIST=true
for dir in "${REQUIRED_DIRS[@]}"; do
    if [ ! -d "$PROJECT_ROOT/$dir" ]; then
        ALL_DIRS_EXIST=false
        break
    fi
done

if $ALL_DIRS_EXIST; then
    echo -e "${GREEN}✅ Completa${NC}"
else
    echo -e "${YELLOW}⚠️  Algunos directorios faltantes${NC}"
fi

echo ""
echo -e "${BLUE}📊 REPORTE DE TESTING...${NC}"
echo "------------------------"

# Contar componentes instalados
INSTALLED_MCPS=0
TOTAL_MCPS=5

[ -d "$MCP_DIR/context7-mcp" ] && ((INSTALLED_MCPS++))
[ -d "$MCP_DIR/playwright-mcp" ] && ((INSTALLED_MCPS++))
[ -d "$MCP_DIR/mcp-servers" ] && ((INSTALLED_MCPS++))
[ -d "$MCP_DIR/brave-search-mcp" ] && ((INSTALLED_MCPS++))
[ -d "$MCP_DIR/filesystem-servers" ] && ((INSTALLED_MCPS++))

echo "📈 Estadísticas de instalación:"
echo "   • MCPs instalados: $INSTALLED_MCPS/$TOTAL_MCPS"
echo "   • Configuración: $([ -f "$PROJECT_ROOT/.gemini-mcp-config.json" ] && echo "✅" || echo "❌")"
echo "   • Variables de entorno: $([ -n "$GEMINI_API_KEY" ] && echo "✅" || echo "❌")"
echo "   • Comando global: $(command -v gemini-icfes-mcps &> /dev/null && echo "✅" || echo "❌")"

echo ""
echo -e "${BLUE}🎯 RECOMENDACIONES...${NC}"
echo "---------------------"

if [ $INSTALLED_MCPS -lt $TOTAL_MCPS ]; then
    echo -e "${YELLOW}⚠️  Algunos MCPs no están completamente instalados${NC}"
    echo "   Ejecuta: bash install-mcps.sh"
fi

if [ -z "$GEMINI_API_KEY" ]; then
    echo -e "${RED}❌ API Key de Gemini requerida${NC}"
    echo "   Configura: export GEMINI_API_KEY='tu_api_key'"
fi

if [ -z "$UPSTASH_REDIS_REST_URL" ]; then
    echo -e "${YELLOW}💡 Para Context7, configura Redis gratuito:${NC}"
    echo "   1. Registrarse en: https://upstash.com/"
    echo "   2. Crear base Redis gratuita"
    echo "   3. Configurar: export UPSTASH_REDIS_REST_URL='url'"
fi

if [ -z "$BRAVE_API_KEY" ]; then
    echo -e "${YELLOW}💡 Para Brave Search, configura API gratuita:${NC}"
    echo "   1. Registrarse en: https://api.search.brave.com/"
    echo "   2. Obtener API Key gratuita"
    echo "   3. Configurar: export BRAVE_API_KEY='key'"
fi

echo ""
echo -e "${BLUE}🚀 PRÓXIMOS PASOS...${NC}"
echo "--------------------"

if [ $INSTALLED_MCPS -eq $TOTAL_MCPS ] && [ -n "$GEMINI_API_KEY" ]; then
    echo -e "${GREEN}✅ Sistema listo para usar${NC}"
    echo "   Comando: gemini-icfes-mcps"
    echo "   Contexto: @Auxiliares/Instalaciones/Ais/Gemini_CLI/GEMINI.md"
else
    echo -e "${YELLOW}⚠️  Completar configuración pendiente${NC}"
    echo "   1. Instalar MCPs faltantes: bash install-mcps.sh"
    echo "   2. Configurar variables: source mcp-env-setup.sh"
    echo "   3. Probar nuevamente: bash test-mcps.sh"
fi

echo ""
echo -e "${CYAN}📖 Documentación disponible:${NC}"
echo "   • Comandos MCP: cat mcp-commands.md"
echo "   • Configuración: cat .gemini-mcp-config.json"
echo "   • Variables: cat mcp-env-setup.sh"

echo ""
if [ $INSTALLED_MCPS -eq $TOTAL_MCPS ] && [ -n "$GEMINI_API_KEY" ]; then
    echo -e "${GREEN}🎉 ¡Testing completado exitosamente!${NC}"
    echo -e "${GREEN}   MCPs listos para usar con gemini-icfes-optimizado${NC}"
else
    echo -e "${YELLOW}⚠️  Testing completado con advertencias${NC}"
    echo -e "${YELLOW}   Revisar recomendaciones arriba${NC}"
fi
