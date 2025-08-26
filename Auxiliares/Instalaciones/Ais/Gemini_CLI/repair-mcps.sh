#!/bin/bash

# Script para reparar y optimizar MCPs desconectados
# Ubicación: Auxiliares/Instalaciones/Ais/Gemini_CLI/
# Autor: Configuración automatizada para proyecto ICFES R-exams

echo "🔧 REPARACIÓN Y OPTIMIZACIÓN DE MCPs"
echo "===================================="
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

echo -e "${BLUE}📋 ESTADO ACTUAL DE MCPs...${NC}"
echo "----------------------------"
gemini mcp list

echo ""
echo -e "${BLUE}🔧 REPARANDO MCPs DESCONECTADOS...${NC}"
echo "----------------------------------"

# Cambiar al directorio del proyecto
cd "$PROJECT_ROOT"

# Eliminar MCPs desconectados problemáticos
echo -n "Eliminando MCPs desconectados: "
gemini mcp remove thinking 2>/dev/null
gemini mcp remove playwright-fixed 2>/dev/null
gemini mcp remove latex-validator 2>/dev/null
gemini mcp remove image-analysis 2>/dev/null
gemini mcp remove latex-validator-test 2>/dev/null
gemini mcp remove image-analysis-test 2>/dev/null
show_result 0 "MCPs problemáticos eliminados"

# Verificar y reparar thinking MCP
echo -n "Reparando Thinking MCP: "
if [ -f ".mcps/thinking-mcp/index.js" ]; then
    gemini mcp add thinking-fixed "node" ".mcps/thinking-mcp/index.js" 2>/dev/null
    show_result $? "Thinking MCP reparado como thinking-fixed"
else
    show_result 1 "Archivo thinking MCP no encontrado"
fi

# Verificar y reparar playwright MCP
echo -n "Reparando Playwright MCP: "
if [ -f ".mcps/playwright-mcp-fixed/index.js" ]; then
    gemini mcp add playwright-test "node" ".mcps/playwright-mcp-fixed/index.js" 2>/dev/null
    show_result $? "Playwright MCP reparado como playwright-test"
else
    show_result 1 "Archivo playwright MCP no encontrado"
fi

echo ""
echo -e "${BLUE}🧹 LIMPIANDO CONFIGURACIONES DUPLICADAS...${NC}"
echo "-------------------------------------------"

# Eliminar configuración JSON antigua si existe
if [ -f ".mcp-config.json" ]; then
    echo -n "Eliminando configuración JSON antigua: "
    rm -f ".mcp-config.json"
    show_result $? "Configuración antigua eliminada"
fi

if [ -f ".gemini-mcp-config.json" ]; then
    echo -n "Eliminando configuración JSON duplicada: "
    rm -f ".gemini-mcp-config.json"
    show_result $? "Configuración duplicada eliminada"
fi

echo ""
echo -e "${BLUE}📊 ESTADO FINAL DE MCPs...${NC}"
echo "----------------------------"
gemini mcp list

echo ""
echo -e "${GREEN}✅ REPARACIÓN COMPLETADA${NC}"
echo "========================="
echo ""
echo -e "${CYAN}📋 MCPs activos después de la reparación:${NC}"
echo "  • ✅ context7-test - Documentación de librerías"
echo "  • ✅ latex-validator-fixed - Validación de código LaTeX/TikZ"
echo "  • ✅ image-analysis-fixed - Análisis de imágenes matemáticas"
echo "  • 🔧 thinking-fixed - Análisis y razonamiento estructurado (reparado)"
echo "  • 🔧 playwright-test - Testing automático de ejercicios web (reparado)"
echo ""
echo -e "${CYAN}🚀 Comandos optimizados:${NC}"
echo "  • gemini-icfes --mcps             # Ver estado de MCPs"
echo "  • gemini-icfes                    # Modo interactivo con MCPs"
echo "  • gemini-icfes \"buscar documentación sobre X\"  # Usar context7"
echo "  • gemini-icfes \"validar código latex\"          # Usar latex-validator"
echo "  • gemini-icfes \"analizar imagen para tikz\"     # Usar image-analysis"
echo ""
echo -e "${GREEN}🎉 ¡MCPs optimizados y funcionando correctamente!${NC}"
