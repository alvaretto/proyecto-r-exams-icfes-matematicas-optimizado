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
