#!/bin/bash

# Script de inicio de Gemini CLI con MCPs optimizado
# Obtener la ruta real del script (resolviendo enlaces simbólicos)
SCRIPT_PATH="$(readlink -f "${BASH_SOURCE[0]}")"
SCRIPT_DIR="$(dirname "$SCRIPT_PATH")"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../../../.." && pwd)"

echo "🤖 GEMINI CLI + MCPs OPTIMIZADO - PROYECTO ICFES R-EXAMS"
echo "========================================================"
echo ""

# Verificar configuración
if [[ ! -f "$PROJECT_DIR/.gemini-mcp-config.json" ]]; then
    echo "❌ Error: Configuración MCP no encontrada"
    echo "   Ejecuta: bash configure-gemini-mcps.sh"
    exit 1
fi

# Cargar variables de entorno para MCPs
if [[ -f "$SCRIPT_DIR/mcp-env-setup.sh" ]]; then
    source "$SCRIPT_DIR/mcp-env-setup.sh"
fi

echo "📁 Directorio del proyecto: $PROJECT_DIR"
echo "🔧 MCPs habilitados:"
echo "   • ✅ Context7 - Documentación de librerías"
echo "   • ✅ Playwright - Automatización web"
echo "   • ✅ Memory - Gestión de memoria persistente"
echo "   • ✅ Brave Search - Búsqueda web privada"
echo "   • ✅ Filesystem - Acceso a archivos locales"
echo ""
echo "🎯 Contexto optimizado con .geminiignore"
echo "📋 Configuración ICFES cargada automáticamente"
echo ""

# Cambiar al directorio del proyecto
cd "$PROJECT_DIR"

# Verificar que no estamos en root
if [[ "$PWD" == "/" ]]; then
    echo "⚠️  Error: No se puede ejecutar desde directorio raíz"
    exit 1
fi

echo "📍 Directorio actual: $(pwd)"
echo "🚀 Iniciando Gemini CLI con MCPs integrados..."
echo ""
echo "💡 Comandos MCP disponibles:"
echo "   • 'buscar información sobre [tema]' → Brave Search"
echo "   • 'documentación de [librería]' → Context7"
echo "   • 'automatizar navegación web' → Playwright"
echo "   • 'recordar [información]' → Memory"
echo "   • 'leer archivo [path]' → Filesystem"
echo ""
echo "📖 Para cargar contexto completo:"
echo "   @Auxiliares/Instalaciones/Ais/Gemini_CLI/GEMINI.md"
echo ""

# Configurar variables de entorno para MCPs
export MCP_CONFIG_PATH="$PROJECT_DIR/.mcp-config.json"
export GEMINI_MCP_CONFIG="$PROJECT_DIR/.gemini-mcp-config.json"

# Iniciar Gemini CLI con modelo forzado a 2.5 Pro
if command -v gemini &> /dev/null; then
    echo "🤖 Forzando uso de Gemini 2.5 Pro..."
    gemini --model gemini-2.5-pro
else
    echo "❌ Error: Gemini CLI no encontrado"
    echo "   Instala primero: bash install-gemini-cli.sh"
    exit 1
fi
