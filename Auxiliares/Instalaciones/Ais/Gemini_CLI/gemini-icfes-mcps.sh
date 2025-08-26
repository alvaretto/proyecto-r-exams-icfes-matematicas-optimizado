#!/bin/bash

# Script optimizado para usar Gemini CLI con MCPs en proyecto ICFES
# Uso: bash gemini-icfes-mcps.sh [prompt]

# Obtener directorio del script y proyecto
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../../.." && pwd)"

# Verificar que estamos en el directorio correcto del proyecto
if [ ! -d "$PROJECT_ROOT" ]; then
    echo "❌ Directorio del proyecto no encontrado: $PROJECT_ROOT"
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
    echo "📋 MCPs activos: context7-test, latex-validator-fixed, image-analysis-fixed"
    echo "💡 Usa comandos como: 'analizar ejercicio', 'validar latex', 'buscar documentación'"
    echo ""
    # Iniciar Gemini CLI (los MCPs se cargan automáticamente si están configurados)
    gemini
elif [ "$1" = "--mcps" ]; then
    # Mostrar información de MCPs disponibles
    echo "🔧 MCPs CONFIGURADOS PARA GEMINI-ICFES"
    echo "======================================"
    echo ""
    echo "📋 Estado actual de MCPs:"
    gemini mcp list
    echo ""
    echo "📋 MCPs principales disponibles:"
    echo "  • ✅ context7-test - Documentación de librerías (conectado)"
    echo "  • ✅ latex-validator-fixed - Validación de código LaTeX/TikZ (conectado)"
    echo "  • ✅ image-analysis-fixed - Análisis de imágenes matemáticas (conectado)"
    echo "  • ⚠️  thinking - Análisis y razonamiento estructurado (desconectado)"
    echo "  • ⚠️  playwright-fixed - Testing automático de ejercicios web (desconectado)"
    echo ""
    echo "💡 Ejemplos de uso:"
    echo "  • gemini-icfes \"usar thinking para analizar este problema\""
    echo "  • gemini-icfes \"validar código latex del archivo\""
    echo "  • gemini-icfes \"testing automático del ejercicio\""
    echo "  • gemini-icfes \"analizar imagen para replicación tikz\""
    echo ""
    echo "🚀 Para modo interactivo: gemini-icfes"
elif [ "$1" = "--help" ] || [ "$1" = "-h" ]; then
    # Mostrar ayuda
    echo "🔧 GEMINI-ICFES - Gemini CLI con MCPs para proyecto ICFES"
    echo "========================================================"
    echo ""
    echo "Uso:"
    echo "  gemini-icfes                    # Modo interactivo"
    echo "  gemini-icfes --mcps             # Mostrar MCPs disponibles"
    echo "  gemini-icfes \"tu pregunta\"      # Prompt directo"
    echo "  gemini-icfes --help             # Mostrar esta ayuda"
    echo ""
    echo "Comandos especializados:"
    echo "  gemini-thinking \"problema\"      # Análisis estructurado"
    echo "  gemini-validate archivo.tex     # Validar LaTeX"
    echo "  gemini-test ejercicio.html      # Testing automático"
    echo "  gemini-image imagen.png         # Análisis de imagen"
    echo ""
    echo "📋 MCPs integrados: thinking, playwright-fixed, latex-validator, image-analysis"
else
    # Modo prompt directo
    # Modo prompt directo (los MCPs se cargan automáticamente si están configurados)
    gemini -p "$*"
fi
