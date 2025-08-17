#!/bin/bash

# Script de inicio optimizado para Gemini CLI
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../../../.." && pwd)"

echo "🤖 GEMINI CLI OPTIMIZADO - PROYECTO ICFES R-EXAMS"
echo "================================================="
echo ""

# Verificar directorio del proyecto
if [[ ! -f "$PROJECT_DIR/.geminiignore" ]]; then
    echo "❌ Error: .geminiignore no encontrado. Ejecuta optimizar-contexto-gemini.sh primero"
    exit 1
fi

# Mostrar información del contexto
echo "📁 Directorio del proyecto: $PROJECT_DIR"
echo "🎯 Contexto optimizado con .geminiignore"
echo "📋 Archivos de configuración cargados:"
echo "   • rules-gemini.md"
echo "   • task-list-gemini.md"
echo "   • GEMINI.md"
echo ""

# Cambiar al directorio del proyecto
cd "$PROJECT_DIR"

# Verificar que no estamos en root
if [[ "$PWD" == "/" ]]; then
    echo "⚠️  Error: No se puede ejecutar desde directorio raíz"
    exit 1
fi

echo "📍 Directorio actual: $(pwd)"
echo "🚀 Iniciando Gemini CLI con contexto optimizado..."
echo ""
echo "💡 Para cargar contexto completo, usa:"
echo "   @Auxiliares/Instalaciones/Ais/Gemini_CLI/GEMINI.md"
echo ""

# Iniciar Gemini CLI
gemini
