#!/bin/bash

# Script de inicio rápido para Gemini CLI con configuración ICFES
# Compatible con tutorial gemini-cli-r-exams.md
# Versión: 2.0 - Optimizado para Cuenta Pro

# Obtener la ruta real del script (resolviendo enlaces simbólicos)
SCRIPT_PATH="$(readlink -f "${BASH_SOURCE[0]}")"
SCRIPT_DIR="$(dirname "$SCRIPT_PATH")"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../../../.." && pwd)"

echo "🤖 GEMINI CLI PARA PROYECTO ICFES R-EXAMS - TUTORIAL v2.0"
echo "========================================================="
echo ""

# Verificar que estamos en el directorio correcto
if [[ ! -f "$PROJECT_DIR/Auxiliares/Instalaciones/Ais/Gemini_CLI/gemini-cli-r-exams.md" ]]; then
    echo "❌ Error: No se encuentra el tutorial gemini-cli-r-exams.md"
    echo "   Verifica que estás en el directorio correcto del proyecto"
    exit 1
fi

echo "📁 Directorio del proyecto: $PROJECT_DIR"
echo "📋 Configuración cargada:"
echo "  • Reglas: rules-gemini.md"
echo "  • Tareas: task-list-gemini.md" 
echo "  • Contexto: GEMINI.md"
echo "  • Tutorial: gemini-cli-r-exams.md"
echo ""

# Verificar API Key
if [[ -z "$GEMINI_API_KEY" ]]; then
    echo "❌ Error: GEMINI_API_KEY no configurada"
    echo "   Configura tu API Key primero:"
    echo "   export GEMINI_API_KEY=\"tu_api_key_aqui\""
    exit 1
fi

echo "✅ API Key configurada"
echo ""

echo "🚀 Comandos rápidos disponibles:"
echo "  • Análisis: 'Analiza este ejercicio: @path/to/file.Rmd'"
echo "  • Creación: 'Crea ejercicio desde esta imagen'"
echo "  • Optimización: 'Optimiza este código manteniendo calidad'"
echo "  • TikZ: 'Genera código TikZ con fidelidad 98%'"
echo ""
echo "💡 Para cargar contexto completo, usa:"
echo "   '@Auxiliares/Instalaciones/Ais/Gemini_CLI/GEMINI.md'"
echo ""
echo "📖 Tutorial completo disponible en:"
echo "   '@Auxiliares/Instalaciones/Ais/Gemini_CLI/gemini-cli-r-exams.md'"
echo ""

# Cambiar al directorio del proyecto
cd "$PROJECT_DIR"

# Verificar que no estamos en root
if [[ "$PWD" == "/" ]]; then
    echo "⚠️  Error: No se puede ejecutar desde directorio raíz"
    exit 1
fi

echo "📍 Directorio actual: $(pwd)"
echo "🚀 Iniciando Gemini CLI con configuración Pro..."
echo ""

# Configurar variables de entorno según tutorial
export GEMINI_PROJECT_ROOT="$PROJECT_DIR"

# Iniciar Gemini CLI con modelo forzado a 2.5 Pro según tutorial
if command -v gemini &> /dev/null; then
    echo "🤖 Usando Gemini 2.5 Pro (Cuenta Pro)..."
    gemini --model gemini-2.5-pro
else
    echo "❌ Error: Gemini CLI no encontrado"
    echo "   Instala primero siguiendo el tutorial:"
    echo "   bash Auxiliares/Instalaciones/Ais/Gemini_CLI/install-gemini-cli.sh"
    exit 1
fi
