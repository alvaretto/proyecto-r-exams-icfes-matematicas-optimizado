#!/bin/bash

# Script de inicio rápido para Gemini CLI con configuración ICFES
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "🤖 INICIANDO GEMINI CLI PARA PROYECTO ICFES R-EXAMS"
echo "=================================================="
echo ""
echo "📋 Configuración cargada:"
echo "  • Reglas: rules-gemini.md"
echo "  • Tareas: task-list-gemini.md" 
echo "  • Contexto: GEMINI.md"
echo ""
echo "🚀 Comandos rápidos disponibles:"
echo "  • Análisis: 'Analiza este ejercicio: @path/to/file.Rmd'"
echo "  • Creación: 'Crea ejercicio desde esta imagen'"
echo "  • Optimización: 'Optimiza este código manteniendo calidad'"
echo ""
echo "💡 Para cargar contexto completo, usa:"
echo "   '@Auxiliares/Instalaciones/Ais/Gemini_CLI/GEMINI.md'"
echo ""

# Obtener directorio del proyecto
PROJECT_DIR="$(cd "$SCRIPT_DIR/../../../.." && pwd)"

# Verificar que estamos en el directorio correcto
if [[ ! -f "$PROJECT_DIR/.gitignore" ]] || [[ ! -d "$PROJECT_DIR/Auxiliares" ]]; then
    echo "❌ Error: No se puede encontrar el directorio del proyecto ICFES R-exams"
    echo "   Directorio actual: $PROJECT_DIR"
    exit 1
fi

echo "📁 Directorio del proyecto: $PROJECT_DIR"
echo "🎯 Iniciando Gemini CLI en contexto del proyecto..."
echo ""

# Cambiar al directorio del proyecto
cd "$PROJECT_DIR"

# Verificar que no estamos en root
if [[ "$PWD" == "/" ]]; then
    echo "⚠️  ADVERTENCIA: Detectado directorio raíz. Cambiando a directorio seguro..."
    cd "$PROJECT_DIR"
fi

# Mostrar directorio actual para confirmación
echo "📍 Directorio actual: $(pwd)"
echo ""

# Iniciar Gemini CLI con configuración específica
gemini
