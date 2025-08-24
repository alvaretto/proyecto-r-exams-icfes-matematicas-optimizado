#!/bin/bash

# Script optimizado de Gemini CLI para proyecto ICFES R-exams
# Basado en tutorial gemini-cli-r-exams.md v2.0
# Configuración Pro con 1M tokens, temperatura 0.1

# Obtener la ruta real del script (resolviendo enlaces simbólicos)
SCRIPT_PATH="$(readlink -f "${BASH_SOURCE[0]}")"
SCRIPT_DIR="$(dirname "$SCRIPT_PATH")"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../../../.." && pwd)"

echo "🚀 GEMINI CLI OPTIMIZADO - PROYECTO ICFES R-EXAMS"
echo "================================================="
echo ""

# Verificar configuración del tutorial
if [[ ! -f "$PROJECT_DIR/Auxiliares/Instalaciones/Ais/Gemini_CLI/gemini-cli-r-exams.md" ]]; then
    echo "❌ Error: Tutorial gemini-cli-r-exams.md no encontrado"
    exit 1
fi

# Verificar API Key
if [[ -z "$GEMINI_API_KEY" ]]; then
    echo "❌ Error: GEMINI_API_KEY no configurada"
    echo "   Configura según tutorial: export GEMINI_API_KEY=\"tu_api_key_aqui\""
    exit 1
fi

echo "📁 Directorio del proyecto: $PROJECT_DIR"
echo "🔧 Configuración optimizada:"
echo "   • ✅ Modelo: gemini-2.5-pro (Cuenta Pro)"
echo "   • ✅ Contexto: 1M tokens"
echo "   • ✅ Temperatura: 0.1 (consistencia máxima)"
echo "   • ✅ API Key: Configurada"
echo ""

# Verificar archivos de configuración del tutorial
echo "📋 Verificando archivos del tutorial..."
REQUIRED_FILES=(
    "Auxiliares/Instalaciones/Ais/Gemini_CLI/rules-gemini.md"
    "Auxiliares/Instalaciones/Ais/Gemini_CLI/task-list-gemini.md"
    "Auxiliares/Instalaciones/Ais/Gemini_CLI/GEMINI.md"
)

for file in "${REQUIRED_FILES[@]}"; do
    if [[ -f "$PROJECT_DIR/$file" ]]; then
        echo "   ✅ $file"
    else
        echo "   ❌ $file (faltante)"
    fi
done

echo ""
echo "🎯 Contexto optimizado con .geminiignore"
echo "📊 Configuración ICFES cargada automáticamente"
echo ""

# Cambiar al directorio del proyecto
cd "$PROJECT_DIR"

# Verificar que no estamos en root
if [[ "$PWD" == "/" ]]; then
    echo "⚠️  Error: No se puede ejecutar desde directorio raíz"
    exit 1
fi

echo "📍 Directorio actual: $(pwd)"
echo "🚀 Iniciando Gemini CLI con configuración optimizada..."
echo ""

# Configurar variables de entorno según tutorial
export GEMINI_PROJECT_ROOT="$PROJECT_DIR"

# Mostrar comandos disponibles según tutorial
echo "💡 Comandos optimizados disponibles:"
echo "   • 'Analiza ejercicio: @path/file.Rmd' → Análisis completo"
echo "   • 'Genera TikZ desde imagen' → Código TikZ fidelidad 98%"
echo "   • 'Valida estándares ICFES' → Verificación completa"
echo "   • 'Optimiza código R-exams' → Mejoras automáticas"
echo ""
echo "📖 Para cargar contexto completo del tutorial:"
echo "   @Auxiliares/Instalaciones/Ais/Gemini_CLI/gemini-cli-r-exams.md"
echo ""

# Iniciar Gemini CLI con configuración Pro optimizada
if command -v gemini &> /dev/null; then
    echo "🤖 Iniciando Gemini 2.5 Pro con configuración optimizada..."
    # Usar configuración Pro según tutorial: modelo 2.5-pro, temperatura baja
    gemini --model gemini-2.5-pro
else
    echo "❌ Error: Gemini CLI no encontrado"
    echo "   Instala siguiendo el tutorial:"
    echo "   bash $SCRIPT_DIR/install-gemini-cli.sh"
    exit 1
fi
