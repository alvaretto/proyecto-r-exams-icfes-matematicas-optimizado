#!/bin/bash

# Aliases para comandos Gemini CLI con MCPs
# Uso: source gemini-aliases.sh

# Obtener directorio del script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Alias principales
alias gemini-icfes="bash $SCRIPT_DIR/gemini-icfes-mcps.sh"
alias gemini-thinking="bash $SCRIPT_DIR/gemini-icfes-mcps.sh 'usar thinking para analizar'"
alias gemini-validate="bash $SCRIPT_DIR/gemini-icfes-mcps.sh 'validar código latex'"
alias gemini-test="bash $SCRIPT_DIR/gemini-icfes-mcps.sh 'testing automático'"
alias gemini-image="bash $SCRIPT_DIR/gemini-icfes-mcps.sh 'analizar imagen'"

echo "✅ Aliases de Gemini CLI configurados:"
echo "   • gemini-icfes - Modo interactivo con MCPs"
echo "   • gemini-thinking - Análisis estructurado"
echo "   • gemini-validate - Validación LaTeX/TikZ"
echo "   • gemini-test - Testing automático"
echo "   • gemini-image - Análisis de imágenes"
