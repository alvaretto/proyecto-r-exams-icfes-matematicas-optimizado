#!/bin/bash

# Wrapper global para gemini-icfes con MCPs
# Instalado automáticamente

# Directorio del proyecto (ruta absoluta con comillas para manejar espacios)
PROJECT_ROOT="/home/proyectos/Insync/alvaroangelm@iepedacitodecielo.edu.co/Google Drive/RepositorioMatematicasICFES_R_Exams"
SCRIPT_DIR="/home/proyectos/Insync/alvaroangelm@iepedacitodecielo.edu.co/Google Drive/RepositorioMatematicasICFES_R_Exams/Auxiliares/Instalaciones/Ais/Gemini_CLI"

# Verificar que el proyecto existe
if [ ! -d "$PROJECT_ROOT" ]; then
    echo "❌ Error: Directorio del proyecto no encontrado: $PROJECT_ROOT"
    echo "   El proyecto puede haber sido movido o eliminado"
    exit 1
fi

# Verificar script principal
if [ ! -f "$SCRIPT_DIR/gemini-icfes-mcps.sh" ]; then
    echo "❌ Error: Script principal no encontrado: $SCRIPT_DIR/gemini-icfes-mcps.sh"
    echo "   Ejecuta: bash \"$SCRIPT_DIR/configure-gemini-mcps.sh\""
    exit 1
fi

# Cambiar al directorio del proyecto (con comillas para espacios)
cd "$PROJECT_ROOT"

# Configurar variables de entorno si existen
if [ -f "$SCRIPT_DIR/mcp-env-setup.sh" ]; then
    source "$SCRIPT_DIR/mcp-env-setup.sh" 2>/dev/null
fi

# Ejecutar script principal con todos los argumentos (con comillas para espacios)
exec bash "$SCRIPT_DIR/gemini-icfes-mcps.sh" "$@"
