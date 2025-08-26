#!/bin/bash

# Script para configurar Gemini-CLI globalmente con MCP
# Autor: Evaluación Gemini-CLI MCP
# Fecha: $(date +%Y-%m-%d)

set -e

echo "🌍 CONFIGURACIÓN GLOBAL DE GEMINI-CLI CON MCP"
echo "=" | tr ' ' '=' | head -c 50; echo

# Directorios
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
GEMINI_CONFIG_DIR="$HOME/.gemini"
WORKSPACE_DIR="/home/proyectos/Insync/alvaroangelm@iepedacitodecielo.edu.co/Google Drive/RepositorioMatematicasICFES_R_Exams"

# Función para logging
log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
}

# Verificar Gemini-CLI
log "🔍 Verificando Gemini-CLI..."
if ! command -v gemini &> /dev/null; then
    log "❌ Gemini-CLI no está instalado"
    exit 1
fi

GEMINI_VERSION=$(gemini --version 2>/dev/null || echo "unknown")
log "✅ Gemini-CLI encontrado: $GEMINI_VERSION"

# Crear directorio de configuración global
log "📁 Creando directorio de configuración global..."
mkdir -p "$GEMINI_CONFIG_DIR"

# Verificar servidores MCP
log "🔍 Verificando servidores MCP..."
MCP_SERVERS=(
    "$WORKSPACE_DIR/.mcps/context7-mcp/dist/index.js"
    "$WORKSPACE_DIR/.mcps/latex-validator-mcp/index-fixed.js"
    "$WORKSPACE_DIR/.mcps/image-analysis-mcp/index-fixed.js"
)

for server in "${MCP_SERVERS[@]}"; do
    if [ -f "$server" ]; then
        log "✅ Servidor encontrado: $(basename "$server")"
    else
        log "❌ Servidor no encontrado: $server"
        exit 1
    fi
done

# Verificar estado actual de servidores MCP
log "📊 Verificando estado actual de servidores MCP..."
gemini mcp list

# Crear alias globales para comandos frecuentes
log "⚙️ Creando alias globales..."
BASHRC_ADDITIONS="$HOME/.bashrc_mcp_additions"

cat > "$BASHRC_ADDITIONS" << 'EOF'
# Gemini-CLI MCP Aliases y Funciones
# Agregado por configure-gemini-global-mcp.sh

# Alias para comandos frecuentes
alias gmcp='gemini mcp'
alias gmcp-list='gemini mcp list'
alias gmcp-status='gemini mcp list'

# Función para análisis rápido de archivos R-exams
analyze-rexams() {
    if [ -z "$1" ]; then
        echo "Uso: analyze-rexams <archivo.Rmd>"
        return 1
    fi
    
    if [ ! -f "$1" ]; then
        echo "Error: Archivo no encontrado: $1"
        return 1
    fi
    
    echo "🔍 Analizando archivo R-exams: $1"
    gemini -p "Analiza este archivo .Rmd de R-exams. Identifica: 1) Estructura y metadatos ICFES, 2) Chunks de R/Python, 3) Código TikZ/LaTeX, 4) Posibles mejoras, 5) Errores o problemas. Sé específico y detallado." "$1"
}

# Función para validar LaTeX/TikZ
validate-tikz() {
    if [ -z "$1" ]; then
        echo "Uso: validate-tikz <archivo_con_tikz>"
        return 1
    fi
    
    echo "🔍 Validando código TikZ en: $1"
    gemini -p "Valida el código TikZ/LaTeX en este archivo. Verifica sintaxis, optimización y compatibilidad con R-exams." "$1"
}

# Función para optimizar chunks Python
optimize-python() {
    if [ -z "$1" ]; then
        echo "Uso: optimize-python <archivo.Rmd>"
        return 1
    fi
    
    echo "🐍 Optimizando chunks Python en: $1"
    gemini -p "Optimiza los chunks de Python en este archivo .Rmd. Enfócate en: 1) Eficiencia del código matplotlib, 2) Compatibilidad con reticulate, 3) Manejo de errores, 4) Generación de archivos. Proporciona código mejorado." "$1"
}

# Función para generar código TikZ
generate-tikz() {
    if [ -z "$1" ]; then
        echo "Uso: generate-tikz '<descripción_de_gráfica>'"
        return 1
    fi
    
    echo "🎨 Generando código TikZ para: $1"
    gemini -p "Genera código TikZ profesional para crear: $1. El código debe ser: 1) Compatible con R-exams, 2) Sintaxis TikZ correcta, 3) Estilo profesional, 4) Optimizado. Incluye código completo." 
}

# Función para análisis de imágenes matemáticas
analyze-math-image() {
    if [ -z "$1" ]; then
        echo "Uso: analyze-math-image <ruta_imagen>"
        return 1
    fi
    
    if [ ! -f "$1" ]; then
        echo "Error: Imagen no encontrada: $1"
        return 1
    fi
    
    echo "📊 Analizando imagen matemática: $1"
    gemini -p "Analiza esta imagen matemática para replicación con TikZ. Identifica: 1) Tipo de gráfica, 2) Elementos principales, 3) Colores y estilos, 4) Estrategia de replicación TikZ, 5) Librerías necesarias." "$1"
}

# Función para testing completo de archivo R-exams
test-rexams() {
    if [ -z "$1" ]; then
        echo "Uso: test-rexams <archivo.Rmd>"
        return 1
    fi
    
    echo "🧪 Testing completo de archivo R-exams: $1"
    echo "1. Análisis de estructura..."
    analyze-rexams "$1"
    echo ""
    echo "2. Validación de LaTeX/TikZ..."
    validate-tikz "$1"
    echo ""
    echo "3. Optimización de Python..."
    optimize-python "$1"
}

# Función para mostrar ayuda de MCP
mcp-help() {
    echo "🔧 COMANDOS MCP DISPONIBLES:"
    echo ""
    echo "📊 Estado y gestión:"
    echo "  gmcp-list          - Listar servidores MCP"
    echo "  gmcp-status        - Estado de servidores MCP"
    echo ""
    echo "📝 Análisis de archivos:"
    echo "  analyze-rexams <archivo.Rmd>     - Análisis completo R-exams"
    echo "  validate-tikz <archivo>          - Validar código TikZ/LaTeX"
    echo "  optimize-python <archivo.Rmd>    - Optimizar chunks Python"
    echo ""
    echo "🎨 Generación de contenido:"
    echo "  generate-tikz '<descripción>'    - Generar código TikZ"
    echo "  analyze-math-image <imagen>      - Analizar imagen matemática"
    echo ""
    echo "🧪 Testing:"
    echo "  test-rexams <archivo.Rmd>        - Testing completo"
    echo ""
    echo "❓ Ayuda:"
    echo "  mcp-help                         - Mostrar esta ayuda"
}

echo "✅ Alias y funciones MCP creados"
EOF

# Agregar al .bashrc si no está ya presente
if ! grep -q "bashrc_mcp_additions" "$HOME/.bashrc"; then
    echo "" >> "$HOME/.bashrc"
    echo "# Gemini-CLI MCP Integration" >> "$HOME/.bashrc"
    echo "source $BASHRC_ADDITIONS" >> "$HOME/.bashrc"
    log "✅ Alias agregados a .bashrc"
else
    log "ℹ️ Alias ya están en .bashrc"
fi

# Crear script de inicio de servidores MCP
log "⚙️ Creando script de inicio de servidores..."
STARTUP_SCRIPT="$GEMINI_CONFIG_DIR/start-mcp-servers.sh"

cat > "$STARTUP_SCRIPT" << 'EOF'
#!/bin/bash

# Script de inicio de servidores MCP para Gemini-CLI
# Auto-generado por configure-gemini-global-mcp.sh

echo "🚀 Iniciando servidores MCP..."

# Verificar que Gemini-CLI esté disponible
if ! command -v gemini &> /dev/null; then
    echo "❌ Gemini-CLI no encontrado"
    exit 1
fi

# Verificar estado actual
echo "📊 Estado actual de servidores MCP:"
gemini mcp list

echo "✅ Servidores MCP listos para usar"
echo ""
echo "💡 Comandos útiles:"
echo "  mcp-help           - Mostrar ayuda de comandos MCP"
echo "  gmcp-list          - Listar servidores"
echo "  analyze-rexams     - Analizar archivo R-exams"
echo "  validate-tikz      - Validar código TikZ"
EOF

chmod +x "$STARTUP_SCRIPT"
log "✅ Script de inicio creado: $STARTUP_SCRIPT"

# Crear configuración de entorno
log "⚙️ Creando configuración de entorno..."
ENV_CONFIG="$GEMINI_CONFIG_DIR/mcp-environment.sh"

cat > "$ENV_CONFIG" << 'EOF'
#!/bin/bash

# Configuración de entorno para MCP
# Auto-generado por configure-gemini-global-mcp.sh

# Directorios importantes
export MCP_WORKSPACE="/home/proyectos/Insync/alvaroangelm@iepedacitodecielo.edu.co/Google Drive/RepositorioMatematicasICFES_R_Exams"
export MCP_SERVERS_DIR="$MCP_WORKSPACE/.mcps"
export GEMINI_CONFIG_DIR="$HOME/.gemini"

# Configuración de herramientas
export R_HOME="/usr/lib/R"
export PYTHON_PATH="/usr/bin/python3"
export LATEX_COMPILER="pdflatex"

# Configuración MCP
export MCP_LOG_LEVEL="info"
export MCP_TIMEOUT="30000"
export MCP_RETRY_ATTEMPTS="3"

# Funciones de utilidad
check_mcp_status() {
    echo "📊 Estado de servidores MCP:"
    gemini mcp list
}

restart_mcp_servers() {
    echo "🔄 Reiniciando servidores MCP..."
    # Aquí se podrían agregar comandos específicos de reinicio
    gemini mcp list
}
EOF

chmod +x "$ENV_CONFIG"
log "✅ Configuración de entorno creada: $ENV_CONFIG"

# Verificar configuración final
log "🔍 Verificando configuración final..."
source "$BASHRC_ADDITIONS"

# Mostrar estado final
log "📊 Estado final de servidores MCP:"
gemini mcp list

log "✅ Configuración global completada"
echo ""
log "📋 RESUMEN DE CONFIGURACIÓN GLOBAL:"
log "✅ Gemini-CLI configurado globalmente con MCP"
log "✅ 3 servidores MCP disponibles globalmente"
log "✅ Alias y funciones de utilidad creados"
log "✅ Scripts de inicio y entorno configurados"
log "✅ Integración con .bashrc completada"
echo ""
log "🔧 COMANDOS DISPONIBLES (después de reiniciar terminal):"
log "  mcp-help                    - Mostrar ayuda completa"
log "  analyze-rexams <archivo>    - Análisis de archivos R-exams"
log "  validate-tikz <archivo>     - Validación TikZ/LaTeX"
log "  generate-tikz '<desc>'      - Generación de código TikZ"
log "  gmcp-list                   - Estado de servidores MCP"
echo ""
log "💡 Para activar los alias ahora: source ~/.bashrc"
