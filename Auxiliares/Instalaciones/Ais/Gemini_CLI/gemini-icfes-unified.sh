#!/bin/bash

# Script Unificado Gemini CLI para Proyecto ICFES R-exams
# Compatible con tutorial gemini-cli-r-exams.md v2.0
# Arquitectura híbrida con modos progresivos
# Autor: Análisis de Configuraciones Gemini CLI - Agosto 2025

# Obtener la ruta real del script (resolviendo enlaces simbólicos)
SCRIPT_PATH="$(readlink -f "${BASH_SOURCE[0]}")"
SCRIPT_DIR="$(dirname "$SCRIPT_PATH")"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../../../.." && pwd)"

# Configuración de colores
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Función para mostrar ayuda
show_help() {
    echo -e "${BLUE}🤖 GEMINI CLI UNIFICADO - PROYECTO ICFES R-EXAMS${NC}"
    echo -e "${BLUE}=================================================${NC}"
    echo ""
    echo -e "${CYAN}📋 MODOS DISPONIBLES:${NC}"
    echo ""
    echo -e "${GREEN}  gemini-icfes${NC}                    # Modo básico (tutorial estándar)"
    echo -e "${GREEN}  gemini-icfes --basic${NC}            # Modo básico explícito"
    echo -e "${GREEN}  gemini-icfes --optimized${NC}        # Modo optimizado con verificaciones"
    echo -e "${GREEN}  gemini-icfes --mcps${NC}             # Modo completo con MCPs"
    echo -e "${GREEN}  gemini-icfes --help${NC}             # Mostrar esta ayuda"
    echo ""
    echo -e "${CYAN}📖 DESCRIPCIÓN DE MODOS:${NC}"
    echo ""
    echo -e "${YELLOW}🔰 BÁSICO:${NC} Compatible 100% con tutorial gemini-cli-r-exams.md v2.0"
    echo "   • Modelo: gemini-2.5-pro"
    echo "   • Tokens: 1M (Cuenta Pro)"
    echo "   • Temperatura: 0.1"
    echo "   • Ideal para: Usuarios nuevos, seguimiento del tutorial"
    echo ""
    echo -e "${YELLOW}⚡ OPTIMIZADO:${NC} Configuración Pro con verificaciones avanzadas"
    echo "   • Todas las características del modo básico"
    echo "   • Verificaciones automáticas de configuración"
    echo "   • Mensajes informativos mejorados"
    echo "   • Ideal para: Uso diario optimizado"
    echo ""
    echo -e "${YELLOW}🚀 MCPs:${NC} Funcionalidades extendidas con Model Context Protocol"
    echo "   • Todas las características del modo optimizado"
    echo "   • Búsqueda web (Brave Search)"
    echo "   • Memoria persistente"
    echo "   • Automatización web (Playwright)"
    echo "   • Documentación APIs (Context7)"
    echo "   • Acceso a archivos (Filesystem)"
    echo "   • Ideal para: Trabajo avanzado, investigación"
    echo ""
    echo -e "${CYAN}💡 EJEMPLOS DE USO:${NC}"
    echo "   gemini-icfes                    # Inicia modo básico"
    echo "   gemini-icfes --optimized        # Inicia modo optimizado"
    echo "   gemini-icfes --mcps             # Inicia modo completo"
    echo ""
}

# Función para verificar configuración básica
verify_basic_config() {
    echo -e "${BLUE}🔍 Verificando configuración básica...${NC}"
    
    # Verificar API Key
    if [[ -z "$GEMINI_API_KEY" ]]; then
        echo -e "${RED}❌ Error: GEMINI_API_KEY no configurada${NC}"
        echo "   Configura tu API Key primero:"
        echo "   export GEMINI_API_KEY=\"tu_api_key_aqui\""
        exit 1
    fi
    echo -e "${GREEN}✅ API Key configurada${NC}"
    
    # Verificar tutorial
    if [[ ! -f "$PROJECT_DIR/Auxiliares/Instalaciones/Ais/Gemini_CLI/gemini-cli-r-exams.md" ]]; then
        echo -e "${RED}❌ Error: Tutorial gemini-cli-r-exams.md no encontrado${NC}"
        exit 1
    fi
    echo -e "${GREEN}✅ Tutorial encontrado${NC}"
    
    # Verificar Gemini CLI
    if ! command -v gemini &> /dev/null; then
        echo -e "${RED}❌ Error: Gemini CLI no encontrado${NC}"
        echo "   Instala siguiendo el tutorial"
        exit 1
    fi
    echo -e "${GREEN}✅ Gemini CLI instalado${NC}"
}

# Función para verificar configuración MCPs
verify_mcps_config() {
    echo -e "${BLUE}🔍 Verificando configuración MCPs...${NC}"
    
    if [[ ! -f "$PROJECT_DIR/.gemini-mcp-config.json" ]]; then
        echo -e "${RED}❌ Error: Configuración MCP no encontrada${NC}"
        echo "   Ejecuta: bash configure-gemini-mcps.sh"
        exit 1
    fi
    echo -e "${GREEN}✅ Configuración MCPs encontrada${NC}"
    
    # Verificar que max_tokens sea correcto (1M)
    local max_tokens=$(grep -o '"max_tokens": [0-9]*' "$PROJECT_DIR/.gemini-mcp-config.json" | grep -o '[0-9]*')
    if [[ "$max_tokens" != "1000000" ]]; then
        echo -e "${YELLOW}⚠️  Corrigiendo max_tokens a 1M (tutorial v2.0)...${NC}"
        sed -i 's/"max_tokens": [0-9]*/"max_tokens": 1000000/' "$PROJECT_DIR/.gemini-mcp-config.json"
        echo -e "${GREEN}✅ max_tokens corregido a 1M${NC}"
    else
        echo -e "${GREEN}✅ max_tokens configurado correctamente (1M)${NC}"
    fi
    
    # Cargar variables de entorno para MCPs
    if [[ -f "$SCRIPT_DIR/mcp-env-setup.sh" ]]; then
        source "$SCRIPT_DIR/mcp-env-setup.sh"
        echo -e "${GREEN}✅ Variables MCPs cargadas${NC}"
    fi
}

# Función para mostrar información del modo
show_mode_info() {
    local mode=$1
    
    echo -e "${BLUE}📁 Directorio del proyecto:${NC} $PROJECT_DIR"
    echo -e "${BLUE}📋 Modo seleccionado:${NC} $mode"
    echo ""
    
    case $mode in
        "BÁSICO")
            echo -e "${CYAN}🔰 CONFIGURACIÓN BÁSICA (Tutorial v2.0):${NC}"
            echo "  • Modelo: gemini-2.5-pro (Cuenta Pro)"
            echo "  • Tokens: 1M (1,000,000)"
            echo "  • Temperatura: 0.1 (consistencia máxima)"
            echo "  • Archivos: GEMINI.md, rules-gemini.md, task-list-gemini.md"
            ;;
        "OPTIMIZADO")
            echo -e "${CYAN}⚡ CONFIGURACIÓN OPTIMIZADA:${NC}"
            echo "  • Todas las características del modo básico"
            echo "  • Verificaciones automáticas avanzadas"
            echo "  • Mensajes informativos mejorados"
            echo "  • Detección automática de archivos del tutorial"
            ;;
        "MCPs")
            echo -e "${CYAN}🚀 CONFIGURACIÓN COMPLETA CON MCPs:${NC}"
            echo "  • Todas las características del modo optimizado"
            echo "  • ✅ Context7 - Documentación de librerías"
            echo "  • ✅ Playwright - Automatización web"
            echo "  • ✅ Memory - Gestión de memoria persistente"
            echo "  • ✅ Brave Search - Búsqueda web privada"
            echo "  • ✅ Filesystem - Acceso a archivos locales"
            ;;
    esac
    echo ""
}

# Función principal de inicio
start_gemini() {
    local mode=$1
    
    # Cambiar al directorio del proyecto
    cd "$PROJECT_DIR"
    
    # Verificar que no estamos en root
    if [[ "$PWD" == "/" ]]; then
        echo -e "${RED}⚠️  Error: No se puede ejecutar desde directorio raíz${NC}"
        exit 1
    fi
    
    echo -e "${BLUE}📍 Directorio actual:${NC} $(pwd)"
    echo -e "${BLUE}🚀 Iniciando Gemini CLI...${NC}"
    echo ""
    
    # Configurar variables de entorno según tutorial
    export GEMINI_PROJECT_ROOT="$PROJECT_DIR"
    
    # Configurar variables específicas según modo
    case $mode in
        "MCPs")
            export MCP_CONFIG_PATH="$PROJECT_DIR/.mcp-config.json"
            export GEMINI_MCP_CONFIG="$PROJECT_DIR/.gemini-mcp-config.json"
            ;;
    esac
    
    # Mostrar comandos disponibles
    echo -e "${CYAN}💡 Comandos disponibles:${NC}"
    echo "   • 'Analiza ejercicio: @path/file.Rmd' → Análisis completo"
    echo "   • 'Genera TikZ desde imagen' → Código TikZ fidelidad 98%"
    echo "   • 'Valida estándares ICFES' → Verificación completa"
    echo "   • 'Optimiza código R-exams' → Mejoras automáticas"
    
    if [[ "$mode" == "MCPs" ]]; then
        echo ""
        echo -e "${CYAN}🔧 Comandos MCPs adicionales:${NC}"
        echo "   • 'buscar información sobre [tema]' → Brave Search"
        echo "   • 'documentación de [librería]' → Context7"
        echo "   • 'automatizar navegación web' → Playwright"
        echo "   • 'recordar [información]' → Memory"
        echo "   • 'leer archivo [path]' → Filesystem"
    fi
    
    echo ""
    echo -e "${CYAN}📖 Para cargar contexto completo:${NC}"
    echo "   @Auxiliares/Instalaciones/Ais/Gemini_CLI/GEMINI.md"
    echo ""
    echo -e "${CYAN}📚 Tutorial completo:${NC}"
    echo "   @Auxiliares/Instalaciones/Ais/Gemini_CLI/gemini-cli-r-exams.md"
    echo ""
    
    # Iniciar Gemini CLI con configuración Pro
    echo -e "${GREEN}🤖 Iniciando Gemini 2.5 Pro...${NC}"
    gemini --model gemini-2.5-pro
}

# Función principal
main() {
    local mode="BÁSICO"  # Modo por defecto
    
    # Procesar argumentos
    case "${1:-}" in
        --help|-h)
            show_help
            exit 0
            ;;
        --basic)
            mode="BÁSICO"
            ;;
        --optimized)
            mode="OPTIMIZADO"
            ;;
        --mcps)
            mode="MCPs"
            ;;
        "")
            mode="BÁSICO"
            ;;
        *)
            echo -e "${RED}❌ Modo desconocido: $1${NC}"
            echo "Usa --help para ver los modos disponibles"
            exit 1
            ;;
    esac
    
    # Mostrar encabezado
    echo -e "${BLUE}🤖 GEMINI CLI UNIFICADO - PROYECTO ICFES R-EXAMS${NC}"
    echo -e "${BLUE}=================================================${NC}"
    echo ""
    
    # Verificar configuración básica
    verify_basic_config
    
    # Verificar configuración específica del modo
    if [[ "$mode" == "MCPs" ]]; then
        verify_mcps_config
    fi
    
    # Mostrar información del modo
    show_mode_info "$mode"
    
    # Iniciar Gemini CLI
    start_gemini "$mode"
}

# Ejecutar función principal con todos los argumentos
main "$@"
