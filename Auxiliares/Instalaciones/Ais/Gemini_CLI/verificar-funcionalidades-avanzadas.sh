#!/bin/bash

# =============================================================================
# SCRIPT DE VERIFICACIÓN DE FUNCIONALIDADES AVANZADAS GEMINI CLI
# =============================================================================
# Verifica que todas las funcionalidades documentadas estén disponibles
# Autor: Especialista en Integración IA Educativa
# Fecha: Agosto 24, 2025
# Proyecto: RepositorioMatematicasICFES_R_Exams
# =============================================================================

set -e

# Colores para output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[✅]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[⚠️]${NC} $1"
}

print_error() {
    echo -e "${RED}[❌]${NC} $1"
}

print_header() {
    echo -e "\n${BLUE}==============================================================================${NC}"
    echo -e "${BLUE}$1${NC}"
    echo -e "${BLUE}==============================================================================${NC}\n"
}

# Verificar directorio
if [[ ! -f "GEMINI.md" ]]; then
    print_error "Ejecutar desde el directorio raíz del proyecto"
    exit 1
fi

print_header "VERIFICACIÓN DE FUNCIONALIDADES AVANZADAS GEMINI CLI"

# Variables para tracking
TOTAL_CHECKS=0
PASSED_CHECKS=0

check_item() {
    TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
    if eval "$2"; then
        print_success "$1"
        PASSED_CHECKS=$((PASSED_CHECKS + 1))
        return 0
    else
        print_error "$1"
        return 1
    fi
}

warning_item() {
    TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
    print_warning "$1"
}

# VERIFICACIONES BÁSICAS
print_status "=== VERIFICACIONES BÁSICAS ==="

check_item "Node.js instalado" "command -v node >/dev/null 2>&1"
check_item "npm instalado" "command -v npm >/dev/null 2>&1"
check_item "VSCode Insiders instalado" "command -v code-insiders >/dev/null 2>&1"

# VERIFICACIONES GEMINI CLI
print_status "=== VERIFICACIONES GEMINI CLI ==="

check_item "Gemini CLI instalado" "command -v gemini >/dev/null 2>&1"

if command -v gemini >/dev/null 2>&1; then
    VERSION=$(npm list -g @google/gemini-cli 2>/dev/null | grep @google/gemini-cli | sed 's/.*@google\/gemini-cli@//' | sed 's/ .*//' || echo "desconocida")
    print_status "Versión instalada: $VERSION"
    
    # Verificar si es versión reciente
    if [[ "$VERSION" =~ ^0\.[3-9]\. ]] || [[ "$VERSION" =~ ^[1-9]\. ]]; then
        print_success "Versión compatible con funcionalidades avanzadas ($VERSION)"
        PASSED_CHECKS=$((PASSED_CHECKS + 1))
    else
        print_warning "Versión antigua ($VERSION) - funcionalidades avanzadas pueden no estar disponibles"
    fi
    TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
fi

# VERIFICACIONES EXTENSIÓN VSCODE
print_status "=== VERIFICACIONES EXTENSIÓN VSCODE ==="

check_item "Extensión Gemini CLI Companion instalada" "code-insiders --list-extensions | grep -q 'google.gemini-cli-vscode-ide-companion'"

if code-insiders --list-extensions | grep -q 'google.gemini-cli-vscode-ide-companion'; then
    EXT_VERSION=$(code-insiders --list-extensions --show-versions | grep "google.gemini-cli-vscode-ide-companion" | sed 's/.*@//')
    print_status "Versión de extensión: $EXT_VERSION"
fi

# VERIFICACIONES CONFIGURACIÓN
print_status "=== VERIFICACIONES CONFIGURACIÓN ==="

check_item "Archivo .vscode/settings.json existe" "[[ -f '.vscode/settings.json' ]]"
check_item "Configuración Gemini en VSCode" "grep -q 'gemini.apiKey' .vscode/settings.json"
check_item "Variable GEMINI_API_KEY configurada" "[[ -n \"\$GEMINI_API_KEY\" ]]"

if [[ -f ".vscode/settings.json" ]]; then
    if grep -q "gemini.cli.enableAdvancedFeatures" .vscode/settings.json; then
        print_success "Configuración avanzada en VSCode detectada"
        PASSED_CHECKS=$((PASSED_CHECKS + 1))
    else
        print_warning "Configuración avanzada en VSCode no detectada"
    fi
    TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
fi

# VERIFICACIONES MCPs
print_status "=== VERIFICACIONES MCPs ==="

check_item "Configuración MCPs existe" "[[ -f '.gemini-mcp-config.json' ]]"
check_item "Script test-mcps.sh existe" "[[ -f 'Auxiliares/Instalaciones/Ais/Gemini_CLI/test-mcps.sh' ]]"

# VERIFICACIONES ARCHIVOS DEL PROYECTO
print_status "=== VERIFICACIONES ARCHIVOS DEL PROYECTO ==="

check_item "GEMINI.md (contexto principal)" "[[ -f 'GEMINI.md' ]]"
check_item "Reglas Gemini" "[[ -f '.gemini/rules-gemini.md' ]]"
check_item "Manual de usuario actualizado" "[[ -f 'Auxiliares/Instalaciones/Ais/Gemini_CLI/manual-usuario-gemini-cli-r-exams-icfes.md' ]]"
check_item "Tutorial técnico" "[[ -f 'Auxiliares/Instalaciones/Ais/Gemini_CLI/gemini-cli-r-exams.md' ]]"

# VERIFICAR CONTENIDO DOCUMENTADO
if [[ -f "Auxiliares/Instalaciones/Ais/Gemini_CLI/manual-usuario-gemini-cli-r-exams-icfes.md" ]]; then
    if grep -q "/id install" "Auxiliares/Instalaciones/Ais/Gemini_CLI/manual-usuario-gemini-cli-r-exams-icfes.md"; then
        print_success "Documentación de comandos avanzados encontrada"
        PASSED_CHECKS=$((PASSED_CHECKS + 1))
    else
        print_warning "Documentación de comandos avanzados no encontrada"
    fi
    TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
fi

# PRUEBAS FUNCIONALES
print_status "=== PRUEBAS FUNCIONALES ==="

print_status "Probando acceso básico a Gemini CLI..."
if timeout 5 bash -c 'echo "test" | gemini --help' >/dev/null 2>&1; then
    print_success "Gemini CLI responde correctamente"
    PASSED_CHECKS=$((PASSED_CHECKS + 1))
else
    print_warning "Gemini CLI no responde o requiere configuración adicional"
fi
TOTAL_CHECKS=$((TOTAL_CHECKS + 1))

# RESUMEN FINAL
print_header "RESUMEN DE VERIFICACIÓN"

PERCENTAGE=$((PASSED_CHECKS * 100 / TOTAL_CHECKS))

echo "📊 RESULTADOS:"
echo "• Verificaciones pasadas: $PASSED_CHECKS/$TOTAL_CHECKS"
echo "• Porcentaje de éxito: $PERCENTAGE%"
echo ""

if [[ $PERCENTAGE -ge 90 ]]; then
    print_success "🎉 EXCELENTE: Sistema completamente configurado"
    echo ""
    echo "✅ FUNCIONALIDADES DISPONIBLES:"
    echo "• Gemini CLI básico: ✅ Disponible"
    echo "• Extensión VSCode: ✅ Instalada"
    echo "• MCPs: ✅ Configurados"
    echo "• Documentación: ✅ Actualizada"
    echo ""
    echo "🚀 PRÓXIMOS PASOS:"
    echo "1. Abrir VSCode Insiders: code-insiders ."
    echo "2. Probar comandos avanzados en terminal integrado:"
    echo "   /id install"
    echo "   /init"
    echo "   /memory show"
    echo "   /chat list"
    
elif [[ $PERCENTAGE -ge 70 ]]; then
    print_warning "⚠️ BUENO: Sistema mayormente configurado, algunas mejoras necesarias"
    echo ""
    echo "🔧 ACCIONES RECOMENDADAS:"
    echo "1. Ejecutar: bash Auxiliares/Instalaciones/Ais/Gemini_CLI/actualizar-gemini-cli-avanzado.sh"
    echo "2. Verificar variable GEMINI_API_KEY"
    echo "3. Reiniciar VSCode Insiders"
    
else
    print_error "❌ NECESITA ATENCIÓN: Configuración incompleta"
    echo ""
    echo "🚨 ACCIONES REQUERIDAS:"
    echo "1. Ejecutar: bash Auxiliares/Instalaciones/Ais/Gemini_CLI/actualizar-gemini-cli-avanzado.sh"
    echo "2. Verificar instalación de Node.js y npm"
    echo "3. Configurar variable GEMINI_API_KEY"
    echo "4. Instalar VSCode Insiders si no está disponible"
fi

echo ""
print_status "Para más ayuda, consultar:"
echo "• Manual: Auxiliares/Instalaciones/Ais/Gemini_CLI/manual-usuario-gemini-cli-r-exams-icfes.md"
echo "• Tutorial: Auxiliares/Instalaciones/Ais/Gemini_CLI/gemini-cli-r-exams.md"
echo ""

exit 0
