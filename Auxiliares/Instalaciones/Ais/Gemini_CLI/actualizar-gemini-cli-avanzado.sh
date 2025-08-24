#!/bin/bash

# =============================================================================
# SCRIPT DE ACTUALIZACIÓN GEMINI CLI AVANZADO
# =============================================================================
# Actualiza Gemini CLI a la versión más reciente con funcionalidades avanzadas
# Autor: Especialista en Integración IA Educativa
# Fecha: Agosto 24, 2025
# Proyecto: RepositorioMatematicasICFES_R_Exams
# =============================================================================

set -e  # Salir si hay errores

# Colores para output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Función para imprimir mensajes con colores
print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[✅ ÉXITO]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[⚠️ ADVERTENCIA]${NC} $1"
}

print_error() {
    echo -e "${RED}[❌ ERROR]${NC} $1"
}

print_header() {
    echo -e "\n${BLUE}==============================================================================${NC}"
    echo -e "${BLUE}$1${NC}"
    echo -e "${BLUE}==============================================================================${NC}\n"
}

# Verificar que estamos en el directorio correcto
if [[ ! -f "GEMINI.md" ]]; then
    print_error "Este script debe ejecutarse desde el directorio raíz del proyecto RepositorioMatematicasICFES_R_Exams"
    exit 1
fi

print_header "ACTUALIZACIÓN GEMINI CLI AVANZADO - PROYECTO R-EXAMS ICFES"

# PASO 1: Verificar versión actual
print_status "Verificando versión actual de Gemini CLI..."
CURRENT_VERSION=$(npm list -g @google/gemini-cli 2>/dev/null | grep @google/gemini-cli | sed 's/.*@google\/gemini-cli@//' | sed 's/ .*//' || echo "no instalado")
print_status "Versión actual: $CURRENT_VERSION"

# PASO 2: Verificar extensión VSCode
print_status "Verificando extensión VSCode Companion..."
if code-insiders --list-extensions | grep -q "google.gemini-cli-vscode-ide-companion"; then
    print_success "Extensión VSCode Companion encontrada"
    EXTENSION_VERSION=$(code-insiders --list-extensions --show-versions | grep "google.gemini-cli-vscode-ide-companion" | sed 's/.*@//')
    print_status "Versión de extensión: $EXTENSION_VERSION"
else
    print_warning "Extensión VSCode Companion no encontrada"
fi

# PASO 3: Backup de configuración actual
print_status "Creando backup de configuración actual..."
BACKUP_DIR="Auxiliares/Instalaciones/Ais/Gemini_CLI/backup-$(date +%Y%m%d-%H%M%S)"
mkdir -p "$BACKUP_DIR"
cp -r .vscode "$BACKUP_DIR/" 2>/dev/null || true
cp -r .gemini "$BACKUP_DIR/" 2>/dev/null || true
cp ~/.config/gemini/icfes-config.json "$BACKUP_DIR/" 2>/dev/null || true
print_success "Backup creado en: $BACKUP_DIR"

# PASO 4: Desinstalar versión antigua
if [[ "$CURRENT_VERSION" != "no instalado" ]]; then
    print_status "Desinstalando versión antigua de Gemini CLI..."
    npm uninstall -g @google/gemini-cli
    print_success "Versión antigua desinstalada"
fi

# PASO 5: Instalar versión más reciente
print_status "Instalando versión más reciente de Gemini CLI..."
npm install -g @google/gemini-cli@latest
NEW_VERSION=$(npm list -g @google/gemini-cli 2>/dev/null | grep @google/gemini-cli | sed 's/.*@google\/gemini-cli@//' | sed 's/ .*//')
print_success "Nueva versión instalada: $NEW_VERSION"

# PASO 6: Verificar comandos disponibles
print_status "Verificando comandos disponibles..."
echo "Comandos básicos disponibles:"
gemini --help | grep -A 10 "Comandos:" || true

# PASO 7: Verificar funcionalidades avanzadas
print_status "Verificando funcionalidades avanzadas..."

# Crear script temporal para probar comandos
cat > /tmp/test_gemini_commands.sh << 'EOF'
#!/bin/bash
echo "Probando comandos avanzados de Gemini CLI..."
timeout 5 bash -c 'echo "/help" | gemini' 2>/dev/null || echo "Comando /help: Requiere sesión interactiva"
EOF

chmod +x /tmp/test_gemini_commands.sh
/tmp/test_gemini_commands.sh
rm /tmp/test_gemini_commands.sh

# PASO 8: Actualizar configuración VSCode
print_status "Verificando configuración VSCode actualizada..."
if grep -q "gemini.cli.enableAdvancedFeatures" .vscode/settings.json; then
    print_success "Configuración VSCode ya actualizada"
else
    print_warning "Configuración VSCode necesita actualización manual"
fi

# PASO 9: Verificar MCPs
print_status "Verificando configuración de MCPs..."
if [[ -f ".gemini-mcp-config.json" ]]; then
    print_success "Configuración MCPs encontrada"
else
    print_warning "Configuración MCPs no encontrada - ejecutar install-mcps.sh si es necesario"
fi

# PASO 10: Instrucciones finales
print_header "INSTRUCCIONES FINALES"

print_success "✅ Actualización de Gemini CLI completada"
echo ""
print_status "PRÓXIMOS PASOS PARA ACTIVAR FUNCIONALIDADES AVANZADAS:"
echo ""
echo "1. 🎯 ABRIR VSCODE INSIDERS:"
echo "   code-insiders ."
echo ""
echo "2. 🚀 PROBAR COMANDOS AVANZADOS (en terminal integrado de VSCode):"
echo "   /id install    # Activar proyecto"
echo "   /init          # Memoria semántica"
echo "   /memory show   # Ver memoria"
echo "   /chat list     # Listar conversaciones"
echo ""
echo "3. 📋 COMANDOS ALTERNATIVOS (si los anteriores no funcionan):"
echo "   gemini         # Iniciar modo interactivo"
echo "   /help          # Ver comandos disponibles"
echo "   /mcp           # Gestionar MCPs"
echo ""
echo "4. 🔧 SI HAY PROBLEMAS:"
echo "   - Reiniciar VSCode Insiders"
echo "   - Verificar variable GEMINI_API_KEY"
echo "   - Ejecutar: bash Auxiliares/Instalaciones/Ais/Gemini_CLI/test-mcps.sh"
echo ""

print_header "VERIFICACIÓN FINAL"

echo "📊 RESUMEN DE INSTALACIÓN:"
echo "• Versión anterior: $CURRENT_VERSION"
echo "• Versión nueva: $NEW_VERSION"
echo "• Extensión VSCode: $(code-insiders --list-extensions | grep gemini-cli-vscode-ide-companion | sed 's/.*@//' || echo 'No encontrada')"
echo "• Backup guardado en: $BACKUP_DIR"
echo ""

print_success "🎉 ¡Actualización completada! Ahora puedes usar las funcionalidades avanzadas documentadas."

# Verificación final de funcionalidades
print_status "Para verificar que todo funciona correctamente:"
echo "1. Abre VSCode Insiders: code-insiders ."
echo "2. Abre terminal integrado (Ctrl+\`)"
echo "3. Prueba: /id install"
echo "4. Si funciona, prueba: /init"
echo ""
print_warning "Si los comandos /id, /init, /memory, /chat no funcionan, significa que necesitas la versión preview o nightly de Gemini CLI"
echo ""
echo "Para instalar versión preview:"
echo "npm install -g @google/gemini-cli@preview"
echo ""
echo "Para instalar versión nightly:"
echo "npm install -g @google/gemini-cli@nightly"
