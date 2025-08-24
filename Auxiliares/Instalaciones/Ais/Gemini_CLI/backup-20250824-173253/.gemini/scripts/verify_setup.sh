#!/bin/bash

# Script de verificación completa - Gemini CLI + R-exams ICFES
echo "🔍 VERIFICACIÓN COMPLETA DEL SETUP GEMINI CLI"
echo "=============================================="

# Colores para output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Función para mostrar resultados
show_result() {
    if [ $1 -eq 0 ]; then
        echo -e "${GREEN}✅ $2${NC}"
    else
        echo -e "${RED}❌ $2${NC}"
    fi
}

# Función para mostrar advertencias
show_warning() {
    echo -e "${YELLOW}⚠️  $1${NC}"
}

# Función para mostrar información
show_info() {
    echo -e "${BLUE}ℹ️  $1${NC}"
}

echo ""
echo "📋 VERIFICANDO INSTALACIONES BÁSICAS..."
echo "======================================="

# Verificar Node.js
if command -v node &> /dev/null; then
    NODE_VERSION=$(node --version)
    show_result 0 "Node.js instalado: $NODE_VERSION"
else
    show_result 1 "Node.js NO encontrado"
fi

# Verificar npm
if command -v npm &> /dev/null; then
    NPM_VERSION=$(npm --version)
    show_result 0 "npm instalado: $NPM_VERSION"
else
    show_result 1 "npm NO encontrado"
fi

# Verificar Gemini CLI
if command -v gemini &> /dev/null; then
    GEMINI_VERSION=$(gemini --version 2>/dev/null || echo "versión no disponible")
    show_result 0 "Gemini CLI instalado: $GEMINI_VERSION"
else
    show_result 1 "Gemini CLI NO encontrado"
fi

echo ""
echo "🔑 VERIFICANDO AUTENTICACIÓN..."
echo "==============================="

# Verificar API Key
if [ -n "$GEMINI_API_KEY" ]; then
    show_result 0 "Variable GEMINI_API_KEY configurada"
    
    # Test básico de autenticación
    if timeout 30 gemini "Test de autenticación: 2+2" &> /dev/null; then
        show_result 0 "Autenticación funcional"
    else
        show_result 1 "Problema con autenticación"
    fi
else
    show_result 1 "Variable GEMINI_API_KEY NO configurada"
fi

echo ""
echo "⚙️  VERIFICANDO CONFIGURACIÓN..."
echo "==============================="

# Verificar archivo de configuración
CONFIG_FILE="$HOME/.config/gemini/icfes-config.json"
if [ -f "$CONFIG_FILE" ]; then
    show_result 0 "Archivo de configuración existe"
    
    # Verificar contenido de configuración
    if grep -q "gemini-2.5-pro" "$CONFIG_FILE"; then
        show_result 0 "Modelo Pro configurado"
    else
        show_warning "Modelo Pro no configurado correctamente"
    fi
    
    if grep -q "1000000" "$CONFIG_FILE"; then
        show_result 0 "Max tokens configurado (1M)"
    else
        show_warning "Max tokens no configurado correctamente"
    fi
    
    if grep -q "0.1" "$CONFIG_FILE"; then
        show_result 0 "Temperatura configurada (0.1)"
    else
        show_warning "Temperatura no configurada correctamente"
    fi
else
    show_result 1 "Archivo de configuración NO existe"
fi

echo ""
echo "📁 VERIFICANDO ARCHIVOS DEL PROYECTO..."
echo "======================================="

# Verificar archivos de contexto
if [ -f "GEMINI.md" ]; then
    show_result 0 "Archivo GEMINI.md existe"
else
    show_result 1 "Archivo GEMINI.md NO existe"
fi

if [ -f ".gemini/rules-gemini.md" ]; then
    show_result 0 "Archivo rules-gemini.md existe"
else
    show_result 1 "Archivo rules-gemini.md NO existe"
fi

if [ -f ".gemini/task-list-gemini.md" ]; then
    show_result 0 "Archivo task-list-gemini.md existe"
else
    show_result 1 "Archivo task-list-gemini.md NO existe"
fi

if [ -f ".geminiignore" ]; then
    show_result 0 "Archivo .geminiignore existe"
else
    show_result 1 "Archivo .geminiignore NO existe"
fi

echo ""
echo "🎯 VERIFICANDO SCRIPTS UNIFICADOS..."
echo "===================================="

# Verificar script unificado
UNIFIED_SCRIPT="Auxiliares/Instalaciones/Ais/Gemini_CLI/gemini-icfes-unified.sh"
if [ -f "$UNIFIED_SCRIPT" ]; then
    show_result 0 "Script unificado existe"
    
    if [ -x "$UNIFIED_SCRIPT" ]; then
        show_result 0 "Script unificado es ejecutable"
    else
        show_warning "Script unificado no es ejecutable"
    fi
else
    show_result 1 "Script unificado NO existe"
fi

# Verificar enlaces simbólicos
if [ -L "$HOME/.local/bin/gemini-icfes" ]; then
    show_result 0 "Enlace simbólico gemini-icfes existe"
    
    if [ -x "$HOME/.local/bin/gemini-icfes" ]; then
        show_result 0 "Enlace simbólico es ejecutable"
    else
        show_warning "Enlace simbólico no es ejecutable"
    fi
else
    show_result 1 "Enlace simbólico gemini-icfes NO existe"
fi

echo ""
echo "🔧 VERIFICANDO INTEGRACIÓN VSCODE..."
echo "===================================="

# Verificar VSCode Insiders
if command -v code-insiders &> /dev/null; then
    show_result 0 "VSCode Insiders instalado"
    
    # Verificar configuración VSCode
    VSCODE_CONFIG="$HOME/.config/Code - Insiders/User/settings.json"
    if [ -f "$VSCODE_CONFIG" ]; then
        show_result 0 "Configuración VSCode Insiders existe"
    else
        show_warning "Configuración VSCode Insiders no encontrada"
    fi
    
    # Verificar tareas del proyecto
    if [ -f ".vscode/tasks.json" ]; then
        show_result 0 "Tareas VSCode configuradas"
    else
        show_warning "Tareas VSCode no configuradas"
    fi
    
else
    show_warning "VSCode Insiders no instalado (opcional)"
fi

echo ""
echo "🧪 REALIZANDO TESTS FUNCIONALES..."
echo "=================================="

# Test básico de Gemini CLI
show_info "Probando comando básico..."
if timeout 30 gemini "Test básico: ¿Cuánto es 2+2?" &> /dev/null; then
    show_result 0 "Test básico exitoso"
else
    show_result 1 "Test básico falló"
fi

# Test de contexto
show_info "Probando carga de contexto..."
if [ -f "GEMINI.md" ]; then
    if timeout 30 gemini --context-file "GEMINI.md" "Resume este proyecto en una línea" &> /dev/null; then
        show_result 0 "Test de contexto exitoso"
    else
        show_result 1 "Test de contexto falló"
    fi
else
    show_warning "No se puede probar contexto (archivo GEMINI.md no existe)"
fi

# Test de script unificado
show_info "Probando script unificado..."
if command -v gemini-icfes &> /dev/null; then
    if gemini-icfes --help &> /dev/null; then
        show_result 0 "Script unificado funcional"
    else
        show_result 1 "Script unificado no funciona"
    fi
else
    show_result 1 "Script unificado no accesible"
fi

echo ""
echo "📊 RESUMEN DE VERIFICACIÓN"
echo "=========================="

echo ""
show_info "Verificación completada. Revisa los resultados arriba."
echo ""

if command -v gemini &> /dev/null && [ -n "$GEMINI_API_KEY" ]; then
    echo -e "${GREEN}🎉 SETUP BÁSICO FUNCIONAL${NC}"
    echo ""
    echo "Próximos pasos recomendados:"
    echo "1. Crear archivos de contexto faltantes"
    echo "2. Probar workflows con ejercicios reales"
    echo "3. Familiarizarse con comandos avanzados"
    echo ""
else
    echo -e "${RED}⚠️  SETUP INCOMPLETO${NC}"
    echo ""
    echo "Acciones requeridas:"
    echo "1. Instalar/configurar Gemini CLI"
    echo "2. Configurar API Key"
    echo "3. Crear archivos de configuración"
    echo ""
fi

echo "Para más ayuda, consulta:"
echo "- Tutorial completo: Auxiliares/Instalaciones/Ais/Gemini_CLI/gemini-cli-r-exams.md"
echo "- Documentación del proyecto: GEMINI.md"
echo "- Reglas específicas: .gemini/rules-gemini.md"
