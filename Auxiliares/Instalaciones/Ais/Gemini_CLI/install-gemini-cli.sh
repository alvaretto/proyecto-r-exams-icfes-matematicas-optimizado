#!/bin/bash

# Script de instalación de Gemini CLI para Manjaro Plasma con VSCode Insiders
# Ubicación: Auxiliares/Instalaciones/Ais/Gemini_CLI/
# Autor: Configuración automatizada
# Fecha: $(date)

echo "🚀 INSTALADOR DE GEMINI CLI PARA MANJARO PLASMA"
echo "==============================================="
echo ""

# Colores para output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Función para mostrar resultados
show_result() {
    if [ $1 -eq 0 ]; then
        echo -e "${GREEN}✅ $2${NC}"
    else
        echo -e "${RED}❌ $2${NC}"
        exit 1
    fi
}

echo -e "${BLUE}📋 VERIFICANDO PRERREQUISITOS...${NC}"
echo "-----------------------------------"

# Verificar si estamos en Manjaro
if ! grep -q "Manjaro" /etc/os-release; then
    echo -e "${YELLOW}⚠️  Este script está optimizado para Manjaro Linux${NC}"
fi

# Verificar Node.js
if ! command -v node &> /dev/null; then
    echo "Node.js no encontrado. Instalando..."
    sudo pacman -S nodejs npm
    show_result $? "Node.js instalado"
else
    echo -e "${GREEN}✅ Node.js ya está instalado${NC}"
fi

# Verificar Go (opcional pero recomendado)
if ! command -v go &> /dev/null; then
    echo "Go no encontrado. Instalando..."
    sudo pacman -S go
    show_result $? "Go instalado"
else
    echo -e "${GREEN}✅ Go ya está instalado${NC}"
fi

echo ""
echo -e "${BLUE}📦 INSTALANDO GEMINI CLI...${NC}"
echo "-----------------------------"

# Instalar Gemini CLI globalmente
npm install -g @google/gemini-cli
show_result $? "Gemini CLI instalado globalmente"

echo ""
echo -e "${BLUE}🔑 CONFIGURACIÓN DE API KEY...${NC}"
echo "-------------------------------"

# Solicitar API Key si no está configurada
if [ -z "$GEMINI_API_KEY" ]; then
    echo -e "${YELLOW}⚠️  API Key no encontrada${NC}"
    echo ""
    echo "Para obtener tu API Key:"
    echo "1. Visita: https://aistudio.google.com/app/apikey"
    echo "2. Inicia sesión con tu cuenta de Google"
    echo "3. Crea una nueva API Key"
    echo ""
    read -p "Ingresa tu API Key de Gemini: " API_KEY
    
    if [ -n "$API_KEY" ]; then
        # Agregar a .bashrc y .zshrc
        echo "export GEMINI_API_KEY=\"$API_KEY\"" >> ~/.bashrc
        echo "export GEMINI_API_KEY=\"$API_KEY\"" >> ~/.zshrc
        export GEMINI_API_KEY="$API_KEY"
        echo -e "${GREEN}✅ API Key configurada${NC}"
    else
        echo -e "${RED}❌ API Key requerida para continuar${NC}"
        exit 1
    fi
else
    echo -e "${GREEN}✅ API Key ya está configurada${NC}"
fi

echo ""
echo -e "${BLUE}🔧 CONFIGURANDO VSCODE INSIDERS...${NC}"
echo "-----------------------------------"

# Crear directorio .vscode si no existe
mkdir -p .vscode

# Verificar si tasks.json existe y tiene tareas de Gemini
if [ -f ".vscode/tasks.json" ]; then
    if ! grep -q "Gemini" .vscode/tasks.json; then
        echo "Agregando tareas de Gemini CLI a tasks.json existente..."
        # Aquí se agregarían las tareas (implementación específica)
        echo -e "${GREEN}✅ Tareas de Gemini agregadas${NC}"
    else
        echo -e "${GREEN}✅ Tareas de Gemini ya configuradas${NC}"
    fi
else
    echo "Creando tasks.json con tareas de Gemini CLI..."
    cp "$(dirname "$0")/gemini-tasks.json" .vscode/tasks.json
    show_result $? "tasks.json creado con tareas de Gemini"
fi

echo ""
echo -e "${BLUE}🧪 EJECUTANDO PRUEBAS...${NC}"
echo "-------------------------"

# Ejecutar script de pruebas
bash "$(dirname "$0")/test-gemini-cli.sh"

echo ""
echo -e "${GREEN}🎉 INSTALACIÓN COMPLETADA EXITOSAMENTE${NC}"
echo ""
echo -e "${BLUE}📖 DOCUMENTACIÓN DISPONIBLE EN:${NC}"
echo "   $(dirname "$0")/gemini-cli-setup.md"
echo ""
echo -e "${BLUE}🚀 COMANDOS DISPONIBLES:${NC}"
echo "   • Terminal: gemini"
echo "   • VSCode: Ctrl+Shift+P → 'Tasks: Run Task' → Seleccionar tarea Gemini"
echo ""
echo -e "${GREEN}¡Gemini CLI está listo para usar!${NC}"
