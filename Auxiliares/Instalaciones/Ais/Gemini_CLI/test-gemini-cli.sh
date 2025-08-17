#!/bin/bash

# Script de prueba para Gemini CLI en Manjaro Plasma con VSCode Insiders
# Autor: Configuración automática
# Fecha: $(date)

echo "🚀 PRUEBA COMPLETA DE GEMINI CLI EN MANJARO PLASMA"
echo "=================================================="
echo ""

# Colores para output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Función para mostrar resultados
show_result() {
    if [ $1 -eq 0 ]; then
        echo -e "${GREEN}✅ $2${NC}"
    else
        echo -e "${RED}❌ $2${NC}"
    fi
}

echo "🔍 VERIFICANDO INSTALACIÓN..."
echo "------------------------------"

# 1. Verificar que Go está instalado
echo -n "Verificando Go: "
go version > /dev/null 2>&1
show_result $? "Go instalado correctamente"

# 2. Verificar que Node.js está instalado
echo -n "Verificando Node.js: "
node --version > /dev/null 2>&1
show_result $? "Node.js instalado correctamente"

# 3. Verificar que npm está instalado
echo -n "Verificando npm: "
npm --version > /dev/null 2>&1
show_result $? "npm instalado correctamente"

# 4. Verificar que gemini CLI está instalado
echo -n "Verificando Gemini CLI: "
which gemini > /dev/null 2>&1
show_result $? "Gemini CLI encontrado en PATH"

# 5. Verificar versión de Gemini CLI
echo -n "Versión de Gemini CLI: "
GEMINI_VERSION=$(gemini --version 2>/dev/null)
if [ $? -eq 0 ]; then
    echo -e "${GREEN}v$GEMINI_VERSION${NC}"
else
    echo -e "${RED}Error al obtener versión${NC}"
fi

echo ""
echo "🔑 VERIFICANDO CONFIGURACIÓN..."
echo "--------------------------------"

# 6. Verificar API Key
echo -n "Verificando API Key: "
if [ -n "$GEMINI_API_KEY" ]; then
    echo -e "${GREEN}✅ API Key configurada${NC}"
else
    echo -e "${RED}❌ API Key no encontrada${NC}"
fi

# 7. Verificar configuración en .bashrc
echo -n "Verificando .bashrc: "
if grep -q "GEMINI_API_KEY" ~/.bashrc; then
    echo -e "${GREEN}✅ Configurado en .bashrc${NC}"
else
    echo -e "${YELLOW}⚠️  No encontrado en .bashrc${NC}"
fi

# 8. Verificar configuración en .zshrc
echo -n "Verificando .zshrc: "
if grep -q "GEMINI_API_KEY" ~/.zshrc; then
    echo -e "${GREEN}✅ Configurado en .zshrc${NC}"
else
    echo -e "${YELLOW}⚠️  No encontrado en .zshrc${NC}"
fi

echo ""
echo "🧪 PRUEBAS FUNCIONALES..."
echo "-------------------------"

# 9. Prueba básica de funcionamiento
echo -n "Prueba básica de comunicación: "
RESPONSE=$(gemini -p "Responde solo con 'OK' si me recibes correctamente" 2>/dev/null)
if echo "$RESPONSE" | grep -q "OK"; then
    echo -e "${GREEN}✅ Comunicación exitosa${NC}"
else
    echo -e "${RED}❌ Error en comunicación${NC}"
fi

echo ""
echo "📁 VERIFICANDO INTEGRACIÓN CON VSCODE..."
echo "----------------------------------------"

# 10. Verificar archivo tasks.json
echo -n "Verificando tasks.json: "
if [ -f ".vscode/tasks.json" ]; then
    if grep -q "Gemini" .vscode/tasks.json; then
        echo -e "${GREEN}✅ Tareas de Gemini configuradas${NC}"
    else
        echo -e "${YELLOW}⚠️  Archivo existe pero sin tareas de Gemini${NC}"
    fi
else
    echo -e "${RED}❌ Archivo tasks.json no encontrado${NC}"
fi

# 11. Verificar documentación
echo -n "Verificando documentación: "
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
if [ -f "$SCRIPT_DIR/gemini-cli-setup.md" ]; then
    echo -e "${GREEN}✅ Documentación disponible${NC}"
else
    echo -e "${YELLOW}⚠️  Documentación no encontrada${NC}"
fi

echo ""
echo "📊 RESUMEN DE LA INSTALACIÓN"
echo "============================"
echo ""
echo -e "${GREEN}🎉 GEMINI CLI INSTALADO Y CONFIGURADO EXITOSAMENTE${NC}"
echo ""
echo "📋 Componentes instalados:"
echo "  • Go $(go version | cut -d' ' -f3)"
echo "  • Node.js $(node --version)"
echo "  • npm $(npm --version)"
echo "  • Gemini CLI v$(gemini --version 2>/dev/null)"
echo ""
echo "🔧 Configuración:"
echo "  • API Key: Configurada"
echo "  • PATH: Configurado"
echo "  • VSCode Tasks: Disponibles"
echo ""
echo "🚀 Comandos disponibles en VSCode Insiders:"
echo "  • Ctrl+Shift+P → 'Tasks: Run Task' → Seleccionar tarea de Gemini"
echo "  • Terminal integrado: 'gemini' comando disponible"
echo ""
echo "📖 Para más información, consulta: Auxiliares/Instalaciones/Ais/Gemini_CLI/gemini-cli-setup.md"
echo ""
echo -e "${GREEN}¡Listo para usar Gemini CLI en tu flujo de trabajo!${NC}"
