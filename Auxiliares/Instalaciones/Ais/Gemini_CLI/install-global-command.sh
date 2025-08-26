#!/bin/bash

# Script para instalar gemini-icfes como comando global
# Ubicación: Auxiliares/Instalaciones/Ais/Gemini_CLI/
# Autor: Configuración automatizada para proyecto ICFES R-exams

echo "🔧 INSTALACIÓN DE COMANDO GLOBAL GEMINI-ICFES"
echo "=============================================="
echo ""

# Colores para output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Función para mostrar resultados
show_result() {
    if [ $1 -eq 0 ]; then
        echo -e "${GREEN}✅ $2${NC}"
    else
        echo -e "${RED}❌ $2${NC}"
    fi
}

# Obtener directorio del script y proyecto
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../../.." && pwd)"

echo -e "${BLUE}📋 VERIFICANDO PRERREQUISITOS...${NC}"
echo "-----------------------------------"

# Verificar Gemini CLI
echo -n "Verificando Gemini CLI: "
if command -v gemini &> /dev/null; then
    GEMINI_VERSION=$(gemini --version 2>/dev/null || echo "instalado")
    echo -e "${GREEN}✅ $GEMINI_VERSION${NC}"
else
    echo -e "${RED}❌ Gemini CLI no encontrado${NC}"
    echo "Por favor, instala Gemini CLI primero"
    exit 1
fi

# Verificar script principal
echo -n "Verificando script principal: "
if [ -f "$SCRIPT_DIR/gemini-icfes-mcps.sh" ]; then
    echo -e "${GREEN}✅ Encontrado${NC}"
else
    echo -e "${RED}❌ No encontrado${NC}"
    echo "Ejecuta: bash configure-gemini-mcps.sh"
    exit 1
fi

echo ""
echo -e "${BLUE}🔧 INSTALANDO COMANDO GLOBAL...${NC}"
echo "--------------------------------"

# Crear directorio para binarios locales si no existe
LOCAL_BIN="$HOME/.local/bin"
mkdir -p "$LOCAL_BIN"

# Crear script wrapper global
cat > "$LOCAL_BIN/gemini-icfes" << 'EOF'
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
EOF

# Hacer ejecutable
chmod +x "$LOCAL_BIN/gemini-icfes"
show_result $? "Script global creado en $LOCAL_BIN/gemini-icfes"

# Verificar si ~/.local/bin está en PATH
echo -n "Verificando PATH: "
if echo "$PATH" | grep -q "$HOME/.local/bin"; then
    echo -e "${GREEN}✅ ~/.local/bin está en PATH${NC}"
else
    echo -e "${YELLOW}⚠️  ~/.local/bin no está en PATH${NC}"
    
    # Agregar a .bashrc si no existe
    if ! grep -q "export PATH=\"\$HOME/.local/bin:\$PATH\"" "$HOME/.bashrc" 2>/dev/null; then
        echo "" >> "$HOME/.bashrc"
        echo "# Agregar ~/.local/bin al PATH para comandos locales" >> "$HOME/.bashrc"
        echo "export PATH=\"\$HOME/.local/bin:\$PATH\"" >> "$HOME/.bashrc"
        show_result $? "PATH agregado a ~/.bashrc"
        
        echo -e "${YELLOW}⚠️  Necesitas recargar tu shell o ejecutar:${NC}"
        echo "   source ~/.bashrc"
    else
        show_result 0 "PATH ya configurado en ~/.bashrc"
    fi
fi

echo ""
echo -e "${BLUE}🔧 CREANDO COMANDOS ADICIONALES...${NC}"
echo "----------------------------------"

# Crear comandos adicionales con parámetros específicos
cat > "$LOCAL_BIN/gemini-icfes-mcps" << EOF
#!/bin/bash
# Comando específico para mostrar MCPs disponibles
exec gemini-icfes --mcps
EOF

cat > "$LOCAL_BIN/gemini-thinking" << EOF
#!/bin/bash
# Comando para análisis estructurado
exec gemini-icfes "usar thinking para analizar: \$*"
EOF

cat > "$LOCAL_BIN/gemini-validate" << EOF
#!/bin/bash
# Comando para validación LaTeX/TikZ
exec gemini-icfes "validar código latex: \$*"
EOF

cat > "$LOCAL_BIN/gemini-test" << EOF
#!/bin/bash
# Comando para testing automático
exec gemini-icfes "testing automático: \$*"
EOF

cat > "$LOCAL_BIN/gemini-image" << EOF
#!/bin/bash
# Comando para análisis de imágenes
exec gemini-icfes "analizar imagen para tikz: \$*"
EOF

# Hacer ejecutables todos los comandos
chmod +x "$LOCAL_BIN/gemini-icfes-mcps"
chmod +x "$LOCAL_BIN/gemini-thinking"
chmod +x "$LOCAL_BIN/gemini-validate"
chmod +x "$LOCAL_BIN/gemini-test"
chmod +x "$LOCAL_BIN/gemini-image"

show_result $? "Comandos adicionales creados"

echo ""
echo -e "${GREEN}✅ INSTALACIÓN COMPLETADA${NC}"
echo "=========================="
echo ""
echo -e "${CYAN}📋 Comandos instalados en $LOCAL_BIN:${NC}"
echo "  • gemini-icfes - Comando principal con MCPs"
echo "  • gemini-icfes-mcps - Mostrar MCPs disponibles"
echo "  • gemini-thinking - Análisis estructurado"
echo "  • gemini-validate - Validación LaTeX/TikZ"
echo "  • gemini-test - Testing automático"
echo "  • gemini-image - Análisis de imágenes"
echo ""
echo -e "${CYAN}🚀 Uso de los comandos:${NC}"
echo "  • gemini-icfes                    # Modo interactivo"
echo "  • gemini-icfes --mcps             # Mostrar MCPs"
echo "  • gemini-icfes \"tu pregunta\"      # Prompt directo"
echo "  • gemini-thinking \"problema\"      # Análisis estructurado"
echo "  • gemini-validate archivo.tex     # Validar LaTeX"
echo ""
if ! echo "$PATH" | grep -q "$HOME/.local/bin"; then
    echo -e "${YELLOW}⚠️  IMPORTANTE: Recarga tu shell para usar los comandos:${NC}"
    echo "   source ~/.bashrc"
    echo "   # O abre una nueva terminal"
else
    echo -e "${GREEN}✅ Los comandos están listos para usar${NC}"
fi
echo ""
echo -e "${GREEN}🎉 ¡Instalación global completada exitosamente!${NC}"
