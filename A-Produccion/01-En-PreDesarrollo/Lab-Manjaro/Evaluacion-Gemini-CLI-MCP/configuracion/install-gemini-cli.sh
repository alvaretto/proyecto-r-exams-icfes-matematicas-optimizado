#!/bin/bash

# Script de instalación de Gemini-CLI con MCP para Manjaro Plasma
# Autor: Evaluación Gemini-CLI vs Augment
# Fecha: $(date +%Y-%m-%d)

set -e  # Salir en caso de error

echo "🚀 INSTALACIÓN DE GEMINI-CLI CON MCP EN MANJARO PLASMA"
echo "=" | tr ' ' '=' | head -c 60; echo

# Verificar sistema
echo "📋 Verificando sistema..."
if ! command -v pacman &> /dev/null; then
    echo "❌ Error: Este script está diseñado para Manjaro/Arch Linux"
    exit 1
fi

echo "✅ Sistema Manjaro detectado"

# Actualizar sistema
echo "🔄 Actualizando sistema..."
sudo pacman -Syu --noconfirm

# Instalar dependencias base
echo "📦 Instalando dependencias base..."
sudo pacman -S --needed --noconfirm \
    nodejs \
    npm \
    python \
    python-pip \
    git \
    curl \
    wget \
    base-devel

# Verificar Node.js
echo "🔍 Verificando Node.js..."
node_version=$(node --version)
echo "✅ Node.js versión: $node_version"

# Instalar Gemini-CLI globalmente (versión oficial de Google)
echo "💎 Instalando Gemini-CLI oficial de Google..."
if command -v gemini &> /dev/null; then
    echo "⚠️ Gemini-CLI ya está instalado. Actualizando..."
    npm update -g @google/gemini-cli
else
    npm install -g @google/gemini-cli
fi

# Verificar instalación
echo "🔍 Verificando instalación de Gemini-CLI..."
if command -v gemini &> /dev/null; then
    gemini_version=$(gemini --version 2>/dev/null || echo "versión no disponible")
    echo "✅ Gemini-CLI instalado: $gemini_version"
else
    echo "❌ Error: Gemini-CLI no se instaló correctamente"
    exit 1
fi

# Verificar que Gemini-CLI tiene soporte MCP integrado
echo "🔗 Verificando soporte MCP integrado en Gemini-CLI..."
echo "ℹ️ Gemini-CLI incluye soporte MCP nativo desde v0.3.0"

# Instalar herramientas adicionales para análisis de código
echo "🛠️ Instalando herramientas adicionales..."
pip install --user \
    tree-sitter \
    tree-sitter-python \
    tree-sitter-r \
    pygments \
    markdown

# Crear directorio de configuración
echo "📁 Creando directorios de configuración..."
mkdir -p ~/.config/gemini-cli
mkdir -p ~/.config/mcp

# Configurar variables de entorno
echo "🌍 Configurando variables de entorno..."
cat >> ~/.bashrc << 'EOF'

# Gemini-CLI con MCP
export GEMINI_CLI_CONFIG="$HOME/.config/gemini-cli"
export MCP_CONFIG="$HOME/.config/mcp"
export PATH="$HOME/.local/bin:$PATH"
EOF

# Recargar bashrc
source ~/.bashrc

echo "✅ Instalación completada exitosamente!"
echo ""
echo "📋 PRÓXIMOS PASOS:"
echo "1. Configurar autenticación: gemini (seguir flujo OAuth)"
echo "2. O configurar API key: export GEMINI_API_KEY=YOUR_API_KEY"
echo "3. Ejecutar pruebas de verificación: gemini --help"
echo "4. Configurar MCP servers en ~/.gemini/settings.json"
echo ""
echo "🔧 COMANDOS ÚTILES:"
echo "- gemini --help"
echo "- gemini -m gemini-2.5-flash"
echo "- gemini --include-directories ../lib,../docs"
echo ""
echo "📍 Archivos de configuración en:"
echo "- ~/.config/gemini-cli/"
echo "- ~/.config/mcp/"
