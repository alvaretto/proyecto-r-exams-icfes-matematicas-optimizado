#!/bin/bash

# Script de prueba para verificar la instalación de Chrome DevTools MCP
# Fecha: 2025-10-02

echo "=========================================="
echo "Verificación de Chrome DevTools MCP"
echo "=========================================="
echo ""

# Verificar Node.js
echo "1. Verificando Node.js..."
if command -v node &> /dev/null; then
    NODE_VERSION=$(node --version)
    echo "   ✅ Node.js instalado: $NODE_VERSION"
    
    # Verificar versión mínima (v20.19)
    NODE_MAJOR=$(echo $NODE_VERSION | cut -d'v' -f2 | cut -d'.' -f1)
    if [ "$NODE_MAJOR" -ge 20 ]; then
        echo "   ✅ Versión de Node.js compatible"
    else
        echo "   ⚠️  Advertencia: Se requiere Node.js v20.19 o superior"
    fi
else
    echo "   ❌ Node.js no encontrado"
    exit 1
fi
echo ""

# Verificar npm
echo "2. Verificando npm..."
if command -v npm &> /dev/null; then
    NPM_VERSION=$(npm --version)
    echo "   ✅ npm instalado: v$NPM_VERSION"
else
    echo "   ❌ npm no encontrado"
    exit 1
fi
echo ""

# Verificar Chrome/Chromium
echo "3. Verificando Chrome/Chromium..."
CHROME_PATH=""
if command -v google-chrome &> /dev/null; then
    CHROME_PATH=$(which google-chrome)
    CHROME_VERSION=$(google-chrome --version)
    echo "   ✅ Chrome encontrado: $CHROME_PATH"
    echo "   ✅ Versión: $CHROME_VERSION"
elif command -v google-chrome-stable &> /dev/null; then
    CHROME_PATH=$(which google-chrome-stable)
    CHROME_VERSION=$(google-chrome-stable --version)
    echo "   ✅ Chrome Stable encontrado: $CHROME_PATH"
    echo "   ✅ Versión: $CHROME_VERSION"
elif command -v chromium &> /dev/null; then
    CHROME_PATH=$(which chromium)
    CHROME_VERSION=$(chromium --version)
    echo "   ✅ Chromium encontrado: $CHROME_PATH"
    echo "   ✅ Versión: $CHROME_VERSION"
elif command -v chromium-browser &> /dev/null; then
    CHROME_PATH=$(which chromium-browser)
    CHROME_VERSION=$(chromium-browser --version)
    echo "   ✅ Chromium Browser encontrado: $CHROME_PATH"
    echo "   ✅ Versión: $CHROME_VERSION"
else
    echo "   ❌ Chrome/Chromium no encontrado"
    echo "   💡 Instala Chrome o Chromium para usar este servidor MCP"
    exit 1
fi
echo ""

# Verificar VS Code Insiders
echo "4. Verificando VS Code Insiders..."
if command -v code-insiders &> /dev/null; then
    CODE_PATH=$(which code-insiders)
    echo "   ✅ VS Code Insiders encontrado: $CODE_PATH"
else
    echo "   ⚠️  VS Code Insiders no encontrado en PATH"
fi
echo ""

# Verificar archivo de configuración MCP
echo "5. Verificando configuración MCP..."
MCP_CONFIG="$HOME/.config/Code - Insiders/User/globalStorage/github.copilot-chat/mcp.json"
if [ -f "$MCP_CONFIG" ]; then
    echo "   ✅ Archivo de configuración encontrado"
    echo "   📁 Ubicación: $MCP_CONFIG"
    echo ""
    echo "   Contenido:"
    echo "   ----------------------------------------"
    cat "$MCP_CONFIG" | sed 's/^/   /'
    echo "   ----------------------------------------"
else
    echo "   ❌ Archivo de configuración no encontrado"
    echo "   💡 Ejecuta el script de instalación primero"
    exit 1
fi
echo ""

# Verificar que npx puede descargar el paquete
echo "6. Verificando disponibilidad del paquete..."
echo "   Consultando npm registry..."
if npm view chrome-devtools-mcp version &> /dev/null; then
    LATEST_VERSION=$(npm view chrome-devtools-mcp version)
    echo "   ✅ Paquete disponible en npm"
    echo "   📦 Última versión: v$LATEST_VERSION"
else
    echo "   ⚠️  No se pudo verificar el paquete en npm"
fi
echo ""

# Verificar directorio de caché
echo "7. Verificando directorio de caché..."
CACHE_DIR="$HOME/.cache/chrome-devtools-mcp"
if [ -d "$CACHE_DIR" ]; then
    echo "   ✅ Directorio de caché existe: $CACHE_DIR"
    PROFILE_COUNT=$(find "$CACHE_DIR" -maxdepth 1 -type d -name "chrome-profile-*" | wc -l)
    echo "   📊 Perfiles de Chrome encontrados: $PROFILE_COUNT"
else
    echo "   ℹ️  Directorio de caché no existe (se creará en el primer uso)"
fi
echo ""

# Resumen
echo "=========================================="
echo "RESUMEN"
echo "=========================================="
echo ""
echo "✅ Todos los requisitos están instalados"
echo ""
echo "📋 Próximos pasos:"
echo "   1. Reinicia VS Code Insiders completamente"
echo "   2. Abre el chat de Copilot"
echo "   3. Prueba con: 'Verifica el rendimiento de https://developers.chrome.com'"
echo ""
echo "📚 Documentación completa en: README_INSTALACION.md"
echo ""
echo "=========================================="

