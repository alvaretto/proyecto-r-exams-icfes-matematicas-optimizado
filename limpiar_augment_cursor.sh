#!/bin/bash

# Script de limpieza profunda de Augment en Cursor
# EJECUTAR SOLO DESPUÉS DE CERRAR CURSOR COMPLETAMENTE

echo "🗑️  LIMPIEZA PROFUNDA DE AUGMENT EN CURSOR"
echo "=========================================="
echo ""

# Verificar si Cursor está corriendo
if pgrep -x "cursor" > /dev/null; then
    echo "⚠️  ADVERTENCIA: Cursor está en ejecución!"
    echo "Por favor cierra Cursor completamente antes de ejecutar este script."
    echo ""
    read -p "¿Deseas continuar de todas formas? (s/N): " respuesta
    if [[ ! "$respuesta" =~ ^[Ss]$ ]]; then
        echo "❌ Operación cancelada."
        exit 1
    fi
fi

echo "📋 Archivos y directorios a eliminar:"
echo ""

# 1. Extensión
if [ -d "$HOME/.cursor/extensions/augment.vscode-augment-0.441.1" ]; then
    echo "✓ Extensión: ~/.cursor/extensions/augment.vscode-augment-0.441.1"
else
    echo "✓ Extensión: (ya eliminada)"
fi

# 2. Datos globales
if [ -d "$HOME/.config/Cursor/User/globalStorage/augment.vscode-augment" ]; then
    echo "✓ Datos globales: ~/.config/Cursor/User/globalStorage/augment.vscode-augment"
fi

# 3. Datos de workspace
WORKSPACES=$(find "$HOME/.config/Cursor/User/workspaceStorage" -type d -iname "*augment*" 2>/dev/null | wc -l)
if [ $WORKSPACES -gt 0 ]; then
    echo "✓ Datos de workspace: $WORKSPACES ubicaciones encontradas"
fi

# 4. Logs
LOGS=$(find "$HOME/.config/Cursor/logs" -type d -iname "*augment*" 2>/dev/null | wc -l)
if [ $LOGS -gt 0 ]; then
    echo "✓ Logs: $LOGS ubicaciones encontradas"
fi

# 5. Configuración en settings.json
if grep -q "augment" "$HOME/.config/Cursor/User/settings.json" 2>/dev/null; then
    echo "✓ Configuración en settings.json"
fi

echo ""
read -p "¿Proceder con la eliminación? (s/N): " confirmar

if [[ ! "$confirmar" =~ ^[Ss]$ ]]; then
    echo "❌ Operación cancelada."
    exit 1
fi

echo ""
echo "🔄 Iniciando limpieza..."
echo ""

# Eliminar extensión (por si quedó algo)
if [ -d "$HOME/.cursor/extensions/augment.vscode-augment-0.441.1" ]; then
    rm -rf "$HOME/.cursor/extensions/augment.vscode-augment-0.441.1"
    echo "✅ Extensión eliminada"
fi

# Eliminar datos globales
if [ -d "$HOME/.config/Cursor/User/globalStorage/augment.vscode-augment" ]; then
    rm -rf "$HOME/.config/Cursor/User/globalStorage/augment.vscode-augment"
    echo "✅ Datos globales eliminados"
fi

# Eliminar datos de workspace
find "$HOME/.config/Cursor/User/workspaceStorage" -type d -iname "*augment*" -exec rm -rf {} + 2>/dev/null
echo "✅ Datos de workspace eliminados"

# Eliminar logs
find "$HOME/.config/Cursor/logs" -type d -iname "*augment*" -exec rm -rf {} + 2>/dev/null
echo "✅ Logs eliminados"

# Crear backup de settings.json antes de modificarlo
if [ -f "$HOME/.config/Cursor/User/settings.json" ]; then
    cp "$HOME/.config/Cursor/User/settings.json" "$HOME/.config/Cursor/User/settings.json.backup_$(date +%Y%m%d_%H%M%S)"
    echo "✅ Backup de settings.json creado"
    
    # Eliminar configuraciones de Augment del settings.json
    if grep -q "augment" "$HOME/.config/Cursor/User/settings.json"; then
        # Eliminar líneas que contienen "augment"
        sed -i '/augment/d' "$HOME/.config/Cursor/User/settings.json"
        echo "✅ Configuración eliminada de settings.json"
    fi
fi

echo ""
echo "🎉 LIMPIEZA COMPLETADA"
echo "====================="
echo ""
echo "📊 Resumen:"
echo "  ✅ Extensión eliminada"
echo "  ✅ Datos globales eliminados"
echo "  ✅ Datos de workspace eliminados"
echo "  ✅ Logs eliminados"
echo "  ✅ Configuración eliminada"
echo ""
echo "💾 Backup creado en: ~/.config/Cursor/User/settings.json.backup_*"
echo ""
echo "✨ Ahora puedes reinstalar Augment limpiamente"
echo ""






