#!/bin/bash

# Script de ayuda rápida para Chrome DevTools MCP
# Fecha: 2025-10-02

function mostrar_menu() {
    clear
    echo "╔══════════════════════════════════════════════════════════════╗"
    echo "║                                                              ║"
    echo "║          Chrome DevTools MCP - Menú de Ayuda                ║"
    echo "║                                                              ║"
    echo "╚══════════════════════════════════════════════════════════════╝"
    echo ""
    echo "Selecciona una opción:"
    echo ""
    echo "  1) Ver resumen de instalación"
    echo "  2) Verificar estado del sistema"
    echo "  3) Ver configuración actual"
    echo "  4) Ver ejemplos rápidos"
    echo "  5) Abrir documentación completa"
    echo "  6) Reiniciar VS Code Insiders"
    echo "  7) Ver logs del servidor MCP"
    echo "  8) Actualizar servidor MCP"
    echo "  9) Desinstalar servidor MCP"
    echo "  0) Salir"
    echo ""
    echo -n "Opción: "
}

function ver_resumen() {
    clear
    cat RESUMEN_INSTALACION.txt
    echo ""
    echo "Presiona Enter para continuar..."
    read
}

function verificar_sistema() {
    clear
    ./test_mcp.sh
    echo ""
    echo "Presiona Enter para continuar..."
    read
}

function ver_configuracion() {
    clear
    echo "╔══════════════════════════════════════════════════════════════╗"
    echo "║          Configuración Actual de MCP                         ║"
    echo "╚══════════════════════════════════════════════════════════════╝"
    echo ""
    
    MCP_CONFIG="$HOME/.config/Code - Insiders/User/globalStorage/github.copilot-chat/mcp.json"
    
    if [ -f "$MCP_CONFIG" ]; then
        echo "📁 Ubicación: $MCP_CONFIG"
        echo ""
        echo "Contenido:"
        echo "────────────────────────────────────────────────────────────"
        cat "$MCP_CONFIG" | jq '.' 2>/dev/null || cat "$MCP_CONFIG"
        echo "────────────────────────────────────────────────────────────"
    else
        echo "❌ Archivo de configuración no encontrado"
    fi
    
    echo ""
    echo "Presiona Enter para continuar..."
    read
}

function ver_ejemplos() {
    clear
    echo "╔══════════════════════════════════════════════════════════════╗"
    echo "║          Ejemplos Rápidos de Uso                             ║"
    echo "╚══════════════════════════════════════════════════════════════╝"
    echo ""
    echo "Copia y pega estos prompts en el chat de Copilot:"
    echo ""
    echo "📊 Análisis de rendimiento:"
    echo "   Analiza el rendimiento de https://developers.chrome.com"
    echo ""
    echo "📸 Tomar screenshot:"
    echo "   Toma un screenshot de https://github.com"
    echo ""
    echo "🌐 Inspeccionar red:"
    echo "   Abre https://example.com y muéstrame las peticiones de red"
    echo ""
    echo "📱 Emular móvil:"
    echo "   Cambia el viewport a 375x667 y navega a https://example.com"
    echo ""
    echo "🐛 Ejecutar JavaScript:"
    echo "   Navega a https://example.com y ejecuta: document.title"
    echo ""
    echo "💡 Ver más ejemplos en: EJEMPLOS_USO.md"
    echo ""
    echo "Presiona Enter para continuar..."
    read
}

function abrir_documentacion() {
    clear
    echo "╔══════════════════════════════════════════════════════════════╗"
    echo "║          Documentación Disponible                            ║"
    echo "╚══════════════════════════════════════════════════════════════╝"
    echo ""
    echo "Selecciona qué documentación abrir:"
    echo ""
    echo "  1) README.md - Índice principal"
    echo "  2) README_INSTALACION.md - Guía completa"
    echo "  3) EJEMPLOS_USO.md - 25+ ejemplos prácticos"
    echo "  4) AGREGAR_MAS_SERVIDORES.md - Expandir MCP"
    echo "  5) Volver al menú principal"
    echo ""
    echo -n "Opción: "
    read doc_opcion
    
    case $doc_opcion in
        1)
            if command -v code-insiders &> /dev/null; then
                code-insiders README.md
            else
                less README.md
            fi
            ;;
        2)
            if command -v code-insiders &> /dev/null; then
                code-insiders README_INSTALACION.md
            else
                less README_INSTALACION.md
            fi
            ;;
        3)
            if command -v code-insiders &> /dev/null; then
                code-insiders EJEMPLOS_USO.md
            else
                less EJEMPLOS_USO.md
            fi
            ;;
        4)
            if command -v code-insiders &> /dev/null; then
                code-insiders AGREGAR_MAS_SERVIDORES.md
            else
                less AGREGAR_MAS_SERVIDORES.md
            fi
            ;;
        5)
            return
            ;;
        *)
            echo "Opción inválida"
            sleep 2
            ;;
    esac
}

function reiniciar_vscode() {
    clear
    echo "╔══════════════════════════════════════════════════════════════╗"
    echo "║          Reiniciar VS Code Insiders                          ║"
    echo "╚══════════════════════════════════════════════════════════════╝"
    echo ""
    echo "⚠️  Esto cerrará todas las ventanas de VS Code Insiders"
    echo ""
    echo -n "¿Continuar? (s/N): "
    read confirmar
    
    if [ "$confirmar" = "s" ] || [ "$confirmar" = "S" ]; then
        echo ""
        echo "Cerrando VS Code Insiders..."
        killall code-insiders 2>/dev/null
        sleep 2
        echo "Abriendo VS Code Insiders..."
        code-insiders &
        echo "✅ VS Code Insiders reiniciado"
        sleep 2
    else
        echo "Operación cancelada"
        sleep 2
    fi
}

function ver_logs() {
    clear
    echo "╔══════════════════════════════════════════════════════════════╗"
    echo "║          Logs del Servidor MCP                               ║"
    echo "╚══════════════════════════════════════════════════════════════╝"
    echo ""
    
    # Buscar logs en ubicaciones comunes
    LOG_LOCATIONS=(
        "/tmp/chrome-devtools-mcp.log"
        "$HOME/.cache/chrome-devtools-mcp/logs"
        "$HOME/.vscode-insiders/logs"
    )
    
    FOUND=false
    for log_path in "${LOG_LOCATIONS[@]}"; do
        if [ -f "$log_path" ]; then
            echo "📄 Encontrado: $log_path"
            echo ""
            tail -n 50 "$log_path"
            FOUND=true
            break
        elif [ -d "$log_path" ]; then
            echo "📁 Directorio de logs: $log_path"
            ls -lh "$log_path"
            FOUND=true
            break
        fi
    done
    
    if [ "$FOUND" = false ]; then
        echo "ℹ️  No se encontraron logs"
        echo ""
        echo "Para habilitar logs, agrega esto a tu configuración MCP:"
        echo ""
        echo '  "env": {'
        echo '    "DEBUG": "*"'
        echo '  }'
    fi
    
    echo ""
    echo "Presiona Enter para continuar..."
    read
}

function actualizar_servidor() {
    clear
    echo "╔══════════════════════════════════════════════════════════════╗"
    echo "║          Actualizar Servidor MCP                             ║"
    echo "╚══════════════════════════════════════════════════════════════╝"
    echo ""
    echo "El servidor se actualiza automáticamente al usar @latest"
    echo ""
    echo "Forzando actualización manual..."
    echo ""
    
    npx -y chrome-devtools-mcp@latest --help > /dev/null 2>&1
    
    if [ $? -eq 0 ]; then
        VERSION=$(npm view chrome-devtools-mcp version 2>/dev/null)
        echo "✅ Servidor actualizado a la última versión: v$VERSION"
    else
        echo "❌ Error al actualizar el servidor"
    fi
    
    echo ""
    echo "Presiona Enter para continuar..."
    read
}

function desinstalar_servidor() {
    clear
    echo "╔══════════════════════════════════════════════════════════════╗"
    echo "║          Desinstalar Servidor MCP                            ║"
    echo "╚══════════════════════════════════════════════════════════════╝"
    echo ""
    echo "⚠️  ADVERTENCIA: Esto eliminará la configuración del servidor"
    echo ""
    echo -n "¿Estás seguro? (s/N): "
    read confirmar
    
    if [ "$confirmar" = "s" ] || [ "$confirmar" = "S" ]; then
        echo ""
        MCP_CONFIG="$HOME/.config/Code - Insiders/User/globalStorage/github.copilot-chat/mcp.json"
        
        if [ -f "$MCP_CONFIG" ]; then
            # Hacer backup
            cp "$MCP_CONFIG" "$MCP_CONFIG.backup"
            echo "✅ Backup creado: $MCP_CONFIG.backup"
            
            # Eliminar configuración
            rm "$MCP_CONFIG"
            echo "✅ Configuración eliminada"
            
            # Limpiar caché
            CACHE_DIR="$HOME/.cache/chrome-devtools-mcp"
            if [ -d "$CACHE_DIR" ]; then
                rm -rf "$CACHE_DIR"
                echo "✅ Caché eliminado"
            fi
            
            echo ""
            echo "✅ Servidor MCP desinstalado"
            echo "💡 Reinicia VS Code Insiders para aplicar cambios"
        else
            echo "❌ No se encontró configuración para eliminar"
        fi
    else
        echo "Operación cancelada"
    fi
    
    echo ""
    echo "Presiona Enter para continuar..."
    read
}

# Bucle principal
while true; do
    mostrar_menu
    read opcion
    
    case $opcion in
        1) ver_resumen ;;
        2) verificar_sistema ;;
        3) ver_configuracion ;;
        4) ver_ejemplos ;;
        5) abrir_documentacion ;;
        6) reiniciar_vscode ;;
        7) ver_logs ;;
        8) actualizar_servidor ;;
        9) desinstalar_servidor ;;
        0) 
            clear
            echo "¡Hasta luego! 👋"
            exit 0
            ;;
        *)
            echo "Opción inválida"
            sleep 2
            ;;
    esac
done

