#!/bin/bash

# Script para limpiar la caché de Visual Studio Code en Manjaro XFCE
# Autor: Alvaretto
# Fecha: $(date +%Y-%m-%d)

# Colores para output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Función para mostrar mensajes con colores
print_message() {
    local color=$1
    local message=$2
    echo -e "${color}${message}${NC}"
}

# Función para mostrar el banner
show_banner() {
    print_message $BLUE "=================================================="
    print_message $BLUE "    LIMPIADOR DE CACHÉ DE VISUAL STUDIO CODE"
    print_message $BLUE "              Para Manjaro XFCE"
    print_message $BLUE "=================================================="
    echo
}

# Función para verificar si VSC está ejecutándose
check_vscode_running() {
    if pgrep -x "code" > /dev/null; then
        print_message $YELLOW "⚠️  Visual Studio Code está ejecutándose."
        print_message $YELLOW "   Se recomienda cerrarlo antes de limpiar la caché."
        echo
        read -p "¿Deseas continuar de todas formas? (y/N): " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            print_message $RED "❌ Operación cancelada por el usuario."
            exit 1
        fi
    fi
}

# Función para calcular el tamaño de un directorio
get_dir_size() {
    local dir=$1
    if [ -d "$dir" ]; then
        du -sh "$dir" 2>/dev/null | cut -f1
    else
        echo "0B"
    fi
}

# Función para limpiar un directorio
clean_directory() {
    local dir=$1
    local description=$2
    
    if [ -d "$dir" ]; then
        local size_before=$(get_dir_size "$dir")
        print_message $YELLOW "🧹 Limpiando $description..."
        print_message $BLUE "   Ubicación: $dir"
        print_message $BLUE "   Tamaño antes: $size_before"
        
        rm -rf "$dir"/* 2>/dev/null
        
        local size_after=$(get_dir_size "$dir")
        print_message $GREEN "   ✅ Limpiado. Tamaño después: $size_after"
        echo
    else
        print_message $YELLOW "⚠️  $description no encontrado en: $dir"
        echo
    fi
}

# Función para limpiar archivos específicos
clean_files() {
    local pattern=$1
    local description=$2
    
    print_message $YELLOW "🧹 Limpiando $description..."
    
    local count=0
    for file in $pattern; do
        if [ -f "$file" ]; then
            rm -f "$file" 2>/dev/null
            ((count++))
        fi
    done
    
    if [ $count -gt 0 ]; then
        print_message $GREEN "   ✅ $count archivo(s) eliminado(s)"
    else
        print_message $YELLOW "   ⚠️  No se encontraron archivos para eliminar"
    fi
    echo
}

# Función principal de limpieza
clean_vscode_cache() {
    print_message $GREEN "🚀 Iniciando limpieza de caché de VSC..."
    echo

    # Detectar tipo de instalación de VSC
    local is_snap=false
    if [ -d "$HOME/snap/code" ]; then
        is_snap=true
        print_message $BLUE "📦 Detectada instalación Snap de VSC"
    else
        print_message $BLUE "📁 Detectada instalación tradicional de VSC"
    fi
    echo

    # Directorios de caché de VSC - Instalación tradicional
    local vscode_config="$HOME/.config/Code"
    local vscode_cache="$HOME/.cache/vscode"
    local vscode_logs="$HOME/.config/Code/logs"
    local vscode_crashdumps="$HOME/.config/Code/CachedData"
    local vscode_workspaceStorage="$HOME/.config/Code/User/workspaceStorage"
    local vscode_globalStorage="$HOME/.config/Code/User/globalStorage"

    # Directorios de caché de VSC - Instalación Snap
    local snap_vscode_base="$HOME/snap/code"
    local snap_vscode_common="$HOME/snap/code/common"
    local snap_vscode_current="$HOME/snap/code/current"
    
    # Limpiar caché principal (tradicional)
    clean_directory "$vscode_cache" "Caché principal de VSC (tradicional)"

    # Limpiar logs (tradicional)
    clean_directory "$vscode_logs" "Logs de VSC (tradicional)"

    # Limpiar datos en caché (tradicional)
    clean_directory "$vscode_crashdumps" "Datos en caché y crash dumps (tradicional)"

    # Limpiar caché de Snap si existe
    if [ "$is_snap" = true ]; then
        print_message $YELLOW "🔧 Limpiando caché específica de Snap..."

        # Limpiar caché común de Snap
        clean_directory "$snap_vscode_common/.cache" "Caché común de Snap VSC"

        # Limpiar logs de versiones específicas
        for version_dir in "$snap_vscode_base"/*/; do
            if [ -d "$version_dir" ] && [[ $(basename "$version_dir") =~ ^[0-9]+$ ]]; then
                local version=$(basename "$version_dir")
                print_message $BLUE "   Limpiando versión Snap: $version"
                clean_directory "$version_dir/.cache" "Caché de versión Snap $version"
                clean_directory "$version_dir/.config" "Configuración de versión Snap $version"
                clean_directory "$version_dir/.local/share" "Datos locales de versión Snap $version"
            fi
        done

        # Limpiar archivos temporales específicos de Snap
        clean_files "$snap_vscode_common/.cache/*" "Archivos de caché común de Snap"
        clean_files "$snap_vscode_current/.cache/*" "Archivos de caché actual de Snap"
    fi
    
    # Limpiar almacenamiento de workspace (opcional)
    if [ -d "$vscode_workspaceStorage" ]; then
        print_message $YELLOW "📁 Encontrado almacenamiento de workspace"
        read -p "¿Deseas limpiar el almacenamiento de workspace? Esto puede eliminar configuraciones específicas de proyectos (y/N): " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            clean_directory "$vscode_workspaceStorage" "Almacenamiento de workspace"
        else
            print_message $BLUE "   ⏭️  Omitiendo almacenamiento de workspace"
            echo
        fi
    fi
    
    # Limpiar almacenamiento global (muy opcional)
    if [ -d "$vscode_globalStorage" ]; then
        print_message $YELLOW "🌐 Encontrado almacenamiento global"
        read -p "¿Deseas limpiar el almacenamiento global? Esto puede eliminar configuraciones de extensiones (y/N): " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            clean_directory "$vscode_globalStorage" "Almacenamiento global"
        else
            print_message $BLUE "   ⏭️  Omitiendo almacenamiento global"
            echo
        fi
    fi
    
    # Limpiar archivos temporales (tradicional)
    clean_files "$HOME/.config/Code/User/History-*" "Archivos de historial (tradicional)"
    clean_files "$HOME/.config/Code/User/*.log" "Archivos de log de usuario (tradicional)"
    clean_files "/tmp/vscode-*" "Archivos temporales de VSC"
    clean_files "/tmp/snap.code*" "Archivos temporales de Snap VSC"
    
    # Limpiar caché de extensiones (opcional)
    local extensions_cache="$HOME/.vscode/extensions"
    local snap_extensions_cache="$snap_vscode_common/.vscode/extensions"

    # Verificar extensiones tradicionales
    if [ -d "$extensions_cache" ]; then
        print_message $YELLOW "🔌 Encontrada caché de extensiones (tradicional)"
        read -p "¿Deseas limpiar la caché de extensiones tradicionales? (y/N): " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            find "$extensions_cache" -name "*.log" -delete 2>/dev/null
            find "$extensions_cache" -name "*.cache" -delete 2>/dev/null
            find "$extensions_cache" -name "node_modules" -type d -exec rm -rf {} + 2>/dev/null
            print_message $GREEN "   ✅ Caché de extensiones tradicionales limpiada"
            echo
        else
            print_message $BLUE "   ⏭️  Omitiendo caché de extensiones tradicionales"
            echo
        fi
    fi

    # Verificar extensiones de Snap
    if [ "$is_snap" = true ] && [ -d "$snap_extensions_cache" ]; then
        print_message $YELLOW "🔌 Encontrada caché de extensiones (Snap)"
        read -p "¿Deseas limpiar la caché de extensiones de Snap? (y/N): " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            find "$snap_extensions_cache" -name "*.log" -delete 2>/dev/null
            find "$snap_extensions_cache" -name "*.cache" -delete 2>/dev/null
            find "$snap_extensions_cache" -name "node_modules" -type d -exec rm -rf {} + 2>/dev/null
            print_message $GREEN "   ✅ Caché de extensiones de Snap limpiada"
            echo
        else
            print_message $BLUE "   ⏭️  Omitiendo caché de extensiones de Snap"
            echo
        fi
    fi
}

# Función para mostrar estadísticas finales
show_final_stats() {
    print_message $GREEN "📊 ESTADÍSTICAS FINALES:"
    echo

    local vscode_config_size=$(get_dir_size "$HOME/.config/Code")
    local vscode_cache_size=$(get_dir_size "$HOME/.cache/vscode")

    print_message $BLUE "   Tamaño actual de configuración VSC (tradicional): $vscode_config_size"
    print_message $BLUE "   Tamaño actual de caché VSC (tradicional): $vscode_cache_size"

    # Mostrar estadísticas de Snap si existe
    if [ -d "$HOME/snap/code" ]; then
        local snap_size=$(get_dir_size "$HOME/snap/code")
        print_message $BLUE "   Tamaño actual de VSC Snap: $snap_size"
    fi

    echo
    print_message $GREEN "✨ ¡Limpieza completada exitosamente!"
    print_message $YELLOW "💡 Recomendación: Reinicia Visual Studio Code para aplicar los cambios."
}

# Función principal
main() {
    show_banner
    
    # Verificar si el usuario quiere continuar
    print_message $YELLOW "Este script limpiará la caché y archivos temporales de Visual Studio Code."
    print_message $YELLOW "Algunas configuraciones específicas de proyectos podrían perderse."
    echo
    read -p "¿Deseas continuar? (y/N): " -n 1 -r
    echo
    echo
    
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        print_message $RED "❌ Operación cancelada por el usuario."
        exit 1
    fi
    
    # Verificar si VSC está ejecutándose
    check_vscode_running
    
    # Realizar la limpieza
    clean_vscode_cache
    
    # Mostrar estadísticas finales
    show_final_stats
}

# Verificar si el script se está ejecutando directamente
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
