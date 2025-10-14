#!/bin/bash

################################################################################
# Script de Instalación de Zoom para Manjaro Linux
# Versión: 1.0
# Fecha: 2025-10-12
# Descripción: Instala Zoom usando el método más seguro y recomendado
################################################################################

set -e  # Salir si hay algún error

# Colores para output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Función para imprimir mensajes
print_info() {
    echo -e "${BLUE}ℹ️  $1${NC}"
}

print_success() {
    echo -e "${GREEN}✅ $1${NC}"
}

print_warning() {
    echo -e "${YELLOW}⚠️  $1${NC}"
}

print_error() {
    echo -e "${RED}❌ $1${NC}"
}

print_header() {
    echo -e "${CYAN}"
    echo "╔════════════════════════════════════════════════════════════════╗"
    echo "║        Instalador de Zoom para Manjaro Linux                  ║"
    echo "║        Versión 1.0 - Octubre 2025                             ║"
    echo "╚════════════════════════════════════════════════════════════════╝"
    echo -e "${NC}"
}

# Función para verificar si un comando existe
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Función para verificar requisitos del sistema
check_system_requirements() {
    print_info "Verificando requisitos del sistema..."
    
    # Verificar que estamos en Manjaro/Arch
    if [ ! -f /etc/os-release ]; then
        print_error "No se puede determinar el sistema operativo"
        exit 1
    fi
    
    . /etc/os-release
    if [[ "$ID" != "manjaro" && "$ID_LIKE" != *"arch"* ]]; then
        print_warning "Este script está diseñado para Manjaro Linux"
        read -p "¿Deseas continuar de todos modos? (s/N): " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Ss]$ ]]; then
            exit 1
        fi
    fi
    
    print_success "Sistema operativo compatible detectado: $NAME"
}

# Función para instalar Flatpak si no está instalado
install_flatpak() {
    if ! command_exists flatpak; then
        print_warning "Flatpak no está instalado"
        print_info "Instalando Flatpak..."
        sudo pacman -S --noconfirm flatpak
        print_success "Flatpak instalado correctamente"
    else
        print_success "Flatpak ya está instalado"
    fi
}

# Función para configurar Flathub
setup_flathub() {
    print_info "Verificando configuración de Flathub..."
    
    if ! flatpak remotes | grep -q "flathub"; then
        print_info "Agregando repositorio Flathub..."
        flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
        print_success "Flathub configurado correctamente"
    else
        print_success "Flathub ya está configurado"
    fi
}

# Función para instalar Zoom via Flatpak
install_zoom_flatpak() {
    print_info "Instalando Zoom via Flatpak..."
    print_warning "Esto descargará aproximadamente 5.2 MB y ocupará 11.2 MB instalado"
    
    # Verificar si Zoom ya está instalado
    if flatpak list | grep -q "us.zoom.Zoom"; then
        print_warning "Zoom ya está instalado"
        read -p "¿Deseas reinstalar? (s/N): " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Ss]$ ]]; then
            print_info "Desinstalando versión anterior..."
            flatpak uninstall -y us.zoom.Zoom
        else
            print_info "Manteniendo instalación actual"
            return 0
        fi
    fi
    
    # Instalar Zoom
    flatpak install -y flathub us.zoom.Zoom
    print_success "Zoom instalado correctamente via Flatpak"
}

# Función para instalar Zoom via AUR
install_zoom_aur() {
    print_info "Instalando Zoom via AUR..."
    
    # Verificar si yay está instalado
    if ! command_exists yay; then
        print_error "yay no está instalado. Instálalo primero o usa el método Flatpak"
        exit 1
    fi
    
    # Verificar si Zoom ya está instalado
    if pacman -Q zoom >/dev/null 2>&1; then
        print_warning "Zoom ya está instalado"
        read -p "¿Deseas reinstalar? (s/N): " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Ss]$ ]]; then
            print_info "Manteniendo instalación actual"
            return 0
        fi
    fi
    
    # Instalar Zoom
    yay -S --noconfirm zoom
    print_success "Zoom instalado correctamente via AUR"
}

# Función para verificar la instalación
verify_installation() {
    print_info "Verificando instalación..."
    
    if [ "$INSTALL_METHOD" = "flatpak" ]; then
        if flatpak list | grep -q "us.zoom.Zoom"; then
            print_success "Zoom está instalado correctamente"
            ZOOM_VERSION=$(flatpak info us.zoom.Zoom | grep "Versión" | awk '{print $2}')
            print_info "Versión instalada: $ZOOM_VERSION"
            return 0
        fi
    elif [ "$INSTALL_METHOD" = "aur" ]; then
        if pacman -Q zoom >/dev/null 2>&1; then
            print_success "Zoom está instalado correctamente"
            ZOOM_VERSION=$(pacman -Q zoom | awk '{print $2}')
            print_info "Versión instalada: $ZOOM_VERSION"
            return 0
        fi
    fi
    
    print_error "No se pudo verificar la instalación de Zoom"
    return 1
}

# Función para configurar permisos adicionales (Flatpak)
configure_flatpak_permissions() {
    if [ "$INSTALL_METHOD" = "flatpak" ]; then
        print_info "Configurando permisos adicionales para Flatpak..."
        
        # Otorgar acceso a dispositivos (cámara, micrófono)
        flatpak override us.zoom.Zoom --device=all
        
        # Instalar portal de escritorio para KDE si no está instalado
        if ! pacman -Q xdg-desktop-portal-kde >/dev/null 2>&1; then
            print_info "Instalando xdg-desktop-portal-kde para mejor integración..."
            sudo pacman -S --noconfirm xdg-desktop-portal-kde
        fi
        
        print_success "Permisos configurados correctamente"
    fi
}

# Función para mostrar instrucciones de uso
show_usage_instructions() {
    echo
    print_header
    print_success "¡Instalación completada exitosamente!"
    echo
    print_info "Cómo ejecutar Zoom:"
    echo
    
    if [ "$INSTALL_METHOD" = "flatpak" ]; then
        echo -e "  ${GREEN}1.${NC} Desde el menú de aplicaciones: Busca 'Zoom'"
        echo -e "  ${GREEN}2.${NC} Desde la terminal: ${CYAN}flatpak run us.zoom.Zoom${NC}"
        echo -e "  ${GREEN}3.${NC} Crear alias (opcional):"
        echo -e "     ${CYAN}echo 'alias zoom=\"flatpak run us.zoom.Zoom\"' >> ~/.bashrc${NC}"
        echo -e "     ${CYAN}source ~/.bashrc${NC}"
        echo
        print_info "Actualizar Zoom: ${CYAN}flatpak update us.zoom.Zoom${NC}"
        print_info "Desinstalar Zoom: ${CYAN}flatpak uninstall us.zoom.Zoom${NC}"
    elif [ "$INSTALL_METHOD" = "aur" ]; then
        echo -e "  ${GREEN}1.${NC} Desde el menú de aplicaciones: Busca 'Zoom'"
        echo -e "  ${GREEN}2.${NC} Desde la terminal: ${CYAN}zoom${NC}"
        echo
        print_info "Actualizar Zoom: ${CYAN}yay -Syu zoom${NC}"
        print_info "Desinstalar Zoom: ${CYAN}yay -Rns zoom${NC}"
    fi
    
    echo
    print_info "Checklist de verificación:"
    echo "  [ ] Abrir Zoom e iniciar sesión"
    echo "  [ ] Probar cámara (Configuración → Video)"
    echo "  [ ] Probar micrófono (Configuración → Audio)"
    echo "  [ ] Probar altavoces (Configuración → Audio)"
    echo "  [ ] Probar compartir pantalla (en una reunión de prueba)"
    echo
    print_info "Para más información, consulta: RDP-Manjaro.md"
    echo
}

# Función principal
main() {
    print_header
    
    # Verificar requisitos del sistema
    check_system_requirements
    
    echo
    print_info "Selecciona el método de instalación:"
    echo
    echo "  1) Flatpak (Recomendado - Más seguro y estable)"
    echo "  2) AUR (Alternativa - Mejor rendimiento)"
    echo "  3) Cancelar"
    echo
    read -p "Selecciona una opción (1-3): " -n 1 -r
    echo
    
    case $REPLY in
        1)
            INSTALL_METHOD="flatpak"
            print_info "Has seleccionado: Flatpak (Recomendado)"
            echo
            install_flatpak
            setup_flathub
            install_zoom_flatpak
            configure_flatpak_permissions
            ;;
        2)
            INSTALL_METHOD="aur"
            print_info "Has seleccionado: AUR"
            echo
            install_zoom_aur
            ;;
        3)
            print_info "Instalación cancelada"
            exit 0
            ;;
        *)
            print_error "Opción inválida"
            exit 1
            ;;
    esac
    
    # Verificar instalación
    if verify_installation; then
        show_usage_instructions
    else
        print_error "La instalación falló. Por favor, revisa los errores anteriores."
        exit 1
    fi
}

# Ejecutar script principal
main

