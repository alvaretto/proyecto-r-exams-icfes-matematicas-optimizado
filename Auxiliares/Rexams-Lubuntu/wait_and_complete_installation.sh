#!/bin/bash

# Script para esperar que terminen las instalaciones actuales y luego completar la configuración
# Para el proyecto R-exams ICFES

echo "🎯 Esperando instalaciones y completando configuración R-exams ICFES"
echo "=================================================================="

# Colores para output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

show_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

show_success() {
    echo -e "${GREEN}[✓]${NC} $1"
}

show_warning() {
    echo -e "${YELLOW}[⚠]${NC} $1"
}

show_error() {
    echo -e "${RED}[✗]${NC} $1"
}

# Función para esperar que terminen las instalaciones
wait_for_installations() {
    show_info "Esperando que terminen las instalaciones actuales..."
    
    while true; do
        apt_processes=$(ps aux | grep -E "(apt|dpkg)" | grep -v grep | wc -l)
        
        if [ $apt_processes -eq 0 ]; then
            show_success "Todas las instalaciones han terminado"
            break
        else
            echo -ne "\r${YELLOW}[⏳]${NC} Esperando... ($apt_processes procesos activos)    "
            sleep 5
        fi
    done
    echo ""
}

# Función para verificar instalaciones básicas
verify_basic_installations() {
    show_info "Verificando instalaciones básicas..."
    
    local all_ok=true
    
    # Verificar R
    if command -v R &> /dev/null; then
        R_VERSION=$(R --version | head -n1)
        show_success "R: $R_VERSION"
    else
        show_error "R no está instalado"
        all_ok=false
    fi
    
    # Verificar LaTeX
    if command -v pdflatex &> /dev/null; then
        show_success "LaTeX: Instalado"
    else
        show_error "LaTeX no está instalado"
        all_ok=false
    fi
    
    # Verificar Pandoc
    if command -v pandoc &> /dev/null; then
        show_success "Pandoc: Instalado"
    else
        show_error "Pandoc no está instalado"
        all_ok=false
    fi
    
    return $all_ok
}

# Función principal
main() {
    # 1. Esperar instalaciones
    wait_for_installations
    
    # 2. Verificar que todo se instaló correctamente
    if verify_basic_installations; then
        show_success "Instalaciones básicas completadas correctamente"
        
        # 3. Ejecutar script de instalación completa
        show_info "Ejecutando instalación completa de R-exams..."
        SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
        if [ -f "$SCRIPT_DIR/install_r_exams_lubuntu.sh" ]; then
            cd "$SCRIPT_DIR"
            ./install_r_exams_lubuntu.sh
        else
            show_error "No se encontró install_r_exams_lubuntu.sh en $SCRIPT_DIR"
            show_info "Ejecuta manualmente desde la carpeta Rexams-Lubuntu: ./install_r_exams_lubuntu.sh"
        fi
        
    else
        show_error "Algunas instalaciones básicas fallaron"
        show_info "Revisa los errores y ejecuta manualmente:"
        show_info "  sudo apt install -y r-base r-base-dev texlive-full pandoc"
        show_info "  ./install_r_exams_lubuntu.sh"
    fi
    
    # 4. Mostrar siguiente paso
    echo ""
    echo "📋 PRÓXIMOS PASOS:"
    echo "=================="
    echo "1. 📁 Navega a tu proyecto:"
    echo "   cd /home/rexams/Proyectos/proyecto-r-exams-icfes-matematicas-optimizado"
    echo ""
    echo "2. 🧪 Prueba un ejercicio simple:"
    echo "   R"
    echo "   library(exams)"
    echo "   # Buscar un archivo .Rmd en el proyecto y probarlo"
    echo ""
    echo "3. 📚 Consulta la documentación:"
    echo "   cat README.md"
    echo "   cat Auxiliares/Rexams-Lubuntu/README.md"
    echo ""
    echo "4. 🎨 Usa las metodologías avanzadas:"
    echo "   - Metodología TikZ para replicar imágenes PNG"
    echo "   - Metodología de corrección de errores recurrentes"
    echo ""
    show_success "¡Tu entorno R-exams ICFES estará listo!"
}

# Ejecutar función principal
main "$@"