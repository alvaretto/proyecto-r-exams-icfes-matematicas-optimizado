#!/bin/bash

# Script para verificar el estado actual de las instalaciones en progreso
# Para el proyecto R-exams ICFES

echo "🔍 Verificando estado actual de instalaciones"
echo "============================================="

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

# 1. Verificar procesos de instalación activos
show_info "Verificando procesos de instalación activos..."
apt_processes=$(ps aux | grep -E "(apt|dpkg)" | grep -v grep | wc -l)
if [ $apt_processes -gt 0 ]; then
    show_warning "Hay $apt_processes procesos de instalación activos:"
    ps aux | grep -E "(apt|dpkg)" | grep -v grep | awk '{print "  - " $11 " " $12 " " $13}'
    echo ""
    show_info "Esperando a que terminen las instalaciones actuales..."
    echo "Puedes monitorear el progreso en las terminales activas."
else
    show_success "No hay procesos de instalación activos"
fi

# 2. Verificar qué está instalado actualmente
show_info "Verificando software instalado..."

# Verificar R
if command -v R &> /dev/null; then
    R_VERSION=$(R --version | head -n1)
    show_success "R está instalado: $R_VERSION"
else
    show_warning "R no está instalado aún"
fi

# Verificar LaTeX
if command -v pdflatex &> /dev/null; then
    LATEX_VERSION=$(pdflatex --version | head -n1)
    show_success "LaTeX está instalado: $LATEX_VERSION"
else
    show_warning "LaTeX no está instalado aún"
fi

# Verificar Pandoc
if command -v pandoc &> /dev/null; then
    PANDOC_VERSION=$(pandoc --version | head -n1)
    show_success "Pandoc está instalado: $PANDOC_VERSION"
else
    show_warning "Pandoc no está instalado aún"
fi

# Verificar Python
if command -v python3 &> /dev/null; then
    PYTHON_VERSION=$(python3 --version)
    show_success "Python está instalado: $PYTHON_VERSION"
else
    show_warning "Python no está instalado"
fi

# 3. Verificar espacio en disco
show_info "Verificando espacio en disco..."
DISK_USAGE=$(df -h / | awk 'NR==2 {print $5}' | sed 's/%//')
AVAILABLE_SPACE=$(df -h / | awk 'NR==2 {print $4}')

if [ $DISK_USAGE -lt 80 ]; then
    show_success "Espacio en disco: ${DISK_USAGE}% usado, ${AVAILABLE_SPACE} disponible"
else
    show_warning "Espacio en disco: ${DISK_USAGE}% usado, ${AVAILABLE_SPACE} disponible"
    show_warning "Considera liberar espacio si es necesario"
fi

# 4. Verificar memoria
show_info "Verificando memoria disponible..."
MEMORY_INFO=$(free -h | awk 'NR==2{printf "Usado: %s/%s (%.2f%%)", $3,$2,$3*100/$2}')
show_info "Memoria: $MEMORY_INFO"

# 5. Verificar conectividad a internet
show_info "Verificando conectividad a internet..."
if ping -c 1 google.com &> /dev/null; then
    show_success "Conectividad a internet: OK"
else
    show_error "Sin conectividad a internet"
fi

# 6. Mostrar recomendaciones
echo ""
echo "📋 RECOMENDACIONES:"
echo "=================="

if [ $apt_processes -gt 0 ]; then
    echo "1. ⏳ Espera a que terminen las instalaciones actuales"
    echo "2. 🔄 Luego ejecuta: ./install_r_exams_lubuntu.sh"
    echo "3. 📁 Navega a tu proyecto: cd /home/rexams/Proyectos/proyecto-r-exams-icfes-matematicas-optimizado"
else
    echo "1. 🚀 Ejecuta: ./install_r_exams_lubuntu.sh"
    echo "2. 📁 Navega a tu proyecto: cd /home/rexams/Proyectos/proyecto-r-exams-icfes-matematicas-optimizado"
    echo "3. 🧪 Prueba con un ejercicio del proyecto"
fi

echo ""
echo "📚 RECURSOS DEL PROYECTO:"
echo "========================"
echo "- README: /home/rexams/Proyectos/proyecto-r-exams-icfes-matematicas-optimizado/README.md"
echo "- Tutorial: /home/rexams/Proyectos/proyecto-r-exams-icfes-matematicas-optimizado/Auxiliares/Instalaciones/"
echo "- Ejemplos: /home/rexams/Proyectos/proyecto-r-exams-icfes-matematicas-optimizado/06-Estadística-Y-Probabilidad/"

# 7. Crear comando de monitoreo
echo ""
show_info "Para monitorear las instalaciones en progreso, usa:"
echo "watch -n 5 'ps aux | grep -E \"(apt|dpkg)\" | grep -v grep'"

echo ""
show_success "Verificación completada"