#!/bin/bash

################################################################################
# Script de Inicio Rápido - R-Exams ICFES
# Proyecto: RepositorioMatematicasICFES_R_Exams
# Descripción: Guía interactiva para comenzar a trabajar después de la instalación
################################################################################

# Colores
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

clear
echo -e "${CYAN}╔════════════════════════════════════════════════════════════════╗${NC}"
echo -e "${CYAN}║         INICIO RÁPIDO - R-EXAMS ICFES MANJARO                 ║${NC}"
echo -e "${CYAN}╚════════════════════════════════════════════════════════════════╝${NC}"
echo ""

PROJECT_DIR="/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams"

# Verificar que estamos en el directorio correcto
if [ ! -d "$PROJECT_DIR" ]; then
    echo -e "${YELLOW}⚠️ Directorio del proyecto no encontrado: $PROJECT_DIR${NC}"
    exit 1
fi

cd "$PROJECT_DIR"

# Función para mostrar menú
show_menu() {
    echo ""
    echo -e "${BLUE}═══════════════════════════════════════════════════════════════${NC}"
    echo -e "${GREEN}¿Qué deseas hacer?${NC}"
    echo -e "${BLUE}═══════════════════════════════════════════════════════════════${NC}"
    echo ""
    echo "  1) 🔧 Instalar entorno completo"
    echo "  2) ✅ Verificar instalación"
    echo "  3) 🧪 Ejecutar pruebas de funcionalidad"
    echo "  4) 🚀 Abrir RStudio con el proyecto"
    echo "  5) 📚 Ver ejemplos funcionales"
    echo "  6) 📖 Abrir documentación"
    echo "  7) 🔄 Actualizar paquetes R"
    echo "  8) 🐍 Verificar Python-R (reticulate)"
    echo "  9) 📊 Ver estado del sistema"
    echo "  0) ❌ Salir"
    echo ""
    echo -e "${BLUE}═══════════════════════════════════════════════════════════════${NC}"
    echo -n "Selecciona una opción [0-9]: "
}

# Función para pausar
pause() {
    echo ""
    read -p "Presiona ENTER para continuar..."
}

# Opción 1: Instalar entorno completo
install_environment() {
    echo ""
    echo -e "${GREEN}🔧 INSTALACIÓN DEL ENTORNO COMPLETO${NC}"
    echo ""
    echo "Este proceso instalará:"
    echo "  • R (versión más reciente)"
    echo "  • RStudio Desktop"
    echo "  • LaTeX (TeX Live completo)"
    echo "  • Python 3 con paquetes"
    echo "  • Todos los paquetes R necesarios"
    echo "  • Integración Python-R"
    echo ""
    echo "Tiempo estimado: 20-40 minutos"
    echo ""
    read -p "¿Deseas continuar? (s/n): " confirm
    
    if [ "$confirm" = "s" ] || [ "$confirm" = "S" ]; then
        chmod +x Auxiliares/Instalaciones/install_manjaro_r_exams_complete.sh
        ./Auxiliares/Instalaciones/install_manjaro_r_exams_complete.sh
    else
        echo "Instalación cancelada"
    fi
    pause
}

# Opción 2: Verificar instalación
verify_installation() {
    echo ""
    echo -e "${GREEN}✅ VERIFICACIÓN DE INSTALACIÓN${NC}"
    echo ""
    chmod +x Auxiliares/Instalaciones/verify_installation.sh
    ./Auxiliares/Instalaciones/verify_installation.sh
    pause
}

# Opción 3: Ejecutar pruebas
run_tests() {
    echo ""
    echo -e "${GREEN}🧪 PRUEBAS DE FUNCIONALIDAD${NC}"
    echo ""
    chmod +x Auxiliares/Instalaciones/test_rexams_functionality.R
    R --vanilla < Auxiliares/Instalaciones/test_rexams_functionality.R
    pause
}

# Opción 4: Abrir RStudio
open_rstudio() {
    echo ""
    echo -e "${GREEN}🚀 ABRIENDO RSTUDIO${NC}"
    echo ""
    
    if command -v rstudio &> /dev/null; then
        echo "Abriendo RStudio con el proyecto..."
        rstudio RepositorioMatematicasICFES_R_Exams.Rproj &
        echo "✅ RStudio abierto en segundo plano"
    else
        echo "❌ RStudio no está instalado"
        echo "Instálalo con: yay -S rstudio-desktop-bin"
    fi
    pause
}

# Opción 5: Ver ejemplos funcionales
view_examples() {
    echo ""
    echo -e "${GREEN}📚 EJEMPLOS FUNCIONALES${NC}"
    echo ""
    
    EXAMPLES_DIR="Auxiliares/Ejemplos-Funcionales-Rmd"
    
    if [ -d "$EXAMPLES_DIR" ]; then
        echo "Archivos .Rmd disponibles:"
        echo ""
        find "$EXAMPLES_DIR" -name "*.Rmd" -type f | head -20
        echo ""
        echo "Para compilar un ejemplo en R:"
        echo ""
        echo "  library(exams)"
        echo "  setwd('$EXAMPLES_DIR')"
        echo "  exams2html('nombre_archivo.Rmd', n = 1)"
        echo ""
    else
        echo "❌ Directorio de ejemplos no encontrado"
    fi
    pause
}

# Opción 6: Abrir documentación
open_documentation() {
    echo ""
    echo -e "${GREEN}📖 DOCUMENTACIÓN DISPONIBLE${NC}"
    echo ""
    echo "Documentación del proyecto:"
    echo ""
    echo "  1. README.md - Descripción general"
    echo "  2. INSTALACION_MANJARO_RESUMEN.md - Resumen de instalación"
    echo "  3. Auxiliares/Instalaciones/INSTRUCCIONES_INSTALACION_MANJARO.md"
    echo "  4. Auxiliares/Instalaciones/README_INSTALACION_MANJARO.md"
    echo "  5. .augment/rules/reglas-generales.md - Reglas del proyecto"
    echo ""
    echo "Documentación externa:"
    echo ""
    echo "  • R-exams: http://www.r-exams.org/"
    echo "  • RStudio: https://docs.posit.co/"
    echo "  • Reticulate: https://rstudio.github.io/reticulate/"
    echo "  • TikZ: https://tikz.dev/"
    echo ""
    read -p "¿Abrir README.md? (s/n): " confirm
    
    if [ "$confirm" = "s" ] || [ "$confirm" = "S" ]; then
        if command -v kate &> /dev/null; then
            kate README.md &
        elif command -v gedit &> /dev/null; then
            gedit README.md &
        else
            cat README.md | less
        fi
    fi
    pause
}

# Opción 7: Actualizar paquetes R
update_r_packages() {
    echo ""
    echo -e "${GREEN}🔄 ACTUALIZACIÓN DE PAQUETES R${NC}"
    echo ""
    echo "Actualizando paquetes R..."
    
    R --vanilla << 'RSCRIPT'
cat("Actualizando paquetes R...\n")
update.packages(ask = FALSE, checkBuilt = TRUE)
cat("\n✅ Paquetes R actualizados\n")
RSCRIPT
    
    pause
}

# Opción 8: Verificar Python-R
verify_python_r() {
    echo ""
    echo -e "${GREEN}🐍 VERIFICACIÓN PYTHON-R (RETICULATE)${NC}"
    echo ""
    
    R --vanilla << 'RSCRIPT'
library(reticulate)

cat("═══════════════════════════════════════════════════════════════\n")
cat("CONFIGURACIÓN PYTHON-R\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# Configurar Python
python3_path <- Sys.which("python3")

if (python3_path != "") {
  use_python(python3_path, required = FALSE)
  
  # Mostrar configuración
  py_config_info <- py_config()
  cat("Python:", py_config_info$python, "\n")
  cat("Versión:", py_config_info$version, "\n\n")
  
  # Probar funcionalidad
  cat("Probando ejecución de código Python...\n")
  py_run_string("print('✅ Python funcional desde R')")
  
  # Verificar paquetes
  cat("\nVerificando paquetes Python:\n")
  tryCatch({
    py_run_string("import matplotlib; print('  ✅ matplotlib')")
    py_run_string("import numpy; print('  ✅ numpy')")
    py_run_string("import pandas; print('  ✅ pandas')")
  }, error = function(e) {
    cat("  ❌ Error:", e$message, "\n")
  })
  
  cat("\n✅ Integración Python-R funcional\n")
} else {
  cat("❌ Python 3 no encontrado\n")
}
RSCRIPT
    
    pause
}

# Opción 9: Ver estado del sistema
show_system_status() {
    echo ""
    echo -e "${GREEN}📊 ESTADO DEL SISTEMA${NC}"
    echo ""
    
    echo "═══════════════════════════════════════════════════════════════"
    echo "VERSIONES INSTALADAS"
    echo "═══════════════════════════════════════════════════════════════"
    echo ""
    
    # R
    if command -v R &> /dev/null; then
        echo -e "${GREEN}✅ R:${NC} $(R --version | head -n 1)"
    else
        echo -e "${YELLOW}❌ R: NO INSTALADO${NC}"
    fi
    
    # RStudio
    if command -v rstudio &> /dev/null; then
        echo -e "${GREEN}✅ RStudio:${NC} Instalado"
    else
        echo -e "${YELLOW}❌ RStudio: NO INSTALADO${NC}"
    fi
    
    # Python
    if command -v python3 &> /dev/null; then
        echo -e "${GREEN}✅ Python:${NC} $(python3 --version)"
    else
        echo -e "${YELLOW}❌ Python: NO INSTALADO${NC}"
    fi
    
    # LaTeX
    if command -v pdflatex &> /dev/null; then
        echo -e "${GREEN}✅ LaTeX:${NC} $(pdflatex --version | head -n 1)"
    else
        echo -e "${YELLOW}❌ LaTeX: NO INSTALADO${NC}"
    fi
    
    # Git
    if command -v git &> /dev/null; then
        echo -e "${GREEN}✅ Git:${NC} $(git --version)"
    else
        echo -e "${YELLOW}❌ Git: NO INSTALADO${NC}"
    fi
    
    echo ""
    echo "═══════════════════════════════════════════════════════════════"
    echo "ESPACIO EN DISCO"
    echo "═══════════════════════════════════════════════════════════════"
    echo ""
    df -h "$PROJECT_DIR" | tail -n 1
    
    echo ""
    echo "═══════════════════════════════════════════════════════════════"
    echo "PAQUETES R CRÍTICOS"
    echo "═══════════════════════════════════════════════════════════════"
    echo ""
    
    R --vanilla --slave << 'RSCRIPT'
packages <- c("exams", "knitr", "rmarkdown", "reticulate", "ggplot2", "tidyverse")

for (pkg in packages) {
  if (require(pkg, character.only = TRUE, quietly = TRUE)) {
    version <- packageVersion(pkg)
    cat(sprintf("  ✅ %-15s %s\n", pkg, version))
  } else {
    cat(sprintf("  ❌ %-15s NO INSTALADO\n", pkg))
  }
}
RSCRIPT
    
    pause
}

# Bucle principal del menú
while true; do
    show_menu
    read option
    
    case $option in
        1) install_environment ;;
        2) verify_installation ;;
        3) run_tests ;;
        4) open_rstudio ;;
        5) view_examples ;;
        6) open_documentation ;;
        7) update_r_packages ;;
        8) verify_python_r ;;
        9) show_system_status ;;
        0) 
            echo ""
            echo -e "${GREEN}¡Hasta luego!${NC}"
            echo ""
            exit 0
            ;;
        *)
            echo ""
            echo -e "${YELLOW}Opción inválida. Por favor selecciona 0-9.${NC}"
            pause
            ;;
    esac
done

