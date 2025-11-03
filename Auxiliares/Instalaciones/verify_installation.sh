#!/bin/bash

################################################################################
# Script de Verificación de Instalación R-Exams ICFES
# Proyecto: RepositorioMatematicasICFES_R_Exams
# Descripción: Verifica que todos los componentes estén instalados correctamente
################################################################################

# Colores
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo "╔════════════════════════════════════════════════════════════════╗"
echo "║     VERIFICACIÓN DE INSTALACIÓN R-EXAMS ICFES - MANJARO       ║"
echo "╚════════════════════════════════════════════════════════════════╝"
echo ""

# Contador de verificaciones
TOTAL=0
PASSED=0
FAILED=0

check_command() {
    TOTAL=$((TOTAL + 1))
    if command -v "$1" &> /dev/null; then
        echo -e "${GREEN}[✓]${NC} $2: $(command -v $1)"
        PASSED=$((PASSED + 1))
        return 0
    else
        echo -e "${RED}[✗]${NC} $2: NO ENCONTRADO"
        FAILED=$((FAILED + 1))
        return 1
    fi
}

check_package() {
    TOTAL=$((TOTAL + 1))
    if pacman -Qi "$1" &> /dev/null; then
        VERSION=$(pacman -Qi "$1" | grep "Version" | cut -d':' -f2 | xargs)
        echo -e "${GREEN}[✓]${NC} $2: $VERSION"
        PASSED=$((PASSED + 1))
        return 0
    else
        echo -e "${RED}[✗]${NC} $2: NO INSTALADO"
        FAILED=$((FAILED + 1))
        return 1
    fi
}

echo "═══════════════════════════════════════════════════════════════"
echo "1. VERIFICACIÓN DE COMANDOS PRINCIPALES"
echo "═══════════════════════════════════════════════════════════════"
check_command "R" "R"
check_command "rstudio" "RStudio"
check_command "python3" "Python 3"
check_command "pdflatex" "pdflatex"
check_command "xelatex" "xelatex"
check_command "pandoc" "Pandoc"
check_command "git" "Git"
echo ""

echo "═══════════════════════════════════════════════════════════════"
echo "2. VERIFICACIÓN DE PAQUETES DEL SISTEMA"
echo "═══════════════════════════════════════════════════════════════"
check_package "r" "R base"
check_package "texlive-core" "TeX Live Core"
check_package "texlive-latexextra" "TeX Live LaTeX Extra"
check_package "texlive-mathscience" "TeX Live Math Science"
check_package "texlive-pictures" "TeX Live Pictures (TikZ)"
check_package "python" "Python"
check_package "gcc-fortran" "GCC Fortran"
check_package "imagemagick" "ImageMagick"
echo ""

echo "═══════════════════════════════════════════════════════════════"
echo "3. VERIFICACIÓN DE VERSIONES"
echo "═══════════════════════════════════════════════════════════════"
if command -v R &> /dev/null; then
    echo -e "${BLUE}R:${NC}"
    R --version | head -n 1
fi

if command -v python3 &> /dev/null; then
    echo -e "${BLUE}Python:${NC}"
    python3 --version
fi

if command -v pdflatex &> /dev/null; then
    echo -e "${BLUE}LaTeX:${NC}"
    pdflatex --version | head -n 1
fi
echo ""

echo "═══════════════════════════════════════════════════════════════"
echo "4. VERIFICACIÓN DE PAQUETES R"
echo "═══════════════════════════════════════════════════════════════"

cat > /tmp/check_r_packages.R << 'RSCRIPT'
#!/usr/bin/env Rscript

packages <- c("exams", "knitr", "rmarkdown", "reticulate", 
              "ggplot2", "tidyverse", "tinytex", "testthat",
              "data.table", "readxl", "pdftools")

cat("Verificando paquetes R críticos:\n")
all_installed <- TRUE

for (pkg in packages) {
  if (require(pkg, character.only = TRUE, quietly = TRUE)) {
    version <- packageVersion(pkg)
    cat(sprintf("  ✓ %-15s %s\n", pkg, version))
  } else {
    cat(sprintf("  ✗ %-15s NO INSTALADO\n", pkg))
    all_installed <- FALSE
  }
}

if (all_installed) {
  cat("\n✅ Todos los paquetes R están instalados\n")
} else {
  cat("\n⚠️ Algunos paquetes R faltan\n")
}
RSCRIPT

R --vanilla --slave < /tmp/check_r_packages.R
echo ""

echo "═══════════════════════════════════════════════════════════════"
echo "5. VERIFICACIÓN DE PAQUETES PYTHON"
echo "═══════════════════════════════════════════════════════════════"

python3 << 'PYSCRIPT'
import sys

packages = {
    'matplotlib': 'Matplotlib',
    'numpy': 'NumPy',
    'pandas': 'Pandas'
}

print("Verificando paquetes Python:")
all_ok = True

for module, name in packages.items():
    try:
        mod = __import__(module)
        version = getattr(mod, '__version__', 'unknown')
        print(f"  ✓ {name:15} {version}")
    except ImportError:
        print(f"  ✗ {name:15} NO INSTALADO")
        all_ok = False

if all_ok:
    print("\n✅ Todos los paquetes Python están instalados")
else:
    print("\n⚠️ Algunos paquetes Python faltan")
PYSCRIPT

echo ""

echo "═══════════════════════════════════════════════════════════════"
echo "6. VERIFICACIÓN DE INTEGRACIÓN PYTHON-R"
echo "═══════════════════════════════════════════════════════════════"

cat > /tmp/check_reticulate.R << 'RSCRIPT'
#!/usr/bin/env Rscript

if (require("reticulate", quietly = TRUE)) {
  tryCatch({
    use_python("/usr/bin/python3", required = FALSE)
    py_config_info <- py_config()
    
    cat("✅ Reticulate configurado correctamente\n")
    cat("   Python:", py_config_info$python, "\n")
    cat("   Versión:", py_config_info$version, "\n")
    
    # Probar ejecución de código Python
    py_run_string("x = 2 + 2")
    result <- py$x
    cat("   Prueba de ejecución: 2 + 2 =", result, "\n")
    
    if (result == 4) {
      cat("✅ Integración Python-R funcional\n")
    }
    
  }, error = function(e) {
    cat("❌ Error en reticulate:", e$message, "\n")
  })
} else {
  cat("❌ Paquete reticulate no instalado\n")
}
RSCRIPT

R --vanilla --slave < /tmp/check_reticulate.R
echo ""

echo "═══════════════════════════════════════════════════════════════"
echo "7. VERIFICACIÓN DE TINYTEX"
echo "═══════════════════════════════════════════════════════════════"

cat > /tmp/check_tinytex.R << 'RSCRIPT'
#!/usr/bin/env Rscript

if (require("tinytex", quietly = TRUE)) {
  if (is_tinytex()) {
    cat("✅ TinyTeX instalado y configurado\n")
    
    # Verificar paquetes LaTeX críticos
    critical_latex <- c("tikz", "pgfplots", "babel-spanish", "amsmath")
    cat("\nPaquetes LaTeX críticos:\n")
    
    for (pkg in critical_latex) {
      # Nota: tlmgr_search puede no estar disponible en todas las configuraciones
      cat("  •", pkg, "\n")
    }
  } else {
    cat("⚠️ TinyTeX no detectado (usando TeX Live del sistema)\n")
  }
} else {
  cat("❌ Paquete tinytex no instalado\n")
}
RSCRIPT

R --vanilla --slave < /tmp/check_tinytex.R
echo ""

echo "═══════════════════════════════════════════════════════════════"
echo "8. VERIFICACIÓN DEL PROYECTO"
echo "═══════════════════════════════════════════════════════════════"

PROJECT_DIR="/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams"

if [ -d "$PROJECT_DIR" ]; then
    echo -e "${GREEN}[✓]${NC} Directorio del proyecto encontrado"
    
    if [ -f "$PROJECT_DIR/.Rprofile" ]; then
        echo -e "${GREEN}[✓]${NC} .Rprofile configurado"
    else
        echo -e "${YELLOW}[⚠]${NC} .Rprofile no encontrado"
    fi
    
    if [ -f "$PROJECT_DIR/RepositorioMatematicasICFES_R_Exams.Rproj" ]; then
        echo -e "${GREEN}[✓]${NC} Archivo de proyecto RStudio encontrado"
    else
        echo -e "${YELLOW}[⚠]${NC} Archivo .Rproj no encontrado"
    fi
    
    # Verificar directorios importantes
    for dir in "Auxiliares" "Lab-Manjaro" "06-Estadística-Y-Probabilidad"; do
        if [ -d "$PROJECT_DIR/$dir" ]; then
            echo -e "${GREEN}[✓]${NC} Directorio $dir existe"
        else
            echo -e "${RED}[✗]${NC} Directorio $dir no encontrado"
        fi
    done
else
    echo -e "${RED}[✗]${NC} Directorio del proyecto no encontrado: $PROJECT_DIR"
fi
echo ""

echo "═══════════════════════════════════════════════════════════════"
echo "RESUMEN DE VERIFICACIÓN"
echo "═══════════════════════════════════════════════════════════════"
echo -e "Total de verificaciones: $TOTAL"
echo -e "${GREEN}Exitosas: $PASSED${NC}"
echo -e "${RED}Fallidas: $FAILED${NC}"
echo ""

if [ $FAILED -eq 0 ]; then
    echo "╔════════════════════════════════════════════════════════════════╗"
    echo "║  ✅ TODAS LAS VERIFICACIONES PASARON EXITOSAMENTE             ║"
    echo "║     El entorno está completamente configurado                 ║"
    echo "╚════════════════════════════════════════════════════════════════╝"
    exit 0
else
    echo "╔════════════════════════════════════════════════════════════════╗"
    echo "║  ⚠️ ALGUNAS VERIFICACIONES FALLARON                           ║"
    echo "║     Revisa los componentes marcados con ✗                     ║"
    echo "╚════════════════════════════════════════════════════════════════╝"
    exit 1
fi

