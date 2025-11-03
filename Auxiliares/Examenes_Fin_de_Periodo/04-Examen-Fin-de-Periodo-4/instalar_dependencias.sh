#!/bin/bash

################################################################################
# SCRIPT: instalar_dependencias.sh
# DESCRIPCIÓN: Instala todas las dependencias necesarias para el sistema
#              de exámenes de fin de período
# SISTEMA: Manjaro Plasma KDE
# FECHA: 2025-01-30
################################################################################

echo "================================================================================"
echo "  INSTALACIÓN DE DEPENDENCIAS - EXAMEN FIN DE PERÍODO 4"
echo "================================================================================"
echo ""

# Colores para output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Función para verificar si un comando existe
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Función para verificar si un paquete Python está instalado
python_package_exists() {
    python3 -c "import $1" >/dev/null 2>&1
}

################################################################################
# 1. VERIFICAR PYTHON
################################################################################

echo "1. Verificando Python..."
if command_exists python3; then
    PYTHON_VERSION=$(python3 --version)
    echo -e "${GREEN}✓${NC} Python instalado: $PYTHON_VERSION"
else
    echo -e "${RED}✗${NC} Python NO instalado"
    echo "   Instalando Python..."
    sudo pacman -S --noconfirm python
fi
echo ""

################################################################################
# 2. INSTALAR MATPLOTLIB Y NUMPY
################################################################################

echo "2. Verificando matplotlib y numpy..."

# Verificar matplotlib
if python_package_exists matplotlib; then
    MATPLOTLIB_VERSION=$(python3 -c "import matplotlib; print(matplotlib.__version__)")
    echo -e "${GREEN}✓${NC} matplotlib instalado: versión $MATPLOTLIB_VERSION"
else
    echo -e "${YELLOW}⚠${NC}  matplotlib NO instalado"
    echo "   Instalando matplotlib..."
    sudo pacman -S --noconfirm python-matplotlib
fi

# Verificar numpy
if python_package_exists numpy; then
    NUMPY_VERSION=$(python3 -c "import numpy; print(numpy.__version__)")
    echo -e "${GREEN}✓${NC} numpy instalado: versión $NUMPY_VERSION"
else
    echo -e "${YELLOW}⚠${NC}  numpy NO instalado"
    echo "   Instalando numpy..."
    sudo pacman -S --noconfirm python-numpy
fi
echo ""

################################################################################
# 3. VERIFICAR R
################################################################################

echo "3. Verificando R..."
if command_exists R; then
    R_VERSION=$(R --version | head -1)
    echo -e "${GREEN}✓${NC} R instalado: $R_VERSION"
else
    echo -e "${RED}✗${NC} R NO instalado"
    echo "   Instalando R..."
    sudo pacman -S --noconfirm r
fi
echo ""

################################################################################
# 4. VERIFICAR LATEX
################################################################################

echo "4. Verificando LaTeX..."
if command_exists pdflatex; then
    LATEX_VERSION=$(pdflatex --version | head -1)
    echo -e "${GREEN}✓${NC} LaTeX instalado: $LATEX_VERSION"
else
    echo -e "${YELLOW}⚠${NC}  LaTeX NO instalado"
    echo "   ¿Deseas instalar LaTeX? (necesario para generar PDFs)"
    echo "   Esto descargará ~2GB de paquetes."
    read -p "   Instalar LaTeX? (s/n): " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Ss]$ ]]; then
        echo "   Instalando LaTeX..."
        sudo pacman -S --noconfirm texlive-most
    else
        echo "   LaTeX no instalado. Los PDFs no se podrán generar."
    fi
fi
echo ""

################################################################################
# 5. VERIFICAR PAQUETES R
################################################################################

echo "5. Verificando paquetes R necesarios..."

# Script R para verificar e instalar paquetes
cat > /tmp/check_r_packages.R << 'EOF'
# Lista de paquetes necesarios
required_packages <- c("exams", "reticulate", "tidyverse", "ggplot2", 
                       "knitr", "testthat", "data.table", "readxl", 
                       "datasets", "digest")

# Verificar e instalar cada paquete
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(sprintf("⚠  Instalando paquete '%s'...\n", pkg))
    install.packages(pkg, repos = "https://cloud.r-project.org/", quiet = TRUE)
  } else {
    cat(sprintf("✓ Paquete '%s' instalado\n", pkg))
  }
}

cat("\n✓ Todos los paquetes R están instalados\n")
EOF

# Ejecutar script R
Rscript /tmp/check_r_packages.R
rm /tmp/check_r_packages.R
echo ""

################################################################################
# 6. RESUMEN FINAL
################################################################################

echo "================================================================================"
echo "  RESUMEN DE INSTALACIÓN"
echo "================================================================================"
echo ""

# Verificar Python
if command_exists python3; then
    echo -e "${GREEN}✓${NC} Python: $(python3 --version)"
else
    echo -e "${RED}✗${NC} Python: NO instalado"
fi

# Verificar matplotlib
if python_package_exists matplotlib; then
    echo -e "${GREEN}✓${NC} matplotlib: $(python3 -c 'import matplotlib; print(matplotlib.__version__)')"
else
    echo -e "${RED}✗${NC} matplotlib: NO instalado"
fi

# Verificar numpy
if python_package_exists numpy; then
    echo -e "${GREEN}✓${NC} numpy: $(python3 -c 'import numpy; print(numpy.__version__)')"
else
    echo -e "${RED}✗${NC} numpy: NO instalado"
fi

# Verificar R
if command_exists R; then
    echo -e "${GREEN}✓${NC} R: Instalado"
else
    echo -e "${RED}✗${NC} R: NO instalado"
fi

# Verificar LaTeX
if command_exists pdflatex; then
    echo -e "${GREEN}✓${NC} LaTeX: Instalado"
else
    echo -e "${YELLOW}⚠${NC}  LaTeX: NO instalado (PDFs no disponibles)"
fi

echo ""
echo "================================================================================"
echo "  INSTALACIÓN COMPLETADA"
echo "================================================================================"
echo ""
echo "Ahora puedes ejecutar el script de generación de exámenes:"
echo "  cd Auxiliares/Examenes_Fin_de_Periodo/04-Examen-Fin-de-Periodo-4"
echo "  Rscript SemilleroFinDePeriodo_v4.R"
echo ""
echo "O desde RStudio:"
echo "  setwd('Auxiliares/Examenes_Fin_de_Periodo/04-Examen-Fin-de-Periodo-4')"
echo "  source('SemilleroFinDePeriodo_v4.R')"
echo ""

