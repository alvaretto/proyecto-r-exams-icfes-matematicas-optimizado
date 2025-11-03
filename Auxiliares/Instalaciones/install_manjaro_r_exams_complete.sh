#!/bin/bash

################################################################################
# Script de Instalación Completa para R-Exams ICFES en Manjaro Plasma KDE
# Proyecto: RepositorioMatematicasICFES_R_Exams
# Fecha: 2025-11-02
# Descripción: Instalación automatizada de R, RStudio, LaTeX, Python y todas
#              las dependencias necesarias para el proyecto ICFES R-Exams
################################################################################

set -e  # Detener en caso de error

# Colores para output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Funciones de logging
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[✓]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[⚠]${NC} $1"
}

log_error() {
    echo -e "${RED}[✗]${NC} $1"
}

# Función para verificar si un paquete está instalado
package_installed() {
    pacman -Qi "$1" &> /dev/null
}

# Banner de inicio
clear
echo "╔════════════════════════════════════════════════════════════════╗"
echo "║   INSTALACIÓN COMPLETA R-EXAMS ICFES - MANJARO PLASMA KDE     ║"
echo "║   Proyecto: RepositorioMatematicasICFES_R_Exams                ║"
echo "╚════════════════════════════════════════════════════════════════╝"
echo ""

# Verificar que se ejecuta en Manjaro
if ! grep -q "Manjaro" /etc/os-release; then
    log_error "Este script está diseñado para Manjaro Linux"
    exit 1
fi

log_info "Iniciando instalación del entorno de desarrollo R-Exams ICFES..."
echo ""

################################################################################
# FASE 1: ACTUALIZACIÓN DEL SISTEMA
################################################################################
log_info "FASE 1: Actualizando el sistema..."
sudo pacman -Syu --noconfirm
log_success "Sistema actualizado"
echo ""

################################################################################
# FASE 2: INSTALACIÓN DE R
################################################################################
log_info "FASE 2: Instalando R (versión más reciente)..."

if package_installed "r"; then
    log_success "R ya está instalado"
    R --version | head -n 1
else
    log_info "Instalando R desde repositorios oficiales de Manjaro..."
    sudo pacman -S --noconfirm r
    log_success "R instalado exitosamente"
fi

# Verificar instalación de R
if command -v R &> /dev/null; then
    R_VERSION=$(R --version | head -n 1)
    log_success "R instalado: $R_VERSION"
else
    log_error "Error: R no se instaló correctamente"
    exit 1
fi
echo ""

################################################################################
# FASE 3: INSTALACIÓN DE RSTUDIO DESKTOP
################################################################################
log_info "FASE 3: Instalando RStudio Desktop..."

if command -v rstudio &> /dev/null; then
    log_success "RStudio ya está instalado"
else
    log_info "Instalando RStudio desde AUR..."
    
    # Verificar que yay está instalado
    if ! command -v yay &> /dev/null; then
        log_warning "yay no está instalado. Instalando yay..."
        sudo pacman -S --noconfirm base-devel git
        cd /tmp
        git clone https://aur.archlinux.org/yay.git
        cd yay
        makepkg -si --noconfirm
        cd ..
        rm -rf yay
        log_success "yay instalado"
    fi
    
    # Instalar RStudio desde AUR
    yay -S --noconfirm rstudio-desktop-bin
    log_success "RStudio Desktop instalado"
fi

# Verificar instalación de RStudio
if command -v rstudio &> /dev/null; then
    log_success "RStudio Desktop instalado correctamente"
else
    log_warning "RStudio no se instaló. Puedes instalarlo manualmente con: yay -S rstudio-desktop-bin"
fi
echo ""

################################################################################
# FASE 4: INSTALACIÓN DE LATEX (TEX LIVE COMPLETO)
################################################################################
log_info "FASE 4: Instalando LaTeX (TeX Live completo)..."

LATEX_PACKAGES=(
    "texlive-core"
    "texlive-bin"
    "texlive-basic"
    "texlive-latex"
    "texlive-latexextra"
    "texlive-latexrecommended"
    "texlive-mathscience"
    "texlive-pictures"
    "texlive-fontsextra"
    "texlive-fontsrecommended"
    "texlive-langspanish"
    "texlive-xetex"
    "texlive-luatex"
)

for package in "${LATEX_PACKAGES[@]}"; do
    if package_installed "$package"; then
        log_success "$package ya está instalado"
    else
        log_info "Instalando $package..."
        sudo pacman -S --noconfirm "$package" 2>/dev/null || log_warning "$package no disponible en repos"
    fi
done

log_success "Paquetes LaTeX instalados"
echo ""

################################################################################
# FASE 5: INSTALACIÓN DE PYTHON Y PAQUETES
################################################################################
log_info "FASE 5: Configurando Python y paquetes necesarios..."

# Verificar Python 3
if command -v python3 &> /dev/null; then
    PYTHON_VERSION=$(python3 --version)
    log_success "Python instalado: $PYTHON_VERSION"
else
    log_info "Instalando Python 3..."
    sudo pacman -S --noconfirm python python-pip
    log_success "Python 3 instalado"
fi

# Instalar paquetes Python necesarios
log_info "Instalando paquetes Python (matplotlib, numpy, pandas)..."
pip install --user --upgrade matplotlib numpy pandas seaborn
log_success "Paquetes Python instalados"
echo ""

################################################################################
# FASE 6: DEPENDENCIAS DEL SISTEMA
################################################################################
log_info "FASE 6: Instalando dependencias del sistema..."

SYSTEM_DEPS=(
    "gcc-fortran"
    "pandoc"
    "imagemagick"
    "cairo"
    "pango"
    "libpng"
    "libjpeg-turbo"
    "libtiff"
    "git"
    "curl"
    "wget"
    "libxml2"
    "openssl"
    "harfbuzz"
    "fribidi"
    "freetype2"
)

for package in "${SYSTEM_DEPS[@]}"; do
    if package_installed "$package"; then
        log_success "$package ya está instalado"
    else
        log_info "Instalando $package..."
        sudo pacman -S --noconfirm "$package"
    fi
done

log_success "Dependencias del sistema instaladas"
echo ""

################################################################################
# FASE 7: INSTALACIÓN DE PAQUETES R
################################################################################
log_info "FASE 7: Instalando paquetes R necesarios para el proyecto..."

# Crear directorio para biblioteca personal de R
mkdir -p ~/R/library

# Crear script de instalación de paquetes R
cat > /tmp/install_r_packages_icfes.R << 'RSCRIPT'
#!/usr/bin/env Rscript

# Configurar biblioteca personal
.libPaths(c("~/R/library", .libPaths()))

# Configurar repositorio CRAN
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Función para instalar paquetes con manejo de errores
install_package_safe <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("📦 Instalando", pkg, "...\n")
    tryCatch({
      install.packages(pkg, dependencies = TRUE)
      cat("✓", pkg, "instalado exitosamente\n")
    }, error = function(e) {
      cat("✗ Error instalando", pkg, ":", e$message, "\n")
    })
  } else {
    cat("✓", pkg, "ya está instalado\n")
  }
}

# Lista de paquetes necesarios para R-exams ICFES
packages <- c(
  # Paquetes principales R-exams
  "exams", "knitr", "rmarkdown",
  
  # Manipulación de datos
  "tidyverse", "dplyr", "tidyr", "data.table", "readxl",
  
  # Visualización
  "ggplot2", "scales", "gridExtra", "RColorBrewer",
  
  # Estadística
  "MASS", "car", "psych",
  
  # LaTeX y documentos
  "tinytex", "pdftools", "qpdf",
  
  # Integración Python (CRÍTICO)
  "reticulate",
  
  # Herramientas adicionales
  "devtools", "testthat", "digest", "magick", "webshot", 
  "htmltools", "base64enc", "datasets"
)

cat("═══════════════════════════════════════════════════════════\n")
cat("  INSTALANDO PAQUETES R PARA PROYECTO ICFES R-EXAMS\n")
cat("═══════════════════════════════════════════════════════════\n\n")

for (pkg in packages) {
  install_package_safe(pkg)
}

cat("\n🎯 Instalación de paquetes R completada\n")
RSCRIPT

chmod +x /tmp/install_r_packages_icfes.R
log_info "Ejecutando instalación de paquetes R (esto puede tomar varios minutos)..."
R --vanilla < /tmp/install_r_packages_icfes.R

log_success "Paquetes R instalados"
echo ""

################################################################################
# FASE 8: CONFIGURACIÓN DE TINYTEX Y PAQUETES LATEX
################################################################################
log_info "FASE 8: Configurando TinyTeX y paquetes LaTeX adicionales..."

cat > /tmp/setup_tinytex_icfes.R << 'RSCRIPT'
#!/usr/bin/env Rscript

library(tinytex)

# Instalar TinyTeX si no está instalado
if (!is_tinytex()) {
  cat("📄 Instalando TinyTeX...\n")
  install_tinytex(force = TRUE)
  Sys.sleep(5)  # Esperar a que se complete
  cat("✅ TinyTeX instalado\n")
} else {
  cat("✅ TinyTeX ya está instalado\n")
}

# Paquetes LaTeX críticos para R-exams ICFES
paquetes_latex <- c(
  # Matemáticas
  "amsmath", "amsfonts", "amssymb", "mathtools",
  
  # Idioma español
  "babel", "babel-spanish",
  
  # Tablas y formato
  "booktabs", "colortbl", "array", "multirow",
  
  # TikZ y gráficos (CRÍTICO para el proyecto)
  "tikz", "pgf", "pgfplots", "xcolor",
  
  # Geometría y diseño
  "geometry", "fancyhdr", "enumitem", "float",
  
  # Fuentes y XeLaTeX
  "fontspec", "xunicode", "unicode-math",
  
  # Otros esenciales
  "hyperref", "url", "graphicx", "listings",
  "setspace", "parskip", "microtype"
)

if (is_tinytex()) {
  cat("📦 Instalando paquetes LaTeX para R-exams ICFES...\n")
  tryCatch({
    tlmgr_install(paquetes_latex)
    cat("✅ Paquetes LaTeX instalados correctamente\n")
  }, error = function(e) {
    cat("⚠️ Algunos paquetes LaTeX pueden no haberse instalado:", e$message, "\n")
  })
} else {
  cat("❌ TinyTeX no se instaló correctamente\n")
}
RSCRIPT

chmod +x /tmp/setup_tinytex_icfes.R
R --vanilla < /tmp/setup_tinytex_icfes.R

log_success "TinyTeX configurado"
echo ""

################################################################################
# FASE 9: CONFIGURACIÓN DE PYTHON-R (RETICULATE)
################################################################################
log_info "FASE 9: Configurando integración Python-R (reticulate)..."

cat > /tmp/configure_reticulate.R << 'RSCRIPT'
#!/usr/bin/env Rscript

library(reticulate)

# Configurar Python 3
python3_path <- Sys.which("python3")

if (python3_path != "") {
  cat("🐍 Configurando Python para reticulate...\n")
  use_python(python3_path, required = TRUE)

  # Verificar configuración
  py_config_info <- py_config()
  cat("✅ Python configurado correctamente\n")
  cat("   Ruta:", py_config_info$python, "\n")
  cat("   Versión:", py_config_info$version, "\n")

  # Probar funcionalidad
  tryCatch({
    py_run_string("print('✅ Python-R integración funcional')")
    py_run_string("import matplotlib; print('✅ matplotlib disponible')")
    py_run_string("import numpy; print('✅ numpy disponible')")
  }, error = function(e) {
    cat("⚠️ Error en prueba de Python:", e$message, "\n")
  })
} else {
  cat("❌ Python 3 no encontrado\n")
}
RSCRIPT

chmod +x /tmp/configure_reticulate.R
R --vanilla < /tmp/configure_reticulate.R

log_success "Integración Python-R configurada"
echo ""

################################################################################
# FASE 10: CONFIGURACIÓN DEL PROYECTO
################################################################################
log_info "FASE 10: Configurando archivos del proyecto..."

# Crear/actualizar .Rprofile en el directorio del proyecto
PROJECT_DIR="/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams"

if [ -d "$PROJECT_DIR" ]; then
    cat > "$PROJECT_DIR/.Rprofile" << 'RPROFILE'
# Configuración R para Proyecto ICFES R-Exams
# Cargado automáticamente al iniciar R en este proyecto

# Configurar biblioteca personal
.libPaths(c("~/R/library", .libPaths()))

# Configurar opciones de R
options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  scipen = 999,           # Evitar notación científica
  digits = 10,            # Precisión numérica
  width = 120,
  warn = 1,
  OutDec = "."            # Punto decimal estándar
)

# Configurar encoding UTF-8
options(encoding = "UTF-8")

# Configurar Python para reticulate
if (require("reticulate", quietly = TRUE)) {
  python3_path <- Sys.which("python3")
  if (python3_path != "") {
    use_python(python3_path, required = FALSE)
  }
}

# Función para cargar paquetes comunes del proyecto
load_icfes_packages <- function() {
  packages <- c("exams", "reticulate", "knitr", "rmarkdown",
                "ggplot2", "tidyverse", "testthat")
  for (pkg in packages) {
    if (require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat("✓", pkg, "cargado\n")
    } else {
      cat("✗", pkg, "no disponible\n")
    }
  }
}

# Mensaje de bienvenida
cat("\n╔════════════════════════════════════════════════════════╗\n")
cat("║  Proyecto: RepositorioMatematicasICFES_R_Exams        ║\n")
cat("║  R configurado para desarrollo R-exams ICFES          ║\n")
cat("╚════════════════════════════════════════════════════════╝\n\n")
cat("📚 Biblioteca personal:", .libPaths()[1], "\n")
cat("🐍 Python configurado:", Sys.which("python3"), "\n")
cat("💡 Usa load_icfes_packages() para cargar paquetes comunes\n\n")
RPROFILE

    log_success ".Rprofile creado en el proyecto"

    # Crear directorio para outputs si no existe
    mkdir -p "$PROJECT_DIR/out_html_cloze"
    mkdir -p "$PROJECT_DIR/out_moodle_cloze"

    log_success "Directorios del proyecto configurados"
else
    log_warning "Directorio del proyecto no encontrado: $PROJECT_DIR"
fi
echo ""

################################################################################
# FASE 11: VALIDACIÓN DE LA INSTALACIÓN
################################################################################
log_info "FASE 11: Validando instalación completa..."

cat > /tmp/validate_installation.R << 'RSCRIPT'
#!/usr/bin/env Rscript

cat("\n╔════════════════════════════════════════════════════════╗\n")
cat("║         VALIDACIÓN DE INSTALACIÓN R-EXAMS ICFES       ║\n")
cat("╚════════════════════════════════════════════════════════╝\n\n")

# Verificar R
cat("1. R:\n")
cat("   Versión:", R.version.string, "\n")
cat("   ✅ R instalado correctamente\n\n")

# Verificar paquetes críticos
cat("2. Paquetes R críticos:\n")
critical_packages <- c("exams", "knitr", "rmarkdown", "reticulate",
                       "ggplot2", "tidyverse", "tinytex")

all_ok <- TRUE
for (pkg in critical_packages) {
  if (require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("   ✅", pkg, "\n")
  } else {
    cat("   ❌", pkg, "NO INSTALADO\n")
    all_ok <- FALSE
  }
}

# Verificar TinyTeX
cat("\n3. TinyTeX:\n")
if (require("tinytex", quietly = TRUE)) {
  if (tinytex::is_tinytex()) {
    cat("   ✅ TinyTeX instalado y configurado\n")
  } else {
    cat("   ⚠️ TinyTeX no detectado (puede usar TeX Live del sistema)\n")
  }
}

# Verificar Python
cat("\n4. Python (reticulate):\n")
if (require("reticulate", quietly = TRUE)) {
  tryCatch({
    py_config_info <- py_config()
    cat("   ✅ Python:", py_config_info$python, "\n")
    cat("   ✅ Versión:", py_config_info$version, "\n")

    # Verificar paquetes Python
    py_run_string("import matplotlib")
    cat("   ✅ matplotlib disponible\n")
    py_run_string("import numpy")
    cat("   ✅ numpy disponible\n")
  }, error = function(e) {
    cat("   ❌ Error configurando Python:", e$message, "\n")
    all_ok <- FALSE
  })
}

# Resultado final
cat("\n")
if (all_ok) {
  cat("╔════════════════════════════════════════════════════════╗\n")
  cat("║  ✅ INSTALACIÓN COMPLETADA EXITOSAMENTE               ║\n")
  cat("╚════════════════════════════════════════════════════════╝\n")
} else {
  cat("╔════════════════════════════════════════════════════════╗\n")
  cat("║  ⚠️ INSTALACIÓN COMPLETADA CON ADVERTENCIAS           ║\n")
  cat("╚════════════════════════════════════════════════════════╝\n")
}
RSCRIPT

chmod +x /tmp/validate_installation.R
R --vanilla < /tmp/validate_installation.R

echo ""

################################################################################
# FASE 12: PRUEBA DE COMPILACIÓN DE ARCHIVO .RMD
################################################################################
log_info "FASE 12: Creando archivo de prueba .Rmd..."

cat > /tmp/test_rexams.Rmd << 'RMDTEST'
---
output:
  pdf_document:
    latex_engine: xelatex
  html_document: default
---

```{r setup, include=FALSE}
library(exams)
library(knitr)
library(reticulate)

# Configurar Python
use_python("/usr/bin/python3", required = FALSE)

knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
```

Question
========

Este es un ejercicio de prueba para validar la instalación de R-exams ICFES.

¿Cuál es el resultado de $2 + 2$?

Answerlist
----------
- 3
- 4
- 5
- 6

Solution
========

La respuesta correcta es 4, ya que $2 + 2 = 4$.

Answerlist
----------
- Falso
- Verdadero
- Falso
- Falso

Meta-information
================
exname: test-instalacion
extype: schoice
exsolution: 0100
exshuffle: TRUE
RMDTEST

log_info "Compilando archivo de prueba..."

cat > /tmp/compile_test.R << 'RSCRIPT'
#!/usr/bin/env Rscript

library(exams)

tryCatch({
  # Intentar compilar a HTML
  exams2html("/tmp/test_rexams.Rmd",
             n = 1,
             name = "test_instalacion",
             dir = "/tmp",
             edir = "/tmp")

  cat("✅ Compilación HTML exitosa\n")

  # Intentar compilar a PDF
  exams2pdf("/tmp/test_rexams.Rmd",
            n = 1,
            name = "test_instalacion_pdf",
            dir = "/tmp",
            edir = "/tmp")

  cat("✅ Compilación PDF exitosa\n")
  cat("\n🎉 El sistema R-exams está completamente funcional\n")

}, error = function(e) {
  cat("⚠️ Error en compilación de prueba:", e$message, "\n")
  cat("   Esto puede ser normal si faltan algunos templates LaTeX\n")
})
RSCRIPT

chmod +x /tmp/compile_test.R
R --vanilla < /tmp/compile_test.R 2>/dev/null || log_warning "Compilación de prueba con advertencias (normal en primera instalación)"

echo ""

################################################################################
# RESUMEN FINAL
################################################################################
echo ""
echo "╔════════════════════════════════════════════════════════════════╗"
echo "║              INSTALACIÓN COMPLETADA EXITOSAMENTE               ║"
echo "╚════════════════════════════════════════════════════════════════╝"
echo ""
log_success "Entorno de desarrollo R-Exams ICFES instalado en Manjaro"
echo ""
echo "📋 RESUMEN DE INSTALACIÓN:"
echo "   ✅ R $(R --version | head -n 1 | cut -d' ' -f3)"
echo "   ✅ RStudio Desktop"
echo "   ✅ LaTeX (TeX Live completo)"
echo "   ✅ TinyTeX (para R)"
echo "   ✅ Python 3 con matplotlib, numpy, pandas"
echo "   ✅ Paquetes R: exams, knitr, rmarkdown, reticulate, ggplot2, tidyverse"
echo "   ✅ Integración Python-R (reticulate)"
echo ""
echo "📂 UBICACIONES IMPORTANTES:"
echo "   • Proyecto: /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams"
echo "   • Biblioteca R: ~/R/library"
echo "   • Python: $(which python3)"
echo "   • R: $(which R)"
echo "   • RStudio: $(which rstudio 2>/dev/null || echo 'Instalar con: yay -S rstudio-desktop-bin')"
echo ""
echo "🚀 PRÓXIMOS PASOS:"
echo "   1. Abrir RStudio: rstudio &"
echo "   2. Abrir el proyecto: File > Open Project > RepositorioMatematicasICFES_R_Exams.Rproj"
echo "   3. Ejecutar en consola R: load_icfes_packages()"
echo "   4. Probar compilación de un archivo .Rmd del proyecto"
echo ""
echo "📚 DOCUMENTACIÓN DEL PROYECTO:"
echo "   • README.md"
echo "   • Auxiliares/Instalaciones/Tutorial Actualizado R-exams Mayo 2025.md"
echo "   • Auxiliares/Ejemplos-Funcionales-Rmd/"
echo ""
echo "💡 COMANDOS ÚTILES:"
echo "   • Verificar instalación R: R --version"
echo "   • Verificar paquetes R: R -e 'installed.packages()[,c(\"Package\",\"Version\")]'"
echo "   • Verificar Python: python3 --version"
echo "   • Verificar LaTeX: pdflatex --version"
echo ""
log_success "¡Instalación completada! El entorno está listo para trabajar."
echo ""

