#!/bin/bash

# 🏗️ Script de Instalación Automatizada RStudio para R-exams ICFES
# Diseñado específicamente para Manjaro Linux y el proyecto de matemáticas ICFES

set -e  # Salir en caso de error

echo "🎯 INSTALACIÓN RSTUDIO PARA PROYECTO R-EXAMS ICFES"
echo "=================================================="
echo ""

# Colores para output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Función para logging
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[✅]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[⚠️]${NC} $1"
}

log_error() {
    echo -e "${RED}[❌]${NC} $1"
}

# Función para verificar si un comando existe
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Función para verificar si un paquete está instalado
package_installed() {
    pacman -Qi "$1" >/dev/null 2>&1
}

# Verificar que estamos en Manjaro
if ! command_exists pacman; then
    log_error "Este script está diseñado para Manjaro/Arch Linux"
    exit 1
fi

log_info "Detectado sistema Manjaro/Arch Linux ✅"

# FASE 1: Actualización del sistema
log_info "FASE 1: Actualizando sistema base..."
sudo pacman -Syu --noconfirm

# Verificar/instalar yay
if ! command_exists yay; then
    log_info "Instalando yay (gestor AUR)..."
    sudo pacman -S --needed --noconfirm git base-devel
    cd /tmp
    git clone https://aur.archlinux.org/yay.git
    cd yay
    makepkg -si --noconfirm
    cd ~
    rm -rf /tmp/yay
    log_success "yay instalado"
else
    log_success "yay ya está disponible"
fi

# FASE 2: Dependencias del sistema
log_info "FASE 2: Instalando dependencias del sistema..."

SYSTEM_PACKAGES=(
    "r"
    "gcc-fortran"
    "texlive-core"
    "texlive-bin"
    "texlive-latexextra"
    "texlive-science"
    "texlive-pictures"
    "pandoc"
    "imagemagick"
    "libpng"
    "cairo"
    "python"
    "python-pip"
)

for package in "${SYSTEM_PACKAGES[@]}"; do
    if package_installed "$package"; then
        log_success "$package ya está instalado"
    else
        log_info "Instalando $package..."
        sudo pacman -S --noconfirm "$package"
        log_success "$package instalado"
    fi
done

# FASE 3: Instalación de RStudio
log_info "FASE 3: Instalando RStudio Desktop..."
if command_exists rstudio; then
    log_success "RStudio ya está instalado"
else
    log_info "Instalando RStudio desde AUR..."
    yay -S --noconfirm rstudio-desktop-bin
    log_success "RStudio instalado"
fi

# FASE 4: Configuración de directorios
log_info "FASE 4: Configurando estructura de directorios..."

# Crear directorios del proyecto
mkdir -p ~/R/library
mkdir -p ~/R/projects/icfes-matematicas/{data,scripts,output,figures,templates,exams,backups}

# Configurar biblioteca de usuario
echo 'R_LIBS_USER=~/R/library' > ~/.Renviron

log_success "Estructura de directorios creada"

# FASE 5: Instalación de paquetes R
log_info "FASE 5: Instalando paquetes R especializados..."

cat > /tmp/install_r_packages.R << 'EOF'
#!/usr/bin/env Rscript

# Configurar repositorio CRAN
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Paquetes principales del proyecto R-exams ICFES
paquetes_principales <- c(
  # Framework R-exams
  "exams", "knitr", "rmarkdown",
  
  # Visualización y gráficos
  "ggplot2", "scales", "gridExtra",
  
  # Manipulación de datos
  "dplyr", "tidyr", "reshape2", "data.table", "tidyverse",
  
  # Estadísticas y testing
  "testthat", "digest",
  
  # Integración Python (crítico para metodologías TikZ)
  "reticulate",
  
  # Procesamiento PDF y LaTeX
  "pdftools", "qpdf", "tinytex",
  
  # Herramientas adicionales del proyecto
  "readxl", "datasets", "magick", "webshot"
)

cat("📦 Instalando", length(paquetes_principales), "paquetes R...\n")

# Instalar con manejo de errores
errores <- c()
for (paquete in paquetes_principales) {
  cat("Instalando:", paquete, "...")
  tryCatch({
    install.packages(paquete, dependencies = TRUE, quiet = TRUE)
    cat(" ✅\n")
  }, error = function(e) {
    cat(" ❌\n")
    errores <- c(errores, paquete)
  })
}

if (length(errores) > 0) {
  cat("❌ Errores en:", paste(errores, collapse = ", "), "\n")
} else {
  cat("✅ Todos los paquetes R instalados correctamente\n")
}
EOF

chmod +x /tmp/install_r_packages.R
R --vanilla < /tmp/install_r_packages.R

log_success "Paquetes R instalados"

# FASE 6: Configuración de TinyTeX
log_info "FASE 6: Configurando TinyTeX..."

cat > /tmp/setup_tinytex.R << 'EOF'
#!/usr/bin/env Rscript

library(tinytex)

# Instalar TinyTeX
if (!is_tinytex()) {
  cat("📄 Instalando TinyTeX...\n")
  install_tinytex(force = TRUE)
} else {
  cat("✅ TinyTeX ya está instalado\n")
}

# Paquetes LaTeX críticos para R-exams ICFES
paquetes_latex <- c(
  "amsmath", "amsfonts", "amssymb", "mathtools",
  "babel", "babel-spanish",
  "booktabs", "colortbl", "array", "multirow",
  "tikz", "pgf", "pgfplots", "xcolor",
  "geometry", "fancyhdr", "enumitem", "float",
  "fontspec", "xunicode", "xetex",
  "hyperref", "url", "graphicx", "listings"
)

if (is_tinytex()) {
  cat("📦 Instalando paquetes LaTeX...\n")
  tlmgr_install(paquetes_latex)
  cat("✅ Paquetes LaTeX instalados\n")
}
EOF

chmod +x /tmp/setup_tinytex.R
R --vanilla < /tmp/setup_tinytex.R

log_success "TinyTeX configurado"

# FASE 7: Configuración de Python
log_info "FASE 7: Configurando integración Python..."

cat > /tmp/setup_python.R << 'EOF'
#!/usr/bin/env Rscript

library(reticulate)

# Configurar Python
python_path <- Sys.which("python")
if (python_path == "") {
  python_path <- Sys.which("python3")
}

if (python_path != "") {
  use_python(python_path, required = TRUE)
  cat("🐍 Python configurado en:", python_path, "\n")
  
  # Instalar paquetes Python críticos
  paquetes_python <- c("numpy", "matplotlib", "pandas", "scipy")
  
  for (paquete in paquetes_python) {
    tryCatch({
      py_install(paquete, pip = TRUE)
      cat("✅ Python package:", paquete, "\n")
    }, error = function(e) {
      cat("❌ Error:", paquete, "\n")
    })
  }
} else {
  cat("❌ Python no encontrado\n")
}
EOF

chmod +x /tmp/setup_python.R
R --vanilla < /tmp/setup_python.R

log_success "Python configurado"

# FASE 8: Validación del sistema
log_info "FASE 8: Validando instalación..."

cat > /tmp/validate_installation.R << 'EOF'
#!/usr/bin/env Rscript

cat("🔍 VALIDACIÓN DEL SISTEMA\n")
cat("========================\n")

# Verificar R
cat("📊 R Version:", R.version.string, "\n")

# Verificar paquetes críticos
paquetes_criticos <- c("exams", "reticulate", "tinytex", "ggplot2")
cat("\n🔧 Paquetes críticos:\n")
todos_ok <- TRUE
for (paquete in paquetes_criticos) {
  if (require(paquete, character.only = TRUE, quietly = TRUE)) {
    cat("✅", paquete, "\n")
  } else {
    cat("❌", paquete, "\n")
    todos_ok <- FALSE
  }
}

# Verificar TinyTeX
cat("\n📄 TinyTeX:\n")
if (require("tinytex", quietly = TRUE) && tinytex::is_tinytex()) {
  cat("✅ TinyTeX operativo\n")
} else {
  cat("❌ TinyTeX no configurado\n")
  todos_ok <- FALSE
}

# Verificar Python
cat("\n🐍 Python:\n")
if (require("reticulate", quietly = TRUE)) {
  tryCatch({
    config <- py_config()
    cat("✅ Python:", config$python, "\n")
    
    if (py_module_available("matplotlib")) {
      cat("✅ matplotlib disponible\n")
    } else {
      cat("❌ matplotlib no disponible\n")
      todos_ok <- FALSE
    }
  }, error = function(e) {
    cat("❌ Error Python:", e$message, "\n")
    todos_ok <- FALSE
  })
}

# Test de compilación
cat("\n🧪 Test de compilación:\n")
tryCatch({
  library(exams)
  
  # Crear ejercicio de prueba
  test_content <- '
Question
========
¿Cuánto es 2 + 2?

Answerlist
----------
- 3
- 4
- 5

Solution
========
La respuesta es 4.

Answerlist
----------
- Falso
- Verdadero
- Falso

Meta-information
================
exname: test_instalacion
extype: schoice
exsolution: 010
'
  
  writeLines(test_content, "/tmp/test_exercise.Rmd")
  result <- exams2html("/tmp/test_exercise.Rmd", n = 1, name = "/tmp/test_output")
  
  if (file.exists("/tmp/test_output1.html")) {
    cat("✅ Compilación R-exams exitosa\n")
    file.remove("/tmp/test_output1.html")
  } else {
    cat("❌ Compilación falló\n")
    todos_ok <- FALSE
  }
  
  file.remove("/tmp/test_exercise.Rmd")
  
}, error = function(e) {
  cat("❌ Error compilación:", e$message, "\n")
  todos_ok <- FALSE
})

cat("\n" %R% "=" %R% 40, "\n")
if (todos_ok) {
  cat("🎯 INSTALACIÓN EXITOSA - SISTEMA LISTO\n")
} else {
  cat("⚠️ INSTALACIÓN CON PROBLEMAS - REVISAR ERRORES\n")
}
cat("=" %R% 40, "\n")
EOF

chmod +x /tmp/validate_installation.R
R --vanilla < /tmp/validate_installation.R

# Limpiar archivos temporales
rm -f /tmp/install_r_packages.R /tmp/setup_tinytex.R /tmp/setup_python.R /tmp/validate_installation.R

echo ""
echo "🎯 INSTALACIÓN COMPLETADA"
echo "========================="
echo ""
log_success "RStudio instalado y configurado para R-exams ICFES"
log_info "Puedes iniciar RStudio con: rstudio"
log_info "Documentación completa en: docs/arquitectura-instalacion-rstudio.md"
echo ""
log_warning "PRÓXIMOS PASOS:"
echo "1. Abrir RStudio"
echo "2. Configurar Tools > Global Options según documentación"
echo "3. Clonar el proyecto R-exams ICFES"
echo "4. Probar compilación de ejemplos funcionales"
echo ""
log_success "¡Sistema listo para desarrollo R-exams ICFES!"