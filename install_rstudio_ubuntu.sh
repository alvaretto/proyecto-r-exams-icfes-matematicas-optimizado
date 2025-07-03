#!/bin/bash

# 🏗️ Script de Instalación RStudio para R-exams ICFES - Ubuntu 24.04
# Adaptado específicamente para Ubuntu y el proyecto de matemáticas ICFES

set -e  # Salir en caso de error

echo "🎯 INSTALACIÓN RSTUDIO PARA PROYECTO R-EXAMS ICFES (UBUNTU)"
echo "=========================================================="
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

# Verificar que estamos en Ubuntu
if ! command_exists apt; then
    log_error "Este script está diseñado para Ubuntu/Debian"
    exit 1
fi

log_info "Detectado sistema Ubuntu $(lsb_release -rs) ✅"

# FASE 1: Actualización del sistema
log_info "FASE 1: Actualizando sistema base..."
sudo apt update && sudo apt upgrade -y

# FASE 2: Dependencias del sistema
log_info "FASE 2: Instalando dependencias del sistema..."

# Instalar dependencias básicas
sudo apt install -y \
    software-properties-common \
    dirmngr \
    wget \
    curl \
    gpg \
    ca-certificates

# Agregar repositorio CRAN para R actualizado
log_info "Configurando repositorio CRAN..."
wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
echo "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/" | sudo tee -a /etc/apt/sources.list.d/cran-r.list

# Actualizar después de agregar repositorio
sudo apt update

# Instalar R y dependencias del sistema
SYSTEM_PACKAGES=(
    "r-base"
    "r-base-dev"
    "gfortran"
    "texlive-full"
    "pandoc"
    "imagemagick"
    "libpng-dev"
    "libcairo2-dev"
    "python3"
    "python3-pip"
    "python3-dev"
    "libxml2-dev"
    "libssl-dev"
    "libcurl4-openssl-dev"
    "libfontconfig1-dev"
    "libharfbuzz-dev"
    "libfribidi-dev"
    "libfreetype6-dev"
    "libtiff5-dev"
    "libjpeg-dev"
    "git"
)

log_info "Instalando paquetes del sistema..."
for package in "${SYSTEM_PACKAGES[@]}"; do
    log_info "Instalando $package..."
    sudo apt install -y "$package"
done

log_success "Dependencias del sistema instaladas"

# FASE 3: Instalación de RStudio
log_info "FASE 3: Instalando RStudio Desktop..."

if command_exists rstudio; then
    log_success "RStudio ya está instalado"
else
    log_info "Descargando e instalando RStudio..."
    
    # Detectar arquitectura
    ARCH=$(dpkg --print-architecture)
    
    if [ "$ARCH" = "amd64" ]; then
        RSTUDIO_URL="https://download1.rstudio.org/electron/jammy/amd64/rstudio-2024.12.0-467-amd64.deb"
    elif [ "$ARCH" = "arm64" ]; then
        RSTUDIO_URL="https://download1.rstudio.org/electron/jammy/arm64/rstudio-2024.12.0-467-arm64.deb"
    else
        log_error "Arquitectura no soportada: $ARCH"
        exit 1
    fi
    
    # Descargar e instalar RStudio
    cd /tmp
    wget "$RSTUDIO_URL" -O rstudio.deb
    sudo dpkg -i rstudio.deb || sudo apt-get install -f -y
    rm rstudio.deb
    
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
  cat("Reintentando instalación de paquetes con errores...\n")
  
  # Reintentar con configuración alternativa
  for (paquete in errores) {
    cat("Reintentando:", paquete, "...")
    tryCatch({
      install.packages(paquete, dependencies = TRUE, type = "source")
      cat(" ✅\n")
    }, error = function(e) {
      cat(" ❌ (persistente)\n")
    })
  }
} else {
  cat("✅ Todos los paquetes R instalados correctamente\n")
}
EOF

chmod +x /tmp/install_r_packages.R
Rscript /tmp/install_r_packages.R

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
  
  # Esperar a que se complete la instalación
  Sys.sleep(5)
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
  "fontspec", "xunicode",
  "hyperref", "url", "graphicx", "listings"
)

if (is_tinytex()) {
  cat("📦 Instalando paquetes LaTeX...\n")
  tryCatch({
    tlmgr_install(paquetes_latex)
    cat("✅ Paquetes LaTeX instalados\n")
  }, error = function(e) {
    cat("⚠️ Algunos paquetes LaTeX pueden no haberse instalado:", e$message, "\n")
  })
} else {
  cat("❌ TinyTeX no se instaló correctamente\n")
}
EOF

chmod +x /tmp/setup_tinytex.R
Rscript /tmp/setup_tinytex.R

log_success "TinyTeX configurado"

# FASE 7: Configuración de Python
log_info "FASE 7: Configurando integración Python..."

# Instalar paquetes Python del sistema
sudo apt install -y python3-numpy python3-matplotlib python3-pandas python3-scipy

cat > /tmp/setup_python.R << 'EOF'
#!/usr/bin/env Rscript

library(reticulate)

# Configurar Python
python_path <- Sys.which("python3")

if (python_path != "") {
  use_python(python_path, required = TRUE)
  cat("🐍 Python configurado en:", python_path, "\n")
  
  # Verificar paquetes Python críticos
  paquetes_python <- c("numpy", "matplotlib", "pandas", "scipy")
  
  for (paquete in paquetes_python) {
    if (py_module_available(paquete)) {
      cat("✅ Python package disponible:", paquete, "\n")
    } else {
      cat("⚠️ Python package no disponible:", paquete, "\n")
      # Intentar instalar con pip
      tryCatch({
        py_install(paquete, pip = TRUE)
        cat("✅ Instalado:", paquete, "\n")
      }, error = function(e) {
        cat("❌ Error instalando:", paquete, "\n")
      })
    }
  }
} else {
  cat("❌ Python no encontrado\n")
}
EOF

chmod +x /tmp/setup_python.R
Rscript /tmp/setup_python.R

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

cat("\n", paste(rep("=", 40), collapse=""), "\n")
if (todos_ok) {
  cat("🎯 INSTALACIÓN EXITOSA - SISTEMA LISTO\n")
} else {
  cat("⚠️ INSTALACIÓN CON PROBLEMAS - REVISAR ERRORES\n")
}
cat(paste(rep("=", 40), collapse=""), "\n")
EOF

chmod +x /tmp/validate_installation.R
Rscript /tmp/validate_installation.R

# Limpiar archivos temporales
rm -f /tmp/install_r_packages.R /tmp/setup_tinytex.R /tmp/setup_python.R /tmp/validate_installation.R

echo ""
echo "🎯 INSTALACIÓN COMPLETADA"
echo "========================="
echo ""
log_success "RStudio instalado y configurado para R-exams ICFES en Ubuntu"
log_info "Puedes iniciar RStudio con: rstudio"
log_info "Documentación completa en: docs/arquitectura-instalacion-rstudio.md"
echo ""
log_warning "PRÓXIMOS PASOS:"
echo "1. Abrir RStudio"
echo "2. Configurar Tools > Global Options según documentación"
echo "3. Probar compilación de ejemplos funcionales del proyecto"
echo ""
log_success "¡Sistema listo para desarrollo R-exams ICFES!"