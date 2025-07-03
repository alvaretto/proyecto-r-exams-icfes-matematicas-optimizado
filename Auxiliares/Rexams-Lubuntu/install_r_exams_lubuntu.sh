#!/bin/bash

# Script de instalación completa para R-exams ICFES en Lubuntu
# Basado en el proyecto: proyecto-r-exams-icfes-matematicas-optimizado

echo "🎯 Instalación completa de R-exams para proyecto ICFES Matemáticas"
echo "=================================================================="

# Colores para output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Función para mostrar progreso
show_progress() {
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

# Verificar si es Ubuntu/Debian
if ! command -v apt &> /dev/null; then
    show_error "Este script está diseñado para sistemas basados en Ubuntu/Debian"
    exit 1
fi

# 1. Actualizar el sistema
show_progress "Actualizando el sistema..."
sudo apt update && sudo apt upgrade -y

# 2. Instalar dependencias del sistema base
show_progress "Instalando dependencias del sistema..."
sudo apt install -y \
    curl \
    wget \
    git \
    build-essential \
    gfortran \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libgit2-dev \
    libssh2-1-dev \
    zlib1g-dev \
    libbz2-dev \
    liblzma-dev \
    libpcre2-dev \
    libreadline-dev \
    libsqlite3-dev \
    libncurses5-dev \
    libncursesw5-dev \
    tk-dev \
    libffi-dev \
    libblas-dev \
    liblapack-dev \
    libatlas-base-dev

# 3. Instalar R
show_progress "Instalando R..."
# Agregar repositorio CRAN para obtener la versión más reciente
wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
echo "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/" | sudo tee -a /etc/apt/sources.list.d/cran-r.list
sudo apt update
sudo apt install -y r-base r-base-dev r-recommended

# 4. Instalar LaTeX (TeX Live completo para compatibilidad total)
show_progress "Instalando LaTeX (TeX Live)..."
sudo apt install -y \
    texlive-full \
    texlive-xetex \
    texlive-luatex \
    texlive-lang-spanish \
    lmodern

# 5. Instalar Pandoc
show_progress "Instalando Pandoc..."
sudo apt install -y pandoc pandoc-citeproc

# 6. Instalar Python y dependencias
show_progress "Instalando Python y dependencias..."
sudo apt install -y \
    python3 \
    python3-pip \
    python3-dev \
    python3-venv \
    python3-matplotlib \
    python3-numpy \
    python3-pandas \
    python3-scipy \
    python3-seaborn

# 7. Instalar herramientas adicionales
show_progress "Instalando herramientas adicionales..."
sudo apt install -y \
    imagemagick \
    ghostscript \
    poppler-utils \
    qpdf \
    librsvg2-bin \
    inkscape \
    graphviz

# 8. Configurar directorio de biblioteca R de usuario
show_progress "Configurando directorio de biblioteca R..."
mkdir -p ~/R/library
echo 'R_LIBS_USER=~/R/library' > ~/.Renviron

# 9. Crear script de instalación de paquetes R
show_progress "Creando script de instalación de paquetes R..."
cat > install_r_packages.R << 'EOF'
#!/usr/bin/env Rscript

# Configurar repositorio CRAN
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Función para instalar paquetes con manejo de errores
install_package_safe <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Instalando", pkg, "...\n")
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
  # Paquetes principales
  "exams",
  "knitr", 
  "rmarkdown",
  
  # Manipulación de datos
  "dplyr",
  "tidyr", 
  "data.table",
  "readxl",
  "jsonlite",
  
  # Visualización
  "ggplot2",
  "scales",
  "gridExtra",
  "RColorBrewer",
  
  # Estadística
  "MASS",
  "car",
  "psych",
  
  # LaTeX y documentos
  "tinytex",
  "pdftools",
  "qpdf",
  
  # Integración Python
  "reticulate",
  
  # Herramientas adicionales
  "devtools",
  "testthat",
  "digest",
  "magick",
  "webshot",
  "htmltools",
  "base64enc"
)

# Instalar paquetes
cat("Instalando paquetes R necesarios para R-exams ICFES...\n")
cat("=====================================================\n")

for (pkg in packages) {
  install_package_safe(pkg)
}

cat("\n🎯 Instalación de paquetes R completada\n")
EOF

# 10. Ejecutar instalación de paquetes R
show_progress "Instalando paquetes R..."
chmod +x install_r_packages.R
Rscript install_r_packages.R

# 11. Configurar TinyTeX (alternativa ligera a TeX Live completo)
show_progress "Configurando TinyTeX como alternativa..."
cat > setup_tinytex.R << 'EOF'
#!/usr/bin/env Rscript

# Instalar TinyTeX si no está disponible TeX Live
if (!tinytex::is_tinytex()) {
  cat("Instalando TinyTeX...\n")
  tinytex::install_tinytex(force = TRUE)
  
  # Instalar paquetes LaTeX adicionales necesarios
  tinytex::tlmgr_install(c(
    "amsmath", "amsfonts", "amssymb", "babel", "babel-spanish",
    "booktabs", "colortbl", "enumitem", "environ", "eurosym",
    "fancyhdr", "fancyvrb", "float", "fontspec", "geometry",
    "graphics", "hyperref", "listings", "mathtools",
    "microtype", "multirow", "parskip", "pgf", "setspace",
    "subfig", "tikz", "tools", "ulem", "url", "xcolor",
    "xkeyval", "zapfding", "framed", "mdframed"
  ))
}
EOF

chmod +x setup_tinytex.R
# Rscript setup_tinytex.R  # Comentado porque ya instalamos texlive-full

# 12. Configurar Python para reticulate
show_progress "Configurando Python para reticulate..."
cat > setup_python_reticulate.R << 'EOF'
#!/usr/bin/env Rscript

library(reticulate)

# Configurar Python del sistema
python_path <- "/usr/bin/python3"
if (file.exists(python_path)) {
  use_python(python_path, required = TRUE)
  cat("✓ Python configurado en:", python_path, "\n")
  
  # Verificar configuración
  config <- py_config()
  cat("Versión de Python:", config$version, "\n")
  cat("Ejecutable:", config$python, "\n")
} else {
  cat("✗ No se encontró Python en", python_path, "\n")
}
EOF

chmod +x setup_python_reticulate.R
Rscript setup_python_reticulate.R

# 13. Crear script de verificación
show_progress "Creando script de verificación..."
cat > verify_installation.R << 'EOF'
#!/usr/bin/env Rscript

cat("🎯 Verificación de instalación R-exams ICFES\n")
cat("===========================================\n\n")

# Verificar R
cat("📊 R:\n")
cat("  Versión:", R.version.string, "\n")
cat("  Rutas de biblioteca:", paste(.libPaths(), collapse = ", "), "\n\n")

# Verificar paquetes principales
required_packages <- c("exams", "knitr", "rmarkdown", "ggplot2", "dplyr", "reticulate")
cat("📦 Paquetes principales:\n")
for (pkg in required_packages) {
  status <- if (require(pkg, character.only = TRUE, quietly = TRUE)) "✓ Instalado" else "✗ Faltante"
  cat("  ", pkg, ":", status, "\n")
}

# Verificar LaTeX
cat("\n📄 LaTeX:\n")
if (require("tinytex", quietly = TRUE)) {
  if (tinytex::is_tinytex()) {
    cat("  TinyTeX: ✓ Instalado\n")
  } else {
    cat("  TeX Live: ✓ Disponible (sistema)\n")
  }
} else {
  cat("  Estado: Verificar manualmente con 'pdflatex --version'\n")
}

# Verificar Python
cat("\n🐍 Python:\n")
if (require("reticulate", quietly = TRUE)) {
  tryCatch({
    config <- py_config()
    cat("  Versión:", config$version, "\n")
    cat("  Ejecutable:", config$python, "\n")
    
    # Verificar módulos Python
    modules <- c("numpy", "matplotlib", "pandas")
    for (mod in modules) {
      status <- if (py_module_available(mod)) "✓" else "✗"
      cat("  ", mod, ":", status, "\n")
    }
  }, error = function(e) {
    cat("  Error:", e$message, "\n")
  })
} else {
  cat("  reticulate no disponible\n")
}

cat("\n🎯 Verificación completada\n")
EOF

chmod +x verify_installation.R

# 14. Ejecutar verificación
show_progress "Ejecutando verificación final..."
Rscript verify_installation.R

# 15. Crear script de prueba con R-exams
show_progress "Creando script de prueba..."
cat > test_r_exams.R << 'EOF'
#!/usr/bin/env Rscript

library(exams)

# Crear un ejercicio de prueba simple
cat("Creando ejercicio de prueba...\n")

# Crear directorio de prueba
dir.create("test_exams", showWarnings = FALSE)

# Crear ejercicio simple
exercise_content <- '
---
output: 
  exams::exams2pdf:
    template: plain
---

# Ejercicio de Prueba - Matemáticas ICFES

¿Cuál es el resultado de $2 + 3$?

```{r, echo=FALSE, results="asis"}
# Generar opciones
correct <- 5
options <- c(correct, 3, 4, 6, 7)
options <- sample(options)

# Encontrar posición correcta
correct_pos <- which(options == correct)

# Mostrar opciones
for(i in 1:length(options)) {
  cat("\\n", LETTERS[i], ") ", options[i], sep="")
}
```

Meta-information
================
extype: schoice
exsolution: `r paste(rep("0", 5), collapse="")`
exsolution: `r paste(replace(rep("0", 5), correct_pos, "1"), collapse="")`
exname: suma_basica
'

# Escribir archivo
writeLines(exercise_content, "test_exams/suma_basica.Rmd")

# Probar compilación
cat("Probando compilación PDF...\n")
tryCatch({
  exams2pdf("test_exams/suma_basica.Rmd", n = 1, name = "prueba_icfes")
  cat("✓ Prueba PDF exitosa\n")
}, error = function(e) {
  cat("✗ Error en prueba PDF:", e$message, "\n")
})

cat("🎯 Prueba completada. Revisa los archivos generados.\n")
EOF

chmod +x test_r_exams.R

# 16. Mostrar resumen final
show_success "Instalación completada!"
echo ""
echo "📋 RESUMEN DE INSTALACIÓN"
echo "========================"
echo "✓ R y paquetes necesarios"
echo "✓ LaTeX (TeX Live completo)"
echo "✓ Pandoc"
echo "✓ Python y módulos científicos"
echo "✓ Herramientas adicionales (ImageMagick, etc.)"
echo ""
echo "📁 ARCHIVOS CREADOS:"
echo "  - install_r_packages.R      (instalación paquetes R)"
echo "  - setup_tinytex.R           (configuración TinyTeX)"
echo "  - setup_python_reticulate.R (configuración Python)"
echo "  - verify_installation.R     (verificación sistema)"
echo "  - test_r_exams.R           (prueba R-exams)"
echo ""
echo "🚀 PRÓXIMOS PASOS:"
echo "1. Ejecutar: ./verify_installation.R"
echo "2. Ejecutar: ./test_r_exams.R"
echo "3. Navegar a tu proyecto: cd /home/rexams/Proyectos/proyecto-r-exams-icfes-matematicas-optimizado"
echo "4. Probar con un ejercicio real del proyecto"
echo ""
echo "📚 DOCUMENTACIÓN:"
echo "  - README del proyecto: /home/rexams/Proyectos/proyecto-r-exams-icfes-matematicas-optimizado/README.md"
echo "  - Tutorial: /home/rexams/Proyectos/proyecto-r-exams-icfes-matematicas-optimizado/Auxiliares/Instalaciones/"
echo ""
show_success "¡Tu entorno R-exams ICFES está listo!"