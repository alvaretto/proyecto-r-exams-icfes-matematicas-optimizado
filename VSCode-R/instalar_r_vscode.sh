#!/bin/bash

# Script de instalación automática de R + VSCode para Manjaro/Arch Linux
# Autor: Augment Agent
# Fecha: $(date)

set -e  # Salir si hay errores

# Colores para output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Función para imprimir mensajes
print_step() {
    echo -e "${BLUE}[PASO $1]${NC} $2"
}

print_success() {
    echo -e "${GREEN}✓${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}⚠${NC} $1"
}

print_error() {
    echo -e "${RED}✗${NC} $1"
}

# Función para verificar si un comando existe
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

echo "🚀 Iniciando instalación de R + VSCode para Manjaro/Arch Linux"
echo "================================================================"

# Paso 1: Verificar estado inicial
print_step "1" "Verificando estado inicial del sistema"

if command_exists R; then
    R_VERSION=$(R --version | head -n1)
    print_success "R ya está instalado: $R_VERSION"
else
    print_warning "R no está instalado, se instalará"
    pamac install r --no-confirm
    print_success "R instalado correctamente"
fi

if command_exists python3; then
    PYTHON_VERSION=$(python3 --version)
    print_success "Python ya está instalado: $PYTHON_VERSION"
else
    print_error "Python3 no está instalado. Instálalo manualmente."
    exit 1
fi

# Paso 2: Instalar pip si no existe
print_step "2" "Verificando e instalando pip"
if ! command_exists pip; then
    pamac install python-pip --no-confirm
    print_success "pip instalado"
else
    print_success "pip ya está disponible"
fi

# Paso 3: Instalar paquetes Python
print_step "3" "Instalando paquetes Python esenciales"
pamac install python-matplotlib python-numpy python-pandas --no-confirm
print_success "Paquetes Python instalados"

# Paso 4: Crear biblioteca personal R
print_step "4" "Configurando biblioteca personal de R"
mkdir -p ~/R/library
echo 'R_LIBS_USER="~/R/library"' > ~/.Renviron
print_success "Biblioteca personal configurada"

# Paso 5: Instalar paquetes R
print_step "5" "Instalando paquetes R esenciales"
R --slave -e "
.libPaths(c('~/R/library', .libPaths()))
required_packages <- c('exams', 'reticulate', 'knitr', 'rmarkdown', 'digest', 'testthat')
missing_packages <- required_packages[!required_packages %in% installed.packages()[,'Package']]
if(length(missing_packages) > 0) {
  install.packages(missing_packages, repos='https://cran.r-project.org', lib='~/R/library')
}
cat('Paquetes R instalados correctamente\n')
"
print_success "Paquetes R instalados"

# Paso 6: Configurar VSCode
print_step "6" "Configurando VSCode para R"
mkdir -p .vscode

cat > .vscode/settings.json << 'EOF'
{
    "r.lsp.enabled": false,
    "r.lsp.promptToInstall": false,
    "r.rterm.linux": "/usr/bin/R",
    "r.rpath.linux": "/usr/bin/R",
    "r.libPaths": ["~/R/library"],
    "r.rterm.option": [
        "--no-save",
        "--no-restore",
        "--quiet"
    ],
    "r.sessionWatcher": true,
    "r.bracketedPaste": true,
    "r.plot.useHttpgd": false,
    "r.plot.useViewer": true,
    "r.alwaysUseActiveTerminal": true,
    "r.rtermSendDelay": 8,
    "files.associations": {
        "*.R": "r",
        "*.r": "r",
        "*.Rmd": "rmd",
        "*.rmd": "rmd"
    },
    "editor.wordWrap": "on",
    "editor.rulers": [80, 120],
    "editor.tabSize": 2,
    "editor.insertSpaces": true,
    "[r]": {
        "editor.defaultFormatter": "REditorSupport.r",
        "editor.formatOnSave": false,
        "editor.tabSize": 2
    },
    "[rmd]": {
        "editor.defaultFormatter": "REditorSupport.r",
        "editor.formatOnSave": false,
        "editor.tabSize": 2,
        "editor.wordWrap": "on"
    }
}
EOF

print_success "Configuración VSCode creada"

# Paso 7: Crear .Rprofile
print_step "7" "Creando configuración R personalizada"
cat > .Rprofile << 'EOF'
# Configuración R para VSCode
.libPaths(c("~/R/library", .libPaths()))

options(
  repos = c(CRAN = "https://cran.r-project.org"),
  scipen = 999,
  digits = 4,
  width = 120,
  warn = 1,
  OutDec = "."
)

options(encoding = "UTF-8")

load_common_packages <- function() {
  packages <- c("exams", "reticulate", "knitr", "rmarkdown")
  for (pkg in packages) {
    if (require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat("✓", pkg, "cargado\n")
    } else {
      cat("✗", pkg, "no disponible\n")
    }
  }
}

if (require("reticulate", quietly = TRUE)) {
  use_python("/usr/bin/python3", required = FALSE)
}

cat("R configurado para VSCode\n")
cat("Biblioteca personal:", .libPaths()[1], "\n")
cat("Usa load_common_packages() para cargar paquetes comunes\n")
EOF

print_success "Archivo .Rprofile creado"

# Paso 8: Crear script de prueba
print_step "8" "Creando script de verificación"
cat > verificar_instalacion.R << 'EOF'
# Script de verificación automática
load_common_packages()

# Probar Python-R
library(reticulate)
use_python("/usr/bin/python3")

py_run_string("
import matplotlib.pyplot as plt
import numpy as np
x = np.linspace(0, 10, 100)
y = np.sin(x)
plt.figure(figsize=(8, 6))
plt.plot(x, y, 'b-', linewidth=2)
plt.title('Verificación: Gráfico Python')
plt.savefig('verificacion_python.png', dpi=150, bbox_inches='tight')
plt.close()
print('✓ Gráfico Python generado')
")

# Probar R nativo
x <- 1:10
y <- x^2
png("verificacion_r.png", width=800, height=600)
plot(x, y, type="b", main="Verificación: Gráfico R", col="blue", pch=16)
dev.off()

cat("✓ Gráfico R generado\n")
cat("✓ Verificación completada exitosamente\n")
cat("Versión R:", R.version.string, "\n")
EOF

print_success "Script de verificación creado"

# Paso 9: Ejecutar verificación
print_step "9" "Ejecutando verificación final"
Rscript verificar_instalacion.R
print_success "Verificación completada"

# Paso 10: Intentar instalar extensiones VSCode
print_step "10" "Intentando instalar extensiones VSCode"
if command_exists code; then
    code --install-extension REditorSupport.r 2>/dev/null || print_warning "Instala manualmente la extensión R en VSCode"
    print_success "Proceso de extensiones completado"
else
    print_warning "VSCode no encontrado en PATH. Instala las extensiones manualmente."
fi

echo ""
echo "🎉 ¡INSTALACIÓN COMPLETADA EXITOSAMENTE!"
echo "========================================"
echo ""
echo "📋 RESUMEN:"
echo "✓ R configurado con biblioteca personal"
echo "✓ Paquetes R esenciales instalados"
echo "✓ Paquetes Python instalados"
echo "✓ VSCode configurado para R"
echo "✓ Integración Python-R funcionando"
echo ""
echo "📁 ARCHIVOS CREADOS:"
echo "- .vscode/settings.json (configuración VSCode)"
echo "- .Rprofile (configuración R)"
echo "- verificar_instalacion.R (script de prueba)"
echo "- verificacion_python.png (gráfico de prueba Python)"
echo "- verificacion_r.png (gráfico de prueba R)"
echo ""
echo "🚀 PRÓXIMOS PASOS:"
echo "1. Abre VSCode: code ."
echo "2. Instala manualmente la extensión 'R Extension for Visual Studio Code'"
echo "3. Abre verificar_instalacion.R y prueba ejecutar código"
echo ""
echo "📚 TUTORIAL COMPLETO: TUTORIAL_INSTALACION_R_VSCODE.md"
