# 📚 Tutorial Completo: Instalación y Configuración de R en VSCode (Manjaro/Arch Linux)

## 🎯 Objetivo
Este tutorial te guiará paso a paso para instalar y configurar correctamente R con VSCode en sistemas Manjaro/Arch Linux, incluyendo la integración con Python para proyectos de r-exams.

## 📋 Requisitos Previos
- Sistema Manjaro/Arch Linux
- VSCode instalado
- Acceso a terminal con permisos sudo
- Conexión a internet

## 🔍 Verificación del Estado Inicial

### Paso 1: Verificar Instalaciones Existentes
```bash
# Verificar R
which R && R --version

# Verificar Python
which python3 && python3 --version

# Verificar extensiones de VSCode para R
code --list-extensions | grep -i r
```

## 📦 Instalación de Componentes Base

### Paso 2: Instalar R (si no está instalado)
```bash
# Instalar R usando pamac
pamac install r --no-confirm
```

### Paso 3: Instalar Python pip
```bash
# Instalar pip para Python
pamac install python-pip --no-confirm
```

### Paso 4: Instalar Paquetes Python Esenciales
```bash
# Instalar paquetes Python necesarios para r-exams
pamac install python-matplotlib python-numpy python-pandas --no-confirm
```

## 🔧 Configuración de R

### Paso 5: Crear Biblioteca Personal de R
```bash
# Crear directorio para biblioteca personal
mkdir -p ~/R/library
```

### Paso 6: Configurar Variables de Entorno R
```bash
# Crear archivo .Renviron
echo 'R_LIBS_USER="~/R/library"' > ~/.Renviron
```

### Paso 7: Instalar Paquetes R Esenciales
```bash
# Ejecutar instalación de paquetes R
R --slave -e "
# Configurar biblioteca personal
.libPaths(c('~/R/library', .libPaths()))

# Lista de paquetes necesarios
required_packages <- c('exams', 'reticulate', 'knitr', 'rmarkdown', 'digest', 'testthat')

# Instalar paquetes faltantes
missing_packages <- required_packages[!required_packages %in% installed.packages()[,'Package']]

if(length(missing_packages) > 0) {
  cat('Instalando paquetes:', paste(missing_packages, collapse=', '), '\n')
  install.packages(missing_packages, repos='https://cran.r-project.org', lib='~/R/library')
}

# Verificar instalación
cat('\nPaquetes instalados:\n')
for(pkg in required_packages) {
  if(pkg %in% installed.packages()[,'Package']) {
    cat('✓', pkg, '\n')
  } else {
    cat('✗', pkg, '\n')
  }
}
"
```

## ⚙️ Configuración de VSCode

### Paso 8: Crear Configuración de VSCode
```bash
# Crear directorio .vscode si no existe
mkdir -p .vscode

# Crear archivo de configuración settings.json
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
```

### Paso 9: Crear Archivo .Rprofile
```bash
# Crear configuración R personalizada
cat > .Rprofile << 'EOF'
# Configuración R para VSCode
# Este archivo se carga automáticamente al iniciar R

# Configurar biblioteca personal
.libPaths(c("~/R/library", .libPaths()))

# Configurar opciones de R
options(
  repos = c(CRAN = "https://cran.r-project.org"),
  scipen = 999,
  digits = 4,
  width = 120,
  warn = 1,
  OutDec = "."
)

# Configurar encoding
options(encoding = "UTF-8")

# Función para cargar paquetes comunes
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

# Configurar Python para reticulate
if (require("reticulate", quietly = TRUE)) {
  use_python("/usr/bin/python3", required = FALSE)
}

# Mensaje de bienvenida
cat("R configurado para VSCode\n")
cat("Biblioteca personal:", .libPaths()[1], "\n")
cat("Usa load_common_packages() para cargar paquetes comunes\n")
EOF
```

## 🧪 Verificación y Pruebas

### Paso 10: Crear Script de Prueba
```bash
# Crear script de verificación
cat > test_instalacion.R << 'EOF'
# Script de verificación de instalación R + VSCode

# Cargar paquetes comunes
load_common_packages()

# Probar integración Python-R
cat("\n=== Probando integración Python-R ===\n")
library(reticulate)
use_python("/usr/bin/python3")

py_run_string("
import matplotlib.pyplot as plt
import numpy as np

# Crear datos de ejemplo
x = np.linspace(0, 10, 100)
y = np.sin(x)

# Crear gráfico
plt.figure(figsize=(8, 6))
plt.plot(x, y, 'b-', linewidth=2)
plt.title('Gráfico de prueba - Función seno')
plt.xlabel('x')
plt.ylabel('sin(x)')
plt.grid(True)
plt.savefig('test_plot.png', dpi=150, bbox_inches='tight')
plt.close()

print('✓ Gráfico Python generado exitosamente')
")

# Crear gráfico R nativo
x <- 1:10
y <- x^2

png("test_plot_r.png", width=800, height=600)
plot(x, y,
     type = "b",
     main = "Gráfico de prueba R nativo",
     xlab = "X",
     ylab = "Y = X²",
     col = "blue",
     pch = 16)
lines(x, y, col = "red", lwd = 2)
dev.off()

cat("✓ Gráfico R nativo generado exitosamente\n")

# Información del sistema
cat("\n=== INFORMACIÓN DEL SISTEMA ===\n")
cat("Versión de R:", R.version.string, "\n")
cat("Biblioteca personal:", .libPaths()[1], "\n")
cat("Python configurado:", py_config()$python, "\n")

cat("\n🎉 ¡Instalación verificada exitosamente!\n")
EOF
```

### Paso 11: Ejecutar Prueba
```bash
# Ejecutar script de verificación
Rscript test_instalacion.R
```

## 🔌 Instalación de Extensiones VSCode

### Paso 12: Instalar Extensiones Manualmente
1. Abrir VSCode
2. Ir a Extensiones (Ctrl+Shift+X)
3. Buscar e instalar:
   - **R Extension for Visual Studio Code** (REditorSupport.r)
   - **R LSP Client** (REditorSupport.r-lsp) [Opcional]
   - **R Debugger** (RDebugger.r-debugger) [Opcional]

### Paso 13: Alternativa por Línea de Comandos
```bash
# Intentar instalación automática (puede fallar por conectividad)
code --install-extension REditorSupport.r
code --install-extension REditorSupport.r-lsp
```

## 🚀 Uso y Comandos Útiles

### Comandos R Esenciales
```bash
# Abrir R en terminal
R

# Ejecutar script R
Rscript archivo.R

# Verificar paquetes instalados
R -e "installed.packages()[,'Package']"

# Verificar configuración de biblioteca
R -e ".libPaths()"
```

### Atajos VSCode para R
- `Ctrl+Enter`: Ejecutar línea/selección actual
- `Ctrl+Shift+S`: Ejecutar archivo completo
- `Ctrl+Shift+P`: Paleta de comandos
- `Ctrl+Shift+K`: Renderizar R Markdown

## 🔧 Solución de Problemas Comunes

### Error "R Tools client: couldn't create connection to server"
**Solución:** Ya está configurado con `"r.lsp.enabled": false`

### Paquetes no encontrados
```bash
# Verificar biblioteca personal
R -e ".libPaths()"

# Reinstalar paquete específico
R -e "install.packages('nombre_paquete', lib='~/R/library')"
```

### Python no detectado por reticulate
```bash
# Verificar Python
which python3

# Configurar en R
R -e "reticulate::use_python('/usr/bin/python3')"
```

## 📁 Estructura de Archivos Creados
```
proyecto/
├── .vscode/
│   └── settings.json          # Configuración VSCode
├── .Rprofile                  # Configuración R automática
├── test_instalacion.R        # Script de verificación
├── test_plot.png             # Gráfico Python generado
└── test_plot_r.png           # Gráfico R generado
```

## ✅ Lista de Verificación Final
- [ ] R instalado y funcionando
- [ ] Python instalado con matplotlib, numpy, pandas
- [ ] Biblioteca personal R configurada
- [ ] Paquetes R esenciales instalados (exams, reticulate, knitr, rmarkdown)
- [ ] VSCode configurado para R
- [ ] Extensión R de VSCode instalada
- [ ] Integración Python-R funcionando
- [ ] Scripts de prueba ejecutados exitosamente

## 🎯 Próximos Pasos
1. Crear tu primer proyecto r-exams
2. Configurar git para control de versiones
3. Explorar funcionalidades avanzadas de r-exams

## 🐧 Adaptación para Otras Distribuciones

### Ubuntu/Debian
```bash
# Instalar R
sudo apt update
sudo apt install r-base r-base-dev

# Instalar Python y pip
sudo apt install python3 python3-pip

# Instalar paquetes Python
sudo apt install python3-matplotlib python3-numpy python3-pandas
```

### Fedora/CentOS/RHEL
```bash
# Instalar R
sudo dnf install R R-devel

# Instalar Python y pip
sudo dnf install python3 python3-pip

# Instalar paquetes Python
sudo dnf install python3-matplotlib python3-numpy python3-pandas
```

### openSUSE
```bash
# Instalar R
sudo zypper install R-base R-base-devel

# Instalar Python y pip
sudo zypper install python3 python3-pip

# Instalar paquetes Python
sudo zypper install python3-matplotlib python3-numpy python3-pandas
```

---
**Nota:** Este tutorial está optimizado para Manjaro/Arch Linux, pero incluye adaptaciones para las principales distribuciones Linux.
