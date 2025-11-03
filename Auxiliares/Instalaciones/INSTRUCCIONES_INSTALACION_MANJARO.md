# 🚀 Guía de Instalación Completa - R-Exams ICFES en Manjaro Plasma KDE

**Proyecto:** RepositorioMatematicasICFES_R_Exams  
**Sistema Operativo:** Manjaro Plasma KDE  
**Fecha:** Noviembre 2025  
**Versión:** 1.0

---

## 📋 Tabla de Contenidos

1. [Requisitos Previos](#requisitos-previos)
2. [Instalación Automática](#instalación-automática)
3. [Instalación Manual](#instalación-manual)
4. [Verificación de la Instalación](#verificación-de-la-instalación)
5. [Configuración Post-Instalación](#configuración-post-instalación)
6. [Solución de Problemas](#solución-de-problemas)
7. [Pruebas de Funcionalidad](#pruebas-de-funcionalidad)

---

## 🎯 Requisitos Previos

### Sistema
- **SO:** Manjaro Linux (Plasma KDE)
- **Espacio en disco:** Mínimo 5 GB libres
- **RAM:** Mínimo 4 GB (recomendado 8 GB)
- **Conexión a Internet:** Requerida para descargar paquetes

### Permisos
- Acceso sudo para instalar paquetes del sistema
- Usuario con permisos de escritura en `/home/bootcamp/Proyectos-2026/`

---

## ⚡ Instalación Automática (RECOMENDADO)

### Paso 1: Descargar el Script de Instalación

El script de instalación automatizada está ubicado en:
```
Auxiliares/Instalaciones/install_manjaro_r_exams_complete.sh
```

### Paso 2: Dar Permisos de Ejecución

```bash
cd /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams
chmod +x Auxiliares/Instalaciones/install_manjaro_r_exams_complete.sh
```

### Paso 3: Ejecutar el Script

```bash
./Auxiliares/Instalaciones/install_manjaro_r_exams_complete.sh
```

### Paso 4: Seguir las Instrucciones en Pantalla

El script instalará automáticamente:
- ✅ R (versión más reciente)
- ✅ RStudio Desktop
- ✅ LaTeX (TeX Live completo)
- ✅ TinyTeX (para R)
- ✅ Python 3 con paquetes (matplotlib, numpy, pandas)
- ✅ Todos los paquetes R necesarios
- ✅ Integración Python-R (reticulate)
- ✅ Dependencias del sistema

**Tiempo estimado:** 20-40 minutos (dependiendo de la velocidad de Internet)

---

## 🔧 Instalación Manual

Si prefieres instalar componente por componente:

### 1. Actualizar el Sistema

```bash
sudo pacman -Syu
```

### 2. Instalar R

```bash
sudo pacman -S r
```

Verificar instalación:
```bash
R --version
```

### 3. Instalar RStudio Desktop

Primero, instalar `yay` si no está instalado:
```bash
sudo pacman -S base-devel git
cd /tmp
git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si
```

Luego instalar RStudio:
```bash
yay -S rstudio-desktop-bin
```

### 4. Instalar LaTeX (TeX Live)

```bash
sudo pacman -S texlive-core texlive-bin texlive-latexextra \
               texlive-mathscience texlive-pictures \
               texlive-fontsextra texlive-langspanish \
               texlive-xetex
```

### 5. Instalar Python y Paquetes

```bash
sudo pacman -S python python-pip
pip install --user matplotlib numpy pandas seaborn
```

### 6. Instalar Dependencias del Sistema

```bash
sudo pacman -S gcc-fortran pandoc imagemagick cairo pango \
               libpng libjpeg-turbo git curl wget libxml2 \
               openssl harfbuzz fribidi freetype2
```

### 7. Instalar Paquetes R

Crear archivo `install_packages.R`:

```r
#!/usr/bin/env Rscript

options(repos = c(CRAN = "https://cloud.r-project.org"))

packages <- c(
  "exams", "knitr", "rmarkdown", "reticulate",
  "tidyverse", "ggplot2", "data.table", "readxl",
  "tinytex", "pdftools", "testthat", "digest",
  "magick", "webshot", "htmltools", "base64enc"
)

install.packages(packages, dependencies = TRUE)
```

Ejecutar:
```bash
chmod +x install_packages.R
R --vanilla < install_packages.R
```

### 8. Configurar TinyTeX

```r
#!/usr/bin/env Rscript

library(tinytex)

# Instalar TinyTeX
install_tinytex(force = TRUE)

# Instalar paquetes LaTeX
tlmgr_install(c(
  "amsmath", "amsfonts", "amssymb", "mathtools",
  "babel", "babel-spanish", "booktabs", "colortbl",
  "tikz", "pgf", "pgfplots", "xcolor",
  "geometry", "fancyhdr", "fontspec", "hyperref"
))
```

---

## ✅ Verificación de la Instalación

### Método 1: Script de Verificación Automática

```bash
cd /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams
chmod +x Auxiliares/Instalaciones/verify_installation.sh
./Auxiliares/Instalaciones/verify_installation.sh
```

Este script verificará:
- ✓ Comandos principales (R, RStudio, Python, LaTeX)
- ✓ Paquetes del sistema
- ✓ Paquetes R
- ✓ Paquetes Python
- ✓ Integración Python-R
- ✓ Configuración del proyecto

### Método 2: Verificación Manual

#### Verificar R:
```bash
R --version
```

#### Verificar RStudio:
```bash
rstudio --version
```

#### Verificar Python:
```bash
python3 --version
python3 -c "import matplotlib, numpy, pandas; print('Paquetes Python OK')"
```

#### Verificar LaTeX:
```bash
pdflatex --version
xelatex --version
```

#### Verificar Paquetes R:
```r
R -e "library(exams); library(reticulate); library(knitr); print('Paquetes R OK')"
```

---

## ⚙️ Configuración Post-Instalación

### 1. Configurar .Rprofile del Proyecto

El script de instalación crea automáticamente un `.Rprofile` en el directorio del proyecto. Si necesitas crearlo manualmente:

```bash
cd /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams
```

Crear archivo `.Rprofile`:

```r
# Configuración R para Proyecto ICFES R-Exams

.libPaths(c("~/R/library", .libPaths()))

options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  scipen = 999,
  digits = 10,
  OutDec = "."
)

options(encoding = "UTF-8")

# Configurar Python
if (require("reticulate", quietly = TRUE)) {
  use_python("/usr/bin/python3", required = FALSE)
}

# Función para cargar paquetes comunes
load_icfes_packages <- function() {
  packages <- c("exams", "reticulate", "knitr", "rmarkdown", 
                "ggplot2", "tidyverse", "testthat")
  for (pkg in packages) {
    if (require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat("✓", pkg, "cargado\n")
    }
  }
}

cat("\n╔════════════════════════════════════════════════════════╗\n")
cat("║  Proyecto: RepositorioMatematicasICFES_R_Exams        ║\n")
cat("╚════════════════════════════════════════════════════════╝\n\n")
```

### 2. Configurar RStudio

1. Abrir RStudio
2. Ir a `Tools > Global Options`
3. Configurar:
   - **General > Default working directory:** `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams`
   - **Code > Saving > Default text encoding:** UTF-8
   - **Sweave > Weave Rnw files using:** knitr
   - **Sweave > Typeset LaTeX into PDF using:** XeLaTeX

### 3. Abrir el Proyecto en RStudio

```bash
cd /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams
rstudio RepositorioMatematicasICFES_R_Exams.Rproj &
```

O desde RStudio:
- `File > Open Project`
- Navegar a `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams`
- Seleccionar `RepositorioMatematicasICFES_R_Exams.Rproj`

---

## 🔍 Solución de Problemas

### Problema 1: RStudio no se instala desde AUR

**Solución:**
```bash
# Descargar manualmente el paquete .deb y convertir
yay -S debtap
sudo debtap -u
wget https://download1.rstudio.org/electron/jammy/amd64/rstudio-2024.09.0-375-amd64.deb
sudo debtap rstudio-2024.09.0-375-amd64.deb
sudo pacman -U rstudio-*.pkg.tar.zst
```

### Problema 2: Error al compilar paquetes R

**Solución:**
```bash
# Instalar herramientas de compilación
sudo pacman -S base-devel gcc-fortran
```

### Problema 3: TinyTeX no se instala correctamente

**Solución:**
Usar TeX Live del sistema en lugar de TinyTeX:
```bash
sudo pacman -S texlive-most
```

### Problema 4: Reticulate no encuentra Python

**Solución en R:**
```r
library(reticulate)
use_python("/usr/bin/python3", required = TRUE)
py_config()  # Verificar configuración
```

### Problema 5: Error de permisos al instalar paquetes R

**Solución:**
```bash
mkdir -p ~/R/library
```

En R:
```r
.libPaths(c("~/R/library", .libPaths()))
```

---

## 🧪 Pruebas de Funcionalidad

### Prueba 1: Compilar un Archivo .Rmd Simple

Crear archivo `test.Rmd`:

```markdown
---
output: html_document
---

```{r}
library(exams)
print("R-exams funcional")
```
```

Compilar:
```r
library(rmarkdown)
render("test.Rmd")
```

### Prueba 2: Probar Integración Python-R

```r
library(reticulate)
use_python("/usr/bin/python3")

# Ejecutar código Python desde R
py_run_string("
import matplotlib.pyplot as plt
import numpy as np

x = np.linspace(0, 10, 100)
y = np.sin(x)

plt.plot(x, y)
plt.savefig('test_plot.png')
")

# Verificar que se creó la imagen
file.exists("test_plot.png")
```

### Prueba 3: Compilar Ejercicio R-exams Completo

```bash
cd /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams
cd Auxiliares/Ejemplos-Funcionales-Rmd
```

En R:
```r
library(exams)

# Compilar a HTML
exams2html("nombre_ejercicio.Rmd", n = 1)

# Compilar a PDF
exams2pdf("nombre_ejercicio.Rmd", n = 1)

# Compilar a Moodle
exams2moodle("nombre_ejercicio.Rmd", n = 5)
```

---

## 📚 Recursos Adicionales

### Documentación del Proyecto
- `README.md` - Descripción general del proyecto
- `Auxiliares/Instalaciones/Tutorial Actualizado R-exams Mayo 2025.md`
- `Auxiliares/Ejemplos-Funcionales-Rmd/` - Ejemplos funcionales

### Documentación Externa
- [R-exams Official](http://www.r-exams.org/)
- [RStudio Documentation](https://docs.posit.co/)
- [Reticulate Documentation](https://rstudio.github.io/reticulate/)
- [TikZ & PGF Manual](https://tikz.dev/)

### Comandos Útiles

```bash
# Verificar versiones
R --version
python3 --version
pdflatex --version

# Listar paquetes R instalados
R -e 'installed.packages()[,c("Package","Version")]'

# Listar paquetes Python instalados
pip list

# Actualizar paquetes R
R -e 'update.packages(ask = FALSE)'

# Actualizar paquetes Python
pip install --upgrade matplotlib numpy pandas
```

---

## ✅ Checklist de Instalación Completa

- [ ] Sistema actualizado (`sudo pacman -Syu`)
- [ ] R instalado y funcional
- [ ] RStudio Desktop instalado
- [ ] LaTeX (TeX Live) instalado
- [ ] Python 3 con matplotlib, numpy, pandas
- [ ] Paquetes R instalados (exams, knitr, reticulate, etc.)
- [ ] TinyTeX configurado
- [ ] Integración Python-R funcional
- [ ] .Rprofile configurado en el proyecto
- [ ] Proyecto abierto en RStudio
- [ ] Compilación de prueba exitosa

---

## 🎉 ¡Instalación Completada!

Si todos los pasos se completaron exitosamente, tu entorno de desarrollo está listo para trabajar con el proyecto **RepositorioMatematicasICFES_R_Exams**.

**Próximos pasos:**
1. Explorar los ejemplos funcionales en `Auxiliares/Ejemplos-Funcionales-Rmd/`
2. Leer la documentación del proyecto
3. Comenzar a crear ejercicios ICFES siguiendo la filosofía del proyecto

---

**Soporte:** Para problemas específicos, consulta la documentación en `Auxiliares/` o revisa los archivos de ejemplo.

