---
output:
  html_document: default
  pdf_document: default
---

# Guía de Instalación para Proyecto R-Exams ICFES Matemáticas

Esta guía proporciona instrucciones detalladas para configurar el entorno de desarrollo necesario para trabajar con el proyecto de exámenes ICFES de matemáticas en Manjaro XFCE.

## Tabla de Contenidos

1. [Requisitos Previos](#requisitos-previos)
2. [Actualización del Sistema](#1-actualización-del-sistema)
3. [Instalación de R y Dependencias](#2-instalación-de-r-y-dependencias)
4. [Instalación de RStudio](#3-instalación-de-rstudio)
5. [Instalación de LaTeX](#4-instalación-de-latex)
6. [Instalación de Python y Dependencias](#5-instalación-de-python-y-dependencias)
7. [Herramientas de Procesamiento de Documentos e Imágenes](#6-herramientas-de-procesamiento-de-documentos-e-imágenes)
8. [Configuración del Proyecto](#7-configuración-del-proyecto)
9. [Verificación de la Instalación](#8-verificación-de-la-instalación)
10. [Mantenimiento del Sistema](#9-mantenimiento-del-sistema)
11. [Respaldo y Recuperación](#10-respaldo-y-recuperación)
12. [Solución de Problemas Comunes](#11-solución-de-problemas-comunes)
13. [Optimización del Sistema](#12-optimización-del-sistema)
14. [Recursos Adicionales](#13-recursos-adicionales)

## Requisitos Previos

### Requisitos de Hardware
- Procesador: 64 bits, 2 núcleos o más
- RAM: Mínimo 4GB (8GB recomendado)
- Almacenamiento: 20GB de espacio libre
- Conexión a internet estable (mínimo 5Mbps)

### Requisitos de Software
- Manjaro XFCE instalado y actualizado
- Acceso a terminal con privilegios de administrador
- Navegador web moderno
- Editor de texto (nano, vim, o similar)

## 1. Actualización del Sistema

### 1.1 Actualización Inicial
```bash
# Actualizar la base de datos de paquetes
sudo pacman -Syy

# Actualizar el sistema completo
sudo pacman -Syu

# Limpiar caché de paquetes
sudo pacman -Scc
```

### 1.2 Instalación de Herramientas Básicas
```bash
# Instalar herramientas de desarrollo
sudo pacman -S base-devel git

# Instalar yay (gestor de paquetes AUR)
git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si
```

## 2. Instalación de R y Dependencias

### 2.1 Instalación de R
```bash
# Instalar R y paquetes de desarrollo necesarios
sudo pacman -S r r-devtools r-tidyverse r-knitr r-rmarkdown

# Instalar dependencias adicionales
sudo pacman -S gcc-fortran gcc-libs
```

### 2.2 Configuración del Entorno R
```bash
# Crear directorio para paquetes de usuario
mkdir -p ~/R/library

# Configurar el archivo .Renviron
echo 'R_LIBS_USER=~/R/library' >> ~/.Renviron
echo 'R_MAX_MEM_SIZE=8000M' >> ~/.Renviron
```

### 2.3 Instalación de Paquetes R Necesarios
```r
# Configurar el repositorio CRAN
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Instalar paquetes básicos
install.packages(c(
  "devtools",
  "exams",
  "tidyverse",
  "knitr",
  "rmarkdown",
  "tinytex",
  "stringr",
  "dplyr",
  "ggplot2",
  "magrittr",
  "purrr",
  "xtable",
  "pander",
  "kableExtra",
  "reticulate",
  "rmdformats",
  "bookdown",
  "webshot",
  "htmltools"
))

# Instalar tinytex para compilación de documentos LaTeX
tinytex::install_tinytex()
```

## 3. Instalación de RStudio

### 3.1 Instalación desde AUR
```bash
# Instalar RStudio
yay -S rstudio-desktop-bin
```

### 3.2 Configuración de RStudio
- Abrir RStudio
- Ir a Tools > Global Options
- Configurar las siguientes opciones:

#### General
- Restore .RData into workspace at startup: NO
- Save workspace to .RData on exit: NO
- Always save history: YES
- Remove duplicate entries in history: YES

#### Code
- Insert spaces for tab: YES
- Tab width: 2
- Show margin: YES
- Margin column: 80
- Soft-wrap R source files: YES
- Auto-indent after paste: YES
- Insert matching parens/quotes: YES
- Surround selection on text insertion: YES

#### Appearance
- Editor theme: Modern
- Font: DejaVu Sans Mono
- Font size: 12
- RStudio theme: Modern
- Cursor blinking: YES
- Cursor style: Line
- Highlight selected word: YES
- Highlight selected line: YES
- Show line numbers: YES
- Show indent guides: YES
- Show whitespace characters: NO
- Rainbow parentheses: YES
- Rainbow fenced divs: YES
- Rainbow function calls: YES

#### Packages
- CRAN mirror: Seleccionar el más cercano a tu ubicación
- Use secure download method for HTTP: YES

#### R Markdown
- Show output preview in: Viewer pane
- Show output inline for all R Markdown documents: NO
- Auto-wrap output: YES
- Maximum output width: 80

## 4. Instalación de LaTeX

### 4.1 Instalación de TeX Live
```bash
# Instalar TeX Live completo
sudo pacman -S texlive-most texlive-lang texlive-latexextra

# Instalar herramientas adicionales
sudo pacman -S texmaker latexmk

# Actualizar la base de datos de TeX
sudo texhash
```

### 4.2 Paquetes LaTeX Adicionales
```bash
# Instalar paquetes adicionales necesarios
sudo pacman -S texlive-fontsextra texlive-pictures texlive-science
sudo pacman -S texlive-latexrecommended texlive-latexindent
```

## 5. Instalación de Python y Dependencias

### 5.1 Instalación de Python
```bash
# Instalar Python y herramientas de desarrollo
sudo pacman -S python python-pip python-setuptools python-wheel
sudo pacman -S python-virtualenv python-pipenv
```

### 5.2 Configuración de Python en RStudio con reticulate
```r
# Instalar y cargar reticulate
install.packages("reticulate")
library(reticulate)

# Configurar el entorno de Python
use_python("/usr/bin/python3")

# Instalar paquetes de Python necesarios a través de reticulate
py_install(c(
  "matplotlib",
  "pandas",
  "numpy",
  "scipy",
  "seaborn",
  "plotly",
  "bokeh",
  "Pillow",
  "opencv-python",
  "jupyter",
  "ipykernel",
  "notebook",
  "qtconsole"
))

# Verificar la instalación
py_list_packages()
```

### 5.3 Verificación de la Instalación de Python
```r
# Verificar la configuración de Python
py_config()

# Probar la instalación con un ejemplo simple
py_run_string("
import matplotlib.pyplot as plt
import numpy as np
x = np.linspace(0, 10, 100)
plt.plot(x, np.sin(x))
plt.show()
")
```

## 6. Herramientas de Procesamiento de Documentos e Imágenes

### 6.1 Herramientas de PDF
```bash
# Instalar herramientas de procesamiento de PDF
sudo pacman -S poppler pdf2svg pdfcrop
sudo pacman -S pdfjam pdf-tools
```

### 6.2 ImageMagick y Dependencias
```bash
# Instalar ImageMagick y sus dependencias
sudo pacman -S imagemagick ghostscript

# Configurar políticas de seguridad para ImageMagick
sudo sed -i 's/rights="none" pattern="PDF"/rights="read|write" pattern="PDF"/' /etc/ImageMagick-7/policy.xml
sudo sed -i 's/rights="none" pattern="PS"/rights="read|write" pattern="PS"/' /etc/ImageMagick-7/policy.xml
```

### 6.3 Herramientas Adicionales
```bash
# Instalar herramientas de gráficos
sudo pacman -S graphviz

# Instalar herramientas de compresión
sudo pacman -S zip unzip

# Instalar herramientas de procesamiento de imágenes
sudo pacman -S optipng jpegoptim
```

## 7. Configuración del Proyecto

### 7.1 Estructura de Directorios
```bash
# Crear estructura de directorios para el proyecto
mkdir -p ~/R/projects/icfes-matematicas/{data,scripts,output,figures,templates,exams,backups}
```

### 7.2 Configuración de Git
```bash
# Instalar Git
sudo pacman -S git

# Configuración básica
git config --global user.name "Tu Nombre"
git config --global user.email "tu@email.com"
git config --global core.editor "nano"
git config --global init.defaultBranch main
git config --global color.ui true

# Configuración adicional
git config --global core.autocrlf input
git config --global core.safecrlf warn
git config --global pull.rebase false
```

## 8. Verificación de la Instalación

### 8.1 Script de Verificación
```bash
#!/bin/bash

echo "Verificando instalaciones..."

# Verificar R
echo "R:"
R --version

# Verificar Python
echo "Python:"
python --version
pip --version

# Verificar LaTeX
echo "LaTeX:"
pdflatex --version

# Verificar RStudio
echo "RStudio:"
rstudio --version

# Verificar ImageMagick
echo "ImageMagick:"
convert --version

# Verificar herramientas PDF
echo "PDF tools:"
pdf2svg --version
pdfcrop --version

# Verificar paquetes Python
python -c "import matplotlib; print('Matplotlib version:', matplotlib.__version__)"
python -c "import pandas; print('Pandas version:', pandas.__version__)"
python -c "import numpy; print('NumPy version:', numpy.__version__)"

# Verificar paquetes R
R -e "installed.packages()"
```

### 8.2 Prueba de Integración
```r
# Crear un documento R Markdown de prueba
rmarkdown::render("test.Rmd", output_format = "pdf_document")
rmarkdown::render("test.Rmd", output_format = "html_document")

# Probar la generación de exámenes
library(exams)
exams2pdf("ejemplo.Rnw")
```

## 9. Mantenimiento del Sistema

### 9.1 Actualización Regular
```bash
# Actualizar el sistema
sudo pacman -Syu

# Limpiar caché
sudo pacman -Scc

# Actualizar paquetes de R
R -e "update.packages(ask = FALSE)"

# Actualizar paquetes de Python
pip list --outdated
pip install --upgrade pip
```

### 9.2 Limpieza del Sistema
```bash
# Limpiar caché de pacman
sudo pacman -Scc

# Limpiar caché de R
R -e "gc()"

# Limpiar caché de Python
pip cache purge

# Limpiar archivos temporales
rm -rf /tmp/*
```

## 10. Respaldo y Recuperación

### 10.1 Respaldo Regular
```bash
# Crear directorio de respaldo
mkdir -p ~/backups

# Respaldo de configuración R
cp -r ~/.R ~/backups/R_config_$(date +%Y%m%d)

# Respaldo de proyectos
tar -czf ~/backups/projects_$(date +%Y%m%d).tar.gz ~/R/projects

# Respaldo de configuración de RStudio
cp -r ~/.config/RStudio ~/backups/RStudio_config_$(date +%Y%m%d)
```

### 10.2 Recuperación
```bash
# Restaurar configuración R
cp -r ~/backups/R_config_YYYYMMDD/* ~/.R/

# Restaurar proyectos
tar -xzf ~/backups/projects_YYYYMMDD.tar.gz -C ~/

# Restaurar configuración de RStudio
cp -r ~/backups/RStudio_config_YYYYMMDD/* ~/.config/RStudio/
```

## 11. Solución de Problemas Comunes

### 11.1 Problemas con LaTeX
```bash
# Reinstalar paquetes LaTeX específicos
sudo pacman -S texlive-latexextra texlive-fontsextra texlive-pictures

# Limpiar caché de LaTeX
sudo texhash

# Verificar instalación de LaTeX
tlmgr update --self
tlmgr update --all
```

### 11.2 Problemas con r-exams
```r
# Actualizar todos los paquetes
update.packages(ask = FALSE)

# Reinstalar r-exams y dependencias
remove.packages("exams")
install.packages("exams", dependencies = TRUE)

# Verificar dependencias
library(exams)
exams_version()
```

### 11.3 Problemas con RStudio
```bash
# Verificar permisos
sudo chown -R $USER:$USER ~/.config/RStudio
sudo chmod -R 755 ~/.config/RStudio

# Reinstalar RStudio
yay -R rstudio-desktop-bin
yay -S rstudio-desktop-bin
```

### 11.4 Problemas con ImageMagick
```bash
# Verificar políticas de seguridad
sudo nano /etc/ImageMagick-7/policy.xml

# Reinstalar ImageMagick
sudo pacman -S imagemagick
```

### 11.5 Problemas con Python/reticulate
```r
# Reinstalar reticulate
remove.packages("reticulate")
install.packages("reticulate")

# Reconfigurar Python
library(reticulate)
use_python("/usr/bin/python3", required = TRUE)
```

## 12. Optimización del Sistema

### 12.1 Optimización de R
```r
# Configurar opciones de memoria
options(mc.cores = parallel::detectCores())
options(max.print = 1000)
options(width = 80)
options(stringsAsFactors = FALSE)
```

### 12.2 Optimización de RStudio
- Aumentar el tamaño del buffer de la consola
- Habilitar el guardado automático
- Configurar el formateo automático del código
- Optimizar la configuración de Git

### 12.3 Optimización del Sistema
```bash
# Limpiar caché del sistema
sudo pacman -Scc

# Optimizar la base de datos de paquetes
sudo pacman-optimize

# Limpiar archivos temporales
sudo rm -rf /tmp/*
```

## 13. Recursos Adicionales

### 13.1 Documentación Oficial
- [R Project](https://www.r-project.org/)
- [RStudio](https://posit.co/download/rstudio-desktop/)
- [LaTeX Project](https://www.latex-project.org/)
- [Python](https://www.python.org/)
- [reticulate](https://rstudio.github.io/reticulate/)
- [r-exams](http://www.r-exams.org/)

### 13.2 Foros y Comunidades
- [RStudio Community](https://community.rstudio.com/)
- [Stack Overflow](https://stackoverflow.com/)
- [Manjaro Forum](https://forum.manjaro.org/)

### 13.3 Tutoriales y Guías
- [R Markdown](https://rmarkdown.rstudio.com/)
- [LaTeX Wikibook](https://en.wikibooks.org/wiki/LaTeX)
- [Python for Data Science](https://www.python.org/doc/)

## Notas Importantes

- Asegúrate de tener suficiente espacio en disco (mínimo 20GB recomendado)
- La instalación completa puede tomar entre 30-60 minutos dependiendo de tu conexión a internet
- Es recomendable tener una copia de seguridad de tus datos antes de realizar instalaciones masivas
- Mantén un registro de las versiones instaladas para facilitar la solución de problemas
- Realiza respaldos regulares de tu configuración y proyectos
- Verifica la integridad de las instalaciones después de cada actualización importante

## Soporte

Si encuentras problemas durante la instalación:

1. Revisa los logs de instalación:
   ```bash
   journalctl -xe
   ```
2. Consulta la documentación oficial:
   - R: https://cran.r-project.org/manuals.html
   - RStudio: https://docs.posit.co/rstudio/
   - LaTeX: https://www.latex-project.org/help/documentation/
   - Manjaro: https://wiki.manjaro.org/
3. Verifica los foros de Manjaro para problemas específicos del sistema
4. Consulta el repositorio de GitHub del proyecto para problemas específicos

---
*Última actualización: Configuración Git automatizada - Enero 2025*