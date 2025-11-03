# 📦 Instalación del Entorno R-Exams ICFES en Manjaro

Este directorio contiene todos los scripts y documentación necesarios para instalar y configurar el entorno de desarrollo completo del proyecto **RepositorioMatematicasICFES_R_Exams** en Manjaro Plasma KDE.

---

## 🎯 Inicio Rápido

### Opción 1: Instalación Automática Completa (RECOMENDADO)

```bash
cd /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams
chmod +x Auxiliares/Instalaciones/install_manjaro_r_exams_complete.sh
./Auxiliares/Instalaciones/install_manjaro_r_exams_complete.sh
```

**Tiempo estimado:** 20-40 minutos

### Opción 2: Solo Paquetes R (si R ya está instalado)

```bash
cd /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams
chmod +x Auxiliares/Instalaciones/install_r_packages_only.R
R --vanilla < Auxiliares/Instalaciones/install_r_packages_only.R
```

**Tiempo estimado:** 10-20 minutos

---

## 📁 Archivos Disponibles

### Scripts de Instalación

| Archivo | Descripción | Uso |
|---------|-------------|-----|
| `install_manjaro_r_exams_complete.sh` | **Script principal** - Instalación completa automatizada | Instalación desde cero |
| `install_r_packages_only.R` | Instalación solo de paquetes R | Si R ya está instalado |
| `verify_installation.sh` | Verificación completa de la instalación | Después de instalar |

### Documentación

| Archivo | Descripción |
|---------|-------------|
| `INSTRUCCIONES_INSTALACION_MANJARO.md` | **Guía completa** con instrucciones detalladas |
| `README_INSTALACION_MANJARO.md` | Este archivo - Resumen rápido |
| `Tutorial Actualizado R-exams Mayo 2025.md` | Tutorial general de R-exams |

---

## 🔧 Componentes Instalados

El script de instalación completa instala:

### Software Principal
- ✅ **R** (versión más reciente de repositorios Manjaro)
- ✅ **RStudio Desktop** (desde AUR)
- ✅ **LaTeX** (TeX Live completo)
- ✅ **Python 3** (con pip)

### Paquetes R (30+ paquetes)
- **R-exams:** exams, knitr, rmarkdown
- **Datos:** tidyverse, dplyr, tidyr, data.table, readxl
- **Visualización:** ggplot2, scales, gridExtra, RColorBrewer
- **Estadística:** MASS, car, psych
- **LaTeX:** tinytex, pdftools, qpdf
- **Python:** reticulate (CRÍTICO para el proyecto)
- **Herramientas:** devtools, testthat, digest, magick, webshot

### Paquetes Python
- matplotlib
- numpy
- pandas
- seaborn

### Paquetes LaTeX (TikZ y más)
- amsmath, amsfonts, amssymb, mathtools
- babel, babel-spanish
- tikz, pgf, pgfplots, xcolor
- geometry, fancyhdr, fontspec
- hyperref, graphicx, listings

### Dependencias del Sistema
- gcc-fortran, pandoc, imagemagick
- cairo, pango, libpng, libjpeg-turbo
- git, curl, wget, libxml2, openssl

---

## ✅ Verificación de la Instalación

### Método Automático

```bash
chmod +x Auxiliares/Instalaciones/verify_installation.sh
./Auxiliares/Instalaciones/verify_installation.sh
```

Este script verifica:
- ✓ Comandos principales (R, RStudio, Python, LaTeX)
- ✓ Paquetes del sistema
- ✓ Paquetes R (30+ paquetes)
- ✓ Paquetes Python
- ✓ Integración Python-R (reticulate)
- ✓ Configuración del proyecto

### Verificación Manual Rápida

```bash
# Verificar R
R --version

# Verificar RStudio
rstudio --version

# Verificar Python
python3 --version

# Verificar LaTeX
pdflatex --version

# Verificar paquetes R críticos
R -e "library(exams); library(reticulate); library(knitr); print('OK')"

# Verificar paquetes Python
python3 -c "import matplotlib, numpy, pandas; print('OK')"
```

---

## 🚀 Uso del Entorno

### 1. Abrir el Proyecto en RStudio

```bash
cd /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams
rstudio RepositorioMatematicasICFES_R_Exams.Rproj &
```

### 2. Cargar Paquetes Comunes

En la consola de R:
```r
load_icfes_packages()
```

### 3. Compilar un Ejercicio de Ejemplo

```r
library(exams)

# Navegar a ejemplos funcionales
setwd("Auxiliares/Ejemplos-Funcionales-Rmd")

# Compilar a HTML
exams2html("nombre_ejercicio.Rmd", n = 1)

# Compilar a PDF
exams2pdf("nombre_ejercicio.Rmd", n = 1)

# Compilar a Moodle
exams2moodle("nombre_ejercicio.Rmd", n = 5)
```

---

## 🔍 Solución de Problemas Comunes

### Problema: RStudio no se instala desde AUR

**Solución:**
```bash
yay -S rstudio-desktop-bin
# Si falla, intentar con debtap (ver INSTRUCCIONES_INSTALACION_MANJARO.md)
```

### Problema: Error al compilar paquetes R

**Solución:**
```bash
sudo pacman -S base-devel gcc-fortran
```

### Problema: Reticulate no encuentra Python

**Solución en R:**
```r
library(reticulate)
use_python("/usr/bin/python3", required = TRUE)
py_config()
```

### Problema: TinyTeX no se instala

**Solución:**
Usar TeX Live del sistema:
```bash
sudo pacman -S texlive-most
```

### Problema: Permisos al instalar paquetes R

**Solución:**
```bash
mkdir -p ~/R/library
```

En R:
```r
.libPaths(c("~/R/library", .libPaths()))
```

---

## 📚 Documentación Adicional

### Dentro del Proyecto
- `README.md` - Descripción general del proyecto
- `Auxiliares/Ejemplos-Funcionales-Rmd/` - Ejemplos funcionales
- `Auxiliares/Python-Documentation/` - Documentación de Python
- `.augment/rules/reglas-generales.md` - Reglas del proyecto

### Externa
- [R-exams Official](http://www.r-exams.org/)
- [RStudio Documentation](https://docs.posit.co/)
- [Reticulate Documentation](https://rstudio.github.io/reticulate/)
- [TikZ & PGF Manual](https://tikz.dev/)

---

## 🎓 Flujo de Trabajo Recomendado

1. **Instalación Inicial**
   ```bash
   ./install_manjaro_r_exams_complete.sh
   ```

2. **Verificación**
   ```bash
   ./verify_installation.sh
   ```

3. **Abrir Proyecto**
   ```bash
   rstudio RepositorioMatematicasICFES_R_Exams.Rproj &
   ```

4. **Explorar Ejemplos**
   - Navegar a `Auxiliares/Ejemplos-Funcionales-Rmd/`
   - Abrir y compilar archivos .Rmd de ejemplo

5. **Crear Nuevos Ejercicios**
   - Seguir la estructura de los ejemplos funcionales
   - Usar las metodologías documentadas en `Auxiliares/`

---

## 📊 Requisitos del Sistema

### Mínimos
- **SO:** Manjaro Linux (Plasma KDE)
- **RAM:** 4 GB
- **Disco:** 5 GB libres
- **Internet:** Conexión estable

### Recomendados
- **RAM:** 8 GB o más
- **Disco:** 10 GB libres
- **Procesador:** Multi-core

---

## 🔄 Actualización del Entorno

### Actualizar Paquetes R

```r
update.packages(ask = FALSE)
```

### Actualizar Paquetes Python

```bash
pip install --upgrade matplotlib numpy pandas
```

### Actualizar Sistema

```bash
sudo pacman -Syu
```

---

## ✅ Checklist de Instalación

- [ ] Script de instalación ejecutado sin errores
- [ ] Verificación exitosa (`verify_installation.sh`)
- [ ] R funcional (`R --version`)
- [ ] RStudio abre correctamente
- [ ] Python 3 con paquetes instalados
- [ ] LaTeX compila documentos
- [ ] Reticulate funcional (Python-R)
- [ ] Proyecto abre en RStudio
- [ ] Compilación de ejemplo exitosa

---

## 💡 Comandos Útiles

```bash
# Verificar versiones
R --version
python3 --version
pdflatex --version

# Listar paquetes R
R -e 'installed.packages()[,c("Package","Version")]'

# Listar paquetes Python
pip list

# Actualizar todo
sudo pacman -Syu
R -e 'update.packages(ask = FALSE)'
pip install --upgrade matplotlib numpy pandas

# Limpiar caché de pacman
sudo pacman -Sc
```

---

## 🆘 Soporte

Para problemas específicos:

1. **Consultar documentación:** `INSTRUCCIONES_INSTALACION_MANJARO.md`
2. **Revisar ejemplos:** `Auxiliares/Ejemplos-Funcionales-Rmd/`
3. **Verificar configuración:** `./verify_installation.sh`
4. **Revisar logs:** Archivos de error en `/tmp/`

---

## 📝 Notas Importantes

- **Tiempo de instalación:** Varía según velocidad de Internet (20-40 min)
- **Espacio requerido:** ~5 GB para instalación completa
- **Permisos sudo:** Necesarios para paquetes del sistema
- **Conexión a Internet:** Requerida durante toda la instalación
- **Primera ejecución:** Puede ser más lenta mientras se configuran paquetes

---

## 🎉 ¡Listo para Comenzar!

Una vez completada la instalación y verificación, el entorno está completamente configurado para trabajar con el proyecto **RepositorioMatematicasICFES_R_Exams**.

**¡Feliz desarrollo de ejercicios ICFES!** 🚀

