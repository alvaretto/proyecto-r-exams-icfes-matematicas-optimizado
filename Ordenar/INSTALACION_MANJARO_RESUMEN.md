# 🚀 Instalación Completa del Entorno R-Exams ICFES en Manjaro - RESUMEN EJECUTIVO

**Proyecto:** RepositorioMatematicasICFES_R_Exams  
**Sistema:** Manjaro Plasma KDE  
**Fecha:** Noviembre 2025

---

## ✅ ARCHIVOS CREADOS

Se han creado los siguientes archivos de instalación y configuración:

### 📁 Scripts de Instalación
```
Auxiliares/Instalaciones/
├── install_manjaro_r_exams_complete.sh    ⭐ SCRIPT PRINCIPAL
├── install_r_packages_only.R              📦 Solo paquetes R
├── verify_installation.sh                 ✓ Verificación
├── test_rexams_functionality.R            🧪 Pruebas
├── INSTRUCCIONES_INSTALACION_MANJARO.md   📖 Guía completa
└── README_INSTALACION_MANJARO.md          📋 Resumen rápido
```

---

## 🎯 INSTALACIÓN RÁPIDA (3 PASOS)

### Paso 1: Ejecutar Script de Instalación

```bash
cd /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams
./Auxiliares/Instalaciones/install_manjaro_r_exams_complete.sh
```

**Tiempo estimado:** 20-40 minutos  
**Requiere:** Conexión a Internet, permisos sudo

### Paso 2: Verificar Instalación

```bash
./Auxiliares/Instalaciones/verify_installation.sh
```

**Verifica:** Todos los componentes instalados correctamente

### Paso 3: Probar Funcionalidad

```bash
R --vanilla < Auxiliares/Instalaciones/test_rexams_functionality.R
```

**Prueba:** Compilación de ejercicios, gráficos, Python-R

---

## 📦 COMPONENTES INSTALADOS

### Software Principal
- ✅ **R** (versión más reciente)
- ✅ **RStudio Desktop**
- ✅ **LaTeX** (TeX Live completo con TikZ)
- ✅ **Python 3** (con matplotlib, numpy, pandas)

### Paquetes R (30+)
```
exams, knitr, rmarkdown, reticulate, tidyverse, ggplot2,
data.table, readxl, tinytex, pdftools, testthat, digest,
magick, webshot, MASS, car, psych, scales, gridExtra
```

### Integración Python-R
- ✅ **reticulate** configurado
- ✅ **matplotlib** para gráficos
- ✅ **numpy** para cálculos
- ✅ **pandas** para datos

### Paquetes LaTeX (TikZ)
```
amsmath, babel-spanish, tikz, pgfplots, xcolor,
geometry, fontspec, hyperref, graphicx
```

---

## 🔍 VERIFICACIÓN RÁPIDA

### Verificar que todo funciona:

```bash
# 1. Verificar R
R --version

# 2. Verificar RStudio
rstudio --version

# 3. Verificar Python
python3 --version

# 4. Verificar LaTeX
pdflatex --version

# 5. Verificar paquetes R
R -e "library(exams); library(reticulate); library(knitr); print('OK')"

# 6. Verificar paquetes Python
python3 -c "import matplotlib, numpy, pandas; print('OK')"

# 7. Verificación completa
./Auxiliares/Instalaciones/verify_installation.sh
```

---

## 🚀 PRIMEROS PASOS DESPUÉS DE LA INSTALACIÓN

### 1. Abrir RStudio

```bash
cd /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams
rstudio RepositorioMatematicasICFES_R_Exams.Rproj &
```

### 2. Cargar Paquetes Comunes (en consola R)

```r
load_icfes_packages()
```

### 3. Probar Compilación de Ejemplo

```r
library(exams)

# Navegar a ejemplos
setwd("Auxiliares/Ejemplos-Funcionales-Rmd")

# Listar archivos disponibles
list.files(pattern = "\\.Rmd$")

# Compilar un ejemplo a HTML
exams2html("nombre_ejercicio.Rmd", n = 1)

# Compilar a PDF
exams2pdf("nombre_ejercicio.Rmd", n = 1)

# Compilar a Moodle
exams2moodle("nombre_ejercicio.Rmd", n = 5)
```

---

## 🔧 CONFIGURACIÓN AUTOMÁTICA

El script de instalación configura automáticamente:

### .Rprofile del Proyecto
- ✅ Biblioteca personal de R
- ✅ Configuración de Python (reticulate)
- ✅ Opciones numéricas (scipen, OutDec)
- ✅ Función `load_icfes_packages()`

### Variables de Entorno
- ✅ Python: `/usr/bin/python3`
- ✅ R: Configuración UTF-8
- ✅ LaTeX: XeLaTeX por defecto

---

## 📚 DOCUMENTACIÓN DISPONIBLE

### En el Proyecto
1. **Guía Completa:** `Auxiliares/Instalaciones/INSTRUCCIONES_INSTALACION_MANJARO.md`
2. **Resumen Rápido:** `Auxiliares/Instalaciones/README_INSTALACION_MANJARO.md`
3. **Ejemplos Funcionales:** `Auxiliares/Ejemplos-Funcionales-Rmd/`
4. **Reglas del Proyecto:** `.augment/rules/reglas-generales.md`

### Externa
- [R-exams Official](http://www.r-exams.org/)
- [RStudio Docs](https://docs.posit.co/)
- [Reticulate](https://rstudio.github.io/reticulate/)
- [TikZ Manual](https://tikz.dev/)

---

## ⚠️ SOLUCIÓN DE PROBLEMAS COMUNES

### Problema 1: RStudio no se instala

```bash
yay -S rstudio-desktop-bin
```

### Problema 2: Error al compilar paquetes R

```bash
sudo pacman -S base-devel gcc-fortran
```

### Problema 3: Reticulate no encuentra Python

En R:
```r
library(reticulate)
use_python("/usr/bin/python3", required = TRUE)
py_config()
```

### Problema 4: TinyTeX no se instala

```bash
sudo pacman -S texlive-most
```

### Problema 5: Permisos al instalar paquetes R

```bash
mkdir -p ~/R/library
```

En R:
```r
.libPaths(c("~/R/library", .libPaths()))
```

---

## 🧪 PRUEBAS DE FUNCIONALIDAD

### Prueba Completa Automatizada

```bash
R --vanilla < Auxiliares/Instalaciones/test_rexams_functionality.R
```

Esta prueba verifica:
- ✓ Carga de paquetes R
- ✓ Integración Python-R
- ✓ Generación de gráficos (ggplot2 y matplotlib)
- ✓ Compilación a HTML
- ✓ Compilación a PDF
- ✓ Diversidad de versiones

### Prueba Manual Rápida

```r
# En R
library(exams)
library(reticulate)

# Probar Python
use_python("/usr/bin/python3")
py_run_string("print('Python funcional')")

# Probar gráfico
library(ggplot2)
ggplot(data.frame(x=1:10, y=1:10), aes(x,y)) + geom_line()
```

---

## 📊 REQUISITOS DEL SISTEMA

### Mínimos
- **RAM:** 4 GB
- **Disco:** 5 GB libres
- **Internet:** Conexión estable

### Recomendados
- **RAM:** 8 GB+
- **Disco:** 10 GB libres
- **Procesador:** Multi-core

---

## 🔄 MANTENIMIENTO

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

## ✅ CHECKLIST DE INSTALACIÓN COMPLETA

- [ ] Script de instalación ejecutado sin errores
- [ ] Verificación exitosa (`verify_installation.sh`)
- [ ] Pruebas funcionales pasadas (`test_rexams_functionality.R`)
- [ ] R funcional (`R --version`)
- [ ] RStudio abre correctamente
- [ ] Python 3 con paquetes instalados
- [ ] LaTeX compila documentos
- [ ] Reticulate funcional (Python-R)
- [ ] Proyecto abre en RStudio
- [ ] Compilación de ejemplo exitosa
- [ ] Gráficos TikZ funcionan
- [ ] Exportación a Moodle funcional

---

## 💡 COMANDOS ÚTILES

```bash
# Verificar versiones
R --version
python3 --version
pdflatex --version
rstudio --version

# Listar paquetes instalados
R -e 'installed.packages()[,c("Package","Version")]'
pip list

# Actualizar todo
sudo pacman -Syu
R -e 'update.packages(ask = FALSE)'
pip install --upgrade matplotlib numpy pandas

# Limpiar caché
sudo pacman -Sc
```

---

## 🎓 FLUJO DE TRABAJO RECOMENDADO

1. **Instalación**
   ```bash
   ./Auxiliares/Instalaciones/install_manjaro_r_exams_complete.sh
   ```

2. **Verificación**
   ```bash
   ./Auxiliares/Instalaciones/verify_installation.sh
   ```

3. **Pruebas**
   ```bash
   R --vanilla < Auxiliares/Instalaciones/test_rexams_functionality.R
   ```

4. **Abrir Proyecto**
   ```bash
   rstudio RepositorioMatematicasICFES_R_Exams.Rproj &
   ```

5. **Explorar Ejemplos**
   - Navegar a `Auxiliares/Ejemplos-Funcionales-Rmd/`
   - Abrir y compilar archivos .Rmd

6. **Crear Ejercicios**
   - Seguir estructura de ejemplos funcionales
   - Usar metodologías documentadas

---

## 📞 SOPORTE

Para problemas específicos:

1. **Consultar:** `Auxiliares/Instalaciones/INSTRUCCIONES_INSTALACION_MANJARO.md`
2. **Revisar:** `Auxiliares/Ejemplos-Funcionales-Rmd/`
3. **Ejecutar:** `./Auxiliares/Instalaciones/verify_installation.sh`
4. **Verificar:** Logs en `/tmp/`

---

## 🎉 ¡INSTALACIÓN COMPLETADA!

Una vez completados los 3 pasos (Instalación, Verificación, Pruebas), el entorno está completamente configurado para trabajar con el proyecto **RepositorioMatematicasICFES_R_Exams**.

### Características Disponibles:
- ✅ Generación de ejercicios R-exams
- ✅ Gráficos con ggplot2 y matplotlib
- ✅ Diagramas TikZ profesionales
- ✅ Integración Python-R completa
- ✅ Exportación a HTML, PDF, Moodle
- ✅ Generación de 300+ versiones únicas
- ✅ Compilación XeLaTeX con soporte español

### Próximos Pasos:
1. Explorar ejemplos funcionales
2. Leer documentación del proyecto
3. Crear primer ejercicio ICFES
4. Compilar y validar resultados

---

**¡Feliz desarrollo de ejercicios ICFES!** 🚀📚🎯

