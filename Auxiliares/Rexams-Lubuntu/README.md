# 🐧 R-exams ICFES para Lubuntu

Esta carpeta contiene todos los scripts y documentación específicos para instalar y configurar R-exams en sistemas Lubuntu, adaptados para el proyecto **proyecto-r-exams-icfes-matematicas-optimizado**.

## 📁 Contenido de la Carpeta

### 🔧 Scripts de Instalación
- **`install_r_exams_lubuntu.sh`** - Script principal de instalación completa
- **`check_current_installations.sh`** - Verificar estado de instalaciones
- **`wait_and_complete_installation.sh`** - Automatización completa

### 📚 Documentación
- **`GUIA_INSTALACION_R_EXAMS_ICFES.md`** - Guía detallada paso a paso
- **`RESUMEN_INSTALACION.md`** - Resumen ejecutivo
- **`README.md`** - Este archivo

## 🚀 Uso Rápido

### Opción 1: Automática (Recomendada)
```bash
cd "/home/rexams/Proyectos/proyecto-r-exams-icfes-matematicas-optimizado/Auxiliares/Rexams-Lubuntu"
./wait_and_complete_installation.sh
```

### Opción 2: Manual
```bash
cd "/home/rexams/Proyectos/proyecto-r-exams-icfes-matematicas-optimizado/Auxiliares/Rexams-Lubuntu"

# 1. Verificar estado actual
./check_current_installations.sh

# 2. Ejecutar instalación completa
./install_r_exams_lubuntu.sh
```

## 🎯 Qué Instala

### Sistema Base
- ✅ R (versión más reciente desde CRAN)
- ✅ LaTeX (TeX Live completo)
- ✅ Pandoc
- ✅ Python 3 y módulos científicos

### Paquetes R Específicos
- ✅ `exams` - Framework principal
- ✅ `knitr`, `rmarkdown` - Documentos dinámicos
- ✅ `ggplot2`, `dplyr` - Visualización y datos
- ✅ `reticulate` - Integración Python
- ✅ `tinytex` - LaTeX ligero
- ✅ Y muchos más...

### Herramientas Adicionales
- ✅ ImageMagick - Procesamiento de imágenes
- ✅ Ghostscript - PostScript/PDF
- ✅ Poppler-utils - Utilidades PDF
- ✅ Inkscape - Gráficos vectoriales

## 🎯 Características del Proyecto

Tu proyecto R-exams ICFES incluye:
- **🎲 300+ versiones únicas** por ejercicio
- **📊 Alineación ICFES completa** (competencias, niveles, contextos)
- **🔄 Múltiples formatos**: PDF, HTML, Moodle, NOPS
- **🎨 Metodología TikZ avanzada** (98% fidelidad visual)
- **🔧 Corrección automática de errores** (<5 minutos)

## 📚 Documentación Relacionada

### En Este Proyecto
- **README principal**: `../../README.md`
- **Tutorial Manjaro**: `../Instalaciones/Tutorial Actualizado R-exams Mayo 2025.md`
- **Metodología TikZ**: `../TikZ-Documentation/TikZ-ICFES-Guide.md`
- **Corrección de Errores**: `../METODOLOGIA_Correccion_Errores_Recurrentes_ICFES_R_Exams.md`

### Ejemplos Funcionales
- **Ejemplos validados**: `../Ejemplos-Funcionales-Rmd/`
- **Plantillas**: `../../General/Plantillas/`

## 🆘 Solución de Problemas

### Error: LaTeX failed to compile
```r
tinytex::parse_install("archivo.log")
tinytex::tlmgr_update()
```

### Error: Paquete R faltante
```r
install.packages("nombre_paquete", dependencies = TRUE)
```

### Error: Python no configurado
```r
library(reticulate)
use_python("/usr/bin/python3", required = TRUE)
```

## 🔄 Diferencias con Tutorial Manjaro

Este conjunto de scripts está específicamente adaptado para **Lubuntu/Ubuntu/Debian**:
- ✅ Usa `apt` en lugar de `pacman`
- ✅ Repositorio CRAN para R actualizado
- ✅ Dependencias específicas de Ubuntu
- ✅ Configuración optimizada para Lubuntu

## 📞 Soporte

1. **📖 Consultar**: `GUIA_INSTALACION_R_EXAMS_ICFES.md`
2. **🔍 Verificar**: `./check_current_installations.sh`
3. **🧪 Probar**: Scripts de prueba incluidos
4. **📚 Revisar**: Documentación del proyecto principal

---

**Creado**: Enero 2025  
**Propósito**: Instalación R-exams ICFES específica para Lubuntu  
**Estado**: ✅ Listo para uso