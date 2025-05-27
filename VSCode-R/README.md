# 📁 VSCode-R: Tutorial Completo de Instalación

## 🎯 Contenido de esta Carpeta

Esta carpeta contiene todo lo necesario para instalar y configurar R con VSCode en sistemas Linux, especialmente optimizado para proyectos r-exams del ICFES.

## 📋 Archivos Incluidos

### 📚 **Documentación Principal**
| Archivo | Descripción | Cuándo Usar |
|---------|-------------|-------------|
| `README.md` | Este archivo (índice) | Empezar aquí |
| `TUTORIAL_INSTALACION_R_VSCODE.md` | **Tutorial completo paso a paso** | Instalación manual detallada |
| `README_INSTALACION_R.md` | Instrucciones rápidas | Referencia rápida |
| `RESUMEN_TUTORIAL.md` | Resumen ejecutivo | Visión general del proyecto |

### 🤖 **Scripts de Instalación**
| Archivo | Descripción | Comando |
|---------|-------------|---------|
| `instalar_r_vscode.sh` | **Script de instalación automática** | `./instalar_r_vscode.sh` |
| `configuracion_respaldo.tar.gz.sh` | Crear respaldo de configuración | `./configuracion_respaldo.tar.gz.sh` |

### 🧪 **Scripts de Verificación**
| Archivo | Descripción | Comando |
|---------|-------------|---------|
| `test_vscode_r.R` | Prueba básica de funcionamiento | `Rscript test_vscode_r.R` |

### ⚙️ **Archivos de Configuración**
| Archivo/Carpeta | Descripción | Ubicación Final |
|-----------------|-------------|-----------------|
| `.vscode/` | Configuración VSCode para R | Directorio del proyecto |
| `.Rprofile` | Configuración R automática | Directorio del proyecto |

## 🚀 **Instalación Rápida**

### Opción 1: Automática (Recomendada)
```bash
# Navegar a la carpeta
cd VSCode-R

# Ejecutar instalación automática
./instalar_r_vscode.sh

# Verificar instalación
Rscript test_vscode_r.R
```

### Opción 2: Manual
```bash
# Seguir el tutorial paso a paso
cat TUTORIAL_INSTALACION_R_VSCODE.md
```

## 🎯 **Para Proyectos r-exams**

Una vez instalado, puedes trabajar directamente con r-exams:

```r
# Cargar paquetes automáticamente
load_common_packages()

# Crear ejercicios matemáticos
library(exams)
library(reticulate)  # Para gráficos Python

# Generar exámenes
exams2pdf("mi_ejercicio.Rmd")
exams2moodle("mi_ejercicio.Rmd")
```

## 🐧 **Distribuciones Soportadas**

- ✅ **Manjaro/Arch Linux** (principal)
- ✅ Ubuntu/Debian (adaptado)
- ✅ Fedora/CentOS/RHEL (adaptado)
- ✅ openSUSE (adaptado)

## 📦 **Componentes que se Instalan**

### Software Base
- R 4.5.0+
- Python 3.13.3+
- pip (gestor de paquetes Python)

### Paquetes R Esenciales
- `exams` - Generación de exámenes
- `reticulate` - Integración Python-R
- `knitr` - Documentos dinámicos
- `rmarkdown` - R Markdown
- `digest` - Funciones hash
- `testthat` - Pruebas unitarias

### Paquetes Python Esenciales
- `matplotlib` - Gráficos
- `numpy` - Cálculos numéricos
- `pandas` - Manipulación de datos

### Extensiones VSCode
- R Extension for Visual Studio Code

## 🔧 **Configuraciones Aplicadas**

- ✅ Biblioteca personal R en `~/R/library`
- ✅ VSCode optimizado para R (sin LSP problemático)
- ✅ Integración Python-R automática
- ✅ Configuración de gráficos y terminal
- ✅ Asociaciones de archivos R/Rmd

## 🆘 **Solución de Problemas**

### Error común: "R Tools client: couldn't create connection"
✅ **Ya solucionado** en la configuración automática

### Paquetes no encontrados
```bash
R -e "install.packages('nombre_paquete', lib='~/R/library')"
```

### Python no detectado
```r
reticulate::use_python("/usr/bin/python3")
```

## 📞 **Soporte**

1. **Revisa** `TUTORIAL_INSTALACION_R_VSCODE.md` para detalles completos
2. **Ejecuta** `test_vscode_r.R` para verificar la instalación
3. **Consulta** `RESUMEN_TUTORIAL.md` para información técnica

## 🔄 **Uso en Futuras Instantáneas**

1. **Copia** esta carpeta `VSCode-R` a tu nueva instantánea
2. **Navega** a la carpeta: `cd VSCode-R`
3. **Ejecuta** el instalador: `./instalar_r_vscode.sh`
4. **Verifica** la instalación: `Rscript test_vscode_r.R`
5. **¡Listo para trabajar!**

---

**🎉 ¡Todo listo para replicar la instalación en cualquier momento!**

*Optimizado para proyectos r-exams del ICFES en sistemas Linux*
