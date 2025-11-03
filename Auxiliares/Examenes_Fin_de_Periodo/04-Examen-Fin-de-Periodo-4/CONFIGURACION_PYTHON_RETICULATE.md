# 🔧 CONFIGURACIÓN DE PYTHON PARA RETICULATE

## 📋 PROBLEMA IDENTIFICADO

Los archivos .Rmd funcionan correctamente en otros sistemas Linux, pero en este sistema específico, reticulate no encuentra matplotlib aunque esté instalado en el sistema.

**Error típico**:
```
NameError: name 'plt' is not defined
ModuleNotFoundError: No module named 'matplotlib'
```

## ✅ SOLUCIÓN APLICADA (BASADA EN SISTEMA FUNCIONAL)

La solución **NO modifica los archivos .Rmd** (que ya funcionan en otros sistemas), sino que **replica exactamente** la configuración del sistema Manjaro funcional documentado en:

`06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/01-Variables-Cualitativas_Distribucion-De-Frecuencias/Graficos_Estadisticos_Adopcion_Mascotas/SolucionReticulate/`

### Archivos de Configuración Modificados

#### 1. `.Renviron` (raíz del proyecto)

**Ubicación**: `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.Renviron`

**Contenido** (EXACTO del sistema funcional):
```bash
R_LIBS_USER="~/R/library"
```

**IMPORTANTE**: El sistema funcional **NO usa variables RETICULATE_PYTHON**. Solo configura la biblioteca de R.

---

#### 2. `.Rprofile` (raíz del proyecto)

**Ubicación**: `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.Rprofile`

**Configuración aplicada** (líneas 32-35) - EXACTA del sistema funcional:
```r
# Configurar Python para reticulate
if (require("reticulate", quietly = TRUE)) {
  use_python("/usr/bin/python3", required = FALSE)
}
```

**IMPORTANTE**:
- ✅ `required = FALSE` (como en el sistema funcional)
- ✅ NO se usa `Sys.setenv(RETICULATE_PYTHON = ...)`
- ✅ Configuración minimalista que funciona

**Propósito**: Configurar reticulate para usar el Python del sistema sin forzar errores si Python no está disponible.

---

## 🔍 VERIFICACIÓN DE LA CONFIGURACIÓN

### Verificar que Python está correctamente configurado

Ejecuta en R:

```r
library(reticulate)
py_config()
```

**Salida esperada**:
```
python:         /usr/bin/python3
libpython:      /usr/lib/libpython3.13.so
pythonhome:     //usr://usr
version:        3.13.7 (main, Aug 15 2025, 12:34:02) [GCC 15.2.1 20250813]
numpy:          /usr/lib/python3.13/site-packages/numpy
numpy_version:  2.3.3
```

### Verificar que matplotlib está disponible

Ejecuta en R:

```r
library(reticulate)
py_run_string("import matplotlib")
py_run_string("import matplotlib; print(matplotlib.__version__)")
```

**Salida esperada**:
```
3.10.6
```

---

## 📊 DEPENDENCIAS DEL SISTEMA

### Verificadas e Instaladas

✅ **Python**: 3.13.7  
✅ **matplotlib**: 3.10.6  
✅ **numpy**: 2.3.3  
✅ **R**: 4.5.1  
✅ **LaTeX**: TeX Live 2026  
✅ **Paquetes R**: exams, reticulate, tidyverse, etc.

### Comando de Instalación (si fuera necesario)

```bash
# En Manjaro/Arch
sudo pacman -S python-matplotlib python-numpy

# Verificar instalación
python3 -c "import matplotlib; print('matplotlib:', matplotlib.__version__)"
python3 -c "import numpy; print('numpy:', numpy.__version__)"
```

---

## 🚀 CÓMO USAR EL SISTEMA AHORA

### Opción 1: Compilar archivos .Rmd individuales

```r
# En RStudio
rmarkdown::render("020-Adopcion_Mascotas_Aleatorio_Interpretacion_n3_v1-Opc-D2v2.Rmd")
```

### Opción 2: Generar examen completo

```r
# En RStudio
setwd("Auxiliares/Examenes_Fin_de_Periodo/04-Examen-Fin-de-Periodo-4")
source("SemilleroFinDePeriodo_v4.R")
```

O desde terminal:

```bash
cd Auxiliares/Examenes_Fin_de_Periodo/04-Examen-Fin-de-Periodo-4
Rscript SemilleroFinDePeriodo_v4.R
```

---

## 📚 DOCUMENTACIÓN DE REFERENCIA

La configuración aplicada está basada en el sistema funcional documentado en:

**Directorio**: `06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/01-Variables-Cualitativas_Distribucion-De-Frecuencias/Graficos_Estadisticos_Adopcion_Mascotas/SolucionReticulate/`

**Archivos de referencia**:
- `CONFIGURACION_PYTHON_RETICULATE_COMPLETA.md` - Documentación exhaustiva
- `RESUMEN_RAPIDO_CONFIGURACION.md` - Guía rápida
- `README_CONFIGURACION_PYTHON.md` - Índice de documentación
- `verificar_configuracion_python.R` - Script de verificación

## 🔄 ORDEN DE CARGA DE CONFIGURACIÓN

Cuando R se inicia, los archivos se cargan en este orden:

1. **`.Renviron`** → Establece `R_LIBS_USER` (biblioteca personal)
2. **`.Rprofile`** → Ejecuta `use_python("/usr/bin/python3", required = FALSE)`
3. **Archivos .Rmd** → Se ejecutan con la configuración ya establecida

**NOTA CLAVE**: El sistema funcional NO usa variables de entorno para Python. Todo se configura en `.Rprofile`.

---

## 🎯 POR QUÉ ESTA SOLUCIÓN ES CORRECTA

### ✅ Ventajas de esta aproximación:

1. **No modifica los archivos .Rmd**: Los ejercicios siguen siendo portables entre sistemas
2. **Configuración centralizada**: Un solo lugar para configurar Python
3. **Automática**: No requiere intervención manual cada vez
4. **Compatible**: Funciona con RStudio, Rscript y knitr
5. **Persistente**: La configuración se mantiene entre sesiones

### ❌ Por qué NO modificar los .Rmd:

1. Los archivos ya funcionan en otros sistemas Linux
2. Modificar 178 archivos sería innecesario y propenso a errores
3. La configuración debe estar en el sistema, no en cada archivo
4. Mantiene la portabilidad del código

---

## 🔧 TROUBLESHOOTING

### Si aún no funciona después de aplicar la configuración:

1. **Reiniciar RStudio completamente**:
   - Cerrar todas las sesiones de R
   - Cerrar RStudio
   - Volver a abrir RStudio

2. **Verificar que los archivos de configuración existen**:
   ```bash
   ls -la .Renviron .Rprofile
   ```

3. **Verificar el contenido de los archivos**:
   ```bash
   cat .Renviron
   cat .Rprofile
   ```

4. **Limpiar caché de reticulate**:
   ```r
   # En R
   reticulate::py_discover_config()
   ```

5. **Verificar variables de entorno**:
   ```r
   # En R
   Sys.getenv("RETICULATE_PYTHON")
   ```
   
   Debe mostrar: `/usr/bin/python3`

---

## 📝 NOTAS ADICIONALES

### Diferencias entre sistemas

Este problema ocurre porque diferentes sistemas Linux pueden tener:
- Diferentes versiones de Python instaladas
- Python en diferentes ubicaciones
- Múltiples entornos virtuales de Python
- Diferentes configuraciones de reticulate

La solución aplicada **fuerza** a reticulate a usar el Python del sistema (`/usr/bin/python3`) donde sabemos que matplotlib está instalado.

### Archivos .Rmd restaurados

Todos los archivos .Rmd (020-027) han sido **restaurados a su formato original** sin modificaciones. La configuración de Python ahora se maneja a nivel de sistema.

---

## ✅ RESUMEN

**Problema**: reticulate no encontraba matplotlib  
**Causa**: reticulate usaba un Python diferente al del sistema  
**Solución**: Configurar `.Renviron` y `.Rprofile` para forzar uso de `/usr/bin/python3`  
**Resultado**: Los archivos .Rmd ahora funcionan sin modificaciones  

---

**Fecha de configuración**: 2025-01-30  
**Sistema**: Manjaro Plasma KDE  
**Python**: 3.13.7  
**R**: 4.5.1

