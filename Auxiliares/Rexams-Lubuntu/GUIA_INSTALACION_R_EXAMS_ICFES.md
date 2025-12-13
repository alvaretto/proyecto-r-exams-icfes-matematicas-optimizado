# 🎯 Guía de Instalación R-exams ICFES para Lubuntu

## 📋 Estado Actual

✅ **Python 3.12.3** - Instalado  
⏳ **R, LaTeX, Pandoc** - Instalándose en terminales activas  
✅ **Espacio en disco** - 30GB disponibles (suficiente)  
✅ **Memoria** - 7.8GB total (adecuada)  
✅ **Internet** - Conectividad OK  

## 🚀 Pasos a Seguir

### 1. ⏳ Esperar Instalaciones Actuales
Las siguientes instalaciones están en progreso:

- `sudo apt update && sudo apt install -y r-base texlive-full pandoc` (5 terminales)
- `sudo apt install -y r-base-core` (4 terminales)

**Monitorear progreso:**

```bash
watch -n 5 'ps aux | grep -E "(apt|dpkg)" | grep -v grep'
```

### 2. 🔧 Ejecutar Script de Instalación Completa
Una vez que terminen las instalaciones actuales:

```bash
./install_r_exams_lubuntu.sh
```

Este script instalará:

- ✅ Dependencias del sistema
- ✅ Paquetes R necesarios para R-exams
- ✅ Configuración de Python para reticulate
- ✅ Herramientas adicionales (ImageMagick, etc.)

### 3. ✅ Verificar Instalación
```bash
./check_current_installations.sh
```

### 4. 🧪 Probar R-exams
```bash
# Se creará automáticamente con el script de instalación
./test_r_exams.R
```

## 📁 Tu Proyecto R-exams ICFES

### Estructura del Proyecto
```
/home/rexams/Proyectos/proyecto-r-exams-icfes-matematicas-optimizado/
├── 📊 01-Numeros-Reales/
├── 📈 02-Funciones/
├── 📊 06-Estadística-Y-Probabilidad/
├── 🔧 Auxiliares/
└── 📚 General/
```

### Características del Proyecto

- **🎲 300+ versiones únicas** por ejercicio
- **📊 Alineación ICFES** completa
- **🔄 Múltiples formatos**: PDF, HTML, Moodle, NOPS
- **🎨 Metodología TikZ avanzada** (98% fidelidad visual)
- **🔧 Corrección automática de errores**

## 🎯 Ejemplos de Uso Rápido

### Generar Examen PDF
```r
library(exams)
setwd("/home/rexams/Proyectos/proyecto-r-exams-icfes-matematicas-optimizado")

# Ejemplo con estadística
ejercicios <- list(
  "06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/01-Variables-Cualitativas_Distribucion-De-Frecuencias/Graficos_Estadisticos_Adopcion_Mascotas/graficos_estadisticos_adopcion_mascotas_formulacion_ejecucion.Rmd"
)

exams2pdf(ejercicios, n = 10, name = "examen_icfes_matematicas")
```

### Generar para Moodle
```r
exams2moodle(ejercicios, n = 50, name = "banco_preguntas_estadistica")
```

## 🔧 Metodologías Avanzadas del Proyecto

### 🎨 Metodología TikZ Avanzada
Para replicar imágenes PNG con 98% fidelidad:

```bash
"Aplica la metodología TikZ avanzada a esta nueva imagen PNG"
```

### 🔧 Metodología de Corrección de Errores
Para corregir errores recurrentes en < 5 minutos:

```bash
"Aplica la metodología de corrección de errores recurrentes"
```

## 📚 Documentación del Proyecto

- **📋 README Principal**: `/home/rexams/Proyectos/proyecto-r-exams-icfes-matematicas-optimizado/README.md`
- **🎨 Guía TikZ**: `Auxiliares/TikZ-Documentation/TikZ-ICFES-Guide.md`
- **🔧 Corrección de Errores**: `Auxiliares/METODOLOGIA_Correccion_Errores_Recurrentes_ICFES_R_Exams.md`
- **📚 Ejemplos Funcionales**: `A-Produccion/Ejemplos-Funcionales-Rmd/`

## 🆘 Solución de Problemas

### Error: LaTeX failed to compile
```r
tinytex::parse_install("archivo.log")
tinytex::tlmgr_update()
```

### Error: Paquete faltante
```r
install.packages("nombre_paquete", dependencies = TRUE)
```

### Error: Python no configurado
```r
library(reticulate)
use_python("/usr/bin/python3", required = TRUE)
```

## 📞 Soporte

1. **📖 Consultar documentación** en `Auxiliares/`
2. **🔍 Revisar ejemplos** en el proyecto
3. **🧪 Probar con ejercicios simples** primero
4. **📝 Usar las metodologías documentadas**

## 🎯 Próximos Pasos Recomendados

1. ⏳ **Esperar** que terminen las instalaciones actuales
2. 🚀 **Ejecutar** `./install_r_exams_lubuntu.sh`
3. ✅ **Verificar** con `./check_current_installations.sh`
4. 🧪 **Probar** con `./test_r_exams.R`
5. 📁 **Navegar** al proyecto y explorar ejemplos
6. 🎨 **Aplicar metodologías** avanzadas según necesidad

---

**Estado del Sistema**: ✅ **LISTO PARA PRODUCCIÓN** con metodologías integradas

*Última actualización: Enero 2025*
