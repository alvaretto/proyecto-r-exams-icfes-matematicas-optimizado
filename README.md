# 📚 RepositorioMatematicasICFES_R_Exams

[![Estado](https://img.shields.io/badge/Estado-Activo-brightgreen)](https://github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado)
[![R-exams](https://img.shields.io/badge/R--exams-Compatible-orange)](https://www.r-exams.org/)
[![Ejercicios](https://img.shields.io/badge/Ejercicios-Optimizados-success)](https://github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado)
[![Aleatorización](https://img.shields.io/badge/Aleatorización-Equilibrada-blue)](https://github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado)

**Sistema completo de ejercicios matemáticos para preparación ICFES con aleatorización equilibrada**

Repositorio especializado en la creación de ejercicios matemáticos de alta calidad para preparación ICFES, utilizando R-exams con algoritmos de aleatorización equilibrada que garantizan distribución uniforme de opciones correctas y máxima variabilidad en cada generación.

---

## 🎯 **Características Principales**

- **⚖️ Aleatorización Equilibrada**: Distribución uniforme garantizada (25% por opción A, B, C, D)
- **🎨 Gráficos TikZ**: Visualizaciones matemáticas generadas dinámicamente
- **📊 Validación Estadística**: Pruebas Chi-cuadrado para verificar uniformidad
- **📋 Competencias ICFES**: Alineación completa con estándares oficiales
- **🔄 Formatos Múltiples**: HTML, PDF, Moodle, Canvas, Blackboard
- **🧮 Ejercicios Híbridos**: Combinación de respuestas numéricas y selección múltiple
## 🚀 **Estado Actual**

✅ **Sistema de Aleatorización Equilibrada Implementado**

- **Algoritmo de distribución uniforme** para opciones correctas
- **Validación estadística** con pruebas Chi-cuadrado
- **Ejercicios optimizados** con aleatorización equilibrada
- **Compatibilidad completa** con R-exams y múltiples formatos
- **Documentación actualizada** y walkthroughs completos

### 🎯 **Ejercicios Disponibles**

✅ **Probabilidad e Intervalos con Aleatorización Equilibrada**
- **Archivos principales**:
  - `probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_cloze_v1.Rmd` (Nivel estándar)
  - `probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_cloze_v1_2.Rmd` (Nivel avanzado)
- **Aleatorización**: Distribución uniforme 25% ± 5% por opción (A, B, C, D)
- **Validación**: Pruebas estadísticas en 50+ versiones confirmadas
- **Formato**: Híbrido cloze (8 respuestas numéricas + 1 selección múltiple)
- **Gráficos**: TikZ dinámico con tablas de probabilidad
- **Diferenciación**: 4 opciones únicas garantizadas en cada versión

## 🛠️ **Tecnologías Utilizadas**

### **Core del Sistema**
- **R** (≥ 4.0) - Motor principal para R-exams
- **R-exams** - Framework de generación de ejercicios
- **LaTeX/TikZ** - Generación de gráficos matemáticos dinámicos
- **HTML/CSS** - Exportación web de ejercicios

### **Paquetes R Requeridos**
```r
# Paquetes esenciales
install.packages(c(
  "exams",      # Framework principal
  "knitr",      # Procesamiento R Markdown
  "rmarkdown",  # Documentos dinámicos
  "tinytex"     # Compilación LaTeX
))
```

### **Formatos de Exportación Soportados**
- **HTML** - Visualización web interactiva
- **PDF** - Documentos imprimibles
- **Moodle XML** - Importación directa a Moodle
- **Canvas QTI** - Compatible con Canvas LMS
- **Blackboard** - Formato para Blackboard Learn

### **Entorno de Desarrollo**
- **RStudio** o **VSCode** - IDEs recomendados
- **Git** - Control de versiones
- **Sistema operativo** - Linux, Windows, macOS

## 📁 **Estructura del Repositorio**

```
RepositorioMatematicasICFES_R_Exams/
├── 📊 01-Numeros-Reales/                   # Ejercicios de números reales
├── 📈 02-Funciones/                        # Ejercicios de funciones
├── 📐 05-Geometría/                        # Ejercicios de geometría
├── 📊 06-Estadística-Y-Probabilidad/       # Ejercicios de estadística
│   └── Pensamiento-Aleatorio/
│       └── 09-Probabilidad-Condicionada_Independencia-De-Sucesos/
│           └── Probabilidad-Intervalos-Curva-13-S1-2024B/
│               ├── probabilidad_intervalos_curva_*_v1.Rmd      # Nivel estándar
│               ├── probabilidad_intervalos_curva_*_v1_2.Rmd    # Nivel avanzado
│               ├── WALKTHROUGH.md                              # Guía de uso
│               └── README.md                                   # Documentación específica
├── 🛠️ Auxiliares/                          # Recursos y herramientas
│   ├── Ejemplos-Funcionales-Rmd/           # Ejercicios de referencia
│   ├── Instalaciones/                      # Scripts de instalación
│   └── Validacion/                         # Herramientas de validación
├── 🧪 Lab-Manjaro/                         # Ejercicios en desarrollo
├── 📖 README.md                            # Este archivo
├── 📖 walkthrough.md                       # Tutorial general
└── 🔧 tools/                               # Herramientas auxiliares
```

## 🚀 **Instalación y Configuración**

### **Requisitos Previos**
```r
# Verificar versión de R (requerida ≥ 4.0)
R.version.string

# Instalar paquetes esenciales
install.packages(c("exams", "knitr", "rmarkdown", "tinytex"))

# Configurar TinyTeX para LaTeX
tinytex::install_tinytex()
```

### **Instalación Rápida**
```bash
# 1. Clonar el repositorio
git clone https://github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado.git
cd RepositorioMatematicasICFES_R_Exams

# 2. Abrir en RStudio o IDE preferido
# 3. Instalar dependencias R (ver sección anterior)
# 4. Probar con un ejercicio de ejemplo
```

### **Verificación de Instalación**
```r
# Cargar librerías principales
library(exams)
library(knitr)

# Verificar que TikZ funciona
system("pdflatex --version")

# Probar generación básica
exams2html("06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/09-Probabilidad-Condicionada_Independencia-De-Sucesos/Probabilidad-Intervalos-Curva-13-S1-2024B/probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_cloze_v1.Rmd",
           name = "test", dir = ".")
```

## 💻 **Uso Básico**

### **Generar Ejercicios HTML**
```r
library(exams)

# Generar una versión HTML del ejercicio de probabilidad (nivel estándar)
exams2html("06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/09-Probabilidad-Condicionada_Independencia-De-Sucesos/Probabilidad-Intervalos-Curva-13-S1-2024B/probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_cloze_v1.Rmd",
           name = "probabilidad_v1",
           dir = "output")

# Generar versión de nivel avanzado
exams2html("06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/09-Probabilidad-Condicionada_Independencia-De-Sucesos/Probabilidad-Intervalos-Curva-13-S1-2024B/probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_cloze_v1_2.Rmd",
           name = "probabilidad_v2",
           dir = "output")
```

### **Generar Múltiples Versiones**
```r
# Generar 10 versiones diferentes para evaluación
for(i in 1:10) {
  exams2html("06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/09-Probabilidad-Condicionada_Independencia-De-Sucesos/Probabilidad-Intervalos-Curva-13-S1-2024B/probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_cloze_v1.Rmd",
             name = paste0("version_", i),
             dir = "evaluacion")
}
```

### **Exportar para LMS**
```r
# Exportar para Moodle
exams2moodle("06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/09-Probabilidad-Condicionada_Independencia-De-Sucesos/Probabilidad-Intervalos-Curva-13-S1-2024B/probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_cloze_v1.Rmd",
             name = "probabilidad_moodle",
             dir = "moodle_export")

# Exportar PDF para impresión
exams2pdf("06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/09-Probabilidad-Condicionada_Independencia-De-Sucesos/Probabilidad-Intervalos-Curva-13-S1-2024B/probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_cloze_v1.Rmd",
          name = "probabilidad_pdf",
          dir = "pdf_export")
```

## 📈 **Ejercicios Disponibles**

### **Probabilidad e Intervalos con Aleatorización Equilibrada**

#### **Archivo Nivel Estándar**: `probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_cloze_v1.Rmd`

**Características:**
- **🎯 Nivel de Dificultad**: 2 (Media)
- **🔢 Precisión Numérica**: 2 decimales (0.XX)
- **📊 Parámetros**:
  - Probabilidad central: 0.40-0.55
  - Límites iniciales: 3-6
  - Límite superior: 14 (fijo)
- **✅ Evaluación**: 7 elementos (6 numéricas + 1 selección múltiple)
- **⚖️ Aleatorización**: Distribución uniforme 25% ± 5% por opción

#### **Archivo Nivel Avanzado**: `probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_cloze_v1_2.Rmd`

**Características:**
- **🎯 Nivel de Dificultad**: 3 (Media-Alta)
- **🔢 Precisión Numérica**: 3 decimales (0.XXX) con tolerancias 0.005
- **📊 Parámetros Ampliados**:
  - Probabilidad central: 0.35-0.65
  - Límites iniciales: 2-8
  - Límite superior: 15-18 (variable)
- **✅ Evaluación Expandida**: 9 elementos (8 numéricas + 1 selección múltiple)
- **🧮 Análisis Adicionales**:
  - Probabilidad fuera del intervalo central
  - Identificación del intervalo con mayor probabilidad
- **⚖️ Aleatorización**: Distribución uniforme verificada estadísticamente

### **Características Comunes**
- **🎨 Gráficos TikZ**: Tablas de probabilidad generadas dinámicamente
- **🔄 Diferenciación**: 4 opciones únicas garantizadas (A, B, C, D)
- **📋 Formato Híbrido**: Combinación cloze + selección múltiple
- **✅ Validación**: Pruebas automáticas de integridad matemática

## 🎯 **Funcionalidades Avanzadas**

### **Sistema de Aleatorización Equilibrada**
```r
# El algoritmo garantiza distribución uniforme de opciones correctas
posicion_correcta_aleatoria <- sample(1:4, 1)  # Selección equiprobable

# Colocación directa elimina sesgos de reorganización
opciones_finales[[posicion_correcta_aleatoria]] <- opcion_correcta

# Verificación estadística automática
# Prueba Chi-cuadrado: p > 0.05 confirma uniformidad
```

### **Validación de Diferenciación**
```r
# Función que garantiza 4 opciones visualmente diferentes
verificar_diferenciacion <- function(opciones) {
  tablas_str <- lapply(opciones, function(tabla) {
    paste(tabla$Intervalo, tabla$Probabilidad, collapse = "|")
  })
  return(length(unique(tablas_str)) == length(tablas_str))
}
```

### **Generación de Gráficos TikZ Dinámicos**
- **Tablas de probabilidad** generadas automáticamente
- **Encabezados alternos** para mayor variabilidad
- **Formato profesional** con alineación y espaciado optimizado
- **Compatibilidad LaTeX** completa para múltiples formatos

### **Formatos de Exportación**
```r
# HTML para visualización web
exams2html(archivo, name = "web_version", dir = "html_output")

# PDF para impresión
exams2pdf(archivo, name = "print_version", dir = "pdf_output")

# Moodle XML para LMS
exams2moodle(archivo, name = "moodle_import", dir = "moodle_output")

# Canvas QTI para Canvas LMS
exams2qti12(archivo, name = "canvas_import", dir = "canvas_output")
```

## 📖 **Documentación**

### **Guías de Uso**
- **[README.md](README.md)** - Este archivo (documentación principal)
- **[walkthrough.md](walkthrough.md)** - Tutorial completo paso a paso
- **[Walkthrough Específico](06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/09-Probabilidad-Condicionada_Independencia-De-Sucesos/Probabilidad-Intervalos-Curva-13-S1-2024B/WALKTHROUGH.md)** - Guía del ejercicio de probabilidad

### **Documentación Técnica**
- **[README Ejercicio](06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/09-Probabilidad-Condicionada_Independencia-De-Sucesos/Probabilidad-Intervalos-Curva-13-S1-2024B/README.md)** - Especificaciones del ejercicio de probabilidad
- **[Estructura del Repositorio](Estructura-Repositorio/Estructura_Repositorio.md)** - Organización de carpetas y archivos

### **Recursos de Apoyo**
- **[Auxiliares/](Auxiliares/)** - Herramientas y recursos adicionales
- **[Ejemplos Funcionales](Auxiliares/Ejemplos-Funcionales-Rmd/)** - Ejercicios de referencia
- **[Validación](Auxiliares/Validacion/)** - Scripts de verificación

### **Referencias Externas**
- **[R-exams.org](https://www.r-exams.org/)** - Documentación oficial de R-exams
- **[ICFES](https://www.icfes.gov.co/)** - Instituto Colombiano para la Evaluación de la Educación
- **[TikZ Documentation](https://tikz.dev/)** - Documentación de TikZ para gráficos LaTeX

## 🤝 **Contribución y Desarrollo**

### **Para Nuevos Colaboradores**
1. **Clonar repositorio**: `git clone https://github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado.git`
2. **Instalar dependencias**: Seguir la sección de instalación
3. **Leer documentación**: Revisar README.md y walkthrough.md
4. **Probar ejercicios**: Generar versiones HTML de prueba
5. **Crear rama**: `git checkout -b nueva-funcionalidad`

### **Estándares de Calidad**
- **✅ Aleatorización Equilibrada**: Distribución uniforme 25% ± 5% por opción
- **✅ Competencias ICFES**: Alineación con estándares oficiales
- **✅ Diferenciación**: 4 opciones únicas garantizadas
- **✅ Validación Estadística**: Pruebas Chi-cuadrado p > 0.05
- **✅ Compatibilidad**: Funcional en HTML, PDF, Moodle
- **✅ Documentación**: Cambios documentados en README y walkthroughs

### **Proceso de Testing**
```r
# 1. Generar múltiples versiones para verificar aleatorización
for(i in 1:20) {
  exams2html(archivo, name = paste0("test_", i), dir = "testing")
}

# 2. Verificar que todas las versiones compilan sin errores
# 3. Validar distribución de opciones correctas
# 4. Confirmar diferenciación entre opciones A, B, C, D
```

### **Estructura de Commits**
```bash
# Formato recomendado para mensajes de commit
git commit -m "[CATEGORÍA] Descripción específica de cambios realizados"

# Ejemplos:
# [EJERCICIO] Nuevo ejercicio de geometría con aleatorización equilibrada
# [CORRECCIÓN] Arreglado error en validación de diferenciación
# [DOCUMENTACIÓN] Actualizado walkthrough con nuevos ejemplos
```

## 🔧 **Solución de Problemas Comunes**

### **Error: "Package 'exams' not found"**
```r
# Instalar R-exams desde CRAN
install.packages("exams")

# Si persiste el error, instalar desde GitHub
devtools::install_github("r-exams/exams")
```

### **Error: "LaTeX not found"**
```r
# Instalar TinyTeX
install.packages("tinytex")
tinytex::install_tinytex()

# Verificar instalación
tinytex::tlmgr_version()
```

### **Error: "TikZ compilation failed"**
```r
# Verificar que pdflatex está disponible
system("pdflatex --version")

# Instalar paquetes LaTeX adicionales si es necesario
tinytex::tlmgr_install("tikz")
tinytex::tlmgr_install("pgfplots")
```

### **Ejercicio no genera opciones diferentes**
- Verificar que la función `verificar_diferenciacion()` está incluida
- Revisar que los parámetros de aleatorización tienen suficiente variabilidad
- Confirmar que el algoritmo de colocación directa está implementado

---

## 📊 **Información del Proyecto**

- **Autor**: Álvaro Ángel Molina
- **Institución**: IE Pedacito de Cielo
- **Propósito**: Preparación ICFES Matemáticas con aleatorización equilibrada
- **Licencia**: Proyecto Educativo
- **Última Actualización**: Septiembre 2025

**🎯 Objetivo**: Proporcionar ejercicios matemáticos de alta calidad para preparación ICFES con sistema de aleatorización equilibrada que garantiza evaluaciones justas y variadas.

---

## 📞 **Soporte y Contacto**

- **Documentación**: Revisar archivos README.md y walkthrough.md
- **Issues**: Reportar problemas en el repositorio de GitHub
- **Contribuciones**: Seguir las guías de contribución en este README

*Para consultas específicas sobre implementación o uso de los ejercicios, consultar la documentación técnica en cada directorio de ejercicios.*
