# 📚 RepositorioMatematicasICFES_R_Exams (Filosofía 2025)

[![Estado](https://img.shields.io/badge/Estado-Activo-brightgreen)](https://github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado)
[![R-exams](https://img.shields.io/badge/R--exams-Compatible-orange)](https://www.r-exams.org/)
[![Calidad](https://img.shields.io/badge/Calidad-ICFES_2025-success)](https://github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado)
[![Metodología](https://img.shields.io/badge/Metodología-Sistema_Condicional-blue)](https://github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado)

**Sistema integrado para la generación de ejercicios matemáticos tipo ICFES en formato R-exams a partir de imágenes, siguiendo la filosofía "Matemáticas ICFES 2025".**

Este repositorio implementa un flujo de trabajo completo y automatizado que transforma imágenes de problemas matemáticos en ejercicios `.Rmd` interactivos, parametrizados y de alta calidad, listos para ser compilados con R-exams.

---

## 🎯 **Filosofía del Sistema**

El proyecto se basa en un flujo de trabajo de 3 fases diseñado para maximizar la calidad y la eficiencia:

1.  **📥 Fase 1: Entrada de Datos y Análisis Automático**
    *   Recepción de una imagen (`.png`) con un problema matemático.
    *   **Sistema Condicional Automático**: Detección inteligente de contenido gráfico para activar el flujo de trabajo adecuado.
    *   Protocolo de conversión para imágenes que no siguen el formato ICFES estándar.

2.  **⚙️ Fase 2: Procesamiento y Generación**
    *   **Flujo A (Sin Gráficas)**: Proceso estándar de 8 fases para la generación del `.Rmd`.
    *   **Flujo B (Con Gráficas)**: Activación del **Agente-Graficador Especializado TikZ** para replicar la imagen con una fidelidad visual superior al 98%.
    *   Generación de código `.Rmd` completo, siguiendo una estructura obligatoria y validada.

3.  **🔄 Fase 3: Iteración y Mejora Continua**
    *   Captura de retroalimentación para la corrección de errores.
    *   Implementación de mejoras basadas en patrones identificados para optimizar la generación de código.

---

## ✨ **Características Principales**

-   **🤖 Sistema Condicional Automático**: Activa flujos de trabajo especializados según el contenido de la imagen.
-   **🎨 Agente-Graficador TikZ**: Replica imágenes con una fidelidad visual del 98%+, manejando colores RGB precisos, posicionamiento sistemático y características avanzadas.
-   **🧩 Soporte Híbrido**: Genera ejercicios que integran R, Python (vía `reticulate`), y LaTeX/TikZ para visualizaciones complejas.
-   **✅ Calidad ICFES Garantizada**: Cumple con metadatos obligatorios, sistema avanzado de distractores y criterios de calidad rigurosos.
-   **🔧 Protocolo Anti-Errores**: Utiliza una base de "ejemplos funcionales" para prevenir errores de implementación, asegurando que todo el código generado sea robusto y compilable.
-   **✔️ Pruebas de Diversidad**: Generación mínima de 300 versiones únicas por ejercicio, verificada con `testthat`.

---

## 🛠️ **Tecnologías Utilizadas**

-   **Motor Principal**: **R** (≥ 4.0) y **R-exams**.
-   **Visualización**:
    -   **LaTeX/TikZ**: Para diagramas, figuras geométricas y notaciones matemáticas de alta calidad.
    -   **Python (`matplotlib`, `numpy`)**: Integrado con `reticulate` para gráficos avanzados.
    -   **R (`ggplot2`)**: Para análisis y gráficos estadísticos.
-   **Entorno**: VSCode con herramientas de IA, sobre Manjaro Plasma KDE.
-   **Control de Versiones**: Git y GitHub.

---

## 📁 **Estructura del Repositorio**

```
RepositorioMatematicasICFES_R_Exams/
├── 📂 01-Algebra-Y-Calculo/
├── 📂 05-Geometria/
├── 📂 06-Estadistica-Y-Probabilidad/
├── 🏆 A-Produccion/                        # ⭐ Archivos listos para producción
│   ├── 📂 Ejemplos-Funcionales-Rmd/       # (OBLIGATORIO) Base de código validado
│   ├── 📂 01-Numeros-Reales/              # Ejercicios de producción por tema
│   ├── 📂 02-Funciones/
│   ├── 📂 05-Geometría/
│   └── 📂 06-Estadística-Y-Probabilidad/
├── 🛠️ Auxiliares/
│   ├── 📂 Agente-Graficador-TikZ/         # Laboratorio del agente TikZ
│   ├── 📂 Estrategia-Avanzada-de-Replicas/ # Templates TikZ profesionales
│   └── 📂 Python-Documentation/             # Guías para integración con Python
├── 🧪 Lab-Manjaro/                         # Ejercicios en desarrollo y pruebas
├── 📄 .gitignore
├── 📖 README.md                            # Este archivo
└── 📖 walkthrough.md                       # Guía de uso detallada
```

---

## 🏆 **Carpeta A-Produccion: Archivos Listos para Uso**

### 📋 **Descripción**

La carpeta **`A-Produccion/`** contiene archivos `.Rmd` y subproyectos funcionales que han sido validados y están **listos para producción**. Estos archivos representan el estándar de calidad del proyecto y pueden utilizarse directamente o con ajustes mínimos.

### ✨ **Características de los Archivos en A-Produccion**

- ✅ **Completamente funcionales**: Compilables sin errores en R-exams
- ✅ **Validados**: Han pasado todas las pruebas de calidad y diversidad (300+ versiones)
- ✅ **Documentados**: Incluyen comentarios y estructura clara
- ✅ **Optimizados**: Siguen las mejores prácticas del proyecto
- ✅ **Modelos de referencia**: Pueden usarse como plantillas para nuevos ejercicios

### 📂 **Contenido de A-Produccion**

```
A-Produccion/
├── 📂 Ejemplos-Funcionales-Rmd/       # ⭐ Base de código validado (CONSULTA OBLIGATORIA)
│   ├── Plantillas/                    # Templates por tipo de ejercicio
│   │   ├── Rmd/                       # Plantillas .Rmd (schoice, cloze, etc.)
│   │   ├── Rnw/                       # Plantillas .Rnw
│   │   └── TikZ-Documentation/        # Documentación y ejemplos TikZ
│   └── [Ejercicios validados]         # Ejercicios funcionales de referencia
├── 📂 01-Numeros-Reales/              # Ejercicios de producción por tema
├── 📂 02-Funciones/
├── 📂 05-Geometría/
└── 📂 06-Estadística-Y-Probabilidad/
```

### 🎯 **Uso Recomendado**

1. **Como referencia**: Consultar antes de crear nuevos ejercicios
2. **Como plantilla**: Copiar y adaptar para nuevos problemas similares
3. **Como validación**: Comparar tu código con estos ejemplos funcionales
4. **Como aprendizaje**: Estudiar las mejores prácticas implementadas

### ⚠️ **Importante**

> **Regla de Oro**: "Si no está en los ejemplos funcionales, no improvises."
>
> Antes de escribir cualquier código nuevo, es **OBLIGATORIO** consultar los ejercicios en `/A-Produccion/Ejemplos-Funcionales-Rmd/` para seguir los patrones validados.

---

## 📦 **Cambio de Ubicación: Ejemplos-Funcionales-Rmd**

### 🔄 **Reorganización del Repositorio**

La carpeta **`Ejemplos-Funcionales-Rmd`** ha sido **reubicada** para mejorar la organización del proyecto y destacar su importancia como base de código de producción.

### 📍 **Ubicaciones**

| Aspecto | Detalle |
|---------|---------|
| **Ubicación Anterior** | `/Auxiliares/Ejemplos-Funcionales-Rmd/` |
| **Ubicación Nueva** | `/A-Produccion/Ejemplos-Funcionales-Rmd/` |
| **Fecha del Cambio** | Diciembre 2025 |
| **Razón del Cambio** | Destacar archivos de producción y mejorar organización |

### 🎯 **Propósito del Cambio**

1. **Mayor visibilidad**: Los ejemplos funcionales ahora están en una carpeta de producción destacada
2. **Mejor organización**: Separación clara entre archivos de producción y auxiliares
3. **Facilitar acceso**: Ubicación más intuitiva para archivos de referencia obligatoria
4. **Estandarización**: Agrupar todos los archivos listos para producción en un solo lugar

### 📝 **Actualización de Referencias**

Todas las referencias en el código, documentación y configuraciones han sido actualizadas para reflejar la nueva ubicación:

- ✅ Archivos de configuración (`.json`, `.yaml`)
- ✅ Documentación (`.md`)
- ✅ Reglas de IA (`.cursor/rules/`, `.augment/rules/`)
- ✅ Scripts y herramientas
- ✅ README y guías de uso

### 🔧 **Acción Requerida**

Si tienes scripts o configuraciones personales que referencian la ubicación antigua, actualízalas a:

```
/A-Produccion/Ejemplos-Funcionales-Rmd/
```

---

## 🚀 **Instalación y Uso**

### **Requisitos Previos**

Asegúrate de tener instalado R (≥ 4.0), una distribución de LaTeX (se recomienda `tinytex`) y Python.

```r
# 1. Instalar paquetes esenciales de R
install.packages(c("exams", "tidyverse", "ggplot2", "knitr", "reticulate", "testthat", "tinytex"))

# 2. Configurar TinyTeX
tinytex::install_tinytex()

# 3. Configurar Python para reticulate
library(reticulate)
use_python("/usr/bin/python3", required = TRUE) # Ajusta la ruta a tu ejecutable de Python
```

### **Uso del Sistema**

El sistema está diseñado para ser operado mediante **Skills de Claude Code** que automatizan el workflow completo.

#### **Skills Disponibles** (Workflow Automatizado)

El proyecto incluye 7 skills configurados en `.claude/skills/` para automatizar cada fase del workflow:

**Workflow Principal:**
- `/analizar-icfes` - Análisis ICFES de imagen según 6 dimensiones (Fase 1)
- `/generar-schoice` - Generar ejercicio de selección única (Fase 3)
- `/generar-cloze` - Generar ejercicio de respuesta abierta (Fase 3)
- `/promover-ejercicio` - Promoción a carpeta de producción (Fase 7)

**Skills de Soporte:**
- `/corregir-error-imagen` - Corrección automática de errores TikZ
- `/validar-diversidad` - Validar 300+ versiones únicas
- `/validar-icfes` - Validar metadatos y estructura R-exams

#### **Ejemplo de Uso con Skills**

```bash
# 1. Analizar imagen de ejercicio ICFES
/analizar-icfes imagen_ejercicio.png

# 2. Generar ejercicio SCHOICE basado en el análisis
/generar-schoice

# 3. Validar diversidad de versiones
/validar-diversidad archivo_generado.Rmd

# 4. Promover a producción después de validar
/promover-ejercicio archivo_generado.Rmd
```

#### **Comandos Tradicionales (Alternativos)**

También puedes usar comandos en lenguaje natural:

> "Aplica el sistema condicional automático a esta imagen PNG para detectar contenido gráfico y activar el flujo apropiado."

Para problemas que requieren replicación gráfica directa:

> "Activa el Agente-Graficador Especializado TikZ para replicar esta imagen con 98%+ fidelidad visual."

El sistema se encargará de analizar la imagen, seleccionar el flujo correcto y generar el archivo `.Rmd` correspondiente en el directorio de trabajo.

---

## 🤖 **Configuración de Claude Code**

El proyecto incluye configuración completa en `.claude/` para automatizar el workflow:

```
.claude/
├── settings.json          # Hooks y configuración global
├── settings.local.json    # Permisos para skills
├── skills/                # 7 skills del workflow automatizado
│   ├── analizar-icfes/
│   ├── generar-schoice/
│   ├── generar-cloze/
│   ├── promover-ejercicio/
│   ├── corregir-error-imagen/
│   ├── validar-diversidad/
│   └── validar-icfes/
└── docs/                  # Documentación del workflow
    ├── WORKFLOW_PASO_A_PASO.md
    ├── GUIA_USUARIO.md
    └── [otros archivos]
```

**Características:**
- ✅ Hooks configurados para recordatorios automáticos
- ✅ Permisos preconfigurados para ejecución sin confirmación
- ✅ Documentación completa del workflow paso a paso
- ✅ Validación automática de estructura ICFES

---

## 🤝 **Contribución y Calidad**

Cualquier contribución debe adherirse estrictamente a las metodologías y protocolos definidos en el proyecto.

### **Regla de Oro Anti-Errores**
**"Si no está en los ejemplos funcionales, no improvises."** Antes de escribir cualquier código, es **obligatorio** consultar los ejercicios en `/A-Produccion/Ejemplos-Funcionales-Rmd/`.

### **Criterios de Calidad**
-   **Fidelidad Visual (98%+)**: Precisión geométrica, cromática, de posicionamiento y completitud.
-   **Funcionalidad R-exams (100%)**: Compatibilidad con `exams2*`, 300+ versiones, y aleatorización completa.
-   **Alineación ICFES**: Metadatos completos, distractores pedagógicos y nivel de dificultad apropiado.

---

## 📊 **Información del Proyecto**

-   **Autor**: Álvaro Ángel Molina
-   **Institución**: IE Pedacito de Cielo
-   **Propósito**: Generación de ejercicios matemáticos de alta calidad para la preparación de la prueba ICFES Saber 11°.
-   **Licencia**: Proyecto Educativo
-   **Última Actualización**: Septiembre 2025

Este proyecto representa un sistema integral y robusto que encapsula las mejores prácticas para la creación de contenido educativo parametrizado y de alta calidad.
