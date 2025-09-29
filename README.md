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
├── 🛠️ Auxiliares/
│   ├── 📂 Agente-Graficador-TikZ/         # Laboratorio del agente TikZ
│   ├── 📂 Ejemplos-Funcionales-Rmd/       # (OBLIGATORIO) Base de código validado
│   ├── 📂 Estrategia-Avanzada-de-Replicas/ # Templates TikZ profesionales
│   └── 📂 Python-Documentation/             # Guías para integración con Python
├── 🧪 Lab-Manjaro/                         # Ejercicios en desarrollo y pruebas
├── 📄 .gitignore
├── 📖 README.md                            # Este archivo
└── 📖 walkthrough.md                       # Guía de uso detallada
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

El sistema está diseñado para ser operado mediante comandos específicos que activan las metodologías integradas.

**Comando principal:**

> "Aplica el sistema condicional automático a esta imagen PNG para detectar contenido gráfico y activar el flujo apropiado."

Para problemas que requieren replicación gráfica directa:

> "Activa el Agente-Graficador Especializado TikZ para replicar esta imagen con 98%+ fidelidad visual."

El sistema se encargará de analizar la imagen, seleccionar el flujo correcto y generar el archivo `.Rmd` correspondiente en el directorio de trabajo.

---

## 🤝 **Contribución y Calidad**

Cualquier contribución debe adherirse estrictamente a las metodologías y protocolos definidos en el proyecto.

### **Regla de Oro Anti-Errores**
**"Si no está en los ejemplos funcionales, no improvises."** Antes de escribir cualquier código, es **obligatorio** consultar los ejercicios en `Auxiliares/Ejemplos-Funcionales-Rmd/`.

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
