# 📚 Walkthrough: Generación de Ejercicios con el Sistema ICFES R-Exams 2025

## 🎯 **Introducción**

Este walkthrough es una guía práctica para utilizar el **Sistema Integrado ICFES R-Exams 2025**. Aprenderás a transformar una simple imagen (`.png`) de un problema matemático en un ejercicio `.Rmd` completo, interactivo y de alta calidad, siguiendo el flujo de trabajo automatizado del proyecto.

El objetivo es guiarte a través de las fases del sistema, desde el análisis inicial de la imagen hasta la generación del archivo final listo para ser compilado.

---

## 📋 **Tabla de Contenidos**

1.  [Requisitos Previos](#-requisitos-previos)
2.  [El Flujo de Trabajo Principal](#-el-flujo-de-trabajo-principal-paso-a-paso)
3.  [Análisis del Archivo `.Rmd` Generado](#-análisis-del-archivo-rmd-generado)
4.  [Metodologías Avanzadas y Anti-Errores](#-metodologías-avanzadas-y-anti-errores)
5.  [Solución de Problemas Comunes](#-solución-de-problemas-comunes)

---

## 🔧 **Requisitos Previos**

### **Software y Configuración**
Asegúrate de tener el entorno correctamente configurado como se describe en el `README.md`.

```r
# R (≥ 4.0), R-exams, tidyverse, ggplot2, knitr, reticulate, testthat, tinytex
# Python (con matplotlib y numpy)
library(reticulate)
use_python("/usr/bin/python3", required = TRUE) # Verifica tu ruta de Python
```

### **Conocimientos Previos**
-   Manejo básico de la terminal.
-   Comprensión de la estructura de un archivo `.Rmd`.
-   Conceptos básicos de R y, opcionalmente, Python para ejercicios con gráficos.

---

## 🚀 **El Flujo de Trabajo Principal: Paso a Paso**

El núcleo del sistema es un proceso automatizado que se activa con un comando específico. A continuación, se detalla cada etapa.

### **Paso 1: Preparar la Imagen**
Ten a mano el archivo de imagen (`.png`) que contiene el problema matemático que deseas convertir.

### **Paso 2: Activar el Sistema Condicional Automático**
Este es el punto de entrada principal. Usa el siguiente comando para iniciar el proceso:

> **"Aplica el sistema condicional automático a esta imagen PNG para detectar contenido gráfico y activar el flujo apropiado."**

### **Paso 3: Análisis Automático**
Una vez activado, el sistema realiza las siguientes acciones de forma autónoma:
1.  **Analiza la imagen** para determinar si contiene elementos gráficos (diagramas, planos cartesianos, figuras geométricas, etc.).
2.  **Activa uno de los dos flujos de trabajo** según el resultado.

### **Paso 4 (Opción A): Flujo A - Sin Gráficas**
Si la imagen contiene principalmente texto, tablas o fórmulas simples, el sistema activará el **proceso estándar de 8 fases**, que se enfoca en:
-   Extraer el texto del problema.
-   Identificar variables para parametrizar.
-   Diseñar la lógica de generación de datos en R.
-   Construir la pregunta, las opciones de respuesta y la solución.
-   Ensamblar el archivo `.Rmd` final.

### **Paso 4 (Opción B): Flujo B - Con Gráficas**
Si se detecta contenido gráfico, el sistema activa el **Agente-Graficador Especializado TikZ**. Este es un flujo más avanzado:
1.  **Activación del Agente**: Se invoca al agente TikZ para que analice y replique la gráfica.
2.  **Replicación de Alta Fidelidad**: El agente genera código TikZ que reproduce la imagen original con una **fidelidad visual superior al 98%**. Esto incluye colores, proporciones, etiquetas y posicionamiento.
3.  **Validación de Fidelidad**: Antes de continuar, se realiza una validación para asegurar que la réplica es precisa.
4.  **Integración**: El código TikZ generado se integra en un chunk específico dentro del archivo `.Rmd`, asegurando que la gráfica se renderice dinámicamente.
5.  **Generación del Ejercicio**: El resto del ejercicio (pregunta, solución, etc.) se construye alrededor de la gráfica replicada.

### **Paso 5: Generación del Archivo `.Rmd`**
Al finalizar cualquiera de los dos flujos, el sistema guardará un nuevo archivo `.Rmd` en tu directorio de trabajo. El nombre del archivo seguirá la convención del proyecto: `[ejercicio]_[componente]_[competencia]_n[Nivel]_v[versión].Rmd`.

### **Paso 6: Compilación y Verificación**
Abre el archivo `.Rmd` generado en RStudio o tu IDE preferido y compílalo para asegurar que todo funcione correctamente.

```r
# Ejemplo de compilación a HTML
library(exams)
exams2html("nombre_del_archivo_generado.Rmd")
```

---

## 🔬 **Análisis del Archivo `.Rmd` Generado**

Todo archivo generado por el sistema sigue una estructura estricta para garantizar la calidad y consistencia.

1.  **Encabezado YAML**: Incluye todas las librerías `header-includes` necesarias para LaTeX, TikZ y el formato en español.
2.  **Chunk `inicio`**: Configuración global de `knitr`, librerías de R, semilla aleatoria (`set.seed(sample(1:100000, 1))`) y configuración de `reticulate`.
3.  **Chunk `data_generation`**: Contiene la función `generar_datos()` donde reside toda la lógica de aleatorización del ejercicio.
4.  **Chunk `version_diversity_test`**: Una prueba con `testthat` que valida que la función `generar_datos()` puede producir al menos 300 versiones únicas.
5.  **Chunks de Gráficos (si aplica)**:
    -   Un chunk `generar_tikz` para renderizar el diagrama con `include_tikz`.
    -   O un chunk Python con `engine='python'` para gráficos con `matplotlib`.
6.  **Sección `Question`**: El enunciado del problema y la lista de respuestas (`Answerlist`).
7.  **Sección `Solution`**: La explicación detallada del proceso de solución.
8.  **Sección `Meta-information`**: Metadatos ICFES obligatorios y la configuración de `extype`, `exsolution`, etc.

---

## 💡 **Metodologías Avanzadas y Anti-Errores**

### **La Regla de Oro: Consultar Ejemplos Funcionales**
El pilar de la robustez del sistema es el protocolo anti-errores. Si necesitas modificar el código generado o si encuentras un error de compilación, tu primer paso **siempre** debe ser consultar los archivos en:

> `Auxiliares/Ejemplos-Funcionales-Rmd/`

Este directorio es la "fuente de la verdad" y contiene patrones de código probados para:
-   Configuraciones correctas de chunks.
-   Sintaxis de Python con `matplotlib` y `reticulate`.
-   Uso correcto de `include_tikz`.
-   Manejo de variables entre R y Python.

**"Si no está en los ejemplos funcionales, no improvises."**

### **Otros Comandos de Activación**
Aunque el sistema condicional es el punto de partida, puedes invocar metodologías específicas:
-   **Para replicar una gráfica directamente**:
    > "Aplica la metodología TikZ avanzada a esta nueva imagen PNG..."
-   **Para corregir errores en un archivo existente**:
    > "Aplica la metodología de corrección de errores recurrentes a este archivo."

---

##  troubleshooting **Solución de Problemas Comunes**

-   **Error de compilación LaTeX/TikZ**:
    1.  **Verifica los paquetes LaTeX**: Asegúrate de tener instalados `tikz`, `pgfplots`, `xcolor`, etc.
    2.  **Consulta los ejemplos funcionales**: Compara el `header-includes` y el código del chunk `generar_tikz` con un ejemplo que funcione.

-   **Error en chunks de Python (`reticulate`)**:
    1.  **Verifica la configuración de `use_python()`**: Asegúrate de que la ruta al ejecutable de Python es correcta.
    2.  **Compara la sintaxis**: Revisa cómo se pasan las variables de R a Python (`r.variable`) y cómo se guardan las figuras (`plt.savefig`) en los ejemplos funcionales.

-   **La aleatorización no produce suficientes versiones**:
    1.  **Revisa `generar_datos()`**: Asegúrate de que los rangos de `sample()` y los parámetros aleatorios son lo suficientemente amplios.
    2.  **No uses `set.seed()` fijo**: La semilla debe ser aleatoria para cada ejecución. El chunk `inicio` ya se encarga de esto.

Este walkthrough te proporciona las herramientas para usar el sistema de manera efectiva. Al seguir el flujo de trabajo y respetar los protocolos anti-errores, podrás generar ejercicios matemáticos de alta calidad de manera consistente.
