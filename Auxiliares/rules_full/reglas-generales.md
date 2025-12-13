# 🎯 REGLAS GENERALES INTEGRADAS - SISTEMA ICFES R-EXAMS 2025

## 📋 FILOSOFÍA MATEMÁTICAS ICFES 2025

### 🎯 OBJETIVO PRINCIPAL
Sistema integrado para generar ejercicios matemáticos ICFES en formato R-exams (.Rmd) a partir de imágenes, siguiendo el flujo de trabajo específico de la filosofía Matemáticas ICFES 2025.

### 🔄 FLUJO DE TRABAJO COMPLETO

#### **FASE 1: ENTRADA DE DATOS**
1. **Recepción de imagen**: Usuario sube imagen con escenario matemático
2. **Análisis automático de formato**:
   - Si la imagen **ES formato ICFES**: Proceder directamente a Fase 2
   - Si la imagen **NO ES formato ICFES**: Activar protocolo de conversión
3. **Protocolo de conversión (cuando NO es formato ICFES)**:
   - Preguntar al usuario por: Competencia ICFES, Componente ICFES, Nivel ICFES
   - Solicitar tipo de pregunta (Schoice, Cloze, etc.)
   - Recopilar metadatos relevantes según filosofía Matemáticas ICFES 2025
   - Convertir el escenario al formato ICFES estándar

#### **FASE 2: PROCESAMIENTO Y GENERACIÓN**
1. **Conversión a código .Rmd**: Exportable para RStudio
2. **Asegurar compatibilidad** con archivos de dependencia:
   - `SemilleroCloze.R`
   - `SemilleroMoodle_v2.R` 
   - `SemilleroUnico_v2.R`
   - `pcielo.tex`
   - `pcielo_nosol.tex`
   - `solpcielo.tex`

#### **FASE 3: ITERACIÓN Y MEJORA**
1. **Capturar retroalimentación** del usuario ante errores
2. **Implementar aprendizaje automático** para mejorar código .Rmd generado
3. **Optimizar soluciones** basadas en patrones identificados

### 🔍 CLASIFICACIÓN AUTOMÁTICA DE TIPOS DE PREGUNTA

El sistema debe identificar y manejar estas variantes:

#### **VARIANTES BÁSICAS**
- ✅ Preguntas sin/con imágenes matemáticas
- ✅ Preguntas sin/con imágenes como opciones de respuesta
- ✅ Preguntas sin/con código TikZ (gráficas, diagramas, figuras geométricas)
- ✅ Preguntas sin/con código Python (Reticulate) para visualizaciones
- ✅ Preguntas sin/con código R para análisis y gráficas
- ✅ Preguntas sin/con código LaTeX (TikZ, pgfplots)

#### **VARIANTES HÍBRIDAS**
- 🔄 Combinaciones de las anteriores
- 🎯 Para cada variante: definir flujo específico de código .Rmd optimizado

## 🎯 PREFERENCIAS GENERALES DEL USUARIO

### 📝 COMUNICACIÓN Y IDIOMA
- **Idioma**: Responder siempre en español
- **Enfoque**: Procesos lentos pero efectivos desde cero
- **Prioridad**: Precisión y calidad sobre velocidad
- **Implementación**: Completa, no versiones simplificadas
- **Documentación**: No generar resúmenes automáticos ni documentación extensa a menos que se solicite específicamente

### 🛠️ CONFIGURACIÓN TÉCNICA
- **Entorno**: VSCode con herramientas de IA como Augment integradas
- **Sistema**: Manjaro Plasma KDE nativo (no contenedorizado)
- **Agente TikZ**: Funcionar directamente con imágenes sin requerir activación de entornos virtuales
- **Integración**: Mantener integración con Augment IA dentro de VSCode
- **Persistencia**: Proyecto GitHub alvaretto/proyecto-r-exams-icfes-matematicas-optimizado

### 📊 TECNOLOGÍAS Y HERRAMIENTAS
- **Archivos principales**: Solo trabajar con archivos .Rmd
- **Python**: Usar chunks de Python con reticulate y engine='python'
- **Gráficos**: 
    - Usar TikZ para diagramas, figuras geométricas, notaciones y fórmulas matemáticas
    - Integrar matplotlib y numpy en chunks Python
- **Testing**: Documentar instrucciones desde RStudio en archivos README
- **TikZ**: Usar también para generar imágenes en archivos .Rnw
- **Etiquetas**: En diagramas TikZ usar negrita cursiva
- **Templates**: Adaptar templates TikZ específicos con estructura axis/addplot

### 🎨 METODOLOGÍAS INTEGRADAS

#### **SISTEMA CONDICIONAL AUTOMÁTICO**
- **Detección automática** de contenido gráfico en imágenes PNG
- **Activación inteligente** de flujos especializados:
  * **FLUJO A** (sin gráficas): Proceso estándar 8 fases
  * **FLUJO B** (con gráficas): Agente-Graficador Especializado TikZ
- **Validación de fidelidad visual** 98%+ antes de continuar
- **Comando**: "Aplica el sistema condicional automático a esta imagen PNG"

#### **METODOLOGÍA TIKZ AVANZADA**
- **Consultar ejemplos funcionales** en `/A-Produccion/Ejemplos-Funcionales-Rmd/`
- **Replicación PNG** con 98% fidelidad visual
- **Características TikZ avanzadas** con colores RGB precisos
- **Posicionamiento sistemático** de elementos
- **Integración** en Agente-Graficador Especializado del sistema condicional

#### **METODOLOGÍA CORRECCIÓN DE ERRORES RECURRENTES**
- **Detección automática** de 5 categorías de errores:
  * A) Gramaticales/Concordancia (ej: "La conteo" → "El conteo")
  * B) Posicionamiento TikZ (orden texto → tabla → pregunta)
  * C) Generación de datos (opciones únicas, anti-duplicados)
  * D) Compilación LaTeX/TikZ (paquetes, caracteres especiales)
  * E) Estructura R-exams (YAML, include_tikz, variables)

#### **PROTOCOLO ANTI-ERRORES DE IMPLEMENTACIÓN**
- **Prevención sistemática** de errores durante implementación
- **OBLIGATORIO**: Consultar ejemplos funcionales ANTES de escribir código
- **Validación continua** chunk por chunk con compilación incremental
- **REGLA DE ORO**: "Si no está en ejemplos funcionales, no improvises"

## 📁 ESTRUCTURA DE ARCHIVOS Y UBICACIONES

### 🎯 DIRECTORIOS CLAVE
- **Ejemplos funcionales**: `/A-Produccion/Ejemplos-Funcionales-Rmd/`
- **Templates TikZ profesionales**: `/Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/Ejemplo/`
- **Agente TikZ**: `/Auxiliares/Agente-Graficador-TikZ/Laboratorio_Agente_TikZ`
- **Documentación Python**: `/Auxiliares/Python-Documentation/Python-ICFES-Guide.md`

### 📋 ARCHIVOS DE DEPENDENCIA
- **SemilleroCloze.R**: Generación de ejercicios tipo Cloze
- **SemilleroMoodle_v2.R**: Exportación a formato Moodle
- **SemilleroUnico_v2.R**: Generación unificada de ejercicios
- **pcielo.tex**: Template LaTeX principal
- **pcielo_nosol.tex**: Template sin soluciones
- **solpcielo.tex**: Template solo soluciones

## 🎯 CRITERIOS DE CALIDAD ICFES

### 📊 EJERCICIOS R-EXAMS ICFES
- **Datasets**: Incluir números pares e impares para cálculos de mediana
- **Opciones de respuesta**: Evitar opciones idénticas, asegurar 4 valores diferentes
- **Datos únicos**: No duplicar valores (problemas de moda)
- **Orden de tabla**: No revelar método de solución en visualización
- **TikZ**: Mantener funcionalidad, buscar ejemplos funcionales antes de remover

### 🎯 SISTEMA AVANZADO DE DISTRACTORES
- **Crear 5+ distractores diferentes** y seleccionar 3 aleatoriamente
- **Variedad en explicaciones** para conceptos estadísticos
- **30% probabilidad** de valores duplicados con justificaciones diferentes
- **8+ tipos de distractores** con selección estratégica
- **Verificación textual única** y justificaciones alternativas ampliadas

### 📋 METADATOS ICFES OBLIGATORIOS
```yaml
icfes:
  competencia: [interpretacion_representacion|formulacion_ejecucion|argumentacion]
  nivel_dificultad: [1|2|3|4]
  contenido:
    categoria: [algebra_calculo|geometria|estadistica]
    tipo: [generico|no_generico]
  contexto: [familiar|laboral|comunitario|matematico]
  eje_axial: [eje1|eje2|eje3|eje4]
  componente: [geometrico_metrico|numerico_variacional|aleatorio]
```

## ⚡ COMANDOS DE ACTIVACIÓN

### 🤖 SISTEMA PRINCIPAL
- **Sistema Condicional**: "Aplica el sistema condicional automático a esta imagen PNG para detectar contenido gráfico y activar el flujo apropiado"
- **TikZ Avanzado**: "Aplica la metodología TikZ avanzada a esta nueva imagen PNG para generar un ejercicio R-exams completo con salidas exams2*"
- **Corrección de Errores**: "Aplica la metodología de corrección de errores recurrentes"
- **Anti-Errores**: "Aplica el protocolo anti-errores de implementación"

### 🎯 AGENTE-GRAFICADOR ESPECIALIZADO
- **Activación**: "Activa el Agente-Graficador Especializado TikZ para replicar esta imagen con 98%+ fidelidad visual"
- **Validación**: "Ejecuta la validación de fidelidad visual comparando el TikZ generado con la imagen original"

## 📊 MÉTRICAS DE ÉXITO

### ✅ FIDELIDAD VISUAL (98%+)
- **Precisión Geométrica** (25%): Proporciones, ángulos, escalas
- **Fidelidad Cromática** (25%): Colores RGB exactos, contrastes
- **Posicionamiento** (25%): Ubicación relativa de elementos
- **Completitud** (25%): Todos los elementos presentes

### ✅ FUNCIONALIDAD R-EXAMS (100%)
- **Compatibilidad**: Sistema exams2* completo
- **Versiones**: 300+ generables automáticamente
- **Aleatorización**: Contenido completamente parametrizado
- **Formatos**: HTML, PDF, Moodle funcionales

### ✅ CALIDAD EDUCATIVA ICFES
- **Nivel apropiado** según clasificación
- **Argumentación matemática** sólida
- **Distractores pedagógicos** efectivos
- **Metadatos completos** y correctos

## 🔧 ESTADO DEL SISTEMA

**✅ SISTEMA OPERATIVO Y LISTO PARA USO INMEDIATO**

Todas las metodologías están integradas y validadas:

- Sistema Condicional Automático
- Metodología TikZ Avanzada  
- Corrección de Errores Recurrentes
- Protocolo Anti-Errores de Implementación

El sistema está preparado para generar ejercicios ICFES de alta calidad siguiendo la filosofía Matemáticas ICFES 2025.

---

## 📋 ESTRUCTURA OBLIGATORIA DEL ARCHIVO .RMD

### 1. ENCABEZADO YAML COMPLETO
```yaml
---
output:
  pdf_document:
    latex_engine: xelatex
    keep_tex: true
  html_document:
    df_print: paged
    mathjax: true
  word_document: default
header-includes:
- \usepackage[spanish]{babel}
- \usepackage{amsmath}
- \usepackage{fontspec}
- \usepackage{unicode-math}
- \usepackage{graphicx}
- \usepackage{adjustbox}
- \usepackage{tikz}
- \usepackage{pgfplots}
- \usetikzlibrary{3d,babel}
---
```

### 2. CHUNK DE CONFIGURACIÓN INICIAL
```r
```{r inicio, include=FALSE}
# Librerías esenciales
library(exams)
library(tidyverse)
library(ggplot2)
library(knitr)
library(reticulate)
library(testthat)
library(data.table)
library(readxl)
library(datasets)

# Configurar Python si es necesario
use_python("/usr/bin/python3", required = TRUE)

# Configuración global
typ <- match_exams_device()
options(scipen = 999)
options(OutDec = ".")
options(digits = 10)

# Configuración de locale para formato numérico consistente
Sys.setlocale(category = "LC_NUMERIC", locale = "C")

knitr::opts_chunk$set(
  warning = FALSE,
  message = FALSE,
  fig.keep = 'all',
  dev = c("png", "pdf"),
  dpi = 150,
  echo = FALSE,
  results = "hide"
)

# Semilla aleatoria para diversidad de versiones
set.seed(sample(1:100000, 1))
```

### 3. CHUNK DE GENERACIÓN DE DATOS
```r
```{r data_generation, echo=FALSE, results="hide"}
# Configuración de formato numérico estándar
options(OutDec = ".")
options(scipen = 999)
options(digits = 10)

# Función para formatear números enteros sin notación científica
formatear_entero <- function(numero) {
  formatC(numero, format = "d", big.mark = "")
}

# Función de formato estándar para números
formato_estandar <- function(x, decimales = 0) {
  if (decimales == 0) {
    return(as.character(as.integer(x)))
  } else {
    resultado <- sprintf(paste0("%.", decimales, "f"), x)
    return(resultado)
  }
}

# Función principal de generación de datos
generar_datos <- function() {
  # IMPLEMENTAR LÓGICA ESPECÍFICA SEGÚN EL PROBLEMA
  # Debe generar al menos 300 versiones únicas
  # Incluir validaciones y manejo de errores
  # Retornar lista con todos los parámetros necesarios
}

# Generar datos del ejercicio
datos <- generar_datos()
```

### 4. CHUNK DE PRUEBA DE DIVERSIDAD
```r
```{r version_diversity_test, echo=FALSE, results="hide"}
# Prueba obligatoria de diversidad de versiones
test_that("Prueba de diversidad de versiones", {
  versiones <- list()
  for(i in 1:1000) {
    datos_test <- generar_datos()
    versiones[[i]] <- digest::digest(datos_test)
  }

  n_versiones_unicas <- length(unique(versiones))
  expect_true(n_versiones_unicas >= 300,
              info = paste("Solo se generaron", n_versiones_unicas,
                          "versiones únicas. Se requieren al menos 300."))
})
```

### 5. CHUNKS DE GRÁFICOS Y VISUALIZACIONES

#### Para gráficos con ggplot2:
```r
```{r generar_graficos_r, echo=FALSE, results="asis"}
# Crear gráficos usando ggplot2
grafico_principal <- ggplot(data = datos_grafico) +
  geom_[tipo_apropiado](...) +
  theme_minimal() +
  labs(title = "...", x = "...", y = "...") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )

# Guardar gráfico
ggsave("grafico_principal.png", grafico_principal,
       width = 8, height = 6, dpi = 150)
```

#### Para gráficos con Python:
```python
```{python generar_graficos_python, echo=FALSE, results="hide"}
import matplotlib.pyplot as plt
import numpy as np
import random

# Recibir datos desde R
datos_r = r.datos_variable

# Configuración de colores aleatorios
colores_disponibles = ['blue', 'green', 'red', 'purple', 'orange', 'brown']
color_principal = random.choice(colores_disponibles)

# Crear gráfico
fig, ax = plt.subplots(figsize=(8, 6))
# [Implementar lógica específica del gráfico]

plt.tight_layout()
plt.savefig('grafico_python.png', dpi=150, bbox_inches='tight')
plt.close()
```

#### Para diagramas TikZ:
```r
```{r generar_tikz, echo=FALSE, results="asis"}
# Plantilla TikZ parametrizada
tikz_diagram <- '
\\begin{tikzpicture}[scale=1.2]
  % [Código TikZ específico según el problema]
\\end{tikzpicture}
'

# Renderizar con include_tikz
include_tikz(tikz_diagram,
             name = "diagrama_tikz",
             markup = "markdown",
             format = typ,
             library = c("3d", "babel"),
             packages = c("tikz", "xcolor", "pgfplots"),
             width = "10cm")
```

### 6. SECCIÓN QUESTION
```markdown
Question
========

[Contexto del problema basado en la imagen analizada]

[Descripción clara y precisa del escenario matemático]

[Pregunta específica que evalúa la competencia ICFES correspondiente]

Answerlist
----------
- [Opción A con justificación matemática]
- [Opción B - distractor plausible]
- [Opción C - distractor plausible]
- [Opción D - distractor plausible]
```

### 7. SECCIÓN SOLUTION
```markdown
Solution
========

[Explicación detallada del proceso de solución]

[Gráficos o diagramas de apoyo si es necesario]

[Justificación matemática completa]

Answerlist
----------
- Verdadero/Falso para cada opción con explicación
```

### 8. META-INFORMATION OBLIGATORIA
```markdown
Meta-information
================
exname: [Nombre descriptivo del ejercicio]
extype: schoice|cloze
exsolution: [Patrón de respuesta, ej: 1000]
exclozetype: [Para tipo cloze: schoice|num|string separados por |]
extol: [Para tipo cloze: tolerancias separadas por |]
exshuffle: TRUE
exsection: [Sección temática]
```

**CONFIGURACIÓN CRÍTICA PARA EJERCICIOS TIPO CLOZE:**
- **Tolerancias numéricas**: Usar tolerancia ≥ 1 para valores monetarios grandes
- **Tolerancias schoice**: Mantener en 0 (exactitud requerida)
- **Formato de números**: Sin separador de miles, punto para decimales
- **Ejemplo tolerancias**: `extol: 0|0|1|1|0|1|0` (schoice=0, numéricas=1)

---

## 🎯 CRITERIOS DE CALIDAD OBLIGATORIOS

### ALEATORIZACIÓN INTELIGENTE:
- **Estándar del proyecto**: Mínimo 300 versiones únicas verificadas con test
- **Diversidad matemáticamente relevante**: Enfocar en aspectos que cambien la experiencia matemática
- **Evitar aleatorización superficial**: No diversificar elementos cosméticos sin valor educativo
- **Parámetros numéricos estratégicos**: Rangos que generen diferentes tipos de problemas
- **Contextos educativamente distintos**: Solo cuando aporten valor pedagógico real
- **Distractores conceptuales**: Representar errores reales que cometen estudiantes

### ROBUSTEZ MATEMÁTICA:
- Validaciones de coherencia matemática
- Manejo de casos extremos
- Precisión numérica apropiada
- Unidades consistentes
- **CONFIGURACIÓN DE TOLERANCIAS APROPIADAS**:
  * Tolerancia 0 para respuestas schoice (exactitud requerida)
  * Tolerancia ≥ 1 para respuestas numéricas con valores grandes
  * Formato estándar: sin separador de miles, punto decimal
  * Evitar notación científica: `options(scipen = 999)`

### CALIDAD GRÁFICA:
- Resolución mínima 150 DPI
- Etiquetas claras y legibles
- Colores contrastantes
- Escalas apropiadas
- Leyendas cuando sea necesario

### ALINEACIÓN ICFES:
- Competencia claramente evaluada
- Nivel de dificultad apropiado
- Contexto realista y relevante
- Distractores plausibles y educativos

## 📊 TIPOS DE PROBLEMAS ESPECÍFICOS

### ÁLGEBRA Y CÁLCULO:
- Funciones lineales, cuadráticas, exponenciales
- Sistemas de ecuaciones
- Optimización
- Límites y derivadas básicas

### GEOMETRÍA:
- Áreas y perímetros
- Volúmenes y superficies
- Teorema de Pitágoras
- Trigonometría básica
- Transformaciones geométricas

### ESTADÍSTICA Y PROBABILIDAD:
- Medidas de tendencia central
- Gráficos estadísticos
- Probabilidad básica
- Distribuciones de frecuencia

## 🔧 HERRAMIENTAS TÉCNICAS AVANZADAS

### INTEGRACIÓN R-PYTHON:
- Usar reticulate para gráficos complejos
- Transferir datos entre R y Python
- Aprovechar matplotlib para visualizaciones avanzadas

### TIKZ PARA DIAGRAMAS:
- Geometría precisa
- Diagramas matemáticos profesionales
- Anotaciones y etiquetas

### CONFIGURACIÓN DE TOLERANCIAS PARA EVALUACIÓN AUTOMÁTICA:
- **Identificar tipos de respuesta**: schoice vs numéricas
- **Configurar tolerancias apropiadas**:
  * schoice: tolerancia 0 (exactitud requerida)
  * numéricas: tolerancia ≥ 1 para valores grandes (monetarios, enteros)
  * numéricas: tolerancia 0.01-0.1 para valores decimales pequeños
- **Documentar configuración**: Comentarios explicativos en código
- **Validar funcionamiento**: Tests para verificar evaluación correcta

### TESTING AUTOMATIZADO:
- Verificar diversidad de versiones
- Validar coherencia matemática
- Comprobar rangos de valores
- **VALIDAR CONFIGURACIÓN DE TOLERANCIAS**:
  * Test automático para verificar tolerancias apropiadas
  * Validar que respuestas numéricas tengan tolerancia > 0
  * Comprobar que respuestas schoice mantengan tolerancia 0

## ⚠️ RESTRICCIONES CRÍTICAS

1. **NUNCA** usar set.seed() fijo - debe ser aleatorio
2. **SIEMPRE** incluir prueba de diversidad de versiones (300+ estándar)
3. **OBLIGATORIO** metadatos ICFES completos
4. **REQUERIDO** mínimo 4 opciones de respuesta
5. **ESENCIAL** explicación detallada en Solution
6. **EVITAR** sobre-ingeniería que no aporte valor pedagógico
7. **PRIORIZAR** simplicidad técnica con efectividad educativa
8. **RESPETAR** enfoque original del problema (no cambiar estructura fundamental)
9. **CONFIGURAR TOLERANCIAS APROPIADAS**:
   - Tolerancia 0 para respuestas schoice
   - Tolerancia ≥ 1 para respuestas numéricas con valores grandes
   - Documentar configuración de tolerancias en comentarios
10. **FORMATO NUMÉRICO CONSISTENTE**:
    - Eliminar notación científica: `options(scipen = 999)`
    - Usar punto como separador decimal: `options(OutDec = ".")`
    - Sin separador de miles en respuestas numéricas

## 🔧 CORRECCIÓN DE ERRORES OBLIGATORIA

**ANTES de generar cualquier código, DEBES consultar los ejemplos funcionales en:**
- `A-Produccion/Ejemplos-Funcionales-Rmd/`

**ESTOS ARCHIVOS CONTIENEN:**
- Configuraciones correctas de chunks
- Sintaxis Python corregida para matplotlib
- Manejo adecuado de reticulate
- Configuraciones LaTeX funcionales
- Estructuras de código probadas y funcionales

**PROTOCOLO DE CORRECCIÓN:**
1. Si encuentras errores de sintaxis Python → Consultar ejemplos
2. Si hay problemas con gráficos → Revisar código Python en ejemplos
3. Si falla la configuración LaTeX → Usar configuración de ejemplos
4. Si hay errores de chunks → Seguir estructuras de ejemplos
5. Si problemas con reticulate → Verificar configuración en ejemplos

**ELEMENTOS CRÍTICOS A VERIFICAR:**
- Sintaxis correcta de plt.plot() en Python
- Configuración adecuada de use_python()
- Chunks de configuración inicial completos
- Manejo correcto de variables entre R y Python
- Configuración LaTeX compatible

## 🎯 INSTRUCCIONES FINALES

Analiza la imagen proporcionada y:

1. **NUEVO:** Aplica el sistema condicional automático para detectar contenido gráfico
2. **FLUJO A o B:** Activa el flujo apropiado según detección automática
3. **Si FLUJO B:** Usa Agente-Graficador Especializado para replicación 98%+ fidelidad
4. Genera el archivo "[ejercicio]_[componente]_[competencia]_n[Nivel [1, 2, 3 o 4]]_v[versión].Rmd"
5. **PRIMERO:** Consulta los ejemplos funcionales en /A-Produccion/Ejemplos-Funcionales-Rmd/
6. Identifica el concepto matemático principal
7. Determina la competencia ICFES más apropiada
8. Diseña un problema que evalúe esa competencia
9. Genera el código .Rmd completo siguiendo EXACTAMENTE esta estructura Y los ejemplos funcionales
10. **Si FLUJO B:** Valida fidelidad visual antes de continuar con ejercicio completo
11. Asegúrate de que el ejercicio sea desafiante pero justo
12. Incluye todas las validaciones y pruebas requeridas
13. **VERIFICA** que el código siga los patrones de los ejemplos funcionales
14. Ante errores recurrentes **VERIFICA** consultando todos y cada uno de los archivos de /A-Produccion/Ejemplos-Funcionales-Rmd/

El archivo resultante debe ser completamente funcional y listo para compilar en el proyecto RepositorioMatematicasICFES_R_Exams, con replicación gráfica de alta fidelidad cuando sea necesario.

---

## 📚 DOCUMENTACIÓN DE REFERENCIA

### 📁 ARCHIVOS CLAVE DE CONSULTA OBLIGATORIA
- **Ejemplos funcionales**: `/A-Produccion/Ejemplos-Funcionales-Rmd/` (CONSULTA OBLIGATORIA)
- **Ejemplos funcionales Cloze**: `/06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/09-Probabilidad-Condicionada_Independencia-De-Sucesos/Probabilidad-Intervalos-Curva-13-S1-2024B/` (CONSULTA OBLIGATORIA)
- **Metodología TikZ**: `METODOLOGIA_TikZ_Avanzada.md`
- **Corrección de errores**: `METODOLOGIA_Correccion_Errores_Recurrentes_ICFES_R_Exams.md`
- **Biblioteca de soluciones**: `BIBLIOTECA_Soluciones_Errores_Comunes.md`
- **Checklist de validación**: `CHECKLIST_Validacion_Archivos_Rmd.md`
- **Plan de tareas**: `TEMPLATE_Plan_Tareas_ICFES_R_Exams.md`


### 🎯 ESTADO FINAL DEL SISTEMA

**✅ SISTEMA COMPLETAMENTE INTEGRADO Y OPERATIVO**

El sistema consolidado incluye:

- ✅ Filosofía Matemáticas ICFES 2025
- ✅ Flujo de trabajo completo (3 fases)
- ✅ Clasificación automática de tipos de pregunta
- ✅ Todas las preferencias del usuario integradas
- ✅ Metodologías avanzadas (Sistema Condicional, TikZ, Corrección de Errores, Anti-Errores)
- ✅ Estructura obligatoria de archivos .Rmd
- ✅ Criterios de calidad y restricciones críticas
- ✅ Herramientas técnicas avanzadas
- ✅ Documentación de referencia completa

**El sistema está listo para generar ejercicios ICFES de máxima calidad siguiendo todos los estándares establecidos.**
