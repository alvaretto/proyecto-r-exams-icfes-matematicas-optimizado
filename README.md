---
output:
  word_document: default
  html_document: default
  pdf_document:
    latex_engine: xelatex
---

# 📚 RepositorioMatematicasICFES_R_Exams (Filosofía 2026)

[![Estado](https://img.shields.io/badge/Estado-Activo-brightgreen)](https://github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado)
[![R-exams](https://img.shields.io/badge/R--exams-Compatible-orange)](https://www.r-exams.org/)
[![Calidad](https://img.shields.io/badge/Calidad-ICFES_2026-success)](https://github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado)
[![Metodología](https://img.shields.io/badge/Metodología-Sistema_Condicional-blue)](https://github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado)
[![Detractor](https://img.shields.io/badge/Detractor-Obligatorio-red)](https://github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado)

**Sistema integrado para la generación de ejercicios matemáticos tipo ICFES en formato R-exams a partir de imágenes, siguiendo la filosofía "Matemáticas ICFES 2026" con ejercicios metacognitivos y revisión adversarial obligatoria.**

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

El proyecto incluye skills configurados en `.claude/skills/` para automatizar cada fase del workflow:

**Workflow Principal:**

- `/analizar-icfes` - Análisis ICFES de imagen según 6 dimensiones (Fase 1)
- `/generar-schoice` - Generar ejercicio de selección única con nomenclatura obligatoria y carpeta estructurada
- `/generar-cloze` - Generar ejercicio tipo CLOZE con configuración de tolerancias apropiadas
- `/promover-ejercicio` - Promoción a carpeta de producción (Fase 7)

**Skills de Revisión (OBLIGATORIOS):**

- `/detractor auditoria [target]` - **🆕** Revisión adversarial en 4 dominios (código, pedagógico, visual, gramática)
- `/detractor [pregunta]` - Modo inline para decisiones puntuales
- `/validar-pedagogico` - Análisis pedagógico basado en evidencias científicas

**Skills de Soporte:**

- `/corregir-error-imagen` - Corrección automática de errores TikZ
- `/validar-diversidad` - Validar 300+ versiones únicas
- `/validar-icfes` - Validar metadatos y estructura R-exams

**Skills del Graficador Experto v2.0:**

- `/analizar-imagen` - Análisis visual detallado con estado persistente
- `/generar-tikz` - Generación TikZ/LaTeX con métricas cuantitativas
- `/generar-python` - Generación Python/Matplotlib con transferencia de conocimiento
- `/generar-r` - Generación R/ggplot2 con lecciones aprendidas
- `/comparar` - Comparación visual con puntuación 0-100 puntos
- `/iterar` - Refinamiento iterativo con contador de iteraciones
- `/exportar` - Exportación completa con estadísticas y carpeta nomenclada
- `/estado` - Visualización de progreso del workflow en tiempo real
- `/auto-iterar` - Iteración automática hasta umbral de similitud

#### **Ejemplo de Uso con Skills**

```bash
# 1. Analizar imagen de ejercicio ICFES
/analizar-icfes imagen_ejercicio.png

# 2. Si tiene gráficos complejos, usar Graficador Experto
/analizar-imagen grafico_matematico.png
/generar-r  # Genera código R (recomendado para R-exams)
/comparar   # Obtiene puntuación 0-100
/auto-iterar r 95 10  # Itera hasta 95+ puntos (máx 10 iteraciones)

# 3. Generar ejercicio SCHOICE (pregunta al usuario qué versión gráfica usar)
/generar-schoice

# 4. Validar diversidad de versiones
/validar-diversidad archivo_generado.Rmd

# 5. Promover a producción después de validar
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
├── CLAUDE.md              # Memory file principal del proyecto (v3.2)
├── detractor-config.yaml  # 🆕 Configuración del skill detractor
├── rules/                 # Reglas modulares (8 reglas obligatorias)
│   ├── ciclo-validacion.md           # Ciclo de Validación Automática (v4.0 con FASE 2C)
│   ├── detractor-obligatorio.md      # 🆕 Revisión adversarial obligatoria
│   ├── ejercicios-metacognitivos.md  # 🆕 Progressive Disclosure obligatorio
│   ├── codigo-rmd.md                 # Reglas para código R/Markdown
│   └── documentacion-verificada.md   # Principio de Documentación Verificada
├── settings.json          # Hooks y configuración global
├── settings.local.json    # Permisos para skills
├── schemas/               # Esquemas JSON para estado persistente
│   ├── workflow_state.schema.json
│   ├── analisis_inicial.schema.json
│   ├── metricas_similitud.schema.json
│   └── lecciones_aprendidas.schema.json
├── skills/                # Skills del workflow automatizado
│   ├── analizar-icfes/
│   ├── generar-schoice/
│   ├── generar-cloze/
│   ├── promover-ejercicio/
│   ├── corregir-error-imagen/
│   ├── validar-diversidad/
│   ├── validar-icfes/
│   └── [Graficador Experto v2.0]
│       ├── analizar-imagen-matematica/
│       ├── generar-tikz/
│       ├── generar-python/
│       ├── generar-r/
│       ├── comparar-visual/
│       ├── refinar-codigo/
│       ├── gestionar-estado/
│       └── transferir-conocimiento/
├── commands/              # Comandos slash para acceso rápido
│   ├── analizar-imagen.md
│   ├── generar-tikz.md
│   ├── generar-python.md
│   ├── generar-r.md
│   ├── comparar.md
│   ├── iterar.md
│   ├── exportar.md
│   ├── estado.md
│   ├── auto-iterar.md
│   ├── generar-schoice.md
│   └── generar-cloze.md
└── docs/                  # Documentación del workflow
    ├── 01-EXPLICACION_COMPLETA_GRAFICADOR_EXPERTO.md
    ├── NOMENCLATURA_ARCHIVOS_RMD.md
    ├── INDICE_DOCUMENTACION.md
    ├── WORKFLOW_PASO_A_PASO.md
    └── [otros archivos]
```

**Características:**

- ✅ Hooks configurados para recordatorios automáticos
- ✅ Permisos preconfigurados para ejecución sin confirmación
- ✅ Documentación completa del workflow paso a paso
- ✅ Validación automática de estructura ICFES
- ✅ **Graficador Experto v2.0** con estado persistente y métricas cuantitativas
- ✅ Sistema de puntuación 0-100 puntos para comparación visual
- ✅ Transferencia de conocimiento entre lenguajes (TikZ → Python → R)
- ✅ Nomenclatura obligatoria con carpetas estructuradas
- ✅ **🆕 Detractor Obligatorio**: Revisión adversarial en FASE 2C del ciclo de validación
- ✅ **🆕 Ejercicios Metacognitivos**: Progressive Disclosure y pool de errores conceptuales

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
-   **Última Actualización**: Febrero 2026

## 🆕 **Novedades Recientes**

### Detractor Obligatorio v1.0 (Febrero 2026)

- ✅ **Revisión adversarial automática**: Skill-detractor se ejecuta en todas las fases de revisión
- ✅ **FASE 2C añadida**: Nueva fase obligatoria en el ciclo de validación
- ✅ **4 dominios de revisión**: Código R-exams, pedagógico, visual, gramática
- ✅ **Fuentes de verdad**: Objeciones respaldadas por documentación oficial (Nivel 1-2)
- ✅ **Bloqueos automáticos**: Objeciones críticas/altas bloquean promoción
- ✅ **Configuración personalizable**: `.claude/detractor-config.yaml`

### Ejercicios Metacognitivos v1.0 (Febrero 2026)

- ✅ **Progressive Disclosure obligatorio**: Todo ejercicio debe ser metacognitivo
- ✅ **Pool de errores conceptuales**: Con códigos y funciones `calcula()`
- ✅ **Metadatos cognitivos**: DOK, Bloom, SOLO obligatorios
- ✅ **Antipatrones documentados**: Ejercicios puramente procedimentales PROHIBIDOS

### Graficador Experto v2.0 (Diciembre 2025)

- ✅ **Estado persistente**: Tracking completo del progreso con recuperación ante interrupciones
- ✅ **Métricas cuantitativas**: Sistema de puntuación 0-100 puntos en 6 categorías
- ✅ **Análisis estructurado**: Formato JSON reutilizable para las 3 generaciones
- ✅ **Transferencia de conocimiento**: Lecciones aprendidas aplicadas entre lenguajes
- ✅ **Iteración automática**: Comando `/auto-iterar` hasta umbral de similitud
- ✅ **Visualización de progreso**: Comando `/estado` para ver avance en tiempo real

### Nomenclatura Obligatoria y Carpetas Estructuradas

- ✅ **Formato estándar**: `[ejercicio]_[componente]_[competencia]_n[nivel]_v[version].Rmd`
- ✅ **Carpetas organizadas**: Cada ejercicio en su propia carpeta con todos sus archivos
- ✅ **Exportación mejorada**: `/exportar` crea carpeta con nomenclatura oficial
- ✅ **Selección de versión gráfica**: Pregunta obligatoria antes de generar .Rmd

### Configuración de Tolerancias para Ejercicios CLOZE

- ✅ **Tolerancias apropiadas**: 0 para schoice, ≥1 para valores numéricos grandes
- ✅ **Documentación mejorada**: Guías específicas en comandos y skills
- ✅ **Validación automática**: Verificación de configuración correcta

Este proyecto representa un sistema integral y robusto que encapsula las mejores prácticas para la creación de contenido educativo parametrizado y de alta calidad.

---

## 🧪 **Ecosistema de Testing Agresivo**

### 🎯 Objetivo: 100% de Cobertura

Este proyecto implementa un ecosistema de testing agresivo para garantizar calidad máxima en la generación automatizada de ejercicios ICFES.

| Componente | Cobertura | Estado |
|------------|-----------|--------|
| **Validación Matemática** | 100% | ✅ Completo |
| **Ortografía Española** | 100% | ✅ Completo |
| **Renderizado 4 Formatos** | 100% | ✅ Completo |
| **Aleatorización y Diversidad** | 100% | ✅ Completo |
| **Flujo B (Graficador)** | 100% | ✅ Completo |
| **Tests de Regresión** | 100% | ✅ Completo |
| **COBERTURA TOTAL** | **100%** | ✅ **OBJETIVO ALCANZADO** |

### 🧪 Suites de Testing Implementadas

#### 1. **Validación Matemática** (`test_validacion_matematica.R`)
- ✅ Detección de errores en chunks R (NaN, Inf, errores de ejecución)
- ✅ Validación de archivos SCHOICE válidos
- ✅ Detección de `exshuffle = FALSE` (prohibido)
- ✅ Validación de inconsistencias en CLOZE
- ✅ Verificación de metadatos ICFES completos

#### 2. **Ortografía Española** (`test_ortografia_espanol.R`)
- ✅ Detección de tildes faltantes
- ✅ Exclusión de metadatos R-exams (ASCII obligatorio)
- ✅ Exclusión de nombres de variables R
- ✅ Correcciones automáticas en texto
- ✅ Preservación de código inline

#### 3. **Renderizado 4 Formatos** (`test_renderizado_4_formatos.R`)
- ✅ SCHOICE → HTML/PDF/DOCX/NOPS sin errores
- ✅ CLOZE → 4 formatos sin errores
- ✅ Validación cruzada con misma semilla
- ✅ Coherencia de contenido entre formatos

#### 4. **Aleatorización y Diversidad** (`test_aleatorization_diversity.R`)
- ✅ `exshuffle = TRUE` genera orden aleatorio
- ✅ Generación de 250+ versiones únicas
- ✅ Cobertura de rangos numéricos esperados
- ✅ Distractores distintos y plausibles

#### 5. **Flujo B Graficador** (`test_flujo_b_graficador.R`)
- ✅ Estructura de `workflow_state.json` correcta
- ✅ Detección obligatoria de gráficos
- ✅ Aprobación secuencial (TikZ → Python → R)
- ✅ Similitud >= 95% requerida
- ✅ Verificación de 5 coherencias

#### 6. **Tests de Regresión** (`test_regression_suite.R`)
- ✅ Ejemplos funcionales continúan renderizando
- ✅ Scripts mantienen compatibilidad
- ✅ Hooks mantienen funcionalidad
- ✅ Plantillas mantienen formato
- ✅ Metadatos ICFES siguen estándar
- ✅ Ciclo completo funciona end-to-end

### 🚀 Ejecución de Tests

#### Ejecutar Suite Completa

```bash
# Ejecutar todos los tests
Rscript tests/run_all_tests.R
```

**Salida esperada:**
```
========================================
  SUITE DE TESTING COMPLETA
  Repositorio Matemáticas ICFES R-Exams
========================================

Ejecutando: Validación Matemática
--------------------------------------------------
✓ Validación Matemática completado en 2.34 segundos

...

========================================
  REPORTE FINAL
========================================

Suites ejecutadas: 6
✓ Exitosas: 6
✗ Fallidas: 0
Tiempo total: 12.45 segundos

Cobertura de testing: 100.0%
🎉 ¡OBJETIVO DE 100% ALCANZADO!

✅ TODOS LOS TESTS PASARON
```

#### Ejecutar Suite Individual

```bash
# Solo validación matemática
Rscript -e "library(testthat); test_file('tests/testthat/test_validacion_matematica.R')"

# Solo ortografía
Rscript -e "library(testthat); test_file('tests/testthat/test_ortografia_espanol.R')"

# Solo renderizado
Rscript -e "library(testthat); test_file('tests/testthat/test_renderizado_4_formatos.R')"
```

### 🔄 Integración Continua (CI/CD)

El repositorio incluye configuración de GitHub Actions (`.github/workflows/ci-testing.yml`) que ejecuta automáticamente:

**Triggers:**
- ✅ Cada push a `main` o `develop`
- ✅ Cada pull request
- ✅ Diariamente a las 02:00 UTC

**Jobs Paralelos:**
1. Tests de Validación Matemática
2. Tests de Ortografía
3. Tests de Renderizado 4 Formatos
4. Tests de Aleatorización y Diversidad
5. Tests de Flujo B (Graficador)
6. Tests de Regresión
7. Reporte de Cobertura

**Política:** Tolerancia cero a regresiones. Si algún test falla, el pipeline completo falla.

### 🛡️ Política de Testing

#### Reglas Obligatorias

- ❌ **PROHIBIDO** hacer push a `main` si algún test falla
- ❌ **PROHIBIDO** usar `git commit --no-verify` para evadir hooks
- ❌ **PROHIBIDO** comentar tests que fallan (arreglar el código, no el test)
- ❌ **PROHIBIDO** reducir cobertura por debajo del 100%

#### Proceso de Contribución

1. **Antes de cada commit:** Hook pre-commit valida ortografía automáticamente
2. **Después de cada `exams2*()`:** Hook post-exams2 valida matemática + genera preview PNG
3. **Antes de cada push:** Ejecutar `Rscript tests/run_all_tests.R` localmente
4. **Antes de cada merge:** CI/CD ejecuta suite completa automáticamente

### 📊 Métricas de Calidad

| Componente | Tests | Tiempo Promedio | Crítico |
|------------|-------|-----------------|---------|
| Validación Matemática | 5 tests | ~2.5s | ✅ Sí |
| Ortografía Española | 5 tests | ~1.5s | ✅ Sí |
| Renderizado 4 Formatos | 6 tests | ~8.0s | ✅ Sí |
| Aleatorización | 4 tests | ~5.0s | ✅ Sí |
| Flujo B | 6 tests | ~1.0s | ✅ Sí |
| Regresión | 7 tests | ~10.0s | ✅ Sí |
| **TOTAL** | **33+ tests** | **~28.0s** | - |

### 📚 Documentación Completa

Ver documentación detallada del ecosistema en:
- **`.claude/docs/ECOSISTEMA_TESTING.md`** - Guía completa de testing
- **`tests/testthat/`** - Suites de tests individuales
- **`tests/run_all_tests.R`** - Script ejecutor principal
- **`.github/workflows/ci-testing.yml`** - Configuración CI/CD

---

## 🔗 **Integración Graficador Experto v2.0 con R-exams**

El sistema incluye un flujo completo e integrado para transformar gráficos matemáticos en ejercicios R-exams:

### Flujo Completo: Imagen → Gráfico → Ejercicio

```
1. ANÁLISIS VISUAL
   /analizar-imagen grafico.png
   ↓
   Genera: analisis_inicial.json + workflow_state.json

2. GENERACIÓN MULTI-LENGUAJE
   /generar-tikz → Validado (98%+)
   /generar-python → Validado (95%+)
   /generar-r → Validado (96%+)
   ↓
   Genera: output_tikz.tex + output_python.py + output_r.R

3. COMPARACIÓN CON MÉTRICAS OBJETIVAS
   /comparar → Puntuación 0-100 en 6 categorías
   /auto-iterar r 95 10 → Iteración automática
   ↓
   Resultado: 95+ puntos de similitud garantizados

4. EXPORTACIÓN CON NOMENCLATURA
   /exportar
   ↓
   Pregunta: ¿Qué versión usar? (TikZ/Python/R)
   ↓
   Crea: [ejercicio]_[componente]_[competencia]_n[nivel]_v[version]/
         ├── Códigos de las 3 versiones
         ├── Imágenes generadas
         ├── Análisis y estado persistente
         └── Reporte consolidado

5. GENERACIÓN DE EJERCICIO R-EXAMS
   /generar-schoice (o /generar-cloze)
   ↓
   Usa la versión gráfica seleccionada
   ↓
   Genera: [nombre_completo].Rmd dentro de la carpeta
```

### Ventajas de la Integración

| Ventaja | Descripción |
|---------|-------------|
| **Trazabilidad** | Cada ejercicio tiene historial completo de cómo se generó su gráfico |
| **Flexibilidad** | 3 versiones disponibles según necesidad (vectorial/Python/R) |
| **Calidad** | Métricas objetivas 0-100 puntos antes de usar en .Rmd |
| **Organización** | Carpetas nomencladas con todos los archivos relacionados |
| **Reproducibilidad** | Estado persistente permite regenerar o modificar |

### Recomendaciones por Tipo de Gráfico

| Tipo de Gráfico | Versión Recomendada | Razón |
|-----------------|---------------------|-------|
| Geometría precisa | TikZ | Vectorial, máxima precisión |
| Estadística básica | R/ggplot2 | Nativo R-exams, fácil mantener |
| Visualización compleja | Python/matplotlib | Flexibilidad, numpy integrado |
| Funciones matemáticas | TikZ o R | Calidad y precisión |
| Gráficos de barras/líneas | R/ggplot2 | Sintaxis simple, temas profesionales |

---
