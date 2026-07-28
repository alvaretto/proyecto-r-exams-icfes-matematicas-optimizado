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
    *   **Flujo B (Con Gráficas)**: Activación del **Graficador Experto**, que genera de forma SECUENCIAL y OBLIGATORIA las **tres versiones** —TikZ, Python (`matplotlib`) y R (`ggplot2`)—, iterando cada una automáticamente hasta alcanzar **≥98% de fidelidad visual**. Claude **no puede elegir** el lenguaje: el **usuario siempre decide** cuál de las tres versiones usar (ver [`graficador-secuencial.md`](.claude/rules/graficador-secuencial.md), regla #3).
    *   Generación de código `.Rmd` completo, siguiendo una estructura obligatoria y validada.

3.  **🔄 Fase 3: Iteración y Mejora Continua**
    *   Captura de retroalimentación para la corrección de errores.
    *   Implementación de mejoras basadas en patrones identificados para optimizar la generación de código.

---

## ✨ **Características Principales**

-   **🤖 Sistema Condicional Automático**: Activa flujos de trabajo especializados según el contenido de la imagen.
-   **🎨 Graficador Experto (TikZ + Python + R)**: Genera SIEMPRE las tres versiones —TikZ, Python (`matplotlib`) y R (`ggplot2`)—, iterando cada una automáticamente hasta alcanzar ≥98% de fidelidad visual (colores RGB precisos, posicionamiento sistemático). El **usuario siempre decide** cuál de las tres usar (ver [`graficador-secuencial.md`](.claude/rules/graficador-secuencial.md), regla #3).
-   **🧩 Soporte Híbrido**: Genera ejercicios que integran R, Python (vía `reticulate`), y LaTeX/TikZ para visualizaciones complejas.
-   **✅ Calidad ICFES Garantizada**: Cumple con metadatos obligatorios, sistema avanzado de distractores y criterios de calidad rigurosos.
-   **🔧 Protocolo Anti-Errores**: Utiliza una base de "ejemplos funcionales" para prevenir errores de implementación, asegurando que todo el código generado sea robusto y compilable.
-   **✔️ Pruebas de Diversidad**: Generación mínima de **200+ versiones únicas** por ejercicio (regla #3 en [`codigo-rmd.md`](.claude/rules/codigo-rmd.md); la práctica operativa apunta a 250+), verificada con `testthat`. Desde la regla #22 ([`diversidad-sustantiva.md`](.claude/rules/diversidad-sustantiva.md)) el conteo de versiones únicas **no basta**: se exige además diversidad **sustantiva** (que la respuesta correcta varíe, no solo el envoltorio narrativo), verificada con `.claude/scripts/validar_diversidad_sustantiva.R --n 40` (`ERR_DIV_COSMETICA` es bloqueante).

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
├── 🔗 SOURCES/                             # ⚙️ Archivos originales compartidos (symlinks)
│   ├── 📂 documentacion_compartida/       # Docs compartidas entre skills
│   ├── 📂 scripts_validacion/             # Scripts centralizados
│   └── 📂 plantillas/                     # Plantillas base (futuro)
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
- ✅ **Validados**: Han pasado las pruebas de calidad y diversidad — 200+ versiones únicas (práctica operativa 250+; regla #3 en [`codigo-rmd.md`](.claude/rules/codigo-rmd.md)) con diversidad **sustantiva** verificada (regla #22 en [`diversidad-sustantiva.md`](.claude/rules/diversidad-sustantiva.md))
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

- `/detractor auditoria [target]` - **🆕** Revisión adversarial en 7 dominios (código, pedagógico, visual, gramática, matemático, metacognitivo, testing)
- `/detractor [pregunta]` - Modo inline para decisiones puntuales
- `/validar-pedagogico` - Análisis pedagógico basado en evidencias científicas

**Skills de Generación de Contenido:**

- `/skill-retroalimentacion` - **🆕** Genera retroalimentación científica estilo ICFES para sección Solution

**Skills de Soporte:**

- `/corregir-error-imagen` - Corrección automática de errores TikZ
- `/validar-diversidad` - Validar 200+ versiones únicas (regla #3) y diversidad sustantiva (regla #22)
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

> "Activa el Graficador Experto para replicar esta imagen: genera las tres versiones (TikZ, Python y R) e itera cada una hasta ≥98% de fidelidad visual."

El sistema se encargará de analizar la imagen, seleccionar el flujo correcto y generar el archivo `.Rmd` correspondiente en el directorio de trabajo. Recuerda que el Graficador Experto produce **siempre los tres lenguajes** y que **la elección final de cuál usar es tuya**, no del sistema (regla #3, [`graficador-secuencial.md`](.claude/rules/graficador-secuencial.md)).

---

## 🤖 **Configuración de Claude Code**

El proyecto incluye configuración completa en `.claude/` para automatizar el workflow:

```
.claude/
├── CLAUDE.md              # Memory file principal del proyecto (v3.17.1, 2026-06-27)
├── detractor-config.yaml  # 🆕 Configuración del skill detractor
├── rules/                 # Reglas modulares (22 reglas obligatorias — ver tabla completa más abajo)
│   ├── ciclo-validacion.md           # Ciclo de Validación Automática (v4.0 con FASE 2C)
│   ├── detractor-obligatorio.md      # 🆕 Revisión adversarial obligatoria (7 dominios)
│   ├── ejercicios-metacognitivos.md  # 🆕 Progressive Disclosure obligatorio
│   ├── graficos-como-opciones.md     # 🆕 Gráficos individuales SCHOICE (sin títulos)
│   ├── graficador-secuencial.md      # 🆕 Workflow 98% + 3 lenguajes obligatorios
│   ├── codigo-rmd.md                 # Reglas para código R/Markdown
│   ├── flujo-b-obligatorio.md        # Graficador Experto activación automática
│   ├── testing-obligatorio.md        # Testing automático permanente
│   ├── ortografia-espanol.md         # Diccionario de referencia obligatorio
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
│   ├── generar-schoice/          # v3.0 Metacognitivo
│   ├── generar-cloze/            # v3.0 Metacognitivo
│   ├── skill-retroalimentacion/  # 🆕 Generación científica Solution
│   ├── promover-ejercicio/
│   ├── corregir-error-imagen/
│   ├── validar-diversidad/
│   ├── validar-pedagogico/
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
- ✅ **Graficador Experto v2.0** con estado persistente y métricas cuantitativas (umbral 98%)
- ✅ Sistema de puntuación 0-100 puntos para comparación visual
- ✅ Transferencia de conocimiento entre lenguajes (TikZ → Python → R)
- ✅ Nomenclatura obligatoria con carpetas estructuradas
- ✅ **🆕 Detractor Obligatorio**: Revisión adversarial en 7 dominios (FASE 2C del ciclo)
- ✅ **🆕 Ejercicios Metacognitivos**: Progressive Disclosure y pool de errores conceptuales
- ✅ **🆕 Gráficos Como Opciones**: PNG separados, sin títulos, doble aleatorización
- ✅ **🆕 Skill-Retroalimentacion**: Generación científica de secciones Solution

---

## 📋 **Índice de Reglas Críticas (22 reglas obligatorias)**

Lista completa sincronizada con `.claude/CLAUDE.md` (sección "Reglas Críticas"). Todas son
obligatorias y sin excepciones salvo lo indicado en cada archivo.

| # | Regla | Archivo |
|---|-------|---------|
| 1 | Ejercicios metacognitivos con Progressive Disclosure | [`ejercicios-metacognitivos.md`](.claude/rules/ejercicios-metacognitivos.md) |
| 2 | Flujo B obligatorio cuando hay gráficos | [`flujo-b-obligatorio.md`](.claude/rules/flujo-b-obligatorio.md) |
| 3 | Proceso secuencial TikZ→Python→R (98% fidelidad, usuario decide) | [`graficador-secuencial.md`](.claude/rules/graficador-secuencial.md) |
| 4 | Gráficos como opciones individuales (PNGs separados) | [`graficos-como-opciones.md`](.claude/rules/graficos-como-opciones.md) |
| 5 | 5 Coherencias a verificar siempre (Semántica, Visual-Texto, Matemática, Código, General) | ver [`ciclo-validacion.md`](.claude/rules/ciclo-validacion.md) |
| 6 | Validación visual iterativa obligatoria | [`ciclo-validacion.md`](.claude/rules/ciclo-validacion.md) |
| 7 | Ortografía española con tildes | [`ortografia-espanol.md`](.claude/rules/ortografia-espanol.md) |
| 8 | Testing automático permanente | [`testing-obligatorio.md`](.claude/rules/testing-obligatorio.md) |
| 9 | Detractor obligatorio en fases de revisión | [`detractor-obligatorio.md`](.claude/rules/detractor-obligatorio.md) |
| 10 | Validación de opciones repetidas en ejercicios `_neg_` | [`validacion-neg-opciones-repetidas.md`](.claude/rules/validacion-neg-opciones-repetidas.md) |
| 11 | Contextos narrativos creativos (no mecánicos) | [`contextos-narrativos-creativos.md`](.claude/rules/contextos-narrativos-creativos.md) |
| 12 | Validación semántica automática (Nivel 4: descripción ↔ datos) | [`ejercicios-metacognitivos.md`](.claude/rules/ejercicios-metacognitivos.md) (sección Validación Semántica) |
| 13 | Validación de correctitud de respuesta (Nivel 5: multi-semilla + cross-check) | [`validacion-correctitud-respuesta.md`](.claude/rules/validacion-correctitud-respuesta.md) |
| 14 | Routing de modelos obligatorio (Opus/Sonnet/Haiku por complejidad) | [`modelo-routing-obligatorio.md`](.claude/rules/modelo-routing-obligatorio.md) |
| 15 | Stress Test Visual (FASE 2H: renderizado masivo + análisis de anomalías) | [`stress-test-visual/SKILL.md`](.claude/skills/stress-test-visual/SKILL.md) |
| 16 | Workflow State Enforcement (gate mecánico PreToolUse + estado persistente) | [`workflow-state-enforcement.md`](.claude/rules/workflow-state-enforcement.md) |
| 17 | Infraestructura `.claude/` protegida (invariantes I-1 a I-9) | [`infraestructura-protegida.md`](.claude/rules/infraestructura-protegida.md) |
| 18 | Markdown-imágenes-PDF (anti `\pandocbounded`) | [`markdown-imagenes-pdf.md`](.claude/rules/markdown-imagenes-pdf.md) |
| 19 | Solution letter-independence (nunca letra/posición en Solution) | [`solution-letter-independence.md`](.claude/rules/solution-letter-independence.md) |
| 20 | Markdown-tablas-pandoc (guard del contador `none`) | [`markdown-tablas-pandoc.md`](.claude/rules/markdown-tablas-pandoc.md) |
| 21 | Familias de Soluciones Reutilizables | [`familias-soluciones-rmd.md`](.claude/rules/familias-soluciones-rmd.md) |
| 22 | Diversidad Sustantiva (la respuesta correcta debe variar entre versiones, no solo el envoltorio narrativo) | [`diversidad-sustantiva.md`](.claude/rules/diversidad-sustantiva.md) |

> **Invariante I-9** (regla #17, añadida 2026-07-28): los `tools:` de un agente en
> `.claude/agents/*.md` deben declararse en **PascalCase** (`Read`, `Write`, `Bash`...); en
> minúscula el agente se instancia **sin ninguna herramienta**.

> **📌 Caso de estudio — SCHOICE con opciones gráficas dinámicas**: el subproyecto
> [`A-Produccion/01-En-PreDesarrollo/desplazamiento-avion-aeropuerto`](A-Produccion/01-En-PreDesarrollo/desplazamiento-avion-aeropuerto/HANDOFF.md)
> ejemplifica el patrón completo de gráficos generados dinámicamente por versión (diagramas
> de vuelo con ángulo/distancia aleatorizados). De su auditoría (2026-06-27/28) surgieron la
> **regla #22** (Diversidad Sustantiva) y los **Errores 23 y 24** del catálogo de patrones.
> Ver [`HANDOFF.md`](A-Produccion/01-En-PreDesarrollo/desplazamiento-avion-aeropuerto/HANDOFF.md)
> y [`docs/BLUEPRINT.md`](A-Produccion/01-En-PreDesarrollo/desplazamiento-avion-aeropuerto/docs/BLUEPRINT.md).

---

## 🔗 **Arquitectura de Symlinks (SOURCES/)**

### Principio: Archivos Compartidos Centralizados

El proyecto utiliza **symlinks bidireccionales** para compartir archivos entre módulos sin duplicación.

```
SOURCES/ (Originales)
  ├── documentacion_compartida/
  │   └── anatomia-metacognitiva.md  ← ORIGINAL
  │          ↑                       ↑
  │          │ symlink              │ symlink
  │          │                       │
  .claude/skills/                    .claude/skills/
    ├── generar-schoice/               └── generar-cloze/
    │   └── references/                    └── references/
    │       └── anatomia-metacognitiva.md     └── anatomia-metacognitiva.md
```

### ✅ Ventajas

1. **DRY (Don't Repeat Yourself)**: Un solo archivo, múltiples referencias
2. **Sincronización automática**: Cambios se propagan inmediatamente a todos los symlinks
3. **Bidireccionalidad**: Edita desde cualquier ubicación (SOURCES/ o symlink)
4. **Git-friendly**: Git trackea symlinks correctamente, GitHub los muestra

### 📋 Archivos Actuales con Symlinks

| Original (SOURCES/) | Symlinks | Propósito |
|---------------------|----------|-----------|
| `documentacion_compartida/anatomia-metacognitiva.md` | `generar-schoice/references/`<br>`generar-cloze/references/` | Estructura metacognitiva de 8 secciones |
| `scripts_validacion/validar_coherencia_matematica.R` | `.claude/scripts/`<br>`.claude/hooks/scripts/` | Validación matemática compartida |
| `scripts_validacion/corregir_ortografia_espanol.R` | `.claude/scripts/` | Corrección ortográfica |
| `scripts_validacion/arsenal_validacion_completa.R` | `.claude/scripts/` | Suite de validación |

### 🛠️ Comandos Útiles

```bash
# Verificar integridad de symlinks
.claude/scripts/verificar_symlinks.sh

# Crear nuevo symlink
cd .claude/skills/nueva-skill/references/
ln -s ../../../../SOURCES/documentacion_compartida/archivo.md archivo.md

# Ver qué symlinks apuntan a un archivo
find . -type l -ls | grep "nombre_archivo"
```

### 📖 Documentación Completa

Ver `SOURCES/README.md` para:
- Mapa completo de dependencias
- Guía de edición bidireccional
- Qué pasa si eliminas un symlink vs. el original
- Workflow de expansión futura

---

## 🤝 **Contribución y Calidad**

Cualquier contribución debe adherirse estrictamente a las metodologías y protocolos definidos en el proyecto.

### **Regla de Oro Anti-Errores**
**"Si no está en los ejemplos funcionales, no improvises."** Antes de escribir cualquier código, es **obligatorio** consultar los ejercicios en `/A-Produccion/Ejemplos-Funcionales-Rmd/`.

### **Criterios de Calidad**
-   **Fidelidad Visual (≥98%, en las tres versiones TikZ/Python/R)**: Precisión geométrica, cromática, de posicionamiento y completitud; el usuario decide la versión final (regla #3).
-   **Funcionalidad R-exams (100%)**: Compatibilidad con `exams2*`, 200+ versiones únicas (práctica operativa 250+; regla #3) con diversidad **sustantiva** verificada (regla #22, `.claude/scripts/validar_diversidad_sustantiva.R --n 40`), y aleatorización completa.
-   **Alineación ICFES**: Metadatos completos, distractores pedagógicos y nivel de dificultad apropiado.

---

## 📊 **Información del Proyecto**

-   **Autor**: Álvaro Ángel Molina
-   **Institución**: IE Pedacito de Cielo
-   **Propósito**: Generación de ejercicios matemáticos de alta calidad para la preparación de la prueba ICFES Saber 11°.
-   **Licencia**: Proyecto Educativo
-   **Última Actualización**: v3.17.1 (2026-06-27) — ver [`.claude/CLAUDE.md`](.claude/CLAUDE.md)

## 🆕 **Novedades Recientes**

### Resumen v3.3.0 → v3.17.1 (2026-02 a 2026-06)

El sistema evolucionó considerablemente desde v3.2.2 (histórico, ver sección siguiente).
Resumen de los hitos principales registrados en el changelog de [`.claude/CLAUDE.md`](.claude/CLAUDE.md):

| Versión | Fecha | Novedad principal |
|---------|-------|--------------------|
| v3.3.0 | 2026-02-14 | Validación de correctitud de respuesta Nivel 5 (cross-check `exsolution`, unicidad de opciones, rangos matemáticos) + validación multi-semilla |
| v3.4.0 | 2026-02-14 | Routing obligatorio de modelos (Opus/Sonnet/Haiku) por complejidad de la tarea |
| v3.5.0 | 2026-02-14 | Capa D: determinismo obligatorio de `calcula()` (prohibido `sample`/`runif`/`rnorm` dentro) |
| v3.6.0 | 2026-02-14 | Stress Test Visual multi-semilla (FASE 2H): renderizado masivo + detección de anomalías |
| v3.7.0 | 2026-03-23 | Skills `/revisar-schoice` y `/revisar-cloze` (retoman el workflow interrumpido en pasos 4-11) |
| v3.8.0 | 2026-04-10 | Resuelto drift hooks/tests/CI/docs; runner unificado de 12 suites; CI simplificado a un solo job |
| v3.10.0 | 2026-05-03 | Regla #18 anti-`\pandocbounded` (imágenes Markdown requieren `{width=...}`) |
| v3.11.0 | 2026-05-12 | Regla #19 *solution-letter-independence* (la Solution nunca referencia letra/posición de opción) |
| v3.12.0 / v3.15.0 | 2026-05-14 / 2026-06-03 | Orquestador CLOZE end-to-end (11 pasos, gemelo del orquestador SCHOICE) |
| v3.13.0 | 2026-05-14 | Formato Equilibrado en gráficos-opción (al menos 2 opciones comparten el formato de la correcta) |
| v3.14.0 | 2026-06-03 | Regla #20: guard del contador `none` para tablas Markdown en PDF/NOPS (pandoc ≥3.7) |
| v3.16.0 | 2026-06-15 | CLOZE: las gráficas-opción van en el enunciado, nunca dentro de un gap (Moodle no las renderiza) |
| v3.17.0 | 2026-06-16 | Validador con soporte real de ejercicios `_neg_` Variante B (texto sinónimo) |
| v3.17.1 | 2026-06-27 | Fix de `WAIT_USER` en orquestadores ejecutados como subagente |

Adicionalmente, las reglas #21 (Familias de Soluciones Reutilizables) y #22 (Diversidad
Sustantiva) se incorporaron al índice de reglas críticas sin entrada de changelog versionada
propia; el origen de la regla #22 está documentado en el incidente del subproyecto
`desplazamiento-avion-aeropuerto` (2026-06-27) — ver la sección "Índice de Reglas Críticas"
más abajo en este mismo documento.

### Sistema v3.2.2 - Gráficos y Graficador (Febrero 2026)

#### 🎨 Gráficos Como Opciones Individuales (Nueva Regla)

- ✅ **Regla obligatoria**: `.claude/rules/graficos-como-opciones.md`
- ✅ **PNG separados**: Cada opción gráfica es archivo individual (diagrama_a.png, etc.)
- ✅ **Sin títulos con letras**: Gráficos NO deben tener título "A", "B", etc. (R-exams asigna automáticamente)
- ✅ **Mezcla interna + exshuffle:TRUE**: Doble capa de aleatorización
- ✅ **Tracking letra_correcta**: Para mostrar opción correcta en Solution
- ✅ **PROHIBIDO grid.arrange()**: Nunca mostrar todas las opciones juntas

#### 🔧 Graficador Secuencial v2.0 (Actualizado)

- ✅ **Umbral aumentado**: 95% → **98%** de fidelidad visual
- ✅ **Iteraciones automáticas**: Sin aprobación intermedia (solo final)
- ✅ **SIEMPRE 3 lenguajes**: TikZ + Python + R (los tres, obligatorio)
- ✅ **Usuario SIEMPRE decide**: Claude NO puede elegir versión final
- ✅ **Regla actualizada**: `.claude/rules/graficador-secuencial.md` v2.0

### Detractor Obligatorio v1.1 (Febrero 2026)

- ✅ **7 dominios de revisión**: código, pedagógico, visual, gramática, **matemático**, **metacognitivo**, **testing**
- ✅ **FASE 2C añadida**: Nueva fase obligatoria en el ciclo de validación
- ✅ **Fuentes de verdad**: Objeciones respaldadas por documentación oficial (Nivel 1-2)
- ✅ **Bloqueos automáticos**: Objeciones críticas/altas bloquean promoción
- ✅ **Configuración personalizable**: `.claude/detractor-config.yaml`

### Ejercicios Metacognitivos v1.0 (Febrero 2026)

- ✅ **Progressive Disclosure obligatorio**: Todo ejercicio debe ser metacognitivo
- ✅ **Pool de errores conceptuales**: Con códigos y funciones `calcula()`
- ✅ **Metadatos cognitivos**: DOK, Bloom, SOLO obligatorios
- ✅ **Skill-retroalimentacion**: Generación científica de sección Solution
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

2. GENERACIÓN MULTI-LENGUAJE (umbral único, regla #3)
   /generar-tikz   → Validado (≥98%)
   /generar-python → Validado (≥98%)
   /generar-r      → Validado (≥98%)
   ↓
   Genera: output_tikz.tex + output_python.py + output_r.R
   (Los tres lenguajes son obligatorios y comparten el mismo umbral de fidelidad;
    la elección final de cuál usar es del usuario, no del sistema — ver
    graficador-secuencial.md, regla #3)

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
