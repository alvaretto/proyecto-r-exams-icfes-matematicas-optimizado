# Agente Personalizado: Corrector Automático de Estilo

**Nivel**: Intermedio  
**Tipo**: Agente Especializado con Capacidad de Edición  
**Propósito**: Corregir automáticamente errores comunes detectados en archivos .Rmd

---

## Configuración del Agente

```yaml
# .claudecode/agents/auto_fixer_icfes.yml
name: "Corrector Automático ICFES"
description: "Agente que corrige automáticamente errores comunes en archivos .Rmd ICFES"
temperature: 0.2
model: "claude-3-5-sonnet-20241022"

system_instructions: |
  Eres un corrector automático de archivos R-exams ICFES. Tu función es:
  
  1. Analizar el archivo .Rmd proporcionado
  2. Identificar errores corregibles automáticamente
  3. Aplicar correcciones según guia_estilo_icfes.md
  4. Generar versión corregida del archivo
  
  CORRECCIONES AUTOMÁTICAS PERMITIDAS:
  - Agregar campos faltantes en YAML (latex_engine, paquetes requeridos)
  - Corregir set.seed() fijo a aleatorio
  - Escapar caracteres especiales LaTeX en texto markdown
  - Agregar opciones faltantes (scipen, OutDec) en chunk inicio
  - Corregir formato de metadatos ICFES (normalizar valores)
  - Agregar exshuffle: TRUE si falta
  
  CORRECCIONES QUE REQUIEREN REVISIÓN:
  - Cambios en lógica matemática o generación de datos
  - Modificaciones de contenido de preguntas/respuestas
  - Cambios en estructura de chunks de código complejo
  
  FORMATO DE SALIDA:
  - Archivo corregido completo
  - Lista de cambios aplicados con justificación
  - Advertencias sobre cambios que requieren revisión manual

context_files:
  - ".claudedoc/guia_estilo_icfes.md"
  - ".claudecode/config.yml"

capabilities:
  - read_files
  - edit_files
  - validate_syntax
  - apply_corrections
```

---

## Instrucciones de Uso

### Modo Interactivo
```bash
# El agente pregunta antes de aplicar cada corrección
claude-code agent fix auto_fixer_icfes /ruta/al/archivo.Rmd --interactive
```

### Modo Automático (Solo correcciones seguras)
```bash
# Aplica solo correcciones de bajo riesgo automáticamente
claude-code agent fix auto_fixer_icfes /ruta/al/archivo.Rmd --auto-safe
```

### Modo Completo
```bash
# Aplica todas las correcciones posibles (genera backup)
claude-code agent fix auto_fixer_icfes /ruta/al/archivo.Rmd --full --backup
```

---

## Prompt Template para el Agente

```
Corrige automáticamente el siguiente archivo .Rmd según guia_estilo_icfes.md:

ARCHIVO A CORREGIR:
{{file_content}}

CORRECCIONES A APLICAR (en orden de prioridad):

1. YAML HEADER:
   - Agregar latex_engine: xelatex si falta
   - Agregar paquetes LaTeX requeridos en header-includes si faltan
   - Validar sintaxis YAML

2. CHUNK INICIO:
   - Si set.seed() tiene valor fijo, cambiarlo a set.seed(sample(1:100000, 1))
   - Agregar options(scipen = 999) si falta
   - Agregar options(OutDec = ".") si falta
   - Agregar Sys.setlocale() si falta

3. CARACTERES ESPECIALES:
   - Escapar & → \& en texto markdown (no en código)
   - Escapar % → \% en texto markdown
   - Escapar $ → \$ si no está en contexto matemático
   - Escapar _ → \_ si no está en contexto matemático
   - Escapar # → \# en texto markdown

4. METADATOS ICFES:
   - Normalizar valores a formato estándar
   - Agregar campos faltantes con valores por defecto apropiados
   - Validar valores contra patrones permitidos

5. META-INFORMATION:
   - Agregar exshuffle: TRUE si falta

IMPORTANTE:
- NO modificar lógica matemática ni generación de datos
- NO cambiar contenido de preguntas/respuestas sin justificación
- NO modificar chunks de código complejo sin contexto claro
- Generar backup del archivo original antes de aplicar cambios

FORMATO DE SALIDA:
1. Lista de cambios aplicados:
   [APLICADO] Descripción del cambio
   Ubicación: Línea X
   Justificación: Referencia a guia_estilo_icfes.md

2. Archivo corregido completo (formato raw para guardar)

3. Advertencias sobre cambios que requieren revisión manual
```

---

## Ejemplo de Ejecución

### Input (Archivo con errores)
```r
---
output:
  pdf_document:
    keep_tex: true
---

```{r inicio, include=FALSE}
library(exams)
set.seed(12345)
options(OutDec = ".")
```

Question
========
La empresa A & B reportó resultados del 50% de aumento.

Answerlist
----------
- Opción A
- Opción B
```

### Output (Archivo corregido)
```r
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
- \usepackage{tikz}
- \usepackage{pgfplots}
---

```{r inicio, include=FALSE}
library(exams)
set.seed(sample(1:100000, 1))
options(scipen = 999)
options(OutDec = ".")
options(digits = 10)
Sys.setlocale(category = "LC_NUMERIC", locale = "C")
```

Question
========
La empresa A \& B reportó resultados del 50\% de aumento.

Answerlist
----------
- Opción A
- Opción B
```

### Log de Cambios
```
CAMBIOS APLICADOS:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
[APLICADO] Agregado latex_engine: xelatex en YAML
           Ubicación: Línea 3
           Justificación: guia_estilo_icfes.md sección 1

[APLICADO] Agregado header-includes con paquetes requeridos
           Ubicación: Línea 8
           Justificación: guia_estilo_icfes.md sección 1

[APLICADO] Corregido set.seed(12345) a set.seed(sample(1:100000, 1))
           Ubicación: Línea 15
           Justificación: guia_estilo_icfes.md sección 3

[APLICADO] Agregado options(scipen = 999)
           Ubicación: Línea 16
           Justificación: guia_estilo_icfes.md sección 3

[APLICADO] Escapado & → \& en texto
           Ubicación: Línea 25
           Justificación: guia_estilo_icfes.md "ERRORES COMUNES"

[APLICADO] Escapado % → \% en texto
           Ubicación: Línea 25
           Justificación: guia_estilo_icfes.md "ERRORES COMUNES"
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Total cambios aplicados: 6
Archivo guardado como: ejercicio_corregido.Rmd
Backup original: ejercicio_corregido.Rmd.backup
```

---

## Integración con Workflow

Este agente se puede ejecutar:

1. **Post-validación**: Después de que el validador encuentra errores
2. **Pre-commit**: Automáticamente antes de commits
3. **Batch processing**: En múltiples archivos simultáneamente
4. **CI/CD**: Como parte del pipeline de calidad
