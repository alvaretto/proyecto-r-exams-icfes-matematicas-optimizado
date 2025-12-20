# Agente Personalizado: Comparador de Estructura contra Ejemplos Funcionales

**Nivel**: Intermedio  
**Tipo**: Agente Especializado con Análisis Comparativo  
**Propósito**: Comparar estructura de archivo .Rmd contra ejemplos funcionales validados

---

## Configuración del Agente

```yaml
# .claudecode/agents/structure_comparator.yml
name: "Comparador de Estructura ICFES"
description: "Compara archivos .Rmd contra ejemplos funcionales para validar estructura"
temperature: 0.0
model: "claude-3-5-sonnet-20241022"

system_instructions: |
  Eres un analizador comparativo de estructura de archivos R-exams. Tu función es:
  
  1. Leer el archivo .Rmd a validar
  2. Cargar ejemplos funcionales de A-Produccion/Ejemplos-Funcionales-Rmd/
  3. Comparar estructura, patrones y configuración
  4. Identificar desviaciones de patrones validados
  5. Sugerir alineación con ejemplos funcionales
  
  ÁREAS DE COMPARACIÓN:
  - Orden y estructura de chunks
  - Configuración de chunks (opciones, parámetros)
  - Patrones de generación de datos
  - Configuración de TikZ/Python si aplica
  - Estructura de secciones markdown
  - Configuración de librerías y dependencias
  
  FORMATO DE RESPUESTA:
  - Tabla comparativa estructura actual vs ejemplos funcionales
  - Lista de patrones válidos encontrados en ejemplos
  - Recomendaciones específicas para alinear estructura
  - Referencias a archivos ejemplo específicos

context_files:
  - ".claudedoc/guia_estilo_icfes.md"
  - "A-Produccion/Ejemplos-Funcionales-Rmd/"
  - ".claudecode/config.yml"

capabilities:
  - read_files
  - compare_structures
  - analyze_patterns
  - suggest_alignment
```

---

## Instrucciones de Uso

### Comparación Simple
```bash
# Comparar contra todos los ejemplos funcionales
claude-code agent compare structure_comparator /ruta/al/archivo.Rmd
```

### Comparación Específica
```bash
# Comparar contra un ejemplo funcional específico
claude-code agent compare structure_comparator \
  /ruta/al/archivo.Rmd \
  --against A-Produccion/Ejemplos-Funcionales-Rmd/Ejemplo_01.Rmd
```

### Modo Detallado
```bash
# Análisis profundo con reporte completo
claude-code agent compare structure_comparator \
  /ruta/al/archivo.Rmd \
  --detailed \
  --output reporte_comparacion.md
```

---

## Prompt Template para el Agente

```
Compara la estructura del siguiente archivo .Rmd contra los ejemplos funcionales:

ARCHIVO A VALIDAR:
{{file_content}}

EJEMPLOS FUNCIONALES DISPONIBLES:
{{functional_examples_list}}

INSTRUCCIONES DE COMPARACIÓN:

1. ESTRUCTURA DE CHUNKS:
   - Identificar orden de chunks en archivo actual
   - Comparar con orden estándar en ejemplos funcionales
   - Detectar chunks faltantes o en orden incorrecto
   - Validar nombres de chunks (deben seguir convenciones)

2. CONFIGURACIÓN DE CHUNKS:
   - Comparar opciones de chunks (echo, results, include, etc.)
   - Validar configuración de chunks Python vs ejemplos
   - Validar configuración de chunks TikZ vs ejemplos
   - Detectar opciones faltantes o incorrectas

3. CONFIGURACIÓN INICIAL:
   - Comparar chunk {r inicio} con ejemplos
   - Validar librerías cargadas
   - Validar opciones globales (options, knitr::opts_chunk$set)
   - Validar configuración de Python (use_python, reticulate)

4. GENERACIÓN DE DATOS:
   - Comparar estructura de función generar_datos()
   - Validar presencia de chunk de prueba de diversidad
   - Comparar patrones de aleatorización

5. ESTRUCTURA MARKDOWN:
   - Validar orden de secciones (Question, Solution, Meta-information)
   - Comparar formato de Answerlist
   - Validar estructura de Meta-information

FORMATO DE SALIDA:

1. TABLA COMPARATIVA:
   | Elemento | Archivo Actual | Ejemplos Funcionales | Estado |
   |----------|----------------|----------------------|--------|
   | [Detalles específicos] | | | |

2. PATRONES IDENTIFICADOS:
   - [Lista de patrones válidos encontrados en ejemplos]
   - [Recomendaciones específicas]

3. DESVIACIONES ENCONTRADAS:
   - [Lista de desviaciones con sugerencias de corrección]

4. REFERENCIAS:
   - [Archivos ejemplo específicos que deben seguirse]
```

---

## Ejemplo de Salida del Agente

```
COMPARACIÓN DE ESTRUCTURA - ejercicio_estadistica_n2_v1.Rmd
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

ARCHIVO COMPARADO: ejercicio_estadistica_n2_v1.Rmd
EJEMPLOS DE REFERENCIA: Ejemplo_01.Rmd, estadistica_diagramas_caja_interpretacion_representacion_Nivel2_v2.Rmd

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
TABLA COMPARATIVA DE ESTRUCTURA
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

| Elemento | Archivo Actual | Ejemplos Funcionales | Estado |
|----------|----------------|----------------------|--------|
| Chunk inicio | Presente, falta options(scipen) | Presente con todas las opciones | ⚠️ INCOMPLETO |
| Chunk data_generation | Presente | Presente | ✅ CORRECTO |
| Chunk version_diversity_test | AUSENTE | Presente en todos los ejemplos | ❌ FALTANTE |
| Configuración Python | use_python() presente | use_python("/usr/bin/python3", required=TRUE) | ⚠️ DIFERENTE |
| Orden secciones | Question → Solution → Meta | Question → Solution → Meta | ✅ CORRECTO |
| Meta-information exshuffle | Presente | Presente | ✅ CORRECTO |

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
PATRONES IDENTIFICADOS EN EJEMPLOS FUNCIONALES
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

✓ Todos los ejemplos incluyen chunk version_diversity_test después de data_generation
✓ Configuración de Python usa ruta explícita: "/usr/bin/python3"
✓ Chunk inicio siempre incluye: options(scipen=999), options(OutDec="."), Sys.setlocale()
✓ Orden estándar: YAML → inicio → data_generation → version_diversity_test → [otros chunks] → Question → Solution → Meta-information

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
DESVIACIONES ENCONTRADAS
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

❌ CRÍTICO: Falta chunk version_diversity_test
   Ubicación esperada: Después de chunk data_generation
   Referencia: Ejemplo_01.Rmd líneas 303-318
   Acción requerida: Agregar chunk de prueba de diversidad

⚠️ IMPORTANTE: use_python() sin ruta explícita
   Ubicación: Chunk inicio línea 43
   Código actual: use_python(Sys.which("python"), required = TRUE)
   Código recomendado: use_python("/usr/bin/python3", required = TRUE)
   Referencia: guia_estilo_icfes.md sección 3

⚠️ IMPORTANTE: Falta options(scipen = 999) en chunk inicio
   Ubicación: Chunk inicio
   Referencia: Ejemplo_01.Rmd línea 26, guia_estilo_icfes.md sección 3

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
RECOMENDACIONES DE ALINEACIÓN
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

1. Agregar chunk version_diversity_test siguiendo patrón de Ejemplo_01.Rmd
2. Actualizar use_python() a ruta explícita "/usr/bin/python3"
3. Agregar options(scipen = 999) en chunk inicio
4. Considerar seguir estructura completa de Ejemplo_01.Rmd como plantilla base

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
REFERENCIAS A EJEMPLOS FUNCIONALES
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

📄 Ejemplo_01.Rmd
   - Estructura completa de chunks
   - Configuración Python correcta
   - Chunk de prueba de diversidad

📄 estadistica_diagramas_caja_interpretacion_representacion_Nivel2_v2.Rmd
   - Configuración de gráficos estadísticos
   - Uso de chunks Python para visualizaciones
```

---

## Integración con Workflow

Este agente se puede usar:

1. **Revisión de código**: Antes de considerar un archivo como "completo"
2. **Onboarding**: Para nuevos desarrolladores aprendiendo patrones
3. **Refactoring**: Para alinear archivos legacy con nuevos estándares
4. **Validación continua**: Como parte del proceso de calidad
