# 🤖 GUÍA COMPLETA: Agentes y Workflows de IA para ICFES R-Exams

## 📋 ÍNDICE

1. [Introducción a Agentes y Workflows](#introducción)
2. [Configuración Básica](#configuración-básica)
3. [Agentes Especializados para el Proyecto](#agentes-especializados)
4. [Workflows Automatizados](#workflows-automatizados)
5. [Ejemplos Prácticos](#ejemplos-prácticos)
6. [Mejores Prácticas](#mejores-prácticas)

---

## 🎯 INTRODUCCIÓN

### ¿Qué son los Agentes de IA?

Los **agentes de IA** son asistentes especializados que pueden:

- Ejecutar tareas complejas de forma autónoma
- Seguir flujos de trabajo predefinidos
- Tomar decisiones basadas en contexto
- Interactuar con herramientas y sistemas

### ¿Qué son los Workflows?

Los **workflows** son secuencias de pasos automatizados que:

- Definen procesos repetibles
- Coordinan múltiples tareas
- Garantizan consistencia
- Optimizan tiempo y esfuerzo

### Beneficios para el Proyecto ICFES R-Exams

✅ **Automatización** de tareas repetitivas\
✅ **Consistencia** en la generación de ejercicios\
✅ **Calidad** mediante validaciones automáticas\
✅ **Velocidad** en el desarrollo\
✅ **Escalabilidad** del proyecto

---

## ⚙️ CONFIGURACIÓN BÁSICA

### Paso 1: Estructura de Directorios

```bash
# Crear estructura para agentes
mkdir -p Auxiliares/Agentes-IA/{agentes,workflows,configuraciones,logs}
```

### Paso 2: Archivo de Configuración Global

Crear `Auxiliares/Agentes-IA/configuraciones/config-global.json`:


```json
{
  "proyecto": "RepositorioMatematicasICFES_R_Exams",
  "version": "1.0.0",
  "rutas": {
    "raiz": "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams",
    "ejemplos_funcionales": "A-Produccion/Ejemplos-Funcionales-Rmd",
    "lab": "Lab-Manjaro",
    "auxiliares": "Auxiliares"
  },
  "configuracion_r_exams": {
    "versiones_minimas": 300,
    "formatos_salida": ["html", "pdf", "moodle", "nops"],
    "motor_latex": "pdflatex",
    "python_path": "/usr/bin/python3"
  },
  "validaciones": {
    "metadatos_icfes": true,
    "diversidad_versiones": true,
    "sintaxis_tikz": true,
    "integracion_python": true
  }
}
```

### Paso 3: Configuración de Agentes en Augment

En VSCode con Augment, los agentes se configuran mediante:


1. **Archivos `.agent.md`**: Definen comportamiento y contexto
2. **Reglas personalizadas**: En `.augment/rules/`
3. **Memorias**: Información persistente para el agente

---

## 🤖 AGENTES ESPECIALIZADOS

### 1. Agente Generador de Ejercicios

**Propósito**: Crear ejercicios .Rmd completos a partir de imágenes

**Archivo**: `Auxiliares/Agentes-IA/agentes/generador-ejercicios.agent.md`

```markdown
# Agente Generador de Ejercicios ICFES

## Identidad
Especialista en generar ejercicios matemáticos ICFES en formato R-exams (.Rmd)

## Contexto del Proyecto
- Proyecto: RepositorioMatematicasICFES_R_Exams
- Framework: R-exams 2.4+
- Estándares: ICFES Colombia

## Flujo de Trabajo Obligatorio

### FASE 1: Análisis de Imagen
1. Detectar contenido gráfico (Sistema Condicional Automático)
2. Identificar concepto matemático principal
3. Determinar competencia ICFES apropiada
4. Clasificar nivel de dificultad (1-4)

### FASE 2: Consulta de Ejemplos
1. OBLIGATORIO: Revisar `/A-Produccion/Ejemplos-Funcionales-Rmd/`
2. Identificar patrón similar exitoso
3. Extraer configuraciones técnicas validadas

### FASE 3: Generación de Código
1. Crear estructura .Rmd completa
2. Implementar chunk de configuración inicial
3. Desarrollar función `generar_datos()`
4. Crear visualizaciones (TikZ/Python según necesidad)
5. Redactar Question y Solution
6. Agregar metadatos ICFES completos

### FASE 4: Validación
1. Test de diversidad (300+ versiones)
2. Validación de sintaxis
3. Compilación de prueba
4. Verificación de metadatos

## Restricciones Críticas
- NUNCA usar set.seed() fijo
- SIEMPRE consultar ejemplos funcionales PRIMERO
- OBLIGATORIO incluir test de diversidad
- REQUERIDO metadatos ICFES completos

## Herramientas Disponibles
- codebase-retrieval: Buscar información en el proyecto
- view: Leer archivos y directorios
- save-file: Crear nuevos archivos .Rmd
- str-replace-editor: Editar archivos existentes
- launch-process: Ejecutar comandos R/Python
```

---

### 2. Agente Validador de Código

**Propósito**: Verificar y corregir errores en archivos .Rmd

**Archivo**: `Auxiliares/Agentes-IA/agentes/validador-codigo.agent.md`

```markdown
# Agente Validador de Código R-exams

## Identidad
Especialista en validación y corrección de archivos .Rmd para R-exams ICFES

## Responsabilidades

### 1. Validación de Estructura
- ✅ Verificar encabezado YAML completo
- ✅ Comprobar chunk de configuración inicial
- ✅ Validar función generar_datos()
- ✅ Revisar test de diversidad de versiones
- ✅ Verificar secciones Question/Solution/Meta-information

### 2. Validación de Sintaxis
- ✅ Sintaxis R correcta
- ✅ Sintaxis Python (reticulate) correcta
- ✅ Sintaxis TikZ válida
- ✅ Sintaxis LaTeX apropiada

### 3. Validación de Metadatos ICFES
- ✅ Competencia válida
- ✅ Nivel de dificultad (1-4)
- ✅ Componente correcto
- ✅ Contexto apropiado
- ✅ Categoría de contenido

### 4. Validación de Configuraciones
- ✅ Tolerancias apropiadas (extol)
- ✅ Formato numérico consistente
- ✅ Configuración Python correcta
- ✅ Configuración TikZ funcional

## Proceso de Corrección

### Paso 1: Diagnóstico
1. Leer archivo completo
2. Identificar errores por categoría
3. Consultar ejemplos funcionales para soluciones
4. Priorizar errores críticos

### Paso 2: Corrección
1. Aplicar soluciones validadas
2. Mantener estructura original
3. Preservar lógica matemática
4. Documentar cambios realizados

### Paso 3: Verificación
1. Compilar archivo corregido
2. Ejecutar tests de diversidad
3. Validar salidas en múltiples formatos
4. Confirmar funcionamiento completo

## Fuentes de Referencia
- `/A-Produccion/Ejemplos-Funcionales-Rmd/` - Patrones correctos
- `/Auxiliares/BIBLIOTECA_Soluciones_Errores_Comunes.md` - Soluciones conocidas
- `/Auxiliares/CHECKLIST_Validacion_Archivos_Rmd.md` - Lista de verificación
```

---

### 3. Agente Graficador TikZ

**Propósito**: Generar código TikZ profesional a partir de imágenes

**Archivo**: `Auxiliares/Agentes-IA/agentes/graficador-tikz.agent.md`

```markdown
# Agente Graficador TikZ Especializado

## Identidad
Experto en replicación de imágenes matemáticas usando código TikZ

## Objetivo
Generar código TikZ con 98%+ de fidelidad visual respecto a imagen original

## Metodología

### Fase 1: Análisis de Imagen
1. Identificar elementos geométricos
2. Detectar colores RGB exactos
3. Medir proporciones y escalas
4. Reconocer texto y etiquetas
5. Analizar estructura general

### Fase 2: Consulta de Templates
1. Revisar `/Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/`
2. Identificar template apropiado
3. Adaptar a necesidades específicas

### Fase 3: Generación de Código
1. Crear estructura base TikZ
2. Implementar elementos geométricos
3. Aplicar colores exactos
4. Posicionar texto y etiquetas
5. Ajustar escalas y proporciones

### Fase 4: Validación de Fidelidad
1. Compilar código TikZ
2. Comparar con imagen original
3. Medir fidelidad visual (objetivo: 98%+)
4. Iterar hasta alcanzar objetivo

## Métricas de Fidelidad Visual

- **Precisión Geométrica** (25%): Proporciones, ángulos, escalas
- **Fidelidad Cromática** (25%): Colores RGB exactos
- **Posicionamiento** (25%): Ubicación relativa de elementos
- **Completitud** (25%): Todos los elementos presentes

## Configuración TikZ para R-exams

```r
# Configuración estándar
include_tikz(codigo_tikz,
             name = "diagrama_principal",
             markup = "markdown",
             format = typ,
             library = c("3d", "babel"),
             packages = c("tikz", "xcolor", "pgfplots"),
             width = "12cm")
```

## Bibliotecas TikZ Permitidas
- tikz (core)
- xcolor (colores)
- pgfplots (gráficos)
- 3d (figuras 3D)
- babel (compatibilidad)
```

---

### 4. Agente Gestor de Metadatos ICFES

**Propósito**: Gestionar y validar metadatos ICFES en ejercicios

**Archivo**: `Auxiliares/Agentes-IA/agentes/gestor-metadatos.agent.md`

```markdown
# Agente Gestor de Metadatos ICFES

## Identidad
Especialista en clasificación y gestión de metadatos ICFES

## Competencias ICFES

### 1. Interpretación y Representación
**Descripción**: Comprender y usar diferentes representaciones matemáticas

**Indicadores**:

- Leer e interpretar información en diversos formatos
- Traducir entre representaciones
- Identificar patrones y relaciones

### 2. Formulación y Ejecución
**Descripción**: Plantear y resolver problemas matemáticos

**Indicadores**:

- Formular problemas matemáticos
- Diseñar estrategias de solución
- Ejecutar procedimientos matemáticos

### 3. Argumentación
**Descripción**: Justificar y validar procedimientos y resultados

**Indicadores**:

- Justificar razonamientos
- Validar procedimientos
- Generalizar resultados

## Componentes Matemáticos

### Geométrico-Métrico
- Geometría plana y espacial
- Medición y estimación
- Transformaciones geométricas

### Numérico-Variacional
- Números y operaciones
- Álgebra y funciones
- Cálculo básico

### Aleatorio
- Estadística descriptiva
- Probabilidad
- Análisis de datos

## Niveles de Dificultad

### Nivel 1 (Básico)
- Aplicación directa de conceptos
- Situaciones familiares
- Un paso de razonamiento

### Nivel 2 (Intermedio)
- Aplicación de conceptos en contextos variados
- Dos o tres pasos de razonamiento
- Conexión entre conceptos

### Nivel 3 (Avanzado)
- Análisis de situaciones complejas
- Múltiples pasos de razonamiento
- Integración de varios conceptos

### Nivel 4 (Superior)
- Situaciones no rutinarias
- Razonamiento abstracto
- Generalización y demostración

## Proceso de Clasificación

### Paso 1: Análisis del Ejercicio
1. Leer enunciado completo
2. Identificar concepto matemático principal
3. Determinar tipo de razonamiento requerido
4. Evaluar complejidad del problema

### Paso 2: Asignación de Competencia
1. Identificar acción principal del ejercicio
2. Comparar con descriptores de competencias
3. Seleccionar competencia más apropiada

### Paso 3: Determinación de Nivel
1. Contar pasos de razonamiento
2. Evaluar familiaridad del contexto
3. Medir complejidad conceptual
4. Asignar nivel (1-4)

### Paso 4: Clasificación Completa
1. Determinar componente matemático
2. Seleccionar contexto apropiado
3. Clasificar tipo de contenido
4. Generar metadatos completos

## Formato de Salida

```yaml
icfes:

  competencia:

    - [competencia_identificada]
  nivel_dificultad: [1|2|3|4]
  contenido:

    categoria: [algebra_calculo|geometria|estadistica]
    tipo: [generico|no_generico]
  contexto: [familiar|laboral|comunitario|matematico]
  eje_axial: [eje1|eje2|eje3|eje4]
  componente: [geometrico_metrico|numerico_variacional|aleatorio]
```
```

---

## 🔄 WORKFLOWS AUTOMATIZADOS

### Workflow 1: Generación Completa de Ejercicio

**Archivo**: `Auxiliares/Agentes-IA/workflows/workflow-generacion-completa.md`

```markdown
# Workflow: Generación Completa de Ejercicio desde Imagen

## Trigger
Usuario proporciona imagen PNG de escenario matemático

## Pasos del Workflow

### 1. Recepción y Análisis Inicial
**Agente**: Sistema Condicional Automático
**Acciones**:

- [ ] Recibir imagen PNG
- [ ] Detectar presencia de contenido gráfico/tabular
- [ ] Determinar flujo apropiado (A o B)
- [ ] Registrar decisión en log

**Salida**: Decisión de flujo (A: sin gráficas, B: con gráficas)

---

### 2A. Flujo A - Sin Gráficas Complejas
**Agente**: Generador de Ejercicios
**Acciones**:

- [ ] Analizar imagen para extraer texto y datos
- [ ] Identificar concepto matemático
- [ ] Consultar ejemplos funcionales similares
- [ ] Generar estructura .Rmd base

**Salida**: Archivo .Rmd con estructura básica

---

### 2B. Flujo B - Con Gráficas Complejas
**Agente**: Graficador TikZ Especializado
**Acciones**:

- [ ] Analizar elementos gráficos de la imagen
- [ ] Consultar templates TikZ apropiados
- [ ] Generar código TikZ con 98%+ fidelidad
- [ ] Validar compilación de código TikZ
- [ ] Integrar en estructura .Rmd

**Salida**: Código TikZ validado + estructura .Rmd

---

### 3. Clasificación ICFES
**Agente**: Gestor de Metadatos ICFES
**Acciones**:

- [ ] Analizar contenido matemático del ejercicio
- [ ] Determinar competencia ICFES apropiada
- [ ] Asignar nivel de dificultad (1-4)
- [ ] Clasificar componente matemático
- [ ] Seleccionar contexto apropiado
- [ ] Generar metadatos ICFES completos

**Salida**: Metadatos ICFES en formato YAML

---

### 4. Desarrollo de Contenido
**Agente**: Generador de Ejercicios
**Acciones**:

- [ ] Implementar función generar_datos()
- [ ] Crear test de diversidad (300+ versiones)
- [ ] Desarrollar visualizaciones (Python/TikZ según necesidad)
- [ ] Redactar sección Question con contexto
- [ ] Crear 4 opciones de respuesta (1 correcta + 3 distractores)
- [ ] Redactar sección Solution con explicación detallada
- [ ] Agregar Meta-information completa

**Salida**: Archivo .Rmd completo

---

### 5. Validación Técnica
**Agente**: Validador de Código
**Acciones**:

- [ ] Verificar estructura completa del archivo
- [ ] Validar sintaxis R, Python, TikZ, LaTeX
- [ ] Comprobar configuraciones técnicas
- [ ] Ejecutar test de diversidad de versiones
- [ ] Validar metadatos ICFES
- [ ] Verificar tolerancias numéricas

**Salida**: Reporte de validación

---

### 6. Compilación de Prueba
**Agente**: Validador de Código
**Acciones**:

- [ ] Compilar a HTML (exams2html)
- [ ] Compilar a PDF (exams2pdf)
- [ ] Compilar a Moodle XML (exams2moodle)
- [ ] Verificar salidas en todos los formatos
- [ ] Confirmar visualizaciones correctas

**Salida**: Archivos compilados en múltiples formatos

---

### 7. Corrección de Errores (si necesario)
**Agente**: Validador de Código
**Acciones**:

- [ ] Identificar errores de compilación
- [ ] Consultar biblioteca de soluciones
- [ ] Aplicar correcciones validadas
- [ ] Re-compilar y verificar
- [ ] Iterar hasta éxito completo

**Salida**: Archivo .Rmd funcional y validado

---

### 8. Entrega Final
**Agente**: Sistema
**Acciones**:

- [ ] Guardar archivo en ubicación apropiada
- [ ] Generar nombre según convención
- [ ] Crear log de proceso completo
- [ ] Documentar decisiones tomadas
- [ ] Presentar resultado al usuario

**Salida**: Ejercicio .Rmd completo, validado y funcional

---

## Métricas de Éxito

✅ **Fidelidad Visual**: 98%+ (si aplica gráficas)
✅ **Diversidad**: 300+ versiones únicas
✅ **Compilación**: Exitosa en HTML, PDF, Moodle
✅ **Metadatos**: ICFES completos y correctos
✅ **Calidad**: Sin errores de sintaxis o lógica

## Tiempo Estimado
- Flujo A (sin gráficas): 5-10 minutos
- Flujo B (con gráficas): 15-25 minutos
```

---

### Workflow 2: Corrección y Optimización de Ejercicio Existente

**Archivo**: `Auxiliares/Agentes-IA/workflows/workflow-correccion-optimizacion.md`

```markdown
# Workflow: Corrección y Optimización de Ejercicio Existente

## Trigger
Usuario solicita corrección/optimización de archivo .Rmd existente

## Pasos del Workflow

### 1. Análisis Inicial
**Agente**: Validador de Código
**Acciones**:

- [ ] Leer archivo .Rmd completo
- [ ] Identificar errores por categoría
- [ ] Evaluar nivel de optimización necesario
- [ ] Priorizar problemas críticos

**Salida**: Diagnóstico completo con errores categorizados

---

### 2. Consulta de Soluciones
**Agente**: Validador de Código
**Acciones**:

- [ ] Buscar en `/A-Produccion/Ejemplos-Funcionales-Rmd/`
- [ ] Consultar `/Auxiliares/BIBLIOTECA_Soluciones_Errores_Comunes.md`
- [ ] Identificar patrones correctos aplicables
- [ ] Seleccionar soluciones validadas

**Salida**: Lista de soluciones a aplicar

---

### 3. Corrección de Errores Críticos
**Agente**: Validador de Código
**Acciones**:

- [ ] Corregir errores de sintaxis
- [ ] Arreglar configuraciones técnicas
- [ ] Solucionar problemas de compilación
- [ ] Validar correcciones aplicadas

**Salida**: Archivo con errores críticos corregidos

---

### 4. Optimización de Código
**Agente**: Validador de Código
**Acciones**:

- [ ] Mejorar función generar_datos()
- [ ] Optimizar generación de visualizaciones
- [ ] Refactorizar código redundante
- [ ] Aplicar mejores prácticas

**Salida**: Código optimizado

---

### 5. Validación de Metadatos ICFES
**Agente**: Gestor de Metadatos ICFES
**Acciones**:

- [ ] Verificar metadatos existentes
- [ ] Corregir clasificaciones incorrectas
- [ ] Completar metadatos faltantes
- [ ] Validar coherencia con contenido

**Salida**: Metadatos ICFES validados

---

### 6. Testing Completo
**Agente**: Validador de Código
**Acciones**:

- [ ] Ejecutar test de diversidad
- [ ] Compilar en múltiples formatos
- [ ] Verificar visualizaciones
- [ ] Validar respuestas y distractores

**Salida**: Reporte de testing completo

---

### 7. Entrega de Versión Optimizada
**Agente**: Sistema
**Acciones**:

- [ ] Guardar versión optimizada
- [ ] Generar reporte de cambios
- [ ] Documentar mejoras aplicadas
- [ ] Presentar resultado al usuario

**Salida**: Archivo .Rmd optimizado y validado
```

---

### Workflow 3: Validación Masiva de Ejercicios

**Archivo**: `Auxiliares/Agentes-IA/workflows/workflow-validacion-masiva.md`

```markdown
# Workflow: Validación Masiva de Ejercicios

## Trigger
Usuario solicita validación de múltiples archivos .Rmd

## Pasos del Workflow

### 1. Escaneo de Directorio
**Agente**: Sistema
**Acciones**:

- [ ] Identificar todos los archivos .Rmd en directorio
- [ ] Crear lista de archivos a validar
- [ ] Establecer orden de procesamiento

**Salida**: Lista de archivos .Rmd

---

### 2. Validación Individual (Loop)
**Agente**: Validador de Código
**Para cada archivo**:

- [ ] Validar estructura
- [ ] Verificar sintaxis
- [ ] Comprobar metadatos ICFES
- [ ] Ejecutar test de diversidad
- [ ] Intentar compilación
- [ ] Registrar resultados

**Salida**: Reporte individual por archivo

---

### 3. Consolidación de Resultados
**Agente**: Sistema
**Acciones**:

- [ ] Agregar resultados de todos los archivos
- [ ] Identificar patrones de errores comunes
- [ ] Calcular estadísticas generales
- [ ] Priorizar archivos con errores críticos

**Salida**: Reporte consolidado

---

### 4. Generación de Reporte Final
**Agente**: Sistema
**Acciones**:

- [ ] Crear reporte en formato Markdown
- [ ] Incluir estadísticas generales
- [ ] Listar archivos con errores
- [ ] Proporcionar recomendaciones
- [ ] Generar plan de corrección

**Salida**: Reporte completo de validación masiva
```

---

## 💡 EJEMPLOS PRÁCTICOS

### Ejemplo 1: Crear Agente Personalizado en Augment

#### Paso 1: Crear archivo de configuración del agente

```bash
# Crear directorio si no existe
mkdir -p .augment/agents

# Crear archivo de agente
touch .augment/agents/generador-estadistica.agent.md
```

#### Paso 2: Definir el agente

Contenido de `.augment/agents/generador-estadistica.agent.md`:


```markdown
# Agente Generador de Ejercicios de Estadística

## Identidad
Especialista en ejercicios de estadística descriptiva para ICFES

## Conocimiento Especializado

### Conceptos que domino:

- Medidas de tendencia central (media, mediana, moda)
- Medidas de dispersión (rango, varianza, desviación estándar)
- Gráficos estadísticos (barras, circulares, histogramas, diagramas de caja)
- Tablas de frecuencia
- Interpretación de datos

### Competencias ICFES que evalúo:

- Interpretación y Representación (principal)
- Formulación y Ejecución
- Argumentación

## Proceso de Generación

1. **Análisis de imagen**:

   - Identificar tipo de gráfico o tabla
   - Extraer datos numéricos
   - Reconocer contexto del problema

2. **Consulta de ejemplos**:

   - Buscar en `/A-Produccion/Ejemplos-Funcionales-Rmd/`
   - Filtrar por ejercicios de estadística
   - Identificar patrón más similar

3. **Generación de código**:

   - Usar Python/matplotlib para gráficos
   - Implementar aleatorización de datos
   - Crear distractores pedagógicos
   - Asegurar 300+ versiones únicas

4. **Validación**:

   - Compilar en HTML, PDF, Moodle
   - Verificar coherencia matemática
   - Validar metadatos ICFES

## Plantillas que uso

### Para gráficos de barras:

```python
import matplotlib.pyplot as plt
import numpy as np
import random

# Datos desde R
categorias = r.categorias
valores = r.valores

# Colores aleatorios
colores = random.choice(['blue', 'green', 'red', 'purple', 'orange'])

# Crear gráfico
fig, ax = plt.subplots(figsize=(10, 6))
ax.bar(categorias, valores, color=colores)
ax.set_xlabel('Categorías')
ax.set_ylabel('Frecuencia')
ax.set_title('Distribución de Frecuencias')

plt.tight_layout()
plt.savefig('grafico_barras.png', dpi=150, bbox_inches='tight')
plt.close()
```

### Para medidas de tendencia central:

```r
generar_datos <- function() {
  # Generar conjunto de datos aleatorio
  n <- sample(8:15, 1)
  datos <- sample(10:100, n, replace = FALSE)
  datos_ordenados <- sort(datos)

  # Calcular medidas
  media <- round(mean(datos), 1)
  mediana <- median(datos)

  # Calcular moda (puede no existir)
  tabla_freq <- table(datos)
  max_freq <- max(tabla_freq)
  if(max_freq > 1) {
    moda <- as.numeric(names(tabla_freq)[tabla_freq == max_freq])
  } else {
    moda <- NULL
  }

  return(list(
    datos = datos,
    datos_ordenados = datos_ordenados,
    media = media,
    mediana = mediana,
    moda = moda,
    n = n
  ))
}
```

## Restricciones

- NUNCA usar datos que generen mediana ambigua sin justificación
- SIEMPRE incluir números pares e impares para cálculo de mediana
- OBLIGATORIO crear distractores basados en errores conceptuales reales
- REQUERIDO validar que todas las opciones sean textualmente únicas
```

#### Paso 3: Usar el agente

En Augment, puedes invocar el agente con:


```
@generador-estadistica Genera un ejercicio de mediana a partir de esta imagen
```

---

### Ejemplo 2: Workflow Automatizado con Scripts

#### Script de validación automática

Crear `Auxiliares/Agentes-IA/scripts/validar-ejercicio.sh`:


```bash
#!/bin/bash

# Script de validación automática de ejercicio .Rmd
# Uso: ./validar-ejercicio.sh archivo.Rmd

ARCHIVO=$1
PROYECTO_ROOT="/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams"

echo "🔍 Validando: $ARCHIVO"
echo "================================"

# 1. Verificar que el archivo existe
if [ ! -f "$ARCHIVO" ]; then
    echo "❌ Error: Archivo no encontrado"
    exit 1
fi

# 2. Verificar estructura YAML
echo "📋 Verificando encabezado YAML..."
if grep -q "^---$" "$ARCHIVO"; then
    echo "✅ Encabezado YAML presente"
else
    echo "❌ Falta encabezado YAML"
fi

# 3. Verificar metadatos ICFES
echo "📊 Verificando metadatos ICFES..."
if grep -q "icfes:" "$ARCHIVO"; then
    echo "✅ Metadatos ICFES presentes"

    # Verificar campos específicos
    grep -q "competencia:" "$ARCHIVO" && echo "  ✅ Competencia definida" || echo "  ❌ Falta competencia"
    grep -q "nivel_dificultad:" "$ARCHIVO" && echo "  ✅ Nivel de dificultad definido" || echo "  ❌ Falta nivel"
    grep -q "componente:" "$ARCHIVO" && echo "  ✅ Componente definido" || echo "  ❌ Falta componente"
else
    echo "❌ Faltan metadatos ICFES"
fi

# 4. Verificar chunks obligatorios
echo "🔧 Verificando chunks obligatorios..."
grep -q "```{r setup" "$ARCHIVO" && echo "✅ Chunk setup presente" || echo "❌ Falta chunk setup"
grep -q "generar_datos" "$ARCHIVO" && echo "✅ Función generar_datos presente" || echo "❌ Falta función generar_datos"
grep -q "version_diversity_test" "$ARCHIVO" && echo "✅ Test de diversidad presente" || echo "❌ Falta test de diversidad"

# 5. Verificar secciones R-exams
echo "📝 Verificando secciones R-exams..."
grep -q "^Question$" "$ARCHIVO" && echo "✅ Sección Question presente" || echo "❌ Falta sección Question"
grep -q "^Solution$" "$ARCHIVO" && echo "✅ Sección Solution presente" || echo "❌ Falta sección Solution"
grep -q "^Meta-information$" "$ARCHIVO" && echo "✅ Sección Meta-information presente" || echo "❌ Falta sección Meta-information"

# 6. Verificar meta-información
echo "ℹ️ Verificando meta-información..."
grep -q "exname:" "$ARCHIVO" && echo "✅ exname definido" || echo "❌ Falta exname"
grep -q "extype:" "$ARCHIVO" && echo "✅ extype definido" || echo "❌ Falta extype"
grep -q "exsolution:" "$ARCHIVO" && echo "✅ exsolution definido" || echo "❌ Falta exsolution"

echo "================================"
echo "✅ Validación completada"
```

#### Hacer ejecutable:


```bash
chmod +x Auxiliares/Agentes-IA/scripts/validar-ejercicio.sh
```

#### Uso:


```bash
./Auxiliares/Agentes-IA/scripts/validar-ejercicio.sh Lab-Manjaro/01-S1-2024B/ejercicio.Rmd
```

---

### Ejemplo 3: Integración con Augment usando @agent

#### Crear comando personalizado

En `.augment/rules/comandos-personalizados.md`:


```markdown
# Comandos Personalizados para ICFES R-Exams

## Comando: Generar Ejercicio Completo

**Sintaxis**: `@generar-ejercicio [ruta-imagen] [competencia] [nivel]`

**Ejemplo**: `@generar-ejercicio imagenes/estadistica01.png interpretacion_representacion 2`

**Proceso**:

1. Analizar imagen en la ruta especificada
2. Aplicar sistema condicional automático
3. Activar flujo apropiado (A o B)
4. Generar código .Rmd completo
5. Validar y compilar
6. Guardar en ubicación apropiada

---

## Comando: Validar y Corregir

**Sintaxis**: `@validar-corregir [ruta-archivo]`

**Ejemplo**: `@validar-corregir Lab-Manjaro/01-S1-2024B/ejercicio.Rmd`

**Proceso**:

1. Leer archivo especificado
2. Ejecutar validaciones completas
3. Identificar errores
4. Consultar soluciones en ejemplos funcionales
5. Aplicar correcciones
6. Re-validar
7. Presentar reporte

---

## Comando: Optimizar Diversidad

**Sintaxis**: `@optimizar-diversidad [ruta-archivo]`

**Ejemplo**: `@optimizar-diversidad Lab-Manjaro/ejercicio.Rmd`

**Proceso**:

1. Analizar función generar_datos()
2. Identificar parámetros aleatorizables
3. Ampliar rangos de variación
4. Agregar nuevos contextos
5. Implementar colores aleatorios
6. Ejecutar test de diversidad
7. Validar 300+ versiones únicas

---

## Comando: Generar Reporte de Calidad

**Sintaxis**: `@reporte-calidad [directorio]`

**Ejemplo**: `@reporte-calidad Lab-Manjaro/01-S1-2024B/`

**Proceso**:

1. Escanear todos los .Rmd en directorio
2. Validar cada archivo
3. Compilar estadísticas
4. Identificar problemas comunes
5. Generar reporte Markdown
6. Proporcionar recomendaciones
```

---

## 🎯 MEJORES PRÁCTICAS

### 1. Diseño de Agentes

#### ✅ Hacer:

- **Especialización**: Crear agentes con responsabilidades específicas
- **Documentación clara**: Definir identidad, responsabilidades y proceso
- **Ejemplos concretos**: Incluir plantillas y código de referencia
- **Restricciones explícitas**: Especificar qué NO debe hacer el agente
- **Fuentes de referencia**: Indicar dónde buscar información

#### ❌ Evitar:

- Agentes demasiado generales sin foco claro
- Instrucciones ambiguas o contradictorias
- Falta de ejemplos concretos
- Omitir restricciones críticas
- No especificar fuentes de información

---

### 2. Configuración de Workflows

#### ✅ Hacer:

- **Pasos claros**: Definir cada paso con acciones específicas
- **Checkboxes**: Usar listas de verificación para seguimiento
- **Agentes asignados**: Especificar qué agente ejecuta cada paso
- **Salidas definidas**: Indicar qué produce cada paso
- **Métricas de éxito**: Establecer criterios de validación

#### ❌ Evitar:

- Pasos vagos sin acciones concretas
- Falta de asignación de responsabilidades
- Omitir validaciones intermedias
- No definir criterios de éxito
- Workflows demasiado largos sin puntos de control

---

### 3. Integración con el Proyecto

#### ✅ Hacer:

- **Respetar estructura**: Seguir organización existente del proyecto
- **Usar ejemplos funcionales**: Consultar `/A-Produccion/Ejemplos-Funcionales-Rmd/`
- **Mantener consistencia**: Aplicar convenciones de nomenclatura
- **Documentar cambios**: Registrar decisiones y modificaciones
- **Validar continuamente**: Compilar y probar frecuentemente

#### ❌ Evitar:

- Crear estructuras paralelas inconsistentes
- Ignorar ejemplos funcionales validados
- Cambiar convenciones sin justificación
- Modificar sin documentar
- Acumular cambios sin validar

---

### 4. Gestión de Conocimiento

#### ✅ Hacer:

- **Centralizar documentación**: Mantener guías en `/Auxiliares/`
- **Actualizar regularmente**: Incorporar nuevos aprendizajes
- **Compartir soluciones**: Documentar errores y correcciones
- **Versionar cambios**: Usar control de versiones (Git)
- **Crear índices**: Facilitar búsqueda de información

#### ❌ Evitar:

- Documentación dispersa en múltiples ubicaciones
- Información desactualizada
- Soluciones no documentadas
- Cambios sin historial
- Falta de organización

---

### 5. Optimización de Agentes

#### ✅ Hacer:

- **Iterar basado en resultados**: Mejorar agentes según desempeño
- **Medir efectividad**: Usar métricas cuantificables
- **Recopilar feedback**: Aprender de errores y éxitos
- **Simplificar cuando posible**: Reducir complejidad innecesaria
- **Automatizar tareas repetitivas**: Identificar patrones automatizables

#### ❌ Evitar:

- Mantener agentes ineficientes sin mejoras
- No medir resultados
- Ignorar errores recurrentes
- Sobre-complicar procesos simples
- Automatizar sin validar primero manualmente

---

## 🚀 IMPLEMENTACIÓN PASO A PASO

### Fase 1: Configuración Inicial (Día 1)

#### Paso 1: Crear estructura de directorios
```bash
cd /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams

mkdir -p Auxiliares/Agentes-IA/{agentes,workflows,configuraciones,scripts,logs}
```

#### Paso 2: Copiar configuración global
```bash
# Crear archivo de configuración
cat > Auxiliares/Agentes-IA/configuraciones/config-global.json << 'EOF'
{
  "proyecto": "RepositorioMatematicasICFES_R_Exams",
  "version": "1.0.0",
  "rutas": {
    "raiz": "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams",
    "ejemplos_funcionales": "A-Produccion/Ejemplos-Funcionales-Rmd",
    "lab": "Lab-Manjaro",
    "auxiliares": "Auxiliares"
  },
  "configuracion_r_exams": {
    "versiones_minimas": 300,
    "formatos_salida": ["html", "pdf", "moodle", "nops"],
    "motor_latex": "pdflatex",
    "python_path": "/usr/bin/python3"
  }
}
EOF
```

#### Paso 3: Crear primer agente
```bash
# Copiar plantilla de agente generador
cp Auxiliares/Agentes-IA/01-GUIA_COMPLETA_Agentes_Workflows_ICFES_R_Exams.md \
   .augment/agents/generador-ejercicios.agent.md
```

---

### Fase 2: Prueba de Concepto (Día 2-3)

#### Paso 1: Probar agente con ejercicio simple
```
@generador-ejercicios Genera un ejercicio simple de suma de fracciones,
competencia formulacion_ejecucion, nivel 1
```

#### Paso 2: Validar resultado
```bash
# Usar script de validación
./Auxiliares/Agentes-IA/scripts/validar-ejercicio.sh [archivo-generado].Rmd
```

#### Paso 3: Compilar ejercicio
```r
# En R/RStudio
library(exams)
exams2html("[archivo-generado].Rmd")
exams2pdf("[archivo-generado].Rmd")
```

#### Paso 4: Iterar y mejorar
- Identificar problemas
- Ajustar configuración del agente
- Re-probar
- Documentar aprendizajes

---

### Fase 3: Expansión (Semana 2)

#### Paso 1: Crear agentes especializados
- Agente Validador
- Agente Graficador TikZ
- Agente Gestor de Metadatos

#### Paso 2: Implementar workflows
- Workflow de generación completa
- Workflow de corrección
- Workflow de validación masiva

#### Paso 3: Automatizar tareas comunes
- Scripts de validación
- Scripts de compilación masiva
- Scripts de generación de reportes

---

### Fase 4: Optimización (Semana 3-4)

#### Paso 1: Medir efectividad
- Tiempo de generación de ejercicios
- Tasa de éxito en compilación
- Calidad de ejercicios generados
- Diversidad de versiones

#### Paso 2: Identificar cuellos de botella
- Pasos que toman más tiempo
- Errores más frecuentes
- Áreas de mejora

#### Paso 3: Optimizar agentes y workflows
- Simplificar pasos innecesarios
- Mejorar prompts de agentes
- Automatizar validaciones
- Actualizar documentación

---

## 📊 MÉTRICAS Y MONITOREO

### Métricas Clave

#### 1. Eficiencia de Generación
- **Tiempo promedio**: Minutos por ejercicio generado
- **Tasa de éxito**: % de ejercicios que compilan correctamente
- **Iteraciones necesarias**: Promedio de correcciones requeridas

#### 2. Calidad de Ejercicios
- **Diversidad**: Número de versiones únicas generadas
- **Fidelidad visual**: % de similitud con imagen original (gráficas)
- **Completitud**: % de ejercicios con metadatos ICFES completos
- **Compilación**: % de éxito en HTML, PDF, Moodle

#### 3. Productividad
- **Ejercicios por día**: Cantidad generada
- **Ejercicios validados**: Cantidad que pasa todas las validaciones
- **Tiempo ahorrado**: Comparado con proceso manual

### Sistema de Logging

Crear `Auxiliares/Agentes-IA/scripts/log-actividad.sh`:


```bash
#!/bin/bash

# Sistema de logging para actividades de agentes
LOG_DIR="Auxiliares/Agentes-IA/logs"
LOG_FILE="$LOG_DIR/actividad-$(date +%Y-%m-%d).log"

# Crear directorio si no existe
mkdir -p "$LOG_DIR"

# Función de logging
log_actividad() {
    local TIPO=$1
    local AGENTE=$2
    local ACCION=$3
    local RESULTADO=$4

    TIMESTAMP=$(date +"%Y-%m-%d %H:%M:%S")
    echo "[$TIMESTAMP] [$TIPO] [$AGENTE] $ACCION - $RESULTADO" >> "$LOG_FILE"
}

# Exportar función para uso en otros scripts
export -f log_actividad
```

---

## 🔗 RECURSOS ADICIONALES

### Documentación del Proyecto
- **Reglas generales**: `.augment/rules/reglas-generales.md`
- **Ejemplos funcionales**: `A-Produccion/Ejemplos-Funcionales-Rmd/`
- **Biblioteca de soluciones**: `Auxiliares/BIBLIOTECA_Soluciones_Errores_Comunes.md`
- **Checklist de validación**: `Auxiliares/CHECKLIST_Validacion_Archivos_Rmd.md`

### Herramientas Externas
- **R-exams**: https://www.r-exams.org/
- **TikZ**: https://tikz.dev/
- **Matplotlib**: https://matplotlib.org/
- **Reticulate**: https://rstudio.github.io/reticulate/

### Comunidad y Soporte
- **GitHub del proyecto**: https://github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado
- **Documentación ICFES**: https://www.icfes.gov.co/

---

## 📝 PRÓXIMOS PASOS

### Inmediatos (Esta semana)
1. ✅ Crear estructura de directorios para agentes
2. ✅ Configurar primer agente (Generador de Ejercicios)
3. ✅ Probar con ejercicio simple
4. ✅ Validar y compilar resultado
5. ✅ Documentar aprendizajes

### Corto plazo (Próximas 2 semanas)
1. ⏳ Crear agentes especializados (Validador, TikZ, Metadatos)
2. ⏳ Implementar workflows automatizados
3. ⏳ Desarrollar scripts de validación y compilación
4. ⏳ Establecer sistema de logging y métricas

### Mediano plazo (Próximo mes)
1. ⏳ Optimizar agentes basado en métricas
2. ⏳ Expandir biblioteca de templates
3. ⏳ Automatizar validación masiva
4. ⏳ Integrar con sistema de control de calidad

### Largo plazo (Próximos 3 meses)
1. ⏳ Sistema completamente automatizado de generación
2. ⏳ Integración con plataformas educativas
3. ⏳ Banco de ejercicios validados (1000+)
4. ⏳ Documentación completa y actualizada

---

## 🎓 CONCLUSIÓN

Los **agentes de IA y workflows automatizados** son herramientas poderosas que pueden:


✅ **Multiplicar tu productividad** en la generación de ejercicios
✅ **Garantizar calidad consistente** mediante validaciones automáticas
✅ **Reducir errores** al seguir procesos estandarizados
✅ **Escalar el proyecto** de manera sostenible
✅ **Liberar tiempo** para tareas de mayor valor

### Claves del Éxito

1. **Empezar simple**: Probar con un agente básico antes de expandir
2. **Iterar constantemente**: Mejorar basado en resultados reales
3. **Documentar todo**: Mantener registro de decisiones y aprendizajes
4. **Medir resultados**: Usar métricas para guiar optimizaciones
5. **Mantener flexibilidad**: Adaptar agentes según necesidades cambiantes

### Recuerda

> "Los agentes no reemplazan tu expertise, la amplifican. Tú defines la estrategia, los agentes ejecutan con precisión y consistencia."

---

**¡Comienza hoy mismo a crear tus agentes personalizados y transforma tu flujo de trabajo!** 🚀

---

## 📞 SOPORTE

Para preguntas o problemas:

1. Consultar esta guía completa
2. Revisar ejemplos funcionales en `/A-Produccion/Ejemplos-Funcionales-Rmd/`
3. Consultar biblioteca de soluciones
4. Documentar nuevos casos en el proyecto

---

**Versión**: 1.0.0
**Fecha**: 2025-11-06
**Autor**: Sistema ICFES R-Exams
**Ubicación**: `Auxiliares/Agentes-IA/01-GUIA_COMPLETA_Agentes_Workflows_ICFES_R_Exams.md`

