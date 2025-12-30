---
name: analizar-icfes
description: >
  Analiza ejercicios matemáticos tipo ICFES según 6 dimensiones oficiales
  (dificultad, competencia, componente, pensamiento, contenido, eje).
  Usa cuando tengas imagen de problema ICFES, pregunta matemática para clasificar,
  o necesites decidir si requiere gráficos complejos. Detecta automáticamente
  si el ejercicio necesita Graficador Experto para replicación visual.

allowed-tools:
  - Read
  - Grep
  - Glob
  - Bash(find:*)
  - Bash(cat:*)
  - Bash(ls:*)
---

# Análisis ICFES de Ejercicios Matemáticos

## 🎯 Propósito de este Skill

Este skill es el **punto de entrada principal** del workflow de generación de ejercicios .Rmd. Analiza imágenes o descripciones de problemas matemáticos tipo ICFES según **6 dimensiones oficiales** y determina la ruta óptima de procesamiento.

### Cuándo usar este skill

**Triggers automáticos** (Claude lo usa cuando detecta):
- "Analiza este ejercicio ICFES"
- "Clasifica este problema matemático"
- "¿Qué nivel tiene este ejercicio?"
- Imagen de problema matemático adjunta
- Mención de competencias/componentes ICFES

**Invocación manual:**
```
/analizar-icfes imagen_ejercicio.png
```

## 📋 Las 6 Dimensiones ICFES

### Dimensión 1: Nivel de Dificultad (1-4)

**Escala oficial ICFES:**

| Nivel | Puntos | Descripción | Características |
|-------|--------|-------------|-----------------|
| **1** | 0-35 pts | Básico | Aplicación directa de fórmulas, un solo paso |
| **2** | 36-50 pts | Intermedio | Requiere 2-3 pasos, interpretación |
| **3** | 51-70 pts | Avanzado | Múltiples pasos, razonamiento complejo |
| **4** | 71-100 pts | Superior | Síntesis, análisis profundo, creatividad |

**Criterios de clasificación:**
- **Número de pasos**: ¿Cuántas operaciones requiere?
- **Abstracción**: ¿Requiere modelado matemático?
- **Interpretación**: ¿El enunciado es directo o requiere inferencias?

**Ejemplo:**
```
Nivel 1: "Calcula el área de un cuadrado de lado 5 cm"
Nivel 3: "Una piscina rectangular de 8m×6m se llena con manguera
          que vierte 2L/min. ¿Cuánto tarda en llenarse a 1.5m altura?"
```

Ver detalles en: @DIMENSIONES.md (sección Nivel de Dificultad)

### Dimensión 2: Competencia (3 categorías)

**Distribución oficial ICFES 2025:**

1. **Interpretación y Representación** (34%)
   - Leer gráficos, tablas, diagramas
   - Traducir entre representaciones
   - Identificar información relevante

2. **Formulación y Ejecución** (43%)
   - Plantear ecuaciones/modelos
   - Ejecutar procedimientos
   - Aplicar algoritmos

3. **Argumentación** (23%)
   - Justificar procedimientos
   - Validar argumentos
   - Demostrar proposiciones

**Ejemplo de clasificación:**
```
"Observa la gráfica y determina el valor máximo"
→ Interpretación y Representación

"Plantea la ecuación que modela la situación y resuélvela"
→ Formulación y Ejecución

"Explica por qué el método usado es correcto"
→ Argumentación
```

Ver ejemplos completos en: @DIMENSIONES.md (sección Competencias)

### Dimensión 3: Componente (3 categorías)

**Según estructura curricular ICFES:**

1. **Numérico-Variacional**
   - Números y operaciones
   - Patrones y regularidades
   - Álgebra y funciones

2. **Geométrico-Métrico**
   - Formas y figuras
   - Medidas y magnitudes
   - Transformaciones

3. **Aleatorio**
   - Datos y estadística
   - Probabilidad
   - Análisis de información

**Criterios de decisión:**
- ¿Trabaja con números/ecuaciones? → Numérico-Variacional
- ¿Involucra figuras/medidas? → Geométrico-Métrico
- ¿Analiza datos/probabilidades? → Aleatorio

Ver tabla de decisión en: @DIMENSIONES.md (sección Componentes)

### Dimensión 4: Tipo de Pensamiento (5 categorías)

Clasifica según el tipo de razonamiento predominante:

1. **Pensamiento Numérico**: Operaciones, propiedades numéricas
2. **Pensamiento Espacial**: Visualización, geometría
3. **Pensamiento Métrico**: Medidas, estimaciones
4. **Pensamiento Variacional**: Cambios, relaciones, funciones
5. **Pensamiento Aleatorio**: Incertidumbre, datos, probabilidad

**Puede haber combinaciones**:
- Problema de geometría analítica → Espacial + Variacional
- Estadística descriptiva → Aleatorio + Numérico

Ver matriz de combinaciones en: @DIMENSIONES.md (sección Pensamiento)

### Dimensión 5: Contenido Curricular

**Clasificación jerárquica:**

```
Álgebra y Cálculo
├── Genéricos (ecuaciones, funciones básicas)
└── No Genéricos (derivadas, límites, series)

Geometría
├── Genéricos (áreas, perímetros, teoremas básicos)
└── No Genéricos (geometría analítica, transformaciones)

Estadística
├── Descriptiva (medidas de tendencia, gráficos)
└── Inferencial (probabilidad, distribuciones)
```

**Regla de oro:**
- Contenido de grados 6-9 → Genérico
- Contenido de grados 10-11 → No Genérico

Ver taxonomía completa en: @DIMENSIONES.md (sección Contenido)

### Dimensión 6: Eje Axial Disciplinar

**Dos categorías:**

1. **Puramente Matemático**
   - Problema abstracto
   - Sin contexto real
   - Ejemplo: "Resuelve x² - 5x + 6 = 0"

2. **Aplicado/Contextualizado**
   - Situación del mundo real
   - Requiere modelado
   - Ejemplo: "Una empresa vende x productos a precio p(x) = 100 - 2x..."

**Criterio simple:**
- ¿Hay contexto narrativo? → Aplicado
- ¿Solo matemática pura? → Puramente Matemático

## 🔍 Proceso de Análisis Paso a Paso

### PASO 1: Lectura y Comprensión Inicial

**Si es imagen:**
1. Leer el enunciado completo
2. Identificar datos numéricos
3. Detectar gráficos, tablas, diagramas
4. Extraer la pregunta principal

**Si es texto:**
1. Identificar el problema matemático
2. Extraer variables y relaciones
3. Determinar qué se pregunta

**Output esperado:**
```markdown
## Comprensión Inicial
- **Enunciado**: [resumen breve]
- **Datos**: [lista de datos numéricos]
- **Pregunta**: [qué se pide calcular/demostrar]
- **Gráficos**: [SÍ/NO - descripción si aplica]
```

### PASO 2: Clasificación por las 6 Dimensiones

**Usar la plantilla:**

```markdown
## Análisis ICFES Completo

### 1. Nivel de Dificultad
- **Nivel**: [1|2|3|4]
- **Justificación**: [por qué este nivel]
- **Puntos estimados**: [0-100]

### 2. Competencia
- **Competencia**: [Interpretación|Formulación|Argumentación]
- **Justificación**: [qué hace principalmente]

### 3. Componente
- **Componente**: [Numérico-Variacional|Geométrico-Métrico|Aleatorio]
- **Justificación**: [área matemática principal]

### 4. Tipo de Pensamiento
- **Pensamiento(s)**: [lista de tipos aplicables]
- **Predominante**: [el principal]

### 5. Contenido Curricular
- **Área**: [Álgebra|Geometría|Estadística]
- **Tipo**: [Genérico|No Genérico]
- **Tema específico**: [ej: "Ecuaciones cuadráticas"]

### 6. Eje Axial
- **Eje**: [Puramente Matemático|Aplicado/Contextualizado]
- **Contexto**: [descripción si es aplicado]
```

Ver plantilla completa en: @PLANTILLA_ANALISIS.md

### PASO 3: Decisión de Flujo de Trabajo

**Criterio crítico: ¿Requiere Graficador Experto?**

**ACTIVAR Graficador Experto (Flujo B) SI:**
- ✓ Hay gráficos matemáticos complejos en la imagen
- ✓ Requiere generar diagramas geométricos precisos
- ✓ Necesita gráficos estadísticos personalizados
- ✓ Debe replicar figuras con medidas exactas

**NO activar (Flujo A estándar) SI:**
- ✗ Solo texto sin gráficos
- ✗ Gráficos simples (se pueden describir)
- ✗ Imágenes decorativas no matemáticas

**Output de decisión:**
```markdown
## Decisión de Flujo

**Flujo seleccionado**: [A: Estándar | B: Con Graficador]

**Razón**: [justificación de la decisión]

**Siguiente paso**:
- Flujo A → Ir a /generar-schoice o /generar-cloze
- Flujo B → Activar Graficador Experto primero
```

### PASO 4: Generar Metadatos R/exams

**Traducir el análisis a metadatos obligatorios:**

```yaml
exname: [Nombre descriptivo del ejercicio]
extype: [schoice|cloze]
exsolution: [Respuesta correcta]
exshuffle: TRUE
extol: 0.01

# Metadatos ICFES (derivados del análisis)
exextra[Type]: [SCHOICE|CLOZE]
exextra[Competencia]: [resultado Dimensión 2]
exextra[Componente]: [resultado Dimensión 3]
exextra[Afirmacion]: [descripción específica basada en Dimensión 5]
exextra[Evidencia]: [descripción específica basada en Dimensión 2]
exextra[Nivel]: [resultado Dimensión 1]
```

**Ejemplo completo:**
```yaml
exname: Ecuación Cuadrática Aplicada Tiempo Llenado
extype: schoice
exsolution: 01000
exshuffle: TRUE
extol: 0.01

exextra[Type]: SCHOICE
exextra[Competencia]: Formulación y Ejecución
exextra[Componente]: Numérico-Variacional
exextra[Afirmacion]: Plantea y resuelve ecuaciones cuadráticas en contextos reales
exextra[Evidencia]: Formula modelos matemáticos a partir de situaciones problema
exextra[Nivel]: 3
```

Ver especificaciones completas en: @.claude/rules/codigo-rmd.md

## 🎓 Ejemplos Completos de Análisis

### Ejemplo 1: Problema Nivel 2 - Geometría

**Enunciado:**
> "Un triángulo rectángulo tiene catetos de 3 cm y 4 cm. ¿Cuál es la longitud de la hipotenusa?"

**Análisis:**
```markdown
### Dimensión 1: Nivel de Dificultad
- Nivel: 2 (Intermedio)
- Justificación: Aplicación directa del teorema de Pitágoras (2 pasos)
- Puntos: 40

### Dimensión 2: Competencia
- Competencia: Formulación y Ejecución
- Justificación: Debe aplicar fórmula y calcular

### Dimensión 3: Componente
- Componente: Geométrico-Métrico
- Justificación: Figuras geométricas y medidas

### Dimensión 4: Pensamiento
- Pensamiento: Espacial + Métrico
- Predominante: Espacial

### Dimensión 5: Contenido
- Área: Geometría
- Tipo: Genérico
- Tema: Teorema de Pitágoras

### Dimensión 6: Eje
- Eje: Puramente Matemático
- Sin contexto aplicado

### Decisión de Flujo
- Flujo: A (Estándar - no requiere gráficos complejos)
- Siguiente: /generar-schoice
```

Ver más ejemplos en: @EJEMPLOS.md

### Ejemplo 2: Problema Nivel 3 - Estadística con Gráfico

**Enunciado:**
> [Imagen con gráfico de barras de ventas mensuales]
> "Según el gráfico, ¿en qué mes hubo el mayor incremento porcentual respecto al mes anterior?"

**Análisis:**
```markdown
### Dimensión 1: Nivel 3 (requiere cálculo de variaciones porcentuales)
### Dimensión 2: Interpretación y Representación
### Dimensión 3: Aleatorio
### Dimensión 4: Aleatorio + Variacional
### Dimensión 5: Estadística Descriptiva (Genérico)
### Dimensión 6: Aplicado (contexto de ventas)

### Decisión de Flujo
- Flujo: B (Graficador Experto)
- Razón: Gráfico de barras necesita replicación exacta
- Siguiente: Activar Graficador → Generar código R/ggplot2
```

## ⚠️ Errores Comunes a Evitar

### Error 1: Confundir Competencia con Componente

**❌ Incorrecto:**
- Competencia: "Geométrico" (esto es un Componente)

**✓ Correcto:**
- Competencia: "Interpretación y Representación"
- Componente: "Geométrico-Métrico"

### Error 2: Nivel muy bajo/alto

**Criterio objetivo:**
- Cuenta los pasos matemáticos necesarios
- Si son 1-2 pasos directos → Nivel 1-2
- Si son 3+ pasos con razonamiento → Nivel 3-4

### Error 3: No detectar necesidad de Graficador

**Regla simple:**
- ¿El gráfico tiene valores numéricos específicos? → SÍ usar Graficador
- ¿Es solo ilustrativo/decorativo? → NO usar Graficador

## 🔗 Referencias y Documentación

### Archivos de Referencia
- @DIMENSIONES.md - Especificación detallada de las 6 dimensiones
- @EJEMPLOS.md - Colección de ejercicios analizados
- @PLANTILLA_ANALISIS.md - Plantilla para análisis consistente

### Reglas del Proyecto
- @.claude/rules/codigo-rmd.md - Metadatos requeridos
- @.claude/rules/ciclo-validacion.md - Qué hacer después del análisis

### Documentación Oficial ICFES
- @.claude/docs/WORKFLOW_PASO_A_PASO.md - Flujo completo
- @.claude/Mermaid_Chart.txt - Diagrama visual del workflow

## 📊 Output Final Esperado

Después de usar este skill, debes tener:

```markdown
# Análisis ICFES Completo: [Nombre del Ejercicio]

## 1. Clasificación por Dimensiones
[Tabla con las 6 dimensiones clasificadas]

## 2. Metadatos R/exams Generados
[Bloque YAML con metadatos]

## 3. Decisión de Flujo
[Flujo A o B, con justificación]

## 4. Siguiente Acción Recomendada
[Comando específico a ejecutar]
```

## 🚀 Integración con Otros Skills

Este skill es el **entry point** que activa otros skills:

```
analizar-icfes
    ↓
    ├─→ [Flujo A] generar-schoice / generar-cloze
    │       ↓
    │   validar-renderizado → validar-coherencia
    │       ↓
    │   promover-ejercicio
    │
    └─→ [Flujo B] analizar-imagen-grafica (Graficador)
            ↓
        generar-codigo-r/python/tikz
            ↓
        comparar-similitud-visual
            ↓
        [volver a Flujo A]
```

---

**Última actualización**: 2025-12-30
**Versión**: 2.0 (Progressive Disclosure)
**Basado en**: Documentación oficial ICFES 2025 + Claude Code best practices
