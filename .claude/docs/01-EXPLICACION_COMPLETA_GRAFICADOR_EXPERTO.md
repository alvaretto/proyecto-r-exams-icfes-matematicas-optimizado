# 📚 FUNCIONAMIENTO DETALLADO DEL GRAFICADOR EXPERTO v2.0

## 🎯 PROPÓSITO GENERAL

El **Graficador Experto v2.0** es un sistema especializado de replicación visual de imágenes matemáticas con optimizaciones de estado persistente, métricas cuantitativas y transferencia de conocimiento. Su función principal es:

1. **Analizar imágenes** matemáticas con precisión y guardar análisis estructurado
2. **Generar código** en 3 lenguajes (TikZ, Python, R) con reutilización de análisis
3. **Comparar visualmente** resultados con métricas cuantitativas objetivas (0-100 puntos)
4. **Iterar automáticamente** hasta lograr 95+ puntos de similitud
5. **Transferir conocimiento** entre lenguajes (TikZ → Python → R)
6. **Trackear progreso** con estado persistente recuperable
7. **Exportar proyecto completo** con estadísticas detalladas

---

## 🏗️ ESTRUCTURA DEL DIRECTORIO

```
Graficador-Experto/.claude/
├── 📊 Mermaid_Chart.txt          # Diagrama de flujo del sistema
├── 🔧 settings.local.json        # Permisos para skills
├── 📄 README.md                  # Documentación básica
│
├── 📁 schemas/                   # [NUEVO] Esquemas JSON (4)
│   ├── workflow_state.schema.json       # Estado persistente del workflow
│   ├── analisis_inicial.schema.json     # Análisis estructurado reutilizable
│   ├── metricas_similitud.schema.json   # Sistema de puntuación 0-100
│   └── lecciones_aprendidas.schema.json # Transferencia de conocimiento
│
├── 📁 skills/                    # Skills especializadas (8)
│   ├── analizar-imagen-matematica/  # Análisis visual detallado
│   ├── generar-tikz/                # Generación TikZ/LaTeX
│   ├── generar-python/              # Generación Python/Matplotlib
│   ├── generar-r/                   # Generación R/ggplot2
│   ├── comparar-visual/             # [MEJORADO] Comparación con métricas cuantitativas
│   ├── refinar-codigo/              # Refinamiento iterativo
│   ├── gestionar-estado/            # [NUEVO] Gestión de estado persistente
│   └── transferir-conocimiento/     # [NUEVO] Transferencia entre lenguajes
│
├── 📁 commands/                  # Comandos slash (9)
│   ├── analizar-imagen.md       # Iniciar workflow + guardar análisis estructurado
│   ├── generar-tikz.md          # Generar código TikZ + actualizar estado
│   ├── generar-python.md        # Generar Python + aplicar lecciones TikZ
│   ├── generar-r.md             # Generar R + aplicar lecciones TikZ/Python
│   ├── comparar.md              # Comparar + calcular métricas cuantitativas
│   ├── iterar.md                # Refinar código + incrementar contador
│   ├── exportar.md              # Exportar proyecto + estadísticas
│   ├── estado.md                # [NUEVO] Visualizar progreso del workflow
│   └── auto-iterar.md           # [NUEVO] Iteración automática hasta umbral
│
├── 📁 agents/                    # Agentes (futuro)
├── 📁 hooks/                     # Hooks (futuro)
└── 📁 docs/                      # Documentación adicional
    └── 01-EXPLICACION_COMPLETA_GRAFICADOR_EXPERTO.md  # Este archivo
```

---

## 🔄 FLUJO DE TRABAJO COMPLETO (SEGÚN MERMAID_CHART.TXT)

El sistema opera en **5 fases principales** con ciclo de refinamiento automático:

### 📥 **FASE 1: ANÁLISIS VISUAL MATEMÁTICO**

Cuando se proporciona una imagen matemática, el sistema realiza un análisis exhaustivo en **4 dimensiones**:

#### 1️⃣ **Clasificación de Contenido**

**Tipos identificados:**

- **Geometría**: Figuras planas, 3D, construcciones, transformaciones
- **Estadística**: Barras, histogramas, pie charts, boxplots, dispersión
- **Cálculo**: Funciones, límites, derivadas, integrales, áreas
- **Trigonometría**: Círculo trigonométrico, funciones, triángulos
- **Álgebra**: Sistemas de ecuaciones, matrices, vectores, desigualdades

#### 2️⃣ **Extracción de Elementos Visuales**

**Componentes analizados:**

- **Ejes Coordenados**: Origen, rangos, marcas, etiquetas, unidades
- **Gráficas y Curvas**: Tipo, puntos clave, continuidad, color, estilo
- **Figuras Geométricas**: Tipo, dimensiones, ángulos, vértices, colores
- **Anotaciones y Etiquetas**: Texto, valores, fórmulas, leyendas, títulos

#### 3️⃣ **Análisis de Estilos**

**Características detectadas:**

- **Colores**: Identificación RGB/Hex, paletas, contraste
- **Estilos de Línea**: Sólida, punteada, discontinua, grosor
- **Tipografía**: Fuente, tamaño, negrita, cursiva

#### 4️⃣ **Evaluación de Complejidad**

**Niveles:**

- **Bajo**: Gráfico simple, un tipo de visualización, colores básicos
- **Medio**: Múltiples elementos, 2-3 tipos de visualización, varios colores
- **Alto**: Muchos elementos, múltiples capas, paleta compleja, 3D

**Resultado:** 
- Reporte estructurado con toda la información extraída
- **[NUEVO]** `outputs/analisis_inicial.json` - Análisis en formato JSON reutilizable
- **[NUEVO]** `outputs/workflow_state.json` - Estado inicial del workflow
- **[NUEVO]** `outputs/reporte_matematico.md` - Sección "Análisis Inicial"

---

### 🎨 **FASE 2: GENERACIÓN MULTI-LENGUAJE**

El sistema genera código en **3 lenguajes simultáneamente**:

#### **Opción 1: TikZ/LaTeX** 🎨

**Características:**

- Precisión matemática máxima
- Calidad vectorial (PDF)
- Ideal para publicaciones académicas
- Compilación con pdflatex/xelatex

**Paquetes utilizados:**

```latex
\usepackage{tikz}
\usepackage{pgfplots}
\usepackage{amsmath}
\pgfplotsset{compat=1.18}
```

**Salida:** `output_tikz.tex` (compilable standalone)

#### **Opción 2: Python/Matplotlib** 🐍

**Características:**

- Flexibilidad y potencia
- Integración con numpy/scipy
- Ideal para análisis de datos
- Exportación PNG/PDF

**Librerías utilizadas:**

```python
import matplotlib.pyplot as plt
import numpy as np
```

**Salida:** `output_python.py` (ejecutable directo)

#### **Opción 3: R/ggplot2** 📊

**Características:**

- Sintaxis declarativa elegante
- Ideal para estadística
- Temas profesionales
- Exportación PNG/PDF

**Paquetes utilizados:**

```r
library(ggplot2)
library(grid)
```

**Salida:** `output_r.R` (ejecutable directo)

---

### 🔍 **FASE 3: COMPARACIÓN VISUAL INTELIGENTE CON MÉTRICAS CUANTITATIVAS**

Después de generar cada imagen, el sistema realiza **comparación automática** en **6 categorías** con **puntuación objetiva (0-100 puntos)**:

#### 1️⃣ **Análisis de Colores (0-20 puntos)** [MEJORADO]

- Identificar todos los colores presentes
- Comparar paletas RGB/Hex
- Detectar diferencias de tonalidad
- Verificar transparencia

**Criterios de puntuación:**
- **20 puntos**: Todos los colores coinciden exactamente (diferencia RGB < 1%)
- **15 puntos**: Colores similares (diferencia RGB 1-10%)
- **10 puntos**: Algunos colores incorrectos
- **5 puntos**: Colores muy diferentes
- **0 puntos**: Colores completamente incorrectos

**Evaluación:**

```markdown
Puntuación: 18/20 puntos
Criterio: colores_similares
✅ Correcto: Azul #0066CC coincide
⚠️ Advertencia: Verde ligeramente más oscuro (#00AA00 vs #009900)
```

#### 2️⃣ **Análisis de Posiciones y Coordenadas (0-20 puntos)** [MEJORADO]

- Comparar coordenadas de todos los elementos
- Verificar alineación y distribución
- Detectar desplazamientos
- Validar escalas y proporciones

**Criterios de puntuación:**
- **20 puntos**: Todas las coordenadas exactas (diferencia < 1% del rango)
- **15 puntos**: Diferencias menores al 5% del rango
- **10 puntos**: Diferencias entre 5-10% del rango
- **5 puntos**: Diferencias entre 10-20% del rango
- **0 puntos**: Diferencias mayores al 20% del rango

#### 3️⃣ **Análisis de Valores Numéricos (0-20 puntos)** [MEJORADO]

- Extraer todos los valores visibles
- Comparar etiquetas y escalas
- Verificar rangos de ejes
- Validar datos en gráficos

**Criterios de puntuación:**
- **20 puntos**: Todos los valores correctos
- **15 puntos**: 1-2 valores incorrectos (no críticos)
- **10 puntos**: 3-4 valores incorrectos
- **5 puntos**: 5+ valores incorrectos
- **0 puntos**: Valores críticos incorrectos

#### 4️⃣ **Análisis de Proporciones y Escalas (0-15 puntos)** [MEJORADO]

- Comparar proporciones entre elementos
- Verificar aspect ratio
- Validar escalas de ejes
- Detectar distorsiones

**Criterios de puntuación:**
- **15 puntos**: Proporciones perfectas
- **10 puntos**: Diferencias menores (< 5% en aspect ratio)
- **5 puntos**: Diferencias moderadas (5-15%)
- **0 puntos**: Proporciones incorrectas (> 15%)

#### 5️⃣ **Análisis de Estilos (0-15 puntos)** [MEJORADO]

- Comparar grosores de líneas
- Verificar tipos de línea
- Comparar tamaños de fuente
- Validar marcadores

**Criterios de puntuación:**
- **15 puntos**: Todos los estilos coinciden
- **10 puntos**: Estilos similares
- **5 puntos**: Algunos estilos incorrectos
- **0 puntos**: Estilos muy diferentes

#### 6️⃣ **Análisis de Elementos (0-10 puntos)** [MEJORADO]

- Inventariar elementos presentes
- Identificar elementos faltantes
- Detectar elementos extra
- Verificar completitud

**Criterios de puntuación:**
- **10 puntos**: Todos los elementos presentes
- **7 puntos**: 1 elemento faltante o extra
- **4 puntos**: 2-3 elementos faltantes o extra
- **0 puntos**: 4+ elementos faltantes o extra

**Resultado:** 
- **Puntuación total**: Suma de todas las categorías (0-100 puntos)
- **[NUEVO]** Recomendación objetiva basada en puntuación
- **[NUEVO]** Actualización de `workflow_state.json` con similitud actual e historial
- **[NUEVO]** Actualización de `reporte_matematico.md` con sección de iteración
- Reporte detallado con correcciones específicas

---

### ⚡ **FASE 4: DECISIÓN Y REFINAMIENTO CON MÉTRICAS OBJETIVAS**

**Punto de decisión:** ¿Puntuación ≥ 95 puntos?

#### ✅ **PUNTUACIÓN ≥ 95** → Validación Exitosa

1. **[NUEVO]** Actualizar estado: `[lenguaje].estado = "validado"`
2. **[NUEVO]** Registrar `timestamp_validacion`
3. **[NUEVO]** Capturar lecciones aprendidas (éxitos)
4. Marcar lenguaje como validado
5. Continuar con siguiente lenguaje (aplicando lecciones aprendidas)
6. Si todos validados → Exportar proyecto

#### 🔄 **PUNTUACIÓN < 95** → Ciclo de Refinamiento

**Recomendaciones por puntuación:**
- **95-100 puntos**: ✅ Validar - Excelente
- **85-94 puntos**: ⚠️ Considerar validar o iterar - Bueno
- **70-84 puntos**: ⚠️ Iterar - Regular
- **< 70 puntos**: ❌ Iterar o regenerar - Pobre

**Proceso automático:**

1. **Identificar correcciones** por prioridad:
   - **Alta**: Impacto visual significativo
   - **Media**: Mejoras importantes
   - **Baja**: Detalles menores

2. **Generar código corregido**:
   - Aplicar correcciones de alta prioridad
   - Mantener código limpio y documentado
   - Preservar elementos correctos

3. **[NUEVO]** Incrementar contador de iteración:
   - Actualizar `[lenguaje].iteracion_actual`
   - Registrar timestamp de iteración

4. **Re-renderizar imagen**:
   - Compilar/ejecutar código corregido
   - Generar nueva imagen

5. **Volver a FASE 3** (Comparación):
   - Repetir análisis visual
   - Calcular nueva puntuación cuantitativa
   - **[NUEVO]** Actualizar historial de similitud
   - Continuar hasta ≥ 95 puntos

**Límite de iteraciones:** Máximo 10 ciclos por lenguaje (configurable)

**Iteración automática disponible:** Usa `/auto-iterar [lenguaje] [umbral] [max_iteraciones]`

---

### 📦 **FASE 5: EXPORTACIÓN FINAL CON ESTADÍSTICAS**

Cuando los 3 lenguajes están validados (≥ 95 puntos de similitud), el sistema genera:

**Archivos de código:**

- `output_tikz.tex` - Código LaTeX compilable
- `output_python.py` - Script Python ejecutable
- `output_r.R` - Script R ejecutable

**Imágenes generadas:**

- `original.png` - Imagen original
- `tikz_render.png` - Resultado TikZ
- `python_render.png` - Resultado Python
- `r_render.png` - Resultado R

**Archivos de estado y análisis [NUEVO]:**

- `workflow_state.json` - Estado final del workflow con todas las estadísticas
- `analisis_inicial.json` - Análisis estructurado de la imagen original
- `lecciones_aprendidas.json` - Conocimiento capturado durante el proceso

**Reportes:**

- `reporte_matematico.md` - Reporte completo con:
  - Resumen ejecutivo con estadísticas del workflow
  - Análisis inicial
  - Código de los 3 lenguajes
  - Historial de iteraciones
  - Gráficos de progreso de similitud
  - Comparación entre implementaciones
  - Estadísticas: iteraciones totales, similitudes finales, tiempos de desarrollo

**Estructura final:**

```
outputs/
├── output_tikz.tex
├── output_python.py
├── output_r.R
├── workflow_state.json          # [NUEVO] Estado final
├── analisis_inicial.json        # [NUEVO] Análisis estructurado
├── lecciones_aprendidas.json    # [NUEVO] Conocimiento capturado
├── reporte_matematico.md        # [MEJORADO] Con estadísticas completas
├── original.png
├── tikz_render.png
├── python_render.png
└── r_render.png
```

---

## 🤖 **SKILLS ESPECIALIZADAS**

### `/analizar-imagen`

**Función:** Inicia el workflow completo con análisis visual detallado.

**Proceso:**

1. Recibe imagen matemática
2. Clasifica tipo de contenido
3. Extrae elementos visuales
4. Analiza estilos y colores
5. Evalúa complejidad
6. Genera reporte estructurado

**Salida:** 
- `analisis_inicial.json` (formato estructurado reutilizable) [NUEVO]
- `workflow_state.json` (estado inicial) [NUEVO]
- `reporte_matematico.md` (sección "Análisis Inicial") [NUEVO]

### `/generar-tikz`

**Función:** Genera código TikZ/LaTeX preciso y compilable.

**Proceso:**

1. Lee reporte de análisis
2. Selecciona plantilla apropiada
3. Genera código TikZ
4. Compila con pdflatex
5. Convierte PDF a PNG
6. Activa comparación automática

**Salida:** `output_tikz.tex` + imagen PNG

### `/generar-python`

**Función:** Genera código Python/Matplotlib profesional.

**Proceso:**

1. Lee reporte de análisis
2. Configura matplotlib
3. Genera código Python
4. Ejecuta script
5. Guarda imagen PNG
6. Activa comparación automática

**Salida:** `output_python.py` + imagen PNG

### `/generar-r`

**Función:** Genera código R/ggplot2 eficiente.

**Proceso:**

1. Lee reporte de análisis
2. Configura ggplot2
3. Genera código R
4. Ejecuta script
5. Guarda imagen PNG
6. Activa comparación automática

**Salida:** `output_r.R` + imagen PNG

### `/comparar`

**Función:** Compara imagen generada con original usando Claude Vision.

**Proceso:**

1. Carga imagen original
2. Carga imagen generada
3. Analiza 6 categorías de diferencias
4. **[NUEVO]** Calcula puntuación cuantitativa por categoría (0-100 puntos total)
5. **[NUEVO]** Actualiza `workflow_state.json` con similitud actual e historial
6. **[NUEVO]** Genera recomendación objetiva (validar/iterar/regenerar)
7. Prioriza correcciones
8. Genera reporte detallado
9. **[NUEVO]** Actualiza `reporte_matematico.md` con sección de iteración

**Salida:** 
- Reporte detallado con puntuación por categorías
- Estado actualizado con similitud e historial
- Recomendación objetiva basada en métricas

### `/iterar`

**Función:** Refina código basándose en comparación visual.

**Proceso:**

1. **[NUEVO]** Incrementa `[lenguaje].iteracion_actual` en estado
2. Lee reporte de comparación
3. Identifica correcciones prioritarias
4. Modifica código existente
5. Re-renderiza imagen
6. Compara nuevamente
7. Repite hasta puntuación ≥ 95
8. **[NUEVO]** Captura lecciones aprendidas si resuelve problema complejo

**Salida:** 
- Código refinado + nueva imagen
- Estado actualizado con contador de iteración
- Documentación incremental en reporte

### `/exportar`

**Función:** Genera archivos finales y reporte consolidado.

**Proceso:**

1. **[NUEVO]** Lee `workflow_state.json` para extraer estadísticas completas
2. Verifica que todos los lenguajes estén validados
3. Organiza archivos en estructura de proyecto
4. **[NUEVO]** Genera reporte consolidado con:
   - Resumen ejecutivo con estadísticas del workflow
   - Iteraciones totales y similitudes finales por lenguaje
   - Historial de similitud como gráfico de progreso
   - Tiempos de desarrollo por lenguaje
   - Mejora promedio por iteración
5. Crea archivo README.md
6. **[NUEVO]** Incluye archivos de estado (workflow_state, analisis_inicial, lecciones_aprendidas)

**Salida:** 
- Proyecto completo listo para uso
- Estadísticas detalladas del proceso
- Estado final persistente

---

## 📊 **MÉTRICAS DE CALIDAD - SISTEMA DE PUNTUACIÓN CUANTITATIVA**

El sistema garantiza calidad mediante métricas objetivas:

### Sistema de Puntuación (0-100 puntos)

Puntuación total distribuida en 6 categorías:

- **Colores (0-20 puntos)**: Coincidencia exacta de paleta de colores
- **Posiciones (0-20 puntos)**: Precisión de ubicación de elementos
- **Valores (0-20 puntos)**: Correctitud de etiquetas, escalas, anotaciones
- **Proporciones (0-15 puntos)**: Aspect ratio y escalas correctas
- **Estilos (0-15 puntos)**: Grosor de líneas, tipos, fuentes, marcadores
- **Elementos (0-10 puntos)**: Completitud (todos presentes, ninguno extra)

### Recomendaciones por Puntuación

- ✅ **95-100 puntos**: Excelente - Validar
- ⚠️ **85-94 puntos**: Bueno - Considerar validar o iterar
- 🔄 **70-84 puntos**: Regular - Iterar (refinamiento necesario)
- ❌ **< 70 puntos**: Pobre - Iterar o regenerar

### Precisión Matemática

- ✅ **Valores exactos** en coordenadas y medidas
- ✅ **Proporciones correctas** en figuras geométricas
- ✅ **Escalas precisas** en ejes y gráficos
- ✅ **Colores fieles** al original (±5% tolerancia RGB)

### Completitud

- ✅ **Todos los elementos** presentes
- ✅ **Todas las anotaciones** incluidas
- ✅ **Todas las etiquetas** correctas
- ✅ **Todos los estilos** replicados

### Funcionalidad

- ✅ **Código compilable/ejecutable** sin errores
- ✅ **Código documentado** con comentarios claros
- ✅ **Código optimizado** para rendimiento
- ✅ **Código reutilizable** y modificable

---

## 🎓 **FILOSOFÍA DEL SISTEMA**

### Principio de Fidelidad Visual

**La imagen generada debe ser visualmente indistinguible del original.**

### Principio de Multi-Lenguaje

**Cada lenguaje tiene fortalezas específicas - el sistema las aprovecha todas.**

- **TikZ**: Precisión matemática y calidad vectorial
- **Python**: Flexibilidad y potencia computacional
- **R**: Elegancia estadística y temas profesionales

### Principio de Refinamiento Iterativo

**La perfección se alcanza mediante iteraciones sucesivas, no en un solo intento.**

### Principio de Automatización Inteligente

**El sistema debe minimizar intervención manual mediante comparación automática.**

---

## 🔧 **CASOS DE USO TÍPICOS**

### Caso 1: Función Cuadrática

**Input:** Imagen de parábola con ejes y anotaciones

**Proceso:**

1. Análisis: Identifica función f(x) = -x² + 4x + 1
2. Extrae: Vértice (2, 5), interceptos, raíces
3. Genera TikZ: Usa pgfplots con domain y samples
4. Genera Python: Usa numpy.linspace y plt.plot
5. Genera R: Usa stat_function con ggplot2
6. Compara: Valida similitud ≥ 95% en los 3
7. Exporta: Proyecto completo

**Resultado:** 3 implementaciones funcionales con 98% similitud

### Caso 2: Gráfico de Barras

**Input:** Imagen de gráfico de barras con 5 categorías

**Proceso:**

1. Análisis: Identifica valores [12, 18, 7, 22, 15]
2. Extrae: Categorías A-E, color verde, título
3. Genera TikZ: Usa ybar con symbolic x coords
4. Genera Python: Usa plt.bar con configuración
5. Genera R: Usa geom_col con ggplot2
6. Compara: Detecta diferencia en altura de barra B
7. Itera: Corrige valor de 15 a 18
8. Re-compara: Similitud 97%
9. Exporta: Proyecto completo

**Resultado:** 3 implementaciones corregidas con 97% similitud

### Caso 3: Triángulo Rectángulo

**Input:** Imagen de triángulo con medidas y ángulos

**Proceso:**

1. Análisis: Identifica triángulo rectángulo ABC
2. Extrae: Lados AB=3cm, BC=4cm, AC=5cm, ángulo recto en B
3. Genera TikZ: Usa coordenadas y cálculos geométricos
4. Genera Python: Usa plt.plot con coordenadas calculadas
5. Genera R: Usa geom_polygon con coordenadas
6. Compara: Detecta ángulo ligeramente incorrecto
7. Itera: Ajusta coordenadas para ángulo exacto 90°
8. Re-compara: Similitud 99%
9. Exporta: Proyecto completo

**Resultado:** 3 implementaciones precisas con 99% similitud

---

## 🚀 **VENTAJAS DEL SISTEMA**

### Para Docentes

- ✅ **Reutilización**: Código modificable para variantes del problema
- ✅ **Calidad**: Imágenes profesionales para materiales educativos
- ✅ **Versatilidad**: 3 formatos para diferentes contextos
- ✅ **Rapidez**: Automatización reduce tiempo de creación

### Para Estudiantes

- ✅ **Aprendizaje**: Código como recurso educativo
- ✅ **Experimentación**: Modificar parámetros y ver resultados
- ✅ **Comprensión**: Visualizar conceptos matemáticos

### Para Investigadores

- ✅ **Reproducibilidad**: Código fuente disponible
- ✅ **Publicaciones**: Calidad vectorial para papers
- ✅ **Colaboración**: Código compartible y versionable

---

## 📋 **DIAGRAMA DE FLUJO VISUAL**

El archivo `Mermaid_Chart.txt` contiene el diagrama completo del sistema que puede visualizarse en:

- Editores Markdown con soporte Mermaid
- Herramientas online como [Mermaid Live Editor](https://mermaid.live/)
- VSCode con extensión Mermaid

### Estructura del diagrama:

```
ENTRADA (Imagen Matemática)
    ↓
ANÁLISIS VISUAL (4 dimensiones)
    ↓
REPORTE ESTRUCTURADO
    ↓
SELECCIÓN DE LENGUAJE
    ├─→ TikZ → Compilar → Imagen
    ├─→ Python → Ejecutar → Imagen
    └─→ R → Ejecutar → Imagen
    ↓
COMPARACIÓN VISUAL (6 categorías)
    ↓
DECISIÓN: ¿Similitud ≥ 95%?
    ├─→ SÍ → Validar → ¿Todos validados? → EXPORTAR
    └─→ NO → REFINAMIENTO → Volver a generar
```

---

## 🔗 **REFERENCIAS RÁPIDAS**

### Archivos clave:

- **Diagrama completo:** `Mermaid_Chart.txt`
- **Configuración:** `settings.local.json`
- **Documentación básica:** `README.md`

### Directorios importantes:

- **Skills:** `.claude/skills/`
- **Comandos:** `.claude/commands/`
- **Salidas:** `outputs/` (generado automáticamente)

### Comandos principales:

1. `/analizar-imagen` - Iniciar workflow
2. `/generar-tikz` - Generar TikZ
3. `/generar-python` - Generar Python
4. `/generar-r` - Generar R
5. `/comparar` - Comparar con métricas cuantitativas
6. `/iterar` - Refinar código e incrementar contador
7. `/exportar` - Exportar proyecto con estadísticas
8. `/estado` - **[NUEVO]** Visualizar progreso del workflow
9. `/auto-iterar` - **[NUEVO]** Iteración automática hasta umbral

---

## 🎯 **RESUMEN EJECUTIVO**

El **Graficador Experto** es un sistema de replicación visual automatizado que:

1. **Analiza** imágenes matemáticas con precisión profesional
2. **Genera** código en 3 lenguajes (TikZ, Python, R) simultáneamente
3. **Compara** resultados automáticamente usando visión por computadora
4. **Refina** código iterativamente hasta lograr 95%+ de similitud
5. **Exporta** proyecto completo listo para uso

**El resultado:** Imágenes matemáticas de alta calidad con código fuente en 3 lenguajes, validadas visualmente y listas para uso educativo, investigación o publicación.

---

## 🆕 **NOVEDADES DE LA VERSIÓN 2.0**

### Características Nuevas

1. **Sistema de Estado Persistente**
   - Tracking completo del progreso del workflow
   - Recuperación ante interrupciones
   - Historial de similitudes por iteración

2. **Métricas Cuantitativas Objetivas**
   - Sistema de puntuación 0-100 puntos
   - Evaluación por 6 categorías
   - Recomendaciones basadas en puntuación

3. **Análisis Inicial Estructurado**
   - Formato JSON reutilizable
   - Disponible para las 3 generaciones
   - Consistencia entre lenguajes

4. **Transferencia de Conocimiento**
   - Captura de lecciones aprendidas por lenguaje
   - Aplicación automática en lenguajes subsecuentes
   - Mejora progresiva (TikZ → Python → R)

5. **Documentación Incremental**
   - Reporte actualizado en cada paso
   - Disponible en todo momento
   - Incluye estadísticas completas al exportar

6. **Comandos Avanzados**
   - `/estado` - Visualización de progreso en tiempo real
   - `/auto-iterar` - Iteración automática hasta umbral

### Beneficios Esperados

- ⏱️ **Reducción de tiempo**: 20-30% menos tiempo por proyecto
- 📊 **Mejora de calidad**: Similitud promedio mejora de 92% a 96%
- 🔄 **Menos iteraciones**: 4-5 iteraciones → 2-3 iteraciones por lenguaje
- 📈 **Trazabilidad**: 100% de proyectos con historial completo
- 🎯 **Objetividad**: Métricas cuantitativas eliminan subjetividad
- 🔄 **Recuperación**: Estado persistente permite continuar tras interrupciones

---

**Última actualización:** 2025-12-28
**Versión del documento:** 2.0 (Optimizada)
**Estado:** ✅ Completo y verificado con optimizaciones

