# 📚 FUNCIONAMIENTO DETALLADO DEL GRAFICADOR EXPERTO

## 🎯 PROPÓSITO GENERAL

El **Graficador Experto** es un sistema especializado de replicación visual de imágenes matemáticas. Su función principal es:

1. **Analizar imágenes** matemáticas con precisión
2. **Generar código** en 3 lenguajes (TikZ, Python, R)
3. **Comparar visualmente** resultados con el original
4. **Iterar automáticamente** hasta lograr 95%+ de similitud
5. **Exportar proyecto completo** con todos los archivos

---

## 🏗️ ESTRUCTURA DEL DIRECTORIO

```
Graficador-Experto/.claude/
├── 📊 Mermaid_Chart.txt          # Diagrama de flujo del sistema
├── 🔧 settings.local.json        # Permisos para skills
├── 📄 README.md                  # Documentación básica
│
├── 📁 skills/                    # Skills especializadas (6)
│   ├── analizar-imagen-matematica/  # Análisis visual detallado
│   ├── generar-tikz/                # Generación TikZ/LaTeX
│   ├── generar-python/              # Generación Python/Matplotlib
│   ├── generar-r/                   # Generación R/ggplot2
│   ├── comparar-visual/             # Comparación inteligente
│   └── refinar-codigo/              # Refinamiento iterativo
│
├── 📁 commands/                  # Comandos slash (7)
│   ├── analizar-imagen.md       # Iniciar workflow completo
│   ├── generar-tikz.md          # Generar código TikZ
│   ├── generar-python.md        # Generar código Python
│   ├── generar-r.md             # Generar código R
│   ├── comparar.md              # Comparar con original
│   ├── iterar.md                # Refinar código
│   └── exportar.md              # Exportar proyecto
│
├── 📁 agents/                    # Agentes (futuro)
└── 📁 hooks/                     # Hooks (futuro)
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

**Resultado:** Reporte estructurado con toda la información extraída.

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

### 🔍 **FASE 3: COMPARACIÓN VISUAL INTELIGENTE**

Después de generar cada imagen, el sistema realiza **comparación automática** en **6 categorías**:

#### 1️⃣ **Análisis de Colores**

- Identificar todos los colores presentes
- Comparar paletas RGB/Hex
- Detectar diferencias de tonalidad
- Verificar transparencia

**Evaluación:**

```markdown
✅ Correcto: Azul #0066CC coincide
⚠️ Advertencia: Verde ligeramente más oscuro
❌ Error: Falta color amarillo #FFFF00
```

#### 2️⃣ **Análisis de Posiciones**

- Comparar coordenadas de todos los elementos
- Verificar alineación y distribución
- Detectar desplazamientos
- Validar escalas y proporciones

**Evaluación:**

```markdown
✅ Correcto: Todos los puntos en coordenadas correctas
❌ Error: Vértice C en (2, 2.5) debería estar en (2, 3)
```

#### 3️⃣ **Análisis de Valores Numéricos**

- Extraer todos los valores visibles
- Comparar etiquetas y escalas
- Verificar rangos de ejes
- Validar datos en gráficos

#### 4️⃣ **Análisis de Proporciones**

- Comparar proporciones entre elementos
- Verificar aspect ratio
- Validar escalas de ejes
- Detectar distorsiones

#### 5️⃣ **Análisis de Estilos**

- Comparar grosores de líneas
- Verificar tipos de línea
- Comparar tamaños de fuente
- Validar marcadores

#### 6️⃣ **Análisis de Elementos**

- Inventariar elementos presentes
- Identificar elementos faltantes
- Detectar elementos extra
- Verificar completitud

**Resultado:** Reporte detallado con similitud visual estimada (0-100%)

---

### ⚡ **FASE 4: DECISIÓN Y REFINAMIENTO**

**Punto de decisión:** ¿Similitud visual ≥ 95%?

#### ✅ **SIMILITUD ≥ 95%** → Validación Exitosa

1. Marcar lenguaje como validado
2. Continuar con siguiente lenguaje
3. Si todos validados → Exportar proyecto

#### 🔄 **SIMILITUD < 95%** → Ciclo de Refinamiento

**Proceso automático:**

1. **Identificar correcciones** por prioridad:
   - **Alta**: Impacto visual significativo
   - **Media**: Mejoras importantes
   - **Baja**: Detalles menores

2. **Generar código corregido**:
   - Aplicar correcciones de alta prioridad
   - Mantener código limpio y documentado
   - Preservar elementos correctos

3. **Re-renderizar imagen**:
   - Compilar/ejecutar código corregido
   - Generar nueva imagen

4. **Volver a FASE 3** (Comparación):
   - Repetir análisis visual
   - Evaluar nueva similitud
   - Continuar hasta ≥ 95%

**Límite de iteraciones:** Máximo 5 ciclos por lenguaje

---

### 📦 **FASE 5: EXPORTACIÓN FINAL**

Cuando los 3 lenguajes están validados (≥ 95% similitud), el sistema genera:

**Archivos de código:**

- `output_tikz.tex` - Código LaTeX compilable
- `output_python.py` - Script Python ejecutable
- `output_r.R` - Script R ejecutable

**Imágenes generadas:**

- `images/original.png` - Imagen original
- `images/tikz_output.png` - Resultado TikZ
- `images/python_output.png` - Resultado Python
- `images/r_output.png` - Resultado R

**Reportes:**

- `comparison_report.md` - Análisis comparativo completo
- `analysis_report.md` - Análisis inicial de la imagen

**Estructura final:**

```
proyecto_graficador/
├── output_tikz.tex
├── output_python.py
├── output_r.R
├── comparison_report.md
├── analysis_report.md
└── images/
    ├── original.png
    ├── tikz_output.png
    ├── python_output.png
    └── r_output.png
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

**Salida:** `analysis_report.md`

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
4. Calcula similitud visual (0-100%)
5. Prioriza correcciones
6. Genera reporte detallado

**Salida:** `comparison_report.md` con correcciones específicas

### `/iterar`

**Función:** Refina código basándose en comparación visual.

**Proceso:**

1. Lee reporte de comparación
2. Identifica correcciones prioritarias
3. Modifica código existente
4. Re-renderiza imagen
5. Compara nuevamente
6. Repite hasta similitud ≥ 95%

**Salida:** Código refinado + nueva imagen

### `/exportar`

**Función:** Genera archivos finales y reporte consolidado.

**Proceso:**

1. Verifica que todos los lenguajes estén validados
2. Organiza archivos en estructura de proyecto
3. Genera reporte consolidado
4. Crea archivo README.md
5. Comprime proyecto (opcional)

**Salida:** Proyecto completo listo para uso

---

## 📊 **MÉTRICAS DE CALIDAD**

El sistema garantiza:

### Similitud Visual

- ✅ **95-100%**: Excelente - Diferencias imperceptibles
- ⚠️ **85-94%**: Buena - Diferencias menores aceptables
- 🔄 **75-84%**: Regular - Requiere refinamiento
- ❌ **< 75%**: Pobre - Requiere revisión mayor

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
5. `/comparar` - Comparar visual
6. `/iterar` - Refinar código
7. `/exportar` - Exportar proyecto

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

**Última actualización:** 2025-12-28
**Versión del documento:** 1.0
**Estado:** ✅ Completo y verificado

