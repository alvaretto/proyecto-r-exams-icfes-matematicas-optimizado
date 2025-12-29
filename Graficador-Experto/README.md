# Graficador Experto ICFES 📐📊

Sistema automatizado inteligente para convertir imágenes matemáticas de problemas ICFES en código reproducible en TikZ (LaTeX), Python (matplotlib/numpy) y R (ggplot2) con validación visual iterativa.

## 🎯 Descripción

Este proyecto implementa un workflow avanzado que utiliza visión por computadora de Claude para analizar imágenes matemáticas y generar código equivalente en tres lenguajes de programación. El sistema refina iterativamente el código generado hasta lograr una representación precisa o mejorada del original.

## 🌟 Características Principales

- **Análisis Visual Inteligente**: Identifica automáticamente el tipo de contenido matemático (Geometría, Estadística, Cálculo, Trigonometría)
- **Generación Multi-Lenguaje**: Produce código en TikZ, Python y R
- **Validación Visual Iterativa**: Compara las imágenes generadas con el original usando Claude Vision
- **Métricas Cuantitativas**: Sistema objetivo de puntuación (0-100 puntos) por categorías [NUEVO]
- **Estado Persistente**: Tracking completo del progreso del workflow con recuperación ante interrupciones [NUEVO]
- **Transferencia de Conocimiento**: Aprendizaje entre lenguajes (TikZ → Python → R) [NUEVO]
- **Refinamiento Automático**: Ajusta el código basándose en diferencias identificadas
- **Iteración Automática**: Opción de iterar automáticamente hasta umbral de similitud [NUEVO]
- **Reportes Detallados**: Genera documentación completa del proceso y resultados con estadísticas

## 📁 Estructura del Proyecto

```
Graficador-Experto/
├── .claude/                   # Configuración de Claude Code
│   ├── commands/              # Comandos slash personalizados
│   │   ├── analizar-imagen.md
│   │   ├── generar-tikz.md
│   │   ├── generar-python.md
│   │   ├── generar-r.md
│   │   ├── comparar.md
│   │   ├── iterar.md
│   │   ├── exportar.md
│   │   ├── estado.md          # [NUEVO] Visualización de progreso
│   │   └── auto-iterar.md     # [NUEVO] Iteración automática
│   ├── skills/                # Skills especializadas
│   │   ├── analizar-imagen-matematica/
│   │   ├── generar-tikz/
│   │   ├── generar-python/
│   │   ├── generar-r/
│   │   ├── comparar-visual/
│   │   ├── refinar-codigo/
│   │   ├── gestionar-estado/      # [NUEVO] Gestión de estado
│   │   └── transferir-conocimiento/ # [NUEVO] Transferencia de conocimiento
│   ├── schemas/                # [NUEVO] Esquemas JSON
│   │   ├── workflow_state.schema.json
│   │   ├── analisis_inicial.schema.json
│   │   ├── metricas_similitud.schema.json
│   │   └── lecciones_aprendidas.schema.json
│   └── README.md              # Documentación de configuración
├── outputs/                   # Archivos generados
│   ├── workflow_state.json    # [NUEVO] Estado persistente del workflow
│   ├── analisis_inicial.json  # [NUEVO] Análisis estructurado inicial
│   ├── lecciones_aprendidas.json # [NUEVO] Lecciones capturadas
│   ├── output_tikz.tex        # Código TikZ final
│   ├── output_python.py       # Código Python final
│   ├── output_r.R             # Código R final
│   ├── reporte_matematico.md  # Reporte consolidado (incremental)
│   └── renders/               # Imágenes renderizadas
└── README.md                  # Este archivo
```

## 🚀 Inicio Rápido

### 1. Compartir una Imagen

Simplemente comparte una imagen matemática de un problema ICFES con Claude.

### 2. Iniciar el Workflow

```
/analizar-imagen
```

Este comando:

1. Analiza la imagen con Claude Vision
2. Identifica el tipo de contenido matemático
3. Extrae elementos visuales y matemáticos
4. Guarda análisis estructurado en `outputs/analisis_inicial.json` [NUEVO]
5. Inicializa estado del workflow en `outputs/workflow_state.json` [NUEVO]
6. Crea reporte inicial en `outputs/reporte_matematico.md` [NUEVO]
7. Inicia automáticamente la generación en los tres lenguajes

### 3. Validación y Refinamiento

El sistema:

- Genera código en cada lenguaje (reutilizando análisis inicial) [NUEVO]
- Actualiza estado del workflow automáticamente [NUEVO]
- Lo renderiza/ejecuta automáticamente
- Compara con el original usando métricas cuantitativas (0-100 puntos) [NUEVO]
- Calcula puntuación por categorías (colores, posiciones, valores, etc.) [NUEVO]
- Actualiza similitud en estado persistente [NUEVO]
- Te presenta el resultado con recomendación objetiva (validar/iterar/regenerar) [NUEVO]
- Refina si solicitas mejoras o usa `/auto-iterar` para iteración automática [NUEVO]

### 4. Exportar Resultados

```
/exportar
```

Genera archivos finales y reporte consolidado.

## 🔗 Integración con Workflow Principal R-Exams

El Graficador-Experto está **integrado** con el workflow principal de generación de ejercicios R-Exams:

### Repositorio Centralizado

Las gráficas TikZ generadas se guardan automáticamente en:

```
Repositorio-Graficas-TikZ/
```

Este repositorio permite:

- **Reutilización**: Gráficas validadas disponibles para múltiples ejercicios
- **Consistencia**: Estilo visual uniforme entre ejercicios
- **Eficiencia**: No regenerar desde cero cada gráfica
- **Calidad**: Solo código TikZ validado visualmente entra al repositorio

### Integración Automática

Cuando se genera un ejercicio con `/generar-schoice` o `/generar-cloze`:

1. **Si detecta necesidad de gráficas TikZ**:
   - Consulta automáticamente `Repositorio-Graficas-TikZ/`
   - Lista opciones disponibles según categoría y tags
   - Permite seleccionar gráfica existente o generar nueva

2. **Si selecciona gráfica existente**:
   - Carga código TikZ del repositorio
   - Integra función parametrizable en el ejercicio
   - Usa valores aleatorios del ejercicio para parametrizar

3. **Si genera nueva gráfica**:
   - Usa workflow completo del Graficador-Experto
   - Guarda automáticamente en repositorio con metadata
   - Disponible para uso futuro

### Comando de Generación Integrado

```
/generar-grafica-nueva [imagen.png]
```

Este comando ejecuta el workflow completo y guarda en el repositorio:

1. Análisis de imagen con Claude Vision
2. Generación iterativa de TikZ (máximo 5 iteraciones)
3. Validación visual (objetivo: >95% similitud)
4. Parametrización con placeholders
5. Guardado en repositorio con metadata JSON
6. Actualización de índice centralizado

### Hook Automático

Un hook (`post-grafica-generada`) detecta cuando se guarda código TikZ en `outputs/` y:

- Copia automáticamente al repositorio
- Solicita metadata (categoría, tags, descripción)
- Genera archivo JSON de metadata
- Actualiza índice del repositorio

**Documentación completa**: 
- `Repositorio-Graficas-TikZ/README.md` - Repositorio centralizado
- `.claude/commands/generar-grafica-nueva.md` - Comando integrado
- `.claude/hooks/post-grafica-generada.md` - Hook automático

## 📝 Comandos Disponibles

### `/analizar-imagen`
Inicia el workflow completo con análisis visual de la imagen compartida.

**Ejemplo**:
```
Usuario: [adjunta imagen de gráfico]
Usuario: /analizar-imagen
```

### `/generar-tikz`
Genera código TikZ para la imagen analizada.

**Opciones**:

- `--refinar`: Refina código existente
- `--forzar`: Regenera desde cero

### `/generar-python`
Genera código Python (matplotlib/numpy).

**Opciones**:

- `--refinar`: Refina código existente
- `--forzar`: Regenera desde cero
- `--formato png|svg`: Especifica formato de salida

### `/generar-r`
Genera código R (ggplot2).

**Opciones**:

- `--refinar`: Refina código existente
- `--forzar`: Regenera desde cero
- `--formato png|svg|pdf`: Especifica formato de salida

### `/comparar`
Compara la imagen generada con la original usando Claude Vision y calcula métricas cuantitativas.

**Uso**:
```
/comparar [lenguaje]
```

**Características**:
- Calcula puntuación cuantitativa (0-100 puntos) por categorías [NUEVO]
- Actualiza estado del workflow con similitud actual e historial [NUEVO]
- Genera recomendación objetiva basada en puntuación [NUEVO]
- Reporte detallado de diferencias con sugerencias de corrección

### `/iterar`
Refina el código del lenguaje actual basándose en la última comparación.

**Uso**:
```
/iterar [lenguaje] [descripción de cambios]
```

### `/exportar`
Genera archivos finales y reporte consolidado con estadísticas del workflow.

**Opciones**:

- `--solo-codigo`: Solo archivos de código
- `--solo-reporte`: Solo reporte
- `--formato html|md`: Formato del reporte

### `/estado` [NUEVO]
Visualiza el estado actual del workflow: progreso por lenguaje, tiempos transcurridos, próximos pasos sugeridos y archivos generados.

**Ejemplo**:
```
/estado
```

Muestra progreso visual con emojis (🟢 Validado, 🟡 En iteración, ⚪ Pendiente) y estadísticas.

### `/auto-iterar` [NUEVO]
Itera automáticamente un lenguaje hasta alcanzar un umbral de similitud o máximo de iteraciones.

**Uso**:
```
/auto-iterar [lenguaje] [umbral] [max_iteraciones]
```

**Parámetros**:
- `lenguaje`: tikz|python|r (requerido)
- `umbral`: Puntuación mínima para validar (default: 95)
- `max_iteraciones`: Máximo de iteraciones permitidas (default: 10)

**Ejemplo**:
```
/auto-iterar tikz 95 10
```

Ejecuta iteraciones automáticas hasta alcanzar 95% de similitud o máximo 10 iteraciones.

## 🔄 Workflow Visual

```mermaid
flowchart TD
    Start[Usuario comparte imagen ICFES] --> Analyze[Analizar imagen con Claude Vision]
    Analyze --> Detect{Contiene elementos matematicos?}
    
    Detect -->|No| End1[Fin: No requiere procesamiento]
    Detect -->|Si| Classify[Clasificar tipo: Geometria/Estadistica/Calculo/Trigonometria]
    
    Classify --> TikZ[Fase TikZ]
    
    subgraph TikZ_Flow [Generacion TikZ]
        TikZ --> GenTikZ[Generar codigo TikZ]
        GenTikZ --> RenderTikZ[Renderizar con LaTeX]
        RenderTikZ --> CompareTikZ[Comparar con original usando Vision]
        CompareTikZ --> ValidateTikZ{Usuario valida?}
        ValidateTikZ -->|No| RefineTikZ[Refinar codigo TikZ]
        RefineTikZ --> GenTikZ
        ValidateTikZ -->|Si| SaveTikZ[Guardar codigo TikZ final]
    end
    
    SaveTikZ --> Python[Fase Python]
    
    subgraph Python_Flow [Generacion Python]
        Python --> GenPython[Generar codigo Python matplotlib/numpy]
        GenPython --> RenderPython[Ejecutar y renderizar]
        RenderPython --> ComparePython[Comparar con original usando Vision]
        ComparePython --> ValidatePython{Usuario valida?}
        ValidatePython -->|No| RefinePython[Refinar codigo Python]
        RefinePython --> GenPython
        ValidatePython -->|Si| SavePython[Guardar codigo Python final]
    end
    
    SavePython --> R_Phase[Fase R]
    
    subgraph R_Flow [Generacion R]
        R_Phase --> GenR[Generar codigo R ggplot2]
        GenR --> RenderR[Ejecutar y renderizar]
        RenderR --> CompareR[Comparar con original usando Vision]
        CompareR --> ValidateR{Usuario valida?}
        ValidateR -->|No| RefineR[Refinar codigo R]
        RefineR --> GenR
        ValidateR -->|Si| SaveR[Guardar codigo R final]
    end
    
    SaveR --> Export[Exportar resultados]
    Export --> Report[Generar reporte consolidado markdown]
    Report --> End2[Fin: Archivos y reporte listos]
```

## 💻 Requisitos del Sistema

### Para TikZ

- LaTeX (TeX Live, MiKTeX, o MacTeX)
- Paquetes: `tikz`, `pgfplots`, `amsmath`, `amssymb`
- Compilador: `pdflatex` o `xelatex`
- Conversor PDF a imagen: ImageMagick o Poppler

### Para Python

- Python 3.8+
- matplotlib
- numpy
- scipy (opcional)
- Pillow

### Para R

- R 4.0+
- ggplot2
- dplyr (recomendado)
- scales (recomendado)

## 📊 Tipos de Contenido Soportados

### Geometría

- Figuras planas y 3D
- Construcciones geométricas
- Transformaciones
- Ángulos y medidas

### Estadística

- Gráficos de barras
- Histogramas
- Gráficos circulares
- Diagramas de dispersión
- Boxplots

### Cálculo

- Gráficas de funciones
- Límites y continuidad
- Derivadas e integrales
- Áreas bajo la curva

### Trigonometría

- Círculo trigonométrico
- Funciones trigonométricas
- Identidades
- Aplicaciones

## 🎓 Ejemplos de Uso

### Ejemplo 1: Función Cuadrática

```
Usuario: [Adjunta imagen de parábola con ejes]
Usuario: /analizar-imagen

Claude: Analizando imagen...
        Tipo: Cálculo - Función cuadrática
        Función detectada: f(x) = x² - 4x + 3
        Vértice: (2, -1)
        Raíces: x ≈ 1, x ≈ 3
        
        Generando código TikZ...
        [Genera, compila y compara]
        
        Puntuación cuantitativa: 92/100 puntos
        - Colores: 18/20 (colores similares)
        - Posiciones: 20/20 (todas exactas)
        - Valores: 20/20 (todos correctos)
        - Proporciones: 14/15 (diferencias menores)
        - Estilos: 15/15 (todos coinciden)
        - Elementos: 5/10 (falta 1 anotación menor)
        
        Recomendación: ⚠️ Considerar validar o iterar
        ¿Deseas refinar o continuar a Python?
```

### Ejemplo 2: Gráfico Estadístico

```
Usuario: [Adjunta gráfico de barras]
Usuario: /analizar-imagen

Claude: Analizando imagen...
        Tipo: Estadística - Gráfico de barras
        Categorías: A, B, C, D, E
        Valores: 12, 18, 7, 22, 15
        
        Generando versiones en TikZ, Python y R...
        [Proceso completo automatizado]
        
Usuario: /exportar

Claude: ✅ Archivos generados:

        - outputs/output_tikz.tex
        - outputs/output_python.py
        - outputs/output_r.R
        - outputs/reporte_matematico.md
```

### Ejemplo 3: Refinamiento Específico

```
Usuario: /comparar tikz

Claude: [Muestra reporte detallado de diferencias]

        - Color del título incorrecto
        - Grosor de línea muy delgado
        - Falta etiqueta en eje Y

Usuario: /iterar tikz "Corregir color del título a azul y aumentar grosor"

Claude: Aplicando correcciones...
        [Refina código, recompila y compara]
        Puntuación mejorada: 92 → 98 puntos
        Recomendación: ✅ Validar - Excelente similitud alcanzada
        ¿Validar y continuar?
```

## 📈 Métricas de Calidad

El sistema evalúa usando métricas cuantitativas objetivas (0-100 puntos):

### Sistema de Puntuación por Categorías

- **Colores (0-20 puntos)**: Coincidencia exacta de paleta de colores
- **Posiciones y Coordenadas (0-20 puntos)**: Precisión de ubicación de elementos
- **Valores Numéricos (0-20 puntos)**: Correctitud de etiquetas, escalas y anotaciones
- **Proporciones y Escalas (0-15 puntos)**: Aspect ratio y escalas correctas
- **Estilos (0-15 puntos)**: Grosor de líneas, tipos de línea, fuentes, marcadores
- **Elementos (0-10 puntos)**: Completitud (todos presentes, ninguno extra)

### Recomendaciones Basadas en Puntuación

- **95-100 puntos**: ✅ Validar - Excelente similitud
- **85-94 puntos**: ⚠️ Considerar validar o iterar - Bueno, mejoras menores posibles
- **70-84 puntos**: ⚠️ Iterar - Regular, necesita refinamiento
- **< 70 puntos**: ❌ Iterar o regenerar - Pobre, requiere correcciones mayores

### Tracking de Progreso

- Historial de similitudes por iteración
- Gráficos de progreso por lenguaje
- Estadísticas de mejora promedio

## 🛠️ Skills Especializadas

El sistema incluye 8 skills principales:

1. **Análisis Visual Matemático**: Identifica y extrae información de imágenes
2. **Generación TikZ**: Crea código LaTeX/TikZ preciso
3. **Generación Python**: Produce código matplotlib/numpy profesional (aplica lecciones de TikZ)
4. **Generación R**: Genera código ggplot2 eficiente (aplica lecciones de TikZ y Python)
5. **Comparación Visual Inteligente**: Analiza diferencias con Claude Vision y calcula métricas cuantitativas
6. **Refinamiento Iterativo**: Mejora código basándose en comparaciones
7. **Gestión de Estado** [NUEVO]: Maneja estado persistente del workflow con tracking completo
8. **Transferencia de Conocimiento** [NUEVO]: Captura y aplica lecciones aprendidas entre lenguajes

## 🔧 Configuración Avanzada

### Hooks Automáticos

El sistema incluye hooks que:

- Compilan TikZ automáticamente al guardar
- Ejecutan Python/R automáticamente al guardar
- Detectan imágenes compartidas y sugieren iniciar workflow
- Inyectan contexto relevante en comparaciones

### Personalización

Puedes modificar:

- `.claude/commands.json`: Ajustar comandos o crear nuevos
- `.claude/hooks.json`: Configurar automatizaciones
- `skills/*.md`: Extender capacidades de cada skill

## 📄 Licencia

Este proyecto es parte del repositorio de matemáticas ICFES y está diseñado para uso educativo.

## 🤝 Contribuciones

Para mejorar el workflow:

1. Identifica áreas de mejora
2. Modifica los archivos de configuración o skills
3. Documenta los cambios
4. Prueba con diferentes tipos de imágenes matemáticas

## 📞 Soporte

Para problemas o sugerencias sobre el workflow, consulta la documentación de cada skill en el directorio `skills/`.

---

**Versión**: 2.0 (Optimizada)  
**Última actualización**: Diciembre 2025  
**Mantenedor**: Equipo ICFES Matemáticas

### Novedades en Versión 2.0

- ✅ Sistema de estado persistente con tracking completo
- ✅ Métricas cuantitativas objetivas (0-100 puntos)
- ✅ Análisis inicial estructurado y reutilizable
- ✅ Documentación incremental automática
- ✅ Comando `/estado` para visualización de progreso
- ✅ Comando `/auto-iterar` para iteración automática
- ✅ Transferencia de conocimiento entre lenguajes

