# Skill: Análisis Visual Matemático

## Descripción
Habilidad especializada para analizar imágenes matemáticas de problemas ICFES y extraer información estructurada que permita su reproducción en código.

## Objetivos

- Identificar tipos de gráficos y contenido matemático
- Extraer valores numéricos precisos
- Reconocer símbolos y notación matemática
- Detectar colores, estilos y elementos visuales
- Evaluar complejidad y requisitos técnicos

## Capacidades

### 1. Clasificación de Contenido Matemático
Identifica el tipo de contenido presente en la imagen:

**Geometría**:

- Figuras planas (triángulos, círculos, polígonos)
- Figuras en 3D (cubos, esferas, prismas)
- Construcciones geométricas (bisectrices, mediatrices)
- Transformaciones (traslaciones, rotaciones, simetrías)

**Estadística**:

- Gráficos de barras, histogramas
- Gráficos circulares (pie charts)
- Diagramas de caja (boxplots)
- Diagramas de dispersión
- Gráficos de línea temporal

**Cálculo**:

- Gráficas de funciones
- Límites y continuidad
- Derivadas e integrales
- Áreas bajo la curva
- Tangentes y normales

**Trigonometría**:

- Círculo trigonométrico
- Funciones trigonométricas
- Triángulos con ángulos
- Identidades trigonométricas

**Álgebra**:

- Sistemas de ecuaciones
- Matrices y vectores
- Desigualdades en el plano
- Regiones factibles

### 2. Extracción de Elementos Visuales

**Ejes Coordenados**:

- Origen y orientación
- Rangos (mínimo, máximo)
- Marcas y graduaciones
- Etiquetas de ejes
- Unidades de medida

**Gráficas y Curvas**:

- Tipo de curva (línea recta, parábola, exponencial, etc.)
- Puntos clave (interceptos, máximos, mínimos)
- Continuidad y dominio
- Color y estilo de línea

**Figuras Geométricas**:

- Tipo de figura
- Dimensiones y medidas
- Ángulos
- Puntos y vértices etiquetados
- Colores y rellenos

**Anotaciones y Etiquetas**:

- Texto descriptivo
- Valores numéricos
- Fórmulas matemáticas
- Leyendas
- Títulos y subtítulos

### 3. Reconocimiento de Notación Matemática

**Símbolos comunes**:

- Operadores: +, -, ×, ÷, =, <, >, ≤, ≥
- Raíces: √, ∛
- Potencias y exponentes
- Fracciones
- Funciones: f(x), g(x), sin(x), cos(x), tan(x)
- Integrales: ∫, ∑
- Derivadas: f'(x), dy/dx
- Variables griegas: α, β, γ, θ, π, etc.

**LaTeX equivalente**:
Identifica la notación LaTeX apropiada para cada símbolo reconocido.

### 4. Análisis de Colores y Estilos

**Colores**:

- Identificación RGB/Hex
- Paletas utilizadas
- Contraste y accesibilidad

**Estilos de línea**:

- Sólida, punteada, discontinua
- Grosor (fino, medio, grueso)
- Tipo de marcador (puntos, cruces, círculos)

**Tipografía**:

- Fuente utilizada
- Tamaño de texto
- Negrita, cursiva, regular

### 5. Evaluación de Complejidad

**Nivel Bajo**:

- Gráfico simple con pocos elementos
- Un solo tipo de visualización
- Colores básicos
- Sin anotaciones complejas

**Nivel Medio**:

- Múltiples elementos integrados
- Combinación de 2-3 tipos de visualización
- Varios colores y estilos
- Anotaciones y etiquetas moderadas

**Nivel Alto**:

- Muchos elementos interrelacionados
- Múltiples capas de información
- Paleta de colores compleja
- Anotaciones matemáticas complejas
- Transformaciones o proyecciones 3D

## Proceso de Análisis

### Paso 1: Observación Inicial
```markdown

1. ¿Qué tipo de imagen matemática es?
2. ¿Cuál es el propósito educativo?
3. ¿Qué elementos llaman más la atención?
```

### Paso 2: Análisis Sistemático
```markdown

1. Examinar ejes y sistema de coordenadas
2. Identificar todas las curvas y figuras
3. Extraer valores numéricos visibles
4. Reconocer anotaciones y texto
5. Catalogar colores y estilos
```

### Paso 3: Extracción de Datos
```markdown
Crear tabla estructurada:
| Elemento | Tipo | Propiedades | Valores |
|----------|------|-------------|---------|
| ...      | ...  | ...         | ...     |
```

### Paso 4: Evaluación Técnica
```markdown
Determinar requisitos para cada lenguaje:

- TikZ: [paquetes, comandos especiales]
- Python: [librerías, funciones específicas]
- R: [paquetes, geoms necesarios]
```

### Paso 5: Reporte Estructurado
```markdown
## Análisis de Imagen Matemática

### Clasificación

- **Tipo**: [Geometría/Estadística/Cálculo/etc.]
- **Subtipo**: [Específico]
- **Complejidad**: [Baja/Media/Alta]

### Sistema de Coordenadas

- **Tipo**: [Cartesiano/Polar/3D/Ninguno]
- **Eje X**: Rango [min, max], etiqueta "..."
- **Eje Y**: Rango [min, max], etiqueta "..."
- **Eje Z** (si aplica): ...

### Elementos Gráficos

#### Curvas/Funciones

1. **Curva 1**:
   - Tipo: [Recta/Parábola/Exponencial/etc.]
   - Ecuación estimada: f(x) = ...
   - Color: #hexcode o RGB(r,g,b)
   - Estilo: [Sólida/Punteada/etc.]
   - Grosor: [px o pt]
   - Puntos clave: [(x1,y1), (x2,y2), ...]

#### Figuras Geométricas

1. **Figura 1**:
   - Tipo: [Triángulo/Círculo/etc.]
   - Dimensiones: ...
   - Posición: ...
   - Color: ...
   - Relleno: [Sí/No]

#### Anotaciones

1. **Anotación 1**:
   - Texto: "..."
   - Posición: (x, y)
   - Estilo: ...

### Paleta de Colores

- Color 1: #hexcode - Uso: [descripción]
- Color 2: #hexcode - Uso: [descripción]
...

### Notación Matemática

- Símbolos presentes: [lista]
- Fórmulas: [lista con LaTeX]

### Requisitos Técnicos

#### TikZ
```
Paquetes necesarios:

- tikz
- pgfplots
- [otros]

Comandos especiales:

- [lista]
```

#### Python
```
Librerías necesarias:

- matplotlib
- numpy
- [otras]

Funciones clave:

- [lista]
```

#### R
```
Paquetes necesarios:

- ggplot2
- [otros]

Geoms principales:

- [lista]
```

### Notas Especiales
[Cualquier observación adicional relevante]
```

## Casos de Uso

### Ejemplo 1: Función Cuadrática
**Input**: Imagen de parábola con ejes
**Output**:
```markdown
### Análisis

- Tipo: Cálculo - Función cuadrática
- Función: f(x) = -x² + 4x + 1
- Vértice: (2, 5)
- Intercepto Y: (0, 1)
- Raíces: x ≈ -0.24, x ≈ 4.24
- Color: Azul (#0066CC)
- Dominio visible: [-1, 5]
- Rango visible: [-2, 6]
```

### Ejemplo 2: Gráfico de Barras
**Input**: Imagen de gráfico de barras con 5 categorías
**Output**:
```markdown
### Análisis

- Tipo: Estadística - Gráfico de barras
- Categorías: A, B, C, D, E
- Valores: 12, 18, 7, 22, 15
- Colores: Verde (#4CAF50)
- Ejes: X (categorías), Y (frecuencia, 0-25)
- Título: "Distribución de respuestas"
```

### Ejemplo 3: Triángulo Rectángulo
**Input**: Imagen de triángulo con medidas
**Output**:
```markdown
### Análisis

- Tipo: Geometría - Triángulo rectángulo
- Vértices: A, B, C
- Lados: AB=3cm, BC=4cm, AC=5cm
- Ángulo recto: En B
- Ángulos: ∠A ≈ 53.13°, ∠C ≈ 36.87°
- Color: Negro (contorno), Sin relleno
- Anotaciones: Medidas en cada lado
```

## Mejores Prácticas

1. **Precisión**: Extraer valores numéricos exactos cuando sea posible
2. **Completitud**: No omitir ningún elemento visible
3. **Contexto**: Considerar el propósito educativo de la imagen
4. **Verificación**: Validar que los datos extraídos sean coherentes
5. **Documentación**: Anotar cualquier ambigüedad o suposición realizada

## Activación
Esta skill se activa automáticamente cuando:

- El usuario comparte una imagen
- Se ejecuta el comando `/analizar-imagen-grafica`
- Se detecta contenido matemático en la conversación

## Salida
La salida de esta skill es un reporte estructurado en formato markdown que sirve como base para las skills de generación de código.

