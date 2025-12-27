# Skill: Comparación Visual Inteligente

## Descripción
Habilidad especializada para comparar imágenes generadas con originales usando capacidades de visión por computadora de Claude, identificando diferencias específicas y sugiriendo correcciones precisas.

## Objetivos

- Comparar imágenes de forma sistemática y detallada
- Identificar diferencias específicas en elementos visuales
- Detectar errores matemáticos y de precisión
- Evaluar similitud visual cuantitativamente
- Generar reportes estructurados con correcciones

## Capacidades de Análisis

### 1. Análisis de Colores

**Detección**:

- Identificar todos los colores presentes en ambas imágenes
- Comparar paletas de colores
- Detectar diferencias de tonalidad, saturación y brillo
- Verificar transparencia y opacidad

**Evaluación**:
```markdown
### Colores

✅ Correcto: Los colores azul y rojo coinciden perfectamente
⚠️ Advertencia: El verde es ligeramente más oscuro en la versión generada
❌ Error: Falta el color amarillo presente en el original
```

**Correcciones**:

- Proporcionar códigos de color exactos (RGB, Hex)
- Sugerir ajustes de código específicos para cada lenguaje

### 2. Análisis de Posiciones y Coordenadas

**Detección**:

- Comparar posición de todos los elementos
- Verificar alineación y distribución espacial
- Detectar desplazamientos o rotaciones
- Validar escalas y proporciones

**Evaluación**:
```markdown
### Posiciones

✅ Correcto: Todos los puntos están en las coordenadas correctas
❌ Error: El vértice C está en (2, 2.5) cuando debería estar en (2, 3)
⚠️ Advertencia: Las etiquetas del eje X están ligeramente desplazadas
```

**Correcciones**:

- Especificar coordenadas exactas
- Sugerir ajustes de transformación

### 3. Análisis de Valores Numéricos

**Detección**:

- Extraer todos los valores numéricos visibles
- Comparar etiquetas, escalas y anotaciones
- Verificar rangos de ejes
- Validar valores en gráficos (alturas de barras, puntos de datos)

**Evaluación**:
```markdown
### Valores Numéricos

✅ Correcto: Escalas de ejes coinciden (X: -5 a 5, Y: -3 a 7)
❌ Error: Barra B muestra 15 cuando debería ser 18
❌ Error: Etiqueta del eje Y dice "Frecuencia" en lugar de "Frecuencias"
```

**Correcciones**:

- Listar todos los valores incorrectos con sus valores correctos
- Sugerir código específico para corregir

### 4. Análisis de Proporciones y Escalas

**Detección**:

- Comparar proporciones entre elementos
- Verificar aspect ratio (relación de aspecto)
- Validar escalas de ejes
- Detectar distorsiones

**Evaluación**:
```markdown
### Proporciones

✅ Correcto: La relación de aspecto es 4:3 como en el original
❌ Error: El círculo se ve elíptico, debe ser perfectamente redondo
⚠️ Advertencia: La escala del eje Y está comprimida
```

**Correcciones**:

- Sugerir uso de `aspect='equal'` o equivalente
- Ajustar rangos de ejes para mantener proporciones

### 5. Análisis de Estilos

**Detección**:

- Comparar grosores de líneas
- Verificar tipos de línea (sólida, punteada, discontinua)
- Comparar tamaños de fuente
- Validar marcadores y símbolos

**Evaluación**:
```markdown
### Estilos

✅ Correcto: Grosor de líneas coincide (2pt)
❌ Error: La línea debería ser discontinua, no sólida
⚠️ Advertencia: Fuente ligeramente más pequeña que el original
```

**Correcciones**:

- Especificar estilos exactos (linewidth, linestyle, etc.)
- Sugerir código para cada lenguaje

### 6. Análisis de Elementos

**Detección**:

- Inventariar todos los elementos presentes
- Identificar elementos faltantes
- Detectar elementos extra no presentes en original
- Verificar completitud

**Evaluación**:
```markdown
### Elementos

✅ Correcto: Todos los ejes, etiquetas y título presentes
❌ Error: Falta la leyenda en la esquina superior derecha
❌ Error: Hay una cuadrícula que no está en el original
✅ Correcto: Las tres funciones están graficadas
```

**Correcciones**:

- Listar elementos a añadir
- Indicar elementos a eliminar

## Proceso de Comparación

### Paso 1: Carga de Imágenes
```markdown

1. Cargar imagen original
2. Cargar imagen generada
3. Verificar que ambas sean legibles
4. Normalizar resoluciones si es necesario (para comparación justa)
```

### Paso 2: Análisis Global
```markdown

1. Observación general de similitud
2. Identificar tipo de contenido (función, geometría, estadística)
3. Evaluar similitud visual aproximada (0-100%)
4. Identificar áreas problemáticas principales
```

### Paso 3: Análisis Detallado por Categoría
```markdown
Para cada categoría (colores, posiciones, valores, etc.):

1. Extraer información de imagen original
2. Extraer información de imagen generada
3. Comparar elemento por elemento
4. Documentar diferencias
5. Priorizar por impacto
```

### Paso 4: Generación de Reporte
```markdown
Crear reporte estructurado con:

- Resumen ejecutivo
- Estado general (✅⚠️❌)
- Diferencias categorizadas
- Correcciones específicas
- Similitud estimada
```

### Paso 5: Sugerencias de Código
```markdown
Para cada diferencia:

1. Identificar la causa en el código
2. Proponer corrección específica
3. Mostrar código antes/después
4. Priorizar por impacto visual
```

## Formato de Reporte Estándar

```markdown
## Comparación Visual - [TikZ/Python/R]

### Resumen Ejecutivo
**Estado General**: ✅ Excelente / ⚠️ Necesita ajustes / ❌ Requiere corrección mayor

**Similitud Visual Estimada**: [85]%

**Iteración**: [N]

**Tiempo de análisis**: [timestamp]

---

### Análisis Detallado

#### 1. Colores
**Estado**: ✅ Correcto / ⚠️ Parcial / ❌ Incorrecto

**Diferencias**:

- [ ] Color 1: Original #0066CC → Generado #0055BB (muy similar, aceptable)
- [x] Color 2: Falta el amarillo (#FFFF00) en la leyenda
- [ ] Color 3: Coincide perfectamente

**Correcciones**:
```[lenguaje]
# Añadir color amarillo a la leyenda
[código específico]
```

**Impacto**: Alto / Medio / Bajo

---

#### 2. Posiciones y Coordenadas
**Estado**: ✅ Correcto / ⚠️ Parcial / ❌ Incorrecto

**Diferencias**:

- [x] Punto A: Original (0,0) → Generado (0.1, 0.1)
- [ ] Punto B: Coincide (4, 0)
- [x] Vértice C: Original (2, 3) → Generado (2, 2.8)

**Correcciones**:
```[lenguaje]
# Corregir coordenadas exactas
[código específico]
```

**Impacto**: Alto / Medio / Bajo

---

#### 3. Valores Numéricos
**Estado**: ✅ Correcto / ⚠️ Parcial / ❌ Incorrecto

**Diferencias**:

- [x] Eje X: Rango correcto (-5, 5)
- [x] Eje Y: Original (-3, 7) → Generado (-2, 6)
- [x] Etiqueta: "Frecuencia" en lugar de "Frecuencias"

**Correcciones**:
```[lenguaje]
# Ajustar rango del eje Y y corregir etiqueta
[código específico]
```

**Impacto**: Alto / Medio / Bajo

---

#### 4. Proporciones y Escalas
**Estado**: ✅ Correcto / ⚠️ Parcial / ❌ Incorrecto

**Diferencias**:

- [ ] Aspect ratio: Correcto (1:1)
- [x] Círculo aparece elíptico (aspect no forzado)
- [ ] Proporciones generales correctas

**Correcciones**:
```[lenguaje]
# Forzar aspecto igual para círculo perfecto
[código específico]
```

**Impacto**: Alto / Medio / Bajo

---

#### 5. Estilos
**Estado**: ✅ Correcto / ⚠️ Parcial / ❌ Incorrecto

**Diferencias**:

- [ ] Grosor de líneas: Correcto (2pt)
- [x] Línea principal: Debería ser discontinua, no sólida
- [ ] Tamaño de fuente: Coincide (11pt)
- [x] Marcadores: Faltan círculos en puntos especiales

**Correcciones**:
```[lenguaje]
# Cambiar estilo de línea y añadir marcadores
[código específico]
```

**Impacto**: Alto / Medio / Bajo

---

#### 6. Elementos
**Estado**: ✅ Correcto / ⚠️ Parcial / ❌ Incorrecto

**Elementos presentes**: 

- ✅ Ejes X e Y
- ✅ Título
- ❌ Leyenda (faltante)
- ✅ Grilla
- ❌ Anotación en (2, -1) (faltante)

**Elementos extra**:

- ⚠️ Cuadrícula menor no presente en original

**Correcciones**:
```[lenguaje]
# Añadir leyenda y anotación, eliminar cuadrícula menor
[código específico]
```

**Impacto**: Alto / Medio / Bajo

---

### Priorización de Correcciones

#### Alta Prioridad (Impacto visual significativo):

1. Corregir coordenada del vértice C (2, 3)
2. Añadir leyenda faltante
3. Ajustar rango del eje Y
4. Forzar aspecto 1:1 para círculo

#### Media Prioridad (Mejoras importantes):

5. Cambiar línea a discontinua
6. Añadir anotación en vértice
7. Corregir etiqueta "Frecuencias"

#### Baja Prioridad (Detalles menores):

8. Ajustar coordenada del punto A (0.1 → 0)
9. Eliminar cuadrícula menor

---

### Código Consolidado de Correcciones

```[lenguaje]
# CORRECCIONES PRIORITARIAS

# 1. Coordenada vértice C
[código]

# 2. Añadir leyenda
[código]

# 3. Rango eje Y
[código]

# 4. Aspecto 1:1
[código]

# CORRECCIONES SECUNDARIAS

# 5-7. [resto de correcciones]
[código]
```

---

### Evaluación Final

**Similitud Visual**: 85% → Esperada tras correcciones: 98%

**Recomendación**: 

- Aplicar correcciones de alta prioridad
- Re-renderizar y comparar nuevamente
- Si similitud > 95%, considerar validar
- Si similitud < 95%, iterar con correcciones medias/bajas

**¿Deseas aplicar estas correcciones automáticamente?** [Sí/No]
```

## Métricas de Similitud

### Cuantitativa (Estimada)

- **90-100%**: Excelente, diferencias mínimas o imperceptibles
- **75-89%**: Buena, algunas diferencias visibles pero aceptables
- **50-74%**: Regular, diferencias significativas que requieren atención
- **< 50%**: Pobre, requiere revisión mayor

### Cualitativa

- **Precisión matemática**: Valores, coordenadas, escalas
- **Fidelidad visual**: Colores, proporciones, estilos
- **Completitud**: Todos los elementos presentes
- **Calidad**: Resolución, legibilidad, profesionalismo

## Casos Especiales

### Funciones Matemáticas

- Verificar puntos clave (interceptos, máximos, mínimos)
- Validar continuidad y suavidad de curvas
- Comparar asíntotas y comportamiento en extremos

### Geometría

- Validar ángulos y medidas
- Verificar teoremas geométricos aplicables
- Comprobar simetrías y proporciones

### Estadística

- Validar valores de datos
- Verificar cálculos (promedios, medianas, etc.)
- Comparar distribuciones visuales

## Mejores Prácticas

1. **Ser específico**: No decir "el color está mal", sino "el color es #0055BB cuando debería ser #0066CC"
2. **Priorizar**: Separar errores críticos de mejoras menores
3. **Ser constructivo**: Proporcionar siempre corrección concreta
4. **Contextualizar**: Explicar el impacto de cada diferencia
5. **Ser sistemático**: Seguir el proceso completo para no omitir nada

## Activación
Esta skill se activa:

- Después de cada generación de código (automático)
- Con el comando `/comparar`
- Cuando el usuario solicita validación

## Salida
Reporte estructurado en markdown con análisis detallado y correcciones específicas.

