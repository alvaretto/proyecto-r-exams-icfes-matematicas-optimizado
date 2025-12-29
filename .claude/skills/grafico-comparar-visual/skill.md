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
## Comparación Visual - [TikZ/Python/R] - Iteración [N]

### Puntuación Cuantitativa

**Similitud Total: [X]/100 puntos**

| Categoría | Puntuación | Criterio Aplicado |
|-----------|------------|-------------------|
| Colores | [X]/20 | [criterio] |
| Posiciones | [X]/20 | [criterio] |
| Valores | [X]/20 | [criterio] |
| Proporciones | [X]/15 | [criterio] |
| Estilos | [X]/15 | [criterio] |
| Elementos | [X]/10 | [criterio] |

### Recomendación

[✅ Validar / ⚠️ Considerar validar o iterar / ⚠️ Iterar / ❌ Iterar o regenerar]

[Justificación basada en puntuación y detalles]

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

### Historial de Similitud

[Mostrar array similitud_historico como progreso]

**Progreso de Similitud**:
- Iteración 1: 75 puntos
- Iteración 2: 82 puntos
- Iteración 3: 89 puntos
- **Tendencia**: Mejora constante (+7 puntos por iteración promedio)

### Evaluación Final

**Puntuación Actual**: 89/100 puntos

**Puntuación Esperada tras correcciones**: 96-98 puntos

**Recomendación**: ⚠️ **Considerar validar o iterar**

**Justificación**: 
- Puntuación de 89 puntos indica buena similitud
- Las diferencias identificadas son menores (colores similares, proporciones ligeramente diferentes)
- Con las correcciones de alta prioridad, se espera alcanzar 95+ puntos
- El usuario puede decidir validar ahora o iterar una vez más para perfeccionar

**Próximos Pasos Sugeridos**:

1. Si **Validar**: Continuar al siguiente lenguaje (Python/R)
2. Si **Iterar**: Aplicar correcciones de alta prioridad y re-comparar
3. Si **Regenerar**: Regenerar desde cero con análisis más detallado

**¿Deseas aplicar estas correcciones automáticamente?** [Sí/No]
```

## Sistema de Métricas Cuantitativas

### Sistema de Puntuación (0-100 puntos)

El sistema de puntuación sigue el esquema `.claude/schemas/metricas_similitud.schema.json` y evalúa 6 categorías:

#### 1. Colores (0-20 puntos)

**Criterios de evaluación**:

- **20 puntos**: Todos los colores coinciden exactamente (diferencia RGB < 1%)
- **15 puntos**: Colores similares (diferencia RGB 1-10%)
- **10 puntos**: Algunos colores incorrectos (1-2 colores con diferencia > 10%)
- **5 puntos**: Colores muy diferentes (3+ colores incorrectos o diferencias > 20%)
- **0 puntos**: Colores completamente incorrectos o paleta completamente diferente

**Cálculo**:
1. Extraer todos los colores de ambas imágenes
2. Comparar cada color usando distancia RGB: `sqrt((R1-R2)² + (G1-G2)² + (B1-B2)²)`
3. Clasificar según porcentaje de colores que coinciden
4. Asignar puntuación según criterio aplicado

#### 2. Posiciones y Coordenadas (0-20 puntos)

**Criterios de evaluación**:

- **20 puntos**: Todas las coordenadas exactas (diferencia < 1% del rango)
- **15 puntos**: Diferencias menores al 5% del rango
- **10 puntos**: Diferencias entre 5-10% del rango
- **5 puntos**: Diferencias entre 10-20% del rango
- **0 puntos**: Diferencias mayores al 20% del rango

**Cálculo**:
1. Identificar todos los puntos/coordenadas en ambas imágenes
2. Calcular diferencia porcentual: `|valor_generado - valor_original| / rango_total * 100`
3. Promediar diferencias de todos los puntos
4. Asignar puntuación según criterio aplicado

#### 3. Valores Numéricos (0-20 puntos)

**Criterios de evaluación**:

- **20 puntos**: Todos los valores correctos (etiquetas, escalas, anotaciones)
- **15 puntos**: 1-2 valores incorrectos (no críticos)
- **10 puntos**: 3-4 valores incorrectos
- **5 puntos**: 5+ valores incorrectos
- **0 puntos**: Valores críticos incorrectos (ejes, puntos clave)

**Cálculo**:
1. Extraer todos los valores numéricos visibles (etiquetas, escalas, anotaciones)
2. Comparar valor por valor
3. Contar valores incorrectos
4. Identificar si hay valores críticos incorrectos
5. Asignar puntuación según criterio aplicado

#### 4. Proporciones y Escalas (0-15 puntos)

**Criterios de evaluación**:

- **15 puntos**: Proporciones perfectas (aspect ratio y escalas idénticas)
- **10 puntos**: Diferencias menores (< 5% en aspect ratio)
- **5 puntos**: Diferencias moderadas (5-15% en aspect ratio o escalas)
- **0 puntos**: Proporciones incorrectas (> 15% diferencia o distorsión visible)

**Cálculo**:
1. Calcular aspect ratio de ambas imágenes
2. Comparar escalas de ejes
3. Verificar proporciones entre elementos
4. Asignar puntuación según criterio aplicado

#### 5. Estilos (0-15 puntos)

**Criterios de evaluación**:

- **15 puntos**: Todos los estilos coinciden (grosor, tipo de línea, fuente, marcadores)
- **10 puntos**: Estilos similares (diferencias menores)
- **5 puntos**: Algunos estilos incorrectos (1-2 estilos diferentes)
- **0 puntos**: Estilos muy diferentes (3+ estilos incorrectos o completamente diferentes)

**Cálculo**:
1. Comparar grosor de líneas
2. Comparar tipos de línea (sólida, punteada, etc.)
3. Comparar tamaños de fuente
4. Comparar marcadores y símbolos
5. Asignar puntuación según criterio aplicado

#### 6. Elementos (0-10 puntos)

**Criterios de evaluación**:

- **10 puntos**: Todos los elementos presentes (ninguno faltante ni extra)
- **7 puntos**: 1 elemento faltante o extra
- **4 puntos**: 2-3 elementos faltantes o extra
- **0 puntos**: 4+ elementos faltantes o extra

**Cálculo**:
1. Inventariar todos los elementos en imagen original
2. Inventariar todos los elementos en imagen generada
3. Identificar elementos faltantes
4. Identificar elementos extra
5. Asignar puntuación según criterio aplicado

### Puntuación Total y Recomendación

**Puntuación Total**: Suma de todas las categorías (0-100 puntos)

**Recomendación basada en puntuación**:

- **95-100 puntos**: ✅ **Validar** - Excelente similitud, listo para validar
- **85-94 puntos**: ⚠️ **Considerar validar o iterar** - Bueno, mejoras menores posibles
- **70-84 puntos**: ⚠️ **Iterar** - Regular, necesita refinamiento
- **< 70 puntos**: ❌ **Iterar o regenerar** - Pobre, requiere correcciones mayores

### Formato de Métricas en Reporte

```json
{
  "timestamp": "2025-12-29T11:45:00Z",
  "lenguaje": "tikz",
  "iteracion": 3,
  "puntuacion_total": 89,
  "categorias": {
    "colores": {
      "puntuacion": 18,
      "criterio_aplicado": "colores_similares",
      "detalles": "Todos los colores coinciden excepto un tono de azul ligeramente más oscuro"
    },
    "posiciones": {
      "puntuacion": 17,
      "criterio_aplicado": "diferencias_menores_5pct",
      "detalles": "Coordenadas correctas con diferencias menores al 3%"
    },
    "valores": {
      "puntuacion": 20,
      "criterio_aplicado": "todos_correctos",
      "detalles": "Todos los valores numéricos coinciden exactamente"
    },
    "proporciones": {
      "puntuacion": 12,
      "criterio_aplicado": "diferencias_menores",
      "detalles": "Aspect ratio ligeramente diferente (4:3 vs 4.1:3)"
    },
    "estilos": {
      "puntuacion": 13,
      "criterio_aplicado": "estilos_similares",
      "detalles": "Grosor de línea ligeramente más delgado"
    },
    "elementos": {
      "puntuacion": 9,
      "criterio_aplicado": "1_faltante_extra",
      "detalles": "Falta una anotación menor en el vértice",
      "elementos_faltantes": ["Anotación vértice"],
      "elementos_extra": []
    }
  },
  "recomendacion": "considerar_validar",
  "justificacion": "Puntuación de 89 puntos indica buena similitud. Las diferencias son menores y pueden ser aceptables o refinadas en una iteración adicional."
}
```

### Integración con Estado del Workflow

Después de calcular las métricas:

1. Guardar métricas en formato JSON (opcional, para historial detallado)
2. Actualizar `outputs/workflow_state.json`:
   - `[lenguaje].similitud_actual` = `puntuacion_total`
   - Añadir `puntuacion_total` a `[lenguaje].similitud_historico`
3. Actualizar `timestamp_ultima_actualizacion`

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

## Referencias

- `.claude/schemas/metricas_similitud.schema.json` - Esquema del sistema de puntuación
- `skills/gestionar-estado/skill.md` - Skill de gestión de estado del workflow
- `outputs/workflow_state.json` - Archivo de estado persistente

## Activación

Esta skill se activa:

- Después de cada generación de código (automático)
- Con el comando `/comparar`
- Cuando el usuario solicita validación

## Salida

1. **Reporte estructurado en markdown** con análisis detallado y correcciones específicas
2. **Métricas cuantitativas** en formato JSON según esquema
3. **Actualización de estado** en `workflow_state.json` con similitud actual e historial
4. **Actualización de documentación** en `reporte_matematico.md` con sección de iteración

