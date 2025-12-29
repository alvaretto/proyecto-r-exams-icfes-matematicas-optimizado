# Skill: Transferencia de Conocimiento entre Lenguajes

## Descripción

Habilidad especializada para capturar lecciones aprendidas de cada lenguaje y aplicarlas en generaciones subsecuentes, mejorando la eficiencia y calidad del proceso iterativo.

## Objetivos

- Documentar éxitos y problemas encontrados en cada lenguaje
- Transferir estrategias exitosas entre lenguajes (TikZ → Python → R)
- Evitar repetir errores ya identificados
- Aplicar soluciones probadas en lenguajes subsecuentes
- Mejorar progresivamente la calidad con cada lenguaje

## Concepto

El sistema aprende de cada lenguaje y transfiere conocimiento:

1. **TikZ** (primero): Establece estrategias base
2. **Python** (segundo): Aplica lecciones de TikZ + documenta propias
3. **R** (tercero): Aplica lecciones de TikZ y Python + documenta propias

## Funciones Principales

### 1. Capturar Éxitos

**Cuándo usar**: Después de validar un lenguaje o cuando una estrategia funciona bien

**Proceso**:

1. Identificar qué funcionó bien en la iteración actual
2. Categorizar el éxito (colores, posicionamiento, estilos, funciones, anotaciones, otro)
3. Documentar código de ejemplo si es relevante
4. Registrar número de iteración donde se aplicó

**Ejemplo**:

```json
{
  "tikz": {
    "exitos": [
      {
        "descripcion": "Usar pgfplots con axis environment para funciones suaves",
        "categoria": "funciones",
        "codigo_ejemplo": "\\begin{axis}[...]\\addplot {x^2};\\end{axis}",
        "iteracion": 2
      },
      {
        "descripcion": "Definir colores personalizados con RGB exacto funcionó perfectamente",
        "categoria": "colores",
        "codigo_ejemplo": "\\definecolor{myblue}{RGB}{0,102,204}",
        "iteracion": 1
      }
    ]
  }
}
```

### 2. Capturar Problemas

**Cuándo usar**: Cuando se identifica un problema que requiere múltiples iteraciones o una solución específica

**Proceso**:

1. Identificar el problema encontrado
2. Categorizar el problema
3. Documentar la solución aplicada
4. Registrar número de iteraciones requeridas para resolverlo

**Ejemplo**:

```json
{
  "tikz": {
    "problemas": [
      {
        "descripcion": "Posicionamiento de etiquetas requirió 3 iteraciones para ajustar",
        "categoria": "posicionamiento",
        "solucion": "Usar coordenadas relativas con anchor y shift para posicionamiento preciso",
        "iteraciones_requeridas": 3
      }
    ]
  }
}
```

### 3. Aplicar Lecciones de TikZ en Python

**Cuándo usar**: Al ejecutar `/generar-python` o `/iterar python`

**Proceso**:

1. Leer `outputs/lecciones_aprendidas.json`
2. Identificar éxitos y problemas de TikZ
3. Traducir estrategias exitosas a Python:
   - Colores RGB: Usar mismos valores RGB en matplotlib
   - Posicionamiento: Aplicar atención especial si fue problemático en TikZ
   - Estilos: Adaptar estilos que funcionaron bien
4. Evitar problemas conocidos de TikZ
5. Documentar aplicación en `aplicar_de_tikz`

**Ejemplo de aplicación**:

```python
# Lección aprendida de TikZ: Colores RGB exactos funcionaron perfectamente
# Aplicar mismo color RGB en Python
color_funcion = '#0066CC'  # Mismo RGB que funcionó en TikZ

# Lección aprendida de TikZ: Posicionamiento de etiquetas fue problemático
# Prestar atención especial al posicionamiento en Python
plt.annotate('Vértice', xy=(2, -1), xytext=(2.5, -1.5), 
             arrowprops=dict(arrowstyle='->'), fontsize=10)
```

### 4. Aplicar Lecciones de TikZ y Python en R

**Cuándo usar**: Al ejecutar `/generar-r` o `/iterar r`

**Proceso**:

1. Leer `outputs/lecciones_aprendidas.json`
2. Identificar éxitos y problemas de TikZ y Python
3. Aplicar lecciones de ambos lenguajes:
   - Colores: Usar mismos valores que funcionaron en TikZ/Python
   - Posicionamiento: Aplicar atención especial si fue problemático
   - Estilos: Adaptar estilos exitosos
4. Evitar problemas conocidos de lenguajes previos
5. Documentar aplicación en `aplicar_de_tikz` y `aplicar_de_python`

**Ejemplo de aplicación**:

```r
# Lección aprendida de TikZ y Python: Colores RGB exactos funcionaron perfectamente
# Aplicar mismo color RGB en R
color_funcion <- "#0066CC"  # Mismo RGB que funcionó en TikZ y Python

# Lección aprendida de TikZ: Posicionamiento de etiquetas fue problemático
# Lección aprendida de Python: Usar coordenadas exactas evitó el problema
# Aplicar coordenadas exactas en R también
annotate("text", x = 2.5, y = -1.5, label = "Vértice", size = 3)
```

### 5. Actualizar Lecciones Aprendidas

**Cuándo usar**: Después de cada validación o cuando se identifica un éxito/problema significativo

**Proceso**:

1. Leer `outputs/lecciones_aprendidas.json` (o crear si no existe)
2. Añadir nuevo éxito o problema al lenguaje correspondiente
3. Si se aplicó una lección de lenguaje previo, documentar en `aplicar_de_[lenguaje]`
4. Actualizar `timestamp_ultima_actualizacion`
5. Guardar archivo

**Estructura inicial** (si no existe):

```json
{
  "timestamp_inicio": "2025-12-29T10:30:00Z",
  "timestamp_ultima_actualizacion": "2025-12-29T10:30:00Z",
  "tikz": {
    "exitos": [],
    "problemas": []
  },
  "python": {
    "aplicar_de_tikz": [],
    "exitos": [],
    "problemas": []
  },
  "r": {
    "aplicar_de_tikz": [],
    "aplicar_de_python": [],
    "exitos": [],
    "problemas": []
  }
}
```

## Categorías de Lecciones

### Colores

- Códigos RGB/Hex que funcionaron bien
- Paletas de colores exitosas
- Problemas con transparencia u opacidad
- Diferencias de color entre lenguajes

### Posicionamiento

- Estrategias exitosas de posicionamiento
- Problemas con coordenadas relativas vs absolutas
- Ajustes de anotaciones y etiquetas
- Alineación de elementos

### Estilos

- Grosores de línea que funcionaron bien
- Tipos de línea exitosos
- Tamaños de fuente apropiados
- Marcadores y símbolos

### Funciones

- Librerías/paquetes que funcionaron bien
- Métodos de renderizado exitosos
- Estrategias para funciones matemáticas complejas

### Anotaciones

- Estrategias de posicionamiento de texto
- Formatos de texto exitosos
- Problemas con renderizado de fórmulas matemáticas

### Otro

- Cualquier otra lección aprendida que no encaje en categorías anteriores

## Ejemplos de Transferencia

### Ejemplo 1: Colores RGB

**TikZ** (éxito):
```json
{
  "descripcion": "Definir color azul con RGB(0,102,204) funcionó perfectamente",
  "categoria": "colores",
  "codigo_ejemplo": "\\definecolor{myblue}{RGB}{0,102,204}",
  "iteracion": 1
}
```

**Python** (aplicación):
```json
{
  "leccion": "Color RGB(0,102,204) funcionó perfectamente en TikZ",
  "aplicacion": "Usar mismo RGB en matplotlib: color='#0066CC'",
  "resultado": "exitoso"
}
```

**R** (aplicación):
```json
{
  "leccion": "Color RGB(0,102,204) funcionó en TikZ y Python",
  "aplicacion": "Usar mismo RGB en ggplot2: color='#0066CC'",
  "resultado": "exitoso"
}
```

### Ejemplo 2: Posicionamiento Problemático

**TikZ** (problema):
```json
{
  "descripcion": "Posicionamiento de etiquetas requirió 3 iteraciones",
  "categoria": "posicionamiento",
  "solucion": "Usar coordenadas relativas con anchor y shift",
  "iteraciones_requeridas": 3
}
```

**Python** (aplicación preventiva):
```json
{
  "leccion": "Posicionamiento de etiquetas fue problemático en TikZ",
  "aplicacion": "Usar coordenadas exactas desde el inicio en matplotlib",
  "resultado": "exitoso"
}
```

## Integración con Comandos

### En `/generar-python`:

1. Leer `outputs/lecciones_aprendidas.json`
2. Aplicar lecciones de TikZ antes de generar código
3. Documentar aplicación en `aplicar_de_tikz`

### En `/generar-r`:

1. Leer `outputs/lecciones_aprendidas.json`
2. Aplicar lecciones de TikZ y Python antes de generar código
3. Documentar aplicación en `aplicar_de_tikz` y `aplicar_de_python`

### En `/iterar`:

1. Si se identifica un éxito significativo, capturarlo
2. Si se resuelve un problema después de múltiples iteraciones, documentarlo

### En `/comparar`:

1. Si se identifica un patrón de éxito o problema, sugerir capturarlo
2. Si la similitud mejora significativamente, considerar capturar estrategias exitosas

## Beneficios Esperados

- **Reducción de iteraciones**: Aplicar estrategias probadas desde el inicio
- **Mejora de calidad**: Evitar problemas conocidos
- **Consistencia**: Mismos colores y estilos entre lenguajes
- **Eficiencia**: Menos tiempo por proyecto
- **Aprendizaje continuo**: El sistema mejora con cada proyecto

## Referencias

- `.claude/schemas/lecciones_aprendidas.schema.json` - Esquema de lecciones aprendidas
- `outputs/lecciones_aprendidas.json` - Archivo de lecciones aprendidas
- `.claude/commands/generar-python.md` - Comando de generación Python
- `.claude/commands/generar-r.md` - Comando de generación R

