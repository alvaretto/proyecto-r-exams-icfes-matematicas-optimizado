---
description: Inicia el workflow completo con análisis visual de una imagen matemática ICFES.
---

# Analizar Imagen Matemática

Analiza la imagen matemática compartida siguiendo estos pasos:

## 1. Análisis Visual Detallado

Usa Claude Vision para identificar:

- **Tipo de contenido matemático**: Geometría, Estadística, Cálculo, Trigonometría
- **Elementos visuales**: ejes, gráficas, figuras, anotaciones, etiquetas, valores
- **Colores y estilos**: estilos de línea, marcadores
- **Escalas y unidades**: rangos, valores, unidades
- **Texto matemático**: fórmulas y símbolos

## 2. Clasificación

Determina la complejidad y requisitos técnicos para TikZ, Python y R.

## 3. Guardar Análisis Estructurado

Guarda el análisis en formato JSON estructurado y reutilizable:

**Archivo**: `outputs/analisis_inicial.json`

Sigue el esquema `.claude/schemas/analisis_inicial.schema.json` e incluye:

- `timestamp`: Timestamp ISO 8601 del análisis
- `tipo_contenido`: geometria|estadistica|calculo|trigonometria|algebra|otro
- `complejidad`: baja|media|alta
- `elementos_visuales`: Objeto estructurado con ejes, funciones, puntos_especiales, anotaciones, figuras_geometricas, graficos_estadisticos
- `paleta_colores`: Objeto con colores identificados (funcion_principal, ejes, grilla, fondo, texto, otros)
- `recomendaciones_tecnicas`: Objeto con recomendaciones específicas para TikZ, Python y R

**Ejemplo de estructura**:

```json
{
  "timestamp": "2025-12-29T10:30:00Z",
  "tipo_contenido": "calculo",
  "complejidad": "media",
  "elementos_visuales": {
    "ejes": {
      "x": {"min": -5, "max": 5, "etiqueta": "x"},
      "y": {"min": -3, "max": 7, "etiqueta": "f(x)"}
    },
    "funciones": [
      {
        "tipo": "cuadratica",
        "ecuacion": "f(x) = x^2 - 4x + 3",
        "color": "#0066CC",
        "estilo": "solida",
        "grosor": 2
      }
    ],
    "puntos_especiales": [
      {"nombre": "Vértice", "coord": [2, -1], "marcador": "circulo"}
    ]
  },
  "paleta_colores": {
    "funcion_principal": "#0066CC",
    "ejes": "#000000",
    "grilla": "#CCCCCC"
  },
  "recomendaciones_tecnicas": {
    "tikz": "Usar pgfplots con axis environment",
    "python": "matplotlib con numpy para curva suave",
    "r": "ggplot2 con stat_function"
  }
}
```

## 4. Inicializar Estado del Workflow

Usa el skill `gestionar-estado` para inicializar el estado persistente:

**Archivo**: `outputs/workflow_state.json`

1. Crear estructura inicial según `.claude/schemas/workflow_state.schema.json`
2. Establecer `timestamp_inicio` con timestamp actual
3. Establecer `fase_actual` como "analisis"
4. Establecer `imagen_original` como "outputs/original.png" (o ruta de la imagen compartida)
5. Inicializar todos los lenguajes (tikz, python, r) con estado "pendiente"
6. Establecer `analisis_completado` como `false` inicialmente

Después de completar el análisis:

7. Establecer `analisis_completado` como `true`
8. Actualizar `timestamp_ultima_actualizacion`

## 5. Crear Reporte Inicial en Documentación Incremental

Crea o actualiza `outputs/reporte_matematico.md` con la sección de análisis inicial:

```markdown
# Reporte de Conversión Matemática ICFES

## Análisis Inicial

### Tipo de contenido

[Geometría/Estadística/Cálculo/Trigonometría/Álgebra]

### Elementos identificados

- Ejes: [descripción]
- Gráficas: [descripción]
- Figuras: [descripción]
- Anotaciones: [descripción]
- Valores numéricos: [lista]
- Colores: [lista]

### Complejidad

[Baja/Media/Alta] - [justificación]

### Requisitos técnicos

- TikZ: [paquetes necesarios]
- Python: [librerías necesarias]
- R: [paquetes necesarios]

### Timestamp

Análisis realizado: [timestamp ISO 8601]
```

## 6. Inicio de Generación

Automáticamente procede a generar la versión TikZ.

## Referencias

- `skills/analizar-imagen-matematica/skill.md` - Skill de análisis visual completo
- `skills/gestionar-estado/skill.md` - Skill de gestión de estado del workflow
- `.claude/schemas/analisis_inicial.schema.json` - Esquema del análisis estructurado
- `.claude/schemas/workflow_state.schema.json` - Esquema del estado del workflow
- `outputs/` - Directorio donde se guardarán los resultados

