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

## 3. Reporte Inicial

Genera un reporte estructurado con todos los elementos identificados:

```markdown
## Análisis de Imagen Matemática ICFES

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
```

## 4. Inicio de Generación

Automáticamente procede a generar la versión TikZ.

## Referencias

- `skills/analizar-imagen-matematica.md` - Skill de análisis visual completo
- `outputs/` - Directorio donde se guardarán los resultados

