---
description: Genera código Python (matplotlib/numpy) para la imagen matemática.
---

# Generar Código Python

Genera código Python completo usando matplotlib y numpy.

## Estructura Base

```python
import matplotlib.pyplot as plt
import numpy as np
from matplotlib import rcParams

# Configuración de estilo
rcParams['font.size'] = 10
rcParams['figure.figsize'] = (8, 6)

# Tu código aquí

plt.savefig('outputs/output_python.png', dpi=300, bbox_inches='tight')
plt.show()
```

## Proceso

1. **Leer Análisis Inicial y Lecciones Aprendidas**:
   - Cargar `outputs/analisis_inicial.json` para reutilizar análisis estructurado
   - Si existe `outputs/lecciones_aprendidas.json`, leer lecciones de TikZ para aplicar estrategias exitosas
   - Usar elementos visuales, paleta de colores y recomendaciones técnicas específicas para Python
   - Aplicar lecciones aprendidas de TikZ (ej: colores RGB que funcionaron bien)

2. **Actualizar Estado del Workflow**:
   - Usar skill `gestionar-estado` para iniciar fase Python
   - Validar que TikZ esté validado o al menos iniciado (flexible)
   - Establecer `python.estado` como "en_iteracion"
   - Establecer `python.iteracion_actual` como 1 (primera iteración)
   - Registrar `python.timestamp_inicio` con timestamp actual
   - Actualizar `fase_actual` como "python_iteracion"
   - Actualizar `timestamp_ultima_actualizacion`

3. **Implementa**:
   - Datos y cálculos necesarios con numpy según elementos_visuales
   - Gráficos con matplotlib (plot, scatter, bar, etc.)
   - Estilos (colores, marcadores, líneas) según paleta identificada
   - Anotaciones, etiquetas, leyendas según anotaciones del análisis
   - Ejes con rangos y escalas correctas según elementos_visuales.ejes
   - Aplicar recomendaciones_tecnicas.python del análisis

4. **Valida**:
   - El código debe ejecutarse sin errores
   - Usa buenas prácticas de Python
   - Incluye comentarios explicativos

5. **Después de generar**:
   - Guarda el código en `outputs/output_python.py`
   - Añade sección "Código Python" en `outputs/reporte_matematico.md` con el código generado
   - Ejecuta el código para generar PNG (hook automático)
   - Ejecuta automáticamente el comando `/comparar python`

## Opciones

- `--refinar`: Refina código existente basado en última comparación
- `--forzar`: Regenera desde cero ignorando código previo
- `--formato png|svg`: Especifica formato de salida (default: png)

## Referencias

- `skills/generar-python/skill.md` - Plantillas y mejores prácticas
- `skills/gestionar-estado/skill.md` - Skill de gestión de estado del workflow
- `skills/transferir-conocimiento/skill.md` - Skill de transferencia de conocimiento (si existe)
- `.claude/schemas/analisis_inicial.schema.json` - Esquema del análisis estructurado
- Hooks automáticos se encargan de ejecución

