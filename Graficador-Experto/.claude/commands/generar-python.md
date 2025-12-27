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

1. **Implementa**:
   - Datos y cálculos necesarios con numpy
   - Gráficos con matplotlib (plot, scatter, bar, etc.)
   - Estilos (colores, marcadores, líneas) coincidentes
   - Anotaciones, etiquetas, leyendas
   - Ejes con rangos y escalas correctas

2. **Valida**:
   - El código debe ejecutarse sin errores
   - Usa buenas prácticas de Python
   - Incluye comentarios explicativos

3. **Después de generar**:
   - Guarda el código en `outputs/output_python.py`
   - Ejecuta el código para generar PNG (hook automático)
   - Ejecuta automáticamente el comando `/comparar python`

## Opciones

- `--refinar`: Refina código existente basado en última comparación
- `--forzar`: Regenera desde cero ignorando código previo
- `--formato png|svg`: Especifica formato de salida (default: png)

## Referencias

- `skills/generar-python.md` - Plantillas y mejores prácticas
- Hooks automáticos se encargan de ejecución

