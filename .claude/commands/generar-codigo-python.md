---
description: Genera código Python (matplotlib/numpy) para la imagen matemática, compatible con R-exams via reticulate.
---

# Generar Código Python

Genera código Python completo usando matplotlib y numpy, **optimizado para integración con R-exams mediante reticulate**.

## Estructura Base para R-exams (Reticulate)

El código Python debe generarse pensando en su uso dentro de archivos `.Rmd` de R-exams usando reticulate:

```python
# ============================================
# CÓDIGO PYTHON PARA R-EXAMS (RETICULATE)
# Archivo: output_python.py
# ============================================
# INSTRUCCIONES DE USO EN R-EXAMS:
#
# 1. En el archivo .Rmd, configurar reticulate:
#    ```{r setup, include=FALSE}
#    library(reticulate)
#    use_python("/usr/bin/python3")  # o la ruta de tu Python
#    ```
#
# 2. Incluir el gráfico con chunk Python:
#    ```{python grafico, echo=FALSE}
#    exec(open("output_python.py").read())
#    ```
#
# 3. O ejecutar como función parametrizable:
#    ```{python}
#    # Pasar parámetros desde R
#    datos_x = r.datos_x  # Acceder a variables R
#    ```
#
# ============================================

import matplotlib.pyplot as plt
import numpy as np
from matplotlib import rcParams

# === CONFIGURACIÓN GLOBAL ===
rcParams['font.size'] = 10
rcParams['figure.figsize'] = (9, 6)

# Tu código aquí

plt.savefig('output_python.png', dpi=150, bbox_inches='tight')
```

## Consideraciones para R-exams (Reticulate)

### 1. Compatibilidad con exams2pdf/exams2html

- **Usar solo bibliotecas estándar**: matplotlib, numpy (instaladas por defecto)
- **Evitar dependencias complejas**: No usar seaborn, plotly, etc. a menos que sea necesario
- **Guardar siempre como archivo**: El gráfico debe guardarse como PNG/PDF
- **Código ejecutable standalone**: Debe funcionar sin contexto adicional

### 2. Estructura para Integración con Reticulate

```python
# === PARÁMETROS (recibibles desde R via reticulate) ===
# Estos valores pueden ser sobrescritos desde R:
# r.param_x, r.param_y, etc.

# Valores por defecto (se usan si no se pasan desde R)
TITULO = "Título del Gráfico"  # PARAM: traducible
XLABEL = "Eje X"               # PARAM: traducible
YLABEL = "Eje Y"               # PARAM: traducible

# === COLORES (PARAM: modificables para variantes) ===
COLOR_1 = '#00BFFF'
COLOR_2 = '#000000'
COLOR_3 = '#CC6600'

# === DATOS (PARAM: generables desde R) ===
# Estos datos pueden generarse dinámicamente en R y pasarse a Python
datos_x = [1960, 1970, 1980, 1990, 2000, 2010]
datos_y = [20, 30, 40, 50, 60, 70]
```

### 3. Función Reutilizable

```python
def generar_grafico(datos_x, datos_y, titulo="", xlabel="", ylabel="",
                    color='#0066CC', output_file='grafico.png'):
    """
    Función reutilizable para generar gráficos desde R-exams.

    Args:
        datos_x: Lista o array de valores X
        datos_y: Lista o array de valores Y
        titulo: Título del gráfico
        xlabel: Etiqueta eje X
        ylabel: Etiqueta eje Y
        color: Color de la línea/puntos
        output_file: Nombre del archivo de salida
    """
    fig, ax = plt.subplots(figsize=(9, 6))
    ax.plot(datos_x, datos_y, color=color, linewidth=2)
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    ax.set_title(titulo)
    ax.grid(True, alpha=0.3)
    plt.tight_layout()
    plt.savefig(output_file, dpi=150, bbox_inches='tight')
    plt.close()
    return output_file
```

## Proceso

1. **Leer Análisis Inicial y Lecciones Aprendidas**:
   - Cargar `outputs/analisis_inicial.json` para reutilizar análisis estructurado
   - Si existe `outputs/lecciones_aprendidas.json`, leer lecciones de TikZ para aplicar estrategias exitosas
   - Usar elementos visuales, paleta de colores y recomendaciones técnicas específicas para Python
   - Aplicar lecciones aprendidas de TikZ (ej: colores RGB que funcionaron bien)

2. **Actualizar Estado del Workflow**:
   - Usar skill `gestionar-estado-graficador` para iniciar fase Python
   - Validar que TikZ esté validado o al menos iniciado (flexible)
   - Establecer `python.estado` como "en_iteracion"
   - Establecer `python.iteracion_actual` como 1 (primera iteración)
   - Registrar `python.timestamp_inicio` con timestamp actual
   - Actualizar `fase_actual` como "python_iteracion"
   - Actualizar `timestamp_ultima_actualizacion`

3. **Implementa (Pensando en R-exams/Reticulate)**:
   - Datos y cálculos necesarios con numpy según elementos_visuales
   - **Variables parametrizables al inicio del archivo**
   - Gráficos con matplotlib (plot, scatter, bar, etc.)
   - **Colores como constantes para fácil modificación**
   - Estilos (colores, marcadores, líneas) según paleta identificada
   - Anotaciones, etiquetas, leyendas según anotaciones del análisis
   - **Etiquetas como variables para traducción/variantes**
   - Ejes con rangos y escalas correctas según elementos_visuales.ejes
   - Aplicar recomendaciones_tecnicas.python del análisis
   - **Guardar siempre como archivo PNG y PDF**

4. **Valida**:
   - El código debe ejecutarse sin errores con Python 3.x
   - Usa buenas prácticas de Python
   - **Compatible con reticulate en R-exams**
   - Incluye comentarios explicativos
   - **Marca secciones parametrizables para R**

5. **Después de generar**:
   - Guarda el código en `outputs/output_python.py`
   - Añade sección "Código Python" en `outputs/reporte_matematico.md` con el código generado
   - Ejecuta el código para generar PNG (hook automático)
   - Ejecuta automáticamente el comando `/comparar-similitud-visual python`

**IMPORTANTE**: Este comando NO debe preguntar al usuario durante iteraciones. La pregunta se hace al alcanzar el umbral en `/auto-refinar-grafico`.

## Plantilla R-exams Compatible (Reticulate)

```python
#!/usr/bin/env python3
# ============================================
# CÓDIGO PYTHON PARA R-EXAMS (RETICULATE)
# Archivo: output_python.py
# ============================================
# INSTRUCCIONES DE USO EN R-EXAMS:
#
# Opción 1 - Ejecutar directamente:
#    ```{python grafico, echo=FALSE}
#    exec(open("output_python.py").read())
#    ```
#
# Opción 2 - Importar función y parametrizar:
#    ```{python}
#    from output_python import generar_grafico
#    generar_grafico(datos_x=r.x, datos_y=r.y, titulo=r.titulo)
#    ```
#
# ============================================

import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import numpy as np

# === CONFIGURACIÓN GLOBAL ===
plt.rcParams['font.size'] = 10
plt.rcParams['axes.linewidth'] = 1.0

# === PARÁMETROS (PARAM: recibibles desde R) ===
TITULO = ""
XLABEL = "Eje X"
YLABEL = "Eje Y"

# === COLORES (PARAM: modificables para variantes) ===
COLOR_1 = '#00BFFF'  # Cyan
COLOR_2 = '#000000'  # Negro
COLOR_3 = '#CC6600'  # Naranja/marrón
COLOR_4 = '#0066CC'  # Azul
COLOR_5 = '#FF9900'  # Naranja
COLOR_GRID = '#CCCCCC'

# === DATOS (PARAM: generables desde R) ===
datos_serie1_x = [1960, 1970, 1980, 1990, 2000, 2010]
datos_serie1_y = [20e6, 30e6, 35e6, 40e6, 42e6, 43e6]

# === FUNCIÓN PRINCIPAL ===
def generar_grafico(output_file='python_final.png'):
    """Genera el gráfico y lo guarda como archivo."""

    fig, ax = plt.subplots(figsize=(9, 6))

    # Serie 1
    ax.plot(datos_serie1_x, datos_serie1_y,
            color=COLOR_1, linestyle=':', linewidth=2, label='Serie 1')

    # Configurar ejes
    ax.set_xlabel(XLABEL)
    ax.set_ylabel(YLABEL)
    ax.set_title(TITULO)

    # Grilla
    ax.grid(True, linestyle='-', linewidth=0.5, color=COLOR_GRID, alpha=0.7)

    # Leyenda
    ax.legend(loc='upper left', bbox_to_anchor=(1.02, 1), frameon=False)

    # Ajustar layout
    plt.tight_layout()

    # Guardar
    plt.savefig(output_file, dpi=150, bbox_inches='tight')
    plt.savefig(output_file.replace('.png', '.pdf'), bbox_inches='tight')

    print(f"Gráfico guardado: {output_file}")
    return output_file

# === EJECUCIÓN PRINCIPAL ===
if __name__ == "__main__":
    generar_grafico('python_final.png')
```

## Opciones

- `--refinar`: Refina código existente basado en última comparación
- `--forzar`: Regenera desde cero ignorando código previo
- `--formato png|svg|pdf`: Especifica formato de salida (default: png)
- `--r-exams`: Genera código optimizado para R-exams/reticulate (default: activado)

## Referencias

- `skills/generar-codigo-python/skill.md` - Plantillas y mejores prácticas
- `skills/gestionar-estado-graficador/skill.md` - Skill de gestión de estado del workflow
- `skills/transferir-conocimiento-grafico/skill.md` - Skill de transferencia de conocimiento (si existe)
- `.claude/schemas/analisis_inicial.schema.json` - Esquema del análisis estructurado
- Documentación reticulate: https://rstudio.github.io/reticulate/
- Documentación R-exams: https://www.r-exams.org/
- Hooks automáticos se encargan de ejecución

