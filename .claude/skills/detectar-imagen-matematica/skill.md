# Skill: Detectar Imagen Matematica e Integrar Graficador-Experto

## Descripcion

Skill que detecta automaticamente cuando el usuario comparte una imagen con escenario matematico ICFES y activa el modulo Graficador-Experto para generar codigo triple (TikZ, Python, R).

## Tipo
- **Activacion:** Automatica al detectar imagen matematica
- **Categoria:** Workflow principal ICFES
- **Dependencia:** Graficador-Experto

## Flujo de Activacion

```
Usuario comparte imagen ICFES
          ↓
    ¿Contiene elementos matematicos?
          ↓
    SI → Activar Graficador-Experto
          ↓
    Generar codigo triple:
    ├── TikZ/LaTeX
    ├── Python (matplotlib/numpy)
    └── R (ggplot2)
          ↓
    Validar fidelidad visual (≥98%)
          ↓
    Preguntar al usuario cual codigo usar
          ↓
    Continuar workflow ICFES normal
```

## Criterios de Deteccion

Una imagen se considera **matematica** si contiene:

### Elementos Graficos Matematicos
- Ejes coordenados (cartesiano, polar)
- Funciones matematicas (parabolas, rectas, exponenciales)
- Figuras geometricas (triangulos, circulos, poligonos)
- Graficos estadisticos (barras, histogramas, circulares, boxplots)
- Diagramas de dispersion
- Arboles de decision o diagramas de flujo matematicos
- Vectores y transformaciones
- Construcciones geometricas

### Notacion Matematica Visual
- Formulas y ecuaciones
- Simbolos matematicos (integrales, sumatorias, raices)
- Etiquetas con variables (x, y, f(x), etc.)
- Medidas y angulos

## Proceso de Ejecucion

### FASE 1: Deteccion y Clasificacion

```markdown
1. Analizar imagen con Claude Vision
2. Identificar tipo de contenido:
   - Geometria (plana, 3D)
   - Estadistica (graficos, diagramas)
   - Calculo (funciones, derivadas)
   - Trigonometria
   - Algebra
3. Evaluar complejidad (Baja/Media/Alta)
4. Determinar elementos a replicar
```

### FASE 2: Generacion Triple de Codigo

Ejecutar workflow del Graficador-Experto ubicado en:
```
/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/Graficador-Experto/.claude
```

**2.1 Generar Codigo TikZ/LaTeX**
```latex
% Para diagramas, figuras geometricas, notaciones
\begin{tikzpicture}
  % Codigo generado automaticamente
\end{tikzpicture}
```
- Usar paquetes: tikz, pgfplots, amsmath, amssymb
- Optimizar para integracion en .Rmd (R-exams)
- Guardar en: `Graficador-Experto/outputs/output_tikz.tex`

**2.2 Generar Codigo Python (Reticulate)**
```python
# Para matplotlib/numpy, integrable via reticulate
import matplotlib.pyplot as plt
import numpy as np
# Codigo generado automaticamente
```
- Usar matplotlib, numpy, scipy
- Formato compatible con chunks R-Markdown via reticulate
- Guardar en: `Graficador-Experto/outputs/output_python.py`

**2.3 Generar Codigo R (ggplot2)**
```r
# Para graficos estadisticos
library(ggplot2)
# Codigo generado automaticamente
```
- Usar ggplot2, dplyr, scales
- Optimizado para chunks R-exams
- Guardar en: `Graficador-Experto/outputs/output_r.R`

### FASE 3: Validacion Visual

1. Compilar/ejecutar cada codigo
2. Generar imagen renderizada
3. Comparar con imagen original usando Claude Vision
4. Calcular fidelidad visual (objetivo: ≥98%)
5. Si <95%, iterar hasta maximo 5 veces

### FASE 4: Seleccion de Usuario

Presentar al usuario:

```markdown
## Codigos Generados

He generado codigo en 3 lenguajes para replicar la imagen:

### 1. TikZ/LaTeX (Recomendado para geometria y diagramas precisos)
- Ubicacion: `Graficador-Experto/outputs/output_tikz.tex`
- Fidelidad: XX%

### 2. Python (matplotlib)
- Ubicacion: `Graficador-Experto/outputs/output_python.py`
- Fidelidad: XX%
- Uso: Via reticulate en chunk R-Markdown

### 3. R (ggplot2)
- Ubicacion: `Graficador-Experto/outputs/output_r.R`
- Fidelidad: XX%
- Uso: Nativo en chunk R-exams

**¿Cual deseas implementar en el ejercicio?**
```

### FASE 5: Integracion con Workflow ICFES

Una vez seleccionado el codigo:

1. Copiar codigo al directorio del ejercicio en desarrollo
2. Integrar en chunk .Rmd apropiado
3. Actualizar Repositorio-Graficas-TikZ si aplica
4. Continuar con workflow normal:
   - `/analizar-icfes` (si no se ha hecho)
   - `/generar-schoice` o `/generar-cloze`
   - Ciclo de Validacion (Fases 1-3)

## Integracion con Repositorio TikZ

Si el codigo TikZ es seleccionado y validado:

1. Parametrizar codigo con placeholders
2. Guardar en `Repositorio-Graficas-TikZ/` con:
   - `[nombre].tikz` - Codigo parametrizable
   - `[nombre].json` - Metadata
   - `[nombre].png` - Preview
3. Actualizar indice del repositorio

## Compatibilidad R-exams

### Chunk TikZ en .Rmd
```{r, echo=FALSE, results='asis'}
include_tikz(
  '% Codigo TikZ aqui',
  format = knitr::opts_knit$get("rmarkdown.pandoc.to")
)
```

### Chunk Python via Reticulate
```{r setup-python, include=FALSE}
library(reticulate)
```

```{python grafico, echo=FALSE}
# Codigo Python aqui
```

### Chunk R Nativo
```{r grafico, echo=FALSE, fig.width=8, fig.height=6}
# Codigo ggplot2 aqui
```

## Hooks Asociados

### post-imagen-detectada
Se activa cuando se detecta imagen matematica:
- Registra deteccion en log
- Inicia workflow Graficador-Experto automaticamente

### post-codigo-generado
Se activa cuando se genera codigo:
- Valida compilacion/ejecucion
- Notifica al usuario

## Mensajes al Usuario

### Deteccion Exitosa
```
He detectado elementos matematicos en la imagen:
- Tipo: [Geometria/Estadistica/Calculo/etc.]
- Elementos: [lista de elementos]
- Complejidad: [Baja/Media/Alta]

Iniciando generacion de codigo triple (TikZ, Python, R)...
```

### Generacion Completada
```
Codigos generados exitosamente:

| Lenguaje | Fidelidad | Ubicacion |
|----------|-----------|-----------|
| TikZ     | XX%       | outputs/output_tikz.tex |
| Python   | XX%       | outputs/output_python.py |
| R        | XX%       | outputs/output_r.R |

¿Cual deseas usar en el ejercicio?
1. TikZ (recomendado para diagramas precisos)
2. Python (via reticulate)
3. R (ggplot2 nativo)
```

## Condiciones Criticas

- **NO** continuar si fidelidad < 90% sin aprobacion del usuario
- **SIEMPRE** generar los 3 codigos
- **SIEMPRE** validar compilacion antes de presentar
- **SIEMPRE** preguntar al usuario cual codigo implementar
- **DOCUMENTAR** codigo seleccionado para referencia futura

## Dependencias

- Graficador-Experto: `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/Graficador-Experto/`
- Claude Vision: Para analisis y comparacion visual
- LaTeX: Para compilacion TikZ
- Python 3.8+: matplotlib, numpy
- R 4.0+: ggplot2, dplyr

## Ubicacion Outputs

```
Graficador-Experto/
├── outputs/
│   ├── output_tikz.tex      # Codigo TikZ final
│   ├── output_python.py     # Codigo Python final
│   ├── output_r.R           # Codigo R final
│   ├── reporte_matematico.md # Reporte consolidado
│   └── renders/             # Imagenes renderizadas
│       ├── render_tikz.png
│       ├── render_python.png
│       └── render_r.png
```

---

**Version:** 1.0
**Fecha:** 2025-12-27
**Integracion:** Workflow ICFES R-Exams
