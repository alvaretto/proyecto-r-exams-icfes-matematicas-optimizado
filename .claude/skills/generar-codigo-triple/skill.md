# Skill: Generar Codigo Triple (TikZ, Python, R)

## Descripcion

Skill que genera codigo en 3 lenguajes (TikZ/LaTeX, Python matplotlib, R ggplot2) a partir de una imagen matematica, optimizado para integracion en archivos .Rmd de R-exams.

## Invocacion

```
/generar-codigo-triple [ruta/imagen.png]
```

O automaticamente via hook `post-imagen-matematica-detectada`.

## Proceso Completo

### PASO 1: Analisis de Imagen

Usar Claude Vision para analizar la imagen:

```markdown
## Analisis de Imagen Matematica

### Clasificacion
- **Tipo**: [Geometria/Estadistica/Calculo/Trigonometria/Algebra]
- **Subtipo**: [Especifico]
- **Complejidad**: [Baja/Media/Alta]

### Elementos Detectados
- Ejes: [Si/No] - Rango X: [min, max], Rango Y: [min, max]
- Curvas/Funciones: [lista]
- Figuras geometricas: [lista]
- Anotaciones: [lista]
- Colores principales: [lista hex]

### Requisitos Tecnicos
- TikZ: [paquetes necesarios]
- Python: [librerias necesarias]
- R: [paquetes necesarios]
```

### PASO 2: Generacion TikZ/LaTeX

```latex
\documentclass[border=2mm]{standalone}
\usepackage{tikz}
\usepackage{pgfplots}
\pgfplotsset{compat=1.18}
\usetikzlibrary{arrows.meta, calc, patterns}

\begin{document}
\begin{tikzpicture}
    % Codigo generado segun analisis
\end{tikzpicture}
\end{document}
```

**Guardar en:** `Graficador-Experto/outputs/output_tikz.tex`

**Compilar:**
```bash
cd Graficador-Experto/outputs
pdflatex output_tikz.tex
convert -density 300 output_tikz.pdf -quality 100 renders/render_tikz.png
```

### PASO 3: Generacion Python (Reticulate-compatible)

```python
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Grafico generado por Graficador-Experto
Compatible con R-Markdown via reticulate
"""

import matplotlib.pyplot as plt
import numpy as np
from matplotlib import rcParams

# Configuracion para alta calidad
rcParams['figure.dpi'] = 300
rcParams['savefig.dpi'] = 300
rcParams['font.size'] = 11

# Crear figura
fig, ax = plt.subplots(figsize=(8, 6))

# Codigo generado segun analisis
# ...

# Guardar
plt.savefig('renders/render_python.png', dpi=300, bbox_inches='tight')
plt.close()
```

**Guardar en:** `Graficador-Experto/outputs/output_python.py`

**Ejecutar:**
```bash
cd Graficador-Experto/outputs
python3 output_python.py
```

### PASO 4: Generacion R (ggplot2)

```r
#!/usr/bin/env Rscript
# Grafico generado por Graficador-Experto
# Listo para usar en chunk R-exams

library(ggplot2)
library(dplyr)

# Configuracion
theme_set(theme_minimal())

# Codigo generado segun analisis
# ...

# Guardar
ggsave("renders/render_r.png", width = 8, height = 6, dpi = 300)
```

**Guardar en:** `Graficador-Experto/outputs/output_r.R`

**Ejecutar:**
```bash
cd Graficador-Experto/outputs
Rscript output_r.R
```

### PASO 5: Validacion Visual

Comparar cada render con imagen original usando Claude Vision:

```markdown
## Validacion de Fidelidad

| Codigo | Render | Fidelidad | Estado |
|--------|--------|-----------|--------|
| TikZ   | render_tikz.png | XX% | [OK/Refinar] |
| Python | render_python.png | XX% | [OK/Refinar] |
| R      | render_r.png | XX% | [OK/Refinar] |

### Diferencias Detectadas
- TikZ: [lista de diferencias]
- Python: [lista de diferencias]
- R: [lista de diferencias]
```

**Criterio:** Fidelidad >= 95% es aceptable. Si < 90%, iterar.

### PASO 6: Iteracion (si necesario)

Maximo 5 iteraciones por lenguaje:

```markdown
Iteracion N/5 para [lenguaje]:
- Diferencias a corregir: [lista]
- Ajustes aplicados: [lista]
- Nueva fidelidad: XX%
```

### PASO 7: Presentacion al Usuario

```markdown
## Codigos Generados Exitosamente

He generado codigo en 3 lenguajes para replicar la imagen matematica:

### 1. TikZ/LaTeX
- **Fidelidad visual:** XX%
- **Ubicacion:** `Graficador-Experto/outputs/output_tikz.tex`
- **Uso recomendado:** Diagramas geometricos precisos, figuras matematicas

### 2. Python (matplotlib)
- **Fidelidad visual:** XX%
- **Ubicacion:** `Graficador-Experto/outputs/output_python.py`
- **Uso recomendado:** Graficos estadisticos, via reticulate en .Rmd

### 3. R (ggplot2)
- **Fidelidad visual:** XX%
- **Ubicacion:** `Graficador-Experto/outputs/output_r.R`
- **Uso recomendado:** Graficos estadisticos nativos en R-exams

---

**¿Cual codigo deseas implementar en el ejercicio?**
1. TikZ (recomendado para geometria)
2. Python (via reticulate)
3. R (nativo ggplot2)
```

### PASO 8: Integracion en .Rmd

Segun seleccion del usuario:

**Opcion TikZ:**
```{r grafico-tikz, echo=FALSE, results='asis'}
include_tikz <- function(code, format) {
  if (format == "latex" || format == "beamer") {
    cat("\\begin{tikzpicture}\n", code, "\n\\end{tikzpicture}")
  } else {
    # Para HTML: generar PNG y incluir
    # ... codigo de generacion ...
  }
}

tikz_code <- '
% Codigo TikZ aqui
'

include_tikz(tikz_code, knitr::opts_knit$get("rmarkdown.pandoc.to"))
```

**Opcion Python:**
```{r setup-python, include=FALSE}
library(reticulate)
# Usar Python del sistema
use_python("/usr/bin/python3")
```

```{python grafico, echo=FALSE, fig.width=8, fig.height=6}
# Codigo Python aqui
```

**Opcion R:**
```{r grafico, echo=FALSE, fig.width=8, fig.height=6}
# Codigo ggplot2 aqui
```

## Reporte Final

Generar reporte consolidado en `Graficador-Experto/outputs/reporte_matematico.md`:

```markdown
# Reporte de Generacion - Graficador-Experto

## Imagen Analizada
- Archivo: [nombre]
- Tipo: [clasificacion]
- Fecha: [timestamp]

## Codigos Generados

### TikZ/LaTeX
- Archivo: output_tikz.tex
- Fidelidad: XX%
- Lineas de codigo: N

### Python
- Archivo: output_python.py
- Fidelidad: XX%
- Librerias: matplotlib, numpy

### R
- Archivo: output_r.R
- Fidelidad: XX%
- Paquetes: ggplot2, dplyr

## Codigo Seleccionado
- Lenguaje: [seleccion]
- Integrado en: [ruta .Rmd]

## Notas
[observaciones adicionales]
```

## Manejo de Errores

### Error de Compilacion LaTeX
```
Error: pdflatex failed
Solucion: Verificar paquetes TikZ instalados
Alternativa: Usar Python o R
```

### Error de Ejecucion Python
```
Error: ModuleNotFoundError
Solucion: pip install matplotlib numpy
```

### Error de Ejecucion R
```
Error: Package not found
Solucion: install.packages("ggplot2")
```

## Condiciones Criticas

- **SIEMPRE** generar los 3 codigos
- **SIEMPRE** validar compilacion/ejecucion antes de presentar
- **SIEMPRE** preguntar al usuario cual implementar
- **NUNCA** implementar sin aprobacion del usuario
- **DOCUMENTAR** en reporte_matematico.md

## Ubicacion de Archivos

```
Graficador-Experto/
├── outputs/
│   ├── output_tikz.tex       # Codigo TikZ
│   ├── output_tikz.pdf       # PDF compilado
│   ├── output_python.py      # Codigo Python
│   ├── output_r.R            # Codigo R
│   ├── reporte_matematico.md # Reporte
│   └── renders/
│       ├── render_tikz.png   # Render TikZ
│       ├── render_python.png # Render Python
│       └── render_r.png      # Render R
```

---

**Version:** 1.0
**Fecha:** 2025-12-27
**Integracion:** Workflow ICFES R-Exams + Graficador-Experto
