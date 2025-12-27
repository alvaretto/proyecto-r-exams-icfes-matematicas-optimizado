---
description: Genera código TikZ para la imagen matemática analizada con validación visual iterativa.
---

# Generar Código TikZ

Genera código TikZ completo para reproducir la imagen matemática analizada.

## Estructura Base

```latex
\documentclass[border=2mm]{standalone}
\usepackage{tikz}
\usepackage{pgfplots}
\usepackage{amsmath}
\pgfplotsset{compat=1.18}

\begin{document}
\begin{tikzpicture}
% Tu código aquí
\end{tikzpicture}
\end{document}
```

## Proceso

1. **Implementa**:
   - Coordenadas exactas basadas en el análisis
   - Estilos (colores, grosores, tipos de línea)
   - Anotaciones y etiquetas con posicionamiento preciso
   - Escalas y proporciones correctas

2. **Valida**:
   - El código debe compilar sin errores
   - Usa sintaxis TikZ/pgfplots estándar
   - Incluye comentarios explicativos

3. **Después de generar**:
   - Guarda el código en `outputs/output_tikz.tex`
   - Compila con pdflatex (hook automático)
   - Convierte a PNG para comparación
   - Ejecuta automáticamente el comando `/comparar tikz`

## Opciones

- `--refinar`: Refina código existente basado en última comparación
- `--forzar`: Regenera desde cero ignorando código previo

## Referencias

- `skills/generar-tikz.md` - Plantillas y mejores prácticas
- Hooks automáticos se encargan de compilación

