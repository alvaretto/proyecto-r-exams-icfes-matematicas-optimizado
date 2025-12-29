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

1. **Leer Análisis Inicial**:
   - Cargar `outputs/analisis_inicial.json` para reutilizar análisis estructurado
   - Usar elementos visuales, paleta de colores y recomendaciones técnicas específicas para TikZ
   - Si el archivo no existe, usar análisis del comando `/analizar-imagen`

2. **Actualizar Estado del Workflow**:
   - Usar skill `gestionar-estado` para iniciar fase TikZ
   - Establecer `tikz.estado` como "en_iteracion"
   - Establecer `tikz.iteracion_actual` como 1 (primera iteración)
   - Registrar `tikz.timestamp_inicio` con timestamp actual
   - Actualizar `fase_actual` como "tikz_iteracion"
   - Actualizar `timestamp_ultima_actualizacion`

3. **Implementa**:
   - Coordenadas exactas basadas en el análisis estructurado
   - Estilos (colores, grosores, tipos de línea) según paleta identificada
   - Anotaciones y etiquetas con posicionamiento preciso
   - Escalas y proporciones correctas según elementos_visuales.ejes
   - Aplicar recomendaciones_tecnicas.tikz del análisis

4. **Valida**:
   - El código debe compilar sin errores
   - Usa sintaxis TikZ/pgfplots estándar
   - Incluye comentarios explicativos

5. **Después de generar**:
   - Guarda el código en `outputs/output_tikz.tex`
   - Añade sección "Código TikZ" en `outputs/reporte_matematico.md` con el código generado
   - Compila con pdflatex (hook automático)
   - Convierte a PNG para comparación
   - Ejecuta automáticamente el comando `/comparar tikz`

## Opciones

- `--refinar`: Refina código existente basado en última comparación
- `--forzar`: Regenera desde cero ignorando código previo

## Referencias

- `skills/generar-tikz/skill.md` - Plantillas y mejores prácticas
- `skills/gestionar-estado/skill.md` - Skill de gestión de estado del workflow
- `.claude/schemas/analisis_inicial.schema.json` - Esquema del análisis estructurado
- Hooks automáticos se encargan de compilación

