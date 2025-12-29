---
description: Genera código TikZ para la imagen matemática analizada con validación visual iterativa.
---

# Generar Código TikZ

Genera código TikZ completo para reproducir la imagen matemática analizada, **optimizado para integración con R-exams**.

## Estructura Base para R-exams

El código TikZ debe generarse pensando en su uso dentro de archivos `.Rmd` de R-exams:

```latex
% CÓDIGO TIKZ PARA R-EXAMS
% Este código está diseñado para ser incluido en archivos .Rmd
% usando el chunk: ```{r, echo=FALSE, results='asis'}
% include_tikz("archivo.tex")
% ```

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

## Consideraciones para R-exams

### 1. Compatibilidad con exams2pdf/exams2html

- **NO usar paquetes exóticos**: Solo tikz, pgfplots, amsmath, amssymb
- **Evitar dependencias externas**: No cargar imágenes externas
- **Código autocontenido**: Todo debe estar en un solo archivo .tex
- **Tamaño controlado**: Usar dimensiones absolutas (cm, mm) no relativas

### 2. Estructura Modular

```latex
% === DEFINICIONES (pueden parametrizarse desde R) ===
% Colores
\definecolor{colorPrincipal}{HTML}{0066CC}

% === DATOS (pueden generarse dinámicamente) ===
% Coordenadas que pueden variar por ejercicio
\def\puntoA{(2,3)}
\def\puntoB{(5,7)}

% === GRÁFICO PRINCIPAL ===
\begin{tikzpicture}
  % Código del gráfico
\end{tikzpicture}
```

### 3. Parametrización para Variantes

El código debe facilitar la creación de variantes:

```latex
% Variables que R puede modificar:
% - Valores numéricos en coordenadas
% - Etiquetas de ejes
% - Colores de series
% - Rangos de datos

% Ejemplo de coordenadas parametrizables:
\addplot coordinates {
    (1960, 20000000)  % <- R puede generar estos valores
    (1970, 27000000)
    (1980, 31000000)
};
```

## Proceso

1. **Leer Análisis Inicial**:
   - Cargar `outputs/analisis_inicial.json` para reutilizar análisis estructurado
   - Usar elementos visuales, paleta de colores y recomendaciones técnicas específicas para TikZ
   - Si el archivo no existe, usar análisis del comando `/analizar-imagen-grafica`

2. **Actualizar Estado del Workflow**:
   - Usar skill `gestionar-estado-graficador` para iniciar fase TikZ
   - Establecer `tikz.estado` como "en_iteracion"
   - Establecer `tikz.iteracion_actual` como 1 (primera iteración)
   - Registrar `tikz.timestamp_inicio` con timestamp actual
   - Actualizar `fase_actual` como "tikz_iteracion"
   - Actualizar `timestamp_ultima_actualizacion`

3. **Implementa (Pensando en R-exams)**:
   - Coordenadas exactas basadas en el análisis estructurado
   - **Comentarios que indiquen qué valores son parametrizables**
   - Estilos (colores, grosores, tipos de línea) según paleta identificada
   - **Colores definidos con \definecolor para fácil modificación**
   - Anotaciones y etiquetas con posicionamiento preciso
   - **Etiquetas como variables para facilitar traducción/variantes**
   - Escalas y proporciones correctas según elementos_visuales.ejes
   - Aplicar recomendaciones_tecnicas.tikz del análisis

4. **Valida**:
   - El código debe compilar sin errores con pdflatex
   - Usa sintaxis TikZ/pgfplots estándar
   - **Compatible con exams2pdf y exams2html**
   - Incluye comentarios explicativos
   - **Marca secciones parametrizables para R**

5. **Después de generar**:
   - Guarda el código en `outputs/output_tikz.tex`
   - Añade sección "Código TikZ" en `outputs/reporte_matematico.md` con el código generado
   - Compila con pdflatex
   - Convierte a PNG para comparación
   - Ejecuta automáticamente la comparación visual (NO preguntar al usuario)
   - Retorna control al ciclo de `/auto-refinar-grafico` para continuar iteración

**IMPORTANTE**: Este comando NO debe preguntar al usuario durante iteraciones. La pregunta se hace al alcanzar el umbral en `/auto-refinar-grafico`.

## Plantilla R-exams Compatible

```latex
% ============================================
% CÓDIGO TIKZ PARA R-EXAMS
% Archivo: output_tikz.tex
% ============================================
% INSTRUCCIONES DE USO EN R-EXAMS:
%
% 1. En el archivo .Rmd, incluir el gráfico así:
%    ```{r grafico, echo=FALSE, results='asis'}
%    include_tikz("output_tikz.tex", name = "grafico1")
%    ```
%
% 2. Para variantes, modificar las variables marcadas con % PARAM
%
% ============================================

\documentclass[border=2mm]{standalone}
\usepackage{tikz}
\usepackage{pgfplots}
\usepackage{amsmath}
\pgfplotsset{compat=1.18}

% === COLORES (PARAM: modificables para variantes) ===
\definecolor{color1}{HTML}{00BFFF}
\definecolor{color2}{HTML}{000000}
\definecolor{color3}{HTML}{CC6600}
\definecolor{gridcolor}{HTML}{CCCCCC}

\begin{document}
\begin{tikzpicture}

\begin{axis}[
    % === DIMENSIONES (PARAM) ===
    width=12cm,
    height=8cm,
    % === ETIQUETAS (PARAM: traducibles) ===
    xlabel={Eje X},
    ylabel={Eje Y},
    % === RANGOS (PARAM: ajustables por variante) ===
    xmin=0, xmax=100,
    ymin=0, ymax=100,
    % === CONFIGURACIÓN ESTÁNDAR ===
    grid=both,
    grid style={gridcolor, line width=0.5pt},
    legend pos=outer north east,
    legend style={font=\small, draw=none},
]

% === DATOS (PARAM: generables desde R) ===
\addplot[color=color1, line width=1.5pt] coordinates {
    (0, 10)
    (50, 50)
    (100, 90)
};
\addlegendentry{Serie 1}

\end{axis}
\end{tikzpicture}
\end{document}
```

## Opciones

- `--refinar`: Refina código existente basado en última comparación
- `--forzar`: Regenera desde cero ignorando código previo
- `--r-exams`: Genera código optimizado para R-exams (default: activado)

## Referencias

- `skills/generar-codigo-tikz/skill.md` - Plantillas y mejores prácticas
- `skills/gestionar-estado-graficador/skill.md` - Skill de gestión de estado del workflow
- `.claude/schemas/analisis_inicial.schema.json` - Esquema del análisis estructurado
- Documentación R-exams: https://www.r-exams.org/
- Hooks automáticos se encargan de compilación

