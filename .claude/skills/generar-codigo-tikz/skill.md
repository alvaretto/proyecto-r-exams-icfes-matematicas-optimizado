# Skill: Generación de Código TikZ

## Descripción
Habilidad especializada para generar código TikZ/pgfplots preciso y compilable que reproduzca imágenes matemáticas analizadas.

## Objetivos

- Generar código TikZ sintácticamente correcto
- Reproducir elementos visuales con precisión matemática
- Optimizar código para compilación eficiente
- Mantener legibilidad y documentación clara

## Fundamentos de TikZ

### Paquetes Esenciales
```latex
\usepackage{tikz}           % Dibujo básico
\usepackage{pgfplots}       % Gráficos y plots
\usepackage{amsmath}        % Matemáticas
\usepackage{amssymb}        % Símbolos matemáticos
\pgfplotsset{compat=1.18}   % Versión de pgfplots
```

### Librerías Útiles
```latex
\usetikzlibrary{arrows.meta}      % Flechas
\usetikzlibrary{calc}             % Cálculos
\usetikzlibrary{patterns}         % Patrones de relleno
\usetikzlibrary{decorations}      % Decoraciones
\usetikzlibrary{angles}           % Ángulos
\usetikzlibrary{quotes}           % Etiquetas
\usetikzlibrary{shapes.geometric} % Formas
```

## Capacidades por Tipo de Contenido

### 1. Funciones y Gráficas

**Funciones básicas**:
```latex
\begin{axis}[
    xlabel=$x$,
    ylabel=$y$,
    domain=-5:5,
    samples=100,
    axis lines=middle,
    grid=major
]
\addplot[blue, thick] {x^2};
\addplot[red, dashed] {2*x + 1};
\end{axis}
```

**Funciones trigonométricas**:
```latex
\addplot[domain=0:2*pi, samples=200] {sin(deg(x))};
\addplot[domain=0:2*pi, samples=200] {cos(deg(x))};
```

**Funciones por partes**:
```latex
\addplot[blue, thick] coordinates {
    (-5, 0) (-2, 0)
};
\addplot[blue, thick] coordinates {
    (-2, 1) (2, 1)
};
\addplot[blue, thick] coordinates {
    (2, 0) (5, 0)
};
```

### 2. Figuras Geométricas

**Triángulo**:
```latex
\draw[thick] (0,0) -- (4,0) -- (2,3) -- cycle;
\node[below left] at (0,0) {$A$};
\node[below right] at (4,0) {$B$};
\node[above] at (2,3) {$C$};
```

**Círculo**:
```latex
\draw[thick] (0,0) circle (2cm);
\fill[blue, opacity=0.3] (0,0) circle (2cm);
```

**Polígonos regulares**:
```latex
\draw[thick] (0:2) \foreach \x in {72,144,...,288} {
    -- (\x:2)
} -- cycle;
```

### 3. Estadística

**Gráfico de barras**:
```latex
\begin{axis}[
    ybar,
    symbolic x coords={A,B,C,D,E},
    xtick=data,
    nodes near coords,
    ylabel={Frecuencia}
]
\addplot coordinates {(A,12) (B,18) (C,7) (D,22) (E,15)};
\end{axis}
```

**Histograma**:
```latex
\begin{axis}[
    ybar interval,
    xlabel={Rango},
    ylabel={Frecuencia}
]
\addplot coordinates {
    (0,5) (10,12) (20,8) (30,15) (40,10) (50,0)
};
\end{axis}
```

**Gráfico circular**:
```latex
\pie[radius=2, text=legend]{
    30/Categoría A,
    25/Categoría B,
    20/Categoría C,
    25/Categoría D
}
```

### 4. Geometría Avanzada

**Ángulos**:
```latex
\draw (0,0) -- (3,0) -- (2,2) -- cycle;
\pic[draw, angle radius=0.5cm, "$\theta$"] {angle=B--A--C};
```

**Construcciones geométricas**:
```latex
% Mediatriz
\draw[dashed] ($(A)!0.5!(B) - (0,2)$) -- ($(A)!0.5!(B) + (0,2)$);

% Bisectriz
\draw[dashed] (A) -- ($(B)!(A)!(C)$);
```

## Plantillas por Tipo

### Plantilla: Función Matemática
```latex
\documentclass[border=2mm]{standalone}
\usepackage{tikz}
\usepackage{pgfplots}
\pgfplotsset{compat=1.18}

\begin{document}
\begin{tikzpicture}
    \begin{axis}[
        width=10cm,
        height=8cm,
        xlabel={$x$},
        ylabel={$y$},
        domain=-5:5,
        samples=100,
        axis lines=middle,
        grid=major,
        legend pos=north west
    ]
    
    % Función principal
    \addplot[blue, thick] {x^2 - 4*x + 3};
    \addlegend{$f(x)=x^2-4x+3$}
    
    % Puntos especiales
    \addplot[only marks, mark=*, red] coordinates {(1,0) (3,0) (2,-1)};
    
    % Anotaciones
    \node[above] at (axis cs:1,0) {$(1,0)$};
    \node[above] at (axis cs:3,0) {$(3,0)$};
    \node[below] at (axis cs:2,-1) {Vértice $(2,-1)$};
    
    \end{axis}
\end{tikzpicture}
\end{document}
```

### Plantilla: Geometría Plana
```latex
\documentclass[border=2mm]{standalone}
\usepackage{tikz}
\usetikzlibrary{angles, quotes, calc}

\begin{document}
\begin{tikzpicture}[scale=1.5]
    
    % Definir puntos
    \coordinate (A) at (0,0);
    \coordinate (B) at (4,0);
    \coordinate (C) at (2,3);
    
    % Dibujar triángulo
    \draw[thick] (A) -- (B) -- (C) -- cycle;
    
    % Etiquetas de vértices
    \node[below left] at (A) {$A$};
    \node[below right] at (B) {$B$};
    \node[above] at (C) {$C$};
    
    % Medidas de lados
    \draw[|<->|, >=stealth] ($(A)+(0,-0.3)$) -- ($(B)+(0,-0.3)$) 
        node[midway, below] {$4$ cm};
    
    % Ángulos
    \pic[draw, angle radius=0.5cm, "$\alpha$"] {angle=B--A--C};
    
    % Altura
    \draw[dashed] (C) -- ($(A)!(C)!(B)$) node[midway, right] {$h$};
    
\end{tikzpicture}
\end{document}
```

### Plantilla: Estadística
```latex
\documentclass[border=2mm]{standalone}
\usepackage{pgfplots}
\pgfplotsset{compat=1.18}

\begin{document}
\begin{tikzpicture}
    \begin{axis}[
        ybar,
        width=12cm,
        height=8cm,
        bar width=15pt,
        symbolic x coords={Lun,Mar,Mié,Jue,Vie},
        xtick=data,
        nodes near coords,
        nodes near coords align={vertical},
        ylabel={Ventas},
        xlabel={Día de la semana},
        title={Ventas Semanales},
        ymin=0,
        ymax=30,
        grid=major
    ]
    
    \addplot[fill=blue!60] coordinates {
        (Lun,20) (Mar,25) (Mié,18) (Jue,28) (Vie,22)
    };
    
    \end{axis}
\end{tikzpicture}
\end{document}
```

## Estilos y Personalización

### Colores
```latex
% Colores predefinidos
\draw[red] ...;
\draw[blue!70] ...;        % 70% azul
\draw[red!50!blue] ...;    % Mezcla 50-50

% Colores personalizados
\definecolor{myblue}{RGB}{0,102,204}
\definecolor{mygreen}{HTML}{4CAF50}
```

### Líneas
```latex
\draw[thin] ...;          % Delgada
\draw[thick] ...;         % Gruesa
\draw[very thick] ...;    % Muy gruesa
\draw[dashed] ...;        % Discontinua
\draw[dotted] ...;        % Punteada
\draw[dash dot] ...;      % Raya-punto
```

### Flechas
```latex
\draw[->] ...;                    % Flecha simple
\draw[<->] ...;                   % Doble flecha
\draw[-{Stealth[length=3mm]}] ...; % Flecha personalizada
```

### Rellenos
```latex
\fill[blue] ...;              % Relleno sólido
\fill[blue, opacity=0.3] ...; % Relleno transparente
\pattern[pattern=dots] ...;   % Patrón de puntos
```

## Proceso de Generación

### Paso 1: Estructura Base
```latex
\documentclass[border=2mm]{standalone}
% Paquetes necesarios
\begin{document}
\begin{tikzpicture}
% Contenido
\end{tikzpicture}
\end{document}
```

### Paso 2: Sistema de Coordenadas

- Determinar si se necesita `axis` (pgfplots) o coordenadas directas
- Configurar rangos y escalas
- Establecer etiquetas de ejes

### Paso 3: Elementos Principales

- Añadir curvas, figuras o datos
- Aplicar colores y estilos
- Mantener orden lógico

### Paso 4: Anotaciones y Detalles

- Agregar etiquetas y texto
- Incluir leyendas si es necesario
- Añadir título si corresponde

### Paso 5: Refinamiento

- Ajustar posiciones
- Optimizar espaciado
- Verificar compilación

## Mejores Prácticas

1. **Modularidad**: Usar `\coordinate` para puntos reutilizables
2. **Comentarios**: Documentar secciones del código
3. **Escalas**: Usar `scale` o dimensiones explícitas
4. **Nombres**: Usar nombres descriptivos para nodos y coordenadas
5. **Orden**: Dibujar de atrás hacia adelante (fondo primero)
6. **Precisión**: Usar cálculos exactos cuando sea posible

## Errores Comunes y Soluciones

### Error: Dimensión demasiado grande
**Solución**: Reducir `samples` o ajustar `domain`

### Error: Punto indefinido
**Solución**: Verificar que las coordenadas estén bien definidas

### Error: Paquete no encontrado
**Solución**: Incluir paquete faltante en preámbulo

### Compilación lenta
**Solución**: Reducir `samples`, simplificar cálculos

## Optimización de Código

```latex
% MAL: Repetitivo
\draw (0,0) -- (1,0);
\draw (1,0) -- (1,1);
\draw (1,1) -- (0,1);
\draw (0,1) -- (0,0);

% BIEN: Conciso
\draw (0,0) rectangle (1,1);

% MAL: Sin reutilización
\fill[blue] (2,3) circle (0.1);
\node[above] at (2,3) {$P$};

% BIEN: Con coordenada nombrada
\coordinate (P) at (2,3);
\fill[blue] (P) circle (0.1);
\node[above] at (P) {$P$};
```

## Validación

Antes de entregar el código, verificar:

- [ ] Compila sin errores
- [ ] Todos los elementos están visibles
- [ ] Colores coinciden con el original
- [ ] Proporciones son correctas
- [ ] Texto es legible
- [ ] Código está comentado
- [ ] Estructura es clara

## Salida
El código TikZ completo y compilable, guardado en `outputs/output_tikz.tex`.

