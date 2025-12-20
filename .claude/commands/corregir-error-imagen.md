---
description: Corrige errores de imágenes faltantes reemplazando \includegraphics por código TikZ.
---

# Corrector de Errores de Imágenes Faltantes

Detecta y corrige errores del tipo:
```
! Package pdftex.def Error: File 'imagen.png' not found
```

## Diagnóstico

El error ocurre cuando el archivo .Rmd usa `\includegraphics{imagen.png}` en lugar de generar la imagen con código TikZ.

## Solución

### Paso 1: Localizar el archivo .Rmd problemático
```bash
# Buscar en En-Desarrollo
find A-Produccion/En-Desarrollo -name "*.Rmd" -type f
```

### Paso 2: Identificar el uso de \includegraphics
```bash
grep -n "includegraphics" A-Produccion/En-Desarrollo/*.Rmd
```

### Paso 3: Consultar ejemplos funcionales con TikZ
```bash
# Buscar ejemplos con figuras geométricas 3D
grep -r "begin{tikzpicture}" A-Produccion/En-Produccion/05-Geometría/ | head -5
```

### Paso 4: Reemplazar con código TikZ

**NUNCA usar `\includegraphics` para figuras matemáticas.**

En su lugar, usar chunk TikZ:

```r
```{r generar_tikz, echo=FALSE, results="asis"}
tikz_code <- '
\\begin{tikzpicture}[scale=1.2]
  % Código TikZ para la figura
  % Consultar ejemplos en A-Produccion/En-Produccion/
\\end{tikzpicture}
'

include_tikz(tikz_code,
             name = "figura_geometrica",
             markup = "markdown",
             format = typ,
             library = c("3d", "babel"),
             packages = c("tikz", "xcolor", "pgfplots"),
             width = "10cm")
```
```

### Paso 5: Para cilindros específicamente

Consultar ejemplos en:
- `/A-Produccion/En-Produccion/05-Geometría/`
- Buscar patrones de cilindros 3D con TikZ

Estructura típica de cilindro:
```latex
\begin{tikzpicture}[scale=1.5]
  % Base inferior (elipse)
  \draw[thick] (0,0) ellipse (2cm and 0.5cm);
  
  % Líneas laterales
  \draw[thick] (-2,0) -- (-2,4);
  \draw[thick] (2,0) -- (2,4);
  
  % Base superior (elipse)
  \draw[thick] (0,4) ellipse (2cm and 0.5cm);
  
  % Etiquetas
  \node at (0,-0.8) {Radio: \textbf{\textit{r}}};
  \node at (2.5,2) {Altura: \textbf{\textit{h}}};
\end{tikzpicture}
```

## Regla de Oro

**SIEMPRE** generar figuras geométricas con TikZ, **NUNCA** con `\includegraphics`.

Consultar `/A-Produccion/En-Produccion/` antes de escribir código TikZ.

