---
description: Corrige errores específicos de visualización gráfica en archivos .Rmd.
---

# Corrector de Errores Gráficos

Soluciones para los 4 tipos de errores gráficos del ciclo de validación.

## ERR_G1: Gráficas No Visualizadas

**Síntoma**: `File '*.png' not found`

**Solución rápida**:
```r
# Cambiar de:
include_tikz(tikz_code, name="grafico", ...)

# A renderizado condicional:
if (knitr::is_latex_output()) {
  cat("\\begin{center}\n")
  cat(tikz_code)
  cat("\n\\end{center}\n")
} else {
  include_tikz(tikz_code, name="grafico", ...)
}
```

**Referencia completa**: `.claude/docs/patrones-errores-conocidos.md#error-1`

---

## ERR_G2: Gráficas Solapadas

**Síntoma**: Elementos superpuestos, texto sobre figuras

**Solución rápida**:
```r
# Agregar espaciado en R/exams
cat("\n\n")  # Antes del gráfico
# ... código del gráfico ...
cat("\n\n")  # Después del gráfico
```

**En TikZ**:
```latex
% Agregar espaciado vertical
\vspace{1cm}
\begin{tikzpicture}
  % Posicionamiento explícito
  \node at (0,0) {...};
  \node at (4,0) {...};  % Separar 4 unidades
\end{tikzpicture}
\vspace{1cm}
```

---

## ERR_G3: Renderizado Incorrecto

**Síntoma**: Gráfico distorsionado, colores incorrectos

**Diagnóstico**:
```bash
# Verificar paquetes TikZ
grep -n "usepackage.*tikz" archivo.Rmd
grep -n "usetikzlibrary" archivo.Rmd
```

**Solución - Paquetes necesarios**:
```yaml
header-includes:
- \usepackage{tikz}
- \usepackage{pgfplots}
- \usetikzlibrary{3d,babel}
```

**Solución - Python/matplotlib**:
```python
import matplotlib
matplotlib.use('Agg')  # Backend sin GUI
plt.savefig('grafico.png', dpi=150, bbox_inches='tight')
```

---

## ERR_G4: Tamaño Inadecuado

**Síntoma**: Gráfico muy grande o muy pequeño

**Solución TikZ - Escala global**:
```latex
\begin{tikzpicture}[scale=0.7]  % Reducir 30%
```

**Solución include_tikz**:
```r
include_tikz(tikz_code, width = "6cm")  # Ancho fijo
```

**Solución markdown**:
```markdown
![](grafico.png){width=40%}
```

**Solución Python**:
```python
fig, ax = plt.subplots(figsize=(5, 4))  # Dimensiones específicas
```

---

## Tabla de Referencia Rápida

| Error | Síntoma | Solución Clave |
|-------|---------|----------------|
| ERR_G1 | No visualizada | Renderizado condicional |
| ERR_G2 | Solapamiento | `\vspace{}` o `cat("\n\n")` |
| ERR_G3 | Distorsión | Verificar paquetes TikZ |
| ERR_G4 | Tamaño malo | `scale=`, `width=`, `figsize=` |

## Después de Corregir

1. Ejecutar `/validar-renderizado`
2. Verificar visualmente los 4 formatos
3. Si persiste error → revisar diagnóstico
4. Si éxito → continuar workflow

## Referencias

- `.claude/docs/patrones-errores-conocidos.md`
- `.claude/skills/corregir-graficos/skill.md`
- `.claude/skills/corregir-error-imagen/skill.md`

