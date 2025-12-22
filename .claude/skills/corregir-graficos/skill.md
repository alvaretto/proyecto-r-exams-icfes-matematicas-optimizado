---
name: corregir-graficos
description: Soluciona errores específicos de visualización gráfica (no visualizadas, solapadas, renderizado incorrecto, tamaño inadecuado).
---

# Skill: Corrector de Errores Gráficos

## Propósito
Aplicar correcciones específicas para los 4 tipos de errores gráficos identificados en el ciclo de validación visual.

## Tipos de Errores y Soluciones

### ERR_G1: Gráficas No Visualizadas

**Síntoma**: `File '*.png' not found` o imagen ausente en output

**Causa raíz**: `include_tikz()` en chunk de generación crea archivos temporales inaccesibles

**Solución**:
```r
# ❌ ANTES (incorrecto)
```{r generar, echo=FALSE, results="hide"}
include_tikz(tikz_code, name = "grafico", ...)
```

# ✅ DESPUÉS (correcto)
```{r mostrar, echo=FALSE, results='asis'}
if (knitr::is_latex_output()) {
  cat("\\begin{center}\n")
  cat(tikz_code)
  cat("\n\\end{center}\n")
} else {
  include_tikz(tikz_code, name = "grafico", ...)
}
```
```

### ERR_G2: Gráficas Solapadas

**Síntoma**: Elementos gráficos superpuestos, texto sobre figuras

**Causa raíz**: Posicionamiento incorrecto en TikZ o márgenes insuficientes

**Solución**:
```latex
% Agregar espaciado entre elementos
\vspace{1cm}  % Antes del gráfico

\begin{tikzpicture}
  % Usar posicionamiento absoluto
  \node at (0,0) {...};
  \node at (3,0) {...};  % Separar horizontalmente
\end{tikzpicture}

\vspace{1cm}  % Después del gráfico
```

**En markdown R/exams**:
```r
cat("\n\n")  # Doble salto antes
cat(tikz_code)
cat("\n\n")  # Doble salto después
```

### ERR_G3: Renderizado Incorrecto

**Síntoma**: Gráfico visible pero distorsionado, colores incorrectos, formas deformadas

**Diagnóstico**:
1. Verificar sintaxis TikZ
2. Revisar librerías cargadas
3. Comprobar coordenadas y escalas

**Solución TikZ**:
```latex
% Verificar paquetes necesarios
\usepackage{tikz}
\usepackage{pgfplots}
\usetikzlibrary{3d,babel}

% Usar escala consistente
\begin{tikzpicture}[scale=1.0]
  % Coordenadas precisas
  \draw (0,0) -- (3,0) -- (3,2) -- (0,2) -- cycle;
\end{tikzpicture}
```

**Solución Python/matplotlib**:
```python
# Configurar backend sin GUI
import matplotlib
matplotlib.use('Agg')

# DPI consistente
plt.savefig('grafico.png', dpi=150, bbox_inches='tight')
```

### ERR_G4: Tamaño Inadecuado

**Síntoma**: Gráfico demasiado grande/pequeño respecto al escenario

**Solución TikZ**:
```latex
% Ajustar escala global
\begin{tikzpicture}[scale=0.8]  % Reducir 20%

% O especificar dimensiones en include_tikz
include_tikz(tikz_code, width = "6cm")  % Ancho específico
```

**Solución Python**:
```python
# Controlar tamaño de figura
fig, ax = plt.subplots(figsize=(6, 4))  # 6x4 pulgadas
```

**Solución en markdown**:
```markdown
![](grafico.png){width=40%}  # Porcentaje del ancho
```

## Algoritmo de Corrección

```
Identificar tipo de error (ERR_G1/G2/G3/G4)
    ↓
Localizar chunk problemático
    ↓
Aplicar patrón de corrección específico
    ↓
Guardar archivo .Rmd modificado
    ↓
Re-ejecutar validar-renderizado
    ↓
┌──────────────────────┐
│ ¿Error corregido?    │
│   Sí → Continuar     │
│   No → Iterar        │
└──────────────────────┘
```

## Checklist de Corrección

- [ ] Identificar código de error (ERR_G1-G4)
- [ ] Localizar línea/chunk problemático
- [ ] Aplicar solución del patrón correspondiente
- [ ] Verificar sintaxis del código modificado
- [ ] Ejecutar renderizado de prueba
- [ ] Confirmar visualización correcta
- [ ] Documentar corrección aplicada

## Integración

- **Activado por**: diagnosticar-errores cuando categoría = GRÁFICOS
- **Siguiente paso**: validar-renderizado (ciclo hasta éxito)

## Referencias

- `.claude/docs/patrones-errores-conocidos.md` (Error 1, 3)
- `.claude/skills/corregir-error-imagen/skill.md`
- `.claude/agents/graficador-tikz.md`

