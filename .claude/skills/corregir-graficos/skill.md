---
name: corregir-graficos
description: Ejecuta 📚 SUBFASE 3A para errores gráficos - Corrección basada en ejemplos funcionales.
---

# Skill: 📚 SUBFASE 3A - Corrección de Errores Gráficos

## ⚡ CONTEXTO: Ciclo de Validación y Corrección Automática

Este skill ejecuta la **SUBFASE 3A: CORRECCIÓN BASADA EN EJEMPLOS** para errores gráficos:

```
⚡ FASE 3: Decisión y Acción
    │
    └── ✓ CON ERRORES GRÁFICOS:
            │
            ├── 📚 SUBFASE 3A: Corrección basada en ejemplos ← ESTE SKILL
            │       ↓
            ├── 🔄 SUBFASE 3B: Revalidación (volver a FASE 1)
            │
            └── 📊 SUBFASE 3C: Documentar solución
```

## Propósito
Aplicar correcciones específicas para errores gráficos (ERR_G1-G4) basándose
OBLIGATORIAMENTE en ejemplos funcionales.

## ⚠️ PASO OBLIGATORIO: Consultar Ejemplos Funcionales

**ANTES de aplicar cualquier corrección:**

```bash
# Consultar ejemplos funcionales con TikZ
ls /A-Produccion/Ejemplos-Funcionales-Rmd/

# Buscar patrones de include_tikz
grep -l "include_tikz" /A-Produccion/Ejemplos-Funcionales-Rmd/*.Rmd

# Buscar patrones de renderizado condicional
grep -l "is_latex_output" /A-Produccion/Ejemplos-Funcionales-Rmd/*.Rmd
```

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

## Checklist de Corrección (SUBFASE 3A)

- [ ] 📚 Consultar ejemplos funcionales PRIMERO
- [ ] Identificar código de error (ERR_G1-G4)
- [ ] Localizar línea/chunk problemático
- [ ] Extraer patrón de solución de ejemplo funcional
- [ ] Aplicar corrección basada en ejemplo
- [ ] Verificar sintaxis del código modificado

## 🔄 SUBFASE 3B: Revalidación Obligatoria

**DESPUÉS de aplicar correcciones:**

```
⚠️ OBLIGATORIO: Volver automáticamente a FASE 1
→ Ejecutar validar-renderizado
→ Ejecutar validar-coherencia
→ Verificar que error gráfico está resuelto
→ REPETIR si persisten errores
```

## 📊 SUBFASE 3C: Documentar Solución (Solo si éxito)

**Solo después de revalidación exitosa:**

1. Documentar error y solución en `.claude/docs/patrones-errores-conocidos.md`
2. Incluir ejemplo funcional utilizado
3. Registrar código antes/después

## ⛔ CONDICIONES CRÍTICAS

1. ❌ **NO terminar** con errores gráficos sin resolver
2. ✓ **SIEMPRE** consultar ejemplos funcionales ANTES de corregir
3. ✓ **SIEMPRE** ejecutar SUBFASE 3B después de correcciones
4. ✓ **Ejemplos funcionales** = Fuente de verdad ABSOLUTA

## Integración con Ciclo Completo

- **diagnosticar-errores** → Activa este skill para ERR_G
- **Este skill** → Ejecuta SUBFASE 3A para gráficos
- **SUBFASE 3B** → Vuelve a FASE 1 (validar-renderizado)

## Referencias

- `/A-Produccion/Ejemplos-Funcionales-Rmd/` (FUENTE DE VERDAD)
- `.claude/Mermaid_Chart.txt` (diagrama de flujo oficial)
- `.claude/docs/patrones-errores-conocidos.md` (Error 1, 3)
- `.claude/skills/corregir-error-imagen/skill.md`
- `.claude/agents/graficador-tikz.md`

