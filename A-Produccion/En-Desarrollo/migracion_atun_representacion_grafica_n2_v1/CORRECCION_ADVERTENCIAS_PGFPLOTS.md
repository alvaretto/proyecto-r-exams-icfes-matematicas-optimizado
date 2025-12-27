# ⚠️ CORRECCIÓN DE ADVERTENCIAS - pgfplots compatibilidad

## 📅 Fecha: 2025-12-25

## 🔴 Advertencia Reportada

**Síntoma**: Múltiples advertencias durante la compilación:

```
Package pgfplots Warning: running in backwards compatibility mode
(unsuitable tick labels; missing features). Consider writing
\pgfplotsset{compat=1.18} into your preamble.
```

**Frecuencia**: Una advertencia por cada gráfica generada (4 advertencias en total).

## 🔍 Diagnóstico

### Causa Raíz

Aunque el archivo `.Rmd` tenía la configuración correcta en el `header-includes`:

```yaml
header-includes:
- \pgfplotsset{compat=1.18}
```

Esta configuración solo aplica al **documento principal** `.tex`. Cuando `include_tikz()` genera gráficas, crea **documentos standalone separados** que no heredan el `header-includes` del documento principal.

**Flujo del problema**:
1. `include_tikz()` genera documento TikZ standalone para cada gráfica
2. Cada documento standalone tiene su propio preámbulo
3. El preámbulo standalone NO incluye `\pgfplotsset{compat=1.18}`
4. pgfplots detecta falta de configuración → Genera advertencia

## ✅ Solución Aplicada

### Pasar `header` explícitamente a cada `include_tikz()`

Agregar el parámetro `header = "\\pgfplotsset{compat=1.18}"` en cada llamada a `include_tikz()`:

**Código ANTES**:
```r
include_tikz(codigo_grafica_a, name = "grafica_opcion_a", markup = "none",
             format = fmt_tikz, packages = c("tikz", "pgfplots"), width = "10cm")
```

**Código DESPUÉS**:
```r
include_tikz(codigo_grafica_a, name = "grafica_opcion_a", markup = "none",
             format = fmt_tikz, packages = c("tikz", "pgfplots"),
             header = "\\pgfplotsset{compat=1.18}", width = "10cm")
```

### Aplicado a las 4 gráficas

Las líneas modificadas (265-282):

```r
```{r renderizar_graficas, echo=FALSE, results="hide"}
fmt_tikz <- if (identical(typ, "nops")) "pdf" else if (identical(typ, "pandoc")) "html" else typ

# Gráfica A
include_tikz(codigo_grafica_a, name = "grafica_opcion_a", markup = "none",
             format = fmt_tikz, packages = c("tikz", "pgfplots"),
             header = "\\pgfplotsset{compat=1.18}", width = "10cm")

# Gráfica B
include_tikz(codigo_grafica_b, name = "grafica_opcion_b", markup = "none",
             format = fmt_tikz, packages = c("tikz", "pgfplots"),
             header = "\\pgfplotsset{compat=1.18}", width = "10cm")

# Gráfica C
include_tikz(codigo_grafica_c, name = "grafica_opcion_c", markup = "none",
             format = fmt_tikz, packages = c("tikz", "pgfplots"),
             header = "\\pgfplotsset{compat=1.18}", width = "10cm")

# Gráfica D
include_tikz(codigo_grafica_d, name = "grafica_opcion_d", markup = "none",
             format = fmt_tikz, packages = c("tikz", "pgfplots"),
             header = "\\pgfplotsset{compat=1.18}", width = "10cm")
```
```

## ✅ Verificación

### Test HTML
```bash
$ Rscript -e 'library(exams); exams2html("...", n=1, dir="test/html")' 2>&1 | grep -c "pgfplots Warning"
0
```

✅ **0 advertencias** en generación HTML

### Test PDF
```bash
$ Rscript -e 'library(exams); exams2pdf("...", n=1, dir="test/pdf")' 2>&1 | grep -c "pgfplots Warning"
0
```

✅ **0 advertencias** en generación PDF

### Archivos Generados

```
test/html/plain1.html: 218 KB ✅
test/pdf/plain1.pdf: 152 KB ✅
```

## 📚 Lección Aprendida

### Regla General para `include_tikz()` con pgfplots

**Siempre** pasar el parámetro `header` cuando se usa pgfplots en gráficas TikZ:

```r
include_tikz(codigo_tikz,
             name = "nombre_grafica",
             packages = c("tikz", "pgfplots"),
             header = "\\pgfplotsset{compat=1.18}",  # ← OBLIGATORIO
             ...)
```

### ¿Por qué `header-includes` no es suficiente?

| Contexto | `header-includes` | Parámetro `header` |
|----------|-------------------|-------------------|
| Documento principal `.Rmd` | ✅ Aplica | ❌ No necesario |
| Documentos TikZ standalone | ❌ No aplica | ✅ **OBLIGATORIO** |

## 🎯 Casos Aplicables

Esta solución aplica para:
- ✅ Cualquier uso de `include_tikz()` con paquete `pgfplots`
- ✅ Ejercicios con múltiples gráficas TikZ
- ✅ Gráficas de dispersión, barras, líneas, etc. con pgfplots
- ✅ Prevención de advertencias en compilación

## 📅 Historial

| Fecha | Acción | Resultado |
|-------|--------|-----------|
| 2025-12-25 23:43 | Agregado `header` a 4 llamadas `include_tikz()` | ✅ 0 advertencias |
| 2025-12-25 | Reporte inicial de usuario | ⚠️ "Muuuuuchas" advertencias |

---

**Corrección aplicada por**: Claude Sonnet 4.5
**Verificación**: 2025-12-25 23:43 UTC
**Estado**: ✅ RESUELTO
