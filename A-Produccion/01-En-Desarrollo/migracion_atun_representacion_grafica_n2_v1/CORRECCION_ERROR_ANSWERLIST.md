# ⚠️ CORRECCIÓN DE ERROR - Answerlist mostrando texto literal

## 📅 Fecha: 2025-12-25

## 🔴 Error Detectado

**Síntoma**: El chunk `answerlist_graficas` mostraba el texto literal de los nombres de archivo en lugar de las imágenes:

```
[1] "grafica_opcion_a.png" [1] "grafica_opcion_b.png" [1] "grafica_opcion_c.png" [1] "grafica_opcion_d.png"
```

**Captura de error proporcionada por usuario**: Imagen mostrando el output literal en la salida HTML.

## 🔍 Diagnóstico

### Causa Raíz

Las llamadas a `include_tikz()` estaban **dentro del mismo chunk** que genera el Answerlist (`results='asis'`). Esto causaba que el valor de retorno de `include_tikz()` (el nombre del archivo generado) se imprimiera como output visible.

### Código Problemático

```r
```{r answerlist_graficas, echo=FALSE, results='asis'}
# Renderizar cada gráfica por separado
fmt_tikz <- if (identical(typ, "nops")) "pdf" else if (identical(typ, "pandoc")) "html" else typ

# Gráfica A
include_tikz(codigo_grafica_a, name = "grafica_opcion_a", ...)  # ← Retorna nombre de archivo
# Gráfica B
include_tikz(codigo_grafica_b, name = "grafica_opcion_b", ...)  # ← Retorna nombre de archivo
# Gráfica C
include_tikz(codigo_grafica_c, name = "grafica_opcion_c", ...)  # ← Retorna nombre de archivo
# Gráfica D
include_tikz(codigo_grafica_d, name = "grafica_opcion_d", ...)  # ← Retorna nombre de archivo

# Generar Answerlist
cat("Answerlist\n----------\n\n")
cat("- ![](grafica_opcion_a.", extension, "){width=60%}\n\n", sep="")
...
```
```

**Problema**: Con `results='asis'`, todos los valores de retorno se imprimen literalmente.

## ✅ Solución Implementada

### Patrón Correcto (basado en ejemplos funcionales)

Separar el renderizado de imágenes y la generación del Answerlist en **dos chunks distintos**:

1. **Chunk de renderizado** (`results="hide"`): Genera los archivos sin mostrar output
2. **Chunk de Answerlist** (`results='asis'`): Solo contiene los `cat()` para referencias markdown

### Código Corregido

```r
```{r renderizar_graficas, echo=FALSE, results="hide"}
# Renderizar cada gráfica por separado - DEBE ejecutarse ANTES del Answerlist
fmt_tikz <- if (identical(typ, "nops")) "pdf" else if (identical(typ, "pandoc")) "html" else typ

# Gráfica A
include_tikz(codigo_grafica_a, name = "grafica_opcion_a", markup = "none",
             format = fmt_tikz, packages = c("tikz", "pgfplots"), width = "10cm")

# Gráfica B
include_tikz(codigo_grafica_b, name = "grafica_opcion_b", markup = "none",
             format = fmt_tikz, packages = c("tikz", "pgfplots"), width = "10cm")

# Gráfica C
include_tikz(codigo_grafica_c, name = "grafica_opcion_c", markup = "none",
             format = fmt_tikz, packages = c("tikz", "pgfplots"), width = "10cm")

# Gráfica D
include_tikz(codigo_grafica_d, name = "grafica_opcion_d", markup = "none",
             format = fmt_tikz, packages = c("tikz", "pgfplots"), width = "10cm")
```
```

**Nuevo chunk separado** para Answerlist:

```r
```{r answerlist_graficas, echo=FALSE, results='asis'}
# Determinar extensión de archivo según formato de salida
extension <- if (identical(typ, "pdf") || identical(typ, "nops") || identical(typ, "tex")) "pdf" else "png"

# Generar Answerlist con las imágenes de las gráficas ya renderizadas
cat("Answerlist\n")
cat("----------\n\n")
cat("- ![](grafica_opcion_a.", extension, "){width=60%}\n\n", sep="")
cat("- ![](grafica_opcion_b.", extension, "){width=60%}\n\n", sep="")
cat("- ![](grafica_opcion_c.", extension, "){width=60%}\n\n", sep="")
cat("- ![](grafica_opcion_d.", extension, "){width=60%}\n\n", sep="")
```
```

## 🎯 Referencia: Ejemplo Funcional

El patrón correcto se tomó de:

`/A-Produccion/En-Produccion/06-Estadística-Y-Probabilidad/.../probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_v1.Rmd`

En ese ejemplo, utilizan una función auxiliar `generar_tabla_multi_formato()` que encapsula las llamadas a `include_tikz()`, pero el principio es el mismo: **renderizar primero, referenciar después**.

## ✅ Verificación Post-Corrección

### FASE 1: Renderizado

```
📄 HTML:   ✅ EXITOSO
📄 PDF:    ✅ EXITOSO
📄 DOCX:   ✅ EXITOSO
📄 NOPS:   ✅ EXITOSO
```

### Archivos Generados

```bash
$ ls -1 *.png
grafica_opcion_a.png
grafica_opcion_b.png
grafica_opcion_c.png
grafica_opcion_d.png
```

### Verificación en HTML

Las imágenes ahora se muestran correctamente como gráficas embebidas (base64) sin texto literal visible.

## 📚 Lecciones Aprendidas

### Regla de Oro para R-exams

**NUNCA** llamar `include_tikz()` directamente en un chunk con `results='asis'` que genere Answerlist.

### Patrón Correcto

1. **Chunk 1** (`results="hide"`): Renderizar todas las imágenes TikZ
2. **Chunk 2** (`results='asis'`): Generar solo las referencias markdown

### Error Común

```r
# ❌ MAL: Mezlar renderizado y Answerlist
```{r answerlist, echo=FALSE, results='asis'}
include_tikz(codigo_a, ...)  # Esto imprime el nombre del archivo
cat("- ![](imagen_a.png)\n")
```
```

```r
# ✅ BIEN: Separar renderizado y Answerlist
```{r render, echo=FALSE, results="hide"}
include_tikz(codigo_a, ...)  # Sin output visible
```

```{r answerlist, echo=FALSE, results='asis'}
cat("- ![](imagen_a.png)\n")  # Solo referencias
```
```

## 🔗 Archivos Modificados

- `migracion_atun_representacion_grafica_aleatorio_interpretacion_n2_v1.Rmd` (líneas 260-303)

## ✅ Estado Final

**ERROR CORREGIDO** - El ejercicio ahora renderiza correctamente las 4 gráficas como imágenes en las opciones de respuesta del Answerlist.

---

**Reportado por**: Usuario
**Corregido por**: Claude Sonnet 4.5
**Fecha de corrección**: 2025-12-25 23:35 UTC
