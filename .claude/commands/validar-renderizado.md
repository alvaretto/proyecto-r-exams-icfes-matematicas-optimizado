---
description: Ejecuta ciclo completo de renderizado exams2* y valida todos los formatos de salida.
---

# Validador de Renderizado exams2*

Ejecuta renderizado en 4 formatos y reporta resultados.

## Uso Rápido

```bash
# Desde la carpeta del ejercicio
Rscript -e 'source("SemilleroUnico_v2.R")'
```

## Validación Manual por Formato

### HTML
```r
library(exams)
exams2html("ejercicio.Rmd", n = 1, dir = "test_html")
# Verificar: test_html/plain1.html
```

### PDF
```r
exams2pdf("ejercicio.Rmd", n = 1, dir = "test_pdf", template = "plain")
# Verificar: test_pdf/plain1.pdf
```

### DOCX
```r
exams2pandoc("ejercicio.Rmd", n = 1, dir = "test_docx", type = "docx")
# Verificar: test_docx/plain1.docx
```

### NOPS
```r
exams2nops("ejercicio.Rmd", n = 1, dir = "test_nops")
# Verificar: test_nops/nops1.pdf
```

## Script de Validación Completa

```r
# Guardar como test_renderizado.R
library(exams)
set.seed(123)

archivo <- "ejercicio.Rmd"
resultados <- list()

# HTML
resultados$html <- tryCatch({
  exams2html(archivo, n=1, dir="test/html")
  "✅ EXITOSO"
}, error = function(e) paste("❌", e$message))

# PDF
resultados$pdf <- tryCatch({
  exams2pdf(archivo, n=1, dir="test/pdf")
  "✅ EXITOSO"
}, error = function(e) paste("❌", e$message))

# DOCX
resultados$docx <- tryCatch({
  exams2pandoc(archivo, n=1, dir="test/docx", type="docx")
  "✅ EXITOSO"
}, error = function(e) paste("❌", e$message))

# NOPS
resultados$nops <- tryCatch({
  exams2nops(archivo, n=1, dir="test/nops")
  "✅ EXITOSO"
}, error = function(e) paste("❌", e$message))

# Reporte
cat("\n=== REPORTE DE RENDERIZADO ===\n")
cat("HTML:", resultados$html, "\n")
cat("PDF:", resultados$pdf, "\n")
cat("DOCX:", resultados$docx, "\n")
cat("NOPS:", resultados$nops, "\n")
```

## Interpretación de Resultados

| Resultado | Acción |
|-----------|--------|
| 4/4 ✅ | Continuar a inspección visual |
| 3/4 ✅ | Diagnosticar formato fallido |
| <3/4 ✅ | Ejecutar `/diagnosticar-errores` |

## Errores Comunes

### Error: File 'imagen.png' not found
```
→ Ejecutar: /corregir-error-imagen
→ Ver: .claude/docs/patrones-errores-conocidos.md#error-1
```

### Error: LaTeX failed to compile
```
→ Verificar paquetes en header-includes
→ Revisar sintaxis TikZ
→ Ver: .claude/docs/patrones-errores-conocidos.md
```

## Siguiente Paso

Después de validación exitosa:
1. Inspección visual de outputs
2. Verificar gráficos correctos
3. Ejecutar `/validar-coherencia`

