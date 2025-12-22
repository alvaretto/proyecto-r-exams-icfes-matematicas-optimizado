---
name: validar-renderizado
description: Ejecuta ciclo completo de renderizado exams2* (html, pdf, docx, nops) y valida outputs visuales.
---

# Skill: Validador de Renderizado exams2*

## Propósito
Ejecutar el ciclo completo de renderizado en todos los formatos de R/exams y validar que los outputs se generan correctamente antes de continuar con el workflow.

## Formatos a Validar

| Formato | Función | Validación |
|---------|---------|------------|
| HTML | `exams2html()` | Visualización web correcta |
| PDF | `exams2pdf()` | Compilación LaTeX exitosa |
| DOCX | `exams2pandoc()` | Documento Word generado |
| NOPS | `exams2nops()` | Examen escaneable listo |

## Algoritmo de Ejecución

### Paso 1: Preparar entorno
```r
library(exams)
set.seed(sample(1:100000, 1))
archivo_rmd <- "nombre_ejercicio.Rmd"
```

### Paso 2: Ejecutar renderizado secuencial
```r
# HTML
resultado_html <- tryCatch(
  exams2html(archivo_rmd, n = 1, dir = "output/html"),
  error = function(e) list(error = TRUE, mensaje = e$message)
)

# PDF
resultado_pdf <- tryCatch(
  exams2pdf(archivo_rmd, n = 1, dir = "output/pdf", template = "plain"),
  error = function(e) list(error = TRUE, mensaje = e$message)
)

# DOCX
resultado_docx <- tryCatch(
  exams2pandoc(archivo_rmd, n = 1, dir = "output/docx", type = "docx"),
  error = function(e) list(error = TRUE, mensaje = e$message)
)

# NOPS
resultado_nops <- tryCatch(
  exams2nops(archivo_rmd, n = 1, dir = "output/nops"),
  error = function(e) list(error = TRUE, mensaje = e$message)
)
```

### Paso 3: Generar reporte de validación
```
Formato    | Resultado | Archivo Generado
-----------|-----------|------------------
HTML       | ✅/❌     | ruta/archivo.html
PDF        | ✅/❌     | ruta/archivo.pdf
DOCX       | ✅/❌     | ruta/archivo.docx
NOPS       | ✅/❌     | ruta/archivo_nops.pdf

Tasa de éxito: X de 4 formatos (XX%)
```

## Criterios de Éxito

- ✅ **100%**: Todos los formatos compilan sin errores
- ⚠️ **75%+**: Al menos 3 formatos funcionan
- ❌ **<75%**: Requiere corrección inmediata

## Flujo de Decisión

```
Renderizado exitoso (100%)
    → Continuar a Inspección Visual
    
Renderizado parcial (75%+)
    → Diagnosticar errores en formatos fallidos
    → Aplicar correcciones
    → Re-ejecutar validación
    
Renderizado fallido (<75%)
    → Activar skill diagnosticar-errores
    → Clasificar tipo de error
    → Aplicar corrección específica
    → Re-ejecutar ciclo completo
```

## Integración con Otros Skills

- **diagnosticar-errores**: Se activa automáticamente si hay errores
- **corregir-graficos**: Se activa si errores son de tipo gráfico
- **validar-coherencia**: Se ejecuta después de validación exitosa

## Referencias

- `.claude/docs/patrones-errores-conocidos.md`
- `.claude/docs/TRES_NIVELES_VALIDACION.md`
- `.claude/Mermaid_Chart.txt` (diagrama de flujo)

## Ejecución

Cuando el usuario invoca `/validar-renderizado`:

1. Identificar archivo .Rmd objetivo
2. Ejecutar renderizado en los 4 formatos
3. Capturar errores y mensajes
4. Generar reporte de validación
5. Si hay errores → activar diagnóstico
6. Si éxito total → confirmar y sugerir inspección visual

