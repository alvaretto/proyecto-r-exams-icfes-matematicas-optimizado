---
name: validar-renderizado
description: Ejecuta 🔄 FASE 1 del Ciclo de Validación Automática - Renderizado completo exams2*.
---

# Skill: 🔄 FASE 1 - Renderizado Inicial

## ⚡ CONTEXTO: Ciclo de Validación y Corrección Automática

Este skill ejecuta la **FASE 1: RENDERIZADO INICIAL** del ciclo obligatorio:

```
🔄 FASE 1: RENDERIZADO INICIAL ← ESTE SKILL
    │
    ▼
🔍 FASE 2: Validación Visual y Funcional
    │
    ▼
⚡ FASE 3: Decisión y Acción
    ├── 📚 SUBFASE 3A: Corrección basada en ejemplos
    ├── 🔄 SUBFASE 3B: Revalidación (volver aquí)
    └── 📊 SUBFASE 3C: Documentar solución
```

## Propósito
Ejecutar el renderizado completo en todos los formatos de R/exams y capturar
TODOS los errores y advertencias antes de continuar con FASE 2.

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

### Paso 2: Ejecutar renderizado secuencial con captura de errores
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

### Paso 3: Generar reporte de FASE 1
```
═══════════════════════════════════════════════════════════
🔄 FASE 1: RENDERIZADO INICIAL - REPORTE
═══════════════════════════════════════════════════════════

Formato    | Resultado | Archivo Generado
-----------|-----------|------------------
HTML       | ✅/❌     | ruta/archivo.html
PDF        | ✅/❌     | ruta/archivo.pdf
DOCX       | ✅/❌     | ruta/archivo.docx
NOPS       | ✅/❌     | ruta/archivo_nops.pdf

Tasa de éxito: X de 4 formatos (XX%)

Errores capturados: [Lista de errores]
Advertencias: [Lista de advertencias]

→ SIGUIENTE: FASE 2 - Validación Visual y Funcional
═══════════════════════════════════════════════════════════
```

## Flujo de Decisión Post-FASE 1

```
Renderizado 100% exitoso
    → Continuar a FASE 2: Validación Visual y Funcional

Renderizado con errores (cualquier %)
    → Continuar a FASE 2 con errores registrados
    → Errores se procesarán en FASE 3
```

## ⛔ CONDICIONES CRÍTICAS

1. ✓ SIEMPRE ejecutar los 4 formatos
2. ✓ SIEMPRE capturar y registrar errores
3. ✓ SIEMPRE continuar a FASE 2 (incluso con errores)
4. ❌ NUNCA omitir formatos de renderizado

## Integración con Ciclo Completo

- **Este skill** → Ejecuta FASE 1
- **validar-coherencia** → Ejecuta FASE 2
- **diagnosticar-errores** → Inicia FASE 3 si hay errores
- **SUBFASE 3B** → Vuelve a ejecutar este skill

## Referencias

- `.claude/Mermaid_Chart.txt` (diagrama de flujo oficial)
- `/A-Produccion/Ejemplos-Funcionales-Rmd/` (fuente de verdad)
- `.claude/docs/patrones-errores-conocidos.md`
- `.claude/docs/TRES_NIVELES_VALIDACION.md`

## Ejecución

Cuando el usuario invoca `/validar-renderizado`:

1. Identificar archivo .Rmd objetivo
2. Ejecutar renderizado en los 4 formatos (FASE 1)
3. Capturar TODOS los errores y mensajes
4. Generar reporte de FASE 1
5. Continuar automáticamente a FASE 2 (validar-coherencia)

