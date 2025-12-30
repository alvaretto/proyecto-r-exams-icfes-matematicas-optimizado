---
name: diagnosticar-errores
description: >
  Ejecuta FASE 3 del Ciclo de Validación - Diagnostica errores, clasifica por categoría,
  activa SUBFASE 3A (corrección ejemplos), SUBFASE 3B (revalidación FASE 1).
  Usa automáticamente cuando FASE 1 o FASE 2 detectan errores, o cuando compilación falla.
  Loop hasta resolver TODOS los errores.

allowed-tools:
  - Read
  - Write
  - Edit
  - Grep
  - Glob
  - Bash(grep:*)
  - Bash(find:*)
  - Bash(cat:*)
---

# Diagnosticador de Errores R/exams - FASE 3

## 🎯 Propósito de este Skill

Este skill ejecuta la **FASE 3: DECISIÓN Y ACCIÓN** del Ciclo de Validación. Es el **skill de debugging crítico** que analiza errores, los clasifica, propone soluciones y activa las SUBFASES de corrección.

### Cuándo usar este skill

**Triggers automáticos** (Claude lo usa cuando detecta):
- FASE 1 (validar-renderizado) reporta errores en cualquier formato
- FASE 2 (validar-coherencia) detecta incoherencias
- Usuario reporta "error de compilación", "no renderiza", "sale error"
- Mensaje de error R/exams en conversación
- Advertencias (warnings) críticas acumuladas

**Invocación manual:**
```
/diagnosticar-errores archivo.Rmd
/diagnosticar-errores  # Diagnostica último error reportado
```

**Invocación automática:**
- Se ejecuta automáticamente si FASE 1 o FASE 2 detectan errores
- Parte integral del Ciclo de Validación

## 📋 Contexto: Ciclo de Validación Completo

Este skill es la FASE 3 del ciclo de 3 fases:

```
🔄 FASE 1: RENDERIZADO INICIAL (validar-renderizado)
    ↓
🔍 FASE 2: VALIDACIÓN VISUAL Y FUNCIONAL (validar-coherencia)
    ↓
⚡ FASE 3: DECISIÓN Y ACCIÓN ← ESTE SKILL
    │
    ├── ❌ SIN ERRORES → ✅ Aprobar para producción
    │
    └── ✓ CON ERRORES:
            │
            ├── 📚 SUBFASE 3A: Corrección basada en ejemplos
            │      ├─→ Consultar /A-Produccion/Ejemplos-Funcionales-Rmd/
            │      ├─→ Identificar patrón de solución
            │      └─→ Aplicar corrección
            │
            ├── 🔄 SUBFASE 3B: Revalidación OBLIGATORIA
            │      └─→ Volver a FASE 1 (LOOP)
            │
            └── 📊 SUBFASE 3C: Documentar solución (si éxito)
                   └─→ Actualizar patrones-errores-conocidos.md
```

Ver ciclo completo en: @.claude/rules/ciclo-validacion.md

## 📊 Categorías de Errores (4 Tipos)

### Categoría 1: ERRORES DE GRÁFICOS (ERR_G)

**Prefijo:** `ERR_G`

| Código | Error | Síntomas | Patrón Detección | Solución |
|--------|-------|----------|------------------|----------|
| **ERR_G1** | Gráficas no visualizadas | Espacio en blanco donde debe ir gráfico | `File '*.png' not found` | include_tikz() condicional |
| **ERR_G2** | Gráficas solapadas | Gráficos se superponen | Inspección visual | Ajustar posicionamiento |
| **ERR_G3** | Renderizado incorrecto | Gráfico visible pero distorsionado | Valores incorrectos en plot | Revisar código TikZ/Python/R |
| **ERR_G4** | Tamaño inadecuado | Gráfico muy grande/pequeño | Desborda página / ilegible | Ajustar scale/width/height |
| **ERR_G5** | Leyenda faltante/incorrecta | Sin leyenda o valores mal | Inspección visual | Agregar/corregir legend() |
| **ERR_G6** | Ejes sin labels | Ejes sin nombre de variable | Inspección visual | Agregar xlab(), ylab() |

**Skill de corrección:** `corregir-graficos` (SUBFASE 3A)

### Categoría 2: ERRORES DE TEXTO (ERR_T)

**Prefijo:** `ERR_T`

| Código | Error | Síntomas | Patrón Detección | Solución |
|--------|-------|----------|------------------|----------|
| **ERR_T1** | LaTeX no compila | PDF falla | `LaTeX Error:` `! Undefined control sequence` | Revisar sintaxis LaTeX |
| **ERR_T2** | Encoding incorrecto | Caracteres raros (Ã©, Ã±) | `invalid multibyte string` | encoding = "UTF-8" |
| **ERR_T3** | Metadatos faltantes | Advertencia missing exname | `exname`, `extype` ausentes | Completar Meta-information |
| **ERR_T4** | Ecuaciones mal formateadas | Ecuación no renderiza | `$ $` sin cerrar, sintaxis LaTeX | Revisar delimitadores LaTeX |
| **ERR_T5** | Caracteres especiales | Error LaTeX por %, _, # | `! LaTeX Error: Missing \begin` | Escapar: \%, \_, \# |

**Skill de corrección:** Manual (Edit tool)

### Categoría 3: ERRORES DE ESTRUCTURA (ERR_S)

**Prefijo:** `ERR_S`

| Código | Error | Síntomas | Patrón Detección | Solución |
|--------|-------|----------|------------------|----------|
| **ERR_S1** | Opciones incorrectas | <4 opciones o duplicados | `length(answerlist) < 4` | Regenerar opciones |
| **ERR_S2** | Solución no coincide | exsolution incorrecto | Manual verification | Recalcular respuesta |
| **ERR_S3** | Answerlist faltante | Sin lista de respuestas | Grep `Answerlist` → not found | Agregar Answerlist |
| **ERR_S4** | CLOZE gaps incorrectos | ##ANSWER1## mal numerados | Gaps salteados (1, 3, 4) | Renumerar secuencial |
| **ERR_S5** | extol/exclozetype desincronizado | Número elementos ≠ gaps | Count mismatch | Igualar longitudes |

**Skill de corrección:** Manual (Edit tool) + validación

### Categoría 4: ERRORES DE COHERENCIA (ERR_C)

**Prefijo:** `ERR_C`

| Código | Error | Síntomas | Patrón Detección | Solución |
|--------|-------|----------|------------------|----------|
| **ERR_C1** | Coherencia matemática | Cálculo incorrecto | Manual verification | Revisar lógica |
| **ERR_C2** | Coherencia imagen-texto | Descripción ≠ gráfico | Inspección visual | Alinear texto/visual |
| **ERR_C3** | Coherencia de código | Variables R/Python desincronizadas | `non-numeric argument` | Sincronizar parámetros |
| **ERR_C4** | Coherencia opciones | Distractores ilógicos | Todas opciones cercanas | Diversificar distractores |
| **ERR_C5** | Coherencia metadatos | Metadatos ≠ contenido | Nivel 1 pero problema complejo | Alinear clasificación |

**Skill de corrección:** Manual (revisión completa)

## 🔍 Algoritmo de Diagnóstico (Decision Tree)

### Diagrama de Decisión

```
Error reportado
    │
    ├─→ ¿Contiene "File.*png.*not found"?
    │    └─→ SÍ → ERR_G1 (Gráficas no visualizadas)
    │
    ├─→ ¿Contiene "LaTeX Error" o "! Undefined"?
    │    └─→ SÍ → ERR_T1 (LaTeX no compila)
    │
    ├─→ ¿Contiene "invalid multibyte"?
    │    └─→ SÍ → ERR_T2 (Encoding incorrecto)
    │
    ├─→ ¿Contiene "object.*not found"?
    │    └─→ SÍ → ERR_C3 (Variable R no definida)
    │
    ├─→ ¿Contiene "exercise type 'cloze'.*not supported"?
    │    └─→ SÍ → ERR_S5 (CLOZE incompatible con NOPS)
    │
    ├─→ ¿Contiene "non-numeric argument"?
    │    └─→ SÍ → ERR_C3 (Coherencia de código)
    │
    ├─→ ¿Contiene "exsolution"?
    │    └─→ SÍ → ERR_S2 (Solución incorrecta)
    │
    └─→ NO → Analizar archivo .Rmd completo
            │
            ├─→ ¿Falta "exname:"? → ERR_T3
            ├─→ ¿Answerlist < 4 items? → ERR_S1
            ├─→ ¿Gaps CLOZE no secuenciales? → ERR_S4
            └─→ Categorizar como ERR_X (Desconocido)
```

### Código de Diagnóstico Completo

```r
#!/usr/bin/env Rscript

diagnosticar_error <- function(mensaje_error, archivo_rmd = NULL, contexto = NULL) {

  # PASO 1: Diagnóstico por patrón de mensaje

  # Gráficos
  if (grepl("File.*\\.png.*not found|include_tikz.*failed", mensaje_error, ignore.case = TRUE)) {
    return(list(
      categoria = "GRÁFICOS",
      codigo = "ERR_G1",
      descripcion = "Gráficas no visualizadas",
      causa = "include_tikz() no funciona o ruta incorrecta",
      skill_correccion = "corregir-graficos",
      prioridad = "ALTA",
      solucion_sugerida = "Usar renderizado condicional: if (knitr::is_latex_output())"
    ))
  }

  # LaTeX
  if (grepl("LaTeX Error|! Undefined control sequence|! Missing", mensaje_error)) {
    # Detectar tipo específico de error LaTeX
    if (grepl("tikz\\.sty|pgfplots\\.sty", mensaje_error)) {
      return(list(
        categoria = "TEXTO",
        codigo = "ERR_T1",
        descripcion = "Paquete LaTeX faltante",
        causa = "header-includes no tiene \\usepackage{tikz}",
        skill_correccion = "manual-edit",
        prioridad = "CRÍTICA",
        solucion_sugerida = "Agregar en YAML:\nheader-includes:\n  - \\usepackage{tikz}"
      ))
    } else if (grepl("babel.*spanish", mensaje_error)) {
      return(list(
        categoria = "TEXTO",
        codigo = "ERR_T1",
        descripcion = "Babel/Spanish no configurado",
        causa = "Paquete babel español faltante",
        skill_correccion = "manual-edit",
        prioridad = "CRÍTICA",
        solucion_sugerida = "Agregar:\n\\usepackage[spanish]{babel}\n\\decimalpoint"
      ))
    } else {
      return(list(
        categoria = "TEXTO",
        codigo = "ERR_T1",
        descripcion = "LaTeX no compila",
        causa = "Error genérico de sintaxis LaTeX",
        skill_correccion = "manual-edit",
        prioridad = "ALTA",
        solucion_sugerida = "Revisar log de LaTeX, verificar caracteres especiales"
      ))
    }
  }

  # Encoding
  if (grepl("invalid multibyte|UTF-8|encoding", mensaje_error, ignore.case = TRUE)) {
    return(list(
      categoria = "TEXTO",
      codigo = "ERR_T2",
      descripcion = "Encoding incorrecto",
      causa = "Archivo no está en UTF-8 o falta especificar encoding",
      skill_correccion = "manual-edit",
      prioridad = "MEDIA",
      solucion_sugerida = "Agregar encoding = 'UTF-8' en exams2*() calls"
    ))
  }

  # Variables R no definidas
  if (grepl("object.*not found|Error in eval", mensaje_error)) {
    # Extraer nombre de variable si es posible
    variable <- sub(".*object '(.*)' not found.*", "\\1", mensaje_error)
    return(list(
      categoria = "COHERENCIA",
      codigo = "ERR_C3",
      descripcion = "Variable R no definida",
      causa = sprintf("Variable '%s' no está calculada o no está en la lista retornada", variable),
      skill_correccion = "manual-edit",
      prioridad = "CRÍTICA",
      solucion_sugerida = sprintf("Verificar que '%s' está en generar_datos() y se retorna en la lista", variable)
    ))
  }

  # CLOZE incompatible con NOPS
  if (grepl("exercise type 'cloze'.*not supported|exams2nops.*cloze", mensaje_error, ignore.case = TRUE)) {
    return(list(
      categoria = "ESTRUCTURA",
      codigo = "ERR_S5",
      descripcion = "CLOZE incompatible con NOPS",
      causa = "NOPS solo soporta schoice/mchoice, no CLOZE con num/string",
      skill_correccion = "none",
      prioridad = "BAJA",
      solucion_sugerida = "Aceptar que NOPS fallará para este ejercicio CLOZE (es esperado)"
    ))
  }

  # Argumentos non-numeric
  if (grepl("non-numeric argument|NaN produced", mensaje_error)) {
    return(list(
      categoria = "COHERENCIA",
      codigo = "ERR_C3",
      descripcion = "Operación matemática con valores no numéricos",
      causa = "Variable formateada (string) usada en cálculo numérico",
      skill_correccion = "manual-edit",
      prioridad = "ALTA",
      solucion_sugerida = "Usar variable numérica original, no formateada (formatear solo al mostrar)"
    ))
  }

  # PASO 2: Si hay archivo .Rmd, analizar contenido
  if (!is.null(archivo_rmd) && file.exists(archivo_rmd)) {
    contenido <- readLines(archivo_rmd, warn = FALSE)

    # Verificar metadatos
    if (!any(grepl("^exname:", contenido))) {
      return(list(
        categoria = "TEXTO",
        codigo = "ERR_T3",
        descripcion = "Metadatos faltantes (exname)",
        causa = "No hay sección Meta-information completa",
        skill_correccion = "manual-edit",
        prioridad = "CRÍTICA",
        solucion_sugerida = "Agregar al final:\nMeta-information\n================\nexname: Nombre Ejercicio"
      ))
    }

    # Verificar Answerlist
    answerlist_lines <- grep("^\\* ", contenido, value = TRUE)
    if (length(answerlist_lines) < 4) {
      return(list(
        categoria = "ESTRUCTURA",
        codigo = "ERR_S1",
        descripcion = "Menos de 4 opciones en Answerlist",
        causa = sprintf("Solo %d opciones detectadas", length(answerlist_lines)),
        skill_correccion = "manual-edit",
        prioridad = "ALTA",
        solucion_sugerida = "Agregar opciones hasta tener mínimo 4"
      ))
    }

    # Verificar gaps CLOZE secuenciales (si es CLOZE)
    if (any(grepl("extype: cloze", contenido, ignore.case = TRUE))) {
      gaps <- grep("##ANSWER[0-9]+##", contenido, value = TRUE)
      gap_nums <- as.numeric(sub(".*##ANSWER([0-9]+)##.*", "\\1", gaps))
      if (length(gap_nums) > 0) {
        esperado <- 1:max(gap_nums)
        if (!all(esperado %in% gap_nums)) {
          return(list(
            categoria = "ESTRUCTURA",
            codigo = "ERR_S4",
            descripcion = "Gaps CLOZE no secuenciales",
            causa = sprintf("Gaps detectados: %s, esperado: 1 a %d",
                          paste(sort(unique(gap_nums)), collapse = ", "),
                          max(gap_nums)),
            skill_correccion = "manual-edit",
            prioridad = "ALTA",
            solucion_sugerida = "Renumerar gaps secuencialmente sin saltos"
          ))
        }
      }
    }
  }

  # PASO 3: Error no clasificado
  return(list(
    categoria = "DESCONOCIDO",
    codigo = "ERR_X",
    descripcion = "Error no clasificado",
    causa = "Patrón no reconocido",
    skill_correccion = "manual-analysis",
    prioridad = "MEDIA",
    solucion_sugerida = "Revisar log completo, consultar patrones-errores-conocidos.md"
  ))
}

# Función para generar reporte de diagnóstico
generar_reporte_diagnostico <- function(diagnostico) {
  cat("\n╔════════════════════════════════════════╗\n")
  cat("║     DIAGNÓSTICO DE ERROR               ║\n")
  cat("╠════════════════════════════════════════╣\n")
  cat(sprintf("║ Categoría: %-28s║\n", diagnostico$categoria))
  cat(sprintf("║ Código: %-31s║\n", diagnostico$codigo))
  cat(sprintf("║ Prioridad: %-28s║\n", diagnostico$prioridad))
  cat("║                                        ║\n")
  cat(sprintf("║ Descripción:                           ║\n"))
  cat(sprintf("║   %s\n", diagnostico$descripcion))
  cat("║                                        ║\n")
  cat(sprintf("║ Causa probable:                        ║\n"))
  cat(sprintf("║   %s\n", diagnostico$causa))
  cat("║                                        ║\n")
  cat(sprintf("║ Solución recomendada:                  ║\n"))
  # Imprimir solución multilínea
  solucion_lines <- strsplit(diagnostico$solucion_sugerida, "\n")[[1]]
  for (line in solucion_lines) {
    cat(sprintf("║   %s\n", line))
  }
  cat("║                                        ║\n")
  cat(sprintf("║ Skill a activar: %-21s║\n", diagnostico$skill_correccion))
  cat("╚════════════════════════════════════════╝\n\n")
}

# Ejemplo de uso
# diagnostico <- diagnosticar_error(
#   "Error: File 'grafico.png' not found",
#   archivo_rmd = "ejercicio.Rmd"
# )
# generar_reporte_diagnostico(diagnostico)
```

## 🔍 Proceso de Ejecución Paso a Paso

### PASO 1: Recibir Errores de FASE 1 o FASE 2

**Input esperado:**

1. **De FASE 1 (validar-renderizado):**
   - Lista de errores por formato (HTML, PDF, DOCX, NOPS)
   - Mensajes de error completos
   - Warnings acumulados

2. **De FASE 2 (validar-coherencia):**
   - Incoherencias matemáticas detectadas
   - Problemas imagen-texto
   - Problemas de coherencia de código

**Formato de input:**
```r
errores_fase1 <- list(
  html = NULL,  # Sin error
  pdf = "LaTeX Error: File `tikz.sty' not found",
  docx = NULL,
  nops = "LaTeX Error: File `tikz.sty' not found"
)

errores_fase2 <- list(
  coherencia_matematica = NULL,
  coherencia_imagen = "Gráfico muestra 5 barras pero texto menciona 6 meses",
  coherencia_codigo = NULL
)
```

### PASO 2: Clasificar Errores por Categoría

**Ejecutar algoritmo de diagnóstico para cada error:**

```r
diagnosticos <- list()

# Clasificar errores de FASE 1
for (formato in names(errores_fase1)) {
  if (!is.null(errores_fase1[[formato]])) {
    diag <- diagnosticar_error(
      mensaje_error = errores_fase1[[formato]],
      archivo_rmd = "ejercicio.Rmd"
    )
    diag$origen <- sprintf("FASE 1 (%s)", toupper(formato))
    diagnosticos <- c(diagnosticos, list(diag))
  }
}

# Clasificar errores de FASE 2
for (tipo in names(errores_fase2)) {
  if (!is.null(errores_fase2[[tipo]])) {
    diag <- diagnosticar_error(
      mensaje_error = errores_fase2[[tipo]],
      archivo_rmd = "ejercicio.Rmd",
      contexto = "FASE2"
    )
    diag$origen <- sprintf("FASE 2 (%s)", tipo)
    diagnosticos <- c(diagnosticos, list(diag))
  }
}
```

### PASO 3: Priorizar Errores

**Orden de prioridad:**

1. **CRÍTICA** (bloquea todo)
2. **ALTA** (bloquea formatos principales)
3. **MEDIA** (afecta calidad)
4. **BAJA** (opcional/esperado)

**Reordenar diagnósticos:**
```r
orden_prioridad <- c("CRÍTICA", "ALTA", "MEDIA", "BAJA")
diagnosticos <- diagnosticos[order(match(sapply(diagnosticos, function(d) d$prioridad), orden_prioridad))]
```

### PASO 4: Generar Reporte de Diagnóstico

**Reporte completo con todos los diagnósticos:**

```
═══════════════════════════════════════════════════════════
⚡ FASE 3: DIAGNÓSTICO Y DECISIÓN
═══════════════════════════════════════════════════════════

Archivo: ecuacion_cuadratica_numerico_variacional_formulacion_ejecucion_n2_v1.Rmd
Errores detectados: 2
Prioridad máxima: CRÍTICA

╔════════════════════════════════════════╗
║     DIAGNÓSTICO DE ERROR #1            ║
╠════════════════════════════════════════╣
║ Origen: FASE 1 (PDF)                   ║
║ Categoría: TEXTO                       ║
║ Código: ERR_T1                         ║
║ Prioridad: CRÍTICA                     ║
║                                        ║
║ Descripción:                           ║
║   Paquete LaTeX faltante               ║
║                                        ║
║ Causa probable:                        ║
║   header-includes no tiene \usepackage{tikz}
║                                        ║
║ Solución recomendada:                  ║
║   Agregar en YAML:                     ║
║   header-includes:                     ║
║     - \usepackage{tikz}                ║
║                                        ║
║ Skill a activar: manual-edit           ║
╚════════════════════════════════════════╝

╔════════════════════════════════════════╗
║     DIAGNÓSTICO DE ERROR #2            ║
╠════════════════════════════════════════╣
║ Origen: FASE 2 (coherencia_imagen)    ║
║ Categoría: COHERENCIA                  ║
║ Código: ERR_C2                         ║
║ Prioridad: ALTA                        ║
║                                        ║
║ Descripción:                           ║
║   Coherencia imagen-texto              ║
║                                        ║
║ Causa probable:                        ║
║   Descripción ≠ gráfico                ║
║                                        ║
║ Solución recomendada:                  ║
║   Alinear número de barras en gráfico  ║
║   con texto del enunciado              ║
║                                        ║
║ Skill a activar: manual-edit           ║
╚════════════════════════════════════════╝

═══════════════════════════════════════════════════════════
→ SIGUIENTE: SUBFASE 3A - Corrección basada en ejemplos
═══════════════════════════════════════════════════════════
```

### PASO 5: Ejecutar SUBFASE 3A - Corrección Basada en Ejemplos

**OBLIGATORIO: Consultar ejemplos funcionales ANTES de cualquier corrección**

**Proceso detallado:**

```bash
# 1. Listar ejemplos funcionales disponibles
ls -lh /A-Produccion/Ejemplos-Funcionales-Rmd/*.Rmd

# 2. Buscar ejemplos con patrón similar al error
# Ejemplo: ERR_T1 (paquete tikz faltante)
grep -l "usepackage{tikz}" /A-Produccion/Ejemplos-Funcionales-Rmd/*.Rmd

# Ejemplo: ERR_G1 (gráficas no visualizadas)
grep -l "include_tikz\|is_latex_output" /A-Produccion/Ejemplos-Funcionales-Rmd/*.Rmd

# 3. Leer ejemplo funcional completo
cat /A-Produccion/Ejemplos-Funcionales-Rmd/ejemplo_con_tikz.Rmd

# 4. Extraer patrón de solución
# Identificar sección específica que resuelve el error
grep -A 10 "header-includes" /A-Produccion/Ejemplos-Funcionales-Rmd/ejemplo_con_tikz.Rmd

# 5. Aplicar corrección basada en ejemplo
# Copiar patrón exacto del ejemplo funcional
```

**Ejemplo de aplicación:**

**Antes (con error ERR_T1):**
```yaml
---
output:
  pdf_document:
    keep_tex: true
---
```

**Después (basado en ejemplo funcional):**
```yaml
---
output:
  pdf_document:
    keep_tex: true
header-includes:
  - \usepackage{tikz}
  - \usepackage{pgfplots}
  - \pgfplotsset{compat=1.18}
  - \usepackage[spanish]{babel}
  - \decimalpoint
---
```

### PASO 6: Ejecutar SUBFASE 3B - Revalidación OBLIGATORIA

**⚠️ CRÍTICO: SIEMPRE volver a FASE 1 después de aplicar correcciones**

**Proceso:**

```r
# 1. Aplicar corrección (SUBFASE 3A completada)

# 2. OBLIGATORIO: Volver a FASE 1
validar-renderizado ejercicio.Rmd

# 3. Ejecutar FASE 2
validar-coherencia ejercicio.Rmd

# 4. Volver a FASE 3 (este skill)
diagnosticar-errores ejercicio.Rmd

# 5. Verificar resultado:
#    - SI errores persisten → Volver a SUBFASE 3A con solución alternativa
#    - SI errores nuevos → Diagnosticar y corregir
#    - SI sin errores → Continuar a SUBFASE 3C
```

**Diagrama de loop:**

```
SUBFASE 3A: Corregir
    ↓
SUBFASE 3B: Revalidar
    ↓
¿Errores resueltos?
    │
    ├─→ NO → Volver a SUBFASE 3A (solución alternativa)
    │         ↓
    │    [LOOP hasta resolver]
    │
    └─→ SÍ → Continuar a SUBFASE 3C
```

**Regla Absoluta**: NO terminar ciclo hasta resolver TODOS los errores.

### PASO 7: Ejecutar SUBFASE 3C - Documentar Solución (Solo si éxito)

**Solo después de revalidación 100% exitosa:**

**Proceso de documentación:**

```bash
# 1. Abrir archivo de patrones de errores
nano .claude/docs/patrones-errores-conocidos.md

# 2. Agregar nueva entrada siguiendo formato
```

**Formato de documentación:**

```markdown
## Error N: [Título descriptivo del error]

### ❌ Mensaje de Error
```
[Mensaje exacto del error]
```

### 🔍 Causa Raíz
[Explicación técnica del problema]

### ✅ Solución Verificada

**Código ANTES:**
```yaml
[Código con error]
```

**Código DESPUÉS:**
```yaml
[Código corregido]
```

### 🧪 Validación de la Solución
```
[Comandos ejecutados y resultados]
- HTML: ✅
- PDF: ✅
- DOCX: ✅
- NOPS: ✅
```

### 📋 Checklist de Corrección
- [ ] Paso 1: [acción específica]
- [ ] Paso 2: [acción específica]
- [ ] Paso 3: [acción específica]

### 📚 Ejemplo Funcional Utilizado
[Referencia a archivo en /A-Produccion/Ejemplos-Funcionales-Rmd/]
- Archivo: ejemplo_con_tikz.Rmd
- Patrón copiado: header-includes con TikZ

### 📅 Historial
| Fecha | Archivo | Resultado |
|-------|---------|-----------|
| 2025-12-30 | ecuacion_cuadratica_n2_v1.Rmd | ✅ Resuelto |
```

Ver principio completo en: @.claude/rules/documentacion-verificada.md

## 🎓 Ejemplos Completos de Diagnóstico

### Ejemplo 1: ERR_T1 - Paquete LaTeX Faltante

**Contexto:**
- FASE 1 reporta: PDF y NOPS fallan
- Error: `! LaTeX Error: File 'tikz.sty' not found`

**Diagnóstico:**
```
╔════════════════════════════════════════╗
║     DIAGNÓSTICO DE ERROR               ║
╠════════════════════════════════════════╣
║ Categoría: TEXTO                       ║
║ Código: ERR_T1                         ║
║ Prioridad: CRÍTICA                     ║
║ Descripción: Paquete LaTeX faltante    ║
║ Causa: header-includes sin \usepackage{tikz} ║
║ Solución: Agregar en YAML              ║
║ Skill: manual-edit                     ║
╚════════════════════════════════════════╝
```

**SUBFASE 3A - Corrección:**
```bash
# Consultar ejemplo funcional
grep -l "usepackage{tikz}" /A-Produccion/Ejemplos-Funcionales-Rmd/*.Rmd
# Resultado: ejemplo_geometria_triangulo.Rmd

# Leer header del ejemplo
head -20 /A-Produccion/Ejemplos-Funcionales-Rmd/ejemplo_geometria_triangulo.Rmd

# Copiar patrón
```

**Aplicar corrección:**
```yaml
header-includes:
  - \usepackage{tikz}
  - \usepackage{pgfplots}
  - \pgfplotsset{compat=1.18}
```

**SUBFASE 3B - Revalidar:**
```bash
Rscript validar_fase1.R ejercicio.Rmd
# Resultado: 4/4 formatos OK ✅
```

**SUBFASE 3C - Documentar:**
Agregar a patrones-errores-conocidos.md (patrón ya documentado, solo actualizar historial)

### Ejemplo 2: ERR_G1 - Gráficas No Visualizadas

**Contexto:**
- FASE 2 reporta: Gráfico no se ve en PDF
- Advertencia FASE 1: `File 'grafico.png' not found`

**Diagnóstico:**
```
╔════════════════════════════════════════╗
║ Categoría: GRÁFICOS                    ║
║ Código: ERR_G1                         ║
║ Prioridad: ALTA                        ║
║ Descripción: Gráficas no visualizadas  ║
║ Causa: include_tikz en chunk incorrecto║
║ Solución: Renderizado condicional      ║
║ Skill: corregir-graficos               ║
╚════════════════════════════════════════╝
```

**SUBFASE 3A - Corrección:**
```bash
# Consultar ejemplos con renderizado condicional
grep -l "is_latex_output\|include_tikz" /A-Produccion/Ejemplos-Funcionales-Rmd/*.Rmd
# Resultado: grafico_barras_ggplot2.Rmd, grafico_lineas_poblacion.Rmd
```

**Antes (código con error):**
```r
```{r data_generation, include=FALSE}
# ... generar gráfico ...
include_tikz("grafico.tex")  # ❌ Falla en HTML
```
```

**Después (basado en ejemplo funcional):**
```r
```{r data_generation, include=FALSE}
# ... generar gráfico ...

# Renderizado condicional
if (knitr::is_latex_output()) {
  include_tikz("grafico.tex")
} else {
  knitr::include_graphics("grafico.png")
}
```
```

**SUBFASE 3B - Revalidar:**
```bash
Rscript validar_fase1.R ejercicio.Rmd
# Resultado: 4/4 formatos OK ✅
```

### Ejemplo 3: ERR_C3 - Variable R No Definida

**Contexto:**
- FASE 1 reporta: HTML, PDF, DOCX, NOPS todos fallan (0/4)
- Error: `object 'resultado' not found`

**Diagnóstico:**
```
╔════════════════════════════════════════╗
║ Categoría: COHERENCIA                  ║
║ Código: ERR_C3                         ║
║ Prioridad: CRÍTICA                     ║
║ Descripción: Variable R no definida    ║
║ Causa: 'resultado' no en generar_datos()║
║ Solución: Agregar a lista retornada    ║
║ Skill: manual-edit                     ║
╚════════════════════════════════════════╝
```

**SUBFASE 3A - Corrección:**
```r
# Revisar función generar_datos()

# Antes (código con error):
generar_datos <- function() {
  a <- sample(2:10, 1)
  b <- sample(5:15, 1)
  resultado <- a * b  # ✓ Calculado

  list(
    a = a,
    b = b
    # ❌ 'resultado' NO retornado
  )
}

# Después (corregido):
generar_datos <- function() {
  a <- sample(2:10, 1)
  b <- sample(5:15, 1)
  resultado <- a * b

  list(
    a = a,
    b = b,
    resultado = resultado  # ✅ Agregado
  )
}
```

**SUBFASE 3B - Revalidar:**
```bash
Rscript validar_fase1.R ejercicio.Rmd
# Resultado: 4/4 formatos OK ✅
```

## ⚠️ Patrones de Errores Conocidos

### Patrón 1: TikZ No Renderiza en HTML

**Síntomas:**
- PDF OK, HTML muestra espacio en blanco

**Diagnóstico:**
- ERR_G1

**Solución verificada:**
```r
if (knitr::is_latex_output()) {
  include_tikz("grafico.tex")
} else {
  knitr::include_graphics("grafico.png")
}
```

**Ejemplo funcional:**
- `/A-Produccion/Ejemplos-Funcionales-Rmd/grafico_lineas_poblacion.Rmd`

### Patrón 2: Caracteres Especiales Rompen LaTeX

**Síntomas:**
- PDF falla con `! LaTeX Error: Missing \begin{document}`
- HTML OK

**Diagnóstico:**
- ERR_T5

**Solución verificada:**
Escapar caracteres: `_` → `\_`, `%` → `\%`, `#` → `\#`, `&` → `\&`

**Ejemplo funcional:**
- Ver patrones-errores-conocidos.md (Error 7)

### Patrón 3: CLOZE Falla en NOPS

**Síntomas:**
- HTML/PDF/DOCX OK (3/4)
- NOPS falla: `exercise type 'cloze' with 'num' not supported`

**Diagnóstico:**
- ERR_S5

**Solución:**
Aceptar que NOPS fallará. Es comportamiento esperado para CLOZE con gaps num/string.

**Acción:**
Ninguna. Marcar error como BAJA prioridad y continuar.

### Patrón 4: Operación con Variable Formateada

**Síntomas:**
- Error: `non-numeric argument to binary operator`

**Diagnóstico:**
- ERR_C3

**Causa:**
```r
# Incorrecto:
poblacion <- formatear_entero(poblacion_inicial)  # "150.000" (string)
incremento <- poblacion * 1.05  # ❌ Error: string * number
```

**Solución:**
```r
# Correcto:
poblacion <- poblacion_inicial  # 150000 (numeric)
poblacion_formateada <- formatear_entero(poblacion)  # "150.000" (solo para mostrar)
incremento <- poblacion * 1.05  # ✅ OK: numeric * number
```

## ⛔ CONDICIONES CRÍTICAS

### Obligatorio ANTES de iniciar diagnóstico

1. ✓ Recibir lista completa de errores de FASE 1 y/o FASE 2
2. ✓ Tener archivo .Rmd accesible para análisis
3. ✓ Acceso a /A-Produccion/Ejemplos-Funcionales-Rmd/
4. ✓ Acceso a patrones-errores-conocidos.md

### Obligatorio DURANTE el diagnóstico

1. ✓ Clasificar TODOS los errores (no omitir ninguno)
2. ✓ Priorizar por nivel crítico
3. ✓ SIEMPRE consultar ejemplos funcionales ANTES de corregir
4. ✓ Aplicar soluciones basadas en ejemplos, NO improvisar
5. ✓ Documentar diagnóstico completo

### Obligatorio DESPUÉS de aplicar correcciones

1. ✓ SIEMPRE ejecutar SUBFASE 3B (revalidación FASE 1)
2. ✓ NUNCA marcar como "completado" si quedan errores
3. ✓ Si errores persisten → Volver a SUBFASE 3A con solución alternativa
4. ✓ Solo si éxito 100% → Ejecutar SUBFASE 3C (documentar)
5. ✓ NO terminar ciclo hasta resolver TODOS los errores

**Regla Absoluta**: LOOP hasta éxito total. NO hay alternativa a resolver los errores.

## 🔗 Referencias y Documentación

### Archivos de Referencia Obligatorios

- **Ciclo Validación**: @.claude/rules/ciclo-validacion.md (FASE 1→2→3 + SUBFASES)
- **Ejemplos Funcionales**: @A-Produccion/Ejemplos-Funcionales-Rmd/ (**FUENTE DE VERDAD**)
- **Patrones Errores**: @.claude/docs/patrones-errores-conocidos.md
- **Reglas Código**: @.claude/rules/codigo-rmd.md

### Reglas del Proyecto (OBLIGATORIAS)

- @.claude/rules/ciclo-validacion.md - FASE 1→2→3 + SUBFASES
- @.claude/rules/codigo-rmd.md - Pre-edit/write checks
- @.claude/rules/documentacion-verificada.md - **Solo documentar verificado**

### Documentación Técnica

- @.claude/docs/WORKFLOW_PASO_A_PASO.md - Flujo completo
- @.claude/docs/TROUBLESHOOTING.md - Solución problemas
- @.claude/docs/TRES_NIVELES_VALIDACION.md - Detalle 3 niveles

## 🚀 Integración con Otros Skills

Este skill se integra en el Ciclo de Validación:

```
generar-schoice / generar-cloze
    ↓
validar-renderizado (FASE 1)
    ↓
validar-coherencia (FASE 2)
    ↓
diagnosticar-errores ← ESTE SKILL (FASE 3)
    ↓
    ├─→ [Sin errores] → promover-ejercicio
    │
    └─→ [Con errores]:
            │
            ├─→ SUBFASE 3A: corregir-graficos (si ERR_G*)
            │                manual-edit (otros errores)
            │
            ├─→ SUBFASE 3B: validar-renderizado (LOOP)
            │                ↓
            │           validar-coherencia
            │                ↓
            │           [Volver aquí] diagnosticar-errores
            │
            └─→ SUBFASE 3C: Actualizar patrones-errores-conocidos.md
                           (solo si éxito)
```

**Skills relacionados:**
- **Prerequisito**: `validar-renderizado` (FASE 1) y/o `validar-coherencia` (FASE 2)
- **SUBFASE 3A**: `corregir-graficos` (si ERR_G*), manual Edit (otros)
- **SUBFASE 3B**: Volver a `validar-renderizado` → LOOP
- **Final**: `promover-ejercicio` (si sin errores)

## 📊 Output Final Esperado

Después de usar este skill, debes tener:

**Si hay errores:**
```
outputs/
├── diagnostico_fase3.txt (reporte completo de diagnóstico)
├── correcion_aplicada.log (log de correcciones)
└── revalidacion_fase1.txt (resultado de SUBFASE 3B)
```

**Reporte de FASE 3 debe contener:**
- ✓ Lista de errores clasificados (código, categoría, prioridad)
- ✓ Diagnóstico detallado de cada error
- ✓ Solución recomendada basada en ejemplos
- ✓ Skill de corrección a activar
- ✓ Ejemplo funcional utilizado
- ✓ Resultado de revalidación (SUBFASE 3B)

**Si sin errores:**
```
═══════════════════════════════════════════════════════════
⚡ FASE 3: SIN ERRORES DETECTADOS
═══════════════════════════════════════════════════════════
✅ Ejercicio aprobado para promoción
→ SIGUIENTE: promover-ejercicio
═══════════════════════════════════════════════════════════
```

---

**Última actualización**: 2025-12-30
**Versión**: 2.0 (Progressive Disclosure)
**Basado en**: Ciclo de Validación oficial + Claude Code best practices (nov 2025)
