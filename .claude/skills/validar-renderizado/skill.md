---
name: validar-renderizado
description: >
  Ejecuta FASE 1 del Ciclo de Validación - Renderiza en 4 formatos (HTML, PDF, DOCX, NOPS).
  Usa automáticamente después de generar/modificar archivo .Rmd, cuando hay errores de compilación,
  o necesites validar que ejercicio renderiza correctamente en todos los formatos.
  Captura TODOS los errores y continúa a FASE 2.

allowed-tools:
  - Read
  - Bash(Rscript:*)
  - Bash(exams2*:*)
  - Bash(ls:*)
  - Bash(cat:*)
  - Write
---

# Validador de Renderizado R/exams - FASE 1

## 🎯 Propósito de este Skill

Este skill ejecuta la **FASE 1: RENDERIZADO INICIAL** del Ciclo de Validación y Corrección Automática. Es el **primer checkpoint obligatorio** después de crear o modificar cualquier archivo .Rmd.

### Cuándo usar este skill

**Triggers automáticos** (Claude lo usa cuando detecta):
- Después de ejecutar `/generar-schoice` o `/generar-cloze`
- Después de editar un archivo .Rmd
- Cuando hay errores de compilación reportados
- Al preparar ejercicio para promoción
- Usuario dice "valida el renderizado", "compila el ejercicio", "testea en todos los formatos"

**Invocación manual:**
```
/validar-renderizado archivo.Rmd
```

**Invocación automática:**
- Se ejecuta automáticamente como parte del workflow tras generar ejercicio
- No requiere invocación explícita en flujo normal

## 📋 Contexto: Ciclo de Validación Completo

Este skill es la FASE 1 del ciclo de 3 fases:

```
🔄 FASE 1: RENDERIZADO INICIAL ← ESTE SKILL
    │
    ├─→ Renderiza en 4 formatos (HTML, PDF, DOCX, NOPS)
    ├─→ Captura TODOS los errores y warnings
    ├─→ Genera reporte detallado
    │
    ▼
🔍 FASE 2: Validación Visual y Funcional (validar-coherencia)
    │
    ├─→ Verifica coherencia matemática
    ├─→ Verifica coherencia imagen-texto
    ├─→ Verifica coherencia de código
    │
    ▼
⚡ FASE 3: Decisión y Acción (diagnosticar-errores)
    │
    ├─→ SI hay errores → SUBFASE 3A (corregir)
    ├─→ SUBFASE 3B: Volver a FASE 1 ← ⚠️ LOOP
    └─→ SI todo OK → Continuar workflow
```

Ver ciclo completo en: @.claude/rules/ciclo-validacion.md

## 📊 Los 4 Formatos Obligatorios

### Formato 1: HTML (Más Rápido - Debugging)

**Función R/exams:**
```r
exams2html(archivo.Rmd, n = 1, dir = "output/html", encoding = "UTF-8")
```

**Propósito:**
- Formato más rápido para debugging
- Visualización inmediata en navegador
- Detecta errores de sintaxis Markdown
- Valida chunks R básicos

**Características:**
- ✅ Renderiza ecuaciones LaTeX (MathJax)
- ✅ Muestra gráficos inline
- ✅ Preserva formato Answerlist
- ⚠️ NO valida PDF/LaTeX específico

**Errores comunes detectados:**
- Sintaxis Markdown incorrecta
- Variables R no definidas
- Chunks con errores de ejecución
- Gráficos no generados

### Formato 2: PDF (Principal - Formato Oficial)

**Función R/exams:**
```r
exams2pdf(archivo.Rmd, n = 1, dir = "output/pdf",
          template = "plain", encoding = "UTF-8")
```

**Propósito:**
- Formato principal de entrega
- Validación completa LaTeX
- Formato oficial para impresión
- Detecta errores de compilación LaTeX

**Características:**
- ✅ Compilación LaTeX completa
- ✅ Ecuaciones renderizadas profesionalmente
- ✅ Gráficos en alta calidad
- ⚠️ Más lento que HTML (30-60 segundos)

**Errores comunes detectados:**
- Paquetes LaTeX faltantes (\usepackage)
- Caracteres especiales sin escapar
- Errores de TikZ
- Imágenes no encontradas
- Ecuaciones mal formateadas
- Errores de babel/spanish

**Archivos generados:**
- `archivo1.pdf` - PDF final
- `archivo1.tex` - Código LaTeX (si keep_tex: true)
- `archivo1.log` - Log completo de compilación

### Formato 3: DOCX (Revisión - Microsoft Word)

**Función R/exams:**
```r
exams2pandoc(archivo.Rmd, n = 1, dir = "output/docx",
             type = "docx", encoding = "UTF-8")
```

**Propósito:**
- Formato editable para revisión
- Compatible con Microsoft Word
- Permite comentarios y correcciones
- Útil para colaboración

**Características:**
- ✅ Documento Word estándar (.docx)
- ✅ Ecuaciones convertidas a formato Word
- ✅ Gráficos embebidos
- ⚠️ Formato puede variar vs PDF

**Errores comunes detectados:**
- Problemas de conversión Pandoc
- Ecuaciones no convertibles
- Tablas mal formateadas
- Gráficos no embebidos

**Limitaciones:**
- Ecuaciones complejas pueden no renderizar igual que PDF
- Formato TikZ no soportado (requiere conversión a PNG)

### Formato 4: NOPS (Escaneable - Examen con Respuestas)

**Función R/exams:**
```r
exams2nops(archivo.Rmd, n = 1, dir = "output/nops", encoding = "UTF-8")
```

**Propósito:**
- Formato escaneable para corrección automática
- Genera PDF con código de barras
- Incluye hoja de respuestas
- Permite escaneo y corrección masiva

**Características:**
- ✅ PDF con código de barras único
- ✅ Hoja de respuestas separada
- ✅ Metadata para escáner
- ⚠️ Solo para ejercicios tipo schoice/mchoice

**Errores comunes detectados:**
- Tipo de ejercicio incompatible (cloze con num/string)
- Metadatos faltantes
- Formato de respuesta incorrecto

**Limitaciones:**
- ❌ NO funciona con ejercicios CLOZE que tengan gaps tipo num o string
- ✅ SÍ funciona con SCHOICE
- ✅ SÍ funciona con CLOZE que SOLO tenga gaps schoice/mchoice

## 🔍 Proceso de Ejecución Paso a Paso

### PASO 1: Identificar Archivo Objetivo

**Input esperado:**
- Ruta completa: `/A-Produccion/En-Desarrollo/ejercicio/nombre.Rmd`
- O nombre relativo: `ejercicio.Rmd` (busca en directorio actual)

**Validación:**
```bash
# Verificar que archivo existe
if [ ! -f "$archivo" ]; then
  echo "❌ Error: Archivo $archivo no encontrado"
  exit 1
fi

# Verificar que es .Rmd
if [[ ! "$archivo" =~ \.Rmd$ ]]; then
  echo "❌ Error: Archivo debe ser .Rmd"
  exit 1
fi
```

### PASO 2: Preparar Entorno de Renderizado

**Crear directorios de salida:**
```bash
mkdir -p outputs/html
mkdir -p outputs/pdf
mkdir -p outputs/docx
mkdir -p outputs/nops
```

**Limpiar outputs previos (opcional):**
```bash
rm -f outputs/html/*
rm -f outputs/pdf/*
rm -f outputs/docx/*
rm -f outputs/nops/*
```

### PASO 3: Ejecutar Renderizado Secuencial con Captura de Errores

**Script R completo para FASE 1:**

```r
#!/usr/bin/env Rscript

# Cargar librería
library(exams)

# Configuración
archivo_rmd <- "ejercicio.Rmd"
set.seed(sample(1:100000, 1))

# Función de renderizado con captura de errores
renderizar_formato <- function(archivo, formato, funcion_exams, ...) {
  cat(sprintf("\n🔄 Renderizando %s...\n", formato))

  inicio <- Sys.time()
  resultado <- tryCatch({
    funcion_exams(archivo, n = 1, encoding = "UTF-8", ...)
    list(
      exito = TRUE,
      tiempo = as.numeric(difftime(Sys.time(), inicio, units = "secs")),
      error = NULL
    )
  }, error = function(e) {
    list(
      exito = FALSE,
      tiempo = as.numeric(difftime(Sys.time(), inicio, units = "secs")),
      error = e$message
    )
  }, warning = function(w) {
    list(
      exito = TRUE,
      tiempo = as.numeric(difftime(Sys.time(), inicio, units = "secs")),
      error = NULL,
      advertencia = w$message
    )
  })

  if (resultado$exito) {
    cat(sprintf("✅ %s renderizado en %.2f segundos\n", formato, resultado$tiempo))
  } else {
    cat(sprintf("❌ %s falló: %s\n", formato, resultado$error))
  }

  return(resultado)
}

# Ejecutar renderizado en los 4 formatos
cat("\n═══════════════════════════════════════════════════════════\n")
cat("🔄 FASE 1: RENDERIZADO INICIAL\n")
cat("═══════════════════════════════════════════════════════════\n")

resultados <- list(
  html = renderizar_formato(archivo_rmd, "HTML", exams2html,
                            dir = "outputs/html"),

  pdf = renderizar_formato(archivo_rmd, "PDF", exams2pdf,
                           dir = "outputs/pdf", template = "plain"),

  docx = renderizar_formato(archivo_rmd, "DOCX", exams2pandoc,
                            dir = "outputs/docx", type = "docx"),

  nops = renderizar_formato(archivo_rmd, "NOPS", exams2nops,
                            dir = "outputs/nops")
)

# Generar reporte
cat("\n═══════════════════════════════════════════════════════════\n")
cat("📊 REPORTE FASE 1: RENDERIZADO INICIAL\n")
cat("═══════════════════════════════════════════════════════════\n\n")

exitos <- sum(sapply(resultados, function(r) r$exito))
tasa <- round(100 * exitos / length(resultados), 1)

cat(sprintf("Archivo: %s\n", archivo_rmd))
cat(sprintf("Tasa de éxito: %d de %d formatos (%s%%)\n\n",
            exitos, length(resultados), tasa))

cat("Formato    | Resultado | Tiempo     | Detalles\n")
cat("-----------|-----------|------------|------------------\n")

for (nombre in names(resultados)) {
  r <- resultados[[nombre]]
  simbolo <- if (r$exito) "✅" else "❌"
  detalles <- if (!is.null(r$error)) r$error else "OK"
  cat(sprintf("%-10s | %-9s | %6.2fs | %s\n",
              toupper(nombre), simbolo, r$tiempo, detalles))
}

cat("\n═══════════════════════════════════════════════════════════\n")

if (exitos == length(resultados)) {
  cat("✅ FASE 1 COMPLETADA - Todos los formatos OK\n")
  cat("→ SIGUIENTE: FASE 2 - Validación Visual y Funcional\n")
} else {
  cat(sprintf("⚠️ FASE 1 COMPLETADA CON ERRORES (%d/%d)\n",
              length(resultados) - exitos, length(resultados)))
  cat("→ SIGUIENTE: FASE 2 con errores registrados\n")
  cat("→ Errores se procesarán en FASE 3\n")
}

cat("═══════════════════════════════════════════════════════════\n")

# Retornar código de salida
if (exitos == length(resultados)) {
  quit(status = 0)
} else {
  quit(status = 1)
}
```

### PASO 4: Capturar y Registrar Errores

**Tipos de errores a capturar:**

1. **Errores de sintaxis R:**
   ```
   Error in eval(expr, envir, enclos) : object 'variable_no_definida' not found
   ```

2. **Errores de compilación LaTeX:**
   ```
   ! LaTeX Error: File `tikz.sty' not found.
   ! Undefined control sequence.
   ```

3. **Errores de gráficos:**
   ```
   Error in grid.Call(C_convert, x, as.integer(whatFrom), ...) :
     Viewport has zero dimension(s)
   ```

4. **Errores de formato:**
   ```
   Error in exams2nops: exercise type 'cloze' with 'num' not supported
   ```

**Guardar log de errores:**
```bash
# Guardar stdout y stderr
Rscript validar_fase1.R 2>&1 | tee outputs/fase1_log.txt
```

### PASO 5: Generar Reporte de FASE 1

**Formato de reporte estándar:**

```
═══════════════════════════════════════════════════════════
📊 REPORTE FASE 1: RENDERIZADO INICIAL
═══════════════════════════════════════════════════════════

Archivo: ecuacion_cuadratica_numerico_variacional_formulacion_ejecucion_n2_v1.Rmd
Fecha: 2025-12-30 15:45:23
Tasa de éxito: 3 de 4 formatos (75.0%)

Formato    | Resultado | Tiempo     | Detalles
-----------|-----------|------------|------------------
HTML       | ✅        |   2.34s    | OK
PDF        | ✅        |  45.67s    | OK
DOCX       | ✅        |  12.45s    | OK
NOPS       | ❌        |   5.23s    | Error: exercise type 'cloze' with 'num' not supported

Errores capturados:
1. NOPS: Tipo de ejercicio incompatible (CLOZE con gap num)

Advertencias:
- PDF: LaTeX Warning: Reference `fig:grafico' on page 1 undefined

═══════════════════════════════════════════════════════════
⚠️ FASE 1 COMPLETADA CON ERRORES (1/4)
→ SIGUIENTE: FASE 2 con errores registrados
→ Errores se procesarán en FASE 3
═══════════════════════════════════════════════════════════
```

### PASO 6: Validación Post-Renderizado

**Verificar archivos generados:**

```bash
# HTML
if [ -f "outputs/html/ejercicio1.html" ]; then
  echo "✅ HTML generado"
else
  echo "❌ HTML no generado"
fi

# PDF
if [ -f "outputs/pdf/ejercicio1.pdf" ]; then
  # Verificar que PDF no está vacío
  size=$(wc -c < "outputs/pdf/ejercicio1.pdf")
  if [ $size -gt 1000 ]; then
    echo "✅ PDF generado ($size bytes)"
  else
    echo "⚠️ PDF muy pequeño, posible error"
  fi
else
  echo "❌ PDF no generado"
fi

# DOCX
if [ -f "outputs/docx/ejercicio1.docx" ]; then
  echo "✅ DOCX generado"
else
  echo "❌ DOCX no generado"
fi

# NOPS
if [ -f "outputs/nops/nops1.pdf" ]; then
  echo "✅ NOPS generado"
else
  echo "❌ NOPS no generado"
fi
```

### PASO 7: Continuar a FASE 2

**Decisión automática:**

```
SI todos los formatos OK (100%)
  → Continuar a FASE 2: validar-coherencia
  → Confianza alta en el ejercicio

SI 75% OK (3/4 formatos)
  → Continuar a FASE 2 con advertencia
  → Errores se procesarán en FASE 3

SI 50% OK (2/4 formatos)
  → Continuar a FASE 2 con advertencia crítica
  → Alta probabilidad de ir a FASE 3

SI < 50% OK (0-1/4 formatos)
  → Continuar a FASE 2 pero probable fallo total
  → FASE 3 será obligatoria
```

**Regla absoluta**: SIEMPRE continuar a FASE 2, NUNCA detener el flujo en FASE 1.

## 🎓 Ejemplos Completos de Ejecución

### Ejemplo 1: Renderizado 100% Exitoso (Ideal)

**Archivo:** `area_rectangulo_geometrico_metrico_formulacion_ejecucion_n1_v1.Rmd`

**Comando:**
```bash
Rscript validar_fase1.R area_rectangulo_geometrico_metrico_formulacion_ejecucion_n1_v1.Rmd
```

**Output:**
```
═══════════════════════════════════════════════════════════
🔄 FASE 1: RENDERIZADO INICIAL
═══════════════════════════════════════════════════════════

🔄 Renderizando HTML...
✅ HTML renderizado en 2.12 segundos

🔄 Renderizando PDF...
✅ PDF renderizado en 38.45 segundos

🔄 Renderizando DOCX...
✅ DOCX renderizado en 8.23 segundos

🔄 Renderizando NOPS...
✅ NOPS renderizado en 4.67 segundos

═══════════════════════════════════════════════════════════
📊 REPORTE FASE 1: RENDERIZADO INICIAL
═══════════════════════════════════════════════════════════

Archivo: area_rectangulo_geometrico_metrico_formulacion_ejecucion_n1_v1.Rmd
Tasa de éxito: 4 de 4 formatos (100.0%)

Formato    | Resultado | Tiempo     | Detalles
-----------|-----------|------------|------------------
HTML       | ✅        |   2.12s    | OK
PDF        | ✅        |  38.45s    | OK
DOCX       | ✅        |   8.23s    | OK
NOPS       | ✅        |   4.67s    | OK

═══════════════════════════════════════════════════════════
✅ FASE 1 COMPLETADA - Todos los formatos OK
→ SIGUIENTE: FASE 2 - Validación Visual y Funcional
═══════════════════════════════════════════════════════════
```

**Archivos generados:**
```
outputs/
├── html/
│   └── area_rectangulo1.html
├── pdf/
│   ├── area_rectangulo1.pdf
│   └── area_rectangulo1.tex
├── docx/
│   └── area_rectangulo1.docx
└── nops/
    └── nops1.pdf
```

### Ejemplo 2: Renderizado con Error LaTeX (Común)

**Archivo:** `ejercicio_con_tikz_error.Rmd`

**Error:** Paquete TikZ no cargado en YAML

**Output:**
```
🔄 Renderizando HTML...
✅ HTML renderizado en 2.34 segundos

🔄 Renderizando PDF...
❌ PDF falló: Error in texi2dvi(file = file, pdf = TRUE, ...):
   Running 'texi2pdf' on 'ejercicio1.tex' failed.
   LaTeX errors:
   ! LaTeX Error: File `tikz.sty' not found.

🔄 Renderizando DOCX...
✅ DOCX renderizado en 9.12 segundos

🔄 Renderizando NOPS...
❌ NOPS falló: (same LaTeX error)

═══════════════════════════════════════════════════════════
📊 REPORTE FASE 1: RENDERIZADO INICIAL
═══════════════════════════════════════════════════════════

Tasa de éxito: 2 de 4 formatos (50.0%)

Formato    | Resultado | Tiempo     | Detalles
-----------|-----------|------------|------------------
HTML       | ✅        |   2.34s    | OK
PDF        | ❌        |  15.23s    | LaTeX Error: tikz.sty not found
DOCX       | ✅        |   9.12s    | OK
NOPS       | ❌        |   8.45s    | LaTeX Error: tikz.sty not found

Errores capturados:
1. PDF: Paquete tikz.sty no encontrado
2. NOPS: Paquete tikz.sty no encontrado

Solución sugerida:
Agregar en YAML:
header-includes:
  - \usepackage{tikz}

═══════════════════════════════════════════════════════════
⚠️ FASE 1 COMPLETADA CON ERRORES (2/4)
→ SIGUIENTE: FASE 2 con errores registrados
→ Errores se procesarán en FASE 3
═══════════════════════════════════════════════════════════
```

### Ejemplo 3: Error de Variable R No Definida

**Error:** Variable `resultado` usada pero no calculada

**Output:**
```
🔄 Renderizando HTML...
❌ HTML falló: Error in eval(expr, envir, enclos) :
   object 'resultado' not found
   Calls: <Anonymous> ... in_dir -> block_exec -> call_block -> eval -> eval

🔄 Renderizando PDF...
❌ PDF falló: (mismo error)

🔄 Renderizando DOCX...
❌ DOCX falló: (mismo error)

🔄 Renderizando NOPS...
❌ NOPS falló: (mismo error)

═══════════════════════════════════════════════════════════
Tasa de éxito: 0 de 4 formatos (0.0%)

Errores capturados:
1. HTML/PDF/DOCX/NOPS: Variable 'resultado' no definida

Solución sugerida:
Revisar chunk data_generation:
- ¿Variable 'resultado' está calculada?
- ¿Está dentro de la función generar_datos()?
- ¿Se retorna en la lista?

═══════════════════════════════════════════════════════════
❌ FASE 1 FALLÓ COMPLETAMENTE (0/4)
→ SIGUIENTE: FASE 2 (informativo)
→ FASE 3 SERÁ OBLIGATORIA - Error crítico
═══════════════════════════════════════════════════════════
```

## ⚠️ Errores Comunes por Formato

### Errores Específicos de HTML

**Error 1: Encoding incorrecto**
```
Error: invalid multibyte string at '<e1><e9>'
```
**Solución:**
```r
exams2html(archivo, encoding = "UTF-8")  # Especificar UTF-8
```

**Error 2: MathJax no carga ecuaciones**
```
Ecuaciones muestran código LaTeX sin renderizar
```
**Solución:** Verificar conexión a internet (MathJax requiere CDN online)

### Errores Específicos de PDF

**Error 1: Paquete LaTeX faltante**
```
! LaTeX Error: File `pgfplots.sty' not found.
```
**Solución:**
```yaml
header-includes:
  - \usepackage{pgfplots}
  - \pgfplotsset{compat=1.18}
```

**Error 2: Caracteres especiales sin escapar**
```
! LaTeX Error: Missing \begin{document}.
```
**Causa:** Uso de `_`, `%`, `#`, `&` sin escapar
**Solución:** Usar `\_`, `\%`, `\#`, `\&`

**Error 3: Babel/Spanish conflicto**
```
! Package babel Error: Unknown option `spanish'. Either you misspelled it
```
**Solución:**
```yaml
header-includes:
  - \usepackage[spanish]{babel}
  - \decimalpoint
```

### Errores Específicos de DOCX

**Error 1: Pandoc no instalado**
```
Error: pandoc document conversion failed
```
**Solución:** Instalar Pandoc (`sudo apt install pandoc`)

**Error 2: Ecuaciones no convertibles**
```
Warning: Could not convert TeX math, rendering as TeX
```
**Solución:** Simplificar ecuaciones complejas o aceptar como limitación

### Errores Específicos de NOPS

**Error 1: Tipo de ejercicio incompatible**
```
Error in exams2nops: exercise type 'cloze' with 'num' not supported
```
**Solución:** NOPS solo soporta schoice/mchoice, no CLOZE con num/string

**Error 2: Metadatos faltantes**
```
Error: missing 'exsolution' specification
```
**Solución:** Verificar que Meta-information esté completa

## ⛔ CONDICIONES CRÍTICAS

### Obligatorio ANTES de ejecutar FASE 1

1. ✓ Archivo .Rmd existe y es accesible
2. ✓ Directorio de outputs/ existe o puede crearse
3. ✓ Librería exams cargada
4. ✓ R está instalado y funcional

### Obligatorio DURANTE la ejecución

1. ✓ SIEMPRE ejecutar los 4 formatos (HTML, PDF, DOCX, NOPS)
2. ✓ SIEMPRE capturar errores (tryCatch)
3. ✓ SIEMPRE registrar warnings además de errores
4. ✓ SIEMPRE medir tiempo de renderizado
5. ✓ NUNCA detener ejecución por un error (continuar con otros formatos)

### Obligatorio DESPUÉS de la ejecución

1. ✓ SIEMPRE generar reporte de FASE 1
2. ✓ SIEMPRE continuar a FASE 2 (incluso con errores 0/4)
3. ✓ SIEMPRE preservar logs de errores
4. ✓ NUNCA marcar ejercicio como "completado" si hay errores
5. ✓ NUNCA omitir FASE 2 aunque FASE 1 sea perfecta

**Regla Absoluta**: FASE 1 NUNCA detiene el flujo. SIEMPRE continuar a FASE 2.

## 🔗 Referencias y Documentación

### Archivos de Referencia Obligatorios

- **Ciclo Validación**: @.claude/rules/ciclo-validacion.md (FASE 1→2→3 completo)
- **Reglas Código**: @.claude/rules/codigo-rmd.md (qué validar)
- **Ejemplos Funcionales**: @A-Produccion/Ejemplos-Funcionales-Rmd/ (referencia)
- **Patrones Errores**: @.claude/docs/patrones-errores-conocidos.md

### Reglas del Proyecto (OBLIGATORIAS)

- @.claude/rules/ciclo-validacion.md - FASE 1→2→3 + SUBFASES
- @.claude/rules/codigo-rmd.md - Pre-edit/write checks
- @.claude/rules/documentacion-verificada.md - Solo documentar verificado

### Documentación Técnica

- @.claude/docs/WORKFLOW_PASO_A_PASO.md - Flujo completo
- @.claude/docs/TRES_NIVELES_VALIDACION.md - Detalle 3 niveles
- @.claude/docs/TROUBLESHOOTING.md - Solución errores

### Documentación R/exams

- R/exams rendering: https://www.r-exams.org/intro/dynamic/
- Output formats: https://www.r-exams.org/tutorials/exams2/

## 🚀 Integración con Otros Skills

Este skill se integra en el Ciclo de Validación:

```
generar-schoice / generar-cloze
    ↓
validar-renderizado ← ESTE SKILL (FASE 1)
    ↓
validar-coherencia (FASE 2)
    ↓
[SI errores detectados en FASE 1 o 2]
    ↓
diagnosticar-errores (FASE 3)
    ↓
corregir-graficos (SUBFASE 3A - si errores gráficos)
    ↓
[LOOP] → validar-renderizado (SUBFASE 3B - revalidar)
    ↓
[SI todo OK]
    ↓
promover-ejercicio
```

**Skills relacionados:**
- **Prerequisito**: `/generar-schoice` o `/generar-cloze` (genera .Rmd)
- **Siguiente**: `validar-coherencia` (FASE 2 - siempre)
- **Si errores**: `diagnosticar-errores` (FASE 3)
- **Si errores gráficos**: `corregir-graficos` (SUBFASE 3A)
- **Revalidación**: Este mismo skill (SUBFASE 3B - loop)

## 📊 Output Final Esperado

Después de usar este skill, debes tener:

```
outputs/
├── html/
│   └── ejercicio1.html (siempre generado si no hay error R)
├── pdf/
│   ├── ejercicio1.pdf (generado si LaTeX OK)
│   └── ejercicio1.tex (código fuente LaTeX)
├── docx/
│   └── ejercicio1.docx (generado si Pandoc OK)
├── nops/
│   └── nops1.pdf (generado si tipo compatible)
└── fase1_log.txt (log completo con errores)
```

**Reporte de FASE 1 debe contener:**
- ✓ Tasa de éxito (X de 4 formatos)
- ✓ Tiempo de renderizado por formato
- ✓ Lista de errores capturados
- ✓ Lista de advertencias (warnings)
- ✓ Sugerencias de corrección (si errores conocidos)
- ✓ Decisión: continuar a FASE 2

**Siguiente acción:**
```
# Siempre continuar a FASE 2
validar-coherencia ejercicio.Rmd

# FASE 2 decidirá si proceder o ir a FASE 3
```

---

**Última actualización**: 2025-12-30
**Versión**: 2.0 (Progressive Disclosure)
**Basado en**: R/exams documentation + Claude Code best practices (nov 2025)
