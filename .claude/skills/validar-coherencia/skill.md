---
name: validar-coherencia
description: >
  Ejecuta FASE 2 del Ciclo de Validación - Validación Visual y Funcional.
  Verifica 4 tipos de coherencia: matemática, imagen-texto, código, metadatos.
  Usa cuando renderizado pasa FASE 1 o necesites verificar sincronización.
  Detecta ERR_C1-C5 (coherencia) antes de diagnóstico en FASE 3.

allowed-tools:
  - Read
  - Grep
  - Glob
  - Bash(grep:*)
  - Bash(find:*)
  - Bash(cat:*)
---

# Validación de Coherencia Visual y Funcional (FASE 2)

## 🎯 Propósito de este Skill

Este skill ejecuta la **FASE 2: VALIDACIÓN VISUAL Y FUNCIONAL** del Ciclo de Validación Automática. Es el **segundo paso crítico** después del renderizado inicial (FASE 1) y antes del diagnóstico de errores (FASE 3).

**Objetivo principal**: Verificar que **TODO esté internamente consistente** - matemáticas correctas, imágenes sincronizadas con texto, código sin conflictos, metadatos completos.

### Cuándo usar este skill

**Triggers automáticos** (Claude lo usa cuando detecta):
- "Verifica la coherencia del ejercicio"
- "¿Está todo sincronizado correctamente?"
- "Valida que las matemáticas sean correctas"
- Renderizado exitoso en FASE 1 (continúa automáticamente a FASE 2)
- Usuario pide validación de imagen vs texto

**Invocación manual:**
```
/validar-coherencia ejercicio.Rmd
```

## ⚡ Contexto: Ciclo de Validación Completo

```
🔄 FASE 1: Renderizado Inicial (validar-renderizado)
    ↓ [Si pasa los 4 formatos]
    ↓
🔍 FASE 2: VALIDACIÓN VISUAL Y FUNCIONAL ← ESTE SKILL
    ↓ [Verifica 4 tipos de coherencia]
    ↓
⚡ FASE 3: Decisión y Acción (diagnosticar-errores)
    ├── 📚 SUBFASE 3A: Corrección basada en ejemplos
    ├── �� SUBFASE 3B: Revalidación (volver a FASE 1)
    └── 📊 SUBFASE 3C: Documentar solución
```

**Posición en el flujo:**
- **Input**: Archivo .Rmd que pasó FASE 1 (renderiza sin errores críticos)
- **Output**: Reporte de coherencia + decisión para FASE 3

## 📋 Los 4 Tipos de Coherencia

### Tipo 1: Coherencia Matemática (ERR_C1)

**Definición**: Verificar que cálculos, fórmulas, y respuesta correcta sean matemáticamente válidos.

**Qué se verifica:**

1. **Fórmulas aplicadas correctamente**
   - La fórmula usada es apropiada para el problema
   - Parámetros en orden correcto
   - Unidades consistentes

2. **Cálculos intermedios válidos**
   - Cada paso matemático es correcto
   - Orden de operaciones correcto
   - Operadores correctos (+, -, *, /, ^)

3. **Resultado final correcto**
   - El valor calculado coincide con `exsolution`
   - Redondeo apropiado (según `extol`)
   - Formato numérico correcto

4. **Opciones de respuesta coherentes**
   - Distractores plausibles (errores comunes)
   - No hay respuestas duplicadas
   - Opciones en orden lógico

**Checklist detallado:**
```
□ Fórmula: La fórmula usada es correcta para el problema
□ Valores: Los valores numéricos están en rangos válidos
□ Cálculo: Operaciones intermedias son correctas
□ Resultado: La respuesta correcta coincide con el cálculo
□ Distractores: Son matemáticamente plausibles
□ Redondeo: No hay errores de redondeo significativos
□ exsolution: Coincide con la respuesta correcta identificada
```

**Ejemplo de error ERR_C1:**
```r
# Problema: Calcular área de círculo con radio 5 cm
radio <- 5
area <- pi * radio  # ❌ Error: debería ser pi * radio^2
area_redondeada <- round(area, 2)

# Answerlist (opciones)
opciones <- c(
  area_redondeada,     # "Correcta" pero mal calculada
  round(2 * pi * radio, 2),
  round(pi * radio^2, 2),  # Esta es la correcta real
  round(4 * pi * radio, 2)
)
```

**Corrección:**
```r
radio <- 5
area <- pi * radio^2  # ✅ Correcto: A = πr²
area_redondeada <- round(area, 2)  # 78.54

opciones <- c(
  area_redondeada,              # ✅ Correcta: 78.54
  round(pi * radio, 2),         # Distractor: olvidó ^2
  round(2 * pi * radio, 2),     # Distractor: confundió con perímetro
  round(4 * pi * radio^2, 2)    # Distractor: multiplicó por 4
)

# exsolution debe ser: 10000 (primera opción correcta en SCHOICE con 4 opciones)
```

**Patrón de detección:**
```r
# Verificar que exsolution coincida con cálculo esperado
if (abs(area_redondeada - opciones[1]) > 0.01) {
  warning("ERR_C1: exsolution no coincide con cálculo esperado")
}
```

### Tipo 2: Coherencia Imagen-Texto (ERR_C2)

**Definición**: Verificar sincronización entre descripción textual y gráficos generados.

**Qué se verifica:**

1. **Valores numéricos sincronizados**
   - Dimensiones en texto = dimensiones en gráfico
   - Coordenadas en texto = coordenadas en TikZ/matplotlib
   - Variables R usadas tanto en texto como en gráfico

2. **Elementos visuales consistentes**
   - Colores descritos = colores mostrados
   - Formas descritas = formas mostradas
   - Orientación correcta (horizontal/vertical)

3. **Etiquetas y escalas**
   - Etiquetas legibles y en posición correcta
   - Ejes con escalas apropiadas
   - Unidades especificadas y consistentes

**Checklist detallado:**
```
□ Dimensiones: Valores en texto coinciden con gráfico
□ Colores: Colores descritos son los mostrados
□ Formas: Figuras descritas coinciden con renderizado
□ Orientación: Posición/rotación es la descrita
□ Etiquetas: Son legibles y correctas
□ Escala: Apropiada para los valores mostrados
□ Sincronización: Variables R usadas en ambos (texto y gráfico)
```

**Ejemplo de error ERR_C2:**
```r
# Chunk R: define variables
radio_cilindro <- 5
altura_cilindro <- 10

# Texto en Question:
El cilindro tiene radio de `r radio_cilindro` cm y altura de `r altura_cilindro` cm.

# TikZ (mal sincronizado):
<<echo=FALSE, results="asis">>=
tikz_code <- "
\\begin{tikzpicture}
  \\def\\radio{3}    % ❌ Hardcodeado, no usa variable R
  \\def\\altura{8}   % ❌ Hardcodeado, no usa variable R
  \\draw[fill=blue] (0,0) ellipse (\\radio cm and 0.5cm);
  \\draw[blue] (\\radio,0) -- (\\radio,\\altura);
\\end{tikzpicture}
"
cat(tikz_code)
@
```

**Corrección:**
```r
radio_cilindro <- 5
altura_cilindro <- 10

# Texto en Question:
El cilindro tiene radio de `r radio_cilindro` cm y altura de `r altura_cilindro` cm.

# TikZ (bien sincronizado):
<<echo=FALSE, results="asis">>=
tikz_code <- paste0("
\\begin{tikzpicture}
  \\def\\radio{", radio_cilindro, "}    % ✅ Sincronizado con R
  \\def\\altura{", altura_cilindro, "}  % ✅ Sincronizado con R
  \\draw[fill=blue] (0,0) ellipse (\\radio cm and 0.5cm);
  \\draw[blue] (\\radio,0) -- (\\radio,\\altura);
\\end{tikzpicture}
")
cat(tikz_code)
@
```

**Patrón de detección:**
```r
# Extraer variables R definidas
vars_r <- grep("^[a-z_]+ <-", contenido_rmd, value = TRUE)

# Extraer variables TikZ hardcodeadas
vars_tikz_hardcoded <- grep("\\\\def\\\\[a-z]+\\{[0-9.]+\\}", contenido_rmd, value = TRUE)

if (length(vars_tikz_hardcoded) > 0) {
  warning("ERR_C2: Variables TikZ hardcodeadas en lugar de usar paste0() con R")
}
```

### Tipo 3: Coherencia de Código (ERR_C3)

**Definición**: Verificar sincronización entre chunks R, Python, y TikZ.

**Qué se verifica:**

1. **Variables R → Python (reticulate)**
   - Variables pasadas correctamente con `r.variable`
   - Tipos de datos compatibles
   - Arrays/listas correctamente transferidos

2. **Variables R → TikZ**
   - Uso de `paste0()` para insertar valores
   - No usar variables formateadas en cálculos
   - Definiciones `\def\variable{valor}` sincronizadas

3. **Orden de ejecución**
   - Variables definidas antes de usarse
   - No hay referencias circulares
   - Chunks en orden lógico

4. **Funciones matemáticas**
   - `abs()`, `sqrt()`, etc. sobre números (no strings)
   - Formateo DESPUÉS de cálculos
   - No operar sobre variables ya formateadas

**Checklist detallado:**
```
□ R→Python: Variables transferidas con r.variable
□ R→TikZ: Variables insertadas con paste0()
□ Orden: Variables definidas antes de usar
□ Funciones: Operan sobre números, no strings
□ Formato: sprintf() DESPUÉS de cálculos
□ Semilla: set.seed() genera datos válidos
□ Scope: Variables disponibles en contexto correcto
```

**Ejemplo de error ERR_C3:**
```r
# Definir variable
b <- -2.5

# Formatear demasiado pronto
b_formateado <- sprintf("%.1f", b)  # b_formateado = "-2.5" (string)

# Usar función matemática sobre string
ecuacion <- paste0("y = x - ", abs(b_formateado))  # ❌ abs() sobre string

# Python intenta usar
```python
import matplotlib.pyplot as plt
b_python = r.b_formateado  # Recibe string "-2.5"
y = [x - abs(b_python) for x in range(10)]  # ❌ Error: abs() sobre string
```
```

**Corrección:**
```r
# Definir variable numérica
b <- -2.5

# Calcular PRIMERO sobre número
b_abs <- abs(b)  # ✅ abs() sobre número = 2.5

# Formatear DESPUÉS
b_formateado <- sprintf("%.1f", b_abs)  # "2.5" (string para mostrar)

# Ecuación usa variable formateada solo para texto
ecuacion <- paste0("y = x - ", b_formateado)  # ✅ Solo concatenación

# Python usa variable numérica original
```python
import matplotlib.pyplot as plt
b_python = r.b  # ✅ Recibe número -2.5
y = [x - abs(b_python) for x in range(10)]  # ✅ abs() sobre número
```
```

**Patrón de detección:**
```r
# Detectar funciones matemáticas sobre variables formateadas
patron_error <- "\\b(abs|sqrt|round|floor|ceiling)\\([^)]*formateado"

lineas_con_error <- grep(patron_error, contenido_rmd, value = TRUE)

if (length(lineas_con_error) > 0) {
  warning("ERR_C3: Función matemática aplicada sobre variable formateada (string)")
}
```

### Tipo 4: Coherencia de Metadatos (ERR_C4)

**Definición**: Verificar que metadatos ICFES estén completos y sean consistentes.

**Qué se verifica:**

1. **Metadatos obligatorios presentes**
   - `exname`: Nombre descriptivo
   - `extype`: schoice o cloze
   - `exsolution`: Respuesta correcta en formato correcto
   - `exshuffle`: TRUE (para aleatorizar)
   - `extol`: 0.01 (tolerancia numérica)

2. **Metadatos ICFES (6 dimensiones)**
   - `exextra[Type]`: SCHOICE o CLOZE
   - `exextra[Competencia]`: Interpretación|Formulación|Argumentación
   - `exextra[Componente]`: Numérico-Variacional|Geométrico-Métrico|Aleatorio
   - `exextra[Afirmacion]`: Descripción específica
   - `exextra[Evidencia]`: Descripción específica
   - `exextra[Nivel]`: 1|2|3|4

3. **Formato de exsolution**
   - **SCHOICE**: String binario (ej: "10000" = 5 opciones, 1ra correcta)
   - **CLOZE**: Formato por tipo (ej: "num|string|mchoice:10000")

4. **Answerlist vs exsolution**
   - Número de opciones coincide con longitud de exsolution
   - Posición de "1" en exsolution coincide con respuesta correcta

**Checklist detallado:**
```
□ exname: Presente y descriptivo
□ extype: "schoice" o "cloze" (lowercase)
□ exsolution: Formato correcto según extype
□ exshuffle: TRUE
□ extol: 0.01 o apropiado
□ exextra[Type]: SCHOICE o CLOZE (uppercase)
□ exextra[Competencia]: Valor válido ICFES
□ exextra[Componente]: Valor válido ICFES
□ exextra[Afirmacion]: Presente y descriptivo
□ exextra[Evidencia]: Presente y descriptivo
□ exextra[Nivel]: 1, 2, 3, o 4
□ Answerlist: Número de items = longitud exsolution
```

**Ejemplo de error ERR_C4:**
```yaml
exname: Area Circulo
extype: schoice
exsolution: 1  # ❌ Debería ser "10000" para 5 opciones
exshuffle: TRUE
extol: 0.01

exextra[Type]: SCHOICE
exextra[Competencia]: Formulación y Ejecución
# ❌ Falta: exextra[Componente]
exextra[Afirmacion]: Calcula áreas de figuras planas
# ❌ Falta: exextra[Evidencia]
exextra[Nivel]: 2
```

**Corrección:**
```yaml
exname: Area Circulo
extype: schoice
exsolution: 10000  # ✅ Correcto: 5 opciones, 1ra correcta
exshuffle: TRUE
extol: 0.01

exextra[Type]: SCHOICE
exextra[Competencia]: Formulación y Ejecución
exextra[Componente]: Geométrico-Métrico  # ✅ Agregado
exextra[Afirmacion]: Calcula áreas de figuras planas usando fórmulas
exextra[Evidencia]: Aplica fórmula del área del círculo correctamente  # ✅ Agregado
exextra[Nivel]: 2
```

**Patrón de detección:**
```r
verificar_metadatos <- function(archivo_rmd) {
  contenido <- readLines(archivo_rmd, warn = FALSE)
  errores <- c()

  # Obligatorios
  if (!any(grepl("^exname:", contenido))) errores <- c(errores, "exname faltante")
  if (!any(grepl("^extype:", contenido))) errores <- c(errores, "extype faltante")
  if (!any(grepl("^exsolution:", contenido))) errores <- c(errores, "exsolution faltante")

  # ICFES
  if (!any(grepl("^exextra\\[Componente\\]:", contenido)))
    errores <- c(errores, "exextra[Componente] faltante")
  if (!any(grepl("^exextra\\[Evidencia\\]:", contenido)))
    errores <- c(errores, "exextra[Evidencia] faltante")

  return(errores)
}
```

## 🔍 Proceso de Validación Paso a Paso

### PASO 1: Cargar y Parsear Archivo .Rmd

**Objetivo**: Extraer todos los elementos necesarios para validación.

**Qué extraer:**
- Metadatos (YAML header)
- Chunks de código R
- Chunks de código Python (si usa reticulate)
- Código TikZ embebido
- Texto de Question
- Answerlist

**Código R:**
```r
cargar_rmd <- function(archivo_rmd) {
  if (!file.exists(archivo_rmd)) {
    stop("Archivo no encontrado: ", archivo_rmd)
  }

  contenido <- readLines(archivo_rmd, warn = FALSE, encoding = "UTF-8")

  # Extraer secciones
  metadatos <- extraer_metadatos(contenido)
  chunks_r <- extraer_chunks_r(contenido)
  chunks_python <- extraer_chunks_python(contenido)
  codigo_tikz <- extraer_codigo_tikz(contenido)
  texto_question <- extraer_texto_question(contenido)
  answerlist <- extraer_answerlist(contenido)

  return(list(
    contenido = contenido,
    metadatos = metadatos,
    chunks_r = chunks_r,
    chunks_python = chunks_python,
    codigo_tikz = codigo_tikz,
    texto_question = texto_question,
    answerlist = answerlist
  ))
}

extraer_metadatos <- function(contenido) {
  # Buscar líneas entre primeros dos "---"
  inicio <- which(contenido == "---")[1]
  fin <- which(contenido == "---")[2]

  if (is.na(inicio) || is.na(fin)) return(NULL)

  return(contenido[(inicio+1):(fin-1)])
}

extraer_chunks_r <- function(contenido) {
  # Buscar bloques <<...>>= hasta @
  inicio_chunks <- grep("^<<.*>>=", contenido)
  fin_chunks <- grep("^@$", contenido)

  chunks <- list()
  for (i in seq_along(inicio_chunks)) {
    chunks[[i]] <- contenido[(inicio_chunks[i]+1):(fin_chunks[i]-1)]
  }

  return(chunks)
}
```

### PASO 2: Validar Coherencia Matemática

**Ejecutar chunk R y verificar:**
```r
validar_coherencia_matematica <- function(chunks_r, metadatos) {
  errores <- c()

  # Ejecutar chunks R en entorno temporal
  env_temp <- new.env()
  for (chunk in chunks_r) {
    tryCatch({
      eval(parse(text = chunk), envir = env_temp)
    }, error = function(e) {
      errores <- c(errores, paste("ERR_C1: Error en chunk R:", e$message))
    })
  }

  # Verificar que exsolution coincida con respuesta calculada
  exsolution_linea <- metadatos[grep("^exsolution:", metadatos)]
  if (length(exsolution_linea) > 0) {
    exsolution <- sub("^exsolution:\\s*", "", exsolution_linea)

    # Si es SCHOICE, verificar posición del "1"
    if (grepl("^[01]+$", exsolution)) {
      posicion_correcta <- which(strsplit(exsolution, "")[[1]] == "1")[1]

      # Verificar que coincida con opciones[posicion_correcta]
      if (exists("opciones", envir = env_temp)) {
        opciones <- get("opciones", envir = env_temp)
        if (exists("area_redondeada", envir = env_temp)) {
          respuesta_esperada <- get("area_redondeada", envir = env_temp)

          if (abs(opciones[posicion_correcta] - respuesta_esperada) > 0.01) {
            errores <- c(errores, "ERR_C1: exsolution no coincide con cálculo esperado")
          }
        }
      }
    }
  }

  return(errores)
}
```

### PASO 3: Validar Coherencia Imagen-Texto

**Comparar variables R usadas en texto vs código gráfico:**
```r
validar_coherencia_imagen_texto <- function(chunks_r, codigo_tikz, texto_question) {
  errores <- c()

  # Extraer variables R definidas
  vars_r <- extraer_variables_r(chunks_r)

  # Extraer variables usadas en texto (inline R: `r variable`)
  vars_texto <- extraer_variables_inline(texto_question)

  # Extraer variables TikZ hardcodeadas
  vars_tikz_hardcoded <- grep("\\\\def\\\\[a-z_]+\\{[0-9.-]+\\}", codigo_tikz, value = TRUE)

  if (length(vars_tikz_hardcoded) > 0) {
    # Verificar si esas variables existen en R
    nombres_tikz <- sub(".*\\\\def\\\\([a-z_]+)\\{.*", "\\1", vars_tikz_hardcoded)

    for (nombre in nombres_tikz) {
      # Convertir de TikZ a R (ej: \radio → radio_cilindro)
      if (!nombre %in% names(vars_r)) {
        errores <- c(errores, paste0("ERR_C2: Variable TikZ '", nombre,
                                      "' hardcodeada, debería usar paste0() con R"))
      }
    }
  }

  # Verificar que todas las variables inline existan en R
  for (var in vars_texto) {
    if (!var %in% names(vars_r)) {
      errores <- c(errores, paste0("ERR_C2: Variable '", var,
                                    "' usada en texto pero no definida en R"))
    }
  }

  return(errores)
}
```

### PASO 4: Validar Coherencia de Código

**Detectar funciones matemáticas sobre strings:**
```r
validar_coherencia_codigo <- function(contenido) {
  errores <- c()

  # Patrón: función matemática sobre variable con "formateado" en nombre
  patron_abs_format <- "abs\\([^)]*formateado"
  patron_sqrt_format <- "sqrt\\([^)]*formateado"
  patron_round_format <- "round\\([^)]*formateado"

  patrones <- c(patron_abs_format, patron_sqrt_format, patron_round_format)
  funciones <- c("abs", "sqrt", "round")

  for (i in seq_along(patrones)) {
    lineas_error <- grep(patrones[i], contenido, value = TRUE)

    if (length(lineas_error) > 0) {
      errores <- c(errores, paste0("ERR_C3: Función ", funciones[i],
                                    "() aplicada sobre variable formateada (string)"))
      errores <- c(errores, paste0("  Línea: ", lineas_error[1]))
    }
  }

  # Verificar orden de definición de variables
  lineas_asignacion <- grep("<-", contenido)
  for (i in lineas_asignacion) {
    linea <- contenido[i]

    # Extraer variable asignada
    var_asignada <- sub("^\\s*([a-z_][a-z0-9_]*)\\s*<-.*", "\\1", linea)

    # Verificar si usa otras variables
    vars_usadas <- extraer_vars_usadas_en_linea(linea)

    for (var_usada in vars_usadas) {
      # Verificar si esa variable fue definida antes
      lineas_previas_def <- grep(paste0("^\\s*", var_usada, "\\s*<-"),
                                   contenido[1:(i-1)])

      if (length(lineas_previas_def) == 0) {
        errores <- c(errores, paste0("ERR_C3: Variable '", var_usada,
                                      "' usada antes de definirse (línea ", i, ")"))
      }
    }
  }

  return(errores)
}
```

### PASO 5: Validar Coherencia de Metadatos

**Verificar presencia y formato:**
```r
validar_coherencia_metadatos <- function(metadatos, answerlist) {
  errores <- c()

  # Lista de metadatos obligatorios
  obligatorios <- c("exname", "extype", "exsolution", "exshuffle", "extol")

  for (campo in obligatorios) {
    if (!any(grepl(paste0("^", campo, ":"), metadatos))) {
      errores <- c(errores, paste0("ERR_C4: Metadato obligatorio '", campo, "' faltante"))
    }
  }

  # Metadatos ICFES
  icfes <- c("Type", "Competencia", "Componente", "Afirmacion", "Evidencia", "Nivel")

  for (campo in icfes) {
    if (!any(grepl(paste0("^exextra\\[", campo, "\\]:"), metadatos))) {
      errores <- c(errores, paste0("ERR_C4: Metadato ICFES 'exextra[", campo, "]' faltante"))
    }
  }

  # Verificar formato de exsolution
  exsolution_linea <- metadatos[grep("^exsolution:", metadatos)]
  if (length(exsolution_linea) > 0) {
    exsolution <- sub("^exsolution:\\s*", "", exsolution_linea)

    extype_linea <- metadatos[grep("^extype:", metadatos)]
    if (length(extype_linea) > 0) {
      extype <- sub("^extype:\\s*", "", extype_linea)

      if (extype == "schoice") {
        # Debe ser string binario (ej: "10000")
        if (!grepl("^[01]+$", exsolution)) {
          errores <- c(errores, "ERR_C4: exsolution para SCHOICE debe ser binario (ej: '10000')")
        } else {
          # Verificar que longitud coincida con Answerlist
          num_opciones <- nchar(exsolution)
          num_answerlist <- length(answerlist)

          if (num_opciones != num_answerlist) {
            errores <- c(errores, paste0("ERR_C4: exsolution tiene ", num_opciones,
                                          " caracteres pero Answerlist tiene ",
                                          num_answerlist, " items"))
          }
        }
      }
    }
  }

  return(errores)
}
```

### PASO 6: Consolidar Resultados

**Generar reporte unificado:**
```r
consolidar_resultados <- function(errores_matematica, errores_imagen_texto,
                                   errores_codigo, errores_metadatos) {

  total_errores <- c(errores_matematica, errores_imagen_texto,
                      errores_codigo, errores_metadatos)

  reporte <- list(
    coherencia_matematica = list(
      estado = if (length(errores_matematica) == 0) "OK" else "ERROR",
      errores = errores_matematica
    ),
    coherencia_imagen_texto = list(
      estado = if (length(errores_imagen_texto) == 0) "OK" else "ERROR",
      errores = errores_imagen_texto
    ),
    coherencia_codigo = list(
      estado = if (length(errores_codigo) == 0) "OK" else "ERROR",
      errores = errores_codigo
    ),
    coherencia_metadatos = list(
      estado = if (length(errores_metadatos) == 0) "OK" else "ERROR",
      errores = errores_metadatos
    ),
    estado_general = if (length(total_errores) == 0) "APROBADO" else "REQUIERE_CORRECCION",
    total_errores = length(total_errores)
  )

  return(reporte)
}
```

### PASO 7: Generar Reporte y Decidir Siguiente Acción

**Formato de reporte:**
```r
imprimir_reporte <- function(reporte) {
  cat("\n")
  cat("╔════════════════════════════════════════════════╗\n")
  cat("║     REPORTE DE COHERENCIA - FASE 2             ║\n")
  cat("╠════════════════════════════════════════════════╣\n")

  # Coherencia Matemática
  cat(sprintf("║ Coherencia Matemática:    %-20s ║\n",
              if (reporte$coherencia_matematica$estado == "OK") "✅ OK" else "⚠️ ERRORES"))
  if (length(reporte$coherencia_matematica$errores) > 0) {
    for (error in reporte$coherencia_matematica$errores) {
      cat(sprintf("║   → %-42s ║\n", substr(error, 1, 42)))
    }
  }

  # Coherencia Imagen-Texto
  cat(sprintf("║ Coherencia Imagen-Texto:  %-20s ║\n",
              if (reporte$coherencia_imagen_texto$estado == "OK") "✅ OK" else "⚠️ ERRORES"))
  if (length(reporte$coherencia_imagen_texto$errores) > 0) {
    for (error in reporte$coherencia_imagen_texto$errores) {
      cat(sprintf("║   → %-42s ║\n", substr(error, 1, 42)))
    }
  }

  # Coherencia de Código
  cat(sprintf("║ Coherencia de Código:     %-20s ║\n",
              if (reporte$coherencia_codigo$estado == "OK") "✅ OK" else "⚠️ ERRORES"))
  if (length(reporte$coherencia_codigo$errores) > 0) {
    for (error in reporte$coherencia_codigo$errores) {
      cat(sprintf("║   → %-42s ║\n", substr(error, 1, 42)))
    }
  }

  # Coherencia de Metadatos
  cat(sprintf("║ Coherencia de Metadatos:  %-20s ║\n",
              if (reporte$coherencia_metadatos$estado == "OK") "✅ OK" else "⚠️ ERRORES"))
  if (length(reporte$coherencia_metadatos$errores) > 0) {
    for (error in reporte$coherencia_metadatos$errores) {
      cat(sprintf("║   → %-42s ║\n", substr(error, 1, 42)))
    }
  }

  cat("╠════════════════════════════════════════════════╣\n")
  cat(sprintf("║ Estado General: %-30s ║\n", reporte$estado_general))
  cat(sprintf("║ Total Errores:  %-30d ║\n", reporte$total_errores))
  cat("╚════════════════════════════════════════════════╝\n")
  cat("\n")

  # Decisión siguiente acción
  if (reporte$estado_general == "APROBADO") {
    cat("✅ FASE 2 COMPLETADA: Sin errores de coherencia.\n")
    cat("📋 Siguiente: Aprobar para producción (si FASE 1 también pasó).\n")
  } else {
    cat("⚠️ FASE 2 DETECTÓ ERRORES: Requiere corrección.\n")
    cat("📋 Siguiente: FASE 3 (diagnosticar-errores) para clasificación y corrección.\n")
  }
}
```

## 🎓 Ejemplos Completos de Validación

### Ejemplo 1: Ejercicio con Coherencia Perfecta

**Archivo**: `area_circulo.Rmd`

**Contenido:**
```yaml
exname: Area Circulo Perfecto
extype: schoice
exsolution: 10000
exshuffle: TRUE
extol: 0.01

exextra[Type]: SCHOICE
exextra[Competencia]: Formulación y Ejecución
exextra[Componente]: Geométrico-Métrico
exextra[Afirmacion]: Calcula áreas de figuras planas
exextra[Evidencia]: Aplica fórmula del área del círculo
exextra[Nivel]: 2
```

```r
<<echo=FALSE, results="hide">>=
# Definir variables
radio <- sample(3:8, 1)

# Calcular área (correcto)
area <- pi * radio^2
area_redondeada <- round(area, 2)

# Generar distractores plausibles
opciones <- c(
  area_redondeada,              # Correcta
  round(pi * radio, 2),         # Olvidó ^2
  round(2 * pi * radio, 2),     # Confundió con perímetro
  round(4 * pi * radio^2, 2),   # Multiplicó por 4
  round(pi * (radio + 1)^2, 2)  # Usó radio+1
)
@

Question
========
Un círculo tiene radio de `r radio` cm. ¿Cuál es su área?

Answerlist
==========
* `r opciones[1]` cm²
* `r opciones[2]` cm²
* `r opciones[3]` cm²
* `r opciones[4]` cm²
* `r opciones[5]` cm²
```

**Resultado de validación:**
```
╔════════════════════════════════════════════════╗
║     REPORTE DE COHERENCIA - FASE 2             ║
╠════════════════════════════════════════════════╣
║ Coherencia Matemática:    ✅ OK                ║
║ Coherencia Imagen-Texto:  ✅ OK                ║
║ Coherencia de Código:     ✅ OK                ║
║ Coherencia de Metadatos:  ✅ OK                ║
╠════════════════════════════════════════════════╣
║ Estado General: APROBADO                       ║
║ Total Errores:  0                              ║
╚════════════════════════════════════════════════╝

✅ FASE 2 COMPLETADA: Sin errores de coherencia.
📋 Siguiente: Aprobar para producción (si FASE 1 también pasó).
```

### Ejemplo 2: Error ERR_C2 - Imagen-Texto Desincronizado

**Archivo**: `cilindro_mal_sincronizado.Rmd`

**Chunk R:**
```r
<<echo=FALSE, results="hide">>=
radio_cilindro <- 5
altura_cilindro <- 10
volumen <- pi * radio_cilindro^2 * altura_cilindro
@
```

**Texto:**
```
Question
========
Un cilindro tiene radio de `r radio_cilindro` cm y altura de `r altura_cilindro` cm.

<<echo=FALSE, results="asis">>=
tikz_code <- "
\\begin{tikzpicture}
  \\def\\radio{3}   # ❌ Hardcodeado
  \\def\\altura{8}  # ❌ Hardcodeado
  \\draw[fill=blue] (0,0) ellipse (\\radio cm and 0.5cm);
\\end{tikzpicture}
"
cat(tikz_code)
@
```

**Resultado de validación:**
```
╔════════════════════════════════════════════════╗
║     REPORTE DE COHERENCIA - FASE 2             ║
╠════════════════════════════════════════════════╣
║ Coherencia Matemática:    ✅ OK                ║
║ Coherencia Imagen-Texto:  ⚠️ ERRORES           ║
║   → ERR_C2: Variable TikZ 'radio' hardcod...   ║
║   → ERR_C2: Variable TikZ 'altura' hardco...   ║
║ Coherencia de Código:     ✅ OK                ║
║ Coherencia de Metadatos:  ✅ OK                ║
╠════════════════════════════════════════════════╣
║ Estado General: REQUIERE_CORRECCION            ║
║ Total Errores:  2                              ║
╚════════════════════════════════════════════════╝

⚠️ FASE 2 DETECTÓ ERRORES: Requiere corrección.
📋 Siguiente: FASE 3 (diagnosticar-errores) para clasificación y corrección.
```

**Corrección aplicada:**
```r
tikz_code <- paste0("
\\begin{tikzpicture}
  \\def\\radio{", radio_cilindro, "}   # ✅ Sincronizado
  \\def\\altura{", altura_cilindro, "}  # ✅ Sincronizado
  \\draw[fill=blue] (0,0) ellipse (\\radio cm and 0.5cm);
\\end{tikzpicture}
")
```

### Ejemplo 3: Error ERR_C3 - Función sobre String

**Archivo**: `ecuacion_mal_calculada.Rmd`

**Código problemático:**
```r
<<echo=FALSE, results="hide">>=
b <- -2.5
b_formateado <- sprintf("%.1f", b)  # String "-2.5"

# ❌ abs() sobre string
ecuacion <- paste0("y = x - ", abs(b_formateado))
@
```

**Resultado de validación:**
```
╔════════════════════════════════════════════════╗
║     REPORTE DE COHERENCIA - FASE 2             ║
╠════════════════════════════════════════════════╣
║ Coherencia Matemática:    ✅ OK                ║
║ Coherencia Imagen-Texto:  ✅ OK                ║
║ Coherencia de Código:     ⚠️ ERRORES           ║
║   → ERR_C3: Función abs() aplicada sobre v...  ║
║   → Línea: ecuacion <- paste0("y = x - ", ... ║
║ Coherencia de Metadatos:  ✅ OK                ║
╠════════════════════════════════════════════════╣
║ Estado General: REQUIERE_CORRECCION            ║
║ Total Errores:  1                              ║
╚════════════════════════════════════════════════╝

⚠️ FASE 2 DETECTÓ ERRORES: Requiere corrección.
📋 Siguiente: FASE 3 (diagnosticar-errores) para clasificación y corrección.
```

**Corrección aplicada:**
```r
b <- -2.5
b_abs <- abs(b)  # ✅ abs() sobre número
b_formateado <- sprintf("%.1f", b_abs)  # Formatear DESPUÉS
ecuacion <- paste0("y = x - ", b_formateado)
```

## ⚠️ Errores Comunes Detectados

### Patrón 1: Variables TikZ Hardcodeadas

**Detección:**
- Buscar `\def\variable{valor}` con valores numéricos literales
- Comparar con variables R definidas

**Solución:**
- Usar `paste0()` para insertar valores R en TikZ
- Mantener sincronización automática

### Patrón 2: Funciones Matemáticas sobre Strings

**Detección:**
- Buscar `abs(`, `sqrt(`, `round(` seguidos de variables con "formateado" o "_str"
- Verificar tipo de dato (numérico vs string)

**Solución:**
- Aplicar función matemática ANTES de formatear
- Guardar resultado numérico en variable separada

### Patrón 3: exsolution Desincronizado

**Detección:**
- Longitud de exsolution ≠ número de items en Answerlist
- Posición del "1" no coincide con respuesta correcta

**Solución:**
- Contar items en Answerlist
- Generar exsolution con longitud correcta
- Verificar que primera opción (o la marcada) sea la correcta

### Patrón 4: Metadatos ICFES Incompletos

**Detección:**
- Faltan campos obligatorios (Componente, Evidencia, etc.)
- Valores fuera del dominio válido

**Solución:**
- Consultar @.claude/skills/analizar-icfes/skill.md
- Completar las 6 dimensiones ICFES

## 🔗 Referencias y Documentación

### Archivos de Referencia
- @.claude/rules/codigo-rmd.md - Reglas de código y metadatos
- @.claude/rules/ciclo-validacion.md - Especificación del ciclo completo
- @.claude/docs/patrones-errores-conocidos.md - Errores ERR_C1-C5

### Ejemplos Funcionales
- @A-Produccion/Ejemplos-Funcionales-Rmd/ - Ejercicios validados (FUENTE DE VERDAD)

### Skills Relacionados
- @.claude/skills/validar-renderizado/skill.md - FASE 1 (antes de este)
- @.claude/skills/diagnosticar-errores/skill.md - FASE 3 (después de este)
- @.claude/skills/analizar-icfes/skill.md - Análisis de dimensiones ICFES

## 📊 Output Final Esperado

Después de usar este skill, debes tener:

```markdown
# Reporte de Coherencia FASE 2: [Nombre del Archivo]

## 1. Coherencia Matemática
[Estado: OK / ERRORES]
[Lista de errores ERR_C1 si aplica]

## 2. Coherencia Imagen-Texto
[Estado: OK / ERRORES]
[Lista de errores ERR_C2 si aplica]

## 3. Coherencia de Código
[Estado: OK / ERRORES]
[Lista de errores ERR_C3 si aplica]

## 4. Coherencia de Metadatos
[Estado: OK / ERRORES]
[Lista de errores ERR_C4 si aplica]

## 5. Estado General
[APROBADO / REQUIERE_CORRECCION]

## 6. Siguiente Acción
[Aprobar para producción / Continuar a FASE 3]
```

## 🚀 Integración con Ciclo de Validación

Este skill es el **puente entre renderizado y diagnóstico**:

```
validar-renderizado (FASE 1)
    ↓ [Si pasa 4 formatos]
    ↓
validar-coherencia (FASE 2) ← ESTE SKILL
    ↓ [Clasifica errores ERR_C1-C5]
    ↓
diagnosticar-errores (FASE 3)
    ↓ [Si hay errores]
    ↓
SUBFASE 3A: corregir-graficos (o skill apropiado)
    ↓
SUBFASE 3B: VOLVER A FASE 1 (revalidación)
    ↓
SUBFASE 3C: Documentar solución (solo si éxito 100%)
```

**Criterio de decisión:**
- **Si coherencia 100% + renderizado 100%** → Aprobar para producción
- **Si cualquier error** → Continuar a FASE 3

---

**Última actualización**: 2025-12-30
**Versión**: 2.0 (Progressive Disclosure)
**Basado en**: Documentación oficial ICFES 2025 + Claude Code best practices
