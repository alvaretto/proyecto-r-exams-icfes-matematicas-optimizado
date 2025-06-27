# BIBLIOTECA DE SOLUCIONES PARA ERRORES COMUNES EN R-EXAMS ICFES

## Catálogo de Errores Recurrentes y Sus Soluciones Probadas

---

## CATEGORÍA A: ERRORES GRAMATICALES Y DE CONCORDANCIA

### A1: Concordancia de Género - "La conteo" → "El conteo"

**PROBLEMA DETECTADO:**
```r
# INCORRECTO
termino_cantidad <- sample(c("cantidad", "número", "total", "suma", "conteo"), 1)
# Uso: "La `r termino_cantidad`" → "La conteo" ❌
```

**SOLUCIÓN PROBADA:**
```r
# CORRECTO
terminos_cantidad_data <- data.frame(
  termino = c("cantidad", "número", "total", "suma", "conteo"),
  articulo = c("La", "El", "El", "La", "El"),
  stringsAsFactors = FALSE
)
termino_seleccionado <- sample(1:nrow(terminos_cantidad_data), 1)
termino_cantidad <- terminos_cantidad_data$termino[termino_seleccionado]
articulo_cantidad <- terminos_cantidad_data$articulo[termino_seleccionado]

# Uso: "`r articulo_cantidad` `r termino_cantidad`" → "El conteo" ✅
```

### A2: Concordancia de Número - Singular/Plural

**PROBLEMA DETECTADO:**
```r
# INCORRECTO - No considera si hay 1 o múltiples elementos
"Los `r num_elementos` elemento"  # "Los 1 elemento" ❌
```

**SOLUCIÓN PROBADA:**
```r
# CORRECTO
elemento_texto <- ifelse(num_elementos == 1, "elemento", "elementos")
articulo_elemento <- ifelse(num_elementos == 1, "El", "Los")
# Uso: "`r articulo_elemento` `r num_elementos` `r elemento_texto`" ✅
```

### A3: Preposiciones con Números Ordinales

**PROBLEMA DETECTADO:**
```r
# INCORRECTO
"en la posición `r posicion`"  # "en la posición 1" ❌
```

**SOLUCIÓN PROBADA:**
```r
# CORRECTO
posicion_ordinal <- c("primera", "segunda", "tercera", "cuarta", "quinta")[posicion]
# Uso: "en la `r posicion_ordinal` posición" ✅
```

---

## CATEGORÍA B: ERRORES DE POSICIONAMIENTO TIKZ

### B1: Elementos Superpuestos

**PROBLEMA DETECTADO:**
```tikz
% INCORRECTO - Mismas coordenadas Y
\node at (0, 2) {Texto 1};
\node at (5, 2) {Texto 2};  % Se superponen visualmente
```

**SOLUCIÓN PROBADA:**
```tikz
% CORRECTO - Espaciado adecuado
\node at (0, 2.5) {Texto 1};
\node at (0, 1.5) {Texto 2};  % Separación mínima 1.0 unidad
```

### B2: Orden Incorrecto Tabla-Texto

**PROBLEMA DETECTADO:**
```r
# INCORRECTO - Tabla antes que texto explicativo
"% Tabla de datos\n",
"\\node[anchor=north west] at (0, 1.5) {...};\n",
"% Texto explicativo\n",
"\\node[anchor=north west] at (0, 0) {...};\n"
```

**SOLUCIÓN PROBADA:**
```r
# CORRECTO - Texto explicativo PRIMERO
"% Texto explicativo\n",
"\\node[anchor=north west, text width=12cm] at (0, 1.5) {\n",
"  \\textbf{La siguiente tabla muestra...}\n",
"};\n\n",
"% Tabla de datos DESPUÉS\n",
"\\node[anchor=north west] at (0, 0.5) {\n",
"  \\begin{tabular}{|c|c|c|c|c|}\n",
"    ...\n",
"  \\end{tabular}\n",
"};\n\n"
```

### B3: Coordenadas Inconsistentes

**PROBLEMA DETECTADO:**
```tikz
% INCORRECTO - Saltos grandes e inconsistentes
\node at (0, 5) {Elemento 1};
\node at (0, 2.7) {Elemento 2};
\node at (0, -0.3) {Elemento 3};
```

**SOLUCIÓN PROBADA:**
```tikz
% CORRECTO - Espaciado uniforme y lógico
\node at (0, 3.0) {Elemento 1};    % Título/Principal
\node at (0, 1.5) {Elemento 2};    % Contenido
\node at (0, 0.0) {Elemento 3};    % Pregunta/Final
% Espaciado estándar: 1.5 unidades entre elementos principales
```

---

## CATEGORÍA C: ERRORES DE GENERACIÓN DE DATOS

### C1: Opciones de Respuesta Duplicadas

**PROBLEMA DETECTADO:**
```r
# INCORRECTO - No verifica duplicados
opciones <- c(respuesta_correcta, distractor1, distractor2, distractor3)
opciones_finales <- sample(opciones, 4)  # Puede tener duplicados ❌
```

**SOLUCIÓN PROBADA:**
```r
# CORRECTO - Sistema anti-duplicados robusto
generar_opciones_unicas <- function(respuesta_correcta, num_distractores = 3) {
  opciones <- c(respuesta_correcta)
  intentos <- 0
  max_intentos <- 100
  
  while(length(unique(opciones)) < (num_distractores + 1) && intentos < max_intentos) {
    # Generar diferentes tipos de distractores
    if(intentos %% 4 == 0) {
      nuevo <- respuesta_correcta + sample(c(-3:-1, 1:3), 1)
    } else if(intentos %% 4 == 1) {
      nuevo <- round(respuesta_correcta * sample(c(0.8, 0.9, 1.1, 1.2), 1))
    } else if(intentos %% 4 == 2) {
      nuevo <- respuesta_correcta^2
    } else {
      nuevo <- respuesta_correcta + sample(-10:10, 1)
    }
    
    if(!nuevo %in% opciones && nuevo > 0) {
      opciones <- c(opciones, nuevo)
    }
    intentos <- intentos + 1
  }
  
  return(sample(unique(opciones)[1:(num_distractores + 1)]))
}
```

### C2: Valores Idénticos en Datasets

**PROBLEMA DETECTADO:**
```r
# INCORRECTO - Puede generar valores repetidos
datos <- sample(1:20, 10, replace = TRUE)  # Valores repetidos ❌
```

**SOLUCIÓN PROBADA:**
```r
# CORRECTO - Garantiza valores únicos
generar_dataset_unico <- function(n, rango_min = 1, rango_max = 50) {
  if((rango_max - rango_min + 1) < n) {
    stop("Rango insuficiente para generar valores únicos")
  }
  return(sample(rango_min:rango_max, n, replace = FALSE))
}

# Para casos donde se necesitan algunos duplicados controlados
generar_dataset_controlado <- function(n, prob_duplicado = 0.2) {
  datos_base <- sample(1:30, ceiling(n * (1 - prob_duplicado)), replace = FALSE)
  if(prob_duplicado > 0) {
    duplicados <- sample(datos_base, floor(n * prob_duplicado), replace = TRUE)
    datos_final <- c(datos_base, duplicados)
  } else {
    datos_final <- datos_base
  }
  return(sample(datos_final)[1:n])
}
```

---

## CATEGORÍA D: ERRORES DE COMPILACIÓN LATEX/TIKZ

### D1: Paquetes Faltantes

**PROBLEMA DETECTADO:**
```r
# INCORRECTO - Paquetes insuficientes
options(tikzLatexPackages = c("\\usepackage{tikz}"))
```

**SOLUCIÓN PROBADA:**
```r
# CORRECTO - Paquetes completos para ICFES
options(tikzLatexPackages = c(
  "\\usepackage{tikz}",
  "\\usepackage{colortbl}",
  "\\usepackage{amsmath}",
  "\\usepackage{array}",
  "\\usepackage{xcolor}",
  "\\usepackage{pgfplots}",
  "\\usetikzlibrary{shapes.geometric,arrows,positioning,calc}"
))
```

### D2: Caracteres Especiales Sin Escapar

**PROBLEMA DETECTADO:**
```r
# INCORRECTO
texto <- "El 50% de los datos"  # % sin escapar ❌
```

**SOLUCIÓN PROBADA:**
```r
# CORRECTO
escapar_latex <- function(texto) {
  texto <- gsub("%", "\\\\%", texto)
  texto <- gsub("\\$", "\\\\$", texto)
  texto <- gsub("#", "\\\\#", texto)
  texto <- gsub("&", "\\\\&", texto)
  return(texto)
}

texto <- escapar_latex("El 50% de los datos")  # "El 50\\% de los datos" ✅
```

---

## CATEGORÍA E: ERRORES DE ESTRUCTURA R-EXAMS

### E1: Headers YAML Incompletos

**PROBLEMA DETECTADO:**
```yaml
# INCORRECTO - Faltan campos obligatorios
---
output: html_document
---
```

**SOLUCIÓN PROBADA:**
```yaml
# CORRECTO - Header completo para ICFES
---
output:
  html_document: default
  word_document: default
  pdf_document: default
---
```

### E2: Configuración include_tikz Incorrecta

**PROBLEMA DETECTADO:**
```r
# INCORRECTO - Parámetros insuficientes
include_tikz(tikz_code, name = "figura")
```

**SOLUCIÓN PROBADA:**
```r
# CORRECTO - Configuración completa
include_tikz(tikz_final,
             name = "nombre_descriptivo",
             markup = "markdown",
             format = typ,
             packages = c("tikz", "colortbl", "amsmath", "array"),
             width = "14cm",
             options = list(pointsize = 12))
```

---

## PATRONES DE DETECCIÓN AUTOMÁTICA

### Función de Detección Integrada

```r
detectar_errores_comunes <- function(archivo_rmd) {
  contenido <- paste(readLines(archivo_rmd), collapse = "\n")
  errores <- list()
  
  # A1: Concordancia de género
  if(grepl("La (conteo|número|total)", contenido)) {
    errores$A1 <- "Concordancia de género incorrecta detectada"
  }
  
  # B2: Orden tabla-texto
  if(grepl("% Tabla de datos.*% Texto explicativo", contenido)) {
    errores$B2 <- "Orden incorrecto: tabla antes que texto"
  }
  
  # C1: Posibles duplicados
  if(grepl("sample.*opciones.*4" , contenido) && !grepl("unique", contenido)) {
    errores$C1 <- "Posible generación de opciones duplicadas"
  }
  
  # D2: Caracteres sin escapar
  if(grepl("[^\\\\][%$#&]", contenido)) {
    errores$D2 <- "Caracteres especiales sin escapar detectados"
  }
  
  return(errores)
}
```

---

## CHECKLIST DE APLICACIÓN RÁPIDA

### ✅ Antes de Compilar
- [ ] Ejecutar `detectar_errores_comunes(archivo)`
- [ ] Verificar concordancia de género en variables dinámicas
- [ ] Confirmar orden correcto en elementos TikZ
- [ ] Validar unicidad en opciones de respuesta

### ✅ Después de Compilar
- [ ] Verificar output visual (tabla después de texto)
- [ ] Confirmar que todas las opciones son diferentes
- [ ] Revisar gramática en el resultado final
- [ ] Validar cálculos matemáticos

---

**Nota**: Esta biblioteca debe actualizarse cada vez que se identifique un nuevo patrón de error recurrente.
