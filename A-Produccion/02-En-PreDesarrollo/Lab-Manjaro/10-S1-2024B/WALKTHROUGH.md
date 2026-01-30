# 🚶‍♂️ WALKTHROUGH COMPLETO: Teorema de Pitágoras - Entrenamiento Completo

## 📖 Índice
1. [Introducción](#introducción)
2. [Estructura del Archivo](#estructura)
3. [Explicación Bloque por Bloque](#bloques)
4. [Guía de Resolución](#resolución)
5. [Glosario](#glosario)

---

## 🎯 Introducción {#introducción}

Este ejercicio evalúa el Teorema de Pitágoras mediante **12 pasos progresivos** organizados en **4 fases**:

### Fases del Ejercicio
- **FASE I (Pasos 1-3):** Comprensión Conceptual
- **FASE II (Pasos 4-6):** Aplicación Numérica  
- **FASE III (Pasos 7-9):** Verificación y Análisis
- **FASE IV (Pasos 10-12):** Transferencia y Síntesis

### Características Técnicas
- ✅ Genera **300+ versiones únicas**
- ✅ Tipo **Cloze** (pregunta compuesta)
- ✅ Gráfico **TikZ dinámico**
- ✅ Compatible con **HTML, PDF, Moodle, Word**

---

## 🏗️ Estructura del Archivo {#estructura}

El archivo .Rmd contiene estos bloques principales:

```
📄 Archivo .Rmd
├── 🔧 BLOQUE 1: Encabezado YAML (líneas 1-8)
├── 🛠️ BLOQUE 2: Configuración Inicial (líneas 10-44)
├── 📦 BLOQUE 3: Funciones Auxiliares (líneas 46-72)
├── 🎲 BLOQUE 4: Generación de Datos (líneas 74-543)
├── 🔄 BLOQUE 5: Preparación de Soluciones (líneas 545-582)
├── ✅ BLOQUE 6: Prueba de Diversidad (líneas 584-626)
├── 🖼️ BLOQUE 7: Generación de Gráfico TikZ (líneas 628-680)
├── ❓ BLOQUE 8: Sección Question (líneas 682-778)
├── ✔️ BLOQUE 9: Sección Solution (líneas 780-859)
└── 📋 BLOQUE 10: Meta-información (líneas 861-877)
```

---

## 🔍 Explicación Bloque por Bloque {#bloques}

### 📌 BLOQUE 1: Encabezado YAML (líneas 1-8)

```yaml
---
output:
  html_document: default
  pdf_document:
    keep_tex: true
  word_document: default
---
```

**¿Qué hace este bloque?**
Define los formatos de salida permitidos para el ejercicio.

**Para novatos:**

- `html_document`: Genera archivo HTML para ver en navegador
- `pdf_document`: Genera PDF para imprimir
- `keep_tex: true`: Guarda archivos LaTeX intermedios (útil para depuración)
- `word_document`: Genera archivo Word

---

### 📌 BLOQUE 2: Configuración Inicial (líneas 10-44)

```r
Sys.setlocale(category = "LC_NUMERIC", locale = "C")
options(OutDec = ".")
options(scipen = 999)
options(digits = 10)
```

**¿Qué hace este bloque?**
Configura el entorno de trabajo de R para garantizar resultados consistentes.

**Explicación línea por línea:**

| Línea | ¿Qué hace? | Ejemplo |
|-------|-----------|---------|
| `Sys.setlocale("LC_NUMERIC", "C")` | Formato numérico estándar (punto decimal) | `3.14` en lugar de `3,14` |
| `options(OutDec = ".")` | Separador decimal es punto | `5.7` no `5,7` |
| `options(scipen = 999)` | Evita notación científica | `10000` no `1e+04` |
| `options(digits = 10)` | Muestra hasta 10 dígitos | Mayor precisión |

**Bibliotecas cargadas:**

```r
library(exams)      # Sistema de generación de ejercicios
library(reticulate) # Integración con Python (no usada aquí)
library(digest)     # Crear hashes únicos
library(testthat)   # Sistema de pruebas automáticas
library(knitr)      # Motor para R Markdown
```

**Variables importantes:**

```r
typ <- match_exams_device()  # Detecta formato: "html", "pdf", "moodle"
```

**Configuración de chunks:**

```r
knitr::opts_chunk$set(
  warning = FALSE,    # No mostrar advertencias
  message = FALSE,    # No mostrar mensajes
  echo = FALSE,       # No mostrar código fuente
  dev = c("png", "pdf"),  # Formatos de gráficos
  dpi = 150,          # Resolución de imágenes
  fig.pos = "H"       # Posición fija en PDF
)
```

---

### 📌 BLOQUE 3: Funciones Auxiliares (líneas 46-72)

Este bloque define funciones reutilizables para formatear datos.

#### **Función 1: `mchoice2string()`**

```r
mchoice2string <- function(x) {
  if (is.logical(x)) {
    return(paste(as.integer(x), collapse = ""))
  }
}
```

**Para novatos:**

Convierte respuestas de selección múltiple a string.

**Ejemplo:**

- Entrada: `c(TRUE, FALSE, TRUE, FALSE)` 
- Salida: `"1010"` (1=correcto, 0=incorrecto)

#### **Función 2: `formatear_entero()`**

```r
formatear_entero <- function(numero) {
  formatC(numero, format = "d", big.mark = "")
}
```

**Para novatos:**

Formatea números enteros sin separadores.

**Ejemplo:**

- Entrada: `1000`
- Salida: `"1000"` (no `"1,000"`)

#### **Función 3: `formato_estandar()`**

```r
formato_estandar <- function(x, decimales = 0) {
  if (decimales == 0) {
    return(as.character(as.integer(x)))
  } else {
    sprintf(paste0("%.", decimales, "f"), x)
  }
}
```

**Para novatos:**

Controla el número de decimales mostrados.

**Ejemplos:**

- `formato_estandar(3.7, 1)` → `"3.7"`
- `formato_estandar(5.999, 0)` → `"6"`
- `formato_estandar(12.345, 2)` → `"12.35"`

---

### 📌 BLOQUE 4: Generación de Datos (líneas 74-543)

Este es el **corazón del ejercicio**. Genera todos los valores aleatorios.

#### **Paso 1: Inicialización**

```r
set.seed(sample(1:10000, 1))
```

**Para novatos:**

Genera una semilla aleatoria diferente cada vez.
- **¿Por qué?** Para crear ejercicios únicos en cada compilación

#### **Paso 2: Ternas Pitagóricas**

```r
ternas_pedagogicas <- list(
  c(3, 4, 5),      # 3² + 4² = 9 + 16 = 25 = 5²
  c(5, 12, 13),    # 5² + 12² = 25 + 144 = 169 = 13²
  c(8, 15, 17),    # 8² + 15² = 64 + 225 = 289 = 17²
  ...              # Total: 20 ternas
)

terna_seleccionada <- sample(ternas_pedagogicas, 1)[[1]]
```

**Para novatos:**

Una **terna pitagórica** cumple: `a² + b² = c²`

El sistema elige aleatoriamente 1 de 20 ternas disponibles.

#### **Paso 3: Generación de Valores Decimales**

```r
# 1. Crear multiplicador decimal aleatorio
multiplicador <- round(runif(1, 1.1, 5.9), 1)  # Ej: 2.3

# 2. Aplicar multiplicador a la terna
valores_decimales <- terna_seleccionada * multiplicador
# Ej: (3, 4, 5) × 2.3 = (6.9, 9.2, 11.5)

# 3. Redondear a 1 decimal
cateto_a <- round(valores_ordenados[1], 1)      # 6.9
cateto_b <- round(valores_ordenados[2], 1)      # 9.2
hipotenusa_c <- round(valores_ordenados[3], 1)  # 11.5
```

**Para novatos:**

Este proceso garantiza que los valores sean:

- ✅ Decimales (más realista)
- ✅ Variados (no siempre números enteros)
- ✅ Válidos (cumplen el teorema)

#### **Paso 4: Validación Matemática**

```r
# Verificar que cumple el teorema (con tolerancia por redondeo)
if (abs((cateto_a^2 + cateto_b^2) - hipotenusa_c^2) > 1e-9) {
  # Si falla, usar terna (3,4,5) como respaldo
  multiplicador_respaldo <- round(runif(1, 1.1, 2.5), 1)
  cateto_a <- 3 * multiplicador_respaldo
  cateto_b <- 4 * multiplicador_respaldo
  hipotenusa_c <- 5 * multiplicador_respaldo
}

# Asegurar validez (detiene ejecución si falla)
stopifnot(abs((cateto_a^2 + cateto_b^2) - hipotenusa_c^2) < 1e-9)
stopifnot(hipotenusa_c > cateto_a && hipotenusa_c > cateto_b)
```

**Para novatos:**

- **Primera validación:** Verifica que a² + b² ≈ c²
- **Sistema de respaldo:** Si falla, usa terna segura (3,4,5)
- **`stopifnot()`:** Detiene si los valores son inválidos

#### **Paso 5: Contextos y Terminología**

```r
# 20 contextos diferentes
contextos_educativos <- c(
  "construcción de una rampa de acceso",
  "diseño de una escalera",
  "cálculo de distancias en un plano cartesiano",
  ...
)
contexto_seleccionado <- sample(contextos_educativos, 1)

# Terminología variada
terminos_cateto <- c("cateto", "lado", "base", "altura")
terminos_hipotenusa <- c("hipotenusa", "diagonal", "distancia")
terminos_teorema <- c("Teorema de Pitágoras", "fórmula pitagórica")

termino_cateto <- sample(terminos_cateto, 1)
termino_hipotenusa <- sample(terminos_hipotenusa, 1)
termino_teorema <- sample(terminos_teorema, 1)
```

**Para novatos:**

El ejercicio varía el **vocabulario** para enseñar al estudiante a reconocer conceptos con diferentes palabras.

**Ejemplo:**

En lugar de siempre decir "cateto", puede decir:

- "lado"
- "base"
- "altura"

#### **Paso 6: Los 12 Pasos del Ejercicio**

El ejercicio genera datos para 12 preguntas. Aquí explicamos los tipos:

##### **PASO 1: Identificación (schoice)**

```r
respuesta_correcta_paso1 <- "dos catetos y una hipotenusa"
distractores_paso1 <- c(
  "tres catetos",
  "una hipotenusa y dos ángulos agudos",
  "tres hipotenusas",
  ...  # 7 distractores disponibles
)

# Seleccionar 3 distractores al azar
opciones_seleccionadas_paso1 <- sample(distractores_paso1, 3)

# Mezclar respuesta correcta con distractores
opciones_paso1_mezcladas <- sample(c(
  respuesta_correcta_paso1, 
  opciones_seleccionadas_paso1
))

# Encontrar posición de la correcta
indice_correcto_paso1 <- which(
  opciones_paso1_mezcladas == respuesta_correcta_paso1
)
```

**Para novatos:**

| Concepto | Explicación |
|----------|-------------|
| **schoice** | Selección única (1 correcta de 4) |
| **Proceso** | 1. Define correcta<br>2. Elige 3 incorrectas al azar<br>3. Mezcla las 4<br>4. Guarda posición correcta |

**Ejemplo de salida:**
```
A) una hipotenusa y dos ángulos agudos
B) dos catetos y una hipotenusa  ← CORRECTA (índice=2)
C) tres catetos
D) dos hipotenusas y un cateto
```

##### **PASO 2: Reconocimiento (mchoice)**

```r
opciones_correctas_paso2 <- c(
  "Se aplica solo a triángulos rectángulos",
  "La hipotenusa es el lado más largo",
  "Se puede escribir como a² + b² = c²"
)

opciones_incorrectas_paso2 <- c(
  "Se aplica a cualquier triángulo",
  "Los catetos son siempre iguales",
  ...  # 5 incorrectas disponibles
)

# Crear mezcla: 3 correctas + 1 incorrecta aleatoria
opciones_paso2_mezcladas <- sample(c(
  opciones_correctas_paso2, 
  sample(opciones_incorrectas_paso2, 1)
))

# Vector lógico de respuestas
correctas_paso2 <- c(
  opciones_paso2_mezcladas[1] %in% opciones_correctas_paso2,
  opciones_paso2_mezcladas[2] %in% opciones_correctas_paso2,
  opciones_paso2_mezcladas[3] %in% opciones_correctas_paso2,
  opciones_paso2_mezcladas[4] %in% opciones_correctas_paso2
)
```

**Para novatos:**

| Concepto | Explicación |
|----------|-------------|
| **mchoice** | Selección múltiple (3 correctas de 4) |
| **Proceso** | 1. Toma 3 correctas fijas<br>2. Elige 1 incorrecta al azar<br>3. Mezcla las 4<br>4. Crea vector TRUE/FALSE |

**Ejemplo:**
```
☑ Se aplica solo a triángulos rectángulos (TRUE)
☐ Se aplica a cualquier triángulo (FALSE)
☑ La hipotenusa es el lado más largo (TRUE)
☑ Se puede escribir como a² + b² = c² (TRUE)

Vector: c(TRUE, FALSE, TRUE, TRUE)
String: "1011"
```

##### **PASOS 4-6, 8, 12: Respuestas Numéricas (num)**

```r
datos_pasos$paso4 <- list(
  pregunta = "Calcula 4.5²:",
  respuesta = round(cateto_a^2, 2),  # 20.25
  tipo = "num",
  tolerancia = 0.5
)
```

**Para novatos:**

| Campo | Explicación | Ejemplo |
|-------|-------------|---------|
| **tipo** | Respuesta numérica | `"num"` |
| **respuesta** | Valor correcto | `20.25` |
| **tolerancia** | Rango aceptado | `±0.5` → acepta 19.75 a 20.75 |

**¿Por qué tolerancia?**
Evita problemas con redondeo. Si el estudiante calcula `20.24` o `20.26`, se acepta como correcto.

##### **PASO 12: Transferencia**

```r
# Genera un NUEVO problema diferente
nueva_terna <- sample(
  list(c(9, 12, 15), c(12, 16, 20), c(15, 20, 25)), 
  1
)[[1]]

multiplicador_nuevo <- round(runif(1, 1.1, 3.5), 1)

nuevo_cateto_a <- round(nueva_terna[1] * multiplicador_nuevo, 1)
nuevo_cateto_b <- round(nueva_terna[2] * multiplicador_nuevo, 1)
nueva_hipotenusa <- round(nueva_terna[3] * multiplicador_nuevo, 1)
```

**Para novatos:**
Este paso evalúa si el estudiante puede aplicar lo aprendido a un problema completamente nuevo con valores diferentes.

---

### 📌 BLOQUE 5: Preparación de Soluciones (líneas 545-582)

```r
soluciones_cloze <- c()
tipos_cloze <- c()
tolerancias_cloze <- c()

for(i in 1:12) {
  paso <- datos$pasos[[i]]

  if(paso$tipo == "schoice") {
    # Crear vector: 0s con 1 en posición correcta
    solucion_vector <- rep(0, length(paso$opciones))
    solucion_vector[paso$correcta] <- 1
    soluciones_cloze[i] <- paste(solucion_vector, collapse = "")
    tipos_cloze[i] <- "schoice"
    tolerancias_cloze[i] <- 0

  } else if(paso$tipo == "mchoice") {
    # Vector lógico de correctas
    soluciones_cloze[i] <- paste(as.integer(paso$correctas), collapse = "")
    tipos_cloze[i] <- "mchoice"
    tolerancias_cloze[i] <- 0

  } else if(paso$tipo == "num") {
    # Valor numérico directo
    soluciones_cloze[i] <- as.character(paso$respuesta)
    tipos_cloze[i] <- "num"
    tolerancias_cloze[i] <- paso$tolerancia
  }
}

# Unir con "|" para formato R-exams
solucion_final <- paste(soluciones_cloze, collapse = "|")
tipos_final <- paste(tipos_cloze, collapse = "|")
tolerancias_final <- paste(tolerancias_cloze, collapse = "|")
```

**¿Qué hace este bloque?**
Convierte las respuestas a formato compatible con R-exams.

**Para novatos:**

**Ejemplo de conversión:**

| Paso | Tipo | Respuesta | Formato exams |
|------|------|-----------|---------------|
| 1 | schoice | Correcta es 2ª de 4 | `"0100"` |
| 2 | mchoice | Correctas: 1, 3, 4 | `"1011"` |
| 3 | schoice | Correcta es 3ª de 4 | `"0010"` |
| 4 | num | 20.25 | `"20.25"` |
| 5 | num | 36.00 | `"36.00"` |
| ... | ... | ... | ... |

**Salida final:**
```r
solucion_final = "0100|1011|0010|20.25|36.00|56.25|..."
tipos_final = "schoice|mchoice|schoice|num|num|num|..."
tolerancias_final = "0|0|0|0.5|0.5|0.5|..."
```

---

### 📌 BLOQUE 6: Prueba de Diversidad (líneas 584-626)

```r
versiones <- list()
hashes_unicos <- c()

for(i in 1:500) {
  set.seed(sample(1:100000, 1))
  datos_test <- generar_datos_completos()
  
  # Crear huella digital única
  hash_elementos <- paste(
    datos_test$cateto_a,
    datos_test$cateto_b, 
    datos_test$hipotenusa_c,
    datos_test$contexto,
    ...
  )
  
  hash_final <- digest::digest(hash_elementos)
  hashes_unicos <- c(hashes_unicos, hash_final)
}

n_versiones_unicas <- length(unique(hashes_unicos))

if(n_versiones_unicas >= 300) {
  cat("✅ Prueba EXITOSA:", n_versiones_unicas, "versiones únicas\n")
} else {
  warning("⚠️ Solo", n_versiones_unicas, "versiones únicas")
}
```

**¿Qué hace este bloque?**
Verifica que el ejercicio puede generar ≥300 versiones diferentes.

**Para novatos:**

**Proceso:**

1. Genera 500 versiones del ejercicio
2. Calcula hash (huella digital) de cada una
3. Cuenta cuántos hashes son únicos
4. Valida: ≥300 únicos = ✅ / <300 = ⚠️

**¿Qué es un hash?**
Una cadena única que representa el contenido:

```
Versión A:
  cateto_a=3.3, cateto_b=4.4, contexto="rampa"
  → Hash: "a7f5c2d8b1e4..."

Versión B:
  cateto_a=6.9, cateto_b=9.2, contexto="escalera"
  → Hash: "b3e8d1f7a2c5..."
```

Si dos versiones tienen el mismo hash → son idénticas (duplicadas).

---

### 📌 BLOQUE 7: Generación de Gráfico TikZ (líneas 628-680)

```r
codigo_tikz_triangulo <- paste0("
\\begin{tikzpicture}[scale=1.5]
  % Vértices del triángulo
  \\coordinate (A) at (0,0);
  \\coordinate (B) at (", datos$cateto_a / 10, ",0);
  \\coordinate (C) at (0,", datos$cateto_b / 10, ");

  % Dibujar triángulo con relleno
  \\draw[black, very thick, fill=lightgray!20] 
    (A) -- (B) -- (C) -- cycle;

  % Símbolo de ángulo recto
  \\draw[black, thick] (0.15,0) -- (0.15,0.15) -- (0,0.15);

  % Etiquetas de los lados
  \\node[font=\\large\\bfseries] at (...) {", 
    formato_estandar(datos$cateto_a, 1), 
  "};
  ...
\\end{tikzpicture}
")
```

**¿Qué hace este bloque?**
Genera un diagrama vectorial del triángulo usando TikZ (lenguaje de gráficos LaTeX).

**Para novatos:**

**Estructura del código TikZ:**

```
     C (cateto_b)
     |╲
     | ╲  (hipotenusa_c)
     |  ╲
     |___╲
     A    B (cateto_a)
```

**Elementos del diagrama:**

| Comando TikZ | ¿Qué dibuja? |
|--------------|-------------|
| `\coordinate (A) at (0,0)` | Vértice A en origen |
| `\draw ... (A) -- (B) -- (C) -- cycle` | Triángulo conectando vértices |
| `fill=lightgray!20` | Relleno gris claro |
| `\draw (0.15,0) -- (0.15,0.15) -- (0,0.15)` | Cuadradito de ángulo recto |
| `\node[...] at (...) {4.5}` | Etiqueta numérica |

**Función multi-formato:**

```r
generar_triangulo_multi_formato <- function(codigo_tikz, nombre) {
  if (typ == "pdf") {
    # Para PDF: formato vectorial
    include_tikz(codigo_tikz, format = "pdf", ...)
  } else {
    # Para HTML/Moodle: formato PNG
    include_tikz(codigo_tikz, format = "png", ...)
  }
}
```

**Para novatos:**

El gráfico se adapta al formato de salida:

- **PDF:** Vectorial (calidad infinita, escalable)
- **HTML/Moodle:** PNG (compatible con navegadores)

---

### 📌 BLOQUE 8: Sección Question (líneas 682-778)

```markdown
Question
========

## Entrenamiento Completo: `r datos$termino_teorema`

**Contexto:** En el ámbito de `r datos$contexto`, ...

[Imagen del triángulo]

### **FASE I: Comprensión Conceptual**

**Paso 1.** `r datos$pasos$paso1$pregunta`
##ANSWER1##

**Paso 2.** `r datos$pasos$paso2$pregunta`
##ANSWER2##

...

Answerlist
----------
* `r datos$pasos$paso1$opciones[1]`
* `r datos$pasos$paso1$opciones[2]`
...
```

**¿Qué hace este bloque?**
Define el contenido que verá el estudiante.

**Para novatos:**

**Elementos dinámicos:**

| Código | Se reemplaza por... | Ejemplo |
|--------|---------------------|---------|
| `` `r datos$termino_teorema` `` | Término aleatorio | "Teorema de Pitágoras" |
| `` `r datos$contexto` `` | Contexto aleatorio | "construcción de rampa" |
| `` `r datos$pasos$paso1$pregunta` `` | Pregunta del paso 1 | "¿Cuáles son los elementos...?" |
| `##ANSWER1##` | Campo de respuesta 1 | Caja de selección o campo numérico |

**Renderizado para el estudiante:**

```
Paso 1. ¿Cuáles son los elementos de un triángulo rectángulo?
○ tres catetos
○ dos catetos y una hipotenusa  ← [SELECCIONAR]
○ una hipotenusa y dos ángulos
○ dos hipotenusas y un cateto

Paso 4. Calcula 4.5²:
[_______] ← [ESCRIBIR NÚMERO]
```

---

### 📌 BLOQUE 9: Sección Solution (líneas 780-859)

```markdown
Solution
========

## Solución Completa

### **FASE I: Comprensión Conceptual**

**Paso 1:** Los elementos son **dos catetos y una hipotenusa**. 
Los catetos forman el ángulo recto...

**Paso 2:** Las afirmaciones correctas son:
- Sí: Se aplica solo a triángulos rectángulos
- Sí: La hipotenusa es el lado más largo
...

### **FASE II: Aplicación Numérica**

**Paso 4:** 4.5² = **20.25**

**Paso 5:** 6.0² = **36.00**

...
```

**¿Qué hace este bloque?**
Proporciona explicaciones detalladas de las respuestas.

**Para novatos:**

**Características de la solución:**

1. **Respuestas con explicaciones:**
   No solo dice "la respuesta es X", explica POR QUÉ.

2. **Valores específicos:**
   Usa los valores generados aleatoriamente para esta versión.

3. **Cálculos paso a paso:**
   Muestra el proceso completo:
   ```
   √(7.5² - 4.5²) = √(56.25 - 20.25) = √36 = 6.0
   ```

4. **Chunk dinámico:**
   ```r
   for(i in 1:12) {
     paso <- datos$pasos[[i]]
     cat(paste0("**Paso ", i, ":** "))
     
     if(paso$tipo == "schoice") {
       cat(paste("Respuesta:", paso$opciones[paso$correcta]))
     }
     ...
   }
   ```
   Genera resumen automático de todas las respuestas.

---

### 📌 BLOQUE 10: Meta-información (líneas 861-877)

```markdown
Meta-information
================
exname: teorema_pitagoras_entrenamiento_completo...
extype: cloze
exsolution: `r solucion_final`
exclozetype: `r tipos_final`
extol: `r tolerancias_final`
exshuffle: TRUE
exsection: Geometria|Teorema de Pitagoras
...
```

**¿Qué hace este bloque?**
Define metadatos técnicos (no visibles para el estudiante).

**Para novatos:**

| Campo | Descripción | Valor |
|-------|-------------|-------|
| `exname` | Nombre del ejercicio | `teorema_pitagoras_...` |
| `extype` | Tipo de pregunta | `cloze` (compuesta) |
| `exsolution` | Respuestas correctas | `"0100\|1011\|...\|15.0"` |
| `exclozetype` | Tipos de sub-preguntas | `"schoice\|mchoice\|...\|num"` |
| `extol` | Tolerancias numéricas | `"0\|0\|...\|0.5"` |
| `exshuffle` | ¿Aleatorizar? | `TRUE` |
| `exsection` | Categoría | `"Geometria"` |
| `exlanguage` | Idioma | `es` (español) |

**Importante:**

- Los 3 campos principales deben tener **12 elementos** (uno por paso)
- Separados por `|` (pipe)
- Orden debe coincidir

---

## 🎓 Guía de Resolución {#resolución}

### 📝 Ejemplo Práctico Completo

Supongamos que el ejercicio generó:

- **cateto_a = 4.5**
- **cateto_b = 6.0**
- **hipotenusa_c = 7.5**

---

### ✅ FASE I: Comprensión Conceptual

**Paso 1:** ¿Elementos del triángulo rectángulo?

- ○ tres catetos
- **● dos catetos y una hipotenusa** ✅
- ○ una hipotenusa y dos ángulos
- ○ dos lados iguales

**Razonamiento:** Siempre tiene 2 catetos + 1 hipotenusa.

---

**Paso 2:** ¿Afirmaciones correctas sobre el teorema?

- **☑ Se aplica solo a triángulos rectángulos** ✅
- ☐ Se aplica a cualquier triángulo
- **☑ La hipotenusa es el lado más largo** ✅
- **☑ Se puede escribir como a² + b² = c²** ✅

**Razonamiento:** Las 3 primeras son verdaderas.

---

**Paso 3:** ¿Cuál es la hipotenusa en 4.5, 6.0, 7.5?

- ○ 4.5
- ○ 6.0
- **● 7.5** ✅
- ○ 10.5

**Razonamiento:** Es el lado más largo.

---

### ✅ FASE II: Aplicación Numérica

**Paso 4:** Calcula 4.5²

- **Respuesta:** `20.25`
- **Cálculo:** 4.5 × 4.5 = 20.25

---

**Paso 5:** Calcula 6.0²

- **Respuesta:** `36.00`
- **Cálculo:** 6.0 × 6.0 = 36.00

---

**Paso 6:** Calcula 4.5² + 6.0²

- **Respuesta:** `56.25`
- **Cálculo:** 20.25 + 36.00 = 56.25

---

### ✅ FASE III: Verificación

**Paso 7:** ¿Es 4.5² + 6.0² = 7.5²?

- **● Sí** ✅
- ○ No

**Verificación:** 56.25 = 56.25 → Correcto

---

**Paso 8:** Si cateto=4.5 e hipotenusa=7.5, ¿otro cateto?

- **Respuesta:** `6.0`
- **Fórmula:** b = √(c² - a²)
- **Cálculo:** √(7.5² - 4.5²) = √(56.25 - 20.25) = √36 = 6.0

---

**Paso 9:** ¿Qué operación usar para verificar?

- ○ 7.5 + 4.5
- **● √(7.5² - 4.5²)** ✅
- ○ 7.5 × 4.5
- ○ √(7.5² + 4.5²)

**Razonamiento:** Fórmula para encontrar cateto.

---

### ✅ FASE IV: Transferencia

**Paso 10:** ¿Aplicaciones válidas del teorema?

- **☑ Calcular distancias directas** ✅
- **☑ Determinar ángulos rectos** ✅
- ☐ Calcular áreas de círculos
- **☑ Verificar medidas** ✅

---

**Paso 11:** Error al sumar 4.5 + 6.0 = 10.5

- **● Sumó los catetos en lugar de usar el teorema** ✅

**Razonamiento:** Debió calcular √(4.5² + 6.0²) = 7.5

---

**Paso 12:** Si catetos son 18.0 y 24.0, ¿hipotenusa?

- **Respuesta:** `30.0`
- **Cálculo:** √(18² + 24²) = √(324 + 576) = √900 = 30

---

## 📚 Glosario {#glosario}

### Términos de Programación

| Término | Definición |
|---------|------------|
| **chunk** | Bloque de código R entre \`\`\`{r} y \`\`\` |
| **echo=FALSE** | Oculta código, muestra resultados |
| **results="hide"** | Oculta resultados |
| **set.seed()** | Inicializa números aleatorios |
| **sample()** | Selecciona elementos al azar |
| **runif()** | Genera decimal aleatorio |
| **round()** | Redondea número |
| **paste0()** | Concatena textos |
| **stopifnot()** | Detiene si condición es falsa |

### Términos R-exams

| Término | Significado |
|---------|-------------|
| **extype: cloze** | Pregunta compuesta |
| **schoice** | Selección única |
| **mchoice** | Selección múltiple |
| **num** | Respuesta numérica |
| **extol** | Tolerancia numérica |
| **exshuffle** | Aleatorizar opciones |

### Términos Matemáticos

| Término | Definición |
|---------|------------|
| **Terna pitagórica** | Tres números: a² + b² = c² |
| **Cateto** | Lado del ángulo recto |
| **Hipotenusa** | Lado opuesto al ángulo recto |

---

## 🎯 Consejos Finales

### Para Estudiantes

✅ Lee cada pregunta cuidadosamente\
✅ Identifica la hipotenusa (lado más largo)\
✅ Usa calculadora para cuadrados y raíces\
✅ Verifica tus cálculos

### Para Profesores

✅ El ejercicio genera 300+ versiones únicas\
✅ Evalúa comprensión en 4 niveles progresivos\
✅ Compatible con Moodle, Canvas, Blackboard\
✅ Soluciones detalladas automáticas

---

**📧 Contacto:** alvaroangelm@iepedacitodecielo.edu.co\
**🏫 Institución:** I. E. Pedacito de Cielo, La Tebaida, Quindío, Colombia\
**📅 Versión:** 2025
