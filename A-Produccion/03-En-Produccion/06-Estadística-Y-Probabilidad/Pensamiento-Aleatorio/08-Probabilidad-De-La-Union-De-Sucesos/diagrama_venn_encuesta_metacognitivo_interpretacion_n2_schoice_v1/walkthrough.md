# Walkthrough: diagrama_venn_encuesta_metacognitivo_interpretacion_n2_schoice_v1

> Tutorial generado automáticamente. Describe la estructura interna del ejercicio,
> las decisiones de diseño y una receta para crear ejercicios similares.
> No es necesario leer el .Rmd para seguir este documento.

---

## Mapa del ejercicio

| Atributo | Valor |
|---|---|
| Tipo | SCHOICE (selección única, 4 opciones gráficas) |
| Tema | Operaciones entre conjuntos — diagramas de Venn de 3 conjuntos |
| Patrón metacognitivo | Identificación de región sombreada correcta |
| Nivel ICFES | 2 |
| Competencia | Interpretación |
| Componente | Aleatorio |
| DOK | 2 (Skill/Concept) |
| Bloom | Analizar |
| SOLO | Relacional |
| Gráficos | Sí — TikZ compilado a PNG vía pdflatex + magick |
| Versiones únicas | 250+ (contexto × datos × tipo de región × orden opciones) |
| Líneas totales | 1 164 |

---

## 1. Vista de pájaro

### La idea en una frase

El ejercicio le muestra a un estudiante cuatro diagramas de Venn y le pregunta cuál sombreado representa correctamente una operación de conjuntos específica (por ejemplo, "los elementos que están solo en A, sin contar los de B ni los de C").

### Analogía del mundo real

Imagina un mapa de calles con tres barrios que se superponen. La pregunta sería: "¿Cuál de estos mapas pinta correctamente la zona que está en el Barrio A pero que NO toca ni el Barrio B ni el Barrio C?". Cada mapa (opción) pinta una zona diferente — uno correcto y tres con errores típicos de quien confunde las regiones del diagrama.

### Por qué es metacognitivo

No se trata de calcular un número, sino de **identificar qué zona del diagrama corresponde a una expresión matemática**. El estudiante debe detectar cuál de los cuatro diagramas tiene el sombreado correcto y, de paso, reconocer qué error cometió quien dibujó cada diagrama incorrecto. Esto activa análisis y evaluación, no solo memorización.

### ¿Qué varía en cada renderizado?

Cada vez que R-exams genera una versión del ejercicio:

1. Se elige uno de 8 **contextos narrativos** (jugos, deportes, instrumentos, mascotas, etc.)
2. Se generan **7 valores numéricos** aleatorios para las regiones del diagrama
3. Se elige uno de **8 tipos de región** (solo A, intersección A∩B sin triple, triple A∩B∩C, complemento, etc.)
4. Se **mezcla el orden** de las 4 opciones (A/B/C/D puede ser cualquier diagrama)
5. Se elige una **reflexión metacognitiva** de un pool de 5

---

## 2. Anatomía del .Rmd

```
diagrama_venn_encuesta_metacognitivo_interpretacion_n2_schoice_v1.Rmd
│
├── [L1-5]    YAML header (html_document + pdf_document)
│
├── [L6-1062] Chunk data_generation  ←── ¡TODO el trabajo ocurre aquí!
│   │
│   ├── [L13-59]   generar_tex()        — plantilla TikZ del diagrama
│   ├── [L66-160]  generar_sombreado()  — 12 tipos de sombreado TikZ
│   ├── [L162-184] compilar_tikz()      — tex → PNG (pdflatex + magick)
│   │
│   ├── [L191-321] 8 contextos narrativos (funciones plantilla)
│   ├── [L327-331] Selección aleatoria de contexto y protagonista
│   │
│   ├── [L333-419] Generación de 7 regiones + repeat-until (4 valores únicos)
│   │
│   ├── [L430-977] Bloque condicional por categoría
│   │   ├── "solo"        — región exclusiva de 1 conjunto
│   │   ├── "par"         — intersección de 2 sin el triple
│   │   ├── "triple"      — triple intersección
│   │   └── "complemento" — todo lo que NO está en un conjunto
│   │
│   ├── [L980-1003]  Mezcla interna (sample) + tracking letra_correcta
│   ├── [L1009-1020] Compilación TikZ → 5 PNGs (contexto + A/B/C/D)
│   ├── [L1026-1033] Pool de 5 reflexiones metacognitivas
│   ├── [L1039-1048] sol_texts para el answerlist de Solution
│   └── [L1054-1061] stopifnot() — verificaciones defensivas
│
├── [L1064-1085] Sección Question   — enunciado + diagrama + pregunta + 4 PNGs
├── [L1087-1146] Sección Solution   — análisis, procedimiento, respuesta, estrategia
└── [L1148-1165] Meta-information   — extype, exsolution, 6 dimensiones ICFES
```

---

## 3. Bloque por bloque

### 3.1 Funciones auxiliares TikZ

El ejercicio necesita generar imágenes de diagramas de Venn en tiempo de renderizado. Para esto hay tres funciones especializadas.

**`generar_tex()`** — construye el código LaTeX/TikZ completo de un diagrama.

Recibe los parámetros del diagrama (etiquetas de conjuntos, cantidades por región) y devuelve un string con el código TikZ listo para compilar. Es como una plantilla de documento Word donde hay celdas que se rellenan con variables de R.

```r
# Fragmento representativo de generar_tex()
generar_tex <- function(etiqueta_a, etiqueta_b, etiqueta_c,
                         solo1, solo2, solo3,
                         int12, int13, int23, triple,
                         outside, titulo, sombreado_code) {
  paste0(
    "\\documentclass[border=5pt]{standalone}\n",
    "\\usepackage{tikz}\n",
    "\\begin{document}\n",
    "\\begin{tikzpicture}\n",
    sombreado_code,         # <-- aquí se inyecta el sombreado
    # ... coordenadas de los 3 círculos, etiquetas, números ...
    "\\end{tikzpicture}\n",
    "\\end{document}\n"
  )
}
```

**`generar_sombreado()`** — devuelve el fragmento TikZ que pinta la región correcta o incorrecta.

Recibe un `tipo` (cadena de texto como `"clip_fill_minus"`) y devuelve solo el código de sombreado, que luego se inyecta dentro de `generar_tex()`. Hay 12 tipos posibles, uno por cada sombreado diferente que aparece en las opciones.

```r
# Estructura general de generar_sombreado()
generar_sombreado <- function(tipo) {
  switch(tipo,
    "clip_fill_minus" = {
      # Solo la región de A que NO toca B ni C
      # Técnica TikZ: clip A, luego "restar" B y C con even-odd rule
      paste0("\\begin{scope}\\clip (circA); ...", " \\fill[gray!40] (circA);\\end{scope}")
    },
    "fill_two" = {
      # Intersección de A y B incluyendo el triple
      paste0("\\begin{scope}\\clip (circA);\\clip (circB); \\fill[gray!40] ...; \\end{scope}")
    },
    # ... 10 tipos más ...
  )
}
```

**¿Por qué esta separación en dos funciones?**

Separa la "estructura del diagrama" (siempre igual) del "qué se pinta" (varía por opción). Si quisieras añadir un nuevo tipo de sombreado, solo tocas `generar_sombreado()` sin tocar la estructura.

**`compilar_tikz()`** — convierte el código TikZ en un archivo PNG.

```r
# Flujo completo dentro de compilar_tikz()
compilar_tikz <- function(codigo_tex, nombre_png) {
  # 1. Escribir el .tex a disco
  writeLines(codigo_tex, con = paste0(nombre_base, ".tex"))

  # 2. Compilar con pdflatex (produce un PDF)
  system2("pdflatex", args = c("-interaction=nonstopmode", paste0(nombre_base, ".tex")))

  # 3. Convertir PDF a PNG con magick
  system2("magick", args = c(paste0(nombre_base, ".pdf"), "-density 150", nombre_png))

  # 4. Limpiar archivos temporales (.tex, .pdf, .aux, .log)
  # IMPORTANTE: R-exams incluye TODOS los archivos del directorio.
  # Si no se borran los .tex/.aux, aparecerán en el output final.
  file.remove(paste0(nombre_base, c(".tex", ".pdf", ".aux", ".log")))
}
```

**¿Qué pasaría si no se limpian los archivos?**

R-exams recoge todos los archivos del directorio del ejercicio y los incluye en el HTML/PDF generado. Si quedan los `.tex` y `.aux`, el estudiante podría ver archivos extraños adjuntos o el HTML podría romperse.

---

### 3.2 Contextos narrativos creativos

El ejercicio define 8 contextos temáticos (jugos, deportes, instrumentos, mascotas, colores, comida, pasatiempos, música). Cada contexto tiene:

- Una **plantilla narrativa** (función que recibe protagonista y produce el enunciado)
- Una lista de **protagonistas** específicos del contexto
- **Etiquetas** para los 3 conjuntos (por ejemplo: "Manzana", "Naranja", "Uva")

```r
# Ejemplo del contexto "jugos"
list(
  nombre_ctx = "jugos",
  plantilla = function(prot, sets) {
    paste0(
      "En una encuesta escolar, ", prot,
      " preguntó a un grupo de estudiantes qué jugos preferían: ",
      sets[1], ", ", sets[2], " o ", sets[3], "."
    )
  },
  protagonistas = c(
    "la profesora de nutrición",
    "el comité de bienestar",
    "una estudiante de décimo grado"
  ),
  sets = list(
    c("Manzana", "Naranja", "Uva"),
    c("Mango", "Maracuyá", "Guayaba"),
    c("Mora", "Lulo", "Tomate de árbol")
  )
)
```

**¿Por qué 8 contextos y no 1?**

Con un solo contexto, todos los estudiantes de un salón recibirán el mismo enunciado y podrán copiar. Con 8 contextos × 3 protagonistas × permutaciones de sets, la probabilidad de que dos estudiantes reciban el mismo enunciado es muy baja.

**¿Por qué la plantilla es una función?**

Permite que el texto sea gramaticalmente correcto en todos los casos. Un simple `paste0()` fijo no puede adaptarse a si el protagonista es singular/plural o masculino/femenino. La función tiene esa lógica embebida.

---

### 3.3 Generación de datos aleatorios

El ejercicio genera 7 valores numéricos que representan las regiones del diagrama de Venn:

| Variable | Región del diagrama | Rango |
|---|---|---|
| `solo1`, `solo2`, `solo3` | Solo en A / solo en B / solo en C | 3–12 |
| `int12`, `int13`, `int23` | Intersección de 2 (sin el triple) | 5–18 |
| `triple` | Triple intersección A∩B∩C | 2–8 |
| `outside` | Fuera de los 3 conjuntos | 1–5 |

El `n_total` es la suma de todo y debe quedar entre 50 y 120 (rango realista para una encuesta escolar).

El mecanismo usa un **bucle `repeat-until`** para garantizar que las 4 opciones numéricas sean distintas entre sí:

```r
repeat {
  # Generar los 7 valores
  solo1 <- sample(3:12, 1)
  # ... más valores ...

  # Calcular el valor de las 4 opciones
  # (cada opción es una región o combinación de regiones)
  valor_correcta   <- <cálculo según tipo de región>
  valor_distractor1 <- <cálculo del error CNJ-VEN-XX>
  valor_distractor2 <- <cálculo del error CNJ-VEN-YY>
  valor_distractor3 <- <cálculo del error CNJ-VEN-ZZ>

  # Solo salir si las 4 opciones son numéricamente distintas
  if (length(unique(c(valor_correcta,
                       valor_distractor1,
                       valor_distractor2,
                       valor_distractor3))) == 4) break
}
```

**¿Por qué se necesita el bucle?**

Los valores se calculan aritméticamente (sumas de regiones). Dependiendo de los números aleatorios, dos errores distintos podrían producir el mismo valor numérico. Si eso pasa, el diagrama correcto y un distractor tendrían el mismo sombreado numéricamente — el estudiante no podría distinguirlos. El bucle regenera los datos hasta que los 4 valores sean diferentes.

**¿Qué pasaría si los rangos fueran muy pequeños?**

Si `solo1` solo pudiera ser 0 o 1, habría pocas combinaciones posibles y el bucle tardaría mucho o podría volverse infinito. Los rangos actuales (3–12 para solos, 5–18 para intersecciones) dan suficiente variabilidad para que la condición de unicidad se cumpla rápido, típicamente en 1–3 intentos.

---

### 3.4 Bloque condicional por categoría

El tipo de región elegido determina qué pregunta se hace y qué errores tienen sentido. El ejercicio tiene 8 tipos de región que se agrupan en 4 categorías:

| Tipo numérico | Categoría | Ejemplo de región |
|---|---|---|
| 1, 2 | `solo` | Solo en A (excluye B y C) |
| 3, 4 | `par` | A∩B sin contar A∩B∩C |
| 5 | `triple` | A∩B∩C |
| 6, 7, 8 | `complemento` | Todo lo que NO está en A |

Cada categoría tiene su propio bloque de código que define:

1. `pregunta_texto`: El enunciado de la pregunta para el estudiante
2. `notacion_latex`: La expresión matemática (ej: `A \setminus (B \cup C)`)
3. `errores_conceptuales`: Lista de 3 errores típicos para esa categoría
4. `opciones_config`: Los 4 tipos de sombreado (correcto + 3 distractores)
5. Textos de solución: análisis del error, procedimiento correcto, estrategia

**Ejemplo simplificado del bloque `solo`:**

```r
if (categoria == "solo") {
  pregunta_texto <- paste0(
    "¿Cuál diagrama representa los estudiantes que marcaron solo ",
    sets[conjunto_sel], " y no marcaron ningún otro?"
  )
  notacion_latex <- paste0(
    sets[conjunto_sel], " \\setminus (", sets[otro1], " \\cup ", sets[otro2], ")"
  )

  errores_conceptuales <- list(
    list(
      codigo = "CNJ-VEN-04",
      nombre = "Exclusión parcial (olvidó excluir un conjunto)",
      descripcion_corta = paste0(
        "Excluyó ", sets[otro1], " pero no excluyó ", sets[otro2]
      ),
      descripcion_larga = "...",
      causa_raiz = "Aplicó solo una de las dos exclusiones requeridas",
      calcula = function(datos_ord, datos_presentados = NULL) {
        # Calcula el valor que produciría este error
        solo_sel + int_con_otro2  # incluye la intersección que no debió incluir
      }
    ),
    # ... CNJ-VEN-05, CNJ-VEN-06 ...
  )

  opciones_config <- list(
    correcta    = list(tipo = "clip_fill_minus", label = "Correcto"),
    distractor1 = list(tipo = "clip_fill_minus_one", label = "Error 04"),
    distractor2 = list(tipo = "clip_fill_minus_two", label = "Error 05"),
    distractor3 = list(tipo = "fill_one",             label = "Error 06")
  )
}
```

**¿Por qué 4 categorías y no 8?**

Las 8 regiones se agrupan en categorías porque comparten la misma **lógica de error**. Los errores para "solo en A" son los mismos que para "solo en B" — lo que cambia es qué letra aparece en la pregunta. Esto reduce la cantidad de código sin perder variedad.

---

### 3.5 Pool de errores conceptuales (ejemplo anotado)

Aquí se muestra un error de la categoría `complemento` completamente anotado:

```r
list(
  # Código único — permite rastrear el error en análisis pedagógicos
  codigo = "CNJ-VEN-11",

  # Nombre corto para referencia interna
  nombre = "Sombreó la unión en lugar del complemento",

  # Lo que aparece en las opciones de la Solution (estudiante lo lee)
  descripcion_corta = paste0(
    "Sombreó la unión de los tres conjuntos en lugar del complemento de ",
    sets[conjunto_sel]
  ),

  # Explicación completa para la sección de análisis de error
  descripcion_larga = paste0(
    "En lugar de sombrear todo lo que está FUERA de ", sets[conjunto_sel],
    ", sombreó todo lo que está DENTRO de los tres conjuntos.",
    " Este error confunde el complemento (lo exterior) con la unión (lo interior)."
  ),

  # Diagnóstico pedagógico — útil para el docente
  causa_raiz = paste0(
    "Invirtió la lógica del complemento: pintó lo que hay que excluir",
    " en vez de lo que queda."
  ),

  # Función pura — determinista, sin sample() ni runif()
  # datos_ord: los valores de las regiones, ordenados
  # Devuelve el valor numérico que obtendría el estudiante con este error
  calcula = function(datos_ord, datos_presentados = NULL) {
    # La unión de los tres conjuntos = todo menos "outside"
    n_total - outside
  }
)
```

**Principio de determinismo en `calcula()`**

La función `calcula()` recibe los datos y devuelve siempre el mismo número para los mismos datos. Está **prohibido** usar `sample()`, `runif()` o cualquier función aleatoria dentro de `calcula()`. Si se usara aleatoriedad aquí, el distractor generado no correspondería a los datos que el estudiante ve en su diagrama — el ejercicio sería incorrecto para algunas semillas.

---

### 3.6 Mezcla interna + tracking de letra_correcta

Este es uno de los bloques más delicados del ejercicio:

```r
# 1. Construir la lista de opciones con sus configuraciones de sombreado
todas_opciones <- list(
  correcta    = opciones_config$correcta,
  distractor1 = opciones_config$distractor1,
  distractor2 = opciones_config$distractor2,
  distractor3 = opciones_config$distractor3
)

# 2. Mezclar el orden aleatoriamente (diferente en cada semilla)
opciones_mezcladas <- sample(todas_opciones)

# 3. Identificar en qué posición quedó la opción correcta
indice_correcto <- which(names(opciones_mezcladas) == "correcta")

# 4. Construir el vector de solución binario (1 = correcto, 0 = incorrecto)
solucion <- rep(0, 4)
solucion[indice_correcto] <- 1

# 5. Asignar letras a las posiciones mezcladas
letras <- c("A", "B", "C", "D")
names(opciones_mezcladas) <- letras

# 6. Saber qué letra corresponde a la opción correcta
# (la Solution necesita decir "La respuesta correcta es la Opción X")
letra_correcta <- letras[indice_correcto]
```

**¿Por qué `exshuffle: FALSE` y no `exshuffle: TRUE`?**

Esta es la decisión de diseño más importante. Con `exshuffle: TRUE`, R-exams re-mezclaría el orden de las opciones, pero **no modificaría el texto de la Solution**. La Solution dice `"La respuesta correcta es la Opción **`r letra_correcta`**"`. Si R-exams mueve esa opción a otra posición, la letra que aparece en la Solution ya no coincidiría con la posición correcta. El resultado sería un ejercicio con la respuesta marcada incorrectamente.

La solución: `sample()` interno hace la aleatorización antes de que R-exams intervenga. `exshuffle: FALSE` le dice a R-exams "no toques el orden". Cada renderizado con diferente semilla produce un orden diferente de A/B/C/D gracias al `sample()`.

---

### 3.7 Compilación TikZ → PNG

Después de mezclar las opciones, se generan 5 imágenes PNG:

```r
# diagrama de contexto (muestra la encuesta con los números reales)
diagrama_contexto_code <- generar_tex(
  etiqueta_a = sets[1], etiqueta_b = sets[2], etiqueta_c = sets[3],
  solo1 = solo1, solo2 = solo2, solo3 = solo3,
  int12 = int12, int13 = int13, int23 = int23,
  triple = triple, outside = outside,
  titulo = "Diagrama de la encuesta",
  sombreado_code = ""   # sin sombreado — solo muestra los datos
)
compilar_tikz(diagrama_contexto_code, "diagrama_contexto.png")

# Opciones A, B, C, D (cada una con su tipo de sombreado)
for (letra in c("A", "B", "C", "D")) {
  tipo_somb <- opciones_mezcladas[[letra]]$tipo
  somb_code <- generar_sombreado(tipo_somb)
  diagrama_code <- generar_tex(
    # mismos datos numéricos, diferente sombreado
    ...,
    sombreado_code = somb_code
  )
  compilar_tikz(diagrama_code, paste0("diagrama_", tolower(letra), ".png"))
}
```

Los 5 archivos generados son: `diagrama_contexto.png`, `diagrama_a.png`, `diagrama_b.png`, `diagrama_c.png`, `diagrama_d.png`.

---

### 3.8 La pregunta (sección Question)

```markdown
**Contexto**: `r enunciado_contexto`

El diagrama siguiente muestra los resultados de la encuesta:

```{r mostrar_contexto, echo=FALSE}
knitr::include_graphics("diagrama_contexto.png")
```

`r pregunta_texto`

¿Cuál de los siguientes diagramas representa correctamente
la región $`r notacion_latex`$?

Answerlist
----------
* ![](diagrama_a.png)
* ![](diagrama_b.png)
* ![](diagrama_c.png)
* ![](diagrama_d.png)
```

El enunciado tiene tres partes: el contexto narrativo, el diagrama de referencia (sin sombreado, con los números reales) y la pregunta con notación matemática.

---

### 3.9 La solución (sección Solution)

La solución sigue las 6 subsecciones obligatorias del patrón metacognitivo:

```markdown
### Análisis del error

**Error a evitar:** `r error_seleccionado$descripcion_larga`
**Código:** `r error_seleccionado$codigo`
**Causa raíz:** `r error_seleccionado$causa_raiz`

### Procedimiento correcto

Para identificar la región $`r notacion_latex`$:
`r sol_procedimiento`

### Respuesta correcta: Opción `r letra_correcta`

```{r mostrar_correcto, echo=FALSE}
knitr::include_graphics(paste0("diagrama_", tolower(letra_correcta), ".png"))
```

### Errores comunes en las otras opciones

`r sol_analisis`  ← explica qué error tiene cada opción incorrecta

### Propiedades de los conjuntos relevantes

`r sol_resultado`  ← propiedades matemáticas del tipo de región

### Reflexión metacognitiva

`r reflexion`  ← 1 de 5 reflexiones del pool

### Estrategia para evitar el error

`r sol_estrategia`  ← pasos preventivos
```

**¿Por qué se muestra el diagrama correcto en la Solution?**

Porque el orden A/B/C/D varía en cada renderizado. La Solution no puede decir "vea el diagrama B" de forma hardcodeada. En cambio, usa `letra_correcta` (que sí conoce cuál es el correcto) para mostrar el PNG específico. El estudiante puede ver el sombreado correcto claramente.

---

### 3.10 Metadatos (Meta-information)

```yaml
Meta-information
================
exname: diagrama_venn_encuesta_metacognitivo_interpretacion_n2_schoice_v1
extype: schoice
exsolution: `r paste(as.integer(solucion), collapse="")`
exshuffle: FALSE
extol: 0.01

exextra[Type]: SCHOICE
exextra[Competencia]: Interpretacion
exextra[Componente]: Aleatorio
exextra[Afirmacion]: Interpreta diagramas de Venn con tres conjuntos
exextra[Evidencia]: Identifica la región sombreada que corresponde a una operación de conjuntos
exextra[Nivel]: 2
exextra[DOK]: 2
exextra[Bloom]: Analizar
exextra[SOLO]: Relacional
exextra[TipoMetacognicion]: identificacion_region_diagrama
```

**Nota sobre `exsolution`**: El valor es dinámico — se genera con `paste(as.integer(solucion), collapse="")`. Para 4 opciones donde la correcta es la tercera, esto produce `"0010"`. R-exams interpreta esta cadena como "la tercera opción es correcta".

---

## 4. Patrones clave (decisiones no obvias)

### Patrón 1: Abstracción del sombreado TikZ en una función

El sombreado de regiones en TikZ requiere técnicas complejas de recorte y relleno. En vez de tener ese código TikZ disperso por todo el archivo, existe `generar_sombreado(tipo)`. Esto tiene tres ventajas:

1. Si necesitas añadir un nuevo tipo de sombreado, solo añades un `case` al `switch`
2. Los `opciones_config` usan nombres semánticos ("clip_fill_minus") en lugar de código crudo
3. Se puede reutilizar el mismo sombreado en diferentes contextos sin copiar código

### Patrón 2: Separación total entre datos y presentación

Los mismos 7 valores numéricos se usan en los 5 diagramas (contexto + 4 opciones). Lo único que cambia entre diagramas es el sombreado. Esto garantiza que el estudiante compare opciones con los mismos datos — no puede detectar la correcta por diferencias numéricas.

### Patrón 3: Bloque condicional masivo como "4 ejercicios en uno"

El bloque `if/else if` para `categoria` es largo (~550 líneas), pero refleja la realidad del problema: las 4 categorías de región son conceptualmente diferentes y requieren preguntas, errores y soluciones diferentes. La alternativa — tener 4 archivos .Rmd separados — sería más difícil de mantener. Si se necesita cambiar la forma de compilar TikZ, se cambia en un solo lugar.

### Patrón 4: stopifnot() como red de seguridad

Al final del chunk `data_generation`:

```r
stopifnot(
  file.exists("diagrama_a.png"),
  file.exists("diagrama_b.png"),
  file.exists("diagrama_c.png"),
  file.exists("diagrama_d.png"),
  file.exists("diagrama_contexto.png"),
  length(unique(c(val_correcta, val_dist1, val_dist2, val_dist3))) == 4
)
```

Si pdflatex falla silenciosamente y no genera los PNGs, el ejercicio habría producido HTML con imágenes rotas sin ningún aviso. Con `stopifnot()`, el renderizado falla explícitamente con un mensaje claro. Es preferible un error visible a un ejercicio silenciosamente defectuoso.

---

## 5. Cómo crear uno similar (receta paso a paso)

Esta receta es para crear un ejercicio SCHOICE metacognitivo con opciones gráficas (PNGs generados en tiempo de renderizado).

### Paso 1: Definir el concepto central

Identifica qué operación o representación visual el estudiante debe interpretar. Para este ejercicio: "regiones de un diagrama de Venn de 3 conjuntos". Define cuántos tipos de región tiene sentido preguntar (aquí: 8, agrupados en 4 categorías).

### Paso 2: Diseñar la función de generación de gráficos

Antes de escribir el ejercicio, diseña y prueba la función que genera los gráficos. Para ejercicios con TikZ:

1. Escribe el código TikZ a mano para 1 caso concreto
2. Identifica qué partes son constantes y cuáles varían
3. Parametriza las partes que varían en una función `generar_tex()`
4. Crea `generar_sombreado(tipo)` para los N tipos de gráfico diferente
5. Crea `compilar_tikz()` que llame pdflatex y magick, y limpie archivos

Prueba que `compilar_tikz()` genera un PNG correcto antes de integrarlo al .Rmd.

### Paso 3: Diseñar los errores conceptuales

Para cada categoría de región, identifica los 3 errores más frecuentes que cometen los estudiantes. Cada error debe:

- Tener un código único (prefijo del tema + número)
- Producir un valor numéricamente diferente al correcto
- Tener una `calcula()` que sea función pura (sin aleatoriedad)
- Tener descripción corta, larga y causa raíz

### Paso 4: Crear el pool de contextos narrativos

Define al menos 6 contextos diferentes con plantillas funcionales. Usa tipos variados: acción en curso, situación problema, narración institucional, perspectiva del estudiante. Cada contexto debe tener al menos 3 protagonistas alternativos.

### Paso 5: Implementar la generación de datos con repeat-until

```r
repeat {
  # Generar valores aleatorios
  val1 <- sample(rango_min:rango_max, 1)
  # ...

  # Calcular valores de las 4 opciones
  val_correcta <- <cálculo directo>
  val_d1 <- errores[[1]]$calcula(datos)
  val_d2 <- errores[[2]]$calcula(datos)
  val_d3 <- errores[[3]]$calcula(datos)

  # Condición de salida: 4 valores únicos
  if (length(unique(c(val_correcta, val_d1, val_d2, val_d3))) == 4) break
}
```

### Paso 6: Implementar la mezcla interna

```r
todas_opciones <- list(correcta = ..., distractor1 = ..., distractor2 = ..., distractor3 = ...)
opciones_mezcladas <- sample(todas_opciones)
indice_correcto <- which(names(opciones_mezcladas) == "correcta")
solucion <- rep(0, 4); solucion[indice_correcto] <- 1
names(opciones_mezcladas) <- c("A", "B", "C", "D")
letra_correcta <- c("A", "B", "C", "D")[indice_correcto]
```

### Paso 7: Compilar los PNGs

Genera los N+1 PNGs (1 diagrama de contexto + N opciones). Verifica con `file.exists()` que todos se generaron.

### Paso 8: Escribir Question y Solution

- Question: enunciado + gráfico de contexto + pregunta + answerlist con los N PNGs
- Solution: 6 subsecciones (análisis del error, procedimiento, respuesta con PNG correcto, errores en otras opciones, propiedades, reflexión)

### Paso 9: Configurar Meta-information

```yaml
extype: schoice
exsolution: `r paste(as.integer(solucion), collapse="")`
exshuffle: FALSE     # OBLIGATORIO para opciones gráficas con Solution que referencia letra
```

### Paso 10: Agregar stopifnot()

Al final del chunk `data_generation`, verifica que todos los archivos PNG existen y que las opciones son únicas.

---

## 6. Errores comunes y cómo evitarlos

### Error 1: Poner `exshuffle: TRUE` con opciones gráficas

**Síntoma**: La Solution dice "la Opción B es correcta" pero la opción correcta aparece en posición C en la pregunta.

**Causa**: R-exams re-mezcló las opciones pero no actualizó el texto de la Solution.

**Solución**: Usar `exshuffle: FALSE` + `sample()` interno. La aleatorización la hace `sample()`, no R-exams.

### Error 2: Funciones aleatorias dentro de `calcula()`

**Síntoma**: El ejercicio produce distractores correctos en algunas semillas y erróneos en otras. El validador multi-semilla reporta `ERR_SEM_D`.

**Causa**: Uso de `sample()`, `runif()` o similar dentro de `calcula()`.

**Solución**: `calcula()` debe ser función pura. Todos los valores aleatorios deben generarse **antes** de llamar a `calcula()` y pasarse como parámetros.

### Error 3: No limpiar archivos temporales después de compilar TikZ

**Síntoma**: Los HTMLs generados contienen archivos `.tex` o `.aux` adjuntos. O el directorio del ejercicio se llena de archivos basura.

**Causa**: `compilar_tikz()` no borra los intermedios.

**Solución**: Incluir `file.remove(paste0(nombre_base, c(".tex", ".pdf", ".aux", ".log")))` al final de `compilar_tikz()`.

### Error 4: Bucle repeat-until con rangos demasiado restrictivos

**Síntoma**: El renderizado tarda mucho o se cuelga. En casos extremos: timeout.

**Causa**: Los rangos de los valores aleatorios son tan pequeños que la condición de 4 valores únicos raramente se cumple.

**Solución**: Ampliar los rangos o revisar si la condición de unicidad es alcanzable. Si los distractores son muy similares matemáticamente, quizás los rangos de entrada no dan suficiente variabilidad.

### Error 5: `stopifnot()` ausente — errores silenciosos

**Síntoma**: El ejercicio renderiza sin error pero los diagramas no aparecen (imagen rota).

**Causa**: pdflatex falló (no está instalado, o hay un error de sintaxis TikZ) pero el código R continuó sin verificar.

**Solución**: Añadir `stopifnot(file.exists("diagrama_a.png"), ...)` después de compilar los PNGs.

### Error 6: Títulos con letras en los gráficos de opciones

**Síntoma**: La opción que aparece en posición (b) tiene el título "A" impreso dentro del gráfico.

**Causa**: El código `ggplot2` o TikZ tiene un título hardcodeado con la letra antes de la mezcla.

**Solución**: Los gráficos de opciones nunca deben tener título con letra. R-exams asigna las letras (a), (b), (c), (d) automáticamente. El título dentro del gráfico debe ser `NULL` o un título genérico sin letra.

---

## 7. Glosario rápido

| Término | Definición en este contexto |
|---|---|
| `data_generation` | El chunk R donde se hace todo el cálculo. R-exams lo ejecuta primero para preparar las variables que se usan en Question y Solution. |
| `exshuffle` | Parámetro de R-exams que controla si el framework re-mezcla las opciones. `FALSE` aquí porque la mezcla ya la hace `sample()`. |
| `exsolution` | Cadena binaria que indica cuál opción es correcta. `"0010"` = la tercera opción es correcta. Se genera dinámicamente con `paste(as.integer(solucion), collapse="")`. |
| `letter_correcta` | Variable R que guarda qué letra (A/B/C/D) quedó en la posición de la opción correcta después de la mezcla. La Solution la usa para mostrar el diagrama correcto. |
| `generar_sombreado(tipo)` | Función que devuelve el fragmento TikZ que pinta una región específica del diagrama de Venn. |
| `compilar_tikz()` | Función que convierte código TikZ (texto) en una imagen PNG usando pdflatex y magick. |
| `opciones_config` | Lista de 4 configuraciones (correcta + 3 distractores) que especifican qué tipo de sombreado TikZ tiene cada opción. |
| `errores_conceptuales` | Lista de 3 errores típicos para la categoría de región seleccionada. Cada error tiene código, descripción y función `calcula()`. |
| `repeat-until` | Bucle en R que se repite hasta que se cumple una condición. Aquí garantiza que las 4 opciones tengan valores numéricos distintos. |
| `categoria` | Una de las 4 agrupaciones de región: `"solo"`, `"par"`, `"triple"`, `"complemento"`. Determina qué bloque condicional se ejecuta. |
| `standalone` | Clase de documento LaTeX que produce un PDF sin márgenes, del tamaño exacto del contenido. Ideal para generar imágenes de gráficos. |
| DOK | Depth of Knowledge (Webb). Nivel 2 = requiere interpretar y aplicar conceptos, no solo recordar. |
| `stopifnot()` | Función R que lanza un error si alguna condición es falsa. Se usa como verificación defensiva al final de `data_generation`. |

---

*Generado con /generar-walkthrough — 2026-03-25*
