---
output:
  pdf_document:
    keep_tex: true
    extra_dependencies: ["graphicx", "float", "tikz", "pgfplots", "xcolor", "amsmath", "array"]
  word_document: default
  html_document: default
icfes:
  competencia: interpretacion_representacion
  nivel_dificultad: 2
  contenido:
    categoria: algebra_calculo
    tipo: generico
  contexto: matematico
  eje_axial: eje2
  componente: numerico_variacional
---




``` r
options(OutDec = ".")  # Asegurar punto decimal en este chunk

# Establecer semilla aleatoria
set.seed(sample(1:10000, 1))

# Función de generación de datos aleatorios
generar_datos <- function() {
  # Aleatorización de contextos geométricos
  contextos_geometricos <- list(
    list(punto_movil = "K", punto_fijo1 = "A", punto_fijo2 = "B", 
         termino_distancia = "distancia", termino_grafica = "gráfica"),
    list(punto_movil = "M", punto_fijo1 = "C", punto_fijo2 = "D", 
         termino_distancia = "longitud", termino_grafica = "representación"),
    list(punto_movil = "N", punto_fijo1 = "E", punto_fijo2 = "F", 
         termino_distancia = "medida", termino_grafica = "función"),
    list(punto_movil = "Q", punto_fijo1 = "G", punto_fijo2 = "H", 
         termino_distancia = "distancia", termino_grafica = "curva"),
    list(punto_movil = "R", punto_fijo1 = "I", punto_fijo2 = "J", 
         termino_distancia = "longitud", termino_grafica = "gráfica"),
    list(punto_movil = "S", punto_fijo1 = "L", punto_fijo2 = "M", 
         termino_distancia = "medida", termino_grafica = "representación")
  )
  contexto_seleccionado <- sample(contextos_geometricos, 1)[[1]]
  
  # Aleatorización de altura h
  h <- sample(c(3, 4, 5, 6, 7, 8), 1)
  
  # Aleatorización de colores para las gráficas
  colores_disponibles <- c("blue", "red", "green", "purple", "orange", "brown")
  colores_graficas <- sample(colores_disponibles, 4, replace = FALSE)
  
  # Rango de ángulos (evitar 0° y 90° para evitar problemas matemáticos)
  angulo_min <- sample(5:15, 1)
  angulo_max <- sample(75:85, 1)
  
  # Aleatorizar qué opción (A, B, C, D) será la correcta
  letras <- c("A", "B", "C", "D")
  letra_correcta <- sample(letras, 1)
  
  # Crear mapeo de funciones a letras
  funciones <- list(
    correcta = "cosecante",    # KP = h/sen(α)
    constante = "constante",   # KP = h (línea horizontal)
    lineal = "lineal",         # KP = función lineal decreciente
    cuadratica = "cuadratica"  # KP = función cuadrática
  )
  
  # Asignar funciones a letras aleatoriamente
  funciones_mezcladas <- sample(names(funciones), 4)
  mapeo_funciones <- setNames(funciones_mezcladas, letras)
  
  return(list(
    punto_movil = contexto_seleccionado$punto_movil,
    punto_fijo1 = contexto_seleccionado$punto_fijo1,
    punto_fijo2 = contexto_seleccionado$punto_fijo2,
    termino_distancia = contexto_seleccionado$termino_distancia,
    termino_grafica = contexto_seleccionado$termino_grafica,
    h = h,
    colores_graficas = colores_graficas,
    angulo_min = angulo_min,
    angulo_max = angulo_max,
    letra_correcta = letra_correcta,
    mapeo_funciones = mapeo_funciones
  ))
}

# Generar datos del ejercicio
datos <- generar_datos()

# Extraer variables individuales
punto_movil <- datos$punto_movil
punto_fijo1 <- datos$punto_fijo1
punto_fijo2 <- datos$punto_fijo2
termino_distancia <- datos$termino_distancia
termino_grafica <- datos$termino_grafica
h <- datos$h
colores_graficas <- datos$colores_graficas
angulo_min <- datos$angulo_min
angulo_max <- datos$angulo_max
letra_correcta <- datos$letra_correcta
mapeo_funciones <- datos$mapeo_funciones

# Crear vector de solución
solucion <- rep(0, 4)
indice_correcto <- which(names(mapeo_funciones) == letra_correcta)
solucion[indice_correcto] <- 1
```





Question
========

En la figura se muestra un triángulo rectángulo donde LM es la hipotenusa y S es un punto que se mueve sobre la hipotenusa. La altura del triángulo desde el vértice del ángulo recto hasta la hipotenusa es 7 unidades.

\begin{center}\begin{tikzpicture}[scale=1.2]% Triángulo rectángulo\coordinate (A) at (0,0);\coordinate (B) at (6,0);\coordinate (C) at (2, 7 );\coordinate (K) at (3.5, 4.2 );% Dibujar el triángulo\draw[thick] (A) -- (B) -- (C) -- cycle;% Altura perpendicular\draw[dashed, red] (C) -- (3.5,0);\draw[red] (3.3,0) -- (3.3,0.2) -- (3.7,0.2) -- (3.7,0);% Punto móvil K\fill[blue] (K) circle (2pt);\draw[blue, thick] (C) -- (K);% Etiquetas\node[below] at (A) { L };\node[below] at (B) { M };\node[above] at (C) {C};\node[above right] at (K) { S };\node[left, red] at (1.75, 3.5 ) {h =  7 };% Ángulo α\draw[green!60!black] (3.5,0) arc (90:120:0.5);\node[green!60!black] at (3.2,0.3) {$\alpha$};\end{tikzpicture}\end{center}

Si SP representa la medida desde el punto S hasta el punto P (proyección perpendicular de S sobre LM), y α es el ángulo que forma la línea CS con la perpendicular, entonces SP = h/sen(α).

¿Cuál de las siguientes representacións representa mejor el comportamiento de SP en función del ángulo α?

![](grafica_opcion_A.png){width=8cm}
\vspace{0.5cm}![](grafica_opcion_B.png){width=8cm}
\vspace{0.5cm}![](grafica_opcion_C.png){width=8cm}
\vspace{0.5cm}![](grafica_opcion_D.png){width=8cm}

Solution
========

Para resolver este problema, debemos analizar la función SP = h/sen(α).

**Análisis matemático:**

La función SP = h/sen(α) = h · csc(α) es la función cosecante multiplicada por la constante h.

**Características de la función cosecante:**

1. **Dominio:** α ∈ (0°, 90°) en este contexto geométrico
2. **Comportamiento:**
   - Cuando α → 0°: sen(α) → 0, por lo tanto SP → ∞
   - Cuando α → 90°: sen(α) → 1, por lo tanto SP → h
   - La función es estrictamente decreciente en el intervalo (0°, 90°)

**Verificación con valores específicos:**

- Para α = 30°: SP = h/sen(30°) = h/0.5 = 2h = 14
- Para α = 45°: SP = h/sen(45°) = h/(√2/2) ≈ 1.41h ≈ 9.9
- Para α = 60°: SP = h/sen(60°) = h/(√3/2) ≈ 1.15h ≈ 8

La representación correcta debe mostrar:
- Una curva que decrece de manera no lineal
- Valores muy altos cuando α se acerca a 0°
- Un valor mínimo de h cuando α se acerca a 90°
- Forma característica de la función cosecante

**Análisis de las opciones:**- **Opción A :** Muestra la función cosecante h/sen(α) - **CORRECTA**- **Opción B :** Muestra una función lineal - Incorrecta- **Opción C :** Muestra una función constante - Incorrecta- **Opción D :** Muestra una función cuadrática - Incorrecta**Respuesta correcta:** Opción A

Meta-information
================
extype: schoice
exsolution: 1000
exname: trigonometria_funcion_cosecante_interpretacion_representacion_n2_v1
exshuffle: TRUE
