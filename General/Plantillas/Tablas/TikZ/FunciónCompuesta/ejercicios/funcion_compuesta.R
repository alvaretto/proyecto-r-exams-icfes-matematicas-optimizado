library(exams)

# Generación de datos aleatorios
generate_data <- function() {
  x_values <- -2:2
  f_values <- sample(-4:4, 5, replace = TRUE)
  g_values <- sample(-4:4, 5, replace = TRUE)
  
  data.frame(x = x_values, f = f_values, g = g_values)
}

# Creación del ejercicio
examen <- function(seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  # Generar datos
  data <- generate_data()
  
  # Crear tablas para la pregunta
  tabla1 <- data.frame(x = data$x, "f(x)" = data$f)
  tabla2 <- data.frame(x = data$x, "g(x)" = data$g)
  
  # Determinar si es posible calcular h(-2)
  es_posible <- -2 %in% data$x
  
  # Crear las opciones de respuesta
  opciones <- c(
    "No, porque se desconoce la fórmula utilizada para calcular los valores de f(x).",
    "Sí, porque se conocen los valores de f(x) y g(x) para los mismos valores de x.",
    "No, porque falta información sobre la función g(x) para completar el procedimiento.",
    "Sí, porque ese valor de x aparece en ambas columnas de las tablas de f(x) y g(x)."
  )
  
  # Determinar la respuesta correcta
  solucion <- if(es_posible) which(opciones == "Sí, porque se conocen los valores de f(x) y g(x) para los mismos valores de x.") else which(opciones == "No, porque falta información sobre la función g(x) para completar el procedimiento.")
  
  # Crear el texto de la pregunta
  pregunta <- paste0(
    "Las tablas 1 y 2 muestran algunos valores de las funciones f(x) y g(x) respectivamente.\n\n",
    "Tabla 1:\n", knitr::kable(tabla1, format = "pipe"), "\n\n",
    "Tabla 2:\n", knitr::kable(tabla2, format = "pipe"), "\n\n",
    "Con estas funciones se puede crear una nueva función: h(x) = g(f(x)). ",
    "Para calcular los valores de esta función, a partir de la información de las tablas 1 y 2, se debe efectuar este procedimiento:\n\n",
    "Paso 1. Escoger el valor de x con el que se va a calcular h(x).\n",
    "Paso 2. Ubicar el valor escogido en el paso 1, en la columna de x de la tabla 1 y tomar el valor de f(x) que se encuentra frente a este.\n",
    "Paso 3. Ubicar el valor hallado en el paso 2, en la columna de x de la tabla 2, y tomar el valor de g(x) que se encuentra frente a este.\n",
    "Paso 4. El valor encontrado en el paso 3 corresponde a h(x).\n\n",
    "Con base en lo anterior, ¿es posible hallar el valor de h(-2)?"
  )
  
  # Crear el objeto de la pregunta
  list(pregunta = pregunta,
       opciones = opciones,
       solucion = solucion,
       es_posible = es_posible)
}

# Generar el examen
set.seed(1234)
examen <- examen()

# Crear la pregunta en formato de examen
pregunta <- list(
  type = "schoice",
  question = examen$pregunta,
  solutions = examen$solucion,
  answers = examen$opciones,
  explanation = paste("La respuesta correcta es:", ifelse(examen$es_posible,
    "Sí, porque se conocen los valores de f(x) y g(x) para x = -2 en ambas tablas, lo que permite calcular h(-2) = g(f(-2)).",
    "No, porque no se proporciona el valor de g(x) para el resultado de f(-2), lo que impide completar el cálculo de h(-2) = g(f(-2))."
  ))
)

# Exportar la pregunta
exams2html(pregunta)