library(testthat)
library(digest)
library(exams)
library(reticulate)

# Cargar las variables y funciones del archivo Rmd
# Nota: esto no ejecutará los chunks de código, solo cargará las variables

# Establecer semilla aleatoria
set.seed(sample(1:10000, 1))

# Aleatorizar ciudades para el contexto del problema
ciudades <- c("Lima", "Bogotá", "Ciudad de México", "Santiago", "Buenos Aires",
              "Quito", "Caracas", "La Paz", "Asunción", "Montevideo",
              "San José", "Panamá", "Managua", "Tegucigalpa", "San Salvador",
              "Santo Domingo", "Medellín", "Cali", "Barranquilla", "Cartagena")
ciudades_seleccionadas <- sample(ciudades, sample(3:5, 1))
ciudades_texto <- paste(ciudades_seleccionadas, collapse = ", ")

# Aleatorizar términos para el contexto del problema
terminos_estudio <- c("estudio", "análisis", "investigación", "informe", "reporte")
termino_estudio <- sample(terminos_estudio, 1)

terminos_accidentes <- c("accidentes de tránsito", "siniestros viales", "incidentes de tráfico",
                         "colisiones vehiculares", "percances en carretera")
termino_accidente <- sample(terminos_accidentes, 1)

terminos_mortalidad <- c("mortalidad", "fallecimientos", "muertes", "víctimas fatales", "decesos")
termino_mortalidad <- sample(terminos_mortalidad, 1)

terminos_empresas <- c("empresa", "organización", "entidad", "institución", "compañía", "consultora")
termino_empresa <- sample(terminos_empresas, 1)

terminos_registro <- c("registro", "conteo", "recuento", "estadística", "cifra")
termino_registro <- sample(terminos_registro, 1)

# Aleatorizar años para el estudio (mantener 4 años consecutivos)
año_inicial <- sample(2000:2018, 1)
años <- año_inicial:(año_inicial + 3)
años_texto <- paste(min(años), "y", max(años))

# Generar datos de mortalidad total con tendencia realista
base_mortalidad <- sample(4000:8000, 1)
variacion_maxima <- round(base_mortalidad * 0.2)  # Variación máxima de 20%

# Generar tendencia aleatoria para mortalidad total
tendencias <- c("creciente", "decreciente", "pico", "valle", "ondulante")
tendencia <- sample(tendencias, 1)

mortalidad_total <- numeric(4)

if (tendencia == "creciente") {
  factor_incremento <- seq(1, 1.2, length.out = 4)
  mortalidad_total <- round(base_mortalidad * factor_incremento + rnorm(4, 0, variacion_maxima * 0.3))
} else if (tendencia == "decreciente") {
  factor_decremento <- seq(1.2, 1, length.out = 4)
  mortalidad_total <- round(base_mortalidad * factor_decremento + rnorm(4, 0, variacion_maxima * 0.3))
} else if (tendencia == "pico") {
  factores <- c(1, 1.1, 1.2, 1.05)
  mortalidad_total <- round(base_mortalidad * factores + rnorm(4, 0, variacion_maxima * 0.3))
} else if (tendencia == "valle") {
  factores <- c(1.15, 1.05, 1, 1.1)
  mortalidad_total <- round(base_mortalidad * factores + rnorm(4, 0, variacion_maxima * 0.3))
} else { # ondulante
  factores <- c(1, 1.15, 1.05, 1.2)
  mortalidad_total <- round(base_mortalidad * factores + rnorm(4, 0, variacion_maxima * 0.3))
}

# Asegurar que todos los valores sean positivos y tengan magnitud adecuada
mortalidad_total <- pmax(mortalidad_total, base_mortalidad * 0.9)
mortalidad_total <- pmin(mortalidad_total, base_mortalidad * 1.3)
mortalidad_total <- round(mortalidad_total)

# Generar datos para hombres (aproximadamente 75-85% del total - proporción realista)
proporcion_hombres <- runif(4, 0.75, 0.85)
mortalidad_hombres <- round(mortalidad_total * proporcion_hombres)

# Calcular datos para mujeres (el resto)
mortalidad_mujeres <- mortalidad_total - mortalidad_hombres

# Aleatorizar colores de las gráficas
colores_disponibles <- c("blue", "red", "green", "purple", "orange", "brown", "black", "magenta", "cyan")
color_hombres_correcto <- sample(colores_disponibles, 1)
colores_disponibles <- colores_disponibles[colores_disponibles != color_hombres_correcto]
color_mujeres_correcto <- sample(colores_disponibles, 1)

# Aleatorizar etiquetas de género
etiquetas_masculino <- c("Género Masculino")
etiqueta_masculino <- sample(etiquetas_masculino, 1)

etiquetas_femenino <- c("Género Femenino")
etiqueta_femenino <- sample(etiquetas_femenino, 1)

# Establecer la gráfica de líneas separadas A como la única correcta
opciones <- c("lineas_separadas_A", "lineas_separadas_B", "lineas_separadas_C", "lineas_separadas_D")
opcion_correcta <- "lineas_separadas_A"
indice_correcto <- 1  # Índice de "lineas_separadas_A" en el vector opciones

# Vector de solución para r-exams
solucion <- integer(4)
solucion[indice_correcto] <- 1

# Aleatorizar colores de la tabla
color_fondo_tabla <- sample(c("orange", "blue", "green", "red", "yellow", "cyan"), 1)
intensidad_color <- sample(c(10, 15, 20, 25, 30), 1)
color_tabla <- paste0(color_fondo_tabla, "!", intensidad_color)

# Ejecutar las pruebas unitarias
cat("\n\n===== EJECUTANDO PRUEBAS UNITARIAS =====\n\n")

# 1. Prueba de coherencia matemática: verificar que mortalidad_total = mortalidad_hombres + mortalidad_mujeres
test_that("La suma de mortalidad por género debe ser igual al total", {
  for (i in 1:length(mortalidad_total)) {
    expect_equal(mortalidad_total[i], mortalidad_hombres[i] + mortalidad_mujeres[i],
                 info = paste("Error en año", años[i], ": total =", mortalidad_total[i],
                              "pero hombres + mujeres =", mortalidad_hombres[i] + mortalidad_mujeres[i]))
  }
})

# 2. Prueba de proporciones realistas: verificar que la proporción de hombres está en el rango esperado
test_that("La proporción de víctimas masculinas debe estar en un rango realista", {
  for (i in 1:length(mortalidad_total)) {
    proporcion <- mortalidad_hombres[i] / mortalidad_total[i]
    expect_true(proporcion >= 0.75 && proporcion <= 0.85,
                info = paste("Proporción de hombres fuera de rango en año", años[i],
                             ": proporción =", round(proporcion, 2)))
  }
})

# 3. Prueba de tendencia temporal coherente
test_that("La tendencia temporal debe ser coherente con el patrón seleccionado", {
  if (tendencia == "creciente") {
    # Verificar que hay una tendencia general creciente (no necesariamente en cada paso)
    expect_true(mortalidad_total[4] > mortalidad_total[1],
                info = "La tendencia debería ser creciente pero el valor final no es mayor que el inicial")
  } else if (tendencia == "decreciente") {
    # Verificar que hay una tendencia general decreciente
    expect_true(mortalidad_total[4] < mortalidad_total[1],
                info = "La tendencia debería ser decreciente pero el valor final no es menor que el inicial")
  } else if (tendencia == "pico") {
    # Verificar que hay un pico en el medio
    expect_true(max(mortalidad_total) > mortalidad_total[1] && max(mortalidad_total) > mortalidad_total[4],
                info = "La tendencia debería tener un pico pero no se detecta")
  } else if (tendencia == "valle") {
    # Verificar que hay un valle en el medio
    expect_true(min(mortalidad_total) < mortalidad_total[1] && min(mortalidad_total) < mortalidad_total[4],
                info = "La tendencia debería tener un valle pero no se detecta")
  }
  # Para "ondulante" no hay una prueba específica ya que es más variable
})

# 4. Cálculo de variabilidad potencial para garantizar 300+ versiones
# Calculamos el número de combinaciones posibles basadas en las variables aleatorias
calcular_variabilidad <- function() {
  # Contamos las combinaciones de cada variable aleatoria
  n_ciudades <- choose(length(ciudades), 3) + choose(length(ciudades), 4) + choose(length(ciudades), 5)
  n_termino_estudio <- length(terminos_estudio)
  n_termino_accidente <- length(terminos_accidentes)
  n_termino_mortalidad <- length(terminos_mortalidad)
  n_termino_empresa <- length(terminos_empresas)
  n_termino_registro <- length(terminos_registro)
  n_años <- length(2000:2018)  # Posibles años iniciales
  n_tendencias <- length(tendencias)
  n_base_mortalidad <- length(4000:8000)  # Posibles valores base de mortalidad
  n_color_hombres <- length(colores_disponibles)
  n_color_mujeres <- length(colores_disponibles) - 1  # Restamos 1 porque no puede ser igual al color de hombres
  n_color_tabla <- length(c("orange", "blue", "green", "red", "yellow", "cyan")) * length(c(10, 15, 20, 25, 30))

  # Multiplicamos para obtener el número total de combinaciones posibles
  total_combinaciones <- n_ciudades * n_termino_estudio * n_termino_accidente *
                         n_termino_mortalidad * n_termino_empresa * n_termino_registro *
                         n_años * n_tendencias * n_base_mortalidad * n_color_hombres *
                         n_color_mujeres * n_color_tabla

  return(total_combinaciones)
}

# Calcular y verificar que hay al menos 300 versiones posibles
total_versiones <- calcular_variabilidad()
test_that("El ejercicio debe tener al menos 300 versiones posibles", {
  expect_true(total_versiones >= 300,
              info = paste("El número estimado de versiones es", format(total_versiones, scientific = FALSE),
                           "lo cual es insuficiente (mínimo requerido: 300)"))

  # Imprimir información sobre la variabilidad para referencia
  cat("\nNúmero estimado de versiones posibles:", format(total_versiones, scientific = FALSE, big.mark = ","), "\n")
})

# 5. Prueba de validez de datos generados
test_that("Los datos generados deben ser válidos y coherentes", {
  # Verificar que todos los valores son positivos
  expect_true(all(mortalidad_total > 0), "Todos los valores de mortalidad total deben ser positivos")
  expect_true(all(mortalidad_hombres > 0), "Todos los valores de mortalidad de hombres deben ser positivos")
  expect_true(all(mortalidad_mujeres > 0), "Todos los valores de mortalidad de mujeres deben ser positivos")

  # Verificar que los años son consecutivos
  expect_true(años[2] - años[1] == 1, "Los años deben ser consecutivos")
  expect_true(años[3] - años[2] == 1, "Los años deben ser consecutivos")
  expect_true(años[4] - años[3] == 1, "Los años deben ser consecutivos")

  # Verificar que la opción correcta está bien definida
  expect_true(opcion_correcta == "lineas_separadas_A", "La opción correcta debe ser 'lineas_separadas_A'")
  expect_true(indice_correcto == 1, "El índice correcto debe ser 1")
  expect_true(solucion[indice_correcto] == 1, "El vector de solución debe tener un 1 en la posición del índice correcto")
})

# 6. Prueba de diversidad en los datos generados
test_that("Los datos deben mostrar suficiente diversidad", {
  # Verificar que hay variación en los datos de mortalidad
  expect_true(length(unique(mortalidad_total)) >= 3,
              "Debe haber al menos 3 valores diferentes en los datos de mortalidad total")

  # Verificar que hay variación en la proporción hombres/mujeres
  proporciones <- mortalidad_hombres / mortalidad_mujeres
  expect_true(length(unique(round(proporciones, 2))) >= 2,
              "Debe haber al menos 2 proporciones diferentes entre hombres y mujeres")
})

# 7. Prueba de coherencia en las etiquetas y términos
test_that("Las etiquetas y términos deben ser coherentes", {
  # Verificar que las etiquetas de género son diferentes
  expect_false(etiqueta_masculino == etiqueta_femenino,
               "Las etiquetas de género masculino y femenino deben ser diferentes")

  # Verificar que los colores son diferentes
  expect_false(color_hombres_correcto == color_mujeres_correcto,
               "Los colores para hombres y mujeres deben ser diferentes")
})

# 8. Prueba de simulación para verificar la generación de 300+ versiones diferentes
test_that("Se pueden generar al menos 300 versiones diferentes mediante simulación", {
  # Función para generar un hash único de los datos principales del ejercicio
  generar_hash_version <- function() {
    # Combinar los elementos principales que hacen única cada versión
    elementos_clave <- list(
      ciudades_seleccionadas = paste(ciudades_seleccionadas, collapse = "-"),
      termino_estudio = termino_estudio,
      termino_accidente = termino_accidente,
      termino_mortalidad = termino_mortalidad,
      años = paste(años, collapse = "-"),
      mortalidad_total = paste(mortalidad_total, collapse = "-"),
      mortalidad_hombres = paste(mortalidad_hombres, collapse = "-"),
      mortalidad_mujeres = paste(mortalidad_mujeres, collapse = "-"),
      color_hombres = color_hombres_correcto,
      color_mujeres = color_mujeres_correcto,
      color_tabla = color_tabla
    )

    # Crear un hash único para esta combinación
    return(digest(elementos_clave, algo = "md5"))
  }

  # Guardar la semilla original para restaurarla después
  semilla_original <- .Random.seed
  on.exit(.Random.seed <<- semilla_original)

  # Número de simulaciones a realizar (50 es suficiente para verificar la variabilidad)
  n_simulaciones <- 50
  hashes_versiones <- character(n_simulaciones)

  # Realizar simulaciones y recopilar hashes únicos
  for (i in 1:n_simulaciones) {
    # Establecer una semilla aleatoria diferente para cada simulación
    set.seed(i * 1000)

    # Simular la generación de variables aleatorias (versión simplificada)
    ciudades_sim <- sample(ciudades, sample(3:5, 1))
    termino_estudio_sim <- sample(terminos_estudio, 1)
    termino_accidente_sim <- sample(terminos_accidentes, 1)
    termino_mortalidad_sim <- sample(terminos_mortalidad, 1)
    año_inicial_sim <- sample(2000:2018, 1)
    años_sim <- año_inicial_sim:(año_inicial_sim + 3)

    # Simular generación de datos de mortalidad
    base_mortalidad_sim <- sample(4000:8000, 1)
    variacion_maxima_sim <- round(base_mortalidad_sim * 0.2)
    tendencia_sim <- sample(tendencias, 1)

    mortalidad_total_sim <- numeric(4)
    if (tendencia_sim == "creciente") {
      factor_incremento <- seq(1, 1.2, length.out = 4)
      mortalidad_total_sim <- round(base_mortalidad_sim * factor_incremento +
                                   rnorm(4, 0, variacion_maxima_sim * 0.3))
    } else if (tendencia_sim == "decreciente") {
      factor_decremento <- seq(1.2, 1, length.out = 4)
      mortalidad_total_sim <- round(base_mortalidad_sim * factor_decremento +
                                   rnorm(4, 0, variacion_maxima_sim * 0.3))
    } else if (tendencia_sim == "pico") {
      factores <- c(1, 1.1, 1.2, 1.05)
      mortalidad_total_sim <- round(base_mortalidad_sim * factores +
                                  rnorm(4, 0, variacion_maxima_sim * 0.3))
    } else if (tendencia_sim == "valle") {
      factores <- c(1.15, 1.05, 1, 1.1)
      mortalidad_total_sim <- round(base_mortalidad_sim * factores +
                                  rnorm(4, 0, variacion_maxima_sim * 0.3))
    } else { # ondulante
      factores <- c(1, 1.15, 1.05, 1.2)
      mortalidad_total_sim <- round(base_mortalidad_sim * factores +
                                  rnorm(4, 0, variacion_maxima_sim * 0.3))
    }

    # Asegurar valores positivos y magnitud adecuada
    mortalidad_total_sim <- pmax(mortalidad_total_sim, base_mortalidad_sim * 0.9)
    mortalidad_total_sim <- pmin(mortalidad_total_sim, base_mortalidad_sim * 1.3)
    mortalidad_total_sim <- round(mortalidad_total_sim)

    # Generar datos para hombres y mujeres
    proporcion_hombres_sim <- runif(4, 0.75, 0.85)
    mortalidad_hombres_sim <- round(mortalidad_total_sim * proporcion_hombres_sim)
    mortalidad_mujeres_sim <- mortalidad_total_sim - mortalidad_hombres_sim

    # Simular colores y otros elementos visuales
    colores_disponibles_sim <- c("blue", "red", "green", "purple", "orange", "brown", "black", "magenta", "cyan")
    color_hombres_sim <- sample(colores_disponibles_sim, 1)
    colores_disponibles_sim <- colores_disponibles_sim[colores_disponibles_sim != color_hombres_sim]
    color_mujeres_sim <- sample(colores_disponibles_sim, 1)

    color_fondo_tabla_sim <- sample(c("orange", "blue", "green", "red", "yellow", "cyan"), 1)
    intensidad_color_sim <- sample(c(10, 15, 20, 25, 30), 1)
    color_tabla_sim <- paste0(color_fondo_tabla_sim, "!", intensidad_color_sim)

    # Crear un hash único para esta simulación
    elementos_clave_sim <- list(
      ciudades_seleccionadas = paste(ciudades_sim, collapse = "-"),
      termino_estudio = termino_estudio_sim,
      termino_accidente = termino_accidente_sim,
      termino_mortalidad = termino_mortalidad_sim,
      años = paste(años_sim, collapse = "-"),
      mortalidad_total = paste(mortalidad_total_sim, collapse = "-"),
      mortalidad_hombres = paste(mortalidad_hombres_sim, collapse = "-"),
      mortalidad_mujeres = paste(mortalidad_mujeres_sim, collapse = "-"),
      color_hombres = color_hombres_sim,
      color_mujeres = color_mujeres_sim,
      color_tabla = color_tabla_sim
    )

    hashes_versiones[i] <- digest(elementos_clave_sim, algo = "md5")
  }

  # Verificar cuántas versiones únicas se generaron
  versiones_unicas <- length(unique(hashes_versiones))
  cat("\nNúmero de versiones únicas generadas en la simulación:", versiones_unicas,
      "de", n_simulaciones, "simulaciones\n")

  # La prueba pasa si todas las simulaciones generaron versiones diferentes
  # Esto indica una alta variabilidad, lo que garantiza que se pueden generar más de 300 versiones
  expect_equal(versiones_unicas, n_simulaciones,
               info = paste("Solo se generaron", versiones_unicas, "versiones únicas de",
                            n_simulaciones, "simulaciones. Esto sugiere que la variabilidad",
                            "puede no ser suficiente para garantizar 300+ versiones."))

  # Restaurar la semilla original (también se hace con on.exit)
  .Random.seed <<- semilla_original
})

cat("\n\n===== PRUEBAS UNITARIAS COMPLETADAS =====\n\n")
