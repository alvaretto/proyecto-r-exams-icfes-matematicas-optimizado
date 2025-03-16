# Tests para verificar el correcto funcionamiento de cloze-cuartil-estatura-00.Rmd

# Cargar librerías necesarias
library(testthat)

# Función para calcular cuartiles con método tradicional (importada del Rmd)
calcular_cuartiles_tradicional <- function(datos) {
  n <- length(datos)
  
  # Calcular posiciones
  pos_q1 <- (n + 1) / 4
  pos_q2 <- (n + 1) / 2
  pos_q3 <- 3 * (n + 1) / 4
  
  # Calcular valores
  q1 <- if(pos_q1 %% 1 == 0) {
    datos[pos_q1]
  } else {
    parte_entera <- floor(pos_q1)
    parte_decimal <- pos_q1 - parte_entera
    datos[parte_entera] + parte_decimal * (datos[parte_entera + 1] - datos[parte_entera])
  }
  
  q2 <- if(pos_q2 %% 1 == 0) {
    datos[pos_q2]
  } else {
    parte_entera <- floor(pos_q2)
    parte_decimal <- pos_q2 - parte_entera
    datos[parte_entera] + parte_decimal * (datos[parte_entera + 1] - datos[parte_entera])
  }
  
  q3 <- if(pos_q3 %% 1 == 0) {
    datos[pos_q3]
  } else {
    parte_entera <- floor(pos_q3)
    parte_decimal <- pos_q3 - parte_entera
    datos[parte_entera] + parte_decimal * (datos[parte_entera + 1] - datos[parte_entera])
  }
  
  return(list(q1 = q1, q2 = q2, q3 = q3))
}

# Función para generar datos de estatura (importada del Rmd)
generar_datos_estatura <- function(
  tipo_distribucion = sample(c("simetrica", "sesgo_derecha", "sesgo_izquierda"), 1),
  n = sample(9:25, 1)  # Permitir tamaños pares o impares
) {
  # Configuraciones base según tipo de distribución
  if (tipo_distribucion == "simetrica") {
    base_min <- sample(145:155, 1)
    base_max <- base_min + sample(30:40, 1)
    medio <- (base_min + base_max) / 2
    datos <- sort(round(rnorm(n, mean = medio, sd = (base_max - base_min) / 6)))
  } else if (tipo_distribucion == "sesgo_derecha") {
    base_min <- sample(145:155, 1)
    base_max <- base_min + sample(35:45, 1)
    # Generar con sesgo a la derecha (valores altos menos frecuentes)
    datos <- sort(round(base_min + rbeta(n, 1, 3) * (base_max - base_min)))
  } else { # sesgo_izquierda
    base_min <- sample(145:155, 1)
    base_max <- base_min + sample(35:45, 1)
    # Generar con sesgo a la izquierda (valores bajos menos frecuentes)
    datos <- sort(round(base_min + rbeta(n, 3, 1) * (base_max - base_min)))
  }
  
  # Asegurar que los datos no sean muy uniformes
  if (length(unique(datos)) < n * 0.7) {
    datos <- datos + sample(-3:3, n, replace = TRUE)
    datos <- sort(datos)
  }
  
  # Calcular estadísticas usando el método tradicional
  cuartiles <- calcular_cuartiles_tradicional(datos)
  
  stats <- list(
    datos = datos,
    datos_desordenados = sample(datos),
    minimo = min(datos),
    q1 = cuartiles$q1,
    mediana = cuartiles$q2,
    q3 = cuartiles$q3,
    maximo = max(datos),
    rango_intercuartil = cuartiles$q3 - cuartiles$q1,
    tipo_distribucion = tipo_distribucion
  )
  
  # Aplicar formato de un decimal a los cuartiles
  stats$q1 <- round(stats$q1, 1)
  stats$mediana <- round(stats$mediana, 1)
  stats$q3 <- round(stats$q3, 1)
  
  return(stats)
}

# Función para aplicar diferentes tipos de errores a los diagramas
aplicar_error <- function(stats, tipo_error) {
  # Copiar los stats originales
  valores <- stats
  
  # Aplicar error según tipo
  if (tipo_error == "q1_incorrecto") {
    # Q1 muy bajo o muy alto
    direccion <- sample(c(-1, 1), 1)
    valores$q1 <- valores$q1 + direccion * round(valores$rango_intercuartil * runif(1, 0.3, 0.5))
    
  } else if (tipo_error == "q3_incorrecto") {
    # Q3 muy bajo o muy alto
    direccion <- sample(c(-1, 1), 1)
    valores$q3 <- valores$q3 + direccion * round(valores$rango_intercuartil * runif(1, 0.3, 0.5))
    
  } else if (tipo_error == "mediana_incorrecta") {
    # Mediana fuera de lugar pero dentro de la caja
    if (runif(1) > 0.5) {
      # Cerca de Q1
      valores$mediana <- valores$q1 + (valores$mediana - valores$q1) * 0.2
    } else {
      # Cerca de Q3
      valores$mediana <- valores$q3 - (valores$q3 - valores$mediana) * 0.2
    }
    
  } else if (tipo_error == "extremos_incorrectos") {
    # Extremos acortados o elongados
    if (runif(1) > 0.5) {
      # Acortar
      valores$minimo <- valores$minimo + round((valores$q1 - valores$minimo) * 0.4)
      valores$maximo <- valores$maximo - round((valores$maximo - valores$q3) * 0.4)
    } else {
      # Elongar
      rango <- valores$maximo - valores$minimo
      valores$minimo <- valores$minimo - round(rango * 0.15)
      valores$maximo <- valores$maximo + round(rango * 0.15)
    }
    
  } else if (tipo_error == "mediana_fuera") {
    # Error estructural: mediana fuera de la caja
    if (runif(1) > 0.5) {
      # Por debajo de Q1
      valores$mediana <- valores$q1 - round(valores$rango_intercuartil * runif(1, 0.2, 0.4))
    } else {
      # Por encima de Q3
      valores$mediana <- valores$q3 + round(valores$rango_intercuartil * runif(1, 0.2, 0.4))
    }
  }
  
  return(valores)
}

# Función para calcular valores de diagrama
calcular_valores_diagrama <- function(stats, tipo_diagrama) {
  if (tipo_diagrama == "correcto") {
    return(stats)
  } else if (tipo_diagrama == "escala") {
    # Cambiar la escala del diagrama
    factor <- runif(1, 1.2, 1.5)
    valores <- stats
    rango <- valores$maximo - valores$minimo
    centro <- (valores$maximo + valores$minimo) / 2
    
    valores$minimo <- round(centro - factor * rango / 2)
    valores$maximo <- round(centro + factor * rango / 2)
    return(valores)
  } else if (tipo_diagrama == "invertido") {
    # Invertir el diagrama (espejo)
    valores <- stats
    medio <- (valores$minimo + valores$maximo) / 2
    
    # Reflejar todos los valores respecto al punto medio
    valores$q1_nuevo <- medio + (medio - valores$q3)
    valores$q3_nuevo <- medio + (medio - valores$q1)
    valores$mediana_nueva <- medio + (medio - valores$mediana)
    
    # Actualizar valores
    valores$q1 <- valores$q1_nuevo
    valores$q3 <- valores$q3_nuevo
    valores$mediana <- valores$mediana_nueva
    
    # Eliminar campos temporales
    valores$q1_nuevo <- NULL
    valores$q3_nuevo <- NULL
    valores$mediana_nueva <- NULL
    
    return(valores)
  } else if (tipo_diagrama == "mediana_falsa") {
    # Mediana incorrecta
    valores <- stats
    
    # Mover la mediana a un valor incorrecto
    if (runif(1) > 0.5) {
      # Más cerca de Q1
      valores$mediana <- valores$q1 + (valores$q3 - valores$q1) * runif(1, 0.1, 0.3)
    } else {
      # Más cerca de Q3
      valores$mediana <- valores$q3 - (valores$q3 - valores$q1) * runif(1, 0.1, 0.3)
    }
    
    return(valores)
  }
}

# Función para verificar si dos diagramas son diferentes
son_diagramas_diferentes <- function(diag1, diag2) {
  # Verificar si al menos uno de los valores es diferente
  if (identical(diag1$minimo, diag2$minimo) && 
      identical(diag1$q1, diag2$q1) && 
      identical(diag1$mediana, diag2$mediana) && 
      identical(diag1$q3, diag2$q3) && 
      identical(diag1$maximo, diag2$maximo)) {
    return(FALSE)
  }
  return(TRUE)
}

# Función para asegurar que todos los diagramas son diferentes entre sí
asegurar_diagramas_diferentes <- function(diagramas) {
  nombres <- names(diagramas)
  for (i in 1:(length(nombres)-1)) {
    for (j in (i+1):length(nombres)) {
      diag1 <- diagramas[[nombres[i]]]
      diag2 <- diagramas[[nombres[j]]]
      
      if (!son_diagramas_diferentes(diag1, diag2)) {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

# Función para verificar la diversidad de diagramas
verificar_diversidad_diagramas <- function(n_simulaciones = 1500) {
  huellas_digitales <- character(n_simulaciones)
  
  for (i in 1:n_simulaciones) {
    # Generar datos aleatorios
    stats <- generar_datos_estatura(
      tipo_distribucion = sample(c("simetrica", "sesgo_derecha", "sesgo_izquierda"), 1),
      n = sample(9:25, 1)
    )
    
    # Crear una huella digital basada en los valores clave
    huella <- paste(
      stats$minimo, stats$q1, stats$mediana, 
      stats$q3, stats$maximo, length(stats$datos),
      stats$tipo_distribucion,
      collapse = "_"
    )
    
    huellas_digitales[i] <- huella
  }
  
  # Contar combinaciones únicas
  total_unicos <- length(unique(huellas_digitales))
  
  return(list(
    total_unicos = total_unicos,
    suficiente_diversidad = total_unicos >= 400
  ))
}

# Función para verificar si las etiquetas se solapan
verificar_solapamiento_etiquetas <- function(valores) {
  # Extraer valores clave
  valores_clave <- c(
    valores$minimo,
    valores$q1,
    valores$mediana,
    valores$q3,
    valores$maximo
  )
  
  # Calcular diferencias entre valores adyacentes
  diferencias <- diff(valores_clave)
  
  # Verificar si hay diferencias muy pequeñas (potencial solapamiento)
  min_diferencia <- min(diferencias)
  
  # Consideramos que hay riesgo de solapamiento si la diferencia es menor a 3 unidades
  return(min_diferencia >= 3)
}

# Función para simular la generación de una pregunta completa
simular_generacion_pregunta <- function() {
  # Generar datos base
  stats <- generar_datos_estatura(
    tipo_distribucion = sample(c("simetrica", "sesgo_derecha", "sesgo_izquierda"), 1),
    n = sample(9:25, 1)
  )
  
  # Verificar que no hay solapamiento de etiquetas
  if (!verificar_solapamiento_etiquetas(stats)) {
    return(list(exito = FALSE, razon = "Solapamiento de etiquetas"))
  }
  
  # Crear diagramas para las opciones
  tipos_diagramas <- c("correcto", "escala", "invertido", "mediana_falsa")
  posicion_correcto <- sample(1:4, 1)
  
  valores_diagramas <- list()
  tipo_error_diagramas <- character(4)
  
  for (i in 1:4) {
    if (i == posicion_correcto) {
      valores_diagramas[[i]] <- stats
      tipo_error_diagramas[i] <- "ninguno"
    } else {
      # Seleccionar tipo de diagrama incorrecto
      tipo_diagrama <- sample(tipos_diagramas[tipos_diagramas != "correcto"], 1)
      
      # Generar diagrama incorrecto
      nuevo_diagrama <- calcular_valores_diagrama(stats, tipo_diagrama)
      
      valores_diagramas[[i]] <- nuevo_diagrama
      tipo_error_diagramas[i] <- tipo_diagrama
    }
  }
  
  # Verificar que todos los diagramas son diferentes entre sí
  diagramas_diferentes <- asegurar_diagramas_diferentes(valores_diagramas)
  
  if (!diagramas_diferentes) {
    return(list(exito = FALSE, razon = "Diagramas no suficientemente diferentes"))
  }
  
  # Verificar que no hay gráficas vacías
  for (i in 1:4) {
    diag <- valores_diagramas[[i]]
    if (is.null(diag$minimo) || is.na(diag$minimo) ||
        is.null(diag$q1) || is.na(diag$q1) ||
        is.null(diag$mediana) || is.na(diag$mediana) ||
        is.null(diag$q3) || is.na(diag$q3) ||
        is.null(diag$maximo) || is.na(diag$maximo)) {
      return(list(exito = FALSE, razon = "Gráfica vacía detectada"))
    }
  }
  
  return(list(
    exito = TRUE,
    stats = stats,
    diagramas = valores_diagramas,
    posicion_correcto = posicion_correcto
  ))
}

# PRUEBAS UNITARIAS

# Prueba 1: Verificar cálculo de cuartiles con método tradicional
test_calculo_cuartiles_tradicional <- function() {
  cat("Verificando cálculo de cuartiles con método tradicional...\n")
  
  # Caso 1: Conjunto impar
  datos_impar <- c(5, 7, 10, 15, 20, 25, 30)
  cuartiles_impar <- calcular_cuartiles_tradicional(datos_impar)
  
  expect_equal(cuartiles_impar$q1, 7, info = "Q1 incorrecto para conjunto impar")
  expect_equal(cuartiles_impar$q2, 15, info = "Q2 incorrecto para conjunto impar")
  expect_equal(cuartiles_impar$q3, 25, info = "Q3 incorrecto para conjunto impar")
  
  # Caso 2: Conjunto par
  datos_par <- c(5, 7, 10, 15, 20, 25, 30, 35)
  cuartiles_par <- calcular_cuartiles_tradicional(datos_par)
  
  expect_equal(cuartiles_par$q1, 7.25, info = "Q1 incorrecto para conjunto par")
  expect_equal(cuartiles_par$q2, 17.5, info = "Q2 incorrecto para conjunto par")
  expect_equal(cuartiles_par$q3, 28.75, info = "Q3 incorrecto para conjunto par")
  
  cat("   Cálculo de cuartiles con método tradicional correcto\n")
  return(TRUE)
}

# Prueba 2: Verificar que se permiten cuartiles con posición decimal
test_cuartiles_posicion_decimal <- function() {
  cat("Verificando que se permiten cuartiles con posición decimal...\n")
  
  # Generar varios conjuntos de datos
  for (i in 1:10) {
    n <- sample(8:20, 1) * 2  # Asegurar tamaño par para tener posiciones decimales
    datos <- sort(sample(100:200, n))
    
    cuartiles <- calcular_cuartiles_tradicional(datos)
    
    # Verificar que los cuartiles pueden tener decimales
    q1_decimal <- cuartiles$q1 %% 1 != 0
    q2_decimal <- cuartiles$q2 %% 1 != 0
    q3_decimal <- cuartiles$q3 %% 1 != 0
    
    # Al menos uno de los cuartiles debería tener decimal para conjuntos pares
    expect_true(q1_decimal || q2_decimal || q3_decimal, 
                info = paste("Ningún cuartil tiene posición decimal para conjunto de tamaño", n))
  }
  
  cat("   Se permiten cuartiles con posición decimal\n")
  return(TRUE)
}

# Prueba 3: Verificar que las etiquetas no se solapan
test_no_solapamiento_etiquetas <- function() {
  cat("Verificando que las etiquetas no se solapan...\n")
  
  # Generar varios conjuntos de datos
  exitos <- 0
  intentos <- 20
  
  for (i in 1:intentos) {
    stats <- generar_datos_estatura()
    
    if (verificar_solapamiento_etiquetas(stats)) {
      exitos <- exitos + 1
    }
  }
  
  # Esperamos que al menos el 80% de los casos no tengan solapamiento
  tasa_exito <- exitos / intentos
  expect_true(tasa_exito >= 0.8, 
              info = paste("Solo", tasa_exito * 100, "% de los casos no tienen solapamiento"))
  
  cat("   Las etiquetas no se solapan en la mayoría de los casos\n")
  return(TRUE)
}

# Prueba 4: Verificar que se permiten conjuntos par e impar
test_conjuntos_par_impar <- function() {
  cat("Verificando que se permiten conjuntos par e impar...\n")
  
  # Generar conjuntos par e impar
  n_par <- 10
  n_impar <- 11
  
  stats_par <- generar_datos_estatura(n = n_par)
  stats_impar <- generar_datos_estatura(n = n_impar)
  
  # Verificar longitud de los datos
  expect_equal(length(stats_par$datos), n_par, 
               info = paste("Conjunto par debería tener", n_par, "elementos"))
  expect_equal(length(stats_impar$datos), n_impar, 
               info = paste("Conjunto impar debería tener", n_impar, "elementos"))
  
  # Verificar que los datos desordenados tienen la misma longitud
  expect_equal(length(stats_par$datos_desordenados), n_par, 
               info = "Datos desordenados par no tienen la longitud correcta")
  expect_equal(length(stats_impar$datos_desordenados), n_impar, 
               info = "Datos desordenados impar no tienen la longitud correcta")
  
  # Verificar que los datos desordenados son una permutación de los datos originales
  expect_setequal(stats_par$datos, stats_par$datos_desordenados, 
                  info = "Datos desordenados par no son permutación de los originales")
  expect_setequal(stats_impar$datos, stats_impar$datos_desordenados, 
                  info = "Datos desordenados impar no son permutación de los originales")
  
  # Verificar que los datos desordenados están realmente desordenados
  expect_false(identical(stats_par$datos, stats_par$datos_desordenados), 
               info = "Datos par no están realmente desordenados")
  expect_false(identical(stats_impar$datos, stats_impar$datos_desordenados), 
               info = "Datos impar no están realmente desordenados")
  
  cat("   Se permiten conjuntos par e impar y se muestran desordenados\n")
  return(TRUE)
}

# Prueba 5: Verificar que no hay duplicidad en las opciones de respuesta
test_no_duplicidad_opciones <- function() {
  cat("Verificando que no hay opciones de respuesta duplicadas...\n")
  
  # Número de conjuntos de datos a probar
  n_pruebas <- 50
  exitos <- 0
  
  for (i in 1:n_pruebas) {
    resultado <- simular_generacion_pregunta()
    
    if (resultado$exito) {
      exitos <- exitos + 1
    }
  }
  
  # Esperamos que al menos el 90% de las simulaciones sean exitosas
  tasa_exito <- exitos / n_pruebas
  expect_true(tasa_exito >= 0.9, 
              info = paste("Solo", tasa_exito * 100, "% de las simulaciones generaron opciones diferentes"))
  
  cat("   No se encontraron opciones de respuesta duplicadas en", exitos, "de", n_pruebas, "pruebas\n")
  return(TRUE)
}

# Prueba 6: Verificar la diversidad de diagramas
test_diversidad_diagramas <- function() {
  cat("Verificando diversidad de diagramas...\n")
  
  # Usar un número menor de simulaciones para la prueba
  diversidad <- verificar_diversidad_diagramas(n_simulaciones = 500)
  
  # Verificar que hay suficiente diversidad
  expect_true(diversidad$suficiente_diversidad, 
              info = paste("Solo se pueden generar", diversidad$total_unicos, 
                          "diagramas diferentes. Se requieren al menos 400."))
  
  cat("   Se pueden generar al menos", diversidad$total_unicos, "diagramas diferentes\n")
  return(TRUE)
}

# Prueba 7: Verificar que no hay gráficas vacías
test_no_graficas_vacias <- function() {
  cat("Verificando que no hay gráficas vacías...\n")
  
  # Generar varios conjuntos de datos
  for (i in 1:10) {
    stats <- generar_datos_estatura()
    
    # Verificar que todos los valores clave están presentes
    expect_false(is.null(stats$minimo) || is.na(stats$minimo),
                info = "Valor mínimo es NULL o NA")
    expect_false(is.null(stats$q1) || is.na(stats$q1),
                info = "Q1 es NULL o NA")
    expect_false(is.null(stats$mediana) || is.na(stats$mediana),
                info = "Mediana es NULL o NA")
    expect_false(is.null(stats$q3) || is.na(stats$q3),
                info = "Q3 es NULL o NA")
    expect_false(is.null(stats$maximo) || is.na(stats$maximo),
                info = "Valor máximo es NULL o NA")
    
    # Verificar que hay datos
    expect_true(length(stats$datos) > 0,
               info = "No hay datos en el conjunto")
    expect_true(length(stats$datos_desordenados) > 0,
               info = "No hay datos desordenados en el conjunto")
  }
  
  cat("   No se encontraron gráficas vacías\n")
  return(TRUE)
}

# Función para ejecutar todas las pruebas
run_all_tests <- function() {
  cat("==== Ejecutando pruebas unitarias para cloze-cuartil-estatura-00.Rmd ====\n\n")
  
  # Registrar pruebas pasadas
  pruebas_pasadas <- 0
  total_pruebas <- 7
  
  # Test 1
  tryCatch({
    test_calculo_cuartiles_tradicional()
    pruebas_pasadas <- pruebas_pasadas + 1
  }, error = function(e) {
    cat("❌ FALLO en test_calculo_cuartiles_tradicional:", e$message, "\n")
  })
  
  # Test 2
  tryCatch({
    test_cuartiles_posicion_decimal()
    pruebas_pasadas <- pruebas_pasadas + 1
  }, error = function(e) {
    cat("❌ FALLO en test_cuartiles_posicion_decimal:", e$message, "\n")
  })
  
  # Test 3
  tryCatch({
    test_no_solapamiento_etiquetas()
    pruebas_pasadas <- pruebas_pasadas + 1
  }, error = function(e) {
    cat("❌ FALLO en test_no_solapamiento_etiquetas:", e$message, "\n")
  })
  
  # Test 4
  tryCatch({
    test_conjuntos_par_impar()
    pruebas_pasadas <- pruebas_pasadas + 1
  }, error = function(e) {
    cat("❌ FALLO en test_conjuntos_par_impar:", e$message, "\n")
  })
  
  # Test 5
  tryCatch({
    test_no_duplicidad_opciones()
    pruebas_pasadas <- pruebas_pasadas + 1
  }, error = function(e) {
    cat("❌ FALLO en test_no_duplicidad_opciones:", e$message, "\n")
  })
  
  # Test 6
  tryCatch({
    test_diversidad_diagramas()
    pruebas_pasadas <- pruebas_pasadas + 1
  }, error = function(e) {
    cat("❌ FALLO en test_diversidad_diagramas:", e$message, "\n")
  })
  
  # Test 7
  tryCatch({
    test_no_graficas_vacias()
    pruebas_pasadas <- pruebas_pasadas + 1
  }, error = function(e) {
    cat("❌ FALLO en test_no_graficas_vacias:", e$message, "\n")
  })
  
  # Resumen
  cat("\n==== Resumen de pruebas ====\n")
  cat("Pruebas pasadas:", pruebas_pasadas, "de", total_pruebas, "\n")
  
  if (pruebas_pasadas == total_pruebas) {
    cat("✅ TODAS LAS PRUEBAS PASARON EXITOSAMENTE\n")
  } else {
    cat("❌ ALGUNAS PRUEBAS FALLARON\n")
  }
}

# Ejecutar todas las pruebas si este script se ejecuta directamente
if (!interactive()) {
  run_all_tests()
}