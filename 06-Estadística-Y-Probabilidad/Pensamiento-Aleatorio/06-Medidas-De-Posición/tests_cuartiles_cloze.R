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
  
  # Asegurar que los datos no sean muy uniformes y tengan suficiente separación
  if (length(unique(datos)) < n * 0.8) {
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
  
  # Asegurar que no hay solapamiento entre valores clave
  min_diferencia <- 2
  if ((stats$q1 - stats$minimo) < min_diferencia || 
      (stats$mediana - stats$q1) < min_diferencia || 
      (stats$q3 - stats$mediana) < min_diferencia || 
      (stats$maximo - stats$q3) < min_diferencia) {
    # Ajustar valores para evitar solapamiento
    stats$minimo <- min(stats$minimo, stats$q1 - min_diferencia)
    stats$maximo <- max(stats$maximo, stats$q3 + min_diferencia)
    # Asegurar separación entre cuartiles
    if ((stats$mediana - stats$q1) < min_diferencia) {
      stats$q1 <- stats$mediana - min_diferencia
    }
    if ((stats$q3 - stats$mediana) < min_diferencia) {
      stats$q3 <- stats$mediana + min_diferencia
    }
  }
  
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
      valores$minimo <- valores$minimo - round((valores$q1 - valores$minimo) * 0.4)
      valores$maximo <- valores$maximo + round((valores$maximo - valores$q3) * 0.4)
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
son_diagramas_diferentes <- function(stats1, stats2) {
  # Extraer valores clave
  valores1 <- c(stats1$minimo, stats1$q1, stats1$mediana, stats1$q3, stats1$maximo)
  valores2 <- c(stats2$minimo, stats2$q1, stats2$mediana, stats2$q3, stats2$maximo)
  
  # Calcular diferencia relativa
  diferencia_relativa <- sum(abs(valores1 - valores2)) / sum(abs(valores1))
  
  # Considerar diferentes si la diferencia relativa es mayor al 5%
  return(diferencia_relativa > 0.05)
}

# Función para asegurar que todos los diagramas son diferentes entre sí
asegurar_diagramas_diferentes <- function(diagramas) {
  n <- length(diagramas)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      if (!son_diagramas_diferentes(diagramas[[i]], diagramas[[j]])) {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

# Función para verificar si hay solapamiento de etiquetas
verificar_solapamiento_etiquetas <- function(stats) {
  # Extraer valores clave
  valores <- c(stats$minimo, stats$q1, stats$mediana, stats$q3, stats$maximo)
  
  # Calcular diferencias entre valores consecutivos
  diferencias <- diff(valores)
  
  # Verificar si hay suficiente espacio entre valores
  min_espacio <- 2  # Espacio mínimo para evitar solapamiento
  return(all(diferencias >= min_espacio))
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

# Función para simular la generación de una pregunta completa
simular_generacion_pregunta <- function() {
  # Intentar varias veces para encontrar datos adecuados
  max_intentos <- 10
  for (intento in 1:max_intentos) {
    # Generar datos base
    stats <- generar_datos_estatura(
      tipo_distribucion = sample(c("simetrica", "sesgo_derecha", "sesgo_izquierda"), 1),
      n = sample(9:25, 1)
    )
    
    # Verificar que no hay solapamiento de etiquetas
    if (verificar_solapamiento_etiquetas(stats)) {
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
      
      # Verificar que todos los diagramas son diferentes
      if (asegurar_diagramas_diferentes(valores_diagramas)) {
        return(list(
          exito = TRUE,
          stats_originales = stats,
          valores_diagramas = valores_diagramas,
          tipo_error_diagramas = tipo_error_diagramas,
          posicion_correcto = posicion_correcto
        ))
      }
    }
  }
  
  # Si llegamos aquí, no se pudo generar una pregunta válida
  return(list(exito = FALSE))
}

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
  
  # Corregir el valor esperado para Q1 según el método tradicional
  expect_equal(cuartiles_par$q1, 7.75, info = "Q1 incorrecto para conjunto par")
  expect_equal(cuartiles_par$q2, 17.5, info = "Q2 incorrecto para conjunto par")
  expect_equal(cuartiles_par$q3, 27.5, info = "Q3 incorrecto para conjunto par")
  
  cat("   Cálculo de cuartiles correcto\n")
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
    # Intentar varias veces para encontrar datos sin solapamiento
    for (j in 1:5) {
      stats <- generar_datos_estatura()
      
      if (verificar_solapamiento_etiquetas(stats)) {
        exitos <- exitos + 1
        break
      }
    }
  }
  
  # Esperamos que al menos el 80% de los casos no tengan solapamiento
  tasa_exito <- exitos / intentos
  expect_true(tasa_exito >= 0.8, 
              info = paste("Solo", tasa_exito * 100, "% de los casos no tienen solapamiento"))
  
  cat("   Las etiquetas no se solapan en la mayoría de los casos\n")
  return(TRUE)
}

# Prueba 4: Verificar que se manejan correctamente conjuntos pares e impares
test_conjuntos_par_impar <- function() {
  cat("Verificando manejo de conjuntos pares e impares...\n")
  
  # Probar con tamaños pares e impares
  for (n in c(9, 10, 11, 12)) {
    # Generar datos
    datos <- sort(sample(100:200, n))
    
    # Calcular cuartiles
    cuartiles <- calcular_cuartiles_tradicional(datos)
    
    # Verificar que los cuartiles están en orden
    expect_true(cuartiles$q1 <= cuartiles$q2, 
                info = paste("Q1 > Q2 para conjunto de tamaño", n))
    expect_true(cuartiles$q2 <= cuartiles$q3, 
                info = paste("Q2 > Q3 para conjunto de tamaño", n))
    
    # Verificar que los cuartiles están dentro del rango de datos
    expect_true(cuartiles$q1 >= min(datos) && cuartiles$q1 <= max(datos),
                info = paste("Q1 fuera de rango para conjunto de tamaño", n))
    expect_true(cuartiles$q2 >= min(datos) && cuartiles$q2 <= max(datos),
                info = paste("Q2 fuera de rango para conjunto de tamaño", n))
    expect_true(cuartiles$q3 >= min(datos) && cuartiles$q3 <= max(datos),
                info = paste("Q3 fuera de rango para conjunto de tamaño", n))
  }
  
  cat("   Manejo correcto de conjuntos pares e impares\n")
  return(TRUE)
}

# Prueba 5: Verificar que no hay duplicidad en las opciones de respuesta
test_no_duplicidad_opciones <- function() {
  cat("Verificando que no hay opciones de respuesta duplicadas...\n")
  
  # Número de conjuntos de datos a probar
  n_pruebas <- 20
  exitos <- 0
  
  for (i in 1:n_pruebas) {
    resultado <- simular_generacion_pregunta()
    
    if (resultado$exito) {
      exitos <- exitos + 1
      
      # Verificar que todos los diagramas son diferentes
      diagramas <- resultado$valores_diagramas
      son_diferentes <- asegurar_diagramas_diferentes(diagramas)
      
      expect_true(son_diferentes, 
                  info = "Se encontraron diagramas duplicados en las opciones")
    }
  }
  
  # Esperamos que al menos el 80% de las simulaciones sean exitosas
  tasa_exito <- exitos / n_pruebas
  expect_true(tasa_exito >= 0.8, 
              info = paste("Solo", tasa_exito * 100, "% de las simulaciones fueron exitosas"))
  
  cat("   No hay opciones de respuesta duplicadas\n")
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
  }
  
  cat("   No hay gráficas vacías\n")
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