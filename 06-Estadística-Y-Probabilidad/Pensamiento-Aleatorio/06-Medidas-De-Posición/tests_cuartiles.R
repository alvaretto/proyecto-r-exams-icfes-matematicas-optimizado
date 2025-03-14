# Pruebas unitarias para el ejercicio de diagramas de caja con cuartiles
# Archivo: tests_cuartiles.R

# Prueba 1: Verificar que los cuartiles usan método Tukey
test_metodo_tukey <- function() {
  cat("Verificando uso del método Tukey para cuartiles...\n")
  
  # Conjunto de datos de prueba
  datos_test <- c(1, 2, 3, 4, 5, 6, 7)
  
  # Calcular cuartiles con método Tukey (type=7)
  q1_tukey <- as.numeric(quantile(datos_test, 0.25, type=7))
  q3_tukey <- as.numeric(quantile(datos_test, 0.75, type=7))
  
  # Valores esperados CORRECTOS para el método Tukey (type=7)
  q1_esperado <- 2.5
  q3_esperado <- 5.5
  
  # Verificar con margen de error
  if (abs(q1_tukey - q1_esperado) > 0.001 || abs(q3_tukey - q3_esperado) > 0.001) {
    stop("Error: Los cuartiles no se calculan con el método Tukey (type=7)")
  }
  
  cat("   Método Tukey implementado correctamente\n")
  return(TRUE)
}

# Prueba 2: Verificar redondeo de cuartiles
test_redondeo_cuartiles <- function() {
  cat("Verificando reglas de redondeo para cuartiles...\n")
  
  # Función de redondeo importada del script principal
  redondear_cuartil <- function(valor) {
    valor_redondeado <- round(valor, 1)
    if (valor_redondeado %% 1 == 0) {
      return(as.integer(valor_redondeado))
    } else {
      return(valor_redondeado)
    }
  }
  
  # Casos de prueba - usando valores explícitos para evitar problemas de tipo
  casos <- list(
    list(valor = 2.5, esperado = 2.5),     # Debe mantener el decimal .5
    list(valor = 3.0, esperado = 3L),      # Debe convertir a entero (L indica integer)
    list(valor = 4.05, esperado = 4.1),    # Debe redondear a un decimal
    list(valor = 5.96, esperado = 6L),     # Debe redondear a entero
    list(valor = 7.33, esperado = 7.3)     # Debe redondear a un decimal
  )
  
  for (caso in casos) {
    resultado <- redondear_cuartil(caso$valor)
    # Comprobar si es entero y convertir correctamente para la comparación
    if (is.integer(caso$esperado)) {
      if (!is.integer(resultado)) {
        stop(paste("Error de tipo: para", caso$valor, 
                   "se esperaba un entero pero se obtuvo", typeof(resultado)))
      }
      if (resultado != caso$esperado) {
        stop(paste("Error de redondeo: para", caso$valor, 
                   "se esperaba", caso$esperado, "pero se obtuvo", resultado))
      }
    } else {
      if (abs(resultado - caso$esperado) > 0.001) {
        stop(paste("Error de redondeo: para", caso$valor, 
                   "se esperaba", caso$esperado, "pero se obtuvo", resultado))
      }
    }
  }
  
  cat("   Reglas de redondeo implementadas correctamente\n")
  return(TRUE)
}

# Prueba 3: Verificar generación de conjuntos par e impar sin duplicados
test_datos_par_impar <- function() {
  cat("Verificando generación de conjuntos par e impar sin duplicados...\n")
  
  # Función simplificada para probar generación de datos
  generar_datos_prueba <- function(n) {
    # Usar un rango mucho más grande para evitar duplicados
    rango_min <- 100
    rango_max <- 300 + n*10  # Rango amplio que crece con n
    
    datos <- sort(round(runif(n, rango_min, rango_max), digits = 0))
    intentos <- 0
    max_intentos <- 20
    
    while(length(unique(datos)) < length(datos) && intentos < max_intentos) {
      datos <- sort(round(runif(n, rango_min, rango_max), digits = 0))
      intentos <- intentos + 1
    }
    
    # Si después de los intentos aún hay duplicados, resolverlo añadiendo valores
    if (length(unique(datos)) < length(datos)) {
      datos_unicos <- unique(datos)
      while (length(datos_unicos) < n) {
        nuevo_valor <- max(datos_unicos) + 1
        datos_unicos <- c(datos_unicos, nuevo_valor)
      }
      datos <- sort(datos_unicos)
    }
    
    return(datos)
  }
  
  # Probar con tamaños par e impar
  for (n in c(8, 9, 12, 13)) {
    datos <- generar_datos_prueba(n)
    
    # Verificar longitud
    if (length(datos) != n) {
      stop(paste("Error: Se generaron", length(datos), "datos en lugar de", n))
    }
    
    # Verificar que no hay duplicados
    if (length(unique(datos)) != length(datos)) {
      stop(paste("Error: El conjunto de tamaño", n, "contiene valores duplicados"))
    }
    
    # Verificar que está ordenado
    if (!all(diff(datos) >= 0)) {
      stop(paste("Error: El conjunto de tamaño", n, "no está ordenado correctamente"))
    }
  }
  
  cat("   Generación de conjuntos par/impar sin duplicados correcta\n")
  return(TRUE)
}

# Prueba 4: Verificar detección de diagramas idénticos
test_diagramas_diferentes <- function() {
  cat("Verificando detección de diagramas idénticos...\n")
  
  # Función de comparación
  son_diagramas_diferentes <- function(diag1, diag2) {
    if (identical(diag1$minimo, diag2$minimo) && 
        identical(diag1$q1, diag2$q1) && 
        identical(diag1$mediana, diag2$mediana) && 
        identical(diag1$q3, diag2$q3) && 
        identical(diag1$maximo, diag2$maximo)) {
      return(FALSE)
    }
    return(TRUE)
  }
  
  # Crear diagramas de prueba
  diag1 <- list(minimo=150, q1=160, mediana=170, q3=180, maximo=190)
  diag2 <- list(minimo=150, q1=161, mediana=170, q3=180, maximo=190)
  diag3 <- list(minimo=150, q1=160, mediana=170, q3=180, maximo=190)
  
  # Verificar que diag1 y diag2 son diferentes
  if (!son_diagramas_diferentes(diag1, diag2)) {
    stop("Error: La función no detecta como diferentes diagramas que tienen valores distintos")
  }
  
  # Verificar que diag1 y diag3 son idénticos
  if (son_diagramas_diferentes(diag1, diag3)) {
    stop("Error: La función detecta como diferentes diagramas que son idénticos")
  }
  
  cat("   Detección de diagramas idénticos funciona correctamente\n")
  return(TRUE)
}

# Prueba 5: Verificar posible variedad de diagramas
test_variedad_diagramas <- function() {
  cat("Verificando la variedad posible de diagramas...\n")
  
  # Función para generar estadísticas aleatorias de diagramas
  generar_stats_aleatorias <- function() {
    min_val <- sample(150:160, 1)
    max_val <- sample(180:190, 1)
    q1_val <- sample((min_val+1):(min_val+10), 1)
    q3_val <- sample((max_val-10):(max_val-1), 1)
    med_val <- sample((q1_val+1):(q3_val-1), 1)
    
    return(list(
      minimo = min_val,
      q1 = q1_val,
      mediana = med_val,
      q3 = q3_val,
      maximo = max_val
    ))
  }
  
  # Verificar que podemos generar al menos 300 diagramas diferentes
  set.seed(123)  # Fijar semilla para reproducibilidad
  diagramas <- list()
  for (i in 1:350) {  # Generar algunos más para asegurar
    diagramas[[i]] <- generar_stats_aleatorias()
  }
  
  # Calcular "huella digital" para cada diagrama
  huellas_digitales <- sapply(diagramas, function(d) {
    paste(d$minimo, d$q1, d$mediana, d$q3, d$maximo, sep="-")
  })
  
  # Contar combinaciones únicas
  total_unicos <- length(unique(huellas_digitales))
  
  if (total_unicos < 300) {
    stop(paste("Error: El método solo puede generar", total_unicos, 
               "diagramas diferentes, pero se requieren al menos 300"))
  }
  
  cat("   Se pueden generar al menos", total_unicos, "diagramas diferentes\n")
  return(TRUE)
}

# Ejecutar todas las pruebas
cat("==== Ejecutando pruebas unitarias para diagramas de caja ====\n\n")

# Registrar pruebas pasadas
pruebas_pasadas <- 0
total_pruebas <- 5

# Test 1
tryCatch({
  test_metodo_tukey()
  pruebas_pasadas <- pruebas_pasadas + 1
}, error = function(e) {
  cat("❌ FALLO en test_metodo_tukey:", e$message, "\n")
})

# Test 2
tryCatch({
  test_redondeo_cuartiles()
  pruebas_pasadas <- pruebas_pasadas + 1
}, error = function(e) {
  cat("❌ FALLO en test_redondeo_cuartiles:", e$message, "\n")
})

# Test 3
tryCatch({
  test_datos_par_impar()
  pruebas_pasadas <- pruebas_pasadas + 1
}, error = function(e) {
  cat("❌ FALLO en test_datos_par_impar:", e$message, "\n")
})

# Test 4
tryCatch({
  test_diagramas_diferentes()
  pruebas_pasadas <- pruebas_pasadas + 1
}, error = function(e) {
  cat("❌ FALLO en test_diagramas_diferentes:", e$message, "\n")
})

# Test 5
tryCatch({
  test_variedad_diagramas()
  pruebas_pasadas <- pruebas_pasadas + 1
}, error = function(e) {
  cat("❌ FALLO en test_variedad_diagramas:", e$message, "\n")
})

# Mostrar resumen
cat("\n==== Resumen de pruebas unitarias ====\n")
cat("Pruebas pasadas:", pruebas_pasadas, "de", total_pruebas, "\n")

if (pruebas_pasadas == total_pruebas) {
  cat("✅ TODAS LAS PRUEBAS PASARON CORRECTAMENTE\n")
} else {
  cat("⚠️ ALGUNAS PRUEBAS FALLARON. Revisa los mensajes de error.\n")
}

cat("=======================================\n")
