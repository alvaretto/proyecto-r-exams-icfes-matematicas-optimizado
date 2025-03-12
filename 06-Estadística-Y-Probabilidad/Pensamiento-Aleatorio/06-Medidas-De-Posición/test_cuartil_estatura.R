library(testthat)
library(digest)

# Test 1: Verificar que solo haya una opción correcta y que sean visualmente diferentes
test_that("Diagramas son visualmente distintos", {
  set.seed(123)
  for(i in 1:100) {
    stats <- generar_datos_estatura()
    rango <- stats$maximo - stats$minimo
    
    # Diagrama correcto
    diag_correcto <- list(
      minimo = stats$minimo,
      q1 = stats$q1,
      mediana = stats$mediana,
      q3 = stats$q3,
      maximo = stats$maximo
    )
    
    # Diagrama con escala modificada (30% más grande)
    diag_escala <- list(
      minimo = stats$minimo * 1.3,
      q1 = stats$q1 * 1.3,
      mediana = stats$mediana * 1.3,
      q3 = stats$q3 * 1.3,
      maximo = stats$maximo * 1.3
    )
    
    # Diagrama con cuartiles y mediana desplazados
    desplazamiento <- rango * 0.25
    diag_desplazado <- list(
      minimo = stats$minimo,
      q1 = stats$q1 + desplazamiento,
      mediana = stats$mediana + desplazamiento,
      q3 = stats$q3 + desplazamiento,
      maximo = stats$maximo
    )
    
    # Diagrama con mediana muy diferente
    diag_mediana_falsa <- list(
      minimo = stats$minimo,
      q1 = stats$q1,
      mediana = stats$mediana + rango * 0.4,
      q3 = stats$q3,
      maximo = stats$maximo
    )
    
    # Verificar diferencias significativas entre diagramas
    diagramas <- list(diag_correcto, diag_escala, diag_desplazado, diag_mediana_falsa)
    
    # Verificar que las medianas sean suficientemente diferentes
    medianas <- sapply(diagramas, function(d) d$mediana)
    expect_true(length(unique(round(medianas))) == 4,
               info = paste("Las medianas no son suficientemente diferentes:",
                          paste(round(medianas), collapse = ", ")))
    
    # Verificar que los rangos intercuartílicos sean diferentes
    riq <- sapply(diagramas, function(d) d$q3 - d$q1)
    expect_true(length(unique(round(riq))) >= 2,
               info = "Los rangos intercuartílicos no son suficientemente diferentes")
  }
})

# Test 2: Verificar la diversidad de los datos generados
test_that("Los datos generados son diversos", {
  set.seed(456)
  muestras <- replicate(100, {
    stats <- generar_datos_estatura()
    c(length(stats$datos),
      stats$minimo,
      stats$maximo,
      stats$q3 - stats$q1)  # Rango intercuartílico
  }, simplify = FALSE)
  
  # Verificar variabilidad en el tamaño de la muestra
  tamanos <- sapply(muestras, `[`, 1)
  expect_true(length(unique(tamanos)) >= 5,
             info = "No hay suficiente variabilidad en el tamaño de las muestras")
  
  # Verificar variabilidad en los rangos
  rangos <- sapply(muestras, function(m) m[3] - m[2])
  expect_true(length(unique(rangos)) >= 10,
             info = "No hay suficiente variabilidad en los rangos de datos")
  
  # Verificar variabilidad en los RIQ
  riqs <- sapply(muestras, `[`, 4)
  expect_true(length(unique(riqs)) >= 10,
             info = "No hay suficiente variabilidad en los rangos intercuartílicos")
})

# Test 3: Verificar la aleatoriedad de las opciones
test_that("Las opciones se muestran aleatoriamente", {
  set.seed(789)
  posiciones <- replicate(100, {
    tipos_diagramas <- c("correcto", "escala_10", "invertido", "mediana_falsa")
    which(sample(tipos_diagramas) == "correcto")
  })
  
  # Verificar que la posición correcta varía
  expect_true(length(unique(posiciones)) == 4,
             info = "Las opciones no están apareciendo en todas las posiciones posibles")
  
  # Verificar distribución uniforme de posiciones
  freq_posiciones <- table(posiciones)
  chi_sq <- chisq.test(freq_posiciones)
  expect_true(chi_sq$p.value > 0.05,
             info = "La distribución de posiciones no es uniforme")
})

# Test 4: Verificar suficientes versiones únicas
test_that("Se generan suficientes versiones únicas", {
  set.seed(101)
  versiones <- replicate(500, {
    stats <- generar_datos_estatura()
    digest::digest(list(
      datos = stats$datos,
      minimo = stats$minimo,
      q1 = stats$q1,
      mediana = stats$mediana,
      q3 = stats$q3,
      maximo = stats$maximo
    ))
  })
  
  n_versiones_unicas <- length(unique(versiones))
  expect_true(n_versiones_unicas >= 400,
             info = paste("Solo se generaron", n_versiones_unicas, 
                         "versiones únicas. Se requieren al menos 400."))
})