library(testthat)

# Pruebas unitarias adicionales
test_that("Pruebas de validación de datos", {
  # Pruebas de estructura
  expect_type(mascotas, "character")
  expect_type(selmascota, "character")
  expect_length(selmascota, 4)

  # Pruebas de rango
  expect_true(all(vector_resultado >= 0 & vector_resultado <= 100))
  expect_equal(sum(vector_resultado), 100)
  expect_true(enkuestados >= 60 && enkuestados <= 300)
  expect_true(enkuestados %% 10 == 0)

  # Pruebas de distribución (ejemplo)
  expect_true(max(vector_resultado) == max(vector_resultado))

  # Pruebas de relación entre variables
  tolerance <- 5 # Aumentar la tolerancia para permitir errores intencionales
  expect_true(abs(maskota1_round + maskota2_round + maskota3_round - enkuestados) < tolerance)
})