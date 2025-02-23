# Pruebas unitarias adicionales
test_that("Pruebas de validación de datos", {
  # Pruebas de estructura
  expect_type(datos$precio_base, "double")
  expect_type(datos$tasa_iva, "double")
  expect_type(datos$valor_iva, "double")
  expect_type(datos$precio_final, "double")

  # Pruebas de rango
  expect_true(datos$precio_base >= 50000 & datos$precio_base <= 100000)
  expect_equal(datos$tasa_iva, 0.19)

  # Pruebas de relación entre variables
  expect_equal(datos$valor_iva, datos$precio_base * datos$tasa_iva)
  expect_equal(datos$precio_final, datos$precio_base + datos$valor_iva)
})
