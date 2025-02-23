library(testthat)

context("Pruebas unitarias para DVenn_All_GenMus_02")

# Cargar funciones desde el archivo de funciones
source("functions_DVenn_All_GenMus_02.R")

test_that("Generación de datos válidos", {
  for (i in 1:400) {
    data <- generate_valid_data()
    
    # Verificar que todos los valores sean no negativos
    expect_true(all(unlist(data) >= 0), 
                info = sprintf("Iteración %d: Valores negativos generados", i))
    
    # Verificar consistencia de totales
    total_calculado <- with(data, n_A + n_B + n_C - n_AB - n_AC - n_BC + n_all + n_none)
    expect_equal(total_calculado, data$n_total, 
                 info = sprintf("Iteración %d: Total inconsistente", i))
  }
})

test_that("Generación de géneros musicales", {
  genres <- generate_random_genres()
  expect_true(length(unique(genres)) == 3, 
              info = "No se generaron 3 géneros musicales únicos")
  expect_true(all(genres %in% c("Vallenato", "Salsa", "Merengue", "Reggaeton", 
                                "Bachata", "Cumbia", "Rap", "Electrónica")),
              info = "Géneros musicales no válidos generados")
})

test_that("Generación de texto consistente", {
  data <- generate_valid_data()
  genres <- generate_random_genres()
  A <- genres[1]
  B <- genres[2]
  C <- genres[3]
  
  texto <- generate_random_text(data, A, B, C)
  
  expect_true(grepl(A, texto), info = "Falta género A en texto generado")
  expect_true(grepl(B, texto), info = "Falta género B en texto generado")
  expect_true(grepl(C, texto), info = "Falta género C en texto generado")
})