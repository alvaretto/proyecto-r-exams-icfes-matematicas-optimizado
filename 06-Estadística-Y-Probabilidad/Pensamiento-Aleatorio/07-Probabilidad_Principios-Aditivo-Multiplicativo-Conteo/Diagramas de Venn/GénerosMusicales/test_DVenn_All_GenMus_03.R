library(testthat)

context("Pruebas unitarias para DVenn_All_GenMus_03")

# Cargar funciones desde el archivo de funciones
source("functions_DVenn_All_GenMus_03.R")

test_that("Generación de datos válidos", {
  for (i in 1:10) {  # Reducido a 10 iteraciones para pruebas más rápidas
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
                                "Bachata", "Cumbia", "Rap", "Electrónica", 
                                "Tango", "Gospel")),
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

test_that("Cálculo de respuestas a preguntas", {
  # Crear un conjunto de datos de prueba
  data <- list(
    n_total = 42,
    n_A = 16, n_B = 15, n_C = 22,
    n_AB = 6, n_AC = 10, n_BC = 6, n_all = 2,
    n_only_A = 2, n_only_B = 5, n_only_C = 8,
    n_AB_only = 4, n_AC_only = 8, n_BC_only = 4,
    n_none = 9
  )
  
  # Verificar que las fórmulas dan los resultados esperados
  expect_equal(calcular_maximo_dos_generos(data), 40, 
               info = "Error en cálculo de máximo dos géneros")
  
  expect_equal(calcular_al_menos_un_genero(data), 33, 
               info = "Error en cálculo de al menos un género")
  
  expect_equal(calcular_A_o_B_no_ambos(data), 19, 
               info = "Error en cálculo de A o B pero no ambos")
  
  expect_equal(calcular_A_no_B(data), 10, 
               info = "Error en cálculo de A pero no B")
  
  expect_equal(calcular_maximo_tres_generos(data), 42, 
               info = "Error en cálculo de máximo tres géneros")
  
  expect_equal(calcular_al_menos_dos_generos(data), 18, 
               info = "Error en cálculo de al menos dos géneros")
  
  expect_equal(calcular_exactamente_dos_generos(data), 16, 
               info = "Error en cálculo de exactamente dos géneros")
  
  expect_equal(calcular_exactamente_un_genero(data), 15, 
               info = "Error en cálculo de exactamente un género")
})

test_that("Generación de código TikZ para diagrama de Venn", {
  data <- generate_valid_data()
  genres <- generate_random_genres()
  A <- genres[1]
  B <- genres[2]
  C <- genres[3]
  
  tikz_code <- generar_tikz_venn(data, A, B, C)
  
  expect_true(grepl("\\\\begin\\{tikzpicture\\}", tikz_code), 
              info = "Falta inicio de tikzpicture en código TikZ")
  expect_true(grepl("\\\\draw\\[thick\\] \\(-5,-5\\) rectangle \\(5,5\\)", tikz_code), 
              info = "Falta rectángulo para conjunto universal en código TikZ")
  expect_true(grepl("\\\\end\\{tikzpicture\\}", tikz_code), 
              info = "Falta fin de tikzpicture en código TikZ")
})