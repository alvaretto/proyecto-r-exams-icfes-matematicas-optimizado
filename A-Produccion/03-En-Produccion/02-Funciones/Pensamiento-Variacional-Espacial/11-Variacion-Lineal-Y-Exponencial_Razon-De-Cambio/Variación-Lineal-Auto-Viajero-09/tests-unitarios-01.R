library(testthat)

# Función para generar datos aleatorios
generar_datos <- function() {
  duracion <- sample(2:4, 1)
  rapidez <- sample(30:100, 1)
  distancia <- rapidez * duracion
  pos_inicial <- sample(10:50, 1)
  pos_final <- pos_inicial + distancia
  hora_inicial <- sample(6:10, 1)
  hora_final <- hora_inicial + duracion
  
  list(
    duracion = duracion,
    rapidez = rapidez,
    distancia = distancia,
    pos_inicial = pos_inicial,
    pos_final = pos_final,
    hora_inicial = hora_inicial,
    hora_final = hora_final
  )
}

# Pruebas unitarias
test_that("Todos los valores son enteros", {
  datos <- generar_datos()
  expect_true(all(sapply(datos, function(x) is.integer(x) || all(x == as.integer(x)))))
})

test_that("Distancia es múltiplo exacto de la duración", {
  datos <- generar_datos()
  expect_equal(datos$distancia %% datos$duracion, 0)
})

test_that("Valores están dentro de los rangos especificados", {
  datos <- generar_datos()
  expect_true(datos$duracion %in% 2:4)
  expect_true(datos$rapidez %in% 30:100)
  expect_true(datos$pos_inicial %in% 10:50)
  expect_true(datos$hora_inicial %in% 6:10)
})

# Eliminamos la ejecución directa de pruebas
# Las pruebas ahora deben ejecutarse desde la línea de comandos usando:
# Rscript -e "test_dir('tests', reporter = 'summary')"