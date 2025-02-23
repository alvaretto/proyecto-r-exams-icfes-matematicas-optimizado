# Prueba de diversidad de versiones
test_that("Prueba de diversidad de versiones", {
  # Generar y almacenar 1000 versiones
  versiones <- list()
  for(i in 1:1000) {
    datos_test <- generar_datos()
    versiones[[i]] <- digest::digest(datos_test)  # Crear hash de los datos
  }

  # Verificar al menos 300 versiones únicas
  n_versiones_unicas <- length(unique(versiones))
  expect_true(n_versiones_unicas >= 300,
              info = paste("Solo se generaron", n_versiones_unicas,
                          "versiones únicas. Se requieren al menos 300."))
})
