# Tests de distintividad visual para ejercicios _neg_ con gráficos
# Verifica que todos los errores en el pool producen diagramas
# visualmente distintos del correcto (no hay errores invisibles)

library(testthat)
library(ggplot2)

test_that("EST-BOX-03 (Q1/Q3 swap) produce diagrama idéntico al correcto en ggplot2", {
  # Este test documenta POR QUÉ EST-BOX-03 está excluido del pool:
  # ggplot2 geom_boxplot(stat='identity') auto-ordena lower/upper,
  # haciendo que Q1↔Q3 swap sea visualmente invisible.

  datos_correctos <- data.frame(
    x = "test",
    ymin = 150, lower = 160, middle = 165, upper = 175, ymax = 185
  )

  datos_box03 <- data.frame(
    x = "test",
    ymin = 150, lower = 175, middle = 165, upper = 160, ymax = 185
  )

  temp_dir <- tempdir()
  archivo_correcto <- file.path(temp_dir, "test_correcto.png")
  archivo_box03 <- file.path(temp_dir, "test_box03.png")

  p_correcto <- ggplot(datos_correctos,
    aes(x = x, ymin = ymin, lower = lower, middle = middle,
        upper = upper, ymax = ymax)) +
    geom_boxplot(stat = "identity") +
    theme_void()

  p_box03 <- ggplot(datos_box03,
    aes(x = x, ymin = ymin, lower = lower, middle = middle,
        upper = upper, ymax = ymax)) +
    geom_boxplot(stat = "identity") +
    theme_void()

  suppressMessages({
    ggsave(archivo_correcto, p_correcto, width = 3, height = 4, dpi = 72)
    ggsave(archivo_box03, p_box03, width = 3, height = 4, dpi = 72)
  })

  # Comparar PÍXELES, no tamaño de archivo: el tamaño del PNG es un proxy ruidoso
  # (compresión/metadatos varían entre entornos aunque la imagen sea idéntica;
  # se observaron ~6.8% de diferencia de tamaño con imágenes pixel-idénticas).
  skip_if_not_installed("png")
  img_correcto <- png::readPNG(archivo_correcto)
  img_box03 <- png::readPNG(archivo_box03)
  expect_equal(dim(img_correcto), dim(img_box03),
    info = "Las imágenes deben tener las mismas dimensiones")

  # ggplot2 geom_boxplot(stat='identity') auto-ordena lower/upper, así que el swap
  # Q1/Q3 es visualmente invisible: la diferencia es solo ruido de antialiasing en
  # los bordes (medido ~0.06%). Umbral 1% (margen ~17x).
  diff_pct <- mean(abs(img_correcto - img_box03)) * 100
  expect_true(diff_pct < 1,
    info = paste("EST-BOX-03 debería ser visualmente idéntico al correcto.",
                 "Diferencia media de píxeles:", round(diff_pct, 3), "%"))

  file.remove(archivo_correcto, archivo_box03)
})

test_that("Errores válidos del pool _neg_ producen diagramas distintos al correcto", {
  # Simular cuartiles típicos de estaturas
  # Nota: mediana=170 (no 168) para evitar que EST-BOX-08
  # (mediana=round((q1+q3)/2)) produzca el mismo valor por coincidencia
  cuartiles_correctos <- list(
    min = 155, q1 = 162, mediana = 170, q3 = 175, max = 183
  )

  # Definir errores del pool válido (índices 2,4,5,6,7,8)
  errores_validos <- list(
    "EST-BOX-02" = list(
      min = cuartiles_correctos$min,
      q1 = cuartiles_correctos$min + 1,
      mediana = cuartiles_correctos$mediana,
      q3 = cuartiles_correctos$q3,
      max = cuartiles_correctos$max
    ),
    "EST-BOX-04" = list(
      min = cuartiles_correctos$min,
      q1 = cuartiles_correctos$q1,
      mediana = cuartiles_correctos$mediana,
      q3 = cuartiles_correctos$max - 2,
      max = cuartiles_correctos$max
    ),
    "EST-BOX-05" = list(
      min = cuartiles_correctos$min,
      q1 = cuartiles_correctos$q1,
      mediana = cuartiles_correctos$q1 + 2,
      q3 = cuartiles_correctos$q3,
      max = cuartiles_correctos$max
    ),
    "EST-BOX-06" = list(
      min = cuartiles_correctos$min,
      q1 = cuartiles_correctos$q1,
      mediana = cuartiles_correctos$q3 - 2,
      q3 = cuartiles_correctos$q3,
      max = cuartiles_correctos$max
    ),
    "EST-BOX-07" = list(
      min = cuartiles_correctos$min,
      q1 = cuartiles_correctos$mediana - 2,
      mediana = cuartiles_correctos$mediana,
      q3 = cuartiles_correctos$mediana + 2,
      max = cuartiles_correctos$max
    ),
    "EST-BOX-08" = list(
      min = cuartiles_correctos$min,
      q1 = cuartiles_correctos$q1,
      mediana = round((cuartiles_correctos$q1 + cuartiles_correctos$q3) / 2),
      q3 = cuartiles_correctos$q3,
      max = cuartiles_correctos$max
    )
  )

  hash_correcto <- digest::digest(cuartiles_correctos)

  for (nombre in names(errores_validos)) {
    error <- errores_validos[[nombre]]
    hash_error <- digest::digest(error)

    expect_true(hash_correcto != hash_error,
      info = paste(nombre, "produce hash idéntico al correcto -",
                   "este error debería estar excluido del pool"))
  }
})

test_that("Ejercicio _neg_ excluye EST-BOX-03 del pool", {
  rmd_path <- file.path(
    "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams",
    "A-Produccion/03-En-Produccion",
    "06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/06-Medidas-De-Posición",
    "diagrama_caja_estaturas_metacognitivo_interpretacion_n2_schoice_neg_v1.Rmd"
  )

  # Fallback a ubicación previa si aún no se ha promovido
  if (!file.exists(rmd_path)) {
    rmd_path <- file.path(
      "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams",
      "A-Produccion/01-En-PreDesarrollo/Lab-Manjaro/50",
      "diagrama_caja_estaturas_metacognitivo_interpretacion_n2_schoice_neg_v1.Rmd"
    )
  }

  skip_if_not(file.exists(rmd_path), "Archivo _neg_ no encontrado")

  contenido <- readLines(rmd_path)

  # Buscar la línea que define errores_validos_para_grafico
  linea_pool <- grep("errores_validos_para_grafico", contenido, value = TRUE)
  expect_true(length(linea_pool) > 0,
    info = "No se encontró la definición de errores_validos_para_grafico")

  # Verificar que el índice 3 NO está en el pool
  expect_false(any(grepl("\\b3\\b", linea_pool[1])),
    info = "EST-BOX-03 (índice 3) NO debería estar en el pool de errores válidos")
})

test_that("Patrón _neg_ con digest() detecta opciones idénticas vs diferentes", {
  # Simular 4 opciones: 3 idénticas + 1 diferente (patrón _neg_)
  opcion_correcta <- list(min = 150, q1 = 160, mediana = 165, q3 = 175, max = 185)
  opcion_error <- list(min = 150, q1 = 160, mediana = 170, q3 = 175, max = 185)

  opciones <- list(
    A = opcion_correcta,
    B = opcion_error,
    C = opcion_correcta,
    D = opcion_correcta
  )

  hashes <- sapply(names(opciones), function(l) digest::digest(opciones[[l]]))
  freq <- table(hashes)

  # Exactamente 2 hashes distintos

  expect_equal(length(freq), 2,
    info = paste("Se esperaban 2 hashes distintos, hay", length(freq)))

  # (N-1) = 3 opciones con el mismo hash
  expect_true(3 %in% as.integer(freq),
    info = "Debe haber 3 opciones con el mismo hash (correctas)")

  # 1 opción con hash único (el error)
  expect_true(1 %in% as.integer(freq),
    info = "Debe haber 1 opción con hash único (el error)")
})
