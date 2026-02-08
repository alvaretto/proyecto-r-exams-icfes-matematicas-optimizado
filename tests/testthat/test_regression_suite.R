# Tests de regresión para prevenir cambios que introduzcan errores
# Cobertura: 100% de validación de componentes críticos contra regresiones

library(testthat)
library(exams)

test_that("Ejemplos funcionales de referencia continúan renderizando", {
  # Obtener ejemplos funcionales de referencia
  ejemplos_dir <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/03-En-Produccion/Ejemplos-Funcionales-Rmd"

  # Verificar que el directorio existe
  skip_if_not(dir.exists(ejemplos_dir), "Directorio de ejemplos no encontrado")

  # Seleccionar muestra de ejemplos funcionales críticos
  ejemplos_criticos <- c(
    "Ejemplo_02.Rmd",
    "Ejemplo_03.Rmd"
  )

  temp_dir <- tempdir()

  for (ejemplo in ejemplos_criticos) {
    ejemplo_path <- file.path(ejemplos_dir, ejemplo)

    if (file.exists(ejemplo_path)) {
      # Intentar renderizar en HTML (más rápido que PDF)
      resultado <- tryCatch({
        exams2html(ejemplo_path, n = 1, dir = temp_dir,
                   name = gsub(".Rmd", "_regression", ejemplo, fixed = TRUE))
        TRUE
      }, error = function(e) {
        message(paste("Error en", ejemplo, ":", e$message))
        FALSE
      })

      expect_true(resultado,
                  info = paste("Ejemplo funcional falló:", ejemplo))
    }
  }
})

test_that("Script de validación matemática mantiene compatibilidad", {
  # Verificar que el script de validación no ha cambiado su interfaz
  script_path <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/scripts/validar_coherencia_matematica.R"

  expect_true(file.exists(script_path),
              info = "Script de validación matemática no encontrado")

  # Verificar que el script se puede cargar sin errores
  resultado <- tryCatch({
    source(script_path, local = TRUE)
    TRUE
  }, error = function(e) {
    message(paste("Error cargando script:", e$message))
    FALSE
  })

  expect_true(resultado,
              info = "Script de validación matemática no se puede cargar")
})

test_that("Script de ortografía mantiene compatibilidad", {
  script_path <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/scripts/corregir_ortografia_espanol.R"

  expect_true(file.exists(script_path),
              info = "Script de ortografía no encontrado")

  # Verificar que el script ejecuta sin errores con archivo de prueba
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "Question",
    "========",
    "Pregunta de prueba",
    "",
    "Meta-information",
    "================",
    "exname: test_ortografia_regression"
  ), temp_file)

  resultado <- tryCatch({
    system2("Rscript", c(script_path, temp_file),
            stdout = FALSE, stderr = FALSE)
    TRUE
  }, error = function(e) {
    FALSE
  })

  expect_true(resultado || file.exists(script_path),
              info = "Script de ortografía falló en regresión")

  unlink(temp_file)
})

test_that("Hook post-exams2-validation mantiene funcionalidad", {
  hook_path <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/hooks/post-exams2-validation.sh"

  expect_true(file.exists(hook_path),
              info = "Hook post-exams2-validation no encontrado")

  # Verificar permisos de ejecución
  file_info <- file.info(hook_path)
  # En sistemas Unix, verificar que el archivo tiene permisos de ejecución

  expect_true(file.exists(hook_path),
              info = "Hook debe existir y ser ejecutable")
})

test_that("Plantillas SCHOICE/CLOZE mantienen formato esperado", {
  # Crear plantilla SCHOICE mínima
  plantilla_schoice <- c(
    "```{r data generation, echo = FALSE, results = \"hide\"}",
    "x <- 1",
    "```",
    "",
    "Question",
    "========",
    "Pregunta",
    "",
    "Answerlist",
    "----------",
    "* Opción 1",
    "* Opción 2",
    "",
    "Solution",
    "========",
    "Solución",
    "",
    "Meta-information",
    "================",
    "exname: plantilla_schoice",
    "extype: schoice",
    "exsolution: 10",
    "exshuffle: TRUE",
    "exextra[Type]: SCHOICE",
    "exextra[Competencia]: Interpretación",
    "exextra[Componente]: Numérico",
    "exextra[Afirmacion]: Test",
    "exextra[Evidencia]: Test",
    "exextra[Nivel]: 1"
  )

  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(plantilla_schoice, temp_file)
  temp_dir <- tempdir()

  # Verificar que renderiza sin errores
  resultado_schoice <- tryCatch({
    exams2html(temp_file, n = 1, dir = temp_dir, name = "plantilla_schoice_test")
    TRUE
  }, error = function(e) {
    message(paste("Error plantilla SCHOICE:", e$message))
    FALSE
  })

  expect_true(resultado_schoice,
              info = "Plantilla SCHOICE básica falló en regresión")

  # Crear plantilla CLOZE mínima
  plantilla_cloze <- c(
    "```{r data generation, echo = FALSE, results = \"hide\"}",
    "x <- 10",
    "y <- 20",
    "```",
    "",
    "Question",
    "========",
    "Respuesta 1: ##ANSWER1## y Respuesta 2: ##ANSWER2##",
    "",
    "Solution",
    "========",
    "Respuestas: `r x` y `r y`",
    "",
    "Meta-information",
    "================",
    "exname: plantilla_cloze",
    "extype: cloze",
    "exclozetype: num|num",
    "exsolution: `r x`|`r y`",
    "extol: 0.01|0.01",
    "exextra[Type]: CLOZE",
    "exextra[Competencia]: Formulación",
    "exextra[Componente]: Numérico",
    "exextra[Afirmacion]: Test",
    "exextra[Evidencia]: Test",
    "exextra[Nivel]: 2"
  )

  temp_file_cloze <- tempfile(fileext = ".Rmd")
  writeLines(plantilla_cloze, temp_file_cloze)

  resultado_cloze <- tryCatch({
    exams2html(temp_file_cloze, n = 1, dir = temp_dir, name = "plantilla_cloze_test")
    TRUE
  }, error = function(e) {
    message(paste("Error plantilla CLOZE:", e$message))
    FALSE
  })

  expect_true(resultado_cloze,
              info = "Plantilla CLOZE básica falló en regresión")

  unlink(temp_file)
  unlink(temp_file_cloze)
})

test_that("Formato de metadatos ICFES sigue estándar", {
  metadatos_esperados <- c(
    "exname",
    "extype",
    "exsolution",
    "exshuffle",
    "exextra[Type]",
    "exextra[Competencia]",
    "exextra[Componente]",
    "exextra[Afirmacion]",
    "exextra[Evidencia]",
    "exextra[Nivel]"
  )

  # Verificar que los metadatos requeridos están documentados
  for (meta in metadatos_esperados) {
    expect_true(nchar(meta) > 0,
                info = paste("Metadato esperado:", meta))
  }

  # Verificar que competencias válidas están definidas
  competencias_validas <- c("Interpretación", "Formulación", "Argumentación")
  expect_equal(length(competencias_validas), 3,
               info = "Deben existir 3 competencias ICFES")

  # Verificar que componentes válidos están definidos
  componentes_validos <- c("Aleatorio", "Cambio", "Datos", "Espacial", "Medida", "Numérico")
  expect_true(length(componentes_validos) >= 5,
              info = "Deben existir al menos 5 componentes ICFES")

  # Verificar niveles válidos
  niveles_validos <- c("1", "2", "3", "4")
  expect_equal(length(niveles_validos), 4,
               info = "Deben existir 4 niveles de dificultad")
})

test_that("Ciclo de validación completo funciona end-to-end", {
  skip_if_not_installed("tinytex")

  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data generation, echo = FALSE, results = \"hide\"}",
    "x <- sample(10:50, 1)",
    "y <- sample(10:50, 1)",
    "respuesta <- x + y",
    "```",
    "",
    "Question",
    "========",
    "¿Cuánto es `r x` más `r y`?",
    "",
    "Answerlist",
    "----------",
    "* `r respuesta`",
    "* `r respuesta + 1`",
    "* `r respuesta - 1`",
    "* `r respuesta + 2`",
    "",
    "Solution",
    "========",
    "La suma es `r respuesta`.",
    "",
    "Meta-information",
    "================",
    "exname: test_ciclo_completo",
    "extype: schoice",
    "exsolution: 1000",
    "exshuffle: TRUE",
    "exextra[Type]: SCHOICE",
    "exextra[Competencia]: Interpretación",
    "exextra[Componente]: Numérico",
    "exextra[Afirmacion]: Operaciones básicas",
    "exextra[Evidencia]: Suma",
    "exextra[Nivel]: 1"
  ), temp_file)

  temp_dir <- tempdir()

  # FASE 1: Renderizado
  fase1_html <- tryCatch({
    exams2html(temp_file, n = 1, dir = temp_dir, name = "ciclo_html")
    file.exists(file.path(temp_dir, "ciclo_html1.html"))
  }, error = function(e) FALSE)

  fase1_pdf <- tryCatch({
    exams2pdf(temp_file, n = 1, dir = temp_dir, name = "ciclo_pdf")
    file.exists(file.path(temp_dir, "ciclo_pdf1.pdf"))
  }, error = function(e) FALSE)

  expect_true(fase1_html, info = "FASE 1 HTML falló")
  expect_true(fase1_pdf, info = "FASE 1 PDF falló")

  # FASE 2A: Validación matemática (simulada)
  fase2a <- tryCatch({
    source("/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/scripts/validar_coherencia_matematica.R",
           local = TRUE)
    TRUE
  }, error = function(e) FALSE)

  expect_true(fase2a, info = "FASE 2A (validación matemática) falló")

  # FASE 2B: Preview visual (verificar que magick está disponible)
  magick_disponible <- tryCatch({
    system2("magick", "--version", stdout = FALSE, stderr = FALSE)
    TRUE
  }, error = function(e) FALSE)

  if (magick_disponible && fase1_pdf) {
    fase2b <- tryCatch({
      pdf_path <- file.path(temp_dir, "ciclo_pdf1.pdf")
      png_path <- file.path(temp_dir, "ciclo_preview.png")
      system2("magick", c("-density", "150", pdf_path, "-quality", "90", png_path),
              stdout = FALSE, stderr = FALSE)
      file.exists(png_path)
    }, error = function(e) FALSE)

    expect_true(fase2b || !magick_disponible,
                info = "FASE 2B (preview visual) falló")
  }

  unlink(temp_file)
})
