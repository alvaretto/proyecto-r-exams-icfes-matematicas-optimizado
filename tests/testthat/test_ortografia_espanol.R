# Tests unitarios para corregir_ortografia_espanol.R
# Cobertura: 100% de funcionalidad del script de ortografía

library(testthat)

test_that("Corrección ortográfica detecta tildes faltantes", {
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "Question",
    "========",
    "Cual es el angulo mas grande?",  # Falta tilde en cuál, ángulo, más
    "",
    "Meta-information",
    "================",
    "exname: test_ortografia"
  ), temp_file)

  # Ejecutar corrección (modo verificación)
  result <- system2(
    "Rscript",
    c("/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/scripts/corregir_ortografia_espanol.R",
      temp_file),
    stdout = TRUE,
    stderr = TRUE
  )

  # Verificar que detecta errores
  expect_true(any(grepl("Cuál|ángulo|más", result, ignore.case = TRUE)))

  unlink(temp_file)
})

test_that("Corrección ortográfica NO modifica metadatos R-exams", {
  temp_file <- tempfile(fileext = ".Rmd")
  original_content <- c(
    "Question",
    "========",
    "Pregunta válida",
    "",
    "Meta-information",
    "================",
    "exname: nombre_sin_tildes",
    "exsection: Numerico-Variacional/Interpretacion",  # ASCII obligatorio
    "extype: schoice"
  )
  writeLines(original_content, temp_file)

  # Ejecutar corrección (modo fix)
  system2(
    "Rscript",
    c("/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/scripts/corregir_ortografia_espanol.R",
      temp_file, "--fix"),
    stdout = FALSE,
    stderr = FALSE
  )

  # Verificar que metadatos no se modificaron
  new_content <- readLines(temp_file)
  metadatos_originales <- grep("^(exname|exsection|extype)", original_content, value = TRUE)
  metadatos_nuevos <- grep("^(exname|exsection|extype)", new_content, value = TRUE)

  expect_equal(metadatos_originales, metadatos_nuevos)

  unlink(temp_file)
})

test_that("Corrección ortográfica NO modifica nombres de variables R", {
  temp_file <- tempfile(fileext = ".Rmd")
  original_content <- c(
    "```{r data generation, echo = FALSE, results = \"hide\"}",
    "angulo <- 45  # Variable sin tilde (correcto en R)",
    "solucion <- angulo * 2",
    "```",
    "",
    "Question",
    "========",
    "El ángulo es de `r angulo` grados.",  # Texto con tilde (correcto)
    "",
    "Meta-information",
    "================",
    "exname: test_variables"
  )
  writeLines(original_content, temp_file)

  # Ejecutar corrección (modo fix)
  system2(
    "Rscript",
    c("/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/scripts/corregir_ortografia_espanol.R",
      temp_file, "--fix"),
    stdout = FALSE,
    stderr = FALSE
  )

  # Verificar que variables NO se modificaron
  new_content <- readLines(temp_file)
  expect_true(any(grepl("angulo <-", new_content, fixed = TRUE)))  # Variable sin tilde
  expect_true(any(grepl("El ángulo", new_content, fixed = TRUE)))  # Texto con tilde

  unlink(temp_file)
})

test_that("Corrección ortográfica aplica correcciones automáticas", {
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "Question",
    "========",
    "Segun la grafica, cual es la funcion mas optima?"
  ), temp_file)

  # Ejecutar corrección (modo fix)
  system2(
    "Rscript",
    c("/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/scripts/corregir_ortografia_espanol.R",
      temp_file, "--fix"),
    stdout = FALSE,
    stderr = FALSE
  )

  # Verificar correcciones aplicadas
  new_content <- readLines(temp_file)
  texto <- paste(new_content, collapse = " ")

  expect_true(grepl("Según", texto))
  expect_true(grepl("gráfica", texto))
  # "cual" excluido del diccionario (ambiguo: interrogativo vs relativo)
  expect_true(grepl("función", texto))
  expect_true(grepl("más", texto))
  expect_true(grepl("óptima", texto))

  unlink(temp_file)
})

# =============================================================================
# Punto ciego del 2026-08-06: el corrector firmó "✓ No se encontraron errores"
# sobre un .Rmd que emitía al estudiante `formula` x14, `Si, porque` x11,
# `demas` x7 y `consumio` x4. Estos tests fijan el arreglo para que no se
# vuelva a perder. Ver cabecera de corregir_ortografia_espanol.R.
# =============================================================================

SCRIPT_ORTO <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/scripts/corregir_ortografia_espanol.R"

test_that("Caza las formas del punto ciego 2026-08-06 (inequívocas)", {
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "Question",
    "========",
    "El invitado consumio menos que los demas asistentes.",
    "",
    "* Si, porque se conoce el total.",
    "* No."
  ), temp_file)

  salida <- system2("Rscript", c(SCRIPT_ORTO, temp_file), stdout = TRUE, stderr = TRUE)
  txt <- paste(salida, collapse = "\n")

  expect_true(grepl("consumió", txt), info = "no detectó 'consumio'")
  expect_true(grepl("demás", txt), info = "no detectó 'demas'")
  expect_true(grepl("Sí,", txt), info = "no detectó 'Si,' (adverbio afirmativo)")
  expect_false(grepl("No se encontraron errores", txt),
               info = "declaró limpio un archivo con tildes faltantes")

  unlink(temp_file)
})

test_that("La regla morfológica cubre -cion/-sion/-xion fuera del diccionario", {
  # "situacion" y "reparticion" NUNCA estuvieron en el diccionario léxico:
  # el objetivo de la regla por sufijo es no depender de que alguien las añada.
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "Question",
    "========",
    "La situacion exige una reparticion proporcional y mucha presion.",
    "¿Cual es el resultado?"
  ), temp_file)

  system2("Rscript", c(SCRIPT_ORTO, temp_file, "--fix"), stdout = FALSE, stderr = FALSE)
  txt <- paste(readLines(temp_file, encoding = "UTF-8"), collapse = " ")

  expect_true(grepl("situación", txt))
  expect_true(grepl("repartición", txt))
  expect_true(grepl("presión", txt))
  expect_true(grepl("¿Cuál", txt), info = "no acentuó el interrogativo tras '¿'")

  unlink(temp_file)
})

test_that("Corrige también la forma en MAYÚSCULAS (si no, el hook rechaza para siempre)", {
  # La detección es case-insensitive pero la sustitución solo cubría minúscula y
  # Capitalizada: "OPCION" se reportaba en cada pasada sin corregirse nunca, y el
  # hook pre-commit rechazaba el archivo indefinidamente. Medido el 2026-08-06.
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "Question",
    "========",
    "# V2: OPCION CORRECTA = gráfico de barras",
    "La opcion correcta y la Opcion siguiente."
  ), temp_file)

  system2("Rscript", c(SCRIPT_ORTO, temp_file, "--fix"), stdout = FALSE, stderr = FALSE)
  txt <- paste(readLines(temp_file, encoding = "UTF-8"), collapse = " ")
  expect_true(grepl("OPCIÓN CORRECTA", txt), info = "no corrigió la forma en mayúsculas")
  expect_true(grepl("La opción correcta", txt))
  expect_true(grepl("la Opción siguiente", txt))

  # Y el archivo queda sin errores pendientes: sin esto el hook nunca deja pasar.
  salida <- paste(system2("Rscript", c(SCRIPT_ORTO, temp_file),
                          stdout = TRUE, stderr = TRUE), collapse = "\n")
  expect_false(grepl("ERRORES", salida))

  unlink(temp_file)
})

test_that("La regla morfológica no produce palabras con dos tildes", {
  # "Seleccionó" empareja hasta "-cion" si la regla no lleva el lookahead de
  # vocal acentuada: el resultado era "Selecciónó". Medido el 2026-08-06.
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "Question",
    "========",
    "Seleccionó la opcion equivocada y no la corrigió.",
    "La relación con la presión es directa."
  ), temp_file)

  system2("Rscript", c(SCRIPT_ORTO, temp_file, "--fix"), stdout = FALSE, stderr = FALSE)
  txt <- paste(readLines(temp_file, encoding = "UTF-8"), collapse = " ")

  expect_false(grepl("Selecciónó", txt), info = "produjo una palabra con dos tildes")
  expect_true(grepl("Seleccionó", txt), info = "alteró una palabra ya correcta")
  expect_true(grepl("corrigió", txt))
  expect_true(grepl("relación", txt))
  expect_true(grepl("presión", txt))
  expect_true(grepl("la opción equivocada", txt), info = "no corrigió lo que sí debía")

  unlink(temp_file)
})

test_that("Los casos ambiguos se reportan pero --fix NO los toca", {
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "Question",
    "========",
    "La formula no depende de n y seria distinta en otro caso."
  ), temp_file)

  salida <- system2("Rscript", c(SCRIPT_ORTO, temp_file, "--fix"), stdout = TRUE, stderr = TRUE)
  txt_salida <- paste(salida, collapse = "\n")
  txt_archivo <- paste(readLines(temp_file, encoding = "UTF-8"), collapse = " ")

  # Se reportan...
  expect_true(grepl("REVISION_MANUAL", txt_salida))
  expect_true(grepl("fórmula", txt_salida), info = "no sugirió la forma acentuada")
  # ...pero el archivo NO se modifica: 'formula' puede ser verbo y 'seria' adjetivo.
  expect_true(grepl("La formula", txt_archivo, fixed = TRUE))
  expect_true(grepl("seria distinta", txt_archivo, fixed = TRUE))
  # ...y no se declara limpio.
  expect_false(grepl("No se encontraron errores", txt_salida))

  unlink(temp_file)
})

test_that("NO acentúa identificadores R capitalizados dentro de un chunk", {
  # Bug medido el 2026-08-06 en un barrido masivo: `esta_en_string()` sondeaba
  # en minúsculas ("categoria") lo que la detección había encontrado sin
  # distinguir mayúsculas ("Categoria"). Al no encontrarlo tras enmascarar los
  # strings, concluía "estaba dentro de un string" y reescribía código:
  # `aes(fill = Categoria)` -> `aes(fill = Categoría)`.
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data_generation, echo = FALSE, results = \"hide\"}",
    "datos <- data.frame(Categoria = factor(cats), Frecuencia = vals)",
    "p <- ggplot(datos, aes(x = Categoria, y = Frecuencia, fill = Categoria))",
    "```",
    "",
    "Question",
    "========",
    "¿Qué categoria tiene mayor frecuencia?"
  ), temp_file)

  system2("Rscript", c(SCRIPT_ORTO, temp_file, "--fix"), stdout = FALSE, stderr = FALSE)
  txt <- readLines(temp_file, encoding = "UTF-8")

  expect_true(any(grepl("data.frame(Categoria = factor", txt, fixed = TRUE)),
              info = "acentuó el nombre de columna en la definición")
  expect_true(any(grepl("aes(x = Categoria, y = Frecuencia, fill = Categoria)", txt, fixed = TRUE)),
              info = "acentuó el identificador dentro de aes()")
  # …pero el texto visible al estudiante SÍ se corrige.
  expect_true(any(grepl("categoría tiene mayor frecuencia", txt)))

  unlink(temp_file)
})

test_that("NO acentúa nombres de archivo, ni siquiera dentro de strings", {
  # Un nombre de archivo acentuado rompe la búsqueda del archivo y, en la ruta
  # PDF, \includegraphics con UTF-8. Cubre el nombre literal y el construido
  # por interpolación.
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data_generation, echo = FALSE, results = \"hide\"}",
    "ggsave(\"grafica.png\", plot = p, width = 4)",
    "ruta <- paste0(\"opcion\", letra, \".png\")",
    "py_run_string(\"plt.savefig('operacion.png', dpi=150)\")",
    "```",
    "",
    "Question",
    "========",
    "Observa la grafica y la opcion elegida."
  ), temp_file)

  system2("Rscript", c(SCRIPT_ORTO, temp_file, "--fix"), stdout = FALSE, stderr = FALSE)
  txt <- readLines(temp_file, encoding = "UTF-8")

  expect_true(any(grepl('ggsave("grafica.png"', txt, fixed = TRUE)))
  expect_true(any(grepl('paste0("opcion", letra, ".png")', txt, fixed = TRUE)))
  expect_true(any(grepl("plt.savefig('operacion.png'", txt, fixed = TRUE)))
  # El texto visible sí se corrige.
  expect_true(any(grepl("Observa la gráfica y la opción elegida", txt)))

  unlink(temp_file)
})

test_that("REVISION_MANUAL no dispara el hook pre-commit (busca 'ERRORES')", {
  # El hook .claude/hooks/pre-commit-ortografia.sh bloquea el commit con
  # `grep -q "ERRORES"`. Un ambiguo pide juicio humano, no debe bloquear.
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c("Question", "========", "La formula es correcta."), temp_file)

  salida <- paste(system2("Rscript", c(SCRIPT_ORTO, temp_file),
                          stdout = TRUE, stderr = TRUE), collapse = "\n")

  expect_true(grepl("REVISION_MANUAL", salida))
  expect_false(grepl("ERRORES", salida),
               info = "un caso ambiguo estaría bloqueando el commit")

  unlink(temp_file)
})

test_that("Corrección ortográfica preserva código inline", {
  temp_file <- tempfile(fileext = ".Rmd")
  original_content <- c(
    "Question",
    "========",
    "El valor de x es `r solucion` según el cálculo."
  )
  writeLines(original_content, temp_file)

  # Ejecutar corrección (modo fix)
  system2(
    "Rscript",
    c("/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/scripts/corregir_ortografia_espanol.R",
      temp_file, "--fix"),
    stdout = FALSE,
    stderr = FALSE
  )

  # Verificar que código inline se preserva
  new_content <- readLines(temp_file)
  expect_true(any(grepl("`r solucion`", new_content, fixed = TRUE)))
  expect_true(any(grepl("según", new_content)))
  expect_true(any(grepl("cálculo", new_content)))

  unlink(temp_file)
})
