# Tests unitarios para validar_coherencia_matematica.R
# Cobertura: 100% de funcionalidad del script de validación matemática

library(testthat)
library(exams)

# Cargar funciones UNA vez (source() es seguro gracias al guard sys.nframe())
source("/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/scripts/validar_coherencia_matematica.R")

test_that("Validación matemática detecta errores en chunks R", {
  # Crear archivo .Rmd temporal con chunk que genera NaN
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data generation, echo = FALSE, results = \"hide\"}",
    "x <- sqrt(-1)  # Genera NaN",
    "```",
    "",
    "Question",
    "========",
    "Test question",
    "",
    "Solution",
    "========",
    "Test solution",
    "",
    "Meta-information",
    "================",
    "exname: test_error",
    "extype: schoice",
    "exsolution: 10000",
    "exshuffle: TRUE"
  ), temp_file)

  result <- validar_coherencia_matematica(temp_file)

  # Verificar que detecta error (NaN en variable x)
  expect_false(result$aprobado)
  expect_true(length(result$errores) > 0)

  unlink(temp_file)
})

test_that("Validación matemática acepta archivo SCHOICE válido", {
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data generation, echo = FALSE, results = \"hide\"}",
    "x <- sample(1:10, 1)",
    "respuesta <- x + 5",
    "opciones <- c(respuesta, respuesta + 1, respuesta - 1, respuesta + 2)",
    "```",
    "",
    "Question",
    "========",
    "Cuanto es `r x` + 5?",
    "",
    "Answerlist",
    "----------",
    "* `r opciones[1]`",
    "* `r opciones[2]`",
    "* `r opciones[3]`",
    "* `r opciones[4]`",
    "",
    "Solution",
    "========",
    "La respuesta es `r respuesta`.",
    "",
    "Meta-information",
    "================",
    "exname: test_valido",
    "extype: schoice",
    "exsolution: 1000",
    "exshuffle: TRUE",
    "exextra[Type]: SCHOICE",
    "exextra[Competencia]: Interpretacion",
    "exextra[Componente]: Numerico",
    "exextra[Afirmacion]: Realiza calculos",
    "exextra[Evidencia]: Suma de numeros",
    "exextra[Nivel]: 1"
  ), temp_file)

  result <- validar_coherencia_matematica(temp_file)

  # Verificar que aprueba (archivo SCHOICE válido con metadatos completos)
  expect_true(result$aprobado)

  unlink(temp_file)
})

test_that("Validación detecta exshuffle = FALSE en opciones de texto", {
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data generation, echo = FALSE, results = \"hide\"}",
    "x <- 5",
    "```",
    "",
    "Question",
    "========",
    "Test",
    "",
    "Answerlist",
    "----------",
    "* Opcion 1",
    "* Opcion 2",
    "",
    "Solution",
    "========",
    "Test",
    "",
    "Meta-information",
    "================",
    "exname: test_shuffle",
    "extype: schoice",
    "exsolution: 10",
    "exshuffle: FALSE"
  ), temp_file)

  result <- validar_coherencia_matematica(temp_file)

  expect_false(result$aprobado)
  expect_true(any(grepl("shuffle", result$errores, ignore.case = TRUE)))

  unlink(temp_file)
})

test_that("Validación acepta exshuffle = FALSE en SCHOICE con opciones gráficas PNG", {
  # Excepción: SCHOICE con opciones gráficas individuales (diagrama_*.png)
  # usa sample() interno + exshuffle:FALSE porque TRUE rompería la referencia
  # a letra_correcta en Solution. Ver .claude/rules/graficos-como-opciones.md
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data generation, echo = FALSE, results = \"hide\"}",
    "x <- 5",
    "```",
    "",
    "Question",
    "========",
    "Test",
    "",
    "Answerlist",
    "----------",
    "* ![](diagrama_a.png){width=60%}",
    "* ![](diagrama_b.png){width=60%}",
    "",
    "Solution",
    "========",
    "Test",
    "",
    "Meta-information",
    "================",
    "exname: test_shuffle_graficos",
    "extype: schoice",
    "exsolution: 10",
    "exshuffle: FALSE"
  ), temp_file)

  result <- validar_coherencia_matematica(temp_file)

  # No debe reportar error de exshuffle porque tiene opciones gráficas PNG
  expect_false(any(grepl("exshuffle", result$errores, ignore.case = TRUE)),
    info = "exshuffle:FALSE debe ser aceptado en SCHOICE con opciones gráficas PNG")

  unlink(temp_file)
})

# La excepción de exshuffle = FALSE (regla #4) debe sobrevivir al texto
# alternativo de las opciones. Un test por forma de alt: con las dos formas en
# el mismo archivo, any() dejaba pasar la que fallaba.
rmd_opciones_graficas <- function(lineas_answerlist) {
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data generation, echo = FALSE, results = \"hide\"}",
    "alt_op <- c(\"Gráfica de barras agrupadas\", \"Gráfica de barras apiladas\")",
    "```",
    "",
    "Question",
    "========",
    "Test",
    "",
    "Answerlist",
    "----------",
    lineas_answerlist,
    "",
    "Solution",
    "========",
    "Test",
    "",
    "Meta-information",
    "================",
    "exname: test_shuffle_graficos_alt",
    "extype: schoice",
    "exsolution: 10",
    "exshuffle: FALSE"
  ), temp_file)
  temp_file
}

test_that("Excepción de exshuffle con alt calculado en línea (`r alt_op[i]`)", {
  temp_file <- rmd_opciones_graficas(c(
    "* ![`r alt_op[1]`](diagrama_a.png){width=60%}&#8203;",
    "* ![`r alt_op[2]`](diagrama_b.png){width=60%}&#8203;"
  ))
  result <- validar_coherencia_matematica(temp_file)
  expect_false(any(grepl("exshuffle", result$errores, ignore.case = TRUE)),
    info = "el alt `r alt_op[i]` no debe desactivar la excepción de la regla #4")
  unlink(temp_file)
})

test_that("Excepción de exshuffle con alt literal", {
  temp_file <- rmd_opciones_graficas(c(
    "* ![Gráfica de barras agrupadas](diagrama_a.png){width=60%}&#8203;",
    "* ![Gráfica de barras apiladas](diagrama_b.png){width=60%}&#8203;"
  ))
  result <- validar_coherencia_matematica(temp_file)
  expect_false(any(grepl("exshuffle", result$errores, ignore.case = TRUE)),
    info = "el alt literal no debe desactivar la excepción de la regla #4")
  unlink(temp_file)
})

test_that("Una imagen diagrama_* que no es opción no activa la excepción de exshuffle", {
  # Control negativo: SCHOICE de TEXTO con exshuffle: FALSE y una figura de contexto
  # en el enunciado. Debe seguir dando ERR_C4; mata a los mutantes que aceptan
  # cualquier imagen o cualquier "diagrama_".
  temp_file <- rmd_opciones_graficas(c("* Opción 1", "* Opción 2"))
  lineas <- readLines(temp_file, encoding = "UTF-8")
  i <- which(lineas == "Test")[1]
  lineas <- append(lineas, "![Mapa del barrio](diagrama_contexto.png){width=60%}&#8203;", after = i)
  writeLines(lineas, temp_file)
  result <- validar_coherencia_matematica(temp_file)
  expect_true(any(grepl("exshuffle", result$errores, ignore.case = TRUE)),
    info = "una figura de contexto no convierte un SCHOICE de texto en uno de opciones gráficas")
  unlink(temp_file)
})

# Rutas del hook: post-exams2-validation.sh ejecuta el CLI del validador y el
# arsenal (FASE 2E) como scripts, no la función. Antes ninguna aplicaba la
# excepción de la regla #4 y el hook daba ERR_C4 en todo SCHOICE de opciones
# gráficas (Error 38). Se ejecutan por el symlink de .claude/scripts, como el hook.
# DIR_SCRIPTS_HOOK permite apuntar a copias mutadas al verificar que los tests muerden.
dir_scripts_hook <- Sys.getenv("DIR_SCRIPTS_HOOK",
  normalizePath(file.path(testthat::test_path(), "..", "..", ".claude", "scripts"), mustWork = FALSE))

ejecutar_script_hook <- function(script, rmd) {
  salida <- suppressWarnings(system2("Rscript", c(shQuote(script), shQuote(rmd)),
                                     stdout = TRUE, stderr = TRUE))
  paste(salida, collapse = "\n")
}

# El script no debe abortar: un test que solo mira la AUSENCIA del error pasaría
# también si el script se cae en la rama gráfica.
expect_sin_caida <- function(salida) {
  expect_false(grepl("Execution halted|Error in |Error en ", salida), info = salida)
}

rmd_hook <- function(opciones_graficas) {
  temp_file <- rmd_opciones_graficas(
    if (opciones_graficas) c("* ![`r alt_op[1]`](diagrama_a.png){width=60%}&#8203;",
                             "* ![`r alt_op[2]`](diagrama_b.png){width=60%}&#8203;")
    else c("* Opción 1", "* Opción 2"))
  lineas <- readLines(temp_file, encoding = "UTF-8")
  if (!opciones_graficas) {
    i <- which(lineas == "Test")[1]
    lineas <- append(lineas, "![Mapa del barrio](diagrama_contexto.png){width=60%}&#8203;", after = i)
  }
  writeLines(c(lineas, "exextra[Competencia]: a", "exextra[Componente]: b", "exextra[Nivel]: 1"),
             temp_file)
  temp_file
}

test_that("CLI del validador: acepta opciones gráficas y rechaza SCHOICE de texto", {
  script <- file.path(dir_scripts_hook, "validar_coherencia_matematica.R")
  graf <- rmd_hook(TRUE); texto <- rmd_hook(FALSE)
  on.exit(unlink(c(graf, texto)), add = TRUE)
  out_graf <- ejecutar_script_hook(script, graf)
  expect_sin_caida(out_graf)
  expect_match(out_graf, "exshuffle: FALSE aceptado", fixed = TRUE)
  expect_false(grepl("ERR_C4: exshuffle", out_graf),
    info = "el CLI debe aplicar la excepción de la regla #4")
  out_texto <- ejecutar_script_hook(script, texto)
  expect_true(grepl("ERR_C4: exshuffle", out_texto),
    info = "una figura de contexto no exime a un SCHOICE de texto")
  expect_false(grepl("exshuffle: FALSE aceptado", out_texto, fixed = TRUE))
  # Con exshuffle: TRUE no hay nada que aceptar: el CLI no debe anunciarlo.
  graf_true <- rmd_hook(TRUE)
  on.exit(unlink(graf_true), add = TRUE)
  writeLines(sub("^exshuffle: FALSE$", "exshuffle: TRUE", readLines(graf_true, encoding = "UTF-8")),
             graf_true)
  expect_false(grepl("exshuffle: FALSE aceptado", ejecutar_script_hook(script, graf_true), fixed = TRUE))
})

test_that("FASE 2E del arsenal: acepta opciones gráficas y rechaza SCHOICE de texto", {
  script <- file.path(dir_scripts_hook, "arsenal_validacion_completa.R")
  graf <- rmd_hook(TRUE); texto <- rmd_hook(FALSE)
  on.exit(unlink(c(graf, texto)), add = TRUE)
  out_graf <- ejecutar_script_hook(script, graf)
  expect_sin_caida(out_graf)
  expect_match(out_graf, "FASE 2E \\(Metadatos ICFES\\):\\s+OK")
  expect_false(grepl("ERROR CRÍTICO: exshuffle", out_graf),
    info = "la FASE 2E debe aplicar la excepción de la regla #4")
  out_texto <- ejecutar_script_hook(script, texto)
  expect_true(grepl("ERROR CRÍTICO: exshuffle", out_texto),
    info = "una figura de contexto no exime a un SCHOICE de texto en la FASE 2E")
})

test_that("FASE 2E sin acceso al validador aplica la regla estricta", {
  # Si no puede cargar la excepción, no la relaja en silencio (regla #24 H-5).
  aislado <- file.path(tempfile("arsenal_aislado_"), "arsenal_validacion_completa.R")
  dir.create(dirname(aislado))
  file.copy(file.path(dir_scripts_hook, "arsenal_validacion_completa.R"), aislado)
  graf <- rmd_hook(TRUE)
  on.exit(unlink(c(graf, dirname(aislado)), recursive = TRUE), add = TRUE)
  salida <- ejecutar_script_hook(aislado, graf)
  expect_true(grepl("ERROR CRÍTICO: exshuffle", salida))
  expect_true(grepl("No se pudo cargar la excepción", salida))
})

# Lectura de exshuffle unificada (Error 38): se lee exactamente como R/exams 2.4
# (exams:::read_metainfo). R/exams NO admite comentarios: "FALSE # x" da NA y mezcla.
test_that("clasificar_exshuffle convierte el valor como R/exams", {
  expect_equal(clasificar_exshuffle("TRUE"), "mezcla")
  expect_equal(clasificar_exshuffle("true"), "mezcla")
  expect_equal(clasificar_exshuffle("T"), "mezcla")
  expect_equal(clasificar_exshuffle("5"), "mezcla")
  expect_equal(clasificar_exshuffle("FALSE"), "sin_mezcla")
  expect_equal(clasificar_exshuffle("F"), "sin_mezcla")
  expect_equal(clasificar_exshuffle("TRUE # se mezclan"), "invalido")
  expect_equal(clasificar_exshuffle("FALSE  # mezcla interna"), "invalido")
  expect_equal(clasificar_exshuffle("0"), "invalido")
  expect_equal(clasificar_exshuffle("quizas"), "invalido")
  expect_equal(clasificar_exshuffle(NA_character_), "ausente")
})

con_exshuffle <- function(valor, answerlist = c("* Opción 1", "* Opción 2"), extra = character(0),
                          extype = "schoice", encabezado = "Meta-information") {
  f <- rmd_opciones_graficas(answerlist)
  l <- readLines(f, encoding = "UTF-8")
  l <- if (is.na(valor)) l[l != "exshuffle: FALSE"] else sub("^exshuffle: FALSE$", paste0("exshuffle: ", valor), l)
  l <- sub("^extype: schoice$", paste0("extype: ", extype), l)
  l <- sub("^Meta-information$", encabezado, l)
  writeLines(c(l, extra), f)
  f
}

test_that("La clasificación coincide con lo que R/exams hace al leer el archivo", {
  # Guardia contra la deriva: para cada valor, ¿R/exams mezcla? (shuffle no idéntico a FALSE)
  for (v in c("TRUE", "FALSE", "5", "T", "F", "TRUE # x", "FALSE # x")) {
    f <- con_exshuffle(v)
    mezcla_rexams <- !identical(exams:::read_metainfo(f)$shuffle, FALSE)
    estado <- evaluar_exshuffle(readLines(f, encoding = "UTF-8"))$estado
    expect_equal(estado == "sin_mezcla", !mezcla_rexams, info = v)
    unlink(f)
  }
})

test_that("La función acepta exshuffle entero y rechaza comentarios y valores no válidos", {
  f <- con_exshuffle("5")
  expect_false(any(grepl("exshuffle", validar_coherencia_matematica(f)$errores)))
  unlink(f)
  for (v in c("TRUE # se mezclan", "FALSE # regla 4", "quizas")) {
    f <- con_exshuffle(v)
    expect_true(any(grepl("exshuffle con valor no válido", validar_coherencia_matematica(f)$errores)),
                info = v)
    unlink(f)
  }
})

test_that("La excepción de opciones gráficas no oculta un valor no válido (ni FALSE comentado)", {
  for (v in c("quizas", "FALSE # regla 4")) {
    f <- con_exshuffle(v, c("* ![](diagrama_a.png){width=60%}", "* ![](diagrama_b.png){width=60%}"))
    expect_true(any(grepl("exshuffle con valor no válido", validar_coherencia_matematica(f)$errores)),
                info = v)
    unlink(f)
  }
})

test_that("exshuffle fuera de la sección Meta-information es error (R/exams no lo lee)", {
  # Sin sección reconocible, parsear_rmd() no ve ningún metadato; se prueba la lectura
  # de exshuffle directamente y por la ruta de la FASE 2E.
  f <- con_exshuffle("TRUE", encabezado = "Meta information")
  l <- readLines(f, encoding = "UTF-8")
  expect_true(any(grepl("fuera de la sección Meta-information", validar_metadatos(character(0), l))))
  expect_equal(evaluar_exshuffle_2e(f)$estado, "fuera_de_seccion")
  expect_error(exams:::read_metainfo(f), "no exsolution")  # R/exams no ve la sección
  unlink(f)
})

test_that("Una sola letra diagrama_<letra>.png, aunque se repita, no basta para la excepción", {
  for (repeticiones in 1:2) {
    f <- rmd_opciones_graficas(c("* Opción 1", "* Opción 2"))
    l <- readLines(f, encoding = "UTF-8")
    l <- append(l, rep("![Mapa](diagrama_a.png){width=60%}&#8203;", repeticiones), after = which(l == "Test")[1])
    writeLines(l, f)
    expect_true(any(grepl("ERR_C4: exshuffle debe ser TRUE", validar_coherencia_matematica(f)$errores)),
                info = paste("repeticiones:", repeticiones))
    unlink(f)
  }
})

test_that("validar_metadatos sin el archivo completo sigue leyendo exshuffle", {
  # parsear_rmd()$meta llega sin encabezado: no debe confundirse con "fuera de sección".
  f <- con_exshuffle("TRUE")
  expect_length(grep("exshuffle", validar_metadatos(parsear_rmd(f)$meta), value = TRUE), 0)
  f2 <- con_exshuffle("FALSE")
  expect_true(ERR_EXSHUFFLE_FALSE %in% validar_metadatos(parsear_rmd(f2)$meta))
  unlink(c(f, f2))
})

# Pendientes del Error 38 (7.º detractor): error de lectura, sección como R/exams,
# relevancia por tipo y exshuffle calculado con R en línea.
test_that("Las funciones internas de exams que reproduce el validador existen con su firma", {
  ns <- asNamespace("exams")
  expect_true(all(c("x", "env", "value", "markup") %in% names(formals(ns$extract_environment))))
  expect_true(all(c("x", "command", "type", "markup") %in% names(formals(ns$extract_command))))
})

test_that("Un fallo al leer con exams es 'error_lectura', no 'fuera de sección'", {
  f <- con_exshuffle("TRUE")
  l <- readLines(f, encoding = "UTF-8")
  ns_roto <- list(extract_command = asNamespace("exams")$extract_command)  # sin extract_environment
  ev <- evaluar_exshuffle(l, ns = ns_roto)
  expect_equal(ev$estado, "error_lectura")
  expect_match(errores_exshuffle(ev), "no se pudo leer exshuffle con exams", fixed = TRUE)
  expect_false(any(grepl("fuera de la sección", errores_exshuffle(ev))))
  unlink(f)
})

test_that("Encabezados que R/exams reconoce no hacen abortar la función", {
  for (enc in c("Meta-Information", "Metainformation", "Meta-information  ")) {
    f <- con_exshuffle("FALSE", encabezado = enc)
    r <- validar_coherencia_matematica(f)
    expect_true(ERR_EXSHUFFLE_FALSE %in% r$errores, info = enc)
    unlink(f)
  }
})

test_that("exshuffle ausente es error solo donde R/exams mezclaría opciones", {
  ausente <- function(extype, extra = character(0)) {
    f <- con_exshuffle(NA, extype = extype, extra = extra)
    on.exit(unlink(f))
    evaluar_exshuffle(readLines(f, encoding = "UTF-8"), f)$estado
  }
  expect_equal(ausente("schoice"), "ausente")
  expect_equal(ausente("mchoice"), "ausente")
  expect_equal(ausente("num"), "no_aplica")
  expect_equal(ausente("cloze", "exclozetype: num|string"), "no_aplica")
  expect_equal(ausente("cloze", "exclozetype: schoice|num"), "ausente")
  f <- con_exshuffle(NA)
  expect_match(validar_coherencia_matematica(f)$errores, "exshuffle ausente", all = FALSE)
  unlink(f)
})

test_that("Un archivo sin extype (no es un ejercicio) no exige exshuffle", {
  # p. ej. salida/*_interactivo.Rmd o test_*.Rmd: R/exams no podría leerlo como ejercicio.
  f <- con_exshuffle(NA)
  l <- readLines(f, encoding = "UTF-8")
  expect_equal(evaluar_exshuffle(l[!grepl("^extype:", l)])$estado, "no_aplica")
  unlink(f)
})

test_that("Un CLOZE sin huecos de elección no exige exshuffle: TRUE", {
  f <- con_exshuffle("FALSE", extype = "cloze", extra = "exclozetype: num|num")
  expect_equal(evaluar_exshuffle(readLines(f, encoding = "UTF-8"))$estado, "no_aplica")
  unlink(f)
})

test_that("Plantillas de referencia: exshuffle ausente es aviso, no error", {
  f <- con_exshuffle(NA)
  l <- readLines(f, encoding = "UTF-8")
  ruta <- "/repo/A-Produccion/03-En-Produccion/Ejemplos-Funcionales-Rmd/Plantillas/erres/x.Rmd"
  ev <- evaluar_exshuffle(l, ruta)
  expect_equal(ev$estado, "ausente_plantilla")
  expect_length(errores_exshuffle(ev), 0)
  unlink(f)
})

test_that("exshuffle calculado con R en línea no se da por bueno", {
  f <- con_exshuffle("`r mezclar`")
  ev <- evaluar_exshuffle(readLines(f, encoding = "UTF-8"))
  expect_equal(ev$estado, "dinamico")
  expect_match(errores_exshuffle(ev), "R en línea", fixed = TRUE)
  unlink(f)
})

test_that("FASE 2E: exshuffle ausente en un SCHOICE es error de la fase", {
  script <- file.path(dir_scripts_hook, "arsenal_validacion_completa.R")
  f <- con_exshuffle(NA, extra = c("exextra[Competencia]: a", "exextra[Componente]: b", "exextra[Nivel]: 1"))
  on.exit(unlink(f))
  out <- ejecutar_script_hook(script, f)
  expect_sin_caida(out)
  expect_match(out, "ERROR CRÍTICO: exshuffle ausente", fixed = TRUE)
  expect_match(out, "FASE 2E \\(Metadatos ICFES\\):\\s+ERROR")
})

test_that("Dos opciones gráficas en la misma línea activan la excepción", {
  f <- rmd_opciones_graficas("* ![](diagrama_a.png){width=40%} ![](diagrama_b.png){width=40%}")
  expect_false(any(grepl("exshuffle", validar_coherencia_matematica(f)$errores)))
  unlink(f)
})

# Nombres con sufijo por versión (regla #4 v6.1): en exams2pdf(rep(archivo, n)) todas las
# copias comparten el directorio de LaTeX y, con "&#8203;" tras la imagen, el renombrado de
# duplicados de R/exams no reescribe la referencia: todas las preguntas mostraban las figuras
# de una sola versión. El sufijo es hexadecimal o un `r ...` en línea.
test_that("Excepción de exshuffle con sufijo por versión calculado en línea", {
  f <- rmd_opciones_graficas(c(
    "* ![`r alt_op[1]`](diagrama_a_`r fig_id`.png){width=60%}&#8203;",
    "* ![`r alt_op[2]`](diagrama_b_`r fig_id`.png){width=60%}&#8203;"
  ))
  expect_false(any(grepl("exshuffle", validar_coherencia_matematica(f)$errores)))
  expect_setequal(letras_opciones_graficas(readLines(f, encoding = "UTF-8")), c("a", "b"))
  unlink(f)
})

test_that("Excepción de exshuffle con sufijo hexadecimal literal", {
  f <- rmd_opciones_graficas(c(
    "* ![](diagrama_a_3f9c0b12.png){width=60%}&#8203;",
    "* ![](diagrama_b_3f9c0b12.png){width=60%}&#8203;"
  ))
  expect_false(any(grepl("exshuffle", validar_coherencia_matematica(f)$errores)))
  unlink(f)
})

test_that("Un sufijo semántico no cuenta como nombre neutral de opción", {
  # "_correcta" filtraría la clave en el XML de Moodle (regla #22 §P6); no es el formato neutral.
  f <- rmd_opciones_graficas(c(
    "* ![](diagrama_a_correcta.png){width=60%}&#8203;",
    "* ![](diagrama_b_distractor.png){width=60%}&#8203;"
  ))
  expect_true(any(grepl("ERR_C4: exshuffle debe ser TRUE", validar_coherencia_matematica(f)$errores)))
  unlink(f)
})

test_that("FASE 2E lee el metadato como R/exams: comentarios, enteros, CLOZE y valores no válidos", {
  script <- file.path(dir_scripts_hook, "arsenal_validacion_completa.R")
  ext <- c("exextra[Competencia]: a", "exextra[Componente]: b", "exextra[Nivel]: 1")
  graficas <- c("* ![](diagrama_a.png){width=60%}", "* ![](diagrama_b.png){width=60%}")
  # exshuffle: TRUE con "exshuffle: FALSE" citado en un comentario del cuerpo.
  f1 <- con_exshuffle("TRUE", extra = ext)
  l <- readLines(f1, encoding = "UTF-8")
  writeLines(append(l, "<!-- antes se usaba `exshuffle: FALSE` -->", after = which(l == "Test")[1]), f1)
  f2 <- con_exshuffle("5", extra = ext)
  f3 <- con_exshuffle("FALSE", graficas, extra = ext, extype = "cloze")
  f4 <- con_exshuffle("quizas", extra = ext)
  f5 <- con_exshuffle("FALSE # regla 4", graficas, extra = ext)
  on.exit(unlink(c(f1, f2, f3, f4, f5)), add = TRUE)
  out1 <- ejecutar_script_hook(script, f1)
  expect_sin_caida(out1)
  expect_false(grepl("ERROR CRÍTICO: exshuffle", out1))
  expect_match(out1, "exshuffle: TRUE (correcto)", fixed = TRUE)
  out2 <- ejecutar_script_hook(script, f2)
  expect_sin_caida(out2)
  expect_match(out2, "exshuffle: 5 (correcto)", fixed = TRUE)
  # Además del mensaje, la fase debe quedar en ERROR (el error cuenta, no solo se imprime).
  fase_2e_error <- "FASE 2E \\(Metadatos ICFES\\):\\s+ERROR"
  out3 <- ejecutar_script_hook(script, f3)
  expect_match(out3, "ERROR CRÍTICO: exshuffle debe ser TRUE", fixed = TRUE)
  expect_match(out3, fase_2e_error)
  for (f in c(f4, f5)) {
    out <- ejecutar_script_hook(script, f)
    expect_match(out, "ERROR CRÍTICO: exshuffle con valor no válido", fixed = TRUE)
    expect_match(out, fase_2e_error)
  }
})

test_that("Validación CLOZE detecta inconsistencias de tipos", {
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data generation, echo = FALSE, results = \"hide\"}",
    "x <- 10",
    "y <- 20",
    "```",
    "",
    "Question",
    "========",
    "##ANSWER1## y ##ANSWER2##",
    "",
    "Solution",
    "========",
    "Test",
    "",
    "Meta-information",
    "================",
    "exname: test_cloze",
    "extype: cloze",
    "exclozetype: num|schoice",
    "exsolution: 10",  # Inconsistente: 1 valor, 2 tipos
    "extol: 0.01"
  ), temp_file)

  result <- validar_coherencia_matematica(temp_file)

  expect_false(result$aprobado)
  expect_true(length(result$errores) > 0)

  unlink(temp_file)
})

test_that("Validación detecta metadatos ICFES incompletos", {
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data generation, echo = FALSE, results = \"hide\"}",
    "x <- 5",
    "```",
    "",
    "Question",
    "========",
    "Test",
    "",
    "Solution",
    "========",
    "Test",
    "",
    "Meta-information",
    "================",
    "exname: test_metadatos",
    "extype: schoice",
    "exsolution: 1000",
    "exshuffle: TRUE"
    # Faltan metadatos ICFES (6 dimensiones)
  ), temp_file)

  result <- validar_coherencia_matematica(temp_file)

  expect_false(result$aprobado)
  expect_true(any(grepl("ICFES", result$errores)))

  unlink(temp_file)
})
