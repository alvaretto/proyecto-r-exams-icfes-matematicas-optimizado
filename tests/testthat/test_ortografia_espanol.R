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

# =============================================================================
# CONTRATO DE EXIT STATUS (2026-08-15)
# =============================================================================
# Hasta esta fecha el script imprimía "ERRORES ORTOGRÁFICOS ENCONTRADOS: N" y
# SALÍA CON 0. Un orquestador que midiera el exit —en vez de grepear la salida—
# leía "archivo limpio" sobre un archivo que el pre-commit habría rechazado.
# Tercer gate ciego del repositorio, tras la FASE 2G y la FASE 2I.
#
# Contrato fijado aquí: 0 = nada pendiente | 1 = errores auto-corregibles sin
# aplicar | 2 = solo casos ambiguos (REVISION_MANUAL, que --fix nunca toca).
#
# `stdout=FALSE, stderr=FALSE` hace que system2() devuelva el STATUS, no el texto:
# es lo único que mide de verdad el exit. Con stdout=TRUE devolvería el texto y el
# status quedaría en un atributo, que es justo como se pierde en una comprobación
# descuidada.
exit_de <- function(lineas, extra = character(0)) {
  f <- tempfile(fileext = ".Rmd")
  writeLines(lineas, f, useBytes = TRUE)
  on.exit(unlink(f), add = TRUE)
  system2("Rscript", c(SCRIPT_ORTO, f, extra), stdout = FALSE, stderr = FALSE)
}

test_that("CONTROL POSITIVO: un archivo con faltas sale con exit != 0", {
  st <- exit_de(c(
    "Question", "========",
    "La formula del area de un triangulo es conocida.",
    "Ademas, la funcion lineal tiene una solucion unica.",
    "", "Meta-information", "================",
    "exname: exit_sucio", "extype: schoice"
  ))
  expect_false(identical(as.integer(st), 0L),
               info = "el corrector volvió a salir con 0 teniendo faltas: gate ciego reintroducido")
  expect_equal(as.integer(st), 1L,
               info = "faltas auto-corregibles pendientes deben dar exit 1")
})

test_that("CONTROL NEGATIVO: un archivo limpio sale con exit 0", {
  st <- exit_de(c(
    "Question", "========",
    "La fórmula del área de un triángulo es conocida.",
    "Además, la función lineal tiene una solución única.",
    "", "Meta-information", "================",
    "exname: exit_limpio", "extype: schoice"
  ))
  expect_equal(as.integer(st), 0L,
               info = "un archivo sin faltas NO puede fallar el gate (falso rojo)")
})

test_that("Solo casos ambiguos: exit 2, distinto del 1 de las faltas reales", {
  # 'formula' es ambiguo (sustantivo 'fórmula' vs verbo 'él formula'): se reporta
  # como REVISION_MANUAL y --fix no lo toca. No debe confundirse con una falta.
  st <- exit_de(c(
    "Question", "========",
    "La formula aparece en el enunciado.",
    "", "Meta-information", "================",
    "exname: exit_ambiguo", "extype: schoice"
  ))
  expect_equal(as.integer(st), 2L,
               info = "los ambiguos deben tener su propio código, ni 0 (limpio) ni 1 (falta)")
})

test_that("--fix deja de reportar las faltas que ya escribió en disco", {
  f <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "Question", "========",
    "La solucion tiene una funcion lineal. Ademas es unica.",
    "", "Meta-information", "================",
    "exname: exit_fix", "extype: schoice"
  ), f, useBytes = TRUE)

  # Tras --fix las faltas ya no están pendientes -> no puede seguir dando 1.
  system2("Rscript", c(SCRIPT_ORTO, f, "--fix"), stdout = FALSE, stderr = FALSE)
  st2 <- system2("Rscript", c(SCRIPT_ORTO, f), stdout = FALSE, stderr = FALSE)

  txt <- paste(readLines(f, encoding = "UTF-8"), collapse = " ")
  expect_true(grepl("solución", txt), info = "--fix no escribió la corrección")
  expect_false(identical(as.integer(st2), 1L),
               info = "sigue reportando faltas pendientes después de aplicarlas")

  unlink(f)
})

test_that("El pre-commit sigue leyendo la salida, no el exit (no se rompió)", {
  # El hook real hace `$(Rscript ... || true)` + grep "ERRORES": el cambio de exit
  # NO debe alterar su veredicto. Este control es lo que autoriza el cambio.
  f <- tempfile(fileext = ".Rmd")
  writeLines(c("Question", "========", "La formula del area es conocida.",
               "", "Meta-information", "================",
               "exname: exit_hook", "extype: schoice"), f, useBytes = TRUE)
  salida <- paste(system2("Rscript", c(SCRIPT_ORTO, f), stdout = TRUE, stderr = TRUE),
                  collapse = "\n")
  expect_true(grepl("ERRORES", salida),
              info = "el hook pre-commit dejaría pasar un archivo con faltas")
  unlink(f)
})

# =============================================================================
# ATRIBUTOS HTML (class=, id=, ...): identificadores, no texto visible (2026-08-15)
# =============================================================================
# El corrector marcaba "opcion" dentro de `class="ex-opcion"` como falta y,
# con --fix, escribía `class="ex-opción"` — una tilde dentro de un
# identificador CSS. Detectado en 3 .Rmd de 03-En-Produccion/
# Probabilidad-Intervalos-Curva-13-S1-2024B/ que bloqueaban el commit y, más
# grave, cuyo --fix ya había corrompido el atributo (revertido a mano). Ver
# cabecera del script (SOURCES/scripts_validacion/corregir_ortografia_espanol.R).
#
# La ruta real es un symlink (invariante I-10): SCRIPT_ORTO apunta a
# .claude/scripts/..., que resuelve de forma transparente al archivo fuente
# en SOURCES/scripts_validacion/. Los tests leen y ejecutan por esa ruta como
# cualquier consumidor real; el fix en sí vive únicamente en SOURCES/.
SCRIPT_ORTO_FUENTE <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/SOURCES/scripts_validacion/corregir_ortografia_espanol.R"

test_that("CONTROL POSITIVO: class=\"ex-opcion\" no se reporta y --fix no toca el archivo", {
  # Línea real del bug: HTML embebido en un string R de comillas simples con
  # el atributo en comillas dobles — sin ninguna otra palabra visible.
  f <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data_generation, echo=FALSE, results=\"hide\"}",
    "tabla_renderizada <- sprintf('<table class=\"ex-opcion\" style=\"border-collapse:collapse;\">%s%s</table>', header, rows)",
    "```",
    "",
    "Question",
    "========",
    "Observa la tabla."
  ), f, useBytes = TRUE)
  on.exit(unlink(f), add = TRUE)

  md5_antes <- unname(tools::md5sum(f))

  salida <- paste(system2("Rscript", c(SCRIPT_ORTO, f), stdout = TRUE, stderr = TRUE),
                  collapse = "\n")
  expect_false(grepl("ERRORES", salida),
               info = "reportó como falta un identificador dentro de class=\"...\"")
  expect_true(grepl("No se encontraron errores", salida))

  system2("Rscript", c(SCRIPT_ORTO, f, "--fix"), stdout = FALSE, stderr = FALSE)
  md5_despues <- unname(tools::md5sum(f))
  expect_identical(md5_antes, md5_despues,
                    info = "--fix modificó el archivo aunque no había faltas reales")

  txt <- paste(readLines(f, encoding = "UTF-8"), collapse = " ")
  expect_true(grepl('class="ex-opcion"', txt, fixed = TRUE),
              info = "--fix acentuó el identificador: class=\"ex-opción\" rompe cualquier selector CSS/JS")

  unlink(f)
})

test_that("CONTROL NEGATIVO (anti-sobre-exclusión): 'opcion' en texto visible SÍ se corrige", {
  # Si la exclusión fuera demasiado ancha (cualquier cosa entre comillas),
  # este caso —sin ningún atributo HTML— dejaría de detectarse. Debe seguir
  # detectándose: la regla #7 depende de que el corrector NO se apague entero.
  f <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r mostrar, echo=FALSE, results='asis'}",
    "cat(\"La opcion correcta es la que tiene mayor valor.\")",
    "```",
    "",
    "Question",
    "========",
    "* La opcion correcta es la que tiene mayor valor."
  ), f, useBytes = TRUE)
  on.exit(unlink(f), add = TRUE)

  salida <- paste(system2("Rscript", c(SCRIPT_ORTO, f), stdout = TRUE, stderr = TRUE),
                  collapse = "\n")
  expect_true(grepl("ERRORES", salida),
              info = "dejó de detectar una falta real por sobre-excluir texto entre comillas")
  expect_true(grepl("opcion.*opción|opción", salida))

  system2("Rscript", c(SCRIPT_ORTO, f, "--fix"), stdout = FALSE, stderr = FALSE)
  txt <- paste(readLines(f, encoding = "UTF-8"), collapse = " ")
  expect_true(grepl("opción", txt), info = "--fix no corrigió el texto visible")

  unlink(f)
})

test_that("CASO MIXTO (el real): atributo Y texto visible en la misma línea", {
  # sprintf('<table class="ex-opcion">%s</table>', "la opcion elegida")
  # Debe reportar EXACTAMENTE una falta (la del texto), no dos, no cero — y
  # --fix debe corregir SOLO la aparición visible, dejando class="ex-opcion"
  # intacto.
  f <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data_generation, echo=FALSE, results=\"hide\"}",
    "tabla_renderizada <- sprintf('<table class=\"ex-opcion\">%s</table>', \"la opcion elegida\")",
    "```",
    "",
    "Question",
    "========",
    "Observa la tabla."
  ), f, useBytes = TRUE)
  on.exit(unlink(f), add = TRUE)

  salida <- paste(system2("Rscript", c(SCRIPT_ORTO, f), stdout = TRUE, stderr = TRUE),
                  collapse = "\n")
  n_reportes <- length(gregexpr("Línea \\d+: 'opcion'", salida, perl = TRUE)[[1]])
  # gregexpr devuelve -1 (length 1, valor -1) cuando no hay match: normalizar.
  if (identical(gregexpr("Línea \\d+: 'opcion'", salida, perl = TRUE)[[1]][1], -1L)) n_reportes <- 0L
  expect_equal(n_reportes, 1L,
               info = paste("se esperaba exactamente 1 reporte de 'opcion', hubo", n_reportes))

  system2("Rscript", c(SCRIPT_ORTO, f, "--fix"), stdout = FALSE, stderr = FALSE)
  txt <- paste(readLines(f, encoding = "UTF-8"), collapse = " ")
  expect_true(grepl('class="ex-opcion"', txt, fixed = TRUE),
              info = "--fix corrompió el atributo en la línea mixta")
  expect_true(grepl("la opción elegida", txt, fixed = TRUE),
              info = "--fix no corrigió el texto visible en la línea mixta")

  unlink(f)
})

test_that("MUTACIÓN: sin el reconocimiento de atributos HTML, --fix SÍ corrompe class=\"...\"", {
  # Control de que el mecanismo hace algo (no es un cheque vacío). El fix
  # tiene DOS capas independientes que dependen ambas de
  # ATRIBUTOS_HTML_IDENTIFICADOR / patron_valor_atributo_html():
  #   1) es_atributo_html() — decide si se REPORTA la palabra.
  #   2) proteger_atributos_html()/restaurar_atributos_html() — blindan el
  #      bloque atributo="valor" en el momento del gsub, INCONDICIONALMENTE
  #      (es la razón de que la "línea mixta" corrija el texto visible sin
  #      tocar el atributo).
  # Mutar solo (1) no basta para demostrar corrupción: (2) sigue protegiendo.
  # Se muta la RAÍZ compartida por ambas capas — la lista de atributos — en
  # una COPIA del script en tempdir() (NUNCA el archivo real), y se
  # comprueba que, sin ella, el mismo fixture del control positivo SÍ queda
  # corrompido por --fix.
  script_original <- readLines(SCRIPT_ORTO_FUENTE, encoding = "UTF-8", warn = FALSE)
  linea_raiz <- 'ATRIBUTOS_HTML_IDENTIFICADOR <- "class|id|name|for|href|src|data-[a-zA-Z0-9_-]+"'
  expect_true(any(script_original == linea_raiz),
              info = "no se encontró la línea exacta de la raíz compartida: ajustar el texto mutado")

  script_mutado <- script_original
  script_mutado[script_original == linea_raiz] <-
    'ATRIBUTOS_HTML_IDENTIFICADOR <- "NUNCA_COINCIDE_XYZ"  # MUTADO por el test'
  expect_false(identical(script_original, script_mutado),
               info = "la mutación no cambió nada: el test no estaría probando el mecanismo real")

  script_copia <- tempfile(fileext = ".R")
  writeLines(script_mutado, script_copia, useBytes = TRUE)
  on.exit(unlink(script_copia), add = TRUE)

  f <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data_generation, echo=FALSE, results=\"hide\"}",
    "tabla_renderizada <- sprintf('<table class=\"ex-opcion\">%s</table>', header)",
    "```",
    "",
    "Question",
    "========",
    "Observa la tabla."
  ), f, useBytes = TRUE)
  on.exit(unlink(f), add = TRUE)

  system2("Rscript", c(script_copia, f, "--fix"), stdout = FALSE, stderr = FALSE)
  txt <- paste(readLines(f, encoding = "UTF-8"), collapse = " ")
  expect_true(grepl("ex-opción", txt),
              info = paste("sin la raíz compartida el fixture NO se corrompió:",
                           "el test no está ejercitando el mecanismo real, revisar linea_raiz"))

  unlink(f)
})
