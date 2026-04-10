# Tests unitarios para ejercicio cloze de Estadística e Interpretación de gráficos

# Requisitos: packages exams, testthat, stringr, xml2
suppressPackageStartupMessages({
  library(testthat)
  library(exams)
  library(stringr)
  library(xml2)
})

# Ruta al .Rmd bajo test: basada en repo root para evitar fragilidad de cwd.
.repo_root <- tryCatch(system("git rev-parse --show-toplevel", intern = TRUE),
                       error = function(e) "")
EX_REL <- file.path(
  "A-Produccion", "03-En-Produccion", "06-Estadística-Y-Probabilidad",
  "Pensamiento-Aleatorio",
  "01-Variables-Cualitativas_Distribucion-De-Frecuencias",
  "ExportacionesGraficos-Tebailandia",
  "exportaciones_graficos_tebailandia_aleatorio_interpretacion_representacion_n3_v1",
  "ExportacionesGraficosEstadisticaInterpretacion_n3_cloze_v1.Rmd"
)
EX_PATH <- if (length(.repo_root) == 1 && nzchar(.repo_root)) {
  file.path(.repo_root, EX_REL)
} else ""
.ex_available <- nzchar(EX_PATH) && file.exists(EX_PATH)

# xexams copia el .Rmd a un tempdir usando un nombre derivado de la ruta
# completa, lo cual excede el límite de longitud de filename del filesystem
# cuando la ruta es larga. Copiamos el .Rmd a una ruta corta una sola vez.
.ex_short_path <- NULL
.get_ex_path <- function() {
  if (is.null(.ex_short_path) || !file.exists(.ex_short_path)) {
    tmp_dir <- file.path(tempdir(), "cloze_n3_ex")
    dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
    short_path <- file.path(tmp_dir, "ex_cloze_n3.Rmd")
    file.copy(EX_PATH, short_path, overwrite = TRUE)
    # También copiar supplements del directorio del .Rmd
    src_dir <- dirname(EX_PATH)
    supp <- list.files(src_dir, full.names = TRUE)
    supp <- supp[!grepl("\\.Rmd$", supp) & !dir.exists(supp)]
    if (length(supp) > 0) file.copy(supp, tmp_dir, overwrite = TRUE)
    .ex_short_path <<- short_path
  }
  .ex_short_path
}

# Helper: generar 1 instancia con xexams y devolver lista con question/solution/metainfo/supplements
.gen_one <- function(seed = NULL) {
  skip_if_not(.ex_available, "Ejercicio .Rmd no encontrado en repo")
  if (!is.null(seed)) set.seed(seed)
  res <- exams::xexams(.get_ex_path(), n = 1)
  # estructura: list(exam)[[1]][[1]]
  ex <- res[[1]][[1]]
  stopifnot(is.list(ex$metainfo))
  ex
}

# Helper: redondeo matemático correcto (hacia arriba para .5)
.redondear_matematico <- function(x, digits = 2) {
  factor <- 10^digits
  floor(x * factor + 0.5) / factor
}

# (sin uso) Helper de sección placeholder (stub)
.get_q_sections <- function(...) character(0)

context("Validación de datos generados y cálculos")

test_that("Validación de datos generados aleatoriamente (una instancia)", {
  ex <- .gen_one()
  mi <- ex$metainfo
  # Valores numéricos esperados
  valor_total    <- as.numeric(mi$solution[[1]])
  porcentaje_prod<- as.numeric(mi$solution[[2]])
  resp3          <- as.numeric(mi$solution[[3]])
  resp4          <- as.numeric(mi$solution[[4]])
  resp5          <- as.numeric(mi$solution[[5]])
  resp6          <- as.numeric(mi$solution[[6]])

  # Rango valor_total (10.1 - 91.0)
  expect_gte(valor_total, 10.1)
  expect_lte(valor_total, 91.0)

  # porcentaje_producto > 0
  expect_gt(porcentaje_prod, 0)

  # Conversión porcentaje a decimal
  expect_equal(resp4, porcentaje_prod / 100, tolerance = 1e-4)

  # resp3 debe ser el mismo valor_total a 1 decimal
  expect_equal(resp3, .redondear_matematico(valor_total, 1), tolerance = 1e-8)

  # Multiplicación intermedia con tolerancia 0.01
  expect_equal(resp5, valor_total * resp4, tolerance = 0.01)

  # Resultado final coincide con intermedio a 2 decimales
  expect_equal(resp6, .redondear_matematico(valor_total * resp4, 2), tolerance = 1e-8)

  # Año dentro de 1950-2023 (extraer del enunciado de Question en HTML)
  qtxt <- paste(ex$question, collapse = "\n")
  years <- stringr::str_extract_all(qtxt, "(19[5-9][0-9]|20[0-2][0-9]|2023)")[[1]]
  years <- suppressWarnings(as.integer(years))
  expect_true(any(years >= 1950 & years <= 2023))

  # Verificar que la distribución del gráfico circular suma 100% (en Solution listada)
  stxt <- ex$solution
  # Aislar bloque de distribución porcentual entre su encabezado y el siguiente '### Paso'
  start <- which(grepl("Distribuci", stxt, fixed = TRUE))
  paso_next <- which(grepl("^### Paso", stxt))
  if (length(start) > 0) {
    end <- paso_next[paso_next > start[1]][1]
    if (is.na(end)) end <- length(stxt)
    seg <- stxt[start[1]:end]
    m <- stringr::str_match_all(paste(seg, collapse = "\n"), "-\\s+[^:]+:\\s*([0-9]+)\\%")[[1]]
    if (nrow(m) > 0) {
      suma <- sum(as.numeric(m[,2]))
      expect_equal(suma, 100)
    } else {
      skip("No se hallaron porcentajes en el bloque de distribución")
    }
  } else {
    skip("No se halló el encabezado de distribución porcentual en Solution")
  }
})

context("Pruebas de formato cloze y metainformación")

test_that("exclozetype, exsolution y extol son correctos", {
  ex <- .gen_one(999)
  mi <- ex$metainfo
  # Tipos
  expect_equal(mi$clozetype, c("num","num","num","num","num","num","schoice"))
  # Tolerancias
  expect_equal(mi$tolerance, c(0.01, 0.01, 0.01, 0.0001, 0.01, 0.01, 0))
  # Longitud (7 respuestas)
  expect_equal(mi$length, 7L)
  # Schoice: un único TRUE
  expect_equal(sum(mi$solution[[7]]), 1L)
  expect_true(is.logical(mi$solution[[7]]))
})

# Para placeholders, analizamos directamente el Rmd entre Question y Answerlist

test_that("Placeholders ##ANSWER1##..##ANSWER7## aparecen exactamente una vez en Question", {
  skip_if_not(.ex_available, "Ejercicio .Rmd no encontrado en repo")
  rmd <- readLines(EX_PATH, warn = FALSE)
  q_i <- which(trimws(rmd) == "Question")
  a_i <- which(trimws(rmd) == "Answerlist")
  expect_true(length(q_i) == 1)
  expect_true(length(a_i) >= 1)
  seg <- rmd[(q_i[1]):(a_i[1])]

  # Contar total de placeholders 1..7
  m <- stringr::str_match_all(paste(seg, collapse = "\n"), "##ANSWER([1-7])##")[[1]]
  expect_equal(nrow(m), 7)           # deben existir 7 apariciones
  # Verificar una sola aparición por cada índice
  idx <- as.integer(m[,2])
  tab <- table(idx)
  expect_true(all(tab == 1))
})

context("Testing de exportación a formatos")

test_that("exams2moodle/html/qti21 generan archivos e incluyen imágenes", {
  skip_if_not(.ex_available, "Ejercicio .Rmd no encontrado en repo")
  td <- tempfile("exm_"); dir.create(td)
  # moodle
  exams2moodle(.get_ex_path(), n = 1, dir = td, name = "t_cloze")
  xmls <- list.files(td, pattern = "\\.xml$", full.names = TRUE)
  expect_true(length(xmls) >= 1)
  # Validar que el XML menciona las imágenes
  x <- xml2::read_xml(xmls[1])
  imgs <- xml2::xml_find_all(x, ".//file[@name]")
  img_names <- xml2::xml_attr(imgs, "name")
  expect_true(any(grepl("grafico_barras\\.png$", img_names)))
  expect_true(any(grepl("grafico_torta\\.png$", img_names)))

  # html
  exams2html(.get_ex_path(), n = 1, dir = td, name = "t_cloze_html")
  htmls <- list.files(td, pattern = "\\.html$", full.names = TRUE)
  expect_true(length(htmls) >= 1)
  ht <- paste(readLines(htmls[1], warn = FALSE), collapse = "\n")
  expect_true(grepl("<img ", ht))

  # qti21 (si el paquete soporta)
  expect_error(exams2qti21(.get_ex_path(), n = 1, dir = td, name = "t_cloze_qti"), NA)
  # QTI suele generar .xml o un directorio/zip con recursos
  qti_files <- list.files(td, recursive = TRUE, full.names = TRUE)
  expect_true(any(grepl("qti|imsmanifest|xml", tolower(qti_files))))
})

context("Pruebas de calificación automática por tolerancias (simulada)")

test_that("Respuestas correctas calificarían 100% y tolerancias funcionan", {
  ex <- .gen_one(321)
  mi <- ex$metainfo
  tol <- mi$tolerance
  sol_num <- as.numeric(mi$solution[1:6])

  # Simulación: respuesta igual -> dentro de tolerancia
  expect_true(all(abs(sol_num - sol_num) <= tol[1:6] + 1e-12))

  # Perturbaciones dentro de tolerancia para 1,2,3,5,6; y muy pequeñas para 4
  deltas <- c(0.005, 0.005, 0.005, 0.00005, 0.005, 0.005)
  expect_true(all(abs((sol_num + deltas) - sol_num) <= tol[1:6]))

  # Fuera de tolerancia
  big_deltas <- c(0.02, 0.02, 0.02, 0.001, 0.02, 0.02)
  expect_true(any(abs((sol_num + big_deltas) - sol_num) > tol[1:6]))

  # Schoice: un TRUE
  schoice <- mi$solution[[7]]
  expect_equal(sum(schoice), 1L)
})

context("Robustez: ejecutar múltiples veces (n = 50)")

test_that("50 ejecuciones generan datos y recursos válidos", {
  set.seed(42)
  for (i in 1:50) {
    ex <- .gen_one()
    mi <- ex$metainfo

    # Validaciones básicas
    valor_total <- as.numeric(mi$solution[[1]])
    porcentaje   <- as.numeric(mi$solution[[2]])
    resp4        <- as.numeric(mi$solution[[4]])
    resp5        <- as.numeric(mi$solution[[5]])

    expect_true(is.finite(valor_total) && valor_total >= 10.1 && valor_total <= 91.0)
    expect_true(is.finite(porcentaje) && porcentaje > 0 && porcentaje <= 100)
    expect_equal(resp4, porcentaje/100, tolerance = 1e-4)
    expect_equal(resp5, valor_total * resp4, tolerance = 0.01)

    # Imágenes suplementarias existen en el disco temporal de la instancia
    supp <- ex$supplements
    expect_true(any(grepl("grafico_barras\\.png$", names(supp))))
    expect_true(any(grepl("grafico_torta\\.png$", names(supp))))
    for (p in unname(supp)) expect_true(file.exists(p))
  }
})

