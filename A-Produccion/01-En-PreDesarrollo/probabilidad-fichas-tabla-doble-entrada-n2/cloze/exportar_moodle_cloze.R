## exportar_moodle_cloze.R — exportación del CLOZE para el aula (Moodle).
## Los gaps de opción única (Partes 1 y 5) salen como MULTICHOICE_V: botones en vertical, legibles a
## 360 px en los celulares del aula (F-6). Sin el argumento `cloze`, R/exams 2.4.2 los exporta como
## MULTICHOICE (menú desplegable). Origen: hallazgo H2 del detractor FASE 2C r3 (2026-10-28): la llamada
## solo vivía en render_cloze.R y los Semillero*.R del subproyecto no la llevan.
## Uso (SIEMPRE desde cloze/salida; nunca con el directorio de trabajo en el subproyecto del SCHOICE):
##   cd cloze/salida && Rscript --no-init-file ../exportar_moodle_cloze.R [n]
## n = número de versiones (por defecto 30; máximo 100). Sin semilla fija: cada exportación sortea.
## El .Rmd se busca con edir = ".." para que el nombre de la pregunta no lleve la ruta absoluta.
suppressMessages(library(exams))
stopifnot("correr desde cloze/salida" = basename(getwd()) == "salida" && basename(dirname(getwd())) == "cloze")
args <- commandArgs(trailingOnly = TRUE)
n <- if (length(args)) suppressWarnings(as.integer(args[1])) else 30L
stopifnot("n entre 1 y 100" = !is.na(n) && n >= 1L && n <= 100L)
RMD <- "probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd"
dir.create("moodle_aula", showWarnings = FALSE)
exams2moodle(RMD, n = n, edir = "..", dir = "moodle_aula", name = "probabilidad_fichas_cloze",
             cloze = list(cloze_schoice_display = "MULTICHOICE_V"))
## Comprobaciones antes de subir el XML
x <- paste(readLines(file.path("moodle_aula", "probabilidad_fichas_cloze.xml"), encoding = "UTF-8",
                     warn = FALSE), collapse = "\n")
## El peso del gap va antes de los dos puntos ({3:...} en la Parte 1): los patrones aceptan cualquier
## peso. Con «{1:» fijo, el gap de la Parte 1 quedaba fuera del conteo y del control de imágenes.
cuenta <- function(p) lengths(regmatches(x, gregexpr(p, x)))
gaps <- c(desplegable = cuenta("\\{[0-9]+:MULTICHOICE:"), vertical = cuenta("\\{[0-9]+:MULTICHOICE_V:"),
          multirespuesta = cuenta("\\{[0-9]+:MULTIRESPONSE:"), numerico = cuenta("\\{[0-9]+:NUMERICAL:"))
## Un `}` escapado (`\}`) no cierra el gap en Moodle: el patrón lo salta para no cortar el control a mitad
## de gap (detractor r4, H4).
img_en_gap <- sum(grepl("<img|@@PLUGINFILE@@",
                        regmatches(x, gregexpr("\\{[0-9]+:(?:MULTICHOICE|MULTIRESPONSE)(?:\\\\.|[^}\\\\])*", x,
                                               perl = TRUE))[[1]]))
## Pesos por gap: expoints 3|1|1|1|1|1 (decisión del profesor, 2026-10-28: la Parte 1, que es la pregunta
## oficial, pesa el triple). Cada pregunta del XML debe traer esa secuencia, en ese orden.
preguntas <- strsplit(x, "<question type=\"cloze\">", fixed = TRUE)[[1]][-1]
pesos <- vapply(preguntas, function(q) paste(regmatches(q, gregexpr("(?<=\\{)[0-9]+(?=:(MULTICHOICE|MULTIRESPONSE|NUMERICAL))",
                                                                       q, perl = TRUE))[[1]], collapse = "|"), "")
cat("secuencias de pesos:", paste(unique(pesos), collapse = " · "), "\n")
figuras <- unique(regmatches(x, gregexpr("[A-Za-z0-9_.-]+[.]png", x))[[1]])
nombres <- unique(regmatches(x, gregexpr("<name>\\s*<text>[^<]*</text>", x))[[1]])
print(gaps)
cat("imágenes dentro de un gap:", img_en_gap, "| figuras:", length(figuras),
    "| nombres no neutrales:", sum(!grepl("^tabla_[0-9a-f]+[.]png$", figuras)), "\n")
cat("nombre de la primera pregunta:", sub("<name>\\s*<text>", "", nombres[2]), "\n")
stopifnot(gaps[["desplegable"]] == 0L, gaps[["vertical"]] == 2L * n, gaps[["multirespuesta"]] == n,
          gaps[["numerico"]] == 3L * n, img_en_gap == 0L,
          length(pesos) == n, all(pesos == "3|1|1|1|1|1"),
          all(grepl("^tabla_[0-9a-f]+[.]png$", figuras)), !any(grepl("_home_|/home/", nombres)))
cat("OK:", n, "versiones en moodle_aula/probabilidad_fichas_cloze.xml\n")
