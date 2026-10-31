## exportar_moodle_cloze.R — exportación del CLOZE para el aula (Moodle del colegio, celulares sin internet).
## Los gaps de opción única (Partes 1 y 2) salen como MULTICHOICE_V: botones en vertical, legibles a 360 px. Sin
## el argumento cloze, R/exams 2.4.2 los exporta como MULTICHOICE (menú desplegable).
## Uso (SIEMPRE desde cloze/salida): cd cloze/salida && Rscript --no-init-file ../exportar_moodle_cloze.R [n]
## n = número de versiones (por defecto 30; máximo 100). Sin semilla fija: cada exportación sortea.
## Guardas antes de subir el XML: tipos y número de gaps, pesos 3|1|1|1|1|1 en cada pregunta (F-3: R/exams
## recicla expoints sin avisar), nada de imágenes ni etiquetas HTML dentro de un gap (Incidente G) y nombres sin ruta.
suppressMessages(library(exams))
stopifnot("correr desde cloze/salida" = basename(getwd()) == "salida" && basename(dirname(getwd())) == "cloze")
args <- commandArgs(trailingOnly = TRUE)
n <- if (length(args)) suppressWarnings(as.integer(args[1])) else 30L
stopifnot("n entre 1 y 100" = !is.na(n) && n >= 1L && n <= 100L)
RMD <- "totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd"
cat("md5 del .Rmd:", unname(tools::md5sum(file.path("..", RMD))), "\n")
dir.create("moodle_aula", showWarnings = FALSE)
exams2moodle(RMD, n = n, edir = "..", dir = "moodle_aula", name = "totales_promedios_cloze",
             cloze = list(cloze_schoice_display = "MULTICHOICE_V"))
XML <- file.path("moodle_aula", "totales_promedios_cloze.xml")
x <- paste(readLines(XML, encoding = "UTF-8", warn = FALSE), collapse = "\n")
cuenta <- function(p) lengths(regmatches(x, gregexpr(p, x)))
gaps <- c(desplegable = cuenta("\\{[0-9]+:MULTICHOICE:"), vertical = cuenta("\\{[0-9]+:MULTICHOICE_V:"),
          multirespuesta = cuenta("\\{[0-9]+:MULTIRESPONSE:"), numerico = cuenta("\\{[0-9]+:NUMERICAL:"))
preguntas <- strsplit(x, "<question type=\"cloze\">", fixed = TRUE)[[1]][-1]
pesos <- vapply(preguntas, function(q) paste(regmatches(q, gregexpr("(?<=\\{)[0-9]+(?=:(MULTICHOICE|MULTIRESPONSE|NUMERICAL))",
                                                                   q, perl = TRUE))[[1]], collapse = "|"), "")
nombres <- unique(regmatches(x, gregexpr("<name>\\s*<text>[^<]*</text>", x))[[1]])
print(gaps)
cat("secuencias de pesos:", paste(unique(pesos), collapse = " · "), "\n")
cat("nombre de la primera pregunta:", sub("<name>\\s*<text>", "", nombres[2]), "\n")
## Etiquetas dentro de los gaps, de cualquier tipo, tras desescapar el HTML (script en Python: html.unescape).
etq <- system2("python3", c("-I", "../verificar_gaps_xml.py", XML), stdout = TRUE)
cat(etq, sep = "\n")
n_etq <- as.integer(sub("^.*etiquetas dentro de gaps: ([0-9]+).*$", "\\1", grep("^TOTAL:", etq, value = TRUE)))
stopifnot(gaps[["desplegable"]] == 0L, gaps[["vertical"]] == 2L * n, gaps[["multirespuesta"]] == n,
          gaps[["numerico"]] == 3L * n, length(pesos) == n, all(pesos == "3|1|1|1|1|1"),
          length(n_etq) == 1L, n_etq == 0L, !grepl("<img|@@PLUGINFILE@@", x), !any(grepl("_home_|/home/", nombres)))
cat("OK:", n, "versiones en", XML, "\n")
