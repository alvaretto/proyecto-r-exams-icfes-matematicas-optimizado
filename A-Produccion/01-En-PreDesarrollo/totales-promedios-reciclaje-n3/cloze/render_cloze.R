## render_cloze.R — renderiza el CLOZE en los cuatro formatos aplicables (HTML, PDF, DOCX, Moodle) y comprueba
## que NOPS lo rechace por ser cloze (N/A esperado, Incidente E). Correr SIEMPRE desde cloze/salida:
##   cd cloze/salida && Rscript --no-init-file ../render_cloze.R
## Semillas: la primera canónica y una no canónica por cada cruce rama de la Parte 1 × rama del otro colegio.
## Moodle: gaps de opción única como MULTICHOICE_V (botones en vertical, legibles a 360 px); para el aula, usar
## exportar_moodle_cloze.R, que además vigila los pesos.
suppressMessages(library(exams))
stopifnot("correr desde cloze/salida" = basename(getwd()) == "salida" && basename(dirname(getwd())) == "cloze")
F <- normalizePath("../totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd")
cat("md5 del .Rmd:", unname(tools::md5sum(F)), "\n")
lin <- readLines(F, encoding = "UTF-8"); a <- grep("^```\\{r data_generation", lin)[1]
b <- which(lin == "```" & seq_along(lin) > a)[1]
cod <- parse(text = paste(lin[(a + 1):(b - 1)], collapse = "\n"), encoding = "UTF-8")
info <- function(s) { set.seed(s); e <- new.env(); eval(cod, envir = e)
  c(can = as.character(isTRUE(e$es_canonica)), rama = e$rama, par = e$rama_par) }
I <- t(vapply(1:300, info, character(3)))
primera <- function(cond) which(cond)[1]
SEM <- c(canonica = primera(I[, "can"] == "TRUE"),
         CxC = primera(I[, "can"] == "FALSE" & I[, "rama"] == "CURSOS" & I[, "par"] == "CURSOS"),
         CxP = primera(I[, "can"] == "FALSE" & I[, "rama"] == "CURSOS" & I[, "par"] == "PROMEDIO"),
         PxC = primera(I[, "can"] == "FALSE" & I[, "rama"] == "PROMEDIO" & I[, "par"] == "CURSOS"),
         PxP = primera(I[, "can"] == "FALSE" & I[, "rama"] == "PROMEDIO" & I[, "par"] == "PROMEDIO"))
stopifnot(!anyNA(SEM))
cat("Semillas:", paste(names(SEM), SEM, sep = "=", collapse = " "), "\n")
r <- function(lbl, ex) {
  out <- tryCatch({ ex(); "OK" }, error = function(e) paste("FALLO:", conditionMessage(e)))
  cat(sprintf("%-8s %s\n", lbl, out)); invisible(out)
}
r("HTML",   function() exams2html(F, n = length(SEM), dir = "html", name = "cloze", seed = unname(SEM), mathjax = TRUE))
r("PDF",    function() exams2pdf(F, n = length(SEM), dir = "pdf", name = "cloze", seed = unname(SEM), texdir = "tex"))
r("DOCX",   function() exams2pandoc(F, n = 1, type = "docx", dir = "docx", name = "cloze", seed = unname(SEM[1])))
r("MOODLE", function() { set.seed(SEM[[1]])    # exams2moodle no recibe seed: se fija el RNG global
  exams2moodle(F, n = 5, dir = "moodle", name = "cloze", cloze = list(cloze_schoice_display = "MULTICHOICE_V")) })
nops <- tryCatch({ exams2nops(F, n = 1, dir = "nops"); "renderizó (inesperado)" }, error = function(e) conditionMessage(e))
cat("NOPS    ", if (grepl("cloze exercises", nops)) "N/A (exams2nops rechaza extype cloze por diseño)" else
  paste("ERROR REAL:", nops), "\n")
## Validación realista del PDF (regla #18 y #20): el .tex no usa \pandocbounded y, si trae \LTcaptype{none},
## el .Rmd lleva la guarda del contador none.
tex <- list.files("tex", pattern = "\\.tex$", recursive = TRUE, full.names = TRUE)
tt <- paste(unlist(lapply(tex, readLines, warn = FALSE)), collapse = "\n")
cat(sprintf("tex: %d archivos · \\pandocbounded: %d · \\LTcaptype{none}: %d · guarda c@none en el .Rmd: %s · longtable: %d\n",
            length(tex), lengths(regmatches(tt, gregexpr("pandocbounded", tt, fixed = TRUE))),
            lengths(regmatches(tt, gregexpr("LTcaptype{none}", tt, fixed = TRUE))),
            any(grepl("@ifundefined{c@none}", lin, fixed = TRUE)),
            lengths(regmatches(tt, gregexpr("begin{longtable}", tt, fixed = TRUE)))))
