## render_cloze.R — renderiza el CLOZE en los cuatro formatos aplicables (HTML, PDF en oficio, DOCX, Moodle) y comprueba
## que NOPS lo rechace por ser cloze (N/A esperado, Incidente E). Correr SIEMPRE desde Cloze/salida:
##   cd Cloze/salida && Rscript --no-init-file ../render_cloze.R
## Semillas: la primera canónica y variantes que cubren las tres zonas de la Parte 4, una categoría femenina
## («actividades») y el grupo «personas». PDF con ../../solpcielo_cloze.tex (216 x 330 mm, restricción 5).
## Moodle: gaps de opción única como MULTICHOICE_V (botones en vertical, legibles a 360 px).
suppressMessages(library(exams))
stopifnot("correr desde Cloze/salida" = basename(getwd()) == "salida" && basename(dirname(getwd())) == "Cloze")
F <- normalizePath("../diagrama_venn_juegos_mesa_aleatorio_interpretacion_representacion_n3_cloze_v1.Rmd")
PLANTILLA <- normalizePath("../../solpcielo_cloze.tex")
cat("md5 del .Rmd:", unname(tools::md5sum(F)), "· plantilla PDF:", unname(tools::md5sum(PLANTILLA)), "\n")
lin <- readLines(F, encoding = "UTF-8"); a <- grep("^```\\{r data_generation", lin)[1]
b <- which(lin == "```" & seq_along(lin) > a)[1]
cod <- parse(text = paste(lin[(a + 1):(b - 1)], collapse = "\n"), encoding = "UTF-8")
info <- function(s) { set.seed(s); e <- new.env(); eval(cod, envir = e)
  c(can = as.character(isTRUE(e$es_canonica)), q4 = e$q4, tipo2 = e$ctx2$tipo, grupo2 = e$grupo2) }
I <- t(vapply(1:300, info, character(4)))
primera <- function(cond) which(cond)[1]
nc <- I[, "can"] == "FALSE"
SEM <- c(canonica = primera(I[, "can"] == "TRUE"), s1 = primera(nc & I[, "q4"] == "s1"), s2 = primera(nc & I[, "q4"] == "s2"),
         s3 = primera(nc & I[, "q4"] == "s3"), fem = primera(nc & I[, "tipo2"] == "retorica"),
         personas = primera(nc & I[, "grupo2"] == "personas" & I[, "tipo2"] == "dialogo"))
stopifnot(!anyNA(SEM))
SEM <- SEM[!duplicated(SEM)]
cat("Semillas:", paste(names(SEM), SEM, sep = "=", collapse = " "), "\n")
r <- function(lbl, ex) {
  out <- tryCatch({ ex(); "OK" }, error = function(e) paste("FALLO:", conditionMessage(e)))
  cat(sprintf("%-8s %s\n", lbl, out)); invisible(out)
}
r("HTML",   function() exams2html(F, n = length(SEM), dir = "html", name = "cloze", seed = unname(SEM), mathjax = TRUE))
r("PDF",    function() exams2pdf(F, n = length(SEM), dir = "pdf", name = "cloze", seed = unname(SEM), texdir = "tex",
                                 template = PLANTILLA))
r("DOCX",   function() exams2pandoc(F, n = 1, type = "docx", dir = "docx", name = "cloze", seed = unname(SEM[1])))
r("MOODLE", function() { set.seed(SEM[[1]])    # exams2moodle no recibe seed: se fija el RNG global
  exams2moodle(F, n = 5, dir = "moodle", name = "cloze", cloze = list(cloze_schoice_display = "MULTICHOICE_V")) })
nops <- tryCatch({ exams2nops(F, n = 1, dir = "nops"); "renderizó (inesperado)" }, error = function(e) conditionMessage(e))
cat("NOPS    ", if (grepl("cloze exercises", nops)) "N/A (exams2nops rechaza extype cloze por diseño)" else
  paste("ERROR REAL:", nops), "\n")
## Validación realista del PDF (reglas #18 y #20).
tex <- list.files("tex", pattern = "\\.tex$", recursive = TRUE, full.names = TRUE)
tt <- paste(unlist(lapply(tex, readLines, warn = FALSE)), collapse = "\n")
cuenta <- function(p, x = tt) lengths(regmatches(x, gregexpr(p, x, fixed = TRUE)))
cat(sprintf("tex: %d archivos · \\pandocbounded: %d · \\LTcaptype{none}: %d · longtable: %d · includegraphics: %d\n",
            length(tex), cuenta("pandocbounded"), cuenta("LTcaptype{none}"), cuenta("begin{longtable}"), cuenta("includegraphics")))
## Moodle: V5 (ningún gap con imágenes), fuga por nombre (regla #4 v6.1) y pesos 1|1|1|1|1|1 en cada pregunta (C-3).
x <- paste(readLines(file.path("moodle", "cloze.xml"), encoding = "UTF-8", warn = FALSE), collapse = "\n")
gaps <- regmatches(x, gregexpr("\\{[0-9]+:(MULTICHOICE_V|MULTICHOICE|MULTIRESPONSE|NUMERICAL):[^}]*", x))[[1]]
cat(sprintf("Moodle: %d gaps · con <img o @@PLUGINFILE@@: %d · tipos: %s\n", length(gaps), sum(grepl("<img|@@PLUGINFILE@@", gaps)),
            paste(names(table(sub("^\\{[0-9]+:([A-Z_]+):.*$", "\\1", gaps))), table(sub("^\\{[0-9]+:([A-Z_]+):.*$", "\\1", gaps)), collapse = " ")))
nombres <- sort(unique(regmatches(x, gregexpr("diagrama_[^.\"/]+\\.png", x))[[1]]))
cat("Nombres de imagen que no son neutrales:", sum(!grepl("^diagrama_[a-z](_[0-9a-f]+)?\\.png$", nombres)), "de", length(nombres), "\n")
preg <- strsplit(x, "<question type=\"cloze\">", fixed = TRUE)[[1]][-1]
pesos <- vapply(preg, function(q) paste(regmatches(q, gregexpr("(?<=\\{)[0-9]+(?=:(MULTICHOICE|MULTIRESPONSE|NUMERICAL))", q, perl = TRUE))[[1]],
                                        collapse = "|"), "")
cat("Pesos por pregunta:", paste(unique(pesos), collapse = " · "), "\n")
