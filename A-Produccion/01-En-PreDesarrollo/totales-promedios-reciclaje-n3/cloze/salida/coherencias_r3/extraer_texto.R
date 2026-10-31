## extraer_texto.R — texto visible de las 100 versiones (pregunta, opciones, Solution, retroalimentación) y todas las
## cadenas literales del .Rmd (cubre ramas poco frecuentes). Salida: texto_visible_N100.txt y literales_rmd.txt
suppressMessages(library(exams))
F <- normalizePath("../../totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd")
ex <- xexams(F, n = 100, seed = 1:100, driver = list(sweave = list(quiet = TRUE, pdf = FALSE, png = TRUE, encoding = "UTF-8"),
                                                     read = NULL, transform = NULL, write = NULL))
limpia <- function(x) {
  x <- gsub(" ", " ", x, fixed = TRUE)
  x <- x[!grepl("^\\|[-:| ]+\\|$|^```|^\\\\|^:::|^##ANSWER", x)]
  x <- gsub("\\$[^$]*\\$", " ", x)            # fórmulas
  x <- gsub("<[^>]+>", "", x)                 # span
  x <- gsub("[*|]", " ", x)                   # negrita y barras de tabla
  x[nzchar(trimws(x))]
}
txt <- unlist(lapply(ex, function(m) { e <- m[[1]]; c(limpia(e$question), limpia(e$questionlist), limpia(e$solution), limpia(e$solutionlist)) }))
writeLines(unique(txt), "texto_visible_N100.txt", useBytes = TRUE)
cat("líneas únicas de texto visible:", length(unique(txt)), "\n")

## cadenas literales de todos los chunks de código del .Rmd
lin <- readLines(F, encoding = "UTF-8")
ini <- grep("^```\\{r", lin); fin <- which(lin == "```")
lit <- character(0)
for (a in ini) {
  b <- fin[fin > a][1]
  cod <- paste(lin[(a + 1):(b - 1)], collapse = "\n")
  pd <- getParseData(parse(text = cod, keep.source = TRUE, encoding = "UTF-8"))
  s <- pd$text[pd$token == "STR_CONST"]
  s <- vapply(s, function(z) tryCatch(eval(parse(text = z, encoding = "UTF-8")), error = function(e) z), "")
  lit <- c(lit, s)
}
## y el texto fuera de los chunks (Meta-information y rótulos)
fuera <- lin[!(seq_along(lin) %in% unlist(lapply(ini, function(a) a:fin[fin > a][1])))]
lit <- c(lit, fuera[grepl("^exextra|^exname", fuera)])
lit <- gsub(" ", " ", lit, fixed = TRUE)
lit <- gsub("\\\\[a-z]+", " ", lit)    # órdenes LaTeX
lit <- unique(lit[nzchar(trimws(lit))])
writeLines(lit, "literales_rmd.txt", useBytes = TRUE)
cat("cadenas literales únicas del .Rmd:", length(lit), "\n")
