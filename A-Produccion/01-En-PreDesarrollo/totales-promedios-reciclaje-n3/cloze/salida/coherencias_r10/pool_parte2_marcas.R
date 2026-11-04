## pool_parte2_marcas.R — Paso 8, r10, tanda C2. SE CORRE DESPUÉS de escribir juicio_parte2_ciego.tsv. Lee el `exsolution` emitido por xexams()
## (semillas 1:100), cuenta cada texto como clave y como distractor, y lo compara con el juicio ciego del TSV y con el juicio de la r9
## (k1,k2,k3 correctas; los otros cinco incorrectos). No retoca el TSV. Directorio de trabajo fuera del repo; escribe solo junto a este script.
args0 <- commandArgs(FALSE); dir_s <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", args0, value = TRUE))))
suppressMessages(library(exams))
F <- normalizePath(file.path(dir_s, "..", "..", "totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd"))
nb <- function(x) gsub(intToUtf8(160L), " ", x, fixed = TRUE)
ex <- xexams(F, n = 100, seed = 1:100, driver = list(sweave = list(quiet = TRUE, pdf = FALSE, png = TRUE, encoding = "UTF-8"), read = NULL, transform = NULL, write = NULL))
tsv <- read.delim(file.path(dir_s, "juicio_parte2_ciego.tsv"), sep = "\t", quote = "", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
r9_correctas <- c("Lo que recogió ese grado dividido en partes iguales entre sus cursos.", "Lo que recogió ese grado dividido entre el número de sus cursos.",
                  "Lo que recogió ese grado dividido entre la cantidad de cursos de ese mismo grado.")
op <- lapply(ex, function(m) nb(m[[1]]$questionlist)[5:8]); mk <- lapply(ex, function(m) as.logical(m[[1]]$metainfo$solution[[2]]))
cl <- unlist(Map(function(o, m) o[m], op, mk)); di <- unlist(Map(function(o, m) o[!m], op, mk))
cat("md5 del .Rmd:", unname(tools::md5sum(F)), "\n")
n_disc <- 0L
for (k in seq_len(nrow(tsv))) { t <- tsv$texto[k]; emitido <- if (t %in% cl && !(t %in% di)) "correcta" else if (t %in% di && !(t %in% cl)) "incorrecta" else "AMBIGUO/AUSENTE"
  r9 <- if (t %in% r9_correctas) "correcta" else "incorrecta"
  disc <- tsv$juicio[k] != emitido || tsv$juicio[k] != r9; n_disc <- n_disc + disc
  cat(sprintf("%-10s mi juicio=%-10s marca R/exams=%-10s r9=%-10s clave %3d · distractor %3d %s | %s\n", if (disc) "DISCREPA" else "coincide", tsv$juicio[k], emitido, r9, sum(cl == t), sum(di == t), if (disc) "<<<<" else "", t)) }
cat("Textos emitidos que no están en mi TSV:", length(setdiff(unique(c(cl, di)), tsv$texto)), "\n")
cat("Discrepancias:", n_disc, "\n")
