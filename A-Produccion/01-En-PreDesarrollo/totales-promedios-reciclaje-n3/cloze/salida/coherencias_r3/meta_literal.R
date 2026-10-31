## meta_literal.R — cadenas exclozetype/exsolution/extol que evalúa el .Rmd por semilla y su coincidencia con lo que lee R/exams
suppressMessages(library(exams))
F <- normalizePath("../../totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd")
lin <- readLines(F, encoding = "UTF-8"); a <- grep("^```\\{r data_generation", lin)[1]; b <- which(lin == "```" & seq_along(lin) > a)[1]
cod <- parse(text = paste(lin[(a + 1):(b - 1)], collapse = "\n"), encoding = "UTF-8")
cat("líneas de metadatos del .Rmd:\n"); cat(grep("^(exname|extype|exclozetype|exsolution|extol|expoints|exshuffle|exextra\\[(Nivel|DOK|Bloom|SOLO|Type)\\])", lin, value = TRUE), sep = "\n")
for (s in c(7, 3, 2, 1, 4)) {
  set.seed(s); e <- new.env(); eval(cod, envir = e)
  ex <- xexams(F, n = 1, seed = s, driver = list(sweave = list(quiet = TRUE, pdf = FALSE, png = TRUE, encoding = "UTF-8"), read = NULL, transform = NULL, write = NULL))[[1]][[1]]
  cat(sprintf("\nsemilla %d  exclozetype: %s\n            exsolution: %s   (campos: %d)\n            extol: %s\n", s, e$exclozetype_str, e$exsolution_str,
              length(strsplit(e$exsolution_str, "|", fixed = TRUE)[[1]]), e$extol_str))
  ## tras el exshuffle de R/exams las marcas se permutan: lo que lee R/exams
  cat("            lo que lee R/exams (tras exshuffle):", paste(vapply(ex$metainfo$solution, function(z) if (is.logical(z)) paste0(as.integer(z), collapse = "") else as.character(z), ""), collapse = "|"), "\n")
}
