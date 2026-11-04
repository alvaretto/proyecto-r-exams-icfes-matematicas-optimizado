## meta_literal.R — Paso 8, r10 (md5 e16842c1…): cadenas exclozetype/exsolution/extol que evalúa el .Rmd por semilla y su coincidencia con lo que lee R/exams.
## Solo imprime las líneas de metadatos y las tres cadenas de metadatos; no lee el banco de la Parte 2. Añade la comprobación General DOK ≥ 3 ⇒ Nivel ≥ 3.
suppressMessages(library(exams))
DIR_S <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))))
F <- normalizePath(file.path(DIR_S, "..", "..", "totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd"))
lin <- readLines(F, encoding = "UTF-8"); a <- grep("^```\\{r data_generation", lin)[1]; b <- which(lin == "```" & seq_along(lin) > a)[1]
cod <- parse(text = paste(lin[(a + 1):(b - 1)], collapse = "\n"), encoding = "UTF-8")
cat("líneas de metadatos del .Rmd:\n"); cat(grep("^(exname|extype|exclozetype|exsolution|extol|expoints|exshuffle|exextra\\[(Nivel|DOK|Bloom|SOLO|Type)\\])", lin, value = TRUE), sep = "\n")
dok <- as.integer(sub("^exextra\\[DOK\\]:\\s*", "", grep("^exextra\\[DOK\\]", lin, value = TRUE)))
niv <- as.integer(sub("^exextra\\[Nivel\\]:\\s*", "", grep("^exextra\\[Nivel\\]", lin, value = TRUE)))
cat(sprintf("\nGeneral: DOK = %d · Nivel = %d · «DOK ≥ 3 ⇒ Nivel ≥ 3»: %s\n", dok, niv, if (dok < 3 || niv >= 3) "se cumple" else "NO SE CUMPLE  <<<<<<"))
for (s in c(7, 3, 2, 1, 4)) {
  set.seed(s); e <- new.env(); eval(cod, envir = e)
  ex <- xexams(F, n = 1, seed = s, driver = list(sweave = list(quiet = TRUE, pdf = FALSE, png = TRUE, encoding = "UTF-8"), read = NULL, transform = NULL, write = NULL))[[1]][[1]]
  cat(sprintf("\nsemilla %d  exclozetype: %s\n            exsolution: %s   (campos: %d)\n            extol: %s\n", s, e$exclozetype_str, e$exsolution_str,
              length(strsplit(e$exsolution_str, "|", fixed = TRUE)[[1]]), e$extol_str))
  ## tras el exshuffle de R/exams las marcas se permutan: lo que lee R/exams
  cat("            lo que lee R/exams (tras exshuffle):", paste(vapply(ex$metainfo$solution, function(z) if (is.logical(z)) paste0(as.integer(z), collapse = "") else as.character(z), ""), collapse = "|"), "\n")
}
