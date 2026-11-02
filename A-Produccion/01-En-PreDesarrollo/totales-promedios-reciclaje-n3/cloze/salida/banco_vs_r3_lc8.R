## ¿El oráculo de la r3 (coherencias_r3) juzgó las redacciones del banco actual de la Parte 2? Solo lee.
suppressMessages(library(exams))
V <- parse(file = "../verificar_espacio_cloze.R", keep.source = FALSE, encoding = "UTF-8")
for (e in V) if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% c("chunk", "correr")) eval(e, envir = globalenv())
B <- correr(parse(text = chunk(normalizePath("../totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd")), encoding = "UTF-8"), 1L)$P2$promedio
R3 <- parse(file = "coherencias_r3/verificar_coherencias.R", keep.source = FALSE, encoding = "UTF-8")
for (e in R3) if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% c("P2_OK", "P2_MAL", "P2_PREG")) eval(e, envir = globalenv())
juzgadas <- unique(c(unlist(P2_OK), unlist(P2_MAL)))
cat("banco actual:", length(B$clave), "claves y", length(B$distractores), "distractores\n")
cat("claves juzgadas por la r3:", sum(B$clave %in% unlist(P2_OK)), "de", length(B$clave), "\n")
cat("distractores juzgados por la r3:", sum(B$distractores %in% unlist(P2_MAL)), "de", length(B$distractores), "\n")
cat("redacciones sin juicio en la r3:\n"); print(unname(c(B$clave, B$distractores)[!c(B$clave, B$distractores) %in% juzgadas]))
