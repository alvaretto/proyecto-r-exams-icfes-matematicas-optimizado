## pool_parte2_ciego.R — Paso 8 (coherencias_5), oráculo r10, tanda C2, .Rmd md5 e16842c1…
## Endurecimiento de L-C8: vuelca el banco de la Parte 2 SIN las marcas de R/exams. Solo lo que R/exams EMITE (xexams, semillas
## 1:100): la pregunta y los textos distintos de las opciones del gap 2, en orden alfabético. No imprime la marca, ni la
## retroalimentación (empieza por «Verdadero.» o «Falso.»), ni las frecuencias (las claves salen menos veces que los distractores).
## Se corre con el directorio de trabajo FUERA del repo y escribe solo en la carpeta de este script.
args0 <- commandArgs(FALSE); dir_s <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", args0, value = TRUE))))
suppressMessages(library(exams))
F <- normalizePath(file.path(dir_s, "..", "..", "totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd"))
nbsp <- intToUtf8(160L); nb <- function(x) gsub(nbsp, " ", x, fixed = TRUE)
ex <- xexams(F, n = 100, seed = 1:100, driver = list(sweave = list(quiet = TRUE, pdf = FALSE, png = TRUE, encoding = "UTF-8"),
                                                     read = NULL, transform = NULL, write = NULL))
op <- unlist(lapply(1:100, function(i) nb(ex[[i]][[1]]$questionlist)[5:8]))
pr <- vapply(1:100, function(i) { q <- nb(ex[[i]][[1]]$question); sub("^\\*\\*Parte 2\\.\\*\\* ", "", grep("^\\*\\*Parte 2\\.\\*\\* ", q, value = TRUE)) }, "")
pr_g <- unique(gsub("promedio de \\S+ recogido", "promedio de <material> recogido", pr))
sal <- c(paste("md5 del .Rmd:", unname(tools::md5sum(F))),
         paste("Semillas 1:100 ·", length(op), "opciones emitidas ·", length(unique(op)), "textos distintos"),
         "Preguntas (con el material generalizado):", paste0("  ", pr_g), "Textos, en orden alfabético, sin marca:",
         paste0("  ", sort(unique(op), method = "radix")))
writeLines(sal, file.path(dir_s, "pool_parte2_ciego.txt"))
cat(sal, sep = "\n"); cat("\n")
