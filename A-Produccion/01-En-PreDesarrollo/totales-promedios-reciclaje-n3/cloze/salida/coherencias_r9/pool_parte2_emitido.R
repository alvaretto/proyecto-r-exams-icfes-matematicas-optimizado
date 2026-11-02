## pool_parte2_emitido.R — Paso 8 (coherencias_5), ronda 9, md5 f98f9dda…
## Reconstruye el pool de la Parte 2 SOLO con lo que R/exams EMITE (xexams, semillas 1:100): las cuatro opciones del gap 2,
## su marca, su retroalimentación y el encabezado/viñetas de la Solution. No abre el .Rmd ni lee P2, valor_p2, op2, expl2,
## clave2_expl. Sirve para que YO juzgue los textos; el juicio queda escrito en verificar_coherencias.R (POOL_P2).
suppressMessages(library(exams))
F <- normalizePath("../../totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd")
cat("md5 del .Rmd:", unname(tools::md5sum(F)), "\n")
nb <- function(x) gsub(" ", " ", x, fixed = TRUE)
ex <- xexams(F, n = 100, seed = 1:100, driver = list(sweave = list(quiet = TRUE, pdf = FALSE, png = TRUE, encoding = "UTF-8"),
                                                     read = NULL, transform = NULL, write = NULL))
filas <- do.call(rbind, lapply(1:100, function(i) {
  e <- ex[[i]][[1]]; ql <- nb(e$questionlist); sl <- nb(e$solutionlist); m <- as.logical(e$metainfo$solution[[2]])
  data.frame(semilla = i, j = 1:4, texto = ql[5:8], marca = m, retro = sl[5:8], stringsAsFactors = FALSE)
}))
cat("\nTextos distintos de opción en la Parte 2 (semillas 1:100):", length(unique(filas$texto)), "\n")
cat("Retroalimentaciones distintas:", length(unique(filas$retro)), "\n")
cat("Pares (texto, retroalimentación) distintos:", nrow(unique(filas[, c("texto", "retro")])), "\n\n")
tab <- aggregate(semilla ~ texto + marca + retro, data = filas, FUN = length)
tab <- tab[order(-tab$marca, tab$texto), ]
for (k in seq_len(nrow(tab))) cat(sprintf("[%d] marca R/exams = %s · veces = %d\n    TEXTO: %s\n    RETRO: %s\n\n", k, tab$marca[k], tab$semilla[k], tab$texto[k], tab$retro[k]))
## una sola marca por versión
cat("Marcas por versión (conteo de TRUE):", paste(names(table(tapply(filas$marca, filas$semilla, sum))), table(tapply(filas$marca, filas$semilla, sum)), sep = ":", collapse = " "), "\n")
## preguntas de la Parte 2
pr <- vapply(1:100, function(i) { q <- nb(ex[[i]][[1]]$question); sub("^\\*\\*Parte 2\\.\\*\\* ", "", grep("^\\*\\*Parte 2\\.\\*\\* ", q, value = TRUE)) }, "")
cat("\nPreguntas de la Parte 2 distintas:\n"); print(table(pr))
## Solution: encabezados 'Parte 2 · ...'
hs <- unlist(lapply(1:100, function(i) { s <- nb(ex[[i]][[1]]$solution); grep("^\\*\\*Parte 2", s, value = TRUE) }))
cat("\nEncabezados que empiezan por «**Parte 2» en la Solution (100 versiones):\n"); print(table(hs))
saveRDS(filas, "pool_parte2_emitido.rds")
