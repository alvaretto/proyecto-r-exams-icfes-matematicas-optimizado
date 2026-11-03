## capturar.R <rmd> <salida.rds> [semillas]
## Para cada semilla: .Random.seed al final de data_generation y el texto que emiten los chunks
## enunciado, opciones, solucion y feedback (cat/answerlist capturados), más objetos clave.
suppressMessages(library(exams))
a <- commandArgs(TRUE); rmd <- a[1]; out <- a[2]; S <- if (length(a) >= 3) eval(parse(text = a[3])) else 1:1000
l <- readLines(rmd, encoding = "UTF-8")
chunk <- function(nm) { i0 <- grep(paste0("^```\\{r ", nm, "[,}]"), l); i1 <- which(l == "```"); i1 <- i1[i1 > i0][1]
  parse(text = paste(l[(i0 + 1):(i1 - 1)], collapse = "\n"), encoding = "UTF-8") }
DG <- chunk("data_generation"); CH <- lapply(c("enunciado", "opciones", "solucion", "feedback"), chunk)
names(CH) <- c("enunciado", "opciones", "solucion", "feedback")
res <- vector("list", length(S))
for (k in seq_along(S)) {
  env <- new.env(); set.seed(S[k]); eval(DG, envir = env)
  seed <- get(".Random.seed", envir = globalenv())
  txt <- vapply(CH, function(e) paste(capture.output(eval(e, envir = env)), collapse = "\n"), "")
  res[[k]] <- list(seed = seed, txt = txt, canon = env$es_canonica, rama = env$rama, opciones = env$opciones,
                   sol = env$sol, feedback = env$feedback, tabla_md = env$tabla_md, enunciado = env$enunciado,
                   nomG = env$nomG, conj_filas = env$conj$filas)
}
saveRDS(res, out); cat("capturadas", length(res), "semillas en", out, "\n")
