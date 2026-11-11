## Versiones únicas sobre 300 semillas (regla #3: 250+ de 300) y claves distintas. Uso: Rscript --no-init-file salida/paso9_unicas.R <rmd>
suppressMessages(library(exams)); rmd <- commandArgs(TRUE)[1]
L <- readLines(rmd, encoding = "UTF-8"); i0 <- grep("^```\\{r data_generation", L); i1 <- which(L == "```"); i1 <- i1[i1 > i0][1]
CODE <- paste(L[(i0 + 1):(i1 - 1)], collapse = "\n")
v <- vapply(1:300, function(s) { set.seed(s); e <- new.env(); eval(parse(text = CODE), envir = e)
  c(paste(e$contexto, e$regla, e$pregunta, paste(sort(e$opciones), collapse = "|")), e$opciones[e$sol == 1L]) }, c("", ""))
cat(sprintf("md5 %s | versiones únicas: %d de 300 | claves distintas: %d\n", unname(tools::md5sum(rmd)), length(unique(v[1, ])), length(unique(v[2, ]))))
