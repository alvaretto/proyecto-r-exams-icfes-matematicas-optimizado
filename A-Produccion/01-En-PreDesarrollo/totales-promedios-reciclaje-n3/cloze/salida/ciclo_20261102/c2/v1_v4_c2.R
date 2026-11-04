## v1_v4_c2.R — paso 10 del CLOZE, tanda C2: V1 a V4 del orquestador-cloze sobre el texto EMITIDO (xexams, N = 100, semillas 1:100).
## Cada sonda tiene su control positivo: una alteración del texto o de las marcas emitidas que debe hacerla fallar, y solo a ella.
## Solo lee. Se corre con el directorio de trabajo fuera del repo: Rscript --no-init-file <ruta>/v1_v4_c2.R
args0 <- commandArgs(FALSE); dir_s <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", args0, value = TRUE))))
suppressMessages(library(exams))
F <- normalizePath(file.path(dir_s, "..", "..", "..", "totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd"))
cat("md5 del .Rmd:", unname(tools::md5sum(F)), "· N = 100 (semillas 1:100)\n")
ex <- xexams(F, n = 100, seed = 1:100, driver = list(sweave = list(quiet = TRUE, pdf = FALSE, png = TRUE, encoding = "UTF-8"),
                                                     read = NULL, transform = NULL, write = NULL))
parte <- function(q) grep("^\\*\\*Parte [0-9]+\\.\\*\\* ", q)
v1 <- function(q, tipos) {
  a <- unique(unlist(regmatches(q, gregexpr("##ANSWER[0-9]+##", q))))
  length(a) == length(tipos) && length(a) == length(parte(q))
}
v2 <- function(q) {
  p <- parte(q); n <- length(p); a <- match(paste0("##ANSWER", seq_len(n), "##"), q)
  if (anyNA(a) || is.unsorted(p) || is.unsorted(a)) return(FALSE)
  sig <- c(p[-1], length(q) + 1L)
  all(a > p & a < sig) && all(vapply(seq_len(n), function(i) !any(nzchar(trimws(q[seq.int(p[i] + 1L, length.out = a[i] - p[i] - 1L)]))), NA))
}
v3 <- function(tipos, sol, tol, ql) {
  ok <- length(tipos) == length(sol) && sum(lengths(sol)) == length(ql)
  for (i in seq_along(tipos)) {
    s <- sol[[i]]
    ok <- ok && switch(tipos[i],
      schoice = is.logical(s) && !anyNA(s) && length(s) >= 2L && sum(s) == 1L,
      mchoice = is.logical(s) && !anyNA(s) && length(s) >= 2L && sum(s) >= 1L && sum(s) < length(s),
      num     = is.numeric(s) && length(s) == 1L && is.finite(s) && length(tol[[i]]) >= 1L && all(is.finite(tol[[i]])) && all(tol[[i]] >= 0),
      string  = is.character(s) && length(s) == 1L && nzchar(s), FALSE)
  }
  ok
}
v4 <- function(q) length(parte(q)) >= 6L
r <- t(vapply(ex, function(x) {
  e <- x[[1]]; q <- e$question; m <- e$metainfo
  c(V1 = v1(q, m$clozetype), V2 = v2(q), V3 = v3(m$clozetype, m$solution, as.list(m$tol), e$questionlist), V4 = v4(q))
}, c(V1 = NA, V2 = NA, V3 = NA, V4 = NA)))
cat("Versiones que cumplen, de 100:\n"); print(colSums(r))
e1 <- ex[[1]][[1]]; m1 <- e1$metainfo
cat("exclozetype emitido:", paste(unique(vapply(ex, function(x) paste(x[[1]]$metainfo$clozetype, collapse = "|"), "")), collapse = " ; "), "\n")
cat("partes:", length(parte(e1$question)), "· expoints:", paste(m1$points, collapse = "|"), "· tolerancias (semilla 1):", paste(unlist(m1$tol), collapse = " "), "\n")
## controles positivos, cada uno sobre la semilla 1
q <- e1$question; ty <- m1$clozetype; so <- m1$solution; tl <- as.list(m1$tol); ql <- e1$questionlist
q_a <- q[q != "##ANSWER4##"]
c1 <- !v1(q_a, ty)
i3 <- match("##ANSWER3##", q); q_b <- q; q_b <- q_b[-i3]; q_b <- append(q_b, "##ANSWER3##", after = match("##ANSWER4##", q_b))
c2 <- !v2(q_b) && v1(q_b, ty)
s_c <- so; s_c[[1]][] <- TRUE
c3 <- !v3(ty, s_c, tl, ql)
s_d <- so; s_d[[6]][] <- FALSE
c3b <- !v3(ty, s_d, tl, ql)
t_e <- tl; t_e[[3]] <- -1
c3c <- !v3(ty, so, t_e, ql)
q_f <- q; ip <- parte(q)[6]; q_f[ip] <- sub("^\\*\\*Parte 6\\.\\*\\* ", "Parte seis: ", q_f[ip])
c4 <- !v4(q_f) && v1(q, ty)
ctl <- c("V1 sin ##ANSWER4##" = c1, "V2 ##ANSWER3## tras la Parte 4 (V1 sigue en verde)" = c2, "V3 schoice con cuatro marcas" = c3,
         "V3 mchoice sin marcas" = c3b, "V3 num con tolerancia negativa" = c3c, "V4 cinco encabezados de parte" = c4)
cat("\nControles positivos (TRUE = la sonda dispara):\n"); for (n in names(ctl)) cat(sprintf("  %-55s %s\n", n, ctl[[n]]))
fallos <- sum(!r) + sum(!ctl)
cat(sprintf("\nTOTAL fallos: %d\n", fallos)); quit(status = if (fallos) 1L else 0L, save = "no")
