## muestra_parrafo_c1b.R — tanda C1b: un ejemplo del párrafo «Parte 6.» nuevo por rama del otro colegio y combinación de
## veredictos (HP, HG), más la semilla 10 que citó el coordinador. Solo lee. Uso: cd cloze/salida && Rscript --no-init-file ...
suppressMessages(library(exams))
RMD <- normalizePath("../totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd")
chunk <- function(f, nm) { l <- readLines(f, encoding = "UTF-8"); i0 <- grep(paste0("^```\\{r ", nm, "[,}]"), l)[1]
  i1 <- which(l == "```" & seq_along(l) > i0)[1]; paste(l[(i0 + 1):(i1 - 1)], collapse = "\n") }
DG <- parse(text = chunk(RMD, "data_generation"), encoding = "UTF-8"); SOL <- parse(text = chunk(RMD, "solucion"), encoding = "UTF-8")
NB <- intToUtf8(0xA0)
vistos <- character(0)
for (s in c(10L, 1:400)) {
  set.seed(s); en <- new.env(); eval(DG, envir = en)
  k <- sprintf("%s · HP %s · HG %s", en$rama_par, en$v_hp, en$v_hg)
  if (s != 10L && k %in% vistos) next
  vistos <- c(vistos, k)
  t <- paste(capture.output(eval(SOL, envir = new.env(parent = en))), collapse = "\n")
  p <- regmatches(t, regexpr("Parte 6\\. Cada hipótesis[^\n]*", t, perl = TRUE))
  cat(sprintf("semilla %d · %s · D' = %d, a'_G = %d, a'_P = %d\n  %s\n\n", s, k, en$dp$D, en$dp$aG, en$dp$aP, gsub(NB, "~", p, fixed = TRUE)))
  if (length(vistos) >= 7L) break
}
