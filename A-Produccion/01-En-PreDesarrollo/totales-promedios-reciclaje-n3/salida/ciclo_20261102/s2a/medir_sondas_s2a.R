## Tanda S2a (2026-11-03): cuentas de las sondas S1-Sol, viejas y ampliadas, sobre el texto que emite un .Rmd
## (opciones, Solution y feedback), semillas 1..N. Sirve para los controles exactos y para descartar falsos
## positivos de la clase ampliada en V1 (5cf5b12f) y V1' (6c9d7705). Sin escapes \u en el fuente: los caracteres
## especiales se construyen con intToUtf8().
## Uso: Rscript --no-init-file medir_sondas_s2a.R <ej.Rmd> <N> [etiqueta]
suppressPackageStartupMessages(library(exams))
a <- commandArgs(TRUE); rmd <- a[1]; N <- as.integer(a[2]); et <- if (length(a) >= 3) a[3] else basename(rmd)
l <- readLines(rmd, encoding = "UTF-8")
chunk <- function(nm) { i0 <- grep(paste0("^```\\{r ", nm, "[,}]"), l); cs <- which(l == "```"); i1 <- cs[cs > i0][1]
  parse(text = paste(l[(i0 + 1):(i1 - 1)], collapse = "\n"), encoding = "UTF-8") }
C <- lapply(setNames(nm = c("data_generation", "opciones", "solucion", "feedback")), chunk)
SIGC <- paste0("[.,:;!?)\\]", intToUtf8(c(0xbb, 0x201d, 0x2026), multiple = FALSE), "]")
SEP <- "[^A-Za-z0-9$\\n]{0,3}"
pats <- c(
  s1_texto           = "[0-9$] (kilos?|cursos?)\\b|El Bosque|La Pradera",
  unidad_fuera_vieja = paste0("\\$[ ", intToUtf8(0xa0), "]+(kilos?|cursos?)\\b"),
  unidad_fuera_ancha = paste0("\\$", SEP, "(kilos?|cursos?)\\b"),
  signo_fuera_vieja  = paste0("\\\\text\\{(kilos?|cursos?)\\}\\$", SIGC),
  signo_fuera_ancha  = paste0("\\$", SEP, SIGC),
  dolar_signo        = paste0("\\$", SIGC),
  dentro             = paste0("\\\\ \\\\text\\{(kilos?|cursos?)", SIGC, "?\\}\\$"),
  dentro_signo       = paste0("\\\\text\\{(kilos?|cursos?)", SIGC, "\\}\\$"),
  signo_dentro_sin_u = paste0("\\\\text\\{", SIGC, "\\}\\$"),
  esp_especial       = paste0("[", intToUtf8(c(0x2009, 0x202f, 0x2060), multiple = FALSE), "]"))
t0 <- Sys.time()
res <- matrix(0, N, length(pats), dimnames = list(NULL, names(pats)))
gd <- character(N); rama <- character(N); canon <- logical(N); mpadv <- character(N); err <- 0L
for (s in seq_len(N)) {
  env <- new.env(); set.seed(s)
  e <- tryCatch({ eval(C$data_generation, envir = env); NULL }, error = function(x) conditionMessage(x))
  if (!is.null(e)) { err <- err + 1L; next }
  t <- paste(unlist(lapply(C[c("opciones", "solucion", "feedback")], function(x) capture.output(eval(x, envir = env)))), collapse = "\n")
  res[s, ] <- vapply(pats, function(p) lengths(regmatches(t, gregexpr(p, t, perl = TRUE))), 0)
  gd[s] <- env$afs$gdis$codigo; rama[s] <- env$rama; canon[s] <- isTRUE(env$es_canonica); mpadv[s] <- env$afs$padv$molde
}
cat(sprintf("== %s · semillas 1..%d · errores %d · %.2f s por versión\n", et, N, err, as.numeric(difftime(Sys.time(), t0, units = "secs")) / N))
print(colSums(res))
cat("versiones con al menos uno:\n"); print(colSums(res > 0))
cat("distractor de G (gdis):\n"); print(table(gd))
cat(sprintf("ERR-RESTA en gdis: %d · semillas: %s\n", sum(gd == "ERR-RESTA"), paste(which(gd == "ERR-RESTA"), collapse = " ")))
cat("rama x molde de padv:\n"); print(table(rama, mpadv))
cat(sprintf("canónicas: %d (semillas %s)\n", sum(canon), paste(which(canon), collapse = " ")))
cat("ERR-PARCIAL en gdis por rama:\n"); print(table(rama[gd == "ERR-PARCIAL"]))
