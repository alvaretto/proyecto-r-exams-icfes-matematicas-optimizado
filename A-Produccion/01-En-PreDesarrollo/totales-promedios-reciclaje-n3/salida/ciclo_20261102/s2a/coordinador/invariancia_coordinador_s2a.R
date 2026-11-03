## Prueba independiente del coordinador (2026-11-03): .Rmd de HEAD (4cffbe8b) frente al .Rmd en disco (S1b relanzada).
## No reutiliza los scripts de la S1b. Uso: Rscript --no-init-file invariancia_coordinador.R <viejo.Rmd> <nuevo.Rmd> [N]
suppressPackageStartupMessages(library(exams))
a <- commandArgs(trailingOnly = TRUE)
viejo <- a[1]; nuevo <- a[2]; N <- if (length(a) >= 3) as.integer(a[3]) else 1000L
trozos <- function(f) {
  l <- readLines(f, encoding = "UTF-8")
  uno <- function(nm) {
    i0 <- grep(paste0("^```\\{r ", nm, "[,}]"), l); stopifnot(length(i0) == 1L)
    cs <- which(l == "```"); i1 <- cs[cs > i0][1]
    parse(text = paste(l[(i0 + 1):(i1 - 1)], collapse = "\n"), encoding = "UTF-8")
  }
  nms <- c("data_generation", "enunciado", "opciones", "solucion", "feedback")
  setNames(lapply(nms, uno), nms)
}
CV <- trozos(viejo); CN <- trozos(nuevo)
correr <- function(C, s) {
  env <- new.env(); set.seed(s)
  err <- tryCatch({ eval(C$data_generation, envir = env); NULL }, error = function(e) conditionMessage(e))
  if (!is.null(err)) return(list(err = err))
  sem <- .Random.seed
  txt <- vapply(c("enunciado", "opciones", "solucion", "feedback"), function(nm)
    paste(capture.output(eval(C[[nm]], envir = env)), collapse = "\n"), "")
  obj <- mget(c("opciones", "sol", "rama", "es_canonica", "tabla_md", "enunciado"), envir = env, ifnotfound = list(NULL))
  list(err = NULL, sem = sem, txt = txt, obj = obj)
}
SIG <- "[.,:;!?)\\]»”…]"
## Lleva el texto nuevo a la forma vieja: «\ \text{kilos.}$» -> «$<U+00A0>kilos.»
a_viejo <- function(t) gsub(paste0("\\\\text\\{(", SIG, ")\\}\\$"), "$\\1", t, perl = TRUE)
cuenta <- function(p, t) sum(lengths(regmatches(t, gregexpr(p, t, perl = TRUE))))
r <- list(err = 0L, sem = 0L, obj = 0L, enun = 0L, opc = 0L, sol_bb = 0L, sol_norm = 0L, fb_bb = 0L, fb_norm = 0L,
          fuera_nuevo = 0L, signo_fuera_nuevo = 0L, dentro_nuevo = 0L, signo_dentro_nuevo = 0L, fuera_viejo = 0L)
for (s in seq_len(N)) {
  v <- correr(CV, s); n <- correr(CN, s)
  if (!is.null(v$err) || !is.null(n$err)) { r$err <- r$err + 1L; next }
  r$sem  <- r$sem  + identical(v$sem, n$sem)
  r$obj  <- r$obj  + identical(v$obj, n$obj)
  r$enun <- r$enun + identical(v$txt[["enunciado"]], n$txt[["enunciado"]])
  r$opc  <- r$opc  + identical(v$txt[["opciones"]], n$txt[["opciones"]])
  r$sol_bb  <- r$sol_bb  + identical(v$txt[["solucion"]], n$txt[["solucion"]])
  r$sol_norm <- r$sol_norm + identical(v$txt[["solucion"]], a_viejo(n$txt[["solucion"]]))
  r$fb_bb   <- r$fb_bb   + identical(v$txt[["feedback"]], n$txt[["feedback"]])
  r$fb_norm <- r$fb_norm + identical(v$txt[["feedback"]], a_viejo(n$txt[["feedback"]]))
  tn <- paste(n$txt, collapse = "\n"); tv <- paste(v$txt, collapse = "\n")
  r$fuera_nuevo <- r$fuera_nuevo + cuenta("\\$[  ]+(kilos?|cursos?)\\b", tn)
  r$signo_fuera_nuevo <- r$signo_fuera_nuevo + cuenta(paste0("\\\\text\\{(kilos?|cursos?)\\}\\$", SIG), tn)
  r$dentro_nuevo <- r$dentro_nuevo + cuenta("\\\\text\\{(kilos?|cursos?)", tn)
  r$signo_dentro_nuevo <- r$signo_dentro_nuevo + cuenta(paste0("\\\\text\\{(kilos?|cursos?)", SIG, "\\}\\$"), tn)
  r$fuera_viejo <- r$fuera_viejo + cuenta("\\$[  ]+(kilos?|cursos?)\\b", tv)
}
cat(sprintf("semillas: %d · errores de ejecución: %d\n", N, r$err))
cat(sprintf(".Random.seed idéntico tras data_generation: %d / %d\n", r$sem, N))
cat(sprintf("objetos (opciones, sol, rama, es_canonica, tabla_md, enunciado) idénticos: %d / %d\n", r$obj, N))
cat(sprintf("texto emitido, enunciado idéntico byte a byte: %d / %d · opciones: %d / %d\n", r$enun, N, r$opc, N))
cat(sprintf("Solution: byte a byte %d / %d · tras volver a la forma vieja %d / %d\n", r$sol_bb, N, r$sol_norm, N))
cat(sprintf("feedback: byte a byte %d / %d · tras volver a la forma vieja %d / %d\n", r$fb_bb, N, r$fb_norm, N))
cat(sprintf("nuevo: fórmula que cierra antes de su unidad %d · signo fuera tras \\text{unidad}$ %d\n", r$fuera_nuevo, r$signo_fuera_nuevo))
cat(sprintf("nuevo: unidad dentro de la fórmula %d (con signo dentro %d) · control viejo, fórmula que cierra antes de la unidad: %d\n",
            r$dentro_nuevo, r$signo_dentro_nuevo, r$fuera_viejo))
