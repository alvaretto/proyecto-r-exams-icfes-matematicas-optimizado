## Tanda S2a (2026-11-03): invariancia del .Rmd de HEAD (6c9d7705) frente al .Rmd con H2 aplicado, semillas 1..N.
## Comprueba .Random.seed, objetos, enunciado, opciones y tabla byte a byte, y Solution y feedback tras devolver el
## signo a su sitio viejo («\text{.}$» -> «$.»). Cuenta, en el texto emitido (opciones, Solution, feedback), las
## fórmulas seguidas de un signo fuera de ellas, con unidad o sin ella. Sin escapes \u: intToUtf8().
## Uso: Rscript --no-init-file invariancia_s2a.R <viejo.Rmd> <nuevo.Rmd> [N]
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
  obj <- mget(c("opciones", "sol", "rama", "es_canonica", "tabla_md", "tabla_df", "enunciado", "feedback"), envir = env,
              ifnotfound = list(NULL))
  list(err = NULL, sem = sem, txt = txt, obj = obj, gd = env$afs$gdis$codigo)
}
SIG <- paste0("[.,:;!?)\\]", intToUtf8(c(0xbb, 0x201d, 0x2026), multiple = FALSE), "]")
SEP <- "[^A-Za-z0-9$\\n]{0,3}"
a_viejo <- function(t) gsub(paste0("\\\\text\\{(", SIG, ")\\}\\$"), "$\\1", t, perl = TRUE)
cuenta <- function(p, t) sum(lengths(regmatches(t, gregexpr(p, t, perl = TRUE))))
ESP <- paste0("[", intToUtf8(c(0xa0, 0x2009, 0x202f, 0x2060), multiple = FALSE), "]")
r <- list(err = 0L, sem = 0L, obj = 0L, enun = 0L, opc = 0L, tabla = 0L, sol_bb = 0L, sol_norm = 0L, fb_bb = 0L,
          fb_norm = 0L, sol_dif_resta = 0L, sol_dif_otra = 0L, resta = 0L,
          n_signo_fuera = 0L, n_signo_fuera_v = 0L, n_unidad_fuera = 0L, n_signo_sin_u = 0L, n_dentro = 0L,
          n_dentro_signo = 0L, v_signo_fuera = 0L, v_signo_fuera_v = 0L, v_unidad_fuera = 0L, v_signo_sin_u = 0L,
          n_esp_fuera_tabla_nuevo = 0L)
for (s in seq_len(N)) {
  v <- correr(CV, s); n <- correr(CN, s)
  if (!is.null(v$err) || !is.null(n$err)) { r$err <- r$err + 1L; cat("ERROR semilla", s, ":", v$err, n$err, "\n"); next }
  r$sem  <- r$sem  + identical(v$sem, n$sem)
  r$obj  <- r$obj  + identical(v$obj[names(v$obj) != "feedback"], n$obj[names(n$obj) != "feedback"])
  r$enun <- r$enun + identical(v$txt[["enunciado"]], n$txt[["enunciado"]])
  r$opc  <- r$opc  + identical(v$txt[["opciones"]], n$txt[["opciones"]])
  r$tabla <- r$tabla + (identical(v$obj$tabla_md, n$obj$tabla_md) && identical(v$obj$tabla_df, n$obj$tabla_df))
  sbb <- identical(v$txt[["solucion"]], n$txt[["solucion"]])
  r$sol_bb  <- r$sol_bb  + sbb
  r$sol_norm <- r$sol_norm + identical(v$txt[["solucion"]], a_viejo(n$txt[["solucion"]]))
  es_resta <- identical(n$gd, "ERR-RESTA"); r$resta <- r$resta + es_resta
  if (!sbb) { if (es_resta) r$sol_dif_resta <- r$sol_dif_resta + 1L else r$sol_dif_otra <- r$sol_dif_otra + 1L }
  r$fb_bb   <- r$fb_bb   + identical(v$txt[["feedback"]], n$txt[["feedback"]])
  r$fb_norm <- r$fb_norm + identical(v$txt[["feedback"]], a_viejo(n$txt[["feedback"]]))
  tn <- paste(n$txt[c("opciones", "solucion", "feedback")], collapse = "\n")
  tv <- paste(v$txt[c("opciones", "solucion", "feedback")], collapse = "\n")
  k <- cuenta(paste0("\\$", SEP, SIG), tn); r$n_signo_fuera <- r$n_signo_fuera + k; r$v_signo_fuera <- r$v_signo_fuera + (k > 0)
  k <- cuenta(paste0("\\$", SEP, SIG), tv); r$n_signo_fuera_v <- r$n_signo_fuera_v + k; r$v_signo_fuera_v <- r$v_signo_fuera_v + (k > 0)
  k <- cuenta(paste0("\\$", SEP, "(kilos?|cursos?)\\b"), tn); r$n_unidad_fuera <- r$n_unidad_fuera + k; r$v_unidad_fuera <- r$v_unidad_fuera + (k > 0)
  k <- cuenta(paste0("\\\\text\\{", SIG, "\\}\\$"), tn); r$n_signo_sin_u <- r$n_signo_sin_u + k; r$v_signo_sin_u <- r$v_signo_sin_u + (k > 0)
  r$n_dentro <- r$n_dentro + cuenta(paste0("\\\\ \\\\text\\{(kilos?|cursos?)", SIG, "?\\}\\$"), tn)
  r$n_dentro_signo <- r$n_dentro_signo + cuenta(paste0("\\\\text\\{(kilos?|cursos?)", SIG, "\\}\\$"), tn)
  r$n_esp_fuera_tabla_nuevo <- r$n_esp_fuera_tabla_nuevo + cuenta(paste0("[", intToUtf8(c(0x2009, 0x202f, 0x2060), multiple = FALSE), "]"),
                                                                 paste(n$txt, collapse = "\n"))
}
cat(sprintf("viejo: %s\nnuevo: %s\n", viejo, nuevo))
cat(sprintf("semillas: %d · errores de ejecución: %d\n", N, r$err))
cat(sprintf(".Random.seed idéntico tras data_generation: %d / %d\n", r$sem, N))
cat(sprintf("objetos (opciones, sol, rama, es_canonica, tabla_md, tabla_df, enunciado) idénticos: %d / %d\n", r$obj, N))
cat(sprintf("texto emitido, enunciado idéntico byte a byte: %d / %d · opciones: %d / %d · tabla (tabla_md y tabla_df): %d / %d\n",
            r$enun, N, r$opc, N, r$tabla, N))
cat(sprintf("Solution: byte a byte %d / %d (difieren %d, todas con ERR-RESTA: %d; con otro distractor de G: %d) · tras devolver el signo a su sitio %d / %d\n",
            r$sol_bb, N, N - r$sol_bb, r$sol_dif_resta, r$sol_dif_otra, r$sol_norm, N))
cat(sprintf("feedback: byte a byte %d / %d · tras devolver el signo a su sitio %d / %d\n", r$fb_bb, N, r$fb_norm, N))
cat(sprintf("versiones con ERR-RESTA como distractor de G: %d\n", r$resta))
cat(sprintf("nuevo: fórmula seguida de un signo fuera de ella, con o sin unidad: %d (en %d versiones)\n", r$n_signo_fuera, r$v_signo_fuera))
cat(sprintf("nuevo: fórmula que cierra antes de su unidad (clase ampliada): %d (en %d versiones)\n", r$n_unidad_fuera, r$v_unidad_fuera))
cat(sprintf("nuevo: signo dentro de una fórmula sin unidad («\\text{.}$»): %d (en %d versiones)\n", r$n_signo_sin_u, r$v_signo_sin_u))
cat(sprintf("nuevo: unidad dentro de la fórmula %d (con el signo dentro %d)\n", r$n_dentro, r$n_dentro_signo))
cat(sprintf("nuevo: U+2009, U+202F o U+2060 en todo el texto emitido: %d\n", r$n_esp_fuera_tabla_nuevo))
cat(sprintf("control positivo, viejo: fórmula seguida de un signo fuera de ella: %d (en %d versiones)\n", r$n_signo_fuera_v, r$v_signo_fuera_v))
