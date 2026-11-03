## Coordinador, 2026-11-03: signos que siguen al cierre de CUALQUIER fórmula, en el texto emitido (semillas 1..N).
suppressPackageStartupMessages(library(exams))
a <- commandArgs(trailingOnly = TRUE); N <- as.integer(a[2])
l <- readLines(a[1], encoding = "UTF-8")
ch <- function(nm) { i0 <- grep(paste0("^```\\{r ", nm, "[,}]"), l); cs <- which(l == "```"); i1 <- cs[cs > i0][1]
  parse(text = paste(l[(i0 + 1):(i1 - 1)], collapse = "\n"), encoding = "UTF-8") }
C <- lapply(c("data_generation", "enunciado", "opciones", "solucion", "feedback"), ch)
SIG <- "[.,:;!?)\\]»”…]"
## Cierre de fórmula: un $ precedido de algo que no es espacio ni inicio (un $ de apertura va tras espacio, «(» o inicio).
P_FUERA <- paste0("(?<=[^\\s(])\\$[^A-Za-z0-9$\\n]{0,3}", SIG)
P_DENTRO_SIN_UNIDAD <- paste0("\\\\text\\{", SIG, "\\}\\$")
f <- 0L; d <- 0L; semillas_f <- 0L
for (s in seq_len(N)) { e <- new.env(); set.seed(s); eval(C[[1]], envir = e)
  t <- paste(unlist(lapply(C[2:5], function(x) capture.output(eval(x, envir = e)))), collapse = "\n")
  k <- lengths(regmatches(t, gregexpr(P_FUERA, t, perl = TRUE))); f <- f + k; semillas_f <- semillas_f + (k > 0)
  d <- d + lengths(regmatches(t, gregexpr(P_DENTRO_SIN_UNIDAD, t, perl = TRUE))) }
cat(sprintf("%s · semillas %d: signo fuera tras el cierre de una fórmula %d (en %d semillas) · signo dentro de una fórmula sin unidad %d\n",
            basename(a[1]), N, f, semillas_f, d))
