## Coordinador, 2026-11-03: comprobación independiente de la tanda C1 del CLOZE (no reutiliza sus scripts).
## (A) E1: la Parte 1 del CLOZE nuevo es idéntica al SCHOICE aprobado (mismos objetos para la misma semilla).
## (B) El CLOZE nuevo frente al aprobado (f98f9dda): .Random.seed final y objetos de las Partes 2 a 6 idénticos.
## Uso (desde cloze/): Rscript --no-init-file invariancia_cloze_coordinador.R <schoice.Rmd> <cloze_viejo.Rmd> <cloze_nuevo.Rmd> [N]
suppressPackageStartupMessages(library(exams))
a <- commandArgs(trailingOnly = TRUE); N <- if (length(a) >= 4) as.integer(a[4]) else 1000L
dg <- function(f) { l <- readLines(f, encoding = "UTF-8"); i0 <- grep("^```\\{r data_generation", l); cs <- which(l == "```")
  i1 <- cs[cs > i0][1]; parse(text = paste(l[(i0 + 1):(i1 - 1)], collapse = "\n"), encoding = "UTF-8") }
ES <- dg(a[1]); EV <- dg(a[2]); EN <- dg(a[3])
corre <- function(E, s) { e <- new.env(); set.seed(s)
  err <- tryCatch({ eval(E, envir = e); NULL }, error = function(x) conditionMessage(x))
  list(e = e, err = err, sem = if (is.null(err)) .Random.seed else NULL) }
P1_S <- c("opciones", "sol", "feedback", "rama", "es_canonica", "tabla_md", "respuesta_correcta")
P1_C <- c("opciones_p1", "sol_p1", "feedback_p1", "rama", "es_canonica", "tabla_md", "respuesta_correcta")
P26 <- c("rama_par", "cascada_par", "tabla_par_md", "intro_par", "tipo_p2", "texto_p2", "opciones_p2", "sol_p2",
         "texto_p3", "texto_p4", "texto_p5", "exsol_p3", "exsol_p4", "exsol_p5", "texto_p6", "opciones_p6", "sol_p6", "rol_p6")
r <- c(err = 0, e1 = 0, sem = 0, p26 = 0); dif_p26 <- character(0); dif_e1 <- character(0)
for (s in seq_len(N)) {
  S <- corre(ES, s); V <- corre(EV, s); C <- corre(EN, s)
  if (!is.null(S$err) || !is.null(V$err) || !is.null(C$err)) { r["err"] <- r["err"] + 1; next }
  oS <- mget(P1_S, envir = S$e, ifnotfound = list(NULL)); oC <- mget(P1_C, envir = C$e, ifnotfound = list(NULL))
  igual <- mapply(identical, unname(oS), unname(oC)); if (all(igual)) r["e1"] <- r["e1"] + 1 else dif_e1 <- c(dif_e1, P1_S[!igual])
  r["sem"] <- r["sem"] + identical(V$sem, C$sem)
  pV <- mget(P26, envir = V$e, ifnotfound = list(NULL)); pC <- mget(P26, envir = C$e, ifnotfound = list(NULL))
  ig <- mapply(identical, pV, pC); if (all(ig)) r["p26"] <- r["p26"] + 1 else dif_p26 <- c(dif_p26, P26[!ig])
}
cat(sprintf("semillas: %d · errores de ejecución: %d\n", N, r[["err"]]))
cat(sprintf("(A) E1, Parte 1 del CLOZE nuevo = SCHOICE (%s): %d / %d\n", paste(P1_S, collapse = ", "), r[["e1"]], N))
if (length(dif_e1)) print(table(dif_e1))
cat(sprintf("(B) .Random.seed final idéntico al CLOZE aprobado: %d / %d\n", r[["sem"]], N))
cat(sprintf("(B) Partes 2 a 6 idénticas (%d objetos): %d / %d\n", length(P26), r[["p26"]], N))
if (length(dif_p26)) print(table(dif_p26))
existen <- vapply(P26, exists, TRUE, envir = corre(EN, 1)$e); cat("objetos de P26 que existen en el CLOZE nuevo:", sum(existen), "de", length(P26), "\n")
