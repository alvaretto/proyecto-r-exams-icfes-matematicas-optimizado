## INCIDENTE U / Error 30: la bateria agrega sobre versiones SIN condicionar por
## rama. Con clave alternante las dos ramas son estructuralmente distintas y un
## reparto 100 %/0 % se lee como ~50 %. Aqui se mide DENTRO de cada rama.
R_RAIZ <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams"
source(file.path(R_RAIZ, ".claude/scripts/bateria_eliminacion.R"))
rmd <- "tasa_caminata_velocidad_maxima_numerico_variacional_argumentacion_n4_cloze_v1.Rmd"
ln  <- readLines(rmd, warn = FALSE)
i0  <- grep("^```\\{r data_generation", ln)[1]
i1  <- i0 + which(grepl("^```\\s*$", ln[(i0+1):length(ln)]))[1]
expr <- parse(text = paste(ln[(i0+1):(i1-1)], collapse="\n"))

## N=400 para que el estrato MENOR supere el minimo de 20 (regla #23): con 100
## la rama ligera ronda 40, suficiente, pero 400 deja margen y el coste es bajo.
N <- 400L
op4 <- op5 <- vector("list", N); k4 <- k5 <- integer(N); rama <- logical(N)
for (s in seq_len(N)) {
  set.seed(s * 6421L)
  e <- new.env(parent = globalenv()); eval(expr, envir = e)
  op4[[s]] <- e$opciones_p4; k4[s] <- which(e$sol_p4 == 1L)
  op5[[s]] <- e$opciones_p5; k5[s] <- which(e$sol_p5 == 1L)
  rama[s] <- isTRUE(e$excede)
}
nums <- function(t) sort(as.numeric(regmatches(t, gregexpr("[0-9]+", t))[[1]]))
uniq_ext <- function(v, fn) { if (all(is.na(v))) return(NA_integer_)
  i <- which(v == fn(v, na.rm=TRUE)); if (length(i)==1L) i else NA_integer_ }
score <- function(o, k, S) { if (length(S)==1L && is.na(S[1])) return(1/length(o))
  if (is.logical(S)) S <- which(S); if (!length(S)) return(1/length(o))
  if (k %in% S) 1/length(S) else 0 }
reglas <- list(
  "la mas larga"  = function(o) uniq_ext(nchar(o), max),
  "la mas corta"  = function(o) uniq_ext(nchar(o), min),
  "par mismos numerales" = function(o){ key<-vapply(lapply(o,nums),function(x)paste(x,collapse="-"),"")
      tb<-table(key); g<-names(tb)[tb>=2L]; if(length(g)!=1L) NA_integer_ else which(key==g) },
  "cita vel y cota (atajo CLOZE)" = NULL
)
for (nm in c("p4","p5")) {
  O <- if (nm=="p4") op4 else op5; K <- if (nm=="p4") k4 else k5
  cat("\n=== gap", nm, "— por rama (n total", N, ") ===\n")
  for (rn in c(TRUE, FALSE)) {
    idx <- which(rama == rn); et <- if (rn) "supera la cota" else "cabe en la cota"
    if (length(idx) < 20L) { cat(sprintf("  rama '%s': n=%d < 20 -> NO CONCLUYENTE\n", et, length(idx))); next }
    cat(sprintf("  rama '%-16s' n=%3d   ", et, length(idx)))
    out <- character(0)
    for (rnm in names(reglas)) {
      f <- reglas[[rnm]]; if (is.null(f)) next
      sc <- mean(vapply(idx, function(s) score(O[[s]], K[s], f(O[[s]])), 0))
      out <- c(out, sprintf("%s %.1f%%", rnm, 100*sc))
    }
    cat(paste(out, collapse=" | "), "\n")
  }
}
cat("\nnulo exacto de cualquier regla con 4 opciones: 25.0 %\n")
