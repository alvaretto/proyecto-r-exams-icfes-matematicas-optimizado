## x6_estratos_t8.R — tanda 8 (2026-10-31): la regla cruzada X6 («más palabras en común con la clave de la Parte 2», batería
## cruzada CONGELADA de L-C2) por estrato de la Parte 1, en las MISMAS muestras de verificar_espacio_cloze.R (mezcla 1..100;
## cuotas de 100 por rama desde la semilla 1001), con la clave de la Parte 2 tal como salió y, además, integrada EXACTAMENTE
## sobre las tres paráfrasis (el sorteo pick(q2$clave) es uniforme): exacta en la Parte 2, condicionada a esas Partes 1. Las
## funciones (rec, correr, toks, mas_comun, sc...) se toman por parse del verificador, sin ejecutarlo. Solo lee.
## Uso (desde cloze/salida): Rscript --no-init-file x6_estratos_t8.R <ruta.Rmd>
suppressMessages(library(exams))
V <- parse(file = "../verificar_espacio_cloze.R", keep.source = FALSE, encoding = "UTF-8")
QUIERO <- c("toks", "cif1", "num_all", "tabla_leida", "sujeto", "rec", "chunk", "MARCA", "cap1", "correr", "mas_comun", "sc")
for (e in V) if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% QUIERO) eval(e, envir = globalenv())
RMD <- normalizePath(commandArgs(TRUE)[1]); EXPR <- parse(text = chunk(RMD), encoding = "UTF-8")
cat("md5 del .Rmd:", unname(tools::md5sum(RMD)), "\n")
N <- 100L; NAT <- lapply(1:N, function(z) rec(correr(EXPR, z)))
CU <- list(CURSOS = list(), PROMEDIO = list()); CAN <- 0L
for (z in 1000L + seq_len(4000L)) { if (length(CU$CURSOS) >= N && length(CU$PROMEDIO) >= N && CAN >= 20L) break
  en <- correr(EXPR, z); x <- rec(en); if (x$canon) { CAN <- CAN + 1L; next }; if (length(CU[[x$rama]]) < N) CU[[x$rama]][[length(CU[[x$rama]]) + 1L]] <- x }
CLAVES <- correr(EXPR, 1L)$P2$promedio$clave
x6_sorteada <- function(L) mean(vapply(L, function(x) sc(mas_comun(x$opciones, x$opciones_p2[x$sol_p2]), x$clave), 0))
x6_exacta <- function(L) mean(vapply(L, function(x) mean(vapply(CLAVES, function(k) sc(mas_comun(x$opciones, k), x$clave), 0)), 0))
x6_por_clave <- function(L) vapply(CLAVES, function(k) mean(vapply(L, function(x) sc(mas_comun(x$opciones, k), x$clave), 0)), 0)
for (nm in c("mezcla", "CURSOS", "PROMEDIO")) { L <- if (nm == "mezcla") NAT else CU[[nm]]
  pc <- x6_por_clave(L)
  cat(sprintf("%-8s n = %3d · X6 con la clave sorteada %.1f %% (%+.1f pp) · exacta sobre las 3 claves %.1f %% (%+.1f pp) · por clave: %s\n", nm, length(L),
              100 * x6_sorteada(L), 100 * (x6_sorteada(L) - 0.25), 100 * x6_exacta(L), 100 * (x6_exacta(L) - 0.25),
              paste(sprintf("%+.1f", 100 * (pc - 0.25)), collapse = " / "))) }
cat("claves (en el orden de «por clave»):\n", paste0("  ", CLAVES, "\n"), sep = "")
