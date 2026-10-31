## Diagnóstico de la FALLA «sorteo de la Parte 2» de la corrida verificador_va.txt. Solo lee.
suppressMessages(library(exams))
V <- parse(file = "../verificar_espacio_cloze.R", keep.source = FALSE, encoding = "UTF-8")
QUIERO <- c("toks", "cif1", "num_all", "tabla_leida", "sujeto", "rec", "chunk", "MARCA", "cap1", "correr")
for (e in V) if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% QUIERO) eval(e, envir = globalenv())
RMD <- normalizePath("../totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd")
cat("md5:", unname(tools::md5sum(RMD)), "\n")
EXPR <- parse(text = chunk(RMD), encoding = "UTF-8")
B <- correr(EXPR, 1L)$P2$promedio
cat("banco: claves", length(B$clave), "· distractores", length(B$distractores), "\n")
NAT <- lapply(1:100, function(z) rec(correr(EXPR, z)))
cat("clase de sol_p2:", unique(vapply(NAT, function(x) class(x$sol_p2), "")), "\n")
pos <- vapply(NAT, function(x) which(x$sol_p2), 0L)
cat("posición de la clave de la Parte 2 (1..4):", tabulate(pos, 4L), "\n")
## La comprobación tal como la escribió DeepSeek (con -sol_p2) y con la negación lógica (!sol_p2).
chk <- function(x, neg) { k <- x$opciones_p2[x$sol_p2]; d <- if (neg) x$opciones_p2[!x$sol_p2] else x$opciones_p2[-x$sol_p2]
  length(k) == 1L && k %in% B$clave && all(d %in% B$distractores) && length(unique(c(k, d))) == 4L }
v_menos <- vapply(NAT, chk, NA, neg = FALSE); v_neg <- vapply(NAT, chk, NA, neg = TRUE)
cat("con -sol_p2: pasan", sum(v_menos), "de 100; las que pasan tienen la clave en la posición:", unique(pos[v_menos]), "\n")
cat("con !sol_p2: pasan", sum(v_neg), "de 100\n")
x <- NAT[[which(pos != 1L)[1]]]
cat("ejemplo, sol_p2 =", x$sol_p2, " -> -sol_p2 =", -x$sol_p2, " -> quedan:", sum(seq_along(x$opciones_p2) %in% setdiff(1:4, 1L)), "opciones (siempre sin la 1)\n")
