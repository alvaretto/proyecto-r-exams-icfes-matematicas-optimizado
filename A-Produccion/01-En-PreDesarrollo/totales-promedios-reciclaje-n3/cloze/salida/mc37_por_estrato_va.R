## MC37 por estrato: X6 exacta sobre el sorteo de la Parte 2 con el código mutado (semillas 1:200). Solo lee.
suppressMessages(library(exams))
V <- parse(file = "../verificar_espacio_cloze.R", keep.source = FALSE, encoding = "UTF-8")
QUIERO <- c("toks", "cif1", "num_all", "tabla_leida", "sujeto", "rec", "chunk", "MARCA", "cap1", "correr", "mas_comun", "sc",
            "combinaciones_p2", "medir_exacta_p2")
for (e in V) if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% QUIERO) eval(e, envir = globalenv())
RMD <- normalizePath("../totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd"); COD <- chunk(RMD)
x6 <- function(x) mas_comun(x$opciones, x$opciones_p2[x$sol_p2])
sust <- list(c('"Lo que recogió ese grado dividido en partes iguales entre sus cursos."', '"Lo que recogió ese grado dividido en partes iguales entre sus cursos aunque fue menor."'),
             c('"Lo que recogió ese grado dividido entre el número de sus cursos."', '"Lo que recogió ese grado dividido entre el número de sus cursos aunque fue menor."'),
             c('"Lo que recogió ese grado dividido entre la cantidad de cursos de ese mismo grado."', '"Lo que recogió ese grado dividido entre la cantidad de cursos de ese mismo grado aunque fue menor."'))
for (s in sust) { stopifnot(lengths(regmatches(COD, gregexpr(s[1], COD, fixed = TRUE))) == 1L); COD_M <- if (!exists("COD_M")) COD else COD_M; COD_M <- sub(s[1], s[2], COD_M, fixed = TRUE) }
for (nm in c("original", "mutado")) {
  E <- parse(text = if (nm == "original") COD else COD_M, encoding = "UTF-8")
  B <- correr(E, 1L)$P2$promedio
  L <- list(); for (z in 1:200) { en <- correr(E, z); if (is.null(en$.err)) L[[length(L) + 1L]] <- rec(en) }
  est <- list(mezcla = L, CURSOS = Filter(function(x) !x$canon && x$rama == "CURSOS", L),
              PROMEDIO = Filter(function(x) !x$canon && x$rama == "PROMEDIO", L), canonica = Filter(function(x) x$canon, L))
  cat(sprintf("%-8s %s\n", nm, paste(sprintf("%s n=%d %+.1f pp", names(est), lengths(est),
      vapply(est, function(LL) if (length(LL)) 100 * (medir_exacta_p2(LL, x6, B) - 0.25) else NA_real_, 0)), collapse = " · ")))
}
