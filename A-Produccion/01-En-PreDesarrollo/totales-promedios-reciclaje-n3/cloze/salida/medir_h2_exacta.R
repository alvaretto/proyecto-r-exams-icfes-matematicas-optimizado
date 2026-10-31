#!/usr/bin/env Rscript
## medir_h2_exacta.R — tanda 5 (2026-10-31), paso A: las cifras de H2 (Parte 6) por enumeración EXACTA conducida por el
## código del .Rmd (enumerar_h2_cloze.R), frente a las firmadas en «H2 a» (V0 de reparto_h2_96cfcf35.txt).
##   La cifra real: el código tal cual, con las exclusiones de V11, sobre las 100 Partes 1 de la muestra estándar.
##   El contraste de la ponderación está en contrastar_h2_ponderacion.R. (Una primera versión desactivaba aquí las
##   exclusiones con las sustituciones de MC1 y MC2: las guardas V11 del propio .Rmd abortan entonces 2/3 de las hojas,
##   así que ese contraste no valía y se retiró.)
## Solo lee. Uso (desde cloze/salida): Rscript --no-init-file medir_h2_exacta.R
suppressMessages(library(exams))
stopifnot("correr desde cloze/salida" = basename(getwd()) == "salida" && basename(dirname(getwd())) == "cloze")
V <- parse(file = "../verificar_espacio_cloze.R", keep.source = FALSE, encoding = "UTF-8")
for (e in V) if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% c("chunk", "MARCA", "FIRMA_H2"))
  eval(e, envir = globalenv())
EB <- new.env(); sys.source("../bateria_partes_2_a_6.R", envir = EB, keep.source = FALSE)
stopifnot(unname(tools::md5sum("../bateria_partes_2_a_6.R")) == "de20e0a986f86d44f0d3412517d2f07b")
source("../enumerar_h2_cloze.R", encoding = "UTF-8")
RMD <- normalizePath("../totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd")
COD <- chunk(RMD); cat("md5 del .Rmd:", unname(tools::md5sum(RMD)), "\n")
fmt <- function(T) { T <- h2_juzgar(T, FIRMA_H2); T$ex <- round(100 * T$exceso, 1)
  T[T$ex > 8 | !is.na(T$firma), c("estrato", "base", "papel", "ex", "firma", "veredicto")] }
t0 <- Sys.time(); FF <- list(); merr <- 0; errs <- character(0); nh <- 0L; casc <- 0
for (z in 1:100) { E <- enumerar_h2(COD, MARCA, z); nh <- nh + E$n_hojas; merr <- merr + E$masa_err; errs <- unique(c(errs, E$errores))
  f <- E$filas; f$w <- f$w / sum(f$w) / 100; casc <- casc + sum(f$w[f$cascada > 1L]); FF[[z]] <- f }
A <- h2_agregar(do.call(rbind, FF))
cat(sprintf("\nCon las exclusiones de V11, 100 Partes 1: %d hojas, masa con error %.4f%s, %d filas únicas, cascada (paso > 1) %.2f %% · %s\n",
            nh, merr, if (length(errs)) paste0(" (", paste(errs, collapse = "; "), ")") else "", nrow(A), 100 * casc, format(Sys.time() - t0)))
T1 <- h2_tabla(A, EB); print(fmt(T1), row.names = FALSE)
cat("\nCeldas en las que la cifra exacta con exclusiones difiere de la firmada (un decimal):\n")
J <- h2_juzgar(T1, FIRMA_H2); J$ex <- round(100 * J$exceso, 1)
D <- J[!is.na(J$firma) & J$ex != J$firma, c("estrato", "base", "papel", "ex", "firma", "veredicto")]; print(D, row.names = FALSE)
cat(sprintf("\nresumen: %d celdas firmadas; %d difieren; %d por encima de lo firmado; %d pendientes fuera de la firma\n",
            sum(!is.na(J$firma)), nrow(D), sum(!is.na(J$firma) & J$veredicto == "pendiente"), sum(is.na(J$firma) & J$veredicto == "pendiente")))
