#!/usr/bin/env Rscript
## Batería §P7 (regla #22) del ejercicio, con el ALCANCE FIRMADO de §P7-E: firma del profesor del 2026-11-13, atada al md5 del .Rmd.
##
## Medición principal: enumeración EXACTA del espacio de versiones (salida/enumeracion_exacta.R ejecuta el chunk real forzando
## las 4 decisiones que fijan las opciones) con la batería congelada de salida/reglas_p7.R (44 reglas, sin ampliar en la pasada 4).
## Exceso de cada regla = tasa con la convención del helper sobre las versiones en que la regla aplica, menos 25 %.
## Control: N = 100 versiones reales (semillas 1..100) con evaluar_bateria() del helper (regla #23).
##
## Tres salidas distinguibles:
##   PASS            (exit 0) ninguna regla pasa de +8 pp.
##   ROJO_SIN_CUBRIR (exit 1) alguna regla pasa de +8 pp y: está fuera del alcance, o supera su tope, o no hay firma registrada.
##                            Es el estado de hoy: «pendiente de firma». Nunca se informa como PASS ni como cubierto.
##   ROJO_CUBIERTO   (exit 3) todas las reglas por encima de +8 pp están en el alcance, dentro de su tope, y hay una firma del
##                            profesor registrada (fecha y texto literal) para el md5 del archivo medido.
##   FIRMA_NO_CUBRE  (exit 4) hay reglas por encima de +8 pp y una firma, pero la firma es de otro md5: no cubre este archivo.
##                            Nunca se informa como «cubierto».
## Mutantes (por variable de entorno; no tocan el archivo):
##   MUTANTE_QUITAR=<regla>  saca la regla del alcance: el verificador debe decir «FUERA DEL ALCANCE: <regla>».
##   MUTANTE_TOPE=<regla>    baja el tope de la regla en 1 pp: el verificador debe decir «POR ENCIMA DE SU TOPE: <regla>».
##   VERIFICAR_RMD=<ruta>    mide otro archivo (p. ej. una copia con un cambio): con otro md5 debe salir FIRMA_NO_CUBRE.
## Uso (desde la carpeta del ejercicio): Rscript --no-init-file salida/verificar_bateria_p7.R
raiz <- system("git rev-parse --show-toplevel", intern = TRUE)
source(file.path(raiz, ".claude/scripts/bateria_eliminacion.R"))
source("salida/reglas_p7.R")
source("salida/enumeracion_exacta.R")
rmd <- Sys.getenv("VERIFICAR_RMD", list.files(".", pattern = "_schoice_v1\\.Rmd$")[1])
CORTE_PP <- 8

## --- Firma del profesor (§P7-E, regla por regla), textos literales transmitidos por el coordinador. Atada al md5 del .Rmd firmado
## (incluye el YAML del Knit de RStudio, que el profesor decidió dejar: «No, dejarlas»). ---
FIRMA <- list(fecha = "2026-11-13", md5 = "b9d489a9dd838630875a4958146b9bc8",
  texto = paste("1. Tras ver la tabla de atajos anterior a la pasada 4: «Firmar los atajos de la tabla y aprobar».",
                "2. Tras ver las seis cifras que cambiaron o eran nuevas tras la pasada 4: «Firmar también estos seis y aprobar»."))
## Firmadas FUERA de la batería congelada (no se amplía, §P7-C): medidas por el detractor de la ronda 4.
FIRMADAS_FUERA_BATERIA <- data.frame(stringsAsFactors = FALSE,
  atajo = c("la más cercana a la opción de la calculadora", "entre las de numerador múltiplo de x, la más cercana al seno",
            "si x es positivo la mayor, si es negativo la menor"),
  cifra_pp = c(75.0, 50.0, 8.3),
  nota = c("", "", "acierto del 100 % en la rama x = ±3 con tres términos"),
  fuente = "salida/fase2c_r4_detractor.md (H2)")

## --- Alcance FIRMADO el 2026-11-13: cada regla que pasa de +8 pp con su cifra medida (tope por regla), pasada 4, 2026-11-13. ---
ALCANCE <- data.frame(stringsAsFactors = FALSE,
  regla = c("est_2a_cerca_sen", "rel_par_lejos_sen", "rel_par_mult_x", "rel_par_reducible", "est_rango_reducible",
            "est_rango_no_cerca_sen", "est_cerca_S2", "est_rango_mayor_abs", "rel_par_mas_cercano", "rel_par_mayor_abs",
            "rel_par_menor_abs", "est_signo_x", "est_cerca_x", "est_entre_S2_y_x", "div_no_mult5", "est_rango_menor_abs",
            "est_abs_menor_x", "est_rango_seno", "div_mult_x", "div_reducible", "est_no_cerca_sen"),
  tope_pp = c(75.0, 75.0, 62.5, 62.5, 26.2, 26.1, 25.0, 25.0, 25.0, 25.0, 25.0, 17.4, 16.7, 15.1, 15.0, 13.3, 10.8, 10.0,
              8.3, 8.3, 8.3))
m_q <- Sys.getenv("MUTANTE_QUITAR"); m_t <- Sys.getenv("MUTANTE_TOPE")
if (nzchar(m_q)) { stopifnot(m_q %in% ALCANCE$regla); ALCANCE <- ALCANCE[ALCANCE$regla != m_q, ]; cat("MUTANTE: se quitó", m_q, "del alcance\n") }
if (nzchar(m_t)) { stopifnot(m_t %in% ALCANCE$regla); ALCANCE$tope_pp[ALCANCE$regla == m_t] <- ALCANCE$tope_pp[ALCANCE$regla == m_t] - 1; cat("MUTANTE: se bajó 1 pp el tope de", m_t, "\n") }

## --- 1. Enumeración exacta ---------------------------------------------------------------------
cfg <- enumerar_configs(rmd); w <- vapply(cfg, `[[`, 0, "peso"); rama <- vapply(cfg, `[[`, "", "rama")
T <- tabla_exacta(R, cfg); T <- T[order(-T$exceso_pp), ]
rk <- vapply(cfg, function(c) rank(c$num)[which(c$sol == 1L)], 0)
cat(sprintf("md5 del .Rmd: %s | configuraciones: %d | rango de la clave (exacto): %s %%\n", tools::md5sum(rmd), length(cfg),
            paste(round(100 * tapply(w, rk, sum), 1), collapse = " / ")))
cat("clave en un extremo, por rama (%):", paste(sprintf("%s %.0f", names(tapply(w, rama, sum)),
    100 * tapply(w * (rk %in% c(1, 4)), rama, sum) / tapply(w, rama, sum)), collapse = " · "), "\n\n")
options(width = 200); print(T[T$exceso_pp > CORTE_PP, ], row.names = FALSE)

## --- 2. Márgenes exactos (§P7-B) de las reglas de distancia -------------------------------------
marg <- function(d) { s <- sort(d); if (s[2] == 0) 0 else (s[2] - s[1]) / s[2] }
mediana_w <- function(v, w) { o <- order(v); v <- v[o]; w <- w[o] / sum(w); v[which(cumsum(w) >= 0.5)[1]] }
for (def in list(list("rel_par_mas_cercano", function(c) { d <- abs(outer(c$num, c$num, "-")); diag(d) <- Inf
                          list(acierta = which(c$sol == 1L) %in% which(d == min(d), arr.ind = TRUE)[1, ], m = marg(d[upper.tri(d)])) }),
                 list("est_cerca_x", function(c) { d <- abs(c$num / c$D - c$x); list(acierta = which.min(d) == which(c$sol == 1L), m = marg(d)) }),
                 list("est_cerca_S2", function(c) { d <- abs(c$num / c$D - (c$x - c$x^3 / 6)); list(acierta = which.min(d) == which(c$sol == 1L), m = marg(d)) }))) {
  r <- lapply(cfg, def[[2]]); a <- vapply(r, `[[`, TRUE, "acierta"); mm <- vapply(r, `[[`, 0, "m")
  cat(sprintf("margen %-20s acierta en %.1f %%; margen mediano cuando acierta %.0f %%; aciertos con margen < 15 %%: %.1f %% del peso\n",
              def[[1]], 100 * sum(w * a), 100 * mediana_w(mm[a], w[a]), 100 * sum(w * (a & mm < 0.15))))
}

## --- 3. Control con N = 100 versiones reales (helper) ---------------------------------------------
l <- readLines(rmd, encoding = "UTF-8"); i0 <- grep("^```\\{r data_generation", l); i1 <- which(l == "```")[which(l == "```") > i0][1]
code <- parse(text = l[(i0 + 1):(i1 - 1)], encoding = "UTF-8")
vers <- lapply(1:100, function(s) { set.seed(s); e <- new.env(); eval(code, e); list(num = e$numeradores, D = e$D, x = e$x, clave = which(e$sol == 1L)) })
res <- evaluar_bateria(R, lapply(vers, `[[`, "num"), vapply(vers, `[[`, 0L, "clave"),
                       familias_no_aplicables = c(lexico = "las opciones son fracciones sin palabras"),
                       estimulos = lapply(vers, function(v) list(D = v$D, x = v$x)))
sal <- capture.output(imprimir_bateria(res))
cat("\ncontrol N = 100 (helper, semillas 1..100):\n"); cat(grep("Maximo observado|Techo nulo  |EXCESO  |Exceso ATOMICO|^  (BLOQUEA|PASS|NO_CONCLUYENTE)", sal, value = TRUE), sep = "\n")

## --- 4. Alcance firmado (§P7-E) ---------------------------------------------------------------------
sobre <- T[T$exceso_pp > CORTE_PP, c("regla", "exceso_pp")]
firmada <- nzchar(FIRMA$fecha) && nzchar(FIRMA$texto) && nzchar(FIRMA$md5)
md5_actual <- unname(tools::md5sum(rmd))
estado <- vapply(seq_len(nrow(sobre)), function(i) {
  r <- sobre$regla[i]; v <- sobre$exceso_pp[i]
  if (!(r %in% ALCANCE$regla)) return(paste0("FUERA DEL ALCANCE: ", r, " (", v, " pp)"))
  tope <- ALCANCE$tope_pp[ALCANCE$regla == r]
  if (v > tope + 1e-9) return(paste0("POR ENCIMA DE SU TOPE: ", r, " (", v, " pp > ", tope, ")"))
  if (!firmada) return(paste0("en alcance, pendiente de firma: ", r, " (", v, " pp <= ", tope, ")"))
  paste0("en alcance, firmada: ", r, " (", v, " pp <= ", tope, ")")
}, "")
cat("\n-- Alcance (§P7-E) --\n"); cat(estado, sep = "\n")
cat("\n-- Firmadas fuera de la batería --\n"); for (i in seq_len(nrow(FIRMADAS_FUERA_BATERIA))) with(FIRMADAS_FUERA_BATERIA[i, ],
  cat(sprintf("%s: %+.1f pp%s (%s)\n", atajo, cifra_pp, if (nzchar(nota)) paste0("; ", nota) else "", fuente)))
cat(sprintf("\nFirma: %s · md5 firmado %s · md5 medido %s\n", if (firmada) FIRMA$fecha else "(ninguna)", FIRMA$md5, md5_actual))
if (nrow(sobre) == 0L) { cat("\nVEREDICTO_P7: PASS\n"); quit(status = 0L) }
if (firmada && !identical(md5_actual, FIRMA$md5)) {
  cat(sprintf("\nVEREDICTO_P7: FIRMA_NO_CUBRE — la firma del %s es del md5 %s; este archivo tiene md5 %s\n", FIRMA$fecha, FIRMA$md5, md5_actual))
  quit(status = 4L)
}
if (any(!grepl("^en alcance", estado)) || !firmada) {
  cat(sprintf("\nVEREDICTO_P7: ROJO_SIN_CUBRIR — %d reglas por encima de +%d pp; %s\n", nrow(sobre), CORTE_PP,
              if (any(!grepl("^en alcance", estado))) "hay reglas fuera del alcance o por encima de su tope" else "pendiente de firma del profesor"))
  quit(status = 1L)
}
cat("\nVEREDICTO_P7: ROJO_CUBIERTO — firma del", FIRMA$fecha, "\n"); quit(status = 3L)
