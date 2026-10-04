# verificar_bateria_p7.R — batería §P7 (regla #22) para barras-campeonato-baloncesto-n3
# Batería CONGELADA el 2026-10-02 antes de la primera medición (§P7-C).
# Uso: Rscript verificar_bateria_p7.R   (N = 100, regla #23; semillas i*7919+13)
# Las reglas solo ven las OPCIONES (matriz 2x2 + formato), nunca el enunciado.
args <- commandArgs(trailingOnly = FALSE)
aqui <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])))
raiz <- system("git rev-parse --show-toplevel", intern = TRUE)
source(file.path(raiz, ".claude/scripts/bateria_eliminacion.R"))
rmd <- file.path(aqui, "barras_campeonato_baloncesto_aleatorio_interpretacion_representacion_n3_schoice_v1.Rmd")
L <- readLines(rmd); s <- grep("^```\\{r data_generation", L); e <- grep("^```\\s*$", L); e <- e[e > s][1]
ex <- parse(text = L[(s + 1):(e - 1)])

correr <- function(seed) { set.seed(seed); env <- new.env(parent = globalenv())
  suppressMessages(eval(ex, env))
  list(ops = lapply(1:4, function(k) list(X = env$mats_op[[k]], fmt = env$fmt_op[k])),
       clave = which(env$sol), canon = env$es_canonica) }

N <- 100L
vers <- lapply(seq_len(N), function(i) correr(i * 7919L + 13L))
# Instancia canónica exacta: buscar una semilla que la produzca
i <- 1L; repeat { v <- correr(i); if (v$canon) break; i <- i + 1L }
canon <- v

vec <- function(o) as.vector(o$X)
compart <- function(ops) { A <- sapply(ops, vec)
  sapply(1:4, function(i) sum(sapply(setdiff(1:4, i), function(j) sum(A[, i] == A[, j])))) }
argmax <- function(x) which(x == max(x)); argmin <- function(x) which(x == min(x))
tot <- function(ops) sapply(ops, function(o) sum(o$X))
reglas <- list(
  nueva_regla("mayor_suma_total", "magnitud", function(o) argmax(tot(o))),
  nueva_regla("menor_suma_total", "magnitud", function(o) argmin(tot(o))),
  nueva_regla("mayor_celda_maxima", "magnitud", function(o) argmax(sapply(o, function(x) max(x$X)))),
  nueva_regla("menor_celda_minima", "magnitud", function(o) argmin(sapply(o, function(x) min(x$X)))),
  nueva_regla("mayor_rango_celdas", "magnitud", function(o) argmax(sapply(o, function(x) diff(range(x$X))))),
  nueva_regla("mas_celdas_pares", "divisibilidad", function(o) argmax(sapply(o, function(x) sum(x$X %% 2 == 0)))),
  nueva_regla("menos_celdas_pares", "divisibilidad", function(o) argmin(sapply(o, function(x) sum(x$X %% 2 == 0)))),
  nueva_regla("descartar_no_enteros", "divisibilidad", function(o) sapply(o, function(x) all(x$X == round(x$X)))),
  nueva_regla("ganados_mayor_que_perdidos_total", "signo", function(o) sapply(o, function(x) sum(x$X[, 1]) > sum(x$X[, 2]))),
  nueva_regla("grupo1_mayor_total", "signo", function(o) sapply(o, function(x) sum(x$X[1, ]) > sum(x$X[2, ]))),
  nueva_regla("posicion_a", "posicion", function(o) 1L), nueva_regla("posicion_b", "posicion", function(o) 2L),
  nueva_regla("posicion_c", "posicion", function(o) 3L), nueva_regla("posicion_d", "posicion", function(o) 4L),
  nueva_regla("formato_agrupada", "formato", function(o) sapply(o, function(x) x$fmt == "agrupada")),
  nueva_regla("formato_apilada", "formato", function(o) sapply(o, function(x) x$fmt == "apilada")),
  # relacionales entre pares (§P7-E)
  nueva_regla("medoide_celdas", "posicion", function(o) argmax(compart(o))),
  nueva_regla("anti_medoide", "posicion", function(o) argmin(compart(o))),
  nueva_regla("multiconjunto_mayoritario", "magnitud", function(o) { k <- sapply(o, function(x) paste(sort(vec(x)), collapse = ","))
    t <- table(k); if (max(t) < 2) return(NA); k == names(t)[which.max(t)] }),
  nueva_regla("sin_valor_exclusivo", "magnitud", function(o) { A <- sapply(o, vec)
    !sapply(1:4, function(i) any(!(A[, i] %in% A[, -i]))) }),
  nueva_regla("moda_por_celda", "posicion", function(o) { A <- sapply(o, vec)
    m <- apply(A, 1, function(r) { t <- table(r); if (max(t) > 1 && sum(t == max(t)) == 1) as.numeric(names(t)[which.max(t)]) else NA })
    argmax(colSums(A == m, na.rm = TRUE)) }),
  nueva_regla("formato_mayoritario_par", "formato", function(o) { f <- sapply(o, `[[`, "fmt"); t <- table(f)
    if (length(t) < 2 || t[1] == t[2]) return(NA); f == names(t)[which.max(t)] })
)
res <- evaluar_bateria(reglas, lapply(vers, `[[`, "ops"), sapply(vers, `[[`, "clave"),
                       familias_no_aplicables = c(lexico = "opciones-imagen: el único texto propio es el rótulo del tipo, función uno a uno del formato (cubierto por la familia formato)"))
imprimir_bateria(res)

cat("\n--- Instancia canónica (semilla ", i, ", enumeración exacta) ---\n", sep = "")
for (r in reglas) { S <- .conjunto_de(r$fn(canon$ops), 4L); k <- sum(S)
  sc <- if (k == 0 || k == 4) 0.25 else if (S[canon$clave]) 1 / k else 0
  if (sc > 0.25) cat(sprintf("  %-34s acierto %.2f (sobrevive: %s)\n", r$nombre, sc, paste(letters[which(S)], collapse = ""))) }
cat("  (solo se listan reglas que superan el azar 0,25 en la canónica)\n")

cat("\n--- Rank de magnitud (Incidente H, INC-DISTRACTOR-EXTREMO) ---\n")
rk <- t(sapply(vers, function(v) { tt <- tot(v$ops); c(clave_max = all(tt[v$clave] >= tt), clave_min = all(tt[v$clave] <= tt)) }))
cat(sprintf("  clave = suma total máxima: %.0f %% | mínima: %.0f %%\n", 100 * mean(rk[, 1]), 100 * mean(rk[, 2])))
quit(status = exit_bateria(res))
