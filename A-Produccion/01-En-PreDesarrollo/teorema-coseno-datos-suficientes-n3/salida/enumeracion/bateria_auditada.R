## Bateria §P7 AUDITADA. Cautela (a) del profesor: cada regla debe ser una
## estrategia que un estudiante podria descubrir. Se retiran las reglas
## INERTES POR CONSTRUCCION (aplicabilidad 0 % tras el rediseno) y las
## CONTRIVED ("exactamente k"), que nadie formula y que suben el techo nulo
## sin mover el maximo -> el patron de "bateria rellenada" (regla #22).
## Se CONSERVAN todas las que detectan canal, incluidas las dos que lo hallaron.
RAIZ <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams"
source(file.path(RAIZ, ".claude/scripts/bateria_eliminacion.R"))
rmd <- commandArgs(trailingOnly = TRUE)[1]
N <- 100L

L <- readLines(rmd, warn = FALSE)
i <- grep("data_generation", L, fixed = TRUE)[1]
j <- which(trimws(L) == "```"); j <- j[j > i][1]
codigo <- paste(L[(i + 1):(j - 1)], collapse = "\n")
dir.create(file.path(tempdir(), "figs"), showWarnings = FALSE)
old <- setwd(file.path(tempdir(), "figs"))
opciones <- vector("list", N); claves <- integer(N)
for (k in seq_len(N)) {
  set.seed(k * 7919L + 13L)
  e <- new.env(parent = globalenv()); eval(parse(text = codigo), envir = e)
  opciones[[k]] <- e$opciones; claves[k] <- which(e$sol == 1L)
}
setwd(old)

lads <- function(o) vapply(o, function(t) lengths(regmatches(t, gregexpr("\\b[a-z]\\b", t))), 0)
angs <- function(o) vapply(o, function(t) lengths(regmatches(t, gregexpr("\\b[A-ZÁ-Ú]\\b", t))), 0)
npal <- function(o) lengths(strsplit(o, "\\s+"))
pw   <- function(o) tolower(sub("^[^[:alnum:]]*([[:alnum:]á-úÁ-Úñü]+).*$", "\\1", o))
uniq1<- function(v) { t <- table(v); m <- names(t)[t == 1]; if (length(m) == 1) which(v == m) else NA_integer_ }
## Rotulos de una opcion, excluyendo la "y" de conjuncion (que NO es un rotulo:
## ninguna familia de I-11 usa la letra Y, y un stopifnot del .Rmd lo garantiza).
rmin <- function(t) { v <- unlist(regmatches(t, gregexpr("(?<![[:alpha:]])[a-z](?![[:alpha:]])", t, perl = TRUE))); v[v != "y"] }
rmay <- function(t) unlist(regmatches(t, gregexpr("(?<![[:alpha:]])[A-Z](?![[:alpha:]])", t, perl = TRUE)))
repite <- function(t) any(tolower(rmay(t)) %in% rmin(t))

bateria <- list(
  nueva_regla("la mas larga",           "magnitud", function(o) which.max(nchar(o))),
  nueva_regla("la mas corta",           "magnitud", function(o) which.min(nchar(o))),
  nueva_regla("descartar la mas larga", "magnitud", function(o) seq_along(o) != which.max(nchar(o))),
  nueva_regla("descartar la mas corta", "magnitud", function(o) seq_along(o) != which.min(nchar(o))),
  nueva_regla("la primera", "posicion", function(o) 1L),
  nueva_regla("la segunda", "posicion", function(o) 2L),
  nueva_regla("la tercera", "posicion", function(o) 3L),
  nueva_regla("la ultima",  "posicion", function(o) length(o)),
  nueva_regla("la que cita mas lados",   "formato", function(o) which(lads(o) == max(lads(o)))),
  nueva_regla("la que cita menos lados", "formato", function(o) which(lads(o) == min(lads(o)))),
  nueva_regla("la que cita mas angulos", "formato", function(o) which(angs(o) == max(angs(o)))),
  nueva_regla("la del molde raro (odd-one-out)", "formato",
              function(o) uniq1(paste0(lads(o), "L", angs(o), "A"))),
  ## ---- FAMILIA RELACIONAL (§P7-E) --------------------------------------------
  ## La regla "del par con el mismo molde" que ocupaba este hueco tenia
  ## APLICABILIDAD 0,0 %: nunca disparaba (en la rama general los moldes son
  ## 3x"2L1A" + 1 distinto -> no hay par; en la canonica hay DOS pares -> NA).
  ## §P7-E quedaba cubierta solo NOMINALMENTE, que es peor que no cubrirla:
  ## produce cobertura aparente. Objecion 2 del detractor (2026-09-13). Se
  ## sustituye por tres reglas relacionales que SI aplican.
  nueva_regla("del grupo mayoritario por lados citados", "formato", function(o) {
    g <- vapply(o, function(t) paste(sort(rmin(t)), collapse = ""), ""); t <- table(g)
    which(g == names(t)[which.max(t)]) }),
  nueva_regla("del grupo mayoritario por molde", "formato", function(o) {
    m <- paste0(lads(o), "L", angs(o), "A"); t <- table(m)
    if (max(t) > 1) which(m == names(t)[which.max(t)]) else NA_integer_ }),
  nueva_regla("la mas parecida a las demas (Jaccard de rotulos)", "formato", function(o) {
    R <- lapply(o, function(t) c(rmin(t), rmay(t)))
    sim <- vapply(seq_along(R), function(i) mean(vapply(setdiff(seq_along(R), i), function(j)
      length(intersect(R[[i]], R[[j]])) / length(union(R[[i]], R[[j]])), 0)), 0)
    if (sum(sim == max(sim)) < length(o)) which(sim == max(sim)) else NA_integer_ }),
  nueva_regla("la que empieza por Lados",   "lexico", function(o) which(pw(o) == "lados")),
  nueva_regla("la unica con su 1a palabra", "lexico", function(o) uniq1(pw(o))),
  nueva_regla("la de mas palabras",         "lexico", function(o) which.max(npal(o))),
  nueva_regla("la de menos palabras",       "lexico", function(o) which.min(npal(o))),
  ## SONDA D1 (anadida 2026-09-13). Canal LEXICO INTRA-OPCION: "elige la opcion
  ## cuyos rotulos son todos letras distintas", es decir la que NO repite letra
  ## entre los lados que cita y su(s) angulo(s). Es option-internal: no necesita
  ## el enunciado ni geometria. La bateria NO la tenia y por eso la cifra de la
  ## invariante I-10 se habia obtenido con una bateria CIEGA a este canal, que es
  ## justo el que el override I-12 acepta en rojo. Declararla es endurecer.
  nueva_regla("la que no repite letra entre sus rotulos", "lexico",
              function(o) which(!vapply(o, repite, NA))),
  nueva_regla("la que si repite letra (inverso de la anterior)", "lexico",
              function(o) which(vapply(o, repite, NA)))
)

res <- evaluar_bateria(bateria, opciones, claves,
                       familias_no_aplicables = c(
                         divisibilidad = "las opciones no contienen cifras: son conjuntos de rotulos",
                         signo         = "no hay magnitudes con signo ni conclusiones si/no"))
imprimir_bateria(res)
quit(status = exit_bateria(res))
