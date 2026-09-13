## Bateria §P7 sobre el diseño de UNA SOLA RAMA (coseno pide lado).
## Instrumento oficial: .claude/scripts/bateria_eliminacion.R
RAIZ <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams"
source(file.path(RAIZ, ".claude/scripts/bateria_eliminacion.R"))

rmd <- commandArgs(trailingOnly = TRUE)[1]
N   <- 100L                                   # muestra estandar, regla #23

## --- extraer opciones + clave de N versiones -------------------------------
lineas <- readLines(rmd, warn = FALSE)
ini <- grep("^```\\{r data_generation", lineas)[1]
fin <- grep("^```\\s*$", lineas); fin <- fin[fin > ini][1]
codigo <- paste(lineas[(ini + 1):(fin - 1)], collapse = "\n")

dir.create(file.path(tempdir(), "figs"), showWarnings = FALSE)
old <- setwd(file.path(tempdir(), "figs")); on.exit(setwd(old))

opciones <- vector("list", N); claves <- integer(N)
for (i in seq_len(N)) {
  set.seed(i * 7919L + 13L)                   # misma rejilla que validar_diagnosticidad.R
  e <- new.env(parent = globalenv())
  eval(parse(text = codigo), envir = e)
  opciones[[i]] <- e$opciones
  claves[i]     <- which(e$sol == 1L)
}
setwd(old)

## --- helpers de lectura del molde ------------------------------------------
lads <- function(o) vapply(o, function(t) lengths(regmatches(t, gregexpr("\\b[a-z]\\b", t))), 0)
angs <- function(o) vapply(o, function(t) lengths(regmatches(t, gregexpr("\\b[A-ZÁ-Ú]\\b", t))), 0)
letr <- function(o) lads(o) + angs(o)
nch  <- function(o) nchar(o)
npal <- function(o) lengths(strsplit(o, "\\s+"))
pw   <- function(o) tolower(sub("^[^[:alnum:]]*([[:alnum:]á-úÁ-Úñü]+).*$", "\\1", o))
uniq1 <- function(v) { t <- table(v); m <- names(t)[t == 1]; if (length(m) == 1) which(v == m) else NA_integer_ }

bateria <- list(
  ## magnitud
  nueva_regla("la mas larga",              "magnitud", function(o) which.max(nch(o))),
  nueva_regla("la mas corta",              "magnitud", function(o) which.min(nch(o))),
  nueva_regla("descartar la mas larga",    "magnitud", function(o) seq_along(o) != which.max(nch(o))),
  nueva_regla("descartar la mas corta",    "magnitud", function(o) seq_along(o) != which.min(nch(o))),
  ## posicion
  nueva_regla("la primera",  "posicion", function(o) 1L),
  nueva_regla("la segunda",  "posicion", function(o) 2L),
  nueva_regla("la tercera",  "posicion", function(o) 3L),
  nueva_regla("la ultima",   "posicion", function(o) length(o)),
  ## formato — molde (conteo de lados/angulos): la familia que caza el canal estructural
  nueva_regla("la que cita mas lados",      "formato", function(o) which(lads(o) == max(lads(o)))),
  nueva_regla("la que cita menos lados",    "formato", function(o) which(lads(o) == min(lads(o)))),
  nueva_regla("la que cita mas angulos",    "formato", function(o) which(angs(o) == max(angs(o)))),
  nueva_regla("la unica con 2 lados",       "formato", function(o) uniq1(lads(o))),
  nueva_regla("la unica con 1 angulo",      "formato", function(o) uniq1(angs(o))),
  nueva_regla("la unica con su molde",      "formato", function(o) uniq1(paste0(lads(o), "L", angs(o), "A"))),
  nueva_regla("la que cita menos letras",   "formato", function(o) which(letr(o) == min(letr(o)))),
  nueva_regla("la que cita mas letras",     "formato", function(o) which(letr(o) == max(letr(o)))),
  nueva_regla("la unica que cita 3 letras", "formato", function(o) uniq1(ifelse(letr(o) == 3, "si", "no"))),
  nueva_regla("la unica sin 'angulo'",      "formato", function(o) uniq1(grepl("ngulo", o))),
  ## formato — RELACIONAL entre pares (exigencia §P7-E)
  nueva_regla("del par con el mismo molde", "formato", function(o) {
    m <- paste0(lads(o), "L", angs(o), "A"); t <- table(m); g <- names(t)[t == 2]
    if (length(g) == 1) which(m == g) else NA_integer_ }),
  nueva_regla("del par con las mismas letras", "formato", function(o) {
    s <- vapply(o, function(t) paste(sort(unlist(regmatches(t, gregexpr("\\b[a-zA-ZÁ-Ú]\\b", t)))), collapse = ""), "")
    t <- table(s); g <- names(t)[t == 2]; if (length(g) == 1) which(s == g) else NA_integer_ }),
  ## lexico
  nueva_regla("la de mas palabras",       "lexico", function(o) which.max(npal(o))),
  nueva_regla("la de menos palabras",     "lexico", function(o) which.min(npal(o))),
  nueva_regla("la unica con su 1a palabra","lexico", function(o) uniq1(pw(o))),
  nueva_regla("la unica que empieza por Lados", "lexico", function(o) uniq1(pw(o) == "lados")),
  nueva_regla("la que empieza por Lados", "lexico", function(o) which(pw(o) == "lados"))
)

res <- evaluar_bateria(bateria, opciones, claves,
                       familias_no_aplicables = c(
                         divisibilidad = "las opciones no contienen cifras: son conjuntos de rotulos",
                         signo         = "no hay magnitudes con signo ni conclusiones si/no"))
imprimir_bateria(res)
quit(status = exit_bateria(res))
