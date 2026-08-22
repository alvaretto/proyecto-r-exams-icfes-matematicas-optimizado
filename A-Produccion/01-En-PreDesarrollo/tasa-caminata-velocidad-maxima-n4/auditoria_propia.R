## ===========================================================================
## auditoria_propia.R — verificador del ejercicio (N = 100, regla #23)
## Mide lo que el arsenal compartido NO mide en este ítem:
##   (A) corrección de la clave, versión por versión
##   (B) canal de VEREDICTO (la clave como "la distinta"), CONDICIONADO POR RAMA
##   (C) canal de LONGITUD H1 con su MARGEN (§P7-B), condicionado por rama
##   (D) fuga LÉXICA: token exclusivo de la clave (Error 32), por rama
##   (E) batería §P7 con techo nulo y veredicto por EXCESO (regla #22 v1.6)
## ===========================================================================
suppressMessages(library(stats))
R_RAIZ <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams"
source(file.path(R_RAIZ, ".claude/scripts/bateria_eliminacion.R"))   # aborta si falta

rmd <- "tasa_caminata_velocidad_maxima_numerico_variacional_argumentacion_n4_schoice_v1.Rmd"
ln  <- readLines(rmd, warn = FALSE)
i0  <- grep("^```\\{r data_generation", ln)[1]
i1  <- i0 + which(grepl("^```\\s*$", ln[(i0 + 1):length(ln)]))[1]
expr <- parse(text = paste(ln[(i0 + 1):(i1 - 1)], collapse = "\n"))

## PUNTO CIEGO CERRADO: parsear solo `data_generation` dejaba fuera los chunks
## `enunciado`, `opciones` y `solucion`. Un error de sintaxis en la Solution
## reventaba los 6 renders mientras este verificador seguía en verde.
for (ch in c("enunciado", "opciones", "solucion")) {
  j0 <- grep(sprintf("^```\\{r %s", ch), ln)[1]
  if (is.na(j0)) next
  j1 <- j0 + which(grepl("^```\\s*$", ln[(j0 + 1):length(ln)]))[1]
  ok <- tryCatch({ parse(text = paste(ln[(j0 + 1):(j1 - 1)], collapse = "\n")); TRUE },
                 error = function(e) { cat("SINTAXIS ROTA en chunk '", ch, "': ",
                                           conditionMessage(e), "\n", sep = ""); FALSE })
  if (!ok) quit(status = 1L)
}

N <- 100L
op <- vector("list", N); kk <- integer(N); rama <- logical(N); canon <- logical(N)
errs <- character(0)

for (s in seq_len(N)) {
  set.seed(s * 7919L)
  e <- new.env(parent = globalenv())
  eval(expr, envir = e)
  ## ---- (A) CORRECCIÓN DE LA CLAVE, en cada versión --------------------
  if (e$vel * e$tasa != 60L)                      errs <- c(errs, sprintf("s%d A: vel", s))
  if (e$cota != e$vel * e$H)                      errs <- c(errs, sprintf("s%d A: cota", s))
  if (!identical(e$excede, e$reporte > e$cota))   errs <- c(errs, sprintf("s%d A: veredicto", s))
  if (sum(e$sol) != 1L)                           errs <- c(errs, sprintf("s%d A: sol", s))
  if (!identical(e$opciones[e$sol == 1L], e$texto_clave)) errs <- c(errs, sprintf("s%d A: marca", s))
  if (length(unique(e$opciones)) != 4L)           errs <- c(errs, sprintf("s%d A: unicidad", s))
  op[[s]] <- e$opciones; kk[s] <- which(e$sol == 1L)
  rama[s] <- isTRUE(e$excede); canon[s] <- isTRUE(e$is_canonical)
}

cat("=== (A) CORRECCIÓN ===\n")
cat("versiones:", N, "| errores:", length(errs), "\n")
if (length(errs)) print(head(unique(errs), 20))
cat("rama 'supera la cota':", sum(rama), "| rama 'cabe en la cota':", sum(!rama),
    "| canónicas:", sum(canon), "\n\n")

## --- utilidades -----------------------------------------------------------
norm <- function(x) gsub("\\s+", " ", tolower(gsub("[[:digit:][:punct:]]+", " ", x)))
## veredicto declarado por cada opción: TRUE = "no pudo hacerse a pie"
## N1(a) del detractor: este regex quedó ciego al reescribir los cierres —los
## cierres afirmativos dicen ahora "no pueden/pudo hacerse a pie"—, y clasificaba
## mal el 41,1 % de las opciones, incluida la clave. Las sondas (B)/(B') y la
## regla "única con veredicto distinto" medían ruido.
ver_op <- function(txt) grepl("medio motorizado|no pueden hacerse a pie|no pudo hacerse a pie|increíble|debe descartarse", txt)
## REINCIDENCIA MEDIDA: este regex enumera cierres, así que CADUCA cada vez que se
## añade una opción y nada obliga a actualizarlo. Ya quedó ciego una vez (41,1 % de
## opciones mal clasificadas) y volvió a quedarlo con NUM-TAS-11 (14,0 %). El fix
## estructural no es ampliar la lista: es que la ceguera FALLE RUIDOSAMENTE. La
## comprobación texto<->declaración vive en el .Rmd (ver el bloque de coherencia),
## y aquí se deja constancia de por qué no basta con parchear el patrón.

medir <- function(idx, etq) {
  if (length(idx) < 20L) {
    cat(sprintf("[%s] n = %d  -> NO CONCLUYENTE (regla #23: estrato con n < 20).\n",
                etq, length(idx)))
    cat(sprintf("    N necesario para dictaminar este estrato: %d\n",
                as.integer(ceiling(N * 20 / max(1L, length(idx))))))
    return(invisible(NULL))
  }
  o <- op[idx]; k <- kk[idx]
  ## (B) canal de veredicto: la clave es la ÚNICA con su veredicto
  b <- mean(vapply(seq_along(o), function(i) {
    v <- ver_op(o[[i]]); sum(v == v[k[i]]) == 1L }, logical(1)))
  ## (C) H1 longitud + margen
  nc <- lapply(o, nchar)
  es_max <- vapply(seq_along(o), function(i) nc[[i]][k[i]] == max(nc[[i]]) &&
                     sum(nc[[i]] == max(nc[[i]])) == 1L, logical(1))
  marg <- vapply(seq_along(o), function(i) {
    r <- sort(nc[[i]], decreasing = TRUE); (r[1] - r[2]) / r[2] }, numeric(1))
  es_min <- vapply(seq_along(o), function(i) nc[[i]][k[i]] == min(nc[[i]]) &&
                     sum(nc[[i]] == min(nc[[i]])) == 1L, logical(1))
  ## (D) fuga léxica: token (>2 chars) exclusivo de la clave
  toks <- lapply(seq_along(o), function(i) {
    w <- lapply(strsplit(norm(o[[i]]), " "), function(z) unique(z[nchar(z) > 2L]))
    setdiff(w[[k[i]]], unlist(w[-k[i]])) })
  todos <- table(unlist(toks))
  peor <- if (length(todos)) sort(todos, decreasing = TRUE)[1] else c(ninguno = 0)
  ## (E) firma cross-versión de la clave (gemelo de H3b)
  fir <- vapply(seq_along(o), function(i) norm(o[[i]][k[i]]), character(1))
  frec <- max(table(fir)) / length(fir)

  cat(sprintf("[%s] n = %d\n", etq, length(idx)))
  ## (B) directo es 0 % POR CONSTRUCCIÓN: `stopifnot(length(mismo) >= 1L)` del
  ## .Rmd impide que la clave quede sola. Lo medible es el canal INVERSO.
  binv <- mean(vapply(seq_along(o), function(i) {
    v <- ver_op(o[[i]]); sing <- (sum(v) == 1L) || (sum(!v) == 1L)
    sing && (v[k[i]] != (sum(v) == 1L)) }, logical(1)))
  cat(sprintf("  B  clave única por VEREDICTO ....... %5.1f %%  (0 %% garantizado por stopifnot del .Rmd)\n", 100 * b))
  cat(sprintf("  B' el singleton por veredicto es SIEMPRE distractor (canal INVERSO): %5.1f %% de las versiones\n", 100 * binv))
  cat(sprintf("     -> lift de \"descarta el singleton\": %.1f %% frente al 25 %% de azar\n",
              100 * ((1 - binv) * 0.25 + binv / 3)))
  cat(sprintf("  C  clave = la más larga ............ %5.1f %%  (margen mediano %.1f %%)\n",
              100 * mean(es_max), 100 * median(marg[es_max])))
  cat(sprintf("     clave = la más corta ............ %5.1f %%\n", 100 * mean(es_min)))
  cat(sprintf("  D  token exclusivo de la clave más frecuente: '%s' en %.1f %%\n",
              names(peor), 100 * as.numeric(peor) / length(idx)))
  cat(sprintf("  E  firma normalizada de la clave más repetida: %.1f %%\n", 100 * frec))
}

cat("=== (B)-(E) CANALES, CONDICIONADOS POR RAMA ===\n")
medir(seq_len(N), "AGREGADO")
cat("\n"); medir(which(rama),  "RAMA supera la cota")
cat("\n"); medir(which(!rama), "RAMA cabe en la cota")

## ===========================================================================
## (E) BATERÍA §P7 — congelada al inicio (§P7-C). Seis familias declaradas.
## ===========================================================================
cat("\n=== (F) BATERÍA §P7 (regla #22 v1.6, veredicto por EXCESO) ===\n")
n1    <- function(x) suppressWarnings(as.numeric(sub(".*?(-?[0-9]+([.,][0-9]+)?).*", "\\1", x)))
nlast <- function(x) suppressWarnings(as.numeric(gsub("[.]", "",
            sub(".*[^0-9]([0-9][0-9.]*)[^0-9]*$", "\\1", x))))
solo  <- function(v) { i <- which(v); if (length(i) == 1L) i else NA_integer_ }

bateria <- list(
  nueva_regla("la(s) opción(es) más larga(s)",   "magnitud", function(o) nchar(o) == max(nchar(o))),
  nueva_regla("la(s) opción(es) más corta(s)",   "magnitud", function(o) nchar(o) == min(nchar(o))),
  nueva_regla("la(s) de mayor último número",   "magnitud",
              function(o) { v <- nlast(o); !is.na(v) & v == max(v, na.rm = TRUE) }),
  nueva_regla("la(s) de menor último número",   "magnitud",
              function(o) { v <- nlast(o); !is.na(v) & v == min(v, na.rm = TRUE) }),
  nueva_regla("la(s) de mayor primer número",   "magnitud",
              function(o) { v <- n1(o); !is.na(v) & v == max(v, na.rm = TRUE) }),
  ## AÑADIDA EN LA 3.ª PASADA DE FASE 2C. Su ausencia es el caso de libro del
  ## §P7: la familia `magnitud` se declaraba CON SONDA teniendo sólo la mitad,
  ## y el canal real vivía justo en la mitad que faltaba — la clave abre con
  ## `vel` = 60/tasa, el número MENOR del ítem en el 75 % del espacio, y "elige
  ## el primer número más pequeño" acertaba el 81,5 % con margen del 140 %.
  ## Una batería incompleta no mide "sin señal", mide SIN SONDA.
  nueva_regla("la(s) de menor primer número",   "magnitud",
              function(o) { v <- n1(o); !is.na(v) & v == min(v, na.rm = TRUE) }),
  nueva_regla("la(s) de más cifras",            "magnitud",
              function(o) { v <- lengths(gregexpr("[0-9]+", o)); v == max(v) }),
  nueva_regla("único último número múltiplo de 10", "divisibilidad",
              function(o) solo(!is.na(nlast(o)) & nlast(o) %% 10 == 0)),
  nueva_regla("único último número par",        "divisibilidad",
              function(o) solo(!is.na(nlast(o)) & nlast(o) %% 2 == 0)),
  nueva_regla("único con número de 3 cifras",   "divisibilidad",
              function(o) solo(grepl("[0-9]{3}", gsub("[.]", "", o)))),
  nueva_regla("única que dice 'motorizado'",    "signo", function(o) solo(grepl("motorizado", o))),
  nueva_regla("única que dice 'a pie'",         "signo", function(o) solo(grepl("a pie", o))),
  nueva_regla("única que niega ('no superan')", "signo", function(o) solo(grepl("\\bno superan\\b", o))),
  nueva_regla("única con veredicto distinto",   "signo",
              function(o) { v <- ver_op(o); if (sum(v) == 1L) which(v)
                            else if (sum(!v) == 1L) which(!v) else NA_integer_ }),
  nueva_regla("única con dos magnitudes con unidad", "formato",
              function(o) solo(lengths(gregexpr("kilómetros por hora|km/h", o)) >= 1L)),
  nueva_regla("única con punto y coma",         "formato", function(o) solo(grepl(";", o))),
  nueva_regla("única sin punto y coma",         "formato", function(o) solo(!grepl(";", o))),
  nueva_regla("única que empieza distinto",     "formato",
              function(o) { p <- substr(norm(o), 1, 12); solo(table(p)[p] == 1L) }),
  nueva_regla("única con 'velocidad máxima'",   "lexico", function(o) solo(grepl("velocidad máxima", o))),
  nueva_regla("única con 'menos de'",           "lexico", function(o) solo(grepl("menos de", o))),
  ## AÑADIDAS EN LA 4.ª PASADA DE FASE 2C. La familia `formato` se declaraba CON
  ## SONDA teniendo sólo sondas de puntuación, y el canal vivía en la ESTRUCTURA
  ## del argumento: la clave era la única que enunciaba DOS cotas superiores
  ## (56,9 %, +29,0 pp). Segunda lección idéntica a la de `magnitud` en la pasada
  ## anterior: una batería incompleta no mide "sin señal", mide SIN SONDA.
  nueva_regla("única que enuncia DOS cotas 'menos de'", "formato",
              function(o) solo(vapply(o, function(t)
                sum(gregexpr("menos de", t)[[1]] > 0) >= 2L, logical(1)))),
  ## RESIDUO DECLARADO Y ABIERTO (§P7-D): el grupo que comparte molde de apertura
  ## contiene a la clave más de lo que el azar explica (+11,5 pp con abstención en
  ## empates). Nace de la guarda `abre_vel`, que cerró un canal de magnitud mucho
  ## peor (81,5 % de acierto con margen del 140 %). Cerrarlo exige sortear el molde
  ## de las NUEVE opciones, no sólo de tres: decisión del profesor.
  nueva_regla("grupo que comparte molde de apertura", "formato",
              function(o) { p <- substr(o, 1, 25L); t <- table(p)
                if (max(t) == 1L || sum(t == max(t)) > 1L) return(NA)
                p == names(t)[which.max(t)] }),
  ## FAMILIA RELACIONAL — la que ninguna pasada sondeó (6.ª pasada de FASE 2C).
  ## Las 25 reglas anteriores miran cada opción POR SEPARADO (máximo, mínimo,
  ## "única que..."); ninguna mira RELACIONES ENTRE PARES. Y ahí vivía el canal
  ## mayor del ítem: la clave y NUM-TAS-05 citan el MISMO conjunto de numerales
  ## {vel, cota, R} —porque el error de 05 es justamente invertir el sentido de esa
  ## misma cota— y ninguna otra opción coincide. Medido: dispara en el 93,5 % de
  ## las versiones, la clave está en el par el 94,4 % de las veces, score 45,6 %
  ## => +20,6 pp. Sobre 455 ítems oficiales la misma regla da +0,8 pp: los
  ## cuadernillos también tienen pares convergentes, pero allí la clave cae en
  ## ellos AL AZAR. Tercera repetición de la misma lección en este ejercicio
  ## (faltó "menor primer número" en magnitud, "dos cotas" en formato, y ahora la
  ## familia relacional entera): una batería incompleta no mide "sin señal",
  ## mide SIN SONDA.
  nueva_regla("par que cita los MISMOS numerales", "formato",
              function(o) { cj <- vapply(o, function(t) paste(sort(unique(gsub("[.,]", "",
                              regmatches(t, gregexpr("[0-9][0-9.,]*", t))[[1]]))), collapse = "|"), "")
                tb <- table(cj); g <- names(tb)[tb >= 2L]
                if (!length(g)) return(NA); cj %in% g }),

  ## NO AÑADIR el centroide léxico ("la que más vocabulario comparte"): acierta el
  ## 99,2 % pero su margen mediano es del 3,4 % y el 100 % de las versiones queda
  ## bajo el umbral del 15 %. Es señal perfecta para un ordenador e invisible para
  ## una persona; §P7-B la exime. Como `bateria_eliminacion.R` no conoce márgenes,
  ## incluirla devolvería BLOQUEA por +71 pp — un falso positivo.
  nueva_regla("única con 'reportados'",         "lexico", function(o) solo(grepl("reportad", o))),
  nueva_regla("única con 'implica que'",        "lexico", function(o) solo(grepl("implica que", o))),
  nueva_regla("la(s) de más palabras distintas", "lexico",
              function(o) { v <- vapply(strsplit(norm(o), " "), function(z) length(unique(z)), 1L)
                            v == max(v) })
)

## La familia `posicion` NO se sondea: `exshuffle: TRUE` hace que R/exams
## re-permute el answerlist en el render (read_exercise.R), de modo que la
## posición que ve el estudiante es uniforme por construcción y el orden
## interno de `op` no le llega nunca. Sondearla ahí inflaba el techo nulo
## (+0,7 pp) midiendo un canal que no existe.
res <- evaluar_bateria(bateria, op, kk,
  familias_no_aplicables = c(
    posicion = "exshuffle: TRUE re-permuta el answerlist en el render (read_exercise.R); la posicion que ve el estudiante es uniforme por construccion"))
imprimir_bateria(res)
cat("\nSalida de la batería (exit):", exit_bateria(res), "\n")
if (length(errs)) { cat("\nSIN VEREDICTO: hay", length(errs), "fallos de corrección.\n"); quit(status = 1L) }
