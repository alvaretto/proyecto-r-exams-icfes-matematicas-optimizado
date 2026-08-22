## ===========================================================================
## auditoria_propia_cloze.R — verificador del CLOZE (N = 100, regla #23)
## Nombre ÚNICO por rol: el scratchpad se comparte con los subagentes y un
## script propio ya fue sobrescrito por el de un detractor (HANDOFF §8).
##
## Mide lo que el arsenal compartido NO mide en este ítem:
##   (A) CORRECCIÓN de las SEIS claves, versión por versión
##   (B) SOLIDEZ de los distractores de la Parte 5 (Error 33) + CONTROL POSITIVO
##   (C) coherencia conclusión <-> justificación en la Parte 4 (INC-SINO-BINARIO)
##   (D) batería §P7 por gap, con techo nulo, veredicto por EXCESO y la familia
##       RELACIONAL entre pares que exige §P7-E
##   (E) vara de la INSTANCIA CANÓNICA por enumeración exacta (§P7-E)
## ===========================================================================
suppressMessages(library(stats))
R_RAIZ <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams"
source(file.path(R_RAIZ, ".claude/scripts/bateria_eliminacion.R"))   # aborta si falta

rmd <- "tasa_caminata_velocidad_maxima_numerico_variacional_argumentacion_n4_cloze_v1.Rmd"
ln  <- readLines(rmd, warn = FALSE)
i0  <- grep("^```\\{r data_generation", ln)[1]
i1  <- i0 + which(grepl("^```\\s*$", ln[(i0 + 1):length(ln)]))[1]
expr <- parse(text = paste(ln[(i0 + 1):(i1 - 1)], collapse = "\n"))

## Punto ciego cerrado: parsear sólo `data_generation` dejaría fuera los chunks
## de emisión. Un error de sintaxis en la Solution revienta los renders mientras
## este verificador sigue en verde.
for (ch in c("enunciado", "parte2", "parte3", "parte4", "parte5",
             "answerlist_q", "solucion", "answerlist_s")) {
  j0 <- grep(sprintf("^```\\{r %s", ch), ln)[1]
  if (is.na(j0)) { cat("FALTA chunk '", ch, "'\n", sep = ""); next }
  j1 <- j0 + which(grepl("^```\\s*$", ln[(j0 + 1):length(ln)]))[1]
  ok <- tryCatch({ parse(text = paste(ln[(j0 + 1):(j1 - 1)], collapse = "\n")); TRUE },
                 error = function(e) { cat("SINTAXIS ROTA en '", ch, "': ",
                                           conditionMessage(e), "\n", sep = ""); FALSE })
  if (!ok) quit(status = 1L)
}

N <- 100L
errs <- character(0)
op1 <- op4 <- op5 <- vector("list", N)
k1 <- k4 <- k5 <- integer(N)
rama <- canon <- logical(N)

for (s in seq_len(N)) {
  set.seed(s * 7919L)
  e <- new.env(parent = globalenv())
  eval(expr, envir = e)

  ## ---- (A) CORRECCIÓN DE LAS SEIS CLAVES ------------------------------
  ## En CLOZE hay N claves y por tanto N frentes donde una puede volverse falsa.
  if (e$vel * e$tasa != 60L)                    errs <- c(errs, sprintf("s%d A1 vel", s))
  if (e$cota != e$vel * e$H)                    errs <- c(errs, sprintf("s%d A2 cota", s))
  if (!identical(e$excede, e$reporte > e$cota)) errs <- c(errs, sprintf("s%d A3 veredicto", s))
  ## gap 1: la clave dice lo que el criterio realmente establece
  cl1 <- e$opciones_p1[e$sol_p1 == 1L]
  if (identical(e$eje_p1, "velocidad")) {
    if (!grepl("por debajo de", cl1))  errs <- c(errs, sprintf("s%d A4 p1-vel", s))
  } else {
    if (!grepl("como mínimo", cl1))    errs <- c(errs, sprintf("s%d A4 p1-tiempo", s))
  }
  ## gaps 2 y 3: las respuestas numéricas son las cotas reales
  if (e$resp_p2 != 60L %/% e$tasa)     errs <- c(errs, sprintf("s%d A5 p2", s))
  if (e$resp_p3 != e$resp_p2 * e$H)    errs <- c(errs, sprintf("s%d A6 p3", s))
  ## gaps 4 y 5: la marca coincide con la verdad recomputada (Familia 4)
  if (!identical(e$opciones_p4[e$sol_p4 == 1L], e$texto_p4_clave))
    errs <- c(errs, sprintf("s%d A7 marca p4", s))
  if (!identical(e$opciones_p5[e$sol_p5 == 1L], e$texto_clave))
    errs <- c(errs, sprintf("s%d A8 marca p5", s))
  ## gap 6: cada afirmación marcada verdadera lo es de hecho
  for (j in seq_along(e$afirmaciones_p6)) {
    t <- e$afirmaciones_p6[j]
    esV <- t %in% e$afirm_V
    esF <- t %in% e$afirm_F
    if (!xor(esV, esF)) errs <- c(errs, sprintf("s%d A9 p6 afirm ambigua", s))
    if (esV != (e$sol_p6[j] == 1L)) errs <- c(errs, sprintf("s%d A9 p6 marca", s))
    ## O6(a): comprobar la marca contra la PERTENENCIA al pool es circular (el
    ## pool define la marca). Para las dos afirmaciones PARAMETRIZADAS el valor
    ## de verdad es calculable, así que se verifica ARITMÉTICAMENTE.
    m <- regmatches(t, regexpr("Un recorrido de ([0-9.]+) kil", t))
    if (length(m)) {
      km <- as.numeric(gsub("[^0-9]", "", m))
      verdad <- km < e$cota            # compatible <=> por debajo de la cota
      if (verdad != (e$sol_p6[j] == 1L))
        errs <- c(errs, sprintf("s%d A9b p6 VALOR DE VERDAD (%d vs cota %d)", s, km, e$cota))
    }
  }
  if (sum(e$sol_p1) != 1L || sum(e$sol_p4) != 1L || sum(e$sol_p5) != 1L)
    errs <- c(errs, sprintf("s%d A10 sol", s))
  if (sum(e$sol_p6) < 1L || sum(e$sol_p6) > 5L) errs <- c(errs, sprintf("s%d A11 p6 degenera", s))
  if (length(unique(e$opciones_p1)) != 4L || length(unique(e$opciones_p4)) != 4L ||
      length(unique(e$opciones_p5)) != 4L || length(unique(e$afirmaciones_p6)) != 6L)
    errs <- c(errs, sprintf("s%d A12 unicidad", s))

  ## ---- (B) SOLIDEZ en la Parte 5 (Error 33) ---------------------------
  ## No es identidad con el argumento de la clave: es SOLIDEZ. Un argumento
  ## puede ser sólido sin ser el mismo argumento.
  solido <- function(ee, M, vel, cota, H, reporte) {
    ck <- ee$cota_km(M)
    if (is.na(ck)) return(FALSE)
    if (!identical(ee$sentido, "superior")) return(FALSE)
    Th <- if (is.null(ee$horizonte)) H else ee$horizonte(M)
    if (ck < vel * Th) return(FALSE)
    if (isTRUE(ee$veredicto(M))) reporte > ck && Th >= H
    else                         reporte < ck && ck <= vel * H
  }
  for (i in e$sel) {
    if (solido(e$errores_conceptuales[[i]], e$M, e$vel, e$cota, e$H, e$reporte))
      errs <- c(errs, sprintf("s%d B SEGUNDA CLAVE p5: %s", s, e$errores_conceptuales[[i]]$codigo))
  }

  ## ---- (C) coherencia conclusión <-> justificación en la Parte 4 ------
  for (i in e$sel_p4) {
    cc <- e$conclusiones[[i]]
    dice_no_pie <- grepl("no pudo hacerse a pie|debe descartarse", cc$texto)
    if (!identical(dice_no_pie, isTRUE(cc$veredicto)))
      errs <- c(errs, sprintf("s%d C incoherencia p4: %s", s, cc$codigo))
  }
  ## O6(c): comprobar IDENTIDAD con la clave es el predicado que dejó pasar la
  ## segunda clave del Error 33. Se comprueba SOLIDEZ: una conclusión es una
  ## segunda clave si describe bien la comparación (cota de DISTANCIA contra el
  ## reporte) Y concluye el veredicto verdadero.
  ## O6(c), 2.ª versión. La 1.ª era un regex laxo que casaba "a esa cota" dentro
  ## de "diga esa cota" y confundía VEREDICTO BINARIO con VALIDEZ del argumento:
  ## marcó 90/100 falsos positivos sobre CON-01 (falacia necesario=>suficiente) y
  ## CON-05 (que RECHAZA la cota). Era el Incidente S dentro del verificador.
  ## El defecto de tipo "inferencia" es de FUERZA de la afirmación y NO es
  ## decidible con un regex, así que se verifica la DECLARACIÓN y la no-identidad
  ## textual; la validez semántica de las 6 la auditaron analíticamente el
  ## orquestador y el detractor, y así queda declarado en el HANDOFF.
  for (i in seq_along(e$conclusiones)) {
    cc <- e$conclusiones[[i]]
    if (!isTRUE(cc$defecto %in% c("premisa", "inferencia")))
      errs <- c(errs, sprintf("s%d C p4 sin defecto declarado: %s", s, cc$codigo))
    if (identical(cc$texto, e$texto_p4_clave))
      errs <- c(errs, sprintf("s%d C SEGUNDA CLAVE p4 (identica): %s", s, cc$codigo))
  }

  op1[[s]] <- e$opciones_p1; k1[s] <- which(e$sol_p1 == 1L)
  op4[[s]] <- e$opciones_p4; k4[s] <- which(e$sol_p4 == 1L)
  op5[[s]] <- e$opciones_p5; k5[s] <- which(e$sol_p5 == 1L)
  rama[s] <- isTRUE(e$excede); canon[s] <- isTRUE(e$is_canonical)
}

cat("=== (A)(B)(C) CORRECCIÓN, SOLIDEZ Y COHERENCIA ===\n")
cat("versiones:", N, "| errores:", length(errs), "\n")
if (length(errs)) print(head(unique(errs), 20))
cat("rama 'supera la cota':", sum(rama), "| rama 'cabe en la cota':", sum(!rama),
    "| canónicas:", sum(canon), "\n\n")

## ---------------------------------------------------------------------------
## CONTROL POSITIVO de la sonda de solidez. Un 0 sin control positivo no
## distingue "limpio" de "sonda ciega". Se relaja la precondición por regiones
## de NUM-TAS-08 (la defensa del Error 33) y se comprueba que la sonda DISPARA.
## ---------------------------------------------------------------------------
src_mut <- paste(ln[(i0 + 1):(i1 - 1)], collapse = "\n")
## Se relajan las DOS defensas del Error 33, no una: (1) la precondición por
## regiones de NUM-TAS-08 y (2) el `stopifnot(all(es_malo))` que aborta el
## render. Con sólo la primera, el mutante moría por la SEGUNDA defensa del
## propio .Rmd y mi sonda externa nunca llegaba a medirse: eso es el Incidente S
## (mutante cazado por la sonda equivocada) dentro del control positivo.
src_mut <- sub("precondicion = function\\(M\\) M\\$H != 24L &&\n\\s*\\(if \\(M\\$H < 24L\\) M\\$R < M\\$vel \\* 24L else M\\$R > M\\$vel \\* 24L\\)",
               "precondicion = function(M) TRUE", src_mut)
src_mut <- sub("es_malo <- !vapply(sel, function(i) es_solido(errores_conceptuales[[i]]), logical(1))\nstopifnot(all(es_malo))",
               "es_malo <- !vapply(sel, function(i) es_solido(errores_conceptuales[[i]]), logical(1))",
               src_mut, fixed = TRUE)
## Guarda de MUTANTE MAL CONSTRUIDO, sobre el TEXTO del mutante y no sobre el
## original: si la sustitución no se materializó, el 0 de abajo no probaría nada.
mut_ok <- grepl("precondicion = function\\(M\\) TRUE,\n       texto = function\\(M\\) paste0\\(\n         frase_cota\\(M\\$vel, M\\$vel \\* 24L", src_mut) &&
          !grepl("stopifnot(all(es_malo))", src_mut, fixed = TRUE)
if (!mut_ok) {
  cat("CONTROL POSITIVO MAL CONSTRUIDO: la mutación de NUM-TAS-08 no se aplicó.\n")
  cat("  -> el 0 de la sonda de solidez NO está acreditado.\n\n")
  cp_hits <- NA_integer_
} else {
  expr_mut <- parse(text = src_mut)
  cp_hits <- 0L
  for (s in seq_len(400L)) {                      # serie de semillas INDEPENDIENTE
    set.seed(s * 7331L + 19L)
    em <- new.env(parent = globalenv())
    r <- tryCatch({ eval(expr_mut, envir = em); TRUE }, error = function(x) FALSE)
    if (!isTRUE(r)) next
    solido2 <- function(ee, M, vel, cota, H, reporte) {
      ck <- ee$cota_km(M); if (is.na(ck)) return(FALSE)
      if (!identical(ee$sentido, "superior")) return(FALSE)
      Th <- if (is.null(ee$horizonte)) H else ee$horizonte(M)
      if (ck < vel * Th) return(FALSE)
      if (isTRUE(ee$veredicto(M))) reporte > ck && Th >= H
      else                         reporte < ck && ck <= vel * H
    }
    for (i in em$sel) if (solido2(em$errores_conceptuales[[i]], em$M, em$vel, em$cota, em$H, em$reporte)) {
      cp_hits <- cp_hits + 1L; break
    }
  }
  cat("=== CONTROL POSITIVO (sonda de solidez sobre mutante sin la defensa) ===\n")
  cat("segundas claves detectadas en el mutante:", cp_hits, "/ 400 corridas\n")
  cat(if (cp_hits > 0L) "  -> la sonda DISPARA sobre el defecto conocido: el 0 de arriba vale.\n"
      else "  -> LA SONDA NO DISPARA: el 0 de arriba NO está acreditado.\n", sep = "")
  cat("\n")
}

## ===========================================================================
## (D) BATERÍA §P7 POR GAP — con la familia RELACIONAL de §P7-E
## ===========================================================================
n1    <- function(t) { m <- regmatches(t, regexpr("[0-9]+", t)); if (length(m)) as.numeric(m) else NA_real_ }
nlast <- function(t) { m <- regmatches(t, gregexpr("[0-9]+", t))[[1]]
                       if (length(m)) as.numeric(m[length(m)]) else NA_real_ }
nums  <- function(t) sort(as.numeric(regmatches(t, gregexpr("[0-9]+", t))[[1]]))
nnum  <- function(t) length(regmatches(t, gregexpr("[0-9]+", t))[[1]])
pw    <- function(t) tolower(sub("^[^[:alnum:]]*([[:alnum:]]+).*$", "\\1", t))
lex   <- function(t) unique(strsplit(tolower(gsub("[^[:alpha:] ]", " ", t)), " +")[[1]])

## Abstenerse en EMPATES: `which.max` desempata por el orden interno, que
## `exshuffle: TRUE` destruye antes de que el estudiante lo vea (+12,6 pp
## artefactual frente a +6,2 pp real, HANDOFF §8).
uniq_ext <- function(v, fn) {
  if (all(is.na(v))) return(NA_integer_)
  i <- which(v == fn(v, na.rm = TRUE)); if (length(i) == 1L) i else NA_integer_
}
unico <- function(cond) { i <- which(cond); if (length(i) == 1L) i else NA_integer_ }

## Familia RELACIONAL (§P7-E): reglas función del CONJUNTO, no de cada opción
## por separado. Ninguna regla `which.max` / "la única que..." puede expresar
## "hay dos opciones que se parecen entre sí, elige una de las dos".
par_mismos_numerales <- function(o) {
  ns <- lapply(o, nums); key <- vapply(ns, function(x) paste(x, collapse = "-"), character(1))
  tb <- table(key); g <- names(tb)[tb >= 2L]
  if (length(g) != 1L) return(NA_integer_)
  which(key == g)                                  # devuelve el GRUPO: score 1/|S|
}
par_mismo_molde <- function(o) {
  key <- substr(tolower(o), 1L, 40L); tb <- table(key); g <- names(tb)[tb >= 2L]
  if (length(g) != 1L) return(NA_integer_)
  which(key == g)
}
## Devuelve el GRUPO de longitud equivalente, no un par arbitrario: tomar el
## primer par de `which(..., arr.ind)` subestimaba el conjunto superviviente
## cuando hay tres o más opciones gemelas, e inflaba el score (1/2 en vez de
## 1/3). Es un bug de la SONDA, y corregirlo la vuelve mas estricta, no menos:
## no es ampliar la bateria a mitad de ciclo (§P7-C).
par_long_gemela <- function(o) {
  L <- nchar(o); d <- as.matrix(dist(L)); diag(d) <- Inf
  if (min(d) > 3) return(NA_integer_)
  seed <- which(d == min(d), arr.ind = TRUE)[1, 1]
  g <- which(abs(L - L[seed]) <= 3L)
  if (length(g) < 2L) return(NA_integer_)
  g
}

bateria <- list(
  nueva_regla("mayor primer numeral",  "magnitud", function(o) uniq_ext(vapply(o, n1, 0), max)),
  nueva_regla("menor primer numeral",  "magnitud", function(o) uniq_ext(vapply(o, n1, 0), min)),
  nueva_regla("mayor numeral final",   "magnitud", function(o) uniq_ext(vapply(o, nlast, 0), max)),
  nueva_regla("menor numeral final",   "magnitud", function(o) uniq_ext(vapply(o, nlast, 0), min)),
  nueva_regla("mas numerales citados", "magnitud", function(o) uniq_ext(vapply(o, nnum, 0), max)),
  nueva_regla("menos numerales",       "magnitud", function(o) uniq_ext(vapply(o, nnum, 0), min)),

  nueva_regla("unica con primer numeral par",   "divisibilidad",
              function(o) unico(vapply(o, function(t) { v <- n1(t); !is.na(v) && v %% 2 == 0 }, TRUE))),
  nueva_regla("unica con numeral final par",    "divisibilidad",
              function(o) unico(vapply(o, function(t) { v <- nlast(t); !is.na(v) && v %% 2 == 0 }, TRUE))),
  nueva_regla("unica con numeral multiplo de 10", "divisibilidad",
              function(o) unico(vapply(o, function(t) { v <- nlast(t); !is.na(v) && v %% 10 == 0 }, TRUE))),

  nueva_regla("primera opcion", "posicion", function(o) 1L),
  nueva_regla("ultima opcion",  "posicion", function(o) length(o)),
  nueva_regla("opcion central", "posicion", function(o) 2L),

  nueva_regla("la mas larga",  "formato", function(o) uniq_ext(nchar(o), max)),
  nueva_regla("la mas corta",  "formato", function(o) uniq_ext(nchar(o), min)),
  nueva_regla("unica con punto y coma", "formato", function(o) unico(grepl(";", o))),
  nueva_regla("unica con dos comas o mas", "formato",
              function(o) unico(vapply(o, function(t) lengths(regmatches(t, gregexpr(",", t))) >= 2L, TRUE))),
  nueva_regla("unica que enuncia dos cotas", "formato",
              function(o) unico(vapply(o, function(t) lengths(regmatches(t, gregexpr("menos de|al menos|no llega|no menos", t))) >= 2L, TRUE))),

  nueva_regla("unica con su primera palabra", "lexico",
              function(o) { p <- pw(o); tb <- table(p); unico(p %in% names(tb)[tb == 1L]) }),
  nueva_regla("unica que dice 'no'",     "lexico", function(o) unico(grepl("\\bno\\b", o))),
  nueva_regla("unica que dice 'menos'",  "lexico", function(o) unico(grepl("menos", o))),
  nueva_regla("unica que dice 'modo'",   "lexico", function(o) unico(grepl("de modo que", o))),
  nueva_regla("centroide lexico",        "lexico",
              function(o) { L <- lex(o); voc <- table(unlist(L))
                            sc <- vapply(L, function(w) sum(voc[w]) / length(w), 0); uniq_ext(sc, max) }),

  ## O1 / §P7-E: la FORMA de regla que faltaba. Todas las anteriores eran "la
  ## UNICA que ..."; ninguna era "el GRUPO que ...". Un grupo de 2 sobre 4 rinde
  ## 50 % y ninguna sonda de unicidad lo ve. Es la misma ceguera que §P7-E nombra,
  ## un piso mas abajo: no basta declarar las familias, hay que preguntarse que
  ## forma de regla no se ha escrito.
  nueva_regla("GRUPO: las que dicen 'cota'",   "lexico",
              function(o) { i <- which(grepl("cota", o)); if (length(i) %in% c(1L,2L,3L)) i else NA_integer_ }),
  nueva_regla("GRUPO: las que dicen 'por hora'", "lexico",
              function(o) { i <- which(grepl("por hora", o)); if (length(i) %in% c(1L,2L,3L)) i else NA_integer_ }),
  nueva_regla("GRUPO: las que dicen 'no pudo'",  "signo",
              function(o) { i <- which(grepl("no pudo|no pueden", o)); if (length(i) %in% c(1L,2L,3L)) i else NA_integer_ }),
  nueva_regla("REL: par con los mismos numerales", "formato", par_mismos_numerales),
  nueva_regla("REL: par con el mismo molde",   "formato", par_mismo_molde),
  nueva_regla("REL: par de longitud gemela",   "formato", par_long_gemela)
)

## `signo` SÍ aplica, y no como signo aritmético sino como SENTIDO binario: el
## ítem concluye "a pie" o "no a pie". Declararla no aplicable habría sido el
## error que §P7 nombra —sin sonda leído como sin señal—, porque es justamente
## la dimensión donde vive el canal de veredicto que el hermano tuvo que cerrar.
bateria <- c(bateria, list(
  nueva_regla("unica que dice 'no pudo'",   "signo", function(o) unico(grepl("no pudo|no pueden|debe descartarse", o))),
  nueva_regla("unica que dice 'a pie'",     "signo", function(o) unico(grepl("a pie", o))),
  nueva_regla("unica con veredicto distinto", "signo",
              function(o) { v <- grepl("no pudo|no pueden|debe descartarse|motorizado|increíble", o)
                            tb <- table(v); unico(v %in% names(tb)[tb == 1L] | v == as.logical(names(tb)[tb == 1L][1])) })
))
NO_APLICA <- character(0)

for (g in list(list(nm = "p1", o = op1, k = k1),
               list(nm = "p4", o = op4, k = k4),
               list(nm = "p5", o = op5, k = k5))) {
  cat("=== (D) BATERÍA §P7 — gap ", g$nm, " ===\n", sep = "")
  res <- evaluar_bateria(bateria, g$o, g$k, familias_no_aplicables = NO_APLICA)
  imprimir_bateria(res)
  cat("\n")
}

## ===========================================================================
## (E) VARA DE LA INSTANCIA CANÓNICA — enumeración EXACTA, no muestra (§P7-E)
## La canónica es determinista: no le aplica el mínimo de 20 por estrato de la
## regla #23. Si la fuente OFICIAL tiene el canal peor, §P7-A lo resuelve.
## ===========================================================================
i_can <- which(canon)[1]
if (!is.na(i_can)) {
  cat("=== (E) INSTANCIA CANÓNICA (ítem oficial, enumeración exacta) ===\n")
  oc <- op5[[i_can]]; kc <- k5[i_can]
  for (r in bateria) {
    ## O6(b) del detractor: la POSICIÓN no sobrevive a `exshuffle: TRUE` ni a la
    ## mezcla interna, así que reportarla como "resuelve el ítem oficial" es un
    ## artefacto de la semilla e INFLA la vara que sostiene el override de §5.
    if (identical(r$familia, "posicion")) next
    S <- tryCatch(r$fn(oc), error = function(e) NA_integer_)
    if (length(S) == 1L && is.na(S[1])) next
    if (is.logical(S)) S <- which(S)
    if (!length(S)) next
    if (kc %in% S) cat(sprintf("  %-32s [%s]  score = 1/%d %s\n", r$nombre, r$familia,
                               length(S), if (length(S) == 1L) "<- RESUELVE EL ITEM OFICIAL" else ""))
  }
  cat("\n")
} else cat("=== (E) sin instancia canónica en la muestra ===\n\n")

cat("=== VEREDICTO DEL VERIFICADOR ===\n")
cat("corrección/solidez/coherencia:", if (length(errs) == 0L) "0 errores" else
    paste(length(errs), "ERRORES"), "\n")
cat("control positivo de la sonda de solidez:",
    if (is.na(cp_hits)) "NO ACREDITADO" else sprintf("%d/400 (dispara)", cp_hits), "\n")
quit(status = if (length(errs) == 0L && !is.na(cp_hits) && cp_hits > 0L) 0L else 1L)
