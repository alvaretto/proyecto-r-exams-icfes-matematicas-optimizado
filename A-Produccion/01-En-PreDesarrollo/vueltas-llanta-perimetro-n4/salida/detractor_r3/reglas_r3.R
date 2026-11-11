## Atajos medidos sobre las opciones tal como se ven (sin $). S = conjunto que señala el atajo; crédito 1/|S| si la clave está en S; S vacío = 1/4.
tok3 <- function(x) { x <- gsub("$", "", x, fixed = TRUE); x <- gsub("\\,", "", x, fixed = TRUE)
  x <- gsub("\\dfrac{", " F( ", x, fixed = TRUE); x <- gsub("}{", " / ", x, fixed = TRUE); x <- gsub("}", " ) ", x, fixed = TRUE)
  x <- gsub("\\times", " x ", x, fixed = TRUE); x <- gsub("\\div", " d ", x, fixed = TRUE); strsplit(trimws(gsub("\\s+", " ", x)), " ")[[1]] }
partes <- function(x, i) { m <- regmatches(x, gregexpr("\\\\dfrac\\{[^{}]*\\}\\{[^{}]*\\}", x))[[1]]; sub("\\\\dfrac\\{([^{}]*)\\}\\{([^{}]*)\\}", paste0("\\", i), m) }
fuera <- function(x) gsub("\\\\dfrac\\{[^{}]*\\}\\{[^{}]*\\}", " FR ", x)
gemelas <- function(o) { ts <- lapply(o, tok3); n <- length(o); g <- rep(FALSE, n)
  for (a in 1:(n - 1)) for (b in (a + 1):n) { x <- ts[[a]]; y <- ts[[b]]
    if (length(x) == length(y)) { dif <- which(x != y)
      if (length(dif) == 1L || (length(dif) == 2L && x[dif[1]] == y[dif[2]] && x[dif[2]] == y[dif[1]])) { g[a] <- TRUE; g[b] <- TRUE } } }
  g }
bigr3 <- function(t) paste(head(t, -1), tail(t, -1))
sJ3 <- function(o) { ts <- lapply(o, tok3); colSums(sapply(ts, function(a) sapply(ts, function(z) length(intersect(bigr3(a), bigr3(z))) / length(union(bigr3(a), bigr3(z)))))) - 1 }
sL3 <- function(o) { ts <- lapply(o, tok3); u <- unique(unlist(ts)); ch <- intToUtf8(seq_along(u) + 64L, multiple = TRUE)
  rowSums(adist(vapply(ts, function(t) paste(ch[match(t, u)], collapse = ""), ""))) }
nops <- function(o) lengths(regmatches(o, gregexpr("\\\\times|\\\\div", o)))
R3 <- list(
  "V1 centralidad"                 = function(o, x) { v <- sJ3(o); abs(v - max(v)) < 1e-12 },
  "V1 medoide"                     = function(o, x) { v <- sL3(o); v == min(v) },
  "usa K, d y la letra"            = function(o, x) vapply(o, function(s) all(c(x$K, x$d, x$V) %in% tok3(s)), TRUE),
  "ancho TeX (tol 0,01 pt)"        = function(o, x) abs(x$w - max(x$w)) < 0.01,
  "ancho TeX (tol 1 pt)"           = function(o, x) x$w >= max(x$w) - 1,
  "estrecha TeX (tol 0,01 pt)"     = function(o, x) abs(x$w - min(x$w)) < 0.01,
  "letra arriba (conjunto)"        = function(o, x) vapply(o, function(s) any(partes(s, 1) == x$V), TRUE),
  "letra abajo (conjunto)"         = function(o, x) vapply(o, function(s) any(partes(s, 2) == x$V), TRUE),
  "letra fuera de fraccion"        = function(o, x) vapply(o, function(s) x$V %in% strsplit(fuera(s), "[ ]+")[[1]], TRUE),
  "K abajo (conjunto)"             = function(o, x) vapply(o, function(s) any(partes(s, 2) == x$K), TRUE),
  "contiene x (conjunto)"          = function(o, x) grepl("\\times", o, fixed = TRUE),
  "contiene div (conjunto)"        = function(o, x) grepl("\\div", o, fixed = TRUE),
  "sin operador"                   = function(o, x) nops(o) == 0L,
  "un operador"                    = function(o, x) nops(o) == 1L,
  "dos operadores"                 = function(o, x) nops(o) == 2L,
  "contiene K/V del enunciado"     = function(o, x) grepl(paste0("\\dfrac{", x$K, "}{", x$V, "}"), o, fixed = TRUE),
  "NO contiene K/V del enunciado"  = function(o, x) !grepl(paste0("\\dfrac{", x$K, "}{", x$V, "}"), o, fixed = TRUE),
  "empieza por la distancia"       = function(o, x) startsWith(o, paste0(x$d, " ")),
  "divide entre una fraccion"      = function(o, x) grepl("\\div \\dfrac", o, fixed = TRUE),
  "multiplica por una fraccion"    = function(o, x) grepl("\\times \\dfrac", o, fixed = TRUE),
  "gemelas (1 simbolo o intercambio)" = function(o, x) gemelas(o))
cred <- function(S, k = 1L) { S[is.na(S)] <- FALSE; n <- sum(S); if (n == 0L) 0.25 else if (S[k]) 1 / n else 0 }
