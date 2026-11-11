## Pasada 2 de §P7 (FASE 2C r1, H1): tasa de cada atajo en cada celda (terna de distractores, forma de escritura).
## Es la entrada del programa lineal que elige la tabla de pesos (salida/pesos_lp.py).
## Uso: Rscript --no-init-file salida/pesos_celdas.R <ruta.Rmd> <raíz del repo> <salida.csv> [versiones por celda]
args <- commandArgs(TRUE); rmd <- args[1]; raiz <- args[2]; out <- args[3]
M <- if (length(args) > 3L) as.integer(args[4]) else 100L
cat("md5 del .Rmd:", unname(tools::md5sum(rmd)), "\n")
L <- readLines(rmd, encoding = "UTF-8"); i0 <- grep("^```\\{r data_generation", L); i1 <- which(L == "```"); i1 <- i1[i1 > i0][1]
CODE <- paste(L[(i0 + 1):(i1 - 1)], collapse = "\n")
unico <- function(code, pat) { n <- lengths(regmatches(code, gregexpr(pat, code, fixed = TRUE))); if (n != 1L) stop("patrón no único: ", pat); code }
p1 <- "celda  <- CELDAS[sample.int(nrow(CELDAS), 1L, prob = CELDAS$peso), ]"
p2 <- "activas <- which(CELDAS$peso > 0)"
CODE_F <- sub(p1, "celda  <- CELDAS[FORZADA, ]", unico(CODE, p1), fixed = TRUE)
CODE_F <- sub(p2, "activas <- integer(0)", unico(CODE_F, p2), fixed = TRUE)   # la guarda de la tabla no se mide aquí
gen <- function(code, s, f) { set.seed(s); e <- new.env(); e$FORZADA <- f; eval(parse(text = code), envir = e); e }
## Batería congelada: B2 de referencia (34) + las 2 reglas del estímulo pre-registradas en la ronda 1.
source(file.path(raiz, ".claude/scripts/bateria_eliminacion.R"), local = (BE <- new.env()))
ex <- parse(file.path(raiz, ".claude/scripts/bateria_referencia_icfes.R")); RE <- new.env()
for (x in ex) { eval(x, RE); if (exists("B2", RE, inherits = FALSE)) break }
num_ops <- function(o) as.numeric(regmatches(gsub("\\,", "", o, fixed = TRUE), gregexpr("[0-9]+", gsub("\\,", "", o, fixed = TRUE)))[[1]])
extra <- list(
  BE$nueva_regla("estimulo: contiene K y d del enunciado", "lexico", function(op, est)
    vapply(op, function(o) all(c(est$K, est$d) %in% num_ops(o)), TRUE), usa_estimulo = TRUE),
  BE$nueva_regla("estimulo: contiene la relacion K/V y otra operacion", "formato", function(op, est)
    vapply(op, function(o) grepl(paste0("\\dfrac{", est$K, "}{", est$V, "}"), o, fixed = TRUE) &&
      grepl("\\\\times|\\\\div", o), TRUE), usa_estimulo = TRUE))
reglas <- c(RE$B2, extra)
## Reglas del detractor r1 (fuera de la batería congelada), con las cifras reales de cada versión.
tok <- function(x) { x <- gsub("$", "", x, fixed = TRUE); x <- gsub("\\,", "", x, fixed = TRUE)
  x <- gsub("\\dfrac{", " F( ", x, fixed = TRUE); x <- gsub("}{", " / ", x, fixed = TRUE); x <- gsub("}", " ) ", x, fixed = TRUE)
  x <- gsub("\\times", " x ", x, fixed = TRUE); x <- gsub("\\div", " d ", x, fixed = TRUE); strsplit(trimws(gsub("\\s+", " ", x)), " ")[[1]] }
bigr <- function(t) paste(head(t, -1), tail(t, -1))
cred <- function(g, k) if (k %in% g) 1 / length(g) else 0
detr <- function(e) { op <- e$opciones; k <- which(e$sol == 1L); ts <- lapply(op, tok); u <- unique(unlist(ts))
  ch <- intToUtf8(seq_along(u) + 64L, multiple = TRUE)
  sL <- rowSums(adist(vapply(ts, function(t) paste(ch[match(t, u)], collapse = ""), "")))
  J <- sapply(ts, function(a) sapply(ts, function(z) length(intersect(bigr(a), bigr(z))) / length(union(bigr(a), bigr(z)))))
  sJ <- colSums(J) - 1
  comp <- vapply(ts, function(t) all(c(e$KT, e$dT, e$V) %in% t), TRUE)
  c(centralidad = cred(which(abs(sJ - max(sJ)) < 1e-12), k), medoide = cred(which(sL == min(sL)), k), tres_datos = cred(which(comp), k)) }
e0 <- gen(CODE_F, 1L, 1L); CEL <- e0$CELDAS
filas <- list()
for (f in seq_len(nrow(CEL))) {
  vers <- list(); s <- 0L
  while (length(vers) < M && s < 20L * M) { s <- s + 1L; e <- gen(CODE_F, 100000L * f + s, f); if (!e$es_canonica) vers[[length(vers) + 1L]] <- e }
  op <- lapply(vers, function(e) e$opciones); cl <- vapply(vers, function(e) which(e$sol == 1L), 0L)
  est <- lapply(vers, function(e) list(K = e$K, d = e$d, V = e$V))
  res <- BE$evaluar_bateria(reglas, op, cl, estimulos = est)
  det <- res$detalle; nm <- intersect(c("regla", "nombre"), names(det))[1]
  tb <- setNames(det$tasa, det[[nm]])
  dd <- rowMeans(vapply(vers, detr, c(centralidad = 0, medoide = 0, tres_datos = 0)))
  filas[[f]] <- data.frame(terna = CEL$terna[f], estilo = CEL$estilo[f], n = length(vers), t(c(dd, tb)), check.names = FALSE)
}
X <- do.call(rbind, filas); write.csv(X, out, row.names = FALSE)
cat("celdas:", nrow(X), "| versiones por celda:", M, "| reglas:", ncol(X) - 3L, "\n")
