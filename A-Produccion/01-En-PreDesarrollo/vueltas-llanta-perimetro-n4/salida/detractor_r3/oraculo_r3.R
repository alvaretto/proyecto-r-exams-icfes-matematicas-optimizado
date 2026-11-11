args <- commandArgs(TRUE); rmd <- args[1]; N <- as.integer(args[2]); DIRS <- args[3]
suppressMessages(library(exams)); source(file.path(DIRS, "reglas_r3.R"))
cat("md5:", unname(tools::md5sum(rmd)), "\n")
L <- readLines(rmd, encoding = "UTF-8")
chunk <- function(label) { i0 <- grep(paste0("^```\\{r ", label, "[ ,}]"), L); i1 <- which(L == "```"); i1 <- i1[i1 > i0][1]; paste(L[(i0 + 1):(i1 - 1)], collapse = "\n") }
CG <- parse(text = chunk("data_generation")); CE <- parse(text = chunk("enunciado")); CO <- parse(text = chunk("opciones"))
CS <- parse(text = chunk("solucion")); CF <- parse(text = chunk("feedback"))
lex <- function(s) { s <- gsub("\\,", "", s, fixed = TRUE); s <- gsub("$", "", s, fixed = TRUE); out <- character(0); i <- 1L; n <- nchar(s)
  while (i <= n) { ch <- substr(s, i, i); if (ch == " ") { i <- i + 1L; next }
    if (grepl("[0-9.]", ch)) { j <- i; while (j <= n && grepl("[0-9.]", substr(s, j, j))) j <- j + 1L; out <- c(out, substr(s, i, j - 1L)); i <- j
    } else if (ch == "\\") { m <- regmatches(substr(s, i, n), regexpr("^\\\\[a-zA-Z]+", substr(s, i, n))); out <- c(out, m); i <- i + nchar(m)
    } else { out <- c(out, ch); i <- i + 1L } }
  out }
pe <- function(s, vars) { tk <- lex(s); pos <- 1L
  nxt <- function() { t <- tk[pos]; pos <<- pos + 1L; t }; peek <- function() if (pos <= length(tk)) tk[pos] else ""
  expect <- function(x) { t <- nxt(); if (!identical(t, x)) stop("esperaba ", x) }
  term <- function() { t <- nxt()
    if (identical(t, "\\dfrac")) { expect("{"); a <- ex(); expect("}"); expect("{"); b <- ex(); expect("}"); return(a / b) }
    if (isTRUE(grepl("^[0-9.]+$", t))) return(as.numeric(t)); if (isTRUE(t %in% names(vars))) return(vars[[t]]); stop("token: ", t) }
  ex <- function() { v <- term(); while (peek() %in% c("\\times", "\\div")) { op <- nxt(); w <- term(); v <- if (op == "\\times") v * w else v / w }; v }
  v <- ex(); if (pos <= length(tk)) stop("sobran tokens"); v }
NB <- " "; numtxt <- function(x) as.numeric(gsub(",", ".", gsub(paste0("[", NB, " ]|\\\\,"), "", x)))
CONV <- c("cm m" = 100, "mm m" = 1000, "m km" = 1000); ABR <- c("centímetros" = "cm", "milímetros" = "mm", "metros" = "m")
UD <- c("metro" = "m", "kilómetro" = "km"); UDPL <- c("metros" = "m", "kilómetros" = "km")
FORM <- list(C = function(K, d, P) K * d / P, A = function(K, d, P) K / d, B = function(K, d, P) d * P / K, D = function(K, d, P) K / P,
             E = function(K, d, P) d / P, F = function(K, d, P) K / (P * d), G = function(K, d, P) K * K * d / P)
PT <- c(37, 211.5, 1234)
ident <- function(op, K, d, V) { v <- tryCatch(vapply(PT, function(P) pe(op, setNames(list(P), V)), 0), error = function(e) rep(NA_real_, 3))
  h <- names(FORM)[vapply(FORM, function(f) isTRUE(all(abs(v - f(K, d, PT)) <= 1e-9 * abs(f(K, d, PT)))), TRUE)]; if (length(h) == 1L) h else "?" }
CAN_R <- "La cantidad de vueltas que da una llanta para recorrer un metro es igual a 100 dividido entre el perímetro de la llanta, donde el perímetro de la llanta se expresa en centímetros."
CAN_Q <- "¿Cuál de las siguientes expresiones permite determinar la cantidad de vueltas que debe dar una llanta de perímetro **P** (en cm), para recorrer 150 metros?"
SPAN <- "<span style=\"display:inline-block;margin:0.3em 0\">"
prob <- character(0); addp <- function(s, m) prob <<- c(prob, sprintf("s%d: %s", s, m))
META <- list(); CR <- list(); ej <- list()
for (s in seq_len(N)) {
  set.seed(s); e <- new.env(); r <- tryCatch({ eval(CG, e); NULL }, error = function(err) conditionMessage(err))
  if (!is.null(r)) { addp(s, paste("muere:", r)); next }
  q <- paste(capture.output(eval(CE, e)), collapse = "\n"); oo <- capture.output(eval(CO, e))
  so <- paste(capture.output(eval(CS, e)), collapse = "\n"); fb <- capture.output(eval(CF, e))
  mR <- regmatches(q, regexec("para recorrer un (metro|kilómetro) es igual a ([0-9]+) dividido entre el perímetro (de la|del) ([a-z]+), donde el perímetro (de la|del) ([a-z]+) se expresa en (centímetros|milímetros|metros)\\.", q))[[1]]
  mQ <- regmatches(q, regexec("¿Cuál de las siguientes expresiones permite determinar la cantidad de vueltas que debe dar (una|un) ([a-z]+) de perímetro \\*\\*([Pp])\\*\\* \\(en (cm|mm|m)\\), para recorrer ([0-9]+) (metros|kilómetros)\\?", q))[[1]]
  if (length(mR) != 8L || length(mQ) != 7L) { addp(s, "enunciado ilegible"); next }
  K <- as.numeric(mR[3]); V <- mQ[4]; up <- mQ[5]; d <- as.numeric(mQ[6]); udpl <- mQ[7]; nuc <- mR[5]
  if (ABR[[mR[8]]] != up || UD[[mR[2]]] != UDPL[[udpl]]) addp(s, "unidades: regla y pregunta no coinciden")
  if (CONV[[paste(up, UDPL[[udpl]])]] != K) addp(s, "K no es la conversión de las unidades del texto")
  if (mR[6] != mR[4] || mR[7] != nuc || mQ[3] != nuc) addp(s, "objeto incoherente")
  if (!((nuc %in% c("llanta", "rueda") && mR[4] == "de la" && mQ[2] == "una") || (nuc == "rodillo" && mR[4] == "del" && mQ[2] == "un"))) addp(s, "género")
  for (x in regmatches(q, gregexpr("[0-9]+ (metros|kilómetros)", q))[[1]]) if (numtxt(sub(" .*", "", x)) != d || sub("^[0-9]+ ", "", x) != udpl) addp(s, paste("cifra con unidad ajena:", x))
  it <- sub("^\\* ", "", grep("^\\* ", oo, value = TRUE))
  if (length(it) != 4L || !all(startsWith(it, SPAN) & endsWith(it, "$</span>"))) addp(s, "envoltorio de opciones")
  inner <- gsub("^\\$|\\$$", "", substr(it, nchar(SPAN) + 1L, nchar(it) - 7L))
  if (anyDuplicated(inner)) addp(s, "opciones duplicadas")
  okv <- vapply(inner, function(o) { v <- tryCatch(vapply(PT, function(P) pe(o, setNames(list(P), V)), 0), error = function(err) rep(NA_real_, 3)); isTRUE(all(abs(v - K * d / PT) <= 1e-9 * K * d / PT)) }, TRUE)
  exs <- mchoice2string(as.logical(e$sol)); marc <- which(strsplit(exs, "")[[1]] == "1")
  if (sum(okv) != 1L) addp(s, sprintf("%d opciones correctas", sum(okv))); if (!identical(unname(which(okv)), marc)) addp(s, "la marcada no es la correcta")
  codes <- vapply(inner, ident, "", K = K, d = d, V = V); if (any(codes == "?")) addp(s, "opción que no identifico")
  if (isTRUE(e$es_canonica)) {
    if (!startsWith(q, CAN_R) || !grepl(CAN_Q, q, fixed = TRUE)) addp(s, "canónica: enunciado no verbatim")
    if (!setequal(inner, c("\\dfrac{100}{150}", "150 \\times \\dfrac{P}{100}", "\\dfrac{100}{P} \\times 150", "\\dfrac{100}{P}")) || inner[marc] != "\\dfrac{100}{P} \\times 150") addp(s, "canónica: opciones o clave")
  }
  so2 <- gsub("(?s)```\\{=latex\\}.*?```", "", so, perl = TRUE); par <- strsplit(so2, "\n\n", fixed = TRUE)[[1]]
  m1 <- regmatches(so2, regexec("La expresión correcta es \\$([^$]+)\\$, que equivale a \\$\\\\dfrac\\{([0-9\\\\, ]+)\\}\\{([Pp])\\}\\$ vueltas\\.", so2))[[1]]
  if (length(m1) != 4L || m1[2] != inner[marc] || numtxt(m1[3]) != K * d || m1[4] != V) addp(s, "Respuesta correcta: texto")
  m2 <- regmatches(so2, regexec("La opción correcta escribe lo mismo de otra manera, \\$([^$]+)\\$:", so2))[[1]]
  if (isTRUE(e$es_canonica)) { if (length(m2)) addp(s, "canónica con línea de forma") } else if (length(m2) != 2L || m2[2] != inner[marc]) addp(s, "línea de forma")
  m3 <- regmatches(so2, regexec(paste0("tuviera un perímetro de ([0-9]+) (cm|mm|m): ([0-9]+) (metros|kilómetros) son ([0-9", NB, "]+) (centímetros|milímetros|metros), y ([0-9", NB, "]+) ÷ ([0-9]+) = ([0-9", NB, "]+) vueltas\\. La expresión correcta da lo mismo: \\$([^$]+) = ([0-9\\\\, ]+)\\$"), so2))[[1]]
  Pref <- NA
  if (length(m3) != 12L) addp(s, "caso específico ilegible") else { Pref <- as.numeric(m3[2])
    if (m3[3] != up || as.numeric(m3[4]) != d || m3[5] != udpl || numtxt(m3[6]) != K * d || ABR[[m3[7]]] != up || numtxt(m3[8]) != K * d || as.numeric(m3[9]) != Pref) addp(s, "caso: datos")
    R <- numtxt(m3[10]); if (abs(K * d / Pref - R) > 1e-9) addp(s, "caso: división")
    if (m3[11] != gsub(V, m3[2], inner[marc], fixed = TRUE)) addp(s, "caso: no es la clave con el perímetro")
    if (!isTRUE(abs(tryCatch(pe(m3[11], list()), error = function(err) NA) - R) <= 1e-9) || numtxt(m3[12]) != R) addp(s, "caso: la expresión no da el resultado") }
  for (f in regmatches(so2, gregexpr("\\$[^$]+\\$", so2))[[1]]) if (grepl("=", f)) { sd <- strsplit(gsub("$", "", f, fixed = TRUE), "=", fixed = TRUE)[[1]]
    vv <- lapply(sd, function(x) tryCatch(vapply(PT, function(P) pe(x, setNames(list(P), V)), 0), error = function(err) rep(NA_real_, 3)))
    if (anyNA(unlist(vv)) || any(abs(vv[[1]] - vv[[2]]) > 1e-9 * abs(vv[[1]]))) addp(s, paste("ecuación falsa o ilegible:", f)) }
  dg <- regmatches(so2, gregexpr("\\*\\*(VL-[A-G]) — [^*]+\\*\\* La expresión \\$([^$]+)\\$", so2))[[1]]
  dcod <- sub("^\\*\\*(VL-[A-G]) — .*", "\\1", dg); dexp <- sub(".*La expresión \\$([^$]+)\\$$", "\\1", dg)
  if (length(dg) != 3L || !setequal(dexp, inner[-marc])) addp(s, "diagnóstico no cubre exactamente las 3 incorrectas")
  for (j in seq_along(dexp)) if (paste0("VL-", ident(dexp[j], K, d, V)) != dcod[j]) addp(s, sprintf("diagnóstico rotula %s y la expresión es otra", dcod[j]))
  pB <- par[startsWith(par, "**VL-B")]
  if (length(pB)) { xB <- dexp[dcod == "VL-B"]
    mm <- regmatches(pB, regexec("La expresión \\$[^$]+\\$ (multiplica la distancia por|divide la distancia entre) \\$([^$]+)\\$", pB))[[1]]
    if (length(mm) == 3L) { if (!grepl(paste0(d, if (startsWith(mm[2], "multiplica")) " \\times " else " \\div ", mm[3]), xB, fixed = TRUE)) addp(s, "B_CORTA no describe B")
    } else if (grepl(paste0("divide la distancia entre ", K, " y multiplica el resultado por el perímetro"), pB, fixed = TRUE)) {
      if (xB != paste0("\\dfrac{", d, "}{", K, "} \\times ", V)) addp(s, "B_CORTA (fn) no describe B") } else addp(s, "B_CORTA ilegible") }
  pG <- par[startsWith(par, "**VL-G")]
  if (length(pG)) { xG <- dexp[dcod == "VL-G"]
    if (grepl("y además divide entre el perímetro escrito en", pG, fixed = TRUE)) { if (xG != paste0(K, " \\times ", d, " \\div \\dfrac{", V, "}{", K, "}") || !grepl(paste0("$", K, " \\times ", d, "$ son los"), pG, fixed = TRUE)) addp(s, "G (cociente) no describe G")
    } else if (grepl("multiplica dos veces por", pG, fixed = TRUE)) { if (xG != paste0("\\dfrac{", d, "}{", V, "} \\times ", K, " \\times ", K) || !grepl(paste0("$\\dfrac{", d, "}{", V, "} \\times ", K, "$ ya da las vueltas"), pG, fixed = TRUE)) addp(s, "G (fn) no describe G")
    } else addp(s, "G con texto de la canónica u otro") }
  fit <- sub("^\\* ", "", grep("^\\* ", fb, value = TRUE)); if (length(fit) != 4L) addp(s, "feedback: no son 4")
  for (i in seq_along(fit)) { t <- fit[i]
    if (i == marc) { mN <- regmatches(t, regexec(paste0("Con un perímetro de ([0-9]+) (cm|mm|m) da ([0-9", NB, "]+) vueltas, lo mismo que el cálculo directo\\.$"), t))[[1]]
      if (!startsWith(t, "Correcta. ") || length(mN) != 4L || numtxt(mN[4]) != K * d / as.numeric(mN[2]) || as.numeric(mN[2]) != Pref) addp(s, "feedback de la clave")
      kx <- inner[marc]
      if (grepl("es el perímetro escrito en", t, fixed = TRUE)) { fx <- sub("^Correcta\\. \\$([^$]+)\\$ es el perímetro.*", "\\1", t)
        if (fx != paste0("\\dfrac{", V, "}{", K, "}") || kx != paste0(d, " \\div ", fx)) addp(s, "TXT_CLAVE (cociente)")
      } else if (startsWith(t, "Correcta. Multiplicar por")) { if (kx != paste0("\\dfrac{", d, "}{", V, "} \\times ", K)) addp(s, "TXT_CLAVE (fn)")
      } else if (grepl("son las vueltas en un", t, fixed = TRUE)) { if (kx != paste0("\\dfrac{", K, "}{", V, "} \\times ", d)) addp(s, "TXT_CLAVE (canónica)")
      } else addp(s, "TXT_CLAVE desconocido")
    } else { mX <- regmatches(t, regexec(paste0("Con un perímetro de ([0-9]+) (cm|mm|m), esta expresión da (aproximadamente )?([0-9", NB, ",]+), y el número correcto de vueltas es ([0-9", NB, "]+)\\.$"), t))[[1]]
      if (!startsWith(t, "Incorrecta. ") || length(mX) != 6L) { addp(s, paste("feedback ilegible:", t)); next }
      Pf <- as.numeric(mX[2]); x <- numtxt(mX[5]); ve <- tryCatch(pe(inner[i], setNames(list(Pf), V)), error = function(err) { addp(s, paste0("no evalúo «", inner[i], "» (", conditionMessage(err), ")")); NA_real_ }); if (is.na(ve)) next
      if (Pf != Pref) addp(s, "feedback con otro perímetro")
      if (nzchar(mX[4])) { nd <- nchar(sub(".*,", "", mX[5])); if (!grepl(",", mX[5]) || abs(ve - x) > 0.5 * 10^(-nd) + 1e-12 || abs(ve - x) < 1e-12) addp(s, sprintf("aprox. %s vs %.6f", mX[5], ve))
      } else if (abs(ve - x) > 1e-9 * max(1, abs(ve))) addp(s, sprintf("feedback dice %s y la expresión da %.6f", mX[5], ve))
      if (numtxt(mX[6]) != K * d / Pf) addp(s, "feedback: número correcto")
      cj <- codes[i]; pref <- switch(cj, A = "Incorrecta\\. No contiene el perímetro", B = "Incorrecta\\. (Multiplica|Equivale a multiplicar) la distancia por la longitud de una vuelta",
        D = "Incorrecta\\. Son las vueltas para un solo", E = "Incorrecta\\. Divide (metros|kilómetros) entre", F = "Incorrecta\\. Divide entre la distancia", G = "Incorrecta\\. Aplica dos veces la conversión", "X")
      if (!grepl(paste0("^", pref), t)) addp(s, sprintf("feedback no es el del error %s", cj))
      if (cj == "B" && (inner[i] == paste0(d, " \\times \\dfrac{", V, "}{", K, "}")) != startsWith(t, "Incorrecta. Multiplica")) addp(s, "feedback B: Multiplica/Equivale")
      if (cj == "A" && grepl(V, inner[i], fixed = TRUE)) addp(s, "A contiene la letra")
      if (cj %in% c("E", "G")) { mv <- regmatches(t, regexec("es ([0-9]+) veces (mayor|menor)", t))[[1]]; ra <- ve / (K * d / Pf)
        if (length(mv) != 3L || abs((if (mv[3] == "mayor") ra else 1 / ra) - as.numeric(mv[2])) > 1e-9) addp(s, "veces mayor/menor") } } }
  todo <- paste(q, so2, paste(fit, collapse = " "))
  if (grepl("\\bNA\\b", todo)) addp(s, "NA en el texto"); if (grepl("[Oo]pci[oó]n [A-D]\\b|\\b[A-D]\\)", todo)) addp(s, "letra de opción (regla #19)")
  if (grepl("\\b(la|una|de la) rodillo\\b|\\b(el|un|del) (llanta|rueda)\\b", todo)) addp(s, "concordancia")
  META[[length(META) + 1L]] <- data.frame(s = s, can = isTRUE(e$es_canonica), estilo = e$estilo, sis = e$o$sis, V = V, sel = paste(sort(codes[-marc]), collapse = ""), Pref = Pref)
  if (!isTRUE(e$es_canonica)) { x <- list(K = as.character(K), d = as.character(d), V = V, w = vapply(inner, e$ancho_tex, 0))
    CR[[length(CR) + 1L]] <- list(s = s, estilo = e$estilo, M = t(vapply(names(R3), function(rn) { S <- R3[[rn]](inner, x); vapply(1:4, function(k) cred(S, k), 0) }, numeric(4))), k = marc) }
  key <- paste(e$estilo, e$o$sis); if (is.null(ej[[key]])) ej[[key]] <- paste0("### ", key, " (s", s, ")\n", q, "\n", paste(inner, collapse = " | "), "  [clave ", marc, "]\n", so2, "\n", paste(fit, collapse = "\n"))
}
ME <- do.call(rbind, META)
cat(sprintf("ORÁCULO: %d semillas | versiones leídas %d (canónicas %d) | problemas %d\n", N, nrow(ME), sum(ME$can), length(prob)))
if (length(prob)) { tb <- sort(table(sub("^s[0-9]+: ", "", prob)), decreasing = TRUE); print(head(tb, 30)); cat(head(prob, 20), sep = "\n") }
print(table(ME$estilo, ME$sis)); print(table(ME$estilo, ME$V))
writeLines(unlist(ej), file.path(DIRS, "ejemplos_texto.md"))
## Diagnosticidad sobre la muestra (como la ve el estudiante)
fam <- function(cr, B = 2000L) { n <- length(cr); obs <- rowMeans(sapply(cr, function(z) z$M[, z$k]))
  set.seed(99); nul <- replicate(B, { r <- sample.int(4L, n, TRUE); max(rowMeans(sapply(seq_len(n), function(i) cr[[i]]$M[, r[i]]))) })
  list(obs = obs, techo = mean(nul), p95 = unname(quantile(nul, 0.95))) }
est_v <- vapply(CR, `[[`, "", "estilo")
subs <- c(list(`agregado (primeras 100)` = CR[1:100], `agregado (todas)` = CR), setNames(lapply(c("cociente", "cociente_b", "fraccion_nv"), function(f) CR[est_v == f][1:100]), paste(c("cociente", "cociente_b", "fraccion_nv"), "(100)")),
  setNames(lapply(c("cociente", "cociente_b", "fraccion_nv"), function(f) CR[est_v == f]), paste(c("cociente", "cociente_b", "fraccion_nv"), "(todas)")))
TAB <- sapply(names(subs), function(m) { F <- fam(subs[[m]], if (grepl("todas", m)) 300L else 2000L); c(100 * F$obs, techo = 100 * F$techo, p95 = 100 * F$p95, n = length(subs[[m]])) })
print(round(TAB, 1))
