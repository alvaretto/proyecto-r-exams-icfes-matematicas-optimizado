## verificar_explicaciones.R — Paso 8, ronda 3: cada operando que cita el «Análisis de los distractores» de la Parte 1
## (y el rótulo ERR-xxx de su encabezado) se recalcula desde la tabla MOSTRADA, por tipo de error y por rama.
suppressMessages(library(exams))
F <- normalizePath("../../totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd")
for (e in parse("verificar_coherencias.R", encoding = "UTF-8"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in%
      c("nb", "limpiar", "num", "eval_p1")) eval(e, globalenv())
sem <- 1:100
ex <- xexams(F, n = 100, seed = sem, driver = list(sweave = list(quiet = TRUE, pdf = FALSE, png = TRUE, encoding = "UTF-8"),
                                                   read = NULL, transform = NULL, write = NULL))
fallos <- character(0); cuenta <- list()
for (i in seq_along(sem)) {
  e <- ex[[i]][[1]]; q <- nb(e$question); sol <- nb(e$solution); ql <- nb(e$questionlist)
  tl <- grep("^\\|", q, value = TRUE); tl <- tl[!grepl("^\\|[-:| ]+\\|$", tl)]
  cel <- lapply(tl, function(r) limpiar(strsplit(sub("\\|\\s*$", "", sub("^\\|", "", r)), "|", fixed = TRUE)[[1]]))
  n <- c(num(cel[[2]][2]), num(cel[[3]][2])); a <- c(num(cel[[2]][3]), num(cel[[3]][3])); fil <- c(cel[[2]][1], cel[[3]][1])
  tt <- n * a; g <- which.max(tt); p <- 3L - g
  en1 <- grep("La tabla muestra la cantidad de cursos", q, value = TRUE)[1]
  nm <- regmatches(en1, regexec("por curso en (.+) y en (.+)\\.$", en1))[[1]][2:3]
  nG <- n[g]; nP <- n[p]; aG <- a[g]; aP <- a[p]; TG <- tt[g]; TP <- tt[p]; D <- TG - TP
  k <- abs(nG - nP); dl <- abs(aG - aP); rama <- if (nG > nP) "CURSOS" else "PROMEDIO"
  parG <- if (rama == "CURSOS") k * aG else nG * dl
  parP <- if (rama == "CURSOS") nP * dl else k * aP
  nomG <- nm[g]; nomP <- nm[p]
  heads <- grep("^\\*\\*Parte 1 · ERR-", sol)
  err <- function(m) fallos <<- c(fallos, sprintf("semilla %d: %s", sem[i], m))
  if (length(heads) != 3L) { err(paste("hay", length(heads), "bloques de distractores, no 3")); next }
  falsas <- ql[1:4][!as.logical(e$metainfo$solution[[1]])]
  afirm <- character(0)
  for (h in heads) {
    cod <- sub("^\\*\\*Parte 1 · (ERR-[A-Z]+) — .*$", "\\1", sol[h])
    ia <- h + which(startsWith(sol[(h + 1):length(sol)], "*Afirmación:* «"))[1]
    af <- sub("»$", "", sub("^\\*Afirmación:\\* «", "", sol[ia])); afirm <- c(afirm, af)
    il <- h + which(startsWith(sol[(h + 1):length(sol)], "*Lectura que lleva a elegirla:*"))[1]
    par <- sol[il + 1 + which(nzchar(sol[(il + 1):length(sol)]))[1] - 1]
    ev <- eval_p1(af, nm, n, a); x <- ev$x; s <- ev$sede
    xs <- if (!is.na(x)) x else if (!is.na(ev$xp)) ev$xp else NA_real_     # figura: «en x kilos» o «con x kilos de más»
    esp <- switch(cod,
      "ERR-DATO"    = { ok_x <- xs %in% c(aG, aP); ok_s <- s == g; c(xs, nG, aG, TG, nP, aP, TP, D) },
      "ERR-RESTA"   = { ok_x <- xs == dl; ok_s <- s == g; c(xs, max(aG, aP), min(aG, aP), dl, nG, nP, TG, TP, D) },
      "ERR-PARCIAL" = { ok_x <- xs == parG; ok_s <- s == g
        if (rama == "CURSOS") c(k, aG, xs, if (k > 1) k, dl, nP, dl, parP, xs, parP, D)
        else c(nG, dl, xs, nG, k, k, aP, parP, xs, parP, D) },
      "ERR-TOTAL"   = { ok_x <- xs == TG; ok_s <- s == g; c(xs, nG, aG, TG, TG, TP, D) },
      "ERR-PROM"    = { ok_x <- xs %in% c(parP, dl); ok_s <- s == p
        if (grepl("tuvo mayor promedio con", af)) c(dl, nP, aP, TP, TG)       # molde de la canónica
        else c(dl, nP, dl, xs, nP, k, k, aG, parG, TP, TG) },
      "ERR-CURSOS"  = { ok_x <- xs == parP; ok_s <- s == p; c(k, k, aP, xs, dl, nG, dl, parG, TP, TG) },
      "ERR-INVENTA" = { ok_x <- is.na(xs) || !(xs %in% c(aG, aP, dl, parG, parP, TG, TP, D)); ok_s <- s == p
        if (is.na(xs)) c(TP, TG) else c(xs, D, TP, TG) },
      { ok_x <- FALSE; ok_s <- FALSE; numeric(0) })
    if (!isTRUE(ok_x)) err(sprintf("%s: la cifra %s de la afirmación no corresponde a ese tipo de error (G=%s, P=%s)", cod, xs, nomG, nomP))
    if (!isTRUE(ok_s)) err(sprintf("%s: la afirmación nombra a la sede equivocada", cod))
    got <- num(par)
    if (!identical(as.numeric(got), as.numeric(esp))) err(sprintf("%s (rama %s): cifras de la explicación %s ≠ recalculadas %s", cod, rama, paste(got, collapse = ","), paste(esp, collapse = ",")))
    ## la sede de la que habla la explicación
    if (cod == "ERR-DATO") {
      sede_dato <- if (xs == aG) nomG else nomP
      if (!grepl(paste0("es el promedio de ", sede_dato, ","), par, fixed = TRUE)) err("ERR-DATO: la explicación atribuye el promedio a otra sede")
    }
    cuenta[[length(cuenta) + 1L]] <- c(cod = cod, rama = rama)
  }
  if (!setequal(afirm, falsas)) err("las tres afirmaciones analizadas no son las tres falsas de la Parte 1")
}
cc <- do.call(rbind, cuenta)
cat("Bloques de distractores analizados:", nrow(cc), "en", length(sem), "versiones\n")
print(table(Error = cc[, "cod"], Rama = cc[, "rama"]))
cat("\nFallos:", length(fallos), "\n"); if (length(fallos)) cat(paste0(" * ", fallos), sep = "\n")
