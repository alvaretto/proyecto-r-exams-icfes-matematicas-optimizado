## Enumeración EXACTA del espacio de versiones ejecutando el chunk data_generation real: se sustituye sample.int y se fuerzan
## las cuatro decisiones que fijan las opciones (x, n, reparto b y par de distractores). El orden de las opciones (permutación)
## se deja en la identidad (la clave queda primera) y las elecciones narrativas en la primera alternativa: no cambian las opciones.
## Devuelve una lista de configuraciones con su peso exacto (producto de 1/n de cada decisión).
enumerar_configs <- function(rmd) {
  l <- readLines(rmd, encoding = "UTF-8"); i0 <- grep("^```\\{r data_generation", l); i1 <- which(l == "```")[which(l == "```") > i0][1]
  code <- parse(text = l[(i0 + 1):(i1 - 1)], encoding = "UTF-8")
  correr <- function(pref) {
    i <- 0L; ns <- integer(0); e <- new.env()
    e$sample.int <- function(n, size = n, ...) {
      if (size != 1L) return(seq_len(size))
      i <<- i + 1L
      if (i > 4L) return(1L)
      ns[i] <<- n
      if (i <= length(pref)) pref[i] else 1L
    }
    eval(code, e)
    list(ns = ns, e = e)
  }
  pendientes <- list(integer(0)); out <- list()
  while (length(pendientes)) {
    pref <- pendientes[[1L]]; pendientes <- pendientes[-1L]; r <- correr(pref)
    stopifnot("el chunk hace exactamente 4 decisiones de una opción" = length(r$ns) == 4L)
    if (length(pref) < 4L) { for (j in seq_len(r$ns[length(pref) + 1L])) pendientes <- c(pendientes, list(c(pref, j))) ; next }
    e <- r$e
    out[[length(out) + 1L]] <- list(peso = prod(1 / r$ns), x = e$x, n = e$n, D = e$D, num = e$numeradores, sol = e$sol,
                                     sel = e$sel, rama = sprintf("x=%d,n=%d", e$x, e$n))
  }
  stopifnot("los pesos suman 1" = abs(sum(vapply(out, `[[`, 0, "peso")) - 1) < 1e-9)
  out
}
## Tasa exacta de cada regla con la convención del helper (1/|S| si la clave sobrevive; 1/4 si no elimina nada o elimina todo),
## sobre todas las versiones (diluida) y sobre las versiones en que la regla aplica (0 < |S| < 4); peor rama sobre aplicables.
tabla_exacta <- function(reglas, cfg) {
  w <- vapply(cfg, `[[`, 0, "peso"); rama <- vapply(cfg, `[[`, "", "rama")
  do.call(rbind, lapply(reglas, function(r) {
    sc <- ap <- numeric(length(cfg))
    for (i in seq_along(cfg)) { c <- cfg[[i]]; o <- c$num
      s <- if (isTRUE(r$usa_estimulo)) r$fn(o, list(D = c$D, x = c$x)) else r$fn(o)
      s <- if (is.logical(s)) s & !is.na(s) else seq_along(o) %in% s
      k <- which(c$sol == 1L); m <- sum(s)
      ap[i] <- m > 0 && m < length(o); sc[i] <- if (m == 0 || m == length(o)) 0.25 else if (s[k]) 1 / m else 0 }
    t_dil <- sum(w * sc); t_ap <- if (sum(w * ap) > 0) sum(w * sc * ap) / sum(w * ap) else NA
    pr <- tapply(seq_along(cfg), rama, function(ii) if (sum(w[ii] * ap[ii]) > 0) sum(w[ii] * sc[ii] * ap[ii]) / sum(w[ii] * ap[ii]) else NA)
    data.frame(regla = r$nombre, familia = r$familia, aplica_pct = round(100 * sum(w * ap), 1),
               exceso_pp = round(100 * (t_ap - 0.25), 1), exceso_diluido_pp = round(100 * (t_dil - 0.25), 1),
               peor_rama_pp = round(100 * (max(pr, na.rm = TRUE) - 0.25), 1), peor_rama = names(pr)[which.max(pr)])
  }))
}
