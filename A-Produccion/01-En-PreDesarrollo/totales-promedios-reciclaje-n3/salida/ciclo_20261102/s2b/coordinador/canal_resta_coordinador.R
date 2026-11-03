## Coordinador, 2026-11-03: reproducción independiente de la regla «resta» (H1 de la FASE 2C r6), solo sobre el .Rmd.
## «resta»: si una opción cita |aG - aP| (la resta de los promedios de la tabla), su sede gana y se elige la otra opción
## de esa sede; si ninguna la cita, azar (0,25). Dos lecturas: la PRIMERA cifra de la opción (como cif() del verificador) o
## CUALQUIER cifra.
a <- commandArgs(trailingOnly = TRUE)
l <- readLines(a[1], encoding = "UTF-8"); i0 <- grep("^```\\{r data_generation", l); cs <- which(l == "```"); i1 <- cs[cs > i0][1]
EXPR <- parse(text = paste(l[(i0 + 1):(i1 - 1)], collapse = "\n"), encoding = "UTF-8")
rec <- function(s) { e <- new.env(); set.seed(s); eval(EXPR, envir = e)
  with(e, list(canon = isTRUE(es_canonica), rama = rama, op = opciones, clave = which(sol),
               sede = vapply(afs[rol_opcion], `[[`, "", "sede"), cod = vapply(afs[rol_opcion], `[[`, "", "codigo"),
               aG = d$aG, aP = d$aP)) }
nums <- function(s) as.numeric(regmatches(s, gregexpr("[0-9]+", s))[[1]])
puntua <- function(x, modo) { delta <- abs(x$aG - x$aP)
  cita <- vapply(x$op, function(o) { v <- nums(o); if (modo == "primera") length(v) > 0 && v[1] == delta else delta %in% v }, TRUE)
  hit <- which(cita)
  if (!length(hit)) return(c(aplica = 0, acierto = 0.25, de_G = NA, es_resta = NA))
  cand <- unique(unlist(lapply(hit, function(h) setdiff(which(x$sede == x$sede[h]), h))))
  c(aplica = 1, acierto = mean(cand %in% x$clave), de_G = mean(x$sede[hit] == "G"), es_resta = mean(x$cod[hit] == "ERR-RESTA")) }
E1 <- lapply(1:100, rec)
cur <- list(); pro <- list(); s <- 1000L
while (length(cur) < 100 || length(pro) < 100) { s <- s + 1L; x <- rec(s); if (x$canon) next
  if (x$rama == "CURSOS" && length(cur) < 100) cur[[length(cur) + 1]] <- x
  if (x$rama == "PROMEDIO" && length(pro) < 100) pro[[length(pro) + 1]] <- x }
canon <- Filter(function(x) x$canon, E1)[[1]]
for (modo in c("primera", "cualquiera")) {
  cat(sprintf("\n== cifra: %s ==\n", modo))
  for (est in list(list("E1 (mezcla)", E1), list("CURSOS", cur), list("PROMEDIO", pro), list("canónica", list(canon)))) {
    M <- sapply(est[[2]], puntua, modo = modo)
    cat(sprintf("%-12s n=%3d · aplica %5.1f %% · acierto %5.1f %% (exceso %+5.1f pp) · opción citada de G: %s · es ERR-RESTA: %s\n",
                est[[1]], ncol(M), 100 * mean(M["aplica", ]), 100 * mean(M["acierto", ]), 100 * mean(M["acierto", ]) - 25,
                if (all(is.na(M["de_G", ]))) "—" else sprintf("%.0f/%d", sum(M["de_G", ], na.rm = TRUE), sum(!is.na(M["de_G", ]))),
                if (all(is.na(M["es_resta", ]))) "—" else sprintf("%.0f/%d", sum(M["es_resta", ], na.rm = TRUE), sum(!is.na(M["es_resta", ])))))
  }
}
cat(sprintf("\núltima semilla usada en E2: %d\n", s))
