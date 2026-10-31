#!/usr/bin/env Rscript
## x_canonica_exacta.R — batería cruzada (X6 a X11) sobre la instancia canónica, por ENUMERACIÓN EXACTA (§P7-E).
## Motivo (tanda 2, 2026-10-30): la tanda 1 declaró solo el máximo de la batería cruzada en la canónica (X9, n = 30),
## y el profesor firmó X9 (decisión 3.2 a). Con n = 30 las otras reglas salen con ruido de muestreo; aquí se
## calculan exactas. Las opciones de la Parte 1 son fijas en la canónica (verbatim); las reglas cruzadas solo
## dependen de: la clave de la Parte 2 (6 textos equiprobables), la rama del otro colegio (1/2), la combinación de
## veredictos de las hipótesis (1/3 por rama), el veredicto de la de ventaja (fijo si las hipótesis coinciden; 1/2
## si no) y la redacción de ventaja (2 candidatas, 1/2). Se ignora la cascada F1 (paso 3 o 4: 0 de 330 versiones).
## Las funciones de las reglas son copia literal de verificar_espacio_cloze.R (toks, mas_comun, sc, dentro...).
## Uso (desde cloze/salida): Rscript --no-init-file x_canonica_exacta.R
suppressMessages(library(exams))
stopifnot("correr desde cloze/salida" = basename(getwd()) == "salida" && basename(dirname(getwd())) == "cloze")
RMD <- normalizePath("../totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd")
cat("md5 del .Rmd:", unname(tools::md5sum(RMD)), "\n")
lin <- readLines(RMD, encoding = "UTF-8"); a <- grep("^```\\{r data_generation", lin)[1]
b <- which(lin == "```" & seq_along(lin) > a)[1]
cod <- parse(text = paste(lin[(a + 1):(b - 1)], collapse = "\n"), encoding = "UTF-8")
s <- 0L; repeat { s <- s + 1L; set.seed(s); en <- new.env(); eval(cod, envir = en); if (isTRUE(en$es_canonica)) break }
cat("canónica de la semilla", s, "\n")
## --- copia literal de verificar_espacio_cloze.R ---
toks <- function(s) unique(tolower(regmatches(s, gregexpr("[A-Za-zÀ-ÿ]{3,}", s))[[1]]))
mas_comun <- function(o, ref) { r <- toks(paste(ref, collapse = " ")); k <- vapply(o, function(s) length(intersect(toks(s), r)), 0L); k == max(k) }
sc <- function(S, k) { if (!any(S) || all(S)) 0.25 else if (S[k]) 1 / sum(S) else 0 }
dentro <- function(S, t) { u <- S & t; if (any(u) && any(S & !u)) u else S }
## ---
op <- en$opciones_p1; k <- which(en$sol_p1); rol <- en$rol_opcion
sede_o <- vapply(en$afs[rol], function(z) z$sede, "")
su <- sede_o != sede_o[rol == "psin"]
cat("opciones (orden de esta semilla; clave =", k, "):\n"); cat(paste0("  ", seq_along(op), ". ", op, "\n"), sep = "")
claves_p2 <- unlist(lapply(en$P2, function(q) q$clave))
VEN <- en$P6_VENTAJA
COMBOS <- en$COMBOS_H
filas <- list()
for (rp in c("CURSOS", "PROMEDIO")) for (ic in 1:3) {
  h <- COMBOS[[rp]][[ic]]
  vcs <- if (h[["hg"]] == h[["hp"]]) list(c(v = !h[["hg"]], w = 1)) else list(c(v = TRUE, w = 0.5), c(v = FALSE, w = 0.5))
  for (vc in vcs) {
    cand <- Filter(function(z) (z$si_cursos == (rp == "CURSOS")) == as.logical(vc[["v"]]), VEN)
    for (c6 in cand) {
      verdaderas <- c(if (h[["hp"]]) en$P6_HP, if (h[["hg"]]) en$P6_HG, if (as.logical(vc[["v"]])) c(c6$texto, "Ganó el concurso el grado sexto."))
      w <- 0.5 * (1 / 3) * vc[["w"]] * (1 / length(cand))
      ventaja <- if (rp == "CURSOS") "más cursos" else "mayor promedio"
      for (kp2 in claves_p2) {
        filas[[length(filas) + 1]] <- c(w = w / length(claves_p2),
          X6 = sc(mas_comun(op, kp2), k), X7 = sc(mas_comun(op, verdaderas), k),
          X8 = sc(sede_o == (if (en$rama == rp) "G" else "P"), k),
          X9 = sc(dentro(su, grepl(ventaja, op, fixed = TRUE)), k),
          X10 = sc(dentro(su, mas_comun(op, kp2)), k), X11 = sc(dentro(su, mas_comun(op, verdaderas)), k))
      }
    }
  }
}
M <- do.call(rbind, filas)
stopifnot("pesos que suman 1" = abs(sum(M[, "w"]) - 1) < 1e-9)
ex <- colSums(M[, -1] * M[, "w"])
base <- c(X6 = 0.25, X7 = 0.25, X8 = 0.25, X9 = 0.50, X10 = 0.50, X11 = 0.50)
cat("\nBatería cruzada en la canónica, enumeración exacta (", nrow(M), " casos ponderados):\n", sep = "")
for (r in names(ex)) cat(sprintf("  %-4s %5.1f %%  (%+5.1f pp sobre %2.0f %%)  peso en la mezcla 0,10 × %+.1f = %+.2f pp\n",
                                 r, 100 * ex[[r]], 100 * (ex[[r]] - base[[r]]), 100 * base[[r]], 100 * (ex[[r]] - base[[r]]), 10 * (ex[[r]] - base[[r]])))
