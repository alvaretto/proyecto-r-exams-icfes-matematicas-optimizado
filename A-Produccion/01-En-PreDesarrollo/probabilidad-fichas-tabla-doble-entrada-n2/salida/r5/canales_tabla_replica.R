## Réplica propia de los canales de estímulo del detractor r5, con el ítem oficial (canónica) como vara.
## Reglas fijadas a priori (las del reporte): no se elige la máxima de un conjunto.
rmd <- commandArgs(TRUE)[1]
lin <- readLines(rmd, encoding = "UTF-8")
i <- grep("^```\\{r data_generation", lin)[1]; f <- which(lin == "```" & seq_along(lin) > i)[1]
ex <- parse(text = paste(lin[(i + 1):(f - 1)], collapse = "\n"))
elim <- function(drop, k) { q <- which(!drop); if (!length(q)) q <- seq_along(drop); if (k %in% q) 1 / length(q) else 0 }
reglas <- function(e) {
  ps <- e$pares; k <- which(names(ps) == "clave"); cel <- lapply(ps, function(p) c(e$celdas_tipo(e$w, p[[1]]), e$celdas_tipo(e$w, p[[2]])))
  rep <- vapply(ps, function(p) any(e$celdas_tipo(e$w, p[[1]]) %in% e$celdas_tipo(e$w, p[[2]])), NA)
  molde <- vapply(ps, function(p) paste(sort(p[[1]]$d), collapse = "+"), "")
  gemela <- vapply(seq_along(ps), function(j) any(rep[molde == molde[j] & seq_along(ps) != j]), NA)
  mx <- max(e$w); mn <- min(e$w); rango <- vapply(cel, function(x) diff(range(x)), 0)
  c(base_repite = elim(rep, k), compuesta_repite_y_molde = elim(rep | gemela, k),
    fuera_5_20 = elim(vapply(cel, function(x) any(x < 5 | x > 20), NA), k),
    numero_mayor = elim(vapply(cel, function(x) mx %in% x, NA), k),
    mayor_o_menor = elim(vapply(cel, function(x) any(c(mx, mn) %in% x), NA), k),
    celdas_mas_juntas = { m <- which(rango == min(rango)); if (k %in% m) 1 / length(m) else 0 })
}
nc <- NULL; cn <- NULL
for (s in 1:2000) { set.seed(s); e <- new.env(); eval(ex, e); r <- reglas(e); if (isTRUE(e$es_canonica)) cn <- rbind(cn, r) else nc <- rbind(nc, r) }
m_nc <- colMeans(nc); m_cn <- colMeans(cn)
out <- data.frame(no_canonica = sprintf("%.1f %%", 100 * m_nc), exceso_nc = sprintf("%+.1f", 100 * (m_nc - .25)),
                  item_oficial = sprintf("%.1f %%", 100 * m_cn), exceso_oficial = sprintf("%+.1f", 100 * (m_cn - .25)),
                  mezcla_10 = sprintf("%+.1f", 100 * (.9 * m_nc + .1 * m_cn - .25)), row.names = names(m_nc))
cat("no canónicas:", nrow(nc), "| canónicas:", nrow(cn), "\n"); print(out)
