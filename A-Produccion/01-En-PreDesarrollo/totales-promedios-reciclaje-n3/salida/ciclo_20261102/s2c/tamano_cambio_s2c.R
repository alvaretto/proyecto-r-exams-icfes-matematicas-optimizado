## Tamaño del cambio de cada candidato en data_generation: líneas nuevas o cambiadas de los pares de candidatos_s2c.R que
## cada diseño necesita (sin los pares de constantes ni los de los otros diseños). Se cuentan líneas, no se estiman.
Sys.setenv(S2C_Q_C = 0, S2C_Q_P = 0, S2C_RESTA_C = 1, S2C_RESTA_P = 1, S2C_D2 = 0)
e <- new.env(); sys.source("candidatos_s2c.R", envir = e)
lin <- function(k) { v <- strsplit(e$CAMBIOS[[k]][1], "\n")[[1]]; n <- strsplit(e$CAMBIOS[[k]][2], "\n")[[1]]; length(setdiff(n, v)) }
etq <- c("constantes", "espacio_rama: firma", "espacio_rama: xP", "espacio_rama: p_tabla", "espacio_rama: hay_debajo",
         "sorteo: padv con δ", "pa_deb", "lado_padv", "psin con δ: repartos", "cand_g sin ERR-RESTA", "fuente_padv", "usadas",
         "y y guarda H1 r1", "afirmaciones gdis/padv", "explica ERR-PROM", "calcula ERR-INVENTA", "params",
         "D2: cierre sin cifra", "D2: verdad", "D2: guarda regla #13", "D2: comprobación de calcula", "D2: explica ERR-RESTA", "D2: feedback")
stopifnot(length(etq) == length(e$CAMBIOS))
usa <- list(D1 = c(2:17), D1c40 = c(2:8, 10:12, 14:15, 17), D3 = c(2:8, 10:12, 14:15, 17), D3b = c(2:8, 10:12, 14:15, 17),
            D4 = c(5, 10), D2 = c(14, 18:23))
for (cd in names(usa)) cat(sprintf("%-6s %2d pares, %3d líneas nuevas o cambiadas: %s\n", cd, length(usa[[cd]]),
                                   sum(vapply(usa[[cd]], lin, 0L)), paste(etq[usa[[cd]]], collapse = "; ")))
