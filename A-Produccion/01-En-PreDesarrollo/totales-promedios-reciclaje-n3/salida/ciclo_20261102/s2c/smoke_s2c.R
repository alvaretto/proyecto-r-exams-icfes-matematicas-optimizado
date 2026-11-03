## Humo: aplica cada candidato en memoria y evalúa las semillas 1..40 (solo detecta errores de construcción;
## las cifras oficiales salen de verificar_s2c.R con N = 100).
rmd <- "../../../totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd"
l <- readLines(rmd, encoding = "UTF-8"); i0 <- grep("^```\\{r data_generation", l); ci <- which(l == "```"); i1 <- ci[ci > i0][1]
COD0 <- paste(l[(i0 + 1):(i1 - 1)], collapse = "\n")
rep1 <- function(txt, v, n) { p <- gregexpr(v, txt, fixed = TRUE)[[1]]; if (length(p) != 1L || p[1] < 0) stop("patrón: ", substr(v, 1, 60))
  paste0(substr(txt, 1L, p[1] - 1L), n, substr(txt, p[1] + nchar(v), nchar(txt))) }
cfg <- list(D0 = c(0,0,1,1,0), D1q50 = c(.5,.5,1,1,0), D1q100 = c(1,1,1,1,0), D3 = c(1,0,1,1,0), D3b = c(1,0,1,0,0), D4 = c(0,0,0,0,0), D2 = c(0,0,1,1,1))
for (id in names(cfg)) { v <- cfg[[id]]
  Sys.setenv(S2C_Q_C = v[1], S2C_Q_P = v[2], S2C_RESTA_C = v[3], S2C_RESTA_P = v[4], S2C_D2 = v[5])
  e <- new.env(); sys.source("candidatos_s2c.R", envir = e, keep.source = FALSE)
  cod <- COD0; for (cb in e$CAMBIOS) cod <- rep1(cod, cb[1], cb[2]); ex <- parse(text = cod, encoding = "UTF-8")
  err <- character(0); dl <- c(G = 0, P = 0)
  for (s in 1:40) { en <- new.env(); set.seed(s); r <- tryCatch({ eval(ex, envir = en); NULL }, error = function(z) conditionMessage(z))
    if (!is.null(r)) err <- c(err, sprintf("%d: %s", s, r)) else if (!en$es_canonica) {
      if (en$afs$gdis$codigo == "ERR-RESTA") dl["G"] <- dl["G"] + 1
      if (isTRUE(en$s2c_padv_delta) || isTRUE(en$s2c_psin_delta)) dl["P"] <- dl["P"] + 1 } }
  cat(sprintf("%-7s errores %d/40 · ERR-RESTA en G %d · δ en P %d%s\n", id, length(err), dl[["G"]], dl[["P"]],
              if (length(err)) paste0(" · ", paste(head(unique(sub("^[0-9]+: ", "", err)), 3), collapse = " | ")) else ""))
}
## D0 = original con el mismo RNG: las opciones de las semillas 1..40 deben coincidir
Sys.setenv(S2C_Q_C = 0, S2C_Q_P = 0, S2C_RESTA_C = 1, S2C_RESTA_P = 1, S2C_D2 = 0)
e <- new.env(); sys.source("candidatos_s2c.R", envir = e); cod <- COD0; for (cb in e$CAMBIOS) cod <- rep1(cod, cb[1], cb[2])
igual <- vapply(1:40, function(s) { a <- new.env(); b <- new.env(); set.seed(s); eval(parse(text = COD0), a); set.seed(s); eval(parse(text = cod), b)
  identical(a$opciones, b$opciones) && identical(a$sol, b$sol) && identical(a$fb, b$fb) }, TRUE)
cat("D0 frente al original (opciones, clave y feedback), semillas 1..40:", sum(igual), "de 40 idénticas\n")
