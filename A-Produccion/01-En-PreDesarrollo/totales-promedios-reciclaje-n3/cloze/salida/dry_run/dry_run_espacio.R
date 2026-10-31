## Dry-run orquestador-cloze (2026-10-30): solo lectura. Corre el data_generation del SCHOICE aprobado
## en memoria (no escribe nada) y mide: muestra de la Parte 1, caracteres de sus opciones en un gap de
## Moodle, y factibilidad del "caso paralelo" (Partes 2-6) con las exclusiones de V11.
suppressMessages(library(exams))
rmd <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd"
stopifnot(unname(tools::md5sum(rmd)) == "e293d5c812b1f9790e2bb777b62de9a5")
l <- readLines(rmd, encoding = "UTF-8")
i0 <- grep("^```\\{r data_generation", l); cierres <- which(l == "```"); i1 <- cierres[cierres > i0][1]
EXPR <- parse(text = paste(l[(i0 + 1):(i1 - 1)], collapse = "\n"), encoding = "UTF-8")
correr <- function(s) { e <- new.env(); set.seed(s); eval(EXPR, envir = e); e$.seed <- .Random.seed; e }
nums <- function(s) as.numeric(unlist(regmatches(s, gregexpr("[0-9]+", s))))

t0 <- Sys.time()
semillas <- 1:100
V <- lapply(semillas, correr)
n_can <- sum(vapply(V, function(e) isTRUE(e$es_canonica), NA))
s <- 100L; extra <- list()
while (n_can + length(extra) < 20L && s < 4000L) { s <- s + 1L; e <- correr(s); if (isTRUE(e$es_canonica)) extra[[length(extra) + 1]] <- e }
cat(sprintf("Muestra: 100 semillas (%d canónicas) + %d canónicas de cuota (hasta la semilla %d); %.1f s\n",
            n_can, length(extra), s, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
ramas <- vapply(V, function(e) e$rama, "")
cat("Rama en las 100 semillas:", paste(names(table(ramas)), table(ramas), collapse = " · "), "\n")

## Determinismo del bloque (premisa de E1): misma semilla -> mismo .Random.seed y mismas opciones
e1 <- correr(17); e2 <- correr(17)
cat("Determinismo (semilla 17 dos veces): .Random.seed idéntico =", identical(e1$.seed, e2$.seed),
    "| opciones idénticas =", identical(e1$opciones, e2$opciones), "\n")

## --- Caracteres de las opciones de la Parte 1 dentro de un gap de Moodle ---------------------
TODAS <- c(V, extra)
txt <- unique(unlist(lapply(TODAS, function(e) e$opciones)))
ch <- sort(unique(unlist(strsplit(paste(txt, collapse = ""), ""))))
cat("\nOpciones de la Parte 1 distintas en la muestra:", length(txt), "\n")
cat("Caracteres no alfanuméricos:", paste(sprintf("[%s U+%04X]", ch[!grepl("[[:alnum:] ]", ch)],
    utf8ToInt(paste(ch[!grepl("[[:alnum:] ]", ch)], collapse = ""))), collapse = " "), "\n")
cat("Especiales de la sintaxis cloze de Moodle (~ # } { / \" \\) o markup (* _ $ < >):",
    sum(grepl("[~#{}/\"\\\\*_$<>]", txt)), "opciones\n")
cat("NBSP (U+00A0) en opciones:", sum(grepl(" ", txt)), "| largo mín/mediana/máx:",
    paste(range(nchar(txt))[1], median(nchar(txt)), range(nchar(txt))[2], sep = "/"), "\n")

## --- Lo que la Parte 1 imprime y sus intermedios ------------------------------------------------
p1 <- lapply(TODAS, function(e) {
  ops <- unique(nums(paste(e$opciones, collapse = " ")))
  tab <- c(e$d$nG, e$d$nP, e$d$aG, e$d$aP)
  list(rama = e$rama, canon = isTRUE(e$es_canonica), ops = ops, tab = tab,
       inter = c(e$d$TG, e$d$TP, e$d$D), D = e$d$D, TG = e$d$TG, TP = e$d$TP, enun = nums(e$enunciado))
})
D_all <- vapply(p1, `[[`, 0, "D"); TG_all <- vapply(p1, `[[`, 0, "TG")
cat(sprintf("\nParte 1: D en [%d, %d] (mediana %g); T_G en [%d, %d]; cifras impresas por versión: %s\n",
            min(D_all), max(D_all), median(D_all), min(TG_all), max(TG_all),
            paste(range(vapply(p1, function(x) length(x$ops), 0L)), collapse = "-")))
cat("Números en el enunciado (no canónico):", sum(vapply(p1, function(x) length(x$enun) > 0, NA)), "versiones con cifras\n")

## --- Caso paralelo (Alternativa A): espacio, exclusiones V11 y factibilidad ----------------------
esp_par <- function(rama) {
  g <- expand.grid(nG = 3:8, nP = 3:8, aG = 20:60, aP = 20:60)
  g <- if (rama == "CURSOS") g[g$nG > g$nP & g$aG < g$aP, ] else g[g$nG < g$nP & g$aG > g$aP, ]
  g$k <- abs(g$nG - g$nP); g$delta <- abs(g$aG - g$aP)
  g$TG <- g$nG * g$aG; g$TP <- g$nP * g$aP; g$D <- g$TG - g$TP
  g <- g[g$k <= 3 & g$delta >= 2 & g$delta <= 15 & g$D >= 6 & g$D <= 0.3 * g$TP, ]
  ## propias del caso paralelo: ninguna respuesta (TG, TP, D) es un número de su tabla; sin empates
  ## en las hipótesis de la Parte 6 (un curso más / un curso menos)
  imp <- cbind(g$nG, g$nP, g$aG, g$aP)
  ok <- vapply(seq_len(nrow(g)), function(i) !any(c(g$TG[i], g$TP[i], g$D[i]) %in% imp[i, ]), NA)
  ok <- ok & g$aP != g$D & g$aG != g$D
  g[ok, ]
}
EP <- list(CURSOS = esp_par("CURSOS"), PROMEDIO = esp_par("PROMEDIO"))
cat(sprintf("\nCaso paralelo, espacio sin exclusiones: CURSOS %d · PROMEDIO %d combinaciones\n",
            nrow(EP$CURSOS), nrow(EP$PROMEDIO)))
for (r in names(EP)) {
  g <- EP[[r]]
  cat(sprintf("  %s: «el perdedor con un curso más habría ganado» (a_P > D) verdadera en %.1f %%; «el ganador con un curso menos habría perdido» (a_G > D) en %.1f %%\n",
              r, 100 * mean(g$aP > g$D), 100 * mean(g$aG > g$D)))
}
factible <- function(x, g) {
  prohib_tabla <- unique(c(x$ops, x$inter))                 # la tabla paralela no imprime cifras ni intermedios de P1
  prohib_resp  <- unique(c(x$ops, x$tab, x$inter, x$enun))  # V11 por valor: respuestas de P3-P5 fuera de lo que imprime P1
  ok_t <- !(g$nG %in% prohib_tabla | g$nP %in% prohib_tabla | g$aG %in% prohib_tabla | g$aP %in% prohib_tabla)
  ok_r <- !(g$TG %in% prohib_resp | g$TP %in% prohib_resp | g$D %in% prohib_resp)
  c(total = nrow(g), ok = sum(ok_t & ok_r), excl_tabla = sum(!ok_t), excl_resp = sum(!ok_r))
}
for (r in names(EP)) {
  M <- t(vapply(p1, factible, numeric(4), g = EP[[r]]))
  frac <- M[, "ok"] / M[, "total"]
  cat(sprintf("  Rama paralela %s, por versión de la Parte 1 (n = %d): combinaciones válidas mín %d, p5 %d, mediana %d; fracción excluida máx %.1f %%, mediana %.1f %%\n",
              r, nrow(M), min(M[, "ok"]), as.integer(quantile(M[, "ok"], .05)), as.integer(median(M[, "ok"])),
              100 * (1 - min(frac)), 100 * (1 - median(frac))))
}
## Independencia por construcción: la rama paralela se sortea 50/50 después del bloque P1.
## Cota de lo que aporta un atajo de lado: S_U ya identifica a G en el 100 % de las versiones (O-1),
## así que una regla de perfil cruzado no puede subir la identificación del lado; se mide en E3.

## --- Peso de la Parte 1 frente a O-1 corregido -------------------------------------------------
cat("\nPeso de la Parte 1 y atajo firmado (mezcla 77,6 %, azar 25 %): fracción de la nota por la Parte 1 sin calcular\n")
for (w in list(c(3, 1, 1, 1, 1, 1), c(2, 1, 1, 1, 1, 1), c(1, 1, 1, 1, 1, 1))) {
  f <- w[1] / sum(w)
  cat(sprintf("  %s: Parte 1 = %.1f %% de la pregunta; atajo %.1f %% de la nota frente a %.1f %% al azar (+%.1f pp)\n",
              paste(w, collapse = "|"), 100 * f, 100 * f * 0.776, 100 * f * 0.25, 100 * f * (0.776 - 0.25)))
}
cat(sprintf("\nTiempo total: %.1f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
