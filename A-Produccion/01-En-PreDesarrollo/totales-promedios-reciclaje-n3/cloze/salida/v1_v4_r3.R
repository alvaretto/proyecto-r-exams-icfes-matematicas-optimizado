## V1-V4 del orquestador (tanda 6): conteo de gaps, orden e inmediatez de ##ANSWERi##, exsolution/extol por gap en
## N = 100 semillas y mínimo de 6 partes. Solo lee. Uso: cd cloze/salida && Rscript --no-init-file v1_v4_r3.R
suppressMessages(library(exams))
F <- "../totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd"; lin <- readLines(F, encoding = "UTF-8")
cat("md5:", unname(tools::md5sum(F)), "\n")
a <- grep("^```\\{r data_generation", lin)[1]; b <- which(lin == "```" & seq_along(lin) > a)[1]
cod <- parse(text = paste(lin[(a + 1):(b - 1)], collapse = "\n"), encoding = "UTF-8")
ans <- grep("##ANSWER[0-9]+##", lin); num_ans <- as.integer(sub(".*##ANSWER([0-9]+)##.*", "\\1", lin[ans]))
par <- grep("\\*\\*Parte [0-9]+\\.\\*\\*", lin); num_par <- as.integer(sub(".*\\*\\*Parte ([0-9]+)\\.\\*\\*.*", "\\1", lin[par]))
## Los encabezados «**Parte N.**» del enunciado: los que van antes de la sección Solution
sol <- grep("^Solution[[:space:]]*$", lin)[1]; qp <- par[par < sol]; qn <- num_par[par < sol]; qa <- ans[ans < sol]; qan <- num_ans[ans < sol]
v2 <- all(qan == seq_along(qan)) && all(vapply(seq_along(qa), function(i) { p <- qp[qn == i]; length(p) == 1L && p < qa[i] && !any(qp > p & qp < qa[i]) }, NA))
ok <- function(c, m) cat(if (isTRUE(c)) "OK  " else "FALLA", m, "\n")
set.seed(1); en <- new.env(); eval(cod, envir = en); tipos <- strsplit(en$exclozetype_str, "|", fixed = TRUE)[[1]]
ok(length(unique(qan)) == length(tipos) && length(tipos) == length(unique(qn)), sprintf("V1: %d ##ANSWERi## = %d tipos en exclozetype = %d partes", length(unique(qan)), length(tipos), length(unique(qn))))
ok(v2, "V2: ##ANSWER1..6## en orden, cada uno tras su «**Parte N.**» y antes de la siguiente")
ok(length(unique(qn)) >= 6L, sprintf("V4: %d partes (mínimo 6)", length(unique(qn))))
malos <- 0L
for (s in 1:100) { set.seed(s); e <- new.env(); eval(cod, envir = e)
  xs <- strsplit(e$exsolution_str, "|", fixed = TRUE)[[1]]; xt <- strsplit(e$extol_str, "|", fixed = TRUE)[[1]]
  nop <- c(length(e$opciones_p1), length(e$opciones_p2), NA, NA, NA, length(e$opciones_p6))
  bien <- length(xs) == 6L && length(xt) == 6L && all(as.numeric(xt) >= 0) &&
    all(vapply(1:6, function(g) switch(tipos[g],
      schoice = grepl("^[01]+$", xs[g]) && nchar(xs[g]) == nop[g] && lengths(regmatches(xs[g], gregexpr("1", xs[g]))) == 1L,
      mchoice = grepl("^[01]+$", xs[g]) && nchar(xs[g]) == nop[g] && grepl("1", xs[g]) && grepl("0", xs[g]),
      num = !is.na(suppressWarnings(as.numeric(xs[g]))) && as.numeric(xs[g]) == round(as.numeric(xs[g]))), NA))
  if (!bien) { malos <- malos + 1L; cat("  semilla", s, ":", e$exsolution_str, "\n") } }
ok(malos == 0L, sprintf("V3: exsolution/extol coherentes con su tipo en 100/100 semillas (schoice: un 1 y largo = opciones; mchoice: al menos un 1 y un 0; num: entero; extol >= 0); fallos = %d", malos))
