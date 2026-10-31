## retro_r8.R — tanda 8 (2026-10-31, decisión b): la retroalimentación de la Parte 2, con un solo tipo, en 100 semillas
## (regla #23). Comprueba: «Verdadero.» + clave2_expl solo en la clave; «Falso.» + expl2[nombre] en cada distractor; expl2 con
## exactamente los cinco distractores del banco y todos usados (sin dato muerto); la Solution de la Parte 2 con su encabezado
## único y la explicación de la clave. Solo lee. Uso, desde cloze/salida: Rscript --no-init-file retro_r8.R <ruta.Rmd>
suppressMessages(library(exams))
RMD <- commandArgs(TRUE)[1]
lin <- readLines(RMD, encoding = "UTF-8"); ini <- grep("^```\\{r data_generation", lin)[1]; fin <- which(lin == "```" & seq_along(lin) > ini)[1]
EXPR <- parse(text = paste(lin[(ini + 1):(fin - 1)], collapse = "\n"), encoding = "UTF-8")
f <- character(0); usados <- character(0)
for (z in 1:100) { set.seed(z); en <- new.env(); eval(EXPR, envir = en)
  nm <- names(en$op2)[en$o2]; fb <- en$feedback_p2
  if (!identical(names(en$expl2), names(en$P2$promedio$distractores))) f <- c(f, sprintf("s=%d expl2 no coincide con el banco", z))
  if (!isTRUE(all(startsWith(fb[en$sol_p2], "Verdadero.") & fb[en$sol_p2] == paste("Verdadero.", en$clave2_expl)))) f <- c(f, sprintf("s=%d clave sin su explicación", z))
  if (!isTRUE(all(fb[!en$sol_p2] == paste("Falso.", en$expl2[nm[!en$sol_p2]])))) f <- c(f, sprintf("s=%d distractor sin su explicación", z))
  usados <- union(usados, nm[!en$sol_p2]) }
sol <- paste(lin[grep("^Solution", lin)[1]:length(lin)], collapse = "\n")
cat("semillas: 100 · fallos:", length(f), "\n"); if (length(f)) cat(paste0("  ", head(f, 10), "\n"), sep = "")
cat("expl2 usados en 100 semillas:", length(usados), "de", length(en$expl2), "(", paste(sort(usados), collapse = ", "), ")\n")
cat("Solution: encabezado único de la Parte 2:", grepl('encabezado("Parte 2 · Lecturas del promedio que no sirven")', sol, fixed = TRUE),
    "· sin condicional por tipo:", !grepl("tipo_p2", sol, fixed = TRUE), "\n")
cat("clave2_expl:", en$clave2_expl, "\n")
ok <- !length(f) && setequal(usados, names(en$expl2)) && !grepl("tipo_p2", sol, fixed = TRUE)
cat(if (ok) "RESULTADO: OK\n" else "RESULTADO: FALLA\n"); quit(status = if (ok) 0L else 1L)
