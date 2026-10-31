## rng_tanda8.R — tanda 8 (2026-10-31, decisión b del profesor): con la misma semilla, el .Rmd nuevo (la Parte 2 solo con el
## tipo «promedio») reproduce el anterior en la Parte 1, las Partes 3 a 6, la marca, el orden y los índices sorteados de la
## Parte 2, y el .Random.seed final. Solo lee. Uso, desde cloze/salida:
##   Rscript --no-init-file rng_tanda8.R <anterior.Rmd> <nuevo.Rmd>
suppressMessages(library(exams))
a <- commandArgs(TRUE); stopifnot(length(a) == 2L, all(file.exists(a)))
chunk <- function(f) { lin <- readLines(f, encoding = "UTF-8"); ini <- grep("^```\\{r data_generation", lin)[1]
  fin <- which(lin == "```" & seq_along(lin) > ini)[1]; paste(lin[(ini + 1):(fin - 1)], collapse = "\n") }
EA <- parse(text = chunk(a[1]), encoding = "UTF-8"); EB <- parse(text = chunk(a[2]), encoding = "UTF-8")
correr <- function(e, s) { set.seed(s); en <- new.env(); en$.err <- tryCatch({ eval(e, envir = en); NULL }, error = function(x) conditionMessage(x))
  en$.seed <- get(".Random.seed", envir = globalenv()); en }
cat("anterior:", basename(a[1]), unname(tools::md5sum(a[1])), "\nnuevo:   ", basename(a[2]), unname(tools::md5sum(a[2])), "\n")
can <- integer(0); s <- 1000L; while (length(can) < 20L && s < 5000L) { s <- s + 1L; if (isTRUE(correr(EA, s)$es_canonica)) can <- c(can, s) }
SEM <- c(1:100, can)
## Objetos que la decisión b cambia a propósito (banco, tipo y textos de la Parte 2, su retroalimentación).
PERMITIDOS <- c("P2", "tipo_p2", "q2", "clave_p2", "dis_p2", "op2", "opciones_p2", "texto_p2", "feedback_p2", "clave2_expl", "expl2",
                "textos_2a6", "sin_rotulo", "compara_p2")
dif <- list(); tipos <- character(0); f <- character(0)
for (z in SEM) { A <- correr(EA, z); B <- correr(EB, z)
  if (!is.null(A$.err) || !is.null(B$.err)) { f <- c(f, sprintf("s=%d error: %s / %s", z, format(A$.err), format(B$.err))); next }
  tipos <- c(tipos, A$tipo_p2); ks <- union(setdiff(ls(A), ".seed"), setdiff(ls(B), ".seed"))
  for (k in ks) if (!exists(k, A, inherits = FALSE) || !exists(k, B, inherits = FALSE) || !identical(A[[k]], B[[k]], ignore.environment = TRUE, ignore.bytecode = TRUE, ignore.srcref = TRUE))
    dif[[k]] <- c(dif[[k]], z)
  if (!identical(A$.seed, B$.seed)) f <- c(f, sprintf("s=%d .Random.seed final distinto", z))
  ## Parte 2: misma marca, mismo orden y los mismos índices de clave y distractores dentro de su banco.
  if (!identical(A$sol_p2, B$sol_p2) || !identical(A$o2, B$o2) || match(A$clave_p2, A$q2$clave) != match(B$clave_p2, B$q2$clave) ||
      !identical(match(A$dis_p2, A$q2$distractores), match(B$dis_p2, B$q2$distractores))) f <- c(f, sprintf("s=%d Parte 2: índices sorteados distintos", z))
  if (!identical(B$tipo_p2, "promedio") || !identical(names(B$P2), "promedio")) f <- c(f, sprintf("s=%d el nuevo no es solo «promedio»", z))
  if (A$tipo_p2 == "promedio" && !identical(A$opciones_p2, B$opciones_p2)) f <- c(f, sprintf("s=%d versión que ya era «promedio» con otras opciones", z))
  if (!identical(A$exsolution_str, B$exsolution_str)) f <- c(f, sprintf("s=%d exsolution distinto", z)) }
fuera <- setdiff(names(dif), PERMITIDOS)
cat(sprintf("semillas: 1..100 + %d canónicas (%s); tipo en el anterior: %s\n", length(can), paste(range(can), collapse = "-"),
            paste(names(table(tipos)), table(tipos), sep = " ", collapse = " · ")))
cat("objetos que cambian (semillas):", paste(sprintf("%s %d", names(dif), lengths(dif)), collapse = " · "), "\n")
cat("objetos que cambian fuera de la lista permitida:", if (length(fuera)) paste(fuera, collapse = ", ") else "ninguno", "\n")
cat("fallos:", length(f), "\n"); if (length(f)) cat(paste0("  ", head(f, 20), "\n"), sep = "")
cat(if (!length(f) && !length(fuera)) "RESULTADO: OK — Parte 1, Partes 3 a 6, marca/orden/índices de la Parte 2, exsolution y .Random.seed final idénticos\n" else "RESULTADO: FALLA\n")
quit(status = if (!length(f) && !length(fuera)) 0L else 1L)
