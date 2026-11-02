## comparar_con_r3.R — Paso 8, ronda 9: contraste diferencial con lo que R/exams emitió en la ronda 3 (md5 42f725bf…) para las MISMAS cinco semillas
## (exam_semilla_N.rds de coherencias_r3/, solo lectura). Lo esperado: las Partes 1, 3, 4, 5 y 6 y la tabla del otro colegio no cambian; cambian solo la Parte 2
## (pregunta, opciones, retroalimentación) y los pasajes de la Solution que la nombran. Muestra TODA diferencia.
nb <- function(x) gsub(" ", " ", x, fixed = TRUE)
for (s in c(7, 3, 2, 1, 4)) {
  a <- readRDS(file.path("..", "coherencias_r3", sprintf("exam_semilla_%d.rds", s))); b <- readRDS(sprintf("exam_semilla_%d.rds", s))
  cat(sprintf("\n=== semilla %d ===\n", s))
  qa <- nb(a$question); qb <- nb(b$question)
  cat("pregunta (líneas):", length(qa), "→", length(qb), "· líneas distintas:", paste(which(qa != qb[seq_along(qa)] | is.na(qb[seq_along(qa)])), collapse = ","), "\n")
  d <- which(qa != qb[seq_along(qa)]); for (i in d) cat("  r3:", substr(qa[i], 1, 150), "\n  r9:", substr(qb[i], 1, 150), "\n")
  la <- nb(a$questionlist); lb <- nb(b$questionlist)
  cat("opciones: Parte 1 igual:", identical(la[1:4], lb[1:4]), "· Partes 3-5 igual:", identical(la[9:11], lb[9:11]), "· Parte 6 igual:", identical(la[12:15], lb[12:15]), "· Parte 2 igual:", identical(la[5:8], lb[5:8]), "\n")
  cat("exsolution: Parte 1 igual:", identical(a$metainfo$solution[[1]], b$metainfo$solution[[1]]), "· Partes 3-5 iguales:", identical(a$metainfo$solution[3:5], b$metainfo$solution[3:5]), "· Parte 6 igual:", identical(a$metainfo$solution[[6]], b$metainfo$solution[[6]]),
      "· Parte 2: r3", paste(as.integer(a$metainfo$solution[[2]]), collapse = ""), "r9", paste(as.integer(b$metainfo$solution[[2]]), collapse = ""), "\n")
  sa <- nb(a$solutionlist); sb <- nb(b$solutionlist)
  cat("retroalimentación: iguales las de las Partes 1, 3-5 y 6:", identical(sa[c(1:4, 9:15)], sb[c(1:4, 9:15)]), "· Parte 2 igual:", identical(sa[5:8], sb[5:8]), "\n")
  ua <- nb(a$solution); ub <- nb(b$solution)
  extra_a <- setdiff(ua, ub); extra_b <- setdiff(ub, ua)
  cat("Solution: líneas solo en la r3:", length(extra_a), "· solo en la r9:", length(extra_b), "\n")
  for (z in extra_a) cat("  solo r3:", substr(z, 1, 170), "\n"); for (z in extra_b) cat("  solo r9:", substr(z, 1, 170), "\n")
  cat("orden de las líneas comunes de la Solution conservado:", identical(intersect(ua, ub), intersect(ub, ua)), "\n")
}
