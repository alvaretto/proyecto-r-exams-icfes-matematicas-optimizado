r <- readRDS(path.expand("~/.cache/fase2c_r5_cloze/emit.rds")); N <- r$N; V <- r$V; nb <- function(x) gsub(" ", " ", x)
dif <- character(0); fl <- 0
for (s in names(V)) { a <- N[[s]]; b <- V[[s]]
  for (k in c("question", "questionlist", "solutionlist")) if (!identical(a[[k]], b[[k]])) dif <- c(dif, paste(s, k))
  for (k in setdiff(union(names(a$metainfo), names(b$metainfo)), "file")) if (!identical(a$metainfo[[k]], b$metainfo[[k]])) dif <- c(dif, paste(s, "meta", k))
  if (!identical(basename(a$metainfo$file), basename(b$metainfo$file))) dif <- c(dif, paste(s, "meta file (basename)"))
  sa <- strsplit(paste(a$solution, collapse = "\n"), "\n")[[1]]; sb <- strsplit(paste(b$solution, collapse = "\n"), "\n")[[1]]
  pa <- grep("^Parte 6\\. Cada hipótesis", sa); pb <- grep("^Parte 6\\. Cada hipótesis", sb)
  if (length(pa) != 1 || length(pb) != 1 || !identical(sa[-pa], sb[-pb])) dif <- c(dif, paste(s, "solution fuera del párrafo"))
  na <- as.numeric(regmatches(nb(sa[pa]), gregexpr("[0-9]+", nb(sa[pa])))[[1]])[-1]; nv <- as.numeric(regmatches(nb(sb[pb]), gregexpr("[0-9]+", nb(sb[pb])))[[1]])[-1]
  if (length(na) != 4 || length(nv) != 3 || !identical(na[1:3], nv) || na[4] != nv[2]) dif <- c(dif, paste(s, "cifras del párrafo"))
  if (!identical(regmatches(nb(sa[pa]), gregexpr("el grado \\S+", nb(sa[pa])))[[1]], regmatches(nb(sb[pb]), gregexpr("el grado \\S+", nb(sb[pb])))[[1]])) dif <- c(dif, paste(s, "grados"))
}
cat("Invariancia 6f6f1b13 vs 27f789dc, 100 semillas (xexams): diferencias", length(dif), "\n"); if (length(dif)) print(head(dif, 10))
cat("metainfo$file ejemplo:", N[["1"]]$metainfo$file, "|", V[["1"]]$metainfo$file, "\n")
## Primera semilla con veredictos distintos de HP y HG (MC52 muere ahí), y lectura «cada hipótesis» del criterio aplicada a la de ventaja
prim <- NA; nk <- c(bc = 0, bc_k2 = 0, bc_dif = 0, bp = 0)
for (s in names(N)) { e <- N[[s]]; x <- nb(grep("^Parte 6\\. Cada hipótesis", strsplit(paste(e$solution, collapse = "\n"), "\n")[[1]], value = TRUE))
  hp <- grepl(": habría ganado\\.", x); hg <- grepl(": habría perdido\\.", x); if (is.na(prim) && hp != hg) prim <- as.integer(s)
  L <- strsplit(paste(e$question, collapse = "\n"), "\n")[[1]]; f <- grep("^\\|", L, value = TRUE); f <- f[!grepl("^\\|[-:| ]+\\|$", f)]
  f <- tail(f, 2); cel <- lapply(f, function(z) as.numeric(regmatches(nb(gsub("<span[^>]*>", "", z)), gregexpr("(?<=>|\\| )[0-9]+(?= (cursos|kilos))|[0-9]+(?= (cursos|kilos))", nb(gsub("<span[^>]*>", "", z)), perl = TRUE))[[1]]))
  n <- c(cel[[1]][1], cel[[2]][1]); a <- c(cel[[1]][2], cel[[2]][2]); T <- n * a; w <- which.max(T); l <- 3 - w; D <- T[w] - T[l]; k <- abs(n[w] - n[l])
  vt <- e$questionlist[12:15]
  if (any(grepl("participado con tantos cursos", vt))) { nk["bc"] <- nk["bc"] + 1; if (k >= 2) nk["bc_k2"] <- nk["bc_k2"] + 1
    perd <- any(grepl("perdió hubiera participado con tantos", vt))
    ingenua <- if (perd) a[l] > D else a[w] > D; real <- if (perd) n[w] * a[l] > T[w] else n[l] * a[w] < T[l]
    if (ingenua != real) nk["bc_dif"] <- nk["bc_dif"] + 1 } else nk["bp"] <- nk["bp"] + 1 }
cat("primera semilla con HP != HG:", prim, "\n"); print(nk)
