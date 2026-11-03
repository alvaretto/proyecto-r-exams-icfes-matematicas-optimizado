## FASE 2C r5 — resto de la Solution y del feedback (prosa fuera del párrafo nuevo), leído del texto emitido
r <- readRDS(path.expand("~/.cache/fase2c_r5_cloze/emit.rds")); N <- r$N; nb <- function(x) gsub("\u00a0", " ", x)
strip <- function(x) gsub("<span[^>]*>|</span>|\\*\\*", "", x)
tablas <- function(q) { L <- strsplit(paste(q, collapse = "\n"), "\n")[[1]]; idx <- grep("^\\|", L); bl <- split(idx, cumsum(c(1, diff(idx) != 1)))
  lapply(bl, function(b) { rw <- L[b]; rw <- rw[!grepl("^\\|[-:| ]+\\|$", rw)]
    ce <- lapply(rw, function(z) trimws(nb(strip(strsplit(sub("^\\|", "", sub("\\|\\s*$", "", z)), "|", fixed = TRUE)[[1]])))); dat <- ce[-1]
    data.frame(nombre = tolower(vapply(dat, `[`, "", 1)), n = as.numeric(sub(" cursos?$", "", vapply(dat, `[`, "", 2))), a = as.numeric(sub(" kilos?$", "", vapply(dat, `[`, "", 3))), stringsAsFactors = FALSE) }) }
fallos <- character(0); F <- function(s, ...) fallos <<- c(fallos, sprintf("s=%s %s", s, paste0(...))); n_ok <- numeric(0)
ok <- function(k) n_ok[k] <<- (if (is.na(n_ok[k])) 0 else n_ok[k]) + 1
P2_CLAVE <- "^Lo que recogió ese grado dividido (en partes iguales entre sus cursos|entre el número de sus cursos|entre la cantidad de cursos de ese mismo grado)\\.$"
for (s in names(N)) { e <- N[[s]]; tb <- tablas(e$question); t1 <- tb[[1]]; t2 <- tb[[2]]; sol <- nb(strsplit(paste(e$solution, collapse = "\n"), "\n")[[1]]); sl <- nb(e$solutionlist); ql <- nb(e$questionlist)
  T1 <- t1$n * t1$a; w <- which.max(T1); l <- 3 - w; D1 <- T1[w] - T1[l]; T2 <- t2$n * t2$a; w2 <- which.max(T2); l2 <- 3 - w2; D2 <- T2[w2] - T2[l2]
  num <- function(x) as.numeric(x)
  ## P1 «Ganó …, con …» y su razón
  m <- regmatches(sol, regexec("^Ganó (.+), con \\$([0-9]+) - ([0-9]+) = ([0-9]+)\\\\ \\\\text\\{kilos\\}\\$ más\\. (.*)$", sol)); m <- Filter(length, m)
  if (length(m) != 1) F(s, "P1 Ganó: ", length(m)) else { m <- m[[1]]
    if (!grepl(t1$nombre[w], tolower(m[2]), fixed = TRUE) || !identical(num(m[3:5]), c(T1[w], T1[l], D1))) F(s, "P1 Ganó cifras/nombre")
    rz <- if (t1$n[w] > t1$n[l]) sprintf("Su promedio era menor (%d kilos frente a %d), pero participaron más cursos (%d frente a %d), y eso pesó más.", t1$a[w], t1$a[l], t1$n[w], t1$n[l]) else
      sprintf("Participaron menos cursos (%d frente a %d), pero su promedio era mayor (%d kilos frente a %d), y eso pesó más.", t1$n[w], t1$n[l], t1$a[w], t1$a[l])
    if (m[6] != rz) F(s, "P1 razón: ", m[6]) else ok("p1_gano") }
  ## Partes 3 a 5
  m <- Filter(length, regmatches(sol, regexec("^Ganó el grado (\\S+), con \\$([0-9]+) - ([0-9]+) = ([0-9]+)\\$ kilos más\\. (.*)$", sol)))
  if (length(m) != 1) F(s, "P3-5 Ganó") else { m <- m[[1]]
    rz <- if (t2$n[w2] > t2$n[l2]) sprintf("Su promedio era menor (%d kilos frente a %d), pero participaron más cursos (%d frente a %d).", t2$a[w2], t2$a[l2], t2$n[w2], t2$n[l2]) else
      sprintf("Participaron menos cursos (%d frente a %d), pero su promedio era mayor (%d kilos frente a %d).", t2$n[w2], t2$n[l2], t2$a[w2], t2$a[l2])
    if (m[2] != t2$nombre[w2] || !identical(num(m[3:5]), c(T2[w2], T2[l2], D2)) || m[6] != rz) F(s, "P3-5 Ganó/razón") else ok("p35_gano") }
  for (i in 1:2) if (!any(sol == sprintf("- Grado %s: $%d \\times %d = %d$ kilos.", t2$nombre[i], t2$n[i], t2$a[i], T2[i]))) F(s, "P3-5 lista grado ", i)
  ## Caso específico
  m <- Filter(length, regmatches(sol, regexec("^En la Parte 1, (.+) recogió \\$([0-9]+) \\\\times ([0-9]+) = ([0-9]+)\\\\ \\\\text\\{kilos\\}\\$ y (.+) recogió \\$([0-9]+) \\\\times ([0-9]+) = ([0-9]+)\\\\ \\\\text\\{kilos\\.\\}\\$ La diferencia es \\$([0-9]+) - ([0-9]+) = ([0-9]+)\\\\ \\\\text\\{kilos\\}\\$ a favor de (.+)\\.$", sol)))
  if (length(m) != 1 || !grepl(t1$nombre[w], tolower(m[[1]][2]), fixed = TRUE) || !grepl(t1$nombre[l], tolower(m[[1]][6]), fixed = TRUE) || !grepl(t1$nombre[w], tolower(m[[1]][13]), fixed = TRUE) ||
      !identical(num(m[[1]][c(3:5, 7:9, 10:12)]), c(t1$n[w], t1$a[w], T1[w], t1$n[l], t1$a[l], T1[l], T1[w], T1[l], D1))) F(s, "Caso específico P1") else ok("caso_p1")
  m <- Filter(length, regmatches(sol, regexec("^En el otro colegio, el grado (\\S+) recogió \\$([0-9]+) \\\\times ([0-9]+) = ([0-9]+)\\$ kilos y el grado (\\S+), \\$([0-9]+) \\\\times ([0-9]+) = ([0-9]+)\\$ kilos: la diferencia es ([0-9]+) kilos\\.$", sol)))
  if (length(m) != 1 || m[[1]][2] != t2$nombre[w2] || m[[1]][6] != t2$nombre[l2] || !identical(num(m[[1]][c(3:5, 7:10)]), c(t2$n[w2], t2$a[w2], T2[w2], t2$n[l2], t2$a[l2], T2[l2], D2))) F(s, "Caso específico P2-6") else ok("caso_p26")
  ## Análisis de los distractores: P1 (las tres citadas = las tres no marcadas), P2 y P6
  af <- sub("^\\*Afirmación:\\* «(.*)»$", "\\1", grep("^\\*Afirmación:\\* «", sol, value = TRUE))
  if (!setequal(af, ql[1:4][!e$metainfo$solution[[1]]]) || length(af) != 3) F(s, "Análisis P1") else ok("analisis_p1")
  i2 <- grep("^\\*\\*Parte 2 · Lecturas del promedio que no sirven\\*\\*$", sol); q2 <- sub("^- «(.*)» .*$", "\\1", grep("^- «", sol[(i2 + 1):(i2 + 8)], value = TRUE))
  if (length(q2) != 3 || any(grepl(P2_CLAVE, q2)) || !setequal(q2, ql[5:8][!e$metainfo$solution[[2]]])) F(s, "Análisis P2") else ok("analisis_p2")
  i6 <- grep("^\\*\\*Parte 6 · Afirmaciones falsas\\*\\*$", sol); q6 <- sub("^- «(.*)» .*$", "\\1", grep("^- «", sol[(i6 + 1):min(length(sol), i6 + 8)], value = TRUE))
  if (!setequal(q6, ql[12:15][!e$metainfo$solution[[6]]])) F(s, "Análisis P6") else ok("analisis_p6")
  ## Feedback de la Parte 1 y explicaciones de distractores: totales citados por nombre, «frente a», «por D, no por E»
  txt <- c(sl[1:4], sol); RN <- paste(t1$nombre, collapse = "|")
  for (mm in unlist(regmatches(tolower(txt), gregexpr(paste0("(", RN, ") recogió [0-9]+ kilos"), tolower(txt))))) {
    k <- regmatches(mm, regexec(paste0("(", RN, ") recogió ([0-9]+) kilos"), mm))[[1]]; ok("fb_recogio"); if (num(k[3]) != T1[match(k[2], t1$nombre)]) F(s, "recogió N: ", mm) }
  for (mm in unlist(regmatches(tolower(txt), gregexpr(paste0("los [0-9]+ de (la sede |la jornada de la )?(", RN, ")"), tolower(txt))))) {
    k <- regmatches(mm, regexec(paste0("los ([0-9]+) de (la sede |la jornada de la )?(", RN, ")"), mm))[[1]]; ok("fb_los_de"); if (num(k[2]) != T1[match(k[4], t1$nombre)]) F(s, "los N de: ", mm) }
  for (mm in unlist(regmatches(txt, gregexpr("\\(([0-9]+) kilos frente a ([0-9]+)\\)", txt)))) { k <- num(regmatches(mm, gregexpr("[0-9]+", mm))[[1]]); ok("fb_frente")
    if (!setequal(k, c(T1[w], T1[l])) && !setequal(k, c(t1$a[w], t1$a[l])) && !setequal(k, c(T2[w2], T2[l2])) && !setequal(k, c(t2$a[w2], t2$a[l2]))) F(s, "frente a: ", mm) }
  for (mm in unlist(regmatches(txt, gregexpr("por [0-9]+ kilos, no por [0-9]+", txt)))) { k <- num(regmatches(mm, gregexpr("[0-9]+", mm))[[1]]); ok("fb_por_no_por"); if (k[1] != D1 || k[2] == D1) F(s, "por D no por E: ", mm) }
}
cat("versiones:", length(N), "· FALLOS:", length(fallos), "\n"); print(n_ok); if (length(fallos)) cat(head(fallos, 25), sep = "\n")
