## Detractor r3 — barrido propio de CORRECCIÓN (Partes 2 a 6) y espacio duro, semillas 1:100 con knit real.
suppressMessages({library(exams); library(knitr)})
RMD <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd"
OUT <- "/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/7ecb76b3-2641-4f28-a0dc-aba5add06161/scratchpad/detractor_r3"
NB <- " "
## Verdad propia de la Parte 2: valor que describe el texto para un grado (todas las lecturas razonables).
lect_p2 <- function(txt, n, a, n2) {
  T <- n * a
  if (grepl("la mayoría de los cursos", txt, fixed = TRUE)) return(c(moda_testigo = a - 1))   # el promedio no fija la moda
  b <- sub("^Lo que recogió ese grado dividido ", "", txt)
  switch(b,
    "en partes iguales entre sus cursos." = T / n,
    "entre el número de sus cursos." = T / n,
    "entre la cantidad de cursos de ese mismo grado." = T / n,
    "entre el promedio de sus cursos." = T / a,
    "en partes iguales entre sus cursos y luego entre dos." = c(T / n / 2, T / (n / 2)),
    "entre el número de sus cursos al cuadrado." = c(T / n^2, (T / n)^2),
    "entre la cantidad de cursos de ambos grados." = T / (n + n2),
    NA_real_)
}
## Verdad propia de la Parte 6 (aritmética directa sobre la tabla LEÍDA del markdown mostrado).
leer_tabla <- function(md) {
  f <- grep("^\\|", strsplit(md, "\n", fixed = TRUE)[[1]], value = TRUE)
  f <- f[!grepl("^\\|[-:| ]+\\|$", f)][-1]
  t(vapply(f, function(r) { x <- trimws(strsplit(gsub("<span[^>]*>|</span>|\\*\\*", "", r), "|", fixed = TRUE)[[1]][-1])
    c(nom = tolower(x[1]), n = sub(" cursos$", "", x[2]), a = sub(" kilos$", "", x[3])) }, c(nom = "", n = "", a = "")))
}
verdad6 <- function(txt, tb) {
  nom <- tb[, "nom"]; n <- as.numeric(tb[, "n"]); a <- as.numeric(tb[, "a"]); T <- n * a
  g <- which.max(T); p <- 3L - g
  if (startsWith(txt, "Ganó el concurso el grado ")) return(sub("^Ganó el concurso el grado (.*)\\.$", "\\1", txt) == nom[g])
  if (txt == "Si el grado que perdió hubiera participado con un curso más, conservando su promedio por curso, habría ganado el concurso.") return(T[p] + a[p] > T[g])
  if (txt == "Si al grado que ganó le hubiera faltado un curso, conservando su promedio por curso, habría perdido el concurso.") return(T[g] - a[g] < T[p])
  if (grepl("^Si el grado que perdió hubiera participado con tantos cursos como el que ganó", txt)) return(n[g] * a[p] > T[g])
  if (grepl("^Si el grado que ganó hubiera participado con tantos cursos como el que perdió", txt)) return(n[p] * a[g] < T[p])
  if (grepl("^Si el grado que perdió hubiera tenido el mismo promedio por curso que el que ganó", txt)) return(n[p] * a[g] > T[g])
  if (grepl("^Si el grado que ganó hubiera tenido el mismo promedio por curso que el que perdió", txt)) return(n[g] * a[p] < T[p])
  NA
}
## Aritmética y comparación de cada explicación de la Parte 6 / Partes 3 a 5.
chk_expl <- function(s) {
  m <- regmatches(s, regexec("\\$([0-9]+) (\\+|-|\\\\times) ([0-9]+) = ([0-9]+)\\$", s))[[1]]
  if (!length(m)) return(TRUE)
  x <- as.numeric(m[2]); y <- as.numeric(m[4]); z <- as.numeric(m[5])
  ok <- z == switch(m[3], "+" = x + y, "-" = x - y, x * y)
  c2 <- regmatches(s, regexec("(más que los|menos que los|que no superan los|que siguen superando los) ([0-9]+)", s))[[1]]
  if (length(c2)) ok <- ok && switch(c2[2], "más que los" = z > as.numeric(c2[3]), "menos que los" = z < as.numeric(c2[3]),
                                     "que no superan los" = z <= as.numeric(c2[3]), "que siguen superando los" = z > as.numeric(c2[3]))
  ok
}
fallos <- character(0); F <- function(...) fallos <<- c(fallos, paste0(...))
nb_ctx <- character(0); sp_ctx <- character(0); reg <- list()
for (s in 1:100) {
  set.seed(s); en <- new.env()
  md <- tryCatch(knit(text = readLines(RMD, encoding = "UTF-8"), envir = en, quiet = TRUE), error = function(e) paste("ERROR", conditionMessage(e)))
  if (startsWith(md[1], "ERROR")) { F("s=", s, " ", md); next }
  md <- paste(md, collapse = "\n"); Lm <- strsplit(md, "\n", fixed = TRUE)[[1]]
  if (s <= 3) writeLines(Lm, file.path(OUT, "knit", sprintf("s%03d.md", s)), useBytes = TRUE)
  with(en, {
    tb <- leer_tabla(tabla_par_md); n <- as.numeric(tb[, "n"]); a <- as.numeric(tb[, "a"])
    ## Parte 2
    pos_true <- vapply(opciones_p2, function(o) any(vapply(1:2, function(i) any(abs(lect_p2(o, n[i], a[i], n[3 - i]) - a[i]) < 1e-9), NA)), NA)
    all_true <- vapply(opciones_p2, function(o) all(vapply(1:2, function(i) all(abs(lect_p2(o, n[i], a[i], n[3 - i]) - a[i]) < 1e-9), NA)), NA)
    if (any(is.na(pos_true))) F("s=", s, " P2 texto sin lectura propia: ", paste(opciones_p2[is.na(pos_true)], collapse = " | "))
    if (!identical(unname(pos_true), unname(sol_p2)) || !identical(unname(all_true), unname(sol_p2))) F("s=", s, " P2 verdad propia != sol_p2")
    if (!identical(startsWith(feedback_p2, "Verdadero."), unname(sol_p2))) F("s=", s, " P2 feedback desalineado")
    ids <- names(op2)[o2]
    for (i in which(!sol_p2)) if (!identical(feedback_p2[i], paste("Falso.", expl2[[ids[i]]])) || !identical(opciones_p2[i], unname(q2$distractores[ids[i]]))) F("s=", s, " P2 expl de la opción ", i, " no es la de su distractor")
    if (strsplit(exsolution_str, "|", fixed = TRUE)[[1]][2] != mchoice2string(sol_p2)) F("s=", s, " exsolution P2")
    ## Partes 3 a 5
    T <- n * a; nm <- tb[, "nom"]
    g3 <- sub("^.* el grado (.*)\\?$", "\\1", texto_p3); g4 <- sub("^.* el grado (.*)\\?$", "\\1", texto_p4)
    if (exsol_p3 != T[nm == g3] || exsol_p4 != T[nm == g4] || g3 == g4 || exsol_p5 != abs(T[1] - T[2])) F("s=", s, " Partes 3-5 claves")
    es <- strsplit(exsolution_str, "|", fixed = TRUE)[[1]]
    if (!all(as.numeric(es[3:5]) == c(exsol_p3, exsol_p4, exsol_p5))) F("s=", s, " exsolution 3-5")
    for (fb in c(feedback_p3, feedback_p4, feedback_p5, expl6)) if (!chk_expl(fb)) F("s=", s, " aritmética/comparación: ", fb)
    ## Parte 6
    v <- vapply(opciones_p6, verdad6, NA, tb = tb)
    if (!identical(unname(v), unname(sol_p6))) F("s=", s, " P6 verdad propia != sol_p6: ", paste(rol_p6, v, sol_p6, collapse = "; "))
    if (!identical(startsWith(feedback_p6, "Verdadero."), unname(sol_p6))) F("s=", s, " P6 feedback desalineado")
    if (es[6] != mchoice2string(sol_p6)) F("s=", s, " exsolution P6")
    reg[[length(reg) + 1]] <<- data.frame(s = s, rama = rama, canon = isTRUE(es_canonica), rama_par = rama_par, k = dp$k,
      delta = dp$delta, cascada = cascada_par, clave_p2 = match(clave_p2, q2$clave), dis = paste(sort(names(dis_p2)), collapse = ","),
      ventaja = c6$id, hp = v_hp, hg = v_hg, nv = sum(sol_p6))
  })
  ## Espacio duro: dónde está y dónde falta
  iq <- grep("^Question$", Lm); ia <- grep("^##ANSWER1##$", Lm); is <- grep("^Solution$", Lm); im <- grep("^Meta-information$", Lm)
  for (i in seq_along(Lm)) {
    zona <- if (i < ia) "Q-Parte1" else if (i < is) "Q-Partes2a6" else if (i < im) "Solution" else "Meta"
    l <- Lm[i]; tabla <- startsWith(l, "|")
    if (grepl(NB, l, fixed = TRUE)) nb_ctx <- c(nb_ctx, paste(zona, if (tabla) "TABLA" else "", gsub("sexto|séptimo|octavo|noveno", "GRADO", gsub("[0-9]+", "#", substr(gsub(NB, "~", l, fixed = TRUE), 1, 60)))))
    if (grepl("[0-9]\\$? (kilos|cursos)", l) && !tabla) sp_ctx <- c(sp_ctx, paste(zona, gsub("sexto|séptimo|octavo|noveno", "GRADO", gsub("[0-9]+", "#", substr(l, 1, 60)))))
  }
}
R <- do.call(rbind, reg)
cat("Versiones:", nrow(R), " fallos:", length(fallos), "\n"); if (length(fallos)) print(head(fallos, 30))
cat("\nRama P1:", table(R$rama), " canónicas:", sum(R$canon), " rama_par:", table(R$rama_par), " cascada:", table(R$cascada), "\n")
cat("clave P2 (1,2,3):", table(R$clave_p2), "\n"); print(table(R$ventaja, R$rama_par)); print(table(nv = R$nv))
cat("\n== Contextos con U+00A0 (zona, plantilla) ==\n"); print(sort(table(nb_ctx), decreasing = TRUE))
cat("\n== Cifra + espacio NORMAL + kilos/cursos fuera de tablas ==\n"); print(sort(table(sp_ctx), decreasing = TRUE))
saveRDS(R, file.path(OUT, "reg_1_100.rds"))
