L <- readLines("/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/bateria_partes_2_a_6.R", encoding = "UTF-8")
i <- grep("^\\.NG <- new.env", L); j <- grep("^\\.ngr <- function", L); k <- which(seq_along(L) > j & grepl("^[^ ]", L))[1] - 1L
ip <- grep("^\\.pal <- function", L); kp <- which(seq_along(L) > ip & grepl("^[^ ]", L))[1] - 1L
eval(parse(text = L[c(ip:kp, i, j:k)], encoding = "UTF-8"))
tx <- c(HP = "Si el grado que perdió hubiera participado con un curso más, conservando su promedio por curso, habría ganado el concurso.",
  HG = "Si al grado que ganó le hubiera faltado un curso, conservando su promedio por curso, habría perdido el concurso.",
  BC_perdio = "Si el grado que perdió hubiera participado con tantos cursos como el que ganó, conservando su promedio por curso, habría ganado el concurso.",
  BC_gano = "Si el grado que ganó hubiera participado con tantos cursos como el que perdió, conservando su promedio por curso, habría perdido el concurso.",
  BP_perdio = "Si el grado que perdió hubiera tenido el mismo promedio por curso que el que ganó, conservando su cantidad de cursos, habría ganado el concurso.",
  BP_gano = "Si el grado que ganó hubiera tenido el mismo promedio por curso que el que perdió, conservando su cantidad de cursos, habría perdido el concurso.",
  G1 = "Ganó el concurso el grado sexto.", G2 = "Ganó el concurso el grado octavo.")
mut <- "Si el grado que ganó hubiera tenido el mismo promedio por curso que el que perdió hubiera tenido, conservando su cantidad de cursos, habría perdido el concurso."
NGo <- lapply(tx, .ngr); ngm <- .ngr(mut)
nuevos <- setdiff(ngm, NGo$BP_gano); perdidos <- setdiff(NGo$BP_gano, ngm)
quien <- function(t, lst) paste(names(lst)[vapply(lst, function(v) t %in% v, NA)], collapse = ",")
otros <- NGo[c("BC_perdio", "BC_gano", "BP_perdio")]
cat("n-gramas nuevos en BP_gano mutado y qué otros textos los llevan:\n"); for (t in nuevos) cat(sprintf("  «%s»: %s\n", t, quien(t, otros)))
cat("n-gramas que BP_gano pierde:\n"); for (t in perdidos) cat(sprintf("  «%s»: %s\n", t, quien(t, otros)))
excl_orig <- Filter(function(t) quien(t, otros) == "", NGo$BP_gano); cat("exclusivos de BP_gano original:", paste(sprintf("«%s»", excl_orig), collapse = " "), "\n")
excl_mut <- Filter(function(t) quien(t, otros) == "", ngm); cat("exclusivos de BP_gano mutado:", paste(sprintf("«%s»", excl_mut), collapse = " "), "\n")
