BASE <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3"
L <- readLines(file.path(BASE, "cloze/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd"), encoding = "UTF-8")
i0 <- grep("^espacio_paralelo <- function", L); i1 <- which(L == "}" & seq_along(L) > i0)[1]
eval(parse(text = paste(L[i0:i1], collapse = "\n"), encoding = "UTF-8"))
for (r in c("CURSOS", "PROMEDIO")) for (p in list(c(8L, 3L), c(10L, 4L))) cat(r, p, nrow(espacio_paralelo(r, p[1], p[2])), "\n")
