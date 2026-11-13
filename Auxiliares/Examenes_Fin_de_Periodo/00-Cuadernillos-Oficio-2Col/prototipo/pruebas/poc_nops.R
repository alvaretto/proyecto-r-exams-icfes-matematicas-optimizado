library(exams)
.exams_generation_mode <- TRUE
ej8 <- c("vaso-cilindrico-v6.Rmd", "fracciones_reparto_premio_v1.Rmd", "11_C01_G09_2020_Tipo1.Rmd",
         "2023-Matematicas-11-2-09-Opc-A.Rmd", "01-S2-2025-SEDQ-grafico_circular_bienes_v0.Rmd",
         "cateto_teorema_pitagoras_geometrico_metrico_formulacion_ejecucion_n2_v1_1.Rmd",
         "pasteleria_sabores_ventas_estadistica_interpretacion_representacion_n2_v1.Rmd",
         "ExportacionesGraficosEstadisticaInterpretacion_n3_v1.Rmd")
n <- 2; fecha <- as.Date("2026-11-13")
set.seed(77); semillas <- matrix(sample(1:1e6, n * length(ej8)), nrow = n)
id <- function(i) paste0(format(fecha, "%y%m%d"), formatC(i, width = 5, flag = "0"))
unlink(c("nops", "cuad_a", "cuad_b"), recursive = TRUE); for (d in c("nops", "cuad_a", "cuad_b")) dir.create(d)
x_nops <- exams2nops(ej8, n = n, seed = semillas, dir = "nops", edir = "ej", name = "nops_", date = fecha,
                     language = "es", institution = "I. E. Pedacito de Cielo", title = "Evaluación de Matemáticas",
                     duplex = TRUE, twocolumn = TRUE, samepage = TRUE, header = "\\raggedbottom")
x_a <- exams2pdf(ej8, n = n, seed = semillas, dir = "cuad_a", edir = "ej", name = "cuad_", template = "cuadernillo_oficio_2col",
                 header = list(Date = format(fecha), ID = id))
x_b <- exams2pdf(ej8, n = n, seed = semillas, dir = "cuad_b", edir = "ej", name = "cuad_", template = "cuadernillo_oficio_2col",
                 header = list(Date = format(fecha), ID = id))
firma <- function(e) paste(c(e$question, unlist(e$questionlist)), collapse = "\n")
cat(sprintf("%-8s %-4s %-58s %-9s %-9s %-9s\n", "version", "preg", "archivo", "texto=", "clave=", "repro="))
for (i in seq_len(n)) for (j in seq_along(ej8)) {
  a <- x_a[[i]][[j]]; b <- x_b[[i]][[j]]; z <- x_nops[[i]][[j]]
  cat(sprintf("%-8s %-4d %-58s %-9s %-9s %-9s\n", id(i), j, substr(ej8[j], 1, 58),
      identical(firma(a), firma(z)), identical(a$metainfo$solution, z$metainfo$solution),
      identical(firma(a), firma(b))))
}
cat("IDs NOPS:", names(x_nops), "\n")
