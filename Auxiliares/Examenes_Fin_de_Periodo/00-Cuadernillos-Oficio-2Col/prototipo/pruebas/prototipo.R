library(exams)
.exams_generation_mode <- TRUE
ej8 <- c("vaso-cilindrico-v6.Rmd", "fracciones_reparto_premio_v1.Rmd", "11_C01_G09_2020_Tipo1.Rmd",
         "2023-Matematicas-11-2-09-Opc-A.Rmd", "01-S2-2025-SEDQ-grafico_circular_bienes_v0.Rmd",
         "cateto_teorema_pitagoras_geometrico_metrico_formulacion_ejecucion_n2_v1_1.Rmd",
         "pasteleria_sabores_ventas_estadistica_interpretacion_representacion_n2_v1.Rmd",
         "ExportacionesGraficosEstadisticaInterpretacion_n3_v1.Rmd")
hdr <- list(Date = format(Sys.Date()), ID = function(i) paste0(format(Sys.Date(), "%y%m%d"), formatC(i, width = 5, flag = "0")))
unlink(c("salida", "tex8", "tex10"), recursive = TRUE); dir.create("salida")
set.seed(2026)
exams2pdf(ej8, n = 1, dir = "salida", edir = "ej", name = "proto8_", template = "cuadernillo_oficio_2col",
          header = hdr, texdir = "tex8", encoding = "UTF-8")
set.seed(2026)
exams2pdf(c(ej8, "2023-Matematicas-11-2-04-Op-D-V2.Rmd", "poc_figuras.Rmd"), n = 1, dir = "salida", edir = "ej",
          name = "proto10_", template = "cuadernillo_oficio_2col", header = hdr, texdir = "tex10", encoding = "UTF-8")
