library(exams)
.exams_generation_mode <- TRUE
ej <- c("vaso-cilindrico-v6.Rmd", "fracciones_reparto_premio_v1.Rmd", "11_C01_G09_2020_Tipo1.Rmd",
        "2023-Matematicas-11-2-09-Opc-A.Rmd", "01-S2-2025-SEDQ-grafico_circular_bienes_v0.Rmd",
        "cateto_teorema_pitagoras_geometrico_metrico_formulacion_ejecucion_n2_v1_1.Rmd",
        "pasteleria_sabores_ventas_estadistica_interpretacion_representacion_n2_v1.Rmd",
        "ExportacionesGraficosEstadisticaInterpretacion_n3_v1.Rmd")
unlink(c("salida", "tex"), recursive = TRUE); dir.create("salida")
set.seed(2026)
exams2pdf(ej, n = 1, dir = "salida", edir = "ej", name = "base_estudiante_", template = "estudiante_oficial",
          texdir = "tex", encoding = "UTF-8")
