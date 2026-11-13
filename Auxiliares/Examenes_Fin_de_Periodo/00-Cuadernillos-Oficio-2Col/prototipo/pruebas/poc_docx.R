library(exams)
.exams_generation_mode <- TRUE
ej8 <- c("vaso-cilindrico-v6.Rmd", "11_C01_G09_2020_Tipo1.Rmd", "2023-Matematicas-11-2-09-Opc-A.Rmd",
         "cateto_teorema_pitagoras_geometrico_metrico_formulacion_ejecucion_n2_v1_1.Rmd",
         "pasteleria_sabores_ventas_estadistica_interpretacion_representacion_n2_v1.Rmd")
ref <- normalizePath("docx/referencia_oficio_2col.docx")  # antes de la llamada: exams2pandoc evalúa options en otro directorio
set.seed(5)
exams2pandoc(ej8, n = 1, dir = "docx", edir = "ej", name = "word_", type = "docx",
             options = paste0("--reference-doc=", ref))
