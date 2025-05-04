# Desde R o RStudio
source("ejecutar_pruebas_general.R")
ejecutar_pruebas_completas("volumen_cilindro_hueco_v1.Rmd",
                           n_simulaciones_diversidad = 50,
                           n_simulaciones_opciones = 20,
                           n_simulaciones_visual = 2)
