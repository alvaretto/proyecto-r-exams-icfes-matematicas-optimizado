## Ejemplo de referencia del motor: las 11 preguntas de muestras/ (exámenes 01, 02 y 04), en 3 versiones.
## Uso, desde 00-Cuadernillos-Oficio-2Col/:   Rscript generar_cuadernillos.R configuraciones/examen_real_11.R --limpiar
## Las rutas son relativas a ESTE archivo. La salida queda en salida/examen-real-11/sesion-1/ (git ignora **/salida/).
## La semilla 2025 reproduce la matriz de las muestras del 2026-11-13 (el motor hace set.seed(semilla + número de sesión)).
## Los 11 ejercicios son schoice (medido: clave única en 33 de 33 pares).
ejercicios_11 <- c(
  "../../02-Examen_Fin_de_Periodo_2/01-S2-2025-SEDQ-grafico_circular_bienes_v0.Rmd",
  "../../02-Examen_Fin_de_Periodo_2/11_C01_G09_2020_Tipo1.Rmd",
  "../../01-ExamenFinPeriodo-1/2023-Matematicas-11-2-04-Op-D-V2.Rmd",
  "../../04-Examen-Fin-de-Periodo-4/cateto_teorema_pitagoras_geometrico_metrico_formulacion_ejecucion_n3_v1_1.Rmd",
  "../../04-Examen-Fin-de-Periodo-4/ExportacionesGraficosEstadisticaInterpretacion_n3_v1.Rmd",
  "../../02-Examen_Fin_de_Periodo_2/fracciones_reparto_premio_v1.Rmd",
  "../../01-ExamenFinPeriodo-1/I_1796473-Opc-A2.Rmd",
  "../../01-ExamenFinPeriodo-1/I_1796473-Opc-C2v2.Rmd",
  "../../04-Examen-Fin-de-Periodo-4/pasteleria_sabores_ventas_estadistica_interpretacion_representacion_n2_v1.Rmd",
  "../../01-ExamenFinPeriodo-1/schoice-DVenn_All_GenMus_02.Rmd",
  "../../02-Examen_Fin_de_Periodo_2/vaso-cilindrico-v6.Rmd")

config <- list(
  titulo      = "Evaluación de Matemáticas",
  institucion = "I. E. Pedacito de Cielo, La Tebaida, Quindío",
  fecha       = as.Date("2026-11-13"),
  estudiantes = 3L,
  semilla     = 2025L,
  sesiones    = list(list(nombre = "Sesión 1", startid = 1L, ejercicios = ejercicios_11)),
  salidas     = c("cuadernillo", "hoja", "clave", "retro", "word"),
  dir_salida  = "../salida/examen-real-11"
)
