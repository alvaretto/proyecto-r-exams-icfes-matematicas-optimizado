# Carga de la librería r-exams y el script de funciones personalizadas
library(exams)
source("ExamenLegal2Col.R")

# Definición del archivo de examen y configuración inicial
archivo_examen <- c(
  "2023-Matematicas-11-2-04-Op-B-V2.Rmd", "2023-Matematicas-11-2-09-Opc-A.Rmd",
  "2023-Matematicas-11-2-04-Op-C.Rmd", "2023-Matematicas-11-2-09-Opc-B.Rmd",
  "2023-Matematicas-11-2-04-Op-C-V2.Rmd", "DVenn_All_GenMus_01.Rmd",
  "DVenn_All_GenMus_01.Rmd", "DVenn_All_GenMus_01.Rmd", "DVenn_All_GenMus_01.Rmd",
  "DVenn_All_GenMus_01.Rmd", "DVenn_All_GenMus_01.Rmd", "DVenn_All_GenMus_01.Rmd",
  "2023-Matematicas-11-2-04-Op-D.Rmd",
  "2023-Matematicas-11-2-04-Op-D-V2.Rmd", "2023-Matematicas-11-2-04-Op-D-V2.Rmd")

# Configuración general
copias <- 1
numpreg_por_archivo <- 1  # n preguntas por cada archivo
semilla <- sample(100:1e8, 1)
set.seed(semilla)
dir_salida <- "salida"
dir_ejercicios <- "ejercicios"

# Nombre de los archivos de salida
nombre_sol <- "ExamenFinPeriodo1-sol-"
nombre_examen <- "ExamenFinPeriodo1-"
nombre_docx <- "ExamenFinPeriodo1-docx-"
nombre_nops <- "ExamenFinPeriodo1-nops-"

################################################################################
# 1. Generación de PDF con soluciones (tamaño oficio, doble columna, márgenes estrechos)
################################################################################

set.seed(semilla)
exams2pdf_sol_legal2col(
  rep(archivo_examen, each = numpreg_por_archivo),
  n = copias,
  name = nombre_sol,
  encoding = "UTF-8",
  dir = dir_salida,
  edir = dir_ejercicios
)

################################################################################
# 2. Generación de PDF para examen (tamaño oficio, doble columna, márgenes estrechos)
################################################################################

set.seed(semilla)
exams2pdf_legal2col(
  rep(archivo_examen, each = numpreg_por_archivo),
  n = copias,
  name = nombre_examen,
  encoding = "UTF-8",
  dir = dir_salida,
  edir = dir_ejercicios
)

################################################################################
# 3. Generación de DOCX (tamaño oficio, doble columna, márgenes estrechos)
################################################################################

set.seed(semilla)
exams2pandoc_legal2col(
  rep(archivo_examen, each = numpreg_por_archivo),
  n = copias,
  name = nombre_docx,
  encoding = "UTF-8",
  dir = dir_salida,
  edir = dir_ejercicios,
  svg = TRUE,  # Usar SVG para gráficos
  header = list(Date = Sys.Date())
)

################################################################################
# 4. Generación de NOPS para examen escaneable (tamaño oficio, doble columna)
################################################################################

set.seed(semilla)
exams2nops_legal2col(
  rep(archivo_examen, each = numpreg_por_archivo),
  n = copias,
  name = nombre_nops,
  encoding = "UTF-8",
  dir = dir_salida,
  edir = dir_ejercicios
)

################################################################################
# Mensaje de finalización
cat("\nProceso completado. Los archivos se han generado en el directorio:", dir_salida, "\n")
cat("Formatos generados: PDF, PDF con soluciones, DOCX, NOPS\n")
cat("Semilla utilizada:", semilla, "\n")
