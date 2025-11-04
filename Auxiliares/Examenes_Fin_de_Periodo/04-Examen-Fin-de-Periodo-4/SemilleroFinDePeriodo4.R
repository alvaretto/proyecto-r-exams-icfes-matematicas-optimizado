# Carga de la librería r-exams
library(exams)

# Configurar locale y codificación UTF-8
Sys.setlocale("LC_ALL", "es_ES.UTF-8")
options(encoding = "UTF-8")

# Desactivar tests de diversidad durante generación de exámenes
.exams_generation_mode <- TRUE

# Definición del archivo de examen y configuración inicial
preg01 <- c(preg01 <- c(
  "001-muestreo_sesgo_municipio_aleatorio_argumentacion_n2_v1.Rmd",
  "002-cateto_teorema_pitagoras_geometrico_metrico_formulacion_ejecucion_n2_v1_2.Rmd",
  "003-cateto_teorema_pitagoras_geometrico_metrico_formulacion_ejecucion_n2_v1_1.Rmd",
  "004-pasteleria_sabores_ventas_estadistica_interpretacion_representacion_n2_v1.Rmd",
  "018-porcentajes_grupos_poblacion_v1.Rmd",
  "006-ganancias_comerciales_formulacion_ejecucion_n2_v1.Rmd",
  "007-proporcionalidad_empresarial_formulacion_ejecucion_n2_v1.Rmd",
  "008-funciones_lineales_interpretacion_grafica_v2.Rmd",
  "009-funciones_lineales_interpretacion_grafica_v1.Rmd",
  "010-empaques_tetra_pak_argumentacion_n3_v1.Rmd",
  "011-probabilidad_extraccion_bolas_v1.Rmd",
  "012-probabilidad_combinaciones_v1.Rmd",
  "014-parabrisas-2.Rmd",
  "015-volumen_cilindro_hueco_R_v1.Rmd",
  "017-porcentajes_ordenamiento_sabores_v1.Rmd"
))


archivo_examen <- preg01
archivo_examen <- sample(archivo_examen)

copias <- 1
numpreg_por_archivo <- 1
semilla <- sample(100:1e8, 1)
set.seed(semilla)
dir_salida <- "salida"
dir_ejercicios <- "."

# Nombre del archivo sin la extensión .Rmd (sin acentos para evitar problemas de codificación)
nombre_sin_extension <- "P3C-Matematicas_Evaluacion_Fin_de_Periodo_4"
nombre_arch <- paste0(nombre_sin_extension, "_")

################################################################################
# Generación de n copias en un solo archivo .docx

set.seed(semilla)
exams2pandoc(rep(archivo_examen, each = numpreg_por_archivo),
             n = copias,
             name = "Examen_Periodo4_pandoc_con_soluciones",
             encoding = "UTF-8",
             template = "pcielo.tex",
             header = list(Date = Sys.Date()),
             inputs = NULL,
             options = NULL,
             quiet = TRUE,
             resolution = 100,
             width = 4,
             height = 4,
             svg = TRUE,
             dir = dir_salida,
             edir = dir_ejercicios,
             tdir = NULL,
             sdir = NULL,
             verbose = TRUE,
             points = NULL,
             exshuffle = NULL,
             type = "docx")

################################################################################
# Generación de n copias, sin Solution, en un solo archivo .docx

set.seed(semilla)
exams2pandoc(rep(archivo_examen, each = numpreg_por_archivo),
             n = copias,
             name = "Examen_Periodo4_pandoc_sin_soluciones",
             encoding = "UTF-8",
             template = "pcielo_nosol.tex",
             solution = FALSE,
             header = list(Date = Sys.Date()),
             inputs = NULL,
             options = NULL,
             quiet = TRUE,
             resolution = 100,
             width = 4,
             height = 4,
             svg = TRUE,
             dir = dir_salida,
             edir = dir_ejercicios,
             tdir = NULL,
             sdir = NULL,
             verbose = TRUE,
             points = NULL,
             exshuffle = NULL,
             type = "docx")


################################################################################
# Generación de n copias en un solo archivo de salida para PDF (versión con soluciones)

set.seed(semilla)
exams2pdf(rep(archivo_examen, each = numpreg_por_archivo),
          n = copias,
          name = "Examen_Periodo4_pdf_con_soluciones",
          encoding = "UTF-8",
          template = "solpcielo",
          dir = dir_salida,
          edir = dir_ejercicios,
          verbose = TRUE)

################################################################################
# Generación de n copias en un solo archivo de salida para PDF (versión de examen)

set.seed(semilla)
exams2pdf(rep(archivo_examen, each = numpreg_por_archivo),
          n = copias,
          name = "Examen_Periodo4_pdf_sin_soluciones",
          encoding = "UTF-8",
          template = "exam",
          dir = dir_salida,
          edir = dir_ejercicios,
          verbose = TRUE)
