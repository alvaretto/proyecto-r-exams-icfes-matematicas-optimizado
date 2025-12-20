# SemilleroUnico.R para generar exámenes de Geometría

# Cargar librerías necesarias
library(exams)
library(knitr)

# Definir directorio de trabajo y archivos
#setwd("/home/manjaro_lenovo/Insync/alvaroangelm@iepedacitodecielo.edu.co/Google Drive/RepositorioMatematicasICFES_R_Exams")
archivo_rmd <- "Cilindro_Hueco.Rmd"
n_preguntas <- 5  # Número de variantes a generar

# Crear directorio para los ejercicios si no existe
if(!dir.exists("ejercicios")) dir.create("ejercicios")
# Crear directorio para salida si no existe
if(!dir.exists("salida")) dir.create("salida")

# Establecer semilla para reproducibilidad
set.seed(1234)

# Generar PDF
exams2pdf(archivo_rmd, 
          n = n_preguntas,
          name = "Geometria_Semejanza_Triangulos",
          dir = "salida",
          template = c("pcielo", "solpcielo"),
          encoding = "UTF-8")

# Generar DOCX
exams2pandoc(archivo_rmd, 
             n = n_preguntas,
             name = "Geometria_Semejanza_Triangulos",
             dir = "salida",
             type = "docx",
             template = NULL)

# Generar HTML
exams2html(archivo_rmd,
           n = n_preguntas,
           name = "Geometria_Semejanza_Triangulos",
           dir = "salida",
           mathjax = TRUE,
           solution = TRUE)

# Generar XML para Moodle (Comentado por defecto)
# exams2moodle(archivo_rmd,
#              n = n_preguntas,
#              name = "Geometria_Semejanza_Triangulos",
#              dir = "salida",
#              encoding = "UTF-8")

cat("Generación de documentos completada en el directorio 'salida'\n")