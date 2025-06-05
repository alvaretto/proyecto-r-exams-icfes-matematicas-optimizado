# Script para probar la versión corregida del ejercicio de ortocentro
# Carga de la librería r-exams
library(exams)

# Definición del archivo de examen y configuración inicial
archivo_examen <- "ortocentro_alturas_triangulo_geometria_n2_v1_fixed.Rmd"
copias <- 1
numpreg <- 1  # Probar solo una pregunta para agilizar
semilla <- 12345  # Semilla fija para reproducibilidad
set.seed(semilla)
dir_salida <- "salida"
dir_ejercicios <- "."

# Nombre del archivo sin la extensión .Rmd
nombre_sin_extension <- sub("\\.Rmd$", "", archivo_examen)
nombre_arch <- paste0(nombre_sin_extension, "_")

# Imprimir información de depuración
cat("Iniciando prueba con el archivo:", archivo_examen, "\n")
cat("Semilla:", semilla, "\n")

# Prueba HTML simple (generalmente la más rápida)
cat("\n\n==== PRUEBA HTML ====\n")
resultado_html <- try(
  exams2html(archivo_examen,
             n = 1,
             svg = FALSE,
             verbose = TRUE)
)
if(inherits(resultado_html, "try-error")) {
  cat("\nError en la generación HTML. Revisar los mensajes anteriores.\n")
} else {
  cat("\nGeneración HTML exitosa.\n")
}

# Si HTML funciona, intentar PDF
if(!inherits(resultado_html, "try-error")) {
  cat("\n\n==== PRUEBA PDF ====\n")
  resultado_pdf <- try(
    exams2pdf(archivo_examen,
              n = 1,
              name = paste0(nombre_arch, "_pdf"),
              encoding = "UTF-8",
              template = "solpcielo",
              dir = dir_salida,
              edir = dir_ejercicios,
              verbose = TRUE)
  )
  if(inherits(resultado_pdf, "try-error")) {
    cat("\nError en la generación PDF. Revisar los mensajes anteriores.\n")
  } else {
    cat("\nGeneración PDF exitosa.\n")
  }
}

# Prueba Docx
cat("\n\n==== PRUEBA DOCX ====\n")
resultado_docx <- try(
  exams2pandoc(archivo_examen,
               n = 1,
               name = paste0(nombre_arch, "_docx"),
               encoding = "UTF-8",
               template = "pcielo.tex",
               dir = dir_salida,
               edir = dir_ejercicios,
               format = "docx",
               verbose = TRUE)
)
if(inherits(resultado_docx, "try-error")) {
  cat("\nError en la generación DOCX. Revisar los mensajes anteriores.\n")
} else {
  cat("\nGeneración DOCX exitosa.\n")
}

cat("\n\nPruebas completadas.\n")