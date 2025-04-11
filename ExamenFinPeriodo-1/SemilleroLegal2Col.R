# Carga de la librería r-exams
library(exams)

# Función para modificar el archivo .tex generado por R-exams
fix_tex_file <- function(tex_file) {
  # Leer el contenido del archivo .tex
  tex_content <- readLines(tex_file)
  
  # Modificar el preámbulo para usar tamaño legal y doble columna
  preamble_index <- grep("\\\\documentclass", tex_content)
  if (length(preamble_index) > 0) {
    # Cambiar el tamaño del papel a legal
    tex_content[preamble_index] <- "\\documentclass[10pt]{article}"
    
    # Añadir paquetes y configuraciones necesarias
    geometry_index <- grep("\\\\usepackage\\{geometry\\}", tex_content)
    if (length(geometry_index) > 0) {
      tex_content[geometry_index] <- "\\usepackage[papersize={216mm,356mm},tmargin=10mm,bmargin=10mm,lmargin=10mm,rmargin=10mm]{geometry}"
    } else {
      tex_content <- c(tex_content[1:preamble_index], 
                      "\\usepackage[papersize={216mm,356mm},tmargin=10mm,bmargin=10mm,lmargin=10mm,rmargin=10mm]{geometry}",
                      tex_content[(preamble_index+1):length(tex_content)])
    }
    
    # Añadir configuración para doble columna
    multicol_index <- grep("\\\\usepackage\\{multicol\\}", tex_content)
    if (length(multicol_index) == 0) {
      tex_content <- c(tex_content[1:preamble_index], 
                      "\\usepackage{multicol}",
                      tex_content[(preamble_index+1):length(tex_content)])
    }
    
    # Configurar la separación entre columnas
    columnseprule_index <- grep("\\\\setlength\\{\\\\columnseprule\\}", tex_content)
    if (length(columnseprule_index) == 0) {
      tex_content <- c(tex_content[1:preamble_index], 
                      "\\setlength{\\columnseprule}{0.4pt}",
                      tex_content[(preamble_index+1):length(tex_content)])
    }
  }
  
  # Iniciar el entorno de doble columna después del título
  begin_document_index <- grep("\\\\begin\\{document\\}", tex_content)
  if (length(begin_document_index) > 0) {
    # Buscar dónde termina la página de título
    newpage_index <- grep("\\\\newpage", tex_content)
    if (length(newpage_index) > 0) {
      first_newpage <- min(newpage_index[newpage_index > begin_document_index])
      tex_content <- c(tex_content[1:first_newpage], 
                      "\\begin{multicols}{2}",
                      tex_content[(first_newpage+1):length(tex_content)])
    }
  }
  
  # Cerrar el entorno de doble columna antes de finalizar el documento
  end_document_index <- grep("\\\\end\\{document\\}", tex_content)
  if (length(end_document_index) > 0) {
    tex_content <- c(tex_content[1:(end_document_index-1)], 
                    "\\end{multicols}",
                    tex_content[end_document_index:length(tex_content)])
  }
  
  # Reemplazar longtable por tabular
  tex_content <- gsub("\\\\begin\\{longtable\\}\\{.*?\\}", "\\\\begin{table}[htbp]\n\\\\centering\n\\\\begin{tabular}{ccc}", tex_content)
  tex_content <- gsub("\\\\end\\{longtable\\}", "\\\\end{tabular}\n\\\\end{table}", tex_content)
  
  # Eliminar comandos específicos de longtable
  tex_content <- gsub("\\\\endhead", "", tex_content)
  tex_content <- gsub("\\\\endfirsthead", "", tex_content)
  tex_content <- gsub("\\\\endfoot", "", tex_content)
  tex_content <- gsub("\\\\endlastfoot", "", tex_content)
  
  # Escribir el contenido modificado de vuelta al archivo .tex
  writeLines(tex_content, tex_file)
  
  return(TRUE)
}

# Función personalizada para generar exámenes PDF con tamaño legal y doble columna
exams2pdf_legal2col <- function(file, n = 1, nsamp = NULL, dir = ".", template = NULL,
                               name = NULL, quiet = TRUE, edir = NULL, ...) {
  # Generar el examen PDF usando la plantilla estándar
  result <- exams2pdf(file = file, n = n, nsamp = nsamp, dir = dir, 
                     template = "plain", name = name, quiet = quiet, edir = edir, ...)
  
  # Obtener la lista de archivos .tex generados
  tex_files <- list.files(dir, pattern = paste0(name, ".*\\.tex$"), full.names = TRUE)
  
  # Modificar cada archivo .tex
  for (tex_file in tex_files) {
    fix_tex_file(tex_file)
    
    # Compilar el archivo .tex modificado
    system(paste("cd", dir, "&&", "pdflatex", basename(tex_file)))
  }
  
  return(result)
}

# Definición del archivo de examen y configuración inicial
archivo_examen <- c(
  "2023-Matematicas-11-2-04-Op-B-V2.Rmd", "2023-Matematicas-11-2-09-Opc-A.Rmd",
  "2023-Matematicas-11-2-04-Op-C.Rmd", "2023-Matematicas-11-2-09-Opc-B.Rmd",
  "2023-Matematicas-11-2-04-Op-C-V2.Rmd", "DVenn_All_GenMus_01.Rmd", "DVenn_All_GenMus_01.Rmd",
  "DVenn_All_GenMus_01.Rmd", "DVenn_All_GenMus_01.Rmd", "DVenn_All_GenMus_01.Rmd",
  "DVenn_All_GenMus_01.Rmd", "DVenn_All_GenMus_01.Rmd",
  "2023-Matematicas-11-2-04-Op-D.Rmd",
  "2023-Matematicas-11-2-04-Op-D-V2.Rmd", "2023-Matematicas-11-2-04-Op-D-V2.Rmd")
copias <- 1
numpreg_por_archivo <- 1  # n preguntas por cada archivo
semilla <- sample(100:1e8, 1)
set.seed(semilla)
dir_salida <- "salida"
dir_ejercicios <- "ejercicios"

# Nombre del archivo sin la extensión .Rmd
nombre_sin_extension <- "ExamenFinPeriodo1-sol"  # Cambiado a un nombre genérico para el taller
nombre_arch <- paste0(nombre_sin_extension, "_")

################################################################################
# Generación de n copias en un solo archivo de salida para PDF (versión con soluciones)
# Usando la nueva función personalizada

set.seed(semilla)
exams2pdf_legal2col(rep(archivo_examen, each = numpreg_por_archivo),  # n preguntas de cada archivo
                   n = copias,
                   name = nombre_arch,
                   encoding = "UTF-8",
                   dir = dir_salida,
                   edir = dir_ejercicios)

################################################################################
# Generación de n copias en un solo archivo de salida para PDF (versión de examen)
# Usando la nueva función personalizada

set.seed(semilla)
exams2pdf_legal2col(rep(archivo_examen, each = numpreg_por_archivo),  # n preguntas de cada archivo
                   n = copias,
                   name = "ExamenFinPeriodo1",  # Corregido: nombre como string
                   encoding = "UTF-8",
                   dir = dir_salida,
                   edir = dir_ejercicios)

################################################################################
# Mensaje de finalización
cat("\nProceso completado. Los archivos se han generado en el directorio:", dir_salida, "\n")
cat("Semilla utilizada:", semilla, "\n")
