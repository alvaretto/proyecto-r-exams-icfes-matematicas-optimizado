# Script para trabajar con R-exams sin pdftools

# Cargar exams sin cargar pdftools
suppressPackageStartupMessages(library(exams))

# Función para generar exámenes en formatos que no requieren pdftools
generate_exam <- function(rmd_file, output_dir = ".", n_copies = 1) {
  # Verificar que el archivo existe
  if(!file.exists(rmd_file)) {
    stop(paste("El archivo", rmd_file, "no existe."))
  }
  
  # Crear directorio de salida si no existe
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Obtener nombre base del archivo
  base_name <- tools::file_path_sans_ext(basename(rmd_file))
  
  # Generar HTML (no requiere pdftools)
  cat("Generando HTML...\n")
  exams2html(rmd_file,
             n = n_copies,
             name = base_name,
             dir = output_dir,
             mathjax = TRUE,
             solution = TRUE)
  
  # Generar DOCX (no requiere pdftools)
  cat("Generando DOCX...\n")
  exams2pandoc(rmd_file, 
               n = n_copies,
               name = base_name,
               dir = output_dir,
               type = "docx",
               template = NULL)
  
  cat("\nExámenes generados en el directorio", output_dir, "\n")
}

# Ejemplo de uso
# generate_exam("test_latex_tikz.Rmd")
