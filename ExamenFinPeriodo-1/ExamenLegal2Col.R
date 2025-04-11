# Carga de la librería r-exams
library(exams)

# Función para modificar el archivo .tex generado por R-exams
fix_tex_file <- function(tex_file) {
  # Leer el contenido del archivo .tex
  tex_content <- readLines(tex_file)
  
  # Reemplazar longtable por tabular
  tex_content <- gsub("\\\\begin\\{longtable\\}\\{.*?\\}", "\\\\begin{table}[htbp]\n\\\\centering\n\\\\begin{tabular}{ccc}", tex_content)
  tex_content <- gsub("\\\\end\\{longtable\\}", "\\\\end{tabular}\n\\\\end{table}", tex_content)
  
  # Eliminar comandos específicos de longtable
  tex_content <- gsub("\\\\endhead", "", tex_content)
  tex_content <- gsub("\\\\endfirsthead", "", tex_content)
  tex_content <- gsub("\\\\endfoot", "", tex_content)
  tex_content <- gsub("\\\\endlastfoot", "", tex_content)
  
  # Mejorar el manejo de figuras
  # Identificar figuras de ancho completo (grandes) y aplicar un entorno especial
  # Buscar patrones de inclusión de imágenes
  for (i in 1:length(tex_content)) {
    if (grepl("\\\\includegraphics", tex_content[i])) {
      # Verificar si la figura debe ocupar todo el ancho
      if (grepl("width=", tex_content[i]) && grepl("0\\.[7-9]\\\\", tex_content[i])) {
        # Es una figura grande, debe usar el entorno de figura de ancho completo
        if (i > 1 && grepl("\\\\begin\\{figure\\}", tex_content[i-1])) {
          tex_content[i-1] <- "\\begin{widefigureenv}"
          
          # Buscar el final de la figura
          for (j in i:min(i+10, length(tex_content))) {
            if (grepl("\\\\end\\{figure\\}", tex_content[j])) {
              tex_content[j] <- "\\end{widefigureenv}"
              break
            }
          }
        }
      }
    }
  }
  
  # Mejorar el manejo de tablas grandes
  for (i in 1:length(tex_content)) {
    if (grepl("\\\\begin\\{table\\}", tex_content[i]) && grepl("tabular\\{\\|.*\\|\\}", tex_content[i+1])) {
      # Es probablemente una tabla grande
      tex_content[i] <- "\\begin{widetableenv}"
      
      # Buscar el final de la tabla
      for (j in i:min(i+20, length(tex_content))) {
        if (grepl("\\\\end\\{table\\}", tex_content[j])) {
          tex_content[j] <- "\\end{widetableenv}"
          break
        }
      }
    }
  }
  
  # Escribir el contenido modificado de vuelta al archivo .tex
  writeLines(tex_content, tex_file)
  
  return(TRUE)
}

# Función personalizada para generar exámenes PDF con tamaño legal y doble columna
exams2pdf_legal2col <- function(file, n = 1, nsamp = NULL, dir = ".", template = NULL,
                               name = NULL, quiet = TRUE, edir = NULL, ...) {
  # Generar el examen PDF usando la plantilla personalizada
  result <- exams2pdf(file = file, n = n, nsamp = nsamp, dir = dir, 
                     template = "exam_legal_2col", name = name, quiet = quiet, edir = edir, ...)
  
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

# Función personalizada para generar exámenes con soluciones en PDF con tamaño legal y doble columna
exams2pdf_sol_legal2col <- function(file, n = 1, nsamp = NULL, dir = ".", template = NULL,
                                  name = NULL, quiet = TRUE, edir = NULL, ...) {
  # Generar el examen PDF usando la plantilla personalizada
  result <- exams2pdf(file = file, n = n, nsamp = nsamp, dir = dir, 
                     template = "solpcielo_legal_2col", name = name, quiet = quiet, edir = edir, ...)
  
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

# Función personalizada para generar exámenes DOCX con tamaño legal y doble columna
exams2pandoc_legal2col <- function(file, n = 1, nsamp = NULL, dir = ".", template = NULL,
                                 name = NULL, quiet = TRUE, edir = NULL, ...) {
  # Generar el examen DOCX usando la plantilla personalizada
  result <- exams2pandoc(file = file, n = n, nsamp = nsamp, dir = dir,
                        template = "pcielo_legal_2col.tex", name = name, quiet = quiet, 
                        edir = edir, type = "docx", ...)
  
  return(result)
}

# Función personalizada para generar exámenes NOPS con tamaño legal y doble columna
exams2nops_legal2col <- function(file, n = 1, nsamp = NULL, dir = ".", name = NULL,
                               quiet = TRUE, edir = NULL, ...) {
  # Modificar los parámetros específicos de NOPS para usar tamaño legal
  result <- exams2nops(file = file, n = n, nsamp = nsamp, dir = dir, 
                      name = name, quiet = quiet, edir = edir,
                      language = "es",
                      title = "Examen Final de Periodo",
                      institution = "I. E. Pedacito de Cielo",
                      date = Sys.Date(),
                      replacement = FALSE,
                      blank = 0,
                      duplex = TRUE,
                      pages = NULL,
                      points = NULL,
                      showpoints = FALSE,
                      ...)
  
  # Para exams2nops necesitamos modificar el PDF resultante con pdfjam
  nops_files <- list.files(dir, pattern = paste0(name, ".*\\.pdf$"), full.names = TRUE)
  
  # Utilizar pdfjam para cambiar el tamaño del papel a legal
  for (pdf_file in nops_files) {
    output_file <- sub("\\.pdf$", "_legal.pdf", pdf_file)
    system(paste("pdfjam", pdf_file, "--paper letterpaper --scale 0.95 --offset '0mm 0mm' --outfile", output_file))
    
    # Reemplazar el archivo original con la versión redimensionada
    system(paste("mv", output_file, pdf_file))
  }
  
  return(result)
}

# Ejemplo de uso
# Definición del archivo de examen y configuración inicial
archivo_examen <- "2023-Matematicas-11-2-04-Op-D-V2.Rmd"
copias <- 1
semilla <- 12345
set.seed(semilla)
dir_salida <- "salida"
dir_ejercicios <- "ejercicios"

# Nombre del archivo sin la extensión .Rmd
nombre_sin_extension <- sub("\\.Rmd$", "", archivo_examen)
nombre_arch <- paste0(nombre_sin_extension, "_legal2col_")

# Generación de n copias en un solo archivo de salida para PDF
# Usando la nueva función personalizada
exams2pdf_legal2col(archivo_examen,
                   n = copias,
                   name = nombre_arch,
                   encoding = "UTF-8",
                   dir = dir_salida,
                   edir = dir_ejercicios)

# Mensaje de finalización
cat("\nProceso completado. Los archivos se han generado en el directorio:", dir_salida, "\n")
cat("Nombre del archivo:", paste0(nombre_arch, "1.pdf"), "\n")
cat("Semilla utilizada:", semilla, "\n")