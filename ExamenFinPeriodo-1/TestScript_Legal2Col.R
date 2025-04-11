library(exams)

# Definición del archivo de examen y configuración inicial
archivo_examen <- "2023-Matematicas-11-2-04-Op-D-V2.Rmd"
copias <- 1
numpreg <- 1
semilla <- 12345
set.seed(semilla)
dir_salida <- "salida"
dir_ejercicios <- "ejercicios"

# Nombre del archivo sin la extensión .Rmd
nombre_sin_extension <- sub("\\.Rmd$", "", archivo_examen)
nombre_arch <- paste0(nombre_sin_extension, "_legal2col_")

# Generación de n copias en un solo archivo de salida para PDF
# Usando la nueva plantilla para tamaño legal y doble columna
exams2pdf(rep(archivo_examen, numpreg),
          n = copias,
          name = nombre_arch,
          encoding = "UTF-8",
          template = "solpcielo_legal_2col",
          dir = dir_salida,
          edir = dir_ejercicios)

# Mensaje de finalización
cat("\nPrueba completada. El archivo se ha generado en el directorio:", dir_salida, "\n")
cat("Nombre del archivo:", paste0(nombre_arch, "1.pdf"), "\n")
cat("Semilla utilizada:", semilla, "\n")
cat("\nNOTA: Se ha redefinido el entorno 'longtable' para usar 'tabular' en su lugar,\n")
cat("lo que es compatible con el formato de doble columna.\n")
