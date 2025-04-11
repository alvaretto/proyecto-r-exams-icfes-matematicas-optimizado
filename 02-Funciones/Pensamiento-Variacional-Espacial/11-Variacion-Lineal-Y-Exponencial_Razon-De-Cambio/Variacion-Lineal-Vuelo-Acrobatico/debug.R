library(reticulate)

# Cargar el archivo Rmd como texto
rmd_file <- readLines("vuelo_acrobatico_mejorado_A.Rmd")

# Ejecutar el código R para generar los chunks de Python
source("vuelo_acrobatico_mejorado_A.Rmd", echo = TRUE)

# Imprimir los chunks de Python generados
for (i in 1:4) {
  cat("\n\nChunk", i, ":\n")
  cat(py_chunks[i])
  cat("\n")
}
