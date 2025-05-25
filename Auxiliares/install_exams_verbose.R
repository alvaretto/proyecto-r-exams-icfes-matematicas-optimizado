#!/usr/bin/env Rscript

# Configurar directorio de biblioteca de usuario
dir.create("~/R/library", recursive = TRUE, showWarnings = FALSE)
.libPaths(c("~/R/library", .libPaths()))

# Mostrar rutas de biblioteca
cat("Rutas de biblioteca de R:\n")
print(.libPaths())

# Instalar el paquete exams con opciones detalladas
install.packages("exams", 
                 repos = "https://cloud.r-project.org",
                 lib = "~/R/library",
                 dependencies = TRUE,
                 verbose = TRUE)
