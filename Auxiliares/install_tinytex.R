#!/usr/bin/env Rscript

# Instalar tinytex si no está instalado
if (!require("tinytex")) {
  install.packages("tinytex", repos = "https://cloud.r-project.org")
}

# Instalar TinyTeX con la opción force = TRUE
tinytex::install_tinytex(force = TRUE)
