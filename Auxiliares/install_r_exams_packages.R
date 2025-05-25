#!/usr/bin/env Rscript

# Instalar paquetes necesarios para r-exams
install.packages(c("exams", "knitr", "rmarkdown", "reticulate", "digest", "glue"),
                 repos = "https://cloud.r-project.org")
