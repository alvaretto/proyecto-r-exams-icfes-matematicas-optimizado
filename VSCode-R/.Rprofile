# Configuración R para VSCode
.libPaths(c("~/R/library", .libPaths()))

options(
  repos = c(CRAN = "https://cran.r-project.org"),
  scipen = 999,
  digits = 4,
  width = 120,
  warn = 1,
  OutDec = "."
)

options(encoding = "UTF-8")

load_common_packages <- function() {
  packages <- c("exams", "reticulate", "knitr", "rmarkdown")
  for (pkg in packages) {
    if (require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat("✓", pkg, "cargado\n")
    } else {
      cat("✗", pkg, "no disponible\n")
    }
  }
}

if (require("reticulate", quietly = TRUE)) {
  use_python("/usr/bin/python3", required = FALSE)
}

cat("R configurado para VSCode\n")
cat("Biblioteca personal:", .libPaths()[1], "\n")
cat("Usa load_common_packages() para cargar paquetes comunes\n")
