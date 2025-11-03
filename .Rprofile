# Configuración R para VSCode
# Este archivo se carga automáticamente al iniciar R

# Configurar biblioteca personal
.libPaths(c("~/R/library", .libPaths()))

# Configurar opciones de R
options(
  repos = c(CRAN = "https://cran.r-project.org"),
  scipen = 999,
  digits = 4,
  width = 120,
  warn = 1,
  OutDec = "."
)

# Configurar encoding
options(encoding = "UTF-8")

# Función para cargar paquetes comunes
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

# Configurar Python para reticulate - CONFIGURACIÓN ROBUSTA
# CRÍTICO: Desactivar el uso automático de entornos virtuales con uv
# Esto previene que reticulate 1.44.0+ cree entornos virtuales automáticamente
Sys.setenv(RETICULATE_PYTHON = "/usr/bin/python3")
Sys.setenv(RETICULATE_PYTHON_FALLBACK = "/usr/bin/python3")
Sys.setenv(RETICULATE_USE_MANAGED_VENV = "no")

# Cargar reticulate y forzar configuración
if (require("reticulate", quietly = TRUE)) {
  # Usar required = TRUE para forzar el uso de este Python
  use_python("/usr/bin/python3", required = TRUE)

  # Inicializar Python inmediatamente para evitar que reticulate use otro intérprete
  tryCatch({
    py_config()
  }, error = function(e) {
    # Si falla, intentar de nuevo
    use_python("/usr/bin/python3", required = TRUE, force = TRUE)
  })
}

# Mensaje de bienvenida
cat("R configurado para VSCode\n")
cat("Biblioteca personal:", .libPaths()[1], "\n")
cat("Usa load_common_packages() para cargar paquetes comunes\n")
