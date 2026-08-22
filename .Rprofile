# Configuración R para VSCode
# Este archivo se carga automáticamente al iniciar R

# Biblioteca personal: anteponerla SOLO si existe.
# .libPaths() descarta en silencio las rutas inexistentes, asi que en un runner
# de CI (donde ~/R/library no existe) esta linea dejaba la biblioteca del
# sistema en primer lugar y pisaba el R_LIBS_USER escribible que configura
# r-lib/actions/setup-r -> install.packages() moria con "lib is not writable".
local({
  lib_personal <- path.expand("~/R/library")
  if (dir.exists(lib_personal)) .libPaths(c(lib_personal, .libPaths()))
})

# repos: respetar el que ya venga configurado por el entorno.
# En CI, r-lib/actions/setup-r apunta a Posit Package Manager, que sirve
# binarios precompilados para el runner. Forzar CRAN aqui obligaba a compilar
# desde fuente y la instalacion de 'fs' moria con "fatal error: uv.h: No such
# file or directory", arrastrando a pkgload, rmarkdown, exams y testthat.
local({
  cran <- getOption("repos")[["CRAN"]]
  if (is.null(cran) || !nzchar(cran) || identical(cran, "@CRAN@")) {
    options(repos = c(CRAN = "https://cran.r-project.org"))
  }
})

# Configurar opciones de R
options(
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

# Utilidades locales de la maquina de desarrollo (mcptools, rstudio-open).
# Se cargan SOLO si existen: en CI (GitHub Actions) no estan presentes y su
# ausencia no debe abortar la sesion de R con "Execution halted".
for (.local_helper in c("~/.R/mcp-session-autoconnect.R",
                        "~/.R/open-queue-watcher.R")) {
  if (file.exists(path.expand(.local_helper))) source(.local_helper)
}
rm(.local_helper)

# Mensaje de bienvenida
cat("R configurado para VSCode\n")
cat("Biblioteca personal:", .libPaths()[1], "\n")
cat("Usa load_common_packages() para cargar paquetes comunes\n")
