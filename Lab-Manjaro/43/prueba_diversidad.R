library(testthat)
library(knitr)

# Definir archivo a probar
rmd_file <- "probabilidad_extraccion_bolas_v1.Rmd"

# Función para extraer y ejecutar código R del Rmd
run_rmd_code <- function(file_path) {
  # Crear archivo temporal para el código R
  temp_r_file <- tempfile(fileext = ".R")
  on.exit(unlink(temp_r_file, force = TRUE), add = TRUE)
  
  # Extraer código R
  suppressMessages(
    knitr::purl(file_path, output = temp_r_file, documentation = 0, quiet = TRUE)
  )
  
  # Ejecutar código en un nuevo entorno
  env <- new.env()
  source(temp_r_file, local = env)
  
  return(env)
}

# Prueba de diversidad completa
cat("\n=== Prueba de diversidad para", rmd_file, "===\n")

# Número de versiones a probar
n_versions <- 310  # Un poco más de 300 para asegurar

# Vector para almacenar huellas digitales
huellas <- character(n_versions)

# Generar versiones
cat("Generando", n_versions, "versiones...\n")
pb <- txtProgressBar(min = 0, max = n_versions, style = 3)

for (i in 1:n_versions) {
  # Establecer semilla aleatoria
  set.seed(i)
  
  # Ejecutar código
  env <- run_rmd_code(rmd_file)
  
  # Crear huella digital
  huella <- paste(
    env$total_bolas,
    env$bolas_principal,
    env$bolas_secundario,
    env$bolas_extraer,
    env$min_bolas_ganar,
    env$color_principal,
    env$color_secundario,
    env$contexto,
    env$tipo_concurso,
    env$probabilidad_correcta,
    paste(env$opciones_ordenadas, collapse = ","),
    env$indice_correcto,
    collapse = "|"
  )
  
  huellas[i] <- huella
  
  # Actualizar barra de progreso
  setTxtProgressBar(pb, i)
}

close(pb)

# Contar versiones únicas
n_unicas <- length(unique(huellas))
cat("\nNúmero de versiones únicas generadas:", n_unicas, "de", n_versions, "\n")

# Verificar que hay al menos 300 versiones únicas
if (n_unicas >= 300) {
  cat("✅ ÉXITO: Se generaron al menos 300 versiones únicas.\n")
} else {
  cat("❌ FALLO: Solo se generaron", n_unicas, "versiones únicas. Se requieren al menos 300.\n")
}

# Análisis adicional de diversidad
cat("\n=== Análisis de diversidad ===\n")

# Extraer componentes individuales para análisis
componentes <- list()
for (i in 1:n_versions) {
  partes <- strsplit(huellas[i], "\\|")[[1]]
  
  if (i == 1) {
    # Inicializar listas para cada componente
    for (j in 1:length(partes)) {
      componentes[[j]] <- character(n_versions)
    }
  }
  
  # Almacenar cada componente
  for (j in 1:length(partes)) {
    componentes[[j]][i] <- partes[j]
  }
}

# Nombres de los componentes
nombres_componentes <- c(
  "total_bolas", "bolas_principal", "bolas_secundario", 
  "bolas_extraer", "min_bolas_ganar", "color_principal", 
  "color_secundario", "contexto", "tipo_concurso", 
  "probabilidad_correcta", "opciones", "indice_correcto"
)

# Analizar diversidad de cada componente
cat("Diversidad por componente:\n")
for (j in 1:length(componentes)) {
  if (j <= length(nombres_componentes)) {
    nombre <- nombres_componentes[j]
  } else {
    nombre <- paste("Componente", j)
  }
  
  n_unicas_comp <- length(unique(componentes[[j]]))
  cat(sprintf("- %-20s: %4d valores únicos\n", nombre, n_unicas_comp))
}

cat("\n=== Fin de la prueba de diversidad ===\n")
