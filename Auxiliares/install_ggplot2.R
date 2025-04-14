# Script para instalar ggplot2 y sus dependencias

# Verificar si ggplot2 ya está instalado
if (!("ggplot2" %in% installed.packages()[,"Package"])) {
  cat("Instalando ggplot2 y sus dependencias...\n")
  
  # Instalar ggplot2
  install.packages("ggplot2", dependencies = TRUE)
  
  # Verificar si se instaló correctamente
  if ("ggplot2" %in% installed.packages()[,"Package"]) {
    cat("\nggplot2 se ha instalado correctamente.\n")
  } else {
    cat("\nNo se pudo instalar ggplot2. Por favor, intenta instalarlo manualmente.\n")
  }
} else {
  cat("El paquete ggplot2 ya está instalado.\n")
}

# Instalar paquete ggforce para geom_circle
if (!("ggforce" %in% installed.packages()[,"Package"])) {
  cat("Instalando ggforce para geom_circle...\n")
  
  # Instalar ggforce
  install.packages("ggforce", dependencies = TRUE)
  
  # Verificar si se instaló correctamente
  if ("ggforce" %in% installed.packages()[,"Package"]) {
    cat("\nggforce se ha instalado correctamente.\n")
  } else {
    cat("\nNo se pudo instalar ggforce. Por favor, intenta instalarlo manualmente.\n")
  }
} else {
  cat("El paquete ggforce ya está instalado.\n")
}
