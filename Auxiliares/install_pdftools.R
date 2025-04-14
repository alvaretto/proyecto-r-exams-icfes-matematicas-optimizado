# Script para instalar el paquete pdftools y sus dependencias

# Verificar si pdftools ya está instalado
if (!("pdftools" %in% installed.packages()[,"Package"])) {
  cat("Instalando el paquete pdftools...\n")
  
  # En sistemas Linux como Manjaro, pdftools requiere algunas dependencias del sistema
  cat("Nota: pdftools requiere las bibliotecas poppler-cpp, libpng y libtiff.\n")
  cat("Si la instalación falla, ejecuta en la terminal:\n")
  cat("sudo pacman -S poppler-glib libpng libtiff\n\n")
  
  # Instalar pdftools
  install.packages("pdftools", dependencies = TRUE)
  
  # Verificar si se instaló correctamente
  if ("pdftools" %in% installed.packages()[,"Package"]) {
    cat("\npdftools se ha instalado correctamente.\n")
  } else {
    cat("\nNo se pudo instalar pdftools. Por favor, instala las dependencias del sistema y vuelve a intentarlo.\n")
  }
} else {
  cat("El paquete pdftools ya está instalado.\n")
}

# Verificar otras dependencias comunes que podrían ser necesarias
paquetes_adicionales <- c(
  "qpdf",       # Para manipulación de PDF
  "tinytex",    # Para integración con LaTeX
  "magick",     # Para procesamiento de imágenes
  "webshot"     # Para capturas de pantalla de HTML
)

paquetes_faltantes <- paquetes_adicionales[!(paquetes_adicionales %in% installed.packages()[,"Package"])]

if (length(paquetes_faltantes) > 0) {
  cat("\nInstalando paquetes adicionales que podrían ser necesarios:\n")
  cat(paste(" -", paquetes_faltantes), sep = "\n")
  
  # Instalar paquetes adicionales
  install.packages(paquetes_faltantes, dependencies = TRUE)
  
  # Verificar instalación
  paquetes_aun_faltantes <- paquetes_faltantes[!(paquetes_faltantes %in% installed.packages()[,"Package"])]
  
  if (length(paquetes_aun_faltantes) > 0) {
    cat("\nLos siguientes paquetes no se pudieron instalar:\n")
    cat(paste(" -", paquetes_aun_faltantes), sep = "\n")
  } else {
    cat("\nTodos los paquetes adicionales se instalaron correctamente.\n")
  }
} else {
  cat("\nTodos los paquetes adicionales ya están instalados.\n")
}
