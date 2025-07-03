#!/usr/bin/env Rscript

# Instalar TinyTeX si no está disponible TeX Live
if (!tinytex::is_tinytex()) {
  cat("Instalando TinyTeX...\n")
  tinytex::install_tinytex(force = TRUE)
  
  # Instalar paquetes LaTeX adicionales necesarios
  tinytex::tlmgr_install(c(
    "amsmath", "amsfonts", "amssymb", "babel", "babel-spanish",
    "booktabs", "colortbl", "enumitem", "environ", "eurosym",
    "fancyhdr", "fancyvrb", "float", "fontspec", "geometry",
    "graphics", "hyperref", "listings", "mathtools",
    "microtype", "multirow", "parskip", "pgf", "setspace",
    "subfig", "tikz", "tools", "ulem", "url", "xcolor",
    "xkeyval", "zapfding", "framed", "mdframed"
  ))
}
