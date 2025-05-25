#!/usr/bin/env Rscript

# Instalar tinytex si no está instalado
if (!require("tinytex")) {
  install.packages("tinytex", repos = "https://cloud.r-project.org")
}

# Instalar TinyTeX con la opción force = TRUE
tinytex::install_tinytex(force = TRUE)

# Instalar paquetes LaTeX adicionales
tinytex::tlmgr_install(c(
  "amsmath", "amsfonts", "amssymb", "babel", "babel-spanish", 
  "booktabs", "colortbl", "enumitem", "environ", "eurosym", 
  "fancyhdr", "fancyvrb", "float", "fontspec", "geometry", 
  "graphics", "hyperref", "listings", "lm", "mathtools", 
  "microtype", "multirow", "parskip", "pgf", "setspace", 
  "subfig", "tikz", "tools", "ulem", "url", "xcolor", 
  "xetex", "xkeyval", "xunicode", "zapfding"
))
