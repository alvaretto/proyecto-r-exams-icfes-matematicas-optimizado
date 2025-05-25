---
output:
  html_document: default
  word_document: default
  pdf_document: default
---
# Tutorial Actualizado R-exams Mayo 2025

**Instalación y Configuración de R y R-exams en Manjaro Linux**

Este tutorial documenta el proceso completo para instalar y configurar R, 
RStudio y el paquete R-exams en Manjaro Linux, específicamente para el proyecto 
[RepositorioMatematicasICFES_R_Exams](https://github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado).

## Índice

1. [Requisitos previos](#requisitos-previos)
2. [Instalación de R](#instalación-de-r)
3. [Instalación de RStudio](#instalación-de-rstudio)
4. [Configuración del directorio de biblioteca de usuario](#configuración-del-directorio-de-biblioteca-de-usuario)
5. [Instalación de TinyTeX](#instalación-de-tinytex)
6. [Instalación de paquetes R necesarios](#instalación-de-paquetes-r-necesarios)
7. [Configuración de Python para reticulate](#configuración-de-python-para-reticulate)
8. [Pruebas de compilación](#pruebas-de-compilación)
9. [Solución de problemas comunes](#solución-de-problemas-comunes)
10. [Optimización del entorno](#optimización-del-entorno)
11. [Recursos adicionales](#recursos-adicionales)

## Requisitos previos

Antes de comenzar, asegúrate de tener:

- Manjaro Linux actualizado
- Acceso a internet
- Permisos de administrador (sudo)
- Gestor de paquetes AUR (yay) instalado

Para verificar si tienes yay instalado:

```bash
which yay
```

Si no está instalado, puedes instalarlo con:

```bash
sudo pacman -S --needed git base-devel
git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si
```

## Instalación de R

1. Instala R desde los repositorios oficiales de Manjaro:

```bash
sudo pacman -S r
```

2. Verifica la instalación:

```bash
R --version
```

Deberías ver algo como:

```
R version 4.5.0 (2025-04-11) -- "How About a Twenty-Six"
```

## Instalación de RStudio

1. Instala RStudio desde AUR:

```bash
yay -S rstudio-desktop-bin
```

2. Instala las dependencias necesarias para R y r-exams:

```bash
sudo pacman -S gcc-fortran texlive-core texlive-bin texlive-latexextra texlive-science texlive-pictures pandoc
```

## Configuración del directorio de biblioteca de usuario

Para evitar problemas de permisos y facilitar la gestión de paquetes, es recomendable configurar un directorio de biblioteca de usuario:

1. Crea el directorio:

```bash
mkdir -p ~/R/library
```

2. Crea o edita el archivo `.Renviron` en tu directorio de inicio:

```bash
echo 'R_LIBS_USER=~/R/library' > ~/.Renviron
```

## Instalación de TinyTeX

TinyTeX es una distribución LaTeX ligera diseñada específicamente para R. Es más fácil de instalar y mantener que TeX Live completo.

1. Crea un script para instalar TinyTeX:

```bash
cat > install_tinytex.R << 'EOF'
#!/usr/bin/env Rscript

# Instalar tinytex si no está instalado
if (!require("tinytex")) {
  install.packages("tinytex", repos = "https://cloud.r-project.org")
}

# Instalar TinyTeX con la opción force = TRUE
tinytex::install_tinytex(force = TRUE)
EOF
```

2. Ejecuta el script:

```bash
chmod +x install_tinytex.R
R --vanilla < install_tinytex.R
```

3. Instala paquetes LaTeX adicionales necesarios para r-exams:

```bash
cat > install_latex_packages.R << 'EOF'
#!/usr/bin/env Rscript

# Instalar paquetes LaTeX adicionales
if(require("tinytex") && tinytex::is_tinytex()) {
  tinytex::tlmgr_install(c(
    "amsmath", "amsfonts", "amssymb", "babel", "babel-spanish",
    "booktabs", "colortbl", "enumitem", "environ", "eurosym",
    "fancyhdr", "fancyvrb", "float", "fontspec", "geometry",
    "graphics", "hyperref", "listings", "lm", "mathtools",
    "microtype", "multirow", "parskip", "pgf", "setspace",
    "subfig", "tikz", "tools", "ulem", "url", "xcolor",
    "xetex", "xkeyval", "xunicode", "zapfding"
  ))
}
EOF

chmod +x install_latex_packages.R
R --vanilla < install_latex_packages.R
```

## Instalación de paquetes R necesarios

1. Crea un script para instalar los paquetes R necesarios:

```bash
cat > install_r_exams_packages.R << 'EOF'
#!/usr/bin/env Rscript

# Instalar paquetes necesarios para r-exams
install.packages(c(
  # Paquetes principales
  "exams", "knitr", "rmarkdown",

  # Paquetes para visualización
  "ggplot2", "scales",

  # Paquetes para manipulación de datos
  "dplyr", "tidyr", "reshape2", "data.table",

  # Paquetes para estadísticas
  "testthat", "digest",

  # Paquetes para integración con otros lenguajes
  "reticulate",

  # Paquetes para procesamiento de PDF y LaTeX
  "pdftools", "qpdf",

  # Paquetes adicionales
  "readxl", "datasets", "magick", "webshot"
), repos = "https://cloud.r-project.org")
EOF

chmod +x install_r_exams_packages.R
R --vanilla < install_r_exams_packages.R
```

## Configuración de Python para reticulate

El paquete reticulate permite usar Python desde R, lo cual es útil para ciertos tipos de visualizaciones en r-exams.

1. Crea un script para configurar Python:

```bash
cat > setup_python.R << 'EOF'
#!/usr/bin/env Rscript

# Configurar reticulate para Python
if(require("reticulate")) {
  # Intentar encontrar Python
  python_path <- try(py_config()$python, silent = TRUE)

  if(inherits(python_path, "try-error") || is.null(python_path)) {
    # Intentar usar el Python del sistema
    system_python <- Sys.which("python")
    if(system_python != "") {
      use_python(system_python, required = TRUE)
      cat("Usando Python en:", system_python, "\n")
    } else {
      cat("No se encontró Python en el sistema. Por favor, instala Python manualmente.\n")
    }
  } else {
    cat("Usando Python en:", python_path, "\n")
  }

  # Instalar paquetes Python necesarios
  py_install(c("numpy", "matplotlib", "pandas", "scipy"), pip = TRUE)
}
EOF

chmod +x setup_python.R
R --vanilla < setup_python.R
```

## Pruebas de compilación

Para verificar que todo está configurado correctamente, vamos a probar la compilación de un archivo Rmd con diferentes formatos de salida.

1. Prueba con exams2pdf:

```bash
cat > test_exams2pdf.R << 'EOF'
#!/usr/bin/env Rscript

# Cargar la biblioteca exams
library(exams)

# Compilar el archivo Rmd con exams2pdf
exams2pdf("Lab/12-S2-2025-SEDQ/crecimiento_exponencial_valor_inicial_v1.Rmd",
          n = 1,
          name = "test_exams2pdf")
EOF

chmod +x test_exams2pdf.R
R --vanilla < test_exams2pdf.R
```

2. Prueba con exams2moodle:

```bash
cat > test_exams2moodle.R << 'EOF'
#!/usr/bin/env Rscript

# Cargar la biblioteca exams
library(exams)

# Compilar el archivo Rmd con exams2moodle
exams2moodle("Lab/12-S2-2025-SEDQ/crecimiento_exponencial_valor_inicial_v1.Rmd",
             n = 1,
             name = "test_exams2moodle")
EOF

chmod +x test_exams2moodle.R
R --vanilla < test_exams2moodle.R
```

## Solución de problemas comunes

### Error: LaTeX failed to compile

Si encuentras errores como:

```
Error: LaTeX failed to compile file.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips.
```

Solución:

1. Verifica que TinyTeX esté instalado correctamente:

```r
tinytex::is_tinytex()
```

2. Instala los paquetes LaTeX faltantes:

```r
tinytex::parse_install(file.path("ruta_al_archivo", "archivo.log"))
```

3. Actualiza TinyTeX:

```r
tinytex::tlmgr_update()
```

### Error: no hay paquete llamado 'jsonlite'

Si encuentras errores como:

```
Error en loadNamespace("jsonlite"): no hay paquete llamado 'jsonlite'
```

Solución:

```r
install.packages("jsonlite", repos = "https://cloud.r-project.org")
```

### Error: unable to start device PNG

Si encuentras errores relacionados con dispositivos gráficos:

```
Error in .External2(C_X11, paste0("png::", filename), g$width, g$height,  :
  unable to start device PNG
```

Solución:

```bash
sudo pacman -S libpng cairo
```

## Optimización del entorno

### Configuración de RStudio

1. Abre RStudio
2. Ve a Tools > Global Options
3. En la sección Packages, establece la biblioteca de usuario en `~/R/library`
4. En la sección Sweave, establece el programa LaTeX a TinyTeX

### Script de verificación del entorno

Crea un script para verificar que todo esté configurado correctamente:

```bash
cat > verify_environment.R << 'EOF'
#!/usr/bin/env Rscript

# Verificar R
cat("Versión de R:", R.version.string, "\n")

# Verificar biblioteca de usuario
cat("Rutas de biblioteca de R:\n")
print(.libPaths())

# Verificar TinyTeX
if(require("tinytex")) {
  cat("TinyTeX instalado:", tinytex::is_tinytex(), "\n")
  if(tinytex::is_tinytex()) {
    cat("Versión de TinyTeX:", tinytex::tinytex_root(), "\n")
  }
} else {
  cat("TinyTeX no está instalado\n")
}

# Verificar paquetes instalados
required_packages <- c("exams", "knitr", "rmarkdown", "reticulate")
missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if(length(missing_packages) > 0) {
  cat("Paquetes faltantes:", paste(missing_packages, collapse = ", "), "\n")
} else {
  cat("Todos los paquetes requeridos están instalados\n")
}

# Verificar Python para reticulate
if(require("reticulate")) {
  tryCatch({
    python_config <- py_config()
    cat("Python encontrado en:", python_config$python, "\n")
    cat("Versión de Python:", python_config$version, "\n")

    # Verificar paquetes Python
    py_packages <- c("numpy", "matplotlib", "pandas", "scipy")
    for(pkg in py_packages) {
      has_package <- py_module_available(pkg)
      cat("Paquete Python", pkg, ":", if(has_package) "Instalado" else "No instalado", "\n")
    }
  }, error = function(e) {
    cat("Error al verificar Python:", e$message, "\n")
  })
} else {
  cat("reticulate no está instalado\n")
}
EOF

chmod +x verify_environment.R
R --vanilla < verify_environment.R
```

## Recursos adicionales

- [Documentación oficial de R-exams](http://www.r-exams.org/)
- [Documentación de TinyTeX](https://yihui.org/tinytex/)
- [Guía de RStudio](https://docs.posit.co/ide/user/ide/get-started/)
- [Repositorio del proyecto RepositorioMatematicasICFES_R_Exams](https://github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado)

## Conclusión

Has configurado exitosamente R, RStudio y R-exams en tu sistema Manjaro Linux para el proyecto RepositorioMatematicasICFES_R_Exams. Este entorno te permitirá crear, editar y compilar ejercicios de matemáticas en diferentes formatos de salida, como PDF, HTML y XML para Moodle.

Recuerda mantener actualizado tu sistema y los paquetes R para asegurar la compatibilidad y el rendimiento óptimo.

---

*Última actualización: Mayo 2025*
