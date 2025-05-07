# Archivo: generar_cilindro.R
# Script para pre-renderizar la imagen del cilindro

library(exams)

# Asegurar que tenemos las dependencias necesarias
if (!requireNamespace("magick", quietly = TRUE)) {
  install.packages("magick")
}

# Configuración para asegurar el punto decimal
Sys.setlocale(category = "LC_NUMERIC", locale = "C")
options(OutDec = ".")

# Establecer semilla aleatoria para reproducibilidad
set.seed(12345) # Puedes usar cualquier número fijo

# Definir las medidas del cilindro (usando los mismos valores que en tu ejercicio)
r_int <- 2.5     # Radio interno en cm
r_ext <- 4.0     # Radio externo en cm
d_ext <- 2 * r_ext  # Diámetro externo
altura <- 10.0   # Altura en cm
unidad <- "cm"   # Unidad de medida

# Código TikZ para generar el cilindro
tikz_code <- paste0('
\\begin{tikzpicture}[
line cap=round,
line join=round,
>=stealth,
thick,
scale=1.1
]

% Cargar solo bibliotecas esenciales
\\usetikzlibrary{calc}

% Definimos variables
\\pgfmathsetmacro{\\altura}{', altura, '}
\\pgfmathsetmacro{\\radiointerno}{', r_int, '}
\\pgfmathsetmacro{\\radioexterno}{', r_ext, '}
\\pgfmathsetmacro{\\diametroexterno}{', d_ext, '}
\\pgfmathsetmacro{\\grosor}{', r_ext - r_int, '}

% Radios verticales para la perspectiva
\\pgfmathsetmacro{\\vradioexternobot}{0.45*\\radioexterno}
\\pgfmathsetmacro{\\vradiointernobot}{0.45*\\radiointerno}
\\pgfmathsetmacro{\\vradioexternotop}{0.6*\\radioexterno}
\\pgfmathsetmacro{\\vradiointernotop}{0.6*\\radiointerno}

% Definir colores personalizados
\\definecolor{cilindroColor}{RGB}{60, 120, 180}
\\definecolor{lineaOculta}{RGB}{150, 150, 150}
\\definecolor{etiquetaColor}{RGB}{50, 50, 50}

% Coordenadas de los centros de las bases
\\coordinate (centroinferior) at (0,0);
\\coordinate (centrosuperior) at (0,\\altura);

% Puntos clave en las bases
% Base inferior
\\coordinate (R0b) at (\\radioexterno, 0);
\\coordinate (-R0b) at (-\\radioexterno, 0);
\\coordinate (r0b) at (\\radiointerno, 0);
\\coordinate (-r0b) at (-\\radiointerno, 0);
% Base superior
\\coordinate (R0t) at (\\radioexterno, \\altura);
\\coordinate (-R0t) at (-\\radioexterno, \\altura);
\\coordinate (r0t) at (\\radiointerno, \\altura);
\\coordinate (-r0t) at (-\\radiointerno, \\altura);

% Dibujo de las partes ocultas (dashed)
\\draw[dashed, lineaOculta, line width=0.8pt] (R0b) arc (0:180:{\\radioexterno} and {\\vradioexternobot});
\\draw[dashed, lineaOculta, line width=0.8pt] (r0b) arc (0:180:{\\radiointerno} and {\\vradiointernobot});
\\draw[dashed, lineaOculta, line width=0.8pt] (-r0b) -- (-r0t);
\\draw[dashed, lineaOculta, line width=0.8pt] (r0b) -- (r0t);

% Dibujo de las partes visibles
\\draw[cilindroColor, line width=1.2pt] (-R0b) -- (-R0t);
\\draw[cilindroColor, line width=1.2pt] (R0b) -- (R0t);
\\draw[cilindroColor, line width=1.2pt] (R0b) arc (0:-180:{\\radioexterno} and {\\vradioexternobot});
\\draw[cilindroColor, line width=1.2pt] (r0b) arc (0:-180:{\\radiointerno} and {\\vradiointernobot});
\\draw[cilindroColor, line width=1.2pt] (centrosuperior) ellipse ({\\radioexterno} and {\\vradioexternotop});
\\draw[cilindroColor, line width=1.2pt] (centrosuperior) ellipse ({\\radiointerno} and {\\vradiointernotop});

% Sombreado simple
\\fill[cilindroColor!10] (R0b) arc (0:-180:{\\radioexterno} and {\\vradioexternobot}) -- (-R0t) arc (180:0:{\\radioexterno} and {\\vradioexternotop}) -- cycle;

% Etiquetas y dimensiones
\\tikzset{
  etiqueta/.style={font=\\sffamily\\bfseries, text=etiquetaColor, align=center},
  linea_medicion/.style={|<->|, line width=0.9pt, color=etiquetaColor},
  linea_indicadora/.style={->, rounded corners=8pt, line width=0.9pt, color=etiquetaColor!80},
  linea_radio/.style={->, line width=0.9pt, color=cilindroColor!80}
}

% Altura
\\coordinate (alturaInicio) at ($(R0b) + (1, 0)$);
\\coordinate (alturaFin) at ($(R0t) + (1, 0)$);
\\draw [linea_medicion] (alturaInicio) -- (alturaFin);
\\node[etiqueta, right=0.2cm] at ($(alturaInicio)!0.5!(alturaFin)$) {Altura};

% Radio interno
\\draw [linea_radio] (centrosuperior) -- (r0t);
\\node[etiqueta, above right=0.1cm] at ($(centrosuperior)!0.65!(r0t)$) {Radio interno = ', r_int, ' ', unidad, '};

% Grosor
\\draw [linea_medicion] (-r0t) -- (-R0t);
\\node[etiqueta, above=0.1cm] at ($(-r0t)!0.5!(-R0t)$) {Grosor = ', r_ext - r_int, ' ', unidad, '};

% Diámetro externo
\\coordinate (diamExtIzq) at ($(-R0t) + (0, 1)$);
\\coordinate (diamExtDer) at ($(R0t) + (0, 1)$);
\\draw [linea_medicion] (diamExtIzq) -- (diamExtDer);
\\node[etiqueta, above=0.2cm] at ($(diamExtIzq)!0.5!(diamExtDer)$) {Diámetro externo = ', d_ext, ' ', unidad, '};

% Radio externo
\\draw [linea_radio] (centroinferior) -- (R0b);
\\node[etiqueta, below right=0.1cm] at ($(centroinferior)!0.7!(R0b)$) {Radio externo = ', r_ext, ' ', unidad, '};

\\end{tikzpicture}
')

# Generar la imagen en formato PNG
# Crear un directorio específico para la salida
output_dir <- "."
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Generar la imagen con ruta explícita
imagen_path <- tex2image(tikz_code, name = "cilindro_hueco", format = "png",
          packages = c("tikz"), width = "10cm", density = 300,
          dir = output_dir,
          header = "\\usetikzlibrary{calc}")

# En sistemas donde no funcione tex2image, una alternativa es usar tikz2pdf y luego convertir:
# tikz2pdf(tikz_code, name = "cilindro_hueco",
#          packages = c("tikz"), width = "10cm",
#          header = "\\usetikzlibrary{calc}")
# system("convert -density 300 cilindro_hueco.pdf cilindro_hueco.png")

cat("Imagen del cilindro generada en:", imagen_path, "\n")
cat("Directorio de trabajo actual:", getwd(), "\n")
