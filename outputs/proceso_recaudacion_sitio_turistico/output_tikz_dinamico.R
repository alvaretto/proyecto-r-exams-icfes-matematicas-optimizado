# ============================================================
# Código TikZ Dinámico para Diagrama de Flujo - Sitio Turístico
# Compatible con R-exams usando include_tikz()
# ============================================================

# Función para generar datos aleatorios del ejercicio
generar_datos_proceso <- function() {
  # Precios de entrada (aleatorios dentro de rangos realistas)
  precio_reserva <- sample(seq(20, 30, by = 0.5), 1)
  precio_sin_reserva <- sample(seq(15, 25, by = 0.5), 1)

  # Asegurar que precio con reserva > precio sin reserva
  while (precio_reserva <= precio_sin_reserva) {
    precio_reserva <- sample(seq(20, 30, by = 0.5), 1)
  }

  # Formatear con coma decimal (español)
  precio_reserva_fmt <- format(precio_reserva, decimal.mark = ",", nsmall = 1)
  precio_sin_reserva_fmt <- format(precio_sin_reserva, decimal.mark = ",", nsmall = 1)

  # Limpiar espacios
  precio_reserva_fmt <- gsub(" ", "", precio_reserva_fmt)
  precio_sin_reserva_fmt <- gsub(" ", "", precio_sin_reserva_fmt)

  list(
    precio_reserva = precio_reserva,
    precio_sin_reserva = precio_sin_reserva,
    precio_reserva_fmt = precio_reserva_fmt,
    precio_sin_reserva_fmt = precio_sin_reserva_fmt
  )
}

# Función para generar código TikZ dinámico
generar_tikz_diagrama_flujo <- function(datos) {
  tikz_code <- paste0(
'\\begin{tikzpicture}[
    proceso/.style={rectangle, rounded corners=2pt, draw=cyan!50!blue!60, fill=cyan!5!blue!3, text width=3.6cm, minimum height=1.3cm, align=center, font=\\small},
    resultado/.style={rectangle, rounded corners=2pt, draw=cyan!50!blue!60, fill=cyan!5!blue!3, text width=2cm, minimum height=1.3cm, align=center, font=\\small},
    paso/.style={circle, fill=orange, text=white, font=\\bfseries\\small, minimum size=0.6cm, inner sep=0pt},
    pasopeq/.style={circle, fill=orange, text=white, font=\\bfseries\\fontsize{6}{6}\\selectfont, minimum size=0.35cm, inner sep=0pt},
    flecha/.style={->, >=Stealth, line width=0.8pt, cyan!50!blue!70}
]

% Fila superior (Con reserva)
\\node[paso] (p1a) {1};
\\node[proceso, right=0.25cm of p1a] (caja1a) {Sumar la cantidad de personas que entraron con reserva durante la semana.};
\\node[paso, right=1cm of caja1a] (p2a) {2};
\\node[proceso, right=0.25cm of p2a, text width=4cm] (caja2a) {Multiplicar la cantidad obtenida en el paso \\raisebox{-0.08em}{\\tikz[baseline=-0.5ex]{\\node[pasopeq]{1};}} por ', datos$precio_reserva_fmt, '.};

% Fila inferior (Sin reserva)
\\node[proceso, below=1cm of caja1a] (caja1b) {Sumar la cantidad de personas que entraron sin reserva durante la semana.};
\\node[paso, left=0.25cm of caja1b] (p1b) {1};
\\node[proceso, right=1cm of caja1b, text width=4cm] (caja2b) {Multiplicar la cantidad obtenida en el paso \\raisebox{-0.08em}{\\tikz[baseline=-0.5ex]{\\node[pasopeq]{1};}} por ', datos$precio_sin_reserva_fmt, '.};
\\node[paso, left=0.25cm of caja2b] (p2b) {2};

% Paso 3 (Resultado)
\\coordinate (medio) at ($(caja2a.east)!0.5!(caja2b.east)$);
\\node[paso, right=1.8cm of medio] (p3) {3};
\\node[resultado, right=0.25cm of p3] (caja3) {Comparar los resultados};

% Flechas
\\draw[flecha] (caja1a.east) -- (p2a.west);
\\draw[flecha] (caja1b.east) -- (p2b.west);
\\draw[flecha] (caja2a.east) -- ++(0.5,0) |- (caja3.west);
\\draw[flecha] (caja2b.east) -- ++(0.5,0) |- (caja3.west);

\\end{tikzpicture}'
  )

  return(tikz_code)
}

# ============================================================
# Ejemplo de uso con R-exams
# ============================================================

# Generar datos
datos <- generar_datos_proceso()

# Generar código TikZ
tikz_code <- generar_tikz_diagrama_flujo(datos)

# Para usar con include_tikz() en R-exams:
# include_tikz(tikz_code,
#              name = "diagrama_flujo",
#              markup = "markdown",
#              library = c("positioning", "arrows.meta", "calc"),
#              width = "0.9\\textwidth")

# Mostrar datos generados
cat("Precio con reserva:", datos$precio_reserva_fmt, "\n")
cat("Precio sin reserva:", datos$precio_sin_reserva_fmt, "\n")
cat("\nCódigo TikZ generado correctamente.\n")
