# =============================================================================
# Familias de Soluciones Reutilizables para .Rmd — Librería de helpers canónicos
# Regla #21: .claude/rules/familias-soluciones-rmd.md
# =============================================================================
# Fuente de verdad de la versión canónica de cada helper. Por la auto-contención
# de los .Rmd bajo R-exams (edir temporal), se RECOMIENDA COPIAR el helper que se
# necesite dentro del chunk data_generation del .Rmd, no source() por ruta frágil.
#
# Verificado en rango_colesterol_..._cloze_v1 (2026-06-05): HTML/PDF/DOCX/Moodle.
# =============================================================================

# ---- Familia 5: muestreo seguro (evita la trampa sample(escalar, k)) --------
# sample(x, k) reinterpreta x como 1:x cuando length(x)==1. Estas funciones lo evitan.
pick_int <- function(a, b) {
  # Entero uniforme en [a, b]; devuelve a si a >= b (soporte de un solo valor).
  if (a >= b) a else sample(a:b, 1L)
}

safe_sample <- function(x, size, replace = TRUE) {
  # Muestra de tamaño 'size'; si x tiene un solo elemento, repite ese valor.
  # Guarda length-0: NULL o vector vacío (típicamente un campo inexistente en una
  # lista, p.ej. un typo de nombre de campo) hacían caer sample() en
  # sample.int(0, ...) -> "primer argumento inválido", error críptico e intermitente.
  # Aquí falla de inmediato señalando la causa real.
  if (length(x) == 0L)
    stop("safe_sample(): el vector de entrada está vacío o es NULL ",
         "(¿campo inexistente en una lista o nombre de campo mal escrito?).")
  if (length(x) == 1L) rep(x, size) else sample(x, size, replace = replace)
}

# ---- Familia 1: construir un vector con un RANGO objetivo garantizado -------
# Sustituye bucles repeat/while que resamplean hasta cumplir una condición que
# puede ser imposible (causa de cuelgues — Error 22).
construir_valores_con_rango <- function(rango_objetivo, n, lo, hi) {
  # Devuelve n enteros en [lo, hi] cuyo (max - min) == rango_objetivo, mezclados.
  stopifnot(rango_objetivo >= 0, rango_objetivo <= (hi - lo), n >= 2)
  base    <- pick_int(lo, hi - rango_objetivo)
  relleno <- if (rango_objetivo == 0) rep(base, n - 2)
             else sample(base:(base + rango_objetivo), n - 2, replace = TRUE)
  v <- sample(c(base, base + rango_objetivo, relleno))
  stopifnot(max(v) - min(v) == rango_objetivo)
  v
}

# ---- Familia 2: tabla RESPONSIVA cross-formato ------------------------------
# PDF/NOPS: tabla Markdown (longtable; requiere guard \newcounter{none}, regla #20).
# HTML/Moodle: fenced div con scroll horizontal. DOCX: tabla nativa de Word.
# bold_rows: índices de filas a resaltar (negrita Markdown nativa **x**).
tabla_responsiva <- function(df, align = NULL, bold_rows = integer(0)) {
  df2 <- data.frame(lapply(df, as.character), stringsAsFactors = FALSE, check.names = FALSE)
  if (length(bold_rows)) for (r in bold_rows)
    df2[r, ] <- lapply(df2[r, , drop = FALSE], function(x) paste0("**", x, "**"))
  md <- paste(knitr::kable(df2, format = "markdown", align = align), collapse = "\n")
  if (knitr::is_latex_output()) {
    md
  } else {
    paste0("::: {style=\"overflow-x:auto; -webkit-overflow-scrolling:touch; max-width:100%; margin:0.6em 0;\"}\n\n",
           md, "\n\n:::\n")
  }
}

# ---- Familia 3: ecuación display RESPONSIVA ---------------------------------
# 'tex' es el contenido LaTeX SIN los delimitadores $$. En HTML/Moodle se envuelve
# en un fenced div con scroll (MathJax renderiza dentro). OJO: el espacio en "::: {".
eq_display <- function(tex) {
  if (knitr::is_latex_output()) {
    paste0("$$", tex, "$$")
  } else {
    paste0("::: {style=\"overflow-x:auto; -webkit-overflow-scrolling:touch; max-width:100%;\"}\n\n",
           "$$", tex, "$$\n\n:::\n")
  }
}

# ---- Guard regla #20 (para copiar al inicio de la sección Question) ---------
# ```{=latex}
# \makeatletter\@ifundefined{c@none}{\newcounter{none}}{}\makeatother
# ```
