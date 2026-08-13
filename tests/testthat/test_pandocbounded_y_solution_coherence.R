# test_pandocbounded_y_solution_coherence.R
# ============================================================
# Suite de regresión para Errores 16 y 17 (sesión 2026-05-03).
#
# Error 16: `\pandocbounded` undefined al renderizar PDF con
#           imágenes Markdown sin atributo `{width=...}`.
#
# Error 17: Inconsistencia Solution↔Answerlist por
#           `exshuffle: TRUE` con referencia explícita a letra.
#
# Esta suite NO renderiza ejercicios (eso lo hacen otros tests);
# hace análisis estático del .Rmd para detectar los antipatrones.
# ============================================================

library(testthat)

# Localizar la raíz del repo de forma robusta (independiente de cwd)
.repo_root <- tryCatch({
  trimws(system("git rev-parse --show-toplevel", intern = TRUE))
}, error = function(e) getwd())

# Directorios a auditar
.dirs_auditar <- c(
  file.path(.repo_root, "A-Produccion", "01-En-PreDesarrollo"),
  file.path(.repo_root, "A-Produccion", "02-En-Desarrollo"),
  file.path(.repo_root, "A-Produccion", "03-En-Produccion")
)
.dirs_auditar <- .dirs_auditar[dir.exists(.dirs_auditar)]

# Excluir:
#  - Ejemplos funcionales inmutables (snapshots históricos)
#  - Lab-Manjaro/ (experimentos legacy pre-workflow, no se renderizan en pipeline)
#  - Cualquier .Rmd marcado como _legacy_ en el nombre
.excluir_glob <- c(
  "Ejemplos-Funcionales-Rmd",
  "Lab-Manjaro",
  "_legacy_"
)

# Lista de .Rmd legacy conocidos en 02/03 con bug histórico que aún no
# pasaron por re-fix. Reportar como WARNING informativo, no como FAIL.
# Si un .Rmd nuevo cae en esta lista, eliminarlo de aquí Y arreglarlo.
.legacy_known_pandocbounded <- c(
  "Variacion-Lineal-Vuelo-Acrobatico/vuelo_acrobatico_mejorado_A.Rmd",
  "Variacion-Lineal-Vuelo-Acrobatico/vuelo_acrobatico_mejorado_B.Rmd",
  "Variacion-Lineal-Vuelo-Acrobatico/vuelo_acrobatico_mejorado_C.Rmd",
  "Accidentalidad_Vial_Genero-16/accidentalidad_vial_genero_aleatorio_interpretacion_representacion_n2_v1.Rmd",
  "Accidentalidad_Vial_Genero-16/accidentalidad_vial_genero_aleatorio_interpretacion_representacion_n2_v2.Rmd"
)

listar_rmd <- function() {
  todos <- character(0)
  for (d in .dirs_auditar) {
    encontrados <- list.files(d, pattern = "\\.Rmd$",
                              recursive = TRUE, full.names = TRUE)
    todos <- c(todos, encontrados)
  }
  # Filtrar exclusiones
  for (patron in .excluir_glob) {
    todos <- todos[!grepl(patron, todos, fixed = TRUE)]
  }
  todos
}

# ---------- Detección Error 16: Markdown sin width ----------

detectar_markdown_sin_width <- function(ruta_rmd) {
  # Lee el archivo y reporta líneas con `![](*.png)` sin `{width=...}`
  # Soporta variantes: ![alt](file.png), ![](file.png), comillas opcionales.
  if (!file.exists(ruta_rmd)) return(character(0))
  lineas <- readLines(ruta_rmd, warn = FALSE)

  # Patrón: !\[[^]]*\]\([^)]+\.(png|jpg|jpeg|svg|pdf)\)
  # Sin atributo width siguiente: NO seguido inmediatamente por `{width=`
  patron_imagen <- "!\\[[^\\]]*\\]\\([^)]+\\.(png|jpe?g|svg|pdf)\\)"
  patron_completo_con_width <- "!\\[[^\\]]*\\]\\([^)]+\\.(png|jpe?g|svg|pdf)\\)\\{[^}]*width\\s*="

  hits <- character(0)
  for (i in seq_along(lineas)) {
    if (grepl(patron_imagen, lineas[i], perl = TRUE) &&
        !grepl(patron_completo_con_width, lineas[i], perl = TRUE)) {
      # Excluir comentarios R puros (línea empieza con #) y líneas de texto
      # explicativo dentro de `# Pseudocódigo` o documentación
      linea_trimmed <- trimws(lineas[i])
      if (!startsWith(linea_trimmed, "#")) {
        hits <- c(hits, paste0("L", i, ": ", trimws(lineas[i])))
      }
    }
  }
  hits
}

# ---------- Detección Error 17: exshuffle:TRUE + letra explícita ----------

detectar_exshuffle_con_letra <- function(ruta_rmd) {
  # Reporta si el .Rmd combina:
  #   1. exshuffle: TRUE en Meta-information
  #   2. Solution con `r letra_correcta` o "Opción [A-D]" hardcoded
  if (!file.exists(ruta_rmd)) return(character(0))
  contenido <- paste(readLines(ruta_rmd, warn = FALSE), collapse = "\n")

  tiene_exshuffle_true <- grepl(
    "(?m)^\\s*exshuffle\\s*:\\s*TRUE",
    contenido, perl = TRUE, ignore.case = FALSE
  )
  if (!tiene_exshuffle_true) return(character(0))

  # Buscar referencias a letra en Solution
  patrones_letra <- c(
    "`r\\s+letra_correcta\\s*`",
    "Opci[oó]n\\s+[A-D]\\s*\\(?CORRECTA\\)?",
    "Opci[oó]n\\s+\\*\\*[A-D]\\*\\*"
  )

  hits <- character(0)
  for (p in patrones_letra) {
    if (grepl(p, contenido, perl = TRUE)) {
      hits <- c(hits, paste0("Patrón sospechoso: ", p))
    }
  }
  if (length(hits) > 0) {
    hits <- c("exshuffle: TRUE detectado en Meta-information", hits)
  }
  hits
}

# ============================================================
# TESTS
# ============================================================

context("Regresión Error 16: Markdown sin width attribute")

test_that("Detector de imágenes Markdown sin width funciona en string controlado", {
  # Crear archivo temporal con bug
  tmp <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "Question",
    "========",
    "![](grafico.png)",
    "",
    "Answerlist",
    "----------",
    "* Opción A"
  ), tmp)

  hits <- detectar_markdown_sin_width(tmp)
  expect_true(length(hits) >= 1,
              info = "Debe detectar `![](grafico.png)` sin width")
  expect_true(any(grepl("L3", hits)),
              info = "Debe reportar línea 3")

  unlink(tmp)
})

test_that("Detector NO reporta imágenes con width attribute", {
  tmp <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "Question",
    "========",
    "![](grafico.png){width=80%}",
    "![](otro.png){ width=50% }"
  ), tmp)

  hits <- detectar_markdown_sin_width(tmp)
  expect_equal(length(hits), 0,
               info = "No debe reportar imágenes con width attribute")

  unlink(tmp)
})

test_that("Detector reporta cat() con Markdown sin width", {
  tmp <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r, echo=FALSE, results='asis'}",
    "cat(\"![](grafico.png)\\n\")",
    "```"
  ), tmp)

  hits <- detectar_markdown_sin_width(tmp)
  expect_true(length(hits) >= 1,
              info = "Debe detectar Markdown sin width emitido vía cat()")

  unlink(tmp)
})

test_that("Repositorio: ningún .Rmd nuevo (no-legacy) tiene Markdown sin width", {
  rmds <- listar_rmd()
  if (length(rmds) == 0) {
    skip("No hay .Rmd en directorios auditados")
  }

  archivos_con_bug_nuevos <- list()
  archivos_con_bug_legacy <- list()
  for (rmd in rmds) {
    hits <- detectar_markdown_sin_width(rmd)
    if (length(hits) > 0) {
      # Es legacy conocido?
      es_legacy <- any(sapply(.legacy_known_pandocbounded,
                              function(p) grepl(p, rmd, fixed = TRUE)))
      if (es_legacy) {
        archivos_con_bug_legacy[[rmd]] <- hits
      } else {
        archivos_con_bug_nuevos[[rmd]] <- hits
      }
    }
  }

  # Reportar legacy como aviso (no bloquea)
  if (length(archivos_con_bug_legacy) > 0) {
    message(sprintf(
      "[INFO] %d archivos legacy con Error 16 conocido (no bloquean):\n  %s\n  -> Re-renderizar tras aplicar fix Patrón A.",
      length(archivos_con_bug_legacy),
      paste(basename(names(archivos_con_bug_legacy)), collapse = ", ")
    ))
  }

  # Bloqueante solo para .Rmd nuevos (no en lista de legacy conocidos)
  if (length(archivos_con_bug_nuevos) > 0) {
    msg <- paste(
      "ARCHIVOS NUEVOS con `![](*.png)` sin atributo {width=...} (Error 16):",
      paste(
        names(archivos_con_bug_nuevos),
        sapply(archivos_con_bug_nuevos, function(h) paste(h, collapse = "; ")),
        sep = "\n  ", collapse = "\n"
      ),
      "Ver .claude/rules/markdown-imagenes-pdf.md",
      "Si es legacy aceptado, agregar a .legacy_known_pandocbounded en este test.",
      sep = "\n"
    )
    expect_equal(length(archivos_con_bug_nuevos), 0, info = msg)
  } else {
    expect_equal(length(archivos_con_bug_nuevos), 0)
  }
})

context("Regresión Error 17: exshuffle:TRUE + Solution con letra explícita")

test_that("Detector de exshuffle:TRUE+letra funciona en string controlado", {
  tmp <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "Question",
    "========",
    "Pregunta...",
    "",
    "Solution",
    "========",
    "La respuesta correcta es la **Opción `r letra_correcta`** porque...",
    "",
    "Meta-information",
    "================",
    "extype: schoice",
    "exshuffle: TRUE",
    "exsolution: 0100"
  ), tmp)

  hits <- detectar_exshuffle_con_letra(tmp)
  expect_true(length(hits) >= 2,
              info = "Debe detectar combinación exshuffle:TRUE + r letra_correcta")

  unlink(tmp)
})

test_that("Detector NO reporta exshuffle:FALSE", {
  tmp <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "Solution",
    "========",
    "La respuesta correcta es la **Opción `r letra_correcta`** porque...",
    "",
    "Meta-information",
    "================",
    "exshuffle: FALSE"
  ), tmp)

  hits <- detectar_exshuffle_con_letra(tmp)
  expect_equal(length(hits), 0,
               info = "No debe reportar cuando exshuffle:FALSE")

  unlink(tmp)
})

test_that("Detector NO reporta exshuffle:TRUE sin letra explícita", {
  tmp <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "Solution",
    "========",
    "La respuesta correcta se obtiene aplicando la fórmula...",
    "",
    "Meta-information",
    "================",
    "exshuffle: TRUE"
  ), tmp)

  hits <- detectar_exshuffle_con_letra(tmp)
  expect_equal(length(hits), 0,
               info = "No debe reportar cuando Solution NO referencia letra")

  unlink(tmp)
})

test_that("Repositorio: ningún .Rmd activo combina exshuffle:TRUE + letra", {
  rmds <- listar_rmd()
  if (length(rmds) == 0) {
    skip("No hay .Rmd en directorios auditados")
  }

  archivos_con_bug <- list()
  for (rmd in rmds) {
    hits <- detectar_exshuffle_con_letra(rmd)
    if (length(hits) > 0) {
      archivos_con_bug[[rmd]] <- hits
    }
  }

  if (length(archivos_con_bug) > 0) {
    msg <- paste(
      "Archivos con exshuffle:TRUE + Solution con letra (Error 17):",
      paste(
        names(archivos_con_bug),
        sapply(archivos_con_bug, function(h) paste(h, collapse = "; ")),
        sep = "\n  ", collapse = "\n"
      ),
      "Ver .claude/rules/codigo-rmd.md regla #6 y graficos-como-opciones.md",
      sep = "\n"
    )
    expect_equal(length(archivos_con_bug), 0, info = msg)
  } else {
    expect_equal(length(archivos_con_bug), 0)
  }
})

context("Regresión: pandocbounded en .tex generados recientemente")

test_that("Si hay .tex generados HOY, ninguno contiene \\pandocbounded", {
  # Solo verifica .tex modificados en las últimas 24 horas
  # (artefactos viejos pueden contener el bug por compilaciones pasadas
  #  con pandoc 2.x; lo que importa es prevenir NUEVOS).
  candidatos_dirs <- c(
    file.path(.repo_root, "A-Produccion", "01-En-PreDesarrollo"),
    file.path(.repo_root, "A-Produccion", "02-En-Desarrollo"),
    file.path(.repo_root, "A-Produccion", "03-En-Produccion")
  )
  candidatos_dirs <- candidatos_dirs[dir.exists(candidatos_dirs)]

  texs <- character(0)
  for (d in candidatos_dirs) {
    encontrados <- list.files(d, pattern = "\\.tex$",
                              recursive = TRUE, full.names = TRUE)
    # Filtrar Lab-Manjaro y Ejemplos-Funcionales
    encontrados <- encontrados[!grepl("Lab-Manjaro|Ejemplos-Funcionales-Rmd",
                                       encontrados, perl = TRUE)]
    texs <- c(texs, encontrados)
  }

  if (length(texs) == 0) {
    skip("No hay .tex generados; el test pasa por defecto")
  }

  # Solo .tex modificados en las últimas 24 horas
  ahora <- Sys.time()
  un_dia <- as.difftime(24, units = "hours")
  texs_recientes <- texs[sapply(texs, function(t) {
    info <- file.info(t)
    !is.na(info$mtime) && (ahora - info$mtime) < un_dia
  })]

  if (length(texs_recientes) == 0) {
    skip("No hay .tex modificados en las últimas 24h; el test pasa por defecto")
  }

  # El Error 16 NO es "aparece la cadena \pandocbounded": es "se USA sin estar
  # DEFINIDO". Pandoc >=3.2.1 emite en el preambulo
  #     \providecommand{\pandocbounded}[1]{#1}
  # que es precisamente la DEFENSA -- ese .tex compila. Contarlo como bug es
  # castigar a la proteccion, el mismo modo de fallo que el discriminador de
  # citas de nomenclatura (v3.20.8): no distinguia "usa el patron" de "advierte
  # contra el patron". Falso rojo detectado el 2026-08-13 sobre los texkeep del
  # CLOZE informacion-insuficiente-lote-n4, cuyos .tex SI llevan la definicion.
  esta_definido <- function(txt) {
    grepl("\\providecommand{\\pandocbounded}", txt, fixed = TRUE) ||
    grepl("\\newcommand{\\pandocbounded}",     txt, fixed = TRUE) ||
    grepl("\\def\\pandocbounded",              txt, fixed = TRUE) ||
    grepl("\\renewcommand{\\pandocbounded}",   txt, fixed = TRUE)
  }
  archivos_con_bug <- character(0)
  for (tex in texs_recientes) {
    contenido <- tryCatch(
      paste(readLines(tex, warn = FALSE), collapse = "\n"),
      error = function(e) ""
    )
    if (grepl("\\pandocbounded", contenido, fixed = TRUE) &&
        !esta_definido(contenido)) {
      archivos_con_bug <- c(archivos_con_bug, tex)
    }
  }

  # Controles del propio detector (sin tocar disco): que siga cazando el uso sin
  # definicion, y que NO marque el .tex protegido.
  .bug  <- "\\pandocbounded{\\includegraphics{g.png}}"
  .safe <- paste("\\providecommand{\\pandocbounded}[1]{#1}",
                 "\\pandocbounded{\\includegraphics{g.png}}", sep = "\n")
  stopifnot(
    grepl("\\pandocbounded", .bug, fixed = TRUE) && !esta_definido(.bug),
    grepl("\\pandocbounded", .safe, fixed = TRUE) && esta_definido(.safe)
  )

  expect_equal(
    length(archivos_con_bug), 0,
    info = paste(
      ".tex RECIENTES (<24h) con \\pandocbounded undefined detectados:",
      paste(archivos_con_bug, collapse = "\n  "),
      "Re-renderizar el .Rmd correspondiente tras aplicar fix de Error 16.",
      sep = "\n"
    )
  )
})
