# =============================================================================
# test_glifos_latex.R — glifos Unicode que ROMPEN pdflatex (FASE 2O)
# =============================================================================
#
# ORIGEN (2026-08-15): un ejercicio de `03-En-Produccion/` llevaba tiempo sin
# compilar en PDF y nada lo detectaba. Causa medida: el carácter `✓` (U+2713)
# LITERAL en encabezados Markdown de la sección Solution.
#
#   ! LaTeX Error: Unicode character ✓ (U+2713) not set up for use with LaTeX
#
# El defecto es INVISIBLE en HTML (no pasa por LaTeX) y ningún validador del
# arsenal miraba los caracteres del fuente.
#
# QUÉ FIJA ESTE TEST
#   1. Ningún `.Rmd` nuevo de 01/02/03 introduce glifos rompedores en Markdown.
#   2. El detector dispara con glifo visible (control positivo).
#   3. NO dispara con glifo sólo en comentario R (control negativo) — medido:
#      un `.Rmd` así COMPILA.
#   4. NO dispara con tildes españolas ni con los glifos medidos que sí
#      compilan (× ÷ ° ² → — •) — control negativo contra falsos positivos.
#      Esto importa: las tildes son OBLIGATORIAS por la regla #7, así que un
#      detector que las marcara sería peor que no tener detector.
#   5. Los documentos operativos de `.claude/` no contradicen al detector (último
#      bloque del archivo): ninguna lista de glifos a mano que dé por rompedor lo
#      que el detector midió como seguro.
#
# El allowlist `glifos-latex-legacy.txt` NO ADMITE ALTAS y sólo puede decrecer.
# =============================================================================

library(testthat)

repo_root <- tryCatch(
  system("git rev-parse --show-toplevel", intern = TRUE)[1],
  error = function(e) normalizePath("../..", mustWork = FALSE))
if (is.na(repo_root) || !nzchar(repo_root)) repo_root <- normalizePath("../..", mustWork = FALSE)

DETECTOR <- file.path(repo_root, ".claude", "scripts", "validar_glifos_latex.R")
ALLOWLIST <- file.path(repo_root, "tests", "testthat", "glifos-latex-legacy.txt")

# Invoca el detector como proceso aparte y devuelve el exit REAL (sin tuberías,
# que enmascararían el código de salida — trampa documentada en el repo).
correr_detector <- function(ruta) {
  salida <- tempfile()
  st <- system2("Rscript", c("--vanilla", shQuote(DETECTOR), shQuote(ruta)),
                stdout = salida, stderr = salida)
  list(exit = st, texto = paste(readLines(salida, warn = FALSE), collapse = "\n"))
}

escribir_rmd <- function(ruta, cuerpo) {
  con <- file(ruta, open = "w", encoding = "UTF-8")
  writeLines(cuerpo, con); close(con)
  ruta
}

# El barrido usa el MISMO motor cargado en proceso: lanzar un Rscript por
# archivo son ~200 arranques de R y la suite pasa de segundos a minutos. La
# guarda `sys.nframe() == 0L` del detector impide que source() dispare su CLI.
# Los controles de arriba SÍ usan system2 a propósito: son los que verifican el
# contrato del CLI y el exit REAL, que es lo que consume el hook.
if (file.exists(DETECTOR)) source(DETECTOR, local = TRUE, encoding = "UTF-8")

bloquea <- function(ruta) length(analizar_glifos(ruta)$bloqueantes) > 0

META <- paste(
  "", "Meta-information", "================", "exname: fx",
  "extype: schoice", "exsolution: 10", "exshuffle: FALSE", sep = "\n")

# -----------------------------------------------------------------------------
test_that("el detector existe y es invocable", {
  expect_true(file.exists(DETECTOR), info = paste("falta el detector:", DETECTOR))
  expect_true(file.exists(ALLOWLIST), info = paste("falta el allowlist:", ALLOWLIST))
})

# -----------------------------------------------------------------------------
# CONTROL POSITIVO: glifo rompedor en texto Markdown visible -> DEBE disparar.
# Sin este control, un detector roto que nunca dispara pasaría por "todo limpio".
test_that("CONTROL POSITIVO: glifo en Markdown visible dispara ERR_GLIFO_LATEX", {
  d <- tempfile(); dir.create(d)
  f <- escribir_rmd(file.path(d, "visible.Rmd"), paste0(
    "Question\n========\n### Paso 1 verificado ✓\n\n",
    "Texto con π y ≤ visibles.\n\n",
    "Answerlist\n----------\n* A\n* B\n\nSolution\n========\nSol.\n\n",
    "Answerlist\n----------\n* Correcto\n* Incorrecto\n", META))

  r <- correr_detector(f)
  expect_equal(r$exit, 1L,
    info = paste0("El detector NO disparó con U+2713/U+03C0/U+2264 en Markdown. ",
                  "Salida:\n", r$texto))
  expect_true(grepl("ERR_GLIFO_LATEX", r$texto, fixed = TRUE),
    info = "Falta el código de error ERR_GLIFO_LATEX en la salida")
  expect_true(grepl("U+2713", r$texto, fixed = TRUE),
    info = "El detector no nombra el codepoint que rompió")
})

# -----------------------------------------------------------------------------
# CONTROL NEGATIVO 1: glifo SOLO en comentario R -> NO debe disparar.
# Medido: un .Rmd así COMPILA. Bloquearlo sería un falso positivo.
test_that("CONTROL NEGATIVO: glifo sólo en comentario R no dispara", {
  d <- tempfile(); dir.create(d)
  f <- escribir_rmd(file.path(d, "comentario.Rmd"), paste0(
    "```{r datos, echo=FALSE}\n",
    "# ✓ verificado: π y ≤ solo en comentario\n",
    "v <- 1\n```\n\n",
    "Question\n========\nTexto limpio. Valor: `r v`.\n\n",
    "Answerlist\n----------\n* A\n* B\n\nSolution\n========\nSol.\n\n",
    "Answerlist\n----------\n* Correcto\n* Incorrecto\n", META))

  r <- correr_detector(f)
  expect_equal(r$exit, 0L,
    info = paste0("Disparó por un glifo en COMENTARIO R, que está medido como ",
                  "inocuo (compila). Salida:\n", r$texto))
  expect_true(grepl("INFO_GLIFO_COMENTARIO", r$texto, fixed = TRUE),
    info = "Debería informar del comentario sin contarlo como error")
})

# -----------------------------------------------------------------------------
# CONTROL NEGATIVO 2: tildes españolas + glifos MEDIDOS que sí compilan.
# Un detector que marcara las tildes contradiría la regla #7, que las EXIGE.
test_that("CONTROL NEGATIVO: tildes y glifos que sí compilan no disparan", {
  d <- tempfile(); dir.create(d)
  f <- escribir_rmd(file.path(d, "limpio.Rmd"), paste0(
    "Question\n========\n",
    "Tildes obligatorias (regla #7): ángulo, función, gráfica, ",
    "ñame, pingüino, ¿cuál?, ¡ojo!\n\n",
    "Glifos medidos que SÍ compilan: × ÷ ° ² ³ º ",
    "§ « » · ± — – … ‰ • ",
    "← ↑ → £ €\n\n",
    "Answerlist\n----------\n* A\n* B\n\nSolution\n========\nSol.\n\n",
    "Answerlist\n----------\n* Correcto\n* Incorrecto\n", META))

  r <- correr_detector(f)
  expect_equal(r$exit, 0L,
    info = paste0("FALSO POSITIVO: marcó tildes españolas o glifos medidos como ",
                  "compilables (× ÷ ° ² → — • ...). Salida:\n", r$texto))
  expect_true(grepl("OK:", r$texto, fixed = TRUE),
    info = paste0("Debería reportar OK. Salida:\n", r$texto))
})

# -----------------------------------------------------------------------------
# CONTROL DE COBERTURA: las flechas ← ↑ → son excepciones MEDIDAS (compilan),
# pero ↔ ⇒ ⇔ ↺ del mismo bloque Unicode NO. Fija que la excepción no se coma
# el rango entero — sería el modo de fallo silencioso más probable aquí.
test_that("las excepciones de flechas no desactivan el resto del bloque", {
  d <- tempfile(); dir.create(d)
  f_ok <- escribir_rmd(file.path(d, "flechas_ok.Rmd"), paste0(
    "Question\n========\nFlechas que compilan: ← ↑ →\n\n",
    "Answerlist\n----------\n* A\n* B\n\nSolution\n========\nSol.\n\n",
    "Answerlist\n----------\n* Correcto\n* Incorrecto\n", META))
  f_mal <- escribir_rmd(file.path(d, "flechas_mal.Rmd"), paste0(
    "Question\n========\nFlechas que rompen: ↔ ⇒ ⇔ ↺\n\n",
    "Answerlist\n----------\n* A\n* B\n\nSolution\n========\nSol.\n\n",
    "Answerlist\n----------\n* Correcto\n* Incorrecto\n", META))

  expect_equal(correr_detector(f_ok)$exit, 0L,
    info = "← ↑ → están MEDIDOS como compilables: no deben disparar")
  expect_equal(correr_detector(f_mal)$exit, 1L,
    info = "↔ ⇒ ⇔ ↺ están MEDIDOS como rompedores: deben disparar")
})

# -----------------------------------------------------------------------------
# BARRIDO DEL REPOSITORIO
test_that("ningún .Rmd fuera del allowlist tiene glifos rompedores en Markdown", {
  dirs <- file.path(repo_root, "A-Produccion",
                    c("01-En-PreDesarrollo", "02-En-Desarrollo", "03-En-Produccion"))
  dirs <- dirs[dir.exists(dirs)]
  skip_if(length(dirs) == 0, "no existen los directorios de A-Produccion")

  archivos <- unlist(lapply(dirs, list.files, pattern = "\\.Rmd$",
                            recursive = TRUE, full.names = TRUE))
  skip_if(length(archivos) == 0, "no se encontraron .Rmd")

  legacy <- readLines(ALLOWLIST, warn = FALSE, encoding = "UTF-8")
  legacy <- trimws(legacy[!grepl("^\\s*#", legacy) & nzchar(trimws(legacy))])

  rel <- function(p) sub(paste0("^", repo_root, "/"), "", normalizePath(p, mustWork = FALSE))

  infractores <- character(0)
  for (a in archivos) {
    if (bloquea(a) && !(rel(a) %in% legacy))
      infractores <- c(infractores, rel(a))
  }

  expect_equal(length(infractores), 0L,
    info = paste0(
      "Estos .Rmd tienen glifos que ROMPEN pdflatex en texto Markdown visible ",
      "y no están en el allowlist legacy:\n  ",
      paste(infractores, collapse = "\n  "),
      "\n\nNo compilarán en PDF/NOPS (HTML no lo detecta: no pasa por LaTeX).",
      "\nFix: sustituir por ASCII o por el comando LaTeX en math ($\\le$, $\\pi$).",
      "\nDiagnóstico: Rscript .claude/scripts/validar_glifos_latex.R <archivo>",
      "\nEl allowlist NO ADMITE ALTAS: sólo puede decrecer."))
})

# -----------------------------------------------------------------------------
# HIGIENE DEL ALLOWLIST: si un archivo listado ya se corrigió o desapareció,
# debe salir de la lista. Es lo que hace que "sólo puede decrecer" sea real y
# no una frase en un comentario.
test_that("el allowlist no contiene entradas obsoletas", {
  legacy <- readLines(ALLOWLIST, warn = FALSE, encoding = "UTF-8")
  legacy <- trimws(legacy[!grepl("^\\s*#", legacy) & nzchar(trimws(legacy))])
  skip_if(length(legacy) == 0, "allowlist vacío")

  inexistentes <- legacy[!file.exists(file.path(repo_root, legacy))]
  expect_equal(length(inexistentes), 0L,
    info = paste0("Entradas del allowlist que ya no existen:\n  ",
                  paste(inexistentes, collapse = "\n  "), "\nBorrarlas."))

  vivos <- legacy[file.exists(file.path(repo_root, legacy))]
  ya_limpios <- vivos[!vapply(vivos, function(p)
    bloquea(file.path(repo_root, p)), logical(1))]
  expect_equal(length(ya_limpios), 0L,
    info = paste0("Estos archivos del allowlist YA están limpios; sacarlos ",
                  "para que la lista decrezca:\n  ",
                  paste(ya_limpios, collapse = "\n  ")))
})

# -----------------------------------------------------------------------------
test_that("el hook post-exams2 cablea la FASE 2O", {
  hook <- file.path(repo_root, ".claude", "hooks", "post-exams2-validation.sh")
  skip_if_not(file.exists(hook), "no existe el hook")
  txt <- paste(readLines(hook, warn = FALSE), collapse = "\n")

  expect_true(grepl("FASE 2O", txt, fixed = TRUE),
    info = "El hook no declara la FASE 2O")
  expect_true(grepl("validar_glifos_latex.R", txt, fixed = TRUE),
    info = "El hook no invoca el detector compartido (¿implementación duplicada?)")
  expect_true(grepl("ERR_GLIFO_LATEX", txt, fixed = TRUE),
    info = "El hook no usa el código de error ERR_GLIFO_LATEX")

  # BUG REAL cazado el 2026-08-15 por el control end-to-end del hook:
  # la fase se escribió con `if [ -f "$RMD_FILE" ]`, que es la ruta RELATIVA al
  # cwd del comando. El hook no hace cd a ese cwd, así que la condición era
  # falsa siempre que el usuario invocara exams2pdf("archivo.Rmd") con ruta
  # relativa — el caso normal. La fase entera quedaba MUDA sin que nada avisara:
  # un gate que no se ejecuta se ve exactamente igual que un gate que no
  # encuentra nada. La variable correcta es $RMD_PATH (absoluta).
  # Recorte POR LÍNEAS: en R el `.` de una regex no cruza saltos de línea, así
  # que un sub(".*FASE 2O...") devolvería el archivo entero — y entonces el
  # check daría un falso positivo por la FASE 2K, que sí usa $RMD_FILE.
  lineas <- readLines(hook, warn = FALSE)
  ini <- grep("FASE 2O: GLIFOS UNICODE", lineas)[1]
  fin <- grep("RESUMEN FINAL Y ACCIONES", lineas)
  fin <- fin[fin > ini][1]
  expect_false(is.na(ini), info = "No se localizó el bloque de la FASE 2O en el hook")
  bloque_lineas <- lineas[ini:(fin - 1)]
  # Sólo CÓDIGO: el bloque lleva un comentario que ADVIERTE contra `-f "$RMD_FILE"`,
  # y un discriminador que no distinga "usa el patrón" de "advierte contra el
  # patrón" castiga justo a la documentación que arregla el problema. Es el
  # mismo bug de discriminador que el repo ya cazó en la v3.20.8.
  bloque <- paste(grep("^\\s*#", bloque_lineas, invert = TRUE, value = TRUE),
                  collapse = "\n")

  expect_true(grepl('-f "$RMD_PATH"', bloque, fixed = TRUE),
    info = paste0("La FASE 2O debe condicionar sobre $RMD_PATH (absoluta). ",
                  "Con $RMD_FILE (relativa) la fase no se ejecuta nunca y el ",
                  "gate queda mudo."))
  expect_false(grepl('-f "$RMD_FILE"', bloque, fixed = TRUE),
    info = "La FASE 2O sigue condicionando sobre $RMD_FILE (ruta relativa)")
})

# -----------------------------------------------------------------------------
# COHERENCIA DE LOS DOCUMENTOS OPERATIVOS CON EL DETECTOR (2026-10-29)
#
# ORIGEN: los dos orquestadores (`.claude/agents/orquestador-{schoice,cloze}.md`)
# traían, en el Incidente `INC-CAMPO-NO-EMITIDO` y en su «Validación realista»,
# una lista de glifos PROPIA, `grep -nP '[\x{2212}\x{2264}\x{2265}\x{00D7}]'`, que
# daba `×` (U+00D7) por rompedor «fuera de modo matemático». El detector midió lo
# contrario en las dos cosas: `×` compila en texto visible y el modo matemático
# NO salva (`$a ≤ b$` falla igual). Dos fuentes de verdad rivales, y los agentes
# obedecían la que estaba en prosa. Es la lección de test_nomenclatura_rmd.R: lo
# que vive en un documento pierde contra lo que vive en un regex, así que el
# documento se ata al detector.
#
# QUÉ ROMPE NO SE COPIA AQUÍ: la fuente es es_rompedor() del detector (cargado con
# source() arriba; su CLI no corre por la guarda `sys.nframe() == 0L`). Los helpers
# son PUROS (líneas de texto + esa función) para poder aplicarlos también a
# mutantes y no sólo a los archivos de hoy:
#   G1 clases_grep_con_seguros       una clase de grep con \x{HHHH} no contiene ningún
#                                    code point que el detector dé por seguro.
#   G2 incidente_delega_en_detector  el incidente y el punto «reutiliza un pool» de
#                                    la Validación realista remiten a
#                                    validar_glifos_latex.R, y el incidente dice que
#                                    el modo matemático no salva.
#   G3 seguros_presentados_como_peligrosos
#                                    un U+XXXX seguro no aparece sin decir, en su
#                                    misma línea, que es «seguro» o que «compila».
#
# LÍMITES, declarados: G1 sólo ve clases escritas con \x{HHHH} en una línea con
# `grep`. NO se extendió a glifos literales porque eso sí tiene usos legítimos que
# ya existen (`Opci[oó]n [A-D]` en .claude/commands/validate.md y en los dos
# orquestadores): marcaría tildes, que la regla #7 EXIGE. Si algún día aparece un
# uso legítimo de \x{HHHH} que no sea una comprobación de LaTeX (p. ej. detectar un
# espacio duro), se acota ESTA guarda; no se debilita el helper. G3 es una
# heurística de presencia: no entiende negaciones («U+00D7 no compila» pasaría).
# Lo que una guarda no ve lo cubre otra: G2 obliga a remitir al detector.

clases_grep_con_seguros <- function(lineas, rompe) {
  hallazgos <- character(0)
  candidatas <- which(grepl("grep", lineas, fixed = TRUE) &
                      grepl("\\x{", lineas, fixed = TRUE))
  for (i in candidatas) {
    # Una clase [...] puede llevar clases POSIX anidadas: [[:space:]\x{00D7}].
    clases <- regmatches(lineas[i], gregexpr(
      "\\[(?:\\[:\\^?[a-z]+:\\]|[^\\]])*\\]", lineas[i], perl = TRUE))[[1]]
    for (clase in clases) {
      hex <- regmatches(clase, gregexpr(
        "\\\\x\\{[0-9A-Fa-f]{2,6}\\}", clase, perl = TRUE))[[1]]
      cps <- unique(strtoi(sub("^\\\\x\\{([0-9A-Fa-f]+)\\}$", "\\1", hex), 16L))
      for (cp in cps[!is.na(cps)])
        if (!isTRUE(rompe(cp)))
          hallazgos <- c(hallazgos, sprintf(
            "línea %d: U+%04X en una clase de grep: %s",
            i, cp, substr(trimws(lineas[i]), 1, 90)))
    }
  }
  hallazgos
}

incidente_delega_en_detector <- function(lineas) {
  fallas <- character(0)
  # Los títulos se buscan FUERA de las vallas de código: un `# comentario` dentro
  # de un bloque bash no es un título y cortaría la sección antes de su comprobación.
  en_valla <- cumsum(grepl("^\\s*```", lineas, perl = TRUE)) %% 2L == 1L
  es_titulo <- grepl("^#{1,3}\\s", lineas, perl = TRUE) & !en_valla

  ini <- which(es_titulo &
               grepl("^###\\s+Incidente\\b.*INC-CAMPO-NO-EMITIDO", lineas, perl = TRUE))
  if (length(ini) != 1L) {
    fallas <- c(fallas, sprintf(
      "hay %d secciones «### Incidente … INC-CAMPO-NO-EMITIDO» y debe haber exactamente 1",
      length(ini)))
  } else {
    sig <- which(es_titulo & seq_along(lineas) > ini)
    fin <- if (length(sig)) sig[1L] - 1L else length(lineas)
    seccion <- lineas[ini:fin]
    if (!any(grepl("validar_glifos_latex.R", seccion, fixed = TRUE)))
      fallas <- c(fallas, "la sección del incidente no remite a validar_glifos_latex.R")
    if (!any(grepl("\\bno\\s+(?:los\\s+|lo\\s+)?salva", seccion,
                   ignore.case = TRUE, perl = TRUE)))
      fallas <- c(fallas, "la sección del incidente no dice que el modo matemático «no salva»")
  }

  punto <- which(grepl("\\*\\*Si el [^*]*reutiliza un pool", lineas, perl = TRUE))
  if (!length(punto)) {
    fallas <- c(fallas, "falta el punto «Si el .Rmd reutiliza un pool» de la Validación realista")
  } else {
    for (i in punto[!grepl("validar_glifos_latex.R", lineas[punto], fixed = TRUE)])
      fallas <- c(fallas, sprintf(
        "línea %d: el punto «reutiliza un pool» no remite a validar_glifos_latex.R", i))
  }
  fallas
}

seguros_presentados_como_peligrosos <- function(lineas, rompe) {
  hallazgos <- character(0)
  for (i in which(grepl("U\\+[0-9A-Fa-f]{4,6}", lineas, perl = TRUE))) {
    if (grepl("segur|compila", lineas[i], ignore.case = TRUE)) next
    # «U+2264/U+2265» (y «U+2264/2265»): cada elemento del rango cuenta.
    fichas <- regmatches(lineas[i], gregexpr(
      "U\\+[0-9A-Fa-f]{4,6}(?:/(?:U\\+)?[0-9A-Fa-f]{4,6})*", lineas[i], perl = TRUE))[[1]]
    cps <- unique(strtoi(sub("^U\\+", "", unlist(strsplit(fichas, "/", fixed = TRUE))), 16L))
    for (cp in cps[!is.na(cps)])
      if (!isTRUE(rompe(cp)))
        hallazgos <- c(hallazgos, sprintf(
          "línea %d: U+%04X sin «seguro» ni «compila» en su misma línea: %s",
          i, cp, substr(trimws(lineas[i]), 1, 90)))
  }
  hallazgos
}

test_that("los orquestadores y las reglas no contradicen al detector de glifos (regla #25)", {
  leer_md <- function(p) readLines(p, warn = FALSE, encoding = "UTF-8")
  rel <- function(p) sub(paste0("^", repo_root, "/"), "", p)
  # Una falla única y por la causa prevista: un mutante que muere por OTRA sonda no
  # prueba la guarda (Incidente P de los orquestadores, INC-MUTANTE-SONDA).
  solo <- function(r, causa) length(r) == 1L && grepl(causa, r)

  # --- Anclas: lo único que los controles asumen del detector -------------------
  # Si el detector cambia de criterio, que falle AQUÍ y lo diga, no en un control
  # con un mensaje que parezca un bug de la guarda.
  expect_false(any(vapply(c(0x00D7, 0x00F7, 0x2014), es_rompedor, logical(1))),
    info = paste0("El detector ya da por rompedor U+00D7 (×), U+00F7 (÷) o U+2014 (—). ",
                  "Los controles de esta guarda parten de lo medido el 2026-08-15 (compilan ",
                  "en texto visible): reconciliar orquestadores y controles con la nueva medición."))
  expect_true(all(vapply(c(0x2212, 0x2264, 0x2265), es_rompedor, logical(1))),
    info = paste0("El detector ya no da por rompedor U+2212 (−), U+2264 (≤) o U+2265 (≥), ",
                  "los de INC-CAMPO-NO-EMITIDO: los controles de esta guarda quedaron desfasados."))

  # --- G1: controles del discriminador ----------------------------------------
  # Línea REAL de orquestador-schoice.md:613 en 39923879 (antes del fix): DEBE marcar U+00D7.
  g1_vieja <- paste0("- Comprobación: `grep -nP '[\\x{2212}\\x{2264}\\x{2265}\\x{00D7}]' ",
                     "<archivo.Rmd>`.")
  r <- clases_grep_con_seguros(g1_vieja, es_rompedor)
  expect_true(solo(r, "U\\+00D7"),
    info = paste0("G1 NO caza la lista de glifos a mano del orquestador viejo (grep con ",
                  "U+00D7): está roto. Marcó: [", paste(r, collapse = " | "), "]"))
  # Una clase sólo con rompedores medidos NO es un defecto.
  r <- clases_grep_con_seguros("grep -nP '[\\x{2212}\\x{2264}]' <archivo.Rmd>", es_rompedor)
  expect_equal(length(r), 0L,
    info = paste0("FALSO POSITIVO de G1: marcó una clase que sólo contiene rompedores ",
                  "medidos: [", paste(r, collapse = " | "), "]"))
  # Las dos formas de esquivarlo: hexadecimal en minúsculas y clase POSIX anidada.
  r <- clases_grep_con_seguros(c("grep -P '[\\x{2212}\\x{00d7}]' x.Rmd",
                                 "grep -nP '[[:space:]\\x{00D7}]' x.Rmd"), es_rompedor)
  expect_equal(length(r), 2L,
    info = paste0("G1 se deja esquivar con hexadecimal en minúsculas o con una clase ",
                  "POSIX anidada. Marcó: [", paste(r, collapse = " | "), "]"))

  # --- G3: controles del discriminador ----------------------------------------
  # Línea REAL de orquestador-schoice.md:611 en 39923879 (antes del fix): DEBE marcar
  # sólo U+00D7 (U+2264/U+2265 sí rompen y no son un defecto).
  g3_vieja <- paste0("  signo menos tipográfico), U+2264/U+2265 (`≤` `≥`), U+00D7 (`×`) fuera ",
                     "de modo matemático. Las")
  r <- seguros_presentados_como_peligrosos(g3_vieja, es_rompedor)
  expect_true(solo(r, "U\\+00D7"),
    info = paste0("G3 NO caza un U+00D7 presentado como peligroso (×) fuera de modo ",
                  "matemático: está roto. Marcó: [", paste(r, collapse = " | "), "]"))
  # Texto posterior al fix: lo seguro dicho como seguro NO es un defecto.
  g3_nueva <- paste0("  Medidos como seguros en texto visible (no se tocan): U+00D7 (`×`), ",
                     "U+00F7 (`÷`), U+2014 (`—`) y las tildes.")
  r <- seguros_presentados_como_peligrosos(g3_nueva, es_rompedor)
  expect_equal(length(r), 0L,
    info = paste0("FALSO POSITIVO de G3: marcó la línea que declara seguros a U+00D7, ",
                  "U+00F7 y U+2014: [", paste(r, collapse = " | "), "]"))
  # Rompedores solos (con y sin el prefijo U+ en el segundo elemento del rango).
  r <- seguros_presentados_como_peligrosos(
    c("Peligrosos: U+2212 y U+2264/U+2265.", "Peligrosos: U+2264/2265."), es_rompedor)
  expect_equal(length(r), 0L,
    info = paste0("FALSO POSITIVO de G3: marcó rompedores medidos: [",
                  paste(r, collapse = " | "), "]"))
  # El segundo elemento de un rango también cuenta (U+2264/U+00F7).
  r <- seguros_presentados_como_peligrosos("Peligrosos: U+2264/U+00F7 (`≤` `÷`).", es_rompedor)
  expect_true(solo(r, "U\\+00F7"),
    info = paste0("G3 ignora el segundo elemento de un rango «U+XXXX/U+YYYY»: [",
                  paste(r, collapse = " | "), "]"))

  # --- G2: control positivo y un mutante por cada condición ---------------------
  # El único remite-al-detector de la sección va DENTRO de una valla bash, tras una
  # línea `# comentario` (el orquestador-cloze también lleva su comprobación dentro de
  # una valla así): una G2 que tome ese `#` por título cortaría la sección antes y
  # fallaría ESTE documento.
  doc_ok <- c(
    "## Incidentes",
    "### Incidente O · `INC-CAMPO-NO-EMITIDO` — Un campo que no se emite no está probado",
    "- Todo texto del pool debe compilar. **El modo matemático no los salva**: `$a ≤ b$`.",
    "```bash",
    "# comentario DENTRO de una valla: no es un título ni cierra la sección",
    "Rscript .claude/scripts/validar_glifos_latex.R <archivo.Rmd>",
    "```",
    "### Incidente P · `INC-OTRO` — otra cosa",
    paste0("8. **Si el `.Rmd` reutiliza un pool de otro ejercicio** (Incidente O): ",
           "`Rscript .claude/scripts/validar_glifos_latex.R <archivo.Rmd>` → sin `ERR_GLIFO_LATEX`."))
  r <- incidente_delega_en_detector(doc_ok)
  expect_equal(length(r), 0L,
    info = paste0("G2 rechaza un documento que SÍ delega en el detector (¿toma por título ",
                  "un comentario dentro de una valla?). Dijo: [", paste(r, collapse = " | "), "]"))

  m <- doc_ok; m[6] <- "grep -nP '[\\x{2212}\\x{2264}]' <archivo.Rmd>"
  r <- incidente_delega_en_detector(m)
  expect_true(solo(r, "sección del incidente no remite"),
    info = paste0("G2 no caza un incidente que ya no remite al detector. Dijo: [",
                  paste(r, collapse = " | "), "]"))

  m <- doc_ok; m[6] <- "texto sin referencia"
  m <- append(m, "Rscript .claude/scripts/validar_glifos_latex.R <archivo.Rmd>", after = 8L)
  r <- incidente_delega_en_detector(m)
  expect_true(solo(r, "sección del incidente no remite"),
    info = paste0("G2 da por buena una referencia que está en la sección SIGUIENTE. Dijo: [",
                  paste(r, collapse = " | "), "]"))

  m <- doc_ok; m[3] <- "- Todo texto del pool debe compilar, salvo U+00D7 fuera de modo matemático."
  r <- incidente_delega_en_detector(m)
  expect_true(solo(r, "no salva"),
    info = paste0("G2 no caza un incidente que ya no dice que el modo matemático no salva. ",
                  "Dijo: [", paste(r, collapse = " | "), "]"))

  m <- doc_ok
  m[9] <- paste0("8. **Si el `.Rmd` reutiliza un pool de otro ejercicio** (Incidente O): ",
                 "`grep -nP '[\\x{2212}\\x{2264}]' <archivo.Rmd>` → debe ser vacío.")
  r <- incidente_delega_en_detector(m)
  expect_true(solo(r, "reutiliza un pool"),
    info = paste0("G2 no caza un punto «reutiliza un pool» que ya no remite al detector. ",
                  "Dijo: [", paste(r, collapse = " | "), "]"))

  m <- doc_ok; m[2] <- "### Incidente O · `INC-OTRO-NOMBRE` — la sección cambió de nombre"
  r <- incidente_delega_en_detector(m)
  expect_true(solo(r, "0 secciones"),
    info = paste0("G2 no avisa cuando desaparece la sección del incidente (la guarda ",
                  "quedaría muda). Dijo: [", paste(r, collapse = " | "), "]"))

  # --- Barrido real -----------------------------------------------------------
  agentes <- file.path(repo_root, ".claude", "agents",
                       c("orquestador-schoice.md", "orquestador-cloze.md"))
  skip_if_not(all(file.exists(agentes)), "faltan los orquestadores en .claude/agents/")

  # Ámbito de G1: todo documento operativo; fuera `.claude/backups/` (copias históricas
  # que conservan a propósito lo que ya se corrigió).
  dirs <- c(file.path(repo_root, ".claude", c("agents", "commands", "rules", "skills")),
            file.path(repo_root, ".claude", "docs", "reglas"))
  docs <- unlist(lapply(dirs[dir.exists(dirs)], list.files, pattern = "\\.md$",
                        recursive = TRUE, full.names = TRUE))
  docs <- docs[!grepl("/_?backups/", docs)]
  expect_true(all(normalizePath(agentes) %in% normalizePath(docs)),
    info = "El ámbito de G1 dejó de incluir a los orquestadores")

  g1 <- unlist(lapply(docs, function(f) {
    r <- clases_grep_con_seguros(leer_md(f), es_rompedor)
    if (length(r)) paste0(rel(f), ": ", r) else character(0)
  }))
  expect_equal(length(g1), 0L,
    info = paste0(
      "Estas clases de grep incluyen glifos que el detector (regla #25) mide como SEGUROS: ",
      "una lista de glifos a mano que contradice al detector.\n  ",
      paste(g1, collapse = "\n  "),
      "\nFix: sustituir la comprobación por `Rscript .claude/scripts/validar_glifos_latex.R ",
      "<archivo.Rmd>` (el mismo detector de la FASE 2O), no por otra lista."))

  for (a in agentes) {
    lineas <- leer_md(a)
    g2 <- incidente_delega_en_detector(lineas)
    expect_equal(length(g2), 0L,
      info = paste0(basename(a), ": el Incidente INC-CAMPO-NO-EMITIDO o el punto «reutiliza ",
                    "un pool» de la Validación realista no delegan en el detector:\n  ",
                    paste(g2, collapse = "\n  ")))
    g3 <- seguros_presentados_como_peligrosos(lineas, es_rompedor)
    expect_equal(length(g3), 0L,
      info = paste0(basename(a), ": un U+XXXX que el detector mide como SEGURO aparece sin ",
                    "decir «seguro» o «compila» en su línea (¿presentado como peligroso?):\n  ",
                    paste(g3, collapse = "\n  ")))
  }
})
