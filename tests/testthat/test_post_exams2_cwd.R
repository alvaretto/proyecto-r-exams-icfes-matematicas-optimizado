# =============================================================================
# Hook post-render v6.3: el ejercicio sale del .Rmd, no del cwd de la sesión.
# .claude/hooks/post-exams2-validation.sh
#
# Por qué existe. Hasta la v6.2 el hook trataba el cwd de la sesión como la carpeta del ejercicio:
#   - sellaba renderizado_4_formatos y arsenal_post_render, sin nota, sobre el ejercicio_state.json
#     de esa carpeta tras cualquier render con 0 errores (25 de 42 estados del repo llevaban la
#     firma: los dos sellos con 40-80 ms de diferencia);
#   - dejaba el stress test (stress_test_output/), las vistas previas (preview_*.png) y lo que los
#     chunks dibujan en su cwd (tabla_*.png) donde estuviera la sesión;
#   - buscaba .tex con `find "$CWD" -maxdepth 4` y le atribuía al ejercicio los de cualquier otro.
# La v6.3 deduce el ejercicio de la ruta del .Rmd (RMD_ABS), trabaja en una carpeta de corrida de la
# caché (work/, preview/, stress_test_output/), no sella (imprime el comando `complete` con su nota)
# y no escribe nada dentro del repo.
#
# Se ejecuta el hook REAL con un proyecto falso (CLAUDE_PROJECT_DIR): validadores de mentira que
# anotan su cwd, su primer argumento y su entorno de R; un `magick` de mentira; el workflow-state.sh
# real. La sesión está en la carpeta A y el render es de un .Rmd de la carpeta B.
#
# HOOK_CWD apunta a otra copia del hook. Contra la v6.2 (git show e7402c0d:<hook>) esta suite tiene
# que fallar, y contra cada mutante también: los mutantes de abajo se aplican sobre copias en
# tempdir() del hook vivo, y cada uno tiene que hacer fallar su caso. Con HOOK_CWD puesto no se
# aplican (mutarían una copia ya mutada): ahí solo corren T1-T7 contra la copia dada.
#
# Casos
#   T1 la sesión en A no se toca: estados de A y B intactos, nada nuevo bajo el proyecto falso y los
#      artefactos (vistas previas, stress test, lo que dibujan los chunks) en la caché
#   T2 el `complete` sugerido apunta a B, con el md5 de su .Rmd y la copia estable del informe
#   T3 con ruta relativa ../B/x.Rmd el ejercicio es B (ORIGEN.txt y argumentos de los validadores)
#   T4 un .tex señuelo bajo A no se le atribuye al ejercicio; con texdir= nombrado sí se revisa
#   T5 la poda deja 5 corridas y no borra lo que no es una corrida
#   T6 los validadores corren con el .Rprofile y el .Renviron del proyecto, solo si existen
#   T7 la 2B toma el dir= correcto aunque edir= o texdir= vayan antes
#   T8 si arsenal_post_render ya está sellado, la sugerencia lo dice antes del mismo comando
# El comando cambia de directorio antes de renderizar (2026-10-31): la sesión está en la raíz del proyecto
# falso y el .Rmd es relativo al directorio al que llega el comando, no al de la sesión
#   T9  cd <dir> && … exams2html("rel.Rmd"): el hook corre sin avisar «no existe» y el ejercicio es el de
#       ese .Rmd (cd a la carpeta, a una subcarpeta con ../x.Rmd, varios cd encadenados con comillas y --)
#   T10 lo mismo con setwd("<dir>") dentro de Rscript -e (comillas dobles, simples, y tras un cd)
#   T11 un .Rmd relativo que no está ni ahí ni en el cwd de la sesión: sigue saliendo el aviso
#   T12 ese aviso nombra los dos directorios si difieren y, si no, solo el de la sesión (el texto de siempre)
#   T13 respaldo: un cd que no cambia el directorio real (un subshell) se resuelve contra el cwd de la
#       sesión, como hasta la v6.3, y el PDF también se busca ahí
#   T14 el PDF (con y sin dir=) y el .tex de texdir= se buscan desde el directorio al que llega el comando
#   T15 un cd que no se resuelve de forma literal (cd "$PWD") conserva el directorio anterior
#   T16 las formas de cd y de setwd, una por una: el recorrido se extrae del hook y se corre solo
# Contra el hook de antes (git show 8e75b132:.claude/hooks/post-exams2-validation.sh) T9, T10, T12, T14, T15
# y T16 tienen que fallar; T11 y T13 son de compatibilidad y pasan con los dos.
# Mutantes (cada uno hace fallar el caso que se indica)
#   M1 estado de vuelta a $CWD, con complete (la v6.2)        -> T1     M6  sin R_ENVIRON_USER  -> T6
#   M2 las dos llamadas a complete, sobre la carpeta de B     -> T1     M7  dir= sin borde      -> T7
#   M3a vistas previas en $CWD   M3b stress test en $CWD      -> T1     M8  validadores con la ruta
#   M4 find "$CWD" -maxdepth 4 en la 2I                       -> T4         relativa, no RMD_ABS -> T3
#   M5 validadores con cd "$CWD"                              -> T1     M9  sin poda            -> T5
#                                                                       M10 nota -> <nombre>.txt -> T2
#   M11 siempre dice «ya está sellado» -> T2     M12 nunca lo dice -> T8
#   M13 sin recorrido (CWD_EFECTIVO = $CWD)       -> T9     M18 2B: dir= desde $CWD                  -> T14
#   M14 ignora los cd                             -> T9     M19 2B: carpetas de siempre desde $CWD   -> T14
#   M15 ignora los setwd                          -> T10    M20 2I: texdir= desde $CWD               -> T14
#   M16 sin respaldo contra $CWD                  -> T13    M21 el aviso nombra un solo directorio   -> T12
#   M17 el respaldo no devuelve las salidas a $CWD -> T13   M22 cd "$PWD" tomado como literal        -> T15
# =============================================================================
library(testthat)

repo_root <- local({
  r <- tryCatch(system("git rev-parse --show-toplevel", intern = TRUE),
                warning = function(w) NULL, error = function(e) NULL)
  if (!is.null(r) && length(r) == 1 && dir.exists(r)) r else normalizePath("../..")
})

en_entorno <- function(variable, defecto) {
  v <- Sys.getenv(variable, "")
  if (nzchar(v)) v else defecto
}

HOOK         <- en_entorno("HOOK_CWD", file.path(repo_root, ".claude", "hooks", "post-exams2-validation.sh"))
HOOK_EXTERNO <- nzchar(Sys.getenv("HOOK_CWD", ""))
WORKFLOW_SH  <- file.path(repo_root, ".claude", "scripts", "workflow-state.sh")
stopifnot(file.exists(HOOK), file.exists(WORKFLOW_SH))

ARCHIVO_STRAY <- "tabla_dibujada_por_un_chunk.png"   # lo que un chunk escribe en su cwd
CORRIDA_RE    <- "^[0-9]{8}-[0-9]{6}-[0-9]+$"
VALIDADORES   <- c("2A", "arsenal", "2G", "2H")      # los que el hook lanza con `env ... Rscript`

RMD_B <- c("```{r data generation, echo = FALSE, results = \"hide\"}",
           "x <- sample(1:10, 1)", "```", "",
           "Question", "========", "![](g.png){width=60%}", "",
           "Solution", "========", "La respuesta correcta es la que conserva la suma.")

# --- El proyecto falso -------------------------------------------------------------------------
# Cada validador de mentira anota `nombre|cwd|arg1|R_PROFILE_USER|R_ENVIRON_USER|HUMO_RPROFILE|
# HUMO_RENVIRON|FIN` (el FIN conserva los campos vacíos del final) y dibuja un archivo en su cwd,
# como hace un chunk de data_generation.
stub_r <- function(nombre, cuerpo = character()) {
  c(sprintf('NOMBRE <- "%s"', nombre),
    "a <- commandArgs(trailingOnly = TRUE)",
    'campos <- c(NOMBRE, getwd(), a[1], Sys.getenv("R_PROFILE_USER"), Sys.getenv("R_ENVIRON_USER"),',
    '            Sys.getenv("HUMO_RPROFILE"), Sys.getenv("HUMO_RENVIRON"), "FIN")',
    'cat(paste(campos, collapse = "|"), "\\n", file = Sys.getenv("STUB_LOG"), append = TRUE, sep = "")',
    sprintf('file.create("%s")', ARCHIVO_STRAY),
    cuerpo,
    "quit(status = 0)")
}

# `magick` de mentira: anota el PDF que recibe (3.er argumento) y crea el PNG de salida (el último).
STUB_MAGICK <- c("#!/bin/bash", 'echo "$3" >> "$MAGICK_LOG"', ': > "${@: -1}"')

nuevo_mundo <- function(perfil_r = TRUE) {
  base <- tempfile("cwd_"); dir.create(base); base <- normalizePath(base)
  m <- list(base = base, proy = file.path(base, "proy"), informes = file.path(base, "informes"),
            otro = file.path(base, "otro"), bin = file.path(base, "bin"),
            log = file.path(base, "stubs.log"), log_magick = file.path(base, "magick.log"),
            perfil_externo = file.path(base, "sin_perfil", ".Rprofile"),
            environ_externo = file.path(base, "sin_perfil", ".Renviron"))
  ejercicios <- file.path(m$proy, "A-Produccion", "01-En-PreDesarrollo")
  m$A <- file.path(ejercicios, "A"); m$B <- file.path(ejercicios, "B")
  scripts <- file.path(m$proy, ".claude", "scripts")
  fuentes <- file.path(m$proy, "SOURCES", "scripts_validacion")
  for (d in c(scripts, fuentes, m$A, m$B, m$informes, m$otro, m$bin)) dir.create(d, recursive = TRUE)

  # Dos ejercicios con estado: A (la sesión) va un paso por delante de B, para que el `next` de
  # cada uno sea distinto; en ambos, los pasos 5 y 6 siguen pendientes.
  m$rmd_a <- file.path(m$A, "a.Rmd"); m$rmd_b <- file.path(m$B, "x.Rmd")
  writeLines(c(RMD_B, "", "<!-- ejercicio A: otro md5 -->"), m$rmd_a, useBytes = TRUE)
  writeLines(RMD_B, m$rmd_b, useBytes = TRUE)
  file.copy(WORKFLOW_SH, scripts)
  wf <- file.path(scripts, "workflow-state.sh")
  sh <- function(...) invisible(suppressWarnings(system2("bash", shQuote(c(wf, ...)), stdout = FALSE, stderr = FALSE)))
  for (d in c(m$A, m$B)) {
    sh("init", d, "--tipo", "schoice"); sh("complete", d, "analisis_icfes")
    sh("complete", d, "flujo_b", "--requerido", "false"); sh("complete", d, "generacion_rmd")
  }
  sh("complete", m$A, "retroalimentacion")

  writeLines(stub_r("2A", 'cat("APROBADO\\n")'), file.path(scripts, "validar_coherencia_matematica.R"))
  writeLines(stub_r("arsenal", 'cat("Total ERRORES: 0\\nTotal ADVERTENCIAS: 0\\n")'),
             file.path(scripts, "arsenal_validacion_completa.R"))
  writeLines(stub_r("2G", 'cat("Semillas: 100\\n")'), file.path(scripts, "validar_multisemilla.R"))
  writeLines(stub_r("2O"), file.path(scripts, "validar_glifos_latex.R"))
  writeLines(stub_r("2H", c('od <- a[which(a == "--output-dir") + 1]',
                            'dir.create(file.path(od, "pngs"), recursive = TRUE, showWarnings = FALSE)',
                            'writeLines("{}", file.path(od, "reporte.json"))',
                            'file.create(file.path(od, "pngs", "seed_07919.png"))')),
             file.path(fuentes, "stress_test_visual.R"))
  writeLines(STUB_MAGICK, file.path(m$bin, "magick")); Sys.chmod(file.path(m$bin, "magick"), "755")
  writeLines("%PDF-1.4", file.path(m$A, "render.pdf"))   # el PDF que el render dejó en la sesión

  if (perfil_r) {
    writeLines('Sys.setenv(HUMO_RPROFILE = "proyecto-falso")', file.path(m$proy, ".Rprofile"))
    writeLines("HUMO_RENVIRON=proyecto-falso", file.path(m$proy, ".Renviron"))
  }
  m
}

# --- Utilidades --------------------------------------------------------------------------------
md5 <- function(p) unname(tools::md5sum(p))

huella <- function(d) {
  f <- sort(list.files(d, recursive = TRUE, all.files = TRUE, full.names = TRUE, include.dirs = TRUE, no.. = TRUE))
  es_dir <- dir.exists(f)
  h <- character(length(f))
  h[es_dir] <- "<dir>"                                  # una carpeta nueva también cuenta
  h[!es_dir] <- unname(tools::md5sum(f[!es_dir]))
  setNames(h, substring(f, nchar(d) + 2L))
}

diferencias <- function(a, b) {
  comunes <- intersect(names(a), names(b))
  cambiados <- if (length(comunes)) comunes[!mapply(identical, a[comunes], b[comunes])] else character()
  c(setdiff(names(b), names(a)), setdiff(names(a), names(b)), cambiados)
}

render <- function(ruta) sprintf("Rscript -e 'exams::exams2html(\"%s\")'", ruta)
contexto <- function(r) r$json$hookSpecificOutput$additionalContext
contiene <- function(x, patron) any(grepl(patron, x, fixed = TRUE, useBytes = TRUE))
leer_utf8 <- function(f) if (file.exists(f)) readLines(f, warn = FALSE, encoding = "UTF-8") else character()

# Corre el hook como lo corre Claude Code: JSON por stdin; el proceso del hook en una carpeta ajena.
# R_PROFILE_USER/R_ENVIRON_USER salen de fábrica apuntando a archivos que no existen: así el
# validador de mentira no carga el perfil del usuario y se ve si el hook los pisa o los deja.
correr_hook <- function(m, comando, cwd = m$A, hook = HOOK) {
  json <- tempfile(fileext = ".json"); out <- tempfile(); err <- tempfile()
  writeLines(jsonlite::toJSON(list(tool_input = list(command = comando), cwd = cwd), auto_unbox = TRUE),
             json, useBytes = TRUE)
  viejo <- setwd(m$otro); on.exit(setwd(viejo))
  env <- c(paste0("CLAUDE_PROJECT_DIR=", shQuote(m$proy)),
           paste0("POST_EXAMS2_INFORME_DIR=", shQuote(m$informes)),
           paste0("PATH=", shQuote(paste(m$bin, Sys.getenv("PATH"), sep = ":"))),
           paste0("STUB_LOG=", shQuote(m$log)), paste0("MAGICK_LOG=", shQuote(m$log_magick)),
           paste0("R_PROFILE_USER=", shQuote(m$perfil_externo)),
           paste0("R_ENVIRON_USER=", shQuote(m$environ_externo)))
  ec <- suppressWarnings(system2("bash", shQuote(hook), stdin = json, stdout = out, stderr = err, env = env))
  salida <- readLines(out, warn = FALSE, encoding = "UTF-8")
  list(ec = ec, stdout = salida, stderr = leer_utf8(err),
       json = if (length(salida))
         tryCatch(jsonlite::fromJSON(paste(salida, collapse = "\n"), simplifyVector = FALSE),
                  error = function(e) NULL))
}

leer_log <- function(ruta) {
  nombres <- c("stub", "cwd", "arg1", "r_profile_user", "r_environ_user", "humo_rprofile", "humo_renviron")
  vacio <- setNames(data.frame(matrix(character(), 0, 7), stringsAsFactors = FALSE), nombres)
  campos <- strsplit(leer_utf8(ruta), "|", fixed = TRUE)
  campos <- campos[lengths(campos) == 8L]
  if (!length(campos)) return(vacio)
  setNames(as.data.frame(do.call(rbind, lapply(campos, `[`, 1:7)), stringsAsFactors = FALSE), nombres)
}

# --- Escenarios y casos ------------------------------------------------------------------------
# Cada `problemas_*` devuelve character(0) si el hook se comporta como debe, o lo que halló mal:
# el mismo código sirve para el hook vivo (tiene que dar 0) y para cada mutante (tiene que dar > 0).

# Base: sesión en A, render de ../B/x.Rmd con ruta relativa.
escenario_base <- function(hook = HOOK, perfil_r = TRUE) {
  m <- nuevo_mundo(perfil_r)
  estado <- function(d) md5(file.path(d, "ejercicio_state.json"))
  antes <- list(proy = huella(m$proy), a = estado(m$A), b = estado(m$B))
  r <- correr_hook(m, render("../B/x.Rmd"), hook = hook)
  despues <- list(proy = huella(m$proy), a = estado(m$A), b = estado(m$B))
  corridas <- list.dirs(file.path(m$informes, "x"), recursive = FALSE)
  list(m = m, r = r, antes = antes, despues = despues, corridas = corridas,
       run = if (length(corridas) == 1L) corridas else NA_character_,
       informe = leer_utf8(file.path(m$informes, "x.txt")), log = leer_log(m$log))
}

problemas_t1 <- function(e) {
  p <- character()
  if (!identical(e$r$ec, 0L)) p <- c(p, sprintf("el hook salió con %s y no con 0", e$r$ec))
  if (!identical(e$antes$a, e$despues$a)) p <- c(p, "cambió el ejercicio_state.json de A, la carpeta de la sesión")
  if (!identical(e$antes$b, e$despues$b)) p <- c(p, "cambió el ejercicio_state.json de B, la carpeta del .Rmd")
  d <- diferencias(e$antes$proy, e$despues$proy)
  if (length(d)) p <- c(p, paste0("el hook escribió bajo el proyecto falso: ", paste(d, collapse = ", ")))
  if (is.na(e$run)) {
    p <- c(p, sprintf("se esperaba 1 carpeta de corrida en la caché y hay %d", length(e$corridas)))
  } else {
    esperados <- c("ORIGEN.txt", "informe.txt", file.path("work", ARCHIVO_STRAY),
                   file.path("preview", "preview_x.png"), file.path("stress_test_output", "reporte.json"),
                   file.path("stress_test_output", "pngs", "seed_07919.png"))
    faltan <- esperados[!file.exists(file.path(e$run, esperados))]
    if (length(faltan)) p <- c(p, paste0("faltan en la corrida de la caché: ", paste(faltan, collapse = ", ")))
  }
  p
}

problemas_t2 <- function(e) {
  ctx <- contexto(e$r); m <- e$m
  if (is.null(ctx)) return("el hook no entregó additionalContext")
  if (is.na(e$run)) return("no hay una corrida única de la que sacar RUN_DIR/informe.txt")
  p <- character()
  sugerido <- sprintf('complete "%s" arsenal_post_render --nota "informe %s · md5 %s · ',
                      m$B, file.path(e$run, "informe.txt"), md5(m$rmd_b))
  if (!contiene(ctx, sugerido)) p <- c(p, paste0("el contexto no sugiere «", sugerido, "…»"))
  if (contiene(ctx, sprintf('complete "%s"', m$A))) p <- c(p, "el complete sugerido apunta a A, la carpeta de la sesión")
  if (contiene(ctx, md5(m$rmd_a))) p <- c(p, "la nota lleva el md5 del .Rmd de A")
  if (!contiene(ctx, "renderizado_4_formatos se sella solo con HTML, PDF, DOCX y Moodle (NOPS es N/A en un cloze)"))
    p <- c(p, "falta el recordatorio de renderizado_4_formatos")
  if (!contiene(ctx, "Siguiente: retroalimentacion"))
    p <- c(p, "el `next` no sale del estado de B (esperaba «Siguiente: retroalimentacion»)")
  if (contiene(ctx, "Siguiente: renderizado")) p <- c(p, "el `next` sale del estado de A")
  if (contiene(ctx, "ya está sellado")) p <- c(p, "dice «ya está sellado» con el paso 6 pendiente")
  p
}

problemas_t3 <- function(e) {
  p <- character(); m <- e$m
  origen <- if (is.na(e$run)) character() else leer_utf8(file.path(e$run, "ORIGEN.txt"))
  if (!identical(origen, c(paste0("rmd: ", m$rmd_b), paste0("md5: ", md5(m$rmd_b)))))
    p <- c(p, "ORIGEN.txt no identifica el .Rmd de B (ruta absoluta y md5)")
  if (!file.exists(file.path(m$informes, "x.txt"))) p <- c(p, "falta el informe contractual informes/x.txt")
  if (file.exists(file.path(m$informes, "a.txt"))) p <- c(p, "hay un informe a.txt: el hook tomó a A por el ejercicio")
  for (s in VALIDADORES) {
    f <- e$log[e$log$stub == s, ]
    if (nrow(f) != 1L) { p <- c(p, sprintf("el validador %s corrió %d veces y se esperaba 1", s, nrow(f))); next }
    if (!identical(f$arg1, m$rmd_b)) p <- c(p, sprintf("%s recibió «%s» y no la ruta absoluta del .Rmd de B", s, f$arg1))
    if (!identical(f$cwd, file.path(e$run, "work")))
      p <- c(p, sprintf("%s corrió en «%s» y no en work/ de su corrida", s, f$cwd))
  }
  p
}

problemas_t6 <- function(e, con_archivos = TRUE) {
  p <- character(); m <- e$m
  esperado_perfil  <- if (con_archivos) file.path(m$proy, ".Rprofile") else m$perfil_externo
  esperado_environ <- if (con_archivos) file.path(m$proy, ".Renviron") else m$environ_externo
  esperado_humo    <- if (con_archivos) "proyecto-falso" else ""
  for (s in VALIDADORES) {
    f <- e$log[e$log$stub == s, ]
    if (nrow(f) != 1L) { p <- c(p, sprintf("el validador %s corrió %d veces y se esperaba 1", s, nrow(f))); next }
    if (!identical(f$r_profile_user, esperado_perfil))
      p <- c(p, sprintf("%s: R_PROFILE_USER = «%s» y se esperaba «%s»", s, f$r_profile_user, esperado_perfil))
    if (!identical(f$r_environ_user, esperado_environ))
      p <- c(p, sprintf("%s: R_ENVIRON_USER = «%s» y se esperaba «%s»", s, f$r_environ_user, esperado_environ))
    if (!identical(f$humo_rprofile, esperado_humo)) p <- c(p, sprintf("%s: R no cargó el .Rprofile del proyecto", s))
    if (!identical(f$humo_renviron, esperado_humo)) p <- c(p, sprintf("%s: R no cargó el .Renviron del proyecto", s))
  }
  p
}

# T4: un .tex con \pandocbounded{ más nuevo que el .Rmd de B; sin nombrarlo y nombrándolo con texdir=.
escenario_t4 <- function(hook = HOOK, nombrar_texdir = FALSE) {
  m <- nuevo_mundo()
  senuelo <- file.path(m$A, if (nombrar_texdir) file.path("tex_out", "exercise1.tex") else "senuelo.tex")
  dir.create(dirname(senuelo), showWarnings = FALSE)
  writeLines("\\pandocbounded{x}", senuelo); Sys.setFileTime(senuelo, Sys.time() + 10)
  comando <- if (nombrar_texdir) "Rscript -e \"exams::exams2pdf('../B/x.Rmd', texdir = 'tex_out')\"" else render("../B/x.Rmd")
  r <- correr_hook(m, comando, hook = hook)
  list(m = m, r = r, informe = leer_utf8(file.path(m$informes, "x.txt")))
}

problemas_t4 <- function(hook = HOOK) {
  p <- character()
  e <- escenario_t4(hook, nombrar_texdir = FALSE)
  if (contiene(e$informe, "ERROR 16: uso de")) p <- c(p, "el .tex señuelo de A se le atribuyó al ejercicio de B (ERROR 16)")
  if (!is.null(e$r$json$decision)) p <- c(p, "el señuelo bloqueó el render de B")
  if (!contiene(e$informe, "sin salida .tex del render que revisar (el comando no nombra dir=/texdir=)"))
    p <- c(p, "la 2I no declara que no hay .tex del render que revisar")
  e <- escenario_t4(hook, nombrar_texdir = TRUE)
  if (!contiene(e$informe, "ERROR 16: uso de")) p <- c(p, "con texdir= nombrado, el .tex con \\pandocbounded no se revisó")
  if (!identical(e$r$json$decision, "block")) p <- c(p, "con el ERROR 16 del texdir= el hook no bloquea")
  p
}

# T5: ocho corridas viejas sembradas + algo que no es una corrida; el hook añade la novena.
problemas_t5 <- function(hook = HOOK) {
  m <- nuevo_mundo()
  base_x <- file.path(m$informes, "x"); dir.create(base_x)
  viejas <- sprintf("2020010%d-120000-%d", 1:8, 1000L + 1:8)
  for (v in viejas) dir.create(file.path(base_x, v, "work"), recursive = TRUE)
  dir.create(file.path(base_x, "notas")); writeLines("no soy una corrida", file.path(base_x, "leeme.txt"))
  correr_hook(m, render("../B/x.Rmd"), hook = hook)
  nombres <- basename(list.dirs(base_x, recursive = FALSE))
  corridas <- sort(nombres[grepl(CORRIDA_RE, nombres)])
  p <- character()
  if (length(corridas) != 5L) p <- c(p, sprintf("quedan %d corridas y se esperaban 5", length(corridas)))
  if (!all(tail(viejas, 4L) %in% corridas)) p <- c(p, "la poda no conservó las 4 viejas más recientes")
  if (any(head(viejas, 4L) %in% corridas)) p <- c(p, "la poda dejó corridas de las más antiguas")
  if (length(setdiff(corridas, viejas)) != 1L) p <- c(p, "la corrida nueva no sobrevivió a la poda")
  if (!("notas" %in% nombres) || !file.exists(file.path(base_x, "leeme.txt")))
    p <- c(p, "la poda borró algo que no es una corrida")
  p
}

# T7: edir= y texdir= antes que dir=; el PDF de dir= convive con otro PDF más cercano al cwd.
problemas_t7 <- function(hook = HOOK) {
  p <- character()
  for (comillas in c("dobles", "simples")) {
    m <- nuevo_mundo()
    esperado <- file.path(m$A, "out_pdf", "salida.pdf")
    dir.create(dirname(esperado)); writeLines("%PDF-1.4", esperado); dir.create(file.path(m$A, "tex_out"))
    comando <- if (comillas == "dobles")
      "Rscript -e 'exams::exams2pdf(\"../B/x.Rmd\", edir = \"fuente\", texdir = \"tex_out\", dir = \"out_pdf\")'"
    else
      "Rscript -e \"exams::exams2pdf('../B/x.Rmd', edir = 'fuente', texdir = 'tex_out', dir = 'out_pdf')\""
    correr_hook(m, comando, hook = hook)
    usado <- leer_utf8(m$log_magick)
    if (!identical(usado, esperado))
      p <- c(p, sprintf("con comillas %s la 2B previsualizó «%s» y no el PDF de dir=: «%s»",
                        comillas, paste(usado, collapse = ", "), esperado))
  }
  p
}

# T8: el paso 6 de B ya está sellado (un ejercicio que se vuelve a renderizar): la sugerencia lo dice
# antes del mismo comando, porque re-sellar pisa el timestamp; y el hook no toca el estado.
problemas_t8 <- function(hook = HOOK) {
  m <- nuevo_mundo()
  wf <- file.path(m$proy, ".claude", "scripts", "workflow-state.sh")
  for (paso in c("renderizado_4_formatos", "arsenal_post_render"))
    system2("bash", shQuote(c(wf, "complete", m$B, paso)), stdout = FALSE, stderr = FALSE)
  antes <- md5(file.path(m$B, "ejercicio_state.json"))
  r <- correr_hook(m, render("../B/x.Rmd"), hook = hook)
  despues <- md5(file.path(m$B, "ejercicio_state.json"))
  corridas <- list.dirs(file.path(m$informes, "x"), recursive = FALSE)
  ctx <- contexto(r)
  if (is.null(ctx) || length(corridas) != 1L) return("el hook no entregó contexto o no hay una corrida única")
  frase <- "arsenal_post_render ya está sellado; re-sellar solo si este informe sustituye al anterior:"
  comando <- sprintf('complete "%s" arsenal_post_render --nota "informe %s · md5 %s · ',
                     m$B, file.path(corridas, "informe.txt"), md5(m$rmd_b))
  p <- character()
  if (!contiene(ctx, frase)) p <- c(p, "con el paso 6 sellado la sugerencia no lo dice")
  if (!contiene(ctx, comando)) p <- c(p, "falta el mismo comando complete")
  if (contiene(ctx, frase) && contiene(ctx, comando) &&
      regexpr(frase, ctx, fixed = TRUE, useBytes = TRUE)[[1]] > regexpr(comando, ctx, fixed = TRUE, useBytes = TRUE)[[1]])
    p <- c(p, "la frase debe ir antes del comando")
  if (!identical(antes, despues)) p <- c(p, "el hook modificó el estado de B")
  p
}

# El escenario base del hook vivo se corre una vez y lo comparten T1, T2, T3 y T6.
base_vivo <- local({
  e <- NULL
  function() { if (is.null(e)) e <<- escenario_base(HOOK); e }
})

# --- Los casos contra el hook ------------------------------------------------------------------
test_that("T1: la sesión en A no se toca; los artefactos van a la caché", {
  expect_equal(problemas_t1(base_vivo()), character(0))
})

test_that("T2: el complete sugerido apunta a B, con su md5 y la copia estable del informe", {
  expect_equal(problemas_t2(base_vivo()), character(0))
})

test_that("T3: con ruta relativa ../B/x.Rmd el ejercicio es B", {
  expect_equal(problemas_t3(base_vivo()), character(0))
})

test_that("T4: un .tex señuelo bajo A no se atribuye; con texdir= nombrado sí se revisa", {
  expect_equal(problemas_t4(), character(0))
})

test_that("T5: la poda deja 5 corridas y no borra lo que no es una corrida", {
  expect_equal(problemas_t5(), character(0))
})

test_that("T6: los validadores corren con el .Rprofile y el .Renviron del proyecto, solo si existen", {
  expect_equal(problemas_t6(base_vivo()), character(0))
  sin_archivos <- escenario_base(HOOK, perfil_r = FALSE)   # control: sin esos archivos no se fija nada
  expect_equal(problemas_t6(sin_archivos, con_archivos = FALSE), character(0))
})

test_that("T7: la 2B toma el dir= correcto aunque edir= o texdir= vayan antes", {
  expect_equal(problemas_t7(), character(0))
})

test_that("T8: con arsenal_post_render ya sellado, la sugerencia lo dice antes del mismo comando", {
  expect_equal(problemas_t8(), character(0))
})

# --- cd y setwd dentro del comando (T9-T16) ----------------------------------------------------
# La sesión está en la raíz del proyecto falso y el comando cambia de directorio antes de renderizar:
# `cd A-Produccion/.../B/salida && Rscript -e 'exams::exams2html("../x.Rmd")'`. Hasta el 2026-10-31 el
# hook resolvía la ruta relativa contra el cwd de la sesión: no hallaba el .Rmd, avisaba «no existe» y
# el arsenal no corría; el PDF y el dir=/texdir= también los buscaba desde allí.
REL_B <- file.path("A-Produccion", "01-En-PreDesarrollo", "B")

rscript_e <- function(codigo, comilla = "'") paste0("Rscript -e ", comilla, codigo, comilla)

# Una corrida del hook en un mundo nuevo. `comando` y `sesion` son funciones del mundo porque llevan
# sus rutas; por defecto la sesión está en la raíz del proyecto falso.
en_mundo_nuevo <- function(comando, hook = HOOK, sesion = function(m) m$proy, preparar = function(m) NULL) {
  m <- nuevo_mundo(); preparar(m)
  list(m = m, r = correr_hook(m, comando(m), cwd = sesion(m), hook = hook))
}
preparar_salida <- function(m) invisible(dir.create(file.path(m$B, "salida")))

# El arsenal corrió sobre el .Rmd de B: sin aviso de «no existe», y con su ruta absoluta y su md5 en
# ORIGEN.txt, en los validadores y en el `complete` sugerido.
problemas_ejercicio_b <- function(e) {
  m <- e$m; ctx <- contexto(e$r); p <- character()
  if (!identical(e$r$ec, 0L)) p <- c(p, sprintf("el hook salió con %s y no con 0", e$r$ec))
  if (is.null(ctx)) return(c(p, "el hook no entregó additionalContext"))
  if (contiene(ctx, "pero ese archivo no existe")) p <- c(p, "el hook avisa que no halla el .Rmd: el arsenal no corrió")
  corridas <- list.dirs(file.path(m$informes, "x"), recursive = FALSE)
  if (length(corridas) != 1L) return(c(p, sprintf("se esperaba 1 corrida de x en la caché y hay %d", length(corridas))))
  origen <- leer_utf8(file.path(corridas, "ORIGEN.txt"))
  if (!identical(origen, c(paste0("rmd: ", m$rmd_b), paste0("md5: ", md5(m$rmd_b)))))
    p <- c(p, "ORIGEN.txt no identifica el .Rmd de B (ruta absoluta y md5)")
  log <- leer_log(m$log)
  for (s in VALIDADORES) {
    f <- log[log$stub == s, ]
    if (nrow(f) != 1L) { p <- c(p, sprintf("el validador %s corrió %d veces y se esperaba 1", s, nrow(f))); next }
    if (!identical(f$arg1, m$rmd_b)) p <- c(p, sprintf("%s recibió «%s» y no la ruta absoluta del .Rmd de B", s, f$arg1))
  }
  if (!contiene(ctx, sprintf('complete "%s" arsenal_post_render', m$B))) p <- c(p, "el complete sugerido no apunta a B")
  p
}

problemas_variantes <- function(variantes, hook, cuales) {
  p <- character()
  for (v in names(variantes[cuales])) {
    pp <- problemas_ejercicio_b(en_mundo_nuevo(variantes[[v]], hook, preparar = preparar_salida))
    if (length(pp)) p <- c(p, paste0(v, ": ", pp))
  }
  p
}

# T9: cd. El .Rmd de B es relativo al directorio al que llega el comando.
VARIANTES_T9 <- list(
  "cd a la carpeta del .Rmd" =
    function(m) paste0("cd ", REL_B, " && ", render("x.Rmd")),
  "cd a una subcarpeta y ../x.Rmd (Rscript --no-init-file)" =
    function(m) paste0("cd ", REL_B, "/salida && ", sub("^Rscript ", "Rscript --no-init-file ", render("../x.Rmd"))),
  "varios cd: ; && comillas dobles, simples y --" =
    function(m) paste0("cd A-Produccion; cd -- \"01-En-PreDesarrollo\" && cd 'B' && ", render("x.Rmd"))
)
problemas_t9 <- function(hook = HOOK, cuales = seq_along(VARIANTES_T9)) problemas_variantes(VARIANTES_T9, hook, cuales)

# T10: setwd() dentro del código R.
VARIANTES_T10 <- list(
  "setwd() con comillas dobles" =
    function(m) rscript_e(sprintf('setwd("%s"); exams::exams2html("x.Rmd")', REL_B)),
  "setwd() con comillas simples, en un Rscript -e entre comillas dobles" =
    function(m) rscript_e(sprintf("setwd('%s/salida'); exams::exams2html('../x.Rmd')", REL_B), '"'),
  "cd y luego setwd() relativo a ese cd" =
    function(m) paste0("cd A-Produccion && ", rscript_e('setwd("01-En-PreDesarrollo/B"); exams::exams2html("x.Rmd")'))
)
problemas_t10 <- function(hook = HOOK, cuales = seq_along(VARIANTES_T10)) problemas_variantes(VARIANTES_T10, hook, cuales)

# T11 y T12: un .Rmd relativo que no está ni en el directorio del comando ni en el de la sesión. La sesión
# está en `otro`, que no es prefijo de B ni al revés: cada directorio solo puede aparecer si el aviso lo nombra.
escenarios_inexistente <- function(hook = HOOK) {
  una <- function(con_cd) en_mundo_nuevo(
    function(m) if (con_cd) paste0("cd ", shQuote(m$B), " && ", render("no_esta.Rmd")) else render("no_esta.Rmd"),
    hook, sesion = function(m) m$otro)
  list(con_cd = una(TRUE), sin_cd = una(FALSE))
}

problemas_t11 <- function(es) {
  p <- character()
  for (n in names(es)) {
    e <- es[[n]]; ctx <- contexto(e$r)
    if (!identical(e$r$ec, 0L)) p <- c(p, sprintf("%s: el hook salió con %s y no con 0", n, e$r$ec))
    if (is.null(ctx)) { p <- c(p, sprintf("%s: el hook salió callado", n)); next }
    if (!contiene(ctx, "NO corrió")) p <- c(p, sprintf("%s: el aviso no dice que el arsenal NO corrió", n))
    if (!contiene(ctx, "no_esta.Rmd")) p <- c(p, sprintf("%s: el aviso no nombra el .Rmd", n))
    if (!is.null(e$r$json$decision)) p <- c(p, sprintf("%s: un .Rmd inexistente no debe bloquear", n))
    if (dir.exists(file.path(e$m$informes, "no_esta"))) p <- c(p, sprintf("%s: el arsenal corrió sobre un .Rmd que no existe", n))
  }
  p
}

problemas_t12 <- function(es) {
  p <- character()
  con <- contexto(es$con_cd$r); sin <- contexto(es$sin_cd$r)
  if (is.null(con) || is.null(sin)) return("el hook salió callado")
  if (!contiene(con, es$con_cd$m$B)) p <- c(p, "con cd, el aviso no nombra el directorio al que llega el comando")
  if (!contiene(con, es$con_cd$m$otro)) p <- c(p, "con cd, el aviso no nombra el cwd de la sesión")
  if (!contiene(sin, es$sin_cd$m$otro)) p <- c(p, "sin cd, el aviso no nombra el cwd de la sesión")
  if (contiene(sin, "directorio al que llega el comando")) p <- c(p, "sin cd, el aviso habla de un directorio del comando")
  p
}

inexistente_vivo <- local({
  e <- NULL
  function() { if (is.null(e)) e <<- escenarios_inexistente(HOOK); e }
})

# T13: el cd va en un subshell, así que el render sigue en la carpeta de la sesión (A), pero el recorrido
# lo sigue hasta `otro`. El .Rmd no está ahí: el respaldo lo busca contra A, y la 2B busca el PDF en A.
problemas_t13 <- function(hook = HOOK) {
  e <- en_mundo_nuevo(function(m) sprintf("(cd %s && ls > /dev/null); %s", shQuote(m$otro), render("../B/x.Rmd")),
                      hook, sesion = function(m) m$A)
  p <- problemas_ejercicio_b(e)
  usado <- leer_utf8(e$m$log_magick); esperado <- file.path(e$m$A, "render.pdf")
  if (!identical(usado, esperado))
    p <- c(p, sprintf("la 2B previsualizó «%s» y no el PDF de la sesión «%s»", paste(usado, collapse = ", "), esperado))
  p
}

# T14: las salidas del render, desde el directorio al que llega el comando. En la raíz (la sesión) hay un
# PDF señuelo; en B, el PDF de dir = "out_pdf", el PDF que deja un render sin dir= y un .tex con
# \pandocbounded{ más nuevo que el .Rmd en tex_out.
COMANDOS_T14 <- list(
  dir     = function(m) paste0("cd ", REL_B, " && ", rscript_e('exams::exams2pdf("x.Rmd", dir = "out_pdf")')),
  defecto = function(m) paste0("cd ", REL_B, " && ", rscript_e('exams::exams2pdf("x.Rmd")')),
  texdir  = function(m) paste0("cd ", REL_B, " && ", rscript_e("exams::exams2pdf('x.Rmd', texdir = 'tex_out')", '"'))
)

preparar_t14 <- function(m) {
  preparar_salida(m)
  pdf <- function(...) {
    f <- file.path(...); dir.create(dirname(f), showWarnings = FALSE); writeLines("%PDF-1.4", f)
  }
  pdf(m$B, "out_pdf", "salida.pdf"); pdf(m$B, "default.pdf"); pdf(m$proy, "decoy.pdf")
  tex <- file.path(m$B, "tex_out", "exercise1.tex")
  dir.create(dirname(tex)); writeLines("\\pandocbounded{x}", tex); Sys.setFileTime(tex, Sys.time() + 10)
}

problemas_t14 <- function(hook = HOOK, cuales = names(COMANDOS_T14)) {
  p <- character()
  for (v in cuales) {
    e <- en_mundo_nuevo(COMANDOS_T14[[v]], hook, preparar = preparar_t14); m <- e$m
    if (v == "texdir") {
      if (!contiene(leer_utf8(file.path(m$informes, "x.txt")), "ERROR 16: uso de"))
        p <- c(p, "texdir: el .tex de texdir= no se revisó desde el directorio del comando")
      if (!identical(e$r$json$decision, "block")) p <- c(p, "texdir: con el ERROR 16 el hook no bloquea")
    } else {
      esperado <- if (v == "dir") file.path(m$B, "out_pdf", "salida.pdf") else file.path(m$B, "default.pdf")
      usado <- leer_utf8(m$log_magick)
      if (!identical(usado, esperado))
        p <- c(p, sprintf("%s: la 2B previsualizó «%s» y se esperaba «%s»", v, paste(usado, collapse = ", "), esperado))
    }
  }
  p
}

# T15: tras un cd literal, `cd "$PWD"` no se adivina: queda B (que es lo que hace de verdad ese cd).
problemas_t15 <- function(hook = HOOK) {
  problemas_ejercicio_b(en_mundo_nuevo(function(m) paste0("cd ", REL_B, " && cd \"$PWD\" && ", render("x.Rmd")), hook))
}

# T16: el recorrido solo, sin el resto del hook. Se extrae del propio hook (el bloque entre
# `IFS= read -r -d '' PY_CWD_EFECTIVO <<'PY'` y la línea `PY`) y se corre con Python como lo corre el hook:
# una copia aquí podría divergir sin que nada lo notara. Sesión en /sesion y HOME=/casa.
extraer_recorrido <- function(hook = HOOK) {
  l <- readLines(hook, warn = FALSE, encoding = "UTF-8")
  ini <- grep("^IFS= read -r -d '' PY_CWD_EFECTIVO <<'PY'$", l)
  if (length(ini) != 1L || ini == length(l)) return(NULL)
  fin <- ini + match("PY", l[(ini + 1L):length(l)])
  if (is.na(fin)) return(NULL)
  l[(ini + 1L):(fin - 1L)]
}

directorio_del_comando <- function(py, comando) {
  f <- tempfile(fileext = ".py"); writeLines(py, f, useBytes = TRUE)
  suppressWarnings(system2("python3", c("-I", shQuote(f), shQuote("/sesion")), input = comando,
                           stdout = TRUE, stderr = FALSE, env = "HOME=/casa"))
}

R_X <- render("x.Rmd")
CASOS_T16 <- list(
  # nada que seguir; un cd posterior al render no cuenta
  c(R_X, "/sesion"),
  c(paste0(R_X, " && cd a"), "/sesion"),
  # cd: las formas
  c(paste0("cd a && ", R_X), "/sesion/a"),
  c(paste0("cd \"a b\" && ", R_X), "/sesion/a b"),
  c(paste0("cd 'a b' && ", R_X), "/sesion/a b"),
  c(paste0("cd a\\ b && ", R_X), "/sesion/a b"),
  c(paste0("cd -- a && ", R_X), "/sesion/a"),
  c(paste0("cd -P a && ", R_X), "/sesion/a"),
  c(paste0("cd a/ && ", R_X), "/sesion/a"),
  c(paste0("cd a 2>/dev/null && ", R_X), "/sesion/a"),
  c(paste0("cd a || exit 1; ", R_X), "/sesion/a"),
  c(paste0("cd a\n", R_X), "/sesion/a"),
  c(paste0("cd ~/x && ", R_X), "/casa/x"),
  # encadenados: cada uno contra el anterior; el primero, contra la sesión
  c(paste0("cd a; cd b; ", R_X), "/sesion/a/b"),
  c(paste0("cd a && cd ../c && ", R_X), "/sesion/c"),
  c(paste0("cd /abs && cd rel && ", R_X), "/abs/rel"),
  c(paste0("cd a\ncd b\n", R_X), "/sesion/a/b"),
  # lo que no se resuelve de forma literal no se adivina: queda el directorio de antes
  c(paste0("cd \"$S\" && ", R_X), "/sesion"),
  c(paste0("cd a && cd \"$S\" && ", R_X), "/sesion/a"),
  c(paste0("cd a && cd $(pwd)/b && ", R_X), "/sesion/a"),
  c(paste0("cd a && cd - && ", R_X), "/sesion/a"),
  c(paste0("cd a/* && ", R_X), "/sesion"),
  c(paste0("cd ~otro/x && ", R_X), "/sesion"),
  c(paste0("cd \"~/x\" && ", R_X), "/sesion"),
  c(paste0("cd \"a\"/b && ", R_X), "/sesion"),
  # lo que no está en posición de comando no es un cd
  c(paste0("echo \"x; cd foo\" && ", R_X), "/sesion"),
  c(paste0("echo 'cd foo' && ", R_X), "/sesion"),
  c(paste0("abcd a && ", R_X), "/sesion"),
  # setwd
  c(rscript_e('setwd("a"); exams::exams2html("x.Rmd")'), "/sesion/a"),
  c(rscript_e("setwd('a'); exams::exams2html('x.Rmd')", '"'), "/sesion/a"),
  c(rscript_e('setwd(dir = "a"); exams::exams2html("x.Rmd")'), "/sesion/a"),
  c(rscript_e('setwd("/abs/a"); exams::exams2html("x.Rmd")'), "/abs/a"),
  c(rscript_e('setwd("a"); setwd("b"); exams::exams2html("x.Rmd")'), "/sesion/a/b"),
  c(rscript_e('setwd(dirname(f)); exams::exams2html("x.Rmd")'), "/sesion"),
  c(rscript_e('exams::exams2html("x.Rmd"); setwd("a")'), "/sesion"),
  c(paste0("cd a && ", rscript_e('setwd("b"); exams::exams2html("x.Rmd")')), "/sesion/a/b"),
  c(paste0("cd a/b && ", rscript_e('setwd(".."); exams::exams2html("x.Rmd")')), "/sesion/a")
)

problemas_t16 <- function(hook = HOOK) {
  py <- extraer_recorrido(hook)
  if (is.null(py)) return("el hook no trae el recorrido del comando (el bloque PY_CWD_EFECTIVO)")
  p <- character()
  for (caso in CASOS_T16) {
    obtenido <- directorio_del_comando(py, caso[1])
    if (!identical(obtenido, caso[2]))
      p <- c(p, sprintf("«%s» dio «%s» y se esperaba «%s»", gsub("\n", "\\n", caso[1], fixed = TRUE),
                        paste(obtenido, collapse = "|"), caso[2]))
  }
  p
}

test_that("T9: cd <dir> && … exams2html(\"rel.Rmd\"): el hook halla el .Rmd y el ejercicio es el suyo", {
  expect_equal(problemas_t9(), character(0))
})

test_that("T10: setwd(\"<dir>\") dentro del código R: el hook halla el .Rmd y el ejercicio es el suyo", {
  expect_equal(problemas_t10(), character(0))
})

test_that("T11: un .Rmd relativo que no está en ninguno de los dos directorios sigue avisando", {
  expect_equal(problemas_t11(inexistente_vivo()), character(0))
})

test_that("T12: ese aviso nombra los dos directorios si difieren, y solo el de la sesión si no", {
  expect_equal(problemas_t12(inexistente_vivo()), character(0))
})

test_that("T13: respaldo: un cd que no cambia el directorio real se resuelve contra el cwd de la sesión", {
  expect_equal(problemas_t13(), character(0))
})

test_that("T14: el PDF y el .tex del render se buscan desde el directorio al que llega el comando", {
  expect_equal(problemas_t14(), character(0))
})

test_that("T15: un cd que no se resuelve de forma literal conserva el directorio anterior", {
  expect_equal(problemas_t15(), character(0))
})

test_that("T16: el recorrido del comando, forma por forma de cd y de setwd", {
  expect_equal(problemas_t16(), character(0))
})

# --- Los mutantes ------------------------------------------------------------------------------
# Texto literal, sin expresiones regulares: el `old` debe aparecer exactamente `veces` veces.
aplicar <- function(txt, old, new, veces = 1L) {
  partes <- strsplit(paste0(txt, "\001FIN"), old, fixed = TRUE)[[1]]
  if (length(partes) - 1L != veces)
    stop(sprintf("el mutante no se aplica: «%s» aparece %d veces y se esperaban %d (¿cambió el hook?)",
                 old, length(partes) - 1L, veces), call. = FALSE)
  sub("\001FIN$", "", paste(partes, collapse = new))
}

ANCLA_SELLO <- 'WORKFLOW_SCRIPT="$PROJECT_DIR/.claude/scripts/workflow-state.sh"'
SELLO_V62 <- paste0("\n  if [ -f \"$WORKFLOW_SCRIPT\" ] && [ $ERRORES_TOTALES -eq 0 ]; then",
                    "\n    bash \"$WORKFLOW_SCRIPT\" complete \"$EJ_DIR\" renderizado_4_formatos 2>/dev/null",
                    "\n    bash \"$WORKFLOW_SCRIPT\" complete \"$EJ_DIR\" arsenal_post_render 2>/dev/null",
                    "\n  fi")
PDF_DIR_V62 <- r"---(PDF_DIR=$(echo "$COMMAND" | grep -oP 'dir\s*=\s*"[^"]*"' | head -1 | grep -oP '"[^"]*"' | tr -d '"'))---"

caso_base <- function(problemas) function(hook) problemas(escenario_base(hook))

MUTANTES <- list(
  M1 = list(desc = "estado de vuelta a $CWD, con complete (el defecto de la v6.2)", caso = "T1",
            muta = function(t) aplicar(aplicar(t, 'EJ_DIR=$(dirname "$RMD_ABS")', 'EJ_DIR="$CWD"'),
                                       ANCLA_SELLO, paste0(ANCLA_SELLO, SELLO_V62)),
            caso_f = caso_base(problemas_t1)),
  M2 = list(desc = "las dos llamadas a complete, sobre la carpeta de B", caso = "T1",
            muta = function(t) aplicar(t, ANCLA_SELLO, paste0(ANCLA_SELLO, SELLO_V62)),
            caso_f = caso_base(problemas_t1)),
  M3a = list(desc = "vistas previas en $CWD", caso = "T1",
             muta = function(t) aplicar(t, 'PREVIEW_PNG="$RUN_DIR/preview/preview_${RMD_BASENAME}.png"',
                                        'PREVIEW_PNG="$CWD/preview_${RMD_BASENAME}.png"'),
             caso_f = caso_base(problemas_t1)),
  M3b = list(desc = "stress test en $CWD", caso = "T1",
             muta = function(t) aplicar(t, 'STRESS_OUTPUT_DIR="$RUN_DIR/stress_test_output"',
                                        'STRESS_OUTPUT_DIR="$CWD/stress_test_output"'),
             caso_f = caso_base(problemas_t1)),
  M4 = list(desc = 'find "$CWD" -maxdepth 4 en la 2I', caso = "T4",
            muta = function(t) aplicar(t, 'TEX_FILES=$(find "${TEX_DIRS[@]}" -maxdepth 2 -name "*.tex" -newer "$RMD_ABS" 2>/dev/null)',
                                       'TEX_FILES=$(find "$CWD" -maxdepth 4 -name "*.tex" -newer "$RMD_ABS" 2>/dev/null)'),
            caso_f = problemas_t4),
  M5 = list(desc = 'validadores con cd "$CWD"', caso = "T1",
            muta = function(t) aplicar(t, 'cd "$RUN_DIR/work" && env "${ENV_R[@]}" Rscript',
                                       'cd "$CWD" && env "${ENV_R[@]}" Rscript', veces = 4L),
            caso_f = caso_base(problemas_t1)),
  M6 = list(desc = "sin R_ENVIRON_USER", caso = "T6",
            muta = function(t) aplicar(t, '[ -f "$PROJECT_DIR/.Renviron" ] && ENV_R+=("R_ENVIRON_USER=$PROJECT_DIR/.Renviron")', ":"),
            caso_f = caso_base(problemas_t6)),
  M7 = list(desc = "dir= sin borde de palabra (la 2B de la v6.2)", caso = "T7",
            muta = function(t) aplicar(t, "PDF_DIR=$(valores_literales_del_comando dir | sed -n '1p')", PDF_DIR_V62),
            caso_f = problemas_t7),
  M8 = list(desc = "los validadores reciben la ruta relativa, no RMD_ABS", caso = "T3",
            muta = function(t) aplicar(t, 'Rscript "$SCRIPT_MATH" "$RMD_ABS"', 'Rscript "$SCRIPT_MATH" "$RMD_FILE"'),
            caso_f = caso_base(problemas_t3)),
  M9 = list(desc = "sin poda de corridas", caso = "T5",
            muta = function(t) aplicar(t, 'if [ "$N_VIEJAS" -gt 0 ]; then', "if false; then"),
            caso_f = problemas_t5),
  M10 = list(desc = "la nota cita <nombre>.txt, no la copia de la corrida", caso = "T2",
             muta = function(t) aplicar(t, "informe $RUN_DIR/informe.txt · md5", "informe $INFORME · md5"),
             caso_f = caso_base(problemas_t2)),
  M11 = list(desc = "siempre dice «ya está sellado»", caso = "T2",
             muta = function(t) aplicar(t, 'if bash "$WORKFLOW_SCRIPT" check "$EJ_DIR" arsenal_post_render >/dev/null 2>&1; then', "if true; then"),
             caso_f = caso_base(problemas_t2)),
  M12 = list(desc = "nunca dice «ya está sellado»", caso = "T8",
             muta = function(t) aplicar(t, 'if bash "$WORKFLOW_SCRIPT" check "$EJ_DIR" arsenal_post_render >/dev/null 2>&1; then', "if false; then"),
             caso_f = problemas_t8),
  M13 = list(desc = "sin recorrido: el directorio efectivo es siempre el de la sesión", caso = "T9",
             muta = function(t) aplicar(t, 'CWD_EFECTIVO=$(python3 -I -c "$PY_CWD_EFECTIVO" "$CWD" <<< "$COMMAND" 2>/dev/null)',
                                        'CWD_EFECTIVO=""'),
             caso_f = function(hook) problemas_t9(hook, 1L)),
  M14 = list(desc = "el recorrido ignora los cd", caso = "T9",
             muta = function(t) aplicar(t, "for m in CD.finditer(antes):", "for m in []:"),
             caso_f = function(hook) problemas_t9(hook, 1L)),
  M15 = list(desc = "el recorrido ignora los setwd", caso = "T10",
             muta = function(t) aplicar(t, "for m in SETWD.finditer(antes):", "for m in []:"),
             caso_f = function(hook) problemas_t10(hook, 1L)),
  M16 = list(desc = "sin respaldo contra el cwd de la sesión", caso = "T13",
             muta = function(t) aplicar(t, '  RMD_PATH="$CWD/$RMD_FILE"\n  [ -f "$RMD_PATH" ] && CWD_EFECTIVO="$CWD"',
                                        '  RMD_PATH="$CWD_EFECTIVO/$RMD_FILE"'),
             caso_f = problemas_t13),
  M17 = list(desc = "el respaldo no devuelve las salidas al cwd de la sesión", caso = "T13",
             muta = function(t) aplicar(t, '[ -f "$RMD_PATH" ] && CWD_EFECTIVO="$CWD"', ":"),
             caso_f = problemas_t13),
  M18 = list(desc = "2B: el dir= relativo, desde $CWD", caso = "T14",
             muta = function(t) aplicar(t, 'SEARCH_DIRS+=("$CWD_EFECTIVO/$PDF_DIR")', 'SEARCH_DIRS+=("$CWD/$PDF_DIR")'),
             caso_f = function(hook) problemas_t14(hook, "dir")),
  M19 = list(desc = "2B: output_pdf, output y el propio directorio, desde $CWD", caso = "T14",
             muta = function(t) aplicar(t, 'SEARCH_DIRS+=("$CWD_EFECTIVO/output_pdf" "$CWD_EFECTIVO/output_pdf_test" "$CWD_EFECTIVO/output" "$CWD_EFECTIVO")',
                                        'SEARCH_DIRS+=("$CWD/output_pdf" "$CWD/output_pdf_test" "$CWD/output" "$CWD")'),
             caso_f = function(hook) problemas_t14(hook, "defecto")),
  M20 = list(desc = "2I: el texdir= relativo, desde $CWD", caso = "T14",
             muta = function(t) aplicar(t, '[[ "$D" = /* ]] || D="$CWD_EFECTIVO/$D"', '[[ "$D" = /* ]] || D="$CWD/$D"'),
             caso_f = function(hook) problemas_t14(hook, "texdir")),
  M21 = list(desc = "el aviso nombra un solo directorio", caso = "T12",
             muta = function(t) aplicar(t, 'if [[ "$RMD_FILE" = /* ]] || [ "$CWD_EFECTIVO" = "$CWD" ]; then', "if true; then"),
             caso_f = function(hook) problemas_t12(escenarios_inexistente(hook))),
  M22 = list(desc = 'cd "$PWD" tomado como un directorio literal', caso = "T15",
             muta = function(t) aplicar(t, 'if not texto or texto == "-" or "$" in texto or "`" in texto:', "if not texto:"),
             caso_f = problemas_t15)
)

# Copia mutada del hook vivo en tempdir().
escribir_mutante <- function(mu) {
  txt <- paste(readLines(HOOK, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  copia <- tempfile(fileext = ".sh")
  writeLines(mu$muta(txt), copia, useBytes = TRUE)
  copia
}

for (nombre in names(MUTANTES)) local({
  mu <- MUTANTES[[nombre]]; nombre <- nombre
  test_that(sprintf("%s muere (%s): lo caza %s", nombre, mu$desc, mu$caso), {
    skip_if(HOOK_EXTERNO, "HOOK_CWD apunta a otra copia: los mutantes se aplican sobre el hook vivo")
    copia <- escribir_mutante(mu)
    expect_false(identical(readLines(copia, warn = FALSE), readLines(HOOK, warn = FALSE)),
                 info = "el mutante quedó idéntico al hook")
    problemas <- mu$caso_f(copia)
    expect_gt(length(problemas), 0L, label = paste0("problemas que ", mu$caso, " halla en ", nombre))
  })
})
