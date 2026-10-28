#!/usr/bin/env Rscript
# =============================================================================
# sincronizar_doc_suites.R: mantiene al día, desde el runner, la parte del
# documento de cobertura que se deriva de él (tabla de suites, cifras y frase
# del modo rápido).
#
# Por qué existe: .claude/docs/VALIDATOR_COBERTURA_ICFES.md decía «19 suites»
# cuando tests/run_all_tests.R ya tenía 42. Una cifra copiada a mano en prosa se
# desactualiza sin que nada lo note. Aquí esa parte del documento se GENERA y la
# guardia tests/testthat/test_doc_suites_sincronizada.R falla si diverge.
#
# Uso (desde cualquier directorio; la raíz del repo sale de la ubicación del script):
#   Rscript tests/sincronizar_doc_suites.R           reescribe el bloque generado
#   Rscript tests/sincronizar_doc_suites.R --check   no escribe; sale con 1 e imprime
#                                                    las diferencias si no coincide
#
# Variables de entorno (por defecto, las rutas reales del repo):
#   DOC_COBERTURA   documento que lleva el bloque
#   RUNNER_SUITES   archivo que define `suites <- list(...)`
#
# Código de salida: 0 = al día (o reescrito) · 1 = --check halló diferencias ·
#                   2 = uso incorrecto o error de lectura/escritura.
#
# El runner NO se ejecuta: se parsea y se evalúa SOLO la asignación de nivel
# superior a `suites`, en un entorno limpio. Los archivos de las suites se
# resuelven contra la raíz del repo (también si RUNNER_SUITES apunta a una copia).
#
# El bloque va entre dos marcadores HTML (MARCA_INICIO / MARCA_FIN). La primera
# vez, si no existen, reemplaza la tabla y la frase del modo rápido escritas a mano.
#
# Con source() el script solo define funciones (leer_suites(), contar_test_that(),
# generar_bloque(), comparar(), reescribir(), main()...): la CLI corre únicamente
# cuando se lo invoca con Rscript, y así el test de guardia las usa sin efectos.
# =============================================================================

# --- Constantes ----------------------------------------------------------------
# Las cadenas con tilde van en UTF-8 literal y el bloque se compara carácter a carácter
# con el documento: la CLI exige una locale UTF-8 (asegurar_utf8(), más abajo).
MARCA_INICIO <- "<!-- SUITES:INICIO — generado por tests/sincronizar_doc_suites.R; no editar a mano -->"
MARCA_FIN <- "<!-- SUITES:FIN -->"
PREFIJO_INICIO <- "<!-- SUITES:INICIO"
PREFIJO_FIN <- "<!-- SUITES:FIN"
PREFIJO_CIFRAS <- "Cifras (derivadas del runner):"
PREFIJO_FRASE <- "En modo rápido"
PREFIJO_TABLA <- "| # | Archivo |"
ENCABEZADO_TABLA <- c("| # | Archivo | Qué valida |", "|---|---------|-----------|")
COMANDO_SINCRONIZAR <- "Rscript tests/sincronizar_doc_suites.R"

# --- Rutas ---------------------------------------------------------------------
raiz_repo <- function() {
  # Bajo Rscript el script vive en <raiz>/tests/: de ahí sale la raíz, sin depender del cwd.
  f <- sub("^--file=", "", grep("^--file=.*sincronizar_doc_suites\\.R$", commandArgs(FALSE), value = TRUE))
  if (length(f) == 1L && file.exists(f)) return(normalizePath(file.path(dirname(f), "..")))
  # Con source() (el test): la raíz del repositorio donde se está parado.
  r <- tryCatch(suppressWarnings(system2("git", c("rev-parse", "--show-toplevel"), stdout = TRUE, stderr = FALSE)),
                error = function(e) character(0))
  if (length(r) == 1L && nzchar(r) && dir.exists(r)) return(r)
  normalizePath(getwd())
}

ruta_de_entorno <- function(variable, por_defecto) {
  v <- Sys.getenv(variable, "")
  if (nzchar(v)) v else file.path(raiz_repo(), por_defecto)
}

ruta_doc <- function() ruta_de_entorno("DOC_COBERTURA", file.path(".claude", "docs", "VALIDATOR_COBERTURA_ICFES.md"))
ruta_runner <- function() ruta_de_entorno("RUNNER_SUITES", file.path("tests", "run_all_tests.R"))

leer_lineas <- function(ruta) {
  if (!file.exists(ruta)) stop("no existe el archivo: ", ruta, call. = FALSE)
  readLines(ruta, warn = FALSE, encoding = "UTF-8")
}

# --- Lectura del runner ----------------------------------------------------------
# Devuelve un data.frame con una fila por suite, en el orden del runner:
#   nombre, archivo y con_watch (TRUE si la suite declara el campo `watch`).
# El resto del runner (setwd, library, system, quit...) no se ejecuta.
leer_suites <- function(runner = ruta_runner()) {
  if (!file.exists(runner)) stop("no existe el runner: ", runner, call. = FALSE)
  exprs <- parse(runner, keep.source = FALSE, encoding = "UTF-8")
  asigna_suites <- function(e) {
    is.call(e) && length(e) == 3L &&
      (identical(e[[1L]], as.name("<-")) || identical(e[[1L]], as.name("="))) &&
      identical(e[[2L]], as.name("suites"))
  }
  hay <- which(vapply(exprs, asigna_suites, logical(1)))
  if (length(hay) != 1L)
    stop("se esperaba UNA asignación de nivel superior a `suites` en ", runner, " y hay ", length(hay), call. = FALSE)
  amb <- new.env(parent = baseenv())
  eval(exprs[[hay]], amb)
  lista <- amb$suites
  if (!is.list(lista) || !length(lista))
    stop("`suites` no es una lista con al menos una suite en ", runner, call. = FALSE)
  filas <- lapply(seq_along(lista), function(i) {
    s <- lista[[i]]
    cadena <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
    if (!is.list(s) || !cadena(s[["nombre"]]) || !cadena(s[["archivo"]]))
      stop("la suite ", i, " del runner no tiene `nombre` y `archivo` (cadenas de longitud 1)", call. = FALSE)
    # Igual que el runner: «sin watch» es is.null(suite$watch) (aquí con [[ ]], sin coincidencia parcial).
    data.frame(nombre = s[["nombre"]], archivo = s[["archivo"]], con_watch = !is.null(s[["watch"]]),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, filas)
}

# Líneas `^\s*test_that\(` de los archivos de las suites (cada archivo, una vez).
contar_test_that <- function(archivos, raiz = raiz_repo()) {
  absoluta <- grepl("^(/|[A-Za-z]:)", archivos)
  rutas <- unique(ifelse(absoluta, archivos, file.path(raiz, archivos)))
  faltan <- rutas[!file.exists(rutas)]
  if (length(faltan))
    stop("archivos de suites que no existen: ", paste(faltan, collapse = ", "), call. = FALSE)
  as.integer(sum(vapply(rutas, function(r)
    sum(grepl("^\\s*test_that\\(", readLines(r, warn = FALSE), useBytes = TRUE)), numeric(1))))
}

# --- Generación del bloque -------------------------------------------------------
pl <- function(n, uno, varios) paste(n, if (n == 1L) uno else varios)

unir_y <- function(x) {
  x <- as.character(x)
  if (length(x) <= 1L) return(x)
  paste0(paste(x[-length(x)], collapse = ", "), " y ", x[length(x)])
}

# `_` va escapado (\_) como en el resto del documento; `|` rompería la tabla.
escapar_md <- function(x) gsub("|", "\\|", gsub("_", "\\_", x, fixed = TRUE), fixed = TRUE)

# Las suites sin `watch` corren siempre en modo rápido; las demás, solo si cambió algo que vigilan.
frase_modo_rapido <- function(n, sin_watch) {
  k <- length(sin_watch)
  intro <- "En modo rápido (`R_TESTS_QUICK=1`, el del pre-push)"
  vigilan <- "corren solo si cambió algún archivo que vigilan."
  if (k == 0L) return(paste0(intro, " ninguna suite corre siempre: todas ", vigilan))
  siempre <- if (k == 1L) {
    paste0(" corre siempre la única suite sin `watch` (", sin_watch, ").")
  } else {
    paste0(" corren siempre las ", k, " suites sin `watch` (", unir_y(sin_watch), ").")
  }
  resto <- n - k
  cola <- if (resto == 0L) {
    ""
  } else if (resto == 1L) {
    " La otra corre solo si cambió algún archivo que vigila."
  } else {
    paste0(" Las otras ", resto, " ", vigilan)
  }
  paste0(intro, siempre, cola)
}

# El bloque completo, línea por línea: marcador, cifras, tabla, frase y marcador.
# La numeración es POSICIONAL (1..N): no depende de cómo llegó el data.frame.
generar_bloque <- function(suites, n_test_that) {
  n <- nrow(suites)
  if (n == 0L) stop("el runner no define ninguna suite", call. = FALSE)
  pos <- seq_len(n)
  sin_watch <- pos[!suites$con_watch]
  cifras <- paste0(PREFIJO_CIFRAS, " ", pl(n, "suite", "suites"), ", ", pl(n_test_that, "bloque", "bloques"),
                   " `test_that` y ", pl(length(sin_watch), "suite", "suites"), " sin `watch`.")
  filas <- paste0("| ", pos, " | `", basename(suites$archivo), "` | ", escapar_md(suites$nombre), " |")
  c(MARCA_INICIO, cifras, "", ENCABEZADO_TABLA, filas, "", frase_modo_rapido(n, sin_watch), MARCA_FIN)
}

# --- Comparación -----------------------------------------------------------------
# Posición de los marcadores en el documento; `error` explica cualquier anomalía.
localizar_marcadores <- function(doc) {
  ini <- which(startsWith(doc, PREFIJO_INICIO))
  fin <- which(startsWith(doc, PREFIJO_FIN))
  error <- NULL
  if (!length(ini) && !length(fin)) {
    error <- "el documento no tiene el bloque generado: faltan los marcadores SUITES:INICIO y SUITES:FIN"
  } else if (!length(ini)) {
    error <- "falta el marcador de inicio (<!-- SUITES:INICIO ... -->)"
  } else if (!length(fin)) {
    error <- "falta el marcador de fin (<!-- SUITES:FIN -->)"
  } else if (length(ini) > 1L || length(fin) > 1L) {
    error <- paste0("los marcadores deben aparecer una sola vez (inicio: ", length(ini), ", fin: ", length(fin), ")")
  } else if (fin < ini) {
    error <- "el marcador de fin aparece antes que el de inicio"
  }
  if (is.null(error)) list(ini = ini, fin = fin, error = NULL) else list(ini = NA_integer_, fin = NA_integer_, error = error)
}

filas_de_tabla <- function(lineas) {
  texto <- lineas[grepl("^\\|\\s*[0-9]+\\s*\\|", lineas)]
  list(clave = as.integer(sub("^\\|\\s*([0-9]+)\\s*\\|.*$", "\\1", texto)), texto = texto)
}

# Primera línea (1-based) en que dos vectores de líneas difieren; NA si son iguales.
primera_diferencia <- function(a, e) {
  n <- max(length(a), length(e))
  a <- c(a, rep(NA_character_, n - length(a)))
  e <- c(e, rep(NA_character_, n - length(e)))
  distinta <- is.na(a) != is.na(e) | (!is.na(a) & !is.na(e) & a != e)
  if (any(distinta)) which(distinta)[1L] else NA_integer_
}

# Compara el bloque del documento con el esperado. No escribe nada.
# Devuelve list(ok = <lógico>, diferencias = <mensajes>); ok es TRUE solo si el bloque,
# marcadores incluidos, es idéntico línea por línea al generado.
comparar <- function(doc, esperado) {
  pos <- localizar_marcadores(doc)
  if (!is.null(pos$error)) return(list(ok = FALSE, diferencias = pos$error))
  actual <- doc[pos$ini:pos$fin]
  if (identical(actual, esperado)) return(list(ok = TRUE, diferencias = character(0)))

  dif <- character(0)
  if (!identical(actual[1L], esperado[1L]))
    dif <- c(dif, "el marcador de inicio no es el generado (¿editado a mano?)")

  # Cifras
  ca <- actual[startsWith(actual, PREFIJO_CIFRAS)]
  ce <- esperado[startsWith(esperado, PREFIJO_CIFRAS)]
  if (!length(ca)) {
    dif <- c(dif, paste0("falta la línea de cifras; el runner da: ", ce))
  } else if (!identical(ca, ce)) {
    dif <- c(dif, paste0("cifras desactualizadas\n      documento: ", paste(ca, collapse = " / "),
                         "\n      runner:    ", ce))
  }

  # Tabla: filas sobrantes, faltantes, repetidas o distintas (la clave es el número de fila)
  fa <- filas_de_tabla(actual)
  fe <- filas_de_tabla(esperado)
  for (k in unique(fa$clave[duplicated(fa$clave)]))
    dif <- c(dif, paste0("la fila ", k, " aparece ", sum(fa$clave == k), " veces"))
  for (k in setdiff(fe$clave, fa$clave))
    dif <- c(dif, paste0("falta la fila ", k, " (la define el runner): ", fe$texto[fe$clave == k]))
  for (k in setdiff(fa$clave, fe$clave))
    dif <- c(dif, paste0("sobra la fila ", k, " (el runner no la define): ", fa$texto[fa$clave == k][1L]))
  for (k in intersect(fa$clave, fe$clave)) {
    ta <- fa$texto[fa$clave == k][1L]
    te <- fe$texto[fe$clave == k]
    if (!identical(ta, te))
      dif <- c(dif, paste0("la fila ", k, " difiere\n      documento: ", ta, "\n      runner:    ", te))
  }

  # Frase del modo rápido
  pa <- actual[startsWith(actual, PREFIJO_FRASE)]
  pe <- esperado[startsWith(esperado, PREFIJO_FRASE)]
  if (!length(pa)) {
    dif <- c(dif, paste0("falta la frase del modo rápido; el runner da: ", pe))
  } else if (!identical(pa, pe)) {
    dif <- c(dif, paste0("frase del modo rápido desactualizada\n      documento: ", paste(pa, collapse = " / "),
                         "\n      runner:    ", pe))
  }

  # Lo que las comprobaciones anteriores no ven (orden de las filas, líneas sueltas, marcador de fin...)
  if (!length(dif)) {
    i <- primera_diferencia(actual, esperado)
    dif <- paste0("el bloque difiere del generado en la línea ", i, " de su texto (orden de filas, líneas sueltas o formato)",
                  "\n      documento: ", if (i <= length(actual)) actual[i] else "(no existe)",
                  "\n      runner:    ", if (i <= length(esperado)) esperado[i] else "(no existe)")
  }
  list(ok = FALSE, diferencias = dif)
}

# --- Reescritura -----------------------------------------------------------------
# Devuelve el documento con el bloque esperado en su sitio. Con marcadores, reemplaza lo que
# hay entre ellos; sin ellos (la primera vez), reemplaza desde la tabla `| # | Archivo | ...`
# hasta la frase «En modo rápido ...» que había escritas a mano.
reescribir <- function(doc, esperado) {
  pos <- localizar_marcadores(doc)
  if (is.null(pos$error)) {
    despues <- if (pos$fin < length(doc)) doc[(pos$fin + 1L):length(doc)] else character(0)
    return(c(doc[seq_len(pos$ini - 1L)], esperado, despues))
  }
  if (any(startsWith(doc, PREFIJO_INICIO)) || any(startsWith(doc, PREFIJO_FIN)))
    stop("marcadores incompletos o repetidos (", pos$error, "): restaurar a mano el marcador que falta ",
         "(o dejar uno solo de cada) antes de regenerar", call. = FALSE)
  tabla <- which(startsWith(doc, PREFIJO_TABLA))
  if (length(tabla) != 1L)
    stop("no hay bloque con marcadores ni una única tabla anterior (", PREFIJO_TABLA, " ...): ",
         "insertar a mano los marcadores alrededor de la tabla de suites y de la frase del modo rápido",
         call. = FALSE)
  frase <- which(startsWith(doc, PREFIJO_FRASE) & seq_along(doc) > tabla)
  if (!length(frase))
    stop("no encuentro la frase del modo rápido (", PREFIJO_FRASE, " ...) después de la tabla anterior", call. = FALSE)
  frase <- frase[1L]
  despues <- if (frase < length(doc)) doc[(frase + 1L):length(doc)] else character(0)
  c(doc[seq_len(tabla - 1L)], esperado, despues)
}

# --- CLI ---------------------------------------------------------------------------
# Los regex de base R sobre texto UTF-8 fallan en una locale que no lo es (LC_ALL=C: «unable to
# translate ... to a wide string») y dan diferencias falsas. Por eso la CLI fija LC_CTYPE a una
# UTF-8 si hace falta. Con source() no se toca nada: lo llama quien lo necesite (p. ej. el test).
asegurar_utf8 <- function() {
  if (isTRUE(l10n_info()[["UTF-8"]])) return(invisible(TRUE))
  for (loc in c("C.UTF-8", "en_US.UTF-8", "es_CO.UTF-8", "es_ES.UTF-8"))
    if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", loc)))) return(invisible(TRUE))
  stop("hace falta una locale UTF-8 (p. ej. LANG=C.UTF-8); esta sesión usa ", Sys.getlocale("LC_CTYPE"),
       call. = FALSE)
}

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (length(setdiff(args, "--check"))) {
    cat("Uso: ", COMANDO_SINCRONIZAR, " [--check]\n", sep = "", file = stderr())
    return(2L)
  }
  chequear <- "--check" %in% args
  tryCatch({
    asegurar_utf8()
    doc_path <- ruta_doc()
    suites <- leer_suites(ruta_runner())
    esperado <- generar_bloque(suites, contar_test_that(suites$archivo, raiz_repo()))
    doc <- leer_lineas(doc_path)

    if (chequear) {
      cmp <- comparar(doc, esperado)
      if (cmp$ok) {
        cat("OK: el bloque SUITES de ", doc_path, " coincide con el runner.\n  ", esperado[2L], "\n", sep = "")
        return(0L)
      }
      cat("DESACTUALIZADO: el bloque SUITES de ", doc_path, " no coincide con el runner:\n", sep = "")
      cat(paste0("  - ", cmp$diferencias, "\n"), sep = "")
      cat("Ejecutar: ", COMANDO_SINCRONIZAR, "\n", sep = "")
      return(1L)
    }

    nuevo <- reescribir(doc, esperado)
    if (identical(nuevo, doc)) {
      cat("Sin cambios: el bloque SUITES de ", doc_path, " ya coincide con el runner.\n  ", esperado[2L], "\n", sep = "")
      return(0L)
    }
    primera_vez <- !is.null(localizar_marcadores(doc)$error)
    writeLines(nuevo, doc_path, useBytes = TRUE)
    cat(if (primera_vez) "Bloque SUITES insertado (reemplaza la tabla y la frase escritas a mano): " else "Bloque SUITES actualizado: ",
        doc_path, "\n  ", esperado[2L], "\n", sep = "")
    0L
  }, error = function(e) {
    cat("ERROR: ", conditionMessage(e), "\n", sep = "", file = stderr())
    2L
  })
}

if (!interactive() && sys.nframe() == 0L &&
    any(grepl("^--file=.*sincronizar_doc_suites\\.R$", commandArgs(FALSE)))) {
  quit(save = "no", status = main())
}
