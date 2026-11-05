# =============================================================================
# nops_control_calidad.R — Triaje de calidad de escaneos NOPS previo a nops_eval()
# =============================================================================
# Tras escanear las hojas de un examen con exams::nops_scan(), el ZIP
# nops_scan_<fecha>.zip contiene un Daten.txt (una línea por hoja) y los PNG.
# exams::nops_fix() corrige los escaneos a mano, pero es interactiva: el
# profesor no sabe cuántas hojas tienen problemas hasta abrirlas y, si no la
# ejecuta, nops_eval() evalúa con los datos malos. Este script hace el TRIAJE
# sin interacción: lista qué hojas tienen problemas, propone las llamadas de
# nops_fix() necesarias y sale con 1 si no conviene evaluar todavía.
# NO modifica el ZIP.
#
# Uso:
#   Rscript nops_control_calidad.R [nops_scan_x.zip ...]
#           [--preguntas N] [--tipo schoice|mchoice] [--lista estudiantes.csv]
#           [--json]
#   Sin ZIP explícito se usa dir(pattern = "^nops_scan_[[:digit:]]*\\.zip$")
#   del directorio actual.
#   Exit: 0 sin problemas; 1 hay hojas por revisar; 2 NO VERIFICABLE (sin ZIP,
#   ZIP sin Daten.txt, argumentos inválidos).
#
# Formato de Daten.txt (verificado en exams/R/nops_fix.R, versión 2.4-5),
# una línea por hoja, separada por espacios, sin encabezado:
#   file id scrambling type replacement registration a1 a2 ... a45
#   - id: 11 dígitos; scrambling: 2; type: 3 (los dígitos 2 y 3 son el número
#     de preguntas); replacement: 0/1; registration: 7 a 10 dígitos;
#   - a1..a45: cinco dígitos 0/1 cada una (01000 = marcó la b, 00000 = nada).
#   Una hoja que no se pudo leer lleva la palabra ERROR en su línea y puede
#   tener menos campos: se detecta con grepl() ANTES de usar read.table, y del
#   resto de la línea solo vale el primer campo (file).
#   Leer SIEMPRE con colClasses = "character": los ceros a la izquierda
#   importan (registro 0157193, type 005, etc.).

# -----------------------------------------------------------------------------
# Utilidades de validación (mismas reglas que nops_fix())
# -----------------------------------------------------------------------------

valid_digits <- function(x, n = NULL) {
  # Igual que valid_digits de nops_fix(), con una guardia para NA: un campo
  # ausente también es inválido. Todo ceros es inválido; "XXXXXXX" también.
  if (length(x) != 1L || is.na(x)) return(FALSE)
  !is.null(x) && (is.null(n) || (n == nchar(x))) && grepl("^[0-9]*$", x) &&
    !grepl("^[0]*$", x)
}

respuesta_valida <- function(x) {
  # Respuesta válida: exactamente cinco caracteres, todos 0 o 1.
  length(x) == 1L && !is.na(x) && nchar(x) == 5L && grepl("^[0-1]*$", x)
}

marcas_de <- function(x) {
  # Número de casillas marcadas en una respuesta válida de cinco dígitos.
  nchar(gsub("0", "", x, fixed = TRUE))
}

# -----------------------------------------------------------------------------
# Lectura del ZIP
# -----------------------------------------------------------------------------

leer_daten <- function(zip) {
  # Descomprime SOLO Daten.txt en un directorio temporal (que se borra al
  # salir) y devuelve list(hojas = data.frame, ilegibles = vector de file).
  # hojas tiene UNA fila por línea de Daten.txt — también las ilegibles, con
  # todos los campos en NA salvo file — para que el número de fila del triaje
  # coincida con el de Daten.txt (contando las ilegibles).
  if (length(zip) != 1L || is.na(zip) || !nzchar(zip) || !file.exists(zip)) {
    stop("el ZIP no existe: ", zip)
  }
  contenido <- tryCatch(utils::unzip(zip, list = TRUE)$Name,
                        error = function(e) stop("no se pudo abrir el ZIP: ", zip, call. = FALSE))
  nombre_daten <- contenido[basename(contenido) == "Daten.txt"]
  if (!length(nombre_daten)) stop("el ZIP no contiene Daten.txt: ", zip, call. = FALSE)
  # Si por accidente hay dos entradas Daten.txt, leer solo la primera.
  nombre_daten <- nombre_daten[[1]]
  tmp <- tempfile("nops_control_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  utils::unzip(zip, files = nombre_daten, exdir = tmp, junkpaths = TRUE)
  ruta <- file.path(tmp, basename(nombre_daten))
  lineas <- readLines(ruta, warn = FALSE)
  columnas <- c("file", "id", "scrambling", "type", "replacement", "registration",
                paste0("a", 1:45))
  filas <- vector("list", length(lineas))
  ilegibles <- character()
  for (k in seq_along(lineas)) {
    ln <- sub("\r$", "", lineas[[k]])
    if (!nzchar(trimws(ln))) {
      # Línea vacía: nada legible, pero conserva su número de fila.
      filas[[k]] <- c("", rep(NA_character_, 50L))
      ilegibles <- c(ilegibles, "")
      next
    }
    if (grepl("ERROR", ln, fixed = TRUE)) {
      # Hoja que no se pudo leer: detectar ANTES de read.table porque la línea
      # puede tener menos campos. Del resto solo vale el primer campo (file).
      trozos <- strsplit(trimws(ln), "[[:space:]]+", perl = TRUE)[[1]]
      f <- if (length(trozos) > 0L) trozos[[1]] else ""
      filas[[k]] <- c(f, rep(NA_character_, 50L))
      ilegibles <- c(ilegibles, f)
      next
    }
    d <- read.table(text = ln, header = FALSE, colClasses = "character", fill = TRUE,
                    comment.char = "", strip.white = TRUE)
    vals <- unlist(d[1, ], use.names = FALSE)
    vals <- if (length(vals) >= 51L) vals[seq_len(51L)] else {
      c(vals, rep(NA_character_, 51L - length(vals)))
    }
    filas[[k]] <- vals
  }
  hojas <- if (length(filas) > 0L) {
    as.data.frame(do.call(rbind, filas), stringsAsFactors = FALSE)
  } else {
    as.data.frame(stats::setNames(rep(list(character()), 51L), columnas),
                  stringsAsFactors = FALSE)
  }
  names(hojas) <- columnas
  list(hojas = hojas, ilegibles = ilegibles)
}

# -----------------------------------------------------------------------------
# Triaje
# -----------------------------------------------------------------------------

triaje <- function(lectura, preguntas = NULL, tipo = "schoice", lista = NULL) {
  # Devuelve un data.frame de hallazgos con columnas file, fila (número de
  # fila en Daten.txt, contando las ilegibles), problema y detalle. Valores
  # de problema: ilegible, id_invalido, tipo_invalido, registro_invalido,
  # id_duplicado, registro_duplicado, registro_fuera_de_lista, multimarca,
  # sin_marcar, todas_marcadas, respuesta_invalida.
  if (!is.list(lectura) || !all(c("hojas", "ilegibles") %in% names(lectura))) {
    stop("lectura debe ser la salida de leer_daten()")
  }
  if (length(tipo) != 1L || !tipo %in% c("schoice", "mchoice")) {
    stop("tipo debe ser \"schoice\" o \"mchoice\"")
  }
  if (!is.null(preguntas)) {
    if (length(preguntas) != 1L || is.na(preguntas) || preguntas < 1L ||
        preguntas > 45L || preguntas != as.integer(preguntas)) {
      stop("preguntas debe ser un entero entre 1 y 45")
    }
    preguntas <- as.integer(preguntas)
  }
  registros_lista <- NULL
  if (!is.null(lista)) {
    if (!is.data.frame(lista) || !"registration" %in% names(lista)) {
      stop("lista debe ser un data.frame con la columna registration")
    }
    registros_lista <- as.character(lista$registration)
  }
  hojas <- lectura$hojas
  ilegibles <- lectura$ilegibles
  n_filas <- nrow(hojas)
  id_ok <- vapply(seq_len(n_filas), function(i) valid_digits(hojas$id[i], 11L), logical(1))
  ty_ok <- vapply(seq_len(n_filas), function(i) valid_digits(hojas$type[i], 3L), logical(1))
  rg_ok <- vapply(seq_len(n_filas), function(i) valid_digits(hojas$registration[i]), logical(1))
  # Duplicados: la misma hoja escaneada dos veces (id repetido) y dos hojas con
  # el mismo estudiante (registration repetido). Se marcan todas las filas
  # repetidas salvo la primera; nops_eval descarta los duplicados con aviso.
  filas_id_dup <- which(id_ok)[duplicated(hojas$id[id_ok])]
  filas_rg_dup <- which(rg_ok)[duplicated(hojas$registration[rg_ok])]
  # Un registro válido que no está en la lista de estudiantes es anómalo.
  filas_fuera <- if (is.null(registros_lista)) integer() else {
    which(rg_ok & !(hojas$registration %in% registros_lista))
  }
  hallazgos <- list()
  agregar <- function(file, fila, problema, detalle) {
    hallazgos[[length(hallazgos) + 1L]] <<- data.frame(
      file = file, fila = fila, problema = problema, detalle = detalle,
      stringsAsFactors = FALSE
    )
  }
  for (i in seq_len(n_filas)) {
    f <- hojas$file[i]
    if (is.na(hojas$id[i]) && f %in% ilegibles) {
      agregar(f, i, "ilegible", "")
      next
    }
    if (!id_ok[i]) {
      agregar(f, i, "id_invalido", if (is.na(hojas$id[i])) "" else hojas$id[i])
    }
    if (!ty_ok[i]) {
      agregar(f, i, "tipo_invalido", if (is.na(hojas$type[i])) "" else hojas$type[i])
    }
    if (!rg_ok[i]) {
      agregar(f, i, "registro_invalido", if (is.na(hojas$registration[i])) "" else hojas$registration[i])
    }
    if (i %in% filas_id_dup) agregar(f, i, "id_duplicado", hojas$id[i])
    if (i %in% filas_rg_dup) agregar(f, i, "registro_duplicado", hojas$registration[i])
    if (i %in% filas_fuera) agregar(f, i, "registro_fuera_de_lista", hojas$registration[i])
    # Respuestas: solo se miran las preguntas 1..n. Si preguntas es NULL y el
    # type de la hoja es inválido no se revisan (ya está el hallazgo
    # tipo_invalido).
    n <- if (!is.null(preguntas)) preguntas else {
      if (ty_ok[i]) as.integer(substr(hojas$type[i], 2L, 3L)) else NULL
    }
    if (!is.null(n) && n > 0L) {
      multi <- integer(); faltan <- integer(); todas <- integer(); malas <- integer()
      for (p in seq_len(n)) {
        a <- hojas[[paste0("a", p)]][i]
        if (!respuesta_valida(a)) { malas <- c(malas, p); next }
        marcas <- marcas_de(a)
        if (marcas == 0L) {
          faltan <- c(faltan, p)
        } else if (tipo == "schoice" && marcas > 1L) {
          # En schoice más de una casilla es anómala; en mchoice no.
          multi <- c(multi, p)
        } else if (tipo == "mchoice" && marcas == 5L) {
          # En mchoice marcar las cinco casillas sí es anómalo.
          todas <- c(todas, p)
        }
      }
      if (length(multi)) agregar(f, i, "multimarca", paste(multi, collapse = ","))
      if (length(faltan)) agregar(f, i, "sin_marcar", paste(faltan, collapse = ","))
      if (length(todas)) agregar(f, i, "todas_marcadas", paste(todas, collapse = ","))
      if (length(malas)) agregar(f, i, "respuesta_invalida", paste(malas, collapse = ","))
    }
  }
  if (length(hallazgos) == 0L) {
    return(data.frame(file = character(), fila = integer(), problema = character(),
                      detalle = character(), stringsAsFactors = FALSE))
  }
  res <- do.call(rbind, hallazgos)
  res$fila <- as.integer(res$fila)
  rownames(res) <- NULL
  res
}

# -----------------------------------------------------------------------------
# Recomendaciones
# -----------------------------------------------------------------------------

recomendar <- function(hallazgos, zip) {
  # Vector de órdenes de R listas para copiar y pegar. Los duplicados y los
  # registros fuera de la lista no tienen orden de nops_fix(): se indican como
  # comentario para revisarlos a mano.
  if (!is.data.frame(hallazgos)) stop("hallazgos debe ser un data.frame")
  if (nrow(hallazgos) == 0L) return(character())
  if (!all(c("file", "fila", "problema", "detalle") %in% names(hallazgos))) {
    stop("hallazgos no tiene las columnas file, fila, problema y detalle")
  }
  filas <- as.integer(hallazgos$fila)
  ords <- character()
  ileg <- sort(unique(filas[hallazgos$problema == "ilegible"]))
  if (length(ileg)) {
    ords <- c(ords, sprintf('nops_fix("%s", display = "interactive")  # hoja(s) ilegible(s), fila(s) de Daten.txt: %s',
                            zip, paste(ileg, collapse = ", ")))
  }
  campos <- sort(unique(filas[hallazgos$problema %in% c("id_invalido", "tipo_invalido", "registro_invalido")]))
  if (length(campos)) {
    ords <- c(ords, sprintf('nops_fix("%s", exam = c(%s))', zip, paste(campos, collapse = ", ")))
  }
  if (any(hallazgos$problema == "multimarca")) {
    ords <- c(ords, sprintf('nops_fix("%s", check = "schoice")', zip))
  }
  if (any(hallazgos$problema == "sin_marcar")) {
    ords <- c(ords, sprintf('nops_fix("%s", check = "missing")', zip))
  }
  if (any(hallazgos$problema == "todas_marcadas")) {
    ords <- c(ords, sprintf('nops_fix("%s", check = "mchoice")', zip))
  }
  a_mano <- hallazgos[hallazgos$problema %in% c("id_duplicado", "registro_duplicado", "registro_fuera_de_lista"), , drop = FALSE]
  if (nrow(a_mano) > 0L) {
    for (j in seq_len(nrow(a_mano))) {
      ords <- c(ords, sprintf("# revisar a mano: %s en la fila %d (detalle: %s)",
                              a_mano$problema[j], a_mano$fila[j], a_mano$detalle[j]))
    }
  }
  ords
}

# -----------------------------------------------------------------------------
# Interfaz de línea de órdenes
# -----------------------------------------------------------------------------

ayuda <- function() {
  cat("Uso: Rscript nops_control_calidad.R [nops_scan_x.zip ...]\n")
  cat("        [--preguntas N] [--tipo schoice|mchoice] [--lista estudiantes.csv] [--json]\n")
}

no_verificable <- function(mensaje, json = FALSE) {
  if (json) {
    cat(jsonlite::toJSON(list(estado = "NO_VERIFICABLE", mensaje = mensaje),
                         auto_unbox = TRUE, pretty = TRUE), "\n")
  } else {
    cat(mensaje, "\n", sep = "")
    cat("NOPS_NO_VERIFICABLE\n")
  }
  quit(status = 2L)
}

procesar_un_zip <- function(zip, preguntas, tipo, lista) {
  lectura <- leer_daten(zip)
  list(zip = zip, lectura = lectura,
       hallazgos = triaje(lectura, preguntas = preguntas, tipo = tipo, lista = lista))
}

informe_texto <- function(resultados) {
  for (r in resultados) {
    h <- r$hallazgos
    cat("== ", r$zip, " ==\n", sep = "")
    cat("hojas leídas: ", nrow(r$lectura$hojas), sep = "")
    k <- length(r$lectura$ilegibles)
    if (k > 0L) cat(" (", k, " ilegible", if (k == 1L) "" else "s", ")", sep = "")
    cat("\n")
    if (nrow(h) == 0L) {
      cat("sin hallazgos\n")
    } else {
      cat("hallazgos por problema:\n")
      orden <- c("ilegible", "id_invalido", "tipo_invalido", "registro_invalido",
                 "id_duplicado", "registro_duplicado", "registro_fuera_de_lista",
                 "multimarca", "sin_marcar", "todas_marcadas", "respuesta_invalida")
      conteo <- table(factor(h$problema, levels = orden))
      conteo <- conteo[conteo > 0L]
      for (p in names(conteo)) cat(sprintf("  %s: %d\n", p, conteo[[p]]))
      cat("tabla de hallazgos:\n")
      n_mostrar <- min(nrow(h), 30L)
      print(h[seq_len(n_mostrar), , drop = FALSE], row.names = FALSE)
      if (nrow(h) > 30L) {
        cat(sprintf("... y %d filas más (total %d filas)\n", nrow(h) - 30L, nrow(h)))
      }
    }
    ords <- recomendar(h, r$zip)
    if (length(ords) > 0L) {
      cat("recomendaciones:\n")
      for (o in ords) cat("  ", o, "\n", sep = "")
    }
    if (nrow(h) == 0L) {
      cat("NOPS_LISTO_PARA_EVALUAR\n")
    } else {
      m <- length(unique(h$fila))
      cat(sprintf("NOPS_REVISAR %d hallazgo%s en %d hoja%s\n",
                  nrow(h), if (nrow(h) == 1L) "" else "s",
                  m, if (m == 1L) "" else "s"))
    }
  }
  invisible(NULL)
}

informe_json <- function(resultados) {
  archivos <- lapply(unname(resultados), function(r) {
    list(zip = r$zip,
         hojas_leidas = nrow(r$lectura$hojas),
         ilegibles = r$lectura$ilegibles,
         hallazgos = r$hallazgos,
         recomendaciones = recomendar(r$hallazgos, r$zip))
  })
  n_h <- sum(vapply(resultados, function(r) nrow(r$hallazgos), integer(1)))
  salida <- list(estado = if (n_h > 0L) "REVISAR" else "LISTO", archivos = archivos)
  cat(jsonlite::toJSON(salida, auto_unbox = TRUE, pretty = TRUE, dataframe = "rows"), "\n")
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  zips <- character()
  preguntas <- NULL
  tipo <- "schoice"
  lista_ruta <- NULL
  json <- FALSE
  i <- 1L
  while (i <= length(args)) {
    a <- args[[i]]
    if (a == "--preguntas") {
      i <- i + 1L
      if (i > length(args) || !grepl("^[0-9]+$", args[[i]])) {
        ayuda(); no_verificable("--preguntas necesita un entero positivo", json)
      }
      preguntas <- as.integer(args[[i]])
      if (preguntas < 1L || preguntas > 45L) {
        ayuda(); no_verificable("--preguntas debe estar entre 1 y 45", json)
      }
    } else if (a == "--tipo") {
      i <- i + 1L
      tipo <- if (i <= length(args)) args[[i]] else ""
      if (!tipo %in% c("schoice", "mchoice")) {
        ayuda(); no_verificable("--tipo debe ser schoice o mchoice", json)
      }
    } else if (a == "--lista") {
      i <- i + 1L
      if (i > length(args) || !file.exists(args[[i]])) {
        ayuda(); no_verificable("--lista necesita un archivo CSV existente", json)
      }
      lista_ruta <- args[[i]]
    } else if (a == "--json") {
      json <- TRUE
    } else if (startsWith(a, "--")) {
      ayuda(); no_verificable(paste("argumento desconocido:", a), json)
    } else {
      zips <- c(zips, a)
    }
    i <- i + 1L
  }
  zips <- unique(zips)
  if (length(zips) == 0L) {
    zips <- dir(pattern = "^nops_scan_[[:digit:]]*\\.zip$")
    if (length(zips) == 0L) {
      no_verificable("no hay archivos nops_scan_*.zip en el directorio actual y no se indicó ninguno", json)
    }
  }
  # Validar TODOS los ZIP antes de procesar ninguno.
  for (z in zips) {
    if (!file.exists(z)) no_verificable(paste("el archivo no existe:", z), json)
    contenido <- tryCatch(utils::unzip(z, list = TRUE)$Name,
                          error = function(e) character(),
                          warning = function(w) character())
    if (!length(contenido) || !any(basename(contenido) == "Daten.txt")) {
      no_verificable(paste("el ZIP no contiene Daten.txt:", z), json)
    }
  }
  lista <- NULL
  if (!is.null(lista_ruta)) {
    # CSV2 (separador ;, el que escribe write.csv2), con la columna
    # registration leída como carácter para conservar los ceros a la izquierda.
    lista <- tryCatch(utils::read.csv2(lista_ruta, colClasses = "character", check.names = FALSE),
                      error = function(e) NULL)
    if (is.null(lista) || !"registration" %in% names(lista)) {
      no_verificable(paste("--lista sin la columna registration (CSV con separador ;):", lista_ruta), json)
    }
  }
  resultados <- list()
  for (z in zips) {
    resultados[[z]] <- tryCatch(
      procesar_un_zip(z, preguntas = preguntas, tipo = tipo, lista = lista),
      error = function(e) no_verificable(conditionMessage(e), json)
    )
  }
  n_h <- sum(vapply(resultados, function(r) nrow(r$hallazgos), integer(1)))
  if (json) {
    informe_json(resultados)
  } else {
    informe_texto(resultados)
  }
  if (n_h > 0L) quit(status = 1L) else quit(status = 0L)
}

# Separación CLI/funciones: al ejecutar con Rscript (sys.nframe() == 0) se
# corre main(); al cargar con source() el archivo solo define funciones.
if (!interactive() && sys.nframe() == 0L) {
  main()
}
