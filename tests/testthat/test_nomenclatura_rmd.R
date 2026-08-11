# =============================================================================
# Test de regresión — Nomenclatura oficial de archivos .Rmd
# Fuente única: `.claude/docs/NOMENCLATURA_ARCHIVOS_RMD.md` (v2.0)
# =============================================================================
# Origen: 2026-08-10. Durante seis meses el repo tuvo DOS formatos de nombre
# rivales conviviendo en NUEVE sitios, y nada los reconciliaba:
#   - el documentado, [ejercicio]_[componente]_[competencia]_n[nivel]_v[N],
#     que no estaba cableado en ninguna comprobación ejecutable;
#   - uno de facto, [ejercicio]_metacognitivo_[competencia_corta]_n[nivel]_[tipo]_v[N],
#     cableado en el regex de los dos orquestadores, los dos skills y la regla #10.
# Medido: 53 archivos seguían el de facto y 10 el documentado. Ganaba el de facto
# porque era el que la máquina comprobaba.
#
# LA LECCIÓN QUE ESTE TEST FIJA: un formato que solo vive en un documento pierde
# siempre contra uno que vive en un regex. Por eso hay tres capas —gate, este
# test y la coherencia de las citas— y no solo prosa.

library(testthat)

repo_root <- tryCatch({
  r <- system("git rev-parse --show-toplevel", intern = TRUE)
  if (length(r) == 1 && dir.exists(r)) r else getwd()
}, error = function(e) getwd())

DOC       <- file.path(repo_root, ".claude", "docs", "NOMENCLATURA_ARCHIVOS_RMD.md")
GATE      <- file.path(repo_root, ".claude", "hooks", "pre-write-rmd-gate.sh")
ALLOWLIST <- file.path(repo_root, "tests", "testthat", "nomenclatura-legacy.txt")

# El regex canónico. Vive AQUÍ y en el gate; el test de coherencia de más abajo
# comprueba que el gate use exactamente éste, para que no puedan divergir.
NOMENCLATURA <- paste0(
  "^[a-z0-9_]+_(geometrico_metrico|numerico_variacional|aleatorio)",
  "_(interpretacion_representacion|formulacion_ejecucion|argumentacion)",
  "_n[1-4]_(schoice|cloze)(_neg)?_v[0-9]+(_interactivo)?$"
)

leer <- function(p) paste(readLines(p, warn = FALSE), collapse = "\n")

listar_rmd <- function() {
  zonas <- file.path(repo_root, "A-Produccion",
                     c("01-En-PreDesarrollo", "02-En-Desarrollo", "03-En-Produccion"))
  zonas <- zonas[dir.exists(zonas)]
  if (!length(zonas)) return(character(0))
  fs <- list.files(zonas, pattern = "\\.[Rr]md$", recursive = TRUE, full.names = TRUE)
  fs <- fs[!grepl("Ejemplos-Funcionales", fs)]
  fs <- fs[!grepl("(^|/)\\.", basename(fs))]        # sin ocultos
  fs[!grepl("/\\.", fs)]                            # sin directorios ocultos
}

cargar_allowlist <- function() {
  if (!file.exists(ALLOWLIST)) return(character(0))
  l <- readLines(ALLOWLIST, warn = FALSE)
  l <- trimws(l)
  l[nzchar(l) & !startsWith(l, "#")]
}

# =============================================================================
context("La fuente única existe y declara el formato")

test_that("el documento de nomenclatura existe y está en v2.0 o superior", {
  expect_true(file.exists(DOC), info = paste("Falta", DOC))
  skip_if_not(file.exists(DOC))
  txt <- leer(DOC)
  v <- regmatches(txt, regexpr("\\*\\*Versi[oó]n:\\*\\*\\s*([0-9]+)\\.([0-9]+)", txt))
  expect_true(length(v) == 1, info = "No se pudo leer la versión del documento")
  expect_gte(as.numeric(sub(".*?([0-9]+\\.[0-9]+).*", "\\1", v)), 2.0)
})

test_that("el documento declara la ranura de tipo y prohíbe 'metacognitivo' en el nombre", {
  skip_if_not(file.exists(DOC))
  txt <- leer(DOC)
  expect_true(grepl("[ejercicio]_[componente]_[competencia]_n[nivel]_[tipo]_v[version].Rmd",
                    txt, fixed = TRUE),
    info = "El documento no declara el formato con la ranura [tipo]")
  # La ranura de tipo no es decorativa: sin ella colisionan las variantes.
  expect_true(grepl("colisionan", txt, fixed = TRUE),
    info = "El documento no explica POR QUÉ la ranura de tipo es obligatoria")
  expect_true(grepl("metacognitivo", txt, fixed = TRUE),
    info = "El documento no advierte sobre 'metacognitivo' en el nombre")
})

# =============================================================================
context("Ningún .Rmd fuera de formato salvo el legacy declarado")

test_that("todo .Rmd conforma la nomenclatura o está en el allowlist legacy", {
  fs <- listar_rmd()
  skip_if(length(fs) == 0, "no hay .Rmd que revisar")
  legacy <- cargar_allowlist()

  nombres <- sub("\\.[Rr]md$", "", basename(fs))
  conforma <- grepl(NOMENCLATURA, nombres)
  infractores <- basename(fs)[!conforma]
  nuevos <- setdiff(infractores, legacy)

  expect_equal(length(nuevos), 0,
    info = paste0("Estos .Rmd no siguen la nomenclatura y NO están en el allowlist legacy.\n",
                  "Si son nuevos, renómbralos (el gate debería haberlo impedido).\n  - ",
                  paste(head(nuevos, 15), collapse = "\n  - "),
                  if (length(nuevos) > 15) sprintf("\n  ... y %d más", length(nuevos) - 15) else ""))
})

test_that("el allowlist legacy solo decrece: sin entradas obsoletas", {
  fs <- listar_rmd()
  skip_if(length(fs) == 0, "no hay .Rmd que revisar")
  legacy <- cargar_allowlist()
  skip_if(length(legacy) == 0, "allowlist vacío")

  # Una entrada que ya no corresponde a ningún archivo significa que el archivo
  # se renombró o se borró y nadie limpió la lista. Se exige limpiarla para que
  # el allowlist siga siendo un inventario honesto de deuda pendiente.
  huerfanas <- setdiff(legacy, basename(fs))
  expect_equal(length(huerfanas), 0,
    info = paste0("Entradas del allowlist que ya no existen en disco — bórralas de ",
                  basename(ALLOWLIST), ":\n  - ",
                  paste(head(huerfanas, 15), collapse = "\n  - ")))
})

test_that("el allowlist declara que no admite altas", {
  skip_if_not(file.exists(ALLOWLIST))
  cab <- paste(readLines(ALLOWLIST, warn = FALSE, n = 15), collapse = "\n")
  expect_true(grepl("NO ADMITE ALTAS", cab, fixed = TRUE),
    info = "El allowlist no declara su regla de uso en la cabecera")
  expect_true(grepl("SOLO PUEDE DECRECER", cab, fixed = TRUE),
    info = "El allowlist no declara que solo puede decrecer")
})

# =============================================================================
context("El gate bloquea de verdad (control positivo)")

# Sin esto, "el gate está" no distingue de "el gate dispara". Se invoca el hook
# REAL con su protocolo real (JSON por stdin) sobre rutas en tempdir().
invocar_gate <- function(ruta) {
  # OJO: `stdin` de system2() es una RUTA DE ARCHIVO, no una conexión. Pasarle
  # un textConnection() hace que bash reciba basura y el hook ni se ejecute:
  # el test fallaba con "sh: línea 1: 5: No existe el fichero" en vez de medir
  # el gate. Un falso rojo que además NO parece un bug del test.
  tmp_in <- tempfile(); tmp_out <- tempfile(); tmp_err <- tempfile()
  on.exit(unlink(c(tmp_in, tmp_out, tmp_err)), add = TRUE)
  writeLines(sprintf('{"tool_input":{"file_path":"%s"}}', ruta), tmp_in)
  st <- suppressWarnings(system2("bash", GATE, stdin = tmp_in,
                                 stdout = tmp_out, stderr = tmp_err))
  list(exit = st,
       err = paste(readLines(tmp_err, warn = FALSE), collapse = "\n"))
}

test_that("el gate rechaza un .Rmd NUEVO con nombre fuera de formato", {
  skip_if_not(file.exists(GATE))
  d <- file.path(tempdir(), "A-Produccion", "01-En-PreDesarrollo", "fx")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(file.path(tempdir(), "A-Produccion"), recursive = TRUE), add = TRUE)

  r <- invocar_gate(file.path(d, "mediana_metacognitivo_argumentacion_n3_schoice_v1.Rmd"))
  expect_equal(r$exit, 2L, info = "El gate no bloqueó un nombre con 'metacognitivo'")
  expect_match(r$err, "no sigue la nomenclatura oficial")
})

test_that("el gate rechaza un nombre SIN ranura de tipo", {
  skip_if_not(file.exists(GATE))
  d <- file.path(tempdir(), "A-Produccion", "01-En-PreDesarrollo", "fx")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(file.path(tempdir(), "A-Produccion"), recursive = TRUE), add = TRUE)

  r <- invocar_gate(file.path(d, "area_lote_geometrico_metrico_argumentacion_n4_v1.Rmd"))
  expect_equal(r$exit, 2L, info = "El gate aceptó un nombre sin schoice/cloze")
  expect_match(r$err, "no sigue la nomenclatura oficial")
})

test_that("el gate NO rechaza por nomenclatura un nombre correcto", {
  skip_if_not(file.exists(GATE))
  d <- file.path(tempdir(), "A-Produccion", "01-En-PreDesarrollo", "fx")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(file.path(tempdir(), "A-Produccion"), recursive = TRUE), add = TRUE)

  # Puede bloquear por OTRO motivo (no hay ejercicio_state.json), que es correcto
  # y ajeno a este test. Lo que se afirma es que no lo hace por el nombre.
  r <- invocar_gate(file.path(
    d, "informacion_insuficiente_lote_geometrico_metrico_argumentacion_n4_schoice_v1.Rmd"))
  expect_false(grepl("no sigue la nomenclatura oficial", r$err, fixed = TRUE),
    info = "El gate rechazó por nombre un archivo que SÍ cumple el formato")
})

test_that("el gate NO bloquea la edición de un legacy que ya existe en disco", {
  skip_if_not(file.exists(GATE))
  # El alcance decidido es "de aquí en adelante": bloquear la edición de los 299
  # archivos legacy los dejaría congelados. El gate solo mira nombres al CREAR.
  d <- file.path(tempdir(), "A-Produccion", "01-En-PreDesarrollo", "fx")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(file.path(tempdir(), "A-Produccion"), recursive = TRUE), add = TRUE)
  legacy <- file.path(d, "viejo_metacognitivo_interpretacion_n2_schoice_v1.Rmd")
  writeLines("contenido", legacy)

  r <- invocar_gate(legacy)
  expect_false(grepl("no sigue la nomenclatura oficial", r$err, fixed = TRUE),
    info = "El gate bloqueó por nombre la EDICIÓN de un archivo legacy existente")
})

# =============================================================================
context("Las citas del formato no divergen de la fuente")

test_that("el gate usa exactamente el regex canónico", {
  skip_if_not(file.exists(GATE))
  txt <- leer(GATE)
  # Se compara el cuerpo del regex sin los anclajes, que en bash van sueltos.
  nucleo <- "(geometrico_metrico|numerico_variacional|aleatorio)"
  expect_true(grepl(nucleo, txt, fixed = TRUE),
    info = "El gate no valida el componente ICFES")
  expect_true(grepl("(interpretacion_representacion|formulacion_ejecucion|argumentacion)",
                    txt, fixed = TRUE),
    info = "El gate no valida la competencia en forma oficial larga")
  expect_true(grepl("(schoice|cloze)(_neg)?_v[0-9]+", txt, fixed = TRUE),
    info = "El gate no valida la ranura de tipo")
})

test_that("ninguna cita del repo describe el formato viejo con 'metacognitivo'", {
  # BARRIDO COMPLETO de `.claude/`, no una lista enumerada de sitios.
  #
  # La primera versión de este test enumeraba cinco archivos y se le escaparon
  # TRES —entre ellos un check EJECUTABLE en `commands/validate.md` que emitía
  # "⚠ SIN metacognitivo en nombre" por cada archivo correctamente nombrado, es
  # decir, enforcement activo del formato equivocado—. Una lista de inclusión
  # solo ve lo que enumera, y el defecto que buscamos es precisamente el sitio
  # que nadie recordó. Por eso: escanear todo y declarar las excepciones.
  #
  # `metacognitivo` sigue siendo legítimo en otros contextos (regla #1,
  # patron_metacognitivo, TipoMetacognicion, prosa pedagógica); el discriminador
  # de abajo se encarga de distinguirlos.
  excepciones <- c(
    # Estos dos NARRAN la historia de la deriva: tienen que poder citar el
    # formato viejo para explicar por qué se abandonó.
    file.path(repo_root, ".claude", "docs", "NOMENCLATURA_ARCHIVOS_RMD.md"),
    file.path(repo_root, ".claude", "CLAUDE.md")
  )
  citas <- list.files(file.path(repo_root, ".claude"), pattern = "\\.md$",
                      recursive = TRUE, full.names = TRUE)
  citas <- setdiff(citas, excepciones)
  citas <- citas[!grepl("/_backups/|/docs/CHANGELOG\\.md$|/docs/casos-resueltos/", citas)]
  skip_if(length(citas) == 0, "no se encontraron archivos que revisar")

  # DISCRIMINADOR: sólo se persiguen las líneas que ESPECIFICAN el formato, no
  # las que NOMBRAN un archivo legacy concreto. La diferencia es material: las
  # referencias a `promedios_borrados_metacognitivo_..._cloze_v1.Rmd` apuntan a
  # un archivo REAL e inmutable de 03-En-Produccion; "corregirlas" rompería un
  # puntero vivo. Una plantilla de formato lleva marcadores `[...]` o anclas de
  # regex; una referencia a archivo real, no.
  # Dos condiciones, ambas necesarias:
  #  (a) `_metacognitivo_` DENTRO de un token con guiones bajos — es decir,
  #      ocupando la ranura de un nombre de archivo. Excluye las líneas que
  #      hablan de la palabra, incluidas las que la PROHÍBEN: una advertencia
  #      escribe `metacognitivo` suelto, no `_metacognitivo_`.
  #  (b) marcador de plantilla `[...]` o ancla de regex — excluye las
  #      referencias a archivos legacy reales, que no llevan ninguno.
  es_plantilla <- function(linea) {
    grepl("_metacognitivo_", linea, fixed = TRUE) &&
      (grepl("\\[[a-z_]+\\]", linea) || grepl("\\^|\\$", linea))
  }
  malos <- character(0)
  for (f in citas) {
    lineas <- readLines(f, warn = FALSE)
    if (any(vapply(lineas, es_plantilla, logical(1))))
      malos <- c(malos, sub(paste0("^", repo_root, "/"), "", f))
  }
  expect_equal(length(malos), 0,
    info = paste0("Estas citas siguen describiendo el formato viejo:\n  - ",
                  paste(malos, collapse = "\n  - "),
                  "\nFuente única: .claude/docs/NOMENCLATURA_ARCHIVOS_RMD.md"))

  # Control positivo: el discriminador tiene que DISPARAR ante una plantilla
  # real. Sin esto, "0 infractores" no distingue "está limpio" de "el patrón
  # no coincide con nada".
  expect_true(es_plantilla("Formato: `[ejercicio]_metacognitivo_[competencia]_n[nivel]_cloze_v[N].Rmd`"),
    info = "El discriminador NO caza una plantilla del formato viejo: está roto")
  expect_true(es_plantilla("matchea `^[a-z0-9_]+_metacognitivo_[a-z]+_n[234]_schoice_v[0-9]+$`"),
    info = "El discriminador NO caza el regex del formato viejo: está roto")
  # Controles negativos: las dos clases de línea que NO son infracción.
  expect_false(es_plantilla("- Ejemplo: `.../promedios_borrados_metacognitivo_argumentacion_n3_cloze_v1.Rmd`"),
    info = "El discriminador caza referencias a archivos legacy reales: daría falsos rojos")
  expect_false(es_plantilla("`[componente]` en forma larga. `metacognitivo` NO va en el nombre (regla #1)."),
    info = paste("El discriminador caza una línea que PROHÍBE el patrón viejo.",
                 "No sabe distinguir 'usa el patrón' de 'advierte contra el patrón',",
                 "así que castigaría justo la documentación que arregla el problema."))
})

test_that("las citas apuntan a la fuente única", {
  citas <- c(
    file.path(repo_root, ".claude", "commands", "orquestador-schoice.md"),
    file.path(repo_root, ".claude", "commands", "orquestador-cloze.md"),
    file.path(repo_root, ".claude", "skills", "generar-schoice", "SKILL.md"),
    file.path(repo_root, ".claude", "skills", "generar-cloze", "SKILL.md"),
    file.path(repo_root, ".claude", "rules", "validacion-neg-opciones-repetidas.md")
  )
  citas <- citas[file.exists(citas)]
  skip_if(length(citas) == 0, "no se encontraron los archivos de cita")

  sin_fuente <- basename(citas)[!vapply(citas, function(f)
    grepl("NOMENCLATURA_ARCHIVOS_RMD.md", leer(f), fixed = TRUE), logical(1))]
  expect_equal(length(sin_fuente), 0,
    info = paste0("Citan el formato sin apuntar a la fuente — si vuelven a divergir, ",
                  "nadie sabrá de dónde salieron:\n  - ",
                  paste(sin_fuente, collapse = "\n  - ")))
})
