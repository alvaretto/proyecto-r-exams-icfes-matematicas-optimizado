#!/usr/bin/env Rscript
## =============================================================================
## verificar_espacio.R — verificador PROPIO de este ejercicio
##
## POR QUE EXISTE (punto ciego declarado del arsenal):
##   Los distractores de este item no son VALORES NUMERICOS sino CONJUNTOS DE
##   DATOS. `validar_coherencia_matematica.R` inspecciona el pool (Capas A-D) y
##   `validar_diagnosticidad.R` mide canales de texto, pero NINGUNO puede decidir
##   si un conjunto de datos resuelve el problema: eso exige geometria, no
##   estadistica. La Capa B (21 keywords) es de estadistica descriptiva y aqui no
##   tiene reglas aplicables, asi que su APROBADO no acredita nada sobre la
##   correccion conceptual del pool.
##   Este script cierra ese hueco: enumera el ESPACIO COMPLETO y prueba UNICIDAD
##   DE CLAVE, que es la propiedad que el ejercicio necesita y nadie mas mira.
##
## ACOPLAMIENTO DECLARADO: `N_DATOS` y `MOLDE_INSUFICIENTE` son INSEPARABLES por
## construccion -- cambiar el numero de datos de una opcion cambia tambien su
## molde. M3 disparara siempre las dos. Se declara en vez de presentarlo como
## un mutante que "aisla" su sonda, que seria falso.
##
## CONTRATO DE MUTACION (Incidente P, INC-MUTANTE-SONDA):
##   cada mutante DECLARA su sonda esperada y la fase falla si muere por otra.
##   Ademas se comprueba que la mutacion llego al ENTORNO, no al texto.
##
## Uso:  Rscript verificar_espacio.R [ruta.Rmd]        (N = 100, regla #23)
## =============================================================================

args <- commandArgs(trailingOnly = TRUE)
rmd  <- if (length(args) >= 1) args[1] else
  "teorema_coseno_datos_suficientes_geometrico_metrico_formulacion_ejecucion_n3_schoice_v1.Rmd"
N <- 100L
errores <- character(0)
fallo <- function(...) errores <<- c(errores, paste0(...))

cat("=============================================================\n")
cat("VERIFICADOR PROPIO — teorema-coseno-datos-suficientes-n3\n")
cat("archivo:", basename(rmd), "| N =", N, "\n")
cat("=============================================================\n\n")

## --- PARTE 1: enumeracion del espacio COMPLETO ------------------------------
## Criterio (invariante local I-7): "una unica vez el teorema del Coseno" =
## UNA sustitucion entrega lo pedido con valor unico. Para un lado hacen falta
## los otros dos lados y el angulo comprendido, que es el opuesto al buscado.
## La suma de angulos internos es aritmetica libre.
LAD <- c("x", "y", "z"); ANG <- c("X", "Y", "Z")
cierre <- function(a) if (length(a) == 2L) union(a, setdiff(ANG, a)) else a
resuelve <- function(lados, angs, objetivo = "x") {
  a <- cierre(angs)
  all(setdiff(LAD, objetivo) %in% lados) && (toupper(objetivo) %in% a)
}

universo <- setdiff(c(LAD, ANG), "x")          # el lado pedido no puede ser un dato
combos   <- combn(universo, 3L, simplify = FALSE)
es_clave <- vapply(combos, function(K)
  resuelve(intersect(K, LAD), intersect(K, ANG), "x"), NA)
molde    <- vapply(combos, function(K)
  paste0(length(intersect(K, LAD)), "L+", length(intersect(K, ANG)), "A"), "")

cat("PARTE 1 — espacio completo de conjuntos de 3 datos\n")
cat("  subconjuntos posibles :", length(combos), "\n")
cat("  claves                :", sum(es_clave), " ->",
    paste(vapply(combos[es_clave], paste, "", collapse = ","), collapse = " | "), "\n")
cat("  distractores legitimos:", sum(!es_clave), "\n")
cat("  del mismo molde que la clave:",
    sum(molde == molde[es_clave] & !es_clave), "\n")
if (sum(es_clave) != 1L) fallo("CLAVE_NO_UNICA: el espacio admite ", sum(es_clave), " claves")
if (sum(molde == molde[es_clave] & !es_clave) < 2L)
  fallo("MOLDE_SIN_GEMELO: no hay 2 distractores del molde de la clave")
cat("  [clave unica en el espacio]", if (sum(es_clave) == 1L) "OK\n\n" else "FALLA\n\n")

## --- PARTE 2: las 4 opciones realmente mostradas, en N versiones ------------
L <- readLines(rmd, warn = FALSE)
i <- grep("data_generation", L, fixed = TRUE)[1]
j <- which(trimws(L) == "```"); j <- j[j > i][1]
codigo <- paste(L[(i + 1):(j - 1)], collapse = "\n")

dir.create(file.path(tempdir(), "vfigs"), showWarnings = FALSE)
old <- setwd(file.path(tempdir(), "vfigs")); on.exit(setwd(old), add = TRUE)

envs <- vector("list", N)
for (k in seq_len(N)) {
  set.seed(k * 7919L + 13L)
  e <- new.env(parent = globalenv())
  ok <- tryCatch({ eval(parse(text = codigo), envir = e); TRUE },
                 error = function(x) { fallo("RENDER_FALLA (semilla ", k, "): ",
                                             conditionMessage(x)); FALSE })
  envs[[k]] <- if (ok) e else NULL
}
setwd(old)
envs <- Filter(Negate(is.null), envs)
cat("PARTE 2 — verificacion sobre", length(envs), "versiones generadas\n")

n_can <- 0L; claves_txt <- character(0); moldes_tab <- integer(0)
for (e in envs) {
  conjuntos <- c(list(list(lados = e$clave_lados, angulos = e$clave_angulos)),
                 lapply(e$distractores, function(d) list(lados = d$lados, angulos = d$angulos)))
  ## (a) unicidad de clave ENTRE LAS 4 OPCIONES mostradas
  n_res <- sum(vapply(conjuntos, function(c0)
    e$resuelve_un_coseno(c0$lados, c0$angulos, e$x), NA))
  if (n_res != 1L) fallo("SEGUNDA_CLAVE: ", n_res, " opciones resuelven en una aplicacion")
  ## (b) la clave es la que esta marcada
  if (!identical(e$opciones[which(e$sol == 1L)], e$clave_texto))
    fallo("CLAVE_MAL_MARCADA")
  ## (c) todas con 3 datos (canal de recuento cerrado)
  nd <- vapply(conjuntos, function(c0) length(c0$lados) + length(c0$angulos), 0L)
  if (!all(nd == 3L)) fallo("N_DATOS: una opcion no tiene 3 datos (", paste(nd, collapse = ","), ")")
  ## (d) ninguna opcion contiene el lado pedido
  if (any(vapply(conjuntos, function(c0) e$x %in% c0$lados, NA))) fallo("PEDIDO_EN_OPCION")
  ## (e) opciones distintas
  if (length(unique(e$opciones)) != 4L) fallo("OPCIONES_DUPLICADAS")
  ## (f) molde compartido
  moldes_tab <- c(moldes_tab, e$n_molde_clave)
  if (e$n_molde_clave < if (isTRUE(e$es_canonica)) 2L else 3L) fallo("MOLDE_INSUFICIENTE")
  ## (g) instancia canonica verbatim
  if (isTRUE(e$es_canonica)) {
    n_can <- n_can + 1L
    if (!identical(e$pregunta, "¿Con cuál de los siguientes datos es posible calcular el valor de **q** usando una única vez el teorema del Coseno?"))
      fallo("CANONICA_NO_VERBATIM: enunciado")
    if (!setequal(e$opciones, c("Lados r y s y ángulo Q", "Lado r y ángulos R y Q",
                                "Lado s y ángulos R y Q", "Lados r y s y ángulo R")))
      fallo("CANONICA_NO_VERBATIM: opciones")
  }
  claves_txt <- c(claves_txt, e$clave_texto)
}
cat("  instancias canonicas        :", n_can, "\n")
cat("  textos de clave distintos   :", length(unique(claves_txt)), "\n")
cat("  opciones con el molde clave :"); print(table(moldes_tab))
cat("\n")

## --- PARTE 3: pruebas de mutacion con sonda declarada -----------------------
## Se mutan ESTRUCTURAS EN MEMORIA, nunca el archivo en disco.
cat("PARTE 3 — pruebas de mutacion (contrato: cada mutante muere por SU sonda)\n")
e0 <- envs[[which(!vapply(envs, function(e) isTRUE(e$es_canonica), NA))[1]]]

sondas <- function(conjuntos, env, canonica = FALSE) {
  b <- character(0)
  n_res <- sum(vapply(conjuntos, function(c0)
    env$resuelve_un_coseno(c0$lados, c0$angulos, env$x), NA))
  if (n_res > 1L) b <- c(b, "SEGUNDA_CLAVE")
  if (n_res < 1L) b <- c(b, "CLAVE_NO_RESUELVE")
  nd <- vapply(conjuntos, function(c0) length(c0$lados) + length(c0$angulos), 0L)
  if (!all(nd == 3L)) b <- c(b, "N_DATOS")
  m <- vapply(conjuntos, function(c0)
    paste0(length(c0$lados), "L+", length(c0$angulos), "A"), "")
  if (sum(m == m[1]) < if (canonica) 2L else 3L) b <- c(b, "MOLDE_INSUFICIENTE")
  ## sonda que faltaba: ninguna opcion puede citar el propio lado pedido
  if (any(vapply(conjuntos, function(c0) env$x %in% c0$lados, NA)))
    b <- c(b, "PEDIDO_EN_OPCION")
  b
}

base_conj <- c(list(list(lados = e0$clave_lados, angulos = e0$clave_angulos)),
               lapply(e0$distractores, function(d) list(lados = d$lados, angulos = d$angulos)))
stopifnot(length(sondas(base_conj, e0)) == 0L)   # control: el original NO dispara

mutantes <- list(
  list(id = "M1", sonda = "CLAVE_NO_RESUELVE",
       desc = "la clave lleva un angulo adyacente en vez del comprendido",
       mut  = function(cj) { cj[[1]]$angulos <- toupper(e0$y); cj },
       llego = function(cj) !identical(cj[[1]]$angulos, toupper(e0$X))),
  list(id = "M2", sonda = "SEGUNDA_CLAVE",
       desc = "un distractor incluye los dos lados y el angulo comprendido",
       mut  = function(cj) { cj[[2]]$lados <- c(e0$y, e0$z)
                             cj[[2]]$angulos <- c(e0$X); cj },   # 3 datos: AISLA la sonda
       llego = function(cj) e0$resuelve_un_coseno(cj[[2]]$lados, cj[[2]]$angulos, e0$x)),
  list(id = "M3", sonda = "N_DATOS",
       desc = "un distractor con solo 2 datos (el GEO-COS-07 retirado)",
       mut  = function(cj) { cj[[3]]$lados <- c(e0$y); cj[[3]]$angulos <- c(e0$X); cj },
       llego = function(cj) (length(cj[[3]]$lados) + length(cj[[3]]$angulos)) == 2L),
  list(id = "M5", sonda = "PEDIDO_EN_OPCION",
       desc = "un distractor cita el propio lado pedido",
       mut  = function(cj) { cj[[2]]$lados <- c(e0$x, e0$y); cj },
       llego = function(cj) e0$x %in% cj[[2]]$lados),
  list(id = "M4", sonda = "MOLDE_INSUFICIENTE",
       desc = "se retira uno de los dos distractores del molde de la clave",
       mut  = function(cj) { cj[[3]]$lados <- c(e0$y)
                             cj[[3]]$angulos <- c(e0$X, toupper(e0$y))
                             cj[[2]]$lados <- c(e0$z)
                             cj[[2]]$angulos <- c(e0$X, toupper(e0$z)); cj },
       llego = function(cj) sum(vapply(cj, function(c0) length(c0$lados), 0L) == 2L) < 2L)
)

res_mut <- data.frame(mutante = character(0), sonda_esperada = character(0),
                      sonda_real = character(0), veredicto = character(0),
                      stringsAsFactors = FALSE)
for (m in mutantes) {
  cj <- m$mut(base_conj)
  if (!m$llego(cj)) {
    fallo("MUTANTE ", m$id, " MAL CONSTRUIDO: la mutacion no llego al entorno")
    v <- "mal_construido"; reales <- "-"
  } else {
    b <- sondas(cj, e0)
    reales <- if (length(b)) paste(b, collapse = "+") else "ninguna"
    if (!any(grepl(m$sonda, b, fixed = TRUE))) {
      fallo("MUTANTE ", m$id, " CAZADO POR LA SONDA EQUIVOCADA: esperaba ",
            m$sonda, ", disparo ", reales)
      v <- if (length(b)) "cazado_por_otra" else "no_detectado"
    } else v <- "cazado_por_su_sonda"
  }
  res_mut <- rbind(res_mut, data.frame(mutante = m$id, sonda_esperada = m$sonda,
                                       sonda_real = reales, veredicto = v,
                                       stringsAsFactors = FALSE))
  cat(sprintf("  %s  %-22s esperada=%-19s real=%-28s %s\n",
              m$id, substr(m$desc, 1, 22), m$sonda, reales, v))
}
cat("\n")

## --- VEREDICTO ---------------------------------------------------------------
cat("=============================================================\n")
if (length(errores) == 0L) {
  cat("RESULTADO: APROBADO (0 errores)\n")
  cat("  clave unica en el espacio completo Y en las 4 opciones de las", length(envs), "versiones\n")
  cat("  ", nrow(res_mut), "mutantes, todos cazados por su propia sonda\n")
  cat("=============================================================\n")
  quit(status = 0)
} else {
  cat("RESULTADO: RECHAZADO (", length(errores), " errores)\n", sep = "")
  for (x in unique(errores)) cat("  - ", x, "\n", sep = "")
  cat("=============================================================\n")
  quit(status = 1)
}
