## Construye verificar_s2c.R a partir de verificar_espacio.R (md5 9c654748…), sin tocar el original:
## E1-E4 y la decisión 2 a íntegros, sin los mutantes de E5; inyección del candidato en memoria; parches de
## robustez a NA (solo D2 tiene una opción sin cifra; con cifras, idénticos) y bloque S2c de medidas adicionales.
src <- "../../../verificar_espacio.R"
stopifnot(unname(tools::md5sum(src)) == "9c65474801b5b838183a0507233b8ea1")
L <- readLines(src, encoding = "UTF-8"); txt <- paste(L, collapse = "\n")
rep1 <- function(txt, viejo, nuevo) { pos <- gregexpr(viejo, txt, fixed = TRUE)[[1]]
  if (length(pos) != 1L || pos[1] < 0L) stop("patrón no único: ", substr(viejo, 1, 70))
  paste0(substr(txt, 1L, pos[1] - 1L), nuevo, substr(txt, pos[1] + nchar(viejo), nchar(txt))) }
## E5 fuera: se corta en su encabezado
corte <- regexpr("## ---- E5. Mutantes", txt, fixed = TRUE); stopifnot(corte > 0)
txt <- substr(txt, 1L, corte - 1L)
txt <- rep1(txt, 'CODIGO <- paste(l[(i0 + 1):(i1 - 1)], collapse = "\\n")', paste0(
'CODIGO <- paste(l[(i0 + 1):(i1 - 1)], collapse = "\\n")
## ---- S2c: mutación del candidato, en memoria (no toca el .Rmd) ----
S2C_ID <- Sys.getenv("S2C_ID", "B0"); S2C_CAND <- Sys.getenv("S2C_CAND", ""); S2C_DIR <- Sys.getenv("S2C_DIR", ".")
reemplazar1 <- function(txt, viejo, nuevo) { pos <- gregexpr(viejo, txt, fixed = TRUE)[[1]]
  if (length(pos) != 1L || pos[1] < 0L) stop(sprintf("S2c: el patrón «%s» aparece %d veces (se exige 1)",
    substr(viejo, 1, 60), if (pos[1] < 0L) 0L else length(pos)))
  paste0(substr(txt, 1L, pos[1] - 1L), nuevo, substr(txt, pos[1] + nchar(viejo), nchar(txt))) }
if (nzchar(S2C_CAND)) { source(S2C_CAND, local = TRUE, encoding = "UTF-8")
  for (cb in CAMBIOS) CODIGO <- reemplazar1(CODIGO, cb[1], cb[2])
  cat(sprintf("S2c: candidato %s, %d cambios aplicados en memoria (patrón único cada uno)\\n", S2C_ID, length(CAMBIOS))) }
writeLines(CODIGO, file.path(S2C_DIR, sprintf("codigo_%s.R", S2C_ID)), useBytes = TRUE)'))
txt <- rep1(txt, '  lado = if (isTRUE(es_canonica)) NA_character_ else lado_gdis))', paste0(
'  lado = if (isTRUE(es_canonica)) NA_character_ else lado_gdis))
rasgos0 <- rasgos
rasgos <- function(env) c(rasgos0(env), list(
  s2c_fb = isTRUE(get0("s2c_fallback", envir = env, inherits = FALSE)),
  s2c_pd = isTRUE(get0("s2c_padv_delta", envir = env, inherits = FALSE)),
  s2c_sd = isTRUE(get0("s2c_psin_delta", envir = env, inherits = FALSE)), clave_txt = env$afs$clave$texto))'))
txt <- rep1(txt, 'for (b in names(CU)) ok(length(CU[[b]]) >= 20,', 'S2C_E2_ULT <- s\nfor (b in names(CU)) ok(length(CU[[b]]) >= 20,')
txt <- rep1(txt, 'lado_cumple <- function(x) isTRUE(x$canon) || isTRUE(', 'lado_cumple <- function(x) isTRUE(x$canon) || is.na(x$X) || isTRUE(')
txt <- rep1(txt, 'env0 <- correr(1L)$env', paste0('env0 <- correr(1L)$env
esp_s2c <- function(b) { e <- env0$espacio_rama(b)
  if ("xP_delta" %in% names(formals(env0$espacio_rama))) { e2 <- env0$espacio_rama(b, xP_delta = TRUE)
    cc <- intersect(names(e), names(e2)); e <- unique(rbind(e[, cc], e2[, cc])) }
  rownames(e) <- NULL; e }'))
txt <- rep1(txt, '  esp <- env0$espacio_rama(b); filas[[b]] <- nrow(esp)', '  esp <- esp_s2c(b); filas[[b]] <- nrow(esp)')
txt <- rep1(txt, '  esp <- env0$espacio_rama(b); pG <- if', '  esp <- esp_s2c(b); pG <- if')
txt <- rep1(txt, '      lapply(mtot(pP), function(m) list(m = m, x = f$parcialP, sede = "P")),
      lapply(c("U_a", "U_b", "U_c", "U_d"), function(m) list(m = m, x = f$D + 1, sede = "P")))',
'      do.call(c, lapply(mtot(pP), function(m) lapply(unique(c(f$parcialP, f$delta)), function(x) list(m = m, x = x, sede = "P")))),
      do.call(c, lapply(c("U_a", "U_b", "U_c", "U_d"), function(m) lapply(c(f$D + 1, f$delta), function(x) list(m = m, x = x, sede = "P")))),
      if (isTRUE(env0$D2_SIN_CIFRA)) lapply(c(mtot(pG), "N_a", "N_b"), function(m) list(m = m, x = NA_real_, sede = "G")))')
txt <- rep1(txt, '      if (cc$sede == "G" && cc$x != f$D) {', '      if (cc$sede == "G" && isTRUE(cc$x != f$D)) {')
stopifnot(lengths(regmatches(txt, gregexpr("as.numeric(x$X < x$D)", txt, fixed = TRUE))) == 2L)
txt <- gsub("as.numeric(x$X < x$D)", "as.numeric(isTRUE(x$X < x$D))", txt, fixed = TRUE)
txt <- rep1(txt, '      S <- x$sede_o == "G"; v <- vapply(x$opciones, cif, 0); i <- which(S)', '      S <- x$sede_o == "G"; v <- vapply(x$opciones, cif, 0); i <- which(S & !is.na(v))')
txt <- rep1(txt, 'for (est in list(list("mezcla natural", NAT), list("rama CURSOS", CU$CURSOS), list("rama PROMEDIO", CU$PROMEDIO))) {',
                 'BAT_S2C <- list()\nfor (est in list(list("mezcla natural", NAT), list("rama CURSOS", CU$CURSOS), list("rama PROMEDIO", CU$PROMEDIO))) {')
txt <- rep1(txt, '  if (est[[1]] == "mezcla natural") VEREDICTO_MEZCLA <- res', '  if (est[[1]] == "mezcla natural") VEREDICTO_MEZCLA <- res
  BAT_S2C[[est[[1]]]] <- list(max = res$max_obs, techo = res$techo_nulo, exceso = res$exceso, regla = res$regla_top, veredicto = res$veredicto)')
extra <- readLines("bloque_s2c.R", encoding = "UTF-8")
writeLines(c(strsplit(txt, "\n", fixed = TRUE)[[1]], extra), "verificar_s2c.R", useBytes = TRUE)
cat("verificar_s2c.R escrito:", unname(tools::md5sum("verificar_s2c.R")), "\n")
