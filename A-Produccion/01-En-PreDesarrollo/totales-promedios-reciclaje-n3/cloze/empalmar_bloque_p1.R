## empalmar_bloque_p1.R — trasplanta el data_generation del SCHOICE aprobado a la Parte 1 del CLOZE (F-4, paso 3).
## Copia por script, no a mano (riesgo principal para E1): toma las líneas 2..(cierre-1) del chunk del SCHOICE,
## aplica tres renombres en cuatro líneas exactas y sustituye la línea marcadora del CLOZE.
## Tanda C1 del ciclo 2026-11-02 (2026-11-03): el SCHOICE aprobado pasa de e293d5c8 (líneas 2-561) a 63e2713e
## (líneas 2-612). Si el CLOZE no tiene el marcador, el script comprueba primero que sus líneas 2-561 son EXACTAMENTE
## el bloque viejo (e293d5c8, leído de git en 8efdf504, con sus 4 renombres) y solo entonces las cambia por el
## marcador; después empalma el bloque nuevo. Idempotente: si el bloque nuevo ya está, no hace nada.
## Uso: Rscript --no-init-file empalmar_bloque_p1.R  (desde cloze/)
args_all <- commandArgs(FALSE); f_arg <- sub("^--file=", "", args_all[grep("^--file=", args_all)])
DIR <- if (length(f_arg)) dirname(normalizePath(f_arg)) else getwd()
S <- file.path(DIR, "..", "totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd")
C <- file.path(DIR, "totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd")
MD5_NUEVO <- "63e2713e6ac0bdbf250f4dc33901b2ec"; MD5_VIEJO <- "e293d5c812b1f9790e2bb777b62de9a5"
stopifnot("SCHOICE aprobado (md5)" = unname(tools::md5sum(S)) == MD5_NUEVO)
MARCADOR <- "## @@BLOQUE_P1_DEL_SCHOICE@@"
REN <- c("opciones <- unname(textos[o])" = "opciones_p1 <- unname(textos[o])",
         "sol <- unname(verdaderas[o])" = "sol_p1 <- unname(verdaderas[o])",
         "respuesta_correcta <- opciones[sol]" = "respuesta_correcta <- opciones_p1[sol_p1]",
         "feedback <- unname(fb[rol_opcion])" = "feedback_p1 <- unname(fb[rol_opcion])")
bloque <- function(ls_, fin_esperado) {
  stopifnot("cabecera del chunk" = grepl("^```\\{r data_generation", ls_[1]))
  fin <- which(ls_ == "```")[1]
  if (fin != fin_esperado) stop(sprintf("el cierre del chunk está en la línea %d, no en la %d", fin, fin_esperado))
  blk <- ls_[2:(fin - 1L)]
  for (k in names(REN)) { i <- which(blk == k); stopifnot("cada renombre aparece una sola vez" = length(i) == 1L); blk[i] <- REN[[k]] }
  blk
}
ls_ <- readLines(S, encoding = "UTF-8"); lc <- readLines(C, encoding = "UTF-8")
blk <- bloque(ls_, 613L)                                   # bloque nuevo: líneas 2 a 612 de 63e2713e
## Ningún otro símbolo de CÓDIGO se llama opciones, sol o feedback: se mira con el analizador de R, así que
## los comentarios y los literales de texto («opciones únicas») no cuentan.
pd <- utils::getParseData(parse(text = blk, keep.source = TRUE, encoding = "UTF-8"))
quedan <- pd[pd$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_SUB") &
               pd$text %in% c("opciones", "sol", "feedback"), c("line1", "text")]
if (nrow(quedan)) print(quedan)
stopifnot("sin otros usos de opciones/sol/feedback en el código del bloque" = nrow(quedan) == 0L)
if (identical(lc[2:(length(blk) + 1L)], blk)) { cat("El bloque de 63e2713e ya está empalmado.\n"); quit(status = 0) }
if (sum(lc == MARCADOR) == 0L) {
  ## Paso previo: el bloque viejo, comprobado línea a línea, se cambia por el marcador.
  REL <- "A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd"
  viejo <- system2("git", c("-C", DIR, "show", paste0("8efdf504:", REL)), stdout = TRUE)
  tmp <- tempfile(fileext = ".Rmd"); writeLines(viejo, tmp, useBytes = FALSE)
  stopifnot("SCHOICE viejo leído de git (md5)" = unname(tools::md5sum(tmp)) == MD5_VIEJO)
  blk_viejo <- bloque(readLines(tmp, encoding = "UTF-8"), 562L)   # líneas 2 a 561 de e293d5c8
  stopifnot("el bloque viejo tiene 560 líneas" = length(blk_viejo) == 560L,
            "las líneas 2-561 del CLOZE son el bloque viejo con sus 4 renombres" = identical(lc[2:561], blk_viejo))
  lc <- c(lc[1], MARCADOR, lc[562:length(lc)])
  cat("Bloque viejo (e293d5c8, líneas 2-561, 560 líneas) comprobado y sustituido por el marcador.\n")
}
stopifnot("marcador único" = sum(lc == MARCADOR) == 1L)
i <- which(lc == MARCADOR)
out <- c(lc[seq_len(i - 1L)], blk, lc[(i + 1L):length(lc)])
writeLines(out, C, useBytes = FALSE)
cat(sprintf("Empalmadas %d líneas (SCHOICE %s, líneas 2..%d) con 4 renombres; .Rmd: %d líneas, md5 %s\n",
            length(blk), substr(MD5_NUEVO, 1, 8), length(blk) + 1L, length(out), unname(tools::md5sum(C))))
