## empalmar_bloque_p1.R — trasplanta el data_generation del SCHOICE aprobado a la Parte 1 del CLOZE (F-4, paso 3).
## Copia por script, no a mano (riesgo principal para E1): toma las líneas 2..(cierre-1) del chunk del SCHOICE,
## aplica tres renombres en cuatro líneas exactas y sustituye la línea marcadora del CLOZE. Idempotente: si el
## marcador ya no está, no hace nada. Uso: Rscript --no-init-file empalmar_bloque_p1.R  (desde cloze/)
args_all <- commandArgs(FALSE); f_arg <- sub("^--file=", "", args_all[grep("^--file=", args_all)])
DIR <- if (length(f_arg)) dirname(normalizePath(f_arg)) else getwd()
S <- file.path(DIR, "..", "totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd")
C <- file.path(DIR, "totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd")
stopifnot("SCHOICE aprobado (md5)" = unname(tools::md5sum(S)) == "e293d5c812b1f9790e2bb777b62de9a5")
ls_ <- readLines(S, encoding = "UTF-8"); lc <- readLines(C, encoding = "UTF-8")
MARCADOR <- "## @@BLOQUE_P1_DEL_SCHOICE@@"
if (sum(lc == MARCADOR) == 0L) { cat("Sin marcador: el bloque ya está empalmado.\n"); quit(status = 0) }
stopifnot("marcador único" = sum(lc == MARCADOR) == 1L, "cabecera del chunk" = grepl("^```\\{r data_generation", ls_[1]))
fin <- which(ls_ == "```")[1]
stopifnot("bloque P1 = líneas 2 a 561 (dry-run)" = fin == 562L)
blk <- ls_[2:(fin - 1L)]
REN <- c("opciones <- unname(textos[o])" = "opciones_p1 <- unname(textos[o])",
         "sol <- unname(verdaderas[o])" = "sol_p1 <- unname(verdaderas[o])",
         "respuesta_correcta <- opciones[sol]" = "respuesta_correcta <- opciones_p1[sol_p1]",
         "feedback <- unname(fb[rol_opcion])" = "feedback_p1 <- unname(fb[rol_opcion])")
for (k in names(REN)) { i <- which(blk == k); stopifnot(length(i) == 1L); blk[i] <- REN[[k]] }
## Ningún otro símbolo de CÓDIGO se llama opciones, sol o feedback: se mira con el analizador de R, así que
## los comentarios y los literales de texto («opciones únicas») no cuentan.
pd <- utils::getParseData(parse(text = blk, keep.source = TRUE, encoding = "UTF-8"))
quedan <- pd[pd$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_SUB") &
               pd$text %in% c("opciones", "sol", "feedback"), c("line1", "text")]
if (nrow(quedan)) print(quedan)
stopifnot("sin otros usos de opciones/sol/feedback en el código del bloque" = nrow(quedan) == 0L)
i <- which(lc == MARCADOR)
out <- c(lc[seq_len(i - 1L)], blk, lc[(i + 1L):length(lc)])
writeLines(out, C, useBytes = FALSE)
cat(sprintf("Empalmadas %d líneas (SCHOICE 2..%d) con 4 renombres; .Rmd: %d líneas, md5 %s\n",
            length(blk), fin - 1L, length(out), unname(tools::md5sum(C))))
