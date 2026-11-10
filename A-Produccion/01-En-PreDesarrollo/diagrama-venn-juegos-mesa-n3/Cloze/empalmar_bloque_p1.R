## empalmar_bloque_p1.R — trasplanta el chunk data_generation del SCHOICE firmado a la Parte 1 del CLOZE (C-2, paso 3).
## Por script, no a mano (es el riesgo principal para E1). Pasos:
##   1. Toma el CUERPO firmado del SCHOICE: si el archivo empieza con el YAML de un Knit de RStudio, lo que sigue a ese
##      bloque; si no, el archivo entero. Su md5 debe ser f886dfaa… (firma del profesor del 2026-11-06).
##   2. Extrae su chunk data_generation y renombra tres símbolos de CÓDIGO con el analizador de R: opciones, sol y feedback
##      pasan a opciones_p1, sol_p1 y feedback_p1. Los comentarios y los textos no se tocan.
##   3. Escribe el bloque entre los marcadores @@INICIO_BLOQUE_P1@@ y @@FIN_BLOQUE_P1@@ del CLOZE. Idempotente.
## No escribe fuera de Cloze/. Uso: cd Cloze && Rscript --no-init-file empalmar_bloque_p1.R
args_all <- commandArgs(FALSE); f_arg <- sub("^--file=", "", args_all[grep("^--file=", args_all)])
DIR <- if (length(f_arg)) dirname(normalizePath(f_arg)) else getwd()
S <- file.path(DIR, "..", "diagrama_venn_juegos_mesa_aleatorio_interpretacion_representacion_n3_schoice_v1.Rmd")
C <- file.path(DIR, "diagrama_venn_juegos_mesa_aleatorio_interpretacion_representacion_n3_cloze_v1.Rmd")
MD5_CUERPO <- "f886dfaa45b80a5ee16f9820908049b5"
INI <- "## @@INICIO_BLOQUE_P1@@"; FIN <- "## @@FIN_BLOQUE_P1@@"
REN <- c(opciones = "opciones_p1", sol = "sol_p1", feedback = "feedback_p1")

## --- 1. Cuerpo firmado ------------------------------------------------------------------------------------
cuerpo_firmado <- function(ruta) {
  raw <- readBin(ruta, "raw", file.size(ruta)); nl <- which(raw == as.raw(10L))
  lin <- readLines(ruta, encoding = "UTF-8", warn = FALSE)
  desde <- 1L
  if (length(lin) && lin[1L] == "---") {
    j <- which(lin == "---")[2L]
    stopifnot("YAML del Knit sin cierre" = !is.na(j))
    desde <- nl[j] + 1L
  }
  tf <- tempfile(fileext = ".Rmd"); writeBin(raw[desde:length(raw)], tf)
  stopifnot("el cuerpo del SCHOICE es el firmado (md5 f886dfaa…)" = unname(tools::md5sum(tf)) == MD5_CUERPO)
  readLines(tf, encoding = "UTF-8", warn = FALSE)
}
cuerpo <- cuerpo_firmado(S)
i0 <- grep("^```\\{r data_generation", cuerpo); stopifnot("un solo chunk data_generation" = length(i0) == 1L)
i1 <- i0 + which(cuerpo[(i0 + 1L):length(cuerpo)] == "```")[1L]
blk <- cuerpo[(i0 + 1L):(i1 - 1L)]

## --- 2. Renombres con el analizador ----------------------------------------------------------------------------
simbolos <- function(cod) {
  pd <- utils::getParseData(parse(text = cod, keep.source = TRUE, encoding = "UTF-8"))
  pd[pd$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_SUB"), c("line1", "col1", "col2", "text")]
}
s0 <- simbolos(blk)
for (k in names(REN)) {
  hit <- s0[s0$text == k, ]
  if (nrow(hit) != 1L) stop(sprintf("«%s» aparece %d veces como símbolo de código; se esperaba una", k, nrow(hit)))
  l <- blk[hit$line1]
  stopifnot("la línea del renombre es ASCII (columnas = bytes = caracteres)" = !grepl("[^ -~]", l),
            "el símbolo está donde dice el analizador" = substr(l, hit$col1, hit$col2) == k)
  blk[hit$line1] <- paste0(substr(l, 1L, hit$col1 - 1L), REN[[k]], substr(l, hit$col2 + 1L, nchar(l)))
}
s1 <- simbolos(blk)
stopifnot("no quedan opciones, sol ni feedback como símbolos de código" = !any(s1$text %in% names(REN)),
          "cada nombre nuevo aparece una vez" = all(table(factor(s1$text[s1$text %in% REN], levels = REN)) == 1L))

## --- 3. Empalme --------------------------------------------------------------------------------------------
lc <- readLines(C, encoding = "UTF-8", warn = FALSE)
i <- which(lc == INI); j <- which(lc == FIN)
stopifnot("marcadores únicos y en orden" = length(i) == 1L && length(j) == 1L && j > i)
actual <- if (j - i > 1L) lc[(i + 1L):(j - 1L)] else character(0)
if (identical(actual, blk)) {
  cat("El bloque firmado ya está empalmado.\n")
} else {
  writeLines(c(lc[seq_len(i)], blk, lc[j:length(lc)]), C, useBytes = FALSE)
  cat(sprintf("Empalmadas %d líneas (cuerpo firmado %s, chunk data_generation) con 3 renombres de código.\n",
              length(blk), substr(MD5_CUERPO, 1L, 8L)))
}
cat(sprintf("CLOZE: %d líneas, md5 %s\n", length(readLines(C, warn = FALSE)), unname(tools::md5sum(C))))
