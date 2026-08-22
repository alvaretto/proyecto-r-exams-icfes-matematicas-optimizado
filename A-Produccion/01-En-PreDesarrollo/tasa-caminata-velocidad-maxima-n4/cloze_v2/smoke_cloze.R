rmd <- "tasa_caminata_velocidad_maxima_numerico_variacional_argumentacion_n4_cloze_v2.Rmd"
ln  <- readLines(rmd, warn = FALSE)
i0  <- grep("^```\\{r data_generation", ln)[1]
i1  <- i0 + which(grepl("^```\\s*$", ln[(i0+1):length(ln)]))[1]
expr <- parse(text = paste(ln[(i0+1):(i1-1)], collapse="\n"))
# sintaxis de TODOS los chunks
for (ch in c("enunciado","parte2","parte3","parte4","parte5","answerlist_q","solucion","answerlist_s")) {
  j0 <- grep(sprintf("^```\\{r %s", ch), ln)[1]
  if (is.na(j0)) { cat("FALTA chunk", ch, "\n"); next }
  j1 <- j0 + which(grepl("^```\\s*$", ln[(j0+1):length(ln)]))[1]
  ok <- tryCatch({parse(text=paste(ln[(j0+1):(j1-1)], collapse="\n")); TRUE},
                 error=function(e){cat("SINTAXIS ROTA",ch,":",conditionMessage(e),"\n");FALSE})
}
N <- 100L; fails <- character(0); canon <- 0L
for (s in seq_len(N)) {
  set.seed(s*7919L)
  e <- new.env(parent=globalenv())
  r <- tryCatch({eval(expr, envir=e); NULL}, error=function(x) conditionMessage(x))
  if (!is.null(r)) { fails <- c(fails, sprintf("s%d: %s", s, r)); next }
  if (isTRUE(e$is_canonical)) canon <- canon + 1L
  # invariantes CLOZE
  if (e$vel*e$tasa != 60L) fails <- c(fails, sprintf("s%d vel", s))
  if (e$cota != e$vel*e$H) fails <- c(fails, sprintf("s%d cota", s))
  if (!identical(e$excede, e$reporte > e$cota)) fails <- c(fails, sprintf("s%d excede", s))
  if (e$resp_p2 != e$pr$vel) fails <- c(fails, sprintf("s%d p2", s))
  if (e$resp_p3 != e$pr$cota) fails <- c(fails, sprintf("s%d p3", s))
  if (length(intersect(c(e$pr$tasa,e$pr$H,e$pr$vel,e$pr$cota,e$pr$R), c(e$tasa,e$H,e$vel,e$cota,e$reporte))) > 0L) fails <- c(fails, sprintf("s%d COLISION practica/oficial", s))
  if (sum(e$sol_p1)!=1L||sum(e$sol_p4)!=1L||sum(e$sol_p5)!=1L) fails <- c(fails, sprintf("s%d sol", s))
  if (sum(e$sol_p6) < 1L || sum(e$sol_p6) > 5L) fails <- c(fails, sprintf("s%d p6", s))
  if (length(strsplit(e$exsolution_str,"|",fixed=TRUE)[[1]]) != 6L) fails <- c(fails, sprintf("s%d exsol", s))
  # marca alineada con la verdad (Familia 4)
  if (!identical(e$opciones_p5[e$sol_p5==1L], e$texto_clave)) fails <- c(fails, sprintf("s%d marca p5", s))
  if (!identical(e$opciones_p4[e$sol_p4==1L], e$texto_p4_clave)) fails <- c(fails, sprintf("s%d marca p4", s))
}
cat("=== SMOKE N=100 ===\nfallos:", length(fails), "| canonicas:", canon, "\n")
if (length(fails)) print(head(unique(fails), 15))
