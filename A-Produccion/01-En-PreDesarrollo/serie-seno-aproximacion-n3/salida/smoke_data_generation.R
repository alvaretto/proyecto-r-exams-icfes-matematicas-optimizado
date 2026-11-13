## Smoke: ejecuta el chunk data_generation en entornos nuevos y resume (no es el verificador del paso 9).
rmd <- list.files(".", pattern = "_schoice_v1\\.Rmd$")[1]
l <- readLines(rmd, encoding = "UTF-8")
i0 <- grep("^```\\{r data_generation", l); i1 <- which(l == "```")[which(l == "```") > i0][1]
code <- parse(text = l[(i0 + 1):(i1 - 1)], encoding = "UTF-8")
N <- as.integer(commandArgs(TRUE)[1]); if (is.na(N)) N <- 300L
res <- lapply(seq_len(N), function(s) {
  set.seed(s); e <- new.env(); r <- tryCatch({ eval(code, e); "ok" }, error = function(err) conditionMessage(err))
  if (r != "ok") return(data.frame(s = s, err = r, canon = NA, rama = NA, clave = NA, rango = NA, terna = NA))
  data.frame(s = s, err = "", canon = e$es_canonica, rama = paste0("x=", e$x, ",n=", e$n), clave = paste0(e$num_clave, "/", e$D),
             rango = rank(c(e$num_clave, e$nums_sel))[1], terna = paste(e$sel, collapse = "+"))
})
R <- do.call(rbind, res)
cat("errores:", sum(R$err != ""), "\n"); if (any(R$err != "")) print(head(R[R$err != "", c("s", "err")], 10))
R <- R[R$err == "", ]
cat("canónicas:", sum(R$canon), "/", nrow(R), "\n"); print(table(R$rama)); cat("claves distintas:", length(unique(R$clave)), "\n")
cat("rango de la clave:\n"); print(table(R$rango)); cat("ternas distintas:", length(unique(R$terna)), "\n")
