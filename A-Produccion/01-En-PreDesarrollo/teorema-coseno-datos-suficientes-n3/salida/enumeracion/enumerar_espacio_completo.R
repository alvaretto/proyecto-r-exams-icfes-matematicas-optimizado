## Enumeracion EXHAUSTIVA del espacio de versiones (no muestreo).
## Regla #23: "cuando el espacio de versiones es finito y pequeno, se enumera
## COMPLETO". Aqui: 12 familias x 3 lados pedidos x 7 terceros distractores
## = 252 configuraciones distintas de opciones (antes de la mezcla).
LADOS <- c("x","y","z"); ANG <- c("X","Y","Z")
familias <- list(c("Q","R","S"),c("M","N","T"),c("P","U","V"),c("D","E","F"),
                 c("G","H","K"),c("R","S","T"),c("E","F","G"),c("K","M","N"),
                 c("N","P","Q"),c("U","V","W"),c("D","G","H"),c("T","W","X"))
n_conf <- 0; fallos <- character(0); claves <- character(0); moldes <- integer(0)
for (V in familias) for (pedido in tolower(V)) {
  lm <- tolower(V); x <- pedido; X <- toupper(x)
  o <- setdiff(lm, x); y <- o[1]; z <- o[2]; Y <- toupper(y); Z <- toupper(z)
  LT <- c(x,y,z); AT <- c(X,Y,Z)
  cierre <- function(a) if (length(a)==2L) union(a, setdiff(AT,a)) else a
  res <- function(l,a) all(setdiff(LT,x) %in% l) && (X %in% cierre(a))
  # pool completo, en el mismo orden que el .Rmd
  pool <- list(list(l=c(y,z),a=c(Y)), list(l=c(y,z),a=c(Z)),
               list(l=c(y),a=c(X,Y)), list(l=c(y),a=c(X,Z)), list(l=c(y),a=c(Y,Z)),
               list(l=c(z),a=c(X,Y)), list(l=c(z),a=c(X,Z)), list(l=c(z),a=c(Y,Z)),
               list(l=character(0),a=c(X,Y,Z)))
  clave <- list(l=c(y,z), a=c(X))
  for (tercero in 3:9) {
    n_conf <- n_conf + 1
    sel <- list(pool[[1]], pool[[2]], pool[[tercero]])
    conj <- c(list(clave), sel)
    # 1 sola clave
    nres <- sum(vapply(conj, function(c0) res(c0$l, c0$a), NA))
    if (nres != 1L) fallos <- c(fallos, sprintf("clave no unica (%d) en %s/%s/t%d", nres, paste(V,collapse=""), x, tercero))
    # 3 datos cada una
    nd <- vapply(conj, function(c0) length(c0$l)+length(c0$a), 0L)
    if (!all(nd==3L)) fallos <- c(fallos, sprintf("n_datos %s en %s/%s/t%d", paste(nd,collapse=","), paste(V,collapse=""), x, tercero))
    # el lado pedido no aparece
    if (any(vapply(conj, function(c0) x %in% c0$l, NA))) fallos <- c(fallos, "pedido en opcion")
    # opciones distintas
    txt <- vapply(conj, function(c0) paste0(paste(c0$l,collapse=","),"|",paste(c0$a,collapse=",")), "")
    if (length(unique(txt)) != 4L) fallos <- c(fallos, sprintf("duplicadas en %s/%s/t%d", paste(V,collapse=""), x, tercero))
    m <- vapply(conj, function(c0) paste0(length(c0$l),"L",length(c0$a),"A"), "")
    moldes <- c(moldes, sum(m==m[1]))
    claves <- c(claves, paste0("Lados ",y," y ",z," y angulo ",X))
  }
}
cat("ENUMERACION EXHAUSTIVA DEL ESPACIO DE VERSIONES\n")
cat("configuraciones examinadas:", n_conf, "(12 familias x 3 lados x 7 terceros)\n")
cat("textos de clave distintos  :", length(unique(claves)), "\n")
cat("opciones del molde de la clave: "); print(table(moldes))
cat("fallos:", length(fallos), "\n")
if (length(fallos)) print(head(unique(fallos), 10)) else
  cat("\nVEREDICTO: en las", n_conf, "configuraciones posibles la clave es UNICA,\n",
      "          las 4 opciones llevan 3 datos, ninguna contiene el lado pedido,\n",
      "          no hay duplicadas y siempre hay 3 opciones del molde de la clave.\n")
