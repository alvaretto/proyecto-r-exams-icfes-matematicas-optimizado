## =====================================================================
## CONTRA-CHEQUEO NUMERICO del hallazgo decisivo:
## en la rama SENO-pide-LADO, ¿de verdad los SEIS conjuntos "1 lado + 2
## angulos" entregan x con UNA aplicacion de la ley de senos?
## Se comprueba reconstruyendo x desde cada conjunto en 5.000 triangulos
## aleatorios y comparandolo con el x verdadero.
## =====================================================================
set.seed(20260913)
d2r <- function(g) g * pi / 180

N <- 5000
fallos <- setNames(integer(6), c("y,X,Y", "y,X,Z", "y,Y,Z", "z,X,Y", "z,X,Z", "z,Y,Z"))
maxerr <- setNames(numeric(6), names(fallos))

for (i in seq_len(N)) {
  ## triangulo aleatorio no degenerado
  X <- runif(1, 20, 120); Y <- runif(1, 20, 150 - X); Z <- 180 - X - Y
  if (Z < 15) next
  escala <- runif(1, 1, 50)
  x <- escala * sin(d2r(X)); y <- escala * sin(d2r(Y)); z <- escala * sin(d2r(Z))

  ## Cada reconstruccion usa UNA sola vez la ley de senos.
  ## La suma de angulos internos (180) es aritmetica libre, no un teorema.
  rec <- c(
    "y,X,Y" = y * sin(d2r(X)) / sin(d2r(Y)),                       # directo
    "y,X,Z" = y * sin(d2r(X)) / sin(d2r(180 - X - Z)),             # Y por suma
    "y,Y,Z" = y * sin(d2r(180 - Y - Z)) / sin(d2r(Y)),             # X por suma
    "z,X,Y" = z * sin(d2r(X)) / sin(d2r(180 - X - Y)),             # Z por suma
    "z,X,Z" = z * sin(d2r(X)) / sin(d2r(Z)),                       # directo
    "z,Y,Z" = z * sin(d2r(180 - Y - Z)) / sin(d2r(Z))              # X por suma
  )
  err <- abs(rec - x) / x
  maxerr <- pmax(maxerr, err)
  fallos <- fallos + as.integer(err > 1e-9)
}

cat("RECONSTRUCCION DE x CON UNA SOLA APLICACION DE LA LEY DE SENOS\n")
cat("triangulos probados:", N, "\n\n")
for (nm in names(fallos))
  cat(sprintf("  conjunto {%-7s}  fallos: %5d   error relativo maximo: %.2e   -> %s\n",
              nm, fallos[[nm]], maxerr[[nm]],
              if (fallos[[nm]] == 0L) "RESUELVE (es CLAVE)" else "no resuelve"))

cat("\nVEREDICTO:", sum(fallos == 0L), "de 6 conjuntos '1 lado + 2 angulos' entregan x.\n")

## CONTROL NEGATIVO: una formula deliberadamente equivocada DEBE fallar,
## para probar que el chequeo es capaz de detectar un fallo.
set.seed(1); X <- 50; Y <- 60; Z <- 70; esc <- 10
x <- esc*sin(d2r(X)); y <- esc*sin(d2r(Y))
malo <- y * sin(d2r(Y)) / sin(d2r(X))   # razon invertida a proposito
cat("\nCONTROL NEGATIVO (razon invertida): error relativo",
    sprintf("%.3f", abs(malo - x)/x), "-> el chequeo SI detecta formulas falsas\n")
stopifnot(abs(malo - x)/x > 1e-3)
