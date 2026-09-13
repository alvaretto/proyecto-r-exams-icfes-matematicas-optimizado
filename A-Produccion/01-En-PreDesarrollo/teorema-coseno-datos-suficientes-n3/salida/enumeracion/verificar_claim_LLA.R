## ¿Es CIERTO lo que la Solution le dice al estudiante sobre GEO-COS-01/02?
## Texto visible: "queda una ecuacion de segundo grado en el lado pedido:
##                 los datos admiten HASTA DOS triangulos distintos"
## Se comprueba resolviendo x^2 - 2*z*cos(Y)*x + (z^2 - y^2) = 0  (ley del coseno
## sobre el lado y, con angulo Y adyacente) y contando raices positivas validas.
set.seed(7)
d2r <- function(g) g*pi/180
cuenta <- function(y, z, Yg) {
  a <- 1; b <- -2*z*cos(d2r(Yg)); c0 <- z^2 - y^2
  disc <- b^2 - 4*a*c0
  if (disc < 0) return(0L)
  r <- c((-b - sqrt(disc))/2, (-b + sqrt(disc))/2)
  r <- r[r > 1e-9]
  # desigualdad triangular
  r <- r[abs(y - z) < r & r < y + z]
  length(unique(round(r, 9)))
}
tab <- table(replicate(20000, {
  z <- runif(1, 1, 20); Yg <- runif(1, 10, 150); y <- runif(1, 0.5, 30)
  cuenta(y, z, Yg)
}))
cat("Numero de triangulos compatibles con (dos lados + angulo ADYACENTE):\n")
print(tab)
cat("\nmaximo observado:", max(as.integer(names(tab))), "\n")
cat("VEREDICTO del claim 'hasta dos triangulos': ",
    if (max(as.integer(names(tab))) == 2L && "0" %in% names(tab))
      "CIERTO -- el maximo es 2 y hay casos con 0; 'hasta dos' es la cota correcta"
    else "REVISAR", "\n\n")
## Y el contraste: la CLAVE (dos lados + angulo COMPRENDIDO) siempre da 1
uno <- replicate(20000, {
  y <- runif(1,1,20); z <- runif(1,1,20); Xg <- runif(1,5,175)
  x <- sqrt(y^2 + z^2 - 2*y*z*cos(d2r(Xg)))
  is.finite(x) && x > 0
})
cat("La clave (angulo COMPRENDIDO) entrega un valor unico y positivo en",
    sum(uno), "/ 20000 casos\n")
