## =====================================================================
## ENUMERACION EXHAUSTIVA DEL ESPACIO DE DATOS — las 4 ramas posibles
## (teorema x magnitud pedida) para el item "datos suficientes".
##
## Triangulo con vertices/angulos X,Y,Z y lados opuestos x,y,z.
## Convencion del .Rmd: cada lado minuscula es OPUESTO al angulo mayuscula
## homonimo. Los lados p y q se encuentran en el vertice del TERCER angulo.
##
## No se decide nada "a ojo": todo sale de un motor de deduccion con reglas
## explicitas + un contra-chequeo NUMERICO de la ambiguedad LLA.
## =====================================================================

LADOS   <- c("x", "y", "z")
ANGULOS <- c("X", "Y", "Z")
opuesto <- c(x = "X", y = "Y", z = "Z", X = "x", Y = "y", Z = "z")

## El angulo COMPRENDIDO entre los lados p y q es el opuesto al tercer lado.
comprendido <- function(lado1, lado2) {
  tercero <- setdiff(LADOS, c(lado1, lado2))
  opuesto[[tercero]]
}

## --- Regla LIBRE: suma de angulos internos (aritmetica, no es un teorema) ---
cierre_suma_angulos <- function(K) {
  ang <- intersect(K, ANGULOS)
  if (length(ang) == 2L) K <- union(K, setdiff(ANGULOS, ang))
  K
}

## --- ¿UNA aplicacion del teorema del COSENO entrega el objetivo? ----------
## Sustitucion directa en una instancia de la formula, con resultado UNICO.
##   lado t   : hacen falta los otros dos lados y el angulo comprendido (= T)
##   angulo T : hacen falta los TRES lados
un_coseno <- function(objetivo, K) {
  K <- cierre_suma_angulos(K)
  if (objetivo %in% LADOS) {
    otros <- setdiff(LADOS, objetivo)
    all(otros %in% K) && (comprendido(otros[1], otros[2]) %in% K)
  } else {
    all(LADOS %in% K)
  }
}

## --- ¿UNA aplicacion de la LEY DE SENOS entrega el objetivo? --------------
##   lado t   : hace falta una pareja (lado s, angulo S) y el angulo T.  UNICO.
##   angulo T : hace falta el lado t, una pareja (s,S) con s != t.       AMBIGUO (LLA)
un_seno <- function(objetivo, K) {
  K <- cierre_suma_angulos(K)
  parejas <- Filter(function(s) s %in% K && opuesto[[s]] %in% K, LADOS)
  if (objetivo %in% LADOS) {
    (opuesto[[objetivo]] %in% K) && length(parejas) >= 1L
  } else {
    lado_t <- opuesto[[objetivo]]
    (lado_t %in% K) && length(setdiff(parejas, lado_t)) >= 1L
  }
}

## --- ¿El conjunto DETERMINA el triangulo (tamano y forma), sin ambiguedad? -
## Clasificacion clasica por configuracion, no por conteo de datos.
configuracion <- function(K) {
  K <- cierre_suma_angulos(K)
  nl <- length(intersect(K, LADOS)); na <- length(intersect(K, ANGULOS))
  if (nl == 3L) return("LLL")
  if (nl == 0L) return("AAA")          # solo forma, sin tamano
  if (nl == 2L) {
    p <- intersect(K, LADOS)
    if (comprendido(p[1], p[2]) %in% K) return("LAL") else return("LLA")
  }
  if (nl == 1L && na >= 2L) return("ALA/AAL")
  "INSUFICIENTE"
}
determina_unico <- function(K) configuracion(K) %in% c("LLL", "LAL", "ALA/AAL")

## =====================================================================
## CONTRA-CHEQUEO NUMERICO de la ambiguedad LLA (control positivo)
## =====================================================================
set.seed(20260913)
chequeo_lla <- local({
  ## r=5, s=7, R=40 -> dos triangulos (citado en la invariante local I-7)
  r <- 5; s <- 7; Rg <- 40 * pi / 180
  ## ley de senos: sin S = s*sin R / r  -> dos soluciones (S y 180-S)
  sinS <- s * sin(Rg) / r
  soluciones <- c()
  for (S in c(asin(sinS), pi - asin(sinS))) {
    Q <- pi - Rg - S
    if (Q > 1e-9 && S > 1e-9) soluciones <- c(soluciones, r * sin(Q) / sin(Rg))
  }
  sort(round(soluciones, 3))
})
cat("CONTROL POSITIVO (LLA r=5,s=7,R=40 grados): q admite",
    length(chequeo_lla), "valores:", paste(chequeo_lla, collapse = " / "), "\n")
stopifnot(length(chequeo_lla) == 2L)   # si fuera 1, el motor estaria mintiendo
cat("  -> LLA queda confirmado AMBIGUO por via numerica, no por autoridad.\n\n")

## =====================================================================
## ENUMERACION POR RAMA
## =====================================================================
molde <- function(K) {
  nl <- length(intersect(K, LADOS)); na <- length(intersect(K, ANGULOS))
  sprintf("%dL+%dA", nl, na)
}

enumerar <- function(objetivo, teorema) {
  ## El objetivo NUNCA puede figurar entre los datos ofrecidos (seria absurdo).
  universo <- setdiff(c(LADOS, ANGULOS), objetivo)
  combos <- combn(universo, 3L, simplify = FALSE)
  un_teorema <- if (teorema == "coseno") un_coseno else un_seno
  el_otro    <- if (teorema == "coseno") un_seno   else un_coseno
  data.frame(
    conjunto   = vapply(combos, function(K) paste(sort(K), collapse = ","), ""),
    molde      = vapply(combos, molde, ""),
    config     = vapply(combos, configuracion, ""),
    una_del_T  = vapply(combos, function(K) un_teorema(objetivo, K), NA),
    una_del_otro = vapply(combos, function(K) el_otro(objetivo, K), NA),
    determina  = vapply(combos, determina_unico, NA),
    stringsAsFactors = FALSE
  )
}

ramas <- list(
  list(id = "A. COSENO pide LADO   (= el item oficial)", obj = "x", teo = "coseno"),
  list(id = "B. COSENO pide ANGULO (rama eliminada)",    obj = "X", teo = "coseno"),
  list(id = "C. SENO   pide LADO   (rama propuesta)",    obj = "x", teo = "seno"),
  list(id = "D. SENO   pide ANGULO",                     obj = "X", teo = "seno")
)

resumen <- list()
for (r in ramas) {
  cat("=====================================================================\n")
  cat(r$id, "  [objetivo:", r$obj, "| teorema:", r$teo, "]\n")
  cat("=====================================================================\n")
  tb <- enumerar(r$obj, r$teo)
  claves <- tb$conjunto[tb$una_del_T]
  tb$rol <- ifelse(tb$una_del_T, "*** CLAVE ***", "distractor legitimo")
  print(tb[order(!tb$una_del_T, tb$molde), c("conjunto","molde","config","una_del_T","una_del_otro","determina","rol")],
        row.names = FALSE)

  molde_clave <- unique(tb$molde[tb$una_del_T])
  mismo_molde_no_clave <- tb$conjunto[tb$molde %in% molde_clave & !tb$una_del_T]

  cat("\n  RESULTADO:\n")
  cat("   claves (conjuntos que el teorema resuelve en UNA aplicacion):",
      length(claves), if (length(claves)) paste0(" -> ", paste(claves, collapse = " | ")) else "", "\n")
  cat("   molde(s) de la clave:", paste(molde_clave, collapse = " / "), "\n")
  cat("   distractores legitimos totales:", sum(!tb$una_del_T), "\n")
  cat("   distractores DEL MISMO MOLDE que la clave:", length(mismo_molde_no_clave),
      if (length(mismo_molde_no_clave)) paste0(" -> ", paste(mismo_molde_no_clave, collapse = " | ")) else " -> NINGUNO", "\n")

  viable_unica   <- length(claves) == 1L
  viable_pool    <- sum(!tb$una_del_T) >= 3L
  viable_molde   <- length(mismo_molde_no_clave) >= 1L
  cat("   [clave unica]            ", if (viable_unica) "OK" else "FALLA", "\n")
  cat("   [>=3 distractores]       ", if (viable_pool) "OK" else "FALLA", "\n")
  cat("   [>=1 distractor mismo molde (P7-E)]", if (viable_molde) "OK" else "FALLA <-- canal de formato 100% determinista", "\n\n")

  resumen[[r$id]] <- c(claves = length(claves), distractores = sum(!tb$una_del_T),
                       mismo_molde = length(mismo_molde_no_clave),
                       viable = as.integer(viable_unica && viable_pool && viable_molde))
}

cat("=====================================================================\n")
cat("RESUMEN DE LAS 4 RAMAS\n")
cat("=====================================================================\n")
print(do.call(rbind, resumen))
