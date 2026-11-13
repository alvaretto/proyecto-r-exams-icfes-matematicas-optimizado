## Pasada de corrección 1 (FASE 2C r1: H1, H2, H4, H5, H6, H8; coherencias H1-H2). Cada patrón debe aparecer una vez.
import sys
p = sys.argv[1]; s = open(p, encoding="utf-8").read()
R = [
("""##   - Residuo de diagnosticidad declarado (no se persigue, regla #22 §P7-D/F): con x = 1 o x = -1 el""",
 """##   - Residuo de diagnosticidad PENDIENTE DE FIRMA del profesor, regla por regla (§P7-E; FASE 2C r1 H3): con x = 1 o x = -1 el"""),
("""##     impresa tiene los dos canales al 100 %. Cifras en salida/enumeracion_diseno_final.txt.""",
 """##     impresa tiene los dos canales al 100 %. Cifras sobre versiones reales: salida/p9_bateria_p7.txt."""),
("""dec_sig <- function(v) sub(".", ",", formatC(v, format = "fg", digits = 2), fixed = TRUE)""",
 """dec_sig <- function(v) sub(".", ",", formatC(v, format = "fg", digits = 3), fixed = TRUE)   # 3 cifras (FASE 2C r1 H4)
decm     <- function(v) sub(",", "{,}", dec(v), fixed = TRUE)       # decimal en modo matemático: signo menos real (H8)
decm_sig <- function(v) sub(",", "{,}", dec_sig(v), fixed = TRUE)"""),
("""calculo <- function(p, nt, den, dtex, sgn) {   # list(l1 = suma simbólica, l2 = sobre D, total = numerador)
  k <- seq_len(nt) - 1L
  coef <- sgn(k) * p$x^(2 * k + 1)
  nums <- coef * p$D / den(k)
  list(l1 = suma_tex(coef, dtex(k)), l2 = nums_tex(nums, p$D), total = sum(nums))
}""",
"""calculo <- function(p, nt, den, dtex, sgn) {   # l0 = sustitución con los signos de la fórmula; l1 = ya sustituida; l2 = sobre D
  k <- seq_len(nt) - 1L
  s <- sgn(k)
  coef <- s * p$x^(2 * k + 1)
  nums <- coef * p$D / den(k)
  xs <- if (p$x < 0) "(-1)" else "1"
  dt <- dtex(k)
  l0 <- ""
  for (i in seq_along(k)) {   # FASE 2C r1 H2: con x = -1 el texto habla de los signos de la fórmula, no de los ya sustituidos
    t <- if (k[i] == 0) xs else fr(paste0(xs, "^{", 2 * k[i] + 1, "}"), dt[i])
    l0 <- if (i == 1L) paste0(if (s[i] < 0) "-" else "", t) else paste0(l0, if (s[i] < 0) " - " else " + ", t)
  }
  list(l0 = l0, l1 = suma_tex(coef, dt), l2 = nums_tex(nums, p$D), total = sum(nums))
}"""),
("""ult_num  <- (-1)^ult * x^(2 * ult + 1)                # numerador del último término sobre D (|x| = 1)""",
 """ult_num  <- (-1)^ult * x^(2 * ult + 1)                # numerador del último término sobre D (|x| = 1), con su signo
ult_tex_signo <- paste0(if (ult %% 2L == 1L) "-" else "", ult_tex)   # el término tal como aparece en la serie
stopifnot("Solution SS-F: el término mostrado vale lo que dice el texto (FASE 2C r1 H1)" =
  abs((if (startsWith(ult_tex_signo, "-")) -1 else 1) * x^(2 * ult + 1) / factorial(2 * ult + 1) - ult_num / D) < 1e-12)"""),
(""""antes y deja por fuera $", ult_tex, "$, que con $x = ", x, "$ vale $", frs(ult_num, D),""",
 """"antes y deja por fuera el término $", ult_tex_signo, "$, que con $x = ", x, "$ vale $", frs(ult_num, D),"""),
("""    "calcula el factorial sin incluir el propio número.",""",
 """    "omite el factor 3 al calcular 3!: es un desliz de cálculo en un solo término.","""),
("""  cc <- calculo(p, e$nt(n), e$den, e$dtex, function(k) e$sgn(k, n))
  stopifnot(abs(cc$total - e$calcula(p)) < 1e-9)""",
 """  cc <- calculo(p, e$nt(n), e$den, e$dtex, function(k) e$sgn(k, n))
  ops <- trimws(regmatches(cc$l0, gregexpr(" [+-] ", cc$l0))[[1L]])
  dice <- list("SS-A" = function(o) all(o == "-"), "SS-B" = function(o) all(o == "+"),
               "SS-D" = function(o) all(o == "+"), "SS-I" = function(o) o[1L] == "+")
  stopifnot("Solution: el cálculo mostrado da el valor de la opción" = abs(cc$total - e$calcula(p)) < 1e-9,
            "coherencia texto-cálculo: los signos mostrados son los que el texto describe (FASE 2C r1 H2)" =
              !(cd %in% names(dice)) || isTRUE(dice[[cd]](ops)))"""),
("""eq_display(paste0(cc$l1, " = ", cc$l2, " = ", frs(cc$total, D)))""",
 """eq_display(paste(unique(c(cc$l0, cc$l1, cc$l2, frs(cc$total, D))), collapse = " = "))"""),
("""términos de la serie tienen los signos alternados (+, -, +, ...) y,""",
 """términos de la serie tienen los signos alternados ($+$, $-$, $+$, $\\\\ldots$) y,"""),
("""cat("- Escribe el signo de cada término: +, -, +, ...\\n")""", """cat("- Escribe el signo de cada término: $+$, $-$, $+$, $\\\\ldots$\\n")"""),
("""cat("Con una calculadora en radianes, $\\\\mathrm{sen}(", x, ")$ es aproximadamente ", dec(sin(x)), ", y $",
    frs(num_clave, D), "$ es aproximadamente ", dec(valor_correcto), ". La diferencia, cerca de ", dec_sig(dif),
    ", es menor que el primer término que se dejó por fuera, $", fr(1, paste0(2 * n + 1, "!")), "$: en una serie ",
    "que alterna los signos, el error de la aproximación nunca supera ese término.\\n\\n", sep = "")""",
"""cat("Con una calculadora en radianes, $\\\\mathrm{sen}(", x, ") \\\\approx ", decm(sin(x)), "$, y $",
    frs(num_clave, D), " \\\\approx ", decm(valor_correcto), "$. La diferencia, cerca de $", decm_sig(dif),
    "$, es menor que el primer término que se dejó por fuera, $", fr(1, paste0(2 * n + 1, "!")), " = ",
    fr(1, format(factorial(2 * n + 1), scientific = FALSE)), " \\\\approx ", decm_sig(cola), "$: en una serie que ",
    "alterna los signos, el error de la aproximación nunca supera ese término.\\n\\n", sep = "")"""),
("""dif <- abs(valor_correcto - sin(x))""",
 """dif <- abs(valor_correcto - sin(x))
stopifnot("Comprobación: la diferencia redondeada que se imprime es menor que la cota impresa (FASE 2C r1 H4)" =
  signif(dif, 3) < signif(cola, 3))"""),
]
for a, b in R:
    c = s.count(a)
    if c != 1: sys.exit("NO COINCIDE (%d): %s" % (c, a[:90]))
    s = s.replace(a, b)
open(p, "w", encoding="utf-8").write(s); print("correcciones aplicadas:", len(R))
