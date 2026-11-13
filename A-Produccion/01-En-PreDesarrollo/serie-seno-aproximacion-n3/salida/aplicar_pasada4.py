## Pasada 4 (instrucción del profesor del 2026-11-13): tope de |valor| de los distractores del banco, una igualdad por display,
## decimales en texto. Conserva intacto el YAML que antepuso el Knit de RStudio (regla #26: se quita solo con el visto bueno del
## profesor). Cada patrón debe aparecer exactamente una vez.
import sys
p = sys.argv[1]; s = open(p, encoding="utf-8").read(); yaml0 = s[:s.index("```{r data_generation")]
R = [
("""##   - Opciones sin simplificar sobre D = (2n - 1)!, el factorial del último término usado, como el impreso.""",
 """##   - Opciones sin simplificar sobre D = (2n - 1)!, el factorial del último término usado, como el impreso.
##   - Pasada 4 (profesor, 2026-11-13): ningún distractor del banco pasa de 4 en valor absoluto (TOPE_VALOR, en la precondición
##     de cada error; salida/enumeracion_tope_r4.txt: es el tope más estricto que deja al menos 3 errores elegibles además de
##     SS-C en las 8 ramas). Una igualdad por ecuación en display (360 y 320 px). Decimales en texto, con coma."""),
("""decm     <- function(v) sub(",", "{,}", dec(v), fixed = TRUE)       # decimal en modo matemático: signo menos real (H8)
decm_sig <- function(v) sub(",", "{,}", dec_sig(v), fixed = TRUE)""",
 """dec_txt     <- function(v) paste0(if (v < 0) "$-$" else "", dec(abs(v)))       # decimal en texto: en MathML la coma {,} sale separada
dec_sig_txt <- function(v) paste0(if (v < 0) "$-$" else "", dec_sig(abs(v)))   # el signo menos va en modo matemático (FASE 2C r1 H8)"""),
("""error_serie <- function(codigo, nombre, ficha, nt, den, dtex, sgn, pre, corta, larga, causa, feedback) {
  list(codigo = codigo, nombre = nombre, ficha = ficha, nt = nt, den = den, dtex = dtex, sgn = sgn,
       precondicion = pre, descripcion_corta = corta, descripcion_larga = larga, causa_raiz = causa,
       feedback = feedback,
       calcula = function(p) numerador(p, nt(p$n), den, function(k) sgn(k, p$n)))
}""",
 """TOPE_VALOR <- 4   # pasada 4: |valor| máximo de un distractor del banco (salida/enumeracion_tope_r4.txt)
error_serie <- function(codigo, nombre, ficha, nt, den, dtex, sgn, pre, corta, larga, causa, feedback) {
  calc <- function(p) numerador(p, nt(p$n), den, function(k) sgn(k, p$n))
  list(codigo = codigo, nombre = nombre, ficha = ficha, nt = nt, den = den, dtex = dtex, sgn = sgn,
       precondicion = function(p) isTRUE(pre(p)) && abs(calc(p) / p$D) <= TOPE_VALOR,
       descripcion_corta = corta, descripcion_larga = larga, causa_raiz = causa,
       feedback = feedback, calcula = calc)
}"""),
("""  "trampa SS-C: no coincide con la clave" = abs(nums_sel[["SS-C"]] - num_clave) > 0.5,""",
 """  "trampa SS-C: no coincide con la clave" = abs(nums_sel[["SS-C"]] - num_clave) > 0.5,
  "pasada 4: ningún distractor del banco pasa de 4 en valor absoluto" = all(abs(nums_sel[names(nums_sel) != "SS-C"] / D) <= 4),"""),
(""": $\\\\mathrm{sen}(", x, ") \\\\approx ", decm(sin(x)),
                  "$, y $", frs(e$calcula(p), D), " \\\\approx ", decm(e$calcula(p) / D), "$. ", e$descripcion_larga,""",
 """: $\\\\mathrm{sen}(", x, ") \\\\approx$ ", dec_txt(sin(x)),
                  ", y $", frs(e$calcula(p), D), " \\\\approx$ ", dec_txt(e$calcula(p) / D), ". ", e$descripcion_larga,"""),
("""{ partes <- unique(c(cc$l0, cc$l1, cc$l2, frs(cc$total, D)))   # FASE 2C r2 H5: dos displays, dentro del margen del PDF
           paste0(eq_display(paste(partes[seq_len(min(2L, length(partes)))], collapse = " = ")),
                  if (length(partes) > 2L) eq_display(paste0("= ", paste(partes[-(1:2)], collapse = " = "))) else "") }, "\\n",""",
 """{ partes <- unique(c(cc$l0, cc$l1, cc$l2, frs(cc$total, D)))   # pasada 4: una igualdad por display (PDF, 360 y 320 px)
           paste0(eq_display(partes[1L]), paste(vapply(partes[-1L], function(t) eq_display(paste0("= ", t)), ""), collapse = "")) }, "\\n","""),
("""que es un número impar. Al sustituir $x = ", x, "$:\\n\\n", sep = "")""",
 """que es un número impar. Al sustituir $x = ", x, "$, $\\\\mathrm{sen}(", x, ")$ es aproximadamente:\\n\\n", sep = "")"""),
("""cat(eq_display(paste0("\\\\mathrm{sen}(", x, ") \\\\approx ", sustitucion)), eq_display(paste0("= ", ck$l1)), "\\n", sep = "")""",
 """cat(eq_display(sustitucion), eq_display(paste0("= ", ck$l1)), "\\n", sep = "")"""),
("""cat(eq_display(paste0(ck$l2, " = ", frs(num_clave, D))), "\\n", sep = "")""",
 """cat(eq_display(ck$l2), eq_display(paste0("= ", frs(num_clave, D))), "\\n", sep = "")"""),
("""cat("Con una calculadora en radianes, $\\\\mathrm{sen}(", x, ") \\\\approx ", decm(sin(x)), "$, y la serie con los primeros ",
    PALABRA[n], " términos da $", frs(num_clave, D), " \\\\approx ", decm(valor_correcto), "$. No son iguales: ",
    "la serie truncada es una aproximación. La diferencia, cerca de $", decm_sig(dif), "$, es menor que el valor ",""",
 """cat("Con una calculadora en radianes, $\\\\mathrm{sen}(", x, ") \\\\approx$ ", dec_txt(sin(x)), ", y la serie con los primeros ",
    PALABRA[n], " términos da $", frs(num_clave, D), " \\\\approx$ ", dec_txt(valor_correcto), ". No son iguales: ",
    "la serie truncada es una aproximación. La diferencia, cerca de ", dec_sig_txt(dif), ", es menor que el valor ","""),
(""" \\\\approx ", decm_sig(cola), "$: a partir del ",""", """ \\\\approx$ ", dec_sig_txt(cola), ": a partir del ","""),
]
for a, b in R:
    c = s.count(a)
    if c != 1: sys.exit("NO COINCIDE (%d): %s" % (c, a[:100]))
    s = s.replace(a, b)
assert s.startswith(yaml0) and "decm" not in s, "YAML alterado o quedan usos de decm"
open(p, "w", encoding="utf-8").write(s); print("pasada 4 aplicada:", len(R), "cambios; YAML intacto")
