## Cambios pedidos por la FASE 2C r3 (APROBAR_CON_CAMBIOS): H3, H5, H4 y H2 (documentación). Cada patrón aparece una vez.
import sys
def aplicar(p, R):
    s = open(p, encoding="utf-8").read()
    for a, b in R:
        c = s.count(a)
        if c != 1: sys.exit("NO COINCIDE (%d) en %s: %s" % (c, p, a[:90]))
        s = s.replace(a, b)
    open(p, "w", encoding="utf-8").write(s); print(p, "->", len(R), "cambios")
aplicar("serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd", [
 ('descripcion_larga = paste0("Ese es el valor del seno, no el de la serie con sus primeros ", PALABRA[n], " términos, que es ",',
  'descripcion_larga = paste0("Esa fracción aproxima el valor del seno; no es el valor de la serie con sus primeros ", PALABRA[n], " términos, que es ",'),
 ("la otra es el valor del seno que da la calculadora", "la otra es la fracción más cercana al valor del seno que da la calculadora"),
 ("(n - 1L):(n + 5L)", "(n - 2L):(n + 5L)"),
 ("##   - Otros dos distractores de un banco de 10 errores, con el RANGO DE LA CLAVE balanceado (se sortea cuántas\n##     opciones quedan por debajo de la clave entre los repartos factibles) y una cascada que prefiere que al menos",
  "##   - Otros dos distractores de un banco de 10 errores: se sortea cuántas opciones quedan por debajo de la clave entre los\n##     repartos factibles (la clave nunca es la extrema del lado de la trampa) y una cascada que prefiere que al menos"),
 ("salida/p9_bateria_p7.txt (N = 100) y salida/enumeracion_diseno_r2.txt (exacta)",
  "salida/p9_bateria_p7.txt (N = 100) y la tabla exacta del H1 de salida/fase2c_r3_detractor.md"),
])
aplicar("ITEM_OFICIAL.md", [
 ("con el rango de la clave balanceado y una cascada",
  "con el número de opciones a cada lado de la clave sorteado entre los repartos factibles (la clave nunca es la extrema del lado de la trampa) y una cascada"),
 ("Equilibrar la divisibilidad. Con abs(x) ≥ 2 la clave siempre se simplifica, y «la irreducible» y «la que se simplifica» quedan por debajo de +8 pp",
  "Equilibrar la divisibilidad. Con abs(x) ≥ 2 la clave siempre se simplifica. Enumeración exacta (FASE 2C r3, H2), sobre las versiones en que la regla aplica: «la irreducible» −19,8 pp; «la que se simplifica» +8,3 pp; «numerador múltiplo de x» +8,3 pp; «no múltiplo de 5» +12,7 pp (diluidas: +6,2, +6,2 y +4,7). Pendiente de decisión del profesor"),
])
aplicar("salida/verificar_mutantes_textos.R", [
 ("de = '(n - 1L):(n + 5L)', a = '0:(n + 5L)'))",
  "de = '(n - 2L):(n + 5L)', a = '0:(n + 5L)'),\n  M6 = list(sonda = \"a partir del último término usado, cada término es menor que el anterior\",\n            de = 'n <- pick(3:4)', a = 'n <- pick(2:4)'))"),
])
aplicar("salida/verificar_bateria_p7.R", [
 ('  nueva_regla("div_termina_1o9", "divisibilidad", function(o) abs(o) %% 10 %in% c(1, 9))\n)',
  '''  ## Ronda 3 (§P7-C: la batería cambia, se vuelve a medir todo): reglas nuevas del detractor r3.
  nueva_regla("rel_par_mult_x", "divisibilidad", function(o, e) { d <- abs(outer(o, o, "-")); diag(d) <- Inf; ij <- which(d == min(d), arr.ind = TRUE)[1, ]; seq_along(o) %in% ij & abs(o) %% abs(e$x) == 0 }, usa_estimulo = TRUE),
  nueva_regla("est_abs_menor_x", "magnitud", function(o, e) abs(o / e$D) < abs(e$x), usa_estimulo = TRUE),
  nueva_regla("div_mult_x", "divisibilidad", function(o, e) abs(o) %% abs(e$x) == 0, usa_estimulo = TRUE),
  nueva_regla("sig_mayoritario", "signo", function(o) { s <- sign(o); tb <- table(s); s == as.numeric(names(tb)[which.max(tb)]) }),
  nueva_regla("div_termina_1o9", "divisibilidad", function(o) abs(o) %% 10 %in% c(1, 9))
)'''),
])
