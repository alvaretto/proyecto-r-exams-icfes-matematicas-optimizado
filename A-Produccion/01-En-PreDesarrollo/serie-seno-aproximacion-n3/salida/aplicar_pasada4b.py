## Pasada 4, ajuste: la guarda de «a partir del último término usado» depende solo de x y n y se comprueba antes de elegir
## distractores (así el mutante M6 muere por su sonda); guarda explícita de selección. Conserva el YAML del Knit.
import sys
p = sys.argv[1]; s = open(p, encoding="utf-8").read(); y0 = s[:s.index("```{r data_generation")]
g = '''  "Solution: a partir del último término usado, cada término es menor que el anterior en valor absoluto (FASE 2C r2 H1)" =
    all(diff(abs(vapply((n - 2L):(n + 5L), function(k) x^(2 * k + 1) / factorial(2 * k + 1), 0))) < 0),
'''
a = 'p <- list(x = x, n = n, D = D)\n'
b = a + '''## Depende solo de x y n: se comprueba antes de elegir distractores (pasada 4; mutantes M5 y M6).
stopifnot("Solution: a partir del último término usado, cada término es menor que el anterior en valor absoluto (FASE 2C r2 H1)" =
  all(diff(abs(vapply((n - 2L):(n + 5L), function(k) x^(2 * k + 1) / factorial(2 * k + 1), 0))) < 0))
'''
a2 = 'otros <- vals[names(vals) != "SS-C"]\n'
b2 = a2 + 'stopifnot("selección: al menos dos distractores del banco además de SS-C" = length(otros) >= 2L)\n'
for x, y in [(g, ""), (a, b), (a2, b2)]:
    if s.count(x) != 1: sys.exit("NO COINCIDE: " + x[:80])
    s = s.replace(x, y)
assert s.startswith(y0); open(p, "w", encoding="utf-8").write(s); print("guarda adelantada; YAML intacto")
