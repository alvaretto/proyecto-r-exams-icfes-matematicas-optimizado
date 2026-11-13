## Pasada de corrección 3 (FASE 2C r2: H1 corrección; H5-H8). Cada patrón debe aparecer exactamente una vez.
import sys
p = sys.argv[1]; s = open(p, encoding="utf-8").read()
R = [
('''"$: a partir del ",
    "segundo término, cada término es menor que el anterior en valor absoluto y los signos se alternan, así que el error ",
    "nunca supera ese término.\\n\\n", sep = "")''',
 '''"$: a partir del ",
    "último término usado, cada término es menor que el anterior en valor absoluto y los signos se alternan, así que el ",
    "error nunca supera el primero de los que se dejaron por fuera.\\n\\n", sep = "")'''),
('''  "Solution: desde el segundo término, cada término es menor que el anterior en valor absoluto" =
    all(diff(abs(terminos[-1])) < 0),''',
 '''  "Solution: a partir del último término usado, cada término es menor que el anterior en valor absoluto (FASE 2C r2 H1)" =
    all(diff(abs(vapply((n - 1L):(n + 5L), function(k) x^(2 * k + 1) / factorial(2 * k + 1), 0))) < 0),'''),
('''         eq_display(paste(unique(c(cc$l0, cc$l1, cc$l2, frs(cc$total, D))), collapse = " = ")), "\\n",''',
 '''         { partes <- unique(c(cc$l0, cc$l1, cc$l2, frs(cc$total, D)))   # FASE 2C r2 H5: dos displays, dentro del margen del PDF
           paste0(eq_display(paste(partes[seq_len(min(2L, length(partes)))], collapse = " = ")),
                  if (length(partes) > 2L) eq_display(paste0("= ", paste(partes[-(1:2)], collapse = " = "))) else "") }, "\\n",'''),
('''cat(eq_display(paste0("\\\\mathrm{sen}(", x, ") \\\\approx ", sustitucion, " = ", ck$l1)), "\\n", sep = "")''',
 '''cat(eq_display(paste0("\\\\mathrm{sen}(", x, ") \\\\approx ", sustitucion)), eq_display(paste0("= ", ck$l1)), "\\n", sep = "")'''),
('''##   - Residuo de diagnosticidad: canales por encima de +8 pp declarados al profesor regla por regla (§P7-E), con
##     las cifras de salida/p9_bateria_p7.txt. Varios son estructurales (la trampa queda siempre junto a la clave).''',
 '''##   - Residuo de diagnosticidad PENDIENTE DE FIRMA del profesor, regla por regla (§P7-E): cifras de la batería ampliada en
##     salida/p9_bateria_p7.txt (N = 100) y salida/enumeracion_diseno_r2.txt (exacta). Varios canales son estructurales de
##     D1: la trampa queda siempre junto a la clave. Sin alcance firmado cableado todavía (FASE 2C r2 H2).'''),
('''es_canonica <- FALSE          # la instancia impresa quedó fuera del espacio (decisión del profesor del 2026-11-13)''',
 '''es_canonica <- FALSE          # instancia impresa fuera (profesor, 2026-11-13); lo leen salida/smoke_data_generation.R y verificar_bateria_p7.R'''),
('''## --- Banco de errores conceptuales (regla #1: 11 entradas, cada una con precondición y calcula puros) ---''',
 '''## --- Banco de errores conceptuales (regla #1: 11 entradas, cada una con precondición y calcula puros) ---
## `ficha = TRUE` solo documenta cuáles son los tres distractores del impreso (SS-A, SS-B, SS-D); ningún código lo lee.'''),
('''"$ hayan hecho un cálculo que ", e$descripcion_corta, ": $\\\\mathrm{sen}(", x, ") \\\\approx ", decm(sin(x)),''',
 '''"$ hayan ", e$descripcion_corta, ": $\\\\mathrm{sen}(", x, ") \\\\approx ", decm(sin(x)),'''),
('''descripcion_corta = paste0("calcula $\\\\mathrm{sen}(", x, ")$ con la calculadora, en radianes, y busca la fracción con ",''',
 '''descripcion_corta = paste0("calculado $\\\\mathrm{sen}(", x, ")$ con la calculadora, en radianes, y buscado la fracción con ",'''),
('''    "calcula $3!$ como $2 \\\\times 1 = 2$",''', '''    "toma $3!$ como $2 \\\\times 1 = 2$",'''),
('''paste0("calcula $5!$ como $1 + 2 + 3 + 4 + 5 = 15$"''', '''paste0("toma $5!$ como $1 + 2 + 3 + 4 + 5 = 15$"'''),
('''feedback = paste0("Incorrecta. Es el valor del seno que da la calculadora, escrito con denominador ", D,''',
 '''feedback = paste0("Incorrecta. Es la fracción con denominador ", D, " más cercana al valor del seno que da la calculadora",'''),
]
for a, b in R:
    c = s.count(a)
    if c != 1: sys.exit("NO COINCIDE (%d): %s" % (c, a[:100]))
    s = s.replace(a, b)
open(p, "w", encoding="utf-8").write(s); print("correcciones r3 aplicadas:", len(R))
