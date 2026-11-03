r"""Tanda S1b (2026-11-02): V1 en el .Rmd. La unidad que cierra una fórmula entra en ella:
   "$", ED, "kilos   ->   "\\ \\text{kilos}$      (20 sitios: Solution y feedback)
más la guarda de L-6 «fórmula y unidad» en data_generation y su comentario. Sin U+00A0 ni U+2060 literales."""
import re, sys, hashlib
p = sys.argv[1]; s = open(p, encoding='utf-8').read()
assert hashlib.md5(s.encode()).hexdigest() == '4cffbe8b1924b749c36d2b92369e4a83'
CIERRE = re.compile(r'"\$"(\s*),(\s*)ED(\s*),(\s*)"(kilos|cursos?)')
assert len(CIERRE.findall(s)) == 20
s = CIERRE.sub(lambda m: '"\\\\ \\\\text{' + m.group(5) + '}$', s)
# comentario junto a ED
old = 'ED <- "\\u00a0"\n'
assert s.count(old) == 1
s = s.replace(old, old +
 "## Tanda S1b (2026-11-02, decisión del profesor «Arreglarlo ahora»): cuando la cifra cierra una fórmula, la unidad va\n"
 "## DENTRO de ella, «$a \\\\times b = T\\\\ \\\\text{kilos}$», y no fuera con ED: Chromium parte el renglón entre la fórmula\n"
 "## y el espacio duro (6b de S1, N = 100: 132 de 1158 a 360 px y 259 de 1158 a 320 px). El PDF ya no lo partía.\n", 1)
# guarda en data_generation, tras el feedback (la Solution la vigila S1-Sol en verificar_espacio.R)
old = 'feedback <- unname(fb[rol_opcion])\n'
assert s.count(old) == 1
s = s.replace(old, old +
 '## L-6 (tanda S1b): ninguna fórmula del feedback ni del análisis de los distractores cierra en «$» seguido de la\n'
 '## unidad; la unidad va dentro de la fórmula. explica() es pura (regla #1), así que llamarla aquí no mueve el RNG.\n'
 'stopifnot("L-6: la unidad que cierra una fórmula va dentro de la fórmula" =\n'
 '            !any(grepl(paste0("\\\\$[ ", ED, "]+(kilos?|cursos?)\\\\b"),\n'
 '                       c(fb, vapply(c("gdis", "padv", "psin"), function(r)\n'
 '                         errores_conceptuales[[afs[[r]]$codigo]]$explica(params, afs[[r]]), "")), perl = TRUE)))\n', 1)
assert '\u00a0' not in s and '\u2060' not in s
open(p, 'w', encoding='utf-8').write(s); print('ok')
