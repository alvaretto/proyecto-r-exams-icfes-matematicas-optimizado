r"""Tanda S1b relanzada (2026-11-03): ediciones del .Rmd sobre V1' (salida de generar_v1p2.py sobre 5cf5b12f).
Etapa A (V1' aplicada): comentario de cabecera y guarda «L-6: el signo que sigue a la unidad…» (endurecer).
Etapa B (final): A + S7, sin la asignación muerta de lado_gdis en la canónica (decisión del profesor del 2026-11-03,
«En esta S1b (Recomendada)»).
Uso: python3 -I editar_rmd_s1b2.py <V1p.Rmd> <A.Rmd> <B.Rmd>"""
import sys
src, dA, dB = sys.argv[1:4]
s = open(src, encoding='utf-8').read()

def una(s, viejo, nuevo):
    assert s.count(viejo) == 1, (s.count(viejo), viejo[:80])
    return s.replace(viejo, nuevo)

# A.1 Comentario de cabecera (tras el de la primera S1b)
ANCLA = "## y el espacio duro (6b de S1, N = 100: 132 de 1158 a 360 px y 259 de 1158 a 320 px). El PDF ya no lo partía.\n"
s = una(s, ANCLA, ANCLA +
  "## Tanda S1b relanzada (2026-11-03, decisión del profesor «V1' (Recomendada)»): el signo que sigue a la unidad entra\n"
  "## también en la fórmula, «$... = T\\\\ \\\\text{kilos.}$»: con el signo fuera, Chromium partía el renglón entre la\n"
  "## fórmula y el signo (6b de V1, N = 100: el punto solo en 34 semillas a 360 px y en 5 a 320 px).\n")

# A.2 Guarda nueva tras la de la unidad (endurecer: el feedback y el análisis de los distractores)
G_OLD = ('                       c(fb, vapply(c("gdis", "padv", "psin"), function(r)\n'
         '                         errores_conceptuales[[afs[[r]]$codigo]]$explica(params, afs[[r]]), "")), perl = TRUE)))\n')
s = una(s, G_OLD, G_OLD +
  '## L-6 (tanda S1b relanzada, V1\'): el signo que sigue a la unidad de una fórmula va dentro de ella, «\\\\text{kilos.}$».\n'
  'stopifnot("L-6: el signo que sigue a la unidad de una fórmula va dentro de la fórmula" =\n'
  '            !any(grepl("\\\\\\\\text\\\\{(kilos?|cursos?)\\\\}\\\\$[.,:;!?)\\\\]\\u00bb\\u201d\\u2026]",\n'
  '                       c(fb, vapply(c("gdis", "padv", "psin"), function(r)\n'
  '                         errores_conceptuales[[afs[[r]]$codigo]]$explica(params, afs[[r]]), "")), perl = TRUE)))\n')
assert ' ' not in s and '⁠' not in s
open(dA, 'w', encoding='utf-8').write(s)

# B. S7: fuera la asignación muerta de lado_gdis en la canónica
L_OLD = ('  d <- list(nG = 5, nP = 4, aG = 40, aP = 45)\n'
         '  ## lado_gdis no se usa en la canónica (cifras 5 < 20 (D) < 40), pero lo leen rasgos() de\n'
         '  ## verificar_espacio.R y rec() de cloze/verificar_espacio_cloze.R: se quita en la tanda del\n'
         '  ## re-empalme, junto con esa lectura (S7, ciclo 2026-11-02).\n'
         '  lado_gdis <- "encima"\n'
         '} else {\n')
s = una(s, L_OLD,
         '  d <- list(nG = 5, nP = 4, aG = 40, aP = 45)\n'
         '  ## S7 (tanda S1b relanzada, 2026-11-03): la canónica no usa lado_gdis; rasgos() de verificar_espacio.R calcula\n'
         '  ## su lado de las cifras (5 < 20 (D) < 40). rec() del CLOZE la deja de leer en el re-empalme.\n'
         '} else {\n')
assert ' ' not in s and '⁠' not in s
open(dB, 'w', encoding='utf-8').write(s)
print('A ->', dA, '· B ->', dB)
