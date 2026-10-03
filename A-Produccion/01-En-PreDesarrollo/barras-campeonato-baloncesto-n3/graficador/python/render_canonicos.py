# -*- coding: utf-8 -*-
import os, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from generador_python import barras_simples, barras_opcion

D = os.path.dirname(os.path.abspath(__file__))
CAT = ["Partidos ganados", "Partidos perdidos"]
GRU = ["Grado sexto", "Grado séptimo"]
COL = ["#F2501E", "#1EAAD8"]
T = "Informe de partidos del campeonato"
Y = "Número de partidos"
p = lambda n: os.path.join(D, "python_" + n + ".png")

barras_simples([8, 4], CAT, ["#FF1A8C", "#00B33C"],
               "Gráfica de información del\ncampeonato para grado séptimo", Y, p("barras_septimo"))
# matriz [grupo x categoria]; grupo1 = sexto, grupo2 = septimo
barras_opcion([[8, 4], [5, 7]], GRU, CAT, COL, "apilada", T, Y, p("opcion_A"))      # A: sept 5+7, sexto 8+4
barras_opcion([[4, 4.5], [8, 5]], GRU, CAT, COL, "apilada", T, Y, p("opcion_B"), ymax=13)    # B
barras_opcion([[5, 7], [8, 4]], GRU, CAT, COL, "agrupada", T, Y, p("opcion_C"))
barras_opcion([[5, 8], [7, 4]], GRU, CAT, COL, "agrupada", T, Y, p("opcion_D"))
barras_opcion([[3, 11], [9, 6]], GRU, CAT, COL, "agrupada", T, Y, p("param_agrupada"))
barras_opcion([[3, 11], [9, 6]], GRU, CAT, COL, "apilada", T, Y, p("param_apilada"))
print("ok")
