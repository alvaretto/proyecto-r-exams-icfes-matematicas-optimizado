"""Pasada 2 de §P7: programa lineal que elige la tabla de pesos (terna, forma) midiendo el vector completo.

Entrada: salida/pesos_celdas.csv (tasa de cada regla en cada celda, de pesos_celdas.R).
Minimiza la tasa máxima entre todas las reglas del vector (batería congelada sin las de posición, que dependen solo
de la mezcla, y sin la del estímulo K/V, que el profesor decidió aceptar; más centralidad, medoide y «usa los tres
datos» del detractor r1), con estas restricciones:
  - centralidad y medoide >= PISO (si la clave nunca es la central, «descartar la central» es otro atajo);
  - la regla K/V no sube respecto del diseño anterior (no se persigue, pero tampoco se empeora);
  - cada forma de escritura entre 20 % y 60 % (sondas H3/H3b: la forma de la clave varía entre versiones);
  - cada error del banco aparece en al menos el 10 % de las versiones (regla #1, Incidente N);
  - ninguna celda pasa de TOPE_CELDA (variedad).
Segunda etapa: con la tasa máxima fija (+0,5 pp), minimiza el peso de la celda más pesada.
Uso: python3 salida/pesos_lp.py salida/pesos_celdas.csv [partes]
"""
import csv, sys, itertools
import numpy as np
from scipy.optimize import linprog

ruta = sys.argv[1]; PARTES = int(sys.argv[2]) if len(sys.argv) > 2 else 40
filas = list(csv.DictReader(open(ruta, encoding="utf-8")))
reglas = [k for k in filas[0] if k not in ("terna", "estilo", "n")]
KV = "estimulo: contiene la relacion K/V y otra operacion"
POS = {"siempre la 1a", "siempre la 2a", "siempre la 3a", "siempre la ultima"}
obj = [r for r in reglas if r != KV and r not in POS]
Rm = np.array([[float(f[r]) for r in obj] for f in filas])   # todas las tasas vienen como fracción
kv = np.array([float(f[KV]) for f in filas])
n = len(filas)
viejo = np.array([1.0 if sum(x in ("VL-B", "VL-F", "VL-G") for x in f["terna"].split("+")) >= 2 else 0.0 for f in filas]); viejo /= viejo.sum()
import os
KV_MAX = float(kv @ viejo); PISO = 0.15
TOPE_CELDA = float(os.environ.get("TOPE_CELDA", "0.15")); MAX_EST = float(os.environ.get("MAX_EST", "0.60"))
MIN_EST = float(os.environ.get("MIN_EST", "0.20")); PISO_ERR = float(os.environ.get("PISO_ERR", "0.10"))
print("Parámetros: TOPE_CELDA=%.2f MAX_EST=%.2f MIN_EST=%.2f PISO_ERR=%.2f" % (TOPE_CELDA, MAX_EST, MIN_EST, PISO_ERR))
print("Diseño anterior (estructura, sin muestreo): K/V %.1f %%; máximo del vector %.1f %% (%s)" % (
    100 * KV_MAX, 100 * (viejo @ Rm).max(), obj[int((viejo @ Rm).argmax())]))
def restricciones(extra_t=None):
    A, b = [], []
    for j in range(len(obj)):
        A.append(list(Rm[:, j]) + [-1.0]); b.append(0.0)
    for nom in ("centralidad", "medoide"):
        j = obj.index(nom); A.append(list(-Rm[:, j]) + [0.0]); b.append(-PISO)
    A.append(list(kv) + [0.0]); b.append(KV_MAX)
    for est in ("fraccion_izq", "numero_izq", "fraccion_unica"):
        m = [1.0 if f["estilo"] == est else 0.0 for f in filas]
        A.append(m + [0.0]); b.append(MAX_EST); A.append([-x for x in m] + [0.0]); b.append(-MIN_EST)
    for e in ("VL-A", "VL-B", "VL-D", "VL-E", "VL-F", "VL-G"):
        A.append([-1.0 if e in f["terna"].split("+") else 0.0 for f in filas] + [0.0]); b.append(-PISO_ERR)
    if extra_t is not None:
        A.append([0.0] * n + [1.0]); b.append(extra_t)
    return np.array(A), np.array(b)
A, b = restricciones()
r1 = linprog(np.r_[np.zeros(n), 1.0], A_ub=A, b_ub=b, A_eq=np.array([[1.0] * n + [0.0]]), b_eq=[1.0],
             bounds=[(0, TOPE_CELDA)] * n + [(0, 1)], method="highs")
print("Etapa 1:", r1.message, "| tasa máxima óptima %.2f %%" % (100 * r1.x[-1]))
# Etapa 2: minimizar la celda más pesada con la tasa máxima fija (+0,5 pp).
A2, b2 = restricciones(r1.x[-1] + 0.005)
A2 = np.c_[A2, np.zeros(A2.shape[0])]
A2 = np.r_[A2, np.c_[np.eye(n), np.zeros((n, 1)), -np.ones((n, 1))]]; b2 = np.r_[b2, np.zeros(n)]
r2 = linprog(np.r_[np.zeros(n + 1), 1.0], A_ub=A2, b_ub=b2, A_eq=np.array([[1.0] * n + [0.0, 0.0]]), b_eq=[1.0],
             bounds=[(0, TOPE_CELDA)] * n + [(0, 1), (0, 1)], method="highs")
w = r2.x[:n]
partes = np.round(w * PARTES).astype(int)
wq = partes / partes.sum()
def informe(wv, rotulo):
    v = wv @ Rm; orden = np.argsort(-v)
    print("\n== %s: K/V %.1f %%" % (rotulo, 100 * (wv @ kv)))
    for j in orden[:8]:
        print("   %-55s %5.1f %%" % (obj[j], 100 * v[j]))
    for nom in ("centralidad", "medoide", "tres_datos"):
        print("   [%s] %.1f %%" % (nom, 100 * v[obj.index(nom)]))
    for est in ("fraccion_izq", "numero_izq", "fraccion_unica"):
        print("   forma %-15s %.1f %%" % (est, 100 * sum(wv[i] for i, f in enumerate(filas) if f["estilo"] == est)))
    for e in ("VL-A", "VL-B", "VL-D", "VL-E", "VL-F", "VL-G"):
        print("   error %s en %.1f %% de las versiones" % (e, 100 * sum(wv[i] for i, f in enumerate(filas) if e in f["terna"].split("+"))))
informe(viejo, "Diseño anterior")
informe(w, "Óptimo continuo")
informe(wq, "Pesos en %d partes" % PARTES)
print("\nPESOS <- c(" + ",\n           ".join('"%s|%s" = %d' % (filas[i]["terna"], filas[i]["estilo"], partes[i])
                                          for i in range(n) if partes[i] > 0) + ")")

# --- Etapa por estrato (añadida en la pasada 2): el agregado no acredita las formas (Incidentes F y R). ---------
# Para un tope T, exige en cada forma s y cada regla j del vector: sum_{c en s} w_c (r_jc - T) <= 0, es decir, que la
# tasa condicional a la forma no pase de T. Bisección sobre T; con el T mínimo factible (+0,5 pp), minimiza el
# máximo agregado.
def factible(T, minimizar_agregado=False):
    A, b = restricciones()
    A = [list(f) for f in A]; b = list(b)
    for est in ("fraccion_izq", "numero_izq", "fraccion_unica"):
        idx = [i for i, f in enumerate(filas) if f["estilo"] == est]
        for j in range(len(obj)):
            fila = [0.0] * (n + 1)
            for i in idx: fila[i] = Rm[i, j] - T
            A.append(fila); b.append(0.0)
    c = np.r_[np.zeros(n), 1.0] if minimizar_agregado else np.zeros(n + 1)
    return linprog(c, A_ub=np.array(A), b_ub=np.array(b), A_eq=np.array([[1.0] * n + [0.0]]), b_eq=[1.0],
                   bounds=[(0, TOPE_CELDA)] * n + [(0, 1)], method="highs")
lo, hi = 0.25, 1.0
for _ in range(30):
    mid = (lo + hi) / 2
    if factible(mid).status == 0: hi = mid
    else: lo = mid
rE = factible(hi + 0.005, minimizar_agregado=True)
wE = rE.x[:n]
print("\n=== Etapa por estrato: tope condicional mínimo por forma %.2f %%; máximo agregado %.2f %%" % (100 * hi, 100 * rE.x[-1]))
for est in ("fraccion_izq", "numero_izq", "fraccion_unica"):
    idx = [i for i, f in enumerate(filas) if f["estilo"] == est]; m = wE[idx].sum()
    v = (wE[idx] @ Rm[idx]) / m; top = np.argsort(-v)[:4]
    print("   forma %-15s (%.1f %% de las versiones): " % (est, 100 * m) + "; ".join("%s %.1f %%" % (obj[j], 100 * v[j]) for j in top))
partesE = np.round(wE * PARTES).astype(int); wEq = partesE / partesE.sum()
informe(wEq, "Pesos por estrato en %d partes" % PARTES)
for est in ("fraccion_izq", "numero_izq", "fraccion_unica"):
    idx = [i for i, f in enumerate(filas) if f["estilo"] == est]; m = wEq[idx].sum()
    v = (wEq[idx] @ Rm[idx]) / m; top = np.argsort(-v)[:3]
    print("   [redondeado] forma %-15s: " % est + "; ".join("%s %.1f %%" % (obj[j], 100 * v[j]) for j in top))
print("\nPESOS_ESTRATO <- c(" + ",\n           ".join('"%s|%s" = %d' % (filas[i]["terna"], filas[i]["estilo"], partesE[i])
                                          for i in range(n) if partesE[i] > 0) + ")")
