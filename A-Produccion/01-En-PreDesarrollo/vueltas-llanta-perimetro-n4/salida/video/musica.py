"""Base musical original (urbano, patrón dembow, 96 BPM) sintetizada por código y ATADA A LA NARRACIÓN.
Sin samples ni obras ajenas.

- Sonidos con armónicos por encima de 300 Hz (bombo y 808 saturados, acordes y melodía de onda rica),
  para que la base exista en un parlante de celular.
- Estructura por escenas: la energía sube y baja con el contenido, y la tonalidad sube al final.
- Efectos en los momentos clave (golpe del gancho, subida antes de la expresión, campana del «coincide»,
  zumbido al descartar, golpe final), colocados con los tiempos reales de las palabras.

Uso: musica.py <segundos>  ->  audio/musica.wav   (lee audio/inicios.json y audio/palabras.json)"""
import json, pathlib, sys, wave
import numpy as np
from scipy.signal import butter, lfilter, sosfilt

SR = 44100
BPM = 96
PASO = 60 / BPM / 4
COMPAS = 16 * PASO
B = pathlib.Path(__file__).parent
rng = np.random.default_rng(28)
dur = float(sys.argv[1]) if len(sys.argv) > 1 else 180.0
N = int((dur + 3) * SR)
ini = json.loads((B / "audio/inicios.json").read_text())
pal = json.loads((B / "audio/palabras.json").read_text())

# buses estéreo: batería y bajo (secos), armonía (con reverberación), efectos
bus = {k: np.zeros((N, 2)) for k in ("bat", "bajo", "arm", "fx")}


def poner(b, t, x, gl=1.0, gr=None):
    i = int(t * SR)
    if i < 0 or i >= N:
        return
    x = x[: N - i]
    bus[b][i:i + len(x), 0] += gl * x
    bus[b][i:i + len(x), 1] += (gl if gr is None else gr) * x


def env(n, tau, ataque=0.003):
    e = np.exp(-np.arange(n) / (tau * SR))
    a = int(ataque * SR)
    e[:a] *= np.linspace(0, 1, a)
    return e


def filtro(x, f, tipo="low", orden=2):
    return sosfilt(butter(orden, f, tipo, fs=SR, output="sos"), x)


def hz(m):
    return 440 * 2 ** ((m - 69) / 12)


def bombo():
    n = int(0.42 * SR); t = np.arange(n) / SR
    f = 52 + 130 * np.exp(-t / 0.03)
    x = np.sin(2 * np.pi * np.cumsum(f) / SR) * env(n, 0.13)
    clic = filtro(rng.standard_normal(n), 2500, "high") * env(n, 0.004) * 0.5
    return np.tanh(3.0 * x) * 0.8 + clic            # la saturación crea armónicos que sí salen en un celular


def bajo808(m, d=0.5):
    n = int(d * SR); t = np.arange(n) / SR
    f = hz(m) * (1 + 0.5 * np.exp(-t / 0.02))        # pequeño «glide» de entrada
    x = np.sin(2 * np.pi * np.cumsum(f) / SR)
    return np.tanh(4.0 * x) * env(n, d * 0.55, 0.004)


def caja():
    n = int(0.22 * SR); t = np.arange(n) / SR
    r = filtro(rng.standard_normal(n), [900, 7000], "band")
    return r * env(n, 0.06) * 0.9 + np.sin(2 * np.pi * 190 * t) * env(n, 0.035) * 0.5


def palmas():
    n = int(0.25 * SR); x = np.zeros(n)
    for d in (0.0, 0.011, 0.023):
        k = int(d * SR)
        r = filtro(rng.standard_normal(n - k), [1100, 5000], "band")
        x[k:] += r * env(n - k, 0.045)
    return x * 0.6


def hat(tau=0.022):
    n = int(0.09 * SR)
    return filtro(rng.standard_normal(n), 7000, "high") * env(n, tau, 0.001) * 0.5


def sierra(m, d, tau, corte=2600, vib=0.0):
    """Nota de onda rica (armónicos 1..10) pasada por un pasa-bajos: suena en la banda media."""
    n = int(d * SR); t = np.arange(n) / SR
    f = hz(m)
    fase = 2 * np.pi * f * t + (vib * np.sin(2 * np.pi * 5.5 * t) if vib else 0)
    x = sum(np.sin(k * fase) / k for k in range(1, 11) if k * f < SR / 2.2)
    return filtro(x, corte) * env(n, tau, 0.006)


def subida(d=1.3):
    n = int(d * SR); t = np.arange(n) / SR
    f = 250 * (6 ** (t / d))
    tono = np.sin(2 * np.pi * np.cumsum(f) / SR)
    ruido = filtro(rng.standard_normal(n), 1500, "high")
    return (0.5 * tono + 0.5 * ruido) * (t / d) ** 2


def golpe():
    n = int(1.1 * SR); t = np.arange(n) / SR
    sub = np.tanh(3 * np.sin(2 * np.pi * np.cumsum(40 + 110 * np.exp(-t / 0.08)) / SR)) * env(n, 0.3)
    ruido = filtro(rng.standard_normal(n), 3500) * env(n, 0.22, 0.001)
    return 0.8 * sub + 0.6 * ruido


def campana():
    x = np.zeros(int(0.9 * SR))
    for k, m in enumerate((79, 84, 91)):             # sol, do, sol: «¡bien!»
        nt = sierra(m, 0.6, 0.18, corte=6000)
        i = int(k * 0.085 * SR)
        x[i:i + len(nt)] += nt[: len(x) - i]
    return x


def zumbido():
    n = int(0.3 * SR); t = np.arange(n) / SR
    f = 330 * (0.4 ** (t / 0.3))                     # tono que cae: «esta no»
    fase = 2 * np.pi * np.cumsum(f) / SR
    return filtro(sum(np.sin(k * fase) / k for k in range(1, 8)), 1800) * env(n, 0.12, 0.004)


# ---------- tiempos de la narración ----------
def t_pal(k, palabra, n=1):
    norm = lambda x: x.lower().strip("¡!¿?.,:;")
    h = [w for w in pal[k] if norm(w[0]) == palabra]
    return ini[k] + h[n - 1][1] if len(h) >= n else None


claves = list(ini)
def escena(t):
    k = claves[0]
    for c in claves:
        if t >= ini[c] - 0.05:
            k = c
    return k

NIVEL = {"s1": 2, "s2": 1, "s3": 1, "s4": 2, "s5": 0, "s6": 1, "s7": 2, "s8": 2, "s9": 2}
t_expr = t_pal("s6", "expresión") or 1e9             # ahí «cae» la base completa
t_por = t_pal("s5", "por") or 1e9
t_sube = ini.get("s8", 1e9)                          # desde aquí, todo un tono arriba

# vi-IV-I-V (la menor, fa, do, sol) y dos motivos
ACORDES = [(45, [69, 72, 76]), (41, [69, 72, 77]), (48, [67, 72, 76]), (43, [67, 71, 74])]
MOTIVO_A = [(0, 81), (3, 79), (6, 76), (10, 79), (16, 76), (19, 74), (22, 72), (26, 74)]
MOTIVO_B = [(0, 84), (3, 81), (6, 79), (8, 81), (11, 79), (14, 76), (16, 79), (19, 76), (22, 74), (27, 76)]
K, C, P, H, HA = bombo(), caja(), palmas(), hat(), hat(0.06)

n_comp = int(dur / COMPAS) + 1
niveles = []
for c in range(n_comp):
    t0 = c * COMPAS
    k = escena(t0 + COMPAS / 2)
    lv = NIVEL[k]
    if k == "s6" and t0 + COMPAS / 2 >= t_expr: lv = 2
    if k == "s5" and t0 >= t_por: lv = 1
    niveles.append(lv)

for c in range(n_comp):
    t0 = c * COMPAS
    lv = niveles[c]
    tr = 2 if t0 + COMPAS / 2 >= t_sube else 0
    raiz, triada = ACORDES[c % 4]
    sube_luego = c + 1 < n_comp and niveles[c + 1] > lv
    redoble = (c % 4 == 3)
    for p in range(16):
        t = t0 + p * PASO
        # batería
        if lv >= 2 and p in (0, 4, 8, 12):
            poner("bat", t, K, 0.9)
        if lv >= 1 and p in (3, 6, 11, 14):
            poner("bat", t, C, 0.5)
            if p in (6, 14): poner("bat", t, P, 0.45, 0.55)
        if lv == 0 and p in (4, 12):
            poner("bat", t, P, 0.3)
        if sube_luego and p >= 12:                    # relleno de caja antes de subir de nivel
            poner("bat", t, C, 0.35 + 0.08 * (p - 12))
        if lv >= 2 or p % 2 == 0:
            g = 0.5 if p % 4 == 2 else 0.28
            poner("bat", t, HA if (p == 14 and lv >= 1) else H, g * 0.9, g)
        if redoble and lv >= 2 and p in (13, 15):     # redoble de hi-hat en tresillo
            for j in range(3):
                poner("bat", t + j * PASO / 3, H, 0.3, 0.35)
        # bajo 808
        if lv >= 1 and p in (0, 3, 6, 10):
            poner("bajo", t, bajo808(raiz + tr, 0.42 if p else 0.55), 0.55)
        # acordes punteados
        if p in (0, 3, 6, 8, 11, 14):
            for j, m in enumerate(triada):
                x = sierra(m + tr, 0.32, 0.1, corte=2200 + 600 * lv)
                poner("arm", t + 0.004 * j, x, 0.13 + 0.03 * (j == 0), 0.13 + 0.03 * (j == 2))
    # melodía
    if lv >= 2:
        mot = MOTIVO_B if tr else MOTIVO_A
        for p, m in mot:
            if p // 16 == c % 2:
                x = sierra(m + tr, 0.5, 0.2, corte=3800, vib=0.06)
                poner("arm", t0 + (p % 16) * PASO, x, 0.2, 0.24)
    elif lv == 0 and c % 2 == 0:                      # colchón en la parte tranquila
        for m in triada:
            poner("arm", t0, sierra(m + tr - 12, 2 * COMPAS, 1.6, corte=900) * 0.6, 0.12)

# ---------- efectos atados a la narración ----------
poner("fx", 0.0, golpe(), 0.9)                                         # gancho: golpe en t = 0
if t_expr < 1e8:
    poner("fx", t_expr - 1.3, subida(), 0.5); poner("fx", t_expr, golpe(), 0.7)
t = t_pal("s7", "también")
if t: poner("fx", t + 0.9, campana(), 0.55)                            # «¡Coincide!»
for palabra, n in (("ciento", 2), ("cien", 3), ("solo", 2)):           # opciones que caen
    t = t_pal("s8", palabra, n)
    if t: poner("fx", t - 0.15, zumbido(), 0.45)
t = t_pal("s8", "setenta", 3)                                           # «…llega a setenta y cinco vueltas»
assert t, "falta la palabra que ancla la subida y la campana de la respuesta"
poner("fx", t - 0.4, subida(1.0), 0.22); poner("fx", t + 0.6, campana(), 0.4)   # bajos: caen sobre la frase de la respuesta
t_fin = ini[claves[-1]] + pal[claves[-1]][-1][1] + pal[claves[-1]][-1][2]
poner("fx", t_fin + 0.15, golpe(), 0.9)                                # golpe final seco
for m in (57, 64, 69, 72):
    poner("fx", t_fin + 0.15, sierra(m + 2, 1.4, 0.45, corte=3000), 0.14)

# ---------- reverberación corta en armonía y efectos ----------
def reverb(x, mezcla=0.22):
    y = np.zeros_like(x)
    for D, g in ((0.0297, 0.72), (0.0371, 0.70), (0.0411, 0.68), (0.0437, 0.66)):
        d = int(D * SR)
        a = np.zeros(d + 1); a[0] = 1; a[d] = -g
        y += lfilter([1], a, x, axis=0)
    return x + mezcla * filtro(y.T, 5000).T / 4

mix = bus["bat"] + bus["bajo"] + reverb(bus["arm"]) + reverb(bus["fx"], 0.3)
# la música calla tras el golpe final (queda su cola)
i_fin = int((t_fin + 0.15) * SR)
cola = np.ones(N); cola[i_fin:] = 0
suave = int(0.02 * SR)
cola[i_fin - suave:i_fin] = np.linspace(1, 0, suave)
mix = (bus["bat"] + bus["bajo"]) * cola[:, None] + reverb(bus["arm"] * cola[:, None]) + reverb(bus["fx"], 0.3)
mix = mix[: int(dur * SR)]
mix = np.tanh(1.6 * mix / np.max(np.abs(mix))) * 0.92
out = B / "audio" / "musica.wav"
with wave.open(str(out), "wb") as w:
    w.setnchannels(2); w.setsampwidth(2); w.setframerate(SR)
    w.writeframes((mix * 32767).astype("<i2").tobytes())
print(out, round(dur, 1), "s; niveles por compás:", "".join(map(str, niveles)))
