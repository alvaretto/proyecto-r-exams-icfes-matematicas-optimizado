"""Mide el audio del video para que un revisor que NO puede oír trabaje con datos:
sonoridad, relación voz/música, música en las pausas, contenido grave (parlante de celular),
ritmo de habla, repetición de la base y una transcripción automática de la mezcla final
(si el transcriptor entiende la voz con la música encima, un oyente también).
Uso: ~/.venvs/manim/bin/python audio_medidas.py  ->  revision/audio_medidas.md"""
import json, pathlib, re, subprocess, unicodedata, wave
import numpy as np

B = pathlib.Path(__file__).parent
R = B / "revision"; R.mkdir(exist_ok=True)
TMP = R / "stems"; TMP.mkdir(exist_ok=True)
video = B / "vueltas_llanta_explicacion.mp4"
ini = json.loads((B / "audio/inicios.json").read_text())
dur = json.loads((B / "audio/duraciones.json").read_text())
pal = json.loads((B / "audio/palabras.json").read_text())
SR = 48000

def run(cmd):
    return subprocess.run(cmd, capture_output=True, text=True)

# ---------- pistas separadas: las deja montar.py en audio/stems ----------
ST = B / "audio" / "stems"
for nombre in ("voz", "musica_final"):
    subprocess.run(["ffmpeg", "-y", "-v", "error", "-i", str(ST / f"{nombre}.wav"), "-ac", "1", "-ar", str(SR),
                    "-c:a", "pcm_s16le", str(TMP / f"{nombre}.wav")], check=True)
subprocess.run(["ffmpeg", "-y", "-v", "error", "-i", str(video), "-vn", "-ac", "1", "-ar", str(SR), str(TMP / "mezcla.wav")], check=True)

def leer(p):
    with wave.open(str(p)) as w:
        x = np.frombuffer(w.readframes(w.getnframes()), dtype="<i2").astype(float) / 32768
    return x

voz, mus, mez = leer(TMP / "voz.wav"), leer(TMP / "musica_final.wav"), leer(TMP / "mezcla.wav")
L = min(len(voz), len(mus), len(mez)); voz, mus, mez = voz[:L], mus[:L], mez[:L]
db = lambda x: 20 * np.log10(np.sqrt(np.mean(x ** 2)) + 1e-9)

# ventanas de 0,5 s
W = SR // 2
nv = L // W
rv = np.array([db(voz[i * W:(i + 1) * W]) for i in range(nv)])
rm = np.array([db(mus[i * W:(i + 1) * W]) for i in range(nv)])
habla = rv > -40
ratio = rv[habla] - rm[habla]
# lo mismo en la banda que reproduce un parlante de celular (300-4000 Hz)
from scipy.signal import butter, sosfilt
_sos = butter(4, [300, 4000], "band", fs=SR, output="sos")
vb, mb = sosfilt(_sos, voz), sosfilt(_sos, mus)
rvb = np.array([db(vb[i * W:(i + 1) * W]) for i in range(nv)])
rmb = np.array([db(mb[i * W:(i + 1) * W]) for i in range(nv)])
ratio_b = rvb[habla] - rmb[habla]

# sonoridad EBU R128 de la mezcla
e = run(["ffmpeg", "-hide_banner", "-i", str(video), "-vn", "-af", "ebur128=peak=true", "-f", "null", "-"]).stderr
I = re.findall(r"I:\s+(-?[\d.]+) LUFS", e)[-1]; LRA = re.findall(r"LRA:\s+(-?[\d.]+) LU", e)[-1]
PK = re.findall(r"Peak:\s+(-?[\d.]+) dBFS", e)[-1]

# contenido por bandas de la música (¿qué queda en un parlante de celular, que casi no da < 300 Hz?)
def bandas(x):
    X = np.abs(np.fft.rfft(x[: SR * 60])) ** 2
    fr = np.fft.rfftfreq(SR * 60 if len(x) >= SR * 60 else len(x), 1 / SR)[: len(X)]
    tot = X.sum()
    return [100 * X[(fr >= a) & (fr < b)].sum() / tot for a, b in ((0, 150), (150, 300), (300, 2000), (2000, 24000))]
bm = bandas(leer(B / "audio/musica.wav")[::1] if False else mus)

# ritmo de habla por escena
ritmo = {k: len(ws) / (ws[-1][1] + ws[-1][2] - ws[0][1]) for k, ws in pal.items()}
# pausas dentro de la voz (silencios entre palabras > 0,6 s)
pausas = []
for k, ws in pal.items():
    for a, b in zip(ws, ws[1:]):
        g = b[1] - (a[1] + a[2])
        if g > 0.6:
            pausas.append((round(ini[k] + a[1] + a[2], 1), round(g, 2)))

# transcripción automática de la MEZCLA final
from faster_whisper import WhisperModel
modelo = WhisperModel("small", device="cpu", compute_type="int8")
# se le pasa el audio ya decodificado (16 kHz): el decodificador propio choca con la versión de «av» de Manim
subprocess.run(["ffmpeg", "-y", "-v", "error", "-i", str(TMP / "mezcla.wav"), "-ar", "16000", str(TMP / "mezcla16.wav")], check=True)
segs, _ = modelo.transcribe(leer(TMP / "mezcla16.wav").astype(np.float32), language="es", beam_size=5)
segs = list(segs)
trans = " ".join(s.text.strip() for s in segs)

def norm(t):
    t = unicodedata.normalize("NFD", t.lower())
    t = "".join(c for c in t if unicodedata.category(c) != "Mn")
    return re.findall(r"[a-z0-9]+", t)
ref = norm(" ".join(w[0] for ws in pal.values() for w in ws))
hip = norm(trans)
# distancia de edición por palabras
d = np.arange(len(hip) + 1)
for i, r in enumerate(ref, 1):
    prev, d[0] = d[0], i
    for j, h in enumerate(hip, 1):
        prev, d[j] = d[j], min(d[j] + 1, d[j - 1] + 1, prev + (r != h))
wer = 100 * d[-1] / len(ref)

comp = 60 / 96 * 4
out = f"""# Medidas del audio del video (ningún revisor lo oyó: son mediciones)

Duración: {L / SR:.1f} s. Voz: es-CO-SalomeNeural (edge-tts), velocidad +12 %, tono +12 Hz.
Música: base original sintetizada por `musica.py`.

## Sonoridad de la mezcla
- Integrada: {I} LUFS (referencia habitual de plataformas de video: cerca de -14 LUFS).
- Rango de sonoridad (LRA): {LRA} LU. Pico: {PK} dBFS.

## Voz frente a música (ventanas de 0,5 s)
- Mientras hay voz ({habla.sum() / 2:.0f} s): la voz queda en promedio {ratio.mean():.1f} dB por encima de la música
  (mínimo {ratio.min():.1f} dB, percentil 10 = {np.percentile(ratio, 10):.1f} dB; ventanas con menos de 10 dB de ventaja: {100 * (ratio < 10).mean():.0f} %).
- En la banda de un parlante de celular (300-4000 Hz): la voz queda {ratio_b.mean():.1f} dB por encima de la música
  (percentil 10 = {np.percentile(ratio_b, 10):.1f} dB; ventanas con menos de 6 dB de ventaja: {100 * (ratio_b < 6).mean():.0f} %).
- Nivel medio de la voz al hablar: {rv[habla].mean():.1f} dBFS. Música bajo la voz: {rm[habla].mean():.1f} dBFS.
- Música en las pausas sin voz ({(~habla).sum() / 2:.0f} s en total): {rm[~habla].mean():.1f} dBFS
  (sube {rm[~habla].mean() - rm[habla].mean():.1f} dB respecto a cuando hay voz).

## Qué queda de la música en un parlante de celular
Reparto de la energía de la música (primer minuto): < 150 Hz: {bm[0]:.0f} % · 150-300 Hz: {bm[1]:.0f} % ·
300-2000 Hz: {bm[2]:.0f} % · > 2000 Hz: {bm[3]:.0f} %. Un parlante de celular casi no reproduce por debajo de 300 Hz.

## Estructura de la base
Ver la cabecera y el código de `musica.py` (niveles de energía por escena, cambio de tonalidad y efectos atados a la narración).

## Ritmo de habla (palabras por segundo, por escena)
{chr(10).join(f"- Escena {k[1:]}: {v:.2f}" for k, v in ritmo.items())}
- Media: {np.mean(list(ritmo.values())):.2f}. Pausas dentro de la voz mayores de 0,6 s: {len(pausas)} {pausas[:12]}

## ¿Se entiende la voz con la música encima? (transcripción automática de la mezcla final)
- Palabras del guion: {len(ref)}. Error de palabras del transcriptor (WER): {wer:.1f} %.
- OJO con esa cifra: el transcriptor escribe los números con dígitos («75») y el guion los tiene en letras
  («setenta y cinco»), así que casi todo el error es de formato, no de comprensión. Leer la transcripción:
  lo que importa es si alguna palabra salió cambiada (por ejemplo «y van centímetros» donde el guion dice
  «y va en centímetros») o si una exclamación salió como pregunta (pista de la entonación).
- Transcripción obtenida:

> {trans}
"""
(R / "audio_medidas.md").write_text(out)
print(out[:1800])
