"""Mide el audio del video, porque ningún revisor automático lo oye: sonoridad, ventaja de la voz sobre la
música (total, banda de celular y banda de las consonantes), ritmo de habla y una transcripción automática
de la mezcla final comparada con el guion.

Compuertas (exit 1 si falla alguna): sonoridad de -15 ± 0,5 LUFS, pico <= -0,5 dBFS y, en 1-3 kHz, la voz
por encima de la música en el 10 % peor de los momentos.

OJO: la transcripción cambia de una corrida a otra. Una palabra distinta es una SEÑAL para que el profesor
oiga ese tramo, no una prueba.

Uso: python -m video_icfes.medidas [--sin-transcripcion]  ->  revision_<formato>/audio_medidas.md y .json"""
import difflib, json, re, subprocess, sys, unicodedata, wave
import numpy as np
from scipy.signal import butter, sosfilt
from . import proyecto

SR = 48000
NUMEROS = set("cero uno una un dos tres cuatro cinco seis siete ocho nueve diez once doce trece catorce quince veinte "
              "treinta cuarenta cincuenta sesenta setenta ochenta noventa cien ciento doscientos doscientas trescientos "
              "trescientas cuatrocientos quinientos mil y coma medio media".split())


def leer(p):
    with wave.open(str(p)) as w:
        return np.frombuffer(w.readframes(w.getnframes()), dtype="<i2").astype(float) / 32768


def norm(t):
    t = unicodedata.normalize("NFD", t.lower())
    return re.findall(r"[a-z0-9]+", "".join(c for c in t if unicodedata.category(c) != "Mn"))


def main():
    B = proyecto.carpeta(); fmt = proyecto.formato(); g = proyecto.guion(B)
    R = B / f"revision_{fmt}"; TMP = R / "stems"; TMP.mkdir(parents=True, exist_ok=True)
    video = B / f"{g.NOMBRE}_{fmt}.mp4"
    ini = proyecto.leer(f"inicios_{fmt}.json", B); pal = proyecto.leer("palabras.json", B)
    ST = B / "audio" / f"stems_{fmt}"
    for nombre in ("voz", "musica_final"):
        subprocess.run(["ffmpeg", "-y", "-v", "error", "-i", str(ST / f"{nombre}.wav"), "-ac", "1", "-ar", str(SR),
                        "-c:a", "pcm_s16le", str(TMP / f"{nombre}.wav")], check=True)
    subprocess.run(["ffmpeg", "-y", "-v", "error", "-i", str(video), "-vn", "-ac", "1", "-ar", str(SR), str(TMP / "mezcla.wav")], check=True)
    voz, mus, mez = leer(TMP / "voz.wav"), leer(TMP / "musica_final.wav"), leer(TMP / "mezcla.wav")
    L = min(len(voz), len(mus), len(mez)); voz, mus, mez = voz[:L], mus[:L], mez[:L]
    db = lambda x: 20 * np.log10(np.sqrt(np.mean(x ** 2)) + 1e-9)
    W = SR // 2; nv = L // W                      # ventanas de 0,5 s
    vent = lambda x: np.array([db(x[i * W:(i + 1) * W]) for i in range(nv)])
    rv, rm = vent(voz), vent(mus)
    habla = rv > -40
    ratio = rv[habla] - rm[habla]

    def banda(a, b):
        sos = butter(4, [a, b], "band", fs=SR, output="sos")
        return vent(sosfilt(sos, voz)) - vent(sosfilt(sos, mus))
    r_cel, r_con = banda(300, 4000), banda(1000, 3000)
    claves = list(ini)
    esc = np.array([max(i for i, k in enumerate(claves) if (j * 0.5) >= ini[k] - 0.26) for j in range(nv)])
    por_escena = {k: (float(np.median(r_con[habla & (esc == i)])), float(np.percentile(r_con[habla & (esc == i)], 10)))
                  for i, k in enumerate(claves) if (habla & (esc == i)).sum() >= 4}

    e = subprocess.run(["ffmpeg", "-hide_banner", "-i", str(video), "-vn", "-af", "ebur128=peak=true", "-f", "null", "-"],
                       capture_output=True, text=True).stderr
    I = float(re.findall(r"I:\s+(-?[\d.]+) LUFS", e)[-1]); LRA = re.findall(r"LRA:\s+(-?[\d.]+) LU", e)[-1]
    PK = float(re.findall(r"Peak:\s+(-?[\d.]+) dBFS", e)[-1])

    X = np.abs(np.fft.rfft(mus[: SR * 60])) ** 2
    fr = np.fft.rfftfreq(min(len(mus), SR * 60), 1 / SR)
    bm = [100 * X[(fr >= a) & (fr < b)].sum() / X.sum() for a, b in ((0, 150), (150, 300), (300, 2000), (2000, 24000))]

    ritmo = {k: len(ws) / (ws[-1][1] + ws[-1][2] - ws[0][1]) for k, ws in pal.items()}
    pausas = [(round(ini[k] + a[1] + a[2], 1), round(b[1] - (a[1] + a[2]), 2))
              for k, ws in pal.items() for a, b in zip(ws, ws[1:]) if b[1] - (a[1] + a[2]) > 0.6]

    p10 = float(np.percentile(r_con[habla], 10))
    puertas = {"sonoridad_-15±0,5_LUFS": abs(I + 15.0) <= 0.5, "pico_<=_-0,5_dBFS": PK <= -0.5,
               "voz_sobre_musica_1-3kHz_p10_>0": p10 > 0}

    trans, wer, distintas = "(no se transcribió)", None, []
    if "--sin-transcripcion" not in sys.argv:
        from faster_whisper import WhisperModel
        modelo = WhisperModel("small", device="cpu", compute_type="int8")
        # se le pasa el audio ya decodificado (16 kHz): el decodificador propio choca con la versión de «av» de Manim
        subprocess.run(["ffmpeg", "-y", "-v", "error", "-i", str(TMP / "mezcla.wav"), "-ar", "16000", str(TMP / "mezcla16.wav")], check=True)
        segs, _ = modelo.transcribe(leer(TMP / "mezcla16.wav").astype(np.float32), language="es", beam_size=5)
        trans = " ".join(s.text.strip() for s in segs)
        ref, hip = norm(" ".join(w[0] for ws in pal.values() for w in ws)), norm(trans)
        sm = difflib.SequenceMatcher(None, ref, hip, autojunk=False)
        errores = 0
        for op, a0, a1, b0, b1 in sm.get_opcodes():
            if op == "equal": continue
            errores += max(a1 - a0, b1 - b0)
            r_, h_ = ref[a0:a1], hip[b0:b1]
            de_formato = all(w in NUMEROS for w in r_) and all(w.isdigit() or w in NUMEROS for w in h_) and r_
            if not de_formato:
                distintas.append((" ".join(ref[max(0, a0 - 3):a0]), " ".join(r_) or "∅", " ".join(h_) or "∅"))
        wer = 100 * errores / len(ref)

    lineas_dist = "\n".join(f"- después de «…{c}»: el guion dice «{r_}» y se transcribió «{h_}»" for c, r_, h_ in distintas) \
        or "- Ninguna (aparte de los números, que el transcriptor escribe con cifras)."
    out = f"""# Medidas del audio del video (ningún revisor lo oyó: son mediciones)

Formato: {fmt}. Duración: {L / SR:.1f} s. Voz: {g.VOZ} (edge-tts). Música: base original sintetizada.
Volumen de música (perilla del profesor): {getattr(g, "VOL_MUSICA", 0.36)}.

## Compuertas
{chr(10).join(f"- {'PASA ' if v else 'FALLA'} {k}" for k, v in puertas.items())}

## Sonoridad de la mezcla
- Integrada: {I} LUFS. Rango de sonoridad (LRA): {LRA} LU. Pico: {PK} dBFS.

## Voz frente a música (ventanas de 0,5 s, mientras hay voz: {habla.sum() / 2:.0f} s)
- Todo el espectro: la voz queda en promedio {ratio.mean():.1f} dB por encima (percentil 10 = {np.percentile(ratio, 10):.1f} dB).
- Banda de un parlante de celular (300-4000 Hz): {r_cel[habla].mean():.1f} dB por encima
  (percentil 10 = {np.percentile(r_cel[habla], 10):.1f} dB; ventanas con menos de 6 dB de ventaja: {100 * (r_cel[habla] < 6).mean():.0f} %).
- Banda de las consonantes (1000-3000 Hz): mediana {np.median(r_con[habla]):.1f} dB, percentil 10 = {p10:.1f} dB;
  momentos con la música por encima de la voz: {100 * (r_con[habla] < 0).mean():.0f} %.
- Por escena en 1000-3000 Hz (mediana / percentil 10): {" · ".join(f"{k} {a:.1f}/{b:.1f}" for k, (a, b) in por_escena.items())}
- Música en las pausas sin voz ({(~habla).sum() / 2:.0f} s): sube {rm[~habla].mean() - rm[habla].mean():.1f} dB respecto a cuando hay voz.

## Qué queda de la música en un parlante de celular
Energía de la música (primer minuto): < 150 Hz: {bm[0]:.0f} % · 150-300 Hz: {bm[1]:.0f} % · 300-2000 Hz: {bm[2]:.0f} % · > 2000 Hz: {bm[3]:.0f} %.
Un parlante de celular casi no reproduce por debajo de 300 Hz.

## Ritmo de habla (palabras por segundo)
{" · ".join(f"{k}: {v:.2f}" for k, v in ritmo.items())} · media {np.mean(list(ritmo.values())):.2f}
Pausas dentro de la voz mayores de 0,6 s: {len(pausas)} {pausas[:14]}

## ¿Se entiende la voz con la música encima? (transcripción automática de la mezcla final)
Error de palabras (WER): {"sin medir" if wer is None else f"{wer:.1f} %"}. Casi todo suele ser formato de números.
La transcripción varía entre corridas: cada línea de abajo es un tramo para OÍR, no un defecto confirmado.

### Palabras que el transcriptor oyó distinto
{lineas_dist}

### Transcripción completa
> {trans}
"""
    (R / "audio_medidas.md").write_text(out)
    (R / "audio_medidas.json").write_text(json.dumps(
        {"lufs": I, "pico_dbfs": PK, "p10_1_3khz": p10, "mediana_1_3khz": float(np.median(r_con[habla])),
         "wer": wer, "distintas": distintas, "puertas": puertas, "duracion": L / SR}, ensure_ascii=False, indent=1))
    print(out[:1500])
    sys.exit(0 if all(puertas.values()) else 1)


if __name__ == "__main__":
    main()
