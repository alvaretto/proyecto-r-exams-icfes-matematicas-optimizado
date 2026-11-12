"""Montaje final: narración en su instante (sin el silencio de cola), música que cede ante la voz,
y subtítulos dinámicos palabra por palabra en una franja inferior (el cuadro se reduce al 88 %
para que no tapen nada)."""
import json, pathlib, subprocess
B = pathlib.Path(__file__).parent
ini = json.loads((B / "audio/inicios.json").read_text())
dur = json.loads((B / "audio/duraciones.json").read_text())
pal = json.loads((B / "audio/palabras.json").read_text())
# La voz entrega las palabras sin signos: aquí se devuelven los que deben verse en el subtítulo.
# Se toma cada palabra tal como está escrita en SEG de narracion.py (leído sin ejecutarlo, para no rehacer la voz).
import ast
_arbol = ast.parse((B / "narracion.py").read_text())
SEG = next(ast.literal_eval(n.value) for n in _arbol.body if isinstance(n, ast.Assign) and n.targets[0].id == "SEG")
_norm = lambda x: x.lower().strip("¡!¿?.,:;")
for k, ws in pal.items():
    toks = [t for f in SEG[k] for t in f[0].split()]
    assert len(toks) == len(ws) and all(_norm(t) == _norm(w[0]) for t, w in zip(toks, ws)), f"SIGNOS: la escena {k} no casa con el guion"
    for t, w in zip(toks, ws):
        w[0] = t
video = B / "media/videos/escenas/1080p30/Video.mp4"
salida = B / "vueltas_llanta_explicacion.mp4"

# ---------- subtítulos ASS: grupos de hasta 3 palabras, la palabra dicha en amarillo ----------
def ts(t):
    h, r = divmod(t, 3600); m, s_ = divmod(r, 60)
    return f"{int(h)}:{int(m):02d}:{s_:05.2f}"

ev = []
for k, ws in pal.items():
    grupos, g = [], []
    for i, w in enumerate(ws):
        pausa = i > 0 and w[1] - (ws[i - 1][1] + ws[i - 1][2]) > 0.22
        pegada = g and g[-1][0].lower().strip("¡!¿?.,:;") in {"y", "ciento", "setenta", "coma"}   # no partir «setenta y | cinco»
        if g and (pausa or (len(g) >= 3 and not pegada) or len(g) >= 5):
            grupos.append(g); g = []
        g.append(w)
    if g: grupos.append(g)
    for g in grupos:
        for j, w in enumerate(g):
            a = ini[k] + w[1]
            b = ini[k] + (g[j + 1][1] if j + 1 < len(g) else w[1] + w[2] + 0.12)
            txt = " ".join((r"{\c&H15CCFA&}" + x[0].upper() + r"{\c&HFFFFFF&}") if x is w else x[0].upper() for x in g)
            ev.append([a, b, txt])
# Ningún subtítulo se solapa con el siguiente: si se solapan, libass apila el segundo encima del cuadro.
for i in range(len(ev) - 1):
    ev[i][1] = min(ev[i][1], ev[i + 1][0])
lineas = [f"Dialogue: 0,{ts(a)},{ts(b)},S,,0,0,0,,{txt}" for a, b, txt in ev if b - a > 0.02]
ass = B / "audio" / "subtitulos.ass"
ass.write_text("""[Script Info]
ScriptType: v4.00+
PlayResX: 1920
PlayResY: 1080

[V4+ Styles]
Format: Name, Fontname, Fontsize, PrimaryColour, OutlineColour, BackColour, Bold, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV
Style: S,Noto Sans,70,&H00FFFFFF,&H00000000,&H00000000,-1,1,4,0,2,40,40,26

[Events]
Format: Layer, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text
""" + "\n".join(lineas) + "\n", encoding="utf-8")

# ---------- audio en tres pasos: pistas separadas -> medir sonoridad -> mezcla final ----------
import re
VOL_MUSICA = 0.36          # nivel de la música antes de ceder ante la voz
OBJETIVO_LUFS = -15.0      # sonoridad final (las plataformas de video rondan -14)
ST = B / "audio" / "stems"; ST.mkdir(exist_ok=True)
cmd = ["ffmpeg", "-y", "-v", "error"]
filtros = []
for i, (k, t) in enumerate(ini.items()):
    cmd += ["-i", str(B / f"audio/{k}.mp3")]
    filtros.append(f"[{i}:a]atrim=0:{dur[k]},aresample=48000,adelay={int(t * 1000)}:all=1[a{i}]")
n = len(ini)
cmd += ["-i", str(B / "audio/musica.wav")]
filtros.append("".join(f"[a{i}]" for i in range(n)) + f"amix=inputs={n}:normalize=0,asplit=3[voz][llave][vozm]")
# hueco en 1-3 kHz: ahí están las consonantes de la voz (medido: la música la igualaba en el 10 % peor)
filtros.append(f"[{n}:a]aresample=48000,volume={VOL_MUSICA},equalizer=f=1750:t=o:w=1.6:g=-8[mus]")
# la música cede poco y despacio: se sigue oyendo bajo la voz y no «bombea» en cada pausa
filtros.append("[mus][llave]sidechaincompress=threshold=0.03:ratio=2.5:attack=20:release=900,asplit=2[musd][musm]")
filtros.append("[vozm][musm]amix=inputs=2:normalize=0:duration=longest[pre]")
cmd += ["-filter_complex", ";".join(filtros), "-map", "[voz]", str(ST / "voz.wav"), "-map", "[musd]",
        str(ST / "musica_final.wav"), "-map", "[pre]", str(ST / "premezcla.wav")]
subprocess.run(cmd, check=True)
e = subprocess.run(["ffmpeg", "-hide_banner", "-i", str(ST / "premezcla.wav"), "-af", "ebur128", "-f", "null", "-"],
                   capture_output=True, text=True).stderr
lufs = float(re.findall(r"I:\s+(-?[\d.]+) LUFS", e)[-1])
gan = OBJETIVO_LUFS - lufs
print(f"premezcla {lufs} LUFS; ganancia {gan:+.1f} dB")
# ---------- video: cuadro al 88 % arriba, franja inferior del color del fondo, subtítulos encima ----------
subprocess.run(["ffmpeg", "-y", "-v", "error", "-i", str(video), "-i", str(ST / "premezcla.wav"), "-filter_complex",
                f"[0:v]scale=1690:950,pad=1920:1080:115:0:color=0x0f172a,ass={ass.name}[v];"
                f"[1:a]volume={gan}dB,alimiter=limit=0.8:attack=3:release=60:level=disabled[mix]",
                "-map", "[v]", "-map", "[mix]", "-c:v", "libx264", "-crf", "18", "-preset", "medium",
                "-pix_fmt", "yuv420p", "-c:a", "aac", "-b:a", "192k", "-shortest", "-movflags", "+faststart",
                str(salida)], check=True, cwd=ass.parent)
print(salida)
