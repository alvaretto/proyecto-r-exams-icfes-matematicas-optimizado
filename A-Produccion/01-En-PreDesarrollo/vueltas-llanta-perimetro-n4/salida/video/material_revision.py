"""Material para revisar el video sin reproducirlo: un fotograma cada 2 s (nombrado con su segundo)
y la narración con tiempos. Uso: material_revision.py  ->  revision/"""
import json, pathlib, subprocess

B = pathlib.Path(__file__).parent
R = B / "revision"
F = R / "fotogramas"
F.mkdir(parents=True, exist_ok=True)
video = B / "vueltas_llanta_explicacion.mp4"
d = float(subprocess.run(["ffprobe", "-v", "error", "-show_entries", "format=duration", "-of", "csv=p=0", str(video)],
                         capture_output=True, text=True).stdout)
for viejo in F.glob('seg_*.png'):
    viejo.unlink()
n = 0
t = 1
while t < d:
    subprocess.run(["ffmpeg", "-y", "-v", "error", "-ss", str(t), "-i", str(video), "-frames:v", "1",
                    "-vf", "scale=960:-1", str(F / f"seg_{t:03d}.png")], check=True)
    n += 1
    t += 2

ini = json.loads((B / "audio/inicios.json").read_text())
pal = json.loads((B / "audio/palabras.json").read_text())
dur = json.loads((B / "audio/duraciones.json").read_text())
out = ["# Narración del video con tiempos (segundos desde el inicio)\n\n",
       "Es lo que OYE el estudiante. Cada línea: [inicio-fin] frase.\n"]
for k, ws in pal.items():
    out.append(f"\n## Escena {k[1:]} (empieza en {ini[k]:.1f} s, narración de {dur[k]:.1f} s)\n\n")
    linea, t0 = [], None
    for i, w in enumerate(ws):
        if t0 is None:
            t0 = ini[k] + w[1]
        linea.append(w[0])
        fin = ini[k] + w[1] + w[2]
        if i + 1 == len(ws) or ws[i + 1][1] - (w[1] + w[2]) > 0.25:
            out.append(f"- [{t0:6.1f}-{fin:6.1f}] {' '.join(linea)}\n")
            linea, t0 = [], None
(R / "narracion_con_tiempos.md").write_text("".join(out))
print(n, "fotogramas;", round(d, 1), "s")
