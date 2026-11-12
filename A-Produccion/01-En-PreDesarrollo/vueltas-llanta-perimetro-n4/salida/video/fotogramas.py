"""Extrae fotogramas por segmento y arma hojas de contacto. Uso: fotogramas.py <video> <carpeta>"""
import json, subprocess, sys, pathlib
v, out = sys.argv[1], pathlib.Path(sys.argv[2]); out.mkdir(exist_ok=True)
dur = json.load(open(pathlib.Path(__file__).parent / "audio/duraciones.json"))
ini = json.load(open(pathlib.Path(__file__).parent / "audio/inicios.json"))
t0 = 0.0
for frac in (0.5, 0.8, 0.99):
    t0 = 0.0; files = []
    for k, d in dur.items():
        f = out / f"{k}_{int(frac*100)}.png"
        subprocess.run(["ffmpeg", "-y", "-v", "error", "-ss", f"{ini[k] + frac*d:.2f}", "-i", v, "-frames:v", "1",
                        "-vf", "scale=854:-1", str(f)], check=True)
        t0 += d + 0.9
    subprocess.run(f"ffmpeg -y -v error -pattern_type glob -i '{out}/s*_{int(frac*100)}.png' -vf tile=3x3 {out}/hoja_{int(frac*100)}.png",
                   shell=True, check=True)
print("fin esperado", round(t0, 1))
