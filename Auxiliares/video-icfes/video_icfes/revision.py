"""Material para revisar el video sin reproducirlo: un fotograma cada 2 s (nombrado con su segundo)
y la narración con tiempos.  Uso: python -m video_icfes.revision  ->  revision_<formato>/"""
import subprocess
from . import proyecto


def main():
    B = proyecto.carpeta(); fmt = proyecto.formato(); g = proyecto.guion(B)
    R = B / f"revision_{fmt}"; F = R / "fotogramas"; F.mkdir(parents=True, exist_ok=True)
    video = B / f"{g.NOMBRE}_{fmt}.mp4"
    d = float(subprocess.run(["ffprobe", "-v", "error", "-show_entries", "format=duration", "-of", "csv=p=0", str(video)],
                             capture_output=True, text=True).stdout)
    for viejo in F.glob("seg_*.png"):
        viejo.unlink()
    escala = "scale=960:-1" if fmt == "h" else "scale=540:-1"
    n, t = 0, 1
    while t < d:
        subprocess.run(["ffmpeg", "-y", "-v", "error", "-ss", str(t), "-i", str(video), "-frames:v", "1",
                        "-vf", escala, str(F / f"seg_{t:03d}.png")], check=True)
        n += 1; t += 2
    ini = proyecto.leer(f"inicios_{fmt}.json", B); pal = proyecto.leer("palabras.json", B); dur = proyecto.leer("duraciones.json", B)
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
    print(n, "fotogramas;", round(d, 1), "s ->", F)


if __name__ == "__main__":
    main()
