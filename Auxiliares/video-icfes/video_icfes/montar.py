"""Montaje final: narración en su instante, música que cede ante la voz, y subtítulos dinámicos palabra por
palabra en una franja inferior (el cuadro se reduce al 88 % para que no tapen nada).
Deja dos archivos: <NOMBRE>_<formato>.mp4 (con subtítulos) y <NOMBRE>_<formato>_sin_subtitulos.mp4
(cuadro completo, para editar aparte).

Uso: python -m video_icfes.montar <Video.mp4 de Manim>"""
import pathlib, re, subprocess, sys
from . import proyecto

OBJETIVO_LUFS = -15.0      # sonoridad final (las plataformas de video rondan -14)
DECENAS = {"treinta", "cuarenta", "cincuenta", "sesenta", "setenta", "ochenta", "noventa"}
# por formato: cuadro reducido (ancho, alto), margen izquierdo, tamaño de letra, margen inferior, palabras por grupo
CUADRO = {"h": (1690, 950, 115, 70, 26, 3), "v": (950, 1690, 65, 62, 44, 2)}


def ts(t):
    h, r = divmod(t, 3600); m, s_ = divmod(r, 60)
    return f"{int(h)}:{int(m):02d}:{s_:05.2f}"


def subtitulos(pal, ini, fmt, ruta):
    """ASS: grupos cortos de palabras, la palabra dicha en amarillo."""
    W, H = proyecto.FORMATOS[fmt]
    _, _, _, tam, mv, maxg = CUADRO[fmt]
    ev = []
    for k, ws in pal.items():
        grupos, g = [], []
        for i, w in enumerate(ws):
            pausa = i > 0 and w[1] - (ws[i - 1][1] + ws[i - 1][2]) > 0.22
            ult = proyecto.norm(g[-1][0]) if g else ""
            # no partir «setenta y | cinco», «ciento | cincuenta», «cero coma | cinco»
            pegada = g and (ult in {"y", "ciento", "coma"} or (ult in DECENAS and proyecto.norm(w[0]) == "y"))
            if g and (pausa or (len(g) >= maxg and not pegada) or len(g) >= maxg + 2):
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
    ruta.write_text(f"""[Script Info]
ScriptType: v4.00+
PlayResX: {W}
PlayResY: {H}

[V4+ Styles]
Format: Name, Fontname, Fontsize, PrimaryColour, OutlineColour, BackColour, Bold, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV
Style: S,Noto Sans,{tam},&H00FFFFFF,&H00000000,&H00000000,-1,1,4,0,2,40,40,{mv}

[Events]
Format: Layer, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text
""" + "\n".join(lineas) + "\n", encoding="utf-8")


def comprobar_signos(seg, ruta):
    """Lee el .ass YA ESCRITO y exige que cada palabra del guion con ¿ ¡ ? ! esté en él con su signo."""
    import re
    vistos = set()
    for l in ruta.read_text(encoding="utf-8").splitlines():
        if l.startswith("Dialogue:"):
            vistos.update(re.sub(r"\{[^}]*\}", "", l.split(",", 9)[9]).split())
    faltan = [f"{k}[{i}] «{t}»" for k, frases in seg.items() for i, fr in enumerate(frases)
              for t in fr[0].split() if set(t) & set("¿¡?!") and t.upper() not in vistos]
    assert not faltan, f"SIGNOS: faltan en {ruta.name}: " + "; ".join(faltan)


def main():
    B = proyecto.carpeta(); fmt = proyecto.formato(); g = proyecto.guion(B)
    video = pathlib.Path(sys.argv[1])
    ini = proyecto.leer(f"inicios_{fmt}.json", B)
    dur = proyecto.leer("duraciones.json", B)
    pal = proyecto.leer("palabras.json", B)
    vol = getattr(g, "VOL_MUSICA", 0.36)       # perilla del profesor: se ajusta oyendo, no por programa
    salida = B / f"{g.NOMBRE}_{fmt}.mp4"
    limpio = B / f"{g.NOMBRE}_{fmt}_sin_subtitulos.mp4"
    ass = B / "audio" / f"subtitulos_{fmt}.ass"
    subtitulos(proyecto.con_signos(g.SEG, pal), ini, fmt, ass)
    comprobar_signos(g.SEG, ass)

    # ---------- audio en tres pasos: pistas separadas -> medir sonoridad -> mezcla final ----------
    ST = B / "audio" / f"stems_{fmt}"; ST.mkdir(exist_ok=True)
    cmd = ["ffmpeg", "-y", "-v", "error"]
    filtros = []
    for i, (k, t) in enumerate(ini.items()):
        cmd += ["-i", str(B / f"audio/{k}.mp3")]
        filtros.append(f"[{i}:a]atrim=0:{dur[k]},aresample=48000,adelay={int(t * 1000)}:all=1[a{i}]")
    n = len(ini)
    cmd += ["-i", str(B / f"audio/musica_{fmt}.wav")]
    filtros.append("".join(f"[a{i}]" for i in range(n)) + f"amix=inputs={n}:normalize=0,asplit=3[voz][llave][vozm]")
    # hueco en 1-3 kHz: ahí están las consonantes de la voz (medido: sin él la música la igualaba en el 10 % peor)
    filtros.append(f"[{n}:a]aresample=48000,volume={vol},equalizer=f=1750:t=o:w=1.6:g=-8[mus]")
    # la música cede poco y despacio: se sigue oyendo bajo la voz y no «bombea» en cada pausa
    filtros.append("[mus][llave]sidechaincompress=threshold=0.03:ratio=2.5:attack=20:release=900,asplit=2[musd][musm]")
    filtros.append("[vozm][musm]amix=inputs=2:normalize=0:duration=longest[pre]")
    cmd += ["-filter_complex", ";".join(filtros), "-map", "[voz]", str(ST / "voz.wav"), "-map", "[musd]",
            str(ST / "musica_final.wav"), "-map", "[pre]", str(ST / "premezcla.wav")]
    r = subprocess.run(cmd, capture_output=True, text=True)
    assert r.returncode == 0, r.stderr[-800:]
    e = subprocess.run(["ffmpeg", "-hide_banner", "-i", str(ST / "premezcla.wav"), "-af", "ebur128", "-f", "null", "-"],
                       capture_output=True, text=True).stderr
    lufs = float(re.findall(r"I:\s+(-?[\d.]+) LUFS", e)[-1])
    gan = OBJETIVO_LUFS - lufs
    print(f"premezcla {lufs} LUFS; ganancia {gan:+.1f} dB; volumen de música {vol}")
    mezcla = f"[1:a]volume={gan}dB,alimiter=limit=0.8:attack=3:release=60:level=disabled[mix]"
    audio = ["-c:a", "aac", "-b:a", "192k", "-shortest", "-movflags", "+faststart"]
    # ---------- con subtítulos: cuadro al 88 % arriba, franja inferior del color del fondo ----------
    W, H = proyecto.FORMATOS[fmt]; cw, ch, cx = CUADRO[fmt][:3]
    subprocess.run(["ffmpeg", "-y", "-v", "error", "-i", str(video), "-i", str(ST / "premezcla.wav"), "-filter_complex",
                    f"[0:v]scale={cw}:{ch},pad={W}:{H}:{cx}:0:color=0x0f172a,ass={ass.name}[v];" + mezcla,
                    "-map", "[v]", "-map", "[mix]", "-c:v", "libx264", "-crf", "18", "-preset", "medium",
                    "-pix_fmt", "yuv420p", *audio, str(salida)], check=True, cwd=ass.parent)
    # ---------- sin subtítulos: cuadro completo, mismo audio ----------
    subprocess.run(["ffmpeg", "-y", "-v", "error", "-i", str(video), "-i", str(ST / "premezcla.wav"), "-filter_complex",
                    mezcla, "-map", "0:v", "-map", "[mix]", "-c:v", "copy", *audio, str(limpio)], check=True)
    print(salida); print(limpio)


if __name__ == "__main__":
    main()
