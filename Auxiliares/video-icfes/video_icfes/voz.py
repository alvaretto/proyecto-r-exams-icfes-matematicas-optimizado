"""Narración por FRASES (edge-tts; necesita internet SOLO al generar): cada frase lleva su velocidad,
su tono y la pausa que la sigue. Guarda por escena el audio, los tiempos de cada palabra y la duración.

Uso: python -m video_icfes.voz   (lee guion.py de VIDEO_DIR; no regenera si el guion no cambió)"""
import asyncio, hashlib, json, subprocess, sys
import edge_tts
from . import proyecto


async def frase(texto, voz, vel, tono, ruta):
    com = edge_tts.Communicate(texto, voz, rate=f"{vel:+d}%", pitch=f"{tono:+d}Hz", boundary="WordBoundary")
    ws = []
    with open(ruta, "wb") as f:
        async for ch in com.stream():
            if ch["type"] == "audio":
                f.write(ch["data"])
            elif ch["type"] == "WordBoundary":
                ws.append([ch["text"], ch["offset"] / 1e7, ch["duration"] / 1e7])
    return ws


async def generar(d, g):
    aqui = d / "audio"; fr = aqui / "frases"; fr.mkdir(parents=True, exist_ok=True)
    entrada = getattr(g, "ENTRADA", {})
    dur, palabras = {}, {}
    for k, frases in g.SEG.items():
        pos = entrada.get(k, 0.0)
        cmd, filtros, todas = ["ffmpeg", "-y", "-v", "error"], [], []
        for i, (texto, vel, tono, pausa) in enumerate(frases):
            ruta = fr / f"{k}_{i:02d}.mp3"
            ws = await frase(texto, g.VOZ, vel, tono, ruta)
            assert ws, f"La voz no devolvió palabras para la frase {k}[{i}]: {texto!r}"
            a = max(0.0, ws[0][1] - 0.04)                    # se recorta el silencio de entrada y de cola
            b = ws[-1][1] + ws[-1][2] + 0.10
            cmd += ["-i", str(ruta)]
            filtros.append(f"[{i}:a]atrim=start={a:.3f}:end={b:.3f},asetpts=PTS-STARTPTS,"
                           f"aresample=48000,adelay={int(pos * 1000)}:all=1[f{i}]")
            todas += [[w[0], round(pos + w[1] - a, 3), round(w[2], 3)] for w in ws]
            pos += (b - a) + pausa
        n = len(frases)
        filtros.append("".join(f"[f{i}]" for i in range(n)) + f"amix=inputs={n}:normalize=0[o]")
        cmd += ["-filter_complex", ";".join(filtros), "-map", "[o]", "-c:a", "libmp3lame", "-b:a", "160k",
                str(aqui / f"{k}.mp3")]
        # ffmpeg avisa «non monotonically increasing dts» con estos mp3: medido inocuo, se calla
        r = subprocess.run(cmd, capture_output=True, text=True)
        assert r.returncode == 0, r.stderr[-800:]
        palabras[k] = todas
        dur[k] = round(todas[-1][1] + todas[-1][2] + 0.25, 3)
    proyecto.escribir("duraciones.json", dur, d)
    (aqui / "palabras.json").write_text(json.dumps(palabras, ensure_ascii=False))
    return dur


def huella(g):
    dato = json.dumps([g.VOZ, getattr(g, "ENTRADA", {}), g.SEG], ensure_ascii=False, sort_keys=True)
    return hashlib.sha256(dato.encode()).hexdigest()


def main():
    d = proyecto.carpeta(); g = proyecto.guion(d)
    sello = d / "audio" / "voz.sha256"
    h = huella(g)
    if sello.exists() and sello.read_text() == h and (d / "audio" / "palabras.json").exists() and "--forzar" not in sys.argv:
        print("voz: el guion no cambió, se reutiliza el audio"); return
    dur = asyncio.run(generar(d, g))
    sello.write_text(h)
    print("voz:", dur, "total", round(sum(dur.values()), 1), "s")


if __name__ == "__main__":
    main()
