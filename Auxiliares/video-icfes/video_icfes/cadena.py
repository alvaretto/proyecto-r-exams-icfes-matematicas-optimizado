"""Cadena completa de un video, con sus compuertas. Se detiene en la primera que falle y dice por qué.

    python -m video_icfes.cadena <carpeta del video> [--formato h|v|ambos] [--etapa E] [--sin-transcripcion]

Etapas (--etapa):
    revisar   guion + voz + palabras ancla (escenas y música). No renderiza. ~1 min.
    borrador  lo anterior + video a baja resolución y sin sonido + fotogramas. Para ajustar los dibujos.
    todo      (por defecto) video final de cada formato, medidas de audio y material de revisión.
    congelar  copia los programas comunes usados a <carpeta>/comun_congelado/ (tras la aprobación final).

La carpeta del video es <ejercicio>/salida/<algo>/ y contiene guion.py y escenas.py.
Las etapas borrador y todo exigen las paradas «guion» y «plan_escenas» aprobadas (ver estado.py).
Códigos de salida: 0 bien · 1 compuerta fallida · 2 uso incorrecto."""
import argparse, datetime, json, os, pathlib, shutil, subprocess, sys, time

from . import estado, guion_lint, musica, proyecto

PY = sys.executable
PAQUETE = pathlib.Path(__file__).resolve().parent


class Compuerta(Exception):
    pass


def paso(txt):
    print(f"\n=== {txt} ===", flush=True)


def correr(cmd, env, cwd, que):
    t = time.time()
    r = subprocess.run(cmd, env=env, cwd=cwd, capture_output=True, text=True)
    cola = (r.stdout[-1500:] + "\n" + r.stderr[-2500:]).strip()
    if r.returncode != 0:
        raise Compuerta(f"{que} falló (exit {r.returncode}):\n{cola}")
    print(f"{que}: bien ({time.time() - t:.0f} s)", flush=True)
    return r


def ejercicio_de(d):
    for p in d.parents:
        if (p / "ITEM_OFICIAL.md").exists():
            return p
    raise Compuerta(f"Ninguna carpeta por encima de {d} tiene ITEM_OFICIAL.md: el video solo se hace de un ejercicio aprobado.")


def comprobar_ejercicio(d):
    """Compuerta 1: ejercicio aprobado y .Rmd con el md5 firmado. Devuelve {ruta: md5} de los .Rmd."""
    ej = ejercicio_de(d)
    rmds = sorted(ej.rglob("*.Rmd"))
    rmds = [r for r in rmds if "salida" not in r.relative_to(ej).parts]
    if not rmds:
        raise Compuerta(f"{ej} no tiene ningún .Rmd")
    estado = ej / "ejercicio_state.json"
    if not estado.exists():
        raise Compuerta(f"{ej} no tiene ejercicio_state.json")
    ap = json.loads(estado.read_text()).get("pasos", {}).get("aprobacion_usuario", {})
    if not ap.get("completado"):
        raise Compuerta(f"El ejercicio {ej.name} no tiene la aprobación del profesor (paso 11). El video se hace después.")
    md5s = {str(r.relative_to(ej)): proyecto.md5(r) for r in rmds}
    firmado = ap.get("rmd_md5")
    if firmado and firmado not in md5s.values():
        raise Compuerta(f"Ningún .Rmd de {ej.name} tiene el md5 firmado ({firmado}). Hay: {md5s}")
    return ej, md5s


def anclas_por_codigo(d, env, fmt):
    """Recorre las escenas sin dibujar (segundos) y junta TODAS las palabras ancla que falten."""
    e = dict(env, VIDEO_FORMATO=fmt, VIDEO_SOLO_ANCLAS="1")
    correr([PY, "-m", "manim", "render", "--dry_run", "--disable_caching", "--media_dir", str(d / "media_anclas"),
            "escenas.py", "Video"], e, d, f"recorrido de anclas ({fmt})")
    a = proyecto.leer(f"anclas_{fmt}.json", d)
    return a["faltan"], len(a["usadas"])


def video_de_manim(d, fmt, borrador):
    base = d / (f"media_{fmt}" + ("_borrador" if borrador else ""))
    vs = sorted(base.rglob("Video.mp4"), key=lambda p: p.stat().st_mtime)
    if not vs:
        raise Compuerta(f"Manim no dejó Video.mp4 en {base}")
    return vs[-1]


def duracion(v):
    return float(subprocess.run(["ffprobe", "-v", "error", "-show_entries", "format=duration", "-of", "csv=p=0", str(v)],
                                capture_output=True, text=True).stdout)


def congelar(d):
    dest = d / "comun_congelado"
    if dest.exists():
        shutil.rmtree(dest)
    shutil.copytree(PAQUETE, dest / "video_icfes", ignore=shutil.ignore_patterns("__pycache__"))
    (dest / "LEEME.txt").write_text(
        f"Copia de los programas comunes con los que se aprobó este video ({datetime.datetime.now():%Y-%m-%d %H:%M}).\n"
        "Para regenerarlo tal como se aprobó, desde esta carpeta:\n"
        "    PYTHONPATH=. ~/.venvs/manim/bin/python -m video_icfes.cadena ..\n")
    print("Programas comunes congelados en", dest)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("carpeta"); ap.add_argument("--formato", default="ambos", choices=["h", "v", "ambos"])
    ap.add_argument("--etapa", default="todo", choices=["revisar", "borrador", "todo", "congelar"])
    ap.add_argument("--sin-transcripcion", action="store_true")
    a = ap.parse_args()
    d = pathlib.Path(a.carpeta).resolve()
    if not (d / "guion.py").exists() or not (d / "escenas.py").exists():
        print(f"{d} debe contener guion.py y escenas.py (copie la plantilla)"); sys.exit(2)
    fmts = ["h", "v"] if a.formato == "ambos" else [a.formato]
    env = dict(os.environ, VIDEO_DIR=str(d), PYTHONPATH=str(PAQUETE.parent) + os.pathsep + os.environ.get("PYTHONPATH", ""))
    os.environ["VIDEO_DIR"] = str(d)
    informe = {"carpeta": str(d), "formatos": fmts, "etapa": a.etapa, "inicio": f"{datetime.datetime.now():%Y-%m-%d %H:%M:%S}"}
    try:
        paso("1. Ejercicio aprobado y .Rmd firmado")
        ej, md5_antes = comprobar_ejercicio(d)
        print(ej.name, md5_antes)
        try:
            if a.etapa == "congelar":
                estado.exigir(d, "version_final", "congelar los programas comunes")
                congelar(d); return
            if a.etapa in ("borrador", "todo"):
                estado.exigir(d, "guion", "dibujar el video")
                estado.exigir(d, "plan_escenas", "dibujar el video")
        except PermissionError as e:
            raise Compuerta(str(e))

        paso("2. Guion")
        g = proyecto.guion(d)
        err, avisos = guion_lint.revisar(g)
        for x in avisos: print("AVISO", x)
        informe["avisos_guion"] = avisos
        if err:
            raise Compuerta("El guion tiene errores:\n" + "\n".join(err))

        paso("3. Voz (necesita internet)")
        correr([PY, "-m", "video_icfes.voz"], env, d, "voz")
        pal = proyecto.leer("palabras.json", d)

        paso("4. Palabras ancla de escenas y de música")
        faltan = musica.anclas_faltantes(g, pal)
        for f in fmts:
            fa, n = anclas_por_codigo(d, env, f)
            print(f"formato {f}: {n} anclas en escenas, {len(fa)} ausentes")
            faltan += [f"[{f}] {x}" for x in fa]
        if faltan:
            raise Compuerta("\n".join(dict.fromkeys(faltan)))
        if a.etapa == "revisar":
            print("\nRevisión terminada: guion, voz y anclas bien."); return

        borr = a.etapa == "borrador"
        (d / "copias").mkdir(exist_ok=True)
        for f in fmts:
            e = dict(env, VIDEO_FORMATO=f, **({"VIDEO_BORRADOR": "1"} if borr else {}))
            paso(f"5. Dibujar el video ({'borrador ' if borr else ''}{f})")
            media = d / (f"media_{f}" + ("_borrador" if borr else ""))
            correr([PY, "-m", "manim", "render", "--disable_caching", "--media_dir", str(media), "escenas.py", "Video"],
                   e, d, f"render {f}")
            v = video_de_manim(d, f, borr)
            atrasos = proyecto.leer(f"anclas_{f}.json", d)["atrasos"]
            informe[f"atrasos_{f}"] = atrasos
            if atrasos:
                print("AVISO: animaciones que llegan tarde a su palabra (escena, palabra, vez, segundos):", atrasos)
            if borr:
                F = d / f"borrador_{f}"; F.mkdir(exist_ok=True)
                for viejo in F.glob("seg_*.png"): viejo.unlink()
                subprocess.run(["ffmpeg", "-y", "-v", "error", "-i", str(v), "-vf", "fps=1/2", "-start_number", "0",
                                str(F / "seg_%03d.png")], check=True)
                print(f"Borrador: {v}; un fotograma cada 2 s en {F} (seg_NNN = segundo 2·NNN+1 aprox.)")
                continue

            paso(f"6. Música ({f})")
            otra = "h" if f == "v" else "v"
            ini = proyecto.leer(f"inicios_{f}.json", d)
            mus = d / "audio" / f"musica_{f}.wav"; sello = d / "audio" / f"musica_{f}.json"
            huella = json.dumps([ini, g.MUSICA, round(duracion(v), 2), proyecto.md5(d / "audio" / "palabras.json")],
                                sort_keys=True, ensure_ascii=False)
            sello_otra = d / "audio" / f"musica_{otra}.json"
            if mus.exists() and sello.exists() and sello.read_text() == huella:
                print("música: nada cambió, se reutiliza")
            elif sello_otra.exists() and sello_otra.read_text() == huella:
                shutil.copyfile(d / "audio" / f"musica_{otra}.wav", mus); sello.write_text(huella)
                print(f"música: los tiempos son los mismos que en el formato {otra}; se copia")
            else:
                correr([PY, "-m", "video_icfes.musica", f"{duracion(v):.3f}"], e, d, "música"); sello.write_text(huella)

            paso(f"7. Montaje ({f})")
            final = d / f"{g.NOMBRE}_{f}.mp4"
            if final.exists():      # copia del anterior antes de reemplazarlo
                marca = datetime.datetime.fromtimestamp(final.stat().st_mtime).strftime("%Y%m%d_%H%M%S")
                shutil.copy2(final, d / "copias" / f"{final.stem}_{marca}.mp4")
            correr([PY, "-m", "video_icfes.montar", str(v)], e, d, "montaje")

            paso(f"8. Material de revisión y medidas de audio ({f})")
            correr([PY, "-m", "video_icfes.revision"], e, d, "fotogramas y narración con tiempos")
            cmd = [PY, "-m", "video_icfes.medidas"] + (["--sin-transcripcion"] if a.sin_transcripcion else [])
            r = subprocess.run(cmd, env=e, cwd=d, capture_output=True, text=True)
            mj = d / f"revision_{f}" / "audio_medidas.json"
            if not mj.exists() or r.returncode not in (0, 1):
                raise Compuerta("medidas de audio falló:\n" + r.stderr[-2000:])
            m = json.loads(mj.read_text()); informe[f"medidas_{f}"] = m
            print({k: m[k] for k in ("lufs", "pico_dbfs", "p10_1_3khz", "wer", "duracion")})
            malas = [k for k, ok in m["puertas"].items() if not ok]
            if malas:
                raise Compuerta(f"El audio del formato {f} no pasa: {malas}. Ver revision_{f}/audio_medidas.md")

        paso("9. El .Rmd sigue intacto")
        _, md5_despues = comprobar_ejercicio(d)
        if md5_despues != md5_antes:
            raise Compuerta(f"CAMBIÓ UN .Rmd DURANTE LA CADENA: antes {md5_antes}, después {md5_despues}")
        print("md5 sin cambios:", md5_despues)
        informe["md5_rmd"] = md5_despues
        informe["resultado"] = "bien"
    except Compuerta as e:
        informe["resultado"] = "COMPUERTA"; informe["motivo"] = str(e)
        print("\n*** LA CADENA SE DETUVO ***\n" + str(e), flush=True)
        (d / "informe_cadena.json").write_text(json.dumps(informe, ensure_ascii=False, indent=1))
        sys.exit(1)
    informe["fin"] = f"{datetime.datetime.now():%Y-%m-%d %H:%M:%S}"
    (d / "informe_cadena.json").write_text(json.dumps(informe, ensure_ascii=False, indent=1))
    print("\nCADENA TERMINADA. Informe:", d / "informe_cadena.json")
    # Los avisos se repiten al final: uno de atraso pasó inadvertido en mitad del registro (2026-11-12).
    tardes = {f: informe.get(f"atrasos_{f}") for f in fmts if informe.get(f"atrasos_{f}")}
    if tardes:
        print("*** AVISO: animaciones que llegan tarde a su palabra (escena, palabra, vez, segundos):", tardes)
        print("    Causa habitual: el guion cambió y esa palabra ahora aparece antes en la escena; revise el número de vez en self.en().")
    if informe.get("avisos_guion"):
        print(f"*** {len(informe['avisos_guion'])} avisos del guion (arriba, en el paso 2).")
    print("Sin verificar por la cadena: nadie oyó el audio; los fotogramas hay que mirarlos; faltan los revisores (python -m video_icfes.revisores).")


if __name__ == "__main__":
    main()
