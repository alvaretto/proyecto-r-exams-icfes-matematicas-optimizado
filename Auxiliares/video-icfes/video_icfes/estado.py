"""Las cuatro paradas en las que el profesor aprueba, guardadas en <carpeta>/estado_video.json.

    python -m video_icfes.estado <carpeta>                      muestra en qué va el video
    python -m video_icfes.estado <carpeta> aprobar <parada> "<lo que dijo el profesor>"

Paradas, en orden: guion · plan_escenas · primera_version · version_final.
Solo se registra una aprobación que el profesor haya dado en la conversación; nunca por adelantado."""
import datetime, json, pathlib, sys

PARADAS = ["guion", "plan_escenas", "primera_version", "version_final"]
QUE = {"guion": "el profesor lee guion.py (lo que dice la voz)",
       "plan_escenas": "el profesor lee plan_escenas.md (qué se dibuja en cada escena)",
       "primera_version": "el profesor ve y OYE el primer video, antes del adversario",
       "version_final": "el profesor aprueba el video después de las correcciones del adversario"}


def leer(d):
    p = pathlib.Path(d) / "estado_video.json"
    return json.loads(p.read_text()) if p.exists() else {}


def aprobada(d, parada):
    return parada in leer(d)


def exigir(d, parada, para):
    if not aprobada(d, parada):
        raise PermissionError(f"Falta la aprobación del profesor en la parada «{parada}» ({QUE[parada]}) antes de {para}.")


def main():
    d = pathlib.Path(sys.argv[1]).resolve()
    e = leer(d)
    if len(sys.argv) >= 4 and sys.argv[2] == "aprobar":
        parada = sys.argv[3]
        assert parada in PARADAS, f"Parada desconocida: {parada}. Son {PARADAS}"
        previas = [p for p in PARADAS[:PARADAS.index(parada)] if p not in e]
        assert not previas, f"Antes hay que aprobar: {previas}"
        assert len(sys.argv) >= 5 and sys.argv[4].strip(), "Falta citar lo que dijo el profesor"
        e[parada] = {"fecha": f"{datetime.datetime.now():%Y-%m-%d %H:%M}", "dijo": sys.argv[4]}
        (d / "estado_video.json").write_text(json.dumps(e, ensure_ascii=False, indent=1))
    for p in PARADAS:
        print(("APROBADA  " if p in e else "pendiente ") + p + (f"  ({e[p]['fecha']}: {e[p]['dijo']})" if p in e else f"  -> {QUE[p]}"))
    sig = next((p for p in PARADAS if p not in e), None)
    print("Siguiente parada:", sig or "ninguna; el video está aprobado")


if __name__ == "__main__":
    main()
