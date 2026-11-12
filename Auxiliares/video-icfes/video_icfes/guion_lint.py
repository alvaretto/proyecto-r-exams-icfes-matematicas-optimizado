"""Revisa el guion ANTES de gastar una regeneración. Cada regla salió de un fallo medido en el video
de la llanta. Uso: python -m video_icfes.guion_lint   (exit 1 si hay errores)"""
import re, sys
from . import proyecto

VOCALES_PALABRA = {"y", "o", "a", "e", "u"}          # palabras de una letra del español
PUNT = ",.:;!?»"


def revisar(g):
    err, avisos = [], []
    claves = list(g.SEG)
    esperadas = [f"s{i}" for i in range(1, len(claves) + 1)]
    if claves != esperadas:
        err.append(f"Las escenas deben llamarse {esperadas}, no {claves}")
    for k, frases in g.SEG.items():
        for i, fr in enumerate(frases):
            if len(fr) != 4:
                err.append(f"{k}[{i}]: cada frase es (texto, velocidad, tono, pausa)"); continue
            texto = fr[0]
            donde = f"{k}[{i}] «{texto[:50]}»"
            if re.search(r"\d", texto):
                err.append(f"{donde}: hay cifras; la voz las lee mal y los subtítulos no coinciden. Escribir los números en letras.")
            if re.search(r"\b[Oo]pci[oó]n\s+[A-D]\b|\b(la|letra)\s+[A-D]\b(?!\s*[a-záéíóú])", texto):
                err.append(f"{donde}: nombra una opción por su letra. La respuesta se identifica por su contenido.")
            if re.search(r"\b(primera|segunda|tercera|cuarta|última)\s+opci[oó]n\b", texto, re.I):
                err.append(f"{donde}: nombra una opción por su posición.")
            toks = texto.split()
            for j, t in enumerate(toks):
                nucleo = t.strip("¡!¿?«»\"'()" + PUNT)
                if len(nucleo) == 1 and nucleo.isalpha() and nucleo.lower() not in VOCALES_PALABRA:
                    if j + 1 < len(toks) and t[-1] not in PUNT:
                        avisos.append(f"{donde}: la letra «{nucleo}» va sin coma después; la voz puede pegarla a la "
                                      f"palabra siguiente (medido: «la P abajo»). Valorar «{nucleo},».")
            if re.search(r"[A-Za-z]\s*[=+×÷/^]\s*[A-Za-z0-9]", texto):
                err.append(f"{donde}: hay símbolos matemáticos; escribirlos como se dicen (sobre, por, igual a).")
            for ab, ci in (("¿", "?"), ("¡", "!")):
                if texto.count(ab) != texto.count(ci):
                    err.append(f"{donde}: tiene {texto.count(ab)} «{ab}» y {texto.count(ci)} «{ci}»; los subtítulos copian "
                               f"los signos del guion, así que cada pregunta o exclamación lleva los dos.")
            if not 0 <= fr[3] <= 3:
                err.append(f"{donde}: pausa fuera de 0 a 3 s")
            if len(toks) > 28:
                avisos.append(f"{donde}: frase de {len(toks)} palabras; partirla da más control de pausas")
    # pausa tras cada descarte (lección G3): se declara qué frases cierran un descarte
    for k, i in getattr(g, "CIERRES_DE_DESCARTE", []):
        if g.SEG[k][i][3] < 0.9:
            err.append(f"{k}[{i}]: cierra un descarte y su pausa es {g.SEG[k][i][3]} s; debe ser de 1 s para que se asimile")
    if not hasattr(g, "CIERRES_DE_DESCARTE"):
        avisos.append("El guion no declara CIERRES_DE_DESCARTE; no se puede comprobar la pausa de 1 s tras cada descarte")
    return err, avisos


def main():
    err, avisos = revisar(proyecto.guion())
    for a in avisos: print("AVISO", a)
    for e in err: print("ERROR", e)
    print(f"guion: {len(err)} errores, {len(avisos)} avisos")
    sys.exit(1 if err else 0)


if __name__ == "__main__":
    main()
