"""Comprueba, antes de renderizar, que cada self.en("palabra", n) de escenas.py existe en la narración."""
import re, json, sys
src = open('escenas.py').read(); pal = json.load(open('audio/palabras.json'))
norm = lambda x: x.lower().strip("¡!¿?.,:;")
malas = 0
for m in re.finditer(r'def (s\d)_.*?(?=\n    def |\Z)', src, re.S):
    k, cuerpo = m.group(1), m.group(0)
    ws = [norm(w[0]) for w in pal[k]]
    pares = [(c.group(1), int(c.group(2) or 1)) for c in re.finditer(r'self\.en\("([^"]+)"(?:, (\d+))?', cuerpo)]
    pares += [(a, int(b)) for a, b in re.findall(r'\("(\w+)", (\d)\)', cuerpo)]
    pares += [(a, 1) for a in re.findall(r'corrida\([^)]*"(\w+)"', cuerpo)]
    for palabra, n in pares:
        if ws.count(palabra.lower()) < n:
            malas += 1; print("FALTA", k, palabra, n)
print("en() sin palabra:", malas); sys.exit(1 if malas else 0)
