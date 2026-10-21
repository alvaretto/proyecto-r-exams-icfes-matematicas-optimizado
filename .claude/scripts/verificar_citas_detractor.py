#!/usr/bin/env python3
"""Verifica que las citas de un reporte de detractor existan de verdad en los archivos (regla #9 v1.6).

Origen: el detractor llegó a «citar» código reconstruido de memoria en vez del archivo real
(memoria feedback_detractor_alucina_codigo, caso 2026-06-16: campos y pools que no existían).
Desde la v1.6 todo reporte con VEREDICTO_DETRACTOR trae, antes de esa línea, un bloque
legible por máquina, y este script lo contrasta con los archivos:

    CITAS_DETRACTOR
    H1 | /ruta/absoluta/archivo.html:270-271 | fragmento copiado tal cual del archivo
    H2 | ruta/relativa/al/repo.R:88 | otro fragmento literal
    H3 | /ruta/archivo.R | AUSENTE: texto que se afirma que NO está en el archivo
    H4 | SIN_ARCHIVO | razón (hallazgo que solo se apoya en una fuente externa)
    FIN_CITAS_DETRACTOR

Reglas que se comprueban:
  - El archivo existe y el rango de líneas cabe en él.
  - El fragmento (>= 15 caracteres, comparación sin distinguir espacios ni entidades HTML)
    está en esas líneas, con 2 de holgura. Si está en otra parte, falla y dice dónde.
  - AUSENTE: el texto no aparece en ninguna parte del archivo.
  - Todo hallazgo con ID (filas de tabla «| H1 |», encabezados o negritas «H1:») tiene al
    menos una cita verificada o un SIN_ARCHIVO. SIN_ARCHIVO no falla, pero se avisa.
  - Si el reporte tiene hallazgos, el bloque es obligatorio. Sin hallazgos: «NINGUNA».

Uso:
  verificar_citas_detractor.py reporte.md [--repo RAIZ]   # exit 0 si todo verifica, 1 si no
  verificar_citas_detractor.py --hook                     # hook SubagentStop (JSON por stdin)

En modo --hook: si el último mensaje del subagente trae VEREDICTO_DETRACTOR y alguna cita no
verifica, responde {"decision": "block", "reason": …}: el subagente NO termina y recibe la
lista de citas a corregir. Tope: 3 bloqueos por agente (luego deja terminar y lo registra).
Estado y registro en $CITAS_DETRACTOR_ESTADO (por defecto ~/.claude/hooks/logs).
"""
import hashlib
import html
import json
import os
import pathlib
import re
import subprocess
import sys
import time

MARCADOR = 'VEREDICTO_DETRACTOR:'
INICIO, FIN = 'CITAS_DETRACTOR', 'FIN_CITAS_DETRACTOR'
MIN_FRAGMENTO = 15
HOLGURA = 2
TOPE_BLOQUEOS = 3
ID_RE = r'[A-Z]{1,3}-?\d{1,3}'
FILA_TABLA = re.compile(r'^\|\s*\**(' + ID_RE + r')\**\s*\|', re.M)
ETIQUETA = re.compile(r'^(?:#{2,5}\s+|\*\*|- \*\*)\s*(' + ID_RE + r')\b\s*\**\s*[:—–|]', re.M)
CITA = re.compile(r'^\s*(' + ID_RE + r')\s*\|\s*(.+?)\s*\|\s*(.+?)\s*$')


def normalizar(texto):
    texto = html.unescape(texto).replace(' ', ' ')
    return re.sub(r'\s+', ' ', texto).strip()


def limpiar_fragmento(f):
    f = f.strip()
    for a, b in (('«', '»'), ('`', '`'), ('"', '"'), ('“', '”')):
        if len(f) > 2 and f.startswith(a) and f.endswith(b):
            f = f[len(a):-len(b)].strip()
    return f


def raiz_repo(desde):
    try:
        r = subprocess.run(['git', '-C', str(desde), 'rev-parse', '--show-toplevel'],
                           capture_output=True, text=True, timeout=10)
        if r.returncode == 0 and r.stdout.strip():
            return pathlib.Path(r.stdout.strip())
    except Exception:
        pass
    return pathlib.Path(desde)


def resolver(ruta, repo):
    p = pathlib.Path(os.path.expanduser(ruta))
    if p.is_absolute():
        return p
    for base in (repo, pathlib.Path.cwd()):
        if (base / p).exists():
            return base / p
    return repo / p


def verificar(reporte, repo):
    """Devuelve (fallos, avisos, n_citas). fallos y avisos son listas de textos."""
    fallos, avisos = [], []
    ids_hallazgos = set(FILA_TABLA.findall(reporte)) | set(ETIQUETA.findall(reporte))
    ids_hallazgos.discard('ID')
    m = re.search(r'^\s*' + INICIO + r'\s*$(.*?)^\s*' + FIN + r'\s*$', reporte, re.M | re.S)
    if not m:
        if ids_hallazgos:
            fallos.append(f'falta el bloque {INICIO} … {FIN} (el reporte tiene los hallazgos '
                          f'{", ".join(sorted(ids_hallazgos))})')
        elif re.search(r'Objeci[oó]n|Hallazgo', reporte):
            fallos.append(f'falta el bloque {INICIO} … {FIN} (si no hay hallazgos, escribe NINGUNA dentro)')
        return fallos, avisos, 0
    lineas = [l for l in m.group(1).splitlines() if l.strip()]
    if len(lineas) == 1 and lineas[0].strip().upper() == 'NINGUNA':
        if ids_hallazgos:
            fallos.append(f'el bloque dice NINGUNA pero hay hallazgos: {", ".join(sorted(ids_hallazgos))}')
        return fallos, avisos, 0

    cubiertos = set()
    cache = {}
    n = 0
    for linea in lineas:
        c = CITA.match(linea)
        if not c:
            fallos.append(f'línea del bloque con formato inválido (se espera «ID | ruta:línea | fragmento»): {linea.strip()[:120]}')
            continue
        n += 1
        cid, ubic, frag = c.group(1), c.group(2).strip(), limpiar_fragmento(c.group(3))
        etiqueta = f'{cid} | {ubic}'
        if ubic.upper() == 'SIN_ARCHIVO':
            avisos.append(f'{cid}: sin cita de archivo ({frag[:100]})')
            cubiertos.add(cid)
            continue
        mr = re.match(r'^(.*?)(?::(\d+)(?:-(\d+))?)?$', ubic)
        ruta, ini, fin = mr.group(1), mr.group(2), mr.group(3)
        archivo = resolver(ruta, repo)
        if not archivo.is_file():
            fallos.append(f'{etiqueta}: el archivo no existe ({archivo})')
            continue
        if archivo not in cache:
            try:
                cache[archivo] = archivo.read_text(encoding='utf-8', errors='replace').splitlines()
            except Exception as e:
                fallos.append(f'{etiqueta}: no se pudo leer ({e})')
                continue
        texto = cache[archivo]
        ausente = re.match(r'^AUSENTE:\s*(.+)$', frag, re.I)
        if ausente:
            buscado = normalizar(limpiar_fragmento(ausente.group(1)))
            if len(buscado) < 4:
                fallos.append(f'{etiqueta}: AUSENTE necesita un texto de al menos 4 caracteres')
            elif buscado in normalizar('\n'.join(texto)):
                fallos.append(f'{etiqueta}: se afirma AUSENTE «{buscado[:80]}», pero sí está en el archivo')
            else:
                cubiertos.add(cid)
            continue
        f = normalizar(frag)
        if len(f) < MIN_FRAGMENTO:
            fallos.append(f'{etiqueta}: fragmento demasiado corto ({len(f)} caracteres; mínimo {MIN_FRAGMENTO}): «{f}»')
            continue
        if not ini:
            fallos.append(f'{etiqueta}: falta el número de línea (ruta:línea o ruta:inicio-fin)')
            continue
        a, b = int(ini), int(fin or ini)
        if a < 1 or b < a or a > len(texto):
            fallos.append(f'{etiqueta}: rango de líneas inválido (el archivo tiene {len(texto)} líneas)')
            continue
        zona = normalizar(' '.join(texto[max(0, a - 1 - HOLGURA):min(len(texto), b + HOLGURA)]))
        if f in zona:
            cubiertos.add(cid)
            continue
        donde = [i + 1 for i in range(len(texto)) if f in normalizar(texto[i])]
        if not donde:
            ancho = max(2, b - a + 1)
            donde = [i + 1 for i in range(len(texto)) if f in normalizar(' '.join(texto[i:i + ancho]))
                     and f not in normalizar(' '.join(texto[i + 1:i + 1 + ancho]))]
        if donde:
            fallos.append(f'{etiqueta}: el fragmento no está en esas líneas; sí aparece desde la(s) línea(s) '
                          f'{", ".join(map(str, donde[:5]))}: «{f[:80]}»')
        else:
            parecido = [i + 1 for i in range(len(texto)) if f[:25] in normalizar(texto[i])]
            pista = (f' (lo más parecido empieza en la línea {parecido[0]}: cópialo tal cual)' if parecido else '')
            fallos.append(f'{etiqueta}: el fragmento NO está en el archivo: «{f[:100]}»{pista}')

    for hid in sorted(ids_hallazgos - cubiertos):
        fallos.append(f'el hallazgo {hid} no tiene ninguna cita verificada en el bloque')
    return fallos, avisos, n


def formato_bloque():
    return (f'{INICIO}\n'
            'H1 | /ruta/absoluta/archivo:120-121 | fragmento copiado tal cual del archivo (mín. 15 caracteres)\n'
            'H2 | /ruta/absoluta/archivo | AUSENTE: texto que afirmas que no está\n'
            'H3 | SIN_ARCHIVO | razón (solo fuente externa)\n'
            f'{FIN}\n'
            'VEREDICTO_DETRACTOR: …')


def modo_hook():
    try:
        datos = json.load(sys.stdin)
    except Exception:
        return 0  # Entrada ilegible: no se bloquea (falla abierto, como pre-write-rmd-gate).
    mensaje = datos.get('last_assistant_message') or ''
    if not mensaje and datos.get('agent_transcript_path'):
        try:
            for raw in reversed(pathlib.Path(os.path.expanduser(datos['agent_transcript_path'])).read_text().splitlines()):
                d = json.loads(raw)
                if d.get('type') == 'assistant':
                    partes = d.get('message', {}).get('content', [])
                    mensaje = ''.join(p.get('text', '') for p in partes if isinstance(p, dict))
                    if mensaje:
                        break
        except Exception:
            mensaje = ''
    if MARCADOR not in mensaje:
        return 0
    repo = raiz_repo(datos.get('cwd') or os.getcwd())
    estado = pathlib.Path(os.path.expanduser(os.environ.get('CITAS_DETRACTOR_ESTADO', '~/.claude/hooks/logs')))
    estado.mkdir(parents=True, exist_ok=True)
    agente = re.sub(r'[^A-Za-z0-9_-]', '', str(datos.get('agent_id') or 'sin-id'))[:64] or 'sin-id'
    contador = estado / f'citas-detractor-{agente}.count'
    registro = estado / 'citas-detractor.log'
    # El hook vive en dos sitios: el settings.json del repo (solo se carga si la sesión arranca en la raíz)
    # y ~/.claude/settings.json (se carga siempre). Si corren los dos sobre el mismo mensaje, Claude Code
    # los lanza en paralelo: el primero que crea el candado evalúa y el otro sale sin hacer nada, para no
    # gastar dos bloqueos por intento. El candado dura VENTANA segundos (3 por defecto): las dos copias
    # corren en el mismo instante, y reescribir un reporte le toma al modelo bastante más, así que un
    # reporte reenviado sin corregir se vuelve a evaluar.
    ventana = float(os.environ.get('CITAS_DETRACTOR_VENTANA', '3'))
    clave = hashlib.sha1((agente + '\0' + mensaje).encode('utf-8', 'replace')).hexdigest()[:16]
    candado = estado / f'citas-detractor-{agente}-{clave}.lock'
    try:
        os.close(os.open(candado, os.O_CREAT | os.O_EXCL | os.O_WRONLY))
    except FileExistsError:
        try:
            if time.time() - candado.stat().st_mtime < ventana:
                return 0
            candado.touch()
        except OSError:
            return 0
    for viejo in estado.glob('citas-detractor-*.lock'):
        try:
            if time.time() - viejo.stat().st_mtime > 86400:
                viejo.unlink()
        except OSError:
            pass
    try:
        fallos, avisos, n = verificar(mensaje, repo)
    except Exception as e:  # Un error del verificador no debe dejar al agente en un bucle.
        fallos, avisos, n = [], [f'error interno del verificador: {e}'], 0
    previos = int(contador.read_text()) if contador.exists() else 0
    linea = f'{time.strftime("%Y-%m-%d %H:%M:%S")} agente={agente} tipo={datos.get("agent_type")} citas={n} fallos={len(fallos)} bloqueos_previos={previos}'
    if not fallos:
        with registro.open('a') as r:
            r.write(linea + ' → ENTREGA VERIFICADA' + (f' avisos={len(avisos)}' if avisos else '') + '\n')
        contador.unlink(missing_ok=True)
        return 0
    if previos >= TOPE_BLOQUEOS:
        with registro.open('a') as r:
            r.write(linea + ' → TOPE ALCANZADO: se entrega CON CITAS NO VERIFICADAS:\n'
                    + ''.join(f'    - {x}\n' for x in fallos))
        return 0
    contador.write_text(str(previos + 1))
    with registro.open('a') as r:
        r.write(linea + f' → BLOQUEADO ({previos + 1}/{TOPE_BLOQUEOS})\n')
    razon = ('VERIFICACIÓN DE CITAS (regla #9 v1.6): tu reporte todavía no se puede entregar. '
             f'{len(fallos)} problema(s) entre {n} cita(s):\n'
             + ''.join(f'- {x}\n' for x in fallos[:25])
             + ('…\n' if len(fallos) > 25 else '')
             + '\nPara cada uno: abre el archivo con Read en esas líneas y copia el fragmento EXACTO, '
               'corrige el número de línea, o retira el hallazgo si ya no se sostiene. No reconstruyas '
               'código de memoria. Después vuelve a escribir el reporte COMPLETO: tu último mensaje es lo '
               'único que recibe quien te invocó. El bloque va justo antes de la línea del veredicto:\n'
             + formato_bloque())
    print(json.dumps({'decision': 'block', 'reason': razon}, ensure_ascii=False))
    return 0


def main():
    args = sys.argv[1:]
    if '--hook' in args:
        return modo_hook()
    if not args or args[0] in ('-h', '--help'):
        print(__doc__)
        return 2
    repo = raiz_repo(os.getcwd())
    if '--repo' in args:
        repo = pathlib.Path(args[args.index('--repo') + 1])
    reporte = sys.stdin.read() if args[0] == '-' else pathlib.Path(args[0]).read_text(encoding='utf-8')
    fallos, avisos, n = verificar(reporte, repo)
    for a in avisos:
        print('AVISO ' + a)
    for f in fallos:
        print('FALLA ' + f)
    print(f'CITAS {"OK" if not fallos else "FALLA"}: {n} cita(s), {len(fallos)} problema(s), {len(avisos)} aviso(s)')
    return 1 if fallos else 0


if __name__ == '__main__':
    sys.exit(main())
