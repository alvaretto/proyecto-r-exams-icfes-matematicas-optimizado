#!/usr/bin/env python3
"""Mide un PNG de diagrama de Venn contra la geometría del impreso (Flujo B, paso 2b).

Uso: python3 comparar_venn.py <png> --regiones 0,10,2,3,4,5,1 [--sin-ocr] [--json salida.json]

Puntúa las 6 categorías de comparar-similitud-visual con tolerancias, porque la referencia es un
escaneo: colores y grosores no se pueden exigir por debajo de la dispersión entre sus cuatro recortes.
Los números se miden por el borde izquierdo y por toda su caja de tinta (decisión del profesor, 2026-11-06).
Exit 0 si el puntaje es >= 98 y no falla ningún chequeo crítico; exit 1 si no. No sustituye la revisión a ojo del coordinador.
"""
import argparse, json, os, subprocess, sys, tempfile
import numpy as np
from PIL import Image, ImageOps
from scipy import ndimage as ndi
from scipy.optimize import least_squares

G = json.load(open(os.path.join(os.path.dirname(os.path.abspath(__file__)), 'geom_venn.json'), encoding='utf-8'))
W0, H0, R0 = G['W'], G['H'], G['r']
CIRC = [G['circulos'][k] for k in ('c1', 'c2', 'c3')]
# Medidos sobre los 4 recortes: mediana del 20 % más saturado sobre el trazo (y dispersión máxima).
COLOR_REF = [(107, 147, 168), (126, 56, 81), (74, 128, 106)]
TOL_COLOR = 45.0   # unidades RGB; la distancia máxima entre un recorte y la media es ~35-40
PUNTOS = dict(colores=20, posiciones=20, valores=20, proporciones=15, estilos=15, elementos=10)
# Eliminatorios: promediados con los demás, un círculo movido 6 unidades o un total añadido daban >= 98.
CRITICOS = ('marco_4', 'circulo_', 'centro_c', 'radio_c', 'numero_', 'rotulo_', 'tinta_extra', 'holgura_numero',
            'izq_', 'region_')

def grupos(v):
    return np.split(v, np.where(np.diff(v) > 1)[0] + 1) if len(v) else []

def marco(a):
    osc = a.max(2) < 110
    gx, gy = grupos(np.where(osc.mean(0) > .6)[0]), grupos(np.where(osc.mean(1) > .6)[0])
    return gx, gy

def tono(rgb):
    r, g, b = [rgb[..., i].astype(float) for i in range(3)]
    mx, mn = rgb.max(-1).astype(float), rgb.min(-1).astype(float)
    d = np.maximum(mx - mn, 1e-9)
    h = np.where(mx == r, ((g - b) / d) % 6, np.where(mx == g, (b - r) / d + 2, (r - g) / d + 4)) * 60
    s = (mx - mn) / np.maximum(mx, 1e-9)
    return h, s

def ajustar(xs, ys):
    f = lambda p: np.hypot(xs - p[0], ys - p[1]) - p[2]
    r = least_squares(f, [xs.mean(), ys.mean(), (xs.max() - xs.min()) / 2], loss='soft_l1', f_scale=1.5)
    return r.x, float(np.median(np.abs(f(r.x))))

def ocr(mascara, alto_digito_px):
    """Lee un número desde su máscara de tinta; devuelve None si tesseract no corre."""
    k = 36.0 / max(alto_digito_px, 1)
    im = Image.fromarray(255 - mascara.astype(np.uint8) * 255)
    im = im.resize((max(8, int(im.width * k)), max(8, int(im.height * k))), Image.LANCZOS)
    im = ImageOps.expand(im, border=12, fill=255)
    with tempfile.NamedTemporaryFile(suffix='.png') as t:
        im.save(t.name)
        try:
            r = subprocess.run(['tesseract', t.name, '-', '-l', 'spa', '--psm', '7', '-c', 'tessedit_char_whitelist=0123456789'],
                               capture_output=True, text=True, timeout=30)
        except Exception:
            return None
    return r.stdout.strip().replace(' ', '') if r.returncode == 0 else None

def medir(ruta, regiones, usar_ocr=True):
    a = np.array(Image.open(ruta).convert('RGB'))
    H, W = a.shape[:2]
    gx, gy = marco(a)
    chk = {k: [] for k in PUNTOS}
    def anota(cat, nombre, valor, tol, ok):
        chk[cat].append(dict(nombre=nombre, valor=valor, tolerancia=tol, ok=bool(ok)))
    if len(gx) < 2 or len(gy) < 2:
        anota('elementos', 'marco_4_lados', f'{len(gx)} columnas, {len(gy)} filas', '2 y 2', False)
        return chk, None
    x0, x1 = float(gx[0].mean()), float(gx[-1].mean()); y0, y1 = float(gy[0].mean()), float(gy[-1].mean())
    sx, sy = (x1 - x0) / W0, (y1 - y0) / H0
    aU = lambda px, py: ((px - x0) / sx, (py - y0) / sy)
    anota('elementos', 'marco_4_lados', 'sí', 'sí', True)
    anota('proporciones', 'aspecto_marco_%', round(((x1 - x0) / (y1 - y0)) / (W0 / H0) * 100 - 100, 2), '±1', abs(((x1 - x0) / (y1 - y0)) / (W0 / H0) - 1) < .01)
    grosor_marco = float(np.mean([len(g) for g in (gx[0], gx[-1])])) / sx
    anota('estilos', 'grosor_marco', round(grosor_marco, 2), f"{G['trazo_marco']}±0.7", abs(grosor_marco - G['trazo_marco']) <= .7)
    # círculos por tono
    h, s = tono(a)
    hue_ref = [tono(np.array([[list(int(c['color'][i:i + 2], 16) for i in (1, 3, 5))]], dtype=np.uint8))[0][0, 0] for c in CIRC]
    pequena = Image.fromarray(a).resize((max(8, round(W / sx)), max(8, round(H / sy))), Image.BOX)
    p = np.array(pequena).astype(float)
    circ_medidos = []
    dist_tono = np.stack([np.abs((h - hr + 180) % 360 - 180) for hr in hue_ref])
    clase = dist_tono.argmin(0)
    for i, c in enumerate(CIRC):
        m = (s > .22) & (clase == i) & (dist_tono[i] < 40) & (a.max(2) > 50)
        ys, xs = np.where(m)
        if len(xs) < 100:
            anota('elementos', f'circulo_{i + 1}', 'ausente', 'presente', False); circ_medidos.append(None); continue
        (cx, cy, r), res = ajustar(xs.astype(float), ys.astype(float))
        k = np.abs(np.hypot(xs - cx, ys - cy) - r) < 4 * max(sx, sy)
        (cx, cy, r), res = ajustar(xs[k].astype(float), ys[k].astype(float))
        ux, uy = aU(cx, cy); ru = r / ((sx + sy) / 2)
        circ_medidos.append((ux, uy, ru))
        anota('elementos', f'circulo_{i + 1}', f'residuo {res / sx:.2f}', '<1.5', res / sx < 1.5)
        anota('posiciones', f'centro_c{i + 1}', round(float(np.hypot(ux - c['x'], uy - c['y'])), 2), '<=1.5', np.hypot(ux - c['x'], uy - c['y']) <= 1.5)
        anota('proporciones', f'radio_c{i + 1}_%', round(ru / R0 * 100 - 100, 2), '±1', abs(ru / R0 - 1) < .01)
        area = float(k.sum()); grosor = area / (2 * np.pi * r) / sx
        anota('estilos', f'grosor_c{i + 1}', round(grosor, 2), f"{G['trazo_circulo']}±0.7", abs(grosor - G['trazo_circulo']) <= .7)
        # color a escala del recorte (1 px por unidad), con la misma receta que sobre el escaneo
        yy, xx = np.mgrid[0:p.shape[0], 0:p.shape[1]]
        cerca = np.abs(np.hypot(xx - cx / sx, yy - cy / sy) - r / sx) < 3
        hp, sp = tono(p.astype(np.uint8))
        dhp = np.stack([np.abs((hp - hr + 180) % 360 - 180) for hr in hue_ref])
        sel = cerca & (dhp.argmin(0) == i) & (sp > .1)
        if sel.sum() < 10:
            anota('colores', f'color_c{i + 1}', 'sin muestra', f'<={TOL_COLOR}', False); continue
        vals = p[sel]; sat = vals.max(1) - vals.min(1)
        rgb = np.median(vals[sat >= np.percentile(sat, 80)], 0)
        dist = float(np.linalg.norm(rgb - np.array(COLOR_REF[i])))
        anota('colores', f'color_c{i + 1}', dict(rgb=rgb.round().tolist(), distancia=round(dist, 1)), f'<={TOL_COLOR}', dist <= TOL_COLOR)
    # tinta de texto (negra), fuera de las líneas del marco
    v = a.max(2); sat = v.astype(int) - a.min(2)
    tinta = (v < 105) & (sat < 60)
    borde = np.zeros_like(tinta); m = int(round(4 * sx))
    borde[int(y0) + m:int(y1) - m, int(x0) + m:int(x1) - m] = True
    tinta &= borde
    usado = np.zeros_like(tinta)
    def caja_px(x, y, dx, dy):
        return int(max(0, x0 + (x - dx) * sx)), int(min(W, x0 + (x + dx) * sx)), int(max(0, y0 + (y - dy) * sy)), int(min(H, y0 + (y + dy) * sy))
    holgura_min = 1e9
    for j, nom in enumerate(G['orden_regiones']):
        an = G['anclas'][nom]
        # El número crece a la derecha de x_izq: la ventana cubre 9 unidades antes y 27 después.
        X0, X1, Y0, Y1 = caja_px(an['x_izq'] + 9, an['y'], 18, 12)
        ys, xs = np.where(tinta[Y0:Y1, X0:X1])
        if len(xs) < 20:
            anota('valores', f'numero_{nom}', 'ausente', 'presente', False); continue
        usado[Y0:Y1, X0:X1] = True
        bx0, bx1, by0, by1 = xs.min() + X0, xs.max() + X0, ys.min() + Y0, ys.max() + Y0
        # Borde izquierdo y centro vertical de la tinta, no su centro: desde 2026-11-06 el número va alineado a la izquierda.
        ux, _ = aU(float(bx0), 0.0); _, uy = aU(0.0, (by0 + by1) / 2)
        e_izq = abs(ux - an['x_izq']); e_cy = abs(uy - an['y'])
        anota('posiciones', f'izq_{nom}', round(float(e_izq), 2), '<=1.5', e_izq <= 1.5)
        anota('posiciones', f'cy_{nom}', round(float(e_cy), 2), '<=2', e_cy <= 2)
        # Toda la tinta, no un punto: dentro de los círculos de su región y fuera de los demás.
        xu_t, yu_t = aU(xs + X0, ys + Y0)
        en_region = all(((np.hypot(xu_t - c['x'], yu_t - c['y']) < R0) == (k in an['dentro'])).all()
                        for k, c in zip(('c1', 'c2', 'c3'), CIRC))
        anota('posiciones', f'region_{nom}', 'toda la tinta' if en_region else 'fuera', 'toda la tinta', en_region)
        alto = (by1 - by0 + 1) / sy
        anota('estilos', f'alto_{nom}', round(alto, 1), '14.5±1.5', abs(alto - 14.5) <= 1.5)
        # número esperado
        esperado = str(regiones[j])
        lab, n = ndi.label(tinta[Y0:Y1, X0:X1])
        n_comp = sum(1 for q in range(1, n + 1) if (lab == q).sum() > 15)
        leido = ocr(tinta[Y0:Y1, X0:X1], by1 - by0 + 1) if usar_ocr else None
        anota('valores', f'numero_{nom}', dict(esperado=esperado, ocr=leido, componentes=n_comp), 'ocr==esperado', leido == esperado or (leido is None and n_comp == len(esperado)))
        # holgura de la tinta contra los trazos
        pxu, pyu = aU(xs + X0, ys + Y0)
        for c in CIRC:
            holgura_min = min(holgura_min, float((np.abs(np.hypot(pxu - c['x'], pyu - c['y']) - R0) - G['trazo_circulo'] / 2).min()))
    if holgura_min < 1e9:
        anota('estilos', 'holgura_numero_trazo', round(holgura_min, 2), '>=2', holgura_min >= 2)
    # rótulos
    cajas = {'r1': (3, 112, 1, 24), 'r2': (118, 228, 1, 24), 'r3': (150, 228, 205, 236)}
    hrot = 1e9
    for k, (a0, a1, b0, b1) in cajas.items():
        X0, X1, Y0, Y1 = caja_px((a0 + a1) / 2, (b0 + b1) / 2, (a1 - a0) / 2, (b1 - b0) / 2)
        ys, xs = np.where(tinta[Y0:Y1, X0:X1])
        if len(xs) < 40:
            anota('elementos', f'rotulo_{k}', 'ausente', 'presente', False); continue
        usado[Y0:Y1, X0:X1] = True
        anota('elementos', f'rotulo_{k}', 'presente', 'presente', True)
        ru = G['rotulos'][k]; px, _ = aU(np.array([xs.min() + X0, xs.max() + X0]), 0)
        x_medido = px[0] if ru['ajuste'] == 'izquierda' else px[1]
        anota('posiciones', f'rotulo_{k}_x', round(float(abs(x_medido - ru['x'])), 2), '<=3', abs(x_medido - ru['x']) <= 3)
        pxu, pyu = aU(xs + X0, ys + Y0)
        for c in CIRC:
            hrot = min(hrot, float((np.abs(np.hypot(pxu - c['x'], pyu - c['y']) - R0) - G['trazo_circulo'] / 2).min()))
    if hrot < 1e9:
        anota('estilos', 'holgura_rotulo_trazo', round(hrot, 2), '>=1', hrot >= 1)
    lab, n = ndi.label(tinta & ~usado)
    areas = ndi.sum(tinta & ~usado, lab, range(1, n + 1)) if n else []
    umbral = 40 * sx * sy / 9
    extra = int(sum(x for x in areas if x >= umbral))
    anota('elementos', 'tinta_extra_px', extra, 0, extra == 0)
    return chk, circ_medidos

def puntuar(chk):
    tot = 0.0; det = {}
    for cat, pts in PUNTOS.items():
        lst = chk[cat]
        val = pts * (sum(c['ok'] for c in lst) / len(lst)) if lst else 0.0
        det[cat] = round(val, 2); tot += val
    return round(tot, 2), det

if __name__ == '__main__':
    ap = argparse.ArgumentParser()
    ap.add_argument('png'); ap.add_argument('--regiones', required=True); ap.add_argument('--json')
    ap.add_argument('--sin-ocr', action='store_true', help='caso extremo: el OCR confunde 88 con 83; solo cuenta dígitos')
    o = ap.parse_args()
    reg = [int(x) for x in o.regiones.split(',')]
    assert len(reg) == 7, 'hacen falta 7 regiones en el orden s1,p12,s2,ctr,p13,p23,s3'
    chk, _ = medir(o.png, reg, usar_ocr=not o.sin_ocr)
    total, det = puntuar(chk)
    criticas = [c['nombre'] for lst in chk.values() for c in lst if not c['ok'] and c['nombre'].startswith(CRITICOS)]
    print(f'PUNTAJE {total}/100  ', det, ' CRITICAS:', criticas or 'ninguna')
    for cat in PUNTOS:
        for c in chk[cat]:
            if not c['ok']:
                print(f"  FALLA {cat}.{c['nombre']}: {c['valor']} (tolerancia {c['tolerancia']})")
    if o.json:
        json.dump(dict(total=total, categorias=det, criticas=criticas, checks=chk), open(o.json, 'w', encoding='utf-8'), ensure_ascii=False, indent=1)
    sys.exit(0 if total >= 98 and not criticas else 1)
