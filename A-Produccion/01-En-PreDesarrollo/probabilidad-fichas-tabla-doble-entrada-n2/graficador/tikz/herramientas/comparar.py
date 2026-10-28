#!/usr/bin/env python3
"""Compara output_tikz_<v>.png con el original, ambos a la escala del escaneo (1 px = 1 px del original).
Uso: python3 -I comparar.py <vN> [--sin-imagenes]
Escribe en graficador/tikz/: comparacion_tikz_<v>.png, ampliacion_tikz_<v>.png y, en herramientas/_aux/, el JSON de medidas.
Todo lo que mide sobre el render lo mide con las MISMAS funciones que sobre el original."""
import sys, os, json, subprocess, numpy as np
from PIL import Image, ImageDraw, ImageOps

D = '/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2'
ORIG = D + '/graficador/originales/orig_enunciado_tabla.png'
T = D + '/graficador/tikz'
AUX = os.environ.get('TF_AUX', T + '/herramientas/_aux')
os.makedirs(AUX, exist_ok=True)
v = sys.argv[1]
sin_img = '--sin-imagenes' in sys.argv
_pos = [a for a in sys.argv[2:] if not a.startswith('--')]
RENDER = _pos[0] if _pos else f'{T}/output_tikz_{v}.png'

# marco del original (centros de línea, índices de píxel)
XO = [10, 138, 265, 392, 519]
YO = [16, 51.5, 87, 122.5, 158, 193.5, 229]
M = 8                       # margen alrededor de las líneas exteriores
CW, CH = 519 - 10 + 2 * M, 229 - 16 + 2 * M   # 525 x 229
CX0, CY0 = 10 - M, 16 - M                       # (2, 8)

def lum_of(img):
    a = np.asarray(img.convert('RGB')).astype(float)
    return 0.299 * a[..., 0] + 0.587 * a[..., 1] + 0.114 * a[..., 2], a

def runs(mask):
    out, cur = [], None
    for i, m in enumerate(mask):
        if m:
            cur = [i, i] if cur is None else [cur[0], i]
        elif cur is not None:
            out.append(tuple(cur)); cur = None
    if cur is not None: out.append(tuple(cur))
    return out

def detectar_exterior(img_hi):
    l, _ = lum_of(img_hi)
    dark = l < 110
    col, row = dark.sum(axis=0), dark.sum(axis=1)
    rc = runs(col > 0.5 * col.max()); rr = runs(row > 0.4 * row.max())
    xL = (rc[0][0] + rc[0][1]) / 2; xR = (rc[-1][0] + rc[-1][1]) / 2
    yT = (rr[0][0] + rr[0][1]) / 2; yB = (rr[-1][0] + rr[-1][1]) / 2
    return xL, xR, yT, yB, rc, rr

def aplanar(img):
    # compone sobre blanco si trae canal alfa (el PNG de exams::tex2image es RGBA)
    if img.mode in ('RGBA', 'LA', 'PA') or 'transparency' in img.info:
        base = Image.new('RGBA', img.size, (255, 255, 255, 255)); base.alpha_composite(img.convert('RGBA')); return base.convert('RGB')
    return img.convert('RGB')

def alinear(img_hi):
    PADV = 120   # relleno blanco: la caja de recorte puede salirse del PNG cuando el margen del dibujo es pequeño
    img_hi = ImageOps.expand(aplanar(img_hi), border=PADV, fill=(255, 255, 255))
    xL, xR, yT, yB, _, _ = detectar_exterior(img_hi)
    sx = (xR - xL) / (XO[4] - XO[0]); sy = (yB - yT) / (YO[6] - YO[0])
    fx0 = xL - (XO[0] - CX0 + 0.5) * sx + 0.5; fy0 = yT - (YO[0] - CY0 + 0.5) * sy + 0.5
    box = (fx0, fy0, fx0 + CW * sx, fy0 + CH * sy)
    crop = img_hi.convert('RGB').resize((CW, CH), Image.LANCZOS, box=box)
    full = Image.new('RGB', (530, 244), (255, 255, 255)); full.paste(crop, (CX0, CY0))
    return full, dict(xL=xL, xR=xR, yT=yT, yB=yB, sx=sx, sy=sy, px_por_px_orig_x=sx, px_por_px_orig_y=sy,
                      ancho_tabla_hi=xR - xL, alto_tabla_hi=yB - yT, relacion_aspecto=(xR - xL) / (yB - yT))

# ---------- medidas (idénticas para original y render) -------------------------------------------
def lineas(l):
    res = {}
    def hprof(yc, x0, x1, half=6):
        ys = np.arange(int(round(yc)) - half, int(round(yc)) + half + 1)
        p = np.array([np.median(l[y, x0:x1]) for y in ys]); return ys, p
    def vprof(xc, y0, y1, half=6):
        xs = np.arange(int(round(xc)) - half, int(round(xc)) + half + 1)
        p = np.array([np.median(l[y0:y1, x]) for x in xs]); return xs, p
    def cen(ix, p):
        bg = np.percentile(p, 90); w = np.clip(bg - p, 0, None)
        w[w < 0.15 * w.max()] = 0
        return float((ix * w).sum() / w.sum()), float(np.clip(bg - p, 0, None).sum()), float(p.min())
    H = {'y16_c34': (16, 275, 510), 'y51.5_c12': (51.5, 20, 255), 'y51.5_c34': (51.5, 275, 510), 'y87': (87, 150, 510),
         'y122.5': (122.5, 150, 510), 'y158_c2-4': (158, 150, 510), 'y158_c1': (158, 20, 130), 'y193.5': (193.5, 150, 510),
         'y229': (229, 20, 510)}
    for k, (yc, x0, x1) in H.items():
        ix, p = hprof(yc, x0, x1); c, s, mn = cen(ix, p); res[k] = dict(centro=c, integral=s, minimo=mn)
    V = {'x10': (10, 92, 226), 'x138': (138, 92, 226), 'x265_f2-6': (265, 92, 226), 'x265_f1': (265, 20, 48),
         'x392': (392, 92, 226), 'x519_f2-6': (519, 92, 226), 'x519_f1': (519, 20, 48)}
    for k, (xc, y0, y1) in V.items():
        ix, p = vprof(xc, y0, y1); c, s, mn = cen(ix, p); res[k] = dict(centro=c, integral=s, minimo=mn)
    return res

CELDAS = {
 'FigGeom': (XO[2], XO[4], YO[0], YO[1]), 'Color': (XO[0], XO[1], YO[1], YO[2]), 'Categoria': (XO[1], XO[2], YO[1], YO[2]),
 'Circulo': (XO[2], XO[3], YO[1], YO[2]), 'Cuadrado': (XO[3], XO[4], YO[1], YO[2]),
 'Blanca': (XO[0], XO[1], YO[2], YO[4]), 'Negra': (XO[0], XO[1], YO[4], YO[6]),
 'Par_b': (XO[1], XO[2], YO[2], YO[3]), 'Impar_b': (XO[1], XO[2], YO[3], YO[4]),
 'Par_n': (XO[1], XO[2], YO[4], YO[5]), 'Impar_n': (XO[1], XO[2], YO[5], YO[6]),
 '12': (XO[2], XO[3], YO[2], YO[3]), '11': (XO[3], XO[4], YO[2], YO[3]), '8': (XO[2], XO[3], YO[3], YO[4]),
 '15': (XO[3], XO[4], YO[3], YO[4]), '16': (XO[2], XO[3], YO[4], YO[5]), '20': (XO[3], XO[4], YO[4], YO[5]),
 '10': (XO[2], XO[3], YO[5], YO[6]), '8b': (XO[3], XO[4], YO[5], YO[6])}
ENC = {'FigGeom', 'Color', 'Categoria', 'Circulo', 'Cuadrado'}

def textos(l, a):
    res = {}
    for nm, (x0, x1, y0, y1) in CELDAS.items():
        X0, X1, Y0, Y1 = int(round(x0 + 5)), int(round(x1 - 5)), int(round(y0 + 5)), int(round(y1 - 5))
        sub = l[Y0:Y1, X0:X1]; m = sub < 170
        if m.sum() == 0: res[nm] = None; continue
        yy, xx = np.where(m)
        bx0, bx1, by0, by1 = X0 + xx.min(), X0 + xx.max(), Y0 + yy.min(), Y0 + yy.max()
        # línea base: mayor caída de tinta por fila
        ya, yb = y0, y1
        Ya = int(max(by0 - 4, ya + 4)); Yb = int(min(by1 + 8, yb - 3))
        bg = np.percentile(l[Ya:Yb, bx0:bx1 + 1], 90)
        ink = np.array([np.clip((bg - l[y, bx0:bx1 + 1]) / bg, 0, 1).sum() for y in range(Ya, Yb)])
        d = ink[:-1] - ink[1:]; k = int(np.argmax(d)); base = Ya + k + 1
        ink0 = 25 if nm in ENC else 0
        bgc = np.percentile(l[by0 - 3:by1 + 4, bx0 - 3:bx1 + 4], 85)
        cov = float(np.clip((bgc - l[by0 - 3:by1 + 4, bx0 - 3:bx1 + 4]) / (bgc - ink0), 0, 1).sum())
        res[nm] = dict(x0=int(bx0), x1=int(bx1), y0=int(by0), y1=int(by1), w=int(bx1 - bx0 + 1), h=int(by1 - by0 + 1),
                       cx=float((bx0 + bx1 + 1) / 2 - (x0 + x1) / 2), base=float(base), base_rel=float(base - (y0 + y1) / 2),
                       cobertura=cov)
    return res

def colores(l, a):
    out = {}
    def med(sel): return [int(round(float(v))) for v in np.median(sel, axis=0)]
    fills = []
    for (x0, x1, y0, y1) in [(16, 132, 58, 84), (144, 259, 58, 84), (271, 386, 58, 84), (398, 513, 58, 84), (271, 513, 22, 47)]:
        px = a[y0:y1, x0:x1].reshape(-1, 3); ll = l[y0:y1, x0:x1].reshape(-1); fills.append(px[ll > 225])
    out['relleno_encabezado'] = med(np.vstack(fills))
    wh = []
    for (x0, x1, y0, y1) in [(145, 258, 92, 119), (271, 386, 92, 119), (398, 513, 92, 119), (16, 132, 92, 153), (271, 513, 163, 190)]:
        px = a[y0:y1, x0:x1].reshape(-1, 3); ll = l[y0:y1, x0:x1].reshape(-1); wh.append(px[ll > 225])
    out['blanco_celdas'] = med(np.vstack(wh))
    tx = []
    for nm in ('FigGeom', 'Color', 'Categoria', 'Circulo', 'Cuadrado'):
        x0, x1, y0, y1 = CELDAS[nm]
        px = a[int(y0 + 5):int(y1 - 5), int(x0 + 5):int(x1 - 5)].reshape(-1, 3); ll = l[int(y0 + 5):int(y1 - 5), int(x0 + 5):int(x1 - 5)].reshape(-1)
        tx.append(px[ll < 130])
    tx = np.vstack(tx)
    out['trazo_encabezado_lum<130_media'] = [int(round(float(v))) for v in tx.mean(0)]
    out['trazo_encabezado_lum<60_media'] = None
    cores = []
    for nm in ('FigGeom', 'Color', 'Categoria', 'Circulo', 'Cuadrado'):
        x0, x1, y0, y1 = CELDAS[nm]
        px = a[int(y0 + 5):int(y1 - 5), int(x0 + 5):int(x1 - 5)].reshape(-1, 3); ll = l[int(y0 + 5):int(y1 - 5), int(x0 + 5):int(x1 - 5)].reshape(-1)
        cores.append(px[ll < 60])
    cores = np.vstack(cores)
    out['trazo_encabezado_lum<60_media'] = [int(round(float(v))) for v in cores.mean(0)] if len(cores) else None
    # texto negro: núcleo (píxeles muy oscuros) de los rótulos negros y de Par/Impar/cifras
    kk = []
    for nm in ('Blanca', 'Negra', 'Par_b', 'Impar_b', '12', '16', '20'):
        x0, x1, y0, y1 = CELDAS[nm]
        px = a[int(y0 + 5):int(y1 - 5), int(x0 + 5):int(x1 - 5)].reshape(-1, 3); ll = l[int(y0 + 5):int(y1 - 5), int(x0 + 5):int(x1 - 5)].reshape(-1)
        kk.append(px[ll < 40])
    out['texto_negro_lum<40_media'] = [int(round(float(v))) for v in np.vstack(kk).mean(0)]
    # color de línea gruesa (píxeles oscuros alrededor de y=229 y x=519)
    pl = []
    for (yc, x0, x1) in [(229, 20, 510)]:
        sub = a[yc - 4:yc + 5, x0:x1].reshape(-1, 3); ll = l[yc - 4:yc + 5, x0:x1].reshape(-1); pl.append(sub[ll < 70])
    out['linea_gruesa_lum<70_media'] = [int(round(float(v))) for v in np.vstack(pl).mean(0)]
    pl = []
    for yc in (87, 122.5, 158, 193.5):
        yc = int(round(yc)); sub = a[yc - 4:yc + 5, 150:510].reshape(-1, 3); ll = l[yc - 4:yc + 5, 150:510].reshape(-1); pl.append(sub[ll < 130])
    out['linea_fina_lum<130_media'] = [int(round(float(v))) for v in np.vstack(pl).mean(0)]
    return out

def tallos(l):
    def stem(y, x0, x1, ink=0, bg=250):
        row = np.clip((bg - l[y, x0:x1]) / (bg - ink), 0, 1)
        segs, cur = [], None
        for i, vv in enumerate(row):
            if vv > 0.15:
                cur = [i, i, vv] if cur is None else [cur[0], i, cur[2] + vv]
            elif cur is not None: segs.append(cur); cur = None
        if cur is not None: segs.append(cur)
        return [round(float(s[2]), 2) for s in segs]
    return {"I de Impar (y137)": stem(137, 176, 232), "'11' (y106)": stem(106, 444, 468), "'11' (y112)": stem(112, 444, 468),
            "Color 'l' (y65)": stem(65, 48, 100, ink=30, bg=243), "Cuadrado 'd' (y64)": stem(64, 411, 502, ink=30, bg=243),
            "Blanca (y120)": stem(120, 42, 108)}

def medir(img_full):
    l, a = lum_of(img_full)
    return dict(lineas=lineas(l), textos=textos(l, a), colores=colores(l, a), tallos=tallos(l))

if __name__ == '__main__':
    orig = Image.open(ORIG).convert('RGB')
    hi = Image.open(RENDER)
    rfull, geo = alinear(hi)
    rfull.save(f'{AUX}/render_{v}_escala_original.png')
    mo, mr = medir(orig), medir(rfull)
    # ---- RMSE auxiliar: gris, mitad de tamaño, desenfoque 0x1, sobre la caja de la tabla (recorte de 525x229)
    box = (CX0, CY0, CX0 + CW, CY0 + CH)
    orig.crop(box).save(f'{AUX}/o_crop.png'); rfull.crop(box).save(f'{AUX}/r_crop_{v}.png')
    for src, dst in ((f'{AUX}/o_crop.png', f'{AUX}/o_g.png'), (f'{AUX}/r_crop_{v}.png', f'{AUX}/r_g_{v}.png')):
        subprocess.run(['magick', src, '-colorspace', 'Gray', '-resize', '50%', '-blur', '0x1', dst], check=True)
    cp = subprocess.run(['magick', 'compare', '-metric', 'RMSE', f'{AUX}/o_g.png', f'{AUX}/r_g_{v}.png', 'null:'], capture_output=True, text=True)
    rmse_txt = (cp.stderr or cp.stdout).strip()
    print('RMSE aux (gris, 1/2, blur 0x1):', rmse_txt)
    import re
    _m = re.match(r'\s*([0-9.eE+-]+)\s*\(([0-9.eE+-]+)\)', rmse_txt)
    rmse = dict(valor=float(_m.group(1)), normalizado=float(_m.group(2)), procedimiento='gris, mitad de tamaño, blur 0x1, caja de la tabla (525x229 px del escaneo)') if _m else None
    json.dump(dict(geometria=geo, original=mo, render=mr, rmse_aux=rmse), open(f'{AUX}/medidas_{v}.json', 'w'), indent=1, default=float)
    # ---- imágenes
    if not sin_img:
        k = 2
        def panel(im): return im.crop(box).resize((CW * k, CH * k), Image.LANCZOS)
        po, pr = panel(orig), panel(rfull)
        W = po.width + pr.width + 30; Hh = po.height + 40
        out = Image.new('RGB', (W, Hh), (255, 255, 255)); dr = ImageDraw.Draw(out)
        out.paste(po, (0, 40)); out.paste(pr, (po.width + 30, 40))
        dr.text((10, 12), 'ORIGINAL (escaneo)', fill=(0, 0, 0)); dr.text((po.width + 40, 12), f'TikZ {v} (a la escala del escaneo)', fill=(0, 0, 0))
        dr.line([(po.width + 15, 0), (po.width + 15, Hh)], fill=(150, 150, 150))
        out.save(f'{T}/comparacion_tikz_{v}.png')
        # ampliación x5 de dos zonas: esquina superior derecha (título + encabezado) e inferior izquierda (Negra/Impar)
        zonas = [(380, 8, 527, 100), (2, 150, 250, 237)]
        kk = 5; tiles = []
        for z in zonas:
            a_ = orig.crop(z).resize(((z[2] - z[0]) * kk, (z[3] - z[1]) * kk), Image.LANCZOS)
            b_ = rfull.crop(z).resize(((z[2] - z[0]) * kk, (z[3] - z[1]) * kk), Image.LANCZOS)
            tile = Image.new('RGB', (a_.width + b_.width + 20, a_.height), (255, 255, 255)); tile.paste(a_, (0, 0)); tile.paste(b_, (a_.width + 20, 0))
            tiles.append(tile)
        Wt = max(t.width for t in tiles); Ht = sum(t.height for t in tiles) + 20
        amp = Image.new('RGB', (Wt, Ht), (255, 255, 255)); yy = 0
        for t in tiles: amp.paste(t, (0, yy)); yy += t.height + 20
        amp.save(f'{T}/ampliacion_tikz_{v}.png')
    # ---- informe en pantalla
    print('geometría del render: relación de aspecto = %.4f (original 509/213 = %.4f) ; px_hi por px_orig: x=%.3f y=%.3f' % (geo['relacion_aspecto'], 509 / 213, geo['sx'], geo['sy']))
    print('\n== líneas (centro, integral de oscuridad, mínimo)  orig | render')
    for k_ in mo['lineas']:
        o, r = mo['lineas'][k_], mr['lineas'][k_]
        print('%-10s c=%7.2f int=%5.0f min=%3.0f | c=%7.2f int=%5.0f min=%3.0f   dc=%+5.2f dint=%+5.0f' % (k_, o['centro'], o['integral'], o['minimo'], r['centro'], r['integral'], r['minimo'], r['centro'] - o['centro'], r['integral'] - o['integral']))
    print('\n== colores  original | render')
    for k_ in mo['colores']:
        print('%-30s %s | %s' % (k_, mo['colores'][k_], mr['colores'][k_]))
    print('\n== textos: w x h, centroX rel, base rel, cobertura  orig | render')
    for k_ in mo['textos']:
        o, r = mo['textos'][k_], mr['textos'][k_]
        if r is None: print('%-10s render SIN TEXTO' % k_); continue
        print('%-10s %3dx%2d cx=%+4.1f b=%+5.2f cov=%4.0f | %3dx%2d cx=%+4.1f b=%+5.2f cov=%4.0f   dw=%+3d dh=%+3d dcov=%+4.0f(%+3.0f%%)' % (
            k_, o['w'], o['h'], o['cx'], o['base_rel'], o['cobertura'], r['w'], r['h'], r['cx'], r['base_rel'], r['cobertura'],
            r['w'] - o['w'], r['h'] - o['h'], r['cobertura'] - o['cobertura'], 100 * (r['cobertura'] - o['cobertura']) / o['cobertura']))
    print('\n== tallos (anchura equivalente de trazo, px)')
    for k_ in mo['tallos']:
        print('%-22s orig %s | render %s' % (k_, mo['tallos'][k_], mr['tallos'][k_]))
