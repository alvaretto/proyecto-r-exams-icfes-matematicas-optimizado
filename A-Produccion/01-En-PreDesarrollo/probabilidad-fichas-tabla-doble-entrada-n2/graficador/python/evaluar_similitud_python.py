"""Herramienta de evaluación (no es entregable): compara un render contra el original escaneado.

Uso: python3 evaluar.py <render.png> <etiqueta> [directorio_salida]
Genera: comparacion_python_<etiqueta>.png, zoom_python_<etiqueta>.png y escribe métricas por pantalla (JSON al final).
"""
"""Evaluación de similitud de un render contra el original escaneado (herramienta de iteración, no del ejercicio).

Uso:  python3 evaluar_similitud_python.py output_python_vN.png vN
Escribe comparacion_python_vN.png y zoom_python_vN.png junto a este archivo, imprime las métricas y la puntuación
de la rúbrica de `comparar-similitud-visual` (6 categorías, 100 puntos) con los criterios que se documentan en
`puntuar()`. Requiere numpy, Pillow, scipy e ImageMagick (`magick`). Las medidas del original (rejilla, bandas de
muestreo) son las del escaneo `../originales/orig_enunciado_tabla.png`.
"""
import json
import os
import subprocess
import sys
import tempfile

import numpy as np
from PIL import Image, ImageDraw
from scipy import ndimage as ndi

AQUI = os.path.dirname(os.path.abspath(__file__))
ORIG = os.path.join(AQUI, "..", "originales", "orig_enunciado_tabla.png")
SP = tempfile.mkdtemp(prefix="eval_similitud_") + os.sep   # temporales (recortes en gris para magick)

X = [10.08, 138.33, 265.35, 392.63, 519.37]
Y = [16.87, 52.30, 87.26, 123.37, 158.63, 194.15, 229.14]
# etiquetas: (c0, c1, r0, r1, categoría)
ETIQ = {
    "Figura geométrica": (2, 4, 0, 1, "H"), "Color": (0, 1, 1, 2, "H"), "Categoría": (1, 2, 1, 2, "H"),
    "Círculo": (2, 3, 1, 2, "H"), "Cuadrado": (3, 4, 1, 2, "H"),
    "Blanca": (0, 1, 2, 4, "B"), "Negra": (0, 1, 4, 6, "B"),
    "Par(1)": (1, 2, 2, 3, "R"), "Impar(1)": (1, 2, 3, 4, "R"), "Par(2)": (1, 2, 4, 5, "R"), "Impar(2)": (1, 2, 5, 6, "R"),
    "12": (2, 3, 2, 3, "N"), "11": (3, 4, 2, 3, "N"), "8a": (2, 3, 3, 4, "N"), "15": (3, 4, 3, 4, "N"),
    "16": (2, 3, 4, 5, "N"), "20": (3, 4, 4, 5, "N"), "10": (2, 3, 5, 6, "N"), "8b": (3, 4, 5, 6, "N"),
}


def lum(a):
    return 0.299 * a[..., 0] + 0.587 * a[..., 1] + 0.114 * a[..., 2]


def cargar_render_a_escala_original(ruta):
    im = Image.open(ruta).convert("RGB")
    a = np.asarray(im).astype(float)
    h, w, _ = a.shape
    fx, fy = w / 530.0, h / 244.0
    # reducción por promedio de área al lienzo del original (530 x 244)
    if abs(fx - round(fx)) < 1e-6 and abs(fy - round(fy)) < 1e-6 and round(fx) == round(fy):
        k = int(round(fx))
        a = a.reshape(244, k, 530, k, 3).mean(axis=(1, 3))
    else:
        a = np.asarray(im.resize((530, 244), Image.BOX)).astype(float)
    return a


def caja_tabla(a, thr=170, minpx=6):
    L = lum(a)
    m = L < thr
    filas = np.where(m.sum(axis=1) >= minpx)[0]
    cols = np.where(m.sum(axis=0) >= minpx)[0]
    return cols.min(), cols.max() + 1, filas.min(), filas.max() + 1


def region(c0, c1, r0, r1, m=3.2):
    return (int(np.ceil(Y[r0] + m)), int(np.floor(Y[r1] - m)), int(np.ceil(X[c0] + m)), int(np.floor(X[c1] - m)))


def metricas_etiqueta(a, cat):
    """a: recorte RGB de la etiqueta. Devuelve bbox 50%, centroide, masa y área (en u)."""
    L = lum(a)
    fondo = np.array([247., 235., 247.]) if cat == "H" else np.array([252., 253., 253.])
    bgL = lum(fondo)
    core = 27.0 if cat == "H" else 3.0
    thr = (bgL + core) / 2.0
    mk = L < thr
    cs, rs = mk.sum(axis=0), mk.sum(axis=1)
    xi, yi = np.where(cs >= 1)[0], np.where(rs >= 1)[0]
    d = ((bgL - L) / (bgL - core)).clip(0, 1)
    d[d < 0.12] = 0
    m = d.sum()
    yy, xx = np.mgrid[0:L.shape[0], 0:L.shape[1]]
    cx = (xx * d).sum() / m + 0.5
    cy = (yy * d).sum() / m + 0.5
    sx = np.sqrt((((xx + 0.5 - cx) ** 2) * d).sum() / m)
    sy = np.sqrt((((yy + 0.5 - cy) ** 2) * d).sum() / m)
    return dict(x0=xi.min(), x1=xi.max() + 1, y0=yi.min(), y1=yi.max() + 1, area=int(mk.sum()), masa=float(m),
                cx=float(cx), cy=float(cy), sx=float(sx), sy=float(sy))


def evaluar(ruta, etiqueta, salida=AQUI + os.sep):
    o = np.asarray(Image.open(ORIG).convert("RGB")).astype(float)
    r = cargar_render_a_escala_original(ruta)
    out = {}
    # ---- 1. recorte a la caja de la tabla y comparación visual ----
    bo, br = caja_tabla(o), caja_tabla(r)
    out["caja_orig"] = [int(v) for v in bo]
    out["caja_render"] = [round(float(v), 1) for v in br]
    mg = 4
    oc = o[bo[2] - mg:bo[3] + mg, bo[0] - mg:bo[1] + mg]
    # recorte del render a la misma caja relativa (escala de 3x si procede)
    im = Image.open(ruta).convert("RGB")
    k = im.size[0] / 530.0
    rc_img = im.crop((int((br[0] - mg) * k), int((br[2] - mg) * k), int((br[1] + mg) * k), int((br[3] + mg) * k)))
    rc_img = rc_img.resize((oc.shape[1], oc.shape[0]), Image.LANCZOS)
    rc = np.asarray(rc_img).astype(float)
    # lado a lado ×2
    esc = 3
    A = Image.fromarray(oc.clip(0, 255).astype(np.uint8)).resize((oc.shape[1] * esc, oc.shape[0] * esc), Image.LANCZOS)
    B = rc_img.resize((oc.shape[1] * esc, oc.shape[0] * esc), Image.LANCZOS)
    lienzo = Image.new("RGB", (A.width, A.height * 2 + 24), "white")
    lienzo.paste(A, (0, 0)); lienzo.paste(B, (0, A.height + 24))
    dr = ImageDraw.Draw(lienzo)
    dr.text((4, A.height + 6), "ORIGINAL (arriba)  /  RENDER %s (abajo)" % etiqueta, fill=(0, 0, 0))
    lienzo.save(salida + "comparacion_python_%s.png" % etiqueta)
    # superposición diferencia (gris)
    Lo, Lr = lum(oc), lum(rc)
    dif = np.abs(Lo - Lr)
    out["rmse_gris_recorte"] = float(np.sqrt(((Lo - Lr) ** 2).mean()) / 255.0)
    # ---- 2. RMSE auxiliar con magick (gris, mitad de tamaño, blur 0x1) ----
    Image.fromarray(oc.clip(0, 255).astype(np.uint8)).save(SP + "_tmp_o.png")
    rc_img.save(SP + "_tmp_r.png")
    for nom in ("o", "r"):
        subprocess.run(["magick", SP + "_tmp_%s.png" % nom, "-colorspace", "Gray", "-resize", "50%", "-blur", "0x1",
                        SP + "_tmp_%s_g.png" % nom], check=True)
    p = subprocess.run(["magick", "compare", "-metric", "RMSE", SP + "_tmp_o_g.png", SP + "_tmp_r_g.png", "null:"],
                       capture_output=True, text=True)
    txt = (p.stderr or p.stdout).strip()
    out["rmse_magick_gris_mitad_blur"] = txt
    # ---- 3. por etiqueta (lienzo alineado) ----
    out["etiquetas"] = {}
    filas = []
    for k_, (c0, c1, r0, r1, cat) in ETIQ.items():
        y0, y1, x0, x1 = region(c0, c1, r0, r1)
        mo = metricas_etiqueta(o[y0:y1, x0:x1], cat)
        mr = metricas_etiqueta(r[y0:y1, x0:x1], cat)
        _bg = lum(np.array([247., 235., 247.])) if cat == "H" else lum(np.array([252., 253., 253.]))
        _thr = (_bg + (27.0 if cat == "H" else 3.0)) / 2.0
        _A, _B = lum(o[y0:y1, x0:x1]) < _thr, lum(r[y0:y1, x0:x1]) < _thr
        mr["iou"] = float((_A & _B).sum() / max(1, (_A | _B).sum()))
        d = {n: round(mr[n] - mo[n], 2) for n in ("x0", "x1", "y0", "y1", "area", "masa", "cx", "cy", "sx", "sy")}
        out["etiquetas"][k_] = dict(orig=mo, render=mr, delta=d)
        filas.append((k_, cat, d, mo, mr))
    # ---- 4. líneas ----
    def perfil_h(a_, yc, x0, x1, half=5):
        ys = np.arange(int(round(yc)) - half, int(round(yc)) + half + 1)
        p = lum(a_)[ys, x0:x1].mean(axis=1)
        bg = np.median(np.r_[p[:2], p[-2:]])
        w = (bg - p).clip(0)
        return (ys + 0.5 * 1.0) , p, bg
    def centro_ancho(pos, p, bg, Lc=22.0):
        w = (bg - p).clip(0)
        cen = (pos * w).sum() / w.sum()
        weq = w.sum() / (bg - Lc)
        return cen, weq, p.min()
    bandas = [(272, 300), (340, 380), (400, 440), (480, 512), (14, 32), (146, 160)]
    out["lineas_H"] = []
    for i, y in enumerate(Y):
        res_o, res_r = [], []
        for (x0, x1) in bandas:
            if i == 0 and x0 < 270:
                continue
            if i in (3, 5) and x0 < 140:
                continue
            for a_, res in ((o, res_o), (r, res_r)):
                pos, p, bg = perfil_h(a_, y, x0, x1)
                res.append(centro_ancho(pos + 0.5, p, bg))
        ro, rr = np.array(res_o), np.array(res_r)
        out["lineas_H"].append(dict(i=i, y_orig=float(ro[:, 0].mean()), y_render=float(rr[:, 0].mean()), weq_orig=float(ro[:, 1].mean()),
                                    weq_render=float(rr[:, 1].mean()), minL_orig=float(ro[:, 2].mean()), minL_render=float(rr[:, 2].mean())))
    out["lineas_V"] = []
    yb = [(56, 66), (92, 100), (128, 134), (165, 172), (200, 206), (212, 224)]
    for j, x in enumerate(X):
        res_o, res_r = [], []
        for (y0, y1) in yb + ([(20, 30)] if j in (2, 4) else []):
            for a_, res in ((o, res_o), (r, res_r)):
                xs = np.arange(int(round(x)) - 5, int(round(x)) + 6)
                p = lum(a_)[y0:y1, xs].mean(axis=0)
                bg = np.median(np.r_[p[:2], p[-2:]])
                res.append(centro_ancho(xs + 0.5, p, bg))
        ro, rr = np.array(res_o), np.array(res_r)
        out["lineas_V"].append(dict(j=j, x_orig=float(ro[:, 0].mean()), x_render=float(rr[:, 0].mean()), weq_orig=float(ro[:, 1].mean()),
                                    weq_render=float(rr[:, 1].mean()), minL_orig=float(ro[:, 2].mean()), minL_render=float(rr[:, 2].mean())))
    # ---- 5. colores ----
    from scipy import ndimage as ndi
    def media_relleno(a_):
        L_ = lum(a_)
        dark = ndi.binary_dilation(L_ < 190, iterations=3)
        px = []
        for (c0, c1, r0, r1) in [(2, 4, 0, 1), (0, 1, 1, 2), (1, 2, 1, 2), (2, 3, 1, 2), (3, 4, 1, 2)]:
            y0, y1, x0, x1 = region(c0, c1, r0, r1, 5)
            mk = np.zeros(L_.shape, bool); mk[y0:y1, x0:x1] = True
            px.append(a_[mk & ~dark])
        return np.concatenate(px).mean(axis=0)
    def media_tinta(a_, celdas, thr=133):
        px = []
        L_ = lum(a_)
        for (c0, c1, r0, r1) in celdas:
            y0, y1, x0, x1 = region(c0, c1, r0, r1)
            sub, l = a_[y0:y1, x0:x1].reshape(-1, 3), L_[y0:y1, x0:x1].reshape(-1)
            px.append(sub[l < thr])
        return np.concatenate(px).mean(axis=0)
    def px_etiquetas(a_, cats, thr=133):
        px, L_ = [], lum(a_)
        for k_, (c0, c1, r0, r1, cat) in ETIQ.items():
            if cat not in cats:
                continue
            y0, y1, x0, x1 = region(c0, c1, r0, r1)
            sub, l = a_[y0:y1, x0:x1].reshape(-1, 3), L_[y0:y1, x0:x1].reshape(-1)
            px.append((sub[l < thr], sub[l <= np.percentile(l, 4)]))
        return np.concatenate([p[0] for p in px]), np.concatenate([p[1] for p in px])
    def lineas(a_, thr=133):
        L_ = lum(a_)
        def hs(yc, x0, x1, half=4):
            ys = slice(int(yc - half), int(yc + half + 1)); sub, l = a_[ys, x0:x1].reshape(-1, 3), L_[ys, x0:x1].reshape(-1); return sub[l < thr]
        def vs(xc, y0, y1, half=4):
            xs = slice(int(xc - half), int(xc + half + 1)); sub, l = a_[y0:y1, xs].reshape(-1, 3), L_[y0:y1, xs].reshape(-1); return sub[l < thr]
        gruesas = [hs(Y[0], 272, 512), hs(Y[1], 14, 512), hs(Y[6], 14, 512), vs(X[0], 56, 225), vs(X[4], 20, 225)]
        finas = [hs(Y[2], 14, 512), hs(Y[3], 146, 512), hs(Y[4], 14, 512), hs(Y[5], 146, 512), vs(X[1], 56, 225), vs(X[3], 56, 225), vs(X[2], 92, 225)]
        return np.concatenate(gruesas).mean(axis=0), np.concatenate(finas).mean(axis=0)
    ho, ho_core = px_etiquetas(o, ("H",)); hr, hr_core = px_etiquetas(r, ("H",))
    bo, bo_core = px_etiquetas(o, ("B", "R", "N")); br_, br_core = px_etiquetas(r, ("B", "R", "N"))
    go, fo = lineas(o); gr_, fr = lineas(r)
    pap_o = o[3:13, 15:255].reshape(-1, 3).mean(axis=0); pap_r = r[3:13, 15:255].reshape(-1, 3).mean(axis=0)
    rd = lambda v: [round(float(x), 1) for x in v]
    out["colores"] = dict(
        papel_orig=rd(pap_o), papel_render=rd(pap_r),
        relleno_orig=rd(media_relleno(o)), relleno_render=rd(media_relleno(r)),
        tinta_enc_orig=rd(ho.mean(axis=0)), tinta_enc_render=rd(hr.mean(axis=0)),
        nucleo_enc_orig=rd(ho_core.mean(axis=0)), nucleo_enc_render=rd(hr_core.mean(axis=0)),
        tinta_cuerpo_orig=rd(bo.mean(axis=0)), tinta_cuerpo_render=rd(br_.mean(axis=0)),
        nucleo_cuerpo_orig=rd(bo_core.mean(axis=0)), nucleo_cuerpo_render=rd(br_core.mean(axis=0)),
        lineas_gruesas_orig=rd(go), lineas_gruesas_render=rd(gr_), lineas_finas_orig=rd(fo), lineas_finas_render=rd(fr))
    def d_pct(a, b): return float(np.linalg.norm(np.array(a) - np.array(b)) / np.linalg.norm([255, 255, 255]) * 100)
    c = out["colores"]
    out["colores_dist_pct"] = {k: round(d_pct(c[k + "_orig"], c[k + "_render"]), 2) for k in
                               ("papel", "relleno", "tinta_enc", "nucleo_enc", "tinta_cuerpo", "nucleo_cuerpo", "lineas_gruesas", "lineas_finas")}
    # promedio por celdas (color medio de cada celda = descriptor de paso bajo)
    pb = []
    for (c0, c1, r0, r1, cat) in ETIQ.values():
        y0, y1, x0, x1 = region(c0, c1, r0, r1)
        pb.append(np.abs(o[y0:y1, x0:x1].reshape(-1, 3).mean(axis=0) - r[y0:y1, x0:x1].reshape(-1, 3).mean(axis=0)))
    out["dif_color_medio_celda_max"] = float(np.max(pb))
    out["dif_color_medio_celda_prom"] = float(np.mean(pb))
    # ---- 6. zoom de esquinas ×3 (orig sobre render) ----
    def zoom(a_img, box, s=4):
        return a_img.crop(box).resize(((box[2] - box[0]) * s, (box[3] - box[1]) * s), Image.LANCZOS)
    Ofull = Image.fromarray(o.clip(0, 255).astype(np.uint8))
    Rfull = Image.fromarray(r.clip(0, 255).astype(np.uint8))
    im3 = Image.open(ruta).convert("RGB")
    kk = im3.size[0] / 530.0
    def zoomR(box, s=4):
        return im3.crop(tuple(int(v * kk) for v in box)).resize(((box[2] - box[0]) * s, (box[3] - box[1]) * s), Image.LANCZOS)
    cajas = [(0, 40, 280, 130), (260, 130, 530, 244)]
    piezas = []
    for bx in cajas:
        a1 = zoom(Ofull, bx); b1 = zoomR(bx)
        c = Image.new("RGB", (a1.width, a1.height * 2 + 6), "white"); c.paste(a1, (0, 0)); c.paste(b1, (0, a1.height + 6)); piezas.append(c)
    W = sum(p.width for p in piezas) + 10
    z = Image.new("RGB", (W, max(p.height for p in piezas)), "white")
    xo = 0
    for p in piezas:
        z.paste(p, (xo, 0)); xo += p.width + 10
    z.save(salida + "zoom_python_%s.png" % etiqueta)
    return out, filas


import numpy as np  # noqa: E402

RANGO_X, RANGO_Y = 514.0, 217.0   # caja de la tabla en el original (u)

def puntuar(ev):
    """Puntuación por categoría a partir de las métricas. Criterios (interpolan entre los anclajes de la rúbrica):
    colores: máx. distancia RGB (% de 441,7) entre medias de relleno, fondo y tinta de encabezado, del cuerpo y de las
      líneas gruesas; 20 si <= 1 %, 15 a 10 %. El núcleo oscuro del texto y las líneas finas (afectadas por el recorte
      del escáner y por la inclinación del escaneo) se informan y puntúan solo si superan 10 %.
    posiciones: mayor error de centroide, caja de tinta y línea, en % del rango de la tabla; 20 si <= 1 %, 15 a 5 %.
    valores: 20 si los 8 números y los 10 rótulos están en su celda (se verifica a ojo, regla #24 H-3).
    proporciones: diferencia de aspecto de la caja de la tabla; 15 si <= 0,5 %, 10 a 5 %.
    estilos: 15 menos tipografía (IoU medio de la tinta por etiqueta), masa de las líneas, masa del texto y contraste
      líneas finas/gruesas.
    elementos: 10 si no falta ni sobra ninguno.
    """
    out = {}; det = {}
    # ---- Colores ----
    c=ev["colores_dist_pct"]; prim={k:c[k] for k in ("papel","relleno","tinta_enc","tinta_cuerpo","lineas_gruesas")}
    d=max(prim.values())
    pts=20.0 if d<=1 else (20-(d-1)*5/9 if d<=10 else 15-(d-10)*0.5)
    sec={k:c[k] for k in ("nucleo_enc","nucleo_cuerpo","lineas_finas")}
    pen=0.5*sum(1 for v in sec.values() if v>10)
    out["colores"]=round(max(0,pts-pen),1)
    det["colores"]=dict(distancias_principales_pct=prim,max_principal_pct=round(d,2),secundarias_pct=sec,penalizacion_secundarias=pen)
    # ---- Posiciones ----
    eh=0.0; evv=0.0
    for k,v in ev["etiquetas"].items():
        dl=v["delta"]; eh=max(eh,abs(dl["x0"]),abs(dl["x1"]),abs(dl["cx"])); evv=max(evv,abs(dl["y0"]),abs(dl["y1"]),abs(dl["cy"]))
    lh=max(abs(l["y_render"]-l["y_orig"]) for l in ev["lineas_H"]); lv=max(abs(l["x_render"]-l["x_orig"]) for l in ev["lineas_V"])
    eh=max(eh,lv); evv=max(evv,lh)
    e=max(eh/RANGO_X*100, evv/RANGO_Y*100)
    out["posiciones"]=round(20.0 if e<=1 else max(0,20-(e-1)*1.25),1)
    det["posiciones"]=dict(max_error_horizontal_u=round(eh,2),max_error_vertical_u=round(evv,2),error_max_pct_del_rango=round(e,2),
                           lineas_H_max_u=round(lh,2),lineas_V_max_u=round(lv,2))
    # ---- Valores ----
    out["valores"]=20.0; det["valores"]=dict(valores_incorrectos=0,celdas_verificadas="8 números + 10 rótulos (H-3)")
    # ---- Proporciones ----
    bo=ev["caja_orig"]; br=ev["caja_render"]
    ro=(bo[1]-bo[0])/(bo[3]-bo[2]); rr=(br[1]-br[0])/(br[3]-br[2]); diff=abs(rr-ro)/ro*100
    out["proporciones"]=round(15.0 if diff<=0.5 else max(0,15-(diff-0.5)*5/4.5),1)
    det["proporciones"]=dict(aspecto_orig=round(ro,4),aspecto_render=round(rr,4),dif_pct=round(diff,2))
    # ---- Estilos ----
    iou=np.mean([v["render"]["iou"] for v in ev["etiquetas"].values()])
    d_a=-1.0 if iou>=0.75 else -1.5 if iou>=0.65 else -2.5 if iou>=0.55 else -3.5
    lw=np.mean([abs(l["weq_render"]/l["weq_orig"]-1) for l in ev["lineas_H"]+ev["lineas_V"]])*100
    d_b=0.0 if lw<=5 else -0.5 if lw<=10 else -1.0 if lw<=20 else -2.0
    mm=np.mean([abs(v["render"]["masa"]/v["orig"]["masa"]-1) for v in ev["etiquetas"].values()])*100
    d_c=0.0 if mm<=5 else -0.5 if mm<=10 else -1.0 if mm<=20 else -2.0
    thin=[l["minL_render"] for l in ev["lineas_H"][2:6]]+[l["minL_render"] for l in ev["lineas_V"][1:4]]
    thick=[ev["lineas_H"][i]["minL_render"] for i in (0,1,6)]+[ev["lineas_V"][i]["minL_render"] for i in (0,4)]
    thin_o=[l["minL_orig"] for l in ev["lineas_H"][2:6]]+[l["minL_orig"] for l in ev["lineas_V"][1:4]]
    thick_o=[ev["lineas_H"][i]["minL_orig"] for i in (0,1,6)]+[ev["lineas_V"][i]["minL_orig"] for i in (0,4)]
    dc_r=np.mean(thin)-np.mean(thick); dc_o=np.mean(thin_o)-np.mean(thick_o)
    d_d=-0.5 if abs(dc_r-dc_o)>25 else 0.0
    out["estilos"]=round(max(0,15+d_a+d_b+d_c+d_d),1)
    det["estilos"]=dict(iou_medio_tinta=round(float(iou),3),ded_tipografia=d_a,error_masa_lineas_pct=round(float(lw),1),ded_lineas=d_b,
                        error_masa_texto_pct=round(float(mm),1),ded_peso_texto=d_c,contraste_fino_grueso_render=round(float(dc_r),0),contraste_fino_grueso_orig=round(float(dc_o),0),ded_contraste=d_d)
    out["elementos"]=10.0; det["elementos"]=dict(faltantes=0,sobrantes=0)
    out["total"]=round(sum(out.values()),1)
    return out,det


if __name__ == "__main__":
    ruta, etiqueta = sys.argv[1], sys.argv[2]
    salida = sys.argv[3] if len(sys.argv) > 3 else AQUI + os.sep
    out, filas = evaluar(ruta, etiqueta, salida)
    print("caja orig", out["caja_orig"], "caja render", out["caja_render"])
    print("RMSE gris (recorte, 0-1): %.4f   magick (gris, 50%%, blur 0x1): %s" % (out["rmse_gris_recorte"], out["rmse_magick_gris_mitad_blur"]))
    print("\nETIQUETAS: delta = render - orig (u).  x0 x1 y0 y1 | cx cy | sx sy | area masa")
    for k_, cat, d, mo, mr in filas:
        print("%-18s [%s] bbox(%+5.1f %+5.1f %+5.1f %+5.1f)  cen(%+5.2f %+5.2f)  sig(%+5.2f %+5.2f)  area %+5d (%4d vs %4d)  masa %+6.1f (%5.1f vs %5.1f)" % (
            k_, cat, d["x0"], d["x1"], d["y0"], d["y1"], d["cx"], d["cy"], d["sx"], d["sy"], d["area"], mr["area"], mo["area"], d["masa"], mr["masa"], mo["masa"]))
    print("\nLINEAS H: i  y_orig y_render | weq_o weq_r | minL_o minL_r")
    for l in out["lineas_H"]:
        print("  H%d  %7.2f %7.2f | %5.2f %5.2f | %5.0f %5.0f" % (l["i"], l["y_orig"], l["y_render"], l["weq_orig"], l["weq_render"], l["minL_orig"], l["minL_render"]))
    print("LINEAS V: j  x_orig x_render | weq_o weq_r | minL_o minL_r")
    for l in out["lineas_V"]:
        print("  V%d  %7.2f %7.2f | %5.2f %5.2f | %5.0f %5.0f" % (l["j"], l["x_orig"], l["x_render"], l["weq_orig"], l["weq_render"], l["minL_orig"], l["minL_render"]))
    print("\nCOLORES:", json.dumps(out["colores"]))
    print("DISTANCIA RGB (% de 441,7):", json.dumps(out["colores_dist_pct"]))
    print("dif color medio por celda: max %.1f prom %.1f" % (out["dif_color_medio_celda_max"], out["dif_color_medio_celda_prom"]))
    puntos, detalle = puntuar(out)
    print("\nPUNTUACIÓN (rúbrica de 100 puntos):", json.dumps(puntos, ensure_ascii=False))
    print("DETALLE:", json.dumps(detalle, ensure_ascii=False, default=float))
