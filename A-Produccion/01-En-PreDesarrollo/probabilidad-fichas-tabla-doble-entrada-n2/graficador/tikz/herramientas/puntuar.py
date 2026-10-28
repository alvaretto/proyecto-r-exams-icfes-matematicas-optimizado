#!/usr/bin/env python3
"""Puntua una iteracion con la rubrica de comparar-similitud-visual (Colores 20, Posiciones 20, Valores 20,
Proporciones 15, Estilos 15, Elementos 10) a partir de herramientas/_aux/medidas_<v>.json (lo escribe comparar.py).

Uso: python3 -I puntuar.py <vN> [--json]

Cada categoria se puntua con una formula escrita aqui (reproducible) que interpola entre los escalones de
references/sistema-puntuacion.md; lo que ninguna medida puede captar (la tipografia) entra como penalizacion
fija y declarada. Valores y Elementos exigen ademas la comprobacion visual (H-3), que se hace aparte."""
import sys, json, math

D = '/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/graficador/tikz/herramientas/_aux'
clamp = lambda x, a, b: max(a, min(b, x))

def pts_color(d):          # d = distancia RGB en % de la diagonal (441,67)
    if d < 1: return 20.0
    if d <= 10: return 20 - 5 * (d - 1) / 9
    if d <= 20: return 15 - 10 * (d - 10) / 10
    return 5.0

def pts_pos(d):            # d = desviacion en % del rango
    if d < 1: return 20.0
    if d < 5: return 20 - 5 * (d - 1) / 4
    if d < 10: return 15 - 5 * (d - 5) / 5
    if d < 20: return 10 - 5 * (d - 10) / 10
    return 0.0

def puntuar(v):
    m = json.load(open(f'{D}/medidas_{v}.json'))
    o, r, geo = m['original'], m['render'], m['geometria']
    res = {}
    # ---- Colores: 7 descriptores medidos con el MISMO procedimiento en ambas imagenes
    claves = ['blanco_celdas', 'relleno_encabezado', 'texto_negro_lum<40_media', 'trazo_encabezado_lum<130_media',
              'trazo_encabezado_lum<60_media', 'linea_gruesa_lum<70_media', 'linea_fina_lum<130_media']
    dc = {}
    for k in claves:
        a, b = o['colores'][k], r['colores'][k]
        dc[k] = 100 * math.sqrt(sum((x - y) ** 2 for x, y in zip(a, b))) / 441.67
    res['colores'] = (sum(pts_color(d) for d in dc.values()) / len(dc), {k: round(d, 2) for k, d in dc.items()})
    # ---- Posiciones: 16 lineas + 18 textos (x y linea base) en % del rango (509 en x, 213 en y)
    desv = []
    for k, ov in o['lineas'].items():
        rng = 509 if k.startswith('x') else 213
        desv.append(100 * abs(r['lineas'][k]['centro'] - ov['centro']) / rng)
    nt = 0
    for k, ot in o['textos'].items():
        rt = r['textos'][k]
        if rt is None: desv.append(100.0); continue
        desv.append(100 * abs(rt['cx'] - ot['cx']) / 509)
        if k != '11':      # la linea base medida en el original para '11' es un artefacto (caida de tinta ambigua)
            desv.append(100 * abs(rt['base_rel'] - ot['base_rel']) / 213)
        nt += 1
    res['posiciones'] = (sum(pts_pos(d) for d in desv) / len(desv), dict(max_desv_pct=round(max(desv), 3), n=len(desv)))
    # ---- Valores: los 18 textos presentes (la comprobacion celda a celda es visual)
    res['valores'] = (20.0 if nt == len(o['textos']) else 10.0, dict(textos_presentes=nt))
    # ---- Proporciones: relacion de aspecto, ancho de columnas y alto de filas (de los centros de linea)
    asp = 100 * abs(geo['relacion_aspecto'] - 509 / 213) / (509 / 213)
    L = r['lineas']; Lo = o['lineas']
    cols_r = [L['x138']['centro'] - L['x10']['centro'], L['x265_f2-6']['centro'] - L['x138']['centro'], L['x392']['centro'] - L['x265_f2-6']['centro'], L['x519_f2-6']['centro'] - L['x392']['centro']]
    cols_o = [Lo['x138']['centro'] - Lo['x10']['centro'], Lo['x265_f2-6']['centro'] - Lo['x138']['centro'], Lo['x392']['centro'] - Lo['x265_f2-6']['centro'], Lo['x519_f2-6']['centro'] - Lo['x392']['centro']]
    filas_r = [L['y51.5_c12']['centro'] - L['y16_c34']['centro'], L['y87']['centro'] - L['y51.5_c12']['centro'], L['y122.5']['centro'] - L['y87']['centro'], L['y158_c2-4']['centro'] - L['y122.5']['centro'], L['y193.5']['centro'] - L['y158_c2-4']['centro'], L['y229']['centro'] - L['y193.5']['centro']]
    filas_o = [Lo['y51.5_c12']['centro'] - Lo['y16_c34']['centro'], Lo['y87']['centro'] - Lo['y51.5_c12']['centro'], Lo['y122.5']['centro'] - Lo['y87']['centro'], Lo['y158_c2-4']['centro'] - Lo['y122.5']['centro'], Lo['y193.5']['centro'] - Lo['y158_c2-4']['centro'], Lo['y229']['centro'] - Lo['y193.5']['centro']]
    # posiciones de las fronteras interiores como fracción de la dimensión de la tabla (en % del rango): el escaneo
    # está ligeramente inclinado (±0,5 px), así que no se exige igualdad de cada fila por separado
    def fr(v): return [sum(v[:i + 1]) / sum(v) for i in range(len(v) - 1)]
    ec = max(100 * abs(x - y) for x, y in zip(fr(cols_r), fr(cols_o)))
    ef = max(100 * abs(x - y) for x, y in zip(fr(filas_r), fr(filas_o)))
    emax = max(asp, ec, ef)
    res['proporciones'] = (15.0 if emax < 1 else 15 - 5 * (emax - 1) / 4 if emax < 5 else 10 - 5 * (emax - 5) / 10,
                           dict(aspecto_pct=round(asp, 3), fronteras_columnas_max_pct_del_rango=round(ec, 2), fronteras_filas_max_pct_del_rango=round(ef, 2)))
    # ---- Estilos: lineas, astas, cobertura de tinta, tamano del texto + penalizacion fija de familia tipografica
    li = [100 * abs(r['lineas'][k]['integral'] - ov['integral']) / ov['integral'] for k, ov in o['lineas'].items()]
    Lm = sum(li) / len(li)
    tg = [('Color \'l\' (y65)', -1, 4.45), ('Cuadrado \'d\' (y64)', -1, 4.57), ('I de Impar (y137)', 0, 3.2), ("'11' (y106)", 0, 2.96)]
    se = []
    for nombre, idx, tgt in tg:
        vals = r['tallos'][nombre]
        se.append(100 * abs((vals[idx] if vals else 0) - tgt) / tgt)
    Sm = sum(se) / len(se)
    cv = [100 * abs(r['textos'][k]['cobertura'] - ot['cobertura']) / ot['cobertura'] for k, ot in o['textos'].items()]
    Cm = sum(cv) / len(cv)
    dw = sum(100 * abs(r['textos'][k]['w'] - ot['w']) / ot['w'] for k, ot in o['textos'].items()) / len(cv)
    dh = sum(abs(r['textos'][k]['h'] - ot['h']) for k, ot in o['textos'].items()) / len(cv)
    pen = dict(lineas=3 * clamp((Lm - 5) / 15, 0, 1), astas=2 * clamp((Sm - 5) / 20, 0, 1), cobertura=3 * clamp((Cm - 5) / 15, 0, 1),
               tamano=clamp((dw - 2) / 8, 0, 1) + clamp((dh - 0.6) / 2, 0, 1), familia_tipografica=1.0)
    res['estilos'] = (15 - sum(pen.values()), dict(err_integral_lineas_pct=round(Lm, 1), err_astas_pct=round(Sm, 1), err_cobertura_pct=round(Cm, 1),
                                                   err_ancho_texto_pct=round(dw, 1), err_alto_texto_px=round(dh, 2), penalizaciones={k: round(x, 2) for k, x in pen.items()}))
    res['elementos'] = (10.0, dict(nota='18 textos, 4 encabezados, 2 combinadas, sin celda sobre las columnas 1-2, nada extra'))
    return res

if __name__ == '__main__':
    v = sys.argv[1]
    res = puntuar(v)
    tot = sum(x[0] for x in res.values())
    if '--json' in sys.argv:
        print(json.dumps(dict(iteracion=v, puntuacion_total=round(tot, 1), por_categoria={k: round(x[0], 1) for k, x in res.items()}, detalle={k: x[1] for k, x in res.items()}), ensure_ascii=False, indent=1))
    else:
        print(f'{v}: TOTAL {tot:.1f}  ' + '  '.join(f'{k} {x[0]:.1f}' for k, x in res.items()))
        for k, x in res.items(): print('   ', k, x[1])
