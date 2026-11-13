## Pasada 4, pendiente 1: medición pareada del desborde horizontal de las ecuaciones de la Solution (mismas semillas antes y
## después, 360 y 320 px, N = 100 por ancho). Cada versión va en una caja del ancho dado (padding 8 px) dentro de una sola
## página; chromium sin interfaz cuenta las cajas de ecuación (style overflow-x) con scrollWidth > clientWidth.
## Uso: python3 -I medir_desborde_360_320.py <dir_base> <variante> <ancho>
import re, sys, subprocess, json, os
base, variante, ancho = sys.argv[1], sys.argv[2], int(sys.argv[3])
files = [os.path.join(base, variante, f"v{i}.html") for i in range(1, 101)]
head = re.search(r"<head>(.*?)</head>", open(files[0], encoding="utf-8").read(), re.S).group(1)
cajas = []
for i, f in enumerate(files, 1):
    b = re.search(r"<body[^>]*>(.*)</body>", open(f, encoding="utf-8").read(), re.S).group(1)
    cajas.append(f'<section class="ver" data-v="{i}" style="width:{ancho}px;box-sizing:border-box;padding:8px;margin:0 0 40px 0">{b}</section>')
js = ("<script>window.addEventListener('load',function(){setTimeout(function(){var out=[];"
      "document.querySelectorAll('section.ver').forEach(function(s){var n=0,mx=0;s.querySelectorAll('div').forEach(function(d){"
      "var st=d.getAttribute('style')||'';if(st.indexOf('overflow-x')>=0){var e=d.scrollWidth-d.clientWidth;if(e>1){n++;if(e>mx)mx=e;}}});"
      "var pg=s.scrollWidth-s.clientWidth;out.push([+s.dataset.v,n,mx,pg>1?pg:0]);});"
      "var p=document.createElement('pre');p.id='RES';p.textContent=JSON.stringify(out);document.body.appendChild(p);},500);});</script>")
fn = os.path.join(base, f"comb_{variante}_{ancho}.html")
open(fn, "w", encoding="utf-8").write(f"<!DOCTYPE html><html><head>{head}</head><body style='margin:0'>{''.join(cajas)}{js}</body></html>")
dom = subprocess.run(["chromium", "--headless", "--disable-gpu", "--window-size=1024,800", "--virtual-time-budget=5000", "--dump-dom", "file://" + fn],
                     capture_output=True, text=True, timeout=170).stdout
res = json.loads(re.search(r'<pre id="RES">(.*?)</pre>', dom, re.S).group(1))
print(f"{variante:9s} {ancho} px: versiones con alguna ecuación que exige desplazamiento {sum(1 for r in res if r[1] > 0)}/{len(res)}; "
      f"ecuaciones afectadas {sum(r[1] for r in res)}; exceso máximo {max([r[2] for r in res] + [0])} px; versiones con desborde de página {sum(1 for r in res if r[3] > 0)}")
