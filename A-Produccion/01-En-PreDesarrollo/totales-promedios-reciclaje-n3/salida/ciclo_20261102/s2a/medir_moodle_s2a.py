"""Tanda S2a (2026-11-03): en el XML de Moodle (n = 100, set.seed(1)), el signo que sigue al cierre de TODA fórmula
\\(…\\), con unidad o sin ella, y con cualquier separador que no sea letra ni cifra (hasta tres caracteres). Amplía la
métrica de Moodle de medir_pdf_moodle2.py (S1b), que solo miraba las fórmulas que terminan en su unidad.
Los caracteres especiales se construyen con chr(): este fuente no lleva escapes de Unicode.
Uso: python3 -I medir_moodle_s2a.py v=<ej.xml> [v=<ej.xml> ...]"""
import sys, re, collections
SIG = ".,:;!?)]" + chr(0xBB) + chr(0x201D) + chr(0x2026)
SIGC = "[" + re.escape(SIG) + "]"
SEP = r"[^A-Za-z0-9\n<\\]{0,3}"
CIERRE = r"\\\)(?:</span>)?"  # pandoc envuelve la fórmula en <span class="math inline">\(…\)</span>
P = {
    "formulas": re.compile(r"\\\("),
    "signo_tras_cierre": re.compile(CIERRE + SEP + SIGC),
    "unidad_tras_cierre": re.compile(CIERRE + SEP + r"(kilos?|cursos?)\b"),
    "unidad_dentro": re.compile(r"\\text\{(kilos?|cursos?)" + SIGC + r"?\}\\\)"),
    "signo_dentro_con_unidad": re.compile(r"\\text\{(kilos?|cursos?)" + SIGC + r"\}\\\)"),
    "signo_dentro_sin_unidad": re.compile(r"\\text\{" + SIGC + r"\}\\\)"),
    "math_mathml": re.compile(r"<math\b"),
    "esp_especiales": re.compile("[" + chr(0x2009) + chr(0x202F) + chr(0x2060) + "]"),
}
for arg in sys.argv[1:]:
    v, f = arg.split("=", 1)
    x = open(f, encoding="utf-8").read()
    preg = x.count("<question type=")
    c = {k: len(p.findall(x)) for k, p in P.items()}
    ej = collections.Counter(m.group(0)[-1] for m in P["signo_tras_cierre"].finditer(x))
    print(f"{v}: preguntas {preg} · " + " · ".join(f"{k} {n}" for k, n in c.items()) + f" · signos tras el cierre {dict(ej)}")
