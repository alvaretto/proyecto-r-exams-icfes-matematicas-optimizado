# Reporte de prueba (debe verificar)

| ID | Sev. | Evidencia |
|---|---|---|
| H1 | ALTA | sobrescribe ok |
| H2 | MEDIA | primera decisión fija |
| H3 | BAJA | no guarda el primer intento |
| H4 | BAJA | tabla con clase |

CITAS_DETRACTOR
H1 | tests/testthat/fixtures/citas_detractor/fuente.js:4 | S.casos[id] = {r: r, checked: true, ok: ok, ok1: ok1};
H2 | tests/testthat/fixtures/citas_detractor/fuente.js:2-3 | function guardar(S, hito) { if (S.p1 && S.p1.op !== op)
H3 | tests/testthat/fixtures/citas_detractor/fuente.js | AUSENTE: S.casos[id].ok1
H4 | tests/testthat/fixtures/citas_detractor/fuente.js:6 | var tabla = '&lt;table class="resumen"&gt;';
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS
