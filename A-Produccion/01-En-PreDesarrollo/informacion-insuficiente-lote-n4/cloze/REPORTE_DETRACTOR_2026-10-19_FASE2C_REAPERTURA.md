# Auditoría Detractor FASE 2C (reapertura 2026-10-19): CLOZE informacion-insuficiente-lote-n4

Objeciones: 1 CORRECCIÓN, 1 DIAGNOSTICIDAD ALTA (pre-existente, no introducida), 2 MEDIAS.
Evidencia: harness.R / p6.R / diag.R / rend.R en este directorio; N = 100 por rama (regla #23), semillas 1-1017, 0 errores, HEAD y versión corregida sobre las mismas semillas.

## Objeción 1 (CORRECCIÓN): en la Parte 6 la clave sigue siendo falsa en el enunciado hipotético que el estudiante lee
El modelo `quitar_k()` y la frase `frase_quitar` describen enunciados distintos:
- F1, hecho L con T declarado (LT--, LTA-): la frase dice "sino únicamente su área", pero la frase del trazado ("paralelas al lado de 80 metros") y la de la carretera ("uno de sus lados de 80 metros") siguen nombrando un lado. Con área + un lado se calcula el otro, así que LOT-01..05 y POR-09 SÍ tienen respuesta. Render, semilla 3 (LTA-): 80 x 140; hipotético: 11 200 m2 y un lado de 80 m, de donde el otro lado es 140 y el cerco 440 m. La Solution dice "El contorno del lote no se deduce de su área". 82/500 (LT-- 42/100, LTA- 40/100). HEAD: 84/500.
- F2, hecho T con A declarado (-TAN, LTA-): `quitar_k("T")` borra también A, pero la frase solo quita el trazado y el enunciado del estudiante conserva "Las franjas se asignan ... empezando por la que da a la carretera". UBI-02 queda respondida literalmente (render, semilla 4). POR-06 y POR-07 tienen respuesta porque el texto dice "franjas"; POR-08 y POR-09 por inferencia, porque la franja "que da a la carretera" (en singular) fija la dirección, que es el mismo razonamiento de las líneas 288-293. 71/500 (UBI-02 15, POR-06 18, POR-07 5, POR-08 21, POR-09 12). HEAD: 68/500.
- Juntas: 75/100 en LTA-, 36/100 en -TAN, 42/100 en LT--. En esas versiones ninguna de las cuatro opciones pierde la respuesta bajo la lectura literal ("Con esa versión del enunciado -y solo esa-").
- La G10 lo deja pasar porque es léxica: comprueba el texto del motivo, no la verdad de la clave. Además, su `siguen_declarados` mantiene A cuando se quita T y `det_bajo(k2)` no: el código ya reconoce las dos lecturas.
- Corrección: construir el enunciado hipotético con el mismo constructor bajo k2 (`enunciado_k2 <- construir_enunciado(k2$L, k2$T, k2$A, k2$N)`) y mostrarlo citado en la Parte 6. Así la clave coincide por construcción con `det_bajo(DEP, k2)`. Aritmética, semilla 3: 11 200 m2 + "lado más corto"; 80 x 140 da cerco 440 y 70 x 160 da 460, de modo que queda indeterminado. Guarda G11: sin L en k2, ningún "\d+ metros" que no sea "cuadrados"; sin T, ninguna "franja"; sin A, nada de "se asignan". La versión actual debe morir con esa guarda. `siguen_declarados <- names(which(unlist(k_p6)))`. Hay que reescribir `motivo_sin["T"]` de UBI-02, porque también desaparece el orden del acta.

## Objeción 2 (DIAGNOSTICIDAD ALTA, pre-existente): P5 LTA-, "la clave es la opción más larga"
| | tasa | techo nulo | exceso | exceso con margen >= 15 % (§P7-B) |
|---|---|---|---|---|
| HEAD | 65,0 % | 22,8 % | +42,2 pp | +13,8 pp |
| corregida | 62,0 % | 22,0 % | +40,0 pp | +15,5 pp |
Delta de +1,7 pp, dentro del ruido (EE de la diferencia ~ 5 pp): no la causó el filtro. El salto 55,9 -> 71,0 no se reproduce. La causa es estructural: la clave mide 67,9 caracteres de media y los distractores 52,6. Supera +8 pp, así que obliga por §P7-A, o bien override firmado por §P7-E. Corrección: exigir un distractor con nchar >= 0,85·nchar(clave). Riesgo inverso de +1,8 pp (≈0,22 × (1/3 − 1/4)), por debajo de +5,3. Los demás estratos quedan en <= +5,8 pp explotables (P5 L--N: +12,2 bruto y −1,0 explotable).

## Objeción 3 (MEDIA): POR-01..05 y UBI-01 sin `motivo_sin`. Hoy son inalcanzables; si llegaran a ser clave, G10 aborta.
## Objeción 4 (MEDIA): auditoria_propia y verificar_p6_ubi01 no tienen ninguna sonda que contraste la clave de la Parte 6 con el enunciado literal.

## Verificado sin objeción
- Filtro (opción a): -T-N, LT-- y L--N son idénticas a HEAD en 100/100 cada una, y la canónica también. UBI-01 aparece 0 veces en -TAN y LTA-, y 74/76/49 veces en las demás ramas. Sin efectos en PARES_EXCLUYENTES ni en vecinos().
- `motivo_sin` en sus pares de dos dependencias: verdadero en el hipotético salvo F1 y F2. Ortografía, tildes y glifos sin defectos; sin letras.
- G10: 0 falsos positivos (500 + 1017 semillas).

VEREDICTO_DETRACTOR: RECHAZAR
