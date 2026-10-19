# FASE 2C de confirmación (2026-10-19): CLOZE informacion-insuficiente-lote-n4

**Objeciones:** 0 de CORRECCIÓN, 0 ALTAS, 1 MEDIA (O-A), 1 de DIAGNOSTICIDAD que ya existía antes y queda fuera de la batería congelada (O-B), 2 BAJAS (O-C, O-D).

**Muestra (harness propio, en este directorio):**
- gen.R genera los datos. Las muestras son las semillas 1-1500 de `v4.Rmd` (copia exacta del `.Rmd` vigente) y de `p2.Rmd` (el `cur.Rmd` de la pasada 2). Hubo 0 errores en las dos.
- Las cifras de referencia usan N = 100 por rama (regla #23). La canónica sale 95 veces en 1500 semillas y se mide completa.
- Algunas cifras sobre las 1500 semillas se dan solo como confirmación, y van marcadas como «completa».
- Detector de fuga: an.R. Oráculo: orac.R. Render: rend.R.

## 1. ¿O-1 queda cerrada? SÍ
Detector propio, independiente de G12. Extraigo con una regex propia todos los números de `enunciado_p6` que no están en `enunciado`, y busco entre ellos `exsol_p2` y `exsol_p3`.

| rama (N = 100) | copia en p2 | copia en v4 |
|---|---|---|
| L--N | 16 | 0 |
| LT-- | 8 | 0 |
| LTA- | 17 | 0 |
| -T-N, -TAN, canónica | 0 | 0 |

- En la muestra completa (1500 semillas): 0 copias y `p6_L_forzado` = 0 en todas las ramas. La pasada 2 había medido 16/8/17 en p2, y mi detector reproduce exactamente esas cifras.
- El único número nuevo que introduce el texto citado sigue siendo el área cuando el dato quitado es L (311/311 casos).
- Efectos del reordenamiento (todos medidos):
  - El nivel de exclusión de P6 es 1 en el 100 % de las versiones de todas las ramas, tanto en v4 como en p2. No aumentan las repeticiones entre P1 y P6 (0 %).
  - Dato quitado L, por rama: L--N baja de 49,1 % a 34,7 %, LT-- de 42,7 % a 36,7 % y LTA- de 39,9 % a 28,1 %. -T-N, -TAN y la canónica no cambian. El dato quitado es visible para el estudiante, así que esta proporción no es un canal.
  - Cuota máxima de una misma clave de P6 (muestra completa):
    - L--N: de 14,0 % a 16,2 %.
    - LT--: de 24,7 % a 30,7 % (POR-06).
    - LTA-: de 34,2 % a 39,7 % (UBI-02).
    - Por par (rama, dato quitado), la cuota máxima varía como mucho 5 pp.
  - UBI-02 es la clave en el 100 % de las versiones de A en -TAN y LTA-, igual que en p2. Es estructural: la única pregunta que pierde la respuesta sin la asignación es la de la asignación.
  - Cambia la clave con el mismo dato quitado: 5/1500.
  - Canal «clave = la opción que también se preguntó en P2/P3»: en -T-N las cifras son idénticas a p2; en L--N pasa de 13,6 % a 8,7 % y en LT-- de 40,0 % a 34,5 %. El reordenamiento no lo abre (ver O-B).

## 2. ¿G12 es una guarda real? SÍ
- Puede fallar:
  - El mutante M5 del verificador muere en la semilla 10.
  - Mi mutante M6, sin la exclusión `cods_area` en el bucle `rt`, muere por G12 en 5/1500 semillas; la primera es la 84.
  - El bucle `rt` vuelve a elegir con dato quitado L en 73/1500 versiones. Esa línea es portante, y G12 es lo único que la protege.
- La excepción mide la causa: L solo llega en la segunda pasada, cuando ya se agotaron los demás datos en los tres escalones. No mide el síntoma.
- El oráculo del verificador:
  - Es bidireccional y mata a M4 por sí solo.
  - Su `datos_texto` detecta L solo con «metros de largo y». Es ciego al modo F1 de la reapertura (un lado suelto más el área): ese modo lo atrapa G11, no el oráculo (ver O-C).
  - Usa el `DEP` y el `det_bajo` del propio `.Rmd`, así que no puede detectar un `DEP` mal declarado.
- Mi oráculo textual tiene regex propias: L con dos medidas lineales o con un lado suelto más el área; T con franja, paralel o corte; A con «se asignan», «empezando por» o «en el orden en que»; N con cualquier entero que no sea una medida. Resultado: 0 violaciones en las 1500 versiones de v4 (las mismas 0 en p2) y en los 14 pares (rama, dato quitado). En todas: texto = `k_p6`, original = `K_ACTUAL`, clave sin respuesta, distractores con respuesta, 0 lados sueltos.

## 3. Regresiones: ninguna
- CORRECCIÓN de P6 en los 14 pares: 0/1500 (punto 2).
- P5 (N = 100), clave = opción más larga, en bruto y explotable con margen ≥ 15 %:

| rama | bruto | explotable | sin restricción |
|---|---|---|---|
| LTA- | 59 % | 1 % | 1 % |
| L--N | 39 % | 0 % | 0 % |
| -T-N | 29 % | 0 % | 0 % |
| -TAN | 29 % | 0 % | 0 % |
| LT-- | 19 % | 0 % | 0 % |
| canónica | 10,5 % | 0 % | 0 % |

   Es lo mismo que en p2, ±1 pp.
- UBI-01 aparece 0 veces en -TAN y en LTA-. En el resto: -T-N 75, LT-- 74, L--N 50 y la canónica 95/95.
- La canónica está intacta: un único enunciado («…50 metros de largo y 120 metros de ancho… sus 8 hijos.») y P1 = {UBI-01, FIJ-01, LOT-01, FIJ-02} en las 95.
- Render de las semillas en las que cambió el dato quitado: 10 (L→N, L--N), 44 (L→T, LTA-) y 139 (L→A, LTA-).
  - HTML, PDF, DOCX y Moodle: correctos en las tres semillas.
  - El blockquote aparece en todos los formatos: 1 `<blockquote>` por XML, `\begin{quote}` en el PDF y bloque de cita en el DOCX.
  - «Opción [A-D]» en el XML: 0.

## 4. Lo que cambió (O-3 y O-4)
- s44 (T e `info_A`): «no dependen de los datos que se quitaron» y «no depende de los datos que se quitan».
- s10 y s139: en singular.
- «sino solo su área» y «que dijera exactamente esto»: tildes correctas.
- La Solution no depende de letras.
- Ninguna objeción.

## O-A (MEDIA; es de la misma familia que O-1 y ya existía en p2): la Parte 6 sigue dando un atajo de un paso
**Qué se cuestiona.** G12 y `cods_area` solo cubren el valor exacto del área. FIJ-03 (área de cada porción = área/n) y FIJ-05 (2·área/n) se resuelven con una sola división a partir del área que imprime el texto citado; n sigue declarado en L--N y en la canónica.

**Evidencia.**
- Semilla 22 (L--N): P2/P3 piden FIJ-05 y LOT-04. El enunciado dice «60 metros de largo y 90 metros de ancho… 5 hijos». La Parte 6 cita «tiene 5.400 metros cuadrados de área… sus 5 hijos». La respuesta es 2·5400/5 = 2160, sin calcular 60·90.
- Frecuencia con N = 100: canónica 40/95, L--N 29/100, demás 0. Es idéntica en p2, así que O-1 no la introdujo, pero tampoco la cubre.
- En la muestra completa: (40 + 53)/1500 = 6,2 % de las versiones.

**Por qué MEDIA y no ALTA.** No imprime la respuesta: ahorra la multiplicación L·W, y la división sigue haciéndose. En -T-N y -TAN, FIJ-03 ya es área/n por diseño.

**Alternativa medida** (mutante `pr.Rmd`): `cods_area` = los códigos con `num_unidad == "metros cuadrados"` (FIJ-01, 03, 05 y 06).
- Atajo: 0/95 en la canónica y 0/222 en L--N. Errores: 0/1500.
- Coste: dato quitado N en el 86,3 % de la canónica y en el 88,7 % de L--N. La cuota máxima de la clave de P6 en la canónica sube de 23,2 % a 44,2 % (FIJ-05), con solo 2 claves posibles cuando el dato quitado es N.
- Recomendación: no aplicarla a ciegas. Declarar el residuo en el HANDOFF (decide el profesor) o medir otra opción: elegir de nuevo P2/P3 sin códigos en m² cuando `hecho_p6 == "L"`. Esta segunda opción no está medida.

## O-B (DIAGNOSTICIDAD; ya existía, el cambio no la abre y queda fuera de la batería congelada, §P7-C): la clave de P6 coincide con la pregunta que se hizo calcular en P2/P3
**Regla.** Cuando exactamente una opción de P6 repite una pregunta de P2/P3, elegirla. Bajo permutación de la clave, el valor nulo es 25 %.

| rama | N = 100 | completa |
|---|---|---|
| -T-N | 20/44 = 45,5 % (+20,5 pp) | 36/87 = 41,4 % |
| LT-- | 20/58 = 34,5 % (+9,5 pp) | 32/82 = 39,0 % |
| LTA- | 32,1 % | 31,8 % |
| -TAN | 35,0 % | 26,5 % |
| canónica | 29,4 % | 29,4 % |
| L--N | 8,7 % (inverso) | 17,5 % |

- Agregado sin la canónica: 204/674 = 30,3 % (+5,3 pp).
- Mecanismo en -T-N: FIJ-05 con dato quitado N (20 casos) y POR-07 (16).
- En -T-N es idéntico a p2. No lo medí en HEAD.
- Por rama supera +8 pp en -T-N (y en LT-- con N = 100). Pero la regla es nueva a mitad de ciclo (§P7-C) y aplicarla cambiaría la vara. La declaro para que decida el profesor y para el próximo ciclo; no bloquea esta confirmación.
- **Alternativa (sin medir):** excluir `cods[par_23]` de `cand_p6_*` y añadir `cods[idx_p6]` a `ex23` en el bucle `rt`. Así P6 nunca repetiría P2/P3 y el canal quedaría en 0 por construcción. Antes hay que medir los errores de generación y la cuota de P6.

## O-C (BAJA): huecos del verificador
1. No tiene un mutante para la exclusión del bucle `rt`. Añadir M6: `if (identical(hecho_p6, "L")) ex23 <- …` → `NULL`. Muere por G12 en la semilla 84.
2. Su `datos_texto` no deduce L de «un lado suelto + área», que es el modo F1 de la reapertura. Añadir `L = … || (length(unique(lineales)) == 1 && grepl("metros cuadrados", txt))`, para que M3 muera también por el oráculo y no solo por G11.

## O-D (BAJA): la tolerancia `p6_L_forzado` deja pasar una fuga que se puede evitar
Cuando es TRUE y `rt` = 1 es suficiente, P2/P3 conservan el área. La alternativa es volver a elegir `par_23` sin `cods_area` en cuanto `p6_L_forzado` sea TRUE (el mismo código que la rama `rt > 1`), y dejar G12 sin excepción. Frecuencia medida: 0/1500.

## Fuera de alcance, ya conocido
- FASE 2D: 'codigo'.
- WARN_DIV_ESTATICA.
- Objeciones 4, 8 y 10, N2 y N3.
- La rama «quitaron/quitan» (tiempo verbal) ya existía y no la cuento.

## Veredicto
- O-1 queda cerrada y verificada con un detector independiente: copias 0/1500, antes 8,2 %.
- G12 es una guarda real.
- CORRECCIÓN: 0/1500.
- Regresiones: ninguna.
- Ningún hallazgo es CRÍTICO ni ALTO, así que nada bloquea la FASE 3. Los cambios que pido:
  - declarar O-A y O-B en el HANDOFF, con sus cifras, para que decida el profesor;
  - añadir M6 y la inferencia del lado suelto al verificador.

VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS
