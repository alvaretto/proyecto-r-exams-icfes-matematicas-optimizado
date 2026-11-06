# HANDOFF — rapidez-media-error-paso

Ejercicio nuevo, **desde cero**, a partir del ítem 09 de Evaluar para Avanzar 2023, Matemáticas 11-2. Sustituye a
`2023-Matematicas-11-2-09-Op-M.Rmd`, que está en `03-En-Produccion/` y es inmutable. Fuente verbatim:
[`ITEM_OFICIAL.md`](ITEM_OFICIAL.md). Invariantes: [`.claude/CLAUDE.md`](.claude/CLAUDE.md). Sin gráfica, por decisión
del profesor tras mirar el recorte.

## Estado (2026-11-05, aprobado, 11 de 11)

- **11 de 11 pasos sellados.** El profesor firmó y aprobó el 2026-11-05: «Firmo las 64 filas, acepto las 6 623 combinaciones como residuo declarado y apruebo.» La firma está en O-2. Aprobar no promueve: falta la evidencia de aula.
- `.Rmd` md5 `904af1ca1271d97c54123218c9db03c0`: variante C (`EV-D-PROM`, `EV-T-CUENTA`, obligación de L-9) y L-8 sobre operandos. Es el md5 final de esta ronda.
- `verificar_espacio.R` md5 `c80508095d755e7668981768f50270e9`, `salida/verificar_r13.txt`: **exit 4, «ROJO, listado por O-2»**, nunca PASS, 64 listadas, 0 pendientes, 0 no reproducen, 0 errores. Esa ruta se corrió real por primera vez tras la firma. Oráculo 300 + canónica sin problemas; 9 564 estados, 131 586
  conjuntos, 0 abortos; 123 filas iguales a `opcion_c/c2.json`; 36 de 36 mutantes por su sonda, 132 en copia, 2 sobre el generador y V1 a V6.
- **FASE 2C cerrada con la firma abierta.** Ronda 4, detractor nuevo: `APROBAR_CON_CAMBIOS`, 0 defectos de CORRECCIÓN, 3 altas y 4 medias, ninguna pide cambiar el `.Rmd`
  (`salida/fase2c_r4_detractor.md`, 35 citas, 0 fallos). `detractor_fase2c` se selló por orden del coordinador, porque lo único abierto es la firma.
- **Pasos 8, 9 y 10 sobre este md5:** 5 coherencias OK; stresstest, diversidad, diagnosticidad y multisemilla en PASS; literalidad y coherencia matemática con 0 fallos.
- **Render y 6b:** 5 formatos con exit 0, arsenal 0 errores y 7 advertencias, auditoría `APTO_VISUAL`.
- **FASE 2C-bis:** no corrió. El coordinador detuvo `/detractor-hetero` cuando auditaba el `.Rmd` anterior.

**Próximo paso:** llevarlo al aula. Un cambio al `.Rmd` deja la firma sin efecto: el verificador da exit 3 y hay que medir de nuevo.

**Qué está en git.** `.gitignore` ignora `**/salida/`, así que la evidencia entró con `git add -f`, por nombre: la tabla firmada, la corrida `verificar_r13.txt`, los reportes de la FASE 2C, los pasos 8 a 10 y `medicion_exacta/opcion_c/` y `det4/`. El verificador **lee** `opcion_c/`: sin esos archivos no corre. Lo demás de `salida/` se regenera y no se versiona.

## Lo que decidió el profesor (resuelto el 2026-11-05)

1. **Firmar las 64 filas de `salida/tabla_firma_o2.md`**, regla por regla: 33 reglas, 31 filas en no canónicas y 33 en mezcla. Solo una es de la batería congelada, `MAG-segundo-afirmado` en la mezcla, +8,36.
2. **Qué hacer con las 6 623 conjunciones de hasta 3 cláusulas que pasan de +8 pp** en la búsqueda de la r4. No se pueden listar, y §P7-E no admite tope por familia. O las firma como conjunto declarado no agotado, o decide otra cosa.
3. **Si la lectura de las dos causas estructurales le basta.** En los pasos 1 y 2 la clave afirma menos de lo que rechaza siempre, y `EV-T-CUENTA` se descarta a ojo en el 46,85 % de las versiones, porque se lee «N + 1 y no N».

**Qué cambió de D a C, exceso en no canónicas:**

| Regla | Variante D | Variante C | Exige calcular |
|---|---|---|---|
| `EST` sin rótulos | +22,15 | +4,53 | no |
| `X0` | +14,49 | +5,15 | no |
| `X0+` | +23,16 | +8,27 | no |
| `EST+` | +25,21 | +5,36 | no |
| `E1` | +26,76 | +6,65 | no |
| `E2` | +53,68 | +17,51 | no |
| `E3` | +71,20 | +24,49 | no |
| `M2` | +56,76 | +51,20 | no |
| `M3` | +71,20 | +61,88 | no |
| `RE` | +21,20 | +21,20 | no |
| `U` | +13,72 | +13,72 | no |
| `M0` | +46,20 | +46,20 | no |
| `NM` | +10,42 | **+10,62** | no |
| `X1a` | +7,35 | +5,07 | sí |
| `X1b` | +12,42 | **+15,51** | sí |
| `X1` | +29,03 | **+29,43** | sí |
| `R10-150` | +18,58 | +12,32 | sí |
| `R20-120` | +30,15 | +19,58 | sí |
| `R20-120+ent` | +41,25 | +36,67 | sí |
| `R30-90` | +43,32 | +30,59 | sí |
| Solo queda la clave con `R20-120` | 26,2 % | 12,0 % | sí |
| Solo queda la clave con `R30-90` | 47,6 % | 26,8 % | sí |

Suben `NM`, `X1b` y `X1`. No se mueven `RE`, `U` ni `M0`: el impreso marca 100 % en `U` y en `M0`. Batería de 51 reglas, máximo exacto: no canónicas `DIV-mult10` 36,54 %, +7,74; mezcla `MAG-segundo-afirmado` 37,16 %, +8,36.

**Lo que ninguna cifra mide:** dificultad real. El Nivel 3 sigue sin evidencia de aula.

## Objeciones de la FASE 2C r4

| ID | Severidad | Hallazgo | Estado |
|---|---|---|---|
| H1 | ALTA | La enumeración no lee lo que sortea `pick` ni lo que corre tras `elegidos`: 3 mutantes del reparto dejaban la tabla idéntica | Atendida: `RMD_MD5` da exit 3 si el `.Rmd` cambia; V5, V6; la cabecera dice qué queda atado y qué no |
| H2 | ALTA | La glosa de `NM` no decía lo que mide el código | Atendida en la tabla, sin volver a medir |
| H3 | ALTA | Reglas sin cálculo > +8 pp fuera de la tabla | Atendida en parte: 15 reglas medidas en R, a la centésima, en `FIRMA_O2` y en la tabla. Las 6 623 quedan para el profesor |
| H4 | MEDIA | Las sondas de los 4 mutantes re-enumerados no discriminaban | Atendida: O2-A reproduce `c1.json` y O2-B `base.json`, 26 filas y 0 diferencias en cada uno |
| H5 | MEDIA | El modo `enumerar` saltaba las razones nuevas | Atendida |
| H6 | MEDIA | La guarda L-9 repetía la línea de arriba y no había oráculo | Atendida: `ORACULO_L9`, M25b |
| H7 | MEDIA | Punteros viejos en `.claude/CLAUDE.md` | Atendida; «Residuos vigentes» reescrita, con `M0` y `RE` |

**Sin auditar por un detractor:** lo hecho después de la r4. El `.Rmd` no cambió, pero el verificador pasó de `35cf38e9` a `3a84582d`, y las 15 glosas nuevas de la tabla solo las contrastó el orquestador.

## Revisión visual (md5 `904af1ca`)

**Por programa, N = 100 a 360 y 320 px** (`salida/auditoria_visual_r4.md`): 0 desbordes, 0 textos rotos, 4 opciones distintas, una sola «Correcta.» y 0 negativos en los pasos.

**Por imagen, 16 versiones a 360 y las mismas 16 a 320 px, leídas por el auditor:**

| Versión | Qué tiene |
|---|---|
| v01 | `EV-D-FINAL`, `EV-T-CUENTA` |
| v02 | `EV-T-FINAL`, `EV-D-PROM` |
| v03 | `EV-C-UNIDAD`, las dos razones nuevas |
| v04 | `EV-C-UNIDAD`, `EV-D-PROM` |
| v05 | `EV-D-SUMA`, sin razón nueva |
| v07, v27, v47, v54, v66 | canónicas |
| v08 | la de fórmula más ancha, 181 px |
| v09 | `EV-C-UNIDAD`, `EV-T-CUENTA` |
| v13 | `EV-D-FINAL`, `EV-D-PROM` |
| v18 | `EV-T-FINAL`, `EV-T-CUENTA` |
| v25 | `EV-D-SUMA`, `EV-T-CUENTA` |
| v35 | `EV-D-SUMA`, `EV-D-PROM` |

**Orquestador:** Read de `salida/preview_…_q-1`, `q-2`, `s-2` y `s-3` sobre este md5. No abrió `s-1` ni el NOPS de este md5. **Contact sheets:** no se leyeron; el control de las 100 es por programa. Las capturas están en el scratchpad de la sesión.
**Sin auditar por nadie:** el DOCX abierto en Word y los escaneos NOPS.

## Evidencia

- `salida/tabla_firma_o2.md`, `salida/firma_o2_medida.csv`, `salida/verificar_r12.txt`.
- `salida/fase2c_r4_detractor.md` y `salida/medicion_exacta/det4/`, con la búsqueda de 96 568 conjunciones y su control permutado.
- `salida/auditoria_visual_r4.md`, `salida/paso8_coherencias.md`, `salida/paso9_*.txt`, `salida/paso10_*.txt`.
- Copias previas: `salida/medicion_exacta/rmd_md5_25a924d4.Rmd.txt`, `rmd_md5_2dfec0c9.Rmd.txt`, `verificar_md5_41198f5f.R.txt`.

## Histórico: FASE 2C r3, base de la decisión O-2

La comparación de abajo es exacta, sobre los dos diseños. La midió el detractor con un enumerador propio en Python y la repitió el orquestador en
R, con el generador real de cada `.Rmd`. Coinciden a la centésima salvo en `RE`, explicado bajo la tabla. Control positivo: la corrida en R del
diseño anterior da `Q` 45,83 %, `X0` 40,55 % y `X1a` 46,43 %, las cifras conocidas.

**H1. Una cifra firmada no mide lo que dice su glosa.** `EST-afirmado-nuevo` se firmó como «la cifra afirmada no aparece en el enunciado ni en los
pasos», pero el código cuenta los rótulos «Paso 1.», «Paso 2.» y «Paso 3.» como cifras vistas. Una clave de 2 h o 3 h queda descartada aunque ese
dato no aparezca en ningún lado. Sin ese artefacto: **50,95 %, +22,15 pp**, no 37,21 %, +8,41 pp.

**El argumento que decidió la variante D se invierte.** Decía que la peor regla sin cálculo bajaba de +17,03 a +14,49 pp. Medida sin artefacto,
sube de +17,03 a +22,15 pp. Lo que sí es cierto: `Q` acertaba el 100 % en el 27,8 % de las versiones, y con la variante D ya no aplica.

**H2. Hay reglas sin cálculo por encima de +8 pp sin firma.** `U` es la análoga directa de `Q`. `E3` y `M3` llegan al 100 % porque las razones
falsas de distancia y tiempo son siempre un negativo, un dato repetido o una suma que excede el dato mayor, y las del cociente siempre van en
h/km o dan una fracción menor que 1: la clave se halla sin obtener nunca la distancia ni el tiempo. Son conjunciones de 4 o 5 cláusulas que nadie
enunciaría entera, pero cada cláusula es natural. El impreso cae igual: en `U`, `X0+`, `E1`, `E3` y `M0` da 100 %. **Las únicas de la tabla en las
que el ítem generado queda peor que el impreso son `EST`, `EST+`, `RE` y `NM`.**

**H3. `X1a` pasa de +8 pp en la mezcla, y no se clasifica.** 36,84 %, +8,04 pp. `.claude/CLAUDE.md` dice «a 0,65 pp de +8», que solo vale en no canónicas.

### Comparación exacta, por diseño

Tasa exacta y exceso en pp sobre el techo de 28,8 %, en no canónicas. «Mezcla» es la de la variante D con `P_CANON` = 0,05: lo que recibe el estudiante.

**Sin cálculo:**

| Regla | Qué hace quien la usa | Con ramas de signo (md5 `2dfec0c9`) | Variante D (md5 `25a924d4`) | Mezcla D | Impreso |
|---|---|---|---|---|---|
| `Q` | elige la que solo quita el menos a lo que muestra el paso | 45,83 %, **+17,03** | 25,00 %, no aplica | 25,00 % | 25 % |
| `EST-afirmado-nuevo` | la regla firmada, con el artefacto de H1 | 27,20 %, −1,60 | 37,21 %, +8,41 | 36,59 %, +7,79 | 25 % |
| `EST` sin rótulos | descarta la cifra ya vista en el enunciado o en los pasos | 34,07 %, +5,27 | 50,95 %, **+22,15** | 49,65 %, +20,85 | 25 % |
| `RE` | elige la que rechaza un dato del enunciado | 38,89 %, +10,09 | 50,00 %, **+21,20** | 48,75 %, +19,95 | 25 % |
| `U` | elige la que solo cambia las unidades | 41,00 %, +12,20 | 42,52 %, +13,72 | 45,39 %, +16,59 | 100 % |
| `NM` | descarta la que afirma más de lo que rechaza | 30,97 %, +2,17 | 39,22 %, +10,42 | 38,93 %, +10,13 | 33,3 % |
| `X0` | descarta la cifra que repite un dato del enunciado | 40,55 %, +11,75 | 43,29 %, +14,49 | 43,63 %, +14,83 | 50 % |
| `X0+` | `X0`, y además sin negativas | 49,58 %, +20,78 | 51,96 %, +23,16 | 54,36 %, +25,56 | 100 % |
| `EST+` | `EST` sin rótulos, y además sin negativas | 35,96 %, +7,16 | 54,01 %, +25,21 | 52,56 %, +23,76 | 25 % |
| `E1` | positiva, no repite dato, no supera el dato mayor de su magnitud; no toca el paso 3 | 55,56 %, +26,76 | 55,56 %, +26,76 | 57,78 %, +28,98 | 100 % |
| `E2` | `E1`, y sin fracciones h/km | 84,00 %, +55,20 | 82,48 %, +53,68 | 83,36 %, +54,56 | 100 % |
| `E3` | `E2`, y sin fracción menor que 1 | 100 %, +71,20 | 100 %, +71,20 | 100 % | 100 % |
| `M0` | elige en el paso con la marca (`+`, `− 0`, `h/km`; en el diseño anterior, también el negativo) | 75,00 %, +46,20 | 75,00 %, +46,20 | 76,25 %, +47,45 | 100 % |
| `M2` | `M0`, positiva y sin repetir dato | 84,43 %, +55,63 | 85,56 %, +56,76 | 86,28 %, +57,48 | 100 % |
| `M3` | `M0`, `E1` y fracción km/h mayor que 1 | 100 %, +71,20 | 100 %, +71,20 | 100 % | 100 % |

- **`RE`:** las cifras de la tabla son las de la r3. Mi corrida en R dio 34,72 % y 43,75 % porque contó también la fracción del paso 3 cuando empieza por un
  dato, y esa fracción no rechaza un dato. La lectura literal se comprueba a mano por rama: en la variante D, 25 / 75 / 25 / 75 con pesos 1/3, 1/6, 1/6 y 1/3
  dan 50,00 %. No se re-enumeró en R.
- **`U` y `NM`:** la columna del impreso y su mezcla son las de la r3. Mi corrida no evalúa la opción impresa sin «y no».
- `MU` (+54,53) y la búsqueda de 20 875 conjunciones (mejor de un rasgo, +18,82; de hasta tres, +44,02; con la clave permutada, 25,27 %) solo están en el reporte r3.

**Por rama, variante D** (`EV-C-UNIDAD` 33,3 % / `EV-D-FINAL` 16,7 % / `EV-D-SUMA` 16,7 % / `EV-T-FINAL` 33,3 %):

| Regla | C-UNIDAD | D-FINAL | D-SUMA | T-FINAL |
|---|---|---|---|---|
| `EST` sin rótulos | 10,7 | 70,0 | 75,6 | 69,4 |
| `RE` | 25 | 75 | 25 | 75 |
| `U` | 100 | 13,4 | 12,5 | 14,6 |
| `NM` | 47,9 | 36,4 | 33,4 | 34,9 |
| `X0+` | 53,0 | 49,1 | 41,0 | 57,9 |
| `EST+` | 10,7 | 74,6 | 83,9 | 72,1 |
| `E1` | 75,0 | 45,8 | 45,8 | 45,8 |
| `M0` | 75 | 75 | 75 | 75 |
| `M2` | 75,0 | 88,7 | 100 | 87,3 |

**Con cálculo (sustituir en el paso y recalcular):**

| Regla | Con ramas de signo | Variante D |
|---|---|---|
| `X1a`, positiva y en km/h | 46,43 %, +17,63 | 36,15 %, +7,35 (mezcla 36,84 %, +8,04) |
| `X1b`, entera | 38,82 %, +10,02 | 41,22 %, +12,42 |
| `X1`, las dos | 63,40 %, +34,60 | 57,83 %, +29,03 |
| `R10-150` | 55,08 %, +26,28 | 47,38 %, +18,58 (mezcla 47,51 %, +18,71) |
| `R20-120` | 65,47 %, +36,67 | 58,95 %, +30,15 |
| `R20-120+ent` | 74,03 %, +45,23 | 70,05 %, +41,25 |
| `R30-90` | 76,71 %, +47,91 | 72,12 %, +43,32 |
| Solo queda la clave con `R20-120` | 37,6 % | 26,2 % |
| Solo queda la clave con `R30-90` | 56,0 % | 47,6 % |

**Batería congelada de 51 reglas, máximo:** en no canónicas, +6,80 con ramas de signo y +8,41 en la variante D. En la mezcla, +6,68 y +8,87.

Evidencia: `salida/medicion_exacta/comparar_r3.R` y `comparar_r3.out` (el verificador con estas reglas añadidas, solo como medición), y `salida/medicion_exacta/det3/`.
`salida/medicion_exacta/rmd_md5_2dfec0c9.Rmd.txt` es el `.Rmd` anterior: se reconstruyó deshaciendo las 11 ediciones y su md5 coincide. Sirve de diff para quien audite.

### Opciones que se le dieron al profesor

Recomendación del coordinador: C, y si no se autoriza, A. En cualquiera de ellas hay que decidir también si `− 0` cuenta como constructo (H8): está en el 50 % de las versiones de la variante D, el
impreso no lo tiene, y basta ver que se resta algo que no hace nada.

**A. Mantener la variante D y volver a firmar.** O-1 se enmienda: se corrige la glosa de `EST-afirmado-nuevo` y se firman, regla por regla, `EST` sin rótulos, `RE`, `U`,
`NM`, `X0+`, `EST+`, `E1`, `E2`, `E3`, `M0`, `M2`, `M3`, `MU` y `X1a` en la mezcla, con su ámbito. Sigue sin `Q`. Se acepta que `EST` y `RE` queden por encima
del impreso. Después: H3 a H9, un detractor nuevo porque H6 cambia el md5, y los pasos 8, 9 y 10.

**B. Volver al diseño con ramas de signo (md5 `2dfec0c9`).** Se deshace L-8 y vuelve `Q`, +17,03, con el 100 % en sus dos ramas. `EST` baja a +5,27, `RE` a +10,09 y `NM` a
+2,17; `X1a` sube a +17,63 y las R suben. Hay que firmar otra tabla, porque O-1 no cubre ese diseño, y reaplicar H1 y H3 de la r2. Después, el mismo camino que en A.

**C. Autorizar otra variante, medida en modelo por el coordinador (2026-11-05, 16:14).** Sería una excepción nueva a §P7-D. Añade dos razones falsas
nuevas, positivas y dentro de la cota, y exige ofrecer al menos una cuando exista (92 % del peso): `EV-T-CUENTA`, duración + 1 (contar las horas del reloj
incluyendo las dos puntas), y `EV-D-PROM`, promedio de las dos lecturas (solo si es entero y distinto de `D`, `Dm`, `d1` y `d2`). Sin esa obligación apenas sirve:
`EST` queda en +16,39. Con ella, sobre no canónicas: `EST` sin rótulos +4,53, `X0` +5,15, `E1` +6,65, `E3` +24,49, `entre` +6,70, `R30-90` +30,59. No cambian
`RE`, `U`, `NM` ni `M0`. Sube `X1b` a +15,51. 0 conjuntos con dos claves o duplicados en 131 586. **Límites:** es el modelo Python de la r3, que reproduce D a la
centésima, no el `.Rmd`; las razones no tienen texto; no se midió la batería de 51 reglas; la búsqueda de reglas nuevas llegó a 2 condiciones. Evidencia:
`salida/medicion_exacta/opcion_c/`. Lo que quede sobre +8 pp se firma después, con las cifras exactas del `.Rmd`. La historia de §P7-D advierte que perseguir la
diagnosticidad fue lo que volvió falsa una clave: toda razón nueva lleva guarda y mutante. Cuesta un ciclo, igual que A, porque H6 ya obliga a otro detractor.

**Hallazgo del coordinador, no de la r3.** La regla de una condición `entre` («elige la cifra que queda entre los dos datos del enunciado») da 56,19 %, +27,39 pp,
en D y con ramas de signo. La r3 no la vio porque su búsqueda no tenía ese rasgo. **`RE` tiene dos lecturas exactas:** +21,20 sin contar la fracción del paso 3,
+14,95 contándola.

**D. No aprobar el ejercicio.** Queda sin usar en el aula y `Op-M` sigue en producción. No hay más trabajo.

### Objeciones de la FASE 2C r3, todas atendidas o sustituidas por las de la r4

Ninguna está atendida. Detalle y reproducción en `salida/fase2c_r3_detractor.md`.

| ID | Severidad | Qué pasa | Qué pide | Cambia el `.Rmd` |
|---|---|---|---|---|
| H1 | ALTA | `EST-afirmado-nuevo` cuenta los rótulos: +22,15, no +8,41 | Vigilada `EST` sin rótulos, sin tocar la batería congelada; corregir la glosa; firmar de nuevo | No |
| H2 | ALTA | Reglas sin cálculo > +8 pp sin firma | Decisión del profesor | Según la opción |
| H3 | MEDIA | `X1a` en la mezcla +8,04 y `R10-150` +18,71 no se clasifican | Mezcla para todas las métricas; `X1a` y `Q` en los dos ámbitos | No |
| H4 | MEDIA | Exit 4 con `dist = "EV-D-SUMA"`: `ERRS`, `MULT` y `pst` son copias; el modo `exacto` solo también da 4; una regla que falla puntúa 25 % en silencio | Atar la enumeración al `switch` real; contar errores de regla; exit 4 solo en `todo` | No |
| H5 | MEDIA | O1-E aborta 6 241 de 9 564 estados y no prueba nada propio | Sustituirlo por un mutante de pesos, sin abortos (X1b 45,19 %) | No |
| H6 | MEDIA | L-8 no ve un negativo en un operando: 3 mutantes sobreviven | Ampliar guarda y oráculo a los operandos; M17d | **Sí**, sin mover cifras |
| H7 | MEDIA | La prosa de análisis por razón no tiene sonda: 2 mutantes sobreviven | Exigir en `chequeo()` la frase de cada código con las cifras del enunciado | No |
| H8 | MEDIA | La cabecera nombra «un negativo», no nombra `+`; `− 0` es constructo solo en parte | Reescribirla; declarar `M0` y `RE` con cifra | No |
| H9 | MEDIA | M3, M21 y M17a prueban menos de lo que declaran | M3b; mutante sin la precondición de `EV-D-SUMA`; M17a con la entrada en el `switch` | No |
| H10 | MEDIA | La nota visual remitía a un registro que no existía | Registro de abajo | No |

### Revisión visual por imagen de la r3 (md5 `25a924d4`)

Sobre el md5 `25a924d4`. Por programa, las 100 versiones a 360 y 320 px dieron 0 desbordes, 0 textos rotos, 0 opciones duplicadas y 0 negativos en el procedimiento.

| Quién | Qué miró |
|---|---|
| Auditor | v01, v02, v03, v05, v09, v15, v21 y v35 a 360 px; v09, v15, v21 y v35 a 320 px. Las 4 ramas en los dos anchos |
| Orquestador | Canónica v07 a 320 px. PDF: `salida/pdf/version_plain1.pdf`, página 1; `version_solution1.pdf`, páginas 1 a 3. NOPS: página 3 |
| Detractor r3 | v07 a 320 px, y una página de un PDF y otra de un DOCX |

**Sin mirar por imagen:** v27, v47, v54 y v66, las otras canónicas; el resto de las 16 + 16 pedidas, 8 a 360 px y 12 a 320 px; el contact sheet a 320 px. El de 360 px salió
ilegible. Razón declarada por el auditor: el render tardó unos 25 minutos y se le agotaron los turnos. El auditor escribió que la canónica no salía: fue un falso negativo por
el espacio duro del enunciado, ya anotado en `salida/auditoria_visual_r3.md`. Las capturas están en el scratchpad de la sesión, no en `salida/`.

**Sin auditar por nadie:** el DOCX abierto en Word y los escaneos NOPS.

## Histórico: base de la firma O-1, sin efecto

> Se conserva porque es lo que el profesor tenía delante al firmar. Dos cosas ya no valen: la cifra +8,41 de `EST-afirmado-nuevo` no mide
> lo que dice su glosa, y el argumento «+17,03 contra +14,49», que decidió la variante D, se invierte: ver «Lo que vuelve al profesor».

> «Autorizo quitar las ramas de signo, firmo esas 9 cifras y confirmo el componente».

Registro: O-1 y L-8 en `.claude/CLAUDE.md`. Lo que sigue es la base de esa firma.

### 1. Canales por encima de +8 pp (§P7-E, se firma regla por regla)

«Sustituir» significa, en `sust_val()` de `verificar_espacio.R`: poner lo que afirma la opción en lugar del resultado de su
paso, dejar el otro paso como lo muestra el procedimiento y recalcular la rapidez. En el paso 3 se lee la fracción afirmada.

Cifras **exactas**: enumeración de los 14 346 estados y sus conjuntos de razones, con sus pesos reales, sobre el md5 de arriba.
Exceso sobre el techo nulo de 28,8 %, sobre no canónicas.

| Regla | Qué hace quien la usa | ¿Calcula? | Exacta | Exceso | Impreso |
|---|---|---|---|---|---|
| `X0` | descarta las opciones cuya cifra ya está en el enunciado | no | 40,55 % | +11,75 pp | 50 % |
| `Q` | elige la opción que solo le quita el menos a lo que muestra el paso | no | 45,83 % | +17,03 pp | 25 % (no aplica) |
| `X1b` | sustituye y conserva las que dan rapidez entera | sí | 38,82 % | +10,02 pp | 25 % |
| `X1a` | sustituye y descarta rapidez negativa o en h/km (constructo) | sí | 46,43 % | +17,63 pp | 50 % |
| `X1` | `X1a` y `X1b` juntas | sí | 63,40 % | +34,60 pp | 50 % |
| `R10-150` | sustituye y conserva las de 10 a 150 km/h | sí | 55,08 % | +26,28 pp | 50 % |
| `R20-120` | sustituye y conserva las de 20 a 120 km/h | sí | 65,47 % | +36,67 pp | 50 % |
| `R20-120+ent` | la anterior, y además entera | sí | 74,03 % | +45,23 pp | 50 % |
| `R30-90` (`X1c`) | sustituye y conserva las de 30 a 90 km/h, el rango del generador | sí | 76,71 % | +47,91 pp | 50 % |

**Las reglas R y `Q` no estaban declaradas** en la tabla de residuos; se midieron en la revisión del coordinador. R es el canal
mayor. `Q` acierta el 100 % en las ramas de signo sin calcular nada, y salió al leer una versión de cada rama
(`salida/medicion_exacta/una_version_por_rama.md`): pueden existir otras reglas así, y el detractor nuevo debe buscarlas.
`X1` añade +17 pp a `X1a`, casi el doble de lo que sugiere `X1b` sola.

**Lo que se pierde al firmar.** El impreso nunca deja solo la clave: sus opciones dan 50, −40, 40 y −10 km/h, y dos son creíbles.
Con `R20-120` queda solo la clave en el 37,6 % de las versiones (56,0 % con `R30-90`). Por rama, `R20-120`:

| Rama | Peso | Tasa | Solo la clave |
|---|---|---|---|
| `EV-C-UNIDAD` | 33,3 % | 51,3 % | 15,5 % |
| `EV-D-SUMA` | 11,1 % | 60,5 % | 26,5 % |
| `EV-D-FINAL` | 11,1 % | 61,4 % | 28,7 % |
| `EV-T-FINAL` | 16,7 % | 64,6 % | 35,5 % |
| `EV-D-SIGNO` | 11,1 % | 84,5 % | 69,0 % |
| `EV-T-SIGNO` | 16,7 % | 88,1 % | 76,1 % |

**Alternativas medidas (§P7-F), en memoria y sin corregir nada.** Ninguna cierra el canal de sustitución:

| Diseño | Máx. no canónicas | Máx. mezcla 0,05 | `R20-120` | Solo la clave | `X0` |
|---|---|---|---|---|---|
| **Actual** | +6,8 `REL-afirmado-citado-en-otra` | +6,7 ídem | +36,7 | 37,6 % | +11,8 |
| A: preferir una razón falsa con rapidez creíble | +7,4 ídem | **+9,9** `MAG-segundo-afirmado` | +29,5 | 25,2 % | +13,1 |
| D: quitar las ramas de signo | **+8,4** `EST-afirmado-nuevo` | **+8,9** `MAG-segundo-afirmado` | +30,2 | 26,2 % | +14,5 |
| C: A + D | **+9,2** `EST-afirmado-nuevo` | **+12,1** `MAG-segundo-afirmado` | +22,4 | 13,1 % | +15,9 |

Oráculo sin problemas en las tres (N = 100 por rol). Una cuarta variante, A + signo con paso repetido igual a la clave, dio +8,3 pp en
muestra y no se enumeró. Reajustar `RANGO_META` sobre A no se probó: sería una 4.ª pasada, y §P7-D está agotado.

Ampliar L-2 con `D` múltiplo de `t1` no conviene: `X1b` baja de +10,0 a +7,8 pp, pero el espacio pasa de 97 a 40 filas y
`DIV-mult10` sube de +3,4 a +7,4 pp (`salida/medicion_exacta/exacto2.out`).

**Recomendación del coordinador (corregida el 2026-11-05): variante D, sin abrir ninguna otra.** Retirar `EV-D-SIGNO` y
`EV-T-SIGNO` como error del procedimiento y conservarlas como razones falsas, que es donde las puso el impreso. Se retiró la
recomendación anterior, firmar sin cambiar el diseño, porque su argumento era falso:

- La batería de 51 reglas no incluía `Q`. Con ella, la peor regla sin cálculo del diseño actual es +17,03 pp; en D es `X0`, +14,49 pp.
- `EST-afirmado-nuevo` no lo abre D: ya marca 70,0 % y 75,6 % en `EV-D-FINAL` y `EV-D-SUMA`, y las ramas de signo lo diluyen en el
  promedio. Es de la misma familia que `X0`, que sí se aceptaba en +11,75 pp.

**Costos de D:** exige una excepción a §P7-D, que el profesor debe autorizar; la marca «− 0» pasa de 27,8 % a 50 % de las versiones,
porque el paso 2 solo conserva `EV-T-FINAL`; `X0` y `X1b` suben. `X1a` baja a +7,35 pp y `Q` deja de aplicar.

**Cifras exactas de D por encima de +8 pp**, las que firmaría el profesor: `X0` +14,49 · `X1b` +12,42 · `X1` +29,03 · `R10-150` +18,58 ·
`R20-120` +30,15 · `R20-120+ent` +41,25 · `R30-90` +43,32 · `EST-afirmado-nuevo` +8,41 · `MAG-segundo-afirmado` en la mezcla +8,87. No
dependen de semillas: H1 y las guardas de H3 no cambian los datos, así que se conservan si solo se aplican esos cambios.

`X1b` es la única que entrena algo falso («elige la que da entera»); las R y `X1a` premian comprobar si el resultado tiene sentido, lo
que la propia Solution recomienda. `X0` lo trae el impreso con más fuerza (50 %).

### 2. Componente

**Confirmado por el profesor: Numérico-variacional**, así que el nombre del archivo no cambia. El clasificador proponía Geométrico-métrico. Razones: el Marco
2019 pone las razones de cambio en Álgebra y cálculo, y las 6 fichas de 2025-2 con vehículos y rapidez o velocidad (007, 049, 124,
128, 139 y 153) están en Numérico-variacional. Comprobado por el coordinador. El Estándar sigue siendo el métrico, único del EBC que
nombra la velocidad media.

## Corrección a lo declarado: la batería

`.claude/CLAUDE.md` y el orquestador declaran `FMT-empieza-km` +6,0 pp y mezcla +7,9 pp. Esa cifra sale de la **muestra de la semilla
1, con la que se afinó el diseño en sus tres pasadas**. La muestra de la semilla 200001 da `BLOQUEA`, +8,6 pp. Exacto:

| Regla | Exacta | Exceso | Mezcla 0,05 |
|---|---|---|---|
| `REL-afirmado-citado-en-otra` | 35,60 % | +6,80 pp | +6,68 pp |
| `FMT-empieza-km` | 33,92 % | +5,12 pp | +5,09 pp |
| `DIV-mult10` | 32,18 % | +3,38 pp | +3,44 pp |
| `MAG-segundo-afirmado` | 31,86 % | +3,06 pp | +6,47 pp |

No obliga: sigue en zona gris, pero 1,5 pp por encima del control oficial (+5,3), y el veredicto por muestra cambia de lado.
**`P_CANON` se queda en 0,05:** con 0,10, `MAG-segundo-afirmado` pasa a 38,68 %, +9,9 pp exactos.

## Qué falta, en orden

1. Aula: el ejercicio no se promueve sin evidencia de Nivel 3. Con 25 estudiantes hay unos 6 por rama: conviene usar la versión impresa como ancla y entrevistas cortas.
2. `/detractor-hetero` sobre el md5 final, recomendable antes de promover. Hoy no corrió, y deepseek y ollama-cloud estaban sin saldo.
3. Sin auditar: el DOCX en Word, los escaneos NOPS y, por un detractor independiente, los cambios del verificador posteriores a la r4. El coordinador contrastó las 15 glosas nuevas con su código y recalculó 7 cifras; eso no es FASE 2C.

## Trampas de este subproyecto

- **`RMD_MD5` y `FIRMADA` en `verificar_espacio.R`.** Cualquier edición del `.Rmd` da exit 3 hasta actualizar `RMD_MD5` y repetir la medición. Las cifras de `FIRMA_O2` valen solo para ese md5.
- **`salida/verificar_r13.txt` es la corrida que vale.** Las anteriores son de otras versiones; `r10` quedó incompleto.
- **El modo `exacto` reescribe `salida/firma_o2_medida.csv`:** corre copias si no quieres tocarlo.
- **El hook solo corre con una ruta literal dentro de `exams2*("…Rmd")`**; con una variable no se dispara. Sin informe nuevo en `~/.cache/post-exams2-validation/`, no corrió. Su FASE 2B dice «PDF no encontrado»: los previews se generan a mano con `pdftoppm`.
- **R lee el script por tramos.** Editar `verificar_espacio.R` mientras corre corrompe la corrida. Un subagente que para en su tope de turnos, o que espera una tarea de fondo, puede retomar solo: antes de editar el mismo archivo, comprueba que no siga vivo.
- **Quedan artefactos de md5 anteriores**, porque el borrado con `rm -rf` lo denegó el sistema de permisos: `salida/pdf/version1.pdf`, `salida/pdf/tex/`, `tex_r3/`, `salida/realista/tex/` y `tex_r3/`. Valen `version_plain1.pdf`, `version_solution1.pdf`, `tex_r4/` y `realista1.pdf`.
- `salida/paso8_coherencias.md` dice que el pool tiene 14 entradas: son 13.
- `salida/medicion_exacta/exacto.R` y `exacto2.R` ya no corren contra el verificador actual. Quedan como evidencia.
- Las citas de los reportes r1 y r3 ya no verifican contra el árbol porque se atendió lo que señalan. El hook validó las de la r3 a las 15:49:52 y las de la r4 a las 17:56:57.
- `Rscript --no-init-file`: `--vanilla` no sirve en esta máquina.

## Reanudar

Si hiciera falta reanudar otra vez, sesión nueva o la misma:

```
/orquestador-schoice {"ruta_destino":"A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso","nombre_ejercicio":"rapidez_media_error_paso_numerico_variacional_argumentacion_n3_schoice_v1","entrada":"A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/originales/orig_pregunta_09.png","modo":"ejecutar","decisiones_humanas":{"flujo_b":"n"}}
```

Evidencia de las cifras: `salida/medicion_exacta/`.
