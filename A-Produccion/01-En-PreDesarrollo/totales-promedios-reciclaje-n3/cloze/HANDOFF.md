# HANDOFF — CLOZE `totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1`

> Frase disparadora: «Continúa con el CLOZE de totales-promedios-reciclaje-n3».
> Última actualización: 2026-11-02, **aprobado por el profesor para el aula («[a] Aprobar»), 11/11**. Antes: tanda 9b del
> `orquestador-cloze` (pasos 7 a 10 sellados uno por uno, cada uno tras
> comprobar su artefacto; 10/11; md5 sin cambios; el paso 11 espera la decisión del profesor). Antes: cierre de la tanda 9a por
> el coordinador (H2 cerrado con MC39; rótulo «muestral sobre la
> Parte 1» en L-C2; verificador r9 en exit 4, sin pendientes; nada sellado; md5 sin cambios). Antes: tanda 9a del
> `orquestador-cloze` (FASE 2C r3 `APROBAR_CON_CAMBIOS`, sin CORRECCIÓN; paso 8
> con el oráculo reconstruido, 0 fallos; nada sellado; md5 sin cambios). Antes: tanda «Va» del coordinador (las decisiones 1 y 2 de «Va», cableadas en
> `verificar_espacio_cloze.R`; verificador en exit 4, sin pendientes; md5 sin cambios). Antes: tanda 8 del `orquestador-cloze`
> (decisión b: la Parte 2 solo con «promedio»; leyes L-C6 y L-C7; md5 `f98f9dda…`), detenida antes de la FASE 2C r3, y decisión
> del profesor tras ella («Va», 2026-10-31: X6 exacta, L-C7 regla por regla, residuo de la 6b); tanda 7 (L-C4 v2 y pasada 3 de §P7-D, md5 `898608dd…`), tanda 6
> (FASE 2C r2 `APROBAR_CON_CAMBIOS`), tanda 5 (detenida en el paso A), tanda 4 (reparto de H2 no viable), tanda 3 (FASE 2C r1 `RECHAZAR`).
> Si este archivo y `ejercicio_state.json` discrepan, manda el JSON. Las invariantes del SCHOICE (L-1 a L-5,
> «O-1 corregido»), las firmas del CLOZE (F-1 a F-4), las decisiones del profesor y las leyes del CLOZE (L-C1 a L-C5)
> están en [`../.claude/CLAUDE.md`](../.claude/CLAUDE.md) § «CLOZE hermano». El ítem verbatim, en
> [`../ITEM_OFICIAL.md`](../ITEM_OFICIAL.md).

## Estado: 11/11, aprobado para el aula el 2026-11-02 (falta la evidencia de aula; no está en producción)

`.Rmd`: md5 **`f98f9dda47e4f3cafc4e8b4e82cc956b`** (sin cambios desde la tanda 8). **Aprobado para el aula** por el profesor en el
WAIT_USER #3 («[a] Aprobar»), con los residuos de § «Residuos declarados». Uso: Moodle (§ «Uso en el aula»); el DOCX no es
apto para estudiantes y, para papel, está el SCHOICE. Siguiente hito: la evidencia de aula (Nivel 3). La promoción viene
después. El próximo detractor revisa MC39, y H3 está diferido al próximo ciclo, junto con el pendiente del SCHOICE.

## Tanda 9b (2026-11-02): pasos 7 a 10 sellados (registro también en `../.claude/CLAUDE.md`)

- **No cambió nada que no sea el estado:** el `.Rmd` (`f98f9dda…`), `verificar_espacio_cloze.R` (`6901624e…`, el de `92dc8d69`),
  la batería (`820e5146…`) y la lista firmada (`2aa8e742…`) siguen igual. Se sellaron cuatro pasos, cada uno con su propio
  `workflow-state.sh complete` y después de comprobar su artefacto. La nota de cada sello nombra el artefacto y su md5 o su cifra.
  - **Paso 7** (18:26:42): `fase2c_r3_detractor.md` (md5 `70989c4e…`) audita `f98f9dda…`; `verificar_citas_detractor.py` da
    exit 0 (9 citas, 0 problemas). La nota recoge la decisión del 2026-11-02: H1 queda como residuo, H2 se cerró con MC39 en
    la r9 y H3 se difiere. MC39 llegó después de la r3, y lo revisa el próximo detractor.
  - **Paso 8** (18:27:17): `coherencias_r9/`. Los 4 informes y los 7 logs de render llevan el md5 vigente. Resultado: 12 720
    comprobaciones y 0 fallos; 0 fallos entre formatos (2 declarados en el DOCX); las 88 mutaciones, cazadas por su sonda.
    L-C8 se cumple: S2 por versión, S2 global y la cuarta paráfrasis cazada.
  - **Paso 9** (18:28:02):
    - `diversidad_sustantiva_r9.txt` y `diagnosticidad_r9.txt` se volvieron a correr con N = 100 y el md5 en la cabecera;
      salen idénticos a los r8.
    - `diagnosticidad_por_rama_r8.txt` lleva el md5 vigente y n = 100 en los 7 estratos.
    - E5 de la r9: 300 versiones únicas de 300. FASE 2G: 100 de 100.
  - **Paso 10** (18:30:13): V1 a V4, en `v1_v4_r8.txt`. V5 a V11, en `v5_v11_r9.py` y su `.txt` (nuevos; 0 fallos). La
    literalidad de los `exextra` contra el catálogo canónico, en `literalidad_exextra_r9.py` y su `.txt` (nuevos; 0 fallos).
Verificador (`salida/verificar_espacio_cloze_r9.txt`, con MC39): **exit 4, EN ROJO BAJO OVERRIDE FIRMADO, 327 cifras dentro de lo
firmado, 0 fallos, 0 pendientes.**
FASE 2C r3: **`APROBAR_CON_CAMBIOS`**, sin CORRECCIÓN (H1 y H3, al profesor; H2, cerrado por el coordinador con MC39). Paso 8:
**`VEREDICTO_COHERENCIAS: OK`** (N = 100, 0 fallos). Los dos se sellaron en la tanda 9b (pasos 7 y 8).

## Cierre de la tanda 9a (2026-11-02, coordinador; registro también en `../.claude/CLAUDE.md`)

- **Solo cambió `verificar_espacio_cloze.R`** (+37/−5 frente a `e11dc058`). El `.Rmd` y `ejercicio_state.json` (6/11), no.
- **H2 cerrado: MC39 en E8** (L-C7, la cláusula «o que pase de su cifra»):
  - sonda directa: una de las 32 reglas firmadas bajo el tope de su estrato, subida 1 pp sobre su cifra (CURSOS · «curso habría
    ganado» a +13,5, firmada +12,5, tope +25,0), queda pendiente con la regla por regla y cubierta con la de estrato;
  - mutante de texto: BP_gano gana «perdió hubiera tenido», que pasa de +12,5 a +25,0 pp en CURSOS y PROMEDIO:
    `cazado_por_su_sonda`.
- **H1, la parte del coordinador:** las 13 líneas de X5, X6 y X10 en E3 dicen «exacta sobre el sorteo de la Parte 2 y muestral
  sobre la Parte 1 (N = 100)». Las cifras no cambian.
- **r9** (`salida/verificar_espacio_cloze_r9.txt`, 445 s): exit 4, 327 cifras, 0 fallos, 0 pendientes. Contra la «Va», con los
  rótulos devueltos a su forma anterior y sin las 5 líneas de MC39, quedan 0 líneas de diferencia (`salida/comparar_r9_va.py`/`.txt`).
  La primera corrida se cortó al cerrar la sesión del 2026-10-31 (línea 718 de 1 134) y se repitió entera.
- **L-C8 comprobada en `coherencias_r9/`:** S2 por versión y global, y la mutación con una redacción nueva, cazada.
- **Fe de erratas:** en la tabla de «Va», «exacta» lo es solo sobre el sorteo de la Parte 2. Sobre la Parte 1 es muestral: una
  muestra de N = 100 por estrato. Corregido abajo en su sitio; en `../.claude/CLAUDE.md`, como fe de erratas, porque allí solo se
  añade.

## Decisión del profesor tras la tanda 9a (2026-11-02; texto completo en `../.claude/CLAUDE.md`)

1. **H1 (a):** X6 por clave y por rama, **residuo declarado**. La puerta sigue juzgando por rama. Claves 1 y 2, en las 5 réplicas:
   CURSOS +2,0 / +10,0 / +4,5 / +11,0 / +4,5 (500 juntas: +6,4); PROMEDIO +9,0 / +8,5 / +10,5 / +3,0 / +7,5 (+7,7); clave 3,
   +0,0 (`salida/fase2c_r3/x6_replicas.txt`).
2. **H3 (a): se difiere** al próximo ciclo, junto con el pendiente del SCHOICE. El `.Rmd` no cambia.
3. **Candidatas del repo #3, #4 y #5: se aplican ahora**, en un commit aparte (regla #17).
4. **Paso 7 (a): se sella con la r3.** La nota declara que MC39 llegó después; lo revisa el próximo detractor.

## Tanda 9a (2026-10-31): FASE 2C r3 y paso 8, sin sellar (registro también en `../.claude/CLAUDE.md`)

- **No cambió nada sellado:** ni el `.Rmd` (`f98f9dda…`), ni `verificar_espacio_cloze.R`, ni `ejercicio_state.json` (6/11).
- **FASE 2C r3** (`AgenteDetractor` nuevo, sin `name:`): **`APROBAR_CON_CAMBIOS`**, sin ninguna CORRECCIÓN. Reporte íntegro,
  sacado de su transcripción: `salida/fase2c_r3_detractor.md`. Citas: `verificar_citas_detractor.py` → 9 citas, 0 problemas; el
  hook `SubagentStop` registró «ENTREGA VERIFICADA». Sus scripts y salidas: `salida/fase2c_r3/`.
  - Sin hallazgo, con su código: en la Parte 2 solo la clave da el promedio con todas las lecturas razonables; el tipo retirado
    no deja rastro; el sorteo conservado solo mueve el RNG (0 diferencias con `898608dd`); reproduce E1, L-C6, la lista de L-C7,
    las 9 celdas de L-C2, la canónica, RL1 y RL2.

  | ID | Severidad · tipo | Qué | Estado |
  |---|---|---|---|
  | H1 | MEDIA · diagnosticidad | L-C2 «exacta» lo es sobre el sorteo de la Parte 2 y muestral sobre la Parte 1. X6 por rama, en 5 réplicas disjuntas de N = 100: CURSOS de +1,3 a +7,3 y PROMEDIO de +2,0 a +7,0 pp («ok» en todas). Claves 1 y 2: CURSOS +2,0 / +10,0 / +4,5 / +11,0 / +4,5 y PROMEDIO +9,0 / +8,5 / +10,5 / +3,0 / +7,5 pp; clave 3, +0,0. La réplica 1 es la del verificador (`salida/fase2c_r3/x6_replicas.txt`) | Al profesor (la lectura por clave que la decisión «Va» 1 le reservó) |
  | H2 | MEDIA · verificador | Ningún mutante prueba la cláusula «o que pase de su cifra» de L-C7 (`p6_token_juzgar()`, línea 1036): quedan sin prueba las 32 reglas firmadas a la mitad del tope de su estrato | Al coordinador: MC39 |
  | H3 | BAJA · Solution | Parte 6: «habría ganado con un curso más solo si su promedio … fuera mayor que la diferencia» da solo la condición necesaria y, en el caso verdadero, se lee como contrafáctica. No es falsa | Opcional; cambia el `.Rmd` |

- **Paso 8** (`AgenteValidadorVisual`, sin `name:`; oráculo reconstruido en `salida/coherencias_r9/`):
  **`VEREDICTO_COHERENCIAS: OK`**.
  - Juicio propio de las 8 opciones de la Parte 2 (3 correctas y 5 incorrectas), sin abrir el `.Rmd`, y recálculo de cada
    operación con la tabla mostrada: ninguna incorrecta da el promedio (200 de 200 filas); las explicaciones que dicen qué da una
    operación, 240 de 240.
  - N = 100: de 126 a 129 comprobaciones por versión (12 720), 0 fallos. Formatos: HTML 100, PDF 100, Moodle 100 (más 30 por la
    ruta de `exportar_moodle_cloze.R`), PDF con el pandoc de RStudio 5, DOCX 1 (2 BAJA ya declaradas); 5 529 comprobaciones entre
    formatos, 0 fallos. 88 mutaciones de control, cazadas por su sonda (40 + 25 + 12 + 11).
  - Límite de independencia declarado: el volcado del pool le mostró la marca junto a cada texto y `../.claude/CLAUDE.md` le
    llegó al contexto. Lo independiente es el recálculo numérico, más la función de verdad propia del detractor.
  - BAJA de redacción: «al cuadrado» con dos lecturas (las dos falsas; la explicación describe una); las claves 2 y 3 están
    redactadas como operación, no como «lo que indica».

## Decisión del profesor tras la tanda 8 (2026-10-31, «Va»; texto completo en `../.claude/CLAUDE.md`)

1. **X6 exacta sobre la Parte 2 (L-C2).** X5, X6 y X10 se miden exactas sobre el sorteo de la Parte 2 en la mezcla y en las
   dos ramas, y muestrales sobre la Parte 1 (la misma muestra de N = 100 por estrato). X6 en PROMEDIO pasa de +8,5 pp (muestra
   de todo) a +6,0 pp (exacta sobre la Parte 2). Por clave da +9,0, +9,0 y +0,0 pp: es un dato que no se mostró al pedir la
   decisión y está declarado en el registro.
2. **L-C7 regla por regla.** La lista es `firma_p6_palabras_f98f9dda.tsv`: 216 reglas, 54 por estrato de rama y de *k*′. Si
   aparece una palabra por encima de +8 pp que no esté en la lista, queda pendiente.
3. **6b:** los dos BAJO tipográficos quedan como residuo declarado.

Las decisiones 1 y 2 quedaron cableadas en la tanda «Va» (abajo).

## Tanda «Va» (2026-10-31): las decisiones 1 y 2, cableadas en el verificador (registro completo en `../.claude/CLAUDE.md`)

- **Solo cambió `verificar_espacio_cloze.R`.** El `.Rmd`, la batería L-C4 v2 y la lista firmada siguen en sus md5 (`f98f9dda…`,
  `820e5146…`, `2aa8e742…`).
- **Cómo se hizo.** DeepSeek, por `delegar.sh`, escribió el cableado. Lo corrió una vez (`salida/verificador_va.txt`,
  interrumpida en E3) y se colgó en el proxy; su proceso había sobrevivido al cierre de la sesión anterior. El coordinador lo
  detuvo y completó el diff. Corrigió cuatro cosas:
  - la comprobación del sorteo (`-sol_p2` con `sol_p2` lógico quita siempre la opción 1: FALLA falsa en 74/100);
  - MC37, que estaba antes de `mutar()`;
  - la llegada de MC38, que daba siempre FALSE, y su semilla;
  - MC37, que medía solo la mezcla (+8,5 pp, «cazado» por medio punto). Ahora mide la mezcla y las dos ramas, como la puerta.
- **Decisión 1 (L-C2, E3).** X5, X6 y X10, exactas sobre el sorteo de la Parte 2 (30 combinaciones: 3 claves × 10 tríos) y
  muestrales sobre la Parte 1 (una muestra de N = 100 por estrato; fe de erratas del 2026-11-02).
  - Mezcla, CURSOS y PROMEDIO: X5 −4,2 / −2,9 / −6,4; X6 −0,1 / +1,3 / +6,0; X10 −7,3 / −4,0 / +2,7 pp. Todas «ok».
  - X6 por clave en PROMEDIO: +9,0 / +9,0 / +0,0 pp.
  - Supuesto del sorteo: 300/300.
- **Decisión 2 (L-C7, E8).** La lista firmada (216 reglas, md5 comprobado). Las 216 salen cubiertas regla por regla (54 por
  estrato de rama y de *k*′), ninguna cubierta fuera de la lista y ninguna pendiente.
- **Mutantes:**
  - MC37: X6 exacta +38,2 pp en CURSOS: pendiente.
  - MC36: «duda», fuera de la lista.
  - MC38: «justamente», pendiente con la firma regla por regla y cubierta con la de estrato; 12 reglas.
  - Todos, cazados por su sonda. Lo demás, idéntico a la r8b.

## Tanda 8 (2026-10-31): decisión b del profesor — la Parte 2 solo con «promedio»; overrides L-C6 y L-C7

- **Reapertura #4** desde `generacion_rmd` (`workflow-state.sh reopen`, `--reporte salida/bateria_v2_898608dd.txt`).
- **Qué se quitó:** el banco del otro tipo de la Parte 2, sus funciones de verdad y su guarda, sus cinco entradas de `expl2`, su
  entrada de `clave2_expl` y los dos condicionales de la Solution (párrafo «Parte 2.» y encabezado). Sin código muerto: queda una sola
  `clave2_expl` y un solo encabezado («Parte 2 · Lecturas del promedio que no sirven»).
- **RNG conservado.** El sorteo que elegía el tipo se conserva sin uso, `invisible(sample.int(2L, size = 1L))` (mismo consumo que
  `sample()` de dos elementos), y `tipo_p2 <- "promedio"` queda como rótulo del registro (lo leen la batería congelada y el
  verificador). `salida/rng_tanda8.txt` (`rng_tanda8.R`): en 120 semillas (1..100 y 20 canónicas), la Parte 1, las Partes 3 a 6, la
  marca, el orden y los índices sorteados de la Parte 2, `exsolution` y el `.Random.seed` final son idénticos al md5 anterior; solo
  cambian los objetos del banco de la Parte 2 (69 versiones eran del tipo retirado). Las dos puertas pasan ese sorteo a LIBRE
  (`enumerar_canonica_cloze.R`: la canónica baja de 192 a 96 hojas; `enumerar_h2_cloze.R`: mismo número de llamadas).
- **Parte 2 vuelta a medir, exacta, ejecutando el código** (`salida/bateria_v2_f98f9dda.txt`, `medir_bateria_v2_t8.R`): P2-R2 +21,9,
  P2-R1 +13,6, P2-E8c +13,5, P2-E5c +11,1 y P2-M3c +10,0 pp, **iguales a lo firmado**; ninguna otra de las 268 reglas pasa de +8 pp
  (P2-R3 +7,5 en zona gris). Regla por token de la Parte 6: mezcla +0,0, CURSOS +25,0, PROMEDIO +25,0, *k*′ = 1 +16,4, *k*′ ≥ 2
  +16,7 pp, **iguales a lo firmado**; pasan de +8 pp 54 reglas por token en cada estrato de rama y de *k*′.
- **Leyes nuevas** (registro en `../.claude/CLAUDE.md` § «Leyes del CLOZE»): L-C6 (`FIRMA_P2`, mutantes MC31 a MC35) y L-C7
  (`FIRMA_P6_TOKEN`, mutante MC36). MC19 rehecho con las claves «promedio»; MC30 retirado con el tipo retirado.
- **Canónica (exacta):** X5 −25,0, X6 −25,0, X10 −33,3 pp (ya no en rojo); X9 +25,0 y X11 +12,5 pp, dentro de lo firmado.
- **Nuevo, pendiente de firma (F-2 y L-C2):** X6 («más palabras en común con la clave de la Parte 2») en la rama PROMEDIO de la Parte
  1: **+8,5 pp** en la muestra de la puerta (n = 100, clave sorteada; en la tanda 7, +6,2). Exacta sobre las tres claves, con las
  mismas Partes 1: +6,0 pp; por clave +9,0 / +9,0 / +0,0 (`salida/x6_estratos_t8.txt`). Lo abre la decisión b (todas las claves de la
  Parte 2 son ahora de «promedio»). §P7-D está agotado: no se corrige; va al profesor. No se cambió la puerta de muestra a exacta,
  porque eso relaja una guarda (H-5).
- **Medido antes de sellar** (se sella en su orden): paso 9 (`salida/diversidad_sustantiva_r8.txt`, `diagnosticidad_r8.txt` con p6
  excluida por mchoice, `diagnosticidad_por_rama_r8.txt`); paso 10 (`salida/v1_v4_r8.txt`; literalidad de los `exextra` contra el
  catálogo, sin cambios en `Meta-information`).

## Tanda 7 (2026-10-31): L-C4 v2 y pasada 3 de §P7-D (la última) en la Parte 2 — detenida tras el paso 6

**Batería L-C4 v2**, congelada antes de reescribir: `bateria_partes_2_a_6.R`, versión «L-C4 v2 (2026-10-31)», **md5
`820e51464b34deff35b830789584de8a`** (v1 `de20e0a9…`, commit `31b8fe9e`). Qué añade y por qué: cabecera del archivo y
`../.claude/CLAUDE.md` § L-C4 v2. Histórico (§P7-C): `salida/bateria_v2_historico.txt` (`salida/medir_bateria_v2.R`).

| md5 | «promedio»: reglas > +8 pp | «comparar»: reglas > +8 pp | Lo que pedía la orden |
|---|---|---|---|
| `96cfcf35…` | 106 (P2-VX +75,0; «el total» +75,0) | 121 (P2-VX +75,0; «curso» descarta +50,0) | — |
| `42f725bf…` | 54 (**«dividido» +75,0**; párrafo P2-E5 +75,0; P2-VX +75,0) | 69 (**«por curso» descarta +25,0**; «curso» descarta +25,0; P2-VX +75,0) | en rojo: sí |
| `898608dd…` (pasada 3) | **5**: P2-R2 +21,9 · P2-R1 +13,6 · P2-E8c +13,5 · P2-E5c +11,1 · P2-M3c +10,0 | **22**: P2-VX +75,0 · P2-R2 +33,9 · doce tokens de la multiplicación +25,0 · P2-R1 +16,4 · P2-E5 +12,5 · P2-E6 +12,5 · «por» +12,5 · P2-R3 +10,6 · «por el» +9,2 · «por el promedio» +9,2 · P2-E7 +8,5 | todas ≤ +8: no |

Detalle exacto: `salida/bateria_v2_898608dd.txt`. Control: las 18 reglas de v1 dan lo mismo con la medida exacta nueva y con
la enumeración de 720 (diferencia 5,6·10⁻¹⁵ pp). L-C5 con v1 y con v2: 335 celdas, diferencia 0.

- **H1 y H2 de la r2, cerrados.** «promedio»: tres claves y cuatro distractores empiezan por «Lo que recogió ese grado dividido»;
  los distractores dividen con un operando equivocado (entre el promedio, otra vez entre dos, entre el número de cursos al
  cuadrado, entre los cursos de ambos grados) y queda la moda. «dividido» y «entre», +5,0 pp; P2-VX, +0,0 (ningún token
  exclusivo de la clave). «comparar»: ningún distractor dice «curso» ni «por curso».
- **Distractores falsos por construcción.** `valor_p2` (promedio: el valor descrito ≠ *a* en los dos grados de la tabla; la moda,
  sobre un testigo compatible con la tabla) y `compara_p2` (comparar: la cantidad comparada ≠ el total *n* · *a*), con dos guardas
  en `data_generation`; mutantes MC29 y MC30 en `verificar_espacio_cloze.R`. 300 semillas sin error.
- **Retroalimentación** (`salida/retro_r7.txt`): `expl2` con una entrada por distractor (10), sin dato muerto; «moda» afinada
  (BAJA de la r2).
- **Regla de parada de §P7-D.** El arreglo cerró el canal léxico y abrió el relacional: «la que más palabras comparte con las
  otras tres» sube a +21,9 pp («promedio») y +33,9 pp («comparar»), antes +2,5 y −10,6. Con las claves y los distractores
  escritos sobre el mismo molde, la clave es la opción más «central». Es la última pasada: no se parchea; va al profesor como
  residuo con su cifra exacta (WAIT_USER #3). Diseño: búsqueda sobre el vocabulario en
  `/tmp/…/scratchpad/diseno/` (fuera del repo); se probó a mano y por búsqueda local, y en «comparar» ningún banco natural
  dejó las tres redacciones de la multiplicación con un distractor que las repita.
- **Parte 6, regla por token (fuera de la batería, declarada):** mezcla 0,0 pp; estratos de *k*′ hasta +16,7 pp (dominada por
  K1 firmada); estratos de rama +25,0 pp (contenido lógico, RL3). Va al profesor.
- **Sellos:** pasos 3 (09:47), 4, 5 (`salida/render_r7.log`; NOPS N/A) y 6 (hook v6.3 con la ruta absoluta: 0 errores,
  7 advertencias, 2G 100/100, 2H 10 semillas sin anomalías; informe en `~/.cache/post-exams2-validation/…/20261031-094827-2419561/`),
  cada uno con su propio comando tras su artefacto.
- **Verificador** (`salida/verificar_espacio_cloze_r7.txt`): **exit 3, 407 s, 0 fallos, 27 pendientes de firma** (las 5 reglas
  de «promedio» y las 22 de «comparar» de la tabla de arriba; ningún pendiente fuera de la Parte 2); 107 cifras «ROJO, cubierto»
  dentro de lo firmado. Canónica exacta: X6 +0,0 y X10 +8,3 pp (firmadas +16,7), X9 +25,0, X11 +12,5. L-C5: 67 de 69 celdas
  cubiertas, 0 por encima, 2 en +8 pp o menos, 0 fuera de la firma; las líneas de L-C5 son idénticas a las de la r6b.
  Mutantes cazados por su sonda: MC7, MC10 a MC30 (MC28, vocabulario exclusivo: P2-VX +75,0 pp; MC29 y MC30, funciones de
  verdad), MP1 y MP2. Residuos sin cambio: V11 a un paso (11/12/17 %), cascada F1 1 de 330, P6-P2 +9,9 y +8,3 pp NO CONCLUYENTE.
- **No ejecutado** (presupuesto de turnos del orquestador agotado): 6b, FASE 2C r3, pasos 8 a 10, WAIT_USER #3. El
  `.Rmd` y la batería quedan listos para ellos.

## Tanda 6 (2026-10-31): L-C5 con las cifras exactas del código y pasada 2 de §P7-D (H1 y H3)

**Resultado:** md5 `96cfcf35…` → **`42f725bf191b81784f76b858c26f7733`**. Reapertura #2 desde `generacion_rmd`
(`--reporte salida/fase2c_r1_detractor.md`); pasos 3 a 6 sellados de nuevo, cada uno con su artefacto en la nota.
Quedan **Partes 2 a 6: 1 pasada de §P7-D** (usadas: tanda 1 y tanda 6).

- **Paso A (L-C5).** `FIRMA_H2` = columna `ex` de `salida/h2_exacta_96cfcf35.txt` (decisión «a · iii ampliada sí»).
  `salida/verificar_espacio_cloze_r6a.txt` (exit 3, 368 s): 67 celdas «ROJO, cubierto», A1 en CURSOS (+7,0) en «ok»,
  0 por encima, 0 fuera de la firma; MC25 a MC27 cazados; solo los 12 pendientes de la Parte 2.
- **Paso B (H1).** Parte 2 reescrita en sus dos tipos. Las ocho opciones de cada tipo llevan las mismas palabras del
  estímulo («Lo que recogió … grado … sus cursos» en «promedio»; «grado», «del» y «otro» en «comparar»), sin «total»,
  coma ni condicional; por largo, una clave entre las cortas, otra en el medio y otra entre las largas. Distractor nuevo
  «cociente» (en lugar de «mejor») en «comparar». Exactas, 720 versiones por tipo (`salida/p2_redaccion_42f725bf.txt`):
  todas las reglas de L-C4 de la Parte 2 en +2,8 pp o menos (antes, hasta +75,0). Sondas extra fuera de la batería
  (la que menos comparte, fuera del par, prefijo, descarta la más corta, tres primeras palabras): antes hasta +45,0 pp;
  ahora +3,9 pp como máximo.
- **H3.** Encabezado de la Solution según el tipo de la Parte 2.
- **Invariancia.** Por semilla (100), la Parte 1, la tabla del otro colegio, las Partes 3 a 6, el tipo y la posición de la
  clave de la Parte 2 y el `.Random.seed` final son idénticos a HEAD (`salida/rng_partes_3a6_r3.txt`). L-C5, idéntica entre
  r6a y r6b (134 líneas).

- **FASE 2C r2 (`AgenteDetractor` nuevo, sin `name:`): `APROBAR_CON_CAMBIOS`.** Reporte íntegro en
  `salida/fase2c_r2_detractor.md` (sacado de su transcripción); `verificar_citas_detractor.py`: 13 citas, 0 problemas.
  Ninguna CORRECCIÓN; reproduce con código propio L-C4 (Parte 2 en +2,8 pp o menos, con los 3 materiales y los 6 pares de
  grados), las 69 celdas de «H2 a», RL1, RL2, las canónicas, los pesos y los espacios duros.

  | ID | Severidad · tipo | Qué | Estado |
  |---|---|---|---|
  | H1 | MEDIA · diagnosticidad | Tipo «promedio»: las tres claves dicen «dividido entre» y ningún distractor; «elige la que dice dividido», «… entre» o «más palabras con el párrafo del otro colegio»: **+75,0 pp exactos**. Es H1 de la r1 desplazado («total» → «dividido entre»). Fuera de L-C4 | **Al profesor** (§P7-C: no obliga en este ciclo) |
  | H2 | MEDIA · diagnosticidad | Tipo «comparar»: «descarta las que dicen *por curso*»: **+25,0 pp exactos** (tres distractores lo dicen, ninguna clave) | **Al profesor**, con H1 |
  | H3 | MEDIA · cobertura del verificador | L-C4 no lleva el párrafo del otro colegio (`intro_par`) como estímulo de la Parte 2; por ahí entró H1 | Para L-C4 v2 (próximo ciclo; obliga a volver a medir el histórico) |
  | H4 | BAJA · proceso | El `ERR_DIAG_SUPERFICIAL` de p6 es un artefacto de la sonda H3 sobre un mchoice: «las que empiezan por Si son verdaderas» da 0,0 pp exactos; el canal real condicionado es RL2 (+33,3, firmado) | Anotar esta justificación al sellar el paso 9 |

- **Abierto al cerrar la tanda (no ejecutado o sin resultado):**
- **6b (`auditor-visual-html`, N = 24, semillas de `render_6b.R`; 120 capturas por CDP a 320, 360 y 1024 px, con
  `innerWidth` comprobado): `APTO_CON_OBSERVACIONES`**, recibido después de cerrar el reporte de la tanda. Evidencia en
  `salida/6b_r3/`.
  - BAJA 1: en la Solution de las Partes 3 a 6 (texto propio), la cifra de una ecuación queda separada de «kilos.»: 129 cortes
    en 594 casos (feedback (c) a (f), «Procedimiento correcto», «Parte 6 · Afirmaciones falsas», «Caso específico»), aunque
    el espacio duro ya está. La métrica de la r2 no contaba esta clase. Propuesta del auditor, sin probar: envolver ecuación y
    unidad en un `span` con `white-space:nowrap`. Es de tipografía, no de diagnosticidad: no gasta pasada de §P7-D, pero
    cambiar el `.Rmd` exige reapertura y otro detractor.
  - BAJA 2: palabras sueltas al final de las opciones de la Parte 2 a 320 px (22 de 96) y de sus viñetas en la Solution.
    Estética; sin acción.
  - Sin hallazgos de markup, math, partes, tablas, tildes ni anomalías entre versiones. El encabezado de la Parte 2 sigue al
    tipo en 24 de 24. Posición de la clave en 200 versiones: Parte 2, p = 0,73; Parte 1, p = 0,46.
  - Residuos de la Parte 1 (decisión 2 a), iguales a la r2: «15 / kilos.» 154 cortes en 2 014 casos, todos en la Parte 1.
- **Paso 8 (`AgenteValidadorVisual`, salida en `salida/coherencias_r3/`): las 5 coherencias OK en HTML, PDF y Moodle**,
  con un verificador propio que no usa las funciones del `.Rmd` (N = 100: 106 comprobaciones por versión, 0 fallos; 2555
  ecuaciones; Moodle 35/35 versiones y 210 gaps; 20 mutaciones de control detectadas). **Sin sellar**: el orden exige el
  paso 7 antes. Dos BAJA:
  - el DOCX deja visibles los `##ANSWERi##` (comportamiento de `exams2pandoc` con cloze; ya declarado: el DOCX no es formato
    de entrega del CLOZE; para papel, el SCHOICE);
  - `expl2["moda"]` dice «Eso describe la moda, el valor más repetido» de «Lo que recogió la mayoría de sus cursos»: lo que
    recoge la mayoría es siempre la moda, pero la moda puede no ser mayoritaria. Precisión de la Solution, no CORRECCIÓN;
    se puede afinar en la misma reapertura que decida el profesor.
- **Corrección de una nota sellada:** la nota del paso 5 dice «PDF de la semilla 2»; `salida/revision_pdf_r3/c2-*.png` son
  páginas de `cloze2.pdf`, que es la **semilla 3** (CxC). La revisión a ojo vale para esa semilla.
- **Medido, sin sellar** (el orden exige la FASE 2C antes):
  - Paso 9: `salida/diversidad_sustantiva_r3.txt` (exit 0; p2 6/100 = fija-justificada), `salida/diagnosticidad_por_rama_r3.txt`
    y `salida/diagnosticidad_r3.txt`: **exit 1, `ERR_DIAG_SUPERFICIAL` en p6** (H3: en las 17 versiones con una sola
    verdadera, la verdadera empieza por «Si»). La Parte 6 es mchoice con «Puede haber más de una»; esa única verdadera es
    siempre una hipótesis porque «ventaja» y «Ganó…» llevan el mismo veredicto (L-C1, decisión 1 a), y H2 da 0 %. Pendiente
    de juicio del detractor y, si es canal, del profesor: no se tocó la Parte 6.
  - Paso 10: V1 a V4 OK (`salida/v1_v4_r3.txt`); literalidad de Afirmación, Evidencia, Descriptor y Estándar contra el
    catálogo canónico, OK; DOK 2, Bloom «Evaluar», Nivel 3.
  - Stress test visual N = 100 (`salida/stress_n100_r3/`): 0 anomalías. Suites del repo que barren los `.Rmd`: 6/6 en verde
    (`salida/suites_r3.txt`).

## Tanda 5 (2026-10-31): L-C5 cableada; las cifras exactas con el código superan lo firmado en 22 celdas

**Resultado: detenida en el paso A, como mandaba su orden** («si la cifra exacta ejecutando el código difiere de la
firmada en alguna regla, detente y trae las dos cifras»). No se reabrió el `.Rmd` ni se gastó la pasada 2 de §P7-D.
Registro y tabla de las 22 celdas: `../.claude/CLAUDE.md` § «Tanda 5».

- **Puerta de L-C5:** `enumerar_h2_cloze.R`. Enumeración exacta de las Partes 2 a 6 conducida por el código, para
  cualquier Parte 1: ramifica `rama_par`, `orden_h` (la cascada F1 corre dentro), `v_c` y `c6`; integra la fila del caso
  paralelo y la fila del ganador; cuenta las llamadas de cada sorteo libre. En E8, sobre las 100 Partes 1 de la muestra
  estándar (3200 hojas, masa con error 0, 15 892 filas únicas, 45 s). `FIRMA_H2` (69 celdas) y `h2_juzgar()` deciden
  «cubierto» o «pendiente» por celda.
- **Causa de la diferencia:** V0 (lo firmado) no aplica las exclusiones de V11, y el código sí. La ponderación es la del
  código: `salida/contrastar_h2_ponderacion.R` compara, semilla por semilla, la puerta con el modelo transcrito de
  `medir_reparto_h2.R` con exclusiones y cascada (diferencia máxima de peso 1,1·10⁻¹⁹). Ruido de la Parte 1 (semillas 1 a
  50 frente a 51 a 100): semidiferencia mediana 0,3 pp, máxima 1,4 pp (`salida/h2_contraste_96cfcf35.txt`).
- **Cifras** (`salida/h2_exacta_96cfcf35.txt`): 53 de las 69 celdas cambian a un decimal; 22 suben (las mayores: N1 en la
  mezcla, de +14,1 a +14,8; D2 en la rama PROMEDIO, de +31,7 a +32,5; D2 con *k*′ = 1, de +32,1 a +32,8; A1 en la mezcla, de
  +13,6 a +14,2); 2 bajan a +8 pp o menos (A1c en la rama CURSOS, de +8,9 a +7,0); ninguna celda no firmada pasa de +8 pp.
- Un primer contraste desactivaba las exclusiones con las sustituciones de MC1 y MC2: las guardas V11 del propio `.Rmd`
  abortan entonces 2/3 de las hojas, así que no valía y se retiró.
- **§P7-G:** las dos llamadas de `evaluar_bateria()` de E4 declaran `estimulo_no_aplicable` (los canales del enunciado y
  de la tabla de la Parte 1 se miden en las sondas de E4 y en la canónica regla por regla). Ya no imprimen `SIN_COBERTURA`:
  mezcla PASS (+0,3 pp), rama CURSOS PASS (+0,7 pp), rama PROMEDIO BLOQUEA (+10,4 pp, el canal de largo declarado en «O-1
  corregido»), las mismas cifras que en la tanda 4. La batería L-C4 ya lleva reglas del estímulo propias.
- **La Parte 6 en la muestra de E8** queda solo para las reglas del orden de las afirmaciones (P6-P2), que L-C5 no ramifica.
  Las relacionales RL1 y RL2 pasan a la puerta exacta.

## Tanda 4 (2026-10-31): batería L-C4 CONGELADA antes de medir el reparto (§P7-C)

**Resultado: el reparto autorizado no es viable (paso B). La tanda se detuvo antes de reabrir; no se gastó la pasada 2
de §P7-D.** Medido por enumeración exacta del espacio de `espacio_paralelo()` (`salida/medir_reparto_h2.R`,
`salida/reparto_h2_96cfcf35.txt`; las reglas de orden de las afirmaciones son nulas por construcción y no se evalúan ahí):

| Regla (Parte 6, ventaja y ganador salvo indicación) | Diseño actual (V0) | Reparto autorizado (V1), mezcla | V1, *k*′ = 1 | V1, *k*′ = 2 |
|---|---|---|---|---|
| R_k (H2) | +33,3 | +0,0 | +0,0 | +0,0 |
| R_hg (H2), HG | +22,5 | +0,0 | +0,0 | +0,0 |
| R_δ (H2, δ′ ≤ 4) | +6,3 (aplica 13 %) | +0,0 (aplica 0 %) | — | — |
| δ′ ≥ 10 ⇒ ganó el de mayor promedio | +24,0 | **+36,5** | **+39,1** | **+33,9** |
| el grado de menos cursos tiene 5 o más ⇒ mayor promedio | +14,1 | **+28,6** | **+35,7** | **+21,6** |
| el menor promedio es de 40 o más ⇒ más cursos | +13,6 | **+28,6** | **+32,5** | **+24,7** |
| hipótesis por esos rasgos (máximo, HP) | +18,0 | **+25,8** | **+39,1** | +21,2 (HG) |
| RL1, par HP = HG (métrica del par) | +8,3 | +16,7 | **+25,0** | +8,3 |
| RL2, hipótesis contrarias a «Ganó…» (condicional al ganador) | +33,3 | +41,7 | +50,0 | +33,3 |

- δ′ ≥ 10 en la rama CURSOS 20,9 % y en la rama PROMEDIO 93,9 % (el costo anotado en la tanda 3). HG y HP dado *k*′: 50,0 %
  en los dos estratos. Por rama, R_hg da +33,3 pp con signos opuestos (la rama no es observable; en la mezcla, 0).
- Cascada F1 de V1, con la Parte 1 real (semillas 1 a 100): 2,50 versiones de cada 100 sin candidatos en el espacio
  base y 0,50 tampoco en el ampliado (*n*′ ≤ 10).
- **Por qué no hay arreglo dentro del reparto autorizado.** En *k*′ = 1, CURSOS con HG y HP verdaderas exige
  *a*′ ≥ *n*′·δ′ + 6 (*D*′ pequeño) y PROMEDIO con las dos falsas, 2·*a*′ < (*n*′ − 1)·δ′ (*D*′ grande): para cada δ′, sus
  cursos no se solapan (tabla de soporte al principio del archivo de salida). V2, con δ′ equilibrado dentro de *k*′:
  «5 o más cursos» +47,2 pp y «promedio de 40 o más» +42,1 pp; además, en *k*′ = 2 las seis celdas no comparten ningún
  valor de δ′. V3, con los cursos equilibrados: δ′ ≥ 10 +44,8 pp y «promedio de 40 o más» +38,6 pp. En el fondo,
  «hipótesis falsas» exige *D*′ grande, y *D*′ grande hace evidente al ganador por las magnitudes de la tabla.

- Archivo: `bateria_partes_2_a_6.R`, versión «L-C4 v1 (2026-10-31)», **md5 `de20e0a986f86d44f0d3412517d2f07b`**.
  Congelada a las 2026-10-31, tras la línea base y antes de cualquier medida del reparto de H2. Cambiar una regla es
  hacer una batería nueva y volver a medir todo el histórico.
- Línea base sobre el md5 `96cfcf35…` (`salida/bateria_linea_base_96cfcf35.txt`, `salida/medir_bateria_cloze.R`):
  las sondas de H1 y H2 salen en rojo, así que la batería las ve. Parte 2 exacta: P2-L1 «contiene “total”» 100,0 %
  en «comparar» (+75,0 pp) y 70,0 % en «promedio» (+45,0 pp); P2-L2 70,0 % y P2-L3 75,0 % en «promedio». Parte 6,
  N = 100: R_k +31,3 pp (mezcla), R_hg +24,0 pp (mezcla), R_δ +12,8 pp (rama CURSOS).

## Firmado en la tanda 3: X6, X10 y X11 en la canónica (L-C3 ampliada)

La tanda 2 se detuvo porque, al cablear la decisión 3.2 a regla por regla, salieron tres reglas cruzadas más por
encima de +8 pp en la canónica. El profesor las firmó el 2026-10-30 («a · aclaración sí»; registro en
`../.claude/CLAUDE.md` § «Decisión del profesor tras la tanda 2»).

| Regla (canónica) | Exacta | Muestra n = 30 (tanda 2, solo registro) | Peso en la mezcla | Mutante |
|---|---|---|---|---|
| X6 más palabras en común con la clave de la Parte 2 | 41,7 % (+16,7 pp) | 33,3 % (+8,3 pp) | +1,67 pp | MC19 |
| X9 S_U + la que nombra la ventaja del ganador | 75,0 % (+25,0 pp) | 75,0 % (+25,0 pp) | +2,50 pp | MC18 |
| X10 S_U + más palabras en común con la clave de la Parte 2 | 66,7 % (+16,7 pp) | 61,7 % (+11,7 pp) | +1,67 pp | MC20 |
| X11 S_U + más palabras en común con las verdaderas de la Parte 6 | 62,5 % (+12,5 pp) | 66,7 % (+16,7 pp) | +1,25 pp | MC21 |

- **Puerta nueva (tanda 3):** `enumerar_canonica_cloze.R`, enumeración exacta **conducida por el código**: sustituye
  `sample()`/`sample.int()` en el entorno del `.Rmd`, ramifica los seis sorteos que deciden las reglas (192 hojas) e
  integra exactamente la fila del caso paralelo (X1 a X4) y los distractores de la Parte 2 (X5). Una transcripción
  del modelo no ve un mutante que cambie un sorteo: por eso X9 no tenía mutante posible con `x_canonica_exacta.R`.
- Las once reglas, exactas en la canónica: X1 +0,0, X2 +0,0, X3 −1,0, X4 +0,3, X5 −25,0, X6 +16,7, X7 −16,7,
  X8 +0,0, X9 +25,0, X10 +16,7 y X11 +12,5 pp. Fuera de lo firmado, ninguna pasa de +8 pp (la tanda 2 tenía X3 en
  +5,0 pp con la muestra).
- X6 a X11 coinciden con la transcripción de `salida/x_canonica_exacta.R`, y no cambian entre las semillas canónicas
  1001 y 1028 (otro orden de las opciones de la Parte 1).

## FASE 2C r1 (2026-10-31): `RECHAZAR`

`AgenteDetractor` nuevo, lanzado sin `name:`, sobre el md5 `96cfcf35…`. Reporte íntegro, recuperado de su
transcripción: `salida/fase2c_r1_detractor.md`. Citas: `verificar_citas_detractor.py` → 14 citas, 0 problemas.
Sin ningún defecto de CORRECCIÓN: 0 discrepancias de 1500 semillas contra su oráculo propio en las Partes 3 a 6.

| ID | Severidad · tipo | Qué | Estado |
|---|---|---|---|
| H1 | ALTA · diagnosticidad | Parte 2, canal léxico: «contiene “total”» acierta 100 % en «comparar» y 70 % en «promedio» (enumeración exacta; +60 pp con los dos tipos). Las Partes 3 y 4 dicen «en total». En «promedio», además, «descarta “Lo que recogió …”» 70 % y el condicional en -ría 50 %. Verificado aparte leyendo los textos | **Abierto.** Se cierra con redacción (permitido sin consulta), pero se agrupa con H2 y H3 en la pasada 2 de §P7-D |
| H2 | ALTA · diagnosticidad | Tabla del otro colegio: *k*′ predice quién ganó (R_k 80,0 %, N = 100; canónica 82,5 % exacta) y el veredicto de HG (74,0 %); δ′ ≤ 4 ⇒ CURSOS (14 de 14). Decide 2 o 3 de las 4 marcas de la Parte 6. Verificado aparte: `salida/p7f_h2_espacio_96cfcf35.txt` | **Abierto, al profesor** (no es redacción; ver abajo) |
| H3 | BAJA · otro | Encabezado fijo «Parte 2 · Lecturas del promedio que no sirven» también en el tipo «comparar» | Abierto (redacción de la Solution propia) |
| H4 | BAJA · robustez de la puerta L-C3 | `enumerar_canonica_cloze.R` reconocía los sorteos libres solo por su texto | **Corregido** (verificador, no el `.Rmd`): cada hoja cuenta las llamadas libres; mutante **MC22** (v_c con el texto de un sorteo libre) cazado por su sonda. `verificar_espacio_cloze_r3b.txt`: exit 4 |

## Pendiente de decisión del profesor (por esto se detuvo la tanda 3) — RESUELTO

> Lo decidió el profesor el 2026-10-31: «1 a · 2 a · i ii iii» tras la tanda 3 y «H2 a · 1 no, 2 sí» tras la tanda 4
> (`../.claude/CLAUDE.md`). Se conserva como registro.

1. **H2.** Cerrarlo exige cambiar cómo se sortea la fila del otro colegio (`data_generation` de las Partes 2 a 6),
   no solo la redacción. Enumeración previa (§P7-F, `salida/p7f_h2_espacio.R`):
   - **Estructural:** en CURSOS con *k*′ = 1 la HG es siempre verdadera (0 filas con HG falsa); en PROMEDIO casi
     no hay δ′ ≤ 4 (1 de 976 en el espacio base) ni *k*′ = 3 (6 de 976).
   - **Reparto candidato:** *k*′ ∈ {1, 2}; *k*′ = 1 solo en CURSOS (HG=V, HP=V) y en PROMEDIO (HG=F, HP=F), con la misma
     masa; δ′ ≥ 5 en las dos ramas. Equilibra rama, HG y HP dado *k*′ (cálculo de masas en el reporte del
     orquestador). **Costo:** con *k*′ = 1 las dos hipótesis llevan siempre el mismo veredicto (regla relacional
     nueva) y δ′ ≥ 10 queda más frecuente en PROMEDIO. Hay que medirlo con una batería congelada antes de elegir.
   - Opciones: (a) rediseñar el sorteo (pasada 2 de §P7-D) con esa batería; (b) firmar H2 como override con sus
     cifras (R_k 80,0 % N = 100 y 82,5 % canónica; R_hg 74,0 %; R_δ 14/14); (c) otra que proponga el profesor.
2. **Canónica de la Parte 1, regla por regla** (al aplicar la aclaración firmada el 2026-10-30). «O-1 corregido»
   firmó esa canónica como un solo número de estrato, el máximo de la batería (0,10 × 75 pp). Con la aclaración, ese
   máximo no cubre las demás reglas. Exacto, 24 órdenes (`salida/p1_canonica_exacta_96cfcf35.txt`): **12 de las 48
   reglas** de la batería §P7 del SCHOICE pasan de +8 pp (3 a +75,0; 5 a +25,0; 4 a +8,3) y **12 sondas** de E4
   (S5, S7, S_U + S1, S_U + S1 + suma, S_U + atípica y S_U + las tres a +75,0; S1, S2, S3, S_U y S_U + suma a +25,0;
   Atípica a +8,3). Fuera de la batería (§P7-C, no obliga en este ciclo): «descarta la más larga», +8,3 pp. Es
   herencia del SCHOICE aprobado (E1), con el texto verbatim (H-2): no se corrige en el CLOZE. Toca también al SCHOICE.
3. **H1 y H3** se corrigen con redacción en la misma pasada que H2, si el profesor elige (a). Mover los textos de la
   Parte 2 mueve X5, X6 y X10 en la canónica: si X6 o X10 pasan de +16,7 pp, vuelve al profesor (L-C3).

## Qué es

Versión CLOZE de la pregunta impresa 50 del simulacro SAI «Nivelación 6» de grado 10 (MAT-2026-07-050,
Argumentación, D3.10, Nivel 3, DOK 2). Seis partes, `schoice|schoice|num|num|num|mchoice`, pesos `3|1|1|1|1|1`:

| Parte | Tipo | Qué pide | Peso |
|---|---|---|---|
| 1 | schoice | La pregunta oficial. Por código es el SCHOICE de `..`: misma generación, mismo RNG (E1) | 3 |
| 2 | schoice | Qué indica el promedio por curso, o qué se compara para decidir quién gana (clave conceptual fija, 3 paráfrasis por tipo) | 1 |
| 3 | num | Total de un grado del otro colegio (orden de mención sorteado) | 1 |
| 4 | num | Total del otro grado | 1 |
| 5 | num | Cuántos kilos más recogió el grado que ganó, sin nombrarlo | 1 |
| 6 | mchoice | HP, HG, una de ventaja (4 redacciones) y «Ganó el concurso el grado X» (aprobada tal cual, decisión 1 a) | 1 |

Las Partes 2 a 6 trabajan sobre **otro colegio** (concurso entre dos grados, de sexto a noveno), con una tabla
propia. Motivo: la clave de la Parte 1 nombra a la sede ganadora y cita *D* = *T*_G − *T*_P; pedir *D*, *T*_G o
*T*_P, o nombrar a la ganadora, la delataría (Incidente Y, V11).

## Firmas y decisiones del profesor (resumen; texto completo en `../.claude/CLAUDE.md`)

- **F-1 a F-4 (2026-10-30, dry-run):** Flujo B = n; «O-1 corregido» extendido a la Parte 1; pesos `3|1|1|1|1|1`;
  estructura Opción A. Nota que completa F-4: «No nombran sedes de la Parte 1. Al menos una verdadera y una
  falsa, sin pares complementarios, y el veredicto se sortea primero».
- **Tras la tanda 1 (2026-10-30), «1 a · 2 a · 3.1 a · 3.2 a · 4 sí»:** la Parte 6 queda tal cual; los BAJA 1, 2 y
  4 de la 6b (Parte 1) son residuo y sus espacios duros van al próximo ciclo del SCHOICE; un canal cruzado nacido
  de la redacción nueva se cierra endureciéndola, sin parar, y solo vuelve al profesor si queda abierto; X9 en la
  canónica firmada (+25,0 pp); `render_html_shots.R` se arregla en otra tarea (no usarlo para 320 o 360 px). Quedó
  arreglado en `afac46a1`, y la premisa resultó falsa: ver «Cómo repetir cada medición».

## Leyes del CLOZE y sus guardas (tandas 2 a «Va»)

| Ley | Guarda | Mutante → sonda (E6 de `verificar_espacio_cloze.R`) |
|---|---|---|
| L-C1 · Parte 6 | `stopifnot` «Parte 6: HP, HG, una de ventaja y la del ganador, nada más» (nuevo); «sin pares complementarios»; «al menos una verdadera y una falsa»; V10; veredictos 50 % ± 12 | MC16 (nuevo: una conceptual en lugar de la del ganador), MC11 (`v_d` sorteado aparte), MC5, MC6, MC13: todos cazados por su sonda |
| L-C2 · canales cruzados | E3: `veredicto_cruzado()` deja PENDIENTE DE FIRMA (exit 3) todo canal > +8 pp; E1; V11 por valor | MC17 (nuevo: la rama del otro colegio copia la de la Parte 1; X8 +25,0 pp, n = 181 → pendiente), MC14, MC15, MC1, MC2 |
| L-C4 · batería congelada por gap (tanda 4) | E8: md5 de `bateria_partes_2_a_6.R`; Parte 2 exacta (posición en muestra), Parte 6 por estrato con el p95 del máximo permutado; > +8 pp = PENDIENTE | MC23 (clave de la Parte 2 siempre primera: P2-P1 en rojo), MC24 (ganador siempre en la fila 1: P6-P1 en rojo) |
| L-C5 · H2 en la Parte 6, regla por regla y por estrato («H2 a», tanda 5) | E8: `FIRMA_H2` (69 celdas) y `h2_juzgar()` sobre la enumeración exacta de `enumerar_h2_cloze.R` (100 Partes 1, con las exclusiones de V11); dentro, «cubierto» (exit 4); por encima o fuera de la firma, pendiente | MC25 (PROMEDIO con *k*′ = 1: K1 +50,0 pp), MC26 (CURSOS con δ′ < 10: D2), MC27 (`v_c <- !v_hp`: RL2 · HP); las demás celdas, sin mutante propio |
| L-C3 ampliada · canónica de la Parte 1 regla por regla (decisión 2 a, tanda 4) | E4: `FIRMA_P1_CANON` (12 reglas) y `FIRMA_P1_SONDAS` (12 sondas), exactas; dentro, «cubierto» (exit 4) | MP1 («quedarse con 'promedio'» a +75,0), MP2 («más dígitos» a +75,0), sobre copias del registro canónico; las nueve al 100 % no pueden subir |
| L-C3 · X6, X9, X10 y X11 en la canónica (ampliada en la tanda 3) | `FIRMA_CANON` (+16,7, +25,0, +16,7 y +12,5 pp, exactas); `veredicto_canon()` sobre la enumeración exacta de `enumerar_canonica_cloze.R`, las once reglas: dentro, «ROJO, cubierto» (exit 4, nunca PASS); por encima, o cualquier otra regla > +8 pp, pendiente | MC18 (X9 +50,0), MC19 (X6 +25,0), MC20 (X10 +25,0), MC21 (X11 +25,0): cada uno saca su regla de «cubierto» |
| L-C2 · X5, X6 y X10 exactas sobre el sorteo de la Parte 2 y muestrales sobre la Parte 1, N = 100 por estrato (decisión «Va» 1) | E3: `medir_exacta_p2()` (3 claves × C(5,3) tríos) y `veredicto_cruzado()` en la mezcla y en cada rama; supuesto del sorteo con `ok()` (300/300); el rótulo declara las dos cosas desde la r9 | MC37 (las tres claves ganan «aunque fue menor»: X6 exacta +38,2 pp en CURSOS → pendiente) |
| L-C6 · residuo de la Parte 2 «promedio» (decisión b, tanda 8) | E8: `FIRMA_P2` (cinco reglas, exactas con `medir_p2_exacta`); dentro, «cubierto»; por encima u otra regla > +8 pp, pendiente | MC31 a MC35, una por regla |
| L-C7 · Parte 6 por palabra, regla por regla (decisión «Va» 2) | E8: `p6_token_juzgar()` contra `firma_p6_palabras_f98f9dda.tsv` (md5, 216 reglas) y el tope de su estrato, sobre la enumeración exacta de L-C5 | MC36 («duda», fuera de la lista), MC38 («justamente»: pendiente con la firma regla por regla, cubierta con la de estrato), MC39 (una regla de la lista por encima de su cifra y bajo el tope de su estrato: sonda directa y «perdió hubiera tenido» en BP_gano, +12,5 → +25,0) |

## Tanda 2 (2026-10-30): qué cambió en el `.Rmd` (solo tras el marcador del bloque P1)

1. **BAJA 3 de la 6b:** en «Respuestas · Parte 6» cada afirmación va en su renglón, agrupadas por veredicto
   (agrupar no afirma nada sobre posiciones, Incidente Q).
2. **Espacio duro** (`ED <- " "`) entre cifra y unidad en el texto propio de las Partes 3 a 6, en su Solution y
   en su retroalimentación. **No** en la Parte 1 ni en sus párrafos de la Solution, que siguen iguales al SCHOICE
   (decisión 2 a). **No** en las celdas de la tabla del otro colegio, que siguen como la tabla de la Parte 1 (las
   dos medidas en la caja `.qtext` del Moodle del colegio, 0/100 fuera de su caja; 0 cortes dentro de las tablas).
3. **Guarda de composición de la Parte 6** (L-C1), antes de las otras guardas de la Parte 6.

## Evidencia (`salida/`)

| Qué | Archivo | Resultado |
|---|---|---|
| Paso 9, mediciones repetidas (tanda 9b) | `diversidad_sustantiva_r9.txt`, `diagnosticidad_r9.txt` | md5 vigente en la cabecera, N = 100, exit 0 y 0; idénticas a las r8: 6 de 6 gaps (p2 3 valores, fija-justificada); V9 PASS, p6 excluida por ser mchoice |
| Paso 10, V5 a V11 (tanda 9b) | `v5_v11_r9.py` → `v5_v11_r9.txt` | 0 fallos: 390 gaps en 101 XML sin imágenes; 100 HTML sin `##ANSWER`; V6 por M6/M4; V7 N/A; V10 guarda + MC6 + marcas recalculadas; V11 por valor, contenido e independencia |
| Paso 10, literalidad (tanda 9b) | `literalidad_exextra_r9.py` → `literalidad_exextra_r9.txt` | 0 fallos: los 6 campos oficiales, literales en el catálogo (D3.10 = `N3[9]`, estándar `Aleatorio_6-7[7]`) e iguales a `ITEM_OFICIAL.md`; DOK 2, Bloom Evaluar, Nivel 3 coherentes |
| FASE 2C r3 | `fase2c_r3_detractor.md`; scripts en `fase2c_r3/` | `APROBAR_CON_CAMBIOS`: H1 y H2 MEDIA, H3 BAJA; 0 CORRECCIÓN; citas 9/0 |
| Paso 8, oráculo reconstruido (tanda 9a) | `coherencias_r9/` (`verificacion_N100.txt`, `verificacion_formatos.txt`, `control_moodle.txt`, `control_html_pdf.txt`) | `VEREDICTO_COHERENCIAS: OK`; N = 100, 0 fallos; 88 mutaciones cazadas por su sonda |
| Verificador, cierre de la tanda 9a (MC39) | `verificar_espacio_cloze_r9.txt` (445 s); `comparar_r9_va.py`/`.txt` | **exit 4: EN ROJO BAJO OVERRIDE FIRMADO (327 cifras; 0 fallos, 0 pendientes)**. MC39 y su sonda directa, cazados; contra la «Va», solo cambian MC39 (5 líneas) y los 13 rótulos «muestral sobre la Parte 1» |
| Verificador, tanda «Va» | `verificar_espacio_cloze_va.txt` | **exit 4: EN ROJO BAJO OVERRIDE FIRMADO (327 cifras: 13 de «O-1 corregido», 2 de L-C3, 24 de la decisión 2 a, 67 de L-C5, 5 de L-C6 y 216 de L-C7; 0 fallos, 0 pendientes)**. X5, X6 y X10 exactas sobre la Parte 2 (muestrales sobre la Parte 1) ≤ +6,0 pp; MC36, MC37 y MC38 cazados por su sonda; lo demás, idéntico a la r8b |
| Verificador, tanda «Va», primera corrida completa | `verificar_espacio_cloze_va1.txt` (431 s) | exit 4, igual que la final salvo MC37, que medía solo la mezcla (+8,5 pp, n = 200) |
| Diagnóstico de la FALLA del sorteo y de MC37 | `diag_falla_sorteo_va.R`/`.txt`, `mc37_por_estrato_va.R`/`.txt` (semillas 1:100 y 1:200) | `sol_p2` lógico; con `-sol_p2` pasan 26/100 (solo la clave en la posición 1); con `!sol_p2`, 100/100. MC37 mutado, X6 exacta: mezcla +8,5, CURSOS +38,2, PROMEDIO −25,0, canónica +75,0 pp (sin mutar: −2,5, −1,8, +1,1, −25,0) |
| Primera corrida de DeepSeek (interrumpida) | `verificador_va.txt` | FALLA falsa del sorteo (`-sol_p2` lógico) y error en MC37 (`mutar()` sin definir); X6 exacta ya igual a `x6_estratos_t8.txt` |
| Verificador, tanda 8 | `verificar_espacio_cloze_r8b.txt` | exit 3: 1 pendiente (X6 en PROMEDIO, +8,5 pp en la muestra) |
| Verificador, tanda 5 | `verificar_espacio_cloze_r5.txt` (386 s) | exit 3: 34 pendientes (22 de L-C5, las celdas de «H2 a» por encima de lo firmado; 12 de la Parte 2, H1); 86 cifras «cubiertas» (13 de «O-1 corregido», 4 de L-C3, 24 de la decisión 2 a y 45 de L-C5); 8 residuos; 0 fallos; MC25, MC26 y MC27 (L-C5), MC23, MC24, MP1 y MP2 cazados por su sonda; E4 sin `SIN_COBERTURA` (§P7-G) |
| L-C5, cifras exactas y contrastes | `h2_exacta_96cfcf35.txt` (`medir_h2_exacta.R`), `h2_contraste_96cfcf35.txt` (`contrastar_h2_ponderacion.R`) | 22 de 69 celdas por encima de lo firmado; ponderación del código = transcrita con exclusiones (1,1·10⁻¹⁹); ruido de la Parte 1 0,3 pp de mediana |
| Verificador, tanda 4 | `verificar_espacio_cloze_r4b.txt` | exit 3: 65 pendientes, todos de L-C4 (H1, H2 y los demás canales de la batería; 22 más quedan NO CONCLUYENTES); 41 cifras «cubiertas» (13 de «O-1 corregido», 4 de L-C3 y 24 de la decisión 2 a); 0 fallos; MC23, MC24, MP1 y MP2 cazados por su sonda |
| Batería L-C4, línea base | `bateria_linea_base_96cfcf35.txt`, `medir_bateria_cloze.R` | H1 y H2 en rojo; además, en la Parte 2 «comparar»: P2-E4 +55,0, P2-E2 +45,0, P2-E1 +41,7, P2-L4 y P2-L6 +12,5, P2-F3 +11,1, P2-F4 +9,2 pp (exacta) |
| Reparto de H2 (paso B) | `reparto_h2_96cfcf35.txt`, `medir_reparto_h2.R` | V1 no viable (tabla de arriba) |
| FASE 2C r1 | `fase2c_r1_detractor.md` | `RECHAZAR`; H1, H2 ALTA (diagnosticidad), H3, H4 BAJA; 0 CORRECCIÓN; citas 14/0 |
| Verificador tras H4 | `verificar_espacio_cloze_r3b.txt` (71 s) | exit 4, igual que la r3 más MC22 cazado por su sonda (22 mutantes en E6) |
| Canónica de la Parte 1 regla por regla | `p1_canonica_exacta.R`, `p1_canonica_exacta_96cfcf35.txt` | 12 de 48 reglas y 12 sondas por encima de +8 pp (exacto, 24 órdenes) |
| §P7-F de H2 | `p7f_h2_espacio.R`, `p7f_h2_espacio_96cfcf35.txt` | R_k 80,0 %, R_hg 74,0 %, R_δ 14/14 (N = 100); filas viables por rama, combinación, *k*′ y δ′ |
| Paso 9 (preparado, sin medición oficial) | `diagnosticidad_por_rama.R` | V8 y V9 por gap y por estrato, N = 100; se corre cuando se selle el paso 8 |
| Verificador propio, tanda 3 | `verificar_espacio_cloze_r3.txt` (68 s) | **exit 4: EN ROJO BAJO OVERRIDE FIRMADO (17 cifras: 13 de «O-1 corregido» y 4 de L-C3; 0 fallos, 0 pendientes)**. E3 de la canónica por enumeración exacta (192 hojas, masa 1, sin error, invariante entre semillas, igual a la transcripción). E6: 21 mutantes cazados por su sonda (MC18 a MC21 por la enumeración). El resto, igual que la tanda 2 |
| Verificador propio, tanda 2 | `verificar_espacio_cloze_r2.txt` (41 s) | **exit 3: PENDIENTE DE FIRMA (3; 0 fallos)**. E1 111/111 (20 canónicas). E2 330/330 en todos los gaps. E3: V11 por valor, contenido e independencia OK; batería cruzada ≤ +8 pp en la mezcla y por rama (tablas idénticas a la tanda 1); en la canónica, X9 cubierta y X6, X10, X11 pendientes. E4 = SCHOICE (13 cifras dentro de «O-1 corregido»). E5 300/300 únicas. E6: 18 mutantes cazados por su sonda. E7 4/4 |
| Enumeración exacta de la canónica | `x_canonica_exacta.R`, `x_canonica_exacta_e58f3df9.txt` | X6 +16,7, X7 −16,7, X8 0,0, X9 +25,0, X10 +16,7, X11 +12,5 pp (96 casos ponderados). Las Partes 2 a 6 no cambiaron su RNG en la tanda 2: la cifra vale para el md5 nuevo |
| Render, tanda 2 | `render_r2.log`, `html/`, `pdf/`, `tex/`, `docx/`, `moodle/` | HTML, PDF, DOCX y Moodle OK; NOPS N/A (`cloze exercises`); `\pandocbounded` solo como `\providecommand`; `\LTcaptype{none}` con guarda; 0 imágenes en gaps; 0 `##ANSWER` en el HTML |
| PDF revisado a ojo | `revision_pdf_r2/c1-*.png` | Canónica, 5 páginas: la lista de Respuestas, anidada y dentro del margen. Queda «20 / kilos.» en el párrafo de la Parte 1 (igual que el SCHOICE: residuo 2 a) |
| Arsenal (hook v6.3) | `~/.cache/post-exams2-validation/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1/20261030-231556-1522539/informe.txt` | 0 errores, 7 advertencias (2B sin PDF en el comando del hook; 2E «Tarea»; 2F tabla de metadatos, descripción de evaluación, análisis de distractores, patrón «Es posible que los estudiantes», `letra_correcta`); 2G 100/100; 2H 10 semillas, 0 anomalías |
| Moodle del aula | `exportar_r2.log`, `moodle_aula/` | 30/30 con pesos `3|1|1|1|1|1`; 60 MULTICHOICE_V, 30 MULTIRESPONSE, 90 NUMERICAL, 0 desplegables; 0 etiquetas en 180 gaps |
| Cortes cifra-unidad por CDP, pareado con la 6b | `r2_cdp/6b/medir_cortes_r2.txt`, `metricas_extra.json` (N = 24, las semillas de la 6b) | En la Solution, de 132/189/94/128 a 73/62/62/71 cortes (320/360 px y cajas de 270/310 px). Los que quedan llevan espacio normal: todos en texto de la Parte 1. Con espacio duro, 0. En opciones y enunciado, 21/8/14/24, sin cambio (Parte 1) |
| Captura CDP | `r2_cdp/6b/capturar_r2.txt`, `metricas.json`, `strips/` (N = 24 × 5 modos) | `innerWidth` = 320, 360, 1024, 320 y 360 en los cinco modos; 0 desbordes, 0 fugas de markup, ancho del documento ≤ viewport en 120/120 |
| Tanda 1 | `verificar_espacio_cloze_r1_*.txt`, `render_r1.log`, `6b/`, `stress_n100/`, `qtext/` | Ver el historial de este archivo en la tanda 1: 6b APTO_CON_OBSERVACIONES (4 BAJA), stress test N = 100 PASA, `.qtext` 0/100 fuera de su caja |

## Residuos declarados

- **Firmados (F-2), en la Parte 1 por rama:** los de «O-1 corregido», iguales a lo firmado (13 cifras; ver
  `verificar_espacio_cloze_r2.txt`). Canónica: 0,10 × 75 pp = +7,5 pp.
- **Firmados (L-C3):** en la canónica, X6 +16,7, X9 +25,0, X10 +16,7 y X11 +12,5 pp, exactas (pesos en la mezcla
  +1,67, +2,50, +1,67 y +1,25 pp). Cubiertos por dominancia: el atajo de «O-1 corregido» acierta el 100 % ahí.
- **Decisión 2 a (Parte 1):** «15 / kilos.» partido en opciones y Solution; «El / Bosque» y «La / Pradera» fuera
  de la tabla (semillas 6 y 13); «ganará»/«ganó» en 8 de 24 contextos. También los cortes cifra-unidad que quedan
  en los párrafos de la Parte 1 de la Solution, que son iguales a los del SCHOICE.
- **Cascada F1:** 1 de 330 versiones usó el paso 2 (espacio ampliado); ninguna, el 3 o el 4.
- **V11 «a un paso»:** *D* sale de una operación entre dos números de la tabla del otro colegio en el 11-17 % de las
  versiones (por rama); alguna respuesta de las Partes 3 a 5 sale de una operación entre dos números de la Parte 1
  en el 33-63 %. Coincidencias sin relación natural; las relaciones naturales están excluidas (0/330); X3 da
  23-24 %, por debajo del azar.
- **Parte 2:** clave conceptual fija por tipo (V8, fija-justificada): 6 textos de clave.
- **Firmados (decisión b, tanda 8):** el residuo de la Parte 2 «promedio» (L-C6, cinco reglas) y la regla por palabra de la
  Parte 6 (L-C7, regla por regla desde la decisión «Va»: `firma_p6_palabras_f98f9dda.tsv`).
- **6b de la tanda 8 (decisión «Va»):** «sirven» queda sola en su línea a 360 px (24/24) y «cuadrado.» a 320 px (15/96).
- **X6 por clave y por rama (H1 de la FASE 2C r3, decisión del 2026-11-02):** las claves 1 y 2 pasan de +8 pp en 2 de 5 réplicas
  en CURSOS (hasta +11,0) y en 3 de 5 en PROMEDIO (hasta +10,5); con las 500 juntas, +6,4 y +7,7. Por rama, como mucho +7,3; en
  la mezcla por clave, +1,0 / +1,0 / −2,2.
- **H3 de la FASE 2C r3, diferido:** la Solution de la Parte 6 dice «solo si … fuera mayor» (condición necesaria; la relación es
  «si y solo si»). No es falsa.
- **La vara §P7** está medida sobre opción múltiple simple; aplicarla gap a gap no está calibrado para CLOZE.

## Uso en el aula (aprobado el 2026-11-02)

- **Moodle**, desde `cloze/salida`: `Rscript --no-init-file ../exportar_moodle_cloze.R [n]` (n de 1 a 100; 30 por
  defecto). La pregunta vale 8 puntos.
- **DOCX: no apto para estudiantes** (deja los `##ANSWERi##` a la vista). **Para papel, el SCHOICE.** NOPS es N/A.

## Cómo repetir cada medición (siempre desde `cloze/salida`)

```bash
cd A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/salida
Rscript --no-init-file ../verificar_espacio_cloze.R > verificar_espacio_cloze.txt; echo $?   # E1-E7; 4 = rojo cubierto, 3 = pendiente
Rscript --no-init-file x_canonica_exacta.R                                                   # contraste: modelo transcrito (X6-X11)
Rscript --no-init-file medir_h2_exacta.R > h2_exacta.txt                                     # L-C5: H2 exacta con el código, 100 Partes 1 (45 s)
Rscript --no-init-file contrastar_h2_ponderacion.R > h2_contraste.txt                        # L-C5: ponderación y ruido de la Parte 1 (80 s)
Rscript --no-init-file ../render_cloze.R                                                      # 4 formatos + NOPS N/A + .tex
Rscript --no-init-file ../exportar_moodle_cloze.R 30                                          # Moodle del aula + guardas
cd r2_cdp/6b && Rscript --no-init-file render_6b.R && python3 medir_cortes_6b.py ./html . ./fonts.conf <tmp> \
  && python3 capturar_cdp_6b.py ./html . ./fonts.conf 0 <tmp>                                 # CDP real: sin -I (usa websockets del usuario)
```

El hook v6.3 se dispara solo con un `exams2*("<ruta>.Rmd")` literal en el comando (`render_cloze.R` no lo dispara).
`render_html_shots.R` captura por CDP desde `afac46a1` y comprueba que la página mida el ancho pedido. Corrección del
2026-10-31: las capturas viejas de 320 y 360 px también eran válidas en lo que es CSS, porque `--screenshot` vuelve a
maquetar al ancho pedido. El 500 solo lo veían el JavaScript de carga y `--dump-dom`.

**Trampas:** nada de comillas invertidas dentro de los chunks; el bloque P1 no se edita a mano (se re-empalma con
`empalmar_bloque_p1.R` desde el SCHOICE). Un cambio en la Parte 1 exige al profesor. El Edit de las herramientas
convierte un ` ` escrito en el texto en el carácter literal: comprobar con `grep -nP '\xa0'`.

## Próximo paso

**Ahora (2026-11-02):** el CLOZE está aprobado para el aula (11/11). Siguen la evidencia de aula (Nivel 3) y, después,
la promoción (`/promover-ejercicio`). En el próximo ciclo: H3 (la Solution de la Parte 6), la revisión de MC39 por un detractor
nuevo y el pendiente del SCHOICE. Los puntos de abajo son historia: ya están cumplidos.

0. ~~Cablear las decisiones 1 y 2 de «Va»~~ (tanda «Va») y ~~la tanda 9a: FASE 2C r3 y paso 8~~ (ejecutados, sin sellar).
1. ~~**Cerrar la FASE 2C r3 antes de la 9b**~~: cerrada el 2026-11-02 (H1 residuo, H2 MC39, H3 diferido; ver «Decisión del
   profesor tras la tanda 9a»). Texto original:
   - **H1, al profesor.** ¿Lee X6 clave por clave en cada rama? Por rama queda en «ok» en las cinco réplicas (máximo +7,3 pp);
     por clave, las claves 1 y 2 pasan de +8 pp en 2 de 5 réplicas en CURSOS (hasta +11,0) y en 3 de 5 en PROMEDIO (hasta
     +10,5). El estudiante puede ver la clave de la Parte 2, no la rama; en la mezcla por clave, +1,0 / +1,0 / −2,2 pp en la
     muestra del verificador. Si la firma por clave, la cifra sale de enumerar los textos de la Parte 1 (regla #23) o de
     réplicas declaradas. El rótulo de E3 ya dice «exacta sobre la Parte 2, muestral sobre la Parte 1» (r9).
   - ~~**H2, al coordinador.** MC39 en E8~~: hecho el 2026-11-02, cazado por su sonda (`salida/verificar_espacio_cloze_r9.txt`).
     El próximo detractor lo revisa.
   - **H3, al profesor (opcional).** Reescribir el «solo si … fuera mayor» de la Solution de la Parte 6. Si se aplica, cambia el
     `.Rmd`: reapertura desde `generacion_rmd`, pasos 3 a 6 y FASE 2C r4 con un detractor nuevo.
2. ~~**Tanda 9b**~~: hecha el 2026-11-02, con los pasos 7 a 10 sellados (ver «Tanda 9b», arriba). Texto original:
   - paso 7: `salida/fase2c_r3_detractor.md`, con la decisión del 2026-11-02 (H1 residuo, H2 MC39 en la r9, H3 diferido; MC39
     llegó después de la r3 y lo revisa el próximo detractor);
   - paso 8: `salida/coherencias_r9/`;
   - paso 9: `salida/diversidad_sustantiva_r8.txt` (p2 fija-justificada), `diagnosticidad_r8.txt` (p6 excluida por mchoice) y
     `diagnosticidad_por_rama_r8.txt`, medidos sobre `f98f9dda…`; E5 300/300 en `verificar_espacio_cloze_r9.txt`;
   - paso 10: `salida/v1_v4_r8.txt`, V5 a V11 y la literalidad de los `exextra` contra el catálogo canónico;
   - después, el WAIT_USER #3 con los residuos declarados.
3. **WAIT_USER #3 (paso 11), pendiente del profesor.** El paquete de aprobación, con los residuos y sus cifras, está en el
   reporte de la tanda 9b del orquestador; la lista de residuos es la de «Residuos declarados», arriba.
   - «a»: `workflow-state.sh complete <dir> aprobacion_usuario --nota "<respuesta literal, fecha y md5 f98f9dda…>"`.
   - «r»: describir qué corregir. Se reabre con `workflow-state.sh reopen <dir> --desde <paso> --motivo "…"`, y la FASE 2C r4
     la hace un detractor nuevo.
   - «p»: no se toca nada.
4. **Después de la aprobación:** aula → evidencia de Nivel 3 → promoción. Quedan para el próximo ciclo H3 de la r3, el pendiente
   del SCHOICE (decisión 2 a) y la revisión de MC39 por el próximo detractor.

## Tanda C1 del ciclo 2026-11-02 (2026-11-03): reabierto, 6/11 (registro completo en `../.claude/CLAUDE.md`)

> **Este bloque manda sobre el «Estado» de arriba.** El CLOZE se reabrió (reapertura #5, 13:18:39) y ya no está aprobado.
> `.Rmd`: `f98f9dda…` → **`27f789dc6327f7d6bda72cded61502cd`**. Verificador: `6901624e…` → **`6e76321a22e2026adaa3a4b5f576f070`**.

- **Qué cambió.**
  - La Parte 1, re-empalmada por script desde el SCHOICE aprobado `63e2713e` (líneas 2-612). Trae L-6, V1′, S7 y la
    explicación de ERR-RESTA.
  - La prosa propia de la Parte 1 en la Solution, con L-6.
  - H3 en la Parte 6: «si y solo si … es mayor».
  - Las Partes 2 a 6 no cambian: `.Random.seed`, objetos, veredictos y Solution, iguales en 1000 de 1000 semillas
    (`salida/ciclo_20261102/invariancia_c1.txt`).
- **Decisiones del profesor que rigen** (literales en `../.claude/CLAUDE.md`): la (a) del SCHOICE, extendida a la Parte 1;
  H3, diferido a este ciclo; «Extender por alcance (Recomendada)»; «Mantener 3|1|1|1|1|1 (Recomendada)», con F-3 a +25,5 pp.
- **Verificador:** exit 4, 368 cifras dentro de lo firmado (327 más las 41 de la S2d), 0 fallos y 0 pendientes. MC40 a
  MC49, cazados por su sonda. Ninguna cifra firmada del CLOZE se movió (`comparar_r9_c1.txt`). L-8, sin cablear.
- **Sellos:** paso 3 a las 13:29:50, 4 a las 13:30:15, 5 a las 13:32:17 y 6 a las 13:32:20. Render en
  `salida/ciclo_20261102/render_27f789dc/`.
- **6b pareada:** APTO_CON_OBSERVACIONES (N = 100, a 360 y 320 px). La Parte 1 da 0 en todas las métricas de L-6 (control
  `f98f9dda`: 160/274, 317/335 y 79/98). BAJO nuevo: «diferencia.» sola a 360 px en 99/100, por la redacción de H3.
- **Residuos.** Siguen los de «Residuos declarados», salvo los de la Parte 1 por el espacio duro, que quedan cerrados
  (decisión 2 a de la tanda 1). El BAJA 1 de la r3 en las Partes 3 a 6 no cambia: 259/1249 y 206/1249.

### Próximo paso (tanda C1)

1. FASE 2C r4 con un detractor NUEVO, sin `name:`. Qué debe auditar: `../.claude/CLAUDE.md` § «Ciclo 2026-11-02 del CLOZE,
   tanda C1», incluido MC39.
2. Pasos 8 a 10. En el paso 8, el oráculo de `coherencias_r9/` juzgó el banco de la Parte 2, que no cambió, pero la Parte 1
   cambió de texto. Antes de reutilizarlo, comprobar que sus comprobaciones de la Parte 1 leen bien el espacio duro y la
   unidad dentro de la fórmula: E2 p1 del verificador falló justo por eso (L-C8).
3. WAIT_USER #3 (paso 11): la aprobación anterior era de `f98f9dda`.

**Verificación del coordinador de la tanda C1 del CLOZE (2026-11-03, sesión `42032564`)**, independiente de sus scripts
(`cloze/salida/ciclo_20261102/coordinador/`):
- `verificar_espacio_cloze.R` (`6e76321a…`), repetido: exit 4 en 476 s, con 1409 líneas idénticas a
  `cloze/salida/ciclo_20261102/verificar_espacio_cloze_c1.txt`. Hay 368 cifras dentro de lo firmado, 0 fallos y 0 pendientes.
- Invariancia con un script propio (`invariancia_cloze_coordinador.R`), semillas 1 a 1000:
  - (A) E1: la Parte 1 del CLOZE nuevo (`27f789dc`) es idéntica al SCHOICE aprobado (`63e2713e`) en las 1000. Se compararon
    opciones, clave, feedback, rama, canónica, tabla y respuesta correcta.
  - (B) El `.Random.seed` final coincide con el del CLOZE aprobado (`f98f9dda`) en las 1000, y los 18 objetos de las Partes 2 a 6
    también.
- Los sellos 3 a 6 llevan el md5 `27f789dc` y cada uno va después de su artefacto (la invariancia a las 13:23 y el render a las
  13:30). El SCHOICE no se tocó (`63e2713e`, verificador `031d4726`, 11/11).
- El diff fuera del bloque lo revisé y contiene los comentarios, dos U+00A0 literales pasados a escape, L-6 en la prosa de la
  Parte 1 y H3 en indicativo.

## Tanda C1b del ciclo 2026-11-02 (2026-11-03): H1 y H2 de la FASE 2C r4, 6/11 (registro completo en `../.claude/CLAUDE.md`)

> **Este bloque manda sobre el de la tanda C1 y sobre el «Estado» de arriba.** Reapertura #6 desde `generacion_rmd` (16:27:09).
> `.Rmd`: `27f789dc…` → **`6f6f1b1311b0d681bd974c719ecd75d5`**. Verificador: `6e76321a…` → **`6d37faad687a5fc11e69b763429b6624`**.
> Sin aprobar. Se detuvo antes de la FASE 2C r5. Evidencia: `salida/ciclo_20261102/c1b/`.

- **Qué cambió (decisión del profesor del 2026-11-03, «Reescribir ahora (Recomendada)»).**
  - El párrafo «Parte 6.» de la Solution: el criterio general queda sin cifras («basta comparar ese promedio con la diferencia
    entre los totales: si es mayor, el ganador cambia; si es menor, no»), y cada comparación con cifras va resuelta con los datos
    y con su conclusión. Por ejemplo, en la semilla 10: «Con un curso más, el grado séptimo habría sumado a su total 27 kilos,
    menos que la diferencia de 33 kilos: no le habría alcanzado para ganar. Con un curso menos, el grado sexto habría restado a
    su total 42 kilos, más que la diferencia de 33 kilos: habría perdido.»
  - En el `data_generation`, `hp_supera` y `hg_supera` (la comparación, calculada con los datos) y un `stopifnot` que las ata a
    `v_hp` y `v_hg`. Sin sorteos nuevos.
- **Invariancia, semillas 1 a 1000** (`invariancia_c1b.txt`): `.Random.seed`, E1, los objetos y el `.Rmd` fuera de los dos
  chunks, idénticos a `27f789dc`; la Solution, idéntica salvo el párrafo. En el párrafo nuevo, 2000 comparaciones, 0 falsas y 0
  contra la marca. Control: en `27f789dc`, 656 de 1000 versiones tenían alguna comparación falsa leída suelta.
- **Verificador:** exit 4, 368 cifras dentro de lo firmado, 0 fallos y 0 pendientes; ninguna línea distinta de la C1 fuera de las
  añadidas (`comparar_c1_c1b.txt`). Sonda nueva H1-Sol (falsa, cifra, concl, marca, esp y el control exacto de dos comparaciones
  por versión), con MC50, MC51, MC53 y MC54, y la guarda con MC52, todos cazados por su sonda (MC51 también por el control,
  declarado). MC39 (H2): línea base «cubierto» a +12,5 pp y `cazado_por_su_sonda_y_otra`, con 33 pendientes nuevos fuera de `j39`.
- **Sellos:** paso 3 a las 16:41:59, 4 a las 16:42:07, 5 a las 16:42:13 y 6 a las 16:42:23. Render en
  `salida/ciclo_20261102/c1b/render_6f6f1b13/`; arsenal 0 errores y 6 advertencias, 2G 100/100, 2H 0 anomalías.
- **6b pareada** (`auditor-visual-html`, sin `name:`): ver el bloque que sigue, cuando llegue.

### Próximo paso (tanda C1b)

1. FASE 2C r5 con un detractor NUEVO, sin `name:`. Qué debe auditar: `../.claude/CLAUDE.md` § «Ciclo 2026-11-02 del CLOZE,
   tanda C1b».
2. Tanda C2: el paso 8 con un oráculo r10 nuevo (H3 de la r4, con los seis puntos de su reporte), y los pasos 9 y 10.
3. WAIT_USER #3 (paso 11): la aprobación anterior era de `f98f9dda`.

**6b pareada de la tanda C1b: APTO_CON_OBSERVACIONES** (N = 100 por versión, a 360 y 320 px; informe en
`salida/ciclo_20261102/c1b/6b_c1b/informe_6b_c1b_6f6f1b13.md`). «diferencia.» sola en el último renglón a 360 px: 99/100 en
`27f789dc` → 0/100 en `6f6f1b13`; 0 cortes entre cifra y «kilos» (400 pares) y 0 desbordes; la Parte 1, idéntica y con 0 cortes de
L-6. BAJO declarado: a 320 px, «habría | ganado.» puede partirse a mitad del párrafo (3 de 6 recortes; sin medir en las 100).

## Tanda C1c del ciclo 2026-11-02 (2026-11-03): H1 de la FASE 2C r5, 6/11 (registro completo en `../.claude/CLAUDE.md`)

> **Este bloque manda sobre los de las tandas C1 y C1b y sobre el «Estado» de arriba.** Reapertura #7 desde `generacion_rmd` (18:19:27).
> `.Rmd`: `6f6f1b13…` → **`ef2c96dc1c2b667fa904ea231329b4dc`**. Verificador: `6d37faad…` → **`04349f26559ac32cc7f8f0a439c470b6`**.
> Sin aprobar. Se detuvo antes de la FASE 2C r6. Evidencia: `salida/ciclo_20261102/c1c/`.

- **Qué cambió (decisión del profesor del 2026-11-03, «Corregir ahora (Recomendada)»).** Una sola frase de la Solution: «Por eso, con
  un curso más o uno menos, basta comparar ese promedio con la diferencia entre los totales: si el promedio es mayor, el ganador
  cambia; si es menor, no.» Nada más cambia en el `.Rmd`.
- **Invariancia** (`invariancia_c1c.txt`): deshacer la frase da `6f6f1b13` byte a byte. Semillas 1 a 1000: `.Random.seed`, objetos y E1,
  0 diferencias, y ningún objeto nuevo; Solution idéntica salvo la frase. Texto emitido por `xexams`, semillas 1 a 100: 0 diferencias.
- **Verificador:** exit 4, 368 cifras dentro de lo firmado, 0 fallos y 0 pendientes; las 368 líneas firmadas, idénticas a la C1b, y 0
  líneas distintas fuera de las añadidas (`comparar_c1b_c1c.txt`). Sonda nueva Crit-P6, con control exacto 100 y sus mutantes MC56 y
  MC57. H3 de la r5: `mut_h1()` con la convención `_y_otra` y `_y_control` (MC50 y MC54 `_y_otra`, MC51 `_y_otra_y_control`, MC53 sin
  cambio), el comentario de «marca» corregido y MC55, mutante del entorno que prueba esa componente sola.
- **Sellos:** paso 3 a las 18:32:18.418, 5 a las 18:32:18.605, 6 a las 18:32:18.779 y 4 a las 18:33:01.054. **Desviación declarada:** el
  paso 4 quedó después del 5 y del 6, y los otros tres a 361 ms entre sí, porque una comprobación previa mal puesta bloqueó el del 4
  (exigía «frase nueva 1» en `cloze.xml`, que trae 5 versiones). Los artefactos son anteriores a todos los sellos. Render en
  `salida/ciclo_20261102/c1c/render_ef2c96dc/`; arsenal 0 errores y 6 advertencias, 2G 100/100, 2H 0 anomalías.
- **6b pareada** (`auditor-visual-html`, sin `name:`, N = 100 por versión, a 360 y 320 px): APTO_CON_OBSERVACIONES. Informe en
  `salida/ciclo_20261102/c1c/6b_c1c/informe_6b_c1c_ef2c96dc.md`. Una sola palabra en el último renglón, 0/100; 0 cortes entre cifra y
  «kilos»; 0 desbordes; la Parte 1, idéntica y con 0 cortes de L-6.

**Residuo de H2, con las cifras del md5 final** (`6b_c1c/contar_h2_c1c.txt`; sustituye a las de `6f6f1b13`):

| Corte | 360 px | 320 px | Antes, 360 px | Antes, 320 px |
|---|---|---|---|---|
| «habría ⏎ ganado.» | 0/57 | 0/57 | 0/57 | 57/57 |
| «habría ⏎ seguido ganando.» | 19/51 | 19/51 | 29/51 | 43/51 |
| Páginas con alguno de los dos | 19/100 | 19/100 | 29/100 | 84/100 |
| «no le ⏎ habría alcanzado» | 0/43 | 0/43 | 0/43 | 43/43 |
| «habría ⏎ sumado» | 100/100 | 0/100 | 41/100 | 100/100 |

Control positivo: la referencia `6f6f1b13` reproduce exactamente las cifras de la r5. El corte se desplaza, no desaparece, y no viola
ninguna ley. Por decisión del profesor no lleva espacio duro.

**Lectura que la frase deja abierta (medida, no corregida):** no dice a qué grado se aplica el criterio. Aplicado a una BC con *k* = 1,
que siempre va en sentido inverso (rama PROMEDIO: el que perdió pierde un curso o el que ganó gana uno), da el veredicto contrario en
183 de 1000 versiones (`lectura_criterio_c1c.txt`; sin la acotación eran 421). La lectura buscada es verdadera en 1000 de 1000. Va a la
r6 y al profesor. Por la regla de parada, si solo es un BAJA de redacción, queda como residuo declarado.

### Próximo paso (tanda C1c)

1. FASE 2C r6 con un detractor NUEVO, sin `name:`. Qué debe auditar: `../.claude/CLAUDE.md` § «Ciclo 2026-11-02 del CLOZE, tanda C1c».
2. Según su veredicto: si no abre una CORRECCIÓN, tanda C2, con el paso 8 (oráculo r10 nuevo, con los diez puntos de la r5 y el criterio
   literal de esta tanda) y los pasos 9 y 10.

## Tanda C1d del ciclo 2026-11-02 (2026-11-03): alcance del criterio de la Parte 6, 6/11 (registro completo en `../.claude/CLAUDE.md`)

> **Este bloque manda sobre los de las tandas C1, C1b y C1c y sobre el «Estado» de arriba.** Reapertura #8 desde `generacion_rmd` (19:02:57).
> `.Rmd`: `ef2c96dc…` → **`32243d0a6e6c499a4c6b1213a49e95cd`**. Verificador: `04349f26…` → **`5df824570b49c2aff49412b4807cb917`**.
> Sin aprobar. Se detuvo antes de la FASE 2C r6. Evidencia: `salida/ciclo_20261102/c1d/`.

- **Qué cambió (decisión del profesor del 2026-11-03, «Cambiar la frase (Recomendada)»).** Una sola frase de la Solution: «Por eso, si
  el grado que perdió tiene un curso más o el que ganó tiene uno menos, basta comparar el promedio de ese grado con la diferencia entre
  los totales: si el promedio es mayor, el ganador cambia; si es menor, no.» Nada más cambia en el `.Rmd`.
- **Lectura residual, medida** (`lectura_criterio_c1d.txt`, semillas 1 a 1000): 0 afirmaciones de ventaja por cursos dentro del alcance
  literal, y 0 con el veredicto contrario. Las 222 con *k* = 1 (183 con el contrario) y las 273 con *k* ≥ 2 (238) quedan fuera. HP y HG:
  1000 de 1000 contra la tabla y contra la marca, 0 empates. Control: 102 y 80 de la r5, y las seis cifras de la C1c.
- **Dos lecturas abiertas, medidas y no corregidas (van a la r6):** «ese grado» como «el que ganó» (HP sale contrario en 306 de 1000) y
  «tiene un curso más» como comparación entre los dos grados (la tabla lo cumple en 460 de 1000). La lectura buscada es verdadera en
  1000 de 1000. Por la regla de parada, si son BAJA de redacción quedan como residuo declarado.
- **Invariancia** (`invariancia_c1d.txt`): deshacer la frase da `ef2c96dc` byte a byte. Semillas 1 a 1000: `.Random.seed`, objetos y E1,
  0 diferencias, y ningún objeto nuevo; Solution idéntica salvo la frase. Texto emitido por `xexams`, semillas 1 a 100: 0 diferencias.
- **Verificador:** exit 4, 368 cifras dentro de lo firmado, 0 fallos y 0 pendientes; de 1448 líneas solo cambian 4 respecto de la C1c
  (`comparar_c1c_c1d.txt`). Solo cambiaron literales: `P6_CRITERIO`, `P6_NUEVO`, MC56 (vuelve a `ef2c96dc`, H1-Sol no se mueve) y el ancla
  de MC57. Los 58 mutantes, con el mismo veredicto.
- **Sellos, uno por uno y en orden:** paso 3 a las 19:13:14,561; 4 a las 19:13:25,374; 5 a las 19:13:35,765; 6 a las 19:13:48,276. Cada uno
  en su propia llamada, con su artefacto comprobado antes. Render en `salida/ciclo_20261102/c1d/render_32243d0a/`; arsenal 0 errores y 6
  advertencias, 2G 100/100, 2H 0 anomalías. PDF: las 5 páginas leídas.
- **6b pareada** (`auditor-visual-html`, sin `name:`, N = 100 por versión, a 360 y 320 px, 400 páginas): APTO_CON_OBSERVACIONES. Informe en
  `salida/ciclo_20261102/c1d/6b_c1d/informe_6b_c1d_32243d0a.md`. Una sola palabra en el último renglón, 0/100; 0 cortes entre cifra y
  «kilos»; 0 desbordes; la Parte 1, idéntica y con 0 cortes de L-6. El párrafo gana 1,00 renglones a 360 px y 1,97 a 320 px.

**Residuo de H2, con las cifras del md5 final** (`6b_c1d/contar_h2_c1d.txt`; sustituye a las de `ef2c96dc`):

| Corte | 360 px | 320 px | Antes, 360 px | Antes, 320 px |
|---|---|---|---|---|
| «habría ⏎ ganado.» | 0/57 | 57/57 | 0/57 | 0/57 |
| «habría ⏎ seguido ganando.» | 19/51 | 43/51 | 19/51 | 19/51 |
| Páginas con alguno de los dos | 19/100 | 84/100 | 19/100 | 19/100 |
| «no le ⏎ habría alcanzado» | 0/43 | 43/43 | 0/43 | 0/43 |
| «habría ⏎ sumado» | 0/100 | 100/100 | 100/100 | 0/100 |

Control positivo: la referencia `ef2c96dc` reproduce exactamente las cifras de la C1c, y sus renglones son los mismos en las 200 páginas. A
320 px son las cifras de `6f6f1b13` que el profesor tenía delante cuando decidió H2: la mejora de la C1c fue coincidencia de longitud. El
corte se desplaza, no viola ninguna ley y, por decisión del profesor, no lleva espacio duro.

### Próximo paso (tanda C1d)

1. FASE 2C r6 con un detractor NUEVO, sin `name:`, sobre el md5 `32243d0a…`. Qué debe auditar: `../.claude/CLAUDE.md` § «Ciclo 2026-11-02 del
   CLOZE, tanda C1d».
2. Según su veredicto: si no abre una CORRECCIÓN, tanda C2, con el paso 8 (oráculo r10 nuevo, con los diez puntos de la r5 y el criterio
   literal de esta tanda) y los pasos 9 y 10.

## Tanda C1e del ciclo 2026-11-02 (2026-11-03): redacción del criterio de la Parte 6, 6/11 (registro completo en `../.claude/CLAUDE.md`)

> **Este bloque manda sobre los de las tandas C1, C1b, C1c y C1d y sobre el «Estado» de arriba.** Reapertura #9 desde `generacion_rmd` (19:35:22).
> `.Rmd`: `32243d0a…` → **`e16842c1157b3bffe35b8f21682ed6ad`**. Verificador: `5df82457…` → **`1cbc770f15fbf9140cb0ef294c7a0aae`**.
> Sin aprobar y sin commit. Se detuvo antes de la FASE 2C r6. Evidencia: `salida/ciclo_20261102/c1e/`.

- **Qué cambió (decisión del profesor del 2026-11-03, «Reformular (Recomendada)»).** Una sola frase de la Solution: «Por eso, para saber si
  el grado que perdió habría ganado con un curso más, o si el que ganó habría perdido con uno menos, basta comparar el promedio de cada uno
  con la diferencia entre los totales: si el promedio es mayor, sí; si es menor, no.» Nada más cambia en el `.Rmd`.
- **Lecturas, medidas** (`lectura_criterio_c1e.txt`, semillas 1 a 1000): el alcance literal son HP y HG, y en las dos el criterio coincide con
  la tabla y con la marca en 1000 de 1000, con 0 empates. 0 afirmaciones de ventaja por cursos dentro del alcance. Fuera, como registro: las
  222 con *k* = 1 (183 con el contrario) y las 273 con *k* ≥ 2 (238). Controles: 306, 102 y 80, y las seis cifras de la C1c.
- **Otras lecturas, medidas y no corregidas (van a la r6):** «de cada uno» cruzado, HP y HG contrarias en 306 de 1000; un solo veredicto para
  las dos preguntas, 306 de 1000 sin veredicto común; «sí» y «no» por pregunta, las cuatro celdas coinciden (476, 524, 518 y 482);
  «si el promedio es mayor» como «el de mayor promedio», 346 y 348; «con un curso más» como comparación entre los grados, la tabla lo cumple
  en 460 de 1000; «la diferencia» como la que queda tras el cambio, 479 y 481. Por la regla de parada, si son BAJA de redacción quedan como
  residuo declarado.
- **Invariancia** (`invariancia_c1e.txt`): deshacer la frase da `32243d0a` byte a byte. Semillas 1 a 1000: `.Random.seed`, objetos y E1,
  0 diferencias, y ningún objeto nuevo; Solution idéntica salvo la frase. Texto emitido por `xexams`, semillas 1 a 100: 0 diferencias.
- **Verificador:** exit 4, 368 cifras dentro de lo firmado, 0 fallos y 0 pendientes; de 1448 líneas solo cambian 4 respecto de la C1d
  (`comparar_c1d_c1e.txt`). Solo cambiaron literales: `P6_CRITERIO`, `P6_NUEVO`, MC56 (vuelve a `32243d0a`, H1-Sol no se mueve) y el ancla
  de MC57. Los 58 mutantes, con el mismo veredicto.
- **Sellos, uno por uno y en orden:** paso 3 a las 19:44:54,163; 4 a las 19:45:08,327; 5 a las 19:45:17,256; 6 a las 19:45:31,757. Cada uno
  en su propia llamada, con su artefacto comprobado antes. Render en `salida/ciclo_20261102/c1e/render_e16842c1/`; arsenal 0 errores y 6
  advertencias, 2G 100/100, 2H 0 anomalías. PDF: las 5 páginas leídas.
- **6b pareada** (`auditor-visual-html`, sin `name:`, N = 100 por versión, a 360 y 320 px, 400 páginas): APTO_CON_OBSERVACIONES. Informe en
  `salida/ciclo_20261102/c1e/6b_c1e/informe_6b_c1e_e16842c1.md`. Una sola palabra en el último renglón, 0/100; 0 cortes entre cifra y
  «kilos»; 0 desbordes; la Parte 1, idéntica y con 0 cortes de L-6. El párrafo gana 0,78 renglones a 360 px y 0,69 a 320 px. BAJO: el último
  renglón con una sola unidad sube a 40/100 y 79/100 (dos palabras unidas por el espacio duro, no huérfanos).

**Residuo de H2, con las cifras del md5 final** (`6b_c1e/contar_h2_c1e.txt` y `contar_cortes_frase_c1e.txt`; sustituye a las de `32243d0a`):

| Corte | 360 px | 320 px | Antes, 360 px | Antes, 320 px |
|---|---|---|---|---|
| «habría ⏎ ganado.» | 0/57 | 0/57 | 0/57 | 57/57 |
| «habría ⏎ seguido ganando.» | 32/51 | 33/51 | 19/51 | 43/51 |
| Páginas con alguno de los dos | 32/100 | 33/100 | 19/100 | 84/100 |
| «no le ⏎ habría alcanzado» | 0/43 | 0/43 | 0/43 | 43/43 |
| «habría ⏎ sumado» | 0/100 | 0/100 | 0/100 | 100/100 |
| «habría ⏎ ganado con» (solo en la frase nueva) | 100/100 | 0/100 | no existe | no existe |
| «habría ⏎ perdido con» (solo en la frase nueva) | 0/100 | 0/100 | no existe | no existe |

Control positivo: la referencia `32243d0a` reproduce exactamente las cifras de la C1d. El corte se desplaza con cada redacción, no viola
ninguna ley y, por decisión del profesor, no lleva espacio duro.

### Próximo paso (tanda C1e)

1. FASE 2C r6 con un detractor NUEVO, sin `name:`, sobre el md5 `e16842c1…`. Qué debe auditar: `../.claude/CLAUDE.md` § «Ciclo 2026-11-02 del
   CLOZE, tanda C1e».
2. Según su veredicto: si no abre una CORRECCIÓN, tanda C2, con el paso 8 (oráculo r10 nuevo, con los diez puntos de la r5 y el criterio
   literal de esta tanda) y los pasos 9 y 10.
