# HANDOFF — `sistema_ecuaciones_eliminacion_numerico_variacional_argumentacion_n4_schoice_v1`

> **Léeme antes de tocar nada.** Este ejercicio consumió ~5 M tokens en 10 pasadas y 4 auditorías
> el 2026-08-19, y **3 auditorías más el 2026-08-20**, que encontraron dos defectos de CORRECCIÓN
> que las cinco anteriores no vieron (§10). Casi todo lo que se te ocurra intentar **ya se intentó
> y está medido aquí**. El objetivo de este documento es que no repitas nada.
>
> **Si sólo lees una cosa, que sea §3.2 y la trampa 7 de §5:** todo el arsenal miraba las opciones
> y el modelo, y **ninguna sonda miraba el enunciado renderizado**.

**Origen:** ERA-2026 **Sesión 2**, pregunta **impresa 44** · archivo `pagina_013.jpg`, que es la
**página impresa 18** (desfase archivo↔página: regla #24 H-4) · corpus
`Todo-Pajaro/…/Simulacros/Alineacion-Curricular-de-items-ERA-2026/Matematicas/Originales/`.
El mismo ítem aparece como `MAT-2026-1-130` en el corpus Matemáticas-2026-1.
⚠️ **El código `MAT-2026-1-044` es AMBIGUO y no debe usarse solo**: en el corpus
Matemáticas-2026-1 designa **otro ítem distinto** (tablas de probabilidad sobre una curva de
campana, clave C, Nivel 1). Sólo es correcto dentro del corpus ERA-2026. `exextra[Origen]` lo
conserva por trazabilidad, pero la referencia inequívoca es la de arriba.
**Fecha del ciclo:** 2026-08-19; **reabierto y ampliado el 2026-08-20** al derivar el gemelo CLOZE.
**Estado (2026-08-20):** 9/11. **Abiertos: `detractor_fase2c` y `aprobacion_usuario`.**
**⚠️ `apto_para_aula: false`** — ver §10. Lo estuvo en `true` entre el 2026-08-19 22:58 y el
2026-08-20, sobre una versión que imprimía aritmética falsa.
**Gemelo:** `cloze/…_n4_cloze_v1.Rmd` (6 partes, `schoice|num|num|schoice|mchoice|num`),
creado el 2026-08-20. Comparte el **cuerpo ejecutable** de `eq_de`, `ejecutar_prop` y
`proc_de_opcion` (los comentarios SÍ divergen; verificar con
`awk '/^fn <- function/,/^}/' | sed 's/##.*//' | md5sum`, nunca con el md5 del bloque entero):
todo fix de esas funciones va a LOS DOS o divergen (modo de fallo del invariante I-10).

---

## 1. Qué está CERRADO y verificado — no lo re-verifiques, no lo rompas

Todo medido por ejecución, con semillas independientes entre orquestador (`1000/7`), detractor
(`500001/13`) y re-medición (`770001/17`).

| Dimensión | Estado | Cifra |
|---|---|---|
| **Clave falsa en rama 1** | ✅ eliminada | 0/404 opciones · **0/122 claves** (era 402/402 y 95/95) |
| **Fidelidad canónica** | ✅ verbatim exacta contra el JPG | incluidas las 4 opciones y el «en el segundo» en masculino del original |
| **Segunda clave correcta** | ✅ ninguna | 300/300 semillas independientes |
| **Verificación semántica ejecutable** | ✅ | 300/300, **nunca reabrió en 10 pasadas** |
| **Canal de signo** | ✅ cerrado | señuelos con guion 46,3 % → **6,1 %**; regla al 26,2 % = azar |
| **Magnitud `\|v\|/R`** | ⚠️ cerrado **sólo para `valor_mostrado`** | 3,3 · 7,5 · 2,4 por paso. **NO leer como cerrado para los señuelos**: la cota `abs(v) > 40*R` de `elegir_par()` se aplica al error REAL, nunca a las opciones. Medido 2026-08-20 sobre señuelos: `ERR-ALG-06/v2` mediana **327**, p90 **955**, máx **1.702** |
| **Aritmética IMPRESA** | ✅ cerrado 2026-08-20 | Paso 3 del enunciado 0/100 (dos semillados); opciones «= 0» **0/400** (era 69/100 versiones) |
| **Coherencia interna de la opción** | ✅ cerrado 2026-08-20 | **+1,6 pp** schoice · **+2,3 pp** cloze (era **+28,6 pp**), contra un control oficial de +5,3 |
| **OBJ 3 del detractor** (Solution falsa) | ✅ | 0/300 atribuciones falsas de «la igualdad no se conserva» |
| **`cod_mismo ≠ cod_real`** | ✅ blindado con `stopifnot` | era 98/98 duplicados en rama 2 |
| **Léxico ramas 2 y 3** | ✅ | 0,0 % |
| **Ortografía / glifos** | ✅ | exit 0 / exit 0 |
| **5 formatos** | ✅ | html · pdf · docx · nops · moodle, en R limpio |
| **Versiones únicas** | ✅ | **100/100** (estándar del profesor; el 250/300 de la regla #3 NO aplica aquí) |

---

## 2. Qué está ABIERTO

> ⚠️ **ACTUALIZADO POR §13 (V7, 2026-08-20).** Los puntos 1-3 de abajo describen el estado
> **anterior** a la pasada de confirmación. Estado vigente:
>
> - **FASE 2C ejecutada dos veces** (V6 y su confirmación). Las objeciones de la segunda están
>   **aplicadas y medidas** (§13), lo que **caduca ese veredicto** (regla #9): falta una tercera
>   pasada de confirmación sobre V7.
> - **La corrección está cerrada en los dos gemelos** y verificada por un verificador
>   independiente con control positivo por mutación: **0 fallos en 3 bases × 100** cada uno.
> - **`aprobacion_usuario` y `apto_para_aula` siguen en `false`.**
> - **Decisión pendiente del profesor: `X1`** (§13.7) — bajar la tasa canónica o declararlo coste
>   aceptado de la fidelidad verbatim. Sin ella, la batería seguirá diciendo `BLOQUEA` en una de
>   cada tres bases por una causa no corregible sin tocar el cuadernillo.
> - **Presupuesto §P7-D: 1 de 3 pasadas del ciclo V6 consumida.**

1. **FASE 2C** — **`RECHAZAR` en los dos gemelos** (auditoría de cierre, 2026-08-20). Historial:
   4 `RECHAZAR` (2026-08-19) → 1 `APROBAR_CON_CAMBIOS` de vara §P7 → 1 `APROBAR_CON_CAMBIOS`
   post-`den_show` → 1 `RECHAZAR` del CLOZE → **1 `RECHAZAR` de cierre, ambos**.
   **Motivo único y acotado: el lote de §10 introdujo una REGRESIÓN de magnitud (§10.7).**
   La corrección salió limpia: claves verdaderas 100/100 en los dos, 0 segundas claves,
   canónica verbatim, marcas de Moodle coherentes por contenido, regla #19 sin violaciones,
   V1-V9 en verde. OBJ 2, 3, 4 y 5 **cerradas y verificadas** con 0/100 cada una.
2. **`aprobacion_usuario`** — reabierta por la misma razón. La del 2026-08-19 22:58 se dio
   sobre una versión con aritmética falsa impresa.
3. **`apto_para_aula: false`** hasta que la auditoría de cierre se pronuncie.
4. **§P7 — el residuo declarado, con su banda.** La batería congelada de la vara resultó
   **ciega al valor** en este molde: `n1()` toma el primer número, que aquí es el **número de
   paso** (aplicabilidad 0 % en sus cinco reglas de divisibilidad). **Usar `nlast()`.**
   Corregida la ceguera y aplicada la misma ampliación a las tres poblaciones:

   | Población | n | máx | techo nulo | sd | **exceso** |
   |---|---:|---:|---:|---:|---:|
   | **Ejercicio** | 100 | 41,9 % | 32,5 % | 2,80 | **+9,4 pp** |
   | Control oficial | 399 | 33,6 % | 28,4 % | 1,15 | **+5,2 pp** |
   | Corpus oficial | 426 | 32,9 % | 28,3 % | 1,11 | **+4,6 pp** |

   Frente al control: **+4,2 pp, 1,4 sd → no significativa.** Frente al corte de +8: **0,5 sd**,
   intervalo a 2 sd de **[+3,8; +15,0]**. Y el umbral de la sonda decisiva (`múltiplo de 50 y
   positivo`, 41,9 %) **lo fijó el auditor conociendo el ejercicio**, cosa que él mismo declara.
   **La cifra honesta no es una, son las dos.** ⚠️ El «+13,9 pp BLOQUEA» que este documento
   declaró hasta el 2026-08-19 **no es reproducible con vara homogénea**: salía de una batería
   que **creció durante el ciclo**, justo la serie que §P7-C prohíbe encadenar.
5. **H1: CERRADO.** Márgenes entre 3,4 % y 5,7 % contra un umbral de 15 %.
   ⚠️ Tras el lote del 2026-08-20 el reparto cambió de signo —la clave es la única más corta en
   el 28 % (antes la más larga en el 23 %)—, con **margen mediano −13,3 %**, por debajo del
   umbral. `validar_diagnosticidad.R` sigue en `PASS`. **Mide el margen antes de perseguirlo.**
6. **L-4 / afirmación de la clave invariante: CERRADA.** H3b contenido al **17 %** (bloqueo en 90 %).
7. **«Canal de divisibilidad ×50 al 44,7 %»: REFORMULADO.** La sonda simple da 27-35 % (azar
   25 %): ahí no hay canal. Vive en la **conjunción** con «positivo».
8. **19 tokens con soporte < 20** declarados `NO CONCLUYENTE` (`N_necesario` 200 agregado / 76 rama 1).
9. **`WARN_DIV_BAJA` en p4 del CLOZE** (3 valores únicos de 100): **estructural, no defecto** —
   hay exactamente 3 `cod_real` posibles. ⚠️ V6 lo bajó a **2** al retirar el estrato 3, y eso
   abrió la fuga léxica de +50 pp que §13.2 cierra devolviendo `ERR-ALG-09` a `apto_real`.
   Con V7 vuelven a ser 3, y el `WARN` sigue siendo el veredicto correcto: **es el piso del
   diseño, no un defecto corregible.**

## 3. LA CADENA DE CANALES — lo que NO hay que volver a intentar

El canal de eliminación se desplazó **nueve veces**. Cada fix cerró su dimensión y empujó la señal
a la contigua:

```
longitud → léxico → signo → longitud → signo(cerrado) → cifra final
        → longitud de la ecuación → divisibilidad → H1 rama 1 → H1 rama 3
```

**Las nueve vías textuales están AGOTADAS** (reescribir opciones, normalizar signo, igualar
longitudes, molde de ranuras, texto constante, citación uniforme, empate por pool). La décima fue
numérica y **tampoco cerró**: ver §3.1.

### 3.1 La precondición numérica: SIMULADA antes de implementar, y por qué no se aplicó

Antes de escribir el gate se midió qué fracción de versiones ya cumplía cada propiedad:

| Propiedad de los valores impresos | Uniforme en |
|---|---:|
| número de dígitos | **100 %** ← ya cerrado, no hacía falta gate |
| divisibilidad (×50) | 49 % ← **aquí está el canal** |
| signo | **24 %** ← cuello de botella |
| las cuatro a la vez | 12 % |

Exigir las cuatro era inviable. Aplicado sólo el gate de divisibilidad, **33 de 100 versiones se
quedan sin combinación viable** y el render no se produce. Queda como **diagnóstico, no forzado**:
*un gate que impide renderizar no es un gate*.

**La vía no explorada** (recomendación del ciclo): atacar `elegir_par()` **por enumeración previa**
del espacio de parámetros `(k, c, d, b, L, R)`, escogiendo los que hagan caer todos los valores
erróneos en la misma clase de divisibilidad que el correcto. Es un problema de teoría de números
sobre la grilla, **no** un filtro por rechazo — el rechazo es justo lo que vacía la búsqueda.

### 3.2 EL DÉCIMO CANAL (2026-08-20) — y lo creó la pasada 8

La cadena de arriba tiene un eslabón más, y **no es textual**: es **aritmético-semántico**.

`eq_de()` imprimía `…la ecuación debió ser: {ecu(e2)}, de donde {sim} = {v}`, donde `ecu(e2)`
sale de `eq_paso2()` y `v` de `ejecutar_prop()`, que aplica **otro despeje**. Ecuación y valor
venían de operaciones distintas, así que **cada señuelo se refutaba a sí mismo y la clave era la
única coherente consigo misma**:

| Regla | Tasa | Techo nulo | **Exceso** |
|---|---:|---:|---:|
| «elegir la opción cuya conclusión se sigue de su propia ecuación» | **53,9 %** | 25,3 % (sd 2,81) | **+28,6 pp** |
| … replicada sobre el XML de Moodle exportado (N = 30) | 56,1 % | — | — |

Diez desviaciones y 3,6× el corte. Aislaba la clave sola en 28/100 versiones: **el ítem se
resolvía sin leer el enunciado.** Ejemplo real del XML, dos opciones con la MISMA ecuación:

```
=En el paso 3, la ecuación debió ser: 30B = 127.500, de donde B = 4.250
~En el paso 3, la ecuación debió ser: 30B = 127.500, de donde B = 0
```

**Lo creó la pasada 8 de este mismo ciclo**: la «estructura de citación uniforme» que añadió el
`, de donde …` para cerrar el canal de longitud. *Cerró la longitud y abrió la aritmética.*

**Por qué cinco auditorías no lo vieron** —y esto es lo que hay que llevarse—: H1 mide longitud
(márgenes 6 % y 4 %, correctamente silenciosa); H2/H3 están ciegas por molde (L-4); **H3b borra
los dígitos**, que es exactamente donde vivía el canal; y las seis familias de §P7 **no incluyen
una familia aritmético-semántica**. No era invisible por sutil: **ninguna sonda miraba ahí.**

**Fix (§10):** imprimir **la operación**, no sólo su resultado. Con `487.500 ÷ 60 = 8.125` las
cuatro opciones son verdaderas y la regla cae al techo nulo: **+1,6 pp** y **+2,3 pp**.

### Incompatibilidad medida entre dos defensas

**Cerrar el signo impide el empate de longitud en la rama 2.** `ERR-ALG-04` es el único señuelo que
*sí* elimina la incógnita, pero la normalización lo imprime como `0 = 60R + 300.000`, más largo que
la clave `60R = 300.000`. No hay presentación que satisfaga ambas. **El detractor arbitró que el
trade-off está bien resuelto: el signo vale más y la longitud no cuesta nada medible.**

### Causa estructural, diagnosticada

Las opciones **son ecuaciones derivadas de un procedimiento**, y la clave es la que resulta de
operar correctamente. Toda propiedad de «ser correcta» —longitud, valor, divisibilidad,
estructura— es en principio detectable. Por eso el canal migra en vez de desaparecer.

---

## 4. LO QUE SE MIDIÓ Y NO HAY QUE VOLVER A MEDIR

### La vara §P7 para ítems de esta clase (426 ítems oficiales deduplicados)

| Población | n | exceso |
|---|---:|---:|
| Corpus oficial completo | 426 | **+4,6 pp** |
| Control (opciones que no son ecuaciones desnudas) | 399 | **+5,3 pp** |
| Oficiales con opciones-ecuación desnudas | 27 | **−0,7 pp** |
| Este ejercicio al empezar | — | +17,8 pp |
| Este ejercicio, cifra vigente (vara congelada + `nlast()`) | 100 | **+9,4 pp** |

⚠️ El **+12,6 pp** que esta tabla declaró hasta el 2026-08-19 salía de una batería que **creció
durante el ciclo** (la serie que §P7-C prohíbe encadenar) y con `n1()`, ciego al valor en este
molde. La cifra vigente y su banda están en §2 punto 4: **+9,4 pp** contra un control de **+5,2 pp**,
diferencia de **1,4 sd — no significativa**.

**La hipótesis del «piso irreducible» para ítems-ecuación está REFUTADA**: esa familia mide *por
debajo* del control, no por encima. Y este ejercicio **no pertenece a ella** — sus opciones son
prosa con ecuación embebida, así que su clase de comparación es el control. Detalle completo en la
memoria `ref_vara_p7_items_ecuacion.md`.

**Límite de potencia declarado:** a n=27 la sd es 5,6 pp; establecer la vara de la familia exigiría
≈212 ítems y el corpus tiene 27. No se puede descartar un piso pequeño (~+3 pp); sí se descarta que
un piso explique +13 pp.

**Y lo que la vara NO puede ver** (medido el 2026-08-20, §3.2): la batería recibe **sólo las
opciones**, y sus seis familias no incluyen una **aritmético-semántica**. Un canal de **+28,6 pp**
—la conclusión que no se sigue de la propia ecuación— convivió con todas estas cifras en verde.
Por la exigencia 1 de §P7, una familia sin sonda obliga a `SIN_COBERTURA`, no a `PASS`.

### Otras mediciones que ya existen

- **«Elegir la más corta»**: 34,2 % sobre el total (azar 25 %), margen mediano **4,3 %**.
- **«Descartar precio imposible»**: 51,5 % con la batería ciega al signo → **32,3 % (+1,4 pp)** con
  el regex corregido. *El rediseño de señuelos que esto parecía exigir NO está justificado.*
- **Objeción 2 del detractor**: el canal no estaba en el signo del precio sino en su **estructura
  aritmética** (sonda de divisibilidad: máximo 43,5 %).

---

## 5. TRAMPAS DE MEDICIÓN DE ESTE EJERCICIO (todas cometidas al menos una vez)

1. **H1 exige «la ÚNICA más corta», y su margen calibrado es ≥ 15 %.** Una tasa del 57,9 % con
   margen del 4,3 % —tres caracteres sobre setenta— **es inexplotable**. Mide el margen antes de
   perseguir cualquier residuo de longitud. El detractor retiró una reclamación propia del 98,0 %
   por esta razón.
2. **`nums()` de `auditoria_propia.R` no capturaba el signo** (`gregexpr("[0-9][0-9.]*")` sin `-?`):
   cinco reglas de magnitud leían `-3.500` como `3500`. Corregido, con control positivo pegado a la
   definición. **Le pasó dos veces a esta batería** — la primera con el regex de `signo`.
3. **La batería §P7 recibe sólo las opciones, nunca el estímulo.** Un canal que nazca de un valor
   mostrado en el enunciado le es invisible. Así vivió un canal del 56,2 % con 30 reglas en verde.
4. **H3b borra los dígitos** de la firma: parametrizar sólo el número del paso produce un `PASS`
   falso. Lo que debe variar es el vocabulario.
5. **Soporte insuficiente ⇒ `NO CONCLUYENTE`, no escalar el N.** A N=100 el token `dividir`
   (soporte ≈14) quedaba excluido y el léxico daba falso verde. La salida correcta es declararlo con
   su `N_necesario`, no subir la muestra (regla #23).
6. **N = 100 es el estándar** (regla #23) y **100 versiones** es el requisito de producto de este
   repositorio. El umbral 250/300 de la regla #3 está en tensión declarada con ese estándar y **no
   gobierna aquí**.

---

7. **TODO EL ARSENAL MIRA LAS OPCIONES Y EL MODELO; NINGUNA SONDA MIRABA EL ENUNCIADO
   RENDERIZADO.** Es la causa común de los dos defectos de §10. `smoke.R` comprobaba lo
   *calculado*, no lo *impreso*; la batería §P7 recibe sólo las opciones; H3b borra los dígitos.
   Un ejercicio sellado 11/11 llevaba una división falsa impresa en el 34 % de sus versiones y
   una conclusión falsa en el 69 %. **Toda sonda nueva debe parsear la salida real del chunk.**

## 6. Invariantes locales — `.claude/CLAUDE.md` de este directorio

| | Contenido |
|---|---|
| **L-1** | La opción D del cuadernillo **NO tiene errata**: `60L+100R=1.300.000` sale de sumar E1 con la ecuación **del Paso 1**, no con E2 original. Verificado tres veces. Blindado con `stopifnot`. |
| **L-2** | Prohibido el distractor «multiplicar toda la ecuación por un factor de signo opuesto y SUMAR» — es matemáticamente **válido** y sería segunda clave. La opción A canónica no cae ahí (multiplica sólo el primer término). |
| **L-3** | La canónica viola `b > k·d` **deliberadamente**, por fidelidad (H-2). No «normalizar». |
| **L-4** | H2/H3 ciegas por molde uniforme; el relevo es H3b, que borra los dígitos. |
| **L-5** | La Solution **no** lleva nota sobre la opción D ni sobre el cuadernillo. Decisión del profesor. |
| **L-6** | `calcula()` / `ejecutar_proc()` / `ejecutar_prop()` son **puras**. El veredicto de cada opción se obtiene **ejecutando** su propuesta. |
| **L-7** | Guarda contra verificación semántica vacua (`proc == pr_show`). |

---

## 7. Las auditorías de detractor (4 el 2026-08-19; 3 más el 2026-08-20 — ver §10)

Las cuatro `RECHAZAR`. Objeciones vivas de la 4.ª pasada:

| Obj | Estado |
|---|---|
| **1** clave falsa en paso 1 (CRÍTICA) | ✅ **cerrada** en la pasada 9 |
| **2** precio imposible (CRÍTICA) | ⚠️ medida: sobrevive **+1,4 pp** aislada; el canal real es divisibilidad |
| **3** `cod_mismo` = `cod_real` (ALTA) | ✅ cerrada |
| **4** L-4: afirmación de la clave invariante (MEDIA) | ❌ abierta — 1 sola firma H3b quitando el símbolo de contexto |
| **5a** `nums()` ciego al signo (MEDIA) | ✅ cerrada |
| **6** símbolo de la incógnita equivocada (BAJA) | ✅ cerrada con la objeción 1 |

**El detractor declaró qué habría bastado para aprobar:** *«si las objeciones 1 y 2 no existieran,
este reporte diría APROBAR_CON_CAMBIOS»*. La 1 está cerrada; la 2 resultó ser +1,4 pp.

---

## 8. Si retomas esto dentro de seis meses

1. **Lee §3 antes de proponer nada.** Si tu idea es textual, ya se intentó.
2. **No persigas la longitud** sin medir su margen (§5.1).
3. **Corre la batería con las seis familias** y compara el **exceso** contra el techo nulo, nunca la
   tasa absoluta. Referencia: +5,3 pp del control oficial.
4. **La FASE 2C exige un detractor independiente** (regla #9): no la selles con auditoría propia.
5. **Lo más valioso de este ciclo no es el ejercicio**, son las tres memorias de referencia que
   produjo: `feedback_h3b_borra_los_digitos`, `feedback_bateria_p7_no_ve_el_estimulo`,
   `ref_vara_p7_items_ecuacion`.

---

---

## 9. LECTURA FINAL DEL CICLO

**El patrón, que es lo que hay que saber:** cerraron **todas** las dimensiones independientes del
contenido (signo, magnitud, semántica, ortografía) y **ninguna** de las acopladas al contenido
(longitud, léxico, divisibilidad del valor).

> **Lo que funcionó siempre fue la misma forma de defensa: precondición verificada que aborta el
> render. Lo que nunca funcionó fue reescribir texto y medir después.**

**Dos lecciones sobre las correcciones mismas, ambas medidas aquí:**

- **Una corrección puede introducir un defecto peor que el canal que cierra.** La pasada que llevó
  §P7 de +17,8 a +6,3 pp volvió **falsa la clave** en el 31,7 % de las versiones. Ningún gate lo
  vio; lo encontró la auditoría independiente. **Tras una mejora de diagnosticidad, verifica que la
  clave sigue siendo verdadera.**
- **Una objeción cara puede evaporarse al medirla.** Una crítica de +26,6 pp que exigía rediseñar
  dos señuelos quedó en **+1,4 pp** al corregir un regex ciego al signo del propio verificador.
  **Mide antes de rediseñar.**

**Cierre:** el ciclo se cerró por límite explícito del profesor tras 10 pasadas. El ejercicio
**no se selló**: registrar como completos unos pasos con un residuo bloqueante sería falsear el
estado. `versiones_unicas` correcto es **100/100** (el 97 del JSON es la salida de diversidad
sustantiva, otra magnitud).

---

## 10. EL CICLO DEL 2026-08-20 — lo que encontró derivar el gemelo CLOZE

Al construir el CLOZE aparecieron **dos defectos de CORRECCIÓN en este SCHOICE**, que estaba
**sellado 11/11 y aprobado para aula** tras cinco auditorías. Los dos son de la misma clase: *una
cifra impresa que no es la que produce el procedimiento que la genera*.

### 10.1 Los dos defectos, medidos

| Defecto | Frecuencia | Cómo se veía en pantalla |
|---|---:|---|
| **Paso 3 del enunciado**: se imprimía `coef_final` (el coeficiente CORRECTO) mientras `valor_mostrado` venía de `ejecutar_proc()`, que con `op3 = "coef_eliminado"` divide entre `par$a` | **32/100** y **36/100** (dos semillados) = **100 % del estrato paso 3**, siempre `ERR-ALG-07` | `480.000 / 60 = 12.000` (da 8.000) |
| **Opciones**: la conclusión no se seguía de la ecuación impresa (§3.2) | **69/100** versiones con una opción «= 0»; regla de coherencia **+28,6 pp** | `40P = 120.000, de donde P = 0` |

**No eran la trampa deliberada de H-2.** La trampa correcta muestra el divisor equivocado
produciendo, coherentemente, el resultado equivocado. Lo impreso mostraba el divisor **acertado**
con el resultado equivocado — aritmética falsa a secas, que además desmiente al propio
`ERR-ALG-07` («dividir entre el coeficiente de la incógnita YA ELIMINADA»), porque el número
impreso era el de la incógnita que **sí** sobrevive.

### 10.2 Los fixes, y por qué son el mismo

Los dos se cierran con la misma forma de defensa, que es la única que ha funcionado aquí (§9):
**imprimir lo que el procedimiento ejecuta de verdad.**

- **`den_show`** en el enunciado: `if (identical(pr_show$op3, "coef_eliminado")) par$a else coef_final`.
- **`op_val()` / `op_txt()`** en las opciones: se imprime **la operación** (`487.500 ÷ 60 = 8.125`),
  no sólo su resultado. Espejan a `ejecutar_prop()` rama por rama, y `eq_de` **asevera su acuerdo
  en cada llamada** en vez de confiar en que dos funciones se mantengan sincronizadas a mano.
- **Retirada la variante `"invertir"`** (`coef/ci`): única cuyo valor no es entero —siempre ~1e-4—
  y única fuente del «= 0», porque `fmt(digits = 0)` lo colapsaba. Sustituida por `"sumar_coef"`.
- **`fmt_v()`**: muestra decimales si el valor no es entero. Medido: nunca hace falta (**0/400**
  resultados no enteros), lo que cierra de paso el canal «la división que da justa».

⚠️ **Una guarda que compara el modelo consigo mismo no cubre el sitio de impresión.** El primer
`stopifnot` recalculaba `ci/den_show` y lo confrontaba con `valor_mostrado`: un mutante que
revertía **sólo el `cat`** dejando `den_show` definido daba **0 abortos de 100**. Lo cazó el
detractor, no yo.

### 10.3 Las otras cuatro objeciones del CLOZE (aplicadas a quien correspondiera)

| # | Hallazgo | Antes | Después |
|---|---|---:|---:|
| 2 | `PROP-COEF` y `PROP-DIVIDIR` denotan el mismo número ⇒ **segunda clave defendible**, y la Solution afirmaba que «las otras tres SÍ se respetan» | 17/100 | **0/100** |
| 3 | La Parte 5 **nombraba el paso**; como `paso_real` determina `cod_real` (39/39, 29/29, 32/32), fijaba la clave de P4 y reducía P1 a la mitad | +20,4 pp | **0/100** versiones lo nombran |
| 4 | El `nombre` de `ERR-ALG-06` decía «Multiplicar…», operación que sus señuelos **nunca** ejecutan | 92/100 | rótulo reescrito |
| 5 | Prosa de la Solution rota: la cadena de `sub()` sólo casaba con la redacción canónica | 93/100 | `cola()`, concordada |

De la 2 quedan **9/100** versiones con ambas propiedades presentes, pero en **0** de ellas la clave
es una de las dos: ahí el error está en el paso 1 o 2, el paso 3 se ejecuta bien y el procedimiento
respeta ambas de verdad. **No hay segunda clave.**

`viola_pr_show()` no atrapaba la 2 porque compara **identidad sintáctica de mutaciones**, no el
predicado semántico de la propiedad: es rigurosa contra el error para el que se diseñó y ciega a éste.

### 10.4 Verificación del lote (exit real por redirección, nunca por tubería)

| | SCHOICE | CLOZE |
|---|---|---|
| Igualdad impresa del Paso 3 falsa | **0/100** ×2 semillados | **0/100** |
| Opciones «= 0» · no enteras | **0/400** · **0/400** | **0/400** · **0/400** |
| Regla de coherencia interna | 26,6 % vs 25,0 % ⇒ **+1,6 pp** | 27,2 % vs 24,9 % ⇒ **+2,3 pp** |
| Claves | smoke 100/100, 0 fallos | **100/100, 0 defectos de corrección** |
| Arsenal | ortografía 0 · glifos 0 · coherencia APROBADO · diagnosticidad PASS · diversidad PASS | idem, `WARN_DIV_BAJA` en p4 |
| Formatos, R limpio con `exams::` | html·pdf·docx·moodle·nops | html·pdf·docx·moodle; NOPS N/A con motivo verificado |

**Control positivo de cada sonda**, que es lo que hace que un cero signifique algo: la sonda de
aritmética impresa da **32/100** y **26/100** sobre los backups pre-fix, y **0** sobre el corregido.

### 10.5 Lo que hay que llevarse

1. **Un ejercicio sellado 11/11 y aprobado para aula puede tener aritmética falsa impresa.** El
   sellado acredita que se ejecutaron los pasos, no que alguien mirara la pantalla.
2. **Dos detractores independientes convergieron en el mismo defecto raíz** desde ángulos distintos
   —uno por el «= 0», otro por la coherencia interna—. Ninguno de los dos lo habría encontrado
   mirando lo que miraba el arsenal.
3. **El fix de un canal creó el siguiente, otra vez.** La citación uniforme de la pasada 8 cerró la
   longitud y abrió la aritmética. Ya van diez.
4. **Añadir al `smoke.R` la sonda que faltaba**: consistencia aritmética de lo **impreso**. Es la
   deuda más transferible de este ciclo, y aplica a cualquier ítem cuyas opciones sean
   procedimientos.

### 10.7 ⚠️ LA REGRESIÓN QUE INTRODUJO ESTE LOTE — el canal once

**Retirar `invertir` era necesario; el sustituto elegido fue el error.** `sumar_coef` (`ci + coef`)
y `restar` (`ci − coef`) dan un valor **casi igual a `ci`**, es decir dos o tres órdenes de magnitud
por encima del precio correcto, mientras la clave —que divide— es siempre pequeña.

| Regla (techo nulo 25 %) | Antes del lote | Después | Δ |
|---|---:|---:|---:|
| **`M5` la clave es la de menor `\|valor\|`** | **−13,0 pp** | **+31,0 pp** | **+44** |
| `N3` precio plausible (entero, > 0, < 50.000) | — | +24,8 pp | — |
| `M3` divisible por 50 | +6,3 pp | +15,9 pp | +9,6 |
| `C3` el despeje es una división | +0,0 pp | +13,0 pp | +13 |

Verificado por mí de forma independiente sobre el backup `.bak-pre-obj1`: **−13,0 → +31,0 pp**.
Margen mediano **73,4 %** (schoice) y **83,7 %** (cloze), muy por encima del 15 % de §P7-B:
**plenamente explotable**, no es un residuo de tres caracteres.

**Y la clave nunca usa `+` ni `−`: 110/110.** «Descartar toda opción cuyo despeje sume o reste»
elimina el 49 % de los señuelos y jamás a la clave. Regla de una línea, sin aritmética, sin mirar
el enunciado.

**Reabre el invariante que §1 declaraba cerrado**: `|v|/R` mediana **1,00** en la clave contra
**115** (`+`) y **335** (`−`) en los señuelos; **53 %** de los señuelos imprimen un precio > 20×R,
la clave **0/56**. Efecto secundario medido: los dos señuelos absurdos difieren entre sí en 200
pesos sobre 1.185.000 (**0,017 %**), así que para el estudiante son el mismo número y el ítem
opera de facto con **3 opciones, no 4**.

**Por qué el arsenal lo dio en verde:** `validar_diagnosticidad.R` sale `PASS` y su propia salida lo
explica —H2/H3 ciegas por molde, H3b con los dígitos borrados—. **Ninguna de sus cuatro sondas mira
la magnitud del valor final.** Es un `PASS` sin sonda, otra vez.

**Remedio acotado (no toca texto, no es ninguna de las nueve vías de §3):** aplicar al valor del
**señuelo** la misma cota de plausibilidad que `elegir_par` ya aplica al del estímulo
(`abs(v) > 40 * p$R → next`). Sustitutos de `invertir` que la cumplen sin imprimir `= 0` y sin salir
del modelo: dividir entre el coeficiente **de la otra incógnita**, entre `par$d`, o entre
`coef/par$k`. Todos mantienen la operación en `÷`, con lo que cierran también el canal de forma.
Objetivo verificable: `M5 ≤ +8 pp` y `C3` sin dirección.

**La lección, que este documento ya contenía en §9 y yo no apliqué:** *hay que volver a medir el
ítem completo tras cada fix, no sólo la dimensión corregida*. Medí coherencia y claves; **no medí
magnitud**. Es exactamente el modo de fallo que §9 advierte.

#### 10.7.1 EL INTENTO DE ARREGLARLO, Y LO QUE REVELÓ (medido, 2026-08-20)

Se sustituyeron `sumar_coef`/`restar` por **`div_orig_d`/`div_orig_b`** —dividir entre un
coeficiente del sistema original— para recuperar la forma `÷` y una magnitud plausible. **`M5` NO
se movió: +31,0 pp antes y después.** Se probó además la variante simétrica (que TODAS las opciones
concluyan): **también +31,0 pp**.

| Variante | M5 | N3 | «concluye» | L1 | falsedades impresas |
|---|---:|---:|---:|---:|---:|
| Con supresión de OBJ 1b (**estado actual**) | +31,0 | +17,8 | +6,7 | −18,2 | **0/400** |
| Todas concluyen | +31,0 | +11,2 | −2,0 | +9,0 | 60/400 |

**Lo que esto revela, y es el hallazgo de fondo del ciclo:** `M5` no lo causó el sustituto elegido,
sino **la retirada de `invertir`**. Esa variante imprimía un valor de ~1e-4 que `fmt` colapsaba a
«0», y **el 0 era siempre el menor**, de modo que la clave nunca era la de menor valor: `M5` medía
**−13,0 pp**.

> **El ítem estaba protegido del canal de magnitud POR UNA FALSEDAD IMPRESA.**
> Quitar la falsedad expone el canal. No hay estado alcanzable cambiando de variante que satisfaga
> las dos cosas: cualquier señuelo que represente un error algebraico real produce un valor de
> magnitud implausible, porque eso es lo que los errores algebraicos hacen con las magnitudes.

Es la **causa estructural** que §3 ya había diagnosticado, en su forma más aguda: *toda propiedad de
«ser correcta» es en principio detectable*. Y obliga a elegir entre dos clases de defecto que este
repositorio trata de forma distinta —CORRECCIÓN es binaria y bloqueante; DIAGNOSTICIDAD es gradual—,
así que **la elección no es técnica sino del profesor**:

| Camino | Coste |
|---|---|
| **(a) Dejarlo como está** | 0 falsedades impresas · `M5` **+31,0 pp** declarado como residuo estructural |
| **(b) Revertir a `invertir`** | `M5` **−13,0 pp** · reinstala «de donde X = 0» falso en el 69 % de las versiones |
| **(c) La vía no explorada de §3.1** | Enumerar `(k, c, d, b, L, R)` de modo que los procedimientos ERRÓNEOS también caigan en un rango plausible. Es teoría de números sobre la grilla, no un filtro por rechazo. **Es el único camino que cierra las dos a la vez** |

**Estado dejado en disco: (a).** Razón: CORRECCIÓN es bloqueante absoluto y DIAGNOSTICIDAD es
gradual, así que entre las dos se conserva la que no imprime falsedades. Claves verdaderas 100/100
en ambos gemelos, arsenal en exit 0 completo.

### 10.8 Discrepancia declarada sobre `C4`, sin resolver

La auditoría de cierre midió la regla **fuerte** —«la conclusión se sigue de su propia ecuación»— en
**63,0 % ⇒ +38,0 pp**, y concluyó que OBJ 1 no está cerrada. **No reproduzco esa cifra**: con la
convención §P7 (`score = 1/|S|`, `1/n` si `|S| = 0`) y un parser que marca FALSE toda opción que
despeja con dos incógnitas presentes, obtengo **30,0 % ⇒ +5,1 pp** (antes del lote: +2,0 pp).

**Lo que sí está confirmado y no depende de la discrepancia:** **60 de 400 opciones** muestran una
ecuación con dos incógnitas y aun así concluyen un valor para una de ellas —
`20A + 160B = 835.000, de donde B = 835.000 ÷ 160 = 5.218,75`—. La aritmética es cierta; la
inferencia no. Imprimir la operación arregló el sumando aritmético y **dejó intacto el
cuantificador lógico**. El remedio es barato: suprimir la cláusula «de donde» cuando la ecuación
conserva dos incógnitas, como ya hacen las opciones del paso 1 (que retornan temprano en `eq_de`).

⚠️ **El comentario de `eq_de` afirma que «las cuatro opciones son verdaderas y la regla cae al techo
nulo». Eso es falso para la regla fuerte y debe corregirse**, con discrepancia o sin ella: una
afirmación falsa dentro del propio fix es lo que hace que el siguiente auditor no vuelva a mirar ahí.

### 10.10 LA VÍA DE §3.1, RECORRIDA — y la respuesta no estaba donde §3.1 la buscaba

§3.1 proponía enumerar `(k, c, d, b, L, R)` para que los procedimientos erróneos cayeran en rango
plausible. **Se midió primero, y esa premisa era falsa:** los señuelos ya son plausibles — ratios
medianos `|v|/R` de **1,26 a 19,08**, y sólo el **12 %** de las instancias fuera de la banda
[1/20, 20]. El «67 % > 20×R» que se había reportado medía **constantes de ecuación, no precios**.

**El canal no estaba en los parámetros: estaba en la ESCALA DEL ÚLTIMO NÚMERO de cada opción.**
Unas terminaban en un precio (~1e3) y otras en la constante de su ecuación (~1e6), y la clave caía
sistemáticamente en el grupo pequeño. Cuatro variantes medidas sobre las mismas semillas:

| Variante | M5 | N3 | M3 | CC | L1 | abortos | falsedades |
|---|---:|---:|---:|---:|---:|---:|---:|
| **V0** cláusula suprimida si quedan 2 incógnitas | +31,0 | +17,8 | +2,1 | +6,7 | −18,2 | 0 | 0 |
| **V1** ninguna opción concluye | −1,4 | +0,0 | +0,0 | +0,0 | +12,6 | **30** | 0 |
| **V2** todas concluyen con el precio | +63,0 | +28,9 | +13,6 | +0,0 | +35,5 | 0 | 0 |
| **V4** paso 3 nombra el divisor y **toda opción cierra en la ecuación** | **+7,3** | **+0,8** | **−0,8** | **+0,0** | **+3,8** | 2 | 0 |

**Los 30 abortos de V1 fueron el hallazgo que abrió la solución**: eran **exactamente el estrato del
paso 3** (sólo renderizaban pasos 1 y 2). Sin conclusión numérica, un error de despeje **no se puede
expresar** — su ecuación es la misma que la de la clave. *La cláusula era load-bearing para toda una
clase de errores.*

De ahí V4: el paso 3 nombra el **divisor** —que es literalmente lo que se evalúa— pero la frase
termina igualmente en la **ecuación**, de modo que las cuatro opciones cierran en la misma escala.
V3 (terminar en el divisor) dejaba `M5` en **−15,5 pp con margen del 68,6 %**: señal inversa, no
cierre.

```
CLAVE  En el paso 3, se debió dividir entre 40 la ecuación reducida 40P = 70.000
       En el paso 3, se debió dividir entre 10 la ecuación reducida 40P = 70.000
       En el paso 1, la ecuación debió ser: 30A + 60B = 600.000
```

Las dos opciones de paso 3 comparten la ecuación reducida y difieren **sólo en el divisor**: el ítem
pasa a preguntar exactamente lo que dice preguntar.

**Coste medido, declarado:** **2 renders de 100** se pierden por colisión de divisores. Los estratos
se conservan (**38 / 30 / 30**, antes 38/30/32): no barre un estrato como hacía V1.

**Residuos vivos, con su cifra:** `M6` **−10,0 pp** (la clave rara vez es la de MAYOR valor;
**preexistente**, era −11,0 en V0) y `L2` **+7,8 pp**, ambos por debajo o al borde del corte de +8 y
por encima del control oficial de +5,3. `P1` +5,0 pp, dentro de la vara.

**Verificado tras V4, en los dos gemelos:** claves **98/98 verdaderas** · CLOZE **0 defectos de
corrección** · 0 falsedades impresas · arsenal completo **exit 0** (ortografía, glifos, coherencia,
diagnosticidad, diversidad) · **5 formatos** en R limpio, con el `N/A` de NOPS por el motivo correcto.
Corregida además la prosa de la Solution del CLOZE, que anteponía «la ecuación del paso N debió
ser:» a una opción que ya nombra su paso — y que con la escala uniforme pasaba a ser **falsa** para
el paso 3, porque ahí la opción no enuncia una ecuación sino un divisor.

### 10.11 VERIFICACIÓN CONTRA LA PÁGINA IMPRESA — hecha por primera vez (2026-08-20)

Siete auditorías dieron por verbatim la instancia canónica **sin abrir el JPG**; la de cierre lo
declaró explícitamente como dominio no auditado. Hecha ahora, leyendo
`Originales/pagina_013.jpg` del corpus ERA-2026.

**RESULTADO: la canónica es VERBATIM.** Coinciden carácter a carácter el enunciado, las dos
ecuaciones del sistema, los tres pasos con sus resultados (`30L + 40R = 400.000`, la resta que da
`20R = 500.000`, y `R = 500.000/20 = 25.000`), la pregunta de cierre, y **las cuatro opciones**.
Confirmado también el **«y en el segundo» en masculino** del original, que el HANDOFF afirmaba sin
haberlo comprobado en la imagen.

**L-1 CONFIRMADA EN LA FUENTE.** La opción D impresa dice **`60L + 100R = 1.300.000`**. No 40L.

**Y el porqué de L-1 queda documentado:** la ficha oficial de alineación de ese mismo ítem, en su
justificación de la opción D, escribe *«sumar las dos ecuaciones originales para obtener **40L** +
100R = 1.300.000»*. Es decir, **el propio documento oficial contiene la confusión 40L/60R contra la
que L-1 previene**. Quien corrija guiándose por la justificación en vez de por la opción impresa
introduce el error. L-1 no era una precaución: era una defensa contra una fuente secundaria errónea.

**Tres hallazgos que ninguna auditoría previa tenía:**

| | |
|---|---|
| **Colisión de código** | `MAT-2026-1-044` designa **otro ítem** en el corpus Matemáticas-2026-1 (tablas de probabilidad). Corregido en la cabecera de este documento |
| **Desfase archivo↔página (H-4)** | `pagina_013.jpg` es la **página impresa 18**. El ancla correcta es el **número impreso de la pregunta (44)**, exactamente como manda la regla #24 H-4 |
| **Signo tipográfico** | La página usa raya (`–`, U+2013) en `–3` y `–20R`; el `.Rmd` usa guion ASCII. Es **normalización obligatoria**, no infidelidad: U+2212 rompe pdflatex (Incidente O). Se declara para que nadie lo "corrija" hacia el original |

### 10.12 EL INSTRUMENTO CONGELADO, Y LO QUE MIDIÓ (2026-08-20)

La auditoría de cierre de V4 exigió, antes que ningún fix, **ampliar y congelar la batería**: la
vigente (`M5 · N3 · M3 · CC · L1`) no contenía **ninguna regla que comparase dos números dentro de
la misma opción**, y por eso dio V4 por bueno en todo cuando había empeorado tres reglas y una
había cambiado de signo.

**Instrumento: `bateria_congelada.R`** (en este directorio). Pre-registro §P7-C en su cabecera:
congelada **antes** del fix, sin altas a mitad de ciclo. Cubre las **seis familias**, toma los
cortes del helper compartido —no los define— y mide **por estrato**, no sólo agregado. Valida
contra las dos auditorías: `V1` +21,3 pp aquí frente a +20,2 del detractor y +21,02 de Codex.

**LÍNEA BASE: `BLOQUEA`, máximo agregado +21,3 pp.** Y condicionando por estrato aparece lo que
ninguna auditoría había visto entero — **canal en los tres, por mecanismos independientes**:

| Estrato | n | Regla dominante | Exceso |
|---|---:|---|---:|
| paso 1 | 41 | `P1` cita el paso 1 · `X1` léxico «toda la ecuación» | +25,0 · **+22,0** |
| paso 2 | 30 | `NTm` menos números · `L1` la más corta | **+75,0 · +75,0** (100 %) |
| paso 3 | 29 | `V1` divisor = coeficiente · `V5` mayor divisor | **+75,0 · +75,0** (100 %) |

#### `V1` NO es corregible eligiendo otros divisores

Dividir entre el coeficiente **es** la operación correcta. Si la opción muestra la ecuación **y** el
divisor, comprobar su coherencia identifica la clave — sin resolver nada. La única salida es no
mostrar la ecuación, que es la variante V3 de §10.10, y V3 traía el problema de escala (`M5` −15,5
pp con margen del 68,6 %). **No es el canal doce de la cadena: es el techo estructural** que
§10.7.1 describe. Llevarlo a la decisión del profesor, no a otra pasada.

#### Dos correcciones aplicadas y verificadas

| | Antes | Después |
|---|---:|---:|
| Fallos de render (guarda en `elegir_par` contra `\|b−k·d\| ∈ {a,d,b}`) | 3/40 · 4/100 · 20/1000 | **0/60 en ambos gemelos** |
| Afirmación falsa en el comentario de `eq_de` | presente | eliminada |

La celda que fallaba era **determinista**: con `b = (k±1)·d` el divisor de la clave iguala al de un
señuelo y, como ambas opciones de paso 3 imprimen la misma ecuación reducida, los textos quedan
idénticos → falla `unique(txt) == 4` → 400/400 intentos y aborto sin recuperación.

Claves tras los cambios: **100/100** en ambos · CLOZE **0 defectos de corrección** · ortografía exit 0.

#### Codex contradijo al detractor sobre `C4`, y la síntesis es que ambos medían algo real

Codex (motor y familia de modelos distintos, n = 1000) sostiene que la medición correcta es la **B**
(+5,28 pp), no la A (+40,5 pp): *«una opción de paso 1 sin cláusula no se refuta a sí misma: no hay
conclusión que evaluar»*.

**Síntesis, que ninguno de los dos formuló:** son **dos reglas legítimas distintas**. A es de
**selección** («elegir la coherente»), y una opción sin conclusión no es candidata. B es de
**eliminación** («descartar la que se contradice»), y una opción sin conclusión no puede
descartarse. Un estudiante puede usar cualquiera de las dos. **Se declaran ambas; no se corona
ninguna.** Con V4 las dos miden +0,0 pp, así que la disputa es histórica.

Codex confirmó además el canal del divisor con n = 1000 (46,02 % global, **100 % cuando la clave es
de paso 3**) y midió los fallos de render en **20/1000 = 2 %** contra **0/1000** pre-V4 — corrige mi
8 %, que era artefacto de una muestra de 40.

#### Un incidente de edición, declarado

Al limpiar el código muerto usé un regex demasiado amplio y **borré cinco funciones vivas**
(`proc_de_opcion`, `ejecutar_prop`, `paso_de`, `txt_clave`, `txt_senuelo`). **El fichero seguía
parseando**, así que una comprobación de sintaxis no lo habría detectado: lo cazó la verificación de
que cada función siguiera *definida*. Restaurado del backup y rehecho con anclajes estrechos; nada
llegó a ejecutarse en ese estado. **Lección: tras un borrado por patrón, comprobar presencia de
símbolos, no sólo que el código parsee.**

#### Backups

Se conserva sólo `*.bak-pre-v4` en cada gemelo, que es el que permite reproducir el antes/después de
V4. Los demás estados intermedios están descritos con sus cifras en §10.10 y §10.7.1; sus copias se
retiraron para no versionar dieciséis duplicados del mismo fichero.

### 10.9 Pendiente, declarado y NO hecho

- **El corrector de ortografía tiene dos puntos ciegos medidos**: `incognita` no está en su
  diccionario (`grep` → 0) y la línea 108 de `corregir_ortografia_espanol.R` es
  `"exponencial" = "exponencial"`, un mapeo a sí mismo inerte. Su `exit 0` **no acreditaba** ese
  texto. No se corrigió: es infraestructura compartida (symlink a `SOURCES/`, invariante I-10) y
  dar de alta la entrada marcaría `incognita` en todo el repositorio, comentarios y legacy
  incluidos. Necesita su propia medición y su suite.
- **La vía no explorada de §3.1** sigue sin explorar.
- **`ERR-ALG-06` nunca sobrevive como error real** (su `|v|` es un producto y la cota `40*R` lo
  mata): la rama `"multiplicar"` de `op_val`/`op_txt` está verificada **por lectura, no por
  medición**. Declarado, no cerrado.

---

**Última actualización:** 2026-08-20, tras el lote de §10 y la auditoría de cierre (`RECHAZAR` ×2).
**Ciclo REABIERTO.** Bloqueante vivo: la **regresión de magnitud** de §10.7 (`M5` **+31,0 pp**,
margen mediano 73-84 %), introducida por el sustituto de `invertir`. Residuo heredado: §P7
**+9,4 pp** contra un control oficial de +5,2 pp (1,4 sd, no significativa), familia divisibilidad.


---

## 11. EL REDISEÑO DE LA GRILLA (2026-08-20) — decisión del profesor, camino (c) de §10.7.1

§10.12 y L-9 declaraban `V1` **techo estructural** y remitían la decisión al profesor. El profesor
eligió **rediseñar la grilla numérica**. Resultado: **7 de los 8 canales cerrados**, y el que queda
no es del diseño sino del ítem oficial.

### 11.1 Lo que estaba mal en el diagnóstico anterior

**`V1` no era techo estructural, y `E2` lo causaba un filtro puesto por otra razón.** Ver L-9, que
queda **REFUTADA** con sus cifras. En una frase: el canal no venía de mostrar la ecuación, sino de
que **las dos opciones de paso 3 mostraban la MISMA**, con lo que sólo la clave era coherente
consigo misma. Cada señuelo divide entre `d`, `b` o `a` — que **son coeficientes**, de las
ecuaciones originales.

### 11.2 Los siete cambios, cada uno con su medición

| | Cambio | Cierra | Medido |
|---|---|---|---|
| **C1** | `var_mismo` sorteado 1/2: el divisor del señuelo es `d` **o** `b` | `V5` | +15,3 → +7,3 |
| **C2** | Nuevo `ERR-ALG-08`, hermano simétrico de 04: cancela la incógnita y conserva `ci` | `NTm` `U1` | +19,5 → −0,5 · +8,8 → −1,1 |
| **C3** | Rejillas `d ∈ [10,40]`, `b ∈ [50,200]` (152 → 424 estructuras) + `dig_ok` exigido en el paso 2 | `L1` | 81,7 % → 50,0 % en el estrato |
| **C4** | **Cada opción cita la ecuación de la que su divisor es coeficiente** | `V1` | +21,8 → +0,3 |
| **C5** | Retirado el filtro `coef ∉ {a,d,b}` (su colisión ya no existe tras C4) | (habilita C6) | — |
| **C6** | `coef_sis` exigido en el paso 3 + retirada la palabra «reducida» | `E2` `L2` | +21,8 → +0,0 · +20,5 → −5,5 |
| **C7** | Con el error real en el paso 3, los señuelos del otro paso son 04 y 08 (los que cancelan) | `U1` | +15,6 → +2,9 |

### 11.3 Vector completo, antes y después (batería congelada, N=100, semillas 424242/31)

| Regla | antes | después | |
|---|---:|---:|---|
| V1 divisor == coef de su propia ecuación | +21,8 | **+0,3** | cerrado |
| E2 divisor NO es coef del enunciado | +21,8 | **+0,0** | cerrado |
| NTm la opción con menos números | +19,5 | **+2,8** | cerrado |
| V5 divide entre el número más grande | +15,3 | **−5,8** | cerrado |
| M6c descartar la de mayor \|valor\| | +14,7 | **+7,3** | cerrado |
| L1 la más corta | +10,0 | **−8,0** | cerrado |
| **X1 dice «toda la ecuación»** | **+9,0** | **+9,0** | heredado del ICFES · **una muestra**: media real **+5,3**, ver 11.5 |
| U1 su ecuación tiene una incógnita | +8,8 | **+2,9** | cerrado |
| P1 cita el paso 1 | +1,8 | +6,5 | (sube por C7; bajo el corte) |
| M5 la de menor \|valor\| final | +2,5 | −17,8 | ver 11.4 |

**Máximo: +21,8 → +9,0 pp en ESTA muestra.** Sobre cinco muestras independientes: **≈24,3 → ≈7,8**,
y cambiando de regla cada vez (ver 11.5-bis). Sin `X1`, el máximo atribuible al diseño es **M6c +7,3**.

### 11.4 Señales inversas — declaradas, no escondidas

Igualar en una dirección crea señal en la contraria (§10.7.1 y regla #22 v1.3 lo advierten). Estas
tres reglas **no están en la batería congelada** —L-8 prohíbe ampliarla a mitad de ciclo—, así que
se miden aparte y se declaran:

| Regla complementaria | antes | después |
|---|---:|---:|
| M5c descartar la de **menor** \|valor\| | −5,0 | **+6,7** |
| L1c descartar la más corta | −4,7 | −2,8 |
| L2c descartar la más larga | −3,2 | +1,5 |

**`M5c +6,7 pp` es el precio del rediseño**: por debajo del corte de canal (+8) y por encima del
control oficial (+5,3). Residuo declarado, no cerrado.

### 11.5 `X1` NO está sobre el corte: era ruido de una sola muestra

`X1` mide **+0,0 pp exactos fuera de la instancia canónica** y **100 % dentro de ella**. La
canónica es verbatim del cuadernillo (regla #24 H-2, L-3): su clave dice «multiplicar **toda la
ecuación**» y ninguna otra opción, porque así lo escribió el ICFES. Por §P7-A un rasgo del examen
real no es defecto del ejercicio.

**⚠️ CORRECCIÓN a lo que este documento declaró primero.** El `+9,0 pp` de la tabla de 11.3 sale de
**una** muestra de 100 versiones y se presentó como si fuera el valor del ejercicio. Medido sobre
**diez muestras independientes**:

| | |
|---|---|
| canónicas por muestra (esperado 8,3) | 5 · 9 · **12** · 7 · 8 · 3 · 2 · 7 · 8 · 10 |
| `X1` exceso | +3,7 · +6,8 · **+9,0** · +5,2 · +6,0 · +2,3 · +1,5 · +5,2 · +6,0 · +7,5 |

**Media +5,3 pp · sd 2,3 · rango [+1,5; +9,0].** El +9,0 es **la peor de las diez**, con 12
canónicas donde lo esperable son 8. Y +5,3 pp es exactamente el control oficial de §P7-A: `X1`
está **en la vara**, no por encima.

**Consecuencia operativa: la frecuencia `1/12` de la canónica NO se toca.** Se llegó a proponer
bajarla a `1/25` para «cerrar» `X1`; con la medición completa eso habría sido **ajustar el ítem al
verificador** para curar una cifra que ya estaba dentro de la vara — el vicio que §P7 nombra como
batería rellenada, aquí en su forma inversa. La lección es la de §5.1 otra vez: *mide la dispersión
antes de perseguir un residuo*.

### 11.5-bis El máximo agregado, con su dispersión

Máximo de la batería sobre cinco muestras independientes (base `r·100000+7`, mult 31, N=100):

| muestra | antes | después |
|---|---|---|
| 1 | +25,5 `V1` | +7,8 `P1` |
| 2 | +21,0 `V1` | +8,3 `P1` |
| 3 | +23,2 `V1` | +9,0 `X1` |
| 4 | +26,2 `V1` | +7,3 `M6c` |
| 5 | +25,7 `NTm` | +6,5 `M6c` |

**Media ≈ 24,3 → ≈ 7,8 pp.** Pero el dato que importa no es la media: es que **antes el máximo era
SIEMPRE la misma regla** (`V1` en 4 de 5) y ahora **cambia de identidad en cada muestra**
(`P1`, `X1`, `M6c`). Un canal real es estable entre muestras; un máximo que salta de regla en regla
es el sesgo de selección de tomar el mayor de quince — exactamente lo que §P7 dice que está inflado.
Ninguna regla concreta se sostiene por encima del corte al repetir la medición.

### 11.6 Verificación (exit real por redirección, nunca por tubería)

| | schoice | cloze |
|---|---|---|
| Clave verdadera (semillas 770001/17, POST-mezcla) | **100/100** | **100/100** |
| Segunda clave correcta | **0/100** | **0/100** |
| Ecuación citada inexistente | **0** | **0** |
| Divisor que no es coeficiente de su ecuación | **0** | **0** |
| Ortografía / glifos | exit 0 · 0 hallazgos | exit 0 · 0 hallazgos |
| Formatos | 5/5 | 4/4 + NOPS N/A esperado |
| Versiones únicas · claves únicas | 99/100 · 94/100 | — |
| Abortos de render | 0/100 | 0/100 |

Los dos gemelos dan **cifras idénticas**: comparten las funciones byte a byte, como exige el
encabezado de este documento.

### 11.7 Dos errores propios de este ciclo, declarados

1. **Verifiqué la clave sobre `opciones_pre`**, que está **antes** de la mezcla y tiene la clave
   siempre en la posición 1. Daba 81/100 «fallos» que eran míos, no del ejercicio. Es el Incidente Q
   otra vez: *toda verificación de clave se hace sobre el orden POST-mezcla*.
2. **Rompí el render corrigiendo ortografía**: `s.index("#")` tomó por comentario la línea
   `cat("### Reflexión\n\n", reflexion, …)` y renombró la variable a `reflexión`. El chunk seguía
   parseando y `data_generation` seguía evaluando — **sólo lo vio el render**. Ver L-12.

### 11.8 Estado

**FASE 2C sigue abierta**: la regla #9 exige un detractor **independiente**, y estos cambios los
hice yo. El ejercicio **no se sella**. `apto_para_aula` sigue en `false`.

Lo que cambió respecto al cierre anterior: el motivo del `RECHAZAR` ya no está vigente. La
diagnosticidad pasó de **ocho canales sobre el corte** a **uno, heredado del ítem oficial**, con la
corrección intacta y verificada.

---

## 12. V6 (2026-08-20) — FASE 2C ejecutada y sus objeciones aplicadas

Dos detractores independientes y en paralelo (SCHOICE y CLOZE), lanzados **sin `name:`**, ambos con
semillas ajenas a las del ciclo. **Los dos: `RECHAZAR`.** Sus objeciones están aplicadas y medidas.

### 12.1 El diagnóstico que ninguno de mis instrumentos dio

**§11 se declaró un éxito midiendo con la batería congelada, y con reglas ciegas era una regresión.**
El detractor del SCHOICE midió cuatro reglas que la batería no tiene —comparan dos números de la
misma opción **por razón**, no por igualdad, y condicionan una propiedad estructural al paso citado—
sobre el estado anterior y el nuevo, con control aleatorio calibrado:

| Regla ciega | pre-v5 | V5 (§11) | **V6** |
|---|---:|---:|---:|
| G1 la opción de paso 3 con ecuación de UNA incógnita | −0,5 | +22,5 | **+0,0** |
| G2 entre las de paso 2, la del coeficiente menor | +0,0 | +10,9 | **−1,5** |
| G4 mayor cociente constante÷coeficiente | +4,5 | +33,2 | **−0,7** |
| **G10** (G1, y si no aplica G2) | −0,5 | **+40,9** | **−1,5** |
| *control aleatorio* | *−4,3* | *−4,3* | *−1,0* |

**La lección, y es la más cara de este ejercicio:** optimicé contra el instrumento, y el instrumento
no cubría la familia adonde se fue la señal. Es §P7 «batería rellenada» en su forma inversa. *Una
batería congelada protege de mover la vara a mitad de ciclo; no protege de que la vara sea corta.*

### 12.2 Las tres correcciones (binarias, bloqueantes)

| | Defecto | Frecuencia | Origen |
|---|---|---|---|
| **OBJ 1** | `conserva_igualdad = FALSE` en `ERR-ALG-06`/`07` quedó obsoleto con C4: ese señuelo ya no multiplica un miembro, **divide la ecuación completa**, que SÍ conserva la igualdad. La Solution imprimía «rompe la igualdad entre los dos miembros» —falso— y se contradecía con su propia `causa_raiz` dos líneas después | **57/200 (28,5 %)** | C4 |
| **OBJ 1-bis** | El rótulo de `ERR-ALG-04` describía sólo su forma como error real; como señuelo (`restar_invertido`) hace lo contrario | 50/50 | preexistente |
| **OBJ 3** | La frase de cierre incrustaba la opción entera en una subordinada («…sostiene que **En el paso 2, …**»): los cuatro `sub()` sólo conocían los moldes canónicos | 183/200 (91,5 %) | preexistente, agravado por C4 |

Las tres cerradas. **OBJ 1 verificada por mutación**: con el flag revertido la sonda dispara 11/20;
con el fix, 0/20 y aparece la frase verdadera en las mismas 11 versiones.

### 12.3 La decisión de fondo: el molde del cuadernillo

`G2`/`G4` se cerraron sustituyendo `ERR-ALG-08` por **`ERR-ALG-09`**. Aquel empataba la *forma* con
la clave pero su coeficiente era `b + k·d`, y **`|b − k·d| < b + k·d` es una identidad** (119/119
estructuras): la clave llevaba SIEMPRE el coeficiente menor, sin contenido matemático alguno.
`ERR-ALG-09` tiene el **coeficiente correcto** —cancela la incógnita igual que la clave— y yerra
sólo en el término independiente, con dos variantes sorteadas para que el cociente de la clave quede
**en medio** y ningún extremo la identifique.

`G1` costó más, y obligó a corregir mi propio juicio. Lo defendí como «distractor con contenido»
—quien lo usa aplica que no se puede despejar con dos incógnitas—, pero **lo que delata no es la
invalidez de esa opción: es que EXISTA una opción con esa forma, luego el error está en el paso 3**.
Eso es leer una regularidad de construcción.

**Retirado el estrato del paso 3, como paso del error real y como paso señuelo.** Razón medida:
**todos** los canales que este ejercicio peleó durante trece pasadas —`V1`, `E2`, `V5`, `DEN`, `U1`,
`M5` y finalmente `G1`— vivían allí. Las opciones de paso 3 nombran un divisor **y** citan una
ecuación: dos grados de libertad que las de paso 1 y 2 no tienen, y cada cierre en uno abría el otro.

**Y el criterio decisivo es el del ICFES, no el de la métrica:** el ítem oficial no tiene ninguna
opción de paso 3. Sus cuatro opciones son **dos de paso 1 y dos de paso 2** (verificado sobre
`pagina_013.jpg`). El estrato 3 era una extensión nuestra, y era la que generaba los canales.
`ERR-ALG-06` y `ERR-ALG-07` siguen en el catálogo como errores documentados; ya no se instancian.

### 12.4 Verificación

| | schoice | cloze |
|---|---|---|
| Clave verdadera · segunda clave · ecuación citada · divisor coherente | **4/4 OK** | **4/4 OK** |
| Sondas G (script aparte, L-8) | todas **≤ +0,0** | idem (motor compartido) |
| Batería congelada, máximo | **+9,7** (`NTm`) | **+9,7** |
| Ortografía · glifos | exit 0 · 0 hallazgos | exit 0 · 0 hallazgos |
| Formatos | 5/5 | 4/4 + NOPS N/A esperado |
| Versiones únicas · claves únicas | 99/100 · 94/100 | — |
| Abortos | 0/100 | 0/100 |

### 12.5 Residuo declarado

**`NTm` +9,7 pp** (la clave está entre las opciones con menos números). Causa: la clave del paso 2
tiene **3 números porque ha eliminado una incógnita**, y las de paso 1 tienen 4 porque conservan las
dos. Es consecuencia del método, no artificio: `ERR-ALG-09` y `ERR-ALG-04` comparten esa forma con
la clave, así que dentro del estrato la regla deja **dos** opciones en pie, nunca una. Piso
estructural del 50 % por estrato. Cerrarlo del todo exigiría imprimir la reducida como
`0C + 120P = …`, lo que rompería la coherencia con el estímulo y con la canónica verbatim.

**Objeción 3 del detractor CLOZE (instrumento), no aplicada:** el veredicto de
`bateria_congelada.R` sale del **agregado** mientras su sección por estrato imprime `<== CANAL` sin
influir en el exit, y `X1` lo hace bloquear o no según la semilla. Queda declarado, no corregido:
es infraestructura del subproyecto y merece su propia medición.

### 12.6 Estado

**FASE 2C ejecutada con dos detractores independientes; sus objeciones bloqueantes están cerradas y
verificadas.** El ejercicio **no se sella**: aplicar las objeciones caduca el veredicto que las
emitió (regla #9), así que el sello exige una pasada de confirmación sobre V6.
`apto_para_aula` sigue en `false`.

---

## 13. V7 (2026-08-20) — pasada de confirmación de la FASE 2C y sus objeciones aplicadas

Dos detractores independientes sobre V6, lanzados **sin `name:`**, con semillas ajenas al ciclo.
**SCHOICE: `APROBAR_CON_CAMBIOS`. CLOZE: `RECHAZAR`.** Los dos confirmaron que **la corrección está
cerrada** —300 versiones, 30 exportaciones a Moodle, parsers propios, 0 hallazgos— y rechazaron por
diagnosticidad. Sus objeciones están aplicadas y medidas.

### 13.1 El fix de una línea… que abortaba el render en 27 de cada 100 versiones

El detractor del SCHOICE aisló el mecanismo de `NTm +9,6 pp` (media de 3 bases, por encima del
corte de §P7-A que obliga) y propuso un contrafactual **ya medido**: forzar la variante 1 de
`ERR-ALG-09` cuando su variante 2 produce un independiente negativo, porque `ecu()` deja entonces
un miembro vacío y escribe `= 0`, un **tercer número** que aísla a la clave.

Aplicado literalmente: **27 abortos de 100**. Causa, que el propio detractor del CLOZE había medido
en su objeción G sin conectarla: con el error real en el paso 1, la **var 1 de `ERR-ALG-09` ES la
ecuación del Paso 2 del estímulo** (174/174), el gate del bucle la rechaza siempre y los 400
reintentos se agotan.

`var09()` comprueba las dos restricciones con prioridad explícita: **(a) el gate del bucle antes
que (b) el guard del `0`**. Con eso, y sobre las tres bases del propio detractor:

| base | V6 | **V7** | exit |
|---|---:|---:|---|
| 606060/41 | +9,0 (`NTm`) | **+7,0 (`NTm`)** | 1 → **0** |
| 424242/31 | +9,7 (`NTm`) | **+9,0 (`X1`)** | 1 → 1 |
| 918273/13 | +10,2 (`NTm`) | **+7,8 (`NTm`)** | 1 → **0** |

Reproduce **exactamente** el contrafactual del detractor, incluido que en 424242 el máximo pasa a
`X1`. **Lección:** un contrafactual medido por quien no ejecuta el render puede ser correcto en la
métrica y letal en la construcción. *La diagnosticidad se mide sobre versiones que existen.*

### 13.2 El tercer código, y por qué la frecuencia no bastaba

`cods_paso_real` dejaba **un solo código por paso** (01 y 04): `paso_real → cod_real` era una
biyección y la clave de la Parte 4 del CLOZE sólo podía tomar **2 de 7** textos. La regla léxica
«elige la que puede ser clave» acertaba **74-77,5 %** (+49 a +52,5 pp).

`ERR-ALG-09` pasa a `apto_real = TRUE` con su propiedad `PROP-INDEP` (L-17). Medido, **no bastó**:
+42,7 / +37,5 / +36,0 pp. El cuello no era el número de claves sino que **`|S|` variaba**: con
`|S| = 1` en 26-40 de cada 100 versiones, la regla acierta el 100 % allí, y por Jensen
`E[1/|S|] > 1/E[|S|]`.

Fijando que **las tres elegibles se muestren siempre**, el pago es exactamente `1/3`:

| | V6 | 09 apto_real | **+ las tres siempre visibles** |
|---|---:|---:|---:|
| regla léxica de P4 | +49,0 / +48,5 / +52,0 | +42,7 / +37,5 / +36,0 | **+8,3 / +8,3 / +8,3** |
| compuesto P4 → paso → P1 (OBJ C) | +30,8 / +33,2 / +30,8 | — | **+7,8 / +7,3 / +6,7** |

**+8,3 pp es el piso estructural** de un ítem con tres errores reales posibles; bajarlo exige un
cuarto código apto y no lo hay (02/03/05 quedan fuera por razón matemática).

### 13.3 La regresión que el cambio SÍ produjo, y por qué se acepta

`L1 «la más corta»` sube de **+5,0 a +13,0 pp** agregado (+37,5 → +54,2 dentro del estrato 2): con
la clave de 09, el compañero `ERR-ALG-04` en su forma canónica lleva `P1 + k·P2`, siempre mayor que
el `P1 − k·P2` de la clave. La batería sigue en `BLOQUEA` (+11,5 a +13,5 en seis corridas).

**Se acepta, y la razón es §P7-B — medida, no argumentada:**

| regla | aísla la clave | margen mediano | margen máximo |
|---|---:|---:|---:|
| `NTm` en **V6** | 8/100 | **33,3 %** | 33,3 % |
| `NTm` en **V7** | **0/100** | — | — |
| `L1` en V7 | 27-30/100 | **3,9 %** | **6,1 %** |
| `L2` en V7 | 20-28/100 | 3,4-5,1 % | 5,6 % |

El cambio **sustituye un canal de 33 % de margen —un número menos de cuatro, visible de un
vistazo— por uno de 4-6 %**, dos o tres caracteres sobre setenta. Por el umbral del 15 % que este
repositorio calibró para H1, el primero es explotable y el segundo no. *La frecuencia subió y la
explotabilidad bajó: es el resultado correcto, y sólo se ve midiendo el margen.*

### 13.4 `dig_ok` NO era vestigial — objeción 4 refutada por medición

El detractor dedujo, correctamente, que L-11 justificaba `dig_ok` con `ERR-ALG-08`, retirado en V6,
y pidió medirlo antes de retirarlo (H-5). Medido: retirarlo empeora `L1` agregado a
**+13,5 / +14,5 / +15,0** y `NTm` dentro del estrato 2 a **+32,1 / +28,1 / +25,0**. El filtro sigue
igualando longitudes, ahora con `ERR-ALG-09`, que comparte con la clave el coeficiente `b − k·d`.
**No se retira**; se corrige la razón escrita. *Un filtro cuya justificación caducó no es lo mismo
que un filtro inerte.*

### 13.5 Lo que se declara y NO se persigue — L-18

`Z1`, `Z3`, `W1`, `W3` (razón entre dos números de la misma opción; proporcionalidad con `E2`)
miden +10 a +36 pp en nuestro ejercicio. **El ítem oficial de la página 18 puntúa 1,0 en las tres
medibles** y el nuestro 0,375 / 0,509 / 0,615: es **menos filtrable que el examen que reproduce**.
La objeción B del detractor CLOZE («entre las dos del mismo paso, la del número mayor»,
+14/+22/+15 pp tras V7) es **el mismo canal** desde el par del paso 1. Por §P7-A no se persiguen.
**Limitación declarada:** el contraste oficial es n = 1 ítem, no el corpus de 426.

### 13.6 Verificación

| | schoice | cloze |
|---|---|---|
| Abortos | **0/100** | **0/100** |
| Corrección (verificador independiente, 3 bases × 100) | **0 fallos** | **0 fallos** |
| — control positivo por mutación (sólo lo impreso) | **21/40 disparos** | idem |
| Marca-vs-verdad sobre XML de Moodle | — | **0 de 100** |
| Formatos | **5/5** | **4/4** + NOPS N/A esperado |
| Ortografía · glifos | exit **0** · exit **0** | exit **0** · exit **0** |
| Diversidad sustantiva | PASS exit 0, 97/100 | PASS exit 0 (`WARN_DIV_BAJA` en p4: 3 claves) |
| Diagnosticidad (H1/H2/H3/H3b) | PASS exit 0 | PASS exit 0 |
| Multisemilla N=100 | APROBADO, 100 % | APROBADO, 100 % |
| Versiones únicas / claves | **290/300** · 266/300 | **290/300** · 266/300 |
| Batería congelada (3 bases) | +12,5 / +13,0 / +11,5 → `BLOQUEA` | +12,0 / +13,0 / +13,0 → `BLOQUEA` |

**Dos defectos de mis propias sondas, cazados por sus controles y declarados:** el verificador de
corrección negaba el signo del término independiente (100/100 falsos positivos sobre un V6
verificado correcto) y luego confundía **proporcionalidad** con **igualdad**, contando a
`ERR-ALG-03` —que multiplica por `f = b/d` y produce un múltiplo verdadero de `E2` pero inútil para
el paso— como segunda clave en 50/100. Y su `cola()` no reconocía el molde **canónico** (`así:`),
lo que dejaba las 4 canónicas de cada 100 como «0 opciones verdaderas». *Las tres veces la sonda
estaba rota y el artefacto sano; las tres las cazó el control positivo sobre V6.*

### 13.7 ⚠️ CADUCADA el 2026-09-12 — la palanca que describe no existe. Ver §14.2.

**Lo que decía:** que tras el fix el máximo lo tomaba `X1` «en una de cada tres bases», y que sin
la decisión del profesor sobre la frecuencia de la canónica «la batería seguirá diciendo `BLOQUEA`
en una de cada tres bases por una causa que no es corregible sin tocar el cuadernillo».

**Las dos afirmaciones son falsas sobre el artefacto que el propio §13 dejó en disco.** Medido el
2026-09-12 sobre seis bases: la batería bloquea en **6 de 6**, y el máximo lo toma **`L1 la más
corta`** en 5 de esas 6 (`X1` sólo en 314159). §13.3 ya lo había dicho —`L1` subió a +13,0 al
aplicar V7— y §13.6 lo acredita; §13.7 se quedó describiendo el estado intermedio de §13.1.

**Además, bajar la frecuencia canónica no desbloquea**: el barrido de §14.2 muestra que 1/12 ya
está en el mínimo de la curva y que las dos direcciones empeoran. El texto original se conserva
abajo porque su aritmética sobre `X1` sigue siendo correcta y explica el mecanismo.

#### Texto original (conservado, con su conclusión caducada)

Tras el fix, en una de cada tres bases el máximo lo toma **`X1 «dice toda la ecuación»` (+9,0 pp)**.
Mecanismo medido: en las versiones no canónicas **ninguna** opción contiene esa frase, así que `S`
queda vacío y la convención §P7 paga `1/n = 0,25`; en la canónica la clave **es** la opción C
oficial y paga 1. Con `is_canonical = 1/12`: `(1/12)·1 + (11/12)·0,25 = 0,312` → **+6,2 pp
esperado**, contra +6,8 a +9,0 medido.

**No es un defecto de redacción y la regla #24 H-2 prohíbe normalizar el verbatim.** Lo que sí es
nuestro es la **frecuencia** de la canónica. Decisión del profesor entre (a) bajarla (1/12 → 1/20)
o (b) declararlo coste aceptado de la fidelidad verbatim. **Sin esa decisión, la batería seguirá
diciendo `BLOQUEA` en una de cada tres bases por una causa que no es corregible sin tocar el
cuadernillo.**

### 13.8 Estado

**Objeciones aplicadas y medidas; el veredicto que las emitió queda caducado (regla #9).** El
ejercicio **no se sella**: exige otra pasada de confirmación. `apto_para_aula` sigue en `false`.
Presupuesto §P7-D: **ésta es la primera de las tres pasadas del ciclo V6**.

---

## 14. V8 (2026-09-12) — FASE 2C pasada 2 de 3, y el residuo declarado cambia de regla

Dos detractores independientes sobre V7, lanzados **sin `name:`**, con semillas ajenas al ciclo
(`313131/141592/271828` y `313377/717171/202511`). **Los dos: `APROBAR_CON_CAMBIOS`.** Los dos
acreditaron la corrección **con control positivo por mutación** —sin el cual un cero no significa
nada—: 300/300 y 300/300 sobre el mutante, 0/300 sobre el artefacto real; el del CLOZE añadió
30/30 sobre un XML de Moodle corrompido a propósito.

### 14.1 Dos objeciones aplicadas, y dos errores de los detractores corregidos antes de aplicarlas

**(a) El canal de la Parte 4 del CLOZE: +26 a +31 pp.** «Marcar siempre *Multiplicar una igualdad
por un número exige…*» acertaba el 55-60 % contra un techo nulo del 29 %. La causa diagnosticada
es aritmética: `paso_real` se sorteaba uniforme sobre el **paso**, pero el paso 1 tiene **un**
código `apto_real` y el paso 2 tiene **dos**, y la canónica además fuerza el paso 1 →
`P(ERR-ALG-01) = 1/12 + (11/12)/2 = 0,542`.

> ⚠️ **El fix que propuso el detractor tiene un error aritmético, verificado antes de aplicarlo.**
> Dividía el peso del paso 2 entre sus dos códigos (`(4/11)/length(c2)`), cuando `4/11` ya es el
> peso de **cada uno**. Los pesos suman 0,636, `sample()` los normaliza y la marginal aterriza en
> **0,476**, no en 1/3 — habría dejado un exceso de ~+22 pp creyendo haberlo cerrado.

Aplicado con la fórmula general `p(canónico) = (1/K − pc)/(1 − pc)`, `p(resto) = (1/K)/(1 − pc)`,
que suma 1 exactamente para cualquier K. Medido: marginal **36,7 / 30,7 / 32,7 %** (era 58/22/20),
y sobre 400 versiones renderizadas de verdad, `paso_real == 1` en **124/400 = 31 %**.

**(b) La Solution nombraba el mismo código como diagnóstico y como argumento rechazado.**

> ⚠️ **El detractor cifró el alcance en 178/300 sumando «146 de paso 1 + 32 canónicas».** Las
> canónicas **tienen** `paso_real == 1` (línea 290), así que las contó dos veces. La cifra real es
> **161/300**, que es el 100 % de la rama de paso 1, canónicas incluidas. La sustancia se sostiene.

Antes de escribir la prosa que afirma «repite el error que pretende corregir», se **verificó por
ejecución** que es verdad en los 161/161 casos: el señuelo lleva siempre `ambito="primer"` y
`op2="sumar"`. Verificado tras aplicar, sobre 100 HTML renderizados: **0 versiones contradictorias**
y el bullet nuevo aparece en **23/23** de las de paso 1 — coincidencia exacta, ni una de más.

### 14.2 La decisión del profesor sobre `X1`: coste aceptado, y la palanca no existía

Barrido completo de la frecuencia canónica, media del máximo agregado sobre **6 bases**:

| frecuencia | media | BLOQUEA |
|---|---:|---|
| 1/6 | 14,27 pp | 6/6 |
| 1/8 | **11,25 pp** | 6/6 |
| **1/12 (actual)** | **11,30 pp** | 6/6 |
| 1/20 (la opción que §13.7 proponía) | 13,33 pp | 5/6 |

> **Nota de método (pasada 3):** este barrido se hizo sobre **V7**, donde `pc_canon` todavía no
> existía —lo introdujo el fix de §14.1—, así que tocar sólo `sample.int()` era entonces correcto.
> **Desde V8 ya no lo es**, y por eso la pasada 3 exigió la guarda `K_CANON` (§14.8): en V8, cambiar
> una constante sin la otra desbalancea la marginal en silencio.

1/8 y 1/12 son **indistinguibles** (0,05 pp, muy por debajo del ruido de ~2,2 pp) y las dos
direcciones empeoran: **el valor actual ya está en el mínimo de la curva y ninguna frecuencia
desbloquea**. Decisión del profesor (2026-09-12): **coste aceptado de la fidelidad verbatim**
(regla #24 H-2 prohíbe normalizar el cuadernillo). Se deja 1/12 y no se vuelve sobre ello.

### 14.3 EL RESIDUO DECLARADO CAMBIA DE REGLA: es `M6c`, no `L1` ni `X1`

Objeción 2 del detractor SCHOICE, confirmada de forma independiente por el del CLOZE. Aplicando
§P7-B **a cada regla que dispara**, y no sólo a la que sale como máximo:

| regla | exceso | la clave sobrevive | margen mediano | ¿§P7-B la exime? |
|---|---:|---:|---:|---|
| `L1` la más corta | +7,3 a +24,3 pp | 114/300 | **3,8 %** | **sí** — inexplotable |
| `L2` la más larga | +9,7 pp | 122/300 | **3,4 %** | **sí** — inexplotable |
| **`M6c` descartar la de mayor \|valor\|** | **+8,3 pp** | **299/300** | **37,4 %** | **NO** |

`L1` y `L2` se distinguen por **dos o tres caracteres sobre setenta**: por el umbral del 15 % que
este repositorio calibró para H1, no son explotables — y **por eso mismo no pueden ser el residuo
declarado**, que es lo que §13.3 y §13.6 hacían. El único que supera el corte de +8 pp **y**
sobrevive al filtro de margen es `M6c`: «descarta la opción del número más gordo» nunca elimina la
clave (299/300) y sube el azar de 25 % a 33,3 %.

Causa estructural, ya diagnosticada en §10.7.1: **los señuelos suman o multiplican, la clave
resta.** El ítem oficial comparte la propiedad (en la canónica el mayor es la opción D,
`1.300.000`, y la clave es la C) — pero ese contraste es **n = 1**, la misma limitación que L-18
declara sobre sí misma. **Residuo declarado, no perseguido.**

### 14.4 El trade-off del fix, medido en las dos direcciones

| dimensión | V7 | V8 | ¿explotable? |
|---|---:|---:|---|
| CLOZE P4, **frase completa** (memorizar una frase) | 52,0 % → +27 pp | **35,7 % → +10,7 pp** | **sí** → mejora 16 pp |
| CLOZE P4, primera palabra «Restar» (\|S\|=2, paga ½) | 48 % | 68 % | parcial — nunca aísla la clave |
| SCHOICE **`M6c`** (el residuo real) | +8,3 pp | **+8,3 pp** | sí → **sin cambio** |
| SCHOICE `L1` | +11,3 pp medio | +24,3 pp medio | **no** — margen 3,8 % idéntico |
| Corrección (verificador propio, 3 bases) | 0/300 | **0/300** | — |
| Abortos | 0/300 | **0/300** | — |

> ⚠️ **ESTA TABLA FUE CORREGIDA POR LA PASADA 3 (§14.8). Tres cifras suyas están mal:**
> 1. **«empeora sólo lo que no lo es» es FALSO.** El re-pesado subió también `NTm` (+0,8 → **+8,6 pp**)
>    y `U1` (−2,8 → **+6,7 pp**), y a esos §P7-B **no los exime** (brecha del 50 % y categórica).
>    Mecanismo exacto: `exceso(U1) = p₂·½ − ¼`, con `p₂` pasando de 0,50 a 0,63. Ver §14.8.
> 2. **El +10,7 pp del CLOZE está inflado por selección**: es el máximo sobre las tres frases. La
>    media de las tres **pre-especificadas** es **+8,3 pp** — el piso exacto de `|S| = 3`, no un
>    residuo por encima del corte. §P7 exigencia 2 prohíbe exactamente esto.
> 3. **El «68 % de primera palabra» NO es un score §P7**: es tasa bruta. Bajo la convención `1/|S|`
>    mide **33,3 % = +0,0 pp**, o sea el techo nulo. No era un empeoramiento.

**El fix mejora lo explotable y empeora dos reglas que nunca aíslan la clave.** Sólo se ve midiendo
el margen: la cifra de `L1` sube 13 pp mientras su margen se queda **exactamente igual** (mediano
3,8 %, máximo 5,8 %, antes y después; la pasada 3 lo confirmó dígito a dígito en 3,9 %/6,1 %).

**Por qué NO se persigue el canal de primera palabra** (48 → 68 %): que `PROP-SIGNO` y `PROP-INDEP`
compartan el prefijo «Restar» es **deliberado y documentado** en el propio bloque de propiedades —
garantiza que la clave nunca sea el único singleton de prefijo, de modo que H2 no dispare por
construcción (L-4). Reformular una de las dos cerraría este canal y **reabriría aquél**. Se declara.

### 14.5 Familia sin sonda, declarada y NO dada de alta (L-19)

La batería se congeló el **2026-08-20**; la regla #22 **§P7-E** es del **2026-08-22** y exige al
menos una regla **relacional entre pares**. Ninguna de las 15 congeladas lo es. El detractor la
midió **fuera** de la batería, como L-15 manda: «elegir una de las dos opciones que comparten el
miembro izquierdo» da **0,203 → −4,7 pp**. **No hay canal**, porque el par gemelo existe en los dos
estratos y se compensan. También midió y **refutó** su propia hipótesis sobre el mecanismo de `L1`
(`Dmin` «menos dígitos en el número final»: −5,1 pp). No se da de alta: L-8 exigiría re-medir el
histórico completo. Anotada como **L-19**.

### 14.6 Verificación

| | schoice | cloze |
|---|---|---|
| Abortos | **0/300** | **0/300** |
| Corrección (verificador propio que parsea lo IMPRESO, 3 bases × 100) | **0 fallos** | **0 fallos** |
| Coherencia matemática | exit **0** | exit **0** |
| Multisemilla N=100 (regla #23) | **APROBADO 100 %** | **APROBADO 100 %** |
| Formatos | **5/5** | **4/4** + NOPS N/A esperado |
| Ortografía · glifos | exit **0** · exit **0** | exit **0** · exit **0** |
| Diversidad sustantiva | **PASS** exit 0 | exit 0 (`WARN_DIV_BAJA` en p4, estructural) |
| Diagnosticidad (H1/H2/H3/H3b) | **PASS** exit 0 | **PASS** exit 0 |
| Solution sin contradicción de código | **0/100 versiones** | n/a |
| Batería congelada (6 bases) | +15,0 a +31,0, máx `L1` (exento §P7-B) | — |
| **`M6c`, el residuo declarado** | **+8,0 a +8,3 pp** | — |

### 14.7 Estado de la pasada 2

Objeciones aplicadas y medidas; el veredicto que las emitió quedó caducado (regla #9). Fue la
**pasada 2 de las 3** del presupuesto §P7-D.

---

## 14.8 PASADA 3 de 3 (2026-09-12) — confirmación, y la corrección de tres cifras mías

Dos detractores independientes sobre V8, sin `name:`, con semillas ajenas (`606061/838383/959595` y
`808017/525252/616161/909091`). **Los dos: `APROBAR_CON_CAMBIOS`. Ningún defecto de CORRECCIÓN.**

| | schoice | cloze |
|---|---|---|
| Corrección, 3 bases × 100 | **0/300** | **0/300** |
| — control positivo por mutación | **267/300** | 5 mutantes, 4 al **40/40** |
| Moodle, verdad recomputada **del propio XML** | — | **0/60** · 3 corrupciones cazadas |
| Abortos | 0/300 | 0/300 |

### Las tres cifras mías que corrigieron

**(1) `NTm` y `U1` subieron, y §P7-B NO los exime** (detractor SCHOICE). Mi §14.4 decía «empeora
sólo lo que no es explotable»: medí `M6c` y `L1` y generalicé. Medido sobre las mismas bases:

| regla | V7 | V8 | margen | ¿exime §P7-B? |
|---|---:|---:|---|---|
| `M6c` (residuo declarado) | +8,3 | **+8,2** | 65-68 % | no → declarado ✓ |
| `NTm` menos números | +0,8 | **+8,6** | **50 %** | **NO** ← faltaba declarar |
| `U1` una incógnita | −2,8 | **+6,7** | categórica | **NO** ← faltaba declarar |
| `L1` | +8,8 | +23,8 | **3,9 %** (idéntico en V7) | **sí** |

Mecanismo, verificado contra la predicción: `U1` es **determinista dentro de cada estrato** (la clave
está entre las dos de una incógnita en 63/63, 65/65, 62/62 versiones de paso 2 y en 0/37, 0/35, 0/38
de paso 1), así que su exceso es **puro peso de mezcla**: `p₂·½ − ¼`, que da +0,0 pp con `p₂ = 0,50`
y +6,5 pp con `p₂ = 0,63`. Medido: +6,5 / +7,5 / +6,0.

**No es corregible sin reabrir el canal del CLOZE**: uniformar el **código** (lo que V8 hace, y lo
que la Parte 4 necesitaba) y uniformar el **paso** son incompatibles mientras el paso 1 tenga un solo
código `apto_real`, y L-17 ya descartó la única salida. **Se declara, no se persigue.**

**La elección de V8 sigue siendo la correcta, y por una razón que el trade hace explícita:**
`NTm`/`U1` **nunca aíslan la clave** (`|S| = 2`, pago ½), mientras el canal de la Parte 4 que V8
cerró **sí la aislaba** (pago 1, 55-60 %). Se cambió un canal de +26/+31 pp que aísla por uno de
+8,6 pp que no. Lo que estaba mal era presentarlo como un trade sin coste.

**(2) El +10,7 pp del CLOZE estaba inflado por selección** (detractor CLOZE). Es el máximo sobre las
tres frases elegibles, que están **pre-especificadas** por el diseño (L-17) y por tanto son medibles
sin seleccionar: **+10,3 / +8,4 / +6,2 pp, media +8,3 pp**, con sd binomial de 2,7 pp a n = 300 —
±0,7 sd alrededor del piso. No hay frase privilegiada; hay ruido alrededor de `1/3`. §P7 exigencia 2
prohíbe el máximo sobre candidatos, y yo lo apliqué a las reglas pero no a las frases.

**(3) El «68 % de primera palabra» no era un empeoramiento.** Es una **tasa bruta**; bajo la
convención §P7 `1/|S|` mide **33,3 % = +0,0 pp**, exactamente el techo nulo, porque nunca aísla. La
decisión de no perseguirlo (L-4) era correcta y es más sólida de lo que yo escribí.

### Lo que el fix mejoró sin que nadie se lo pidiera

El compuesto `P4 → paso → P1` pasó de +6,7…+7,8 pp a **+2,8 pp**, y la dependencia inversa
`P1 → paso → P4` de 77,1 % a **66,7 %**.

### La guarda `K_CANON` (objeción 1 del detractor CLOZE, aplicada)

`sample.int(12L)` y `pc_canon = 1/12` eran **dos constantes acopladas sin nada que lo comprobara**:
el `stopifnot` se satisfacía para cualquier `pc_canon ∈ (0, 1/3)`. No es hipotético — §13.7 proponía
1/12 → 1/20 y §14.2 barrió cuatro valores. Medido por el detractor: cambiar sólo `sample.int(6L)`
lleva `ERR-ALG-01` de 31,5 % a 39,5 % (predicción analítica 39,4 %), **reabriendo ~8 pp del canal que
V8 acaba de cerrar, con 0 fallos de corrección y ninguna guarda disparando**.

Aplicado `K_CANON` como fuente única más `stopifnot(abs(pc_canon - 1/K_CANON) < 1e-12)`. Verificado:
comportamiento **idéntico** con `K_CANON = 12L` (marginal 36,7/30,7/32,7, la misma cifra que antes) y
la guarda **dispara en 40/40** sobre un mutante desacoplado.

### Residuos §P7 declarados al cierre — la lista completa

| artefacto | regla | exceso | por qué no se persigue |
|---|---|---:|---|
| SCHOICE | `M6c` descartar la de mayor \|valor\| | **+8,2 pp** | estructural §10.7.1: los señuelos suman, la clave resta |
| SCHOICE | `NTm` · `U1` | **+8,6 · +6,7 pp** | canal de mezcla de estratos; incompatible con cerrar el del CLOZE (L-17) |
| SCHOICE | `L1` | +23,8 pp | **exento §P7-B**: margen 3,9 % mediano, 6,1 % máximo |
| CLOZE | regla léxica de P4, 3 frases pre-especificadas | **+8,3 pp** | **piso exacto** de `\|S\| = 3`; no hay cuarto código apto |
| CLOZE | cruzado `P1 → paso → P4` | 66,7 % | declarado por L-17; mejorado desde 77,1 % |

`M6c` es del **SCHOICE**; el CLOZE tiene su propio residuo. L-20 lo decía como si fuera común.

### 14.9 Estado final

**FASE 2C CERRADA.** Presupuesto §P7-D agotado (3 de 3) con dos `APROBAR_CON_CAMBIOS` y **ningún
defecto de corrección** en ninguna de las tres pasadas. Los cambios de la pasada 3 fueron una guarda
de tres líneas sin cambio de comportamiento y correcciones documentales.

**Queda `aprobacion_usuario` (paso 11), que es decisión del profesor, no de un agente.** Y después,
la evidencia de **Nivel 3** (aplicación en aula) antes de cualquier promoción: `apto_para_aula`
permanece en `false` hasta ambas cosas.
