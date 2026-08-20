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
creado el 2026-08-20. Comparte `eq_de`, `ejecutar_prop` y `proc_de_opcion` **byte a byte**:
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
   hay exactamente 3 `cod_real` posibles, porque `ERR-ALG-06` nunca sobrevive a la cota de
   plausibilidad. Ese mismo hecho es el que hacía funcionar la fuga entre gaps de §10.

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
