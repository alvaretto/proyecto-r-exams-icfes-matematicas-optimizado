# HANDOFF — `sistema_ecuaciones_eliminacion_numerico_variacional_argumentacion_n4_schoice_v1`

> **Léeme antes de tocar nada.** Este ejercicio consumió ~5 M tokens en 10 pasadas de corrección y
> 4 auditorías de detractor. Casi todo lo que se te ocurra intentar **ya se intentó y está medido
> aquí**. El objetivo de este documento es que no repitas nada.

**Origen:** `MAT-2026-1-044` · ERA-2026 Sesión 2 · pregunta **impresa** 44 · `pagina_013.jpg`
(verbatim de `MAT-2026-1-130`).
**Fecha del ciclo:** 2026-08-19.
**Estado:** 9/11 pasos sellados. **Abiertos: `detractor_fase2c` y `aprobacion_usuario`.**

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
| **Magnitud `\|v\|/R`** | ✅ cerrado | 3,3 · 7,5 · 2,4 por paso (era 3,3 · 7,9 · **3.600**) |
| **OBJ 3 del detractor** (Solution falsa) | ✅ | 0/300 atribuciones falsas de «la igualdad no se conserva» |
| **`cod_mismo ≠ cod_real`** | ✅ blindado con `stopifnot` | era 98/98 duplicados en rama 2 |
| **Léxico ramas 2 y 3** | ✅ | 0,0 % |
| **Ortografía / glifos** | ✅ | exit 0 / exit 0 |
| **5 formatos** | ✅ | html · pdf · docx · nops · moodle, en R limpio |
| **Versiones únicas** | ✅ | **100/100** (estándar del profesor; el 250/300 de la regla #3 NO aplica aquí) |

---

## 2. Qué está ABIERTO

> ⚠️ **ACTUALIZADO por la 5.ª auditoría (2026-08-19). El «+13,9 pp BLOQUEA» que este documento
> declaraba NO ES REPRODUCIBLE con una vara homogénea.** Salía de `auditoria_propia.R`, cuya batería
> **creció durante el ciclo** — justo la serie que §P7-C prohíbe encadenar. Medido con la batería
> **congelada** de la vara: **−4,2 pp (PASS)**.

1. **§P7 — el residuo real, con su banda.** La batería congelada resultó **ciega al valor** en este
   molde (`n1()` toma el primer número, que aquí es el **número de paso**: aplicabilidad **0 %** en
   sus cinco reglas de divisibilidad). Corregida la ceguera y aplicada **la misma ampliación a las
   tres poblaciones**:

   | Población | n | máx | techo nulo | sd | **exceso** |
   |---|---:|---:|---:|---:|---:|
   | **Ejercicio (actual)** | 100 | 41,9 % | 32,5 % | 2,80 | **+9,4 pp** |
   | Control oficial | 399 | 33,6 % | 28,4 % | 1,15 | **+5,2 pp** |
   | Corpus oficial | 426 | 32,9 % | 28,3 % | 1,11 | **+4,6 pp** |

   Frente al control: **+4,2 pp de diferencia, 1,4 sd → no significativa.** Frente al corte de +8:
   **0,5 sd**, con intervalo a 2 sd de **[+3,8; +15,0]**. Y el umbral de la sonda decisiva
   (`múltiplo de 50 y positivo`, 41,9 %) **lo fijó el auditor conociendo el ejercicio** — lo declara
   él mismo. **La cifra honesta no es una, son las dos.**
2. **H1: CERRADO.** Todos los márgenes entre **3,4 % y 5,7 %**, contra el umbral de 15 %. La rama 3
   bajó además de 67,9 % a 54,8 % con la citación uniforme. `validar_diagnosticidad.R` reporta
   **H1 = 0 % en ambas direcciones** y `PASS`. ⚠️ **Mide siempre el margen antes de perseguir una
   frecuencia** — ver §5.1.
3. **L-4 / objeción 4: CERRADA.** H3b contenido al **17 %** (el bloqueo está en 90 %).
4. **«Canal de divisibilidad ×50 al 44,7 %»: REFORMULADO.** La sonda simple da 27-35 % (azar 25 %):
   ahí no hay canal. Vive en la **conjunción** con «positivo».
3. **19 tokens con soporte < 20** declarados `NO CONCLUYENTE` (`N_necesario` 200 agregado / 76 rama 1).
4. **FASE 2C abierta** — 4 veredictos `RECHAZAR`, el último con la objeción crítica ya cerrada.
5. **`ejercicio_state.json` con sellos stale** (anteriores al `.Rmd`) y `versiones_unicas: 97`, que
   es la salida de diversidad sustantiva, **no** el conteo de versiones. Debe decir `100/100`.

---

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
| Este ejercicio ahora | 100 | **+12,6 pp** |

**La hipótesis del «piso irreducible» para ítems-ecuación está REFUTADA**: esa familia mide *por
debajo* del control, no por encima. Y este ejercicio **no pertenece a ella** — sus opciones son
prosa con ecuación embebida, así que su clase de comparación es el control. Detalle completo en la
memoria `ref_vara_p7_items_ecuacion.md`.

**Límite de potencia declarado:** a n=27 la sd es 5,6 pp; establecer la vara de la familia exigiría
≈212 ítems y el corpus tiene 27. No se puede descartar un piso pequeño (~+3 pp); sí se descarta que
un piso explique +13 pp.

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

## 7. Las 4 auditorías de detractor

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

**Última actualización:** 2026-08-19, tras la pasada 10 (última) y la 4.ª auditoría.
**Ciclo cerrado.** Residuo principal: §P7 `BLOQUEA` +13,9 pp, familia divisibilidad.
