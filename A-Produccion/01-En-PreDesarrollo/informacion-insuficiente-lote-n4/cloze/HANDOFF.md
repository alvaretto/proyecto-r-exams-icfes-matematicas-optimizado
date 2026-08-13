# HANDOFF — CLOZE `informacion_insuficiente_lote_..._n4_cloze_v1`

**Última sesión:** 2026-08-13 · **Estado:** **11/11 — APROBADO PARA LLEVAR AL AULA**

> ✅ **Aprobación del profesor: 2026-08-13.** El ciclo técnico está cerrado y el ejercicio puede
> aplicarse con estudiantes.
>
> **Lo que esa aprobación NO es:** no es la promoción a `03-En-Produccion/`. El gate de
> `/promover-ejercicio` es la **evidencia de Nivel 3 con estudiantes reales**, que se recoge
> *después* de llevarlo al aula. Por eso el ejercicio sigue aquí, en `01-En-PreDesarrollo/`, con
> 11/11 — igual que `permutaciones-pescadores-venia-n4`, que está aprobado y vive en
> `02-En-Desarrollo/`. Un 11/11 sin aula no promueve.
>
> **Siguiente hito real: aplicarlo en clase y recoger la evidencia.**

> **FASE 2C CERRADA en pasada de confirmación: `VEREDICTO_DETRACTOR: APROBAR`.**
> Las 6 objeciones aplicadas y verificadas por un agente distinto sobre la versión vigente
> (`REPORTE_DETRACTOR_FASE2C_CONFIRMACION.md`). El único hallazgo nuevo de esa pasada era un
> **comentario falso** que yo había escrito al declarar el par `POR-08`/`POR-09` — afirmaba que
> `elegir_p6` solo usa `vecinos()` para los distractores, y el código lo desmiente tres líneas
> más abajo. Corregido: `POR-08` **sí** sale del pool de P6 cuando `POR-09` está en P1; lo que
> sostiene su papel es que eso pasa solo en una fracción de versiones, comprobado por rama.

> **LO PRIMERO AL RETOMAR.** El `.Rmd` está íntegro y verificado (smoke 250/250, banco de 25
> preguntas, 25 textos únicos). No hace falta comprobar si quedó a medias como en la sesión
> anterior. Empieza por el estado de los pasos 7 y 8, que son los únicos técnicos abiertos.

## Qué es

Gemelo CLOZE del SCHOICE hermano (directorio padre, **SOLO LECTURA**), derivado del ítem oficial
`MAT-2026-1-010` (descriptor **D4.9**, información insuficiente). 6 partes Progressive Disclosure:

| Parte | Tipo | Evalúa |
|---|---|---|
| P1 | schoice | IDENTIFICAR — es el ítem oficial verbatim: ¿cuál pregunta NO se puede responder? |
| P2, P3 | num | CALCULAR dos magnitudes determinadas |
| P4 | mchoice | EVALUAR — todas las que sí se pueden responder (nº de verdaderas sorteado) |
| P5 | schoice | JUSTIFICAR — «el enunciado no dice ⟨H⟩; ¿cuál queda sin respuesta por eso?» |
| P6 | schoice | TRANSFERIR — si se QUITA un dato, ¿cuál deja de tener respuesta? |

**Motor:** banco de **25 preguntas**, cada una con `det` en función de 4 bits del enunciado
— `L` (medidas), `T` (trazado), `A` (asignación), `N` (nº de partes) — y su `DEP` declarada.
5 ramas vivas en `COMBOS_INFO`: `-T-N`, `-TAN`, `LT--`, `LTA-`, `L--N`.

## ⚠️ MUESTRA ESTÁNDAR: N = 100 (regla #23, nueva el 2026-08-13)

`.claude/rules/muestra-estandar-validacion.md`. **Toda medición usa N = 100** y los validadores
ya lo traen por defecto: invocarlos **sin `--n`** da el estándar. Las cifras de 400 que aparecían
en la versión anterior de este handoff eran ad-hoc y son justo lo que la regla vino a eliminar.

**Excepción medida y cableada — análisis por rama.** Los bloques B y D de `auditoria_propia.R`
condicionan por rama, así que el N global se reparte: con 100, la rama más ligera (`LT--`) queda
en **n=7**, y ahí una tasa no es concluyente. Verificado el 2026-08-13: el token `área` daba
**71,4 % (5/7) con N=100** → RECHAZADO **falso**, y **48,3 % (14/29) con N=400** → correcto.
Por eso el verificador ahora:

- declara **NO CONCLUYENTE** cualquier estrato con `n < MIN_ESTRATO` (20), en vez de puntuarlo;
- calcula el N que lo dictaminaría y lo imprime;
- devuelve **`SIN VEREDICTO` (exit 1)**, nunca `APROBADO`, si queda algún estrato sin medir.

**Comando de auditoría de este ejercicio:** `Rscript auditoria_propia.R <rmd> 300`
(300 = el N que la propia herramienta calcula para que `LT--` alcance n≥20).

## Estado de los 11 pasos

| # | Paso | Estado |
|---|---|---|
| 1-5 | análisis · flujo_b · generación · retroalimentación · render | ✅ sellados |
| **6** | arsenal post-render | ✅ **sellado 2026-08-13** |
| **7** | **detractor FASE 2C** | ⬜ **primera ejecución lanzada el 2026-08-13** — ver abajo |
| 8 | coherencias 5 | ⬜ depende de la auditoría visual (paso 6b) |
| **9** | validar_diversidad | ✅ **sellado — 300/300 versiones únicas (100 %)** |
| **10** | validar_icfes | ✅ **sellado** (con el límite declarado abajo) |
| 11 | aprobación del profesor | ⬜ |

## Cifras verificadas (2026-08-13, banco de 25)

| Medición | Resultado |
|---|---|
| Smoke 250 semillas | **250/250** · 25 preguntas · 25 textos únicos, sin colisión |
| `auditoria_propia.R … 300` | **APROBADO, 0 hallazgos, todos los estratos dictaminados** (exit 0) |
| Bloque **A** (fuga inter-parte) | **0/300 en las 6 sondas bloqueantes**, incluida la nueva **A8** (opción de P6 que reaparece en P4) |
| Bloque **B** (fuga léxica) | peor token 69,1 % (`largo`, rama `-TAN`) — bajo el umbral de 70 %, **pero por poco** |
| Bloque **D** — P6 por rama | `-T-N` 37,8 % · `-TAN` 11,1 % · `L--N` 22,0 % · `LT--` 27,3 % · `LTA-` 19,4 % |
| Bloque **D** — P5 por rama | 9,7 % – 18,2 % |
| Render 5 formatos (R limpio) | HTML/PDF/DOCX/Moodle **4/4 OK**; NOPS **N/A esperado** (rechaza todo `extype: cloze`) |
| `validar_diagnosticidad.R` | **PASS** — H2/H3 declaradas CIEGAS (molde uniforme), H3b 10-21 % |
| `validar_diversidad_sustantiva.R` | **PASS 6/6 gaps** |
| `validar_coherencia_matematica.R` | **APROBADO** (0 errores) |
| `validar_multisemilla.R` | **100/100 semillas, 100 % de éxito** |
| Versiones únicas (producto) | **300/300 (100 %)** — umbral 250+ CUMPLE |
| Ortografía | **0 detecciones en texto visible** (99 líneas detectadas, todas comentarios de código en ASCII deliberado) |

## LOS 2 DEFECTOS DE LA SESIÓN ANTERIOR: CERRADOS

### 1. H3b = 100 % en P6 (ramas `-T-N` y `LT--`) → **CERRADO**

**La hipótesis del handoff anterior era incorrecta a medias.** No bastaba con que `POR-07` y
`POR-09` fueran únicas en su dependencia. Medido con un diagnóstico que enumera las claves
realmente disponibles:

- En esas dos ramas **solo un hecho es retirable**: con el otro no quedan ≥3 opciones que
  mantengan respuesta, así que el `for (h in ...)` de `elegir_p6` no tiene alternativa.
- La segunda candidata, `POR-06`, **queda vetada siempre** por ser gemela de `POR-01`/`POR-04`,
  indeterminadas en esas ramas y por tanto candidatas a clave de P1.
- Resultado: `n_claves_posibles = 1` en **57/57** (`-T-N`) y **27/29** (`LT--`). No era azar del
  sorteo: era falta de material. El fix propuesto («preferir un hecho con ≥2 candidatas») **no
  tenía de dónde elegir**.

**Fix aplicado (decisión del profesor, 2026-08-13):** banco **24 → 25** con `GEO-INF-POR-08`
(«¿Cuál es el lado del lote al que son paralelos los cortes?», `DEP="T"`), sin gemelas. Al quitar
`T` quedan 2 claves posibles en ambas ramas aunque `POR-06` siga vetada.
**Medido: 100 % → 37,8 % y 27,3 %.**

Se descartaron: retirar las 2 ramas (perdía 21,5 % de versiones), y relajar el veto de P6
(reabría una fuga cerrada con argumento explícito).

### 2. `WARN_DIAG_SUPERFICIAL` en p5 → **CERRADO**

Causa: las preguntas nuevas habían roto **a medias** el molde uniforme de opciones, dejando la
clave con prefijo «cuál» en el 93 %. Fix: los dos textos que se salían del molde vuelven a
`¿Cuál es…?` — `FIJ-08` (era «¿Qué relación hay…») y `POR-07` (era «¿Cuántos cortes…»).
Ahora el molde es uniforme al 100 %, las sondas **declaran su ceguera** explícitamente y **H3b
toma el relevo** midiendo 10-21 %. `validar_diagnosticidad.R` da **PASS sin WARN**.

## Historia de correcciones (para no repetirlas)

Siete ciclos, y **cada fix desplazó el defecto de canal**. Vale la pena leerlo antes de tocar nada:

1. **§P4-bis** — P5 con veredicto invariante → su clave es ahora una **pregunta**, no un conjunto.
2. **Error 32** — el token `forma` delataba la clave de P1 al 85,6 % en una rama → 37,2 %.
3. **Fuga inter-parte P2/P3 → P4** (22,5 %) → cerrada; `cods[par_23]` es **innegociable**.
4. **Cupo**: el render bajó a 104/250 → reintento conjunto P5+P4 + banco 20 → 24.
5. **Fuga inter-parte P2/P3 → P5** (65,2 %) → cerrada, integrada como bloque **A5**.
6. **(2026-08-13)** molde uniforme a medias → prefijo al 93 % → molde completo, H3b de relevo.
7. **(2026-08-13)** P6 sin material → banco 24 → 25 con `POR-08`.

## Verificadores

- **`auditoria_propia.R`** — mide lo que el arsenal NO mide: **A** fuga inter-parte (5 sondas) ·
  **B** fuga léxica por rama · **C** alcanzabilidad · **D** H1/H3b **condicionados por rama** ·
  **E** coherencia de claves. Ahora con `MIN_ESTRATO` y veredicto `SIN VEREDICTO`.
  **Por qué existe:** `validar_diagnosticidad.R` agrega **sin condicionar por rama** y daba `PASS`
  sobre un defecto del 100 % dentro de una rama.
- Arsenal estándar: los cuatro validadores, **sin `--n`** (default 100 desde la regla #23).

## Trampas ya pagadas en este subproyecto

- `cmd > log 2>&1; echo $?` **sí** da el exit real; la **notificación del harness** informa el exit
  del comando compuesto. Lee el log, no la notificación.
- Encadenar con `&&` un `grep` que no encuentra nada **corta la cadena** (grep devuelve 1).
- Los validadores de `.claude/scripts/` son **symlinks** a `SOURCES/scripts_validacion/`… salvo
  `validar_diagnosticidad.R` y `validar_diversidad_sustantiva.R`, que son **archivos regulares**.
- **U+2212 en el HTML es MathML legítimo**, NO el Incidente O.
- Las cifras de un agente ajeno no siempre se reproducen: re-mide antes de construir sobre ellas.
- **Una tasa sobre una submuestra de 7 no es una medición** (ver regla #23 arriba).

## FASE 2C — EJECUTADA POR PRIMERA VEZ (2026-08-13)

`VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS` · reporte completo en **`REPORTE_DETRACTOR_FASE2C.md`**
(en este directorio). El primer intento se quedó sin presupuesto midiendo y no entregó; el
reintento con contexto limpio y prohibición de medir sí entregó. **De las 6 objeciones, 2 son
contra `POR-08`, la pregunta añadida ese mismo día** — la independencia hizo su trabajo.

| # | Objeción | Severidad | Estado |
|---|---|---|---|
| 1 | `contextos[[6]]` nunca decía «rectangular» y `FIJ-09` lo afirmaba igual | CRÍTICA-ALTA | ✅ **CORREGIDA** |
| 2 | `veto_p4_2` deja de excluir `cods[idx_p6]`: P4 y P6 pueden mostrar la misma pregunta | ALTA | ✅ **CORREGIDA** |
| 3 | `UBI-02` culpa al hecho equivocado sin trazado; la Solution de P5 se contradice | MEDIA-ALTA | ✅ **CORREGIDA** |
| 4 | `POR-08` presupone las franjas que `POR-06`/`POR-07` declaran no dadas; su contra es tautológico | MEDIA-ALTA | ✅ **CORREGIDA** |
| 5 | `POR-08` nombra el valor de `POR-09` y no están declaradas gemelas | MEDIA | ✅ **CORREGIDA** |
| 6 | P5 anuncia el hecho que bloquea a la clave de P1 en las ramas de omisión única | MEDIA | ✅ **DECLARADA** (residuo, ~60 % del peso) |

**Objeción 2 — era real y grave, medido:** el veto de P4 excluía `cods[idx_p6]` solo en el
escalón 1; en los escalones 2-5 la Parte 4 podía ofrecer el mismo texto que la Parte 6. Como las
cuatro opciones de P6 están **todas determinadas hoy** y su molde lo transparenta, resolver P6
acredita que se responden con el enunciado original. Fix: `cods[idx_p6]` en todos los escalones +
aserción en C-1 + **nueva sonda A8** en `auditoria_propia.R`, que lo deja **medido** y no solo
aseverado. **Prueba de mutación: al revertir el fix, la fuga aparece en 49/100 versiones y A8 la
caza con RECHAZADO.**

**Objeción 4 + G4b:** al reescribir el `motivo` de `POR-08` («cómo se divide el lote») la regex de
`G4b` habría dejado de cubrir ese texto, así que se **amplió la guarda** para no perder capacidad
de cazar una regresión futura. Corregir un texto sin mirar qué guarda lo vigilaba es la forma
habitual de dejar una defensa ciega.

**Objeción 6 — declarada, no corregida:** en las ramas de omisión única (combos 2 y 4, **52/86 ≈
60 % del peso**) la Parte 5 nombra el único dato que falta, que es lo que la Parte 1 pide
descubrir. No se cambia el molde de P5 (los dos rediseños alternativos ya están medidos y
descartados). **Queda declarado con su peso en el código y aquí, para que quien apruebe sepa qué
aprueba.**

**Objeción 1, corregida y verificada:** seis de las siete plantillas narrativas declaraban la forma
del lote; la 6 («Diálogo implícito») no. Como `FIJ-09` («¿Cuál es la forma del lote?») está marcada
`det = TRUE` **siempre**, en esa plantilla una opción marcada «sí se puede responder» **no era
respondible**, y su `razon` le citaba al estudiante una frase que el enunciado no contenía —
penalizando justo a quien hace lo que pide D4.9. El caso duro era `ctx6 ∧ !info_L` (peso 38/86).
Fix: la plantilla 6 declara la forma. Se añadió la guarda **G7b**, que exige que **todas** las
plantillas la declaren con ambas formas de `MED` (G7a solo miraba `contextos[[1]]`, y un `grep` de
«rectangular» daba seis aciertos y parecía limpio). **Probada por mutación: al revertir el fix,
G7b tumba las 5 semillas.** Smoke posterior: 250/250.

> ⚠️ **El veredicto del detractor CADUCÓ** al editar el `.Rmd` para aplicar la objeción 1
> (regla #9: un detractor caduca con cualquier edición posterior). Cuando se cierren las 5
> objeciones restantes hay que **relanzar la FASE 2C** sobre la versión final.

## Próximos pasos, en orden

1. **Pasada de confirmación de la FASE 2C**: lanzada el 2026-08-13 tras aplicar las 6 objeciones
   (aplicarlas caduca el veredicto anterior). Si no entregó con `VEREDICTO_DETRACTOR:`, recuperar
   su reporte de la transcripción en `…/subagents/agent-<nombre>-<hash>.jsonl` — el canal de
   entrega ha fallado 1 de cada 2 veces en este subproyecto, pero el reporte **sí** está en disco.
2. **Paso 6b — auditoría visual HTML completa**: pendiente. Falta lectura visual de v03-v30,
   **desbordes en móvil 360 px** y los pares ambiguos de tildes (`esta/está`, `solo/sólo`,
   `aun/aún`, `si/sí`). Sin esto no se puede sellar el paso 8 (coherencia visual-texto).
3. **Pruebas de mutación**: ninguna escrita. Todas las invariantes siguen `sin_prueba_de_deteccion`.
4. **Paso 11**: aprobación del profesor.

## Pendientes declarados, medidos y sin resolver

- **7 preguntas nunca pueden ser clave de P1** (`POR-01..05`, `FIJ-03`, `FIJ-05`): el filtro de
  ambigüedad de conjunto las rechaza siempre. Gemelo del defecto ALTA 3 del SCHOICE hermano.
- **Fuga léxica al 69,1 %** en la rama `-TAN` (token `largo`): pasa por 0,9 puntos. Un cambio
  menor en el pool puede cruzarlo.
- **Literalidad ICFES = juicio humano.** Los 7 campos oficiales son **idénticos** a los del SCHOICE
  hermano (verificado), pero los catálogos canónicos JSON **no viven en este repo**, así que
  ningún script compara los `exextra[…]` contra la fuente oficial. El paso 10 se selló con ese
  límite declarado.
