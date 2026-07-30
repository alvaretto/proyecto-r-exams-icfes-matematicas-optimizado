# HANDOFF — `permutaciones-pescadores-venia-n4`

| Campo | Valor |
|---|---|
| **Ruta** | `A-Produccion/02-En-Desarrollo/permutaciones-pescadores-venia-n4/` |
| **Repo raíz** | `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams` |
| **Variantes** | **SCHOICE** (raíz) · **CLOZE** de 6 partes (`cloze/`) |
| **Origen ICFES** | `MAT-2026-1-004` (ERA-2026, Sesión 1, pregunta impresa 4) |
| **Sesión fundacional** | 2026-07-29 · **Última sesión**: 2026-07-30 (variante CLOZE) |
| **Frase de reanudación** | `Continúa con el proyecto permutaciones-pescadores-venia` |

> **Al retomar, en este orden**: este archivo → los **dos** `ejercicio_state.json`
> (raíz y `cloze/`) → [`.claude/CLAUDE.md`](.claude/CLAUDE.md) (**20** particularidades
> operativas) → [`.claude/rules/permutaciones-parametricas.md`](.claude/rules/permutaciones-parametricas.md)
> (invariantes I-1..I-7 + C-1..C-3). Ahí está el porqué del código; sin eso, un agente "arregla"
> fixes deliberados. **Solo después** abre los `.Rmd`.

---

## 0. Estado en una línea

Las dos variantes están **técnicamente cerradas y verdes**. La SCHOICE ya tiene aprobación humana
(2026-07-30). **La CLOZE está en 10/11: le falta exactamente un paso, el 11 — la revisión y
aprobación humana, que un agente no puede sellar.** No hay ningún FAIL vivo ni deuda técnica
bloqueante.

> **Sesión 2026-07-30 (tarde).** Al preparar el material del paso 11 se encontraron **dos defectos
> en la misma línea** del pool de la Parte 5 (particularidad 20): `**todos**` llegaba como
> `<strong>todos</strong>` **dentro** de un gap de Moodle (7 de 72 gaps) y el sujeto de la
> afirmación era incorrecto. Corregidos y **todo re-verificado sobre la versión vigente** (§3.1).
> Se añadió `cloze/verif_render/canonica/` con la **instancia canónica renderizada**, que es lo que
> el paso 11 pide revisar. La aprobación humana sigue pendiente.

---

## 1. Objetivo general

Producir y mantener una **familia de ejercicios ICFES metacognitivos de Nivel 4** (competencia
*Formulación y ejecución*, componente *Aleatorio*, descriptor **D4.8**) sobre el conteo de
**permutaciones lineales** de `n` elementos distintos, derivada del ítem real `MAT-2026-1-004`.

El ítem **no evalúa la aritmética del factorial**: evalúa si el estudiante reconoce que al ocupar
posiciones sucesivas el conjunto disponible **decrece**, frente a las estrategias que lo tratan como
constante. Los distractores son errores conceptuales documentados, no ruido numérico.

**Las dos variantes comparten el contrato paramétrico** (`n ∈ {4,5,6}`, clave `n!`, pool de 7
errores, invariantes I-1..I-7, instancia canónica) y difieren solo en la forma de interrogar:

| Variante | Rol | Por qué existe |
|---|---|---|
| **SCHOICE** (raíz) | 1 pregunta, 4 opciones | Sostiene **OE1**: reproduce el ítem oficial tal como se evalúa |
| **CLOZE** (`cloze/`) | 6 partes Progressive Disclosure | Descompone el mismo razonamiento para uso formativo |

**La CLOZE no sustituye a la SCHOICE.** Descomponer en seis partes cambia lo que se mide —deja de
ser «¿sabe resolverlo?» y pasa a ser «¿reconoce cada pieza?»—, así que ya no es el ítem oficial.

**No confundir con los hermanos.** `plano-cartesiano-barco-n2` comparte el patrón (opciones de texto,
ítem verbatim) pero es N2 geométrico. `desplazamiento-avion-aeropuerto` usa opciones **gráficas**:
su patrón **no** es intercambiable con éste.

---

## 2. Objetivos específicos

Tabla completa con evidencia en [`docs/ROADMAP.md`](docs/ROADMAP.md) §2. Resumen al 2026-07-30:

- **OE1-OE10**: cumplidos y verificados.
- **OE11** (evidencia Nivel 3 en aula → `03-En-Produccion/`): abierto, requiere estudiantes reales.
- **OE12** (variante CLOZE): **en curso** — solo falta la aprobación humana.

Persistidos en `~/.claude/projects/<slug>/memory/project_objetivos_permutaciones_pescadores_venia_n4.md`.

---

## 3. Estado real verificado (2026-07-30 **tarde**, re-ejecutado sobre la versión vigente)

Nada de esta tabla es evidencia heredada: todo se volvió a correr **después del fix de la
particularidad 20**, que es el último cambio del `.Rmd`.

> **Cómo comprobar que esta tabla sigue vigente**, en vez de creerle: `ls -la --time-style=+%F\ %R`
> sobre el `.Rmd` y sobre `cloze/verif_render/`. Si el `.Rmd` es **más nuevo** que la evidencia, la
> tabla no cubre el código vigente y hay que re-correr los verificadores. Así se detectó, al abrir
> esta sesión, que la evidencia de la mañana (10:31) era anterior a la última edición (11:53).

### 3.1 Variante CLOZE (`cloze/`)

| Verificación | Comando | Resultado |
|---|---|---|
| Los 11 chequeos | `cd cloze && Rscript verificar_render.R` | **V1-V11 todo verde** |
| Render | idem V1-V3 | HTML, PDF, DOCX OK |
| NOPS | idem V4 | **N/A esperado** — `exams2nops()` rechaza `extype: cloze` (§5.4) |
| Moodle | idem V5 | **12/12** versiones: 6 gaps en orden y tipo · P1 = `n!` · P2 = `n-1` · P4 = `n^n` · **P3 y P6 verificadas semánticamente** · P5 con 6 opciones y 2-4 marcas |
| Espacio de ternas | idem V6 | **105/105**; 93 legales; rango de la clave 1/2/3; mitad baja 41,9 %; «elegir el mayor» 0,0 % |
| Canónica verbatim | idem V7 | contexto 1 con `n=4` == `MAT-2026-1-004` (Parte 1) |
| Estructura CLOZE | idem V8 | 6 `##ANSWERi##` en orden == `exclozetype` == `exsolution` == `extol`; answerlist **16 / 18** |
| Selección legal | idem V9 | **240/240**; 84 ternas distintas |
| C-1 | idem V10 | 8 valores por `n` distintos dos a dos en `n = 4/5/6` |
| D6 | idem V11 | **6/6** afirmaciones con el mismo veredicto en prosa y Answerlist |
| Coherencia matemática | `validar_coherencia_matematica.R` | **APROBADO, 0 errores** |
| Diversidad sustantiva | `validar_diversidad_sustantiva.R --n 40` | exit 0 · `WARN_DIV_BAJA` (estructural, §5.5) |
| Diversidad de render | 300 evaluaciones del `data_generation` | **300/300 versiones únicas** · 90 de 93 ternas legales · 12 canónicas · 0 fallos |
| Ortografía | `corregir_ortografia_espanol.R` | sin errores |

`cloze/ejercicio_state.json`: **10/11**. Falta `aprobacion_usuario`.

### 3.2 Variante SCHOICE (raíz) — no-regresión

Se tocó por dos motivos (§5.2 y §5.3). Re-verificada entera:

| Verificación | Resultado |
|---|---|
| `Rscript verificar_render.R` | **V1-V9 todo verde** (incluye NOPS, que aquí sí aplica) |
| `validar_coherencia_matematica.R` | **APROBADO, 0 errores** |
| `validar_diversidad_sustantiva.R --n 40` | exit 0 · `WARN_DIV_BAJA` esperado |
| Ortografía | sin errores |

`ejercicio_state.json` (raíz): **11/11**, aprobado el 2026-07-30.

---

## 4. Qué se hizo en la sesión del 2026-07-30 (variante CLOZE)

1. **`/goal`** encontró **12 puntos de deriva** entre documentos y realidad, y **2 conflictos entre
   fuentes**. Todos corregidos (§5.6). Dos merecen mención porque eran *cifras citadas que nadie
   volvió a ejecutar*: el HANDOFF atribuía a V9 «89 ternas» cuando reproduce **84** (el 89 era de otra
   medición), y la memoria del proyecto afirmaba que `copias` se había corregido de 100 a **300**
   cuando el script tiene **100** con su justificación escrita.
2. **Se generó la variante CLOZE** con 6 partes, preservando el contrato paramétrico completo.
3. **Se escribió `cloze/verificar_render.R`** (V1-V11) y se **probó por mutación** (§6).
4. **Se leyó el HTML renderizado**, no solo el veredicto de los validadores. Eso destapó tres
   defectos que ningún validador marca (§5.1).
5. **Auditoría adversarial con dos agentes independientes**: 6 hallazgos, todos MENOR, todos
   aplicados (§5.7). Cero críticos.
6. **Se corrigieron tres derivas en los orquestadores del repo raíz** (§5.8).
7. Documentación sincronizada: 8 documentos del subproyecto + `.claude/` local + índice raíz.

---

## 5. Hallazgos y decisiones

### 5.1 Tres defectos que solo aparecieron al LEER el artefacto renderizado

Ninguno de los tres lo marca ningún validador del repo. Los tres se encontraron abriendo el HTML.

| Defecto | Cómo se manifestaba | Fix |
|---|---|---|
| **Desincronización de orden en la Parte 5** | La prosa de la Solution listaba las 6 afirmaciones en un orden y las opciones en otro | Decisión **D6**: la prosa **agrupa** por valor de verdad, nunca enumera. Guarda: `V11` |
| **Concordancia de género rota** | «dos de **los** bandas», «**los mismos** fotografías» en 2 de los 6 contextos (≈33 % de las versiones) | Campo `genero` por contexto + artículos derivados |
| **Vocabulario incoherente** | «Al ubicar uno de los sensores en **la fila**» cuando el enunciado habla de un circuito en serie | Campo `disposicion` por contexto (fila / hilera / secuencia), todos femeninos singulares para que la concordancia no dependa del contexto |

### 5.2 El `.Rmd` no compilaba a PDF: U+2212 en un campo que el SCHOICE nunca emitía

`exams2pdf()` falló con `Unicode character − (U+2212)`. La causa estaba en `descripcion_corta` de
`EST-PER-04`, que contenía el **signo menos tipográfico**. En el SCHOICE ese campo **nunca se
emite** —es dato muerto—, así que la mina llevaba ahí desde la creación del ejercicio sin que nadie
la pisara. La CLOZE sí lo emite (son las opciones de su Parte 3) y explotó.

Corregido **en los dos `.Rmd` a la vez** para que los pools no diverjan. Cableado como incidente en
los dos orquestadores del repo. Las rayas `—` (U+2014) sí compilan; el problema es específicamente
U+2212.

**Patrón para recordar: un campo que no se emite no está probado.**

### 5.3 `exshuffle`: por qué `TRUE` y no `FALSE` (decisión D6)

Al detectar la desincronización de la Parte 5, la salida fácil era `exshuffle: FALSE` (la
aleatorización interna la dan `perm`/`perm3`/`perm5`). Se descartó: dispara **`ERR_C4`**, bloqueante,
porque ICFES exige mezcla. Y la lista ordenada que se pretendía salvar era **redundante** con el
Answerlist de la Solution, que ya da el veredicto por opción alineado por construcción.

Detalle completo en [`docs/BLUEPRINT.md`](docs/BLUEPRINT.md) §7.4.

### 5.4 NOPS es N/A para todo CLOZE, y no por los gaps `num`

`exams2nops()` rechaza **cualquier `extype: cloze`** antes de mirar `exclozetype`. Verificado en el
código de `exams` 2.4.2 (`wrong_type <- ufile[utype == "cloze"]`). El Incidente E del
`orquestador-cloze` lo enunciaba mal («N/A esperado **con gaps num/string**», y afirmaba que un CLOZE
100 % schoice **sí** debía renderizar NOPS) — corregido.

La restricción **no está documentada oficialmente** por R/exams (consultado 2026-07-30): `?exams2nops`
enumera los tipos soportados y omite `cloze` sin decir que no lo admite.

### 5.5 Decisiones cerradas — no reabrir sin autorización

| Decisión | Contenido |
|---|---|
| **Flujo B = false** | Ninguna de las dos variantes tiene figura |
| **`n ∈ {4,5,6}`** | Medido, no elegido. Particularidad 3 |
| **`WARN_DIV_BAJA` se acepta** | Estructural: solo hay 3 claves legales. `ERR_DIV_COSMETICA` sí sería fallo, y no ocurre |
| **D5** | `fmt()` sin separador de miles en la CLOZE (hay gaps `num` que el estudiante escribe); sin `pick_int()` |
| **D6** | La prosa de la Solution nunca enumera la Parte 5 en orden |
| **La CLOZE no sustituye a la SCHOICE** | §1 |

### 5.6 Deriva documental corregida (12 puntos)

Pool «de cinco» cuando son siete (README, SYLLABUS) · cinco marcas «(pendiente)» sobre archivos que
existen · recuento de líneas documentado (585) distinto del real · la tabla de `docs/BLUEPRINT.md` §2 con **cuatro filas
sin su primera celda** (defecto que `/goal` ya había reportado el 2026-07-29 y que la memoria daba
por corregido — no lo estaba) · una cita por número de línea en §4.7 · dos criterios `[ ]` del
ROADMAP ya cumplidos · rango `EST-PER-01 a 05` cuando son 07 · versiones de documento desfasadas ·
`copias` 100 vs 300 · V9 84 vs 89 · referencias `OE1-OE11` y `D1-D4`.

### 5.7 Auditoría adversarial (2 agentes independientes) — 6 hallazgos, 0 críticos

Los seis se **verificaron de forma independiente** antes de actuar (uno de ellos con 200.000 ternas
aleatorias). Todos aplicados.

| # | Hallazgo | Estado |
|---|---|---|
| 1 | Comentario con razones máx/clave del pool **antiguo** de 4 fórmulas (2,7x/5,2x/10,8x); las reales con el pool de 7 son **5,0x/6,0x/10,8x** | ✅ corregido en los dos `.Rmd` |
| 2 | Vocabulario «fila» incoherente con 2 de los 6 contextos | ✅ campo `disposicion` |
| 3 | Comentario que atribuía a la Parte 6 un uso de `sujeto` que no hace | ✅ reescrito |
| 4 | **La guarda `pct_max > 0` de V6 es código muerto**: `legal_rank` solo acumula ternas con `any(d > corr)`, lo que implica que la clave no puede ser el máximo *dentro de ese subconjunto*. La rama es **inalcanzable** | ✅ documentado como dato descriptivo en **los dos** verificadores. La protección real de H1 la da **V9** |
| 5 | V10 es redundante con V6 en cobertura (probado por mutación: una colisión del pool produce 15 ternas duplicadas que V6 ya detecta) | ✅ comentario honesto; se conserva por su mensaje causal y porque sería la única guarda si `n_slots` bajara a 1 |
| 6 | V5 no miraba el gap 5 en absoluto, y las Partes 3 y 6 solo se comprobaban **estructuralmente** («hay una marcada»), no semánticamente | ✅ V5 ahora verifica que la marca de P3 corresponda al error que produce el valor del enunciado, que la de P6 sea coherente con el factor, y que P5 tenga 6 opciones y 2-4 marcas. **Probado por mutación** |

**El hallazgo 4 es el más valioso**: el subproyecto documentaba esas tres condiciones de V6 como
«guardas de no-regresión que **fallan**, no avisan». Una de las tres no podía fallar nunca.

### 5.8 Correcciones propagadas al repo raíz

- `orquestador-cloze`: Incidente E corregido · **V4 subido de 4 a 6 partes** (el estándar subió el
  2026-06-04 y esta validación se quedó atrás) · nuevos **Incidente Q** (prosa que enumera en orden)
  e **Incidente R** (campo sin emitir) · nuevas validaciones **V6** y **V7**.
- `orquestador-schoice`: nuevo **Incidente O** (campo sin emitir — es el lado que *deja* la mina).
- `.claude/commands/orquestador-cloze.md` sincronizado.
- `.claude/CLAUDE.md` raíz: entrada de changelog v3.19.0.

### 5.9 Hallazgos abiertos

Ninguno bloqueante. En [`docs/BACKLOG.md`](docs/BACKLOG.md): `pick_int()` es código muerto en el
SCHOICE (P2.2) · la restricción `exams2nops`+cloze no está documentada upstream (P2.3) · la URL
`R-exams.org/tutorials/moodle_quiz/` da **404** (P2.4) · el corrector ortográfico del repo no
sustituye palabras en MAYÚSCULAS y entra en bucle (P1.4) · el falso positivo de `--fix` con
`codigo-rmd.md` (P1.1).

---

## 6. Prueba de mutación del verificador CLOZE

Un verificador que nunca se ha visto fallar no es evidencia. Cada guarda se probó desactivando lo
que debía atrapar:

| Mutación | Resultado esperado | Resultado real |
|---|---|---|
| Clave falsa en la Parte 1 (+ I-5 y el `stopifnot` del alias desactivados) | V5 falla | **8 incoherencias en 12/12** |
| `exshuffle: FALSE` → `TRUE` con la prosa sin agrupar | V11 falla | **falla**; V5 sigue verde (la clave no cambia) |
| Marca de la Parte 3 movida a otra opción | V5 falla | **11 incoherencias en 12/12** (en 1 acertó por azar) |
| `k_v <- 5` (fuera del rango 2-4 de la Parte 5) | V5 falla | **12/12** |
| Marca de la Parte 6 invertida | V5 falla | **12/12** |

La primera mutación **hubo que repetirla**: al primer intento el chunk abortaba por un `stopifnot`
*distinto* del que se pretendía neutralizar, así que el mutante no llegaba a probar V5. Eso reveló
que la clave está protegida por **dos** guardas independientes, no una.

---

## 7. Riesgos

| Riesgo | Mitigación cableada |
|---|---|
| Un agente trata la CLOZE como reemplazo de la SCHOICE | Particularidad 14 + §1 |
| Un agente "unifica" el `fmt()` de las dos variantes | Particularidad 15: rompe los gaps `num` |
| Un agente restaura la lista ordenada de la Parte 5, o pone `exshuffle: FALSE` | Particularidad 16 + `V11` + `ERR_C4` |
| Un agente toca el pool sin re-correr V10 | Particularidad 17 |
| Un agente reporta el N/A de NOPS como fallo | Particularidad 18 + `V4` |
| Un agente introduce U+2212 en un campo emitido | Particularidad 19 + Incidentes Q/R/O de los orquestadores |
| Un agente factoriza el pool común a un archivo compartido | Particularidad 1 (auto-contención) + el propio validador de diversidad fallaría |
| Un agente edita una variante y no la otra | Regla local `permutaciones-parametricas.md`: el contrato rige para las dos |
| **Correr «el verificador del SCHOICE» y ejecutar el del CLOZE sin darse cuenta** | Ocurrió en esta sesión: un `cd cloze` sin subshell dejó el directorio cambiado. Los comandos de la regla local usan `(cd cloze && …)` |

---

## 8. Cómo retomar

```bash
cd /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams
bash .claude/scripts/workflow-state.sh status A-Produccion/02-En-Desarrollo/permutaciones-pescadores-venia-n4
bash .claude/scripts/workflow-state.sh status A-Produccion/02-En-Desarrollo/permutaciones-pescadores-venia-n4/cloze

cd A-Produccion/02-En-Desarrollo/permutaciones-pescadores-venia-n4
Rscript verificar_render.R            # SCHOICE → V1-V9,  "todo verde"
(cd cloze && Rscript verificar_render.R)   # CLOZE → V1-V11, "todo verde"
```

### Próximo paso concreto

**Revisión humana de la variante CLOZE** (paso 11). Es lo único que falta y no lo puede sellar un
agente. Material para revisar:

- `cloze/verif_render/canonica/canonica1.html` y `.pdf` — **la instancia canónica ya renderizada**
  (contexto 1 con `n = 4`, semilla 20). Su **Parte 1** coincide verbatim con `MAT-2026-1-004` y sus
  opciones son `{64, 4, 24, 16}`. Es el artefacto que conviene leer primero.
- `cloze/verif_render/` — HTML, PDF, DOCX y el XML de Moodle de una versión cualquiera.

  > `verif_render/` está **gitignored** (artefactos regenerables). Para reconstruir la canónica:
  > `(cd cloze && Rscript render_canonica.R)`. El script localiza la versión evaluando el chunk con
  > semillas sucesivas hasta que `ctx_idx == 1` y `n == 4`, y **confirma el acierto buscando el
  > enunciado oficial en el HTML** en vez de darlo por supuesto. Semillas que la producen: 20, 28,
  > 58, 81, 133.
- Las **6 partes**: conviene leer una versión entera de principio a fin y juzgar si la progresión
  cognitiva (aplicar → comprender el decrecimiento → analizar el error → transferir → evaluar
  propiedades → V/F) es la adecuada para N4.

Si se aprueba:
```bash
bash .claude/scripts/workflow-state.sh complete \
  A-Produccion/02-En-Desarrollo/permutaciones-pescadores-venia-n4/cloze aprobacion_usuario
```

Después, **OE11** para las dos variantes: aplicación en aula (requisito de Nivel 3 para
`03-En-Produccion/`).

**Destino reservado en producción**:
`03-En-Produccion/06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/10-Combinatoria_Permutaciones-Variaciones-Combinaciones/`

---

## 9. Enlaces

[`README.md`](README.md) · [`.claude/CLAUDE.md`](.claude/CLAUDE.md) ·
[`.claude/rules/permutaciones-parametricas.md`](.claude/rules/permutaciones-parametricas.md) ·
[`docs/SYLLABUS.md`](docs/SYLLABUS.md) · [`docs/ROADMAP.md`](docs/ROADMAP.md) ·
[`docs/BACKLOG.md`](docs/BACKLOG.md) · [`docs/BLUEPRINT.md`](docs/BLUEPRINT.md) ·
[`cloze/verificar_render.R`](cloze/verificar_render.R)

Ficha oficial del ítem: `Todo-Pajaro/Alineacion-curricular-de-items/Simulacros/Alineacion-Curricular-de-items-ERA-2026/Matematicas/Alineacion-curricular-de-items-Matematicas-ERA-2026.md` (líneas 965-996).
Ítem espejo `MAT-2026-1-029` (mismo D4.8, conteo **con** repetición): líneas 1946-1985. Es la
**Parte 4** de la variante CLOZE.

---

**Versión**: 3.1 (particularidad 20 — markup dentro de un gap de Moodle y sujeto de la afirmación —
con todo re-verificado; instancia canónica renderizada para el paso 11; regla de vigencia de la
evidencia por mtime; v3.0 — variante CLOZE; decisiones D5 y D6; invariantes C-1..C-3; auditoría
adversarial de 6 hallazgos; 12 puntos de deriva documental corregidos; tres correcciones propagadas
a los orquestadores del repo raíz)
**Fecha**: 2026-07-30
