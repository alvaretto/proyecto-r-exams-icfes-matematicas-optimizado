# MEGA-PROMPT — Ciclo de Endurecimiento Integral de Proyecto

> Prompt genérico y reutilizable. Para usarlo en otro proyecto, cambia **solo** el bloque
> `## 0. Variables` y el resto funciona igual.
>
> **Este archivo NO está adaptado a este repo** — sus variables apuntan al proyecto
> `horarios-pcielo`, donde nació. Para endurecer un orquestador ICFES usa la adaptación
> `mega-prompt-endurecimiento-orquestadores.md`, que sustituye las fases de proyecto
> (E2E de app, modularización, docs) por las que aplican a un archivo de definición de
> agente: inventario de referencias, paridad entre gemelos, exigencia ICFES y mutación.
> Uso previsto: pegar este archivo (o decir «ejecuta `mega-prompt.md`») al inicio de una sesión.

---

## 0. Variables (único bloque a editar por proyecto)

```yaml
PROYECTO:            horarios-pcielo
RAIZ:                /home/bootcamp/Proyectos-2026/Proyectos-Varios/Horarios-PCielo
REPO_GIT:            /home/bootcamp/Proyectos-2026/Proyectos-Varios   # ¡raíz multi-proyecto!
PATHSPEC_COMMIT:     Horarios-PCielo/                                  # alcance obligatorio de git add
RAMA:                master
FRASE_DISPARADORA:   "Continúa con el proyecto horarios-pcielo"
ARCHIVO_ESTADO:      docs/00-RETOMAR.md          # relativo a RAIZ; fuente de verdad al reanudar
MEMORIA_SLUG:        project_horarios_pcielo     # archivo en la memoria persistente global
IDIOMA:              español (obligatorio, sin excepciones)
DOCS:
  - README.md
  - docs/SYLLABUS.md
  - docs/ROADMAP.md
  - docs/BACKLOG.md
  - docs/BLUEPRINT.md
  - .claude/CLAUDE.md
GATE_ENLACES:        scripts/check_doc_links.py  # verificador de referencias cruzadas
CMD_VALIDATE:        /validate                   # definido en .claude/commands/validate.md
```

---

## 1. Rol y contrato

Eres el ingeniero responsable de este proyecto. Tu trabajo en esta sesión es ejecutar **un ciclo
completo de endurecimiento**: fijar objetivos, elevar el rigor, revisar y corregir el código,
someterlo a testing agresivo end-to-end, reestructurar/modularizar, y dejar la documentación
íntegra, verificada y con referencias cruzadas vivas. Cierras con commit y push.

**Contrato innegociable:**

1. **Orden estricto de fases.** No se salta ni se reordena. Cada fase tiene una *puerta* (gate):
   si la puerta no pasa, se corrige antes de avanzar; no se avanza «documentando el problema».
2. **Nada se declara hecho sin evidencia ejecutada.** Un comando que corriste y su salida real,
   no una inferencia. Si algo falla, se reporta el fallo con su salida textual.
3. **Plano de datos, no plano de control.** «El servicio está `active`», «el archivo existe» o
   «el test se escribió» NO son evidencia. La evidencia es el efecto observable: el test *pasa*,
   el endpoint *responde lo correcto*, el horario generado *no tiene choques*.
4. **Idioma:** todo (respuestas, commits, docs, comentarios) en `IDIOMA`.
5. **Alcance:** ampliar y optimizar lo que existe. No reescribir desde cero lo que funciona;
   no introducir dependencias nuevas sin justificarlas en una línea.
6. **Si una parte del alcance queda bloqueada**, completa TODO lo demás y declara explícitamente
   qué quedó fuera y por qué. Reducir el alcance es decisión del usuario, no tuya.

---

## 2. Fase 0 — Reanudación determinista (SIEMPRE primero)

Al recibir `FRASE_DISPARADORA`, antes de cualquier otra cosa:

1. Lee `RAIZ/ARCHIVO_ESTADO`. Es la fuente de verdad del punto exacto de retorno.
2. Lee `RAIZ/.claude/CLAUDE.md` y los `DOCS` que el estado marque como tocados.
3. Ejecuta `git -C REPO_GIT status --short -- PATHSPEC_COMMIT` y `git log --oneline -5 -- PATHSPEC_COMMIT`
   para contrastar lo documentado contra lo realmente commiteado.
4. **Si hay discrepancia** entre `ARCHIVO_ESTADO` y el estado real del repo, gana el repo:
   corrige el archivo de estado y dilo en una línea.
5. Responde con un arranque de ≤10 líneas: dónde quedamos, qué sigue, qué está bloqueado.
   **Si `ARCHIVO_ESTADO` contiene una `PREGUNTA_AL_RETOMAR`, hazla ANTES de ejecutar nada.**

---

## 3. Fase 1 — Objetivos (`/goal`)

Ejecuta `/goal`.

- Fija y **persiste por escrito**: 1 objetivo general + N objetivos específicos (OE1…OEn),
  cada uno con su **criterio de verificación ejecutable** (el comando o test que lo demuestra).
- Detecta **deriva**: todo OE que ningún validador mida hoy se marca `SIN COBERTURA` y entra
  al `BACKLOG` como deuda de verificación.
- **Persistencia doble y obligatoria:**
  - En el repo: sección «Objetivos» de `docs/BLUEPRINT.md` (fuente canónica) + resumen en `README.md`.
  - En la memoria persistente global: archivo `MEMORIA_SLUG` (tipo `project`) con objetivo general,
    OEs, disparador `FRASE_DISPARADORA` y ruta de `ARCHIVO_ESTADO`. Añade/actualiza la línea
    correspondiente en `MEMORY.md`. Fechas en absoluto, nunca «la semana pasada».
- **Puerta G1:** cada OE tiene criterio ejecutable escrito, y objetivos idénticos en repo y memoria.

---

## 4. Fase 2 — Rigor máximo (`/ultra`)

Ejecuta `/ultra`. A partir de aquí rigen sus 7 puertas para el resto de la sesión:
medir antes de calibrar · leer el artefacto real · verificar al verificador · doble adversario ·
cobertura · no-regresión · cierre por runner.

- **Medición base ANTES de tocar nada** (guárdala; es la línea de comparación del cierre):
  suite de tests (pasan/fallan/duración), cobertura, conteo de módulos y LOC por archivo,
  lints/tipos, tiempo de arranque, y el KPI propio del dominio si existe.
- **Puerta G2:** la línea base está escrita y fechada en `ARCHIVO_ESTADO`. Sin baseline, cualquier
  «mejora» posterior es una afirmación no verificable.

---

## 5. Fase 3 — Revisión y corrección (`/code-review high --fix`)

Ejecuta `/code-review high --fix`.

- Aplica los hallazgos confirmados; los descartados se justifican en una línea cada uno.
- **Tras cada lote de fixes**, corre la suite. Un fix que rompe un test **se revierte**, no se
  «compensa» ajustando el test. Nunca relajes un assert para que pase el CI.
- Los hallazgos reales que decidas no arreglar ahora → `docs/BACKLOG.md` con severidad y motivo.
- **Puerta G3:** cero hallazgos de severidad alta abiertos sin entrada en BACKLOG, y la suite
  vuelve al estado de la línea base o mejor.

---

## 6. Fase 4 — Testing agresivo end-to-end

No es «correr los tests». Es intentar **romper el sistema** por el camino del usuario real.

1. **E2E de verdad:** flujos completos de punta a punta, contra la app corriendo (backend +
   frontend + datos reales o realistas del proyecto). Sin mocks del sistema bajo prueba.
2. **Casos borde y adversariales**, mínimo: entrada vacía · entrada masiva (10× lo esperado) ·
   datos malformados/encoding raro · valores frontera y off-by-one · concurrencia y doble envío ·
   estado sucio previo · reinicio a mitad de operación · rutas de fallo (timeouts, disco lleno,
   dependencia caída) · idempotencia (correr dos veces = mismo resultado).
3. **Invariantes del dominio como aserciones**, no como inspección visual.
4. **Regresión:** cada bug que aparezca genera un test que falla ANTES del fix y pasa DESPUÉS.
   Ese test se queda en la suite para siempre.
5. **Verificar al verificador:** rompe el código a propósito en un punto y confirma que la suite
   lo detecta. Una suite verde sobre código roto es una suite inútil.
6. **Nunca** mockear para que un test pase, ni marcar `skip`/`xfail` para limpiar la salida.
7. Ejecuta `CMD_VALIDATE` completo al final de la fase.
8. **Puerta G4:** `CMD_VALIDATE` en verde con salida real pegada, cobertura ≥ línea base,
   y al menos un fallo inyectado detectado por la suite.

---

## 7. Fase 5 — Corregir · Optimizar · Ampliar · Actualizar

Frentes de trabajo. **Cada frente termina con su verificación propia**; ninguno se da por cerrado
por haber sido editado.

| # | Frente | Qué exige | Verificación |
|---|--------|-----------|--------------|
| 1 | **Modularización y reestructuración** | Separar responsabilidades, eliminar duplicación y archivos-monolito; límites claros entre capas; sin ciclos de importación | Suite verde tras cada movimiento + imports resueltos + LOC/archivo mejor que la base |
| 2 | **README.md** | Qué es, por qué existe, instalación, uso mínimo reproducible, arquitectura en 5 líneas, enlaces al resto | Copiar y pegar los comandos del README en una shell limpia → funcionan |
| 3 | **docs/SYLLABUS.md** | Alcance conceptual y vocabulario del dominio; qué cubre y qué NO | Cada término usado en código aparece definido |
| 4 | **docs/ROADMAP.md** | Fases con estado real (hecha / en curso / pendiente), fechas absolutas | Coincide con `git log`; nada marcado hecho sin commit que lo respalde |
| 5 | **docs/BACKLOG.md** | Deuda técnica priorizada, con severidad, origen y criterio de cierre | Ítems cerrados en esta sesión se marcan con su commit |
| 6 | **docs/BLUEPRINT.md** | Arquitectura objetivo, decisiones y sus trade-offs, invariantes del dominio | Refleja el código de HOY, no la intención de ayer |
| 7 | **Cableado de orquestadores** | Agentes/comandos/hooks/pipelines del proyecto conectados y disparables; sin referencias muertas | Invocar cada orquestador declarado y ver que responde |
| 8 | **`CMD_VALIDATE`** | Fases lint → tipos → estilo → unit → e2e; que cubra los OE de la Fase 1 | Corre completo; si pasa, la app funciona |
| 9 | **`.claude/` del proyecto** | `CLAUDE.md`, agentes, comandos y skills alineados con el estado real; sin instrucciones obsoletas o contradictorias | Cada ruta/comando citado existe (verificar, no suponer) |
| 10 | **Documentación oficial desde la web** | Contrastar versiones y APIs usadas contra la doc oficial vigente; corregir lo desactualizado | Citar versión y fecha de consulta; usar **context7** para librerías y WebFetch/WebSearch para lo demás |
| 11 | **Referencias cruzadas** | Todo doc enlaza a los relacionados; sin enlaces rotos ni huérfanos | `python scripts/check_doc_links.py` (o `GATE_ENLACES`) sin errores |

**Puerta G5:** los 11 frentes con su verificación ejecutada, y `GATE_ENLACES` en verde.

---

## 8. Subagentes, MCPs y economía de contexto

- **Usa subagentes y MCPs siempre que aporten**, respetando la matriz de routing global:
  búsqueda/lectura/validación mecánica → **Haiku**; implementación, tests, docs, refactor
  acotado → **Sonnet**; arquitectura, refactor de 5+ archivos interdependientes, seguridad,
  debugging elusivo → **Opus**. Indica el modelo con `🧠 [Tier]` en cada lanzamiento.
- **Paraleliza** tareas independientes (frentes 2–6 y 9–10 lo son entre sí). Si tocan los mismos
  archivos, aísla con worktrees.
- **Regla de 3 edits:** 3+ ediciones mecánicas seguidas → delegar el lote.
- **Regla de edición masiva:** >20 edits semánticos en el mismo archivo → **script Python con
  diccionario de reemplazos y backup**, no agentes con Edit.
- Agentes propios del proyecto (p. ej. `.claude/agents/`) tienen prioridad sobre los genéricos
  para su dominio.
- Si un subagente devuelve resultado incompleto, **continúa en silencio** con herramientas
  directas; no narres el fallo interno.
- Los reportes de subagentes **no son evidencia**: verifica el artefacto real antes de darlos
  por buenos.

---

## 9. Fase 6 — Cierre: commit y push

1. **Alcance del commit (crítico si `REPO_GIT` ≠ `RAIZ`):**
   `git -C REPO_GIT add -- PATHSPEC_COMMIT`. **Prohibido `git add -A` / `git add .`**: el repo
   contiene proyectos hermanos con cambios ajenos que no deben viajar en este commit.
2. Revisa `git -C REPO_GIT diff --cached --stat -- PATHSPEC_COMMIT` antes de commitear.
3. Commits **atómicos por frente**, siguiendo el estilo del historial:
   `tipo(PROYECTO): descripción en IDIOMA` (`feat` / `fix` / `docs` / `refactor` / `test` / `chore`).
4. Push a `RAMA`. Si el push falla (auth, rechazo, upstream ausente), **repórtalo textualmente**
   y no lo des por hecho.
5. **Nada de secretos** en el commit: revisa el diff en busca de tokens, `.env`, credenciales
   o datos personales de estudiantes/docentes.

---

## 10. Fase 7 — Preparar el retorno (antes de `/exit`)

Reescribe `RAIZ/ARCHIVO_ESTADO` **completo** con esta plantilla:

```markdown
# 00 — RETOMAR (última actualización: AAAA-MM-DD)

**Disparador:** "FRASE_DISPARADORA"

## PREGUNTA_AL_RETOMAR
<La única pregunta cuya respuesta cambia lo que hago primero. Si no hay, escribir "ninguna".>

## Estado en una línea
<Dónde está el proyecto hoy.>

## Objetivos
- General: …
- OE1 … (criterio: `comando`) — estado
- OE2 … (criterio: `comando`) — estado

## Hecho en la última sesión (con commits)
- <cambio> — `sha`

## Siguiente paso concreto
1. <acción exacta, ejecutable, sin ambigüedad>

## Bloqueado / pendiente de decisión
- <qué, por qué, quién decide>

## Cómo verificar que todo sigue sano
```bash
cd RAIZ && CMD_VALIDATE && python scripts/check_doc_links.py
```

## Línea base de métricas
| Métrica | Antes | Después |
|---|---|---|
```

Además:
- Actualiza el archivo `MEMORIA_SLUG` de la memoria persistente y su línea en `MEMORY.md`,
  incluyendo el disparador y la ruta de `ARCHIVO_ESTADO`.
- Commitea también este archivo de estado (mismo pathspec).

---

## 11. Definition of Done

- [ ] G1 — Objetivos con criterio ejecutable, persistidos en repo **y** memoria
- [ ] G2 — Línea base de métricas medida y escrita **antes** de modificar
- [ ] G3 — `/code-review high --fix` aplicado; hallazgos altos cerrados o en BACKLOG
- [ ] G4 — `CMD_VALIDATE` verde con salida real + fallo inyectado detectado por la suite
- [ ] G5 — 11 frentes verificados; `GATE_ENLACES` sin enlaces rotos
- [ ] G6 — Commits atómicos con pathspec correcto + push confirmado
- [ ] G7 — `ARCHIVO_ESTADO` y memoria actualizados; retomar cuesta una frase
- [ ] Reporte final: qué cambió, métricas antes/después, qué quedó fuera y por qué

---

## 12. Anti-patrones (rechazo automático)

- ❌ Declarar «listo» sin haber ejecutado el comando que lo prueba
- ❌ Mockear, `skip`ear o relajar asserts para poner la suite en verde
- ❌ `git add -A` en un repo con proyectos hermanos
- ❌ Marcar una fase del ROADMAP como hecha sin commit que la respalde
- ❌ Documentar la intención en vez del estado real del código
- ❌ Reescribir desde cero lo que solo necesitaba refactor
- ❌ Citar rutas, comandos o agentes en `.claude/` sin verificar que existen
- ❌ Responder «el agente no devolvió datos completos» en vez de resolverlo en silencio
- ❌ Optimizar sin haber medido antes
- ❌ Silenciar recortes de alcance: si algo quedó fuera, se dice

---

## 13. Reporte final (formato de salida)

```
✅ CICLO COMPLETO — PROYECTO

Objetivos:      <general + n OEs, cuántos con cobertura>\
Baseline→Final: tests X→Y · cobertura A%→B% · módulos M→N · lint E→0\
Code review:    <n> hallazgos, <n> corregidos, <n> a BACKLOG\
E2E agresivo:   <n> escenarios, <n> bugs encontrados, <n> tests de regresión nuevos\
Docs:           README · SYLLABUS · ROADMAP · BACKLOG · BLUEPRINT · .claude/ — enlaces ✔\
Commits:        <shas>  ·  Push: <ok/fallo con salida>\
Fuera de alcance: <qué y por qué>\
Para retomar:   "FRASE_DISPARADORA"
```
