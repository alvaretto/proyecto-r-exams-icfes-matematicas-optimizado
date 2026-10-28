# Sistema de Hooks y Testing Automático

## 🎯 Resumen Ejecutivo

**Sistema de validación automática PERMANENTE** con tolerancia cero a regresiones.

- **Hooks ICFES** cargados por `settings.json`: gate `.Rmd` + recordatorio de tildes (PreToolUse), arsenal post-render (PostToolUse Bash) y citas del detractor (SubagentStop, regla #9); los demás son de Ruflo
- **100% cobertura** de tests (suites en `tests/run_all_tests.R`; tabla y cifras en `.claude/docs/VALIDATOR_COBERTURA_ICFES.md`, sección «Suites de Test»)
- **CI/CD automático** con GitHub Actions (un job llama a `run_all_tests.R`)
- **Bloqueo proactivo** de `.Rmd` sin `ejercicio_state.json` vía gate mecánico
- **Validación matemática + arsenal + preview visual** automática después de renderizar

> Nota (v3.8.0, 2026-04-10): los hooks `pre-edit-testing.sh` y `post-edit-testing.sh`
> fueron eliminados (nunca estuvieron cargados en `settings.json`) y `pre-bash-testing.sh`
> también se retiró. Los git hooks nativos (`.git/hooks/pre-commit`,
> `.git/hooks/pre-push`) siguen siendo los responsables de bloquear el commit
> (ortografía de los `.Rmd` en stage; tests solo con `PRECOMMIT_TESTS=1`) y el push
> (suite en modo rápido, `R_TESTS_QUICK=1`) — ver
> `.claude/rules/testing-obligatorio.md`.

---

## 📋 Índice

1. [Hooks ICFES Configurados](#hooks-icfes-configurados)
2. [Sistema de Testing (100% Cobertura)](#sistema-de-testing-100-cobertura)
3. [Flujo Automático Completo](#flujo-automático-completo)
4. [Garantías del Sistema](#garantías-del-sistema)
5. [CI/CD y Monitoreo](#cicd-y-monitoreo)

---

## Hooks ICFES Configurados

**Configuración**: @.claude/settings.json

> Los hooks ICFES que Claude Code carga desde `settings.json` son los dos
> descritos abajo (PreToolUse: gate + recordatorio de tildes; PostToolUse Bash:
> arsenal post-render) y un SubagentStop que verifica las citas del detractor
> (`.claude/scripts/verificar_citas_detractor.py`, regla #9). Los demás son de Ruflo
> (`.claude/helpers/hook-handler.cjs`, `auto-memory-hook.mjs`). Commits/push se
> protegen con git hooks nativos en `.git/hooks/pre-commit` y
> `.git/hooks/pre-push` (ver `.claude/rules/testing-obligatorio.md`).

### 1. PreToolUse - Write/Edit: Gate de .Rmd + recordatorio de tildes

**Hook principal**: `.claude/hooks/pre-write-rmd-gate.sh`
**Recordatorio**: mensaje `echo` con lista de palabras que llevan tilde.

#### Cuándo se Ejecuta
Antes de que Claude ejecute `Write` o `Edit`.

#### Qué Hace
1. Si el archivo objetivo es un `.Rmd` en `01-En-PreDesarrollo/` o `02-En-Desarrollo/`,
   verifica que exista `ejercicio_state.json` en el directorio del ejercicio y
   que los pasos obligatorios (`analisis_icfes`, `flujo_b`) estén completos.
2. Si alguna verificación falla → **exit 2** (bloquea el tool call) con
   instrucciones concretas en stderr.
3. Si el archivo está fuera del workflow o en `Ejemplos-Funcionales/` → permite.
4. Fail open: si el JSON es inválido, permite (nunca bloquear por error del sistema).

Detalles completos en `.claude/rules/workflow-state-enforcement.md`.

---

### 2. PostToolUse - Bash (exams2*)

**Hook**: `.claude/hooks/post-exams2-validation.sh` (v6.2: 15 fases, de la 2A a la 2O)
**Timeout**: 300 segundos (`.claude/settings.json`). Si se excede, Claude Code descarta toda su salida

#### Cuándo se Ejecuta
Después de un comando `Bash` que termina con 0 (evento PostToolUse). Si el comando
falla, Claude Code dispara PostToolUseFailure y el hook no corre. El hook corre
después del render: no lo impide ni lo deshace. Si no encuentra el `.Rmd` desde el
directorio de la sesión, avisa al modelo de que el arsenal no corrió.

#### Detecta
Comandos `exams2pdf()`, `exams2html()`, `exams2pandoc()`, `exams2nops()`

#### Acciones (si detecta exams2*)

##### FASE 2A - Validación Matemática Automática

```bash
Rscript .claude/scripts/validar_coherencia_matematica.R [archivo.Rmd]
```

**Valida**:
- Chunks R sin errores (NA/NaN/Inf)
- Metadatos ICFES completos
- `exshuffle = TRUE` (excepción: acepta FALSE en SCHOICE con opciones gráficas PNG)
- SCHOICE: `exsolution` binario, exactamente 1 correcta
- CLOZE: tipos/soluciones/tolerancias consistentes
- Coherencia matemática entre variables

**Reporta**: APROBADO / ERRORES

##### FASE 2B - Preview Visual Automático

```bash
# 1. Busca PDF generado
find output_pdf/ output/ -name "*.pdf" -type f

# 2. Convierte a PNG
magick -density 150 [pdf] -quality 90 preview_[nombre].png

# 3. Soporta múltiples páginas
preview_nombre-0.png
preview_nombre-1.png
...
```

**Reporta**:
- Rutas de PNGs generados
- **Emite instrucción OBLIGATORIA para Claude**:
  ```
  Claude DEBE:
  1. Ejecutar Read() sobre cada PNG reportado
  2. Verificar las 5 coherencias VISUALMENTE
  3. Documentar hallazgos con checklist
  4. Solicitar aprobación del usuario
  ```

##### FASES 2C-2O y entrega del informe al modelo

Las fases 2C a 2O (arsenal, multi-semilla con N = 100, stress test visual con 10
semillas y detectores estáticos) se describen en
`.claude/docs/VALIDATOR_COBERTURA_ICFES.md`, sección «El Hook post-exams2». La 2G y
la 2H solo corren si no hubo errores antes.

**Cómo llega el informe al modelo (v6.2)**: el hook escribe en stdout un JSON con
`additionalContext` (el informe, hasta 9 000 caracteres). Si hay errores añade
`decision: "block"` y `reason`, que Claude Code muestra como «hook blocking error», y
siempre un `systemMessage` de una línea para el usuario. El informe completo queda en
`~/.cache/post-exams2-validation/<nombre>.txt`. Hasta la v6.1 el informe no llegaba:
el texto plano de un PostToolUse con exit 0 va al log de depuración. Por eso la
instrucción «Claude DEBE…» solo le llega al modelo desde la v6.2.

---

## Sistema de Testing (100% Cobertura)

**Documentación completa**: @.claude/docs/ECOSISTEMA_TESTING.md
**Regla obligatoria**: @.claude/rules/testing-obligatorio.md

### Las Suites de Tests

La tabla de suites (archivo y qué cubre cada una) y sus cifras viven en
`.claude/docs/VALIDATOR_COBERTURA_ICFES.md`, sección «Suites de Test». La genera
`tests/sincronizar_doc_suites.R` y un test falla si diverge del runner, así que no
se copia aquí.

> El runner (`run_all_tests.R`) marca como pesadas las suites con campo
> `watch` y las salta en modo `R_TESTS_QUICK=1` cuando ningún archivo
> cambiado coincide con los patrones de la suite. Cuáles son hoy: sección
> «Suites de Test» del mismo documento.

### Ejecutor Principal

**Archivo**: `tests/run_all_tests.R` (chmod +x)

**Uso**:
```bash
Rscript tests/run_all_tests.R
```

**Output**:
```
========================================
  SUITE DE TESTING COMPLETA
========================================

Ejecutando suite: Validación Matemática
✓ test_validacion_matematica.R [N tests, ~Ns]

Ejecutando suite: Ortografía Española
✓ test_ortografia_espanol.R [N tests, ~Ns]

...

========================================
  RESUMEN FINAL
========================================

Suites ejecutadas: N
✓ Exitosas: N
✗ Fallidas: 0

Tests totales: N
✓ Pasados: N
✗ Fallidos: 0

⏱  Tiempo total: ~Ns

========================================
✅ TODOS LOS TESTS PASARON
========================================
```

**Exit codes**:
- `0` → Todos los tests pasaron
- `1` → Algún test falló

---

## Flujo Automático Completo

**Documentación detallada**: @.claude/docs/FLUJO_AUTOMATICO_TESTING.md

### Diagrama de Flujo

```
┌─────────────────────────────────────────────────────────┐
│               SISTEMA AUTOMÁTICO DE TESTING              │
│                   (SIEMPRE ACTIVO)                       │
└─────────────────────────────────────────────────────────┘

EVENTO 1: Claude intenta Write/Edit sobre un .Rmd
└─→ PreToolUse Hook → pre-write-rmd-gate.sh
    ├─→ ¿.Rmd en 01-PreDesarrollo/ o 02-Desarrollo/?
    │   ├─→ SÍ: Verificar ejercicio_state.json + pasos 1-2
    │   │   ├─→ OK: Permitir ✅
    │   │   └─→ FALTA: exit 2 → BLOQUEAR ❌
    │   └─→ NO: Permitir (fuera del workflow)

EVENTO 2: Usuario/Claude intenta git commit
└─→ .git/hooks/pre-commit (git hook nativo, NO Claude hook)
    ├─→ Valida ortografía .Rmd modificados
    └─→ (Opcional) Ejecuta tests con PRECOMMIT_TESTS=1

EVENTO 3: Usuario/Claude intenta git push
└─→ .git/hooks/pre-push (git hook nativo; delega en .claude/hooks/pre-push.sh)
    └─→ Ejecuta SIEMPRE R_TESTS_QUICK=1 Rscript tests/run_all_tests.R (modo rápido)

EVENTO 4: Después de exams2*() (solo si el comando Bash termina con 0)
└─→ PostToolUse Hook (Bash) → post-exams2-validation.sh (v6.2, 15 fases: 2A a 2O)
    ├─→ FASE 2A: Validar matemática (script R, Niveles 1-4)
    ├─→ FASE 2B: Generar preview PNG (magick)
    │   └─→ Claude DEBE leer PNG + verificar + aprobar
    ├─→ FASES 2C-2F: Arsenal completo de validación
    ├─→ FASE 2G: Multi-semilla (100 semillas, Nivel 5; regla #23)
    │   └─→ Solo si las fases anteriores no dieron errores
    ├─→ FASE 2H: Stress test visual (10 semillas, render real)
    │   └─→ Solo si las fases anteriores no dieron errores
    ├─→ FASES 2I-2O: detectores estáticos y recordatorios
    └─→ Informe al modelo: JSON con additionalContext
        (más decision: block si hay errores)
```

---

### Escenarios Detallados

#### Escenario 1: Claude crea un ejercicio .Rmd nuevo

```
1. Usuario: "Crea ejercicio de mediana"

2. Claude ejecuta pasos 1-2 (/analizar-icfes + preguntar flujo_b) y
   marca ambos en ejercicio_state.json con workflow-state.sh complete.

3. Claude: Write("mi-ejercicio.Rmd")

4. PreToolUse Hook:
   → pre-write-rmd-gate.sh
   → Verifica ejercicio_state.json + pasos 1-2 completos
   → Permite ✅

5. Claude: Bash("Rscript -e 'exams2pdf(\"mi-ejercicio.Rmd\")'")

6. PostToolUse Hook:
   → post-exams2-validation.sh
   → FASES 2A-2O automáticas (15 fases; ver VALIDATOR_COBERTURA_ICFES.md)
```

#### Escenario 2: Usuario Hace Commit

```
1. Usuario: git commit -m "feat: nueva funcionalidad"

2. .git/hooks/pre-commit (git hook nativo):
   → Valida ortografía en .Rmd modificados
   → Opcional: tests con PRECOMMIT_TESTS=1

3. Resultado:
   - PASA: commit ejecutado
   - FALLA: commit rechazado, corregir errores
```

#### Escenario 3: Claude Genera .Rmd y lo Renderiza

```
1. Claude: Write("ejercicio.Rmd")  → gate permite si pasos previos OK

2. Claude: Bash("Rscript -e 'exams2pdf(\"ejercicio.Rmd\")'")

3. PostToolUse Hook (Bash):
   → post-exams2-validation.sh
   → Detecta: exams2pdf

   FASE 2A:
   → Ejecuta validar_coherencia_matematica.R
   → Valida metadatos, chunks, coherencia
   → Reporta: APROBADO / ERRORES

   FASE 2B:
   → Busca PDF: output_pdf/plain1.pdf
   → Convierte: magick → preview.png
   → Reporta: "PNG generado en [ruta]"
   → Emite: "Claude DEBE leer PNG y verificar 5 coherencias"

   FASES 2C-2O:
   → Arsenal, multi-semilla (N = 100), stress test visual (10 semillas)
     y detectores estáticos

4. Claude ejecuta automáticamente:
   → Read("preview.png")
   → Muestra imagen al usuario
   → Verifica 5 coherencias
   → Solicita aprobación
```

---

## Garantías del Sistema

Con este sistema permanentemente activo:

✅ **1. Ningún cambio rompedor llega a código**
- El gate de `.Rmd` frena la escritura sin workflow; el pre-push corre la suite en modo rápido y el CI la suite completa

✅ **2. 100% de cobertura se mantiene**
- Tests de regresión validan que nada se rompa

✅ **3. Commits con ortografía validada**
- El pre-commit rechaza `.Rmd` con errores ortográficos; los tests corren solo con `PRECOMMIT_TESTS=1`

✅ **4. Push solo con la suite en modo rápido pasando**
- El pre-push corre `R_TESTS_QUICK=1 Rscript tests/run_all_tests.R`; la suite completa (`R_TESTS_FULL=1`) es manual

✅ **5. Validación automática de .Rmd**
- FASE 2A (matemática) + FASE 2B (preview) automáticas

✅ **6. Claude no puede romper el sistema**
- Hooks bloquean cambios peligrosos

✅ **7. CI/CD adicional en remoto**
- GitHub Actions ejecuta la suite completa en push y PR a `main` o `develop`, y a diario (02:00 UTC)

---

## CI/CD y Monitoreo

### GitHub Actions

**Archivo**: `.github/workflows/ci-testing.yml`

**Triggers**:
- Push a `main` o `develop`
- Pull requests a `main` o `develop`
- Daily cron: 02:00 UTC

**Job** (uno solo, `tests-full`):
1. Instala R y las dependencias del sistema y de R
2. Ejecuta la suite completa: `Rscript tests/run_all_tests.R`
3. Sube los resultados como artefacto

**Configuración** (extracto de `ci-testing.yml`):
```yaml
jobs:
  tests-full:
    runs-on: ubuntu-latest
    name: Suite completa (run_all_tests.R)
    steps:
      # ... checkout, setup R, dependencias
      - name: Run full test suite
        run: Rscript tests/run_all_tests.R
```

**Política**: Tolerancia cero → el pipeline falla si el job falla

---

### Métricas y Reportes

Cada hook reporta su estado en tiempo real.

> ⚠️ RETIRADO (v3.8.0): este hook no existe en .claude/hooks/ ni lo carga .claude/settings.json; se conserva como historia.

Informes de `pre-edit-testing.sh` y `post-edit-testing.sh`:

```
🔍 Verificando tests antes de editar...
→ Ejecutando tests de validación matemática...
✅ Tests pasaron - Procediendo con edición

🔍 Validando cambios en: .claude/scripts/validar_coherencia_matematica.R
→ Script de validación matemática modificado
→ Ejecutando tests de validación matemática...
✅ Todos los tests pasaron después del cambio
```

Informe vigente del git hook `.git/hooks/pre-commit` con `PRECOMMIT_TESTS=1`. Sin esa
variable imprime «Tests omitidos (activar con PRECOMMIT_TESTS=1)» y solo revisa la
ortografía:

```
═══════════════════════════════════════
  PRE-COMMIT: Validación Obligatoria
═══════════════════════════════════════

📝 Validando ortografía en archivos .Rmd...
   ✓ ejercicio.Rmd

🧪 Ejecutando tests de regresión...

...

Suites ejecutadas: N
✓ Exitosas: N
✗ Fallidas: 0

✅ PRE-COMMIT: Validación exitosa
```

---

### Variables de Entorno Disponibles

Los hooks tienen acceso a:

| Variable | Descripción | Ejemplo |
|----------|-------------|---------|
| `$CLAUDE_PROJECT_DIR` | Directorio raíz del proyecto | `/home/bootcamp/.../RepositorioMatematicasICFES_R_Exams` |
| `$ARGS_FILE` | Archivo JSON con argumentos del tool | Contiene `file_path` para Edit/Write |
| `$BASH_COMMAND` | Comando bash siendo ejecutado | `git commit -m "mensaje"` |
| `$FILE_PATH` | Ruta del archivo (extraída de $ARGS_FILE) | `.claude/scripts/validar_coherencia_matematica.R` |

---

## Verificación del Sistema

Para verificar que el sistema está activo:

```bash
# 1. Verificar que hooks existen y tienen permisos
ls -la .claude/hooks/*.sh

# 2. Verificar configuración en settings.json
cat .claude/settings.json | grep -A 20 "hooks"

# 3. Probar manualmente suite completa
Rscript tests/run_all_tests.R

# 4. Intentar commit de prueba (el pre-commit revisa la ortografía de los .Rmd en stage;
#    corre tests solo con PRECOMMIT_TESTS=1)
git commit -m "test" --allow-empty
```

---

## 🚨 Mensajes de Error y Acciones

### Error en PreToolUse (Edit)

> ⚠️ RETIRADO (v3.8.0): este hook no existe en .claude/hooks/ ni lo carga .claude/settings.json; se conserva como historia.

```
❌ ==============================================
❌ EDICIÓN BLOQUEADA - TESTS FALLARON
❌ ==============================================

Archivo: .claude/scripts/validar_coherencia_matematica.R

ACCIÓN REQUERIDA:
1. Los tests actuales están fallando
2. Corregir problemas antes de editar
3. NO proceder con edición hasta que tests pasen
```

**Claude debe**:
- NO intentar editar el archivo
- Informar al usuario del problema
- Esperar instrucciones

---

### Error en PostToolUse (Edit)

> ⚠️ RETIRADO (v3.8.0): este hook no existe en .claude/hooks/ ni lo carga .claude/settings.json; se conserva como historia.

```
❌ ==============================================
❌ TESTS FALLARON DESPUÉS DEL CAMBIO
❌ ==============================================

Archivo: .claude/scripts/validar_coherencia_matematica.R

⚠️ ACCIÓN REQUERIDA:
1. El cambio introdujo un error
2. Revisar el código modificado
3. Corregir el problema
4. Los tests deben pasar antes de commit
```

**Claude debe**:
- Analizar el error reportado
- Identificar qué rompió
- Proponer corrección
- Aplicar corrección
- Verificar que tests pasen

---

### Error en PreBash (Commit)

> ⚠️ RETIRADO (v3.8.0): este hook no existe en .claude/hooks/ ni lo carga .claude/settings.json; se conserva como historia. Hoy el rechazo lo hace el git hook `.git/hooks/pre-commit` (ortografía; tests solo con `PRECOMMIT_TESTS=1`).

```
❌ ==============================================
❌ COMMIT RECHAZADO - TESTS FALLARON
❌ ==============================================

Acciones requeridas:
1. Revisar errores de tests arriba ↑
2. Corregir el código que causó la falla
3. Volver a ejecutar: Rscript tests/run_all_tests.R
4. Solo entonces hacer commit

⚠️ PROHIBIDO usar: git commit --no-verify
```

**Claude debe**:
- NO intentar commit con --no-verify
- Informar al usuario qué tests fallaron
- Ayudar a corregir el código
- Esperar a que tests pasen

---

## 🛡️ Mantenimiento del Sistema

### Actualizar Hooks

1. Editar `pre-write-rmd-gate.sh` o `post-exams2-validation.sh`
2. Los tests de regresión y la suite completa validan los cambios en CI
3. Commit y push con git hooks nativos validando

### Agregar Nuevos Tests

1. Crear `tests/testthat/test_nuevo_componente.R`
2. Registrarlo en `tests/run_all_tests.R` (añadir entrada en `suites`)
3. Al próximo push, CI lo ejecuta automáticamente (el CI llama al runner)

### Deshabilitar Temporalmente (PROHIBIDO)

❌ **NO hay forma de deshabilitar el sistema**
❌ **NO usar `--no-verify`**
❌ **NO comentar hooks en settings.json**

✅ **SIEMPRE corregir el código para que pase los tests**

---

## 📚 Documentación Relacionada

- **Regla obligatoria**: @.claude/rules/testing-obligatorio.md
- **Flujo automático detallado**: @.claude/docs/FLUJO_AUTOMATICO_TESTING.md
- **Ecosistema completo**: @.claude/docs/ECOSISTEMA_TESTING.md
- **Ciclo validación**: @.claude/rules/ciclo-validacion.md
- **Configuración**: @.claude/settings.json

---

**Versión**: 1.2
**Fecha**: 2026-02-14
**Estado**: ACTIVO Y PERMANENTE
**Módulo de**: @.claude/CLAUDE.md (v3.4.0)

### Cambios v1.2 (2026-02-14)
- **10 suites** (antes 9): +Correctitud de Respuesta (Nivel 5)
- **82+ tests** (antes 68+): +14 tests Nivel 5A-5E
- **FASE 2G** agregada: Multi-semilla rápida (20 semillas) en hook post-exams2
- Validación Correctitud Nivel 5: cross-check respuesta, unicidad opciones, rangos matemáticos

### Cambios v1.1 (2026-02-13)
- **9 suites** (antes 6): +Distintividad Visual _neg_, +Media-Mediana-Moda, +Validación Semántica
- **68+ tests** (antes 33+)
- Validación Semántica Nivel 4: 3 capas + 21 keyword rules
