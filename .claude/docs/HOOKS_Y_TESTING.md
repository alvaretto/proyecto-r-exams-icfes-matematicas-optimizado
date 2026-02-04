# Sistema de Hooks y Testing Automático

## 🎯 Resumen Ejecutivo

**Sistema de validación automática PERMANENTE** con tolerancia cero a regresiones.

- **4 hooks activos** (PreToolUse/PostToolUse para Edit/Write/Bash)
- **100% cobertura** de tests (6 suites, 33+ tests unitarios)
- **CI/CD automático** con GitHub Actions
- **Bloqueo proactivo** de cambios que rompen tests
- **Validación matemática + visual** automática después de renderizar

---

## 📋 Índice

1. [Los 4 Hooks Configurados](#los-4-hooks-configurados)
2. [Sistema de Testing (100% Cobertura)](#sistema-de-testing-100-cobertura)
3. [Flujo Automático Completo](#flujo-automático-completo)
4. [Garantías del Sistema](#garantías-del-sistema)
5. [CI/CD y Monitoreo](#cicd-y-monitoreo)

---

## Los 4 Hooks Configurados

**Configuración**: @.claude/settings.json

### 1. PreToolUse - Edit/Write

**Hook**: `.claude/hooks/pre-edit-testing.sh`
**Timeout**: 60 segundos
**Status**: "Verificando tests antes de editar..."

#### Cuándo se Ejecuta
Antes de que Claude ejecute herramientas `Edit` o `Write`

#### Qué Hace
1. Extrae ruta del archivo desde `$ARGS_FILE`
2. Detecta si es componente crítico:
   - `.claude/scripts/*`
   - `.claude/hooks/*`
   - `.claude/rules/*`
   - `tests/*`

3. **Si ES crítico**:
   - Ejecuta tests relevantes ANTES de permitir edición
   - Si tests **PASAN** → Permite edición ✅
   - Si tests **FALLAN** → **BLOQUEA** edición ❌

4. **Si NO es crítico**:
   - Permite edición sin tests previos

#### Archivos Específicos

| Archivo | Tests Ejecutados |
|---------|------------------|
| `validar_coherencia_matematica.R` | `test_validacion_matematica.R` |
| `corregir_ortografia_espanol.R` | `test_ortografia_espanol.R` |
| `test_*.R` | Suite completa |
| Otros en `.claude/*` | `test_regression_suite.R` |

#### Ejemplo de Bloqueo

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

---

### 2. PostToolUse - Edit/Write

**Hook**: `.claude/hooks/post-edit-testing.sh`
**Timeout**: 60 segundos
**Status**: "Validando cambios..."

#### Cuándo se Ejecuta
Inmediatamente después de que Claude ejecuta `Edit` o `Write`

#### Qué Hace
1. Extrae ruta del archivo modificado
2. Detecta tipo de archivo:
   - `validar_coherencia_matematica.R` → Tests de validación
   - `corregir_ortografia_espanol.R` → Tests de ortografía
   - `*.Rmd` → Tests de renderizado
   - `test_*.R` → Suite completa
   - Configuración Claude → Tests de regresión

3. Ejecuta tests correspondientes
4. Si **FALLAN**:
   - Muestra error detallado
   - Indica que se debe corregir
   - **NO revierte** cambio (responsabilidad de Claude)

5. Si **PASAN**:
   - Confirma validación exitosa ✅
   - Permite continuar

#### Ejemplo de Error

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

---

### 3. PreToolUse - Bash (git commit/push)

**Hook**: `.claude/hooks/pre-bash-testing.sh`
**Timeout**: 120 segundos
**Status**: "Validando comando bash..."

#### Cuándo se Ejecuta
Antes de que Claude ejecute comando `Bash`

#### Detecta y Bloquea

##### A. git commit (excepto --amend)

**Comando detectado**:
```bash
git commit -m "mensaje"
```

**Acciones**:
1. Ejecuta suite completa:
   ```bash
   Rscript tests/run_all_tests.R
   ```

2. Si **PASA** → Permite commit ✅
   ```
   ✅ ==============================================
   ✅ TESTS PASARON - COMMIT PERMITIDO
   ✅ ==============================================
   ```

3. Si **FALLA** → **RECHAZA** commit ❌
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

##### B. git push

**Comando detectado**:
```bash
git push origin main
```

**Acciones**:
1. Verifica que no hay cambios sin commit
   ```bash
   git diff-index --quiet HEAD --
   ```

2. Ejecuta suite completa:
   ```bash
   Rscript tests/run_all_tests.R
   ```

3. Verifica que CI/CD existe:
   ```bash
   [ -f ".github/workflows/ci-testing.yml" ]
   ```

4. Si **TODO PASA** → Permite push ✅
5. Si **ALGO FALLA** → **RECHAZA** push ❌

---

### 4. PostToolUse - Bash (exams2*)

**Hook**: `.claude/hooks/post-exams2-validation.sh`
**Timeout**: 120 segundos
**Status**: "Validando matemática + preview visual..."

#### Cuándo se Ejecuta
Después de cualquier comando `Bash` exitoso

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
- `exshuffle = TRUE`
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

---

## Sistema de Testing (100% Cobertura)

**Documentación completa**: @.claude/docs/ECOSISTEMA_TESTING.md
**Regla obligatoria**: @.claude/rules/testing-obligatorio.md

### Las 6 Suites de Tests

| Suite | Archivo | Tests | Cubre |
|-------|---------|-------|-------|
| **Validación Matemática** | `test_validacion_matematica.R` | 5 | Script validación, metadatos ICFES, coherencia |
| **Ortografía Española** | `test_ortografia_espanol.R` | 5 | Tildes, metadatos ASCII, variables |
| **Renderizado 4 Formatos** | `test_renderizado_4_formatos.R` | 6 | HTML, PDF, DOCX, NOPS |
| **Aleatorización** | `test_aleatorization_diversity.R` | 4 | exshuffle, 250+ versiones, rangos |
| **Flujo B Graficador** | `test_flujo_b_graficador.R` | 6 | workflow_state.json, aprobaciones, similitud |
| **Regresión** | `test_regression_suite.R` | 7 | Ejemplos funcionales, scripts, hooks |
| **TOTAL** | 6 archivos | **33+** | **100%** |

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
✓ test_validacion_matematica.R [5 tests, ~3.2s]

Ejecutando suite: Ortografía Española
✓ test_ortografia_espanol.R [5 tests, ~2.1s]

...

========================================
  RESUMEN FINAL
========================================

Suites ejecutadas: 6
✓ Exitosas: 6
✗ Fallidas: 0

Tests totales: 33
✓ Pasados: 33
✗ Fallidos: 0

⏱  Tiempo total: ~28.4s

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

EVENTO 1: Claude intenta Edit/Write
├─→ PreToolUse Hook → pre-edit-testing.sh
│   ├─→ ¿Es componente crítico?
│   │   ├─→ SÍ: Ejecutar tests relevantes
│   │   │   ├─→ PASAN: Permitir edición ✅
│   │   │   └─→ FALLAN: BLOQUEAR edición ❌
│   │   └─→ NO: Permitir edición
│   │
├─→ Claude ejecuta Edit/Write
│   │
└─→ PostToolUse Hook → post-edit-testing.sh
    ├─→ Ejecutar tests según tipo de archivo
    ├─→ PASAN: ✅ Cambio validado
    └─→ FALLAN: ❌ Mostrar error + Instrucciones

EVENTO 2: Usuario/Claude intenta git commit
├─→ PreToolUse Hook (Bash) → pre-bash-testing.sh
│   ├─→ Ejecutar: Rscript tests/run_all_tests.R
│   ├─→ PASAN: Permitir commit ✅
│   └─→ FALLAN: RECHAZAR commit ❌

EVENTO 3: Usuario/Claude intenta git push
├─→ PreToolUse Hook (Bash) → pre-bash-testing.sh
│   ├─→ Verificar no hay cambios sin commit
│   ├─→ Ejecutar suite completa
│   ├─→ Verificar CI/CD configurado
│   ├─→ PASAN: Permitir push ✅
│   └─→ FALLAN: RECHAZAR push ❌

EVENTO 4: Después de exams2*()
└─→ PostToolUse Hook (Bash) → post-exams2-validation.sh
    ├─→ FASE 2A: Validar matemática (script R)
    └─→ FASE 2B: Generar preview PNG (magick)
        └─→ Claude DEBE leer PNG + verificar + aprobar
```

---

### Escenarios Detallados

#### Escenario 1: Claude Edita Script de Validación

```
1. Usuario: "Actualiza validar_coherencia_matematica.R"

2. Claude detecta → Edit("validar_coherencia_matematica.R")

3. PreToolUse Hook ejecuta:
   → pre-edit-testing.sh
   → Detecta: archivo crítico
   → Ejecuta: test_validacion_matematica.R
   → Estado actual: PASANDO ✅

4. Claude procede con Edit

5. PostToolUse Hook ejecuta:
   → post-edit-testing.sh
   → Ejecuta: test_validacion_matematica.R con NUEVO código

6. Resultado:
   - Si PASA: ✅ "Tests pasaron después del cambio"
   - Si FALLA: ❌ "Tests fallaron - Corregir código"
```

#### Escenario 2: Usuario Hace Commit

```
1. Usuario: git commit -m "feat: nueva funcionalidad"

2. PreToolUse Hook (Bash) ejecuta:
   → pre-bash-testing.sh
   → Detecta: comando git commit
   → Ejecuta: Rscript tests/run_all_tests.R
   → Suite completa: 6 suites, 33+ tests

3. Resultado:
   - Si TODO PASA:
     ✅ "TESTS PASARON - COMMIT PERMITIDO"
     → Commit se ejecuta

   - Si ALGO FALLA:
     ❌ "COMMIT RECHAZADO - TESTS FALLARON"
     → Commit NO se ejecuta
     → Mostrar errores + instrucciones
```

#### Escenario 3: Claude Genera .Rmd y lo Renderiza

```
1. Claude: Write("ejercicio.Rmd")

2. PostToolUse Hook (Write):
   → post-edit-testing.sh
   → Ejecuta: test_renderizado_4_formatos.R
   → Valida estructura básica

3. Claude: Bash("Rscript -e 'exams2pdf(\"ejercicio.Rmd\")'")

4. PostToolUse Hook (Bash):
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

5. Claude ejecuta automáticamente:
   → Read("preview.png")
   → Muestra imagen al usuario
   → Verifica 5 coherencias
   → Solicita aprobación
```

---

## Garantías del Sistema

Con este sistema permanentemente activo:

✅ **1. Ningún cambio rompedor llega a código**
- Tests se ejecutan ANTES y DESPUÉS de editar

✅ **2. 100% de cobertura se mantiene**
- Tests de regresión validan que nada se rompa

✅ **3. Commits solo con tests pasando**
- Imposible hacer commit con tests fallidos

✅ **4. Push solo con validación completa**
- Suite completa se ejecuta antes de push

✅ **5. Validación automática de .Rmd**
- FASE 2A (matemática) + FASE 2B (preview) automáticas

✅ **6. Claude no puede romper el sistema**
- Hooks bloquean cambios peligrosos

✅ **7. CI/CD adicional en remoto**
- GitHub Actions ejecuta tests en cada push/PR

---

## CI/CD y Monitoreo

### GitHub Actions

**Archivo**: `.github/workflows/ci-testing.yml`

**Triggers**:
- Push a `main` o `develop`
- Pull Requests
- Daily cron: 02:00 UTC

**Jobs** (7 paralelos):
1. Validación matemática
2. Ortografía española
3. Renderizado 4 formatos
4. Aleatorización
5. Flujo B graficador
6. Regresión
7. Suite completa

**Configuración**:
```yaml
strategy:
  fail-fast: true  # Abortar si algún job falla
  matrix:
    suite:
      - validacion_matematica
      - ortografia_espanol
      - renderizado_4_formatos
      - aleatorization_diversity
      - flujo_b_graficador
      - regression_suite
      - completa
```

**Política**: Tolerancia cero → Pipeline falla si cualquier job falla

---

### Métricas y Reportes

Cada hook reporta su estado en tiempo real:

```
🔍 Verificando tests antes de editar...
→ Ejecutando tests de validación matemática...
✅ Tests pasaron - Procediendo con edición

🔍 Validando cambios en: .claude/scripts/validar_coherencia_matematica.R
→ Script de validación matemática modificado
→ Ejecutando tests de validación matemática...
✅ Todos los tests pasaron después del cambio

🔒 BLOQUEO PRE-COMMIT ACTIVADO
Ejecutando suite completa de tests...

========================================
  SUITE DE TESTING COMPLETA
========================================

Suites ejecutadas: 6
✓ Exitosas: 6
✗ Fallidas: 0

✅ TESTS PASARON - COMMIT PERMITIDO
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

# 4. Intentar commit de prueba (debería bloquear si hay tests fallidos)
git commit -m "test" --allow-empty
```

---

## 🚨 Mensajes de Error y Acciones

### Error en PreToolUse (Edit)

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

1. Editar archivo de hook (ej. `pre-edit-testing.sh`)
2. Los tests de regresión validan automáticamente
3. Commit y push con sistema automático validando

### Agregar Nuevos Tests

1. Crear `test_nuevo_componente.R`
2. Sistema detecta que es archivo crítico (`test_*.R`)
3. Pre-edit ejecuta suite completa
4. Post-edit valida que el nuevo test funciona
5. Commit solo permitido si todo pasa

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

**Versión**: 1.0
**Fecha**: 2026-02-04
**Estado**: ACTIVO Y PERMANENTE
**Módulo de**: @.claude/CLAUDE.md (v3.0.0)
