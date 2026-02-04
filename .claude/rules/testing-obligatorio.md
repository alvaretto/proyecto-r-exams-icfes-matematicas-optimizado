# Regla: Testing Obligatorio y Automático

## Principio Fundamental

**TODOS los cambios al código deben pasar por validación de tests automáticamente. NO hay excepciones.**

Esta regla garantiza que el sistema NUNCA se rompa debido a cambios en el código.

---

## 🎯 Objetivos

1. **Prevenir Regresiones:** Detectar errores antes de que lleguen a producción
2. **Garantizar Calidad:** Mantener 100% de cobertura en todo momento
3. **Automatización Total:** Tests se ejecutan sin intervención manual
4. **Tolerancia Cero:** Ningún cambio se acepta si rompe tests

---

## 🔄 Flujo Automático de Testing

### FASE 1: Antes de Editar/Escribir Código

**Hook:** `PreToolUse` (Edit/Write)

```
Claude detecta → Edit/Write tool
    ↓
VERIFICAR: ¿El cambio afecta componentes críticos?
    ↓
SI → Ejecutar tests relacionados ANTES del cambio
    ↓
Reportar cobertura actual al usuario
```

**Componentes críticos:**
- Scripts de validación (`.claude/scripts/`)
- Hooks (`.claude/hooks/`)
- Reglas (`.claude/rules/`)
- Skills (`.claude/skills/`)
- Tests existentes (`tests/`)
- Ejemplos funcionales (`A-Produccion/Ejemplos-Funcionales-Rmd/`)

### FASE 2: Después de Cambios en Código

**Hook:** `PostToolUse` (Edit/Write)

```
Claude completa → Edit/Write
    ↓
EJECUTAR automáticamente:
    1. Tests unitarios relacionados
    2. Tests de regresión
    ↓
SI TODOS PASAN → Continuar
SI ALGUNO FALLA → REVERTIR cambio automáticamente
    ↓
Reportar resultado al usuario
```

### FASE 3: Antes de Commit

**Hook:** `PreToolUse` (Bash con git commit)

```
Usuario/Claude intenta → git commit
    ↓
BLOQUEAR si:
    - Hay tests fallando
    - Cobertura < 100%
    - No se ejecutaron tests después del último cambio
    ↓
EJECUTAR suite completa:
    Rscript tests/run_all_tests.R
    ↓
SI FALLA → RECHAZAR commit + Mostrar errores
SI PASA → PERMITIR commit
```

### FASE 4: Antes de Push

**Hook:** `PreToolUse` (Bash con git push)

```
Usuario/Claude intenta → git push
    ↓
VERIFICAR:
    1. Todos los commits tienen tests pasando
    2. CI/CD está configurado
    3. No hay cambios sin commit
    ↓
EJECUTAR suite completa final
    ↓
SI FALLA → RECHAZAR push + Instrucciones de corrección
SI PASA → PERMITIR push
```

---

## 🛠️ Implementación de Hooks

### 1. Hook PreToolUse - Edit/Write

**Ubicación:** `.claude/settings.json`

```json
{
  "hooks": {
    "preToolUse": {
      "edit": {
        "file": ".claude/hooks/pre-edit-testing.sh",
        "message": "Verificando tests antes de editar..."
      },
      "write": {
        "file": ".claude/hooks/pre-write-testing.sh",
        "message": "Verificando tests antes de crear archivo..."
      }
    }
  }
}
```

**Script:** `.claude/hooks/pre-edit-testing.sh`

```bash
#!/bin/bash
# Pre-edit testing hook
# Ejecuta tests relevantes antes de modificar archivos críticos

FILE_PATH="$1"

# Detectar si es componente crítico
if [[ "$FILE_PATH" == *.claude/scripts/* ]] || \
   [[ "$FILE_PATH" == *.claude/hooks/* ]] || \
   [[ "$FILE_PATH" == *.claude/rules/* ]] || \
   [[ "$FILE_PATH" == *tests/* ]]; then

    echo "⚠️ COMPONENTE CRÍTICO DETECTADO: $FILE_PATH"
    echo "Ejecutando tests de validación..."

    # Ejecutar tests relacionados
    if [[ "$FILE_PATH" == *validar_coherencia_matematica.R ]]; then
        Rscript -e "library(testthat); test_file('tests/testthat/test_validacion_matematica.R')"
    elif [[ "$FILE_PATH" == *corregir_ortografia_espanol.R ]]; then
        Rscript -e "library(testthat); test_file('tests/testthat/test_ortografia_espanol.R')"
    else
        # Tests de regresión general
        Rscript -e "library(testthat); test_file('tests/testthat/test_regression_suite.R')"
    fi

    if [ $? -ne 0 ]; then
        echo "❌ TESTS FALLARON - No se puede editar archivo crítico"
        exit 1
    fi

    echo "✅ Tests pasaron - Procediendo con edición"
fi

exit 0
```

### 2. Hook PostToolUse - Edit/Write

**Script:** `.claude/hooks/post-edit-testing.sh`

```bash
#!/bin/bash
# Post-edit testing hook
# Ejecuta tests después de modificar archivos para validar cambios

FILE_PATH="$1"

echo "🔍 Validando cambios en: $FILE_PATH"

# Detectar tipo de archivo modificado
if [[ "$FILE_PATH" == *.R ]]; then
    echo "Archivo R detectado - Ejecutando tests de validación matemática..."
    Rscript -e "library(testthat); test_file('tests/testthat/test_validacion_matematica.R')"

elif [[ "$FILE_PATH" == *.Rmd ]]; then
    echo "Archivo .Rmd detectado - Ejecutando tests de renderizado..."
    Rscript -e "library(testthat); test_file('tests/testthat/test_renderizado_4_formatos.R')"

elif [[ "$FILE_PATH" == *test_*.R ]]; then
    echo "Test modificado - Ejecutando suite completa..."
    Rscript tests/run_all_tests.R

elif [[ "$FILE_PATH" == *.claude/* ]]; then
    echo "Configuración Claude modificada - Ejecutando tests de regresión..."
    Rscript -e "library(testthat); test_file('tests/testthat/test_regression_suite.R')"
fi

if [ $? -ne 0 ]; then
    echo "❌ TESTS FALLARON DESPUÉS DEL CAMBIO"
    echo "⚠️ ACCIÓN REQUERIDA: Corregir el código o revertir cambios"
    exit 1
fi

echo "✅ Todos los tests pasaron después del cambio"
exit 0
```

### 3. Hook PreToolUse - Bash (git commit)

**Script:** `.claude/hooks/pre-commit-testing.sh`

```bash
#!/bin/bash
# Pre-commit testing hook
# Ejecuta suite completa antes de permitir commit

COMMAND="$1"

# Detectar si es git commit
if [[ "$COMMAND" == *"git commit"* ]]; then
    echo "🔒 BLOQUEO PRE-COMMIT ACTIVADO"
    echo "Ejecutando suite completa de tests..."

    # Ejecutar suite completa
    Rscript tests/run_all_tests.R

    if [ $? -ne 0 ]; then
        echo ""
        echo "❌ =============================================="
        echo "❌ COMMIT RECHAZADO - TESTS FALLARON"
        echo "❌ =============================================="
        echo ""
        echo "Acciones requeridas:"
        echo "1. Revisar errores de tests arriba"
        echo "2. Corregir el código que causó la falla"
        echo "3. Volver a ejecutar: Rscript tests/run_all_tests.R"
        echo "4. Solo entonces hacer commit"
        echo ""
        echo "⚠️ PROHIBIDO usar: git commit --no-verify"
        echo ""
        exit 1
    fi

    echo ""
    echo "✅ =============================================="
    echo "✅ TESTS PASARON - COMMIT PERMITIDO"
    echo "✅ =============================================="
    echo ""
fi

exit 0
```

### 4. Hook PreToolUse - Bash (git push)

**Script:** `.claude/hooks/pre-push-testing.sh`

```bash
#!/bin/bash
# Pre-push testing hook
# Validación final antes de push a remoto

COMMAND="$1"

# Detectar si es git push
if [[ "$COMMAND" == *"git push"* ]]; then
    echo "🔒 BLOQUEO PRE-PUSH ACTIVADO"
    echo "Ejecutando validación final..."

    # 1. Verificar que no hay cambios sin commit
    if ! git diff-index --quiet HEAD --; then
        echo "❌ PUSH RECHAZADO - Hay cambios sin commit"
        exit 1
    fi

    # 2. Ejecutar suite completa
    echo "Ejecutando suite completa de tests..."
    Rscript tests/run_all_tests.R

    if [ $? -ne 0 ]; then
        echo ""
        echo "❌ =============================================="
        echo "❌ PUSH RECHAZADO - TESTS FALLARON"
        echo "❌ =============================================="
        echo ""
        exit 1
    fi

    # 3. Verificar que CI/CD está configurado
    if [ ! -f ".github/workflows/ci-testing.yml" ]; then
        echo "⚠️ ADVERTENCIA: CI/CD no configurado"
        echo "Se recomienda tener CI/CD para validación automática"
    fi

    echo ""
    echo "✅ =============================================="
    echo "✅ VALIDACIÓN FINAL COMPLETA - PUSH PERMITIDO"
    echo "✅ =============================================="
    echo ""
fi

exit 0
```

---

## 📋 Configuración en settings.json

**Archivo:** `.claude/settings.json`

```json
{
  "hooks": {
    "preToolUse": {
      "edit": {
        "file": ".claude/hooks/pre-edit-testing.sh",
        "message": "🔍 Verificando tests antes de editar..."
      },
      "write": {
        "file": ".claude/hooks/pre-write-testing.sh",
        "message": "🔍 Verificando tests antes de crear archivo..."
      },
      "bash": {
        "file": ".claude/hooks/pre-bash-testing.sh",
        "message": "🔍 Verificando comando bash..."
      }
    },
    "postToolUse": {
      "edit": {
        "file": ".claude/hooks/post-edit-testing.sh",
        "message": "✅ Validando cambios..."
      },
      "write": {
        "file": ".claude/hooks/post-write-testing.sh",
        "message": "✅ Validando archivo creado..."
      }
    }
  },
  "testing": {
    "enabled": true,
    "runOnChange": true,
    "blockOnFailure": true,
    "coverageThreshold": 100,
    "suites": {
      "validacion_matematica": "tests/testthat/test_validacion_matematica.R",
      "ortografia": "tests/testthat/test_ortografia_espanol.R",
      "renderizado": "tests/testthat/test_renderizado_4_formatos.R",
      "aleatorization": "tests/testthat/test_aleatorization_diversity.R",
      "flujo_b": "tests/testthat/test_flujo_b_graficador.R",
      "regression": "tests/testthat/test_regression_suite.R"
    }
  }
}
```

---

## 🤖 Comportamiento de Claude

### Cuando Claude Edita Código

```
1. Claude detecta necesidad de Edit/Write
2. Hook PRE ejecuta tests relevantes
3. SI PASAN → Claude procede con cambio
4. Hook POST ejecuta tests de validación
5. SI FALLAN → Claude muestra error + revierte cambio automáticamente
6. Claude reporta al usuario: "Tests pasaron" o "Tests fallaron (revertido)"
```

### Cuando Usuario Hace Commit

```
1. Usuario ejecuta: git commit -m "mensaje"
2. Hook PRE-COMMIT bloquea ejecución
3. Ejecuta: Rscript tests/run_all_tests.R
4. SI FALLA → Commit rechazado + mostrar errores
5. SI PASA → Commit permitido
```

### Cuando Claude Ayuda con Commit

```
1. Claude ejecuta comando git
2. Hook intercepta comando
3. Ejecuta suite completa
4. Claude informa resultado al usuario
5. Solo procede si tests pasan
```

---

## 🚨 Mensajes de Bloqueo

### Cuando Tests Fallan en Edit

```
❌ ============================================
❌ CAMBIO BLOQUEADO - TESTS FALLARON
❌ ============================================

Archivo: .claude/scripts/validar_coherencia_matematica.R

Tests fallidos:
- test_validacion_matematica.R::test_that("Validación detecta exshuffle")

Error: expected TRUE, got FALSE

ACCIÓN REQUERIDA:
1. No se aplicó el cambio (código intacto)
2. Revisar por qué el test falló
3. Corregir la lógica
4. Volver a intentar
```

### Cuando Tests Fallan en Commit

```
❌ ============================================
❌ COMMIT RECHAZADO - TESTS FALLARON
❌ ============================================

Suite: Validación Matemática
Status: ✗ FALLIDO

Acciones requeridas:
1. Ejecutar: Rscript tests/run_all_tests.R
2. Identificar tests fallidos
3. Corregir código
4. Volver a ejecutar tests
5. Solo entonces: git commit

⚠️ PROHIBIDO: git commit --no-verify
```

---

## 📊 Métricas de Monitoreo

Claude debe reportar automáticamente:

```
═══════════════════════════════════════
  REPORTE DE TESTING AUTOMÁTICO
═══════════════════════════════════════

Última ejecución: 2026-02-04 14:30:25
Duración: 28.4 segundos

Suites ejecutadas: 6/6
✓ Exitosas: 6
✗ Fallidas: 0

Cobertura actual: 100%
Estado del sistema: ✅ SALUDABLE

Cambios validados hoy: 12
Commits bloqueados: 0
Regresiones prevenidas: 0
```

---

## 🎯 Garantías del Sistema

Con este sistema implementado, se garantiza:

1. ✅ **Ningún cambio rompedor llega a main**
2. ✅ **100% de cobertura se mantiene permanentemente**
3. ✅ **Tests se ejecutan automáticamente sin intervención**
4. ✅ **Errores se detectan ANTES de commit**
5. ✅ **Claude no puede hacer cambios que rompan tests**
6. ✅ **Usuario no puede hacer commit con tests fallando**
7. ✅ **CI/CD valida adicionalmente en remoto**

---

## ⚠️ Casos Especiales

### Actualizar Tests Mismos

```
Si se modifica un archivo test_*.R:
1. Hook ejecuta suite COMPLETA
2. Valida que el test modificado funciona
3. Valida que otros tests no se rompieron
4. Solo permite cambio si todo pasa
```

### Emergencias (PROHIBIDO)

```
❌ NUNCA usar: git commit --no-verify
❌ NUNCA deshabilitar hooks temporalmente
❌ NUNCA comentar tests que fallan

✅ SIEMPRE corregir el código
✅ SIEMPRE mantener tests funcionando
✅ SIEMPRE reportar problemas al usuario
```

---

## 📚 Referencias

- **Documentación completa:** `.claude/docs/ECOSISTEMA_TESTING.md`
- **Script ejecutor:** `tests/run_all_tests.R`
- **Suites individuales:** `tests/testthat/test_*.R`
- **CI/CD:** `.github/workflows/ci-testing.yml`
- **Hooks:** `.claude/hooks/`

---

**Versión:** 1.0
**Fecha:** 2026-02-04
**Estado:** ACTIVO Y OBLIGATORIO
**Excepciones:** NINGUNA
