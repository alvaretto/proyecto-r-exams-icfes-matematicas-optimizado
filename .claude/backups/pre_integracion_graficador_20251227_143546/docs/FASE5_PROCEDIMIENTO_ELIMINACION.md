# 📋 Procedimiento de Eliminación - Fase 5

**Comando a eliminar:** `/analizar-ejercicio`  
**Fecha de creación del procedimiento:** 2025-12-20  
**Fecha programada de ejecución:** 2025-03-20  
**Tiempo estimado:** 30 minutos

---

## 🎯 Objetivo

Eliminar definitivamente el comando `/analizar-ejercicio` del sistema después de cumplir el período de gracia de 3 meses desde su deprecación.

---

## 📅 Cronograma

| Fecha | Fase | Acción |
|-------|------|--------|
| 2025-12-20 | Fase 1 | Deprecación del comando |
| 2025-01-20 | Fase 2 | Agregar warnings (opcional) |
| 2025-03-20 | **Fase 5** | **Eliminación definitiva** |

---

## 🔍 Pre-Requisitos

### Verificaciones Obligatorias

1. ✅ **Fecha actual ≥ 2025-03-20** (90 días desde deprecación)
2. ✅ **No hay uso reciente del comando** (últimos 30 días)
3. ✅ **No hay referencias activas en código**
4. ✅ **Documentación de deprecación completa**
5. ✅ **Sistema de backup funcional**
6. ✅ **Tests de validación preparados**

### Documentos a Revisar

- [ ] `.claude/docs/FASE5_CHECKLIST_PRE_ELIMINACION.md` - Completar checklist
- [ ] `.claude/docs/COMANDOS_DEPRECADOS.md` - Verificar documentación
- [ ] `.claude/CHANGELOG.md` - Verificar entrada de deprecación

---

## 🚀 Procedimiento de Ejecución

### PASO 1: Preparación (5 minutos)

#### 1.1 Completar Checklist Pre-Eliminación

```bash
# Abrir checklist
nano .claude/docs/FASE5_CHECKLIST_PRE_ELIMINACION.md

# Completar todas las verificaciones
# Marcar cada item con [x]
```

**Criterio de éxito:** Todas las verificaciones marcadas como completadas

#### 1.2 Crear Backup Manual

```bash
# Crear backup manual adicional
mkdir -p .claude/backups
cp .claude/commands/analizar-ejercicio.md \
   .claude/backups/manual_backup_$(date +%Y%m%d_%H%M%S).md

# Verificar que el backup existe
ls -la .claude/backups/manual_backup_*.md
```

**Criterio de éxito:** Backup manual creado exitosamente

---

### PASO 2: Ejecución del Script de Eliminación (10 minutos)

#### 2.1 Ejecutar Script Automatizado

```bash
# Ejecutar script de eliminación
bash .claude/scripts/fase5_eliminar_comando_deprecado.sh
```

**El script realizará:**
1. Verificación de fecha
2. Verificación de referencias activas
3. Creación de backup automático
4. Eliminación del archivo
5. Ejecución de tests de validación

#### 2.2 Revisar Output del Script

**Output esperado:**
```
╔════════════════════════════════════════════════════════════╗
║   FASE 5: ELIMINACIÓN DE COMANDO DEPRECADO                ║
║   Comando: /analizar-ejercicio                            ║
║   Fecha programada: 2025-03-20                            ║
╚════════════════════════════════════════════════════════════╝

[INFO] Verificando fecha de ejecución...
[OK] Verificación de fecha completada
[INFO] Verificando existencia del archivo deprecado...
[OK] Archivo deprecado encontrado
[INFO] Verificando referencias activas al comando deprecado...
[OK] No se encontraron referencias activas
[INFO] Creando backup del archivo deprecado...
[OK] Backup creado en: .claude/backups/fase5_2025-03-20/analizar-ejercicio.md.backup
[INFO] Eliminando archivo deprecado...
[OK] Archivo eliminado: .claude/commands/analizar-ejercicio.md
[INFO] Ejecutando tests de validación post-eliminación...
[OK] Tests de validación pasados

╔════════════════════════════════════════════════════════════╗
║   FASE 5 COMPLETADA EXITOSAMENTE                          ║
╚════════════════════════════════════════════════════════════╝
```

**Criterio de éxito:** Script completa sin errores

---

### PASO 3: Validación Post-Eliminación (5 minutos)

#### 3.1 Verificar Eliminación

```bash
# Verificar que el archivo fue eliminado
ls .claude/commands/analizar-ejercicio.md
# Resultado esperado: "No such file or directory"

# Verificar que el backup existe
ls .claude/backups/fase5_*/analizar-ejercicio.md.backup
# Resultado esperado: Archivo de backup listado
```

#### 3.2 Ejecutar Tests Manualmente

```bash
# Ejecutar tests de validación
bash .claude/scripts/fase5_tests_post_eliminacion.sh
```

**Output esperado:**
```
╔════════════════════════════════════════════════════════════╗
║   TESTS DE VALIDACIÓN POST-ELIMINACIÓN                    ║
╚════════════════════════════════════════════════════════════╝

Test: Archivo deprecado eliminado ... PASADO
Test: Comando estándar /analizar-icfes existe ... PASADO
Test: Workflow usa /analizar-icfes ... PASADO
Test: generar-schoice usa /analizar-icfes ... PASADO
Test: generar-cloze usa /analizar-icfes ... PASADO
Test: No hay referencias rotas a /analizar-ejercicio ... PASADO

════════════════════════════════════════════════════════════
RESUMEN DE TESTS
════════════════════════════════════════════════════════════
Tests pasados: 6/6
Tests fallados: 0/6

✅ TODOS LOS TESTS PASARON
```

**Criterio de éxito:** 6/6 tests pasados

---

### PASO 4: Actualización de Documentación (10 minutos)

#### 4.1 Actualizar COMANDOS_DEPRECADOS.md

```bash
# Editar archivo
nano .claude/docs/COMANDOS_DEPRECADOS.md
```

**Cambios a realizar:**

1. Mover entrada de `/analizar-ejercicio` a sección "Historial"
2. Actualizar estadísticas de deprecación
3. Agregar fecha de eliminación

**Ejemplo:**
```markdown
## 🗂️ Historial de Comandos Eliminados

### `/analizar-ejercicio` (ELIMINADO)

**Fecha de deprecación:** 2025-12-20  
**Fecha de eliminación:** 2025-03-20  
**Razón:** Análisis incompleto (3/6 dimensiones ICFES)  
**Alternativa:** `/analizar-icfes`

---

## 📊 Estadísticas de Deprecación

| Métrica | Valor |
|---------|-------|
| **Total de comandos deprecados** | 1 |
| **Comandos en Fase 1** | 0 |
| **Comandos en Fase 2** | 0 |
| **Comandos eliminados (Fase 3)** | 1 |
| **Tasa de migración exitosa** | 100% |
```

#### 4.2 Actualizar CHANGELOG.md

```bash
# Editar archivo
nano .claude/CHANGELOG.md
```

**Agregar nueva entrada:**
```markdown
## [2025-03-20] - Eliminación de Comando Deprecado

### 🗑️ Eliminado

#### `/analizar-ejercicio`

**Razón de eliminación:**
- Período de gracia cumplido (90 días desde deprecación)
- Sin uso documentado durante el período de gracia
- Migración exitosa a `/analizar-icfes` completada

**Detalles:**
- Fecha de deprecación: 2025-12-20
- Fecha de eliminación: 2025-03-20
- Backup creado: `.claude/backups/fase5_2025-03-20/`
- Tests post-eliminación: 6/6 pasados

**Impacto:**
- ✅ Sin impacto en workflow (ya usa `/analizar-icfes`)
- ✅ Sin referencias rotas en documentación
- ✅ Sistema funcionando correctamente

### 📚 Documentación

#### Archivos Actualizados
- ✅ `.claude/docs/COMANDOS_DEPRECADOS.md` - Movido a Historial
- ✅ `.claude/CHANGELOG.md` - Este archivo
```

---

## ⚠️ Plan de Contingencia

### Si Algo Sale Mal

#### Opción 1: Rollback Automático

```bash
# Ejecutar script de rollback
bash .claude/scripts/fase5_rollback.sh
```

Este script:
1. Busca el backup más reciente
2. Restaura el archivo eliminado
3. Ejecuta tests de validación
4. Genera log de rollback

#### Opción 2: Rollback Manual

```bash
# Restaurar desde backup manual
cp .claude/backups/manual_backup_[FECHA].md \
   .claude/commands/analizar-ejercicio.md

# Verificar restauración
ls -la .claude/commands/analizar-ejercicio.md
```

### Condiciones para Rollback

Ejecutar rollback si:
- ❌ Tests post-eliminación fallan
- ❌ Se descubren referencias rotas
- ❌ Workflow deja de funcionar
- ❌ Se reportan errores críticos

---

## 📊 Criterios de Éxito Final

### Obligatorios

- ✅ Archivo `.claude/commands/analizar-ejercicio.md` eliminado
- ✅ Backup creado y verificado
- ✅ Tests post-eliminación: 6/6 pasados
- ✅ Documentación actualizada (COMANDOS_DEPRECADOS.md, CHANGELOG.md)
- ✅ No hay referencias rotas
- ✅ Workflow funciona correctamente

### Opcionales

- ✅ Log de eliminación generado
- ✅ Checklist completado
- ✅ Sin incidentes reportados (primeras 24 horas)

---

## 📝 Registro de Ejecución

**Completar después de la ejecución:**

```
Fecha de ejecución: [YYYY-MM-DD]
Hora de inicio: [HH:MM]
Hora de fin: [HH:MM]
Duración total: [Minutos]
Ejecutado por: [Nombre]

Resultado: [ ] Exitoso  [ ] Fallido  [ ] Rollback ejecutado

Tests pasados: [X]/6

Notas:
[Agregar notas relevantes]
```

---

**Última actualización:** 2025-12-20

