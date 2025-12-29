# ✅ Checklist de Verificación Pre-Eliminación - Fase 5

**Comando a eliminar:** `/analizar-ejercicio`  
**Fecha programada:** 2025-03-20  
**Responsable:** [Nombre del ejecutor]  
**Fecha de ejecución:** [Fecha real de ejecución]

---

## 📋 VERIFICACIONES OBLIGATORIAS PRE-ELIMINACIÓN

### 1. Verificación de Fecha

- [ ] **Fecha actual ≥ 2025-03-20** (3 meses desde deprecación)
- [ ] **Período de gracia cumplido** (90 días mínimo)
- [ ] **Notificación previa enviada** (si aplica)

**Notas:**
```
Fecha de deprecación: 2025-12-20
Fecha programada de eliminación: 2025-03-20
Días transcurridos: [Calcular]
```

---

### 2. Verificación de Uso del Comando

- [ ] **Búsqueda en logs de uso** (si existen)
- [ ] **Verificar que no hay uso reciente** (últimos 30 días)
- [ ] **Confirmar que usuarios migraron a `/analizar-icfes`**

**Comando de verificación:**
```bash
# Buscar referencias activas (excluyendo documentación de deprecación)
grep -r "analizar-ejercicio" .claude/ --include="*.md" | \
  grep -v "DEPRECADO" | \
  grep -v "COMANDOS_DEPRECADOS.md" | \
  grep -v "CHANGELOG.md" | \
  grep -v "FASE5"
```

**Resultado esperado:** Sin resultados (0 referencias activas)

**Resultado real:**
```
[Pegar resultado aquí]
```

---

### 3. Verificación de Referencias en Código

- [ ] **No hay referencias en workflow oficial** (TROUBLESHOOTING.md)
- [ ] **No hay referencias en comandos activos** (generar-schoice, generar-cloze)
- [ ] **No hay referencias en agentes** (clasificador-icfes, graficador-tikz)
- [ ] **No hay referencias en skills**

**Comando de verificación:**
```bash
# Verificar workflow
grep "analizar-ejercicio" .claude/docs/TROUBLESHOOTING.md

# Verificar comandos
grep "analizar-ejercicio" .claude/commands/generar-*.md

# Verificar agentes
grep "analizar-ejercicio" .claude/agents/*.md
```

**Resultado esperado:** Sin resultados en archivos activos

**Resultado real:**
```
[Pegar resultado aquí]
```

---

### 4. Verificación de Documentación

- [ ] **COMANDOS_DEPRECADOS.md está actualizado**
- [ ] **CHANGELOG.md documenta la deprecación**
- [ ] **README.md no referencia el comando deprecado**
- [ ] **GUIA_USUARIO.md marca el comando como deprecado**

**Archivos a verificar:**
```bash
# Verificar que existe documentación de deprecación
cat .claude/docs/COMANDOS_DEPRECADOS.md | grep "analizar-ejercicio"

# Verificar que changelog documenta deprecación
cat .claude/docs/CHANGELOG.md | grep "analizar-ejercicio"
```

**Resultado:**

- [ ] Documentación completa y correcta

---

### 5. Verificación de Backup

- [ ] **Sistema de backup configurado**
- [ ] **Espacio suficiente para backup**
- [ ] **Permisos de escritura en directorio de backup**

**Comando de verificación:**
```bash
# Crear directorio de backup de prueba
mkdir -p .claude/backups/test_$(date +%Y%m%d)

# Verificar espacio
df -h .claude/backups/

# Limpiar
rmdir .claude/backups/test_$(date +%Y%m%d)
```

**Resultado:**

- [ ] Backup funcional

---

### 6. Verificación de Tests

- [ ] **Tests de validación existen** (fase5_tests_post_eliminacion.sh)
- [ ] **Tests son ejecutables**
- [ ] **Tests pasan en estado actual** (antes de eliminación)

**Comando de verificación:**
```bash
# Verificar que script existe y es ejecutable
ls -la .claude/scripts/fase5_tests_post_eliminacion.sh

# Ejecutar tests en modo dry-run (sin eliminar archivo)
# [Los tests deben fallar en Test 1 porque el archivo aún existe]
```

**Resultado:**

- [ ] Tests preparados y funcionales

---

### 7. Verificación de Plan de Rollback

- [ ] **Procedimiento de rollback documentado**
- [ ] **Backup puede ser restaurado**
- [ ] **Tiempo de rollback estimado** (< 5 minutos)

**Procedimiento de rollback:**
```bash
# En caso de necesitar revertir la eliminación:
cp .claude/backups/fase5_[FECHA]/analizar-ejercicio.md.backup \
   .claude/commands/analizar-ejercicio.md
```

**Resultado:**

- [ ] Plan de rollback verificado

---

### 8. Verificación de Comunicación

- [ ] **Usuarios notificados de eliminación inminente** (si aplica)
- [ ] **Documentación de migración disponible**
- [ ] **Soporte preparado para consultas**

**Canales de comunicación:**

- [ ] Documentación interna actualizada
- [ ] Guía de usuario actualizada
- [ ] Changelog actualizado

---

## 🚀 EJECUCIÓN DE LA ELIMINACIÓN

### Pre-Ejecución

- [ ] **Todas las verificaciones anteriores completadas**
- [ ] **Backup creado manualmente** (adicional al automático)
- [ ] **Ventana de mantenimiento programada** (si aplica)

**Comando de backup manual:**
```bash
cp .claude/commands/analizar-ejercicio.md \
   .claude/backups/manual_backup_$(date +%Y%m%d_%H%M%S).md
```

### Ejecución

- [ ] **Script de eliminación ejecutado**
- [ ] **Tests post-eliminación pasados**
- [ ] **Documentación actualizada**

**Comando de ejecución:**
```bash
bash .claude/scripts/fase5_eliminar_comando_deprecado.sh
```

### Post-Ejecución

- [ ] **Verificar que archivo fue eliminado**
- [ ] **Verificar que backup existe**
- [ ] **Verificar que tests pasan**
- [ ] **Actualizar COMANDOS_DEPRECADOS.md** (mover a Historial)
- [ ] **Actualizar CHANGELOG.md** (agregar entrada de eliminación)

---

## 📊 CRITERIOS DE ÉXITO

### Criterios Obligatorios

- ✅ Archivo `.claude/commands/analizar-ejercicio.md` eliminado
- ✅ Backup creado en `.claude/backups/fase5_[FECHA]/`
- ✅ Tests post-eliminación: 6/6 pasados
- ✅ No hay referencias rotas en documentación
- ✅ Workflow sigue funcionando con `/analizar-icfes`

### Criterios Opcionales

- ✅ Documentación actualizada el mismo día
- ✅ Changelog actualizado con entrada de eliminación
- ✅ Sin incidentes reportados post-eliminación

---

## ⚠️ CONDICIONES DE CANCELACIÓN

**Cancelar la eliminación si:**

1. ❌ Fecha actual < 2025-03-20 (período de gracia no cumplido)
2. ❌ Se encuentran referencias activas al comando
3. ❌ Tests de validación fallan
4. ❌ Sistema de backup no funciona
5. ❌ Hay uso reciente del comando (últimos 30 días)

**En caso de cancelación:**

- Documentar razón en CHANGELOG.md
- Reprogramar eliminación
- Notificar a usuarios (si aplica)

---

## 📝 REGISTRO DE EJECUCIÓN

**Ejecutado por:** [Nombre]  
**Fecha de ejecución:** [YYYY-MM-DD]  
**Hora de inicio:** [HH:MM]  
**Hora de fin:** [HH:MM]  
**Duración total:** [Minutos]

**Resultado:**

- [ ] ✅ Eliminación exitosa
- [ ] ❌ Eliminación cancelada (razón: _____________)
- [ ] ⚠️ Eliminación con advertencias (detalles: _____________)

**Notas adicionales:**
```
[Agregar notas relevantes sobre la ejecución]
```

---

**Última actualización:** 2025-12-20

