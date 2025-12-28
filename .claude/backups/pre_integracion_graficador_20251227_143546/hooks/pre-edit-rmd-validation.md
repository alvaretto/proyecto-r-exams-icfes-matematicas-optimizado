---
name: pre-edit-rmd-validation
trigger: PreToolUse
pattern: str-replace-editor.*\.Rmd
description: Hook que valida coherencia antes de editar archivos .Rmd.
---

# Hook: Pre-Edición de .Rmd

## Trigger
Se activa automáticamente antes de editar un archivo .Rmd con str-replace-editor.

## Comportamiento

### Validaciones Pre-Edición

1. **Detectar patrones problemáticos en código nuevo**:
   - `abs(.*formateado)` → Error #2
   - `round(.*formateado)` → Error #2
   - `include_tikz` en chunk results="hide" → Error #1
   - Variables hardcodeadas en TikZ

2. **Verificar coherencia con código existente**:
   - Nombres de variables consistentes
   - Tipos de datos compatibles
   - Referencias a chunks existentes

### Caso: Patrón Problemático Detectado
```
Detectado: Código a insertar contiene "abs(b_formateado)"
Acción:
  ⚠️ ADVERTENCIA: Posible Error #2 detectado
  📖 Ver: .claude/docs/patrones-errores-conocidos.md#error-2
  💡 Sugerencia: Usar abs(b) antes de formatear
  
  ¿Continuar con la edición?
  [Sí, entiendo el riesgo] [No, corregir primero]
```

### Caso: Sin Problemas Detectados
```
Validación pre-edición: ✅ OK
Procediendo con la edición...
```

## Patrones Monitoreados

| Patrón | Error Asociado | Severidad |
|--------|----------------|-----------|
| `abs\(.*formateado\)` | ERR_C3 | ⚠️ Alta |
| `round\(.*formateado\)` | ERR_C3 | ⚠️ Alta |
| `floor\(.*formateado\)` | ERR_C3 | ⚠️ Alta |
| `ceiling\(.*formateado\)` | ERR_C3 | ⚠️ Alta |
| `include_tikz.*results.*hide` | ERR_G1 | ⚠️ Alta |
| `\\def\\.*{[0-9]+}` sin paste0 | ERR_C2 | ⚠️ Media |

## Flujo de Decisión

```
Solicitud de edición .Rmd
    ↓
Analizar código a insertar
    ↓
┌────────────────────────────┐
│ ¿Patrones problemáticos?   │
└────────────────────────────┘
    ↓           ↓
   Sí          No
    ↓           ↓
Advertir    Proceder
    ↓        con edición
Esperar
confirmación
    ↓
┌──────────────┐
│ ¿Confirma?   │
└──────────────┘
    ↓       ↓
   Sí      No
    ↓       ↓
Proceder  Cancelar
          y sugerir
          corrección
```

## Integración

- **post-exams2-validation**: Se activa después si la edición causa error
- **diagnosticar-errores**: Se referencia en advertencias

## Beneficios

1. **Prevención**: Detectar errores antes de que ocurran
2. **Educación**: Mostrar referencia a documentación
3. **Eficiencia**: Evitar ciclos de corrección innecesarios

## Referencias

- `.claude/docs/patrones-errores-conocidos.md`
- `.claude/skills/validar-coherencia/skill.md`
- `.claude/agents/corrector-coherencia.md`

