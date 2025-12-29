---
name: post-exams2-validation
trigger: PostToolUse
pattern: exams2html|exams2pdf|exams2pandoc|exams2nops|exams2moodle
description: Hook que se activa después de ejecutar funciones exams2* para validar resultados.
---

# Hook: Post-Renderizado exams2*

## Trigger
Se activa automáticamente después de detectar ejecución de:

- `exams2html()`
- `exams2pdf()`
- `exams2pandoc()`
- `exams2nops()`
- `exams2moodle()`

## Comportamiento

### Caso: Compilación Exitosa
```
Detectado: exams2pdf() completado sin errores
Acción:
  ✅ Confirmar éxito al usuario
  📋 Sugerir inspección visual
  🔍 Recordar verificar:

     - Gráficos visibles
     - Tamaños apropiados
     - Sin solapamiento
     - Etiquetas legibles
```

### Caso: Error de Compilación
```
Detectado: exams2pdf() falló con error
Acción:
  ❌ Capturar mensaje de error
  🔍 Activar skill: diagnosticar-errores
  📊 Clasificar error (GRÁFICOS/TEXTO/ESTRUCTURA/COHERENCIA)
  🔧 Sugerir corrección específica
  🔄 Ofrecer re-ejecutar después de corrección
```

## Patrones de Error Manejados

| Patrón en Output | Diagnóstico | Acción |
|------------------|-------------|--------|
| `File '*.png' not found` | ERR_G1 | → corregir-graficos |
| `LaTeX failed to compile` | ERR_T1 | → Revisar sintaxis |
| `non-numeric argument` | ERR_C3 | → validar-coherencia |
| `Error in` (R) | Variable | → Analizar contexto |

## Flujo de Decisión

```
Resultado de exams2*
    ↓
┌─────────────────────┐
│ ¿Compiló sin error? │
└─────────────────────┘
    ↓           ↓
   Sí          No
    ↓           ↓
Inspección   Diagnóstico
  Visual      Automático
    ↓           ↓
¿Errores    Clasificar
visuales?    Error
    ↓           ↓
   Sí        Activar
    ↓        Corrección
Clasificar      ↓
    ↓        Re-ejecutar
Corregir    exams2*
    ↓
Re-ejecutar
exams2*
```

## Integración con Otros Hooks

- **PreToolUse(str-replace-editor)**: Se activa si corrección requiere editar .Rmd
- **PostError(LaTeX|TikZ|Python)**: Se activa si error específico detectado

## Logging

Cada activación del hook registra:
```
[TIMESTAMP] Hook: post-exams2-validation
[TIMESTAMP] Función: exams2pdf
[TIMESTAMP] Resultado: ERROR/SUCCESS
[TIMESTAMP] Archivo: nombre.Rmd
[TIMESTAMP] Acción tomada: [descripción]
```

## Referencias

- `.claude/skills/validar-renderizado/skill.md`
- `.claude/skills/diagnosticar-errores/skill.md`
- `.claude/agents/validador-visual.md`

