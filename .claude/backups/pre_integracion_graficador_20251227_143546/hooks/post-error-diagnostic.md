---
name: post-error-diagnostic
trigger: PostError
pattern: LaTeX|TikZ|Python|Error in|failed to compile
description: Hook que activa diagnóstico automático cuando hay errores de compilación.
---

# Hook: Diagnóstico Post-Error

## Trigger
Se activa automáticamente cuando se detecta un error de:
- Compilación LaTeX
- Renderizado TikZ
- Ejecución Python
- Error R general

## Comportamiento

### Detección de Patrones

| Mensaje de Error | Categoría | Código | Acción Automática |
|------------------|-----------|--------|-------------------|
| `File '*.png' not found` | GRÁFICOS | ERR_G1 | → corregir-graficos |
| `LaTeX failed to compile` | TEXTO | ERR_T1 | → Revisar sintaxis |
| `undefined control sequence` | TEXTO | ERR_T1 | → Verificar paquetes |
| `non-numeric argument` | COHERENCIA | ERR_C3 | → validar-coherencia |
| `Error in abs()` | COHERENCIA | ERR_C3 | → Revisar formateo |
| `reticulate::py_run` error | COHERENCIA | ERR_C3 | → Verificar Python |

### Flujo de Diagnóstico Automático

```
Error detectado
    ↓
Capturar mensaje completo
    ↓
Buscar patrón conocido
    ↓
┌─────────────────────────┐
│ ¿Patrón en base de      │
│ conocimiento?           │
└─────────────────────────┘
    ↓           ↓
   Sí          No
    ↓           ↓
Diagnóstico  Análisis
  rápido     contextual
    ↓           ↓
Mostrar     Proponer
solución    hipótesis
verificada  y pedir
            confirmación
```

### Caso: Error Conocido
```
╔════════════════════════════════════════╗
║  🔍 DIAGNÓSTICO AUTOMÁTICO             ║
╠════════════════════════════════════════╣
║ Error detectado:                       ║
║ "File 'cilindro.png' not found"        ║
║                                        ║
║ Diagnóstico: ERR_G1                    ║
║ Categoría: ERRORES DE GRÁFICOS         ║
║                                        ║
║ Causa: include_tikz() en chunk de      ║
║ generación crea archivos temporales    ║
║ inaccesibles durante compilación LaTeX ║
║                                        ║
║ Solución verificada disponible:        ║
║ → Usar renderizado condicional         ║
║ → Ver: patrones-errores-conocidos.md   ║
║                                        ║
║ ¿Aplicar corrección automática? [S/N]  ║
╚════════════════════════════════════════╝
```

### Caso: Error Desconocido
```
╔════════════════════════════════════════╗
║  🔍 ANÁLISIS DE ERROR                  ║
╠════════════════════════════════════════╣
║ Error detectado:                       ║
║ "[mensaje no reconocido]"              ║
║                                        ║
║ Diagnóstico: No hay patrón conocido    ║
║                                        ║
║ Análisis contextual:                   ║
║ - Archivo: nombre.Rmd                  ║
║ - Línea aproximada: [si disponible]    ║
║ - Contexto: [chunk/sección]            ║
║                                        ║
║ Hipótesis:                             ║
║ [Posibles causas basadas en contexto]  ║
║                                        ║
║ Acción recomendada:                    ║
║ → Investigar manualmente               ║
║ → Si se resuelve, documentar en        ║
║   patrones-errores-conocidos.md        ║
╚════════════════════════════════════════╝
```

## Acciones Automáticas por Categoría

| Categoría | Skill Activado | Descripción |
|-----------|----------------|-------------|
| GRÁFICOS | corregir-graficos | Corrección de visualización |
| TEXTO | - | Guía de corrección manual |
| ESTRUCTURA | - | Verificación de metadatos |
| COHERENCIA | validar-coherencia | Verificación de código |

## Logging

```
[TIMESTAMP] Hook: post-error-diagnostic
[TIMESTAMP] Error: [mensaje capturado]
[TIMESTAMP] Patrón: [conocido/desconocido]
[TIMESTAMP] Diagnóstico: [ERR_XX]
[TIMESTAMP] Acción: [skill activado o manual]
```

## Referencias

- `.claude/docs/patrones-errores-conocidos.md`
- `.claude/skills/diagnosticar-errores/skill.md`
- `.claude/agents/diagnosticador-errores.md`

