---
description: Itera automáticamente un lenguaje hasta alcanzar un umbral de similitud o máximo de iteraciones.
---

# Auto-Iterar Lenguaje

Ejecuta iteraciones automáticas de refinamiento hasta alcanzar un umbral de similitud especificado o un máximo de iteraciones.

## Uso

```
/auto-iterar [lenguaje] [umbral] [max_iteraciones]
```

**Parámetros**:

- `lenguaje`: tikz|python|r (requerido)
- `umbral`: Puntuación mínima para validar (default: 95)
- `max_iteraciones`: Máximo de iteraciones permitidas (default: 10)

**Ejemplos**:

```
/auto-iterar tikz 95 10
/auto-iterar python 90 5
/auto-iterar r 95 8
```

## Proceso

1. **Validar Parámetros**:
   - Verificar que el lenguaje sea válido (tikz, python, r)
   - Establecer umbral (default: 95)
   - Establecer max_iteraciones (default: 10)
   - Leer estado actual del workflow

2. **Validar Estado**:
   - Verificar que el lenguaje especificado esté en estado "en_iteracion" o "pendiente"
   - Si está pendiente, iniciar generación primero con `/generar-[lenguaje]`
   - Si está validado, informar que ya está completado

3. **Ciclo Automático de Iteración**:

```
INICIO:
  - Generar/refinar código con /generar-[lenguaje] o /iterar [lenguaje]
  - Comparar con /comparar [lenguaje]
  - Leer similitud_actual del workflow_state.json
  
  SI similitud_actual >= umbral:
    - Validar lenguaje automáticamente
    - ACTUALIZAR estado: [lenguaje].estado = "validado"
    - FIN: Éxito - Similitud alcanzada
    
  SI iteracion_actual >= max_iteraciones:
    - FIN: Límite alcanzado - Máximo de iteraciones
    
  SI NO:
    - Continuar iteración
    - Volver a INICIO
```

4. **Actualizar Estado en Cada Iteración**:
   - El comando `/iterar` actualiza automáticamente el estado
   - El comando `/comparar` actualiza similitud_actual e historial
   - Verificar progreso después de cada iteración

5. **Reporte Final**:

```markdown
## Auto-Iteración Completada - [Lenguaje]

### Resultado

✅ **Éxito**: Similitud de [X]% alcanzada en [N] iteraciones
⚠️ **Límite alcanzado**: Máximo de [max_iteraciones] iteraciones alcanzado
❌ **Error**: [descripción del error]

### Estadísticas

- **Iteraciones ejecutadas**: [N]
- **Similitud inicial**: [X]%
- **Similitud final**: [Y]%
- **Mejora total**: [+Z] puntos
- **Mejora promedio por iteración**: [+W] puntos
- **Tiempo total**: [duración]

### Historial de Similitud

[Mostrar progreso: valor1 → valor2 → ... → valor_final]

### Próximos Pasos

- Si éxito: Continuar con siguiente lenguaje o ejecutar `/exportar`
- Si límite alcanzado: Revisar diferencias y considerar refinamiento manual
- Si error: Revisar logs y corregir problema
```

## Control de Cancelación

El usuario puede cancelar el proceso en cualquier momento:

- Durante la generación: Esperar a que termine y cancelar antes de comparar
- Durante la comparación: Cancelar y mantener estado actual
- Entre iteraciones: Cancelar antes de iniciar siguiente iteración

**Estado después de cancelación**:
- Mantener estado actual del workflow
- No revertir cambios ya aplicados
- Permitir continuar manualmente después

## Validación Automática

Si se alcanza el umbral:

1. Actualizar estado:
   - `[lenguaje].estado` = "validado"
   - `[lenguaje].timestamp_validacion` = timestamp actual
   - `fase_actual` = "[lenguaje]_validado"
   - Actualizar `timestamp_ultima_actualizacion`

2. Informar al usuario:
   - Mostrar mensaje de éxito
   - Sugerir continuar con siguiente lenguaje
   - Ofrecer opción de exportar

## Límites y Protecciones

### Límite de Iteraciones

- **Default**: 10 iteraciones máximo
- **Razón**: Evitar bucles infinitos y consumo excesivo de recursos
- **Comportamiento**: Detener automáticamente al alcanzar límite

### Detección de Convergencia

Si la mejora entre iteraciones es < 2 puntos:

- Advertir al usuario sobre posible convergencia
- Sugerir revisar diferencias restantes manualmente
- Ofrecer opción de validar con similitud actual o continuar

### Detección de Regresión

Si la similitud disminuye entre iteraciones:

- Detener automáticamente
- Informar al usuario
- Sugerir revisar cambios aplicados
- Ofrecer opción de revertir a iteración anterior

## Integración con Otros Comandos

Este comando utiliza internamente:

- `/generar-[lenguaje]`: Para primera generación
- `/iterar [lenguaje]`: Para refinamientos subsecuentes
- `/comparar [lenguaje]`: Para evaluación después de cada iteración
- Skill `gestionar-estado`: Para actualizar estado del workflow

## Ejemplos de Uso

### Ejemplo 1: Auto-iterar TikZ hasta 95%

```
Usuario: /auto-iterar tikz 95 10

Sistema: Iniciando auto-iteración para TikZ...
         Iteración 1: Generando código...
         Comparando... Similitud: 75%
         Iteración 2: Refinando...
         Comparando... Similitud: 82%
         Iteración 3: Refinando...
         Comparando... Similitud: 89%
         Iteración 4: Refinando...
         Comparando... Similitud: 96%
         
         ✅ Éxito: Similitud de 96% alcanzada en 4 iteraciones
         TikZ validado automáticamente.
```

### Ejemplo 2: Límite de iteraciones alcanzado

```
Usuario: /auto-iterar python 95 3

Sistema: Iniciando auto-iteración para Python...
         Iteración 1: Similitud: 78%
         Iteración 2: Similitud: 85%
         Iteración 3: Similitud: 88%
         
         ⚠️ Límite alcanzado: Máximo de 3 iteraciones alcanzado
         Similitud final: 88% (objetivo: 95%)
         Considera ejecutar /iterar python manualmente para continuar.
```

### Ejemplo 3: Convergencia detectada

```
Sistema: Iteración 4: Similitud: 89%
         Iteración 5: Similitud: 90%
         
         ⚠️ Convergencia detectada: Mejora < 2 puntos entre iteraciones
         Similitud actual: 90%
         ¿Deseas validar con esta similitud o continuar manualmente?
```

## Referencias

- `skills/gestionar-estado/skill.md` - Skill de gestión de estado del workflow
- `.claude/schemas/workflow_state.schema.json` - Esquema del estado del workflow
- `.claude/commands/iterar.md` - Comando de iteración manual
- `.claude/commands/comparar.md` - Comando de comparación visual

