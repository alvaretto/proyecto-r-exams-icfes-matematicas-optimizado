---
description: Refina el código del lenguaje actual basándose en la última comparación visual.
---

# Refinar Código Gráfico

Refina el código del lenguaje actual basándose en la comparación visual.

## Proceso

1. **Actualizar Estado del Workflow**:
   - Leer `outputs/workflow_state.json`
   - Identificar lenguaje activo según `fase_actual` o parámetro proporcionado
   - Incrementar `[lenguaje].iteracion_actual`
   - Registrar timestamp de iteración
   - Actualizar `timestamp_ultima_actualizacion`
   - Guardar estado

2. **Revisa** el último reporte de comparación y métricas cuantitativas

3. **Prioriza** las correcciones por impacto visual y puntuación:
   - Alto: Valores incorrectos, elementos faltantes (afectan categorías de 20 puntos)
   - Medio: Colores, posiciones, proporciones (afectan categorías de 15-20 puntos)
   - Bajo: Estilos menores, ajustes estéticos (afectan categorías de 10-15 puntos)

4. **Aplica correcciones** de forma sistemática:
   - Mantén las partes que ya funcionan correctamente
   - Ajusta solo lo necesario para corregir diferencias
   - Usa valores precisos basados en el análisis visual y análisis_inicial.json
   - Si hay lecciones aprendidas disponibles, aplica estrategias exitosas

5. **Documenta cambios** en `outputs/reporte_matematico.md`:

```markdown
## Iteración [N] - [Lenguaje]

### Cambios aplicados

- [Descripción del cambio 1]
- [Descripción del cambio 2]
...

### Código actualizado

[Código completo]

### Similitud anterior vs esperada

- Anterior: [X] puntos
- Esperada tras correcciones: [Y] puntos
```

6. **Re-renderiza y compara**:
   - Ejecuta el código actualizado
   - Compara nuevamente con `/comparar-similitud-visual` (actualizará estado automáticamente)
   - Continúa iterando si es necesario

## Uso

```
/refinar-codigo-grafico [lenguaje] [descripción de cambios]
```

**Parámetros**:

- `lenguaje`: tikz|python|r (opcional, refina el último trabajado si no se especifica)
- `descripción`: Cambios específicos solicitados por el usuario (opcional)

## Referencias

- `skills/refinar-codigo-grafico/skill.md` - Skill de refinamiento iterativo
- `skills/gestionar-estado-graficador/skill.md` - Skill de gestión de estado del workflow
- `.claude/schemas/workflow_state.schema.json` - Esquema del estado del workflow

