---
description: Refina el código del lenguaje actual basándose en la última comparación visual.
---

# Iterar y Refinar Código

Refina el código del lenguaje actual basándose en la comparación visual.

## Proceso

1. **Revisa** el último reporte de comparación

2. **Prioriza** las correcciones por impacto visual:
   - Alto: Valores incorrectos, elementos faltantes
   - Medio: Colores, posiciones, proporciones
   - Bajo: Estilos menores, ajustes estéticos

3. **Aplica correcciones** de forma sistemática:
   - Mantén las partes que ya funcionan correctamente
   - Ajusta solo lo necesario para corregir diferencias
   - Usa valores precisos basados en el análisis visual

4. **Documenta cambios**:

```markdown
## Iteración [N] - [Lenguaje]

### Cambios aplicados

- [Descripción del cambio 1]
- [Descripción del cambio 2]
...

### Código actualizado

[Código completo]
```

5. **Re-renderiza y compara**:
   - Ejecuta el código actualizado
   - Compara nuevamente con `/comparar`
   - Continúa iterando si es necesario

## Uso

```
/iterar [lenguaje] [descripción de cambios]
```

**Parámetros**:

- `lenguaje`: tikz|python|r (opcional, refina el último trabajado si no se especifica)
- `descripción`: Cambios específicos solicitados por el usuario (opcional)

## Referencias

- `skills/refinar-codigo.md` - Skill de refinamiento iterativo

