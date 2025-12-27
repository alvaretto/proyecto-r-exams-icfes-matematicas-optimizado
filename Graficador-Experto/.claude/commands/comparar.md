---
description: Compara la imagen generada con la original usando Claude Vision.
---

# Comparar Imágenes

Compara visualmente la imagen generada con la original.

## Proceso

1. **Carga ambas imágenes**: Original y generada

2. **Análisis detallado** - Identifica diferencias en:
   - **Colores**: ¿Coinciden los colores exactamente?
   - **Posiciones**: ¿Están los elementos en la ubicación correcta?
   - **Valores numéricos**: ¿Son correctos todos los números, etiquetas y escalas?
   - **Proporciones**: ¿Se mantienen las proporciones y dimensiones?
   - **Estilos**: ¿Coinciden grosores de línea, marcadores, fuentes?
   - **Elementos**: ¿Faltan o sobran elementos?

3. **Genera reporte de comparación**:

```markdown
## Comparación Visual - [Lenguaje]

### Estado General

✅ Excelente / ⚠️ Necesita ajustes / ❌ Requiere corrección mayor

### Diferencias Identificadas

#### Colores

- [ ] Correcto / [ ] Incorrecto: [detalles]

#### Posiciones y Escalas

- [ ] Correcto / [ ] Incorrecto: [detalles]

#### Valores Numéricos

- [ ] Correcto / [ ] Incorrecto: [detalles]

#### Elementos Visuales

- [ ] Completo / [ ] Faltantes: [lista]

### Correcciones Sugeridas

1. [Corrección específica con código]
2. [Corrección específica con código]
...

### Similitud Visual Estimada

[Porcentaje] - [Justificación]
```

4. **Pregunta al usuario**: ¿Deseas refinar el código o continuar al siguiente lenguaje?

## Uso

```
/comparar [lenguaje]
```

**Parámetros**:

- `lenguaje`: tikz|python|r (opcional, compara el último generado si no se especifica)

## Referencias

- `skills/comparar-visual.md` - Skill de comparación detallada

