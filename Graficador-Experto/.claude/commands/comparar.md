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

3. **Calcular Métricas Cuantitativas**:

Usa el sistema de puntuación cuantitativa según `.claude/schemas/metricas_similitud.schema.json`:

**Categorías y Puntuación**:

- **Colores (0-20 puntos)**:
  - Todos los colores coinciden exactamente: 20 pts
  - Colores similares (diferencia < 10% RGB): 15 pts
  - Algunos colores incorrectos: 10 pts
  - Colores muy diferentes: 5 pts
  - Colores completamente incorrectos: 0 pts

- **Posiciones y Coordenadas (0-20 puntos)**:
  - Todas las coordenadas exactas: 20 pts
  - Diferencias < 5% del rango: 15 pts
  - Diferencias 5-10% del rango: 10 pts
  - Diferencias 10-20% del rango: 5 pts
  - Diferencias > 20% del rango: 0 pts

- **Valores Numéricos (0-20 puntos)**:
  - Todos los valores correctos: 20 pts
  - 1-2 valores incorrectos: 15 pts
  - 3-4 valores incorrectos: 10 pts
  - 5+ valores incorrectos: 5 pts
  - Valores críticos incorrectos: 0 pts

- **Proporciones y Escalas (0-15 puntos)**:
  - Proporciones perfectas: 15 pts
  - Diferencias menores: 10 pts
  - Diferencias moderadas: 5 pts
  - Proporciones incorrectas: 0 pts

- **Estilos (0-15 puntos)**:
  - Todos los estilos coinciden: 15 pts
  - Estilos similares: 10 pts
  - Algunos estilos incorrectos: 5 pts
  - Estilos muy diferentes: 0 pts

- **Elementos (0-10 puntos)**:
  - Todos los elementos presentes: 10 pts
  - 1 elemento faltante/extra: 7 pts
  - 2-3 elementos faltantes/extra: 4 pts
  - 4+ elementos faltantes/extra: 0 pts

**Puntuación Total**: Suma de todas las categorías (0-100 puntos)

**Recomendación basada en puntuación**:
- 95-100: ✅ **Validar** - Excelente similitud
- 85-94: ⚠️ **Considerar validar o iterar** - Bueno, mejoras menores posibles
- 70-84: ⚠️ **Iterar** - Regular, necesita refinamiento
- < 70: ❌ **Iterar o regenerar** - Pobre, requiere correcciones mayores

4. **Actualizar Estado del Workflow**:

- Leer `outputs/workflow_state.json`
- Actualizar `[lenguaje].similitud_actual` con puntuación total
- Añadir puntuación al array `[lenguaje].similitud_historico`
- Actualizar `timestamp_ultima_actualizacion`
- Guardar estado

5. **Genera reporte de comparación**:

```markdown
## Comparación Visual - [Lenguaje] - Iteración [N]

### Puntuación Cuantitativa

**Similitud Total: [X]/100 puntos**

| Categoría | Puntuación | Criterio Aplicado |
|-----------|------------|-------------------|
| Colores | [X]/20 | [criterio] |
| Posiciones | [X]/20 | [criterio] |
| Valores | [X]/20 | [criterio] |
| Proporciones | [X]/15 | [criterio] |
| Estilos | [X]/15 | [criterio] |
| Elementos | [X]/10 | [criterio] |

### Recomendación

[✅ Validar / ⚠️ Considerar validar o iterar / ⚠️ Iterar / ❌ Iterar o regenerar]

[Justificación basada en puntuación y detalles]

### Diferencias Identificadas

#### Colores

- Puntuación: [X]/20
- [Detalles específicos]

#### Posiciones y Escalas

- Puntuación: [X]/20
- [Detalles específicos]

#### Valores Numéricos

- Puntuación: [X]/20
- [Detalles específicos]

#### Elementos Visuales

- Puntuación: [X]/10
- [Lista de elementos faltantes/extra]

### Correcciones Sugeridas

1. [Corrección específica con código]
2. [Corrección específica con código]
...

### Historial de Similitud

[Mostrar array similitud_historico como gráfico de progreso]
```

6. **Actualizar Documentación Incremental**:

- Añadir sección "Iteración [N] - [Lenguaje]" en `outputs/reporte_matematico.md`
- Incluir puntuación cuantitativa y recomendación
- Documentar diferencias identificadas y correcciones sugeridas

7. **Pregunta al usuario**: 

Según la recomendación:
- Si **Validar**: ¿Deseas validar esta versión y continuar al siguiente lenguaje?
- Si **Considerar validar o iterar**: ¿Deseas validar o refinar el código?
- Si **Iterar**: ¿Deseas refinar el código basándote en las correcciones sugeridas?
- Si **Iterar o regenerar**: ¿Deseas refinar o regenerar desde cero?

## Uso

```
/comparar [lenguaje]
```

**Parámetros**:

- `lenguaje`: tikz|python|r (opcional, compara el último generado si no se especifica)

## Referencias

- `skills/comparar-visual/skill.md` - Skill de comparación detallada con métricas cuantitativas
- `skills/gestionar-estado/skill.md` - Skill de gestión de estado del workflow
- `.claude/schemas/metricas_similitud.schema.json` - Esquema del sistema de puntuación

