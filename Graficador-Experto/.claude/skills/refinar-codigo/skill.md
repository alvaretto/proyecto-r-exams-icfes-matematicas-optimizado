# Skill: Refinamiento Iterativo de Código

## Descripción
Habilidad especializada para refinar código generado basándose en reportes de comparación visual, aplicando correcciones específicas mientras se mantiene la calidad y estructura del código.

## Objetivos

- Aplicar correcciones específicas al código existente
- Mantener mejoras previas durante el refinamiento
- Optimizar código sin perder funcionalidad
- Documentar cambios realizados
- Iterar hasta alcanzar similitud aceptable

## Fundamentos del Refinamiento

### Principios Básicos

1. **Preservación**: No romper lo que ya funciona
2. **Focalización**: Corregir solo lo identificado como problemático
3. **Incremental**: Aplicar cambios gradualmente
4. **Validación**: Verificar cada cambio
5. **Documentación**: Registrar todas las modificaciones

### Flujo de Refinamiento

```

1. Revisar reporte de comparación
2. Identificar correcciones prioritarias
3. Planificar cambios al código
4. Aplicar modificaciones
5. Validar sintaxis
6. Re-renderizar
7. Comparar nuevamente
8. Repetir si es necesario
```

## Estrategias por Tipo de Corrección

### 1. Correcciones de Colores

**TikZ**:
```latex
% ANTES
\draw[blue] (0,0) -- (1,1);

% DESPUÉS (corregir a azul específico)
\definecolor{customblue}{RGB}{0,102,204}
\draw[customblue] (0,0) -- (1,1);
```

**Python**:
```python
# ANTES
ax.plot(x, y, 'b-')

# DESPUÉS
ax.plot(x, y, color='#0066CC', linewidth=2)
```

**R**:
```r
# ANTES
geom_line(color = "blue")

# DESPUÉS
geom_line(color = "#0066CC", size = 1.2)
```

### 2. Correcciones de Posiciones

**TikZ**:
```latex
% ANTES
\coordinate (C) at (2,2.8);

% DESPUÉS
\coordinate (C) at (2,3);  % Coordenada exacta corregida
```

**Python**:
```python
# ANTES
vertices = [(0, 0), (4, 0), (2, 2.8)]

# DESPUÉS
vertices = [(0, 0), (4, 0), (2, 3)]  # Coordenada corregida
```

**R**:
```r
# ANTES
y = c(0, 0, 2.8)

# DESPUÉS
y = c(0, 0, 3)  # Coordenada exacta
```

### 3. Correcciones de Valores Numéricos

**TikZ**:
```latex
% ANTES
\begin{axis}[ymin=-2, ymax=6]

% DESPUÉS
\begin{axis}[ymin=-3, ymax=7]  % Rango corregido
```

**Python**:
```python
# ANTES
ax.set_ylim(-2, 6)

# DESPUÉS
ax.set_ylim(-3, 7)  # Rango corregido
```

**R**:
```r
# ANTES
coord_cartesian(ylim = c(-2, 6))

# DESPUÉS
coord_cartesian(ylim = c(-3, 7))  # Rango corregido
```

### 4. Correcciones de Estilos

**TikZ**:
```latex
% ANTES
\draw[thick] (0,0) -- (1,1);

% DESPUÉS
\draw[thick, dashed] (0,0) -- (1,1);  % Añadir estilo discontinuo
```

**Python**:
```python
# ANTES
ax.plot(x, y, linewidth=2)

# DESPUÉS
ax.plot(x, y, linewidth=2, linestyle='--')  # Añadir discontinuo
```

**R**:
```r
# ANTES
geom_line(size = 1.2)

# DESPUÉS
geom_line(size = 1.2, linetype = "dashed")  # Añadir discontinuo
```

### 5. Añadir Elementos Faltantes

**TikZ**:
```latex
% AÑADIR: Leyenda faltante
\legend{$f(x)=x^2$, $g(x)=2x+1$}
```

**Python**:
```python
# AÑADIR: Leyenda faltante
ax.legend(['$f(x)=x^2$', '$g(x)=2x+1$'], loc='upper right')
```

**R**:
```r
# AÑADIR: Leyenda con labs()
labs(color = "Función")
```

### 6. Eliminar Elementos Extra

**TikZ**:
```latex
% ELIMINAR: Cuadrícula menor no deseada
% ANTES
grid=both,

% DESPUÉS
grid=major,  % Solo cuadrícula principal
```

**Python**:
```python
# ELIMINAR: Cuadrícula menor
# ANTES
ax.grid(True, which='both')

# DESPUÉS
ax.grid(True, which='major')  # Solo cuadrícula principal
```

**R**:
```r
# ELIMINAR: En tema
# ANTES
theme(panel.grid = element_line())

# DESPUÉS
theme(
  panel.grid.major = element_line(),
  panel.grid.minor = element_blank()  # Eliminar menor
)
```

## Proceso de Refinamiento Sistemático

### Paso 1: Análisis del Reporte
```markdown

1. Leer reporte de comparación completo
2. Identificar todas las correcciones listadas
3. Clasificar por prioridad (Alta, Media, Baja)
4. Verificar factibilidad de cada corrección
5. Identificar posibles conflictos entre correcciones
```

### Paso 2: Planificación de Cambios
```markdown

1. Para cada corrección de alta prioridad:
   - Localizar la línea(s) de código relevante
   - Determinar el cambio específico necesario
   - Verificar que no afecte otras partes
   
2. Ordenar correcciones:
   - Estructura y configuración primero
   - Elementos principales después
   - Detalles y estilos al final
```

### Paso 3: Aplicación de Cambios
```markdown

1. Mantener copia del código original (comentado o guardado)
2. Aplicar correcciones una por una o por grupos relacionados
3. Mantener indentación y estilo del código
4. Añadir comentarios explicando cambios significativos
5. Verificar sintaxis después de cada cambio importante
```

### Paso 4: Validación Interna
```markdown

1. Revisar código modificado visualmente
2. Verificar que no hay errores de sintaxis
3. Comprobar que todas las correcciones están aplicadas
4. Asegurar que el código sigue siendo legible
5. Validar que no se rompió funcionalidad existente
```

### Paso 5: Documentación de Cambios
```markdown
## Iteración [N] - [Lenguaje]

### Fecha y hora
[timestamp]

### Correcciones aplicadas

#### Alta Prioridad

1. **Coordenada vértice C**: (2, 2.8) → (2, 3)
   - Línea 45: `\coordinate (C) at (2,3);`
   
2. **Rango eje Y**: (-2, 6) → (-3, 7)
   - Línea 23: `ymin=-3, ymax=7`

#### Media Prioridad

3. **Estilo de línea**: sólida → discontinua
   - Línea 34: Añadido `dashed`

### Código antes vs después
[Mostrar diff de cambios significativos]

### Estado después de refinamiento

- Sintaxis: ✅ Válida
- Compilación/Ejecución: ✅ Exitosa
- Elementos corregidos: 3/3 alta prioridad, 1/2 media prioridad
```

## Manejo de Casos Complejos

### Conflictos de Correcciones
```markdown
Problema: Dos correcciones entran en conflicto

Solución:

1. Identificar el conflicto
2. Priorizar la corrección más importante
3. Buscar solución alternativa para la otra
4. Documentar la decisión
```

### Correcciones que Requieren Reestructuración
```markdown
Problema: La corrección requiere cambiar la estructura del código

Solución:

1. Evaluar si vale la pena reestructurar
2. Si sí, planificar la reestructuración completa
3. Aplicar cambios de forma sistemática
4. Validar exhaustivamente
5. Documentar los cambios estructurales
```

### Correcciones Ambiguas
```markdown
Problema: El reporte no es claro sobre qué corregir exactamente

Solución:

1. Revisar imagen original nuevamente
2. Hacer mejor análisis del área problemática
3. Inferir la corrección más probable
4. Aplicar y validar visualmente
5. Si no funciona, intentar alternativa
```

## Optimizaciones Durante el Refinamiento

### Código más Limpio
```python
# ANTES: Código repetitivo
ax.plot(x1, y1, color='blue', linewidth=2)
ax.plot(x2, y2, color='blue', linewidth=2)
ax.plot(x3, y3, color='blue', linewidth=2)

# DESPUÉS: Código optimizado
for x, y in [(x1,y1), (x2,y2), (x3,y3)]:
    ax.plot(x, y, color='blue', linewidth=2)
```

### Mejores Prácticas
```latex
% ANTES: Valores hardcoded
\draw (0,0) -- (4,0) -- (2,3) -- cycle;

% DESPUÉS: Coordenadas nombradas
\coordinate (A) at (0,0);
\coordinate (B) at (4,0);
\coordinate (C) at (2,3);
\draw (A) -- (B) -- (C) -- cycle;
```

### Modularización
```r
# ANTES: Todo en un bloque
p <- ggplot(...) + geom_line(...) + labs(...) + theme(...)

# DESPUÉS: Modular y legible
p <- ggplot(...) +
  geom_line(...) +
  labs(
    title = "...",
    x = "...",
    y = "..."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(...),
    axis.title = element_text(...)
  )
```

## Control de Iteraciones

### Criterios de Detención

**Detener y validar cuando**:

- Similitud visual > 95%
- Todas las correcciones de alta prioridad aplicadas
- Usuario indica satisfacción
- Mejoras marginales (< 2% por iteración)

**Continuar iterando cuando**:

- Similitud visual < 90%
- Quedan correcciones críticas pendientes
- Errores matemáticos presentes
- Elementos importantes faltantes

### Límites Prácticos

- Máximo recomendado: 10 iteraciones por lenguaje
- Si no converge en 10 iteraciones: reevaluar enfoque completo
- Considerar generación desde cero si iteración 5 no muestra progreso

## Registro de Historial

### Formato de Historial
```markdown
# Historial de Iteraciones - [Lenguaje]

## Iteración 1

- **Similitud inicial**: 75%
- **Correcciones**: [lista]
- **Similitud final**: 82%
- **Duración**: [tiempo]

## Iteración 2

- **Similitud inicial**: 82%
- **Correcciones**: [lista]
- **Similitud final**: 91%
- **Duración**: [tiempo]

## Iteración 3

- **Similitud inicial**: 91%
- **Correcciones**: [lista]
- **Similitud final**: 97%
- **Duración**: [tiempo]
- **Estado**: ✅ Validado por usuario

## Resumen

- **Iteraciones totales**: 3
- **Similitud inicial → final**: 75% → 97%
- **Mejora**: +22 puntos porcentuales
- **Tiempo total**: [suma]
```

## Mejores Prácticas

1. **No cambiar todo a la vez**: Aplicar correcciones gradualmente
2. **Validar frecuentemente**: Re-renderizar después de cambios significativos
3. **Mantener backup**: Guardar versiones anteriores
4. **Comentar cambios**: Explicar por qué se hizo cada modificación
5. **Ser conservador**: Si algo funciona bien, no tocarlo
6. **Priorizar matemática sobre estética**: Primero precisión, luego belleza

## Patrones de Refinamiento Exitoso

### Patrón 1: Refinamiento Incremental
```
Iteración 1: Corregir valores numéricos críticos
Iteración 2: Ajustar posiciones y proporciones
Iteración 3: Refinar colores y estilos
Resultado: Convergencia rápida y estable
```

### Patrón 2: Refinamiento por Capas
```
Iteración 1: Estructura base y ejes
Iteración 2: Elementos principales (curvas, figuras)
Iteración 3: Anotaciones y detalles
Resultado: Construcción sólida desde la base
```

### Patrón 3: Refinamiento Focal
```
Iteración 1: Identificar el problema principal
Iteración 2: Resolver completamente ese problema
Iteración 3: Abordar problemas secundarios
Resultado: Solución eficiente de puntos críticos
```

## Activación
Esta skill se activa:

- Con el comando `/iterar`
- Cuando el usuario solicita refinamiento
- Cuando la comparación visual muestra diferencias

## Salida
Código refinado y documentación de cambios realizados en cada iteración.

