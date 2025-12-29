# Skill: Gestión de Estado del Workflow

## Descripción

Habilidad especializada para gestionar el estado persistente del workflow de Graficador-Experto, permitiendo tracking del progreso, recuperación ante interrupciones y trazabilidad completa del proceso.

## Objetivos

- Mantener estado persistente del workflow en `outputs/workflow_state.json`
- Registrar transiciones de fase y progreso por lenguaje
- Trackear iteraciones y similitudes históricas
- Validar transiciones de estado
- Proporcionar consultas de estado actual

## Funciones Principales

### 1. Inicializar Estado

**Cuándo usar**: Al ejecutar `/analizar-imagen` por primera vez

**Proceso**:

1. Crear archivo `outputs/workflow_state.json` con estructura inicial
2. Establecer `timestamp_inicio` con timestamp ISO 8601 actual
3. Establecer `fase_actual` como "analisis"
4. Inicializar todos los lenguajes (tikz, python, r) con estado "pendiente"
5. Establecer `analisis_completado` como `false`

**Estructura inicial**:

```json
{
  "version": "1.0",
  "timestamp_inicio": "2025-12-29T10:30:00Z",
  "timestamp_ultima_actualizacion": "2025-12-29T10:30:00Z",
  "imagen_original": "outputs/original.png",
  "fase_actual": "analisis",
  "analisis_completado": false,
  "tikz": {
    "estado": "pendiente",
    "iteracion_actual": 0,
    "similitud_actual": 0,
    "similitud_historico": []
  },
  "python": {
    "estado": "pendiente",
    "iteracion_actual": 0,
    "similitud_actual": 0,
    "similitud_historico": []
  },
  "r": {
    "estado": "pendiente",
    "iteracion_actual": 0,
    "similitud_actual": 0,
    "similitud_historico": []
  }
}
```

### 2. Marcar Análisis Completado

**Cuándo usar**: Después de completar el análisis inicial en `/analizar-imagen`

**Proceso**:

1. Leer `outputs/workflow_state.json`
2. Establecer `analisis_completado` como `true`
3. Actualizar `timestamp_ultima_actualizacion`
4. Guardar archivo

### 3. Iniciar Fase de Lenguaje

**Cuándo usar**: Al ejecutar `/generar-tikz`, `/generar-python` o `/generar-r`

**Proceso**:

1. Leer estado actual
2. Validar que el análisis esté completo (si es necesario)
3. Actualizar estado del lenguaje correspondiente:
   - `estado`: "en_iteracion"
   - `iteracion_actual`: 1 (primera iteración)
   - `timestamp_inicio`: timestamp actual
4. Actualizar `fase_actual` según lenguaje:
   - TikZ: "tikz_iteracion"
   - Python: "python_iteracion"
   - R: "r_iteracion"
5. Actualizar `timestamp_ultima_actualizacion`
6. Guardar archivo

**Validaciones**:

- TikZ puede iniciarse después de análisis
- Python requiere TikZ validado o al menos iniciado
- R requiere Python validado o al menos iniciado

### 4. Registrar Iteración

**Cuándo usar**: Al ejecutar `/iterar` o después de cada refinamiento

**Proceso**:

1. Leer estado actual
2. Identificar lenguaje activo según `fase_actual`
3. Incrementar `iteracion_actual` del lenguaje correspondiente
4. Actualizar `timestamp_ultima_actualizacion`
5. Guardar archivo

### 5. Registrar Similitud

**Cuándo usar**: Después de ejecutar `/comparar`

**Proceso**:

1. Leer estado actual
2. Identificar lenguaje evaluado
3. Actualizar `similitud_actual` del lenguaje correspondiente
4. Añadir valor al array `similitud_historico`
5. Actualizar `timestamp_ultima_actualizacion`
6. Guardar archivo

**Ejemplo**:

```json
"tikz": {
  "estado": "en_iteracion",
  "iteracion_actual": 3,
  "similitud_actual": 89,
  "similitud_historico": [75, 82, 89]
}
```

### 6. Validar Lenguaje

**Cuándo usar**: Cuando el usuario valida un lenguaje (similitud ≥ 95% o validación manual)

**Proceso**:

1. Leer estado actual
2. Actualizar estado del lenguaje:
   - `estado`: "validado"
   - `timestamp_validacion`: timestamp actual
3. Actualizar `fase_actual`:
   - TikZ validado: "tikz_validado"
   - Python validado: "python_validado"
   - R validado: "r_validado"
4. Si R está validado, establecer `fase_actual` como "completado"
5. Actualizar `timestamp_ultima_actualizacion`
6. Guardar archivo

### 7. Consultar Estado Actual

**Cuándo usar**: Para el comando `/estado` o para verificar progreso

**Proceso**:

1. Leer `outputs/workflow_state.json`
2. Calcular tiempo transcurrido desde `timestamp_inicio`
3. Determinar progreso por lenguaje (emojis):
   - ✅ Validado: `estado === "validado"`
   - 🟡 En iteración: `estado === "en_iteracion"`
   - ⚪ Pendiente: `estado === "pendiente"`
4. Identificar próximos pasos sugeridos según fase actual
5. Listar archivos generados según estado

**Formato de salida**:

```markdown
## Estado del Workflow

### Progreso General
🟢 TikZ: VALIDADO (4 iteraciones, 96% similitud)
🟡 Python: EN ITERACIÓN (2 iteraciones, 88% similitud)
⚪ R: PENDIENTE

### Fase Actual
Python - Iteración 2

### Tiempo Transcurrido
- Inicio: 2025-12-29 10:30:00
- Duración: 1h 15min

### Próximos Pasos Sugeridos
1. Ejecutar `/comparar python` para evaluar iteración actual
2. Si similitud < 95%: Ejecutar `/iterar python`
3. Si similitud ≥ 95%: Ejecutar `/generar-r` para continuar
```

## Validaciones de Transición

### Reglas de Flujo

1. **Análisis → TikZ**: Siempre permitido después de análisis completo
2. **TikZ → Python**: Permitido si TikZ está validado o en iteración (flexible)
3. **Python → R**: Permitido si Python está validado o en iteración (flexible)
4. **Validación**: Solo si `similitud_actual ≥ 95` o validación manual del usuario

### Validación de Estado

Antes de cualquier transición, verificar:

- ¿El estado anterior es válido?
- ¿Se cumplen las condiciones de transición?
- ¿Los datos son consistentes?

## Manejo de Errores

### Estado No Encontrado

Si `outputs/workflow_state.json` no existe:

1. Inicializar estado desde cero
2. Continuar con el proceso normal

### Estado Corrupto

Si el JSON no es válido:

1. Intentar recuperar datos críticos
2. Reinicializar con datos parciales si es posible
3. Informar al usuario del problema

### Recuperación Ante Interrupciones

Si el workflow se interrumpe:

1. Leer estado persistente
2. Identificar última fase completada
3. Sugerir continuar desde donde se quedó
4. Validar que archivos necesarios existan

## Integración con Otros Skills

### Con `analizar-imagen-matematica`

- Inicializar estado después del análisis
- Guardar referencia a `analisis_inicial.json`

### Con `comparar-visual`

- Registrar similitud después de cada comparación
- Actualizar historial de mejoras

### Con `refinar-codigo`

- Incrementar contador de iteración
- Registrar cambios aplicados

## Ejemplos de Uso

### Ejemplo 1: Inicialización

```python
# Pseudocódigo
estado = inicializar_estado()
estado.timestamp_inicio = datetime.now().isoformat()
estado.fase_actual = "analisis"
guardar_estado(estado)
```

### Ejemplo 2: Iniciar Generación TikZ

```python
estado = leer_estado()
estado.tikz.estado = "en_iteracion"
estado.tikz.iteracion_actual = 1
estado.tikz.timestamp_inicio = datetime.now().isoformat()
estado.fase_actual = "tikz_iteracion"
guardar_estado(estado)
```

### Ejemplo 3: Registrar Similitud

```python
estado = leer_estado()
similitud = 89  # De comparación visual
estado.tikz.similitud_actual = similitud
estado.tikz.similitud_historico.append(similitud)
guardar_estado(estado)
```

### Ejemplo 4: Validar Lenguaje

```python
estado = leer_estado()
if estado.tikz.similitud_actual >= 95:
    estado.tikz.estado = "validado"
    estado.tikz.timestamp_validacion = datetime.now().isoformat()
    estado.fase_actual = "tikz_validado"
    guardar_estado(estado)
```

## Referencias

- `.claude/schemas/workflow_state.schema.json` - Esquema JSON del estado
- `outputs/workflow_state.json` - Archivo de estado persistente
- `.claude/commands/estado.md` - Comando de visualización de estado

