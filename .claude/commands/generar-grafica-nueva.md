---
description: Genera nueva gráfica TikZ usando el workflow completo del Graficador-Experto y la guarda en el repositorio centralizado.
---

# Generar Gráfica TikZ Nueva

Genera código TikZ de alta precisión (98%+ fidelidad visual) a partir de una imagen matemática usando el workflow completo del Graficador-Experto, y lo guarda automáticamente en el repositorio centralizado para reutilización futura.

## Parámetros de entrada

- **$ARGUMENTS**: Ruta a imagen matemática o descripción del tipo de gráfica necesaria

## Propósito

Este comando ejecuta el workflow completo del Graficador-Experto:

1. Analiza imagen con Claude Vision
2. Genera código TikZ iterativo con validación visual
3. Guarda en `Repositorio-Graficas-TikZ/` con metadata completa
4. Retorna código listo para usar en ejercicios .Rmd

## Flujo Completo

### Paso 1: Análisis de Imagen

Si se proporciona imagen:

1. **Analizar con Claude Vision**:
   - Identificar tipo de contenido matemático
   - Extraer elementos visuales (formas, colores, etiquetas)
   - Detectar parámetros variables (dimensiones, valores)

2. **Clasificar gráfica**:
   - Categoría: geometria, estadistica, probabilidad
   - Subcategoría: cilindros, barras, arboles_decision, etc.
   - Componente ICFES: geometrico_metrico, aleatorio, etc.

### Paso 2: Generación Iterativa de TikZ

**Ciclo de refinamiento** (máximo 5 iteraciones):

1. **Generar código TikZ inicial**:
   - Consultar `/A-Produccion/Ejemplos-Funcionales-Rmd/` para patrones
   - Usar agente `graficador-tikz` para generar código
   - Incluir placeholders parametrizables (`%%PARAMETRO%%`)

2. **Renderizar y comparar**:
   - Compilar TikZ a PDF usando pdflatex
   - Convertir PDF a PNG
   - Comparar con imagen original usando Claude Vision
   - Calcular similitud visual (objetivo: >95%)

3. **Validar y refinar**:
   - Si similitud < 95%: Identificar diferencias y refinar
   - Si similitud ≥ 95%: Continuar a Paso 3
   - Si no mejora después de 3 iteraciones: Continuar con advertencia

### Paso 3: Parametrización

**Convertir valores fijos a placeholders**:

```latex
% Antes (valores fijos)
\def\radioValor{3}
\def\alturaValor{5}

% Después (parametrizable)
\def\radioValor{%%RADIO%%}
\def\alturaValor{%%ALTURA%%}
```

**Identificar parámetros**:
- Extraer valores que deben ser variables
- Documentar tipo y rango esperado
- Crear lista de parámetros para metadata

### Paso 4: Guardado en Repositorio

**Estructura de archivos**:

```
Repositorio-Graficas-TikZ/
└── [categoria]/
    └── [subcategoria]/
        ├── [nombre].tikz      # Código TikZ con placeholders
        ├── [nombre].json      # Metadata completa
        └── [nombre].png       # Preview renderizado
```

**Generar metadata JSON**:

```json
{
  "id": "[nombre_unico]",
  "categoria": "[geometria|estadistica|probabilidad]",
  "subcategoria": "[cilindros|barras|arboles_decision|...]",
  "descripcion": "Descripción clara y específica de la gráfica",
  "tags": ["tag1", "tag2", "tag3"],
  "parametros": [
    {
      "nombre": "radio",
      "tipo": "numero",
      "descripcion": "Radio del cilindro en cm",
      "rango": [1, 10]
    }
  ],
  "componente_icfes": "[geometrico_metrico|aleatorio|...]",
  "fecha_creacion": "[YYYY-MM-DD]",
  "validado": true,
  "similitud_visual": 0.98,
  "origen": "graficador-experto",
  "iteraciones": 3
}
```

**Convención de nomenclatura**:
- Formato: `[tipo]_[variante]_[numero].tikz`
- Ejemplos:
  - `cilindro_basico_01.tikz`
  - `grafica_barras_horizontal_02.tikz`
  - `arbol_decision_binario_01.tikz`

### Paso 5: Actualizar Índice

**Actualizar `Repositorio-Graficas-TikZ/indice.json`**:

1. Agregar entrada en array `graficas`
2. Incrementar contador de categoría/subcategoría
3. Actualizar `total_graficas`
4. Actualizar `fecha_actualizacion`

### Paso 6: Crear Preview PNG

**Renderizar preview**:

```bash
# Compilar TikZ con valores de ejemplo
pdflatex [nombre].tikz

# Convertir PDF a PNG
convert [nombre].pdf [nombre].png -density 150
```

**Valores de ejemplo para preview**:
- Usar valores medios del rango de parámetros
- Asegurar que preview sea representativo

### Paso 7: Retornar Código para Uso

**Retornar información**:

```
✅ Gráfica TikZ generada y guardada exitosamente

📁 Ubicación: Repositorio-Graficas-TikZ/[categoria]/[subcategoria]/[nombre].tikz
📊 Metadata: [nombre].json
🖼️ Preview: [nombre].png

📋 Parámetros disponibles:
   - radio: número (1-10)
   - altura: número (1-15)

💡 Para usar en ejercicio .Rmd:
   1. Consultar con skill consultar-grafica-tikz
   2. Integrar función generar_tikz_[tipo]() en data_generation
   3. Usar renderizado condicional en Question
```

## Ejemplo de Uso

```
Usuario: [Adjunta imagen de cilindro]
Usuario: /generar-grafica-nueva

Claude: Analizando imagen...
        Categoría detectada: Geometría - Cilindros
        Componente ICFES: geometrico_metrico
        
        Generando código TikZ...
        [Iteración 1] Similitud: 87%
        [Iteración 2] Similitud: 94%
        [Iteración 3] Similitud: 98% ✅
        
        Parametrizando código...
        Parámetros identificados: radio, altura
        
        Guardando en repositorio...
        ✅ Guardado: Repositorio-Graficas-TikZ/geometria/cilindros/cilindro_basico_01.tikz
        
        Índice actualizado.
        Preview generado.
        
        La gráfica está lista para usar en ejercicios.
```

## Integración con Workflow Principal

Este comando puede ser invocado:

1. **Automáticamente**: Cuando `generar-schoice` o `generar-cloze` no encuentran gráfica adecuada
2. **Manualmente**: Cuando usuario necesita crear nueva gráfica específica
3. **Desde Graficador-Experto**: Cuando se genera TikZ en `outputs/` y se quiere guardar en repositorio

## Validaciones Críticas

Antes de guardar, verificar:

- ✅ Código TikZ compila sin errores
- ✅ Similitud visual ≥ 95% (o advertencia si menor)
- ✅ Placeholders correctamente formateados (`%%PARAMETRO%%`)
- ✅ Metadata JSON válida y completa
- ✅ Preview PNG generado correctamente
- ✅ Índice actualizado correctamente

## Referencias

- `.claude/agents/graficador-tikz.md` - Agente especializado en TikZ
- `Repositorio-Graficas-TikZ/README.md` - Documentación del repositorio
- `.claude/skills/consultar-grafica-tikz/skill.md` - Consultar gráficas existentes
- `/A-Produccion/Ejemplos-Funcionales-Rmd/` - Patrones funcionales de TikZ
- `Graficador-Experto/README.md` - Documentación del Graficador-Experto

## Notas Técnicas

- **Renderizado**: Usa `pdflatex` para compilar TikZ
- **Conversión**: Requiere ImageMagick o Poppler para PDF→PNG
- **Encoding**: Archivos TikZ deben usar UTF-8
- **Placeholders**: Formato `%%PARAMETRO%%` (doble porcentaje)
- **Límite iteraciones**: Máximo 5 iteraciones de refinamiento

