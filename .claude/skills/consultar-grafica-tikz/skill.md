---
description: Consulta el repositorio centralizado de gráficas TikZ para encontrar código reutilizable según categoría, tags o descripción.
---

# Skill: Consultar Gráfica TikZ

Esta skill permite buscar y recuperar código TikZ del repositorio centralizado para integrarlo en ejercicios R-Exams.

## Ubicación del Repositorio

```
Repositorio-Graficas-TikZ/
```

## Funcionalidad Principal

1. **Búsqueda por criterios múltiples**:
   - Tags (ej: "cilindro", "volumen", "3D")
   - Categoría (geometria, estadistica, probabilidad)
   - Subcategoría (cilindros, barras, arboles_decision)
   - Componente ICFES (geometrico_metrico, aleatorio, etc.)
   - Descripción (búsqueda de texto)

2. **Listado de opciones disponibles**:
   - Muestra previews (PNG) cuando están disponibles
   - Lista metadata relevante (descripción, parámetros, tags)
   - Indica si la gráfica está validada

3. **Retorno de código TikZ**:
   - Código completo con placeholders parametrizables
   - Información de parámetros requeridos
   - Ejemplo de uso

4. **Sugerencia de generación**:
   - Si no existe gráfica adecuada, sugiere usar `/generar-grafica-nueva`
   - Proporciona contexto sobre qué tipo de gráfica se necesita

## Uso en Workflow

Esta skill es invocada automáticamente por:

- `generar-schoice` cuando detecta necesidad de gráficas
- `generar-cloze` cuando detecta necesidad de gráficas
- Comandos que requieren integración de TikZ

## Proceso de Consulta

### Paso 1: Cargar Índice

```bash
# Leer índice centralizado
cat Repositorio-Graficas-TikZ/indice.json
```

### Paso 2: Filtrar por Criterios

Buscar en el array `graficas` del índice según:

- **Tags**: Intersección con tags del análisis ICFES
- **Categoría**: Coincidencia exacta con categoría detectada
- **Componente ICFES**: Coincidencia con componente del ejercicio
- **Descripción**: Búsqueda de texto libre

### Paso 3: Presentar Opciones

Para cada gráfica encontrada, mostrar:

```
[#] [nombre].tikz
    Descripción: [descripcion]
    Categoría: [categoria] > [subcategoria]
    Tags: [tag1, tag2, tag3]
    Parámetros: [param1, param2]
    Validado: ✅ / ⚠️
    Preview: [ruta a PNG si existe]
```

### Paso 4: Cargar Código Seleccionado

Si el usuario selecciona una opción:

1. Leer archivo `.tikz` correspondiente
2. Extraer metadata del `.json` asociado
3. Retornar código TikZ con placeholders intactos
4. Proporcionar información de parámetros para reemplazo

## Ejemplo de Uso

### Contexto: Generación de Ejercicio con Cilindro

```r
# Análisis ICFES detecta:
# - Componente: geometrico_metrico
# - Categoría: geometria
# - Tags: ["cilindro", "volumen", "3D"]

# Skill consulta repositorio:
consultar_grafica_tikz(
  categoria = "geometria",
  subcategoria = "cilindros",
  tags = ["cilindro", "volumen"],
  componente_icfes = "geometrico_metrico"
)

# Resultado:
# Encontradas 2 opciones:
# 1. cilindro_basico_01.tikz - Cilindro simple con radio/altura
# 2. cilindro_liquido_02.tikz - Cilindro con nivel de líquido
```

### Código TikZ Retornado

```latex
% Código con placeholders
\def\radioValor{%%RADIO%%}
\def\alturaValor{%%ALTURA%%}
\begin{tikzpicture}
  % ... código TikZ ...
\end{tikzpicture}
```

### Metadata Retornada

```json
{
  "parametros": ["radio", "altura"],
  "ejemplo_reemplazo": {
    "radio": "3",
    "altura": "5"
  }
}
```

## Integración con Generación de Ejercicios

Cuando se usa en `generar-schoice` o `generar-cloze`:

1. **Detección automática**: Si análisis ICFES indica gráficas necesarias
2. **Consulta**: Invocar esta skill con criterios del análisis
3. **Selección**: Usuario elige gráfica existente o genera nueva
4. **Integración**: Código TikZ se integra en chunk `data_generation` como función parametrizable

### Template de Función Generada

```r
generar_tikz_cilindro <- function(radio, altura) {
  # Cargar template del repositorio
  tikz_template <- readLines("Repositorio-Graficas-TikZ/geometria/cilindros/cilindro_basico_01.tikz")
  tikz_code <- paste(tikz_template, collapse = "\n")
  
  # Reemplazar placeholders
  tikz_code <- gsub("%%RADIO%%", radio, tikz_code)
  tikz_code <- gsub("%%ALTURA%%", altura, tikz_code)
  
  return(tikz_code)
}
```

## Casos Especiales

### No se Encuentran Gráficas

Si la búsqueda no retorna resultados:

```
⚠️ No se encontraron gráficas TikZ que coincidan con los criterios.

Criterios de búsqueda:
- Categoría: [categoria]
- Tags: [tags]
- Componente: [componente]

💡 Sugerencia: Usar /generar-grafica-nueva para crear una nueva gráfica
   que se guardará en el repositorio para uso futuro.
```

### Múltiples Opciones Similares

Si hay muchas opciones (más de 5):

1. Mostrar las 5 más relevantes (por score de coincidencia)
2. Ofrecer filtrar por subcategoría específica
3. Permitir búsqueda por texto libre en descripción

### Gráfica No Validada

Si se encuentra gráfica pero `validado: false`:

```
⚠️ ADVERTENCIA: Esta gráfica aún no ha sido validada visualmente.
   Se recomienda validar antes de usar en producción.
   
   ¿Continuar de todas formas? [Sí/No]
```

## Validación de Parámetros

Antes de retornar código, verificar que:

- ✅ Todos los placeholders tienen parámetros correspondientes en metadata
- ✅ Los parámetros requeridos están documentados
- ✅ El código TikZ es válido (sintaxis básica)

## Referencias

- `Repositorio-Graficas-TikZ/README.md` - Documentación del repositorio
- `.claude/commands/generar-grafica-nueva.md` - Generar nuevas gráficas
- `.claude/skills/generar-schoice/skill.md` - Integración en generación de ejercicios

## Notas Técnicas

- El índice `indice.json` debe estar actualizado (usar script `gestionar_repo_tikz.sh reindexar` si es necesario)
- Los archivos `.tikz` deben usar encoding UTF-8
- Los placeholders siguen formato `%%PARAMETRO%%` (doble porcentaje)
- Las rutas son relativas al root del proyecto

