---
name: validar-coherencia
description: Verificar coherencia matemática, imagen-texto, y código R/Python/TikZ en ejercicios .Rmd.
---

# Skill: Validador de Coherencia

## Propósito
Detectar y corregir incoherencias entre los diferentes componentes de un ejercicio R/exams: matemáticas, visualización y código.

## Tipos de Coherencia

### 1. Coherencia Matemática (ERR_C1)

**Verificar**:
- Fórmulas aplicadas correctamente
- Cálculos intermedios válidos
- Resultado final correcto
- Opciones de respuesta coherentes

**Checklist**:
```
□ La fórmula usada es correcta para el problema
□ Los valores numéricos están en rangos válidos
□ La respuesta correcta coincide con el cálculo
□ Los distractores son matemáticamente plausibles
□ No hay errores de redondeo significativos
```

**Ejemplo de error**:
```r
# Problema: Calcular área de círculo
radio <- 5
area <- pi * radio  # ❌ Error: debería ser pi * radio^2
```

**Corrección**:
```r
radio <- 5
area <- pi * radio^2  # ✅ Correcto
```

### 2. Coherencia Imagen-Texto (ERR_C2)

**Verificar**:
- Descripción textual coincide con el gráfico
- Valores etiquetados en imagen = valores en texto
- Colores/formas mencionados = colores/formas mostrados

**Checklist**:
```
□ Las dimensiones en el texto coinciden con el gráfico
□ Los colores descritos son los colores mostrados
□ La orientación/posición descrita es correcta
□ Las etiquetas del gráfico son legibles
□ La escala del gráfico es apropiada para los valores
```

**Ejemplo de error**:
```r
# Texto dice: "un cilindro de radio 5 cm"
# TikZ genera: \def\radio{3}  # ❌ Incoherencia
```

**Corrección**:
```r
radio <- 5  # Variable R
tikz_code <- paste0("\\def\\radio{", radio, "}")  # ✅ Sincronizado
```

### 3. Coherencia de Código (ERR_C3)

**Verificar**:
- Variables R sincronizadas con Python
- Variables R sincronizadas con TikZ
- Formato de números consistente
- Tipos de datos correctos

**Checklist**:
```
□ Variables numéricas usadas antes de formatear
□ Transferencia R→Python correcta (r.variable)
□ Transferencia R→TikZ correcta (paste0)
□ No hay funciones matemáticas sobre strings
□ Semilla aleatoria genera datos válidos
```

**Ejemplo de error**:
```r
b <- -2.5
b_formateado <- sprintf("%.1f", b)
ecuacion <- paste0("y = x - ", abs(b_formateado))  # ❌ abs() sobre string
```

**Corrección**:
```r
b <- -2.5
b_abs <- abs(b)  # ✅ abs() sobre número
b_formateado <- sprintf("%.1f", b_abs)
ecuacion <- paste0("y = x - ", b_formateado)
```

## Algoritmo de Validación

```r
validar_coherencia <- function(archivo_rmd) {
  contenido <- readLines(archivo_rmd)
  errores <- list()
  
  # 1. Buscar funciones matemáticas sobre variables formateadas
  patron_abs <- "abs\\([^)]*formateado"
  if (any(grepl(patron_abs, contenido))) {
    errores <- c(errores, "ERR_C3: abs() sobre variable formateada")
  }
  
  # 2. Verificar sincronización R-TikZ
  vars_r <- extraer_variables_r(contenido)
  vars_tikz <- extraer_variables_tikz(contenido)
  if (!all(vars_tikz %in% names(vars_r))) {
    errores <- c(errores, "ERR_C2: Variables TikZ no definidas en R")
  }
  
  # 3. Verificar metadatos
  if (!any(grepl("^exsolution:", contenido))) {
    errores <- c(errores, "ERR_C1: exsolution no definida")
  }
  
  return(errores)
}
```

## Flujo de Validación

```
Cargar archivo .Rmd
    ↓
Extraer chunks de código (R, Python, TikZ)
    ↓
Verificar coherencia matemática
    ↓
Verificar coherencia imagen-texto
    ↓
Verificar coherencia de código
    ↓
Generar reporte de coherencia
    ↓
Si hay errores → Sugerir correcciones
```

## Reporte de Coherencia

```
╔════════════════════════════════════════╗
║     REPORTE DE COHERENCIA              ║
╠════════════════════════════════════════╣
║ Coherencia Matemática:    ✅ OK        ║
║ Coherencia Imagen-Texto:  ⚠️ 1 error   ║
║   → Línea 45: radio=5 vs TikZ radio=3  ║
║ Coherencia de Código:     ✅ OK        ║
╠════════════════════════════════════════╣
║ Estado: REQUIERE CORRECCIÓN            ║
╚════════════════════════════════════════╝
```

## Integración

- **Se ejecuta**: Después de validar-renderizado exitoso
- **Activado por**: diagnosticar-errores cuando categoría = COHERENCIA
- **Siguiente paso**: Corrección manual o automática según tipo

## Referencias

- `.claude/docs/patrones-errores-conocidos.md` (Error 2)
- `.augment/rules/reglas-generales.md` (criterios ICFES)

