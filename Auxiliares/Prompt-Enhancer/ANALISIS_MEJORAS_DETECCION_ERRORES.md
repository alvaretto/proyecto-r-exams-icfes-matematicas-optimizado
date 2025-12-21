# Análisis: Mejoras para Detección de Errores Comunes en prompt-enhancer.sh

**Fecha:** 2025-12-21
**Objetivo:** Analizar la posibilidad de adaptar genéricamente `prompt-enhancer.sh` para detectar y prevenir errores comunes en archivos .Rmd

---

## 📋 Resumen Ejecutivo

El script `prompt-enhancer.sh` actualmente mejora prompts añadiendo contexto del proyecto. Este análisis propone mejoras para detectar y prevenir errores comunes en archivos .Rmd, específicamente el error de aplicar funciones matemáticas sobre variables formateadas.

### Estado Actual
- ✅ Mejora prompts con contexto del proyecto
- ✅ Detecta ubicación y tipo de contexto
- ✅ Incluye reglas y documentación relevante
- ❌ No detecta errores en código R
- ❌ No valida patrones problemáticos

### Propuesta
- ✅ Agregar detección de patrones problemáticos comunes
- ✅ Incluir advertencias en el prompt mejorado
- ✅ Referenciar documentación de errores conocidos
- ✅ Sugerir correcciones automáticas cuando sea posible

---

## 🔍 Análisis del Error: abs() sobre Variable Formateada

### Patrón del Error
```r
# ❌ Patrón problemático
variable_formateada <- formatear(valor_numerico)
resultado <- abs(variable_formateada)  # Error: abs() requiere numérico
```

### Detección Posible
El patrón puede detectarse mediante:
1. **Búsqueda de expresiones regulares** en archivos .Rmd
2. **Análisis de patrones comunes** de funciones matemáticas sobre variables `*_formateado`
3. **Validación de tipos** (aunque limitado en bash)

---

## 💡 Propuestas de Mejora

### Opción 1: Detección Básica con Grep (Recomendada para inicio)

**Ventajas:**
- ✅ Implementación simple
- ✅ No requiere dependencias adicionales
- ✅ Rápida ejecución
- ✅ Puede integrarse fácilmente en el flujo actual

**Desventajas:**
- ❌ Falsos positivos posibles
- ❌ No valida contexto completo
- ❌ Limitado a patrones específicos

**Implementación propuesta:**

```bash
# ============================================================================
# FUNCIÓN: Detectar patrones problemáticos comunes en .Rmd
# ============================================================================
detect_common_errors() {
    local file_path="$1"
    local errors=""
    
    [[ ! -f "$file_path" ]] && return 0
    
    # Error 1: abs() sobre variable formateada
    if grep -n "abs([^)]*_formateado)" "$file_path" 2>/dev/null | grep -v "#.*abs"; then
        errors+="⚠️  **Error detectado**: Uso de abs() sobre variable formateada\n"
        errors+="   Patrón: abs(variable_formateada)\n"
        errors+="   Solución: Aplicar abs() sobre valor numérico, luego formatear\n"
        errors+="   Ver: .claude/docs/patrones-errores-conocidos.md#error-2\n\n"
    fi
    
    # Error 2: round() sobre variable formateada
    if grep -n "round([^)]*_formateado)" "$file_path" 2>/dev/null | grep -v "#.*round"; then
        errors+="⚠️  **Error detectado**: Uso de round() sobre variable formateada\n"
        errors+="   Patrón: round(variable_formateada)\n"
        errors+="   Solución: Aplicar round() sobre valor numérico, luego formatear\n"
        errors+="   Ver: .claude/docs/patrones-errores-conocidos.md#error-2\n\n"
    fi
    
    # Error 3: include_tikz() en chunk de generación
    if grep -n "include_tikz" "$file_path" 2>/dev/null | grep -E "generar|data_generation|generar_datos"; then
        errors+="⚠️  **Advertencia**: include_tikz() en chunk de generación\n"
        errors+="   Puede causar errores 'File not found' en compilación PDF\n"
        errors+="   Solución: Usar renderizado condicional con knitr::is_latex_output()\n"
        errors+="   Ver: .claude/docs/patrones-errores-conocidos.md#error-1\n\n"
    fi
    
    if [[ -n "$errors" ]]; then
        echo -e "## 🚨 ERRORES DETECTADOS EN ARCHIVO .RMD\n"
        echo -e "$errors"
    fi
}
```

**Integración en `enhance_prompt()`:**

```bash
enhance_prompt() {
    local user_prompt="$1"
    local project_root="$2"
    local context_type="$3"
    local relative_path="$4"
    
    # ... código existente ...
    
    # Si el prompt menciona un archivo .Rmd, detectar errores
    if echo "$user_prompt" | grep -qE "\.Rmd|\.rmd"; then
        # Intentar extraer ruta del archivo del prompt
        local rmd_file=$(echo "$user_prompt" | grep -oE "[^\s]+\.Rmd" | head -1)
        if [[ -n "$rmd_file" && -f "$rmd_file" ]]; then
            enhanced_prompt+="$(detect_common_errors "$rmd_file")"
        fi
    fi
    
    # ... resto del código ...
}
```

### Opción 2: Análisis Avanzado con R (Para implementación futura)

**Ventajas:**
- ✅ Análisis sintáctico completo
- ✅ Validación de tipos
- ✅ Detección de errores más complejos
- ✅ Integración con el ecosistema R/exams

**Desventajas:**
- ❌ Requiere R instalado
- ❌ Más lento
- ❌ Mayor complejidad

**Implementación propuesta (futura):**

```r
# Script R para análisis: analyze_rmd_errors.R
library(knitr)
library(xml2)

analyze_rmd_file <- function(file_path) {
  errors <- list()
  
  # Parsear archivo .Rmd
  chunks <- extract_chunks(file_path)
  
  for (chunk in chunks) {
    # Detectar patrones problemáticos
    if (has_math_function_on_formatted(chunk)) {
      errors <- append(errors, list(
        type = "math_function_on_formatted",
        line = chunk$line,
        pattern = extract_pattern(chunk),
        solution = get_solution("math_function_on_formatted")
      ))
    }
  }
  
  return(errors)
}
```

### Opción 3: Integración con Documentación de Errores

**Mejora al prompt mejorado:**

Cuando se detecta un error o se menciona un problema, incluir automáticamente:

```bash
# En read_claude_documentation()
read_error_patterns() {
    local project_root="$1"
    local error_file="$project_root/.claude/docs/patrones-errores-conocidos.md"
    
    if [[ -f "$error_file" ]]; then
        echo "## 🚨 ERRORES CONOCIDOS Y SOLUCIONES"
        echo ""
        # Extraer resúmenes de errores conocidos
        grep -A 5 "### ❌ Mensaje de Error" "$error_file" | head -20
        echo ""
        echo "Ver documentación completa: .claude/docs/patrones-errores-conocidos.md"
    fi
}
```

---

## 🎯 Recomendación: Implementación Gradual

### Fase 1: Detección Básica (Implementación Inmediata)
**Prioridad:** Alta
**Esfuerzo:** Bajo (2-3 horas)
**Impacto:** Medio

1. Agregar función `detect_common_errors()` con grep
2. Integrar en `enhance_prompt()` cuando se detecte mención de archivo .Rmd
3. Incluir referencias a documentación de errores conocidos

**Código a agregar:**
- Función `detect_common_errors()` (~50 líneas)
- Modificación en `enhance_prompt()` (~10 líneas)
- Función `read_error_patterns()` (~20 líneas)

### Fase 2: Mejora de Detección (Implementación Futura)
**Prioridad:** Media
**Esfuerzo:** Medio (1-2 días)
**Impacto:** Alto

1. Expandir patrones de detección
2. Agregar validación de contexto
3. Mejorar precisión (reducir falsos positivos)

### Fase 3: Análisis Avanzado (Implementación Futura)
**Prioridad:** Baja
**Esfuerzo:** Alto (1 semana)
**Impacto:** Alto

1. Integración con R para análisis sintáctico
2. Validación de tipos
3. Sugerencias automáticas de corrección

---

## 📝 Implementación Recomendada: Fase 1

### Archivo: `prompt-enhancer.sh`

**Agregar después de la línea 277 (después de `read_style_guide()`):**

```bash
# ============================================================================
# FUNCIÓN: Detectar patrones problemáticos comunes en .Rmd
# ============================================================================
detect_common_errors() {
    local file_path="$1"
    local errors=""
    
    [[ ! -f "$file_path" ]] && return 0
    
    # Error: abs() sobre variable formateada
    if grep -nE "abs\([^)]*_formateado\)" "$file_path" 2>/dev/null | grep -vE "^\s*#|#.*abs"; then
        errors+="⚠️  **Error detectado**: Uso de abs() sobre variable formateada\n"
        errors+="   Patrón problemático: abs(variable_formateada)\n"
        errors+="   Solución: Aplicar abs() sobre valor numérico, luego formatear\n"
        errors+="   Documentación: .claude/docs/patrones-errores-conocidos.md#error-2\n\n"
    fi
    
    # Error: round(), floor(), ceiling() sobre variable formateada
    if grep -nE "(round|floor|ceiling)\([^)]*_formateado\)" "$file_path" 2>/dev/null | grep -vE "^\s*#"; then
        errors+="⚠️  **Error detectado**: Función matemática sobre variable formateada\n"
        errors+="   Patrón problemático: funcion_matematica(variable_formateada)\n"
        errors+="   Solución: Aplicar función sobre valor numérico, luego formatear\n"
        errors+="   Documentación: .claude/docs/patrones-errores-conocidos.md#error-2\n\n"
    fi
    
    # Advertencia: include_tikz() en chunk de generación
    if grep -n "include_tikz" "$file_path" 2>/dev/null | grep -E "generar|data_generation|generar_datos" | grep -vE "^\s*#"; then
        errors+="⚠️  **Advertencia**: include_tikz() en chunk de generación\n"
        errors+="   Puede causar errores 'File not found' en compilación PDF\n"
        errors+="   Solución: Usar renderizado condicional con knitr::is_latex_output()\n"
        errors+="   Documentación: .claude/docs/patrones-errores-conocidos.md#error-1\n\n"
    fi
    
    if [[ -n "$errors" ]]; then
        echo -e "## 🚨 ERRORES DETECTADOS EN ARCHIVO .RMD\n"
        echo -e "$errors"
    fi
}

# ============================================================================
# FUNCIÓN: Leer resumen de errores conocidos
# ============================================================================
read_error_patterns() {
    local project_root="$1"
    local error_file="$project_root/.claude/docs/patrones-errores-conocidos.md"
    
    [[ ! -f "$error_file" ]] && return 0
    
    local output=""
    output+="## 🚨 ERRORES CONOCIDOS Y SOLUCIONES\n\n"
    output+="**Referencia completa:** \`.claude/docs/patrones-errores-conocidos.md\`\n\n"
    
    # Extraer resúmenes de errores (primeros 3 errores documentados)
    local error_count=0
    while IFS= read -r line; do
        if [[ "$line" =~ ^##\ Error\ [0-9]+: ]]; then
            error_count=$((error_count + 1))
            if [[ $error_count -le 3 ]]; then
                output+="### $(echo "$line" | sed 's/^## //')\n"
            fi
        elif [[ "$line" =~ ^###\ ❌\ Mensaje\ de\ Error ]] && [[ $error_count -le 3 ]]; then
            output+="$(echo "$line" | sed 's/^### /#### /')\n"
            # Leer siguiente línea con el mensaje
            read -r next_line
            output+="\`\`\`\n$next_line\n\`\`\`\n\n"
        fi
    done < "$error_file"
    
    echo -e "$output"
}
```

**Modificar `enhance_prompt()` (después de línea 400):**

```bash
enhance_prompt() {
    # ... código existente ...
    
    # Detectar errores si se menciona un archivo .Rmd
    if echo "$user_prompt" | grep -qiE "\.Rmd|\.rmd|archivo.*rmd"; then
        # Intentar extraer ruta del archivo del prompt o contexto
        local rmd_file=""
        
        # Buscar en el prompt
        rmd_file=$(echo "$user_prompt" | grep -oE "[^\s\"']+\.Rmd" | head -1)
        
        # Si no se encuentra, buscar en el directorio actual
        if [[ -z "$rmd_file" ]]; then
            rmd_file=$(find "$PWD" -maxdepth 2 -name "*.Rmd" -type f | head -1)
        fi
        
        # Si se encuentra un archivo, detectar errores
        if [[ -n "$rmd_file" && -f "$rmd_file" ]]; then
            local detected_errors=$(detect_common_errors "$rmd_file")
            if [[ -n "$detected_errors" ]]; then
                enhanced_prompt+="$detected_errors\n"
            fi
        fi
        
        # Incluir resumen de errores conocidos
        enhanced_prompt+="$(read_error_patterns "$project_root")"
    fi
    
    # ... resto del código ...
}
```

---

## 🧪 Pruebas Recomendadas

### Test 1: Detección de Error abs()
```bash
# Crear archivo de prueba
cat > test_error.Rmd << 'EOF'
```{r}
b_formateado <- "-2.5"
resultado <- abs(b_formateado)  # Error esperado
```
EOF

# Ejecutar detección
./prompt-enhancer.sh "Corrige este archivo test_error.Rmd"
# Debe detectar el error y sugerir solución
```

### Test 2: Sin Errores
```bash
# Crear archivo correcto
cat > test_correct.Rmd << 'EOF'
```{r}
b <- -2.5
b_abs_formateado <- ifelse(abs(b) == as.integer(abs(b)), 
                          as.character(abs(b)), 
                          sprintf("%.1f", abs(b)))
```
EOF

# Ejecutar detección
./prompt-enhancer.sh "Revisa test_correct.Rmd"
# No debe detectar errores
```

### Test 3: Integración con Prompt
```bash
# Prompt que menciona archivo .Rmd
./prompt-enhancer.sh "Tengo un error en recta_geometria_analitica.Rmd"
# Debe:
# 1. Detectar errores si existen
# 2. Incluir resumen de errores conocidos
# 3. Referenciar documentación completa
```

---

## 📊 Impacto Esperado

### Beneficios
- ✅ **Prevención proactiva**: Detecta errores antes de ejecutar código
- ✅ **Educación**: Enseña patrones correctos
- ✅ **Referencias rápidas**: Enlaza a documentación relevante
- ✅ **Ahorro de tiempo**: Reduce tiempo de debugging

### Limitaciones
- ⚠️ **Falsos positivos**: Puede detectar patrones que no son errores
- ⚠️ **Cobertura limitada**: Solo detecta patrones conocidos
- ⚠️ **No valida ejecución**: No prueba que el código funcione

### Métricas de Éxito
- Reducción de errores reportados: 20-30%
- Tiempo de corrección: Reducción de 50%
- Satisfacción del usuario: Mejora en feedback

---

## 🚀 Plan de Implementación

### Semana 1: Fase 1 (Detección Básica)
- [ ] Implementar `detect_common_errors()`
- [ ] Implementar `read_error_patterns()`
- [ ] Integrar en `enhance_prompt()`
- [ ] Pruebas básicas

### Semana 2: Refinamiento
- [ ] Mejorar precisión (reducir falsos positivos)
- [ ] Expandir patrones de detección
- [ ] Documentación de uso

### Semana 3: Validación
- [ ] Pruebas con archivos reales
- [ ] Feedback de usuarios
- [ ] Ajustes finales

---

## 📚 Referencias

- **Error documentado**: `.claude/docs/patrones-errores-conocidos.md#error-2`
- **Caso resuelto**: `.claude/docs/casos-resueltos/2025-01-XX-recta-abs-formateado.md`
- **Script actual**: `Auxiliares/Prompt-Enhancer/prompt-enhancer.sh`

---

## ✅ Conclusión

La implementación de detección básica de errores en `prompt-enhancer.sh` es **viable y recomendada**. La Fase 1 puede implementarse con bajo esfuerzo y alto impacto, mejorando significativamente la experiencia del usuario al prevenir errores comunes antes de ejecutar código.

**Recomendación final:** Implementar Fase 1 (Detección Básica) como mejora inmediata al script.

