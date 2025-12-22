---
description: Ejecuta 🔍 FASE 2 del Ciclo de Validación Automática - Validación Visual y Funcional.
---

# 🔍 FASE 2: Validación Visual y Funcional

## ⚡ CONTEXTO: Ciclo de Validación y Corrección Automática

Este comando ejecuta la **FASE 2: VALIDACIÓN VISUAL Y FUNCIONAL** del ciclo obligatorio:

```
🔄 FASE 1: Renderizado Inicial
    │
    ▼
🔍 FASE 2: VALIDACIÓN VISUAL Y FUNCIONAL ← ESTE COMANDO
    │
    ▼
⚡ FASE 3: Decisión y Acción
```

Detecta incoherencias entre componentes del ejercicio .Rmd.

## Tipos de Coherencia

### 1. Matemática (ERR_C1)
- Fórmulas correctas
- Cálculos válidos
- Respuesta correcta

### 2. Imagen-Texto (ERR_C2)
- Descripción = gráfico
- Valores sincronizados
- Etiquetas correctas

### 3. Código (ERR_C3)
- R → Python sincronizado
- R → TikZ sincronizado
- Tipos de datos correctos

## Verificación Rápida

### Buscar errores de código comunes:
```bash
# Funciones matemáticas sobre strings
grep -n "abs(.*formateado" archivo.Rmd
grep -n "round(.*formateado" archivo.Rmd

# Variables no sincronizadas
grep -n "\\\\def\\\\" archivo.Rmd  # Definiciones TikZ
grep -n "r\\." archivo.Rmd          # Referencias R→Python
```

### Verificar metadatos:
```bash
grep -n "^exname:" archivo.Rmd
grep -n "^extype:" archivo.Rmd
grep -n "^exsolution:" archivo.Rmd
```

## Errores Comunes y Soluciones

### Error: abs() sobre variable formateada
```r
# ❌ INCORRECTO
b_formateado <- sprintf("%.1f", b)
resultado <- abs(b_formateado)  # Error!

# ✅ CORRECTO
b_abs <- abs(b)  # Primero abs() sobre número
b_formateado <- sprintf("%.1f", b_abs)  # Luego formatear
```

### Error: Variable TikZ no sincronizada
```r
# ❌ INCORRECTO
radio <- 5
tikz_code <- "\\def\\radio{3}"  # Valor hardcodeado

# ✅ CORRECTO
radio <- 5
tikz_code <- paste0("\\def\\radio{", radio, "}")  # Sincronizado
```

### Error: Transferencia R→Python incorrecta
```python
# ❌ INCORRECTO
valor = 5  # Valor hardcodeado en Python

# ✅ CORRECTO
valor = r.variable_r  # Usar valor desde R
```

## Checklist de Coherencia

### Matemática:
- [ ] Fórmula aplicada correctamente
- [ ] Cálculos intermedios verificados
- [ ] Respuesta correcta calculada
- [ ] Distractores plausibles pero incorrectos
- [ ] exsolution coincide con respuesta correcta

### Imagen-Texto:
- [ ] Dimensiones en texto = dimensiones en gráfico
- [ ] Colores descritos = colores mostrados
- [ ] Etiquetas legibles y correctas
- [ ] Escala apropiada

### Código:
- [ ] No hay funciones matemáticas sobre strings
- [ ] Variables R sincronizadas con TikZ
- [ ] Variables R sincronizadas con Python
- [ ] Formato de números consistente

## Reporte de Coherencia

```
╔════════════════════════════════════════╗
║     VALIDACIÓN DE COHERENCIA           ║
╠════════════════════════════════════════╣
║ Matemática:      ✅ OK                 ║
║ Imagen-Texto:    ✅ OK                 ║
║ Código:          ⚠️ 1 advertencia      ║
║   → Línea 87: abs(b_formateado)        ║
╠════════════════════════════════════════╣
║ Recomendación: Corregir línea 87       ║
╚════════════════════════════════════════╝
```

## Siguiente Paso (OBLIGATORIO)

Después de FASE 2, continuar automáticamente a FASE 3:

```
❌ SIN ERRORES → Aprobar para producción
✓ CON ERRORES → Ejecutar `/diagnosticar-errores`
    ├── 📚 SUBFASE 3A: Consultar ejemplos funcionales
    ├── 🔄 SUBFASE 3B: Volver a FASE 1 (revalidación)
    └── 📊 SUBFASE 3C: Documentar solución
```

## ⛔ CONDICIONES CRÍTICAS

1. ✓ SIEMPRE verificar los 4 tipos de coherencia
2. ✓ SIEMPRE registrar errores con clasificación ERR_XX
3. ✓ SIEMPRE continuar a FASE 3 (decisión)
4. ❌ NUNCA omitir verificaciones
5. ❌ NUNCA terminar con errores sin resolver

## Referencias

- `.claude/Mermaid_Chart.txt` (diagrama de flujo oficial)
- `/A-Produccion/Ejemplos-Funcionales-Rmd/` (fuente de verdad)
- `.claude/docs/patrones-errores-conocidos.md#error-2`
- `.claude/skills/validar-coherencia/skill.md`

