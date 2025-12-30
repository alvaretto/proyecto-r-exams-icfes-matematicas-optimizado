# Ciclo de Validación y Corrección Automática

## OBLIGATORIO para todo archivo .Rmd

Cada vez que se renderiza un archivo .Rmd, se ejecuta automáticamente:

### 🔄 FASE 1: RENDERIZADO INICIAL
```r
exams2html("archivo.Rmd", n = 1)
exams2pdf("archivo.Rmd", n = 1)
exams2pandoc("archivo.Rmd", n = 1, type = "docx")
exams2nops("archivo.Rmd", n = 1)
```
Capturar y registrar todos los errores/advertencias.

### 🔍 FASE 2: VALIDACIÓN VISUAL Y FUNCIONAL

Verificar:
1. **Coherencia Matemática**: Fórmulas, cálculos, respuesta correcta
2. **Coherencia Imagen-Texto**: Descripción vs gráfico, valores sincronizados
3. **Coherencia de Código**: R ↔ Python ↔ TikZ sincronizado
4. **Renderizado 4 formatos**: HTML, PDF, DOCX, NOPS correctos

### ⚡ FASE 3: DECISIÓN Y ACCIÓN

**SI NO hay errores** → Continuar workflow normal

**SI hay errores** → Ejecutar subfases OBLIGATORIAS:

#### 📚 SUBFASE 3A: Corrección Basada en Ejemplos
```bash
# SIEMPRE consultar ejemplos funcionales ANTES de corregir
ls /A-Produccion/Ejemplos-Funcionales-Rmd/
# Identificar patrones de solución en archivos similares
# Aplicar correcciones basadas en ejemplos validados
```

#### 🔄 SUBFASE 3B: Ciclo de Revalidación (OBLIGATORIO)
```
⚠️ VOLVER AUTOMÁTICAMENTE A FASE 1
→ Repetir renderizado completo
→ NO TERMINAR hasta resolver TODOS los errores
```

#### 📊 SUBFASE 3C: Gestión de Resultados (Solo si éxito completo)

1. Documentar error y solución en `patrones-errores-conocidos.md`
2. Incluir código completo (antes/después)
3. Documentar ejemplo funcional utilizado
4. Referenciar archivo .Rmd verificado

## ⛔ CONDICIONES CRÍTICAS

- ❌ NO terminar con errores sin resolver
- ❌ NUNCA proceder con errores pendientes
- ✓ Documentar SOLO después de confirmar solución
- ✓ Ejemplos funcionales = Fuente de verdad absoluta
- ✓ VOLVER A FASE 1 después de cada corrección
