---
description: Ejecuta ⚡ FASE 3 del Ciclo de Validación Automática - Decisión y Acción con subfases.
---

# ⚡ FASE 3: Decisión y Acción

## ⚡ CONTEXTO: Ciclo de Validación y Corrección Automática

Este comando ejecuta la **FASE 3: DECISIÓN Y ACCIÓN** del ciclo obligatorio:

```
🔄 FASE 1: Renderizado Inicial
    │
    ▼
🔍 FASE 2: Validación Visual y Funcional
    │
    ▼
⚡ FASE 3: DECISIÓN Y ACCIÓN ← ESTE COMANDO
    │
    ├── ❌ SIN ERRORES → Aprobar para producción
    │
    └── ✓ CON ERRORES:
            ├── 📚 SUBFASE 3A: Corrección basada en ejemplos
            ├── 🔄 SUBFASE 3B: Revalidación (volver a FASE 1)
            └── 📊 SUBFASE 3C: Documentar solución
```

Clasifica errores post-renderizado para aplicar la corrección adecuada.

## Categorías de Errores

### 1. GRÁFICOS (naranja)
- ERR_G1: Gráficas no visualizadas
- ERR_G2: Gráficas solapadas
- ERR_G3: Renderizado incorrecto
- ERR_G4: Tamaño inadecuado

### 2. TEXTO (púrpura)
- ERR_T1: LaTeX no compila
- ERR_T2: Encoding incorrecto
- ERR_T3: Metadatos faltantes

### 3. ESTRUCTURA (azul)
- ERR_S1: Opciones incorrectas
- ERR_S2: Solución no coincide

### 4. COHERENCIA (cyan)
- ERR_C1: Coherencia matemática
- ERR_C2: Coherencia imagen-texto
- ERR_C3: Coherencia de código

## Diagnóstico Rápido

### Por mensaje de error:

```bash
# Buscar patrón del error
grep -i "error" log_compilacion.txt
```

| Mensaje | Categoría | Código |
|---------|-----------|--------|
| `File '*.png' not found` | GRÁFICOS | ERR_G1 |
| `LaTeX failed to compile` | TEXTO | ERR_T1 |
| `non-numeric argument` | COHERENCIA | ERR_C3 |
| `undefined control sequence` | TEXTO | ERR_T1 |

### Por inspección visual:

| Síntoma | Categoría | Código |
|---------|-----------|--------|
| Imagen ausente en PDF | GRÁFICOS | ERR_G1 |
| Elementos superpuestos | GRÁFICOS | ERR_G2 |
| Gráfico distorsionado | GRÁFICOS | ERR_G3 |
| Gráfico muy grande/pequeño | GRÁFICOS | ERR_G4 |
| Caracteres extraños | TEXTO | ERR_T2 |
| Texto no coincide con imagen | COHERENCIA | ERR_C2 |

## Flujo de Diagnóstico

```
1. Capturar mensaje de error o síntoma visual
2. Identificar patrón en tablas anteriores
3. Clasificar en categoría correspondiente
4. Ejecutar comando de corrección:

   GRÁFICOS  → /corregir-graficos
   TEXTO     → Revisar sintaxis manualmente
   ESTRUCTURA→ Revisar metadatos
   COHERENCIA→ /validar-coherencia
```

## Comandos de Corrección

```bash
# Para errores de gráficos
# → Ver skill: corregir-graficos

# Para errores de coherencia
# → Ver skill: validar-coherencia

# Para errores de texto/estructura
# → Corrección manual según patrón
```

## 📚 SUBFASE 3A: Corrección Basada en Ejemplos

**OBLIGATORIO: Consultar ejemplos funcionales ANTES de cualquier corrección**

```bash
# Consultar ejemplos funcionales
ls /A-Produccion/Ejemplos-Funcionales-Rmd/

# Buscar patrones similares al error
grep -l "include_tikz" /A-Produccion/Ejemplos-Funcionales-Rmd/*.Rmd
```

## 🔄 SUBFASE 3B: Revalidación Obligatoria

**DESPUÉS de aplicar correcciones:**

```
⚠️ OBLIGATORIO: Volver automáticamente a FASE 1
→ Ejecutar /validar-renderizado
→ Ejecutar /validar-coherencia
→ REPETIR hasta resolver TODOS los errores
```

## 📊 SUBFASE 3C: Documentar Solución (Solo si éxito)

**Solo después de revalidación exitosa:**
- Documentar en `.claude/docs/patrones-errores-conocidos.md`

## ⛔ CONDICIONES CRÍTICAS

1. ❌ **NO terminar** el ciclo con errores sin resolver
2. ❌ **NUNCA** proceder con errores pendientes
3. ✓ **SIEMPRE** consultar ejemplos funcionales ANTES de corregir
4. ✓ **SIEMPRE** ejecutar SUBFASE 3B después de correcciones
5. ✓ **Ejemplos funcionales** = Fuente de verdad ABSOLUTA

## Documentación de Referencia

- `.claude/Mermaid_Chart.txt` (diagrama de flujo oficial)
- `/A-Produccion/Ejemplos-Funcionales-Rmd/` (FUENTE DE VERDAD)
- `.claude/docs/patrones-errores-conocidos.md`
- `.claude/skills/diagnosticar-errores/skill.md`

## Ejemplo de Diagnóstico

```
╔════════════════════════════════════════════════════════════╗
║     DIAGNÓSTICO - FASE 3                                   ║
╠════════════════════════════════════════════════════════════╣
║ Error: File 'cilindro.png' not found                       ║
║                                                            ║
║ Categoría: GRÁFICOS                                        ║
║ Código: ERR_G1                                             ║
║ Descripción: Gráfica no visualizada                        ║
║                                                            ║
║ 📚 SUBFASE 3A: Consultar ejemplos funcionales              ║
║ Ruta: /A-Produccion/Ejemplos-Funcionales-Rmd/              ║
║                                                            ║
║ Solución: Usar renderizado condicional                     ║
║ Comando: /corregir-graficos                                ║
║                                                            ║
║ 🔄 SIGUIENTE: SUBFASE 3B - Volver a FASE 1                 ║
╚════════════════════════════════════════════════════════════╝
```

