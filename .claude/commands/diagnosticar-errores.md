---
description: Analiza y clasifica errores de compilación/renderizado en 4 categorías.
---

# Diagnosticador de Errores

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

## Documentación de Referencia

- `.claude/docs/patrones-errores-conocidos.md`
- `.claude/Mermaid_Chart.txt` (diagrama de flujo)
- `.claude/skills/diagnosticar-errores/skill.md`

## Ejemplo de Diagnóstico

```
╔════════════════════════════════════════╗
║     DIAGNÓSTICO                        ║
╠════════════════════════════════════════╣
║ Error: File 'cilindro.png' not found   ║
║                                        ║
║ Categoría: GRÁFICOS                    ║
║ Código: ERR_G1                         ║
║ Descripción: Gráfica no visualizada    ║
║                                        ║
║ Causa probable:                        ║
║ include_tikz() en chunk de generación  ║
║                                        ║
║ Solución: Usar renderizado condicional ║
║ Comando: /corregir-graficos            ║
╚════════════════════════════════════════╝
```

