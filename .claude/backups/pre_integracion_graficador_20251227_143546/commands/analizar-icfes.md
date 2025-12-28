---
description: Analiza la imagen según las 6 ramas del Mermaid Chart - Inicia el workflow completo.
---

# Analizador ICFES

Analiza la imagen proporcionada siguiendo estas dimensiones del workflow:

1. **Nivel de Dificultad**: (1-4).
2. **Competencia**: (Interpretación, Formulación, Argumentación).
3. **Componente**: (Numérico, Geométrico, Aleatorio).
4. **Pensamiento**: (Numérico, Espacial, Métrico, Variacional, Aleatorio).
5. **Contenido**: (Álgebra, Geometría, Estadística).
6. **Eje**: (Matemático, Aplicado).

Basado en esto, clasifica el ejercicio y decide si requiere el AgenteTikZ (Flujo B).

## ⚡ Workflow Completo Después del Análisis

```
/analizar-icfes ← ESTE COMANDO
    │
    ▼
Generación del archivo .Rmd (/generar-schoice o /generar-cloze)
    │
    ▼
🔄 FASE 1: /validar-renderizado
    │
    ▼
🔍 FASE 2: /validar-coherencia
    │
    ▼
⚡ FASE 3: /diagnosticar-errores (si hay errores)
    │
    ▼
/promover-ejercicio (si validación exitosa)
```

## ⛔ CONDICIONES CRÍTICAS

1. ✓ **SIEMPRE** consultar ejemplos funcionales ANTES de generar código
2. ✓ **SIEMPRE** ejecutar el Ciclo de Validación Automática completo
3. ✓ **Ejemplos funcionales** = Fuente de verdad ABSOLUTA
4. ❌ **NUNCA** terminar con errores sin resolver

## Referencias

- `/A-Produccion/Ejemplos-Funcionales-Rmd/` (FUENTE DE VERDAD)
- `.claude/Mermaid_Chart.txt` (diagrama de flujo oficial)