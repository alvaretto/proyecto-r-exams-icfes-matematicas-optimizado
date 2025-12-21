---
description: ⚠️ DEPRECADO - Usar /analizar-icfes en su lugar
deprecated: true
replacement: /analizar-icfes
deprecation_date: 2025-12-20
---

# ⚠️ COMANDO DEPRECADO

Este comando ha sido **deprecado** en favor de `/analizar-icfes`.

## 🔴 Razón de Deprecación

- **Análisis incompleto**: Solo cubre 3 de las 6 dimensiones ICFES requeridas
- **No alineado con Mermaid Chart**: Falta dimensiones C5 (Contenido Curricular) y C6 (Eje Axial)
- **Sin uso documentado**: No hay referencias en el workflow oficial
- **Combina dimensiones**: Mezcla "Componente" y "Pensamiento" que deben ser separadas

## ✅ Alternativa Recomendada

**Usar `/analizar-icfes`** que proporciona:

- ✅ Análisis completo de las 6 dimensiones ICFES
- ✅ Alineación total con Mermaid Chart (`.claude/Mermaid_Chart.txt`)
- ✅ Integración con workflow oficial
- ✅ Compatibilidad con agente ClasificadorICFES
- ✅ Separación correcta de dimensiones C3 (Componente) y C4 (Pensamiento)

### Comparación de Dimensiones

| Dimensión | `/analizar-ejercicio` | `/analizar-icfes` |
|-----------|----------------------|-------------------|
| 1. Nivel de Dificultad | ✅ | ✅ |
| 2. Competencia | ✅ | ✅ |
| 3. Componente | ⚠️ (combinado) | ✅ (separado) |
| 4. Pensamiento | ⚠️ (combinado) | ✅ (separado) |
| 5. Contenido Curricular | ❌ | ✅ |
| 6. Eje Axial Disciplinar | ❌ | ✅ |

## 📚 Documentación

- **Comando recomendado**: `.claude/commands/analizar-icfes.md`
- **Registro de deprecación**: `.claude/docs/COMANDOS_DEPRECADOS.md`
- **Workflow oficial**: `.claude/TROUBLESHOOTING.md`

## 🔄 Migración

**Antes:**
```bash
/analizar-ejercicio imagen.png
```

**Después:**
```bash
/analizar-icfes imagen.png
```

**Resultado:** Análisis completo con las 6 dimensiones ICFES requeridas.

---

**NOTA:** Este comando se mantendrá por compatibilidad temporal pero será eliminado en futuras versiones (estimado: 3 meses).
