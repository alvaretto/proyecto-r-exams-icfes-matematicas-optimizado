# Commands Consolidados - 30 Diciembre 2025

## Razón de Consolidación

Según la **documentación oficial de Claude Code (nov 2025)**, los **Slash Commands** y **Skills** tienen propósitos diferentes y complementarios:

- **Skills**: Invocación **automática** (Claude decide cuándo usarlos según contexto)
- **Commands**: Invocación **manual explícita** (usuario decide con `/command`)

## Problema Anterior

Teníamos **15 duplicados exactos**:
- 18 commands
- 19 skills
- Total: 32+ items en `/help` (confuso)

## Solución Aplicada

### ✅ Commands Mantenidos (7)

Estos commands se mantienen porque requieren **decisión manual explícita**:

1. `/analizar-icfes` - Entry point manual al workflow
2. `/generar-schoice` - Decisión explícita de tipo de ejercicio
3. `/generar-cloze` - Decisión explícita de tipo de ejercicio
4. `/promover-ejercicio` - Decisión crítica de promoción a producción
5. `/auto-refinar-grafico` - Control manual de iteración automática
6. `/estado-graficador` - Consulta manual de estado
7. `/exportar-graficos` - Acción manual final de exportación

### 📦 Commands Movidos Aquí (11)

Estos commands fueron movidos a deprecated porque **ya existen como skills automáticos**:

1. `analizar-imagen-grafica.md` → Skill automático cuando detecta gráficos
2. `comparar-similitud-visual.md` → Skill automático en graficador
3. `corregir-error-imagen.md` → Skill automático en errores TikZ
4. `corregir-graficos.md` → Skill automático en SUBFASE 3A
5. `diagnosticar-errores.md` → Skill automático en FASE 3
6. `generar-codigo-python.md` → Skill automático según análisis
7. `generar-codigo-r.md` → Skill automático según análisis
8. `generar-codigo-tikz.md` → Skill automático según análisis
9. `refinar-codigo-grafico.md` → Skill automático en iteraciones
10. `validar-coherencia.md` → Skill automático en FASE 2
11. `validar-renderizado.md` → Skill automático en FASE 1

## Impacto

### Antes de Consolidación
- Commands: 18
- Skills: 19
- Items en `/help`: 32+
- **Problema**: Confusión sobre cuándo usar command vs skill

### Después de Consolidación
- Commands: 7 (solo manuales explícitos)
- Skills: 19 (automáticos)
- Items en `/help`: ~10
- **Beneficio**: Claridad total - Commands = control manual, Skills = automático

## Referencia Oficial

- **Documentación**: Claude Code docs (nov 2025)
- **Sección**: "Slash Commands vs Agent Skills"
- **Tabla comparativa**: skills.md, slash-commands.md

## Reversión

Si necesitas restaurar algún command:

```bash
cp .claude/deprecated/commands-consolidacion-20251230/NOMBRE.md .claude/commands/
git add .claude/commands/NOMBRE.md
```

---

**Fecha**: 2025-12-30
**Razón**: Consolidación según mejores prácticas oficiales Claude Code
**Decisión**: Basada en documentación oficial y análisis de workflow
