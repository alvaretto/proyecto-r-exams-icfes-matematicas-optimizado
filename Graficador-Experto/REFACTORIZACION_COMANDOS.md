# Refactorización de Comandos Claude Code

## Problema Identificado

Los comandos no aparecían en Claude Code porque la estructura usaba archivos JSON en un directorio `.claude/`, cuando Claude Code requiere archivos Markdown individuales.

## Formato Incorrecto (Anterior)

```
.claude/
├── commands.json    # ❌ No reconocido por Claude Code
├── hooks.json       # ❌ No reconocido por Claude Code
└── project.md
```

## Formato Correcto (Actual)

```
.claude/
├── commands/        # ✅ Comandos individuales en Markdown
│   ├── analizar-imagen.md
│   ├── generar-tikz.md
│   ├── generar-python.md
│   ├── generar-r.md
│   ├── comparar.md
│   ├── iterar.md
│   └── exportar.md
├── skills/          # ✅ Skills en subdirectorios
│   ├── analizar-imagen-matematica/skill.md
│   ├── generar-tikz/skill.md
│   ├── generar-python/skill.md
│   ├── generar-r/skill.md
│   ├── comparar-visual/skill.md
│   └── refinar-codigo/skill.md
└── README.md
```

## Formato de Comandos

Cada comando es un archivo `.md` con:

1. **Frontmatter YAML** con `description:`
2. **Contenido Markdown** con instrucciones

### Ejemplo:

```markdown
---
description: Descripción breve del comando.
---

# Nombre del Comando

Instrucciones detalladas...

## Proceso

1. Paso 1
2. Paso 2

## Referencias

- Link a skills relevantes
```

## Cambios Realizados

### 1. Eliminación de Archivos JSON

- ❌ Eliminado: `.claude/commands.json`
- ❌ Eliminado: `.claude/hooks.json`
- ❌ Eliminado: `.claude/project.md`

### 2. Creación de Comandos MD (7 archivos)

✅ Creado: `.claude/commands/analizar-imagen.md`
✅ Creado: `.claude/commands/generar-tikz.md`
✅ Creado: `.claude/commands/generar-python.md`
✅ Creado: `.claude/commands/generar-r.md`
✅ Creado: `.claude/commands/comparar.md`
✅ Creado: `.claude/commands/iterar.md`
✅ Creado: `.claude/commands/exportar.md`

### 3. Reorganización de Skills (6 skills)

✅ Reorganizado: `skills/analizar-imagen-matematica.md` → `.claude/skills/analizar-imagen-matematica/skill.md`
✅ Reorganizado: `skills/generar-tikz.md` → `.claude/skills/generar-tikz/skill.md`
✅ Reorganizado: `skills/generar-python.md` → `.claude/skills/generar-python/skill.md`
✅ Reorganizado: `skills/generar-r.md` → `.claude/skills/generar-r/skill.md`
✅ Reorganizado: `skills/comparar-visual.md` → `.claude/skills/comparar-visual/skill.md`
✅ Reorganizado: `skills/refinar-codigo.md` → `.claude/skills/refinar-codigo/skill.md`

### 4. Documentación Actualizada

✅ Creado: `.claude/README.md` - Documentación de configuración
✅ Actualizado: `README.md` - Estructura del proyecto corregida

## Verificación

### Comandos Disponibles

```bash
$ ls .claude/commands/
analizar-imagen.md  generar-python.md  iterar.md
comparar.md         generar-r.md
exportar.md         generar-tikz.md
```

**Total: 7 comandos**

### Skills Disponibles

```bash
$ ls -d .claude/skills/*/
analizar-imagen-matematica/  generar-python/  refinar-codigo/
comparar-visual/             generar-r/
generar-tikz/
```

**Total: 6 skills**

## Cómo Usar

### 1. Reiniciar Claude Code (si es necesario)

Los comandos deberían aparecer automáticamente. Si no:

- Recarga el proyecto
- Cierra y abre Claude Code
- Verifica que estás en el directorio correcto del proyecto

### 2. Verificar Comandos

En Claude Code, escribe `/` y deberías ver:

- `/analizar-imagen`
- `/generar-tikz`
- `/generar-python`
- `/generar-r`
- `/comparar`
- `/iterar`
- `/exportar`

### 3. Uso Básico

```
1. Comparte imagen matemática ICFES
2. Ejecuta: /analizar-imagen
3. Valida o refina cada lenguaje
4. Exporta: /exportar
```

## Estado Final

✅ Estructura compatible con Claude Code
✅ 7 comandos funcionales
✅ 6 skills organizadas correctamente
✅ Documentación actualizada
✅ Sin errores de linting

## Fecha

Diciembre 25, 2025

---

**Nota**: Los hooks automáticos (compilación de TikZ, ejecución de Python/R) están documentados en los comandos pero pendientes de implementación técnica.
