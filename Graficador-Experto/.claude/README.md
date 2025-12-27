# Configuración de Claude Code - Graficador Experto ICFES

Este directorio contiene toda la configuración de Claude Code para el proyecto Graficador Experto.

## Estructura

```
.claude/
├── commands/           # Comandos slash personalizados
│   ├── analizar-imagen.md
│   ├── generar-tikz.md
│   ├── generar-python.md
│   ├── generar-r.md
│   ├── comparar.md
│   ├── iterar.md
│   └── exportar.md
├── skills/            # Skills especializadas
│   ├── analizar-imagen-matematica/
│   ├── generar-tikz/
│   ├── generar-python/
│   ├── generar-r/
│   ├── comparar-visual/
│   └── refinar-codigo/
├── agents/            # Agentes (futuro)
├── hooks/             # Hooks automáticos (futuro)
└── README.md          # Este archivo
```

## Comandos Disponibles (7)

### 1. `/analizar-imagen`
Inicia el workflow completo con análisis visual de imagen ICFES.

### 2. `/generar-tikz`
Genera código TikZ/LaTeX con validación visual.

### 3. `/generar-python`
Genera código Python (matplotlib/numpy).

### 4. `/generar-r`
Genera código R (ggplot2).

### 5. `/comparar`
Compara imagen generada con original usando Claude Vision.

### 6. `/iterar`
Refina código basándose en comparación visual.

### 7. `/exportar`
Genera archivos finales y reporte consolidado.

## Skills Especializadas (6)

### 1. Análisis Visual Matemático
Identificación y extracción de información de imágenes matemáticas.

### 2. Generación TikZ
Creación de código LaTeX/TikZ preciso.

### 3. Generación Python
Producción de código matplotlib/numpy profesional.

### 4. Generación R
Generación de código ggplot2 eficiente.

### 5. Comparación Visual Inteligente
Análisis de diferencias con Claude Vision.

### 6. Refinamiento Iterativo
Mejora de código basándose en comparaciones.

## Uso

1. Comparte una imagen matemática ICFES
2. Ejecuta `/analizar-imagen` para iniciar el workflow
3. Valida cada lenguaje o usa `/iterar` para refinar
4. Exporta con `/exportar` cuando estés satisfecho

## Workflow Visual

```
/analizar-imagen
    ↓
/generar-tikz → /comparar → /iterar (hasta validar)
    ↓
/generar-python → /comparar → /iterar (hasta validar)
    ↓
/generar-r → /comparar → /iterar (hasta validar)
    ↓
/exportar
```

## Notas

- Los comandos usan formato Markdown con frontmatter YAML
- Las skills están organizadas en subdirectorios con `skill.md`
- Los hooks de compilación/ejecución están documentados pero pendientes de implementación
- Para más detalles, consulta `../README.md` en la raíz del proyecto

