---
description: Genera código R (ggplot2) para la imagen matemática.
---

# Generar Código R

Genera código R completo usando ggplot2.

## Estructura Base

```r
library(ggplot2)
library(scales)

# Preparación de datos
# Tu código aquí

# Gráfico
p <- ggplot(data, aes(x, y)) +
  # Tu código aquí
  theme_minimal() +
  labs(title = "...", x = "...", y = "...")

# Guardar
ggsave("outputs/output_r.png", p, width = 8, height = 6, dpi = 300)
print(p)
```

## Proceso

1. **Implementa**:
   - Datos en formato data.frame
   - Capas de ggplot2 (geom_point, geom_line, etc.)
   - Escalas (scale_x_*, scale_y_*, scale_color_*)
   - Anotaciones (annotate, geom_text)
   - Temas y estilos personalizados

2. **Valida**:
   - El código debe ejecutarse sin errores
   - Usa gramática de gráficos correctamente
   - Incluye comentarios explicativos

3. **Después de generar**:
   - Guarda el código en `outputs/output_r.R`
   - Ejecuta el código con Rscript para generar PNG (hook automático)
   - Ejecuta automáticamente el comando `/comparar r`

## Opciones

- `--refinar`: Refina código existente basado en última comparación
- `--forzar`: Regenera desde cero ignorando código previo
- `--formato png|svg|pdf`: Especifica formato de salida (default: png)

## Referencias

- `skills/generar-r.md` - Plantillas y mejores prácticas
- Hooks automáticos se encargan de ejecución

