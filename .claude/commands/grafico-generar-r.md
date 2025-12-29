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

1. **Leer Análisis Inicial y Lecciones Aprendidas**:
   - Cargar `outputs/analisis_inicial.json` para reutilizar análisis estructurado
   - Si existe `outputs/lecciones_aprendidas.json`, leer lecciones de TikZ y Python para aplicar estrategias exitosas
   - Usar elementos visuales, paleta de colores y recomendaciones técnicas específicas para R
   - Aplicar lecciones aprendidas de lenguajes previos (ej: colores RGB que funcionaron bien)

2. **Actualizar Estado del Workflow**:
   - Usar skill `gestionar-estado` para iniciar fase R
   - Validar que Python esté validado o al menos iniciado (flexible)
   - Establecer `r.estado` como "en_iteracion"
   - Establecer `r.iteracion_actual` como 1 (primera iteración)
   - Registrar `r.timestamp_inicio` con timestamp actual
   - Actualizar `fase_actual` como "r_iteracion"
   - Actualizar `timestamp_ultima_actualizacion`

3. **Implementa**:
   - Datos en formato data.frame según elementos_visuales
   - Capas de ggplot2 (geom_point, geom_line, etc.)
   - Escalas (scale_x_*, scale_y_*, scale_color_*) según elementos_visuales.ejes y paleta_colores
   - Anotaciones (annotate, geom_text) según anotaciones del análisis
   - Temas y estilos personalizados
   - Aplicar recomendaciones_tecnicas.r del análisis

4. **Valida**:
   - El código debe ejecutarse sin errores
   - Usa gramática de gráficos correctamente
   - Incluye comentarios explicativos

5. **Después de generar**:
   - Guarda el código en `outputs/output_r.R`
   - Añade sección "Código R" en `outputs/reporte_matematico.md` con el código generado
   - Ejecuta el código con Rscript para generar PNG (hook automático)
   - Ejecuta automáticamente el comando `/comparar r`

## Opciones

- `--refinar`: Refina código existente basado en última comparación
- `--forzar`: Regenera desde cero ignorando código previo
- `--formato png|svg|pdf`: Especifica formato de salida (default: png)

## Referencias

- `skills/generar-r/skill.md` - Plantillas y mejores prácticas
- `skills/gestionar-estado/skill.md` - Skill de gestión de estado del workflow
- `skills/transferir-conocimiento/skill.md` - Skill de transferencia de conocimiento (si existe)
- `.claude/schemas/analisis_inicial.schema.json` - Esquema del análisis estructurado
- Hooks automáticos se encargan de ejecución

