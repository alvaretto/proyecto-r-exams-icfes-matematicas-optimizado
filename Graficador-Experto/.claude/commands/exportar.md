---
description: Genera archivos finales y reporte consolidado del proceso completo.
---

# Exportar Resultados

Exporta todos los resultados del workflow.

## Proceso

1. **Verifica** que los tres códigos estén validados

2. **Guarda archivos finales**:
   - `outputs/output_tikz.tex` - Código TikZ final
   - `outputs/output_python.py` - Código Python final
   - `outputs/output_r.R` - Código R final
   - `outputs/original.png` - Imagen original
   - `outputs/tikz_render.png` - Renderizado TikZ
   - `outputs/python_render.png` - Renderizado Python
   - `outputs/r_render.png` - Renderizado R

3. **Genera reporte consolidado** (`outputs/reporte_matematico.md`):

```markdown
# Reporte de Conversión Matemática ICFES

## Resumen Ejecutivo

- **Fecha**: [fecha]
- **Tipo de contenido**: [tipo]
- **Iteraciones totales**: TikZ: [N], Python: [N], R: [N]
- **Estado final**: ✅ Completado

## Análisis Inicial

[Resumen del análisis de la imagen original]

## Implementaciones

### TikZ (LaTeX)

**Iteraciones**: [N]
**Similitud visual**: [%]
**Ventajas**:

- Salida vectorial de máxima calidad
- Precisión matemática perfecta
- Ideal para publicaciones académicas

**Desventajas**:

- Requiere compilación LaTeX
- Curva de aprendizaje pronunciada

**Código final**: Ver `output_tikz.tex`

![TikZ Output](tikz_render.png)

### Python (matplotlib/numpy)

**Iteraciones**: [N]
**Similitud visual**: [%]
**Ventajas**:

- Ecosistema científico completo
- Fácil integración con cálculos
- Gran flexibilidad

**Desventajas**:

- Calidad de salida inferior a TikZ
- Configuración de estilos puede ser compleja

**Código final**: Ver `output_python.py`

![Python Output](python_render.png)

### R (ggplot2)

**Iteraciones**: [N]
**Similitud visual**: [%]
**Ventajas**:

- Gramática de gráficos intuitiva
- Excelente para visualización estadística
- Código conciso y legible

**Desventajas**:

- Menos flexible para gráficos complejos
- Rendimiento con datasets grandes

**Código final**: Ver `output_r.R`

![R Output](r_render.png)

## Comparación Visual

| Aspecto | TikZ | Python | R |
|---------|------|--------|---|
| Precisión | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐ |
| Colores | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| Escalas | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐ |
| Anotaciones | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐ |

## Recomendaciones

- **Para publicaciones académicas**: Usar versión TikZ
- **Para análisis interactivo**: Usar versión Python
- **Para reportes estadísticos**: Usar versión R

## Notas Técnicas

[Decisiones de implementación, desafíos encontrados, soluciones aplicadas]

## Historial de Iteraciones

[Resumen de cambios en cada iteración]
```

4. **Confirma exportación completa**

## Opciones

- `--solo-codigo`: Solo genera archivos de código, sin reporte
- `--solo-reporte`: Solo genera reporte, sin archivos individuales
- `--formato html|md`: Formato del reporte (default: md)

