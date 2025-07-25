# Plantillas LaTeX Formato Legal (Oficio) - Dos Columnas

## Descripción

Este conjunto de archivos implementa plantillas LaTeX modificadas para generar documentos en formato legal (8.5" x 14") con layout de dos columnas y separador visible, optimizadas para el sistema R/exams.

## Archivos Creados

### Plantillas LaTeX (.tex)

1. **oficio_pcielo.tex**
   - Plantilla con soluciones incluidas
   - Formato legal de dos columnas
   - Separador visible entre columnas
   - Imágenes optimizadas para columnas

2. **oficio_pcielo_nosol.tex**
   - Plantilla sin soluciones
   - Mismo formato que oficio_pcielo.tex
   - Para exámenes sin respuestas

3. **oficio_solpcielo.tex**
   - Plantilla compleja con funcionalidades avanzadas
   - Formato legal de dos columnas
   - Compatible con exams2pdf
   - Incluye comandos especializados para R/exams

### Script R Modificado

4. **SemilleroUnico_Oficio_v1_modificado.R**
   - Script adaptado para usar las nuevas plantillas
   - Configuraciones ajustadas para formato legal
   - Parámetros de imagen optimizados para dos columnas

## Características Técnicas

### Configuración de Página
- **Tamaño**: Legal (215.9mm x 355.6mm)
- **Márgenes**: 15-20mm según plantilla
- **Layout**: Dos columnas con separador visible

### Configuración de Columnas
- **Separador**: 0.4pt de grosor
- **Espaciado**: 20pt entre columnas
- **Distribución**: Automática con multicol

### Configuración de Imágenes
- **Ancho**: 0.8-0.9 \columnwidth
- **Aspecto**: Mantenido automáticamente
- **Centrado**: Automático en cada columna

## Uso

### Para exams2pdf
```r
exams2pdf(archivo_examen,
          template = "oficio_solpcielo",
          ...)
```

### Para exams2pandoc
```r
exams2pandoc(archivo_examen,
             template = "oficio_pcielo.tex",
             width = 3.5,  # Ajustado para columnas
             height = 3.5, # Ajustado para columnas
             ...)
```

### Para exams2nops
```r
exams2nops(archivo_examen,
           # Usa configuración estándar NOPS
           ...)
```

## Diferencias con Plantillas Originales

| Aspecto | Original | Oficio |
|---------|----------|--------|
| Tamaño papel | A4 (210x297mm) | Legal (216x356mm) |
| Columnas | 1 | 2 con separador |
| Ancho imágenes | \textwidth | \columnwidth |
| Márgenes | 10mm | 15-20mm |
| Espaciado | Estándar | Optimizado para columnas |

## Compatibilidad

- ✅ exams2pdf
- ✅ exams2pandoc (docx)
- ✅ exams2nops
- ✅ exams2html (sin cambios)
- ⚠️ exams2moodle (requiere pruebas adicionales)

## Notas Importantes

1. **Imágenes**: Se ajustan automáticamente al ancho de columna
2. **Texto largo**: Se distribuye automáticamente entre columnas
3. **Saltos**: Usar `\columnbreak` si se necesita control manual
4. **Compatibilidad**: Mantiene toda la funcionalidad de R/exams

## Archivos de Respaldo

Los archivos originales se mantienen intactos:
- `pcielo.tex`
- `pcielo_nosol.tex`
- `solpcielo.tex`
- `SemilleroUnico_Oficio_v1.R`

## Pruebas Recomendadas

1. Generar PDF con exams2pdf usando oficio_solpcielo
2. Generar DOCX con exams2pandoc usando oficio_pcielo.tex
3. Verificar que las imágenes se muestren correctamente
4. Comprobar que el texto se distribuya bien entre columnas
5. Validar que los separadores sean visibles

## Fecha de Creación
24 de julio de 2025

## Autor
Modificaciones realizadas por Augment Agent para formato legal de dos columnas.
