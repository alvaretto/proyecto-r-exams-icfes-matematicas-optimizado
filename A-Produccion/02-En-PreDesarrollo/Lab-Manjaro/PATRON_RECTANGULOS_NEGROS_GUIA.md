# 🎨 Patrón de Rectángulos Negros para Gráficos Circulares

## 📋 **Descripción del Patrón**

Este patrón mejora la visibilidad de las etiquetas de porcentaje en gráficos circulares mediante rectángulos negros de fondo que proporcionan máximo contraste visual.

## 🎯 **Objetivo**

- **Mejorar legibilidad** de porcentajes en gráficos circulares
- **Aumentar contraste** independientemente del color de fondo
- **Estandarizar apariencia** visual en todos los ejercicios ICFES
- **Optimizar visibilidad** para diferentes formatos de salida (PDF, HTML, Moodle)

## 🔧 **Implementación Técnica**

### **Código Python Estándar:**

```python
# Agregar rectángulos negros como fondo de las etiquetas de porcentaje (patrón mejorado)
ax = plt.gca()
for autotext in autotexts:
    # Obtener posición del texto
    x, y = autotext.get_position()
    
    # Obtener el texto para calcular el tamaño del rectángulo
    texto = autotext.get_text()
    
    # Calcular dimensiones del rectángulo para abarcar completamente el texto
    ancho_rect = len(texto) * 0.090  # Ancho duplicado para máxima cobertura
    alto_rect = 0.12  # Alto optimizado para cobertura del texto
    
    # Posicionar el rectángulo exactamente detrás del texto (mismo centro)
    rect_x = x - ancho_rect/2
    rect_y = y - alto_rect/2
    
    # Agregar rectángulo negro como fondo
    rect = plt.Rectangle((rect_x, rect_y), ancho_rect, alto_rect, 
                        facecolor='black', alpha=0.8, zorder=1)
    ax.add_patch(rect)

# Configuración de texto mejorada
for autotext in autotexts:
    autotext.set_color('white')  # Texto blanco sobre fondo negro
    autotext.set_weight('bold')  # Texto en negrita
    autotext.set_zorder(2)  # Asegurar que el texto esté encima del rectángulo
```

## 📊 **Parámetros Optimizados**

### **Dimensiones del Rectángulo:**
- **Ancho**: `len(texto) * 0.090` 
  - Escalable según longitud del texto
  - Cobertura extra amplia para números de 1-2 dígitos + símbolo %
  - Margen visual muy generoso (duplicado)

- **Alto**: `0.12`
  - Optimizado para fuente de 9-14pt
  - Cobertura completa vertical
  - Proporción visual equilibrada

### **Propiedades Visuales:**
- **Color**: `facecolor='black'`
  - Máximo contraste con texto blanco
  - Funciona en cualquier color de fondo

- **Transparencia**: `alpha=0.8`
  - Suficientemente opaco para contraste
  - Permite ligera integración visual

- **Layering**: `zorder=1` (rectángulo), `zorder=2` (texto)
  - Garantiza que texto esté siempre visible
  - Previene ocultamiento accidental

## 🎯 **Aplicación en Ejercicios**

### **Ejercicios Ya Implementados:**

1. **Lab/empaques_tetra_pak_argumentacion_n3_v1/**
   - ✅ Implementado y validado
   - Competencia: Argumentación (Nivel 3)
   - Contexto: Comunitario

2. **Lab/01-S2-2025-SEDQ/**
   - ✅ Implementado en `01-S2-2025-SEDQ-grafico_circular_bienes_v0.Rmd`
   - Competencia: Interpretación y Representación
   - Contexto: Empresarial

3. **Lab/19/**
   - ✅ Implementado en `porcentajes_ordenamiento_sabores_v1.Rmd`
   - Competencia: Interpretación y Representación
   - Contexto: Comercial

### **Candidatos para Implementación:**

```bash
# Buscar ejercicios con gráficos circulares
find Lab/ -name "*.Rmd" -exec grep -l "plt\.pie\|autopct" {} \;
```

## 🔍 **Validación del Patrón**

### **Script de Validación:**
```r
# Ejecutar validación completa
source("Lab/validacion_completa_rectangulos_negros.R")
```

### **Criterios de Validación:**
- ✅ Generación exitosa en HTML
- ✅ Generación exitosa en PDF
- ✅ Generación exitosa en Moodle XML
- ✅ Presencia de todos los parámetros del patrón
- ✅ Funcionamiento sin errores críticos

## 📋 **Checklist de Implementación**

### **Antes de Aplicar el Patrón:**
- [ ] Verificar que el ejercicio usa `plt.pie()` con `autopct`
- [ ] Confirmar que existe variable `autotexts`
- [ ] Revisar estructura del código Python existente

### **Durante la Implementación:**
- [ ] Agregar código de rectángulos antes de configuración de texto
- [ ] Usar parámetros estándar: `ancho_rect = len(texto) * 0.090`
- [ ] Configurar `alto_rect = 0.12`
- [ ] Establecer `alpha=0.8` y `zorder=1`

### **Después de Implementar:**
- [ ] Configurar texto blanco: `autotext.set_color('white')`
- [ ] Establecer `zorder=2` para el texto
- [ ] Probar generación en múltiples formatos
- [ ] Verificar visibilidad en diferentes colores de fondo

## 🎨 **Variaciones del Patrón**

### **Para Textos Más Largos:**
```python
# Ajustar ancho para textos extensos
ancho_rect = max(len(texto) * 0.090, 0.20)  # Mínimo 0.20
```

### **Para Fuentes Más Grandes:**
```python
# Ajustar alto para fuentes grandes
alto_rect = 0.15  # Para fuentes > 14pt
```

### **Para Gráficos Pequeños:**
```python
# Reducir dimensiones para gráficos compactos
ancho_rect = len(texto) * 0.070
alto_rect = 0.10
```

## 🚀 **Beneficios del Patrón**

### **Técnicos:**
- **Consistencia visual** en todos los ejercicios
- **Compatibilidad** con múltiples formatos de salida
- **Escalabilidad** automática según contenido
- **Mantenibilidad** del código

### **Educativos:**
- **Legibilidad mejorada** para estudiantes
- **Accesibilidad visual** aumentada
- **Profesionalismo** en presentación
- **Estándares ICFES** mantenidos

## 📚 **Referencias y Documentación**

- **Documentación matplotlib**: [Rectangle patches](https://matplotlib.org/stable/api/_as_gen/matplotlib.patches.Rectangle.html)
- **Guías R-exams**: Framework de generación de exámenes
- **Estándares ICFES**: Competencias matemáticas colombianas

## 🔄 **Mantenimiento y Actualizaciones**

### **Revisión Periódica:**
- Validar funcionamiento con nuevas versiones de matplotlib
- Ajustar parámetros según feedback de usuarios
- Expandir a otros tipos de gráficos si es necesario

### **Monitoreo:**
- Ejecutar validación mensual con `validacion_completa_rectangulos_negros.R`
- Revisar compatibilidad con actualizaciones de R-exams
- Documentar cualquier modificación necesaria

---

**Creado**: Diciembre 2024  
**Última actualización**: Diciembre 2024  
**Versión del patrón**: 1.0  
**Compatibilidad**: R-exams 2.4+, matplotlib 3.0+