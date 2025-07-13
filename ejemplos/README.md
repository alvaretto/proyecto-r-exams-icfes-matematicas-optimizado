# 📁 Ejemplos del Agente Graficador TikZ

## 📂 Estructura de Directorios

### `/entrada/`
Coloca aquí las imágenes que quieres procesar:
- **Formatos soportados:** PNG, JPG, JPEG, BMP, TIFF
- **Resolución recomendada:** Mínimo 800x600 píxeles
- **Calidad:** Alta calidad, buen contraste

### `/salida/`
Aquí se guardarán los archivos TikZ generados:
- **Archivos .tikz:** Código TikZ generado
- **Archivos _metadata.json:** Información del procesamiento

### `/validacion/`
Resultados de validación y comparación:
- **Archivos PDF:** Compilaciones de prueba
- **Archivos PNG:** Imágenes generadas para comparación

## 🎯 Ejemplos de Uso

### Procesar una imagen
```bash
# Copiar imagen a entrada/
cp mi_grafica.png ejemplos/entrada/

# Procesar con el agente
python ../agente_core.py ejemplos/entrada/mi_grafica.png

# Revisar resultado en salida/
cat ejemplos/salida/mi_grafica.tikz
```

### Procesamiento en lote
```python
from agente_core import AgenteTikZ

agente = AgenteTikZ()
resultados = agente.procesar_lote("ejemplos/entrada/", "ejemplos/salida/")
```

## 📊 Tipos de Imágenes Recomendadas

### ✅ Funciones Matemáticas
- Gráficas de funciones (lineales, cuadráticas, trigonométricas)
- Ejes coordenados claramente marcados
- Curvas suaves y continuas

### ✅ Figuras Geométricas
- Triángulos, círculos, polígonos
- Medidas y ángulos marcados
- Construcciones geométricas

### ✅ Diagramas
- Diagramas de flujo
- Esquemas conceptuales
- Grafos y redes

### ❌ No Recomendadas
- Imágenes con mucho ruido
- Fotografías complejas
- Texto manuscrito
- Imágenes de muy baja resolución
