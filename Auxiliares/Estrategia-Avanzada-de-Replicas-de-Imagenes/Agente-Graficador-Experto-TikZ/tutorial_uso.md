# 📚 Tutorial de Uso - Agente Graficador Experto TikZ

## 🚀 Inicio Rápido

### 1. Instalación

```bash
# Clonar o descargar el agente
cd Agente-Graficador-Experto-TikZ

# Instalar dependencias Python
pip install -r requirements.txt

# Verificar dependencias del sistema
which pdflatex  # Debe estar instalado
which convert   # ImageMagick
```

### 2. Uso Básico

```python
from agente_core import AgenteTikZ

# Inicializar agente
agente = AgenteTikZ()

# Procesar una imagen
resultado = agente.procesar_imagen("mi_grafica.png")

# Ver código TikZ generado
print(resultado.codigo_tikz)

# Verificar si fue exitoso
if resultado.validacion_exitosa:
    print("✅ Código TikZ generado exitosamente")
else:
    print("❌ Errores:", resultado.errores)
```

## 📊 Casos de Uso Comunes

### Caso 1: Gráfica de Función

```python
# Para una imagen que contiene una gráfica de función matemática
resultado = agente.procesar_imagen(
    "funcion_cuadratica.png",
    descripcion="Gráfica de función cuadrática",
    tipo_grafica="funcion"
)

# El agente detectará automáticamente:
# - Ejes coordenados
# - Curva de la función
# - Puntos importantes
# - Cuadrícula si está presente
```

### Caso 2: Figura Geométrica

```python
# Para una imagen con figuras geométricas
resultado = agente.procesar_imagen(
    "triangulo_medidas.png",
    descripcion="Triángulo con medidas",
    tipo_grafica="geometria"
)

# El agente identificará:
# - Formas geométricas (triángulos, círculos, etc.)
# - Ángulos
# - Medidas y etiquetas
# - Relaciones espaciales
```

### Caso 3: Diagrama de Flujo

```python
# Para diagramas y esquemas
resultado = agente.procesar_imagen(
    "diagrama_flujo.png",
    descripcion="Diagrama de flujo de proceso",
    tipo_grafica="diagrama"
)

# El agente detectará:
# - Nodos y elementos
# - Conexiones y flechas
# - Flujo direccional
# - Etiquetas de texto
```

## 🔄 Procesamiento en Lote

```python
# Procesar múltiples imágenes
resultados = agente.procesar_lote(
    directorio_entrada="imagenes/",
    directorio_salida="tikz_generado/"
)

# Generar reporte
reporte = agente.generar_reporte(resultados, "reporte_procesamiento.md")
print(reporte)
```

## ⚙️ Configuración Avanzada

### Personalizar Configuración

```python
# Cargar configuración personalizada
config_personalizada = {
    "max_iteraciones": 10,
    "umbral_similitud": 0.98,
    "escala_default": 1.5,
    "optimizacion_codigo": True
}

agente = AgenteTikZ()
agente.config.update(config_personalizada)
```

### Usar Templates Personalizados

```python
# Agregar template personalizado
template_personalizado = """
\\begin{tikzpicture}[scale=1.2]
% Mi template personalizado
\\draw[blue, thick] (0,0) -- (2,2);
\\end{tikzpicture}
"""

agente.generador.templates['mi_template'] = template_personalizado
```

## 🔍 Análisis Detallado

### Inspeccionar Análisis de Imagen

```python
resultado = agente.procesar_imagen("imagen.png")

# Ver análisis detallado
print("Tipo detectado:", resultado.analisis['tipo_detectado'])
print("Ejes encontrados:", resultado.analisis.get('ejes', {}))
print("Curvas detectadas:", len(resultado.analisis.get('curvas', [])))
print("Colores principales:", resultado.analisis.get('colores_principales', []))
```

### Métricas de Calidad

```python
# Revisar métricas de calidad
metricas = resultado.metricas
print(f"Tiempo de procesamiento: {metricas['tiempo_procesamiento']:.2f}s")
print(f"Iteraciones usadas: {metricas['iteraciones_usadas']}")
print(f"Líneas de código: {metricas['lineas_codigo']}")
print(f"Complejidad estimada: {metricas['complejidad_estimada']:.1f}/10")
```

## 🛠️ Refinamiento Manual

### Refinar Código Específico

```python
# Si el resultado no es satisfactorio, refinar manualmente
errores_detectados = ["coordenada inválida", "escala incorrecta"]

codigo_refinado = agente.generador.refinar(
    resultado.codigo_tikz,
    errores_detectados,
    resultado.analisis
)

print("Código refinado:")
print(codigo_refinado)
```

### Optimizar Código

```python
# Optimizar código para mejor legibilidad
codigo_optimizado = agente.generador.optimizar(resultado.codigo_tikz)
print("Código optimizado:")
print(codigo_optimizado)
```

## 📋 Validación y Testing

### Validar Código Manualmente

```python
# Validar código TikZ específico
validacion = agente.validador.validar(
    codigo_tikz=mi_codigo,
    imagen_original="imagen_referencia.png"
)

if validacion['exitoso']:
    print(f"✅ Validación exitosa (similitud: {validacion['similitud']:.3f})")
else:
    print("❌ Errores de validación:")
    for error in validacion['errores']:
        print(f"  - {error}")
```

### Testing con Qtikz

```bash
# Copiar código generado a Qtikz para testing visual
echo "\\documentclass{standalone}
\\usepackage{tikz}
\\begin{document}
$(cat codigo_generado.tikz)
\\end{document}" > test_qtikz.tex

# Compilar con pdflatex
pdflatex test_qtikz.tex
```

## 🎯 Mejores Prácticas

### 1. Preparación de Imágenes

- **Resolución:** Usar imágenes de al menos 800x600 píxeles
- **Contraste:** Asegurar buen contraste entre elementos
- **Limpieza:** Evitar ruido y elementos irrelevantes
- **Formato:** Preferir PNG o JPEG de alta calidad

### 2. Descripción Efectiva

```python
# Buena descripción
resultado = agente.procesar_imagen(
    "grafica.png",
    descripcion="Gráfica de función seno con amplitud 2, período 2π, ejes marcados cada π/2",
    tipo_grafica="funcion"
)

# Descripción pobre
resultado = agente.procesar_imagen("grafica.png")  # Sin contexto
```

### 3. Iteración y Refinamiento

```python
# Proceso iterativo recomendado
for iteracion in range(3):
    resultado = agente.procesar_imagen("imagen.png")
    
    if resultado.validacion_exitosa:
        break
    
    # Analizar errores y ajustar
    print(f"Iteración {iteracion + 1}: {len(resultado.errores)} errores")
    
    # Ajustar configuración si es necesario
    agente.config['umbral_similitud'] *= 0.95  # Relajar criterio
```

## 🚨 Solución de Problemas

### Errores Comunes

1. **"LaTeX no encontrado"**
   ```bash
   # Instalar LaTeX
   sudo apt-get install texlive-latex-extra texlive-pictures
   ```

2. **"ImageMagick no disponible"**
   ```bash
   # Instalar ImageMagick
   sudo apt-get install imagemagick
   ```

3. **"Similitud muy baja"**
   ```python
   # Ajustar umbral de similitud
   agente.config['umbral_similitud'] = 0.85
   ```

4. **"Timeout de compilación"**
   ```python
   # Aumentar timeout
   agente.config['timeout_compilacion'] = 60
   ```

### Debugging

```python
# Habilitar logging detallado
import logging
logging.getLogger("AgenteTikZ").setLevel(logging.DEBUG)

# Revisar archivos temporales
print("Directorio temporal:", agente.validador.directorio_temp)
```

## 📈 Optimización de Rendimiento

### Para Procesamiento en Lote

```python
# Configuración optimizada para lotes grandes
config_lote = {
    "validacion_automatica": False,  # Desactivar validación visual
    "optimizacion_codigo": False,    # Desactivar optimización
    "max_iteraciones": 2,           # Reducir iteraciones
    "resolucion_analisis": [400, 300]  # Reducir resolución
}

agente.config.update(config_lote)
```

### Paralelización (Avanzado)

```python
from concurrent.futures import ThreadPoolExecutor
import os

def procesar_imagen_wrapper(args):
    imagen, agente_config = args
    agente_local = AgenteTikZ()
    agente_local.config.update(agente_config)
    return agente_local.procesar_imagen(imagen)

# Procesamiento paralelo
imagenes = ["img1.png", "img2.png", "img3.png"]
config = agente.config.copy()

with ThreadPoolExecutor(max_workers=os.cpu_count()) as executor:
    args = [(img, config) for img in imagenes]
    resultados = list(executor.map(procesar_imagen_wrapper, args))
```

---

## 🎓 Recursos Adicionales

- **Documentación TikZ:** [pgf-tikz.github.io](https://pgf-tikz.github.io/)
- **Ejemplos Fausto:** `Auxiliares/Ejemplos-Funcionales-Rmd/Plantillas/TikZ-Documentation/Ejemplos-Fausto/`
- **Templates Profesionales:** `Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/Ejemplo/`
- **Qtikz/Ktikz:** Para testing visual interactivo

¡El agente está listo para convertir tus imágenes matemáticas en código TikZ profesional! 🎨✨
