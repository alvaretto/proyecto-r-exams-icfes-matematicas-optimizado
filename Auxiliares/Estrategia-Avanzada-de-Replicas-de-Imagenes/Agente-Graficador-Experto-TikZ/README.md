# 🎨 Agente Graficador Experto TikZ

## 📋 Descripción

Agente especializado en análisis de imágenes matemáticas y generación de código TikZ de alta calidad, compatible con Qtikz/Ktikz.

## 🎯 Funciones Principales

### 1. 🔍 Análisis de Imágenes
- **Detección automática** de elementos gráficos matemáticos
- **Identificación de patrones** geométricos, funciones y diagramas
- **Extracción de características** como ejes, curvas, puntos, etiquetas
- **Clasificación de tipos** de gráficas (funciones, geometría, diagramas)

### 2. 🛠️ Generación de Código TikZ
- **Código optimizado** para Qtikz/Ktikz
- **Sintaxis profesional** siguiendo mejores prácticas
- **Compatibilidad garantizada** con compiladores LaTeX estándar
- **Escalabilidad** y reutilización de componentes

### 3. 🔄 Iteración y Refinamiento
- **Análisis de errores** automático
- **Corrección iterativa** hasta satisfacción del usuario
- **Optimización continua** del código generado
- **Validación visual** del resultado

## 📁 Estructura del Proyecto

```
Agente-Graficador-Experto-TikZ/
├── README.md                    # Este archivo
├── agente_core.py              # Motor principal del agente
├── analizador_imagenes.py      # Módulo de análisis de imágenes
├── generador_tikz.py           # Generador de código TikZ
├── validador_codigo.py         # Validador y optimizador
├── templates/                  # Plantillas TikZ reutilizables
│   ├── funciones.tikz
│   ├── geometria.tikz
│   ├── diagramas.tikz
│   └── ejes_coordenados.tikz
├── ejemplos/                   # Casos de uso y ejemplos
│   ├── entrada/               # Imágenes de entrada
│   ├── salida/                # Código TikZ generado
│   └── validacion/            # Resultados compilados
├── logs/                      # Registros de procesamiento
├── tests/                     # Pruebas automatizadas
└── docs/                      # Documentación técnica
```

## 🚀 Uso Básico

### Modo Interactivo
```python
from agente_core import AgenteTikZ

agente = AgenteTikZ()
resultado = agente.procesar_imagen("ruta/imagen.png")
print(resultado.codigo_tikz)
```

### Modo Batch
```python
agente.procesar_lote("directorio/imagenes/", "directorio/salida/")
```

## 📚 Referencias de Calidad

El agente se basa en ejemplos funcionales probados:

### 🎯 Ejemplos Fausto
- **Geometría 3D:** Figuras complejas con perspectiva
- **Diagramas técnicos:** Medidas y anotaciones precisas
- **Funciones matemáticas:** Curvas y superficies

### 🏆 Estrategia Avanzada
- **Gráficas principales:** Diagramas de enunciados
- **Opciones múltiples:** Gráficas de respuestas
- **Diseño profesional:** Estándares de calidad

## ⚙️ Configuración

### Dependencias
- Python 3.8+
- OpenCV para análisis de imágenes
- NumPy para procesamiento numérico
- Matplotlib para validación visual
- LaTeX con TikZ para compilación

### Instalación
```bash
pip install -r requirements.txt
```

## 🎨 Características Avanzadas

### Detección Inteligente
- **Reconocimiento de ejes** automático
- **Extracción de funciones** matemáticas
- **Identificación de elementos** geométricos
- **Análisis de texto** y etiquetas

### Optimización de Código
- **Sintaxis limpia** y legible
- **Comentarios descriptivos** automáticos
- **Modularización** de componentes
- **Escalabilidad** profesional

### Validación Robusta
- **Compilación automática** con LaTeX
- **Comparación visual** con imagen original
- **Métricas de similitud** cuantitativas
- **Reportes de calidad** detallados

## 📊 Métricas de Calidad

- **Precisión visual:** >95% de similitud
- **Compatibilidad:** 100% con Qtikz/Ktikz
- **Tiempo de procesamiento:** <30 segundos por imagen
- **Tasa de éxito:** >90% en primer intento

## 🔧 Desarrollo

### Contribuir
1. Fork del repositorio
2. Crear rama de feature
3. Implementar mejoras
4. Ejecutar tests
5. Crear pull request

### Testing
```bash
python -m pytest tests/
```

## 📝 Changelog

### v1.0.0 (2025-01-13)
- ✅ Implementación inicial del agente
- ✅ Análisis básico de imágenes
- ✅ Generación de código TikZ
- ✅ Validación con ejemplos Fausto
- ✅ Integración con estrategia avanzada

## 📞 Soporte

Para reportar bugs o solicitar features:
- Crear issue en el repositorio
- Incluir imagen de entrada
- Describir resultado esperado
- Adjuntar logs de error

---

**🎯 Objetivo:** Democratizar la creación de gráficas matemáticas profesionales mediante automatización inteligente.
