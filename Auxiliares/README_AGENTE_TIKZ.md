# 🎨 Agente Graficador TikZ + Augment + VSCode

## 📍 **Ubicación del Agente**

El **Agente Graficador TikZ** está ubicado en:
```
📁 Auxiliares/Agente-Graficador-TikZ/
```

## 🎯 **¿Qué es el Agente TikZ?**

**Herramienta inteligente** que convierte imágenes matemáticas en código TikZ profesional usando **Augment IA**, completamente integrado con **VSCode**.

### **✨ Características Principales:**
- 🧠 **Análisis inteligente** con Augment IA
- 🛠️ **Generación profesional** de código TikZ
- 🎯 **Integración total** con VSCode
- 🔄 **Compatibilidad garantizada** con QTikz/KTikz y LaTeX

## 🚀 **Uso Rápido**

### **1. Navegar al agente:**
```bash
cd Auxiliares/Agente-Graficador-TikZ/
```

### **2. Leer documentación:**
```
📖 01-Documentacion/01-INDICE_GENERAL.md  # ← EMPEZAR AQUÍ
```

### **3. Configurar (una sola vez):**
```bash
cd 03-Configuracion-VSCode/
python3 05-setup_vscode.py
```

### **4. Usar en VSCode:**
```
1. Abrir imagen en VSCode
2. Ctrl+Shift+P → "Tasks: Run Task" → "🎨 Agente TikZ + Augment"
3. Ver resultado en tikz_generado/
```

## 📁 **Estructura del Agente**

```
📁 Agente-Graficador-TikZ/
├── 📚 01-Documentacion/           # Documentación completa
│   ├── 01-INDICE_GENERAL.md      # ← EMPEZAR AQUÍ
│   ├── 02-REFERENCIA_RAPIDA.md   # Uso rápido
│   ├── 03-TUTORIAL_COMPLETO.md   # Tutorial detallado
│   └── ...
├── 🎯 02-Codigo-Agente/           # Motor del sistema
│   ├── 01-agente_principal.py    # Agente principal
│   ├── 03-agente_simple.py       # Versión simplificada
│   └── ...
├── ⚙️ 03-Configuracion-VSCode/    # Integración VSCode
├── 💡 04-Ejemplos-y-Pruebas/      # Casos de uso verificados
└── 🎨 05-Templates-TikZ/          # Plantillas profesionales
```

## 🎯 **Casos de Uso**

### **📊 Funciones Matemáticas**
- Funciones lineales, cuadráticas, trigonométricas
- Funciones exponenciales y logarítmicas
- Gráficas complejas con múltiples elementos

### **📐 Figuras Geométricas**
- Triángulos, círculos, polígonos
- Construcciones geométricas
- Figuras con medidas y ángulos

### **📈 Diagramas**
- Diagramas de flujo
- Esquemas conceptuales
- Mapas mentales

## ✅ **Estado: Completamente Funcional**

- ✅ **Procesamiento exitoso** verificado con imágenes reales
- ✅ **Integración VSCode** configurada y operativa
- ✅ **Compatibilidad QTikz** sin errores
- ✅ **Documentación completa** con orden secuencial
- ✅ **Templates profesionales** incluidos

## 🔗 **Enlaces Rápidos**

### **📖 Documentación:**
- **Índice General:** `Agente-Graficador-TikZ/01-Documentacion/01-INDICE_GENERAL.md`
- **Uso Rápido:** `Agente-Graficador-TikZ/01-Documentacion/02-REFERENCIA_RAPIDA.md`
- **Tutorial Completo:** `Agente-Graficador-TikZ/01-Documentacion/03-TUTORIAL_COMPLETO.md`

### **🛠️ Configuración:**
- **Setup VSCode:** `Agente-Graficador-TikZ/03-Configuracion-VSCode/05-setup_vscode.py`
- **Configuración:** `Agente-Graficador-TikZ/02-Codigo-Agente/05-configuracion.json`

### **🧪 Testing:**
- **Tests Automatizados:** `Agente-Graficador-TikZ/04-Ejemplos-y-Pruebas/05-test_agente.py`
- **Ejemplos:** `Agente-Graficador-TikZ/04-Ejemplos-y-Pruebas/`

## 🎨 **Ejemplo de Resultado**

**Entrada:** Imagen de función matemática  
**Salida:** Código TikZ profesional compatible con QTikz y LaTeX

```latex
\begin{tikzpicture}[scale=1.2]
% Cuadrícula de fondo sutil
\draw[gray!20, thin] (-4,-3) grid[step=0.5] (4,3);

% Ejes coordenados principales
\draw[thick, ->] (-4,0) -- (4,0) node[below right] {$x$};
\draw[thick, ->] (0,-3) -- (0,3) node[above left] {$y$};

% Función detectada por Augment IA
\draw[blue, very thick] plot[domain=-3:3, samples=50] (\x, {0.5*\x^2 - 1});
\end{tikzpicture}
```

## 🎯 **Integración con el Proyecto**

### **Uso desde cualquier directorio del proyecto:**
```bash
# Desde Lab-Manjaro/
python3 ../Auxiliares/Agente-Graficador-TikZ/02-Codigo-Agente/01-agente_principal.py mi_imagen.png

# Desde cualquier ubicación
python3 /ruta/completa/Auxiliares/Agente-Graficador-TikZ/02-Codigo-Agente/01-agente_principal.py imagen.png
```

### **Templates disponibles para el proyecto:**
- **Funciones:** `Agente-Graficador-TikZ/05-Templates-TikZ/01-template_funciones.tikz`
- **Geometría:** `Agente-Graficador-TikZ/05-Templates-TikZ/02-template_geometria.tikz`
- **Diagramas:** `Agente-Graficador-TikZ/05-Templates-TikZ/03-template_diagramas.tikz`

## 🚀 **¡Empezar Ahora!**

1. **Ir al agente:** `cd Auxiliares/Agente-Graficador-TikZ/`
2. **Leer documentación:** `01-Documentacion/01-INDICE_GENERAL.md`
3. **Configurar sistema:** `python3 03-Configuracion-VSCode/05-setup_vscode.py`
4. **Procesar primera imagen:** Usar tasks de VSCode

---

**¡Convierte tus imágenes matemáticas en código TikZ profesional!** 🎨✨

**Versión:** 1.0.0 | **Ubicación:** `Auxiliares/Agente-Graficador-TikZ/` | **Estado:** ✅ Funcional
