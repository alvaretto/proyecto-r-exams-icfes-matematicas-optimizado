# Skill: Generación de Código Python (matplotlib/numpy)

## Descripción
Habilidad especializada para generar código Python profesional usando matplotlib y numpy que reproduzca imágenes matemáticas analizadas.

## Objetivos

- Generar código Python sintácticamente correcto y ejecutable
- Usar matplotlib para visualizaciones de calidad
- Aplicar numpy para cálculos matemáticos precisos
- Producir imágenes de alta resolución
- Mantener código limpio y bien documentado

## Fundamentos

### Librerías Esenciales
```python
import matplotlib.pyplot as plt
import numpy as np
from matplotlib import rcParams
import matplotlib.patches as mpatches
from matplotlib.patches import FancyBboxPatch, Circle, Rectangle, Polygon
from matplotlib.collections import PatchCollection
```

### Configuración Básica
```python
# Configuración de estilo
rcParams['font.family'] = 'serif'
rcParams['font.size'] = 10
rcParams['figure.figsize'] = (8, 6)
rcParams['figure.dpi'] = 100
rcParams['savefig.dpi'] = 300
rcParams['axes.grid'] = True
rcParams['grid.alpha'] = 0.3
```

## Capacidades por Tipo de Contenido

### 1. Funciones y Gráficas

**Función básica**:
```python
# Dominio
x = np.linspace(-5, 5, 1000)

# Función
y = x**2 - 4*x + 3

# Gráfico
fig, ax = plt.subplots(figsize=(8, 6))
ax.plot(x, y, 'b-', linewidth=2, label=r'$f(x)=x^2-4x+3$')

# Configuración de ejes
ax.set_xlabel('x', fontsize=12)
ax.set_ylabel('y', fontsize=12)
ax.axhline(y=0, color='k', linewidth=0.5)
ax.axvline(x=0, color='k', linewidth=0.5)
ax.grid(True, alpha=0.3)
ax.legend()

plt.savefig('outputs/output_python.png', dpi=300, bbox_inches='tight')
plt.show()
```

**Múltiples funciones**:
```python
x = np.linspace(-5, 5, 1000)

fig, ax = plt.subplots()
ax.plot(x, x**2, 'r-', linewidth=2, label=r'$y=x^2$')
ax.plot(x, 2*x + 1, 'b--', linewidth=2, label=r'$y=2x+1$')
ax.plot(x, np.sin(x), 'g:', linewidth=2, label=r'$y=\sin(x)$')

ax.legend()
ax.grid(True)
plt.show()
```

**Funciones trigonométricas**:
```python
x = np.linspace(0, 2*np.pi, 500)
y_sin = np.sin(x)
y_cos = np.cos(x)

fig, ax = plt.subplots()
ax.plot(x, y_sin, 'b-', label=r'$\sin(x)$')
ax.plot(x, y_cos, 'r-', label=r'$\cos(x)$')

# Marcar puntos especiales
special_x = [0, np.pi/2, np.pi, 3*np.pi/2, 2*np.pi]
ax.scatter(special_x, np.sin(special_x), color='blue', s=50, zorder=5)

ax.set_xticks([0, np.pi/2, np.pi, 3*np.pi/2, 2*np.pi])
ax.set_xticklabels(['0', r'$\frac{\pi}{2}$', r'$\pi$', r'$\frac{3\pi}{2}$', r'$2\pi$'])
ax.grid(True)
ax.legend()
```

### 2. Figuras Geométricas

**Triángulo**:
```python
import matplotlib.pyplot as plt
import matplotlib.patches as patches

fig, ax = plt.subplots()

# Coordenadas del triángulo
vertices = [(0, 0), (4, 0), (2, 3)]
triangle = patches.Polygon(vertices, fill=False, edgecolor='black', linewidth=2)
ax.add_patch(triangle)

# Etiquetas de vértices
ax.text(0, 0, 'A', fontsize=12, ha='right', va='top')
ax.text(4, 0, 'B', fontsize=12, ha='left', va='top')
ax.text(2, 3, 'C', fontsize=12, ha='center', va='bottom')

# Medidas
ax.annotate('', xy=(4, -0.3), xytext=(0, -0.3),
            arrowprops=dict(arrowstyle='<->', lw=1.5))
ax.text(2, -0.5, '4 cm', ha='center', fontsize=10)

ax.set_xlim(-0.5, 4.5)
ax.set_ylim(-1, 3.5)
ax.set_aspect('equal')
ax.axis('off')

plt.savefig('outputs/output_python.png', dpi=300, bbox_inches='tight')
plt.show()
```

**Círculo**:
```python
fig, ax = plt.subplots()

circle = plt.Circle((0, 0), 2, fill=True, facecolor='blue', 
                     alpha=0.3, edgecolor='blue', linewidth=2)
ax.add_patch(circle)

# Radio
ax.plot([0, 2], [0, 0], 'k-', linewidth=1)
ax.text(1, 0.2, 'r=2', fontsize=10)

ax.set_xlim(-3, 3)
ax.set_ylim(-3, 3)
ax.set_aspect('equal')
ax.grid(True, alpha=0.3)
```

**Polígonos**:
```python
# Polígono regular (pentágono)
n = 5
angles = np.linspace(0, 2*np.pi, n+1)
x = np.cos(angles)
y = np.sin(angles)

fig, ax = plt.subplots()
ax.plot(x, y, 'b-', linewidth=2)
ax.fill(x, y, alpha=0.3, color='blue')
ax.scatter(x[:-1], y[:-1], color='red', s=50, zorder=5)

ax.set_aspect('equal')
ax.grid(True)
```

### 3. Estadística

**Gráfico de barras**:
```python
categories = ['A', 'B', 'C', 'D', 'E']
values = [12, 18, 7, 22, 15]

fig, ax = plt.subplots(figsize=(10, 6))
bars = ax.bar(categories, values, color='#4CAF50', width=0.6, 
               edgecolor='black', linewidth=1.2)

# Valores encima de las barras
for bar in bars:
    height = bar.get_height()
    ax.text(bar.get_x() + bar.get_width()/2., height,
            f'{int(height)}',
            ha='center', va='bottom', fontsize=10)

ax.set_xlabel('Categoría', fontsize=12)
ax.set_ylabel('Frecuencia', fontsize=12)
ax.set_title('Distribución de respuestas', fontsize=14)
ax.grid(axis='y', alpha=0.3)

plt.savefig('outputs/output_python.png', dpi=300, bbox_inches='tight')
plt.show()
```

**Histograma**:
```python
# Datos
np.random.seed(42)
data = np.random.normal(100, 15, 1000)

fig, ax = plt.subplots(figsize=(10, 6))
n, bins, patches = ax.hist(data, bins=20, edgecolor='black', 
                            color='skyblue', alpha=0.7)

ax.set_xlabel('Valor', fontsize=12)
ax.set_ylabel('Frecuencia', fontsize=12)
ax.set_title('Histograma', fontsize=14)
ax.grid(axis='y', alpha=0.3)
```

**Diagrama de dispersión**:
```python
np.random.seed(42)
x = np.random.rand(50) * 100
y = 2*x + np.random.randn(50)*10

fig, ax = plt.subplots(figsize=(8, 6))
ax.scatter(x, y, c='blue', s=50, alpha=0.6, edgecolors='black')

# Línea de tendencia
z = np.polyfit(x, y, 1)
p = np.poly1d(z)
ax.plot(x, p(x), 'r--', linewidth=2, label='Tendencia')

ax.set_xlabel('Variable X', fontsize=12)
ax.set_ylabel('Variable Y', fontsize=12)
ax.legend()
ax.grid(True, alpha=0.3)
```

**Gráfico circular (Pie)**:
```python
sizes = [30, 25, 20, 25]
labels = ['Categoría A', 'Categoría B', 'Categoría C', 'Categoría D']
colors = ['#FF6B6B', '#4ECDC4', '#45B7D1', '#FFA07A']
explode = (0.05, 0, 0, 0)

fig, ax = plt.subplots(figsize=(8, 8))
wedges, texts, autotexts = ax.pie(sizes, explode=explode, labels=labels,
                                    colors=colors, autopct='%1.1f%%',
                                    startangle=90, textprops={'fontsize': 11})

for autotext in autotexts:
    autotext.set_color('white')
    autotext.set_fontweight('bold')

ax.set_title('Distribución Porcentual', fontsize=14)
```

### 4. Geometría Avanzada

**Ángulos**:
```python
from matplotlib.patches import Arc

fig, ax = plt.subplots()

# Triángulo
triangle = [(0, 0), (3, 0), (2, 2)]
ax.plot([p[0] for p in triangle] + [0], 
        [p[1] for p in triangle] + [0], 'b-', linewidth=2)

# Ángulo
angle = Arc((0, 0), 1, 1, angle=0, theta1=0, theta2=33.7, 
            color='red', linewidth=2)
ax.add_patch(angle)
ax.text(0.5, 0.2, r'$\theta$', fontsize=12)

ax.set_xlim(-0.5, 3.5)
ax.set_ylim(-0.5, 2.5)
ax.set_aspect('equal')
ax.grid(True, alpha=0.3)
```

**Vectores**:
```python
fig, ax = plt.subplots()

# Vectores
vectors = [(0, 0, 3, 2), (0, 0, -2, 3), (0, 0, 2, -1)]
colors = ['red', 'blue', 'green']
labels = [r'$\vec{u}$', r'$\vec{v}$', r'$\vec{w}$']

for (x, y, dx, dy), color, label in zip(vectors, colors, labels):
    ax.arrow(x, y, dx, dy, head_width=0.2, head_length=0.2, 
             fc=color, ec=color, linewidth=2, label=label)

ax.set_xlim(-3, 4)
ax.set_ylim(-2, 4)
ax.set_aspect('equal')
ax.axhline(y=0, color='k', linewidth=0.5)
ax.axvline(x=0, color='k', linewidth=0.5)
ax.grid(True, alpha=0.3)
ax.legend()
```

## Estilos y Personalización

### Colores
```python
# Colores por nombre
'red', 'blue', 'green', 'yellow', 'cyan', 'magenta', 'black', 'white'

# Códigos hexadecimales
'#FF0000', '#00FF00', '#0000FF', '#4CAF50'

# RGB
(0.5, 0.2, 0.8)  # Valores entre 0 y 1

# Transparencia
alpha=0.5  # 50% transparente
```

### Estilos de Línea
```python
'-'   # Sólida
'--'  # Discontinua
':'   # Punteada
'-.'  # Raya-punto

# Grosor
linewidth=0.5  # Delgada
linewidth=2    # Media
linewidth=3    # Gruesa
```

### Marcadores
```python
'o'   # Círculo
's'   # Cuadrado
'^'   # Triángulo arriba
'v'   # Triángulo abajo
'*'   # Estrella
'+'   # Cruz
'x'   # X
'D'   # Diamante
```

### Texto Matemático (LaTeX)
```python
r'$x^2$'                    # Potencia
r'$\frac{a}{b}$'           # Fracción
r'$\sqrt{x}$'              # Raíz cuadrada
r'$\sin(x)$'               # Seno
r'$\alpha, \beta, \gamma$' # Griegas
r'$\int_0^1 f(x)dx$'       # Integral
```

## Plantillas Completas

### Plantilla: Función Matemática
```python
import matplotlib.pyplot as plt
import numpy as np

# Configuración
plt.rcParams['font.size'] = 11
plt.rcParams['figure.figsize'] = (10, 6)

# Datos
x = np.linspace(-5, 5, 1000)
y = x**2 - 4*x + 3

# Puntos especiales
raices = [1, 3]
vertice = (2, -1)

# Gráfico
fig, ax = plt.subplots()

# Función principal
ax.plot(x, y, 'b-', linewidth=2.5, label=r'$f(x)=x^2-4x+3$')

# Puntos especiales
ax.scatter(raices, [0, 0], color='red', s=100, zorder=5, label='Raíces')
ax.scatter([vertice[0]], [vertice[1]], color='green', s=100, 
           marker='s', zorder=5, label='Vértice')

# Anotaciones
ax.annotate('Raíz (1,0)', xy=(1, 0), xytext=(0.5, 2),
            arrowprops=dict(arrowstyle='->', color='red'),
            fontsize=10)
ax.annotate('Vértice (2,-1)', xy=vertice, xytext=(3.5, -0.5),
            arrowprops=dict(arrowstyle='->', color='green'),
            fontsize=10)

# Ejes
ax.axhline(y=0, color='k', linewidth=0.8)
ax.axvline(x=0, color='k', linewidth=0.8)
ax.grid(True, alpha=0.3, linestyle='--')

# Etiquetas
ax.set_xlabel('$x$', fontsize=13)
ax.set_ylabel('$y$', fontsize=13)
ax.set_title('Función Cuadrática', fontsize=15, fontweight='bold')
ax.legend(loc='upper right', fontsize=10)

# Límites
ax.set_xlim(-1, 5)
ax.set_ylim(-3, 7)

# Guardar
plt.savefig('outputs/output_python.png', dpi=300, bbox_inches='tight')
plt.show()
```

## Mejores Prácticas

1. **Usar numpy para datos**: Evitar bucles, usar operaciones vectorizadas
2. **Alta resolución**: Siempre guardar con `dpi=300`
3. **Nombres descriptivos**: Variables con nombres claros
4. **Comentarios**: Documentar secciones del código
5. **Modularidad**: Separar configuración, datos y visualización
6. **Bbox_inches='tight'**: Evitar recortes al guardar

## Optimización

```python
# MAL: Bucle lento
y = []
for xi in x:
    y.append(xi**2)

# BIEN: Vectorización numpy
y = x**2

# MAL: Múltiples figuras sin cerrar
for i in range(100):
    plt.plot(data[i])
    plt.savefig(f'fig{i}.png')

# BIEN: Cerrar o reutilizar figura
fig, ax = plt.subplots()
for i in range(100):
    ax.clear()
    ax.plot(data[i])
    plt.savefig(f'fig{i}.png')
plt.close()
```

## Validación

Antes de entregar el código:

- [ ] Ejecuta sin errores
- [ ] Genera imagen en outputs/
- [ ] Todos los elementos son visibles
- [ ] Colores correctos
- [ ] Proporciones adecuadas
- [ ] Texto legible
- [ ] Código documentado

## Salida
Código Python completo y ejecutable, guardado en `outputs/output_python.py`.

