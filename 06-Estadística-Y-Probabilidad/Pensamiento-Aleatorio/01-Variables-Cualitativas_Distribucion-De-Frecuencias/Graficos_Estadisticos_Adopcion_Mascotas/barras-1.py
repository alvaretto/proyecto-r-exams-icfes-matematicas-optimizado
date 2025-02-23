import matplotlib.pyplot as plt
import numpy as np

# Datos
animales = ["Loro", "Perro", "Gato"]
cantidad = [30, 40, 30]

# Definir colores en tonos de azul
colores_azules = ['#00B3E6', '#0066CC', '#000099']  # Convertido de RGB a HEX

# Crear el gráfico de barras
fig, ax=plt.subplots(figsize=(5, 2.5))

# Ajustar márgenes y estilos de fuente se hace a través de los métodos de matplotlib
bars = ax.bar(animales, cantidad, color=colores_azules)

# Eliminar los ejes superior y derecho para un look más limpio
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)

# Ajustar el estilo de los ejes restantes
ax.spines['left'].set_linewidth(2)  # Eje Y en negrita
ax.spines['bottom'].set_linewidth(2)  # Eje X en negrita

# Añadir líneas de cuadrícula horizontales
ax.yaxis.grid(True, linestyle='--', linewidth=0.7, color='darkgray')

# Ajustar la fuente de los ejes y títulos
plt.subplots_adjust(left=0.1, right=0.9, top=0.9, bottom=0.1)
plt.rc('font', family='serif', size=12)  # Aplica a todas las etiquetas de texto
plt.xticks(fontweight='bold')
plt.yticks(np.arange(0, 41, 10), fontweight='bold')

# Añadir etiquetas de los ejes con estilo en negrita
plt.xlabel("Animal", fontweight='bold')
plt.ylabel("Cantidad de personas \ninteresadas en adoptar", fontweight='bold')

plt.tight_layout()
plt.show()
