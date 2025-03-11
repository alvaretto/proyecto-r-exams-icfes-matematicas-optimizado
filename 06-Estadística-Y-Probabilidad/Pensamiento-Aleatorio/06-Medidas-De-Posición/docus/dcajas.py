import matplotlib.pyplot as plt
import numpy as np

# Datos ordenados
estaturas = [155, 156, 162, 163, 164, 165, 167, 168, 170, 171, 172]

# Calcular cuartiles manualmente según la tabla
Q1 = 162   # Tercer dato
Q2 = 165   # Sexto dato (mediana)
Q3 = 170   # Noveno dato
minimo = 155
maximo = 172

# Crear una figura y configurar el boxplot manual
plt.figure(figsize=(8, 6))
boxplot = plt.boxplot(
    [estaturas],
    vert=True,
    patch_artist=True,
    whis=[0, 100],  # Bigotes desde mínimo hasta máximo
    widths=0.6,
    showfliers=False  # Ocultar outliers
)

# Sobrescribir los valores del boxplot
boxplot['boxes'][0].set_ydata([Q1, Q1, Q3, Q3, Q1])  # Caja desde Q1 hasta Q3
boxplot['medians'][0].set_ydata([Q2, Q2])             # Línea de la mediana
boxplot['whiskers'][0].set_ydata([minimo, Q1])        # Bigote inferior
boxplot['whiskers'][1].set_ydata([Q3, maximo])        # Bigote superior

# Personalización
plt.title("Diagrama de Cajas de Estaturas (Ajustado)")
plt.ylabel("Estatura (cm)")
plt.yticks(np.arange(150, 175, 5))
plt.grid(axis='y', linestyle='--', alpha=0.7)

# Mostrar el gráfico
plt.show()
