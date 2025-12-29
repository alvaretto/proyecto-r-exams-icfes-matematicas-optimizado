# Skill: Generación de Código R (ggplot2)

## Descripción
Habilidad especializada para generar código R profesional usando ggplot2 que reproduzca imágenes matemáticas analizadas.

## Objetivos

- Generar código R sintácticamente correcto y ejecutable
- Usar gramática de gráficos (ggplot2) eficientemente
- Producir visualizaciones de calidad publicable
- Mantener código limpio y bien documentado
- Aprovechar el ecosistema tidyverse

## Fundamentos

### Paquetes Esenciales
```r
library(ggplot2)      # Visualización principal
library(dplyr)        # Manipulación de datos
library(tidyr)        # Reorganización de datos
library(scales)       # Escalas y formatos
library(grid)         # Elementos gráficos de bajo nivel
library(gridExtra)    # Múltiples gráficos
library(ggforce)      # Extensiones geométricas
```

### Configuración Básica
```r
# Configuración de tema predeterminado
theme_set(theme_minimal())

# Opciones gráficas
options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)
```

## Gramática de Gráficos

### Componentes Básicos
```r
ggplot(data, aes(x, y)) +        # Datos y estética
  geom_point() +                  # Geometría
  scale_x_continuous() +          # Escala X
  coord_cartesian() +             # Sistema de coordenadas
  labs(title, x, y) +             # Etiquetas
  theme_minimal()                 # Tema
```

## Capacidades por Tipo de Contenido

### 1. Funciones y Gráficas

**Función básica**:
```r
library(ggplot2)

# Datos
x <- seq(-5, 5, length.out = 1000)
datos <- data.frame(
  x = x,
  y = x^2 - 4*x + 3
)

# Gráfico
p <- ggplot(datos, aes(x, y)) +
  geom_line(color = "blue", size = 1.2) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 0.5) +
  labs(
    title = expression(f(x) == x^2 - 4*x + 3),
    x = "x",
    y = "y"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12)
  )

# Guardar
ggsave("outputs/output_r.png", p, width = 8, height = 6, dpi = 300)
print(p)
```

**Múltiples funciones**:
```r
library(tidyr)

x <- seq(-5, 5, length.out = 1000)
datos <- data.frame(
  x = x,
  cuadratica = x^2,
  lineal = 2*x + 1,
  seno = sin(x)
) %>%
  pivot_longer(cols = -x, names_to = "funcion", values_to = "y")

p <- ggplot(datos, aes(x, y, color = funcion, linetype = funcion)) +
  geom_line(size = 1.2) +
  scale_color_manual(
    values = c("cuadratica" = "red", "lineal" = "blue", "seno" = "green"),
    labels = c(
      "cuadratica" = expression(y == x^2),
      "lineal" = expression(y == 2*x + 1),
      "seno" = expression(y == sin(x))
    )
  ) +
  scale_linetype_manual(
    values = c("cuadratica" = "solid", "lineal" = "dashed", "seno" = "dotted"),
    labels = c(
      "cuadratica" = expression(y == x^2),
      "lineal" = expression(y == 2*x + 1),
      "seno" = expression(y == sin(x))
    )
  ) +
  labs(x = "x", y = "y", color = "Función", linetype = "Función") +
  theme_minimal()
```

**Funciones trigonométricas**:
```r
x <- seq(0, 2*pi, length.out = 500)
datos <- data.frame(
  x = x,
  sin = sin(x),
  cos = cos(x)
)

# Puntos especiales
puntos <- data.frame(
  x = c(0, pi/2, pi, 3*pi/2, 2*pi),
  y = c(0, 1, 0, -1, 0),
  label = c("0", "π/2", "π", "3π/2", "2π")
)

p <- ggplot(datos, aes(x)) +
  geom_line(aes(y = sin, color = "Seno"), size = 1.2) +
  geom_line(aes(y = cos, color = "Coseno"), size = 1.2) +
  geom_point(data = puntos, aes(x, y), size = 3, color = "blue") +
  scale_x_continuous(
    breaks = c(0, pi/2, pi, 3*pi/2, 2*pi),
    labels = c("0", expression(frac(pi, 2)), expression(pi), 
               expression(frac(3*pi, 2)), expression(2*pi))
  ) +
  scale_color_manual(
    values = c("Seno" = "blue", "Coseno" = "red")
  ) +
  labs(x = "x", y = "y", color = "") +
  theme_minimal()
```

### 2. Figuras Geométricas

**Triángulo**:
```r
library(ggplot2)

# Vértices del triángulo
triangulo <- data.frame(
  x = c(0, 4, 2, 0),
  y = c(0, 0, 3, 0)
)

etiquetas <- data.frame(
  x = c(0, 4, 2),
  y = c(0, 0, 3),
  label = c("A", "B", "C")
)

p <- ggplot(triangulo, aes(x, y)) +
  geom_path(size = 1.5, color = "black") +
  geom_point(data = etiquetas, size = 3, color = "red") +
  geom_text(data = etiquetas, aes(label = label), 
            nudge_x = c(-0.2, 0.2, 0), nudge_y = c(-0.2, -0.2, 0.2),
            size = 5) +
  annotate("segment", x = 0, xend = 4, y = -0.3, yend = -0.3,
           arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
  annotate("text", x = 2, y = -0.5, label = "4 cm", size = 4) +
  coord_equal() +
  theme_void() +
  theme(plot.margin = margin(1, 1, 1, 1, "cm"))

ggsave("outputs/output_r.png", p, width = 8, height = 6, dpi = 300)
```

**Círculo**:
```r
library(ggforce)

p <- ggplot() +
  geom_circle(aes(x0 = 0, y0 = 0, r = 2), 
              fill = "blue", alpha = 0.3, color = "blue", size = 1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 2, yend = 0), size = 1) +
  annotate("text", x = 1, y = 0.2, label = "r = 2", size = 4) +
  coord_equal() +
  xlim(-3, 3) +
  ylim(-3, 3) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray", size = 0.3),
    panel.grid.minor = element_blank()
  )
```

**Polígono regular**:
```r
# Pentágono
n <- 5
angulos <- seq(0, 2*pi, length.out = n + 1)
pentagono <- data.frame(
  x = cos(angulos),
  y = sin(angulos)
)

p <- ggplot(pentagono, aes(x, y)) +
  geom_polygon(fill = "blue", alpha = 0.3, color = "blue", size = 1.5) +
  geom_point(size = 3, color = "red") +
  coord_equal() +
  theme_minimal()
```

### 3. Estadística

**Gráfico de barras**:
```r
datos <- data.frame(
  categoria = c("A", "B", "C", "D", "E"),
  valor = c(12, 18, 7, 22, 15)
)

p <- ggplot(datos, aes(x = categoria, y = valor)) +
  geom_col(fill = "#4CAF50", color = "black", width = 0.6, size = 1) +
  geom_text(aes(label = valor), vjust = -0.5, size = 4) +
  labs(
    title = "Distribución de respuestas",
    x = "Categoría",
    y = "Frecuencia"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave("outputs/output_r.png", p, width = 10, height = 6, dpi = 300)
print(p)
```

**Histograma**:
```r
set.seed(42)
datos <- data.frame(
  valor = rnorm(1000, mean = 100, sd = 15)
)

p <- ggplot(datos, aes(x = valor)) +
  geom_histogram(bins = 20, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(
    title = "Histograma",
    x = "Valor",
    y = "Frecuencia"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14)
  )
```

**Diagrama de dispersión**:
```r
set.seed(42)
datos <- data.frame(
  x = runif(50, 0, 100),
  y = 2 * runif(50, 0, 100) + rnorm(50, 0, 10)
)

p <- ggplot(datos, aes(x, y)) +
  geom_point(color = "blue", size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE) +
  labs(
    x = "Variable X",
    y = "Variable Y"
  ) +
  theme_minimal()
```

**Gráfico circular**:
```r
library(ggplot2)

datos <- data.frame(
  categoria = c("A", "B", "C", "D"),
  valor = c(30, 25, 20, 25)
) %>%
  mutate(
    porcentaje = valor / sum(valor) * 100,
    etiqueta = paste0(categoria, "\n", round(porcentaje, 1), "%")
  )

p <- ggplot(datos, aes(x = "", y = valor, fill = categoria)) +
  geom_col(width = 1, color = "white", size = 2) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#FF6B6B", "#4ECDC4", "#45B7D1", "#FFA07A")) +
  geom_text(aes(label = etiqueta), position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 5) +
  labs(title = "Distribución Porcentual") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  )
```

**Boxplot**:
```r
datos <- data.frame(
  grupo = rep(c("A", "B", "C", "D"), each = 50),
  valor = c(rnorm(50, 10, 2), rnorm(50, 12, 3), 
            rnorm(50, 15, 2), rnorm(50, 11, 2.5))
)

p <- ggplot(datos, aes(x = grupo, y = valor, fill = grupo)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.size = 2) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Comparación entre grupos",
    x = "Grupo",
    y = "Valor"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "none"
  )
```

### 4. Geometría Avanzada

**Vectores**:
```r
vectores <- data.frame(
  x = c(0, 0, 0),
  y = c(0, 0, 0),
  xend = c(3, -2, 2),
  yend = c(2, 3, -1),
  label = c("u", "v", "w")
)

p <- ggplot(vectores) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend, color = label),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
               size = 1.5) +
  geom_text(aes(x = xend, y = yend, label = label, color = label),
            nudge_x = 0.3, nudge_y = 0.3, size = 5) +
  scale_color_manual(values = c("u" = "red", "v" = "blue", "w" = "green")) +
  geom_hline(yintercept = 0, size = 0.5) +
  geom_vline(xintercept = 0, size = 0.5) +
  coord_equal() +
  xlim(-3, 4) +
  ylim(-2, 4) +
  theme_minimal() +
  theme(legend.position = "none")
```

## Temas y Personalización

### Temas Predefinidos
```r
theme_minimal()      # Minimalista
theme_classic()      # Clásico
theme_bw()          # Blanco y negro
theme_light()       # Claro
theme_dark()        # Oscuro
theme_void()        # Vacío (sin ejes)
```

### Personalización de Tema
```r
mi_tema <- theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(size = 11, face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = "gray80")
  )

# Aplicar
p <- p + mi_tema
```

### Escalas de Color
```r
# Paletas predefinidas
scale_color_brewer(palette = "Set1")
scale_fill_viridis_d()
scale_color_gradient(low = "blue", high = "red")

# Colores manuales
scale_color_manual(values = c("red", "blue", "green"))
scale_fill_manual(values = c("#FF6B6B", "#4ECDC4", "#45B7D1"))
```

## Plantilla Completa

```r
library(ggplot2)
library(dplyr)

# Configuración inicial
theme_set(theme_minimal())

# Preparación de datos
x <- seq(-5, 5, length.out = 1000)
datos <- data.frame(
  x = x,
  y = x^2 - 4*x + 3
)

# Puntos especiales
raices <- data.frame(x = c(1, 3), y = c(0, 0), tipo = "Raíces")
vertice <- data.frame(x = 2, y = -1, tipo = "Vértice")

# Gráfico principal
p <- ggplot(datos, aes(x, y)) +
  # Función
  geom_line(color = "blue", size = 1.5) +
  
  # Ejes
  geom_hline(yintercept = 0, color = "black", size = 0.8) +
  geom_vline(xintercept = 0, color = "black", size = 0.8) +
  
  # Puntos especiales
  geom_point(data = raices, aes(x, y), color = "red", size = 4) +
  geom_point(data = vertice, aes(x, y), color = "green", size = 4, shape = 15) +
  
  # Anotaciones
  annotate("text", x = 1, y = 0.5, label = "(1, 0)", color = "red", size = 4) +
  annotate("text", x = 3, y = 0.5, label = "(3, 0)", color = "red", size = 4) +
  annotate("text", x = 2, y = -1.5, label = "Vértice\n(2, -1)", 
           color = "green", size = 4) +
  
  # Etiquetas y título
  labs(
    title = expression(f(x) == x^2 - 4*x + 3),
    x = expression(x),
    y = expression(y)
  ) +
  
  # Límites
  coord_cartesian(xlim = c(-1, 5), ylim = c(-3, 7)) +
  
  # Tema
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    axis.title = element_text(size = 13),
    panel.grid.minor = element_blank()
  )

# Guardar
ggsave("outputs/output_r.png", p, width = 10, height = 6, dpi = 300)

# Mostrar
print(p)
```

## Mejores Prácticas

1. **Usar tuberías (%>%)**: Para código más legible
2. **Data frames tidy**: Estructura de datos larga para ggplot2
3. **Nombres descriptivos**: Variables y objetos claros
4. **Comentarios**: Documentar código
5. **Guardar con alta resolución**: dpi = 300
6. **Modularidad**: Separar preparación de datos y visualización

## Validación

Antes de entregar:

- [ ] Código ejecuta sin errores
- [ ] Genera imagen en outputs/
- [ ] Todos los elementos visibles
- [ ] Colores correctos
- [ ] Proporciones adecuadas
- [ ] Texto legible
- [ ] Código documentado

## Salida
Código R completo y ejecutable, guardado en `outputs/output_r.R`.

