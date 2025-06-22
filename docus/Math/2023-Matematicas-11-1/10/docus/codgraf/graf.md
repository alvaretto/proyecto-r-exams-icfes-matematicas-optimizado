Aquí tienes el código para generar cada uno de los diagramas de árbol en Python, R y TikZ.

**Diagrama A, B y D (son visualmente iguales en estructura)**

Vamos a generar el código para A, B y D, ya que estructuralmente son iguales y sólo difieren en la etiqueta visual en la imagen presentada.

**Python (Matplotlib):**

```python
import matplotlib.pyplot as plt

def dibujar_diagrama_abd(ax, x_start, y_start, labels_cd):
    nivel_x = [x_start, x_start + 1, x_start + 2]
    nivel_y_base = [y_start, y_start + 1.5, y_start + 0, y_start - 1.5]
    y_spacing = 0.8

    # Nivel 1 (CD1)
    ax.text(nivel_x[0], nivel_y_base[0] + y_spacing, labels_cd[0], ha='center', va='bottom')
    ax.plot([nivel_x[0], nivel_x[1]], [nivel_y_base[0], nivel_y_base[1]], 'k-', lw=1) # S
    ax.plot([nivel_x[0], nivel_x[1]], [nivel_y_base[0], nivel_y_base[3]], 'k-', lw=1) # M
    ax.text(nivel_x[1], nivel_y_base[1], 'S', ha='center', va='center', bbox=dict(facecolor='lightblue', edgecolor='black', boxstyle='square,pad=0.3'))
    ax.text(nivel_x[1], nivel_y_base[3], 'M', ha='center', va='center', bbox=dict(facecolor='lightblue', edgecolor='black', boxstyle='square,pad=0.3'))

    # Nivel 2 (CD2)
    ax.text(nivel_x[1], nivel_y_base[1] + y_spacing, labels_cd[1], ha='center', va='bottom')
    ax.plot([nivel_x[1], nivel_x[2]], [nivel_y_base[1], nivel_y_base[0]], 'k-', lw=1) # S-S
    ax.plot([nivel_x[1], nivel_x[2]], [nivel_y_base[1], nivel_y_base[2]], 'k-', lw=1) # S-M
    ax.plot([nivel_x[1], nivel_x[2]], [nivel_y_base[3], nivel_y_base[2] + 2*y_spacing], 'k-', lw=1) # M-S
    ax.plot([nivel_x[1], nivel_x[2]], [nivel_y_base[3], nivel_y_base[3] - 2*y_spacing], 'k-', lw=1) # M-M

    ax.text(nivel_x[2], nivel_y_base[0], 'S', ha='center', va='center', bbox=dict(facecolor='lightblue', edgecolor='black', boxstyle='square,pad=0.3'))
    ax.text(nivel_x[2], nivel_y_base[2], 'M', ha='center', va='center', bbox=dict(facecolor='lightblue', edgecolor='black', boxstyle='square,pad=0.3'))
    ax.text(nivel_x[2], nivel_y_base[2] + 2*y_spacing, 'S', ha='center', va='center', bbox=dict(facecolor='lightblue', edgecolor='black', boxstyle='square,pad=0.3'))
    ax.text(nivel_x[2], nivel_y_base[3] - 2*y_spacing, 'M', ha='center', va='center', bbox=dict(facecolor='lightblue', edgecolor='black', boxstyle='square,pad=0.3'))

    # Nivel 3 (CD3)
    ax.text(nivel_x[2], nivel_y_base[0] + y_spacing, labels_cd[2], ha='center', va='bottom')
    ax.plot([nivel_x[2], nivel_x[2]+1], [nivel_y_base[0], nivel_y_base[0]-y_spacing/2], 'k-', lw=1) # S-S
    ax.plot([nivel_x[2], nivel_x[2]+1], [nivel_y_base[0], nivel_y_base[0]+y_spacing/2], 'k-', lw=1) # S-S
    ax.plot([nivel_x[2], nivel_y_base[2],], [nivel_x[2]+1, nivel_y_base[2]-y_spacing/2],'k-', lw=1) # M-M
    ax.plot([nivel_x[2], nivel_y_base[2],], [nivel_x[2]+1, nivel_y_base[2]+y_spacing/2], 'k-', lw=1) # M-M
    ax.plot([nivel_x[2], nivel_y_base[2] + 2*y_spacing], [nivel_x[2]+1, nivel_y_base[2] + 2*y_spacing-y_spacing/2 ],'k-', lw=1) # S-S
    ax.plot([nivel_x[2], nivel_y_base[2] + 2*y_spacing], [nivel_x[2]+1, nivel_y_base[2] + 2*y_spacing+y_spacing/2 ], 'k-', lw=1) # S-S
    ax.plot([nivel_x[2], nivel_y_base[3] - 2*y_spacing], [nivel_x[2]+1, nivel_y_base[3] - 2*y_spacing-y_spacing/2 ],'k-', lw=1) # M-M
    ax.plot([nivel_x[2], nivel_y_base[3] - 2*y_spacing], [nivel_x[2]+1, nivel_y_base[3] - 2*y_spacing+y_spacing/2 ], 'k-', lw=1) # M-M


    ax.text(nivel_x[2]+1, nivel_y_base[0]-y_spacing/2, 'S', ha='center', va='center', bbox=dict(facecolor='lightblue', edgecolor='black', boxstyle='square,pad=0.3'))
    ax.text(nivel_x[2]+1, nivel_y_base[0]+y_spacing/2, 'M', ha='center', va='center', bbox=dict(facecolor='lightblue', edgecolor='black', boxstyle='square,pad=0.3'))
    ax.text(nivel_x[2]+1, nivel_y_base[2]-y_spacing/2, 'S', ha='center', va='center', bbox=dict(facecolor='lightblue', edgecolor='black', boxstyle='square,pad=0.3'))
    ax.text(nivel_x[2]+1, nivel_y_base[2]+y_spacing/2, 'M', ha='center', va='center', bbox=dict(facecolor='lightblue', edgecolor='black', boxstyle='square,pad=0.3'))
    ax.text(nivel_x[2]+1, nivel_y_base[2] + 2*y_spacing-y_spacing/2, 'S', ha='center', va='center', bbox=dict(facecolor='lightblue', edgecolor='black', boxstyle='square,pad=0.3'))
    ax.text(nivel_x[2]+1, nivel_y_base[2] + 2*y_spacing+y_spacing/2, 'M', ha='center', va='center', bbox=dict(facecolor='lightblue', edgecolor='black', boxstyle='square,pad=0.3'))
    ax.text(nivel_x[2]+1, nivel_y_base[3] - 2*y_spacing-y_spacing/2, 'S', ha='center', va='center', bbox=dict(facecolor='lightblue', edgecolor='black', boxstyle='square,pad=0.3'))
    ax.text(nivel_x[2]+1, nivel_y_base[3] - 2*y_spacing+y_spacing/2, 'M', ha='center', va='center', bbox=dict(facecolor='lightblue', edgecolor='black', boxstyle='square,pad=0.3'))


    ax.set_xlim(x_start-0.5, x_start + 3 + 0.5)
    ax.set_ylim(y_start - 3, y_start + 3)
    ax.axis('off')


fig, axs = plt.subplots(1, 3, figsize=(12, 5))

dibujar_diagrama_abd(axs[0], 0, 0, ['CD1','CD2','CD3'])
axs[0].set_title('Diagrama A')

dibujar_diagrama_abd(axs[1], 0, 0, ['CD1','CD2','CD3'])
axs[1].set_title('Diagrama B')

dibujar_diagrama_abd(axs[2], 0, 0, ['CD1','CD2','CD3'])
axs[2].set_title('Diagrama D')


plt.tight_layout()
plt.show()

```

**R (Base Graphics):**

```R
dibujar_diagrama_abd_r <- function(x_inicio, y_inicio, labels_cd, titulo) {
  plot(NULL, xlim=c(x_inicio-0.5, x_inicio + 3 + 0.5), ylim=c(y_inicio - 3, y_inicio + 3), axes=FALSE, xlab="", ylab="")

  nivel_x <- c(x_inicio, x_inicio + 1, x_inicio + 2)
  nivel_y_base <- c(y_inicio, y_inicio + 1.5, y_inicio + 0, y_inicio - 1.5)
  y_espaciado <- 0.8

  # Nivel 1 (CD1)
  text(nivel_x[0], nivel_y_base[0] + y_espaciado, labels_cd[1], pos=3)
  segments(nivel_x[0], nivel_y_base[0], nivel_x[1], nivel_y_base[1]) # S
  segments(nivel_x[0], nivel_y_base[0], nivel_x[1], nivel_y_base[3]) # M
  rect(nivel_x[1] - 0.2, nivel_y_base[1] - 0.2, nivel_x[1] + 0.2, nivel_y_base[1] + 0.2, col="lightblue", border="black")
  text(nivel_x[1], nivel_y_base[1], 'S')
  rect(nivel_x[1] - 0.2, nivel_y_base[3] - 0.2, nivel_x[1] + 0.2, nivel_y_base[3] + 0.2, col="lightblue", border="black")
  text(nivel_x[1], nivel_y_base[3], 'M')

  # Nivel 2 (CD2)
  text(nivel_x[1], nivel_y_base[1] + y_espaciado, labels_cd[2], pos=3)
  segments(nivel_x[1], nivel_y_base[1], nivel_x[2], nivel_y_base[0]) # S-S
  segments(nivel_x[1], nivel_y_base[1], nivel_x[2], nivel_y_base[2]) # S-M
  segments(nivel_x[1], nivel_y_base[3], nivel_x[2], nivel_y_base[2] + 2*y_espaciado) # M-S
  segments(nivel_x[1], nivel_y_base[3], nivel_x[2], nivel_y_base[3] - 2*y_espaciado) # M-M

  rect(nivel_x[2] - 0.2, nivel_y_base[0] - 0.2, nivel_x[2] + 0.2, nivel_y_base[0] + 0.2, col="lightblue", border="black")
  text(nivel_x[2], nivel_y_base[0], 'S')
  rect(nivel_x[2] - 0.2, nivel_y_base[2] - 0.2, nivel_x[2] + 0.2, nivel_y_base[2] + 0.2, col="lightblue", border="black")
  text(nivel_x[2], nivel_y_base[2], 'M')
    rect(nivel_x[2] - 0.2, nivel_y_base[2] + 2*y_espaciado - 0.2, nivel_x[2] + 0.2, nivel_y_base[2] + 2*y_espaciado+ 0.2, col="lightblue", border="black")
  text(nivel_x[2], nivel_y_base[2] + 2*y_espaciado, 'S')
    rect(nivel_x[2] - 0.2, nivel_y_base[3] - 2*y_espaciado - 0.2, nivel_x[2] + 0.2, nivel_y_base[3] - 2*y_espaciado+ 0.2, col="lightblue", border="black")
  text(nivel_x[2], nivel_y_base[3] - 2*y_espaciado, 'M')

  # Nivel 3 (CD3)
  text(nivel_x[2], nivel_y_base[0] + y_espaciado, labels_cd[3], pos=3)
  segments(nivel_x[2], nivel_y_base[0], nivel_x[2]+1, nivel_y_base[0]-y_espaciado/2) # S-S
  segments(nivel_x[2], nivel_y_base[0], nivel_x[2]+1, nivel_y_base[0]+y_espaciado/2) # S-M
    segments(nivel_x[2], nivel_y_base[2], nivel_x[2]+1, nivel_y_base[2]-y_espaciado/2) # M-S
  segments(nivel_x[2], nivel_y_base[2], nivel_x[2]+1, nivel_y_base[2]+y_espaciado/2) # M-M
    segments(nivel_x[2], nivel_y_base[2]+ 2*y_espaciado, nivel_x[2]+1, nivel_y_base[2]+ 2*y_espaciado-y_espaciado/2) # M-S
  segments(nivel_x[2], nivel_y_base[2]+ 2*y_espaciado, nivel_x[2]+1, nivel_y_base[2]+ 2*y_espaciado+y_espaciado/2) # M-M
    segments(nivel_x[2], nivel_y_base[3] - 2*y_espaciado, nivel_x[2]+1, nivel_y_base[3] - 2*y_espaciado-y_espaciado/2) # M-S
  segments(nivel_x[2], nivel_y_base[3] - 2*y_espaciado, nivel_x[2]+1, nivel_y_base[3] - 2*y_espaciado+y_espaciado/2) # M-M


  rect(nivel_x[2]+1 - 0.2, nivel_y_base[0]-y_espaciado/2 - 0.2, nivel_x[2]+1 + 0.2, nivel_y_base[0]-y_espaciado/2 + 0.2, col="lightblue", border="black")
  text(nivel_x[2]+1, nivel_y_base[0]-y_espaciado/2, 'S')
  rect(nivel_x[2]+1 - 0.2, nivel_y_base[0]+y_espaciado/2 - 0.2, nivel_x[2]+1 + 0.2, nivel_y_base[0]+y_espaciado/2 + 0.2, col="lightblue", border="black")
  text(nivel_x[2]+1, nivel_y_base[0]+y_espaciado/2, 'M')
  rect(nivel_x[2]+1 - 0.2, nivel_y_base[2]-y_espaciado/2 - 0.2, nivel_x[2]+1 + 0.2, nivel_y_base[2]-y_espaciado/2 + 0.2, col="lightblue", border="black")
  text(nivel_x[2]+1, nivel_y_base[2]-y_espaciado/2, 'S')
  rect(nivel_x[2]+1 - 0.2, nivel_y_base[2]+y_espaciado/2 - 0.2, nivel_x[2]+1 + 0.2, nivel_y_base[2]+y_espaciado/2 + 0.2, col="lightblue", border="black")
  text(nivel_x[2]+1, nivel_y_base[2]+y_espaciado/2, 'M')
    rect(nivel_x[2]+1 - 0.2, nivel_y_base[2]+ 2*y_espaciado-y_espaciado/2 - 0.2, nivel_x[2]+1 + 0.2, nivel_y_base[2]+ 2*y_espaciado-y_espaciado/2 + 0.2, col="lightblue", border="black")
  text(nivel_x[2]+1, nivel_y_base[2]+ 2*y_espaciado-y_espaciado/2, 'S')
  rect(nivel_x[2]+1 - 0.2, nivel_y_base[2]+ 2*y_espaciado+y_espaciado/2 - 0.2, nivel_x[2]+1 + 0.2, nivel_y_base[2]+ 2*y_espaciado+y_espaciado/2 + 0.2, col="lightblue", border="black")
  text(nivel_x[2]+1, nivel_y_base[2]+ 2*y_espaciado+y_espaciado/2, 'M')
    rect(nivel_x[2]+1 - 0.2, nivel_y_base[3]- 2*y_espaciado-y_espaciado/2 - 0.2, nivel_x[2]+1 + 0.2, nivel_y_base[3]- 2*y_espaciado-y_espaciado/2 + 0.2, col="lightblue", border="black")
  text(nivel_x[2]+1, nivel_y_base[3]- 2*y_espaciado-y_espaciado/2, 'S')
  rect(nivel_x[2]+1 - 0.2, nivel_y_base[3]- 2*y_espaciado+y_espaciado/2 - 0.2, nivel_x[2]+1 + 0.2, nivel_y_base[3]- 2*y_espaciado+y_espaciado/2 + 0.2, col="lightblue", border="black")
  text(nivel_x[2]+1, nivel_y_base[3]- 2*y_espaciado+y_espaciado/2, 'M')


  title(titulo)
}

par(mfrow=c(1,3))
dibujar_diagrama_abd_r(0, 0, c("",'CD1','CD2','CD3'), "Diagrama A")
dibujar_diagrama_abd_r(0, 0, c("",'CD1','CD2','CD3'), "Diagrama B")
dibujar_diagrama_abd_r(0, 0, c("",'CD1','CD2','CD3'), "Diagrama D")
par(mfrow=c(1,1)) # reset to default
```

**TikZ (LaTeX):**

```latex
\documentclass[tikz,border=2mm]{standalone}
\usepackage{tikz}
\usetikzlibrary{positioning}

\begin{document}
\begin{tikzpicture}
    \node (CD1) at (0,0) {CD1};
    \node[below left=of CD1] (CD1-S) {S};
    \node[below right=of CD1-S] (CD2) {CD2};
    \node[below left=of CD2] (CD2-S) {S};
    \node[below right=of CD2-S] (CD3) {CD3};
    \node[below left=of CD3] (CD3-S1) {S};
    \node[below right=of CD3-S1] (CD3-M1) {M};
    \node[below right=of CD2] (CD2-M) {M};
    \node[below left=of CD2-M] (CD3-S2) {S};
    \node[below right=of CD3-S2] (CD3-M2) {M};
    \node[below right=of CD1] (CD1-M) {M};
    \node[below left=of CD1-M] (CD2-2) {CD2};
    \node[below left=of CD2-2] (CD2-S2) {S};
    \node[below right=of CD2-S2] (CD3-2) {CD3};
    \node[below left=of CD3-2] (CD3-S3) {S};
    \node[below right=of CD3-S3] (CD3-M3) {M};
    \node[below right=of CD2-2] (CD2-M2) {M};
    \node[below left=of CD2-M2] (CD3-4) {CD3};
    \node[below left=of CD3-4] (CD3-S4) {S};
    \node[below right=of CD3-4] (CD3-M4) {M};

    \draw (CD1) -- (CD1-S);
    \draw (CD1-S) -- (CD2);
    \draw (CD2) -- (CD2-S);
    \draw (CD2-S) -- (CD3);
    \draw (CD3) -- (CD3-S1);
    \draw (CD3) -- (CD3-M1);
    \draw (CD2) -- (CD2-M);
    \draw (CD2-M) -- (CD3-S2);
    \draw (CD2-M) -- (CD3-M2);
    \draw (CD1) -- (CD1-M);
    \draw (CD1-M) -- (CD2-2);
    \draw (CD2-2) -- (CD2-S2);
    \draw (CD2-S2) -- (CD3-2);
    \draw (CD3-2) -- (CD3-S3);
    \draw (CD3-2) -- (CD3-M3);
    \draw (CD2-2) -- (CD2-M2);
    \draw (CD2-M2) -- (CD3-4);
    \draw (CD3-4) -- (CD3-S4);
    \draw (CD3-4) -- (CD3-M4);

    \foreach \n in {CD1-S, CD1-M, CD2-S, CD2-M, CD2-S2, CD2-M2, CD3-S1, CD3-M1, CD3-S2, CD3-M2, CD3-S3, CD3-M3, CD3-S4, CD3-M4}
        \node[fill=lightblue, inner sep=2pt, rectangle, rounded corners, fit=({\n})] at (\n) {};
\end{tikzpicture}

\begin{tikzpicture}
    \node (CD1) at (0,0) {CD1};
    \node[below left=of CD1] (CD1-S) {S};
    \node[below right=of CD1-S] (CD2) {CD2};
    \node[below left=of CD2] (CD2-S) {S};
    \node[below right=of CD2-S] (CD3) {CD3};
    \node[below left=of CD3] (CD3-S1) {S};
    \node[below right=of CD3-S1] (CD3-M1) {M};
    \node[below right=of CD2] (CD2-M) {M};
    \node[below left=of CD2-M] (CD3-S2) {S};
    \node[below right=of CD3-S2] (CD3-M2) {M};
    \node[below right=of CD1] (CD1-M) {M};
    \node[below left=of CD1-M] (CD2-2) {CD2};
    \node[below left=of CD2-2] (CD2-S2) {S};
    \node[below right=of CD2-S2] (CD3-2) {CD3};
    \node[below left=of CD3-2] (CD3-S3) {S};
    \node[below right=of CD3-S3] (CD3-M3) {M};
    \node[below right=of CD2-2] (CD2-M2) {M};
    \node[below left=of CD2-M2] (CD3-4) {CD3};
    \node[below left=of CD3-4] (CD3-S4) {S};
    \node[below right=of CD3-4] (CD3-M4) {M};

    \draw (CD1) -- (CD1-S);
    \draw (CD1-S) -- (CD2);
    \draw (CD2) -- (CD2-S);
    \draw (CD2-S) -- (CD3);
    \draw (CD3) -- (CD3-S1);
    \draw (CD3) -- (CD3-M1);
    \draw (CD2) -- (CD2-M);
    \draw (CD2-M) -- (CD3-S2);
    \draw (CD2-M) -- (CD3-M2);
    \draw (CD1) -- (CD1-M);
    \draw (CD1-M) -- (CD2-2);
    \draw (CD2-2) -- (CD2-S2);
    \draw (CD2-S2) -- (CD3-2);
    \draw (CD3-2) -- (CD3-S3);
    \draw (CD3-2) -- (CD3-M3);
    \draw (CD2-2) -- (CD2-M2);
    \draw (CD2-M2) -- (CD3-4);
    \draw (CD3-4) -- (CD3-S4);
    \draw (CD3-4) -- (CD3-M4);

    \foreach \n in {CD1-S, CD1-M, CD2-S, CD2-M, CD2-S2, CD2-M2, CD3-S1, CD3-M1, CD3-S2, CD3-M2, CD3-S3, CD3-M3, CD3-S4, CD3-M4}
        \node[fill=lightblue, inner sep=2pt, rectangle, rounded corners, fit=({\n})] at (\n) {};
\end{tikzpicture}

\begin{tikzpicture}
    \node (CD1) at (0,0) {CD1};
    \node[below left=of CD1] (CD1-S) {S};
    \node[below right=of CD1-S] (CD2) {CD2};
    \node[below left=of CD2] (CD2-S) {S};
    \node[below right=of CD2-S] (CD3) {CD3};
    \node[below left=of CD3] (CD3-S1) {S};
    \node[below right=of CD3-S1] (CD3-M1) {M};
    \node[below right=of CD2] (CD2-M) {M};
    \node[below left=of CD2-M] (CD3-S2) {S};
    \node[below right=of CD3-S2] (CD3-M2) {M};
    \node[below right=of CD1] (CD1-M) {M};
    \node[below left=of CD1-M] (CD2-2) {CD2};
    \node[below left=of CD2-2] (CD2-S2) {S};
    \node[below right=of CD2-S2] (CD3-2) {CD3};
    \node[below left=of CD3-2] (CD3-S3) {S};
    \node[below right=of CD3-S3] (CD3-M3) {M};
    \node[below right=of CD2-2] (CD2-M2) {M};
    \node[below left=of CD2-M2] (CD3-4) {CD3};
    \node[below left=of CD3-4] (CD3-S4) {S};
    \node[below right=of CD3-4] (CD3-M4) {M};

    \draw (CD1) -- (CD1-S);
    \draw (CD1-S) -- (CD2);
    \draw (CD2) -- (CD2-S);
    \draw (CD2-S) -- (CD3);
    \draw (CD3) -- (CD3-S1);
    \draw (CD3) -- (CD3-M1);
    \draw (CD2) -- (CD2-M);
    \draw (CD2-M) -- (CD3-S2);
    \draw (CD2-M) -- (CD3-M2);
    \draw (CD1) -- (CD1-M);
    \draw (CD1-M) -- (CD2-2);
    \draw (CD2-2) -- (CD2-S2);
    \draw (CD2-S2) -- (CD3-2);
    \draw (CD3-2) -- (CD3-S3);
    \draw (CD3-2) -- (CD3-M3);
    \draw (CD2-2) -- (CD2-M2);
    \draw (CD2-M2) -- (CD3-4);
    \draw (CD3-4) -- (CD3-S4);
    \draw (CD3-4) -- (CD3-M4);

    \foreach \n in {CD1-S, CD1-M, CD2-S, CD2-M, CD2-S2, CD2-M2, CD3-S1, CD3-M1, CD3-S2, CD3-M2, CD3-S3, CD3-M3, CD3-S4, CD3-M4}
        \node[fill=lightblue, inner sep=2pt, rectangle, rounded corners, fit=({\n})] at (\n) {};
\end{tikzpicture}
\caption*{Diagramas A, B y D}
\end{document}
```

**Diagrama C**

**Python (Matplotlib):**

```python
import matplotlib.pyplot as plt

def dibujar_diagrama_c(ax, x_start, y_start, labels_cd):
    nivel_x = [x_start, x_start + 1]
    nivel_y = [y_start, y_start - 1, y_start - 3]
    x_spacing = 1.2

    # Nivel 1 (CD1)
    ax.text(x_start - x_spacing, nivel_y[0], labels_cd[0], ha='right', va='center', rotation=90)
    ax.plot([nivel_x[0], nivel_x[1]], [nivel_y[0], nivel_y[0] + 0.7], 'k-', lw=1) # S
    ax.plot([nivel_x[0], nivel_x[1]], [nivel_y[0], nivel_y[0] - 0.7], 'k-', lw=1) # M
    ax.text(nivel_x[0], nivel_y[0] + 0.7, 'S', ha='center', va='center', bbox=dict(facecolor='lightblue', edgecolor='black', boxstyle='square,pad=0.3'))
    ax.text(nivel_x[0], nivel_y[0] - 0.7, 'M', ha='center', va='center', bbox=dict(facecolor='lightblue', edgecolor='black', boxstyle='square,pad=0.3'))


    # Nivel 2 (CD2)
    ax.text(x_start - x_spacing, nivel_y[1], labels_cd[1], ha='right', va='center', rotation=90)
    ax.plot([nivel_x[1], nivel_x[1]+1], [nivel_y[0] + 0.7, nivel_y[1] + 0.7], 'k-', lw=1) # S-S
    ax.plot([nivel_x[1], nivel_x[1]+1], [nivel_y[0] + 0.7, nivel_y[1] - 0.7], 'k-', lw=1) # S-M
    ax.plot([nivel_x[1], nivel_x[1]+1], [nivel_y[0] - 0.7, nivel_y[1] + 2.1], 'k-', lw=1) # M-S
    ax.plot([nivel_x[1], nivel_x[1]+1], [nivel_y[0] - 0.7, nivel_y[1] + 0.7], 'k-', lw=1) # M-M

    ax.text(nivel_x[1]+1, nivel_y[1] + 0.7, 'S', ha='center', va='center', bbox=dict(facecolor='lightblue', edgecolor='black', boxstyle='square,pad=0.3'))
    ax.text(nivel_x[1]+1, nivel_y[1] - 0.7, 'M', ha='center', va='center', bbox=dict(facecolor='lightblue', edgecolor='black', boxstyle='square,pad=0.3'))
    ax.text(nivel_x[1]+1, nivel_y[1] + 2.1, 'S', ha='center', va='center', bbox=dict(facecolor='lightblue', edgecolor='black', boxstyle='square,pad=0.3'))
    ax.text(nivel_x[1]+1, nivel_y[1] + 0.7, 'M', ha='center', va='center', bbox=dict(facecolor='lightblue', edgecolor='black', boxstyle='square,pad=0.3'))


    # Nivel 3 (CD3)
    ax.text(x_start - x_spacing, nivel_y[2], labels_cd[2], ha='right', va='center', rotation=90)
    ax.plot([nivel_x[1]+1, nivel_x[1]+2], [nivel_y[1] + 0.7, nivel_y[2] + 0.35], 'k-', lw=1) # S-S
    ax.plot([nivel_x[1]+1, nivel_x[1]+2], [nivel_y[1] + 0.7, nivel_y[2] + 1.05], 'k-', lw=1) # S-M
    ax.plot([nivel_x[1]+1, nivel_x[1]+2], [nivel_y[1] - 0.7, nivel_y[2] - 0.35], 'k-', lw=1) # M-S
    ax.plot([nivel_x[1]+1, nivel_x[1]+2], [nivel_y[1] - 0.7, nivel_y[2] - 1.05], 'k-', lw=1) # M-M
     ax.plot([nivel_x[1]+1, nivel_x[1]+2], [nivel_y[1] + 2.1, nivel_y[2] + 1.75], 'k-', lw=1) # S-S
    ax.plot([nivel_x[1]+1, nivel_x[1]+2], [nivel_y[1] + 2.1, nivel_y[2] + 2.45], 'k-', lw=1) # S-M
    ax.plot([nivel_x[1]+1, nivel_x[1]+2], [nivel_y[1] + 0.7, nivel_y[2] + 0.35], 'k-', lw=1) # M-S # Overlap: no additional unique branch starting point
    ax.plot([nivel_x[1]+1, nivel_x[1]+2], [nivel_y[1] + 0.7, nivel_y[2] + 1.05], 'k-', lw=1) # M-M # Overlap: no additional unique branch starting point


    ax.text(nivel_x[1]+2, nivel_y[2] + 0.35, 'S', ha='center', va='center', bbox=dict(facecolor='lightblue', edgecolor='black', boxstyle='square,pad=0.3'))
    ax.text(nivel_x[1]+2, nivel_y[2] + 1.05, 'M', ha='center', va='center', bbox=dict(facecolor='lightblue', edgecolor='black', boxstyle='square,pad=0.3'))
    ax.text(nivel_x[1]+2, nivel_y[2] - 0.35, 'S', ha='center', va='center', bbox=dict(facecolor='lightblue', edgecolor='black', boxstyle='square,pad=0.3'))
    ax.text(nivel_x[1]+2, nivel_y[2] - 1.05, 'M', ha='center', va='center', bbox=dict(facecolor='lightblue', edgecolor='black', boxstyle='square,pad=0.3'))
     ax.text(nivel_x[1]+2, nivel_y[2] + 1.75, 'S', ha='center', va='center', bbox=dict(facecolor='lightblue', edgecolor='black', boxstyle='square,pad=0.3'))
    ax.text(nivel_x[1]+2, nivel_y[2] + 2.45, 'M', ha='center', va='center', bbox=dict(facecolor='lightblue', edgecolor='black', boxstyle='square,pad=0.3'))
    ax.text(nivel_x[1]+2, nivel_y[2] + 0.35, 'S', ha='center', va='center', bbox=dict(facecolor='lightblue', edgecolor='black', boxstyle='square,pad=0.3')) # Overlap: no additional unique branch starting point
    ax.text(nivel_x[1]+2, nivel_y[2] + 1.05, 'M', ha='center', va='center', bbox=dict(facecolor='lightblue', edgecolor='black', boxstyle='square,pad=0.3')) # Overlap: no additional unique branch starting point


    ax.set_xlim(x_start-2, x_start + 3)
    ax.set_ylim(y_start - 4, y_start + 1)
    ax.axis('off')


fig, ax = plt.subplots(figsize=(6, 5))
dibujar_diagrama_c(ax, 1, 0, ['CD1','CD2','CD3'])
ax.set_title('Diagrama C')


plt.tight_layout()
plt.show()
```

**R (Base Graphics):**

```R
dibujar_diagrama_c_r <- function(x_inicio, y_inicio, labels_cd, titulo) {
  plot(NULL, xlim=c(x_inicio-2, x_inicio + 3), ylim=c(y_inicio - 4, y_inicio + 1), axes=FALSE, xlab="", ylab="")

  nivel_x <- c(x_inicio, x_inicio + 1)
  nivel_y <- c(y_inicio, y_inicio - 1, y_inicio - 3)
  x_espaciado <- 1.2

  # Nivel 1 (CD1)
  text(x_inicio - x_espaciado, nivel_y[1], labels_cd[1], pos=2, srt=90)
  segments(nivel_x[0], nivel_y[0], nivel_x[1], nivel_y[0] + 0.7) # S
  segments(nivel_x[0], nivel_y[0], nivel_x[1], nivel_y[0] - 0.7) # M
  rect(nivel_x[0] - 0.2, nivel_y[0] + 0.7 - 0.2, nivel_x[0] + 0.2, nivel_y[0] + 0.7 + 0.2, col="lightblue", border="black")
  text(nivel_x[0], nivel_y[0] + 0.7, 'S')
  rect(nivel_x[0] - 0.2, nivel_y[0] - 0.7 - 0.2, nivel_x[0] + 0.2, nivel_y[0] - 0.7 + 0.2, col="lightblue", border="black")
  text(nivel_x[0], nivel_y[0] - 0.7, 'M')

  # Nivel 2 (CD2)
   text(x_inicio - x_espaciado, nivel_y[2], labels_cd[2], pos=2, srt=90)
  segments(nivel_x[1], nivel_y[0] + 0.7, nivel_x[1]+1, nivel_y[1] + 0.7) # S-S
  segments(nivel_x[1], nivel_y[0] + 0.7, nivel_x[1]+1, nivel_y[1] - 0.7) # S-M
  segments(nivel_x[1], nivel_y[0] - 0.7, nivel_x[1]+1, nivel_y[1] + 2.1) # M-S
  segments(nivel_x[1], nivel_y[0] - 0.7, nivel_x[1]+1, nivel_y[1] + 0.7 ) # M-M


  rect(nivel_x[1]+1 - 0.2, nivel_y[1] + 0.7 - 0.2, nivel_x[1]+1 + 0.2, nivel_y[1] + 0.7 + 0.2, col="lightblue", border="black")
  text(nivel_x[1]+1, nivel_y[1] + 0.7, 'S')
  rect(nivel_x[1]+1 - 0.2, nivel_y[1] - 0.7 - 0.2, nivel_x[1]+1 + 0.2, nivel_y[1] - 0.7 + 0.2, col="lightblue", border="black")
  text(nivel_x[1]+1, nivel_y[1] - 0.7, 'M')
   rect(nivel_x[1]+1 - 0.2, nivel_y[1] + 2.1 - 0.2, nivel_x[1]+1 + 0.2, nivel_y[1] + 2.1 + 0.2, col="lightblue", border="black")
  text(nivel_x[1]+1, nivel_y[1] + 2.1, 'S')
    rect(nivel_x[1]+1 - 0.2, nivel_y[1] + 0.7 - 0.2, nivel_x[1]+1 + 0.2, nivel_y[1] + 0.7 + 0.2, col="lightblue", border="black")
  text(nivel_x[1]+1, nivel_y[1] + 0.7, 'M')


  # Nivel 3 (CD3)
   text(x_inicio - x_espaciado, nivel_y[3], labels_cd[3], pos=2, srt=90)
  segments(nivel_x[1]+1, nivel_y[1] + 0.7, nivel_x[1]+2, nivel_y[2] + 0.35) # S-S
  segments(nivel_x[1]+1, nivel_y[1] + 0.7, nivel_x[1]+2, nivel_y[2] + 1.05) # S-M
  segments(nivel_x[1]+1, nivel_y[1] - 0.7, nivel_x[1]+2, nivel_y[2] - 0.35) # M-S
  segments(nivel_x[1]+1, nivel_y[1] - 0.7, nivel_x[1]+2, nivel_y[2] - 1.05 ) # M-M
  segments(nivel_x[1]+1, nivel_y[1] + 2.1, nivel_x[1]+2, nivel_y[2] + 1.75) # S-S
  segments(nivel_x[1]+1, nivel_y[1] + 2.1, nivel_x[1]+2, nivel_y[2] + 2.45) # S-M
  segments(nivel_x[1]+1, nivel_y[1] + 0.7, nivel_x[1]+2, nivel_y[2] + 0.35) # M-S # Overlap: No Branch
  segments(nivel_x[1]+1, nivel_y[1] + 0.7, nivel_x[1]+2, nivel_y[2] + 1.05 ) # M-M # Overlap: No Branch


  rect(nivel_x[1]+2 - 0.2, nivel_y[2] + 0.35 - 0.2, nivel_x[1]+2 + 0.2, nivel_y[2] + 0.35 + 0.2, col="lightblue", border="black")
  text(nivel_x[1]+2, nivel_y[2] + 0.35, 'S')
  rect(nivel_x[1]+2 - 0.2, nivel_y[2] + 1.05 - 0.2, nivel_x[1]+2 + 0.2, nivel_y[2] + 1.05 + 0.2, col="lightblue", border="black")
  text(nivel_x[1]+2, nivel_y[2] + 1.05, 'M')
    rect(nivel_x[1]+2 - 0.2, nivel_y[2] - 0.35 - 0.2, nivel_x[1]+2 + 0.2, nivel_y[2] - 0.35 + 0.2, col="lightblue", border="black")
  text(nivel_x[1]+2, nivel_y[2] - 0.35, 'S')
  rect(nivel_x[1]+2 - 0.2, nivel_y[2] - 1.05 - 0.2, nivel_x[1]+2 + 0.2, nivel_y[2] - 1.05 + 0.2, col="lightblue", border="black")
  text(nivel_x[1]+2, nivel_y[2] - 1.05, 'M')
    rect(nivel_x[1]+2 - 0.2, nivel_y[2] + 1.75 - 0.2, nivel_x[1]+2 + 0.2, nivel_y[2] + 1.75 + 0.2, col="lightblue", border="black")
  text(nivel_x[1]+2, nivel_y[2] + 1.75, 'S')
  rect(nivel_x[1]+2 - 0.2, nivel_y[2] + 2.45 - 0.2, nivel_x[1]+2 + 0.2, nivel_y[2] + 2.45 + 0.2, col="lightblue", border="black")
  text(nivel_x[1]+2, nivel_y[2] + 2.45, 'M')
   rect(nivel_x[1]+2 - 0.2, nivel_y[2] + 0.35 - 0.2, nivel_x[1]+2 + 0.2, nivel_y[2] + 0.35 + 0.2, col="lightblue", border="black") # Overlap: No Text
 text(nivel_x[1]+2, nivel_y[2] + 0.35, 'S') # Overlap: No Text
   rect(nivel_x[1]+2 - 0.2, nivel_y[2] + 1.05 - 0.2, nivel_x[1]+2 + 0.2, nivel_y[2] + 1.05 + 0.2, col="lightblue", border="black")# Overlap: No Text
  text(nivel_x[1]+2, nivel_y[2] + 1.05, 'M')# Overlap: No Text


  title(titulo)
}


dibujar_diagrama_c_r(2, 0, c("","CD1","CD2","CD3"), "Diagrama C")

```

**TikZ (LaTeX):**

```latex
\documentclass[tikz,border=2mm]{standalone}
\usepackage{tikz}
\usetikzlibrary{positioning}

\begin{document}
\begin{tikzpicture}
    \node (CD1) at (0,0) [rotate=90, anchor=east] {CD1};
    \node[right=of CD1] (CD1-node) {};
    \node[above left=of CD1-node] (CD1-S) {S};
    \node[below left=of CD1-node] (CD1-M) {M};


    \node (CD2) at (2,-2) [rotate=90, anchor=east] {CD2};
    \node[right=of CD2] (CD2-node) {};
    \node[above left=of CD2-node] (CD2-S) {S};
    \node[below left=of CD2-node] (CD2-M) {M};
    \node[above right=of CD1-S] (CD2-S-start) {};
    \node[below right=of CD1-M] (CD2-M-start) {};


    \node (CD3) at (4,-4) [rotate=90, anchor=east] {CD3};
    \node[right=of CD3] (CD3-node) {};
    \node[above left=of CD3-node] (CD3-S) {S};
    \node[below left=of CD3-node] (CD3-M) {M};
    \node[above right=of CD2-S] (CD3-S-start) {};
    \node[below right=of CD2-M] (CD3-M-start) {};


    \draw (CD1-node) -- (CD1-S);
    \draw (CD1-node) -- (CD1-M);
    \draw (CD1-S) -- (CD2-S-start);
    \draw (CD1-M) -- (CD2-M-start);
    \draw (CD2-node) -- (CD2-S);
    \draw (CD2-node) -- (CD2-M);
    \draw (CD2-S) -- (CD3-S-start);
    \draw (CD2-M) -- (CD3-M-start);
    \draw (CD3-node) -- (CD3-S);
    \draw (CD3-node) -- (CD3-M);


    \foreach \n in {CD1-S, CD1-M, CD2-S, CD2-M, CD3-S, CD3-M}
        \node[fill=lightblue, inner sep=2pt, rectangle, rounded corners, fit=({\n})] at (\n) {};
\end{tikzpicture}
\caption*{Diagrama C}
\end{document}
```

**Nota:**

*   **Python y R:** Estos códigos usan `matplotlib` en Python y gráficos base en R  para dibujar líneas y texto que simulan el diagrama de árbol. Ajusta los parámetros (como `x_start`, `y_start`,  `y_spacing`, `x_spacing`) para modificar la apariencia si es necesario.
*   **TikZ:**  El código TikZ  es más preciso y escalable para gráficos vectoriales en LaTeX.  Utiliza el sistema de nodos de TikZ para posicionar y conectar los elementos del diagrama. He usado  `positioning` library  para simplificar la ubicación relativa de los nodos.
*   **Diagramas A, B y D:** El código para los diagramas A, B y D es el mismo porque estructuralmente son idénticos. Si tienes más detalles sobre alguna diferencia visual sutil, el código se podría ajustar.
*   **Diagrama C en TikZ:** El Diagrama C requiere un enfoque un poco distinto en TikZ debido a su orientación vertical de las etiquetas 'CD1', 'CD2', 'CD3'.

Para usar el código TikZ, necesitas un entorno LaTeX instalado. Para Python y R, asegúrate de tener instaladas las bibliotecas `matplotlib` y las funciones base de R respectivamente.