**Objetivo:** Configurar RStudio para usar la instalación de Python del sistema de Manjaro de forma persistente (que funcione cada vez que abras RStudio), sin usar entornos virtuales de `reticulate` por defecto.

**Pasos:**

1.  **Encontrar la ruta correcta de Python:**

    *   Abrir una terminal (no RStudio).
    *   Ejecutar: `which python3`
    *   Copiar la ruta completa que se muestra (en tu caso, `/usr/bin/python3`).  Esta es la ruta que usaremos.

2.  **Configurar `.Rprofile` (la clave de la persistencia):**

    *   Abrir o crear el archivo `.Rprofile` en tu directorio principal:
        ```bash
        nano ~/.Rprofile
        ```
        (Usa tu editor de texto favorito si no tienes `nano`).
    *   Añadir las siguientes líneas *dentro* del archivo `.Rprofile`:

        ```R
        library(reticulate)
        use_python("/usr/bin/python3", required = TRUE)
        ```
        (Reemplaza `/usr/bin/python3` con la ruta *exacta* que obtuviste en el paso 1 si fuera diferente).
        *   `library(reticulate)`: Carga el paquete `reticulate`.
        *   `use_python("/usr/bin/python3", required = TRUE)`:  Esto le dice a `reticulate` que use *específicamente* esa ruta de Python.  `required = TRUE` es importante: si la ruta fuera incorrecta, R arrojaría un error, lo que te alertaría del problema.  Esta línea *dentro de `.Rprofile`* es lo que hace que la configuración sea permanente.
    *   Guardar los cambios en `.Rprofile` y cerrar el editor.

3.  **Reiniciar RStudio:**  Cerrar *completamente* RStudio y volver a abrirlo.  Esto es *esencial* para que los cambios en `.Rprofile` tengan efecto.

4.  **Verificar la configuración:**

    *   Abrir un *nuevo* script de R en RStudio.
    *   Ejecutar:

        ```R
        library(reticulate)
        py_config()
        ```
    *   La salida de `py_config()` debe mostrar la ruta que configuraste (`/usr/bin/python3`) y la versión correcta de Python.

5.  **Instalar paquetes de Python (según sea necesario):**

    *   Como estás usando el Python del sistema directamente (sin un entorno virtual de `reticulate`), necesitas instalar los paquetes de Python que vayas a usar desde R *en el sistema*.
    *   La forma *recomendada* en Manjaro es usar el gestor de paquetes `pacman`:

        ```bash
        sudo pacman -S python-numpy  # Para NumPy
        sudo pacman -S python-pandas # Para pandas
        # etc.
        ```
    *   Si un paquete no está disponible en los repositorios de Manjaro, puedes usar `pip install --user <nombre_del_paquete>`, pero siempre da preferencia a `pacman`.

6. **Prueba:**
    ```r
    library(reticulate)
    np <- import("numpy") #Importa numpy
    np$array(c(1,2,3))  #Usa numpy
    ```

**Puntos clave y por qué esto funciona:**

*   **`.Rprofile`:**  El archivo `.Rprofile` se ejecuta automáticamente cada vez que inicias R o RStudio.  Al poner la configuración de `reticulate` allí, la haces persistente.
*   **`use_python()`:**  Esta función de `reticulate` es la forma correcta de decirle a `reticulate` qué ejecutable de Python usar.
*   **`required = TRUE`:**  Esto asegura que si la ruta es incorrecta, obtendrás un error *inmediatamente*, en lugar de tener fallos más sutiles y difíciles de diagnosticar más adelante.
*   **Python del sistema:** Al usar `/usr/bin/python3` (o la ruta que te haya dado `which python3`), estás utilizando la instalación principal de Python de tu sistema Manjaro.
* **Instalación de paquetes**: Al usar `pacman`, se instalan los paquetes a nivel de sistema para que Python los encuentre.

**Qué *no* hacer (y por qué):**

*   **No usar `Sys.setenv(RETICULATE_PYTHON = ...)` en `.Rprofile` en este caso:**  Aunque `Sys.setenv(RETICULATE_PYTHON = ...)` *puede* funcionar, es menos directo que usar `use_python()` dentro de `.Rprofile`. `use_python()` es la forma recomendada por los desarrolladores de `reticulate` para este escenario específico.  `Sys.setenv()` es más útil si quieres configurar un entorno virtual por defecto o si necesitas una configuración que afecte a otras herramientas además de `reticulate`.
*   **No usar `python_paths` en tu script de R Markdown:**  Como vimos, el vector `python_paths` y el bucle `for` que intentaba usarlo eran ineficaces y confusos.  La configuración en `.Rprofile` es la que realmente importa.
*   **No usar `sudo pip install` (sin `--user`):**  Esto puede causar conflictos con el gestor de paquetes de Manjaro.  Usa `pacman` siempre que sea posible.

Siguiendo estos pasos, tienes una configuración limpia, robusta y persistente de Python para RStudio en Manjaro.
