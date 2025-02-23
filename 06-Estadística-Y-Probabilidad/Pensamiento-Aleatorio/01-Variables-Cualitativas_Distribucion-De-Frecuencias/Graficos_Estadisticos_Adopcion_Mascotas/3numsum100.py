def generar_vector_multiplos_10(suma):
    # Validar la suma
    if suma % 10 != 0:
        raise ValueError("La suma debe ser múltiplo de 10")

    # Generar un vector con los 3 números
    vector = [0, 0, 0]

    # Dividir la suma en 3 partes iguales
    tercio = suma // 3

    # Si la suma no es múltiplo de 3, ajustamos el tercer número
    if suma % 3 != 0:
        tercio += 1

    # Asignar el valor a los dos primeros números
    vector[0] = tercio
    vector[1] = tercio

    # Ajustar el tercer número para que la suma sea exacta
    vector[2] = suma - vector[0] - vector[1]

    # Multiplicar los números por 10
    for i in range(3):
        vector[i] *= 10

    return vector


# Ejemplo de uso
vector = generar_vector_multiplos_10(100)
print(vector)


