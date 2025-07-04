# 🚀 GUÍA PASO A PASO PARA CONFIGURAR GIT Y HACER PUSH

## 📋 INFORMACIÓN DEL REPOSITORIO
- **Repositorio**: `https://github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado`
- **Correo**: `alvaroangelm@gmail.com`
- **Token**: `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`
- **Rama**: `experimentos-seguros`

## 🔧 PASO 1: CONFIGURAR IDENTIDAD DE GIT

Ejecuta estos comandos en la terminal:

```bash
# Configurar email
git config --global user.email "alvaroangelm@gmail.com"

# Configurar nombre
git config --global user.name "Alvaro Angel"

# Verificar configuración
git config --global user.email
git config --global user.name
```

## 🔐 PASO 2: CONFIGURAR AUTENTICACIÓN

### Opción A: Usar credential helper (RECOMENDADO)

```bash
# Configurar credential helper para almacenar credenciales
git config --global credential.helper store
```

### Opción B: Configurar remote con token directamente

```bash
# Verificar remote actual
git remote -v

# Si necesitas cambiar el remote para incluir el token:
git remote set-url origin https://alvaretto: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@github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado.git
```

## 📦 PASO 3: PREPARAR Y HACER COMMIT

```bash
# Verificar estado del repositorio
git status

# Agregar todos los cambios
git add .

# Hacer commit con mensaje descriptivo
git commit -m "Actualizando Rexams Lubuntu - Solución definitiva matplotlib y reticulate"
```

## 🚀 PASO 4: HACER PUSH

```bash
# Push con force-with-lease (más seguro que --force)
git push --force-with-lease origin experimentos-seguros
```

## 🔄 COMANDOS COMPLETOS EN SECUENCIA

Copia y pega estos comandos uno por uno:

```bash
# 1. Configurar identidad
git config --global user.email "alvaroangelm@gmail.com"
git config --global user.name "Alvaro Angel"

# 2. Configurar credential helper
git config --global credential.helper store

# 3. Verificar configuración
echo "Email configurado: $(git config --global user.email)"
echo "Nombre configurado: $(git config --global user.name)"

# 4. Verificar estado
git status

# 5. Agregar cambios
git add .

# 6. Hacer commit
git commit -m "Actualizando Rexams Lubuntu - Solución definitiva matplotlib y reticulate"

# 7. Hacer push
git push --force-with-lease origin experimentos-seguros
```

## 🔐 SI TE PIDE CREDENCIALES

Cuando Git te pida credenciales, usa:

- **Username**: `alvaretto`
- **Password**: `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`

## ⚠️ NOTAS IMPORTANTES

1. **Token de acceso**: El token proporcionado tiene permisos para el repositorio
2. **Credential helper**: Una vez configurado, Git recordará tus credenciales
3. **Force-with-lease**: Es más seguro que `--force` porque verifica que no haya cambios remotos
4. **Rama**: Asegúrate de estar en la rama `experimentos-seguros`

## 🔍 VERIFICAR RAMA ACTUAL

```bash
# Ver rama actual
git branch

# Cambiar a la rama correcta si es necesario
git checkout experimentos-seguros
```

## 🆘 SI HAY PROBLEMAS

### Error de autenticación:
```bash
# Limpiar credenciales almacenadas
git config --global --unset credential.helper
git config --global credential.helper store
```

### Error de rama:
```bash
# Verificar ramas disponibles
git branch -a

# Crear y cambiar a la rama si no existe
git checkout -b experimentos-seguros
```

### Error de remote:
```bash
# Verificar remote
git remote -v

# Reconfigurar remote si es necesario
git remote set-url origin https://github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado.git
```

## ✅ COMANDO FINAL SIMPLIFICADO

Si quieres ejecutar todo de una vez:

```bash
git config --global user.email "alvaroangelm@gmail.com" && \
git config --global user.name "Alvaro Angel" && \
git config --global credential.helper store && \
git add . && \
git commit -m "Actualizando Rexams Lubuntu - Solución definitiva matplotlib y reticulate" && \
git push --force-with-lease origin experimentos-seguros
```

---

**🎯 RESULTADO ESPERADO**: Después de ejecutar estos comandos, tus cambios deberían estar subidos al repositorio en la rama `experimentos-seguros`.