# 🎯 Flujo Completo: Sincronización Bidireccional Forzosa

## 📁 Una Sola Carpeta: `~/Documentos/proyecto-r-exams-icfes-matematicas-optimizado/`

---

## 🔄 RAMA MAIN

### 📥 Sincronizar NUBE → LOCAL (forzoso):
```bash
git checkout main
git fetch origin
git reset --hard origin/main
git clean -fd
```
**Resultado:** Tu `main` local queda IDÉNTICO a `origin/main`

### 📤 Sincronizar LOCAL → NUBE (forzoso):
```bash
git checkout main
# Hacer tus cambios...
git add .
git commit -m "Cambios en main"
git push --force-with-lease origin main
```
**Resultado:** `origin/main` queda IDÉNTICO a tu `main` local

---

## 🧪 RAMA EXPERIMENTOS-SEGUROS

### 📥 Sincronizar NUBE → LOCAL (forzoso):
```bash
git checkout experimentos-seguros
git fetch origin
git reset --hard origin/experimentos-seguros
git clean -fd
```
**Resultado:** Tu `experimentos-seguros` local queda IDÉNTICO a `origin/experimentos-seguros`

### 📤 Sincronizar LOCAL → NUBE (forzoso):
```bash
git checkout experimentos-seguros
# Hacer tus experimentos...
git add .
git commit -m "Experimentos arriesgados"
git push --force-with-lease origin experimentos-seguros
```
**Resultado:** `origin/experimentos-seguros` queda IDÉNTICO a tu `experimentos-seguros` local

---

## 🔄 FLUJO COMPLETO DE TRABAJO DIARIO

### Escenario 1: Empezar el día (sincronizar todo desde nube)
```bash
# Sincronizar main
git checkout main
git fetch origin && git reset --hard origin/main && git clean -fd

# Sincronizar experimentos
git checkout experimentos-seguros
git fetch origin && git reset --hard origin/experimentos-seguros && git clean -fd

# Ahora tienes ambas ramas actualizadas desde la nube
```

### Escenario 2: Trabajar en main
```bash
git checkout main
# Editar archivos...
git add .
git commit -m "Mejoras en main"
git push  # Push normal (o --force-with-lease si hay conflictos)
```

### Escenario 3: Trabajar en experimentos
```bash
git checkout experimentos-seguros
# Hacer experimentos arriesgados...
git add .
git commit -m "Experimento: nueva funcionalidad"
git push  # Push normal (o --force-with-lease si hay conflictos)
```

### Escenario 4: Terminar el día (subir todo a la nube)
```bash
# Subir cambios de main
git checkout main
git add . && git commit -m "Trabajo del día en main" && git push

# Subir cambios de experimentos
git checkout experimentos-seguros
git add . && git commit -m "Experimentos del día" && git push
```

---

## ⚠️ COMANDOS DE EMERGENCIA (Forzar Sincronización)

### 🚨 Forzar LOCAL → NUBE (sobrescribir nube completamente):
```bash
# Para main
git checkout main
git push --force origin main

# Para experimentos
git checkout experimentos-seguros
git push --force origin experimentos-seguros
```

### 🚨 Forzar NUBE → LOCAL (sobrescribir local completamente):
```bash
# Para main
git checkout main
git fetch origin && git reset --hard origin/main && git clean -fd

# Para experimentos
git checkout experimentos-seguros
git fetch origin && git reset --hard origin/experimentos-seguros && git clean -fd
```

---

## 📋 COMANDOS DE VERIFICACIÓN

### Ver estado de todas las ramas:
```bash
git fetch origin                    # Actualizar info de ramas remotas
git branch -a                       # Ver todas las ramas
git status                          # Ver estado de rama actual
```

### Comparar ramas:
```bash
git diff main experimentos-seguros  # Ver diferencias entre ramas
git log --oneline --graph --all     # Ver historial visual de todas las ramas
```

### Ver en qué rama estás:
```bash
git branch                          # El asterisco (*) marca la rama activa
```

---

## 🎯 REGLAS DE ORO

1. **Siempre `git checkout` antes de sincronizar**
2. **Cada rama se sincroniza con SU contraparte:**
   - `main` ↔ `origin/main`
   - `experimentos-seguros` ↔ `origin/experimentos-seguros`
3. **`--force-with-lease` es más seguro que `--force`**
4. **`git fetch origin` antes de cualquier reset**

---

## 💡 NOTAS IMPORTANTES

- **Una sola carpeta**: No necesitas clonar el repositorio múltiples veces
- **Cambio de contexto**: `git checkout` cambia toda la carpeta al estado de esa rama
- **Sincronización forzosa**: Los comandos con `--hard` y `--force` sobrescriben completamente
- **Seguridad**: `--force-with-lease` es más seguro que `--force` simple



## Desde otro computador

Otro computador que ya tiene rama principal

## **🔄 Pasos para Acceder a experimentos-seguros**

### **1. Actualizar información de ramas remotas:**
```bash
git fetch origin
```

### **2. Ver todas las ramas disponibles:**
```bash
git branch -a
```

### **3. Cambiar a experimentos-seguros:**
```bash
git checkout experimentos-seguros
```

### **4. Sincronizar con la nube:**
```bash
git reset --hard origin/experimentos-seguros
git clean -fd
```

