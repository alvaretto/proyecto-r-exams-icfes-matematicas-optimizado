# Flujo Completo: Sincronización Bidireccional Forzosa

## Carpeta de Trabajo: `~/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/`

---

## Estructura de Ramas (Actualizado 2026-01-30)

| Rama | Propósito |
|------|-----------|
| **main** | Rama principal y única de desarrollo |
| **gh-pages** | Sitio web GitHub Pages (no modificar manualmente) |

> **Nota**: La rama `experimentos-seguros` fue fusionada con `main` y eliminada.

---

## RAMA MAIN

### Sincronizar NUBE → LOCAL (forzoso):
```bash
git checkout main
git fetch origin --prune
git reset --hard origin/main
git clean -fd
```
**Resultado:** Tu `main` local queda IDÉNTICO a `origin/main`

### Sincronizar LOCAL → NUBE (forzoso):
```bash
git checkout main
# Hacer tus cambios...
git add .
git commit -m "Descripción de cambios"
git push --force-with-lease origin main
```
**Resultado:** `origin/main` queda IDÉNTICO a tu `main` local

---

## FLUJO COMPLETO DE TRABAJO DIARIO

### Escenario 1: Empezar el día (sincronizar desde nube)
```bash
git fetch origin --prune
git checkout main
git reset --hard origin/main
git clean -fd
```

### Escenario 2: Trabajar durante el día
```bash
git checkout main
# Editar archivos...
git add .
git commit -m "Descripción de cambios"
git push
```

### Escenario 3: Terminar el día (subir todo a la nube)
```bash
git add .
git commit -m "Trabajo del día"
git push origin main
```

---

## COMANDOS DE EMERGENCIA

### Forzar LOCAL → NUBE (sobrescribir nube completamente):
```bash
git checkout main
git push --force origin main
```

### Forzar NUBE → LOCAL (sobrescribir local completamente):
```bash
git checkout main
git fetch origin --prune
git reset --hard origin/main
git clean -fd
```

---

## COMANDOS DE VERIFICACIÓN

### Ver estado:
```bash
git fetch origin --prune          # Actualizar info de ramas remotas
git branch -a                     # Ver todas las ramas
git status                        # Ver estado de rama actual
```

### Ver historial:
```bash
git log --oneline -10             # Últimos 10 commits
git log --oneline --graph --all   # Historial visual de todas las ramas
```

### Ver en qué rama estás:
```bash
git branch                        # El asterisco (*) marca la rama activa
```

---

## DESDE OTRO COMPUTADOR

### Primera vez (clonar repositorio):
```bash
git clone git@github.com:alvaretto/proyecto-r-exams-icfes-matematicas-optimizado.git
cd proyecto-r-exams-icfes-matematicas-optimizado
```

### Ya tienes el repositorio (actualizar):
```bash
cd /ruta/al/RepositorioMatematicasICFES_R_Exams

# Actualizar referencias y sincronizar
git fetch --all --prune
git checkout main
git reset --hard origin/main

# Eliminar ramas locales obsoletas (si existen)
git branch -D experimentos-seguros 2>/dev/null
```

### Comando único para sincronizar desde otro PC:
```bash
git fetch --all --prune && git checkout main && git reset --hard origin/main
```

---

## REGLAS DE ORO

1. **Siempre `git fetch --prune`** antes de sincronizar (limpia referencias obsoletas)
2. **`--force-with-lease`** es más seguro que `--force` (verifica que no sobrescribes trabajo de otros)
3. **`git clean -fd`** elimina archivos no rastreados (cuidado con archivos nuevos no commiteados)
4. **Una sola rama principal**: Todo el desarrollo va en `main`

---

## NOTAS IMPORTANTES

- **Una sola carpeta**: No necesitas clonar el repositorio múltiples veces
- **Sincronización forzosa**: Los comandos con `--hard` y `--force` sobrescriben completamente
- **Advertencia**: `git reset --hard` y `git clean -fd` eliminan cambios locales no commiteados
- **Respaldo**: Si tienes cambios importantes, haz `git stash` antes de sincronizar

---

**Última actualización**: 2026-01-30
