# Guía de Autenticación GitHub para Git

## Método 1: Personal Access Token (Recomendado)

### Paso 1: Crear un Personal Access Token en GitHub

1. Ve a GitHub.com e inicia sesión
2. Haz clic en tu foto de perfil (esquina superior derecha)
3. Selecciona **Settings**
4. En el menú izquierdo, haz clic en **Developer settings**
5. Haz clic en **Personal access tokens** → **Tokens (classic)**
6. Haz clic en **Generate new token** → **Generate new token (classic)**
7. Configura el token:
   - **Note**: "Git CLI Access - R-exams Project"
   - **Expiration**: 90 days (o el tiempo que prefieras)
   - **Scopes**: Selecciona al menos:
     - ✅ `repo` (Full control of private repositories)
     - ✅ `workflow` (Update GitHub Action workflows)
     - ✅ `write:packages` (Upload packages to GitHub Package Registry)
8. Haz clic en **Generate token**
9. **¡IMPORTANTE!** Copia el token inmediatamente (solo se muestra una vez)

### Paso 2: Configurar Git con el Token

Una vez que tengas tu token, ejecuta estos comandos:

```bash
# Configurar Git para usar el token
git config --global credential.helper store

# Configurar la URL del repositorio con tu usuario
git remote set-url origin https://alvaretto@github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado.git
```

### Paso 3: Hacer Push con el Token

Cuando hagas push, Git te pedirá credenciales:
- **Username**: `alvaretto`
- **Password**: `[tu-personal-access-token]` (pega el token aquí)

## Método 2: SSH (Alternativo)

### Paso 1: Generar clave SSH
```bash
ssh-keygen -t ed25519 -C "alvaroangelm@gmail.com"
```

### Paso 2: Agregar clave SSH a GitHub
```bash
cat ~/.ssh/id_ed25519.pub
```
Copia la salida y agrégala en GitHub → Settings → SSH and GPG keys

### Paso 3: Cambiar URL del repositorio a SSH
```bash
git remote set-url origin git@github.com:alvaretto/proyecto-r-exams-icfes-matematicas-optimizado.git
```

## Comandos para Verificar Configuración

```bash
# Verificar configuración actual
git config --list | grep user
git remote -v

# Verificar estado del repositorio
git status
git log --oneline -5
```

## Solución de Problemas

### Error: "Authentication failed"
1. Verifica que el token sea correcto
2. Asegúrate de que el token tenga los permisos necesarios
3. Verifica que el usuario sea correcto en la URL

### Error: "Permission denied"
1. Verifica que tengas permisos de escritura en el repositorio
2. Asegúrate de que el repositorio exista
3. Verifica la URL del repositorio

### Error: "Repository not found"
1. Verifica que la URL del repositorio sea correcta
2. Asegúrate de que el repositorio sea público o tengas acceso

## Información del Repositorio

- **Usuario**: alvaretto
- **Repositorio**: proyecto-r-exams-icfes-matematicas-optimizado
- **Rama**: experimentos-seguros
- **URL HTTPS**: https://github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado.git
- **URL SSH**: git@github.com:alvaretto/proyecto-r-exams-icfes-matematicas-optimizado.git