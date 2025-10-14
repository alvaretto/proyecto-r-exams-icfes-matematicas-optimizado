-----

# Tutorial: RDP Gratuito con GitHub Actions y Conexión desde Manjaro Plasma

Este tutorial te guiará a través del proceso de configuración de un servidor de Escritorio Remoto (RDP) gratuito utilizando GitHub Actions. La conexión al servidor se realizará de forma segura a través de Tailscale, y te mostraremos cómo conectarte desde tu sistema Manjaro Plasma.

-----

## Paso 1: Configuración de GitHub

Primero, necesitas una cuenta de GitHub y un repositorio para alojar tu workflow.

1.  **Crea una cuenta de GitHub:** Si aún no tienes una, ve a [github.com](https://github.com) y crea una cuenta gratuita.
2.  **Crea un nuevo repositorio:**
      * Una vez que hayas iniciado sesión, haz clic en el botón "**Create repository**".
      * Dale un nombre a tu repositorio (por ejemplo, `rdp-manjaro`).
      * Puedes dejarlo como público o privado.
      * Haz clic en "**Create repository**".

-----

## Paso 2: Configuración de Tailscale

Tailscale es un servicio de red privada virtual (VPN) que nos permitirá conectarnos de forma segura a nuestro servidor RDP.

1.  **Crea una cuenta de Tailscale:**

      * Ve a [tailscale.com](https://tailscale.com) y haz clic en "**Get Started**" o similar.
      * Regístrate utilizando tu cuenta de GitHub para mantener todo unificado.

2.  **Genera una clave de autenticación (Auth Key):**

      * Una vez dentro de tu panel de Tailscale, ve a "**Settings**" -\> "**Keys**".

      * Haz clic en "**Generate auth key...**".

      * **Configura la clave correctamente:**

          * Asegúrate de que la clave sea **Reusable** (Reutilizable).
          * Asegúrate de que las opciones **Ephemeral** (Efímero) y **Tags** (Etiquetas) estén **desactivadas**.

      * Haz clic en "**Generate key**".

      * **Copia la clave generada y guárdala en un lugar seguro**. La necesitarás en el siguiente paso.

-----

## Paso 3: Configurar el Repositorio de GitHub

Ahora, vamos a configurar nuestro repositorio de GitHub para que pueda utilizar la clave de Tailscale y ejecutar el workflow de RDP.

1.  **Añade la clave de Tailscale como un "Secret":**

      * En tu repositorio de GitHub, ve a "**Settings**" -\> "**Secrets and variables**" -\> "**Actions**".
      * Haz clic en "**New repository secret**".
      * En el campo "**Name**", escribe `TAILSCALE_AUTH_KEY`.
      * En el campo "**Secret**", pega la clave de autenticación de Tailscale que copiaste.
      * Haz clic en "**Add secret**".

2.  **Crea el Workflow de GitHub Actions:**

      * Ve a la pestaña "**Actions**" de tu repositorio.
      * Haz clic en "**set up a workflow yourself**".
      * Nombra el archivo `rdp.yml`.
      * Borra el contenido por defecto y pega el siguiente código:

<!-- end list -->

```yaml
name: RDP
on:
  workflow_dispatch:
jobs:
  secure-rdp:
    runs-on: windows-latest
    timeout-minutes: 360
    steps:
    - name: Configure Core RDP Settings
      run: |
        Set-ItemProperty -Path 'HKLM:\System\CurrentControlSet\Control\Terminal Server' -Name "fDenyTSConnections" -Value 0 -Force
        Set-ItemProperty -Path 'HKLM:\System\CurrentControlSet\Control\Terminal Server\WinStations\RDP-Tcp' -Name "UserAuthentication" -Value 0 -Force
        Set-ItemProperty -Path 'HKLM:\System\CurrentControlSet\Control\Terminal Server\WinStations\RDP-Tcp' -Name "SecurityLayer" -Value 0 -Force
        netsh advfirewall firewall delete rule name="RDP-Tailscale"
        netsh advfirewall firewall add rule name="RDP-Tailscale" dir=in action=allow protocol=TCP localport=3389
        Restart-Service -Name TermService -Force
    - name: Create RDP User with Secure Password
      run: |
        Add-Type -AssemblyName System.Security
        $charSet = @{
            Upper = [char[]](65..90)
            Lower = [char[]](97..122)
            Number = [char[]](48..57)
            Special=([char[]](33..47)+[char[]](58..64)+[char[]](91..96) + [char[]](123..126))
        }
        $rawPassword = @()
        $rawPassword += $charSet.Upper | Get-Random -Count 4
        $rawPassword += $charSet.Lower | Get-Random -Count 4
        $rawPassword += $charSet.Number | Get-Random -Count 4
        $rawPassword += $charSet.Special | Get-Random -Count 4
        $password = -join ($rawPassword | Sort-Object { Get-Random })
        $securePass = ConvertTo-SecureString $password -AsPlainText -Force
        New-LocalUser -Name "RDP" -Password $securePass -AccountNeverExpires
        Add-LocalGroupMember -Group "Administrators" -Member "RDP"
        Add-LocalGroupMember -Group "Remote Desktop Users" -Member "RDP"
        echo "RDP_CREDS=User: RDP | Password: $password" >> $env:GITHUB_ENV
    - name: Install Tailscale
      run: |
        Invoke-WebRequest -Uri "https://pkgs.tailscale.com/stable/tailscale-setup-latest.exe" -OutFile "tailscale.exe"
        ./tailscale.exe /quiet
    - name: Establish Tailscale Connection
      run: |
        & "$env:ProgramFiles\Tailscale\tailscale.exe" up --authkey=${{ secrets.TAILSCALE_AUTH_KEY }} --hostname=gh-runner-$env:GITHUB_RUN_ID
        $tsIP = $null
        $retries = 0
        while (-not $tsIP -and $retries -lt 10) {
            $tsIP = & "$env:ProgramFiles\Tailscale\tailscale.exe" ip -4
            Start-Sleep -Seconds 5
            $retries++
        }
        if (-not $tsIP) {
            Write-Error "Tailscale IP not assigned. Exiting."
            exit 1
        }
        echo "TAILSCALE_IP=$tsIP" >> $env:GITHUB_ENV
    - name: Maintain Connection
      run: |
        Write-Host "`n=== RDP ACCESS ==="
        Write-Host "Address: $env:TAILSCALE_IP"
        Write-Host "Username: RDP"
        Write-Host "Password: $($env:RDP_CREDS -replace 'User: RDP \| Password: ', '')"
        Write-Host "==================`n"
        while ($true) {
            Write-Host "[$([System.DateTime]::UtcNow.ToString('s'))] RDP Active - Use Ctrl+C in workflow to terminate"
            Start-Sleep -Seconds 300
        }
```

  * Haz clic en "**Commit changes...**" y luego en "**Commit changes**".

-----

## Paso 4: Iniciar la Conexión RDP

1.  **Ejecuta el Workflow:**

      * En la pestaña "**Actions**", selecciona el workflow "**RDP**" y haz clic en "**Run workflow**".

2.  **Obtén la IP y las Credenciales:**

      * Espera a que el workflow comience a ejecutarse (ícono amarillo). Haz clic en el trabajo en ejecución para ver los registros.
      * Dentro de los registros, busca la sección "**Maintain Connection**". Allí encontrarás la **dirección IP de Tailscale**, el **nombre de usuario** (`RDP`) y la **contraseña**.

-----

## Paso 5: Conectarse desde Manjaro Plasma

Aquí es donde adaptamos el tutorial para Manjaro. **Se recomienda usar Remmina por su fiabilidad.**

1.  **Instala y Configura Tailscale en Manjaro:**

      * Abre una terminal en Manjaro.
      * Instala Tailscale: `sudo pacman -S tailscale`
      * Inicia y habilita el servicio: `sudo systemctl enable --now tailscaled`
      * Conecta tu máquina: `sudo tailscale up`
      * Sigue el enlace que aparece para autenticar tu máquina. **Asegúrate de iniciar sesión en la misma cuenta de Tailscale a la que pertenece tu clave de autenticación.**

2.  **Instala y Usa Remmina:**

      * Instala Remmina y su plugin RDP:
        ```bash
        sudo pacman -S remmina freerdp
        ```
      * Abre **Remmina** desde tu menú de aplicaciones.
      * Asegúrate de que el protocolo seleccionado sea **RDP**.
      * En la barra superior, introduce la **dirección IP de Tailscale** que obtuviste de GitHub y presiona Enter.
      * Cuando se te solicite, introduce el usuario (`RDP`) y la **contraseña** de los registros de GitHub.

¡Y listo\! Ahora deberías tener una conexión de escritorio remoto a un servidor Windows que se ejecuta en GitHub Actions, todo desde tu escritorio Manjaro Plasma.

-----

## Cómo Iniciar una Nueva Sesión (Futuras Conexiones)

El servidor RDP en GitHub Actions es temporal. Cada vez que el flujo de trabajo se detiene, el servidor se elimina. Para conectarte en el futuro, solo necesitas "encender" uno nuevo siguiendo estos sencillos pasos.

1.  **Ejecuta el Workflow de Nuevo**

      * Ve a tu repositorio en GitHub https://github.com/alvaretto/rdp-manjaro -\> Pestaña **"Actions"**. 
      * Selecciona tu workflow de RDP y haz clic en **"Run workflow"**.

2.  **Obtén la Nueva IP y Contraseña**

      * Espera a que el trabajo comience y, en los registros, ve a la sección **"Maintain Connection"**. 
      * Copia la **nueva dirección IP** y la **nueva contraseña**. Recuerda que estas cambian con cada ejecución.

3.  **Conéctate**

      * Abre Remmina e introduce la nueva IP y contraseña para iniciar tu sesión.

No necesitas volver a configurar Tailscale ni los secretos de GitHub. Solo repite estos tres pasos cada vez que quieras usar tu RDP.
