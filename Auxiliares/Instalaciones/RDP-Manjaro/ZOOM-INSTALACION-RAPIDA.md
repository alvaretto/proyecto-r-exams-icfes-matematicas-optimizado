# 📹 Guía Rápida: Instalación de Zoom en Manjaro Linux

**Fecha:** 2025-10-12  
**Versión de Zoom:** 6.6.0  
**Sistema:** Manjaro Linux (Arch-based)

---

## 🚀 Instalación Rápida (Método Recomendado)

### Opción 1: Script Automatizado (Más Fácil)

```bash
# Ejecutar el script de instalación
./install-zoom.sh
```

El script te guiará paso a paso y configurará todo automáticamente.

---

### Opción 2: Instalación Manual Flatpak (Recomendado)

```bash
# 1. Instalar Flatpak (si no está instalado)
sudo pacman -S flatpak

# 2. Agregar Flathub
flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo

# 3. Instalar Zoom
flatpak install flathub us.zoom.Zoom

# 4. Verificar instalación
flatpak list | grep -i zoom
```

---

### Opción 3: Instalación Manual AUR (Alternativa)

```bash
# Instalar Zoom desde AUR
yay -S zoom

# Verificar instalación
pacman -Q zoom
```

---

## 🎯 ¿Qué Método Elegir?

| Método | Recomendado Para | Ventaja Principal |
|--------|------------------|-------------------|
| **Flatpak** | Mayoría de usuarios | 🛡️ Más seguro (sandboxing) |
| **AUR** | Usuarios avanzados | ⚡ Mejor rendimiento |

---

## ▶️ Ejecutar Zoom

### Flatpak:
```bash
flatpak run us.zoom.Zoom
```

### AUR:
```bash
zoom
```

### Desde el menú:
Busca "Zoom" en tu lanzador de aplicaciones KDE Plasma

---

## 🔄 Actualizar Zoom

### Flatpak:
```bash
flatpak update us.zoom.Zoom
```

### AUR:
```bash
yay -Syu zoom
```

---

## 🗑️ Desinstalar Zoom

### Flatpak:
```bash
flatpak uninstall us.zoom.Zoom
```

### AUR:
```bash
yay -Rns zoom
```

---

## 🛠️ Solución Rápida de Problemas

### Problema: Cámara/micrófono no funciona (Flatpak)
```bash
flatpak override us.zoom.Zoom --device=all
```

### Problema: Compartir pantalla no funciona
```bash
sudo pacman -S xdg-desktop-portal-kde
systemctl --user restart xdg-desktop-portal
```

### Problema: Audio no funciona (AUR)
```bash
sudo pacman -S pulseaudio-alsa
```

---

## ✅ Checklist de Verificación

Después de instalar, verifica:

- [ ] Zoom se abre correctamente
- [ ] Cámara funciona (Configuración → Video)
- [ ] Micrófono funciona (Configuración → Audio)
- [ ] Altavoces funcionan (Configuración → Audio → Probar)
- [ ] Compartir pantalla funciona (en reunión de prueba)

---

## 📊 Comparación Rápida

| Característica | Flatpak | AUR |
|----------------|---------|-----|
| Seguridad | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ |
| Rendimiento | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| Facilidad | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ |
| Espacio | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| Estabilidad | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ |

---

## 🔗 Recursos

- **Documentación completa:** Ver `RDP-Manjaro.md` (sección Zoom)
- **Script de instalación:** `install-zoom.sh`
- **Soporte oficial:** https://support.zoom.us/

---

## 💡 Consejos Adicionales

### Crear alias para Flatpak (opcional):
```bash
echo 'alias zoom="flatpak run us.zoom.Zoom"' >> ~/.bashrc
source ~/.bashrc
```

Ahora puedes ejecutar Zoom simplemente con: `zoom`

### Configurar HiDPI (si la pantalla se ve borrosa):
```bash
# Para Flatpak
flatpak override us.zoom.Zoom --env=QT_AUTO_SCREEN_SCALE_FACTOR=1

# Para AUR, agregar a ~/.bashrc
export QT_AUTO_SCREEN_SCALE_FACTOR=1
```

---

## 📝 Información del Sistema

**Sistema verificado:**
- Manjaro Linux (Kernel 6.16.8-1-MANJARO)
- KDE Plasma
- Flatpak: ✅ Instalado
- Yay: ✅ Instalado

**Dependencias principales ya instaladas:**
- fontconfig 2:2.17.1-1
- glib2 2.84.4-2
- libpulse 17.0+r43+g3e2bb8a1e-1
- libx11 1.8.12-1
- Y más...

---

## 🎉 ¡Listo!

Después de instalar Zoom, podrás:
- ✅ Unirte a reuniones
- ✅ Crear reuniones
- ✅ Compartir pantalla
- ✅ Usar cámara y micrófono
- ✅ Grabar reuniones (con permisos)

**¿Necesitas ayuda?** Consulta la documentación completa en `RDP-Manjaro.md`

---

*Última actualización: 2025-10-12*

