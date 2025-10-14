# Configure Windows Services for Auto-Recovery
# Add this as a new step in the GitHub Actions workflow after "Configure Core RDP Settings"

Write-Host "🔧 Configuring Windows services for auto-recovery..."

# Configure Terminal Services (RDP) for automatic restart on failure
try {
    Write-Host "Configuring Terminal Services recovery policy..."
    
    # Set service to restart automatically on failure
    sc.exe failure "TermService" reset= 86400 actions= restart/5000/restart/10000/restart/30000
    
    # Configure service to restart after 5 seconds on first failure
    # After 10 seconds on second failure, and after 30 seconds on subsequent failures
    # Reset failure count after 24 hours (86400 seconds)
    
    Write-Host "✅ Terminal Services recovery policy configured"
}
catch {
    Write-Warning "⚠️  Failed to configure Terminal Services recovery: $($_.Exception.Message)"
}

# Configure Tailscale service for automatic restart (if it becomes a service)
try {
    # Check if Tailscale is running as a service
    $tailscaleService = Get-Service -Name "Tailscale" -ErrorAction SilentlyContinue
    
    if ($tailscaleService) {
        Write-Host "Configuring Tailscale service recovery policy..."
        sc.exe failure "Tailscale" reset= 86400 actions= restart/5000/restart/10000/restart/30000
        Write-Host "✅ Tailscale service recovery policy configured"
    }
    else {
        Write-Host "ℹ️  Tailscale not running as a service, skipping service configuration"
    }
}
catch {
    Write-Warning "⚠️  Failed to configure Tailscale service recovery: $($_.Exception.Message)"
}

# Configure Windows Firewall service for automatic restart
try {
    Write-Host "Configuring Windows Firewall service recovery policy..."
    sc.exe failure "MpsSvc" reset= 86400 actions= restart/5000/restart/10000/restart/30000
    Write-Host "✅ Windows Firewall service recovery policy configured"
}
catch {
    Write-Warning "⚠️  Failed to configure Windows Firewall service recovery: $($_.Exception.Message)"
}

# Set additional RDP-related registry settings for robustness
try {
    Write-Host "Applying additional RDP robustness settings..."
    
    # Set keep-alive settings
    Set-ItemProperty -Path 'HKLM:\System\CurrentControlSet\Control\Terminal Server\WinStations\RDP-Tcp' -Name "KeepAliveEnable" -Value 1 -Force
    Set-ItemProperty -Path 'HKLM:\System\CurrentControlSet\Control\Terminal Server\WinStations\RDP-Tcp' -Name "KeepAliveInterval" -Value 1 -Force
    
    # Set session timeout settings (prevent automatic disconnection)
    Set-ItemProperty -Path 'HKLM:\System\CurrentControlSet\Control\Terminal Server\WinStations\RDP-Tcp' -Name "MaxConnectionTime" -Value 0 -Force
    Set-ItemProperty -Path 'HKLM:\System\CurrentControlSet\Control\Terminal Server\WinStations\RDP-Tcp' -Name "MaxDisconnectionTime" -Value 0 -Force
    Set-ItemProperty -Path 'HKLM:\System\CurrentControlSet\Control\Terminal Server\WinStations\RDP-Tcp' -Name "MaxIdleTime" -Value 0 -Force
    
    # Enable automatic reconnection
    Set-ItemProperty -Path 'HKLM:\System\CurrentControlSet\Control\Terminal Server\WinStations\RDP-Tcp' -Name "fInheritReconnectSame" -Value 1 -Force
    Set-ItemProperty -Path 'HKLM:\System\CurrentControlSet\Control\Terminal Server\WinStations\RDP-Tcp' -Name "fReconnectSame" -Value 1 -Force
    
    Write-Host "✅ Additional RDP robustness settings applied"
}
catch {
    Write-Warning "⚠️  Failed to apply additional RDP settings: $($_.Exception.Message)"
}

# Configure system to prevent sleep/hibernation during RDP session
try {
    Write-Host "Configuring power management settings..."
    
    # Prevent system sleep
    powercfg.exe /change standby-timeout-ac 0
    powercfg.exe /change standby-timeout-dc 0
    
    # Prevent hibernation
    powercfg.exe /change hibernate-timeout-ac 0
    powercfg.exe /change hibernate-timeout-dc 0
    
    # Prevent display sleep
    powercfg.exe /change monitor-timeout-ac 0
    powercfg.exe /change monitor-timeout-dc 0
    
    Write-Host "✅ Power management configured to prevent sleep"
}
catch {
    Write-Warning "⚠️  Failed to configure power management: $($_.Exception.Message)"
}

Write-Host "🎉 Windows services configuration completed!"
Write-Host ""
Write-Host "Configured services for auto-recovery:"
Write-Host "• Terminal Services (RDP) - Restart on failure"
Write-Host "• Windows Firewall - Restart on failure"
Write-Host "• Tailscale (if service) - Restart on failure"
Write-Host ""
Write-Host "Additional configurations:"
Write-Host "• RDP keep-alive enabled"
Write-Host "• Session timeouts disabled"
Write-Host "• Automatic reconnection enabled"
Write-Host "• Power management optimized"
