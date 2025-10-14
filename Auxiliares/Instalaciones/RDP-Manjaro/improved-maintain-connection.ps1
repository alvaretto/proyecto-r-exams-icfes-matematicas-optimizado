# Improved Maintain Connection step for GitHub Actions RDP workflow
# This replaces the basic while loop with robust monitoring and auto-recovery

Write-Host "`n=== RDP ACCESS ==="
Write-Host "Address: $env:TAILSCALE_IP"
Write-Host "Username: RDP"
Write-Host "Password: $($env:RDP_CREDS -replace 'User: RDP \| Password: ', '')"
Write-Host "==================`n"

# Function to check Tailscale status
function Test-TailscaleConnection {
    try {
        $tsStatus = & "$env:ProgramFiles\Tailscale\tailscale.exe" status --json | ConvertFrom-Json
        return $tsStatus.BackendState -eq "Running"
    }
    catch {
        Write-Warning "Failed to check Tailscale status: $($_.Exception.Message)"
        return $false
    }
}

# Function to check RDP service status
function Test-RDPService {
    try {
        $service = Get-Service -Name "TermService" -ErrorAction Stop
        return $service.Status -eq "Running"
    }
    catch {
        Write-Warning "Failed to check RDP service: $($_.Exception.Message)"
        return $false
    }
}

# Function to test RDP port accessibility
function Test-RDPPort {
    try {
        $testResult = Test-NetConnection -ComputerName $env:TAILSCALE_IP -Port 3389 -InformationLevel Quiet
        return $testResult
    }
    catch {
        Write-Warning "Failed to test RDP port: $($_.Exception.Message)"
        return $false
    }
}

# Function to restart RDP service
function Restart-RDPService {
    try {
        Write-Host "🔄 Restarting RDP service..."
        Restart-Service -Name "TermService" -Force
        Start-Sleep -Seconds 10
        Write-Host "✅ RDP service restarted successfully"
        return $true
    }
    catch {
        Write-Error "❌ Failed to restart RDP service: $($_.Exception.Message)"
        return $false
    }
}

# Function to reconnect Tailscale
function Restart-TailscaleConnection {
    try {
        Write-Host "🔄 Reconnecting Tailscale..."
        & "$env:ProgramFiles\Tailscale\tailscale.exe" down
        Start-Sleep -Seconds 5
        & "$env:ProgramFiles\Tailscale\tailscale.exe" up --authkey=${{ secrets.TAILSCALE_AUTH_KEY }} --hostname=gh-runner-$env:GITHUB_RUN_ID
        Start-Sleep -Seconds 10
        
        # Verify new IP
        $newIP = & "$env:ProgramFiles\Tailscale\tailscale.exe" ip -4
        if ($newIP) {
            $env:TAILSCALE_IP = $newIP
            Write-Host "✅ Tailscale reconnected with IP: $newIP"
            return $true
        }
        else {
            Write-Error "❌ Failed to get new Tailscale IP"
            return $false
        }
    }
    catch {
        Write-Error "❌ Failed to reconnect Tailscale: $($_.Exception.Message)"
        return $false
    }
}

# Main monitoring loop with health checks and auto-recovery
$checkInterval = 60  # Check every minute
$lastHealthCheck = Get-Date
$consecutiveFailures = 0
$maxConsecutiveFailures = 3

Write-Host "🚀 Starting enhanced RDP monitoring with auto-recovery..."
Write-Host "Check interval: $checkInterval seconds"
Write-Host "Max consecutive failures before recovery: $maxConsecutiveFailures"
Write-Host ""

while ($true) {
    $currentTime = Get-Date
    $timestamp = $currentTime.ToString('yyyy-MM-dd HH:mm:ss UTC')
    
    # Perform health checks every minute
    if (($currentTime - $lastHealthCheck).TotalSeconds -ge $checkInterval) {
        Write-Host "[$timestamp] 🔍 Performing health checks..."
        
        $tailscaleOK = Test-TailscaleConnection
        $rdpServiceOK = Test-RDPService
        $rdpPortOK = Test-RDPPort
        
        # Report status
        $tailscaleStatus = if ($tailscaleOK) { "✅ OK" } else { "❌ FAIL" }
        $rdpServiceStatus = if ($rdpServiceOK) { "✅ OK" } else { "❌ FAIL" }
        $rdpPortStatus = if ($rdpPortOK) { "✅ OK" } else { "❌ FAIL" }
        
        Write-Host "   Tailscale: $tailscaleStatus"
        Write-Host "   RDP Service: $rdpServiceStatus"
        Write-Host "   RDP Port: $rdpPortStatus"
        
        # Check if any component failed
        if (-not $tailscaleOK -or -not $rdpServiceOK -or -not $rdpPortOK) {
            $consecutiveFailures++
            Write-Warning "⚠️  Health check failed ($consecutiveFailures/$maxConsecutiveFailures)"
            
            if ($consecutiveFailures -ge $maxConsecutiveFailures) {
                Write-Host "🚨 Maximum consecutive failures reached. Attempting recovery..."
                
                # Attempt recovery
                $recoverySuccess = $true
                
                if (-not $rdpServiceOK) {
                    $recoverySuccess = $recoverySuccess -and (Restart-RDPService)
                }
                
                if (-not $tailscaleOK) {
                    $recoverySuccess = $recoverySuccess -and (Restart-TailscaleConnection)
                }
                
                if ($recoverySuccess) {
                    Write-Host "✅ Recovery successful! Resetting failure counter."
                    $consecutiveFailures = 0
                }
                else {
                    Write-Error "❌ Recovery failed. Continuing monitoring..."
                }
            }
        }
        else {
            # All checks passed
            if ($consecutiveFailures -gt 0) {
                Write-Host "✅ All systems healthy. Resetting failure counter."
            }
            $consecutiveFailures = 0
        }
        
        $lastHealthCheck = $currentTime
    }
    
    # Show periodic status (every 5 minutes)
    if ($currentTime.Minute % 5 -eq 0 -and $currentTime.Second -lt 10) {
        Write-Host "[$timestamp] 💚 RDP Server Active - IP: $env:TAILSCALE_IP - Failures: $consecutiveFailures"
    }
    
    Start-Sleep -Seconds 10
}
