// Script para exportar la configuración de Augment MCP
const fs = require('fs');
const os = require('os');
const path = require('path');

// Posibles ubicaciones del archivo settings.json
const possiblePaths = [
  path.join(os.homedir(), '.config', 'Code', 'User', 'settings.json'),
  path.join(os.homedir(), 'Library', 'Application Support', 'Code', 'User', 'settings.json'),
  path.join(os.homedir(), 'AppData', 'Roaming', 'Code', 'User', 'settings.json'),
  path.join(os.homedir(), '.vscode', 'settings.json')
];

// Función para leer y extraer la configuración de Augment
function extractAugmentConfig() {
  for (const settingsPath of possiblePaths) {
    try {
      if (fs.existsSync(settingsPath)) {
        console.log(`Encontrado archivo de configuración en: ${settingsPath}`);
        const settings = JSON.parse(fs.readFileSync(settingsPath, 'utf8'));
        
        if (settings['augment.advanced'] && settings['augment.advanced'].mcpServers) {
          const mcpConfig = {
            "augment.advanced": {
              "mcpServers": settings['augment.advanced'].mcpServers
            }
          };
          
          // Guardar la configuración en un nuevo archivo
          const outputPath = path.join(process.cwd(), 'augment-mcp-config-export.json');
          fs.writeFileSync(outputPath, JSON.stringify(mcpConfig, null, 2), 'utf8');
          console.log(`Configuración de Augment MCP exportada a: ${outputPath}`);
          return true;
        } else {
          console.log('No se encontró configuración de Augment MCP en este archivo.');
        }
      }
    } catch (error) {
      console.error(`Error al procesar ${settingsPath}:`, error.message);
    }
  }
  
  console.log('No se encontró configuración de Augment MCP en ninguna ubicación conocida.');
  return false;
}

// Ejecutar la extracción
extractAugmentConfig();
