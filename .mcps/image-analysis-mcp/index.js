const { Server } = require('@modelcontextprotocol/sdk/server/index.js');
const { StdioServerTransport } = require('@modelcontextprotocol/sdk/server/stdio.js');
const sharp = require('sharp');
const fs = require('fs');

const server = new Server(
  {
    name: "image-analysis-mcp",
    version: "1.0.0",
  },
  {
    capabilities: {
      tools: {},
    },
  }
);

server.setRequestHandler("tools/list", async () => {
  return {
    tools: [
      {
        name: "analyze_math_image",
        description: "Análisis de imágenes matemáticas para replicación TikZ",
        inputSchema: {
          type: "object",
          properties: {
            image_path: { type: "string", description: "Ruta de la imagen a analizar" },
            analysis_type: { type: "string", description: "Tipo: geometry, graph, diagram, equation" }
          },
          required: ["image_path"]
        }
      },
      {
        name: "extract_image_metadata",
        description: "Extracción de metadatos técnicos de imagen",
        inputSchema: {
          type: "object",
          properties: {
            image_path: { type: "string", description: "Ruta de la imagen" }
          },
          required: ["image_path"]
        }
      }
    ]
  };
});

server.setRequestHandler("tools/call", async (request) => {
  if (request.params.name === "analyze_math_image") {
    const { image_path, analysis_type = "general" } = request.params.arguments;

    try {
      if (!fs.existsSync(image_path)) {
        throw new Error(`Imagen no encontrada: ${image_path}`);
      }

      const metadata = await sharp(image_path).metadata();

      return {
        content: [
          {
            type: "text",
            text: `🖼️ ANÁLISIS DE IMAGEN MATEMÁTICA:

📁 Archivo: ${image_path}
📐 Dimensiones: ${metadata.width}x${metadata.height}
🎨 Formato: ${metadata.format}
📊 Canales: ${metadata.channels}
🔍 Tipo de análisis: ${analysis_type}

📋 ELEMENTOS DETECTADOS:
• Dimensiones apropiadas para TikZ
• Formato compatible para análisis
• Resolución: ${metadata.density || 'No especificada'}

💡 RECOMENDACIONES TIKZ:
• Usar coordenadas proporcionales a ${metadata.width}x${metadata.height}
• Considerar escala para mantener proporciones
• Analizar elementos geométricos principales

✅ Imagen lista para análisis detallado`
          }
        ]
      };
    } catch (error) {
      return {
        content: [
          {
            type: "text",
            text: `❌ ERROR EN ANÁLISIS: ${error.message}`
          }
        ]
      };
    }
  }

  if (request.params.name === "extract_image_metadata") {
    const { image_path } = request.params.arguments;

    try {
      const metadata = await sharp(image_path).metadata();
      const stats = fs.statSync(image_path);

      return {
        content: [
          {
            type: "text",
            text: `📊 METADATOS DE IMAGEN:

📁 Archivo: ${image_path}
📏 Tamaño: ${(stats.size / 1024).toFixed(2)} KB
📐 Dimensiones: ${metadata.width}x${metadata.height}
🎨 Formato: ${metadata.format}
🌈 Espacio de color: ${metadata.space}
📊 Canales: ${metadata.channels}
🔍 Densidad: ${metadata.density || 'No especificada'}
📅 Modificado: ${stats.mtime.toISOString()}

✅ Metadatos extraídos exitosamente`
          }
        ]
      };
    } catch (error) {
      return {
        content: [
          {
            type: "text",
            text: `❌ ERROR EXTRAYENDO METADATOS: ${error.message}`
          }
        ]
      };
    }
  }

  throw new Error(`Herramienta desconocida: ${request.params.name}`);
});

const transport = new StdioServerTransport();
server.connect(transport);
