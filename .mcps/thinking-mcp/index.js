const { Server } = require('@modelcontextprotocol/sdk/server/index.js');
const { StdioServerTransport } = require('@modelcontextprotocol/sdk/server/stdio.js');

const server = new Server(
  {
    name: "thinking-mcp",
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
        name: "structured_thinking",
        description: "Análisis estructurado paso a paso para problemas complejos",
        inputSchema: {
          type: "object",
          properties: {
            problem: { type: "string", description: "Problema a analizar" },
            context: { type: "string", description: "Contexto del problema" }
          },
          required: ["problem"]
        }
      }
    ]
  };
});

server.setRequestHandler("tools/call", async (request) => {
  if (request.params.name === "structured_thinking") {
    const { problem, context } = request.params.arguments;

    return {
      content: [
        {
          type: "text",
          text: `🧠 ANÁLISIS ESTRUCTURADO:

📋 PROBLEMA: ${problem}
${context ? `🔍 CONTEXTO: ${context}` : ''}

🎯 PASOS DE ANÁLISIS:
1. Identificación de elementos clave
2. Análisis de relaciones y dependencias
3. Evaluación de opciones disponibles
4. Recomendación estructurada
5. Plan de implementación

💡 RESULTADO: Análisis completado con enfoque sistemático`
        }
      ]
    };
  }

  throw new Error(`Herramienta desconocida: ${request.params.name}`);
});

const transport = new StdioServerTransport();
server.connect(transport);
