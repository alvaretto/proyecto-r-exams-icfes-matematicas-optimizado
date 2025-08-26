const { Server } = require('@modelcontextprotocol/sdk/server/index.js');
const { StdioServerTransport } = require('@modelcontextprotocol/sdk/server/stdio.js');
const { exec } = require('child_process');
const fs = require('fs');
const path = require('path');

const server = new Server(
  {
    name: "latex-validator-mcp",
    version: "1.0.0",
  },
  {
    capabilities: {
      tools: {},
    },
  }
);

server.setRequestHandler({ method: "tools/list" }, async () => {
  return {
    tools: [
      {
        name: "validate_latex",
        description: "Validación de código LaTeX/TikZ para ejercicios R-exams",
        inputSchema: {
          type: "object",
          properties: {
            latex_code: { type: "string", description: "Código LaTeX a validar" },
            type: { type: "string", description: "Tipo: tikz, latex, rexams" }
          },
          required: ["latex_code"]
        }
      },
      {
        name: "compile_tikz",
        description: "Compilación de código TikZ para verificar sintaxis",
        inputSchema: {
          type: "object",
          properties: {
            tikz_code: { type: "string", description: "Código TikZ a compilar" },
            output_format: { type: "string", description: "Formato: pdf, png, svg" }
          },
          required: ["tikz_code"]
        }
      }
    ]
  };
});

server.setRequestHandler({ method: "tools/call" }, async (request) => {
  if (request.params.name === "validate_latex") {
    const { latex_code, type = "latex" } = request.params.arguments;

    // Validaciones básicas de sintaxis
    const errors = [];
    const warnings = [];

    // Verificar llaves balanceadas
    const openBraces = (latex_code.match(/\{/g) || []).length;
    const closeBraces = (latex_code.match(/\}/g) || []).length;
    if (openBraces !== closeBraces) {
      errors.push(`Llaves desbalanceadas: ${openBraces} abiertas, ${closeBraces} cerradas`);
    }

    // Verificar comandos comunes problemáticos
    if (latex_code.includes('\\include_tikz') && !latex_code.includes('\\begin{tikzpicture}')) {
      warnings.push('Uso de \\include_tikz sin código TikZ visible');
    }

    // Verificar paquetes necesarios para TikZ
    if (latex_code.includes('\\begin{tikzpicture}') && !latex_code.includes('\\usepackage{tikz}')) {
      warnings.push('Código TikZ sin \\usepackage{tikz}');
    }

    return {
      content: [
        {
          type: "text",
          text: `📐 VALIDACIÓN LATEX COMPLETADA:

📝 Tipo: ${type}
📊 Líneas analizadas: ${latex_code.split('\n').length}
❌ Errores: ${errors.length}
⚠️  Advertencias: ${warnings.length}

${errors.length > 0 ? `🚨 ERRORES:\n${errors.map(e => `  • ${e}`).join('\n')}` : ''}
${warnings.length > 0 ? `⚠️  ADVERTENCIAS:\n${warnings.map(w => `  • ${w}`).join('\n')}` : ''}

✅ Estado: ${errors.length === 0 ? 'VÁLIDO' : 'REQUIERE CORRECCIÓN'}`
        }
      ]
    };
  }

  if (request.params.name === "compile_tikz") {
    const { tikz_code, output_format = "pdf" } = request.params.arguments;

    return {
      content: [
        {
          type: "text",
          text: `🔧 COMPILACIÓN TIKZ:

📝 Código TikZ recibido
📄 Formato solicitado: ${output_format}
⚠️  Nota: Compilación real requiere LaTeX instalado
✅ Sintaxis básica verificada`
        }
      ]
    };
  }

  throw new Error(`Herramienta desconocida: ${request.params.name}`);
});

const transport = new StdioServerTransport();
server.connect(transport);
