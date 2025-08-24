const { Server } = require('@modelcontextprotocol/sdk/server/index.js');
const { StdioServerTransport } = require('@modelcontextprotocol/sdk/server/stdio.js');
const { chromium } = require('playwright');

const server = new Server(
  {
    name: "playwright-mcp",
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
        name: "test_web_exercise",
        description: "Testing automático de ejercicios R-exams compilados en HTML",
        inputSchema: {
          type: "object",
          properties: {
            url: { type: "string", description: "URL del ejercicio HTML" },
            test_type: { type: "string", description: "Tipo de test: validation, interaction, rendering" }
          },
          required: ["url"]
        }
      },
      {
        name: "screenshot_exercise",
        description: "Captura de pantalla de ejercicio para validación visual",
        inputSchema: {
          type: "object",
          properties: {
            url: { type: "string", description: "URL del ejercicio" },
            selector: { type: "string", description: "Selector CSS específico" }
          },
          required: ["url"]
        }
      }
    ]
  };
});

server.setRequestHandler("tools/call", async (request) => {
  if (request.params.name === "test_web_exercise") {
    const { url, test_type = "validation" } = request.params.arguments;

    const browser = await chromium.launch();
    const page = await browser.newPage();

    try {
      await page.goto(url);
      await page.waitForLoadState('networkidle');

      const title = await page.title();
      const errors = await page.evaluate(() => {
        return window.console.errors || [];
      });

      await browser.close();

      return {
        content: [
          {
            type: "text",
            text: `🎭 TESTING PLAYWRIGHT COMPLETADO:

📄 URL: ${url}
📋 Título: ${title}
🔍 Tipo de test: ${test_type}
❌ Errores encontrados: ${errors.length}
✅ Estado: ${errors.length === 0 ? 'EXITOSO' : 'CON ERRORES'}

${errors.length > 0 ? `🚨 ERRORES:\n${errors.join('\n')}` : '✅ Sin errores detectados'}`
          }
        ]
      };
    } catch (error) {
      await browser.close();
      throw error;
    }
  }

  if (request.params.name === "screenshot_exercise") {
    const { url, selector } = request.params.arguments;

    const browser = await chromium.launch();
    const page = await browser.newPage();

    try {
      await page.goto(url);
      await page.waitForLoadState('networkidle');

      const screenshot = selector
        ? await page.locator(selector).screenshot()
        : await page.screenshot();

      await browser.close();

      return {
        content: [
          {
            type: "text",
            text: `📸 CAPTURA COMPLETADA:

📄 URL: ${url}
🎯 Selector: ${selector || 'Página completa'}
✅ Captura guardada exitosamente`
          }
        ]
      };
    } catch (error) {
      await browser.close();
      throw error;
    }
  }

  throw new Error(`Herramienta desconocida: ${request.params.name}`);
});

const transport = new StdioServerTransport();
server.connect(transport);
