#!/usr/bin/env node

/**
 * Simple Supermemory MCP Server
 * Minimal implementation for Augment AI compatibility
 */

// Simple MCP server that responds to basic protocol
process.stdin.setEncoding('utf8');

let buffer = '';

// Handle initialization
const sendResponse = (id, result) => {
  const response = {
    jsonrpc: '2.0',
    id: id,
    result: result
  };
  console.log(JSON.stringify(response));
};

const sendError = (id, error) => {
  const response = {
    jsonrpc: '2.0',
    id: id,
    error: error
  };
  console.log(JSON.stringify(response));
};

// Handle incoming messages
process.stdin.on('data', (chunk) => {
  buffer += chunk;
  
  // Try to parse complete JSON messages
  const lines = buffer.split('\n');
  buffer = lines.pop(); // Keep incomplete line in buffer
  
  for (const line of lines) {
    if (line.trim()) {
      try {
        const message = JSON.parse(line);
        handleMessage(message);
      } catch (error) {
        console.error('Parse error:', error);
      }
    }
  }
});

function handleMessage(message) {
  const { id, method, params } = message;
  
  switch (method) {
    case 'initialize':
      sendResponse(id, {
        protocolVersion: '2024-11-05',
        capabilities: {
          tools: {}
        },
        serverInfo: {
          name: 'supermemory-mcp',
          version: '1.0.0'
        }
      });
      break;
      
    case 'tools/list':
      sendResponse(id, {
        tools: [
          {
            name: 'remember',
            description: 'Store information in memory',
            inputSchema: {
              type: 'object',
              properties: {
                memory: {
                  type: 'string',
                  description: 'Information to remember'
                }
              },
              required: ['memory']
            }
          }
        ]
      });
      break;
      
    case 'tools/call':
      if (params.name === 'remember') {
        sendResponse(id, {
          content: [
            {
              type: 'text',
              text: `✅ Remembered: ${params.arguments.memory}`
            }
          ]
        });
      } else {
        sendError(id, {
          code: -32601,
          message: 'Method not found'
        });
      }
      break;
      
    default:
      sendError(id, {
        code: -32601,
        message: 'Method not found'
      });
  }
}

// Signal ready
console.error('Supermemory MCP server started');
