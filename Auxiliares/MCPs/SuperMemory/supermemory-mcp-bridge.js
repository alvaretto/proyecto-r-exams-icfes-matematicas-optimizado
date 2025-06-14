#!/usr/bin/env node

/**
 * Supermemory MCP Bridge for Augment AI
 * Connects to Supermemory's HTTP/SSE MCP endpoint
 */

const { spawn } = require('child_process');
const https = require('https');
const { URL } = require('url');

const SUPERMEMORY_URL = 'https://mcp.supermemory.ai/YhlFnOH5_-FLrwDBHuvpU/sse';

class SupermemoryMCPBridge {
  constructor() {
    this.connected = false;
  }

  connect() {
    console.log('Connecting to Supermemory MCP...');
    
    const url = new URL(SUPERMEMORY_URL);
    
    const options = {
      hostname: url.hostname,
      port: url.port || 443,
      path: url.pathname + url.search,
      method: 'GET',
      headers: {
        'Accept': 'text/event-stream',
        'Cache-Control': 'no-cache',
        'Connection': 'keep-alive'
      }
    };

    const req = https.request(options, (res) => {
      console.log(`Connected to Supermemory MCP (Status: ${res.statusCode})`);
      this.connected = true;

      res.on('data', (chunk) => {
        // Forward SSE data to stdout for MCP protocol
        process.stdout.write(chunk);
      });

      res.on('end', () => {
        console.log('Connection to Supermemory MCP ended');
        this.connected = false;
      });
    });

    req.on('error', (error) => {
      console.error('Error connecting to Supermemory MCP:', error);
      process.exit(1);
    });

    // Handle stdin for MCP protocol
    process.stdin.on('data', (data) => {
      // Forward MCP requests to Supermemory
      req.write(data);
    });

    process.stdin.on('end', () => {
      req.end();
    });

    req.end();
  }

  start() {
    this.connect();
    
    // Keep the process alive
    process.on('SIGINT', () => {
      console.log('Shutting down Supermemory MCP bridge...');
      process.exit(0);
    });
  }
}

// Start the bridge
const bridge = new SupermemoryMCPBridge();
bridge.start();
