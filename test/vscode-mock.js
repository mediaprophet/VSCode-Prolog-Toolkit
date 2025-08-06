// Mock for the vscode module when running tests outside of VS Code environment
module.exports = {
  // Mock window object
  window: {
    showInformationMessage: async (message, ...items) => {
      console.log(`[MOCK] showInformationMessage: ${message}`);
      return null;
    },
    showWarningMessage: async (message, options, ...items) => {
      console.log(`[MOCK] showWarningMessage: ${message}`);
      return null;
    },
    showErrorMessage: async (message, ...items) => {
      console.log(`[MOCK] showErrorMessage: ${message}`);
      return null;
    },
    showInputBox: async (options) => {
      console.log(`[MOCK] showInputBox: ${options?.prompt || 'No prompt'}`);
      return null;
    },
    createWebviewPanel: (viewType, title, showOptions, options) => {
      console.log(`[MOCK] createWebviewPanel: ${title}`);
      return {
        webview: {
          html: '',
          onDidReceiveMessage: () => {},
          postMessage: async (message) => {
            console.log(`[MOCK] webview.postMessage: ${JSON.stringify(message)}`);
            return true;
          }
        },
        onDidDispose: () => {},
        dispose: () => {}
      };
    },
    withProgress: async (options, task) => {
      console.log(`[MOCK] withProgress: ${options?.title || 'No title'}`);
      return await task({
        report: (value) => {
          console.log(`[MOCK] progress.report: ${JSON.stringify(value)}`);
        }
      });
    },
    createTerminal: (name) => {
      console.log(`[MOCK] createTerminal: ${name}`);
      return {
        sendText: (text) => {
          console.log(`[MOCK] terminal.sendText: ${text}`);
        },
        show: () => {
          console.log(`[MOCK] terminal.show`);
        }
      };
    }
  },
  
  // Mock workspace object
  workspace: {
    getConfiguration: (section) => {
      console.log(`[MOCK] getConfiguration: ${section}`);
      return {
        get: (key, defaultValue) => {
          console.log(`[MOCK] config.get: ${key} = ${defaultValue}`);
          return defaultValue;
        },
        update: async (key, value, target) => {
          console.log(`[MOCK] config.update: ${key} = ${value}`);
        }
      };
    }
  },
  
  // Mock commands object
  commands: {
    executeCommand: async (command, ...args) => {
      console.log(`[MOCK] executeCommand: ${command}`);
      return null;
    }
  },
  
  // Mock Uri class
  Uri: {
    parse: (value) => {
      console.log(`[MOCK] Uri.parse: ${value}`);
      return {
        toString: () => value
      };
    },
    file: (path) => {
      console.log(`[MOCK] Uri.file: ${path}`);
      return {
        toString: () => `file://${path}`
      };
    }
  },
  
  // Mock ViewColumn
  ViewColumn: {
    One: 1,
    Two: 2
  },
  
  // Mock ConfigurationTarget
  ConfigurationTarget: {
    Global: 1,
    Workspace: 2,
    WorkspaceFolder: 3
  },
  
  // Mock ProgressLocation
  ProgressLocation: {
    Notification: 1,
    Window: 2,
    SourceControl: 3
  }
};