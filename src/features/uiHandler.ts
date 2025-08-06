export interface UIHandler {
  showErrorMessage(message: string, ...items: string[]): Promise<string | undefined>;
  executeCommand(command: string, ...rest: any[]): Promise<any>;
}

export const defaultUIHandler: UIHandler = {
  showErrorMessage: async (message: string, ...items: string[]): Promise<string | undefined> => {
    console.error(message);
    return undefined;
  },
  executeCommand: async (command: string, ...rest: any[]): Promise<any> => {
    console.log(`Command execution requested: ${command}`);
    return undefined;
  },
};
