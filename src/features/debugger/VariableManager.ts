

export interface IVariable {
  name: string;
  value: string;
  variablesReference?: number;
  type?: string;
  evaluateName?: string;
  children?: IVariable[];
}

export class VariableManager {
  private _variables: IVariable[] = [];

  public setVariables(vars: IVariable[]): void {
    try {
      this._variables = vars;
    } catch (err) {
      console.error('[VariableManager] Error setting variables:', err, vars);
    }
  }

  public getVariables(): IVariable[] {
    return this._variables;
  }

  public clearVariables(): void {
    this._variables = [];
  }

  public findVariable(name: string): IVariable | undefined {
    try {
      return this._variables.find(v => v.name === name);
    } catch (err) {
      console.error('[VariableManager] Error finding variable:', err, name);
      return undefined;
    }
  }

  public evaluate(expression: string): string | null {
    const variable = this.findVariable(expression);
    return variable ? variable.value : null;
  }

  // Support for complex terms and nested structures
  public getChildren(variable: IVariable): IVariable[] {
    return variable.children || [];
  }
}
