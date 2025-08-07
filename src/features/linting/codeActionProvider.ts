import type {
  CancellationToken,
  CodeActionContext,
  Command,
  Range,
  TextDocument,
} from 'vscode';
import { Utils } from '../../utils/utils.js';
import type { ICodeActionProvider } from './interfaces.js';

/**
 * Provides code actions for Prolog linting diagnostics
 */
export class CodeActionProvider implements ICodeActionProvider {
  private commandAddDynamicId: string;
  private commandAddUseModuleId: string;

  constructor() {
    this.commandAddDynamicId = 'prolog.addDynamicDirective';
    this.commandAddUseModuleId = 'prolog.addUseModule';
  }

  /**
   * Provide code actions for diagnostics
   */
  public provideCodeActions(
    document: TextDocument,
    range: Range,
    context: CodeActionContext,
    _token: CancellationToken
  ): Command[] | Promise<Command[]> {
    const codeActions: Command[] = [];

    // Iterate through each diagnostic in the context
    context.diagnostics.forEach(diagnostic => {
      const regex = /Predicate (.+) not defined/;
      const match = diagnostic.message.match(regex);

      // Check if a match is found in the diagnostic message
      if (match && match[1]) {
        let pred = match[1] ?? '';

        // Get modules associated with the predicate using utility function
        const modules = Utils.getPredModules(pred);

        // Check if modules are found for the predicate
        if (modules.length > 0) {
          // Generate code actions for each module and add them to the array
          modules.forEach(module => {
            codeActions.push({
              title: "Add ':- use_module(library(" + module + '), [' + pred + "]).'",
              command: this.commandAddUseModuleId,
              arguments: [document, pred, module, document.uri, diagnostic.range],
            });
          });
        }

        // Extract module information from the document
        const moduleMatch = document.getText().match(/:-\s*module\((\w+),/);
        let currentModule: string = '';
        if (moduleMatch && moduleMatch[1]) {
          currentModule = moduleMatch[1] ?? '';
        }

        // Handle predicates with namespace (module)
        if (pred.indexOf(':') > -1) {
          const [mod, pred1] = pred.split(':');
          if (mod === currentModule && pred1) {
            pred = pred1;
          }
        }

        // Generate code action for adding 'dynamic' directive and add it to the array
        codeActions.push({
          title: "Add ':- dynamic " + pred + ".'",
          command: this.commandAddDynamicId,
          arguments: [document, pred, document.uri, diagnostic.range],
        });
      }
    });

    return codeActions;
  }

  /**
   * Get command IDs used by this provider
   */
  public getCommandIds(): { addDynamic: string; addUseModule: string } {
    return {
      addDynamic: this.commandAddDynamicId,
      addUseModule: this.commandAddUseModuleId,
    };
  }

  /**
   * Check if a diagnostic is handled by this provider
   */
  public canHandleDiagnostic(message: string): boolean {
    return /Predicate .+ not defined/.test(message);
  }

  /**
   * Extract predicate information from diagnostic message
   */
  public extractPredicateFromDiagnostic(message: string): string | null {
    const regex = /Predicate (.+) not defined/;
    const match = message.match(regex);
    return match && match[1] ? match[1] : null;
  }

  /**
   * Get available modules for a predicate
   */
  public getAvailableModules(predicate: string): string[] {
    return Utils.getPredModules(predicate);
  }

  /**
   * Extract current module from document
   */
  public extractCurrentModule(document: TextDocument): string | null {
    const moduleMatch = document.getText().match(/:-\s*module\((\w+),/);
    return moduleMatch && moduleMatch[1] ? moduleMatch[1] : null;
  }

  /**
   * Normalize predicate name (remove module prefix if it matches current module)
   */
  public normalizePredicate(predicate: string, currentModule: string | null): string {
    if (!currentModule || predicate.indexOf(':') === -1) {
      return predicate;
    }

    const [mod, pred] = predicate.split(':');
    return mod === currentModule ? (pred ?? '') : predicate;
  }
}
