import * as vscode from 'vscode';
import { PrologDefinitionProvider } from '../../features/definitionProvider';
import PrologDocumentHighlightProvider from '../../features/documentHighlightProvider';
import PrologHoverProvider from '../../features/hoverProvider';
import PrologLinter from '../../features/linting/prologLinter';
import { PrologFormatter } from '../../features/prologFormatter';
import { PrologReferenceProvider } from '../../features/referenceProvider';
import { PrologCompletionProvider } from '../../features/updateSnippets';

export class LinterService {
  initializeLinter(context: vscode.ExtensionContext, configurationManager: any, PROLOG_MODE: vscode.DocumentFilter) {
    let linter: PrologLinter | undefined;
    if (configurationManager.getConfiguration().linterTrigger !== 'never') {
      linter = new PrologLinter(context);
      linter.activate();

      // Register linter commands
      const linterCommands = [
        {
          command: 'prolog.linter.nextErrLine',
          callback: () => {
            linter?.nextErrLine();
          },
        },
        {
          command: 'prolog.linter.prevErrLine',
          callback: () => {
            linter?.prevErrLine();
          },
        },
      ];

      linterCommands.forEach(command => {
        context.subscriptions.push(vscode.commands.registerCommand(command.command, command.callback));
      });

      // Register code actions provider
      context.subscriptions.push(vscode.languages.registerCodeActionsProvider(PROLOG_MODE, linter));
    }
  }

  registerLanguageProviders(context: vscode.ExtensionContext, PROLOG_MODE: vscode.DocumentFilter) {
    // Hover provider
    context.subscriptions.push(
      vscode.languages.registerHoverProvider(PROLOG_MODE, new PrologHoverProvider())
    );

    // Highlight provider
    context.subscriptions.push(
      vscode.languages.registerDocumentHighlightProvider(PROLOG_MODE, new PrologDocumentHighlightProvider())
    );

    // Definition provider (go to definition command)
    context.subscriptions.push(
      vscode.languages.registerDefinitionProvider(PROLOG_MODE, new PrologDefinitionProvider())
    );

    // Reference provider (find all references command)
    context.subscriptions.push(
      vscode.languages.registerReferenceProvider(PROLOG_MODE, new PrologReferenceProvider())
    );

    // Auto completion provider
    context.subscriptions.push(
      vscode.languages.registerCompletionItemProvider(PROLOG_MODE, new PrologCompletionProvider())
    );

    // File formatting provider
    context.subscriptions.push(
      vscode.languages.registerDocumentRangeFormattingEditProvider(PROLOG_MODE, new PrologFormatter())
    );
    context.subscriptions.push(
      vscode.languages.registerDocumentFormattingEditProvider(PROLOG_MODE, new PrologFormatter())
    );
  }
}
